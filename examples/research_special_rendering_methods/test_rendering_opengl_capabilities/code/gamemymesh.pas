{
  Copyright 2023-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TCastleTransform descendant that renders unlit mesh using TCastleRenderUnlitMesh. }
unit GameMyMesh;

interface

uses SysUtils,
  CastleRenderPrimitives, CastleBoxes, CastleTransform;

type
  { Mesh rendered using TCastleRenderUnlitMesh, not using TCastleScene.
    This is not generally advised, TCastleScene has much more features
    and is easier to use than TCastleRenderUnlitMesh.
    We do this only to test TCastleRenderUnlitMesh with rcForceFixedFunction here. }
  TMyMesh = class(TCastleTransform)
  strict private
    Mesh: TCastleRenderUnlitMesh;
  public
    procedure LocalRender(const Params: TRenderParams); override;
    procedure GLContextClose; override;
    function LocalBoundingBox: TBox3D; override;
  end;

implementation

uses CastleVectors, CastleRenderContext, CastleColors, CastleGLUtils;

{ TMyMesh -------------------------------------------------------------------- }

function TMyMesh.LocalBoundingBox: TBox3D;
begin
  Result := inherited;
  Result := Result + Box3D(
    Vector3(-1, -1, -1),
    Vector3( 1,  1,  1)
  );
end;

procedure TMyMesh.LocalRender(const Params: TRenderParams);

  procedure CreateMesh;
  begin
    Mesh := TCastleRenderUnlitMesh.Create(true);
    Mesh.SetVertexes([
      Vector4(-1, -1, -1, 1),
      Vector4( 1, -1, -1, 1),
      Vector4( 1,  1, -1, 1),
      Vector4(-1,  1, -1, 1),

      Vector4(-1, -1,  1, 1),
      Vector4( 1, -1,  1, 1),
      Vector4( 1,  1,  1, 1),
      Vector4(-1,  1,  1, 1)
    ], false);
    Mesh.SetIndexes([
      // line loop on Z = -1
      0, 1,
      1, 2,
      2, 3,
      3, 0,

      // line loop on Z = 1
      4, 5,
      5, 6,
      6, 7,
      7, 4,

      // connect Z = -1 with Z = 1
      0, 4,
      1, 5,
      2, 6,
      3, 7
    ]);
    Mesh.Color := Yellow;
  end;

var
  SavedDepthTest: Boolean;
  SavedLineWidth: Single;
begin
  inherited;
  SavedDepthTest := RenderContext.DepthTest;
  SavedLineWidth := RenderContext.LineWidth;
  RenderContext.DepthTest := true;
  RenderContext.LineWidth := 5;

  if Mesh = nil then
    CreateMesh;
  Mesh.ModelViewProjection := RenderContext.ProjectionMatrix *
    Params.RenderingCamera.CurrentMatrix * WorldTransform;
  Mesh.Render(pmLines);

  RenderContext.DepthTest := SavedDepthTest;
  RenderContext.LineWidth := SavedLineWidth;
end;

procedure TMyMesh.GLContextClose;
begin
  FreeAndNil(Mesh);
  inherited;
end;

end.
