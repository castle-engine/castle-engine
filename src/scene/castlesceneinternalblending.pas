{
  Copyright 2003-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Blending management for OpenGL rendering.
  @exclude Internal unit for CastleScene. }
unit CastleSceneInternalBlending;

{$I castleconf.inc}

interface

uses CastleSceneCore, CastleGLUtils, CastleShapes, CastleSceneInternalShape,
  CastleRenderOptions;

type
  TBlendingRenderer = class
  private
    SceneCore: TCastleSceneCore;
    Active: Boolean; //< between RenderBegin and RenderEnd
    function DefaultSourceFactor: TBlendingSourceFactor;
    function DefaultDestinationFactor: TBlendingDestinationFactor;
  public
    constructor Create(const AScene: TCastleSceneCore);

    { Start rendering shapes with blending. }
    procedure RenderBegin;

    { Stop rendering shapes with blending.
      It is ignored if RenderBegin was not called earlier. }
    procedure RenderEnd;

    { If we are rendering with blending (between RenderBegin and RenderEnd)
      and this Shape uses blending,
      then determine what blending source/destination factors
      to use for rendering Shape, and set OpenGL state like glBlendFunc. }
    procedure BeforeRenderShape(const Shape: TGLShape);
  end;

{ Fill a TShapeList with only opaque (UseBlending = @false) or
  only transparent shapes (UseBlending = @true). }
procedure ShapesFilterBlending(
  const Tree: TShapeTree;
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean;
  const TestShapeVisibility: TTestShapeVisibility;
  const FilteredShapes: TShapeList; const UseBlending: boolean);

implementation

uses SysUtils,
  {$ifdef FPC} CastleGL, {$else} OpenGL, OpenGLext, {$endif}
  CastleLog, X3DNodes, CastleScene, CastleTimeUtils, CastleRenderContext;

{ TBlendingRenderer ---------------------------------------------------------- }

constructor TBlendingRenderer.Create(const AScene: TCastleSceneCore);
begin
  inherited Create;
  SceneCore := AScene;
end;

function TBlendingRenderer.DefaultSourceFactor: TBlendingSourceFactor;
begin
  Result := TCastleScene(SceneCore).RenderOptions.BlendingSourceFactor;
end;

function TBlendingRenderer.DefaultDestinationFactor: TBlendingDestinationFactor;
begin
  Result := TCastleScene(SceneCore).RenderOptions.BlendingDestinationFactor;
end;

procedure TBlendingRenderer.RenderBegin;
begin
  Active := true;

  RenderContext.DepthBufferUpdate := false;
  RenderContext.BlendingEnable(DefaultSourceFactor, DefaultDestinationFactor);
end;

procedure TBlendingRenderer.RenderEnd;
begin
  if not Active then
    Exit;
  Active := false;

  { restore glDepthMask and blending state to default values }
  RenderContext.DepthBufferUpdate := true;
  RenderContext.BlendingDisable;
end;

procedure TBlendingRenderer.BeforeRenderShape(const Shape: TGLShape);

{ Looks at Scene.RenderOptions.BlendingXxx and Appearance.BlendMode of X3D node.
  If different than currently set, then changes blending mode. }

const
  SrcConstColor = [bsConstantColor, bsOneMinusConstantColor];
  SrcConstAlpha = [bsConstantAlpha, bsOneMinusConstantAlpha];
  DestConstColor = [bdConstantColor, bdOneMinusConstantColor];
  DestConstAlpha = [bdConstantAlpha, bdOneMinusConstantAlpha];
var
  B: TBlendModeNode;
  NewSrc: TBlendingSourceFactor;
  NewDest: TBlendingDestinationFactor;
  NeedsConstColor, NeedsConstAlpha: Boolean;
begin
  if not (Active and Shape.UseBlending) then
    Exit;

  B := Shape.State.BlendMode;
  if B <> nil then
  begin
    NewSrc := B.SrcFactor;
    NewDest := B.DestFactor;
    NeedsConstColor := (NewSrc in SrcConstColor) or (NewDest in DestConstColor);
    NeedsConstAlpha := (NewSrc in SrcConstAlpha) or (NewDest in DestConstAlpha);
  end else
  begin
    NewSrc := DefaultSourceFactor;
    NewDest := DefaultDestinationFactor;
    NeedsConstColor := false;
    NeedsConstAlpha := false;
  end;

  RenderContext.BlendingEnable(NewSrc, NewDest);

  { We track last source/dest factor, but we don't track last constant color/alpha.
    So just set them always, if needed. }
  if GLFeatures.BlendConstant then
  begin
    if NeedsConstColor then
    begin
      Assert(B <> nil);
      glBlendColor(
        B.FdColor.Value[0],
        B.FdColor.Value[1],
        B.FdColor.Value[2],
        1 - B.FdColorTransparency.Value);
    end else
    if NeedsConstAlpha then
    begin
      Assert(B <> nil);
      glBlendColor(0, 0, 0, 1 - B.FdColorTransparency.Value);
    end;
  end;
end;

{ global --------------------------------------------------------------------- }

procedure ShapesFilterBlending(
  const Tree: TShapeTree;
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean;
  const TestShapeVisibility: TTestShapeVisibility;
  const FilteredShapes: TShapeList; const UseBlending: boolean);
var
  List: TShapeList;
  Shape: TShape;
  I: Integer;
begin
  //FrameProfiler.Start(fmRenderShapesFilterBlending);

  { Use "Count := 0" instead of Clear, this way previous Capacity remains }
  FilteredShapes.Count := 0;
  { Set Capacity to max value at the beginning, to speed adding items later. }
  FilteredShapes.Capacity := Tree.MaxShapesCount;

  List := Tree.TraverseList(OnlyActive, OnlyVisible, OnlyCollidable);

  if Assigned(TestShapeVisibility) then
  begin
    for I := 0 to List.Count - 1 do
    begin
      Shape := List[I];
      if (TGLShape(Shape).UseBlending = UseBlending) and TestShapeVisibility(TGLShape(Shape)) then
        FilteredShapes.Add(Shape);
    end;
  end else
  begin
    for I := 0 to List.Count - 1 do
    begin
      Shape := List[I];
      if TGLShape(Shape).UseBlending = UseBlending then
        FilteredShapes.Add(Shape);
    end;
  end;

  //FrameProfiler.Stop(fmRenderShapesFilterBlending);
end;

end.
