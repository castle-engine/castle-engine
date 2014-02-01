{
  Copyright 2003-2014 Michalis Kamburelis.

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
{$modeswitch nestedprocvars}{$H+}

interface

uses CastleSceneCore, CastleGL, CastleShapes, CastleSceneInternalShape;

type
  TBlendingRenderer = class
  private
    SceneCore: TCastleSceneCore;
    SourceFactorSet, DestinationFactorSet: TGLEnum;
    function DefaultSourceFactor: TGLEnum;
    function DefaultDestinationFactor: TGLEnum;
  public
    constructor Create(const AScene: TCastleSceneCore);
    procedure RenderBegin;
    { Determine what blending source/destination factors
      to use for rendering Shape, and set OpenGL glBlendFunc. }
    procedure BeforeRenderShape(const Shape: TShape);
  end;

{ Fill a TShapeList with only opaque (UseBlending = @false) or
  only transparent shapes (UseBlending = @true). }
procedure ShapesFilterBlending(
  Tree: TShapeTree;
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean;
  TestShapeVisibility: TTestShapeVisibility;
  const FilteredShapes: TShapeList; const UseBlending: boolean);

implementation

uses SysUtils, CastleGLUtils, CastleLog, CastleWarnings, X3DNodes, CastleScene;

function BlendingFactorNameToStr(S: string;
  out Factor: TGLEnum;
  var NeedsConstColor, NeedsConstAlpha: boolean;
  Source: boolean): boolean; forward;

{ TBlendingRenderer ---------------------------------------------------------- }

{$define Scene := TCastleScene(SceneCore)}

constructor TBlendingRenderer.Create(const AScene: TCastleSceneCore);
begin
  inherited Create;
  SceneCore := AScene;
end;

function TBlendingRenderer.DefaultSourceFactor: TGLEnum;
begin
  Result := Scene.Attributes.BlendingSourceFactor;
end;

function TBlendingRenderer.DefaultDestinationFactor: TGLEnum;
begin
  Result := Scene.Attributes.BlendingDestinationFactor;
end;

procedure TBlendingRenderer.RenderBegin;
begin
  { Set glBlendFunc using Attributes.BlendingXxxFactor }
  SourceFactorSet := DefaultSourceFactor;
  DestinationFactorSet := DefaultDestinationFactor;
  glBlendFunc(SourceFactorSet, DestinationFactorSet);
end;

procedure TBlendingRenderer.BeforeRenderShape(const Shape: TShape);

{ Looks at Scene.Attributes.BlendingXxx and Appearance.blendMode of X3D node.
  If different than currently set, then changes BlendingXxxFactorSet and updates
  by glBlendFunc. This way, we avoid calling glBlendFunc too often
  (which is potentially costly, since it changes GL state). }

var
  B: TBlendModeNode;
  NewSrc, NewDest: TGLEnum;
  NeedsConstColor, NeedsConstAlpha: boolean;
begin
  NeedsConstColor := false;
  NeedsConstAlpha := false;

  B := Shape.State.BlendMode;
  if B <> nil then
  begin
    if not BlendingFactorNameToStr(B.FdSrcFactor.Value, NewSrc, NeedsConstColor, NeedsConstAlpha, true) then
      NewSrc := DefaultSourceFactor;
    if not BlendingFactorNameToStr(B.FdDestFactor.Value, NewDest, NeedsConstColor, NeedsConstAlpha, false) then
      NewDest := DefaultDestinationFactor;
  end else
  begin
    NewSrc := DefaultSourceFactor;
    NewDest := DefaultDestinationFactor;
  end;

  if (SourceFactorSet <> NewSrc) or
     (DestinationFactorSet <> NewDest) then
  begin
    SourceFactorSet := NewSrc;
    DestinationFactorSet := NewDest;
    glBlendFunc(SourceFactorSet, DestinationFactorSet);
  end;

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

{ Given blending name (as defined by VRML BlendMode node spec,
  http://www.instantreality.org/documentation/nodetype/BlendMode/),
  returns @true and corresponding OpenGL constant as Factor.

  Returns @false if S doesn't match any known name, or it's "none",
  or it's not supported by current OpenGL implementation (some factors
  may require newer OpenGL versions), or it's not for this kind
  (which means it's not for source factor if Source = true,
  or it's not for dest factor is Source = false).

  If returns @true, then also updates NeedsConstXxx.
  "Updates" means that always does something like
    NeedsConstXxx := NeedsConstXxx or <this factor needs them>;
  so can only change from false to true.
}
function BlendingFactorNameToStr(S: string;
  out Factor: TGLEnum;
  var NeedsConstColor, NeedsConstAlpha: boolean;
  Source: boolean): boolean;

type
  TBlendingFactor = record
    Name: string;
    GL: TGLEnum;
    Source, Dest: boolean;
    NeedsConstColor, NeedsConstAlpha: boolean;
  end;

const
  BlendingFactors: array [0..15] of TBlendingFactor =
  (
    { Three most frequently used values are placed at the beginning of the list,
      for speedup. }
    (Name: 'src_alpha'               ; GL: GL_SRC_ALPHA               ; Source: true ; Dest: true ; NeedsConstColor: false; NeedsConstAlpha: false),
    (Name: 'one_minus_src_alpha'     ; GL: GL_ONE_MINUS_SRC_ALPHA     ; Source: true ; Dest: true ; NeedsConstColor: false; NeedsConstAlpha: false),
    (Name: 'one'                     ; GL: GL_ONE                     ; Source: true ; Dest: true ; NeedsConstColor: false; NeedsConstAlpha: false),

    (Name: 'none'                    ; GL: GL_NONE                    ; Source: false; Dest: false; NeedsConstColor: false; NeedsConstAlpha: false),
    (Name: 'zero'                    ; GL: GL_ZERO                    ; Source: true ; Dest: true ; NeedsConstColor: false; NeedsConstAlpha: false),
    (Name: 'dst_color'               ; GL: GL_DST_COLOR               ; Source: true ; Dest: true ; NeedsConstColor: false; NeedsConstAlpha: false),
    (Name: 'src_color'               ; GL: GL_SRC_COLOR               ; Source: true ; Dest: true ; NeedsConstColor: false; NeedsConstAlpha: false),
    (Name: 'one_minus_dst_color'     ; GL: GL_ONE_MINUS_DST_COLOR     ; Source: true ; Dest: true ; NeedsConstColor: false; NeedsConstAlpha: false),
    (Name: 'one_minus_src_color'     ; GL: GL_ONE_MINUS_SRC_COLOR     ; Source: true ; Dest: true ; NeedsConstColor: false; NeedsConstAlpha: false),
    (Name: 'dst_alpha'               ; GL: GL_DST_ALPHA               ; Source: true ; Dest: true ; NeedsConstColor: false; NeedsConstAlpha: false),
    (Name: 'one_minus_dst_alpha'     ; GL: GL_ONE_MINUS_DST_ALPHA     ; Source: true ; Dest: true ; NeedsConstColor: false; NeedsConstAlpha: false),
    (Name: 'src_alpha_saturate'      ; GL: GL_SRC_ALPHA_SATURATE      ; Source: true ; Dest: false; NeedsConstColor: false; NeedsConstAlpha: false),

    (Name: 'constant_color'          ; GL: GL_CONSTANT_COLOR          ; Source: true ; Dest: true ; NeedsConstColor: true ; NeedsConstAlpha: false),
    (Name: 'one_minus_constant_color'; GL: GL_ONE_MINUS_CONSTANT_COLOR; Source: true ; Dest: true ; NeedsConstColor: true ; NeedsConstAlpha: false),
    (Name: 'constant_alpha'          ; GL: GL_CONSTANT_ALPHA          ; Source: true ; Dest: true ; NeedsConstColor: false; NeedsConstAlpha: true ),
    (Name: 'one_minus_constant_alpha'; GL: GL_ONE_MINUS_CONSTANT_ALPHA; Source: true ; Dest: true ; NeedsConstColor: false; NeedsConstAlpha: true )
  );
  SourceToStr: array [boolean] of string = ('destination', 'source');
var
  I: Integer;
begin
  Result := false;

  S := LowerCase(S);

  for I := Low(BlendingFactors) to High(BlendingFactors) do
    if BlendingFactors[I].Name = S then
    begin
      if Source then
        Result := BlendingFactors[I].Source else
        Result := BlendingFactors[I].Dest;

      if Result then
      begin
        Factor := BlendingFactors[I].GL;

        { check is GL version enough, or some GL extensions available
          for more exotic factors. }

        if BlendingFactors[I].NeedsConstColor or
           BlendingFactors[I].NeedsConstAlpha then
        begin
          if not GLFeatures.BlendConstant then
          begin
            if Log then
              WritelnLog('Blending', Format('Blending factor "%s" not available. It requires OpenGL >= 1.4 or ARB_imaging or OpenGL ES >= 2.0 extension, and is known to not work with fglrx (ATI Linux drivers)', [S]));
            Exit(false);
          end;
        end;

        {$ifndef OpenGLES}
        if not GLFeatures.Version_1_4 then
        begin
          if ((Factor = GL_SRC_COLOR) or
              (Factor = GL_ONE_MINUS_SRC_COLOR)) and Source then
          begin
            if Log then
              WritelnLog('Blending', Format('Blending factor "%s" as "source" requires OpenGL 1.4', [S]));
            Exit(false);
          end;

          if ((Factor = GL_DST_COLOR) or
              (Factor = GL_ONE_MINUS_DST_COLOR)) and not Source then
          begin
            if Log then
              WritelnLog('Blending', Format('Blending factor "%s" as "destination" requires OpenGL 1.4', [S]));
            Exit(false);
          end;
        end;
        {$endif}

        NeedsConstColor := NeedsConstColor or BlendingFactors[I].NeedsConstColor;
        NeedsConstAlpha := NeedsConstAlpha or BlendingFactors[I].NeedsConstAlpha;
      end;

      Break;
    end;

  if not Result then
    OnWarning(wtMajor, 'VRML/X3D', Format('Unknown blending %s factor name "%s"',
      [ SourceToStr[Source], S ]));
end;

procedure ShapesFilterBlending(
  Tree: TShapeTree;
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean;
  TestShapeVisibility: TTestShapeVisibility;
  const FilteredShapes: TShapeList; const UseBlending: boolean);

  procedure AddToList(Shape: TShape);
  begin
    if TGLShape(Shape).UseBlending = UseBlending then
      FilteredShapes.Add(Shape);
  end;

  procedure AddToListIfTested(Shape: TShape);
  begin
    if (TGLShape(Shape).UseBlending = UseBlending) and
       TestShapeVisibility(TGLShape(Shape)) then
      FilteredShapes.Add(Shape);
  end;

var
  Capacity: Integer;
begin
  FilteredShapes.Clear;

  { Set Capacity to max value at the beginning, to speed adding items later. }
  Capacity := Tree.ShapesCount(OnlyActive, OnlyVisible, OnlyCollidable);
  FilteredShapes.Capacity := Capacity;

  if Assigned(TestShapeVisibility) then
    Tree.Traverse(@AddToListIfTested, OnlyActive, OnlyVisible, OnlyCollidable) else
    Tree.Traverse(@AddToList, OnlyActive, OnlyVisible, OnlyCollidable);
end;

end.