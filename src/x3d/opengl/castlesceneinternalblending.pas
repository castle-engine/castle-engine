{
  Copyright 2003-2017 Michalis Kamburelis.

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

uses CastleSceneCore, CastleGLUtils, CastleShapes, CastleSceneInternalShape;

type
  TBlendingRenderer = class
  private
    SceneCore: TCastleSceneCore;
    SourceFactorSet: TBlendingSourceFactor;
    DestinationFactorSet: TBlendingDestinationFactor;
    function DefaultSourceFactor: TBlendingSourceFactor;
    function DefaultDestinationFactor: TBlendingDestinationFactor;
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

uses SysUtils, CastleGL, CastleLog, X3DNodes, CastleScene;

function BlendingFactorNameToStr(S: string;
  out Factor: TBlendingSourceFactor;
  var NeedsConstColor, NeedsConstAlpha: boolean): boolean;
const
  FactorNames: array [TBlendingSourceFactor] of string =
  (
    'src_alpha',
    'one_minus_src_alpha',
    'zero',
    'one',

    'dst_color',
    'src_color',
    'dst_alpha',

    'one_minus_dst_color',
    'one_minus_src_color',
    'one_minus_dst_alpha',

    'src_alpha_saturate',

    'constant_color',
    'one_minus_constant_color',
    'constant_alpha',
    'one_minus_constant_alpha'
  );
  ConstColor = [bsConstantColor, bsOneMinusConstantColor];
  ConstAlpha = [bsConstantAlpha, bsOneMinusConstantAlpha];
var
  I: TBlendingSourceFactor;
begin
  Result := false;

  S := LowerCase(S);
  if S = 'none' then Exit(false);

  for I := Low(I) to High(I) do
    if FactorNames[I] = S then
    begin
      Factor := I;

      { check is GL version enough, or some GL extensions available
        for more exotic factors. }

      if ((I in ConstColor) or (I in ConstAlpha)) and
         (not GLFeatures.BlendConstant) then
      begin
        if Log then
          WritelnLog('Blending', Format('Blending factor "%s" not available. It requires OpenGL >= 1.4 or ARB_imaging or OpenGL ES >= 2.0 extension, and is known to not work with fglrx (ATI Linux drivers)', [S]));
        Exit(false);
      end;

      {$ifndef OpenGLES}
      if (not GLFeatures.Version_1_4) and
         (Factor in [bsSrcColor, bsOneMinusSrcColor]) then
      begin
        if Log then
          WritelnLog('Blending', Format('Blending factor "%s" as "source" requires OpenGL 1.4', [S]));
        Exit(false);
      end;
      {$endif}

      NeedsConstColor := NeedsConstColor or (I in ConstColor);
      NeedsConstAlpha := NeedsConstAlpha or (I in ConstAlpha);

      Exit(true);
    end;

  WritelnWarning('VRML/X3D', Format('Unknown blending source factor name "%s"', [S]));
end;

function BlendingFactorNameToStr(S: string;
  out Factor: TBlendingDestinationFactor;
  var NeedsConstColor, NeedsConstAlpha: boolean): boolean;
const
  FactorNames: array [TBlendingDestinationFactor] of string =
  (
    'src_alpha',
    'one_minus_src_alpha',
    'zero',
    'one',

    'dst_color',
    'src_color',
    'dst_alpha',

    'one_minus_dst_color',
    'one_minus_src_color',
    'one_minus_dst_alpha',

    // 'src_alpha_saturate', // not supported as destination factor

    'constant_color',
    'one_minus_constant_color',
    'constant_alpha',
    'one_minus_constant_alpha'
  );
  ConstColor = [bdConstantColor, bdOneMinusConstantColor];
  ConstAlpha = [bdConstantAlpha, bdOneMinusConstantAlpha];
var
  I: TBlendingDestinationFactor;
begin
  Result := false;

  S := LowerCase(S);
  if S = 'none' then Exit(false);

  for I := Low(I) to High(I) do
    if FactorNames[I] = S then
    begin
      Factor := I;

      { check is GL version enough, or some GL extensions available
        for more exotic factors. }

      if ((I in ConstColor) or (I in ConstAlpha)) and
         (not GLFeatures.BlendConstant) then
      begin
        if Log then
          WritelnLog('Blending', Format('Blending factor "%s" not available. It requires OpenGL >= 1.4 or ARB_imaging or OpenGL ES >= 2.0 extension, and is known to not work with fglrx (ATI Linux drivers)', [S]));
        Exit(false);
      end;

      {$ifndef OpenGLES}
      if (not GLFeatures.Version_1_4) and
         (Factor in [bdDstColor, bdOneMinusDstColor]) then
      begin
        if Log then
          WritelnLog('Blending', Format('Blending factor "%s" as "destination" requires OpenGL 1.4', [S]));
        Exit(false);
      end;
      {$endif}

      NeedsConstColor := NeedsConstColor or (I in ConstColor);
      NeedsConstAlpha := NeedsConstAlpha or (I in ConstAlpha);

      Exit(true);
    end;

  WritelnWarning('VRML/X3D', Format('Unknown blending destination factor name "%s"', [S]));
end;

{ TBlendingRenderer ---------------------------------------------------------- }

{$define Scene := TCastleScene(SceneCore)}

constructor TBlendingRenderer.Create(const AScene: TCastleSceneCore);
begin
  inherited Create;
  SceneCore := AScene;
end;

function TBlendingRenderer.DefaultSourceFactor: TBlendingSourceFactor;
begin
  Result := Scene.Attributes.BlendingSourceFactor;
end;

function TBlendingRenderer.DefaultDestinationFactor: TBlendingDestinationFactor;
begin
  Result := Scene.Attributes.BlendingDestinationFactor;
end;

procedure TBlendingRenderer.RenderBegin;
begin
  { Set glBlendFunc using Attributes.BlendingXxxFactor }
  SourceFactorSet := DefaultSourceFactor;
  DestinationFactorSet := DefaultDestinationFactor;
  GLBlendFunction(SourceFactorSet, DestinationFactorSet);
end;

procedure TBlendingRenderer.BeforeRenderShape(const Shape: TShape);

{ Looks at Scene.Attributes.BlendingXxx and Appearance.blendMode of X3D node.
  If different than currently set, then changes BlendingXxxFactorSet and updates
  by glBlendFunc. This way, we avoid calling glBlendFunc too often
  (which is potentially costly, since it changes GL state). }

var
  B: TBlendModeNode;
  NewSrc: TBlendingSourceFactor;
  NewDest: TBlendingDestinationFactor;
  NeedsConstColor, NeedsConstAlpha: boolean;
begin
  NeedsConstColor := false;
  NeedsConstAlpha := false;

  B := Shape.State.BlendMode;
  if B <> nil then
  begin
    if not BlendingFactorNameToStr(B.FdSrcFactor.Value, NewSrc, NeedsConstColor, NeedsConstAlpha) then
      NewSrc := DefaultSourceFactor;
    if not BlendingFactorNameToStr(B.FdDestFactor.Value, NewDest, NeedsConstColor, NeedsConstAlpha) then
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
    GLBlendFunction(SourceFactorSet, DestinationFactorSet);
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

{ Given blending name (as defined by X3D BlendMode node spec,
  http://www.instantreality.org/documentation/nodetype/BlendMode/),
  returns @true and corresponding Factor.

  Returns @false if S doesn't match any known name, or it's "none",
  or it's not supported by the current OpenGL implementation (some factors
  may require newer OpenGL versions), or it's not for this kind
  (which means it's not for source factor if Source = true,
  or it's not for dest factor is Source = false).

  If returns @true, then also updates NeedsConstXxx.
  "Updates" means that always does something like
    NeedsConstXxx := NeedsConstXxx or <this factor needs them>;
  so can only change from false to true.
}

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
