{%MainUnit castletransform.pas}
{
  Copyright 2010-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
{ Rendering parameters (TRenderParams and friends). }

{$ifdef read_interface}

  { Statistics about what was rendered during last frame.
    You will usually access this by viewport property,
    see @link(TCastleViewport.Statistics). }
  TRenderStatistics = record
    { How many shapes were rendered (send to OpenGL)
      versus all shapes that were potentially visible.
      Potentially visible shapes are the ones with
      TShape.Visible inside a TCastleTransform with TCastleTransform.Exists.

      When ShapesRendered is much smaller than ShapesVisible,
      it means that the algorithm for removing invisible scene parts
      works good.
      This includes per-shape frustum culling (see @link(TCastleScene.FrustumCulling)),
      distance culling (@link(TCastleScene.DistanceCulling)),
      occlusion culling (see @link(
      TCastleRenderOptions.OcclusionQuery)). }
    ShapesRendered, ShapesVisible: Cardinal;

    { The number of shapes that were not rendered,
      but their bounding box was rendered to check with occlusion query.
      This is always zero when not using occlusion query (see
      TCastleRenderOptions.OcclusionQuery).
      Basically, this measures the "invisible overhead" of occlusion query. }
    BoxesOcclusionQueriedCount: Cardinal;

    { TCastleScene rendered vs all visible. }
    ScenesRendered, ScenesVisible: Cardinal;

    { Present the statistics information as a multi-line string.
      Display it e.g. by assigning to TCastleLabel.Caption. }
    function ToString: String;
  end;

  { @exclude Internal.

    List of lights.
    Always TLightInstancesList, but we cannot declare it here as such.  }
  TAbstractLightInstancesList = TObject;

  { @exclude Internal.

    Fog node. Always TFogNode, but we cannot declare it here as such. }
  TAbstractFogNode = TObject;

  { @exclude Internal.

    Indicates rendering target for @link(TRenderingCamera.Target). }
  TRenderTarget = (
    { Normal rendering. }
    rtScreen,
    { Rendering color buffer contents to a texture inside RenderedTexture node. }
    rfRenderedTexture,
    { Rendering color buffer contents to a texture inside GeneratedCubeMap node. }
    rtCubeMapEnvironment,
    { Rendering depth buffer contents to shadow map inside GeneratedShadowMap node. }
    rtShadowMap,
    { Rendering with a special VSM shader to capture shadow map texture
      (in the normal color buffer). }
    rtVarianceShadowMap
  );

  { @exclude Internal.
    Rendering pass, for multi-pass rendering, like for shadow volumes. }
  TInternalRenderingPass = 0..1;

  { Rendering pass, for user purposes.
    Useful to keep shaders cached when you render the same scene multiple times
    in the same frame (under different lighting conditions or other things
    that change shaders). }
  TUserRenderingPass = 0..2;

  { @exclude Internal.

    Current camera used for rendering, used for @link(TRenderParams.RenderingCamera).
    This is part of TRenderParams information,
    and (like the rest of TRenderParams) it is "mostly" internal.
    If you're using the engine in a normal way, you should never need
    to deal with this class.

    This is interesting to you only if you write custom rendering code.

    In normal applications, you shoud only get/set camera using TCastleCamera
    descendants, through @link(TCastleViewport.Camera) and related properties.
    Do not use the TRenderingCamera class in normal applications. }
  TRenderingCamera = class
  strict private
    FTarget: TRenderTarget;
  public
    Camera: TCastleCamera;

    { Current camera position.
      In world coordinates, i.e. this is equal TCastleCamera.WorldTranslation,
      and in coordinates of whole TCastleViewport.Items. }
    Position: TVector3;

    { Current camera matrix. Transforms from world space (normal 3D space)
      to camera space (camera space is the space where you're always
      standing on zero point, looking in -Z, and so on).

      This is needed for various things, like
      TextureCoordinateGenerator.mode = "WORLDSPACE*" or generating
      Viewpoint.camera[Inverse]Matrix event.

      Always after changing this, change also all other camera
      fields. }
    Matrix: TMatrix4;

    { Inverse of @link(Matrix).

      Always call InverseMatrixNeeded before using it,
      InverseMatrixNeeded will check InverseMatrixDone
      and eventually will calculate inverse and set InverseMatrixDone to
      @true. }
    InverseMatrix: TMatrix4;
    InverseMatrixDone: boolean;

    { Camera rotation matrix. That is, this is like @link(Matrix) but
      it doesn't move the camera, only rotates it.

      It's guaranteed that this is actually only 3x3 matrix,
      the 4th row and 4th column are all zero except the lowest right item
      which is 1.0. }
    RotationMatrix: TMatrix4;

    { Use RotationMatrix as camera matrix for rendering. }
    RotationOnly: boolean;

    { Inverse of RotationMatrix.

      Always call RotationInverseMatrixNeeded before using it,
      RotationInverseMatrixNeeded will check RotationInverseMatrixDone
      and eventually will calculate inverse and set RotationInverseMatrixDone to
      @true. }
    RotationInverseMatrix: TMatrix4;
    RotationInverseMatrixDone: boolean;

    Frustum: TFrustum;

    procedure InverseMatrixNeeded;
    procedure RotationInverseMatrixNeeded;

    { Camera rotation matrix, as a 3x3 matrix. }
    function RotationMatrix3: TMatrix3;
    function RotationInverseMatrix3: TMatrix3;

    { Set all properties (except Target) from TCastleCamera instance in ACamera.
      See @link(FromMatrix) for comments about @link(Target) property. }
    procedure FromCameraObject(const ACamera: TCastleCamera);

    { Set all properties (except Target) from explict matrices (and explicit position,
      makes it faster than extracting position from matrix).
      ProjectionMatrix is needed to calculate frustum.
      Sets @link(Camera) to @nil. }
    procedure FromMatrix(
      const APosition: TVector3;
      const AMatrix, ARotationMatrix, ProjectionMatrix: TMatrix4);

    property Target: TRenderTarget read FTarget write FTarget;

    procedure Assign(const Source: TRenderingCamera);
  end;

  { Information that a TCastleTransform object needs to render.
    Read-only for @link(TCastleTransform.LocalRender)
    (except @link(Statistics), which should be updated during rendering).

    @bold(This is mostly an internal class).

    You should not need to create it,
    you should not need to read anything inside or deal with this class otherwise,
    and actually you should not need to override
    @link(TCastleTransform.LocalRender) during normal engine usage.
    But it may be useful for special customized rendering. }
  TRenderParams = class
    { Which parts should be rendered: opaque (@false) or transparent (@true).
      This should "filter" the rendered parts by @link(TCastleTransform.LocalRender). }
    Transparent: boolean;

    { Should we render parts that may receive shadow volumes, or ones that don't, or both.
      This should never be [].
      During rendering, simply check does it match TCastleScene.ReceiveShadowVolumes.
      This should "filter" the rendered parts by @link(TCastleTransform.LocalRender). }
    ShadowVolumesReceivers: set of Boolean;

    { If @true, means that we're using multi-pass
      shadowing technique (like shadow volumes),
      and currently doing the "shadowed" pass.

      Which means that most lights (ones with shadowVolumes = TRUE)
      should be turned off, see [https://castle-engine.io/x3d_extensions.php#section_ext_shadows].) }
    InShadow: boolean;

    { Value > 0 means we're inside some stencil test (like for
      InShadow = @false pass of shadow volumes). }
    StencilTest: Cardinal;

    { Rendering pass number, for internal multi-pass rendering,
      like for shadow volumes. }
    InternalPass: TInternalRenderingPass;

    { Rendering pass number, for user purposes.
      Multiplied with InternalPass to get final pass number,
      that determines shaders used. }
    UserPass: TUserRenderingPass;

    { Transformation that should be applied to the rendered result.
      If TransformIdentity, then Transform and InverseTransform is always identity.
      @groupBegin }
    Transform, InverseTransform: PMatrix4;
    TransformIdentity: boolean;
    { @groupEnd }

    { Current rendering statistics, should be updated by each
      @link(TCastleTransform.LocalRender) call. }
    Statistics: TRenderStatistics;

    { Fog that affects all scenes. }
    GlobalFog: TAbstractFogNode;

    { Camera frustum in local coordinates. Local for the TCastleTransform instance
      receiving this TRenderParams as @link(TCastleTransform.LocalRender)
      parameter, so it may be different from RenderingCamera.Frustum.

      As a special exception, it can be @nil, but only when TCastleScene.LocalRender
      is called (this happens from TBackgroundScene.Render).
      The TCastleScene.LocalRender should ignore frustum when it is @nil. }
    Frustum: PFrustum;

    { Camera information for renderer. }
    RenderingCamera: TRenderingCamera;

    { Are we traversing now only inside parents with MeaningfulParent,
      i.e. outside of TCastleTransformReference.Reference. }
    InMeaningfulParent: Boolean;

    { At design-time, cameras update their own projection, to show camera gizmo.
      These are parameters to do this. }
    ProjectionBox: TBox3DEvent;
    ProjectionViewportWidth, ProjectionViewportHeight: Single;

    constructor Create;

    { Lights that shine on object (headlight and lights from all scenes with CastGlobalLights). }
    function GlobalLights: TAbstractLightInstancesList; virtual; abstract;

    function RenderTransform: TMatrix4; deprecated 'use Transform';
    function RenderTransformIdentity: boolean; deprecated 'use TransformIdentity';
  end;

  { Information that a TCastleTransform object needs to prepare rendering.

    This is @bold(mostly an internal class). You should not need to create it,
    you should not need to read anything inside or deal with this class otherwise.

    The only official usage allowed is to pass an instance of this class
    taken from @link(TCastleViewport.PrepareParams)
    as a parameter to @link(TCastleTransform.PrepareResources) and friends
    like @link(T3DResource.Prepare). You should treat this class as a "black box"
    in normal applications. }
  TPrepareParams = class
    { Include a headlight and all lights that shine on all
      scenes (thanks to @link(TCastleScene.CastGlobalLights)).

      It is not necessary to define this (it may be @nil).
      And all the lighting is dynamic, so of course you can always
      turn on / off things like a headlight during the game.
      However, passing here the appropriate lights will mean that the shaders
      are immediately prepared for the current lighting.

      @exclude }
    InternalGlobalLights: TAbstractLightInstancesList;

    { World fog, in any, to prepare for.
      @exclude }
    InternalGlobalFog: TAbstractFogNode;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TRenderStatistics ---------------------------------------------------------- }

function TRenderStatistics.ToString: String;
var
  OcclusionStats: String;
begin
  if BoxesOcclusionQueriedCount <> 0 then
    OcclusionStats := Format(' (+ %d boxes for occlusion query)', [
      BoxesOcclusionQueriedCount
    ])
  else
    OcclusionStats := '';

  Result := Format('Shapes %d%s / %d, Scenes: %d / %d', [
    ShapesRendered,
    OcclusionStats,
    ShapesVisible,
    ScenesRendered,
    ScenesVisible
  ]);
end;

{ TRenderingCamera --------------------------------------------------------------- }

procedure TRenderingCamera.InverseMatrixNeeded;
begin
  if not InverseMatrixDone then
  begin
    if not Matrix.TryInverse(InverseMatrix) then
    begin
      InverseMatrix := TMatrix4.Identity;
      WritelnLogMultiline('Camera', 'Camera matrix cannot be inverted, conversions between world and camera space will not be done. Camera matrix is: ' +
        Matrix.ToRawString('  '));
    end;
    InverseMatrixDone := true;
  end;
end;

procedure TRenderingCamera.RotationInverseMatrixNeeded;
begin
  if not RotationInverseMatrixDone then
  begin
    if not RotationMatrix.TryInverse(RotationInverseMatrix) then
    begin
      RotationInverseMatrix := TMatrix4.Identity;
      WritelnLogMultiline('Camera', 'Camera rotation matrix cannot be inverted, conversions between world and camera space will not be done. Camera matrix is: ' +
        RotationMatrix.ToRawString('  '));
    end;
    RotationInverseMatrixDone := true;
  end;
end;

function TRenderingCamera.RotationMatrix3: TMatrix3;
begin
  Move(RotationMatrix.Data[0], Result.Data[0], SizeOf(Single) * 3);
  Move(RotationMatrix.Data[1], Result.Data[1], SizeOf(Single) * 3);
  Move(RotationMatrix.Data[2], Result.Data[2], SizeOf(Single) * 3);
end;

function TRenderingCamera.RotationInverseMatrix3: TMatrix3;
begin
  Move(RotationInverseMatrix.Data[0], Result.Data[0], SizeOf(Single) * 3);
  Move(RotationInverseMatrix.Data[1], Result.Data[1], SizeOf(Single) * 3);
  Move(RotationInverseMatrix.Data[2], Result.Data[2], SizeOf(Single) * 3);
end;

procedure TRenderingCamera.FromCameraObject(const ACamera: TCastleCamera);
begin
  Camera := ACamera;
  Position := ACamera.WorldTranslation;
  Matrix := ACamera.Matrix;
  InverseMatrixDone := false;
  RotationMatrix := ACamera.RotationMatrix;
  RotationInverseMatrixDone := false;
  Frustum := ACamera.Frustum;
end;

procedure TRenderingCamera.FromMatrix(
  const APosition: TVector3;
  const AMatrix, ARotationMatrix, ProjectionMatrix: TMatrix4);
begin
  Camera := nil;
  Position := APosition;
  Matrix := AMatrix;
  InverseMatrixDone := false;
  RotationMatrix := ARotationMatrix;
  RotationInverseMatrixDone := false;
  Frustum.Init(ProjectionMatrix, AMatrix);
end;

procedure TRenderingCamera.Assign(const Source: TRenderingCamera);
begin
  FTarget                   := Source.FTarget                  ;
  Matrix                    := Source.Matrix                   ;
  InverseMatrix             := Source.InverseMatrix            ;
  InverseMatrixDone         := Source.InverseMatrixDone        ;
  RotationMatrix            := Source.RotationMatrix           ;
  RotationOnly              := Source.RotationOnly             ;
  RotationInverseMatrix     := Source.RotationInverseMatrix    ;
  RotationInverseMatrixDone := Source.RotationInverseMatrixDone;
  Frustum                   := Source.Frustum                  ;
end;

{ TRenderParams -------------------------------------------------------------- }

var
  GlobalIdentityMatrix: TMatrix4;

constructor TRenderParams.Create;
begin
  inherited;
  Transform := @GlobalIdentityMatrix;
  InverseTransform := @GlobalIdentityMatrix;
  TransformIdentity := true;
  InMeaningfulParent := true;
end;

function TRenderParams.RenderTransform: TMatrix4;
begin
  Result := Transform^;
end;

function TRenderParams.RenderTransformIdentity: boolean;
begin
  Result := TransformIdentity;
end;

{$endif read_implementation}
