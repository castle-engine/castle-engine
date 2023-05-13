{
  Copyright 2010-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Screen effects (post-processing) using shaders (TCastleScreenEffects). }
unit CastleScreenEffects;

{$I castleconf.inc}

interface

uses SysUtils, Classes,
  {$ifdef FPC} CastleGL, {$else} OpenGL, OpenGLext, {$endif}
  CastleVectors, CastleGLShaders, CastleUIControls, X3DNodes, CastleGLImages,
  CastleRectangles, CastleScene, CastleTransform, CastleCameras, CastleGLUtils,
  CastleRenderOptions;

{ Standard GLSL vertex shader for screen effect.
  @bold(In your own programs, it's usually easier to use TGLSLScreenEffect,
  and then this function is not necessary). }
function ScreenEffectVertex: string; deprecated 'this will be internal function soon, instead use TCastleScreenEffects or TCastleViewport and add screen effects there';

(*Library of GLSL fragment shader functions useful for screen effects.
  This looks at current OpenGL context multi-sampling capabilities
  to return the correct shader code.

  @bold(In your own programs, it's usually easier to use TGLSLScreenEffect,
  and then this function is not necessary).

  Note that to work with OpenGLES, we have to glue all fragment shaders,
  and ScreenEffectFragment must be before the actual shader code.
  The string returned by this function is guaranteed to end with a newline,
  to make it easy.

  So you usually want to create screen effect shaders like this:

  @longCode(#
    Shader := TGLSLProgram.Create;
    Shader.AttachVertexShader(ScreenEffectVertex);
    Shader.AttachFragmentShader(
      ScreenEffectFragment(false { or true if you use depth }) +
      '... my custom screen effect GLSL code ...');
    Shader.Link;
    { umIgnore is a good idea here, in case some uniform variable
      from ScreenEffectFragment code may be left unused. }
    Shader.UniformMissing := umIgnore;
  #)

*)
function ScreenEffectFragment(const Depth: boolean): string; deprecated 'this will be internal function soon, instead use TCastleScreenEffects or TCastleViewport and add screen effects there';

type
  { GLSL shader program specialized for rendering screen effects.
    See https://castle-engine.io/x3d_extensions_screen_effects.php
    about screen effects.

    Do not use the ancestor AttachVertexShader and AttachFragmentShader
    methods, instead set the @link(ScreenEffectShader).
    This way, the standard GLSL functionality of screen effects
    will be attached to the vertex and fragment shader code automatically.
    At link time, this looks at current OpenGL context multi-sampling capabilities,
    and the @link(NeedsDepth) value, to link the correct shader code. }
  TGLSLScreenEffect = class(TGLSLProgram)
  private
    FScreenEffectShader: string;
    FNeedsDepth: boolean;
  public
    constructor Create;

    property NeedsDepth: boolean read FNeedsDepth write FNeedsDepth default false;

    { In this class, UniformMissing is by default umIgnore, since it's
      normal that screen effect doesn't use some of it's uniform variables. }
    property UniformMissing default umIgnore;

    { Attach GLSL code for the screen effect (executed as part of fragment shader).
      See https://castle-engine.io/x3d_extensions_screen_effects.php . }
    property ScreenEffectShader: string read FScreenEffectShader write FScreenEffectShader;

    procedure Link; override;
  end deprecated 'this will be internal class soon, instead use TCastleScreenEffects or TCastleViewport and add screen effects there';

  { Control that applies shader screen effects (post-processing)
    on the rendering done by children and (when this class is used as an ancestor)
    it's descendants.

    It can be used to apply screen effects over any UI control,
    like @link(TCastleViewport),
    @link(TCastleButton), @link(TCastleImageControl) and so on.
    Simply place the desired control as child of this control.

    To make it easier to apply effects on @link(TCastleViewport),
    it already descends from this class. So, while you can, you don't need to wrap
    @link(TCastleViewport) instance inside another @link(TCastleScreenEffects)
    instance. You can instead directly call @link(AddScreenEffect) on your
    @link(TCastleViewport) instance.

    Note that the UI controls rendered for the screen effects
    (our children and descendants) must always initialize and fill
    colors of the entire rectangle (@link(RenderRect)) of this control.
    Otherwise, the results are undefined, as an internal texture that is used
    for screen effects is initially undefined.
    You may use e.g. @link(TCastleRectangleControl) to fill the background with a solid color
    from @link(TCastleRectangleControl.Color).
    Or use @link(TCastleViewport) with @link(TCastleViewport.Transparent) = @false (default)
    which fills background with @link(TCastleViewport.BackgroundColor).
  }
  TCastleScreenEffects = class(TCastleUserInterface)
  strict private
    type
      TScreenPoint = packed record
        Position: TVector2;
        TexCoord: TVector2;
      end;
    var
      { Scene containing screen effects }
      ScreenEffectsScene: TCastleScene;
      ScreenEffectsRoot: TX3DRootNode;
      FScreenEffectsTimeScale: Single;
      { World to pass dummy camera position to ScreenEffectsScene. }
      World: TCastleRootTransform;

      { OpenGL(ES) resources for screen effects. }
      { If a texture for screen effects is ready, then
        ScreenEffectTextureDest/Src/Depth/Target are non-zero and
        ScreenEffectRTT is non-nil.
        Also, ScreenEffectTextureWidth/Height indicate size of the texture,
        as well as ScreenEffectRTT.Width/Height. }
      ScreenEffectTextureDest, ScreenEffectTextureSrc: TGLuint;
      ScreenEffectTextureTarget: TGLenum;
      ScreenEffectTextureDepth: TGLuint;
      ScreenEffectRTT: TGLRenderToTexture;
      ScreenEffectTextureWidth: Cardinal;
      ScreenEffectTextureHeight: Cardinal;
      { Saved ScreenEffectsCount/NeedDepth result, during rendering. }
      CurrentScreenEffectsCount: Integer;
      CurrentScreenEffectsNeedDepth: boolean;
      ScreenPointVbo: TGLuint;
      ScreenPointVao: TVertexArrayObject;
      ScreenPoint: packed array [0..3] of TScreenPoint;

      FScreenEffectsEnable: Boolean;

    function GetScreenEffectsCount: Cardinal;
    function GetScreenEffectsNeedDepth: Boolean;
    function GetScreenEffect(const Index: Integer): TGLSLProgram;
  protected
    { Valid only between Render and RenderOverChildren calls.
      Tells whether we actually use screen effects, thus RenderWithoutScreenEffects
      renders to texture.
      Read-only in descendants. }
    RenderScreenEffects: Boolean;

    { Descendants with special rendering code should override this,
      and @italic(never) override @link(Render) or @link(RenderOverChildren)
      (as those two methods have special implementation in this class).

      This rendering method will be called regardless if we have
      or not some screen effects.
      When no screen effects are actually used (e.g. @link(AddScreenEffect)
      wasn't used, @code(InternalExtraScreenEffectsCount) is zero), then our @link(Render)
      trivially calls this method without doing anything else. }
    procedure RenderWithoutScreenEffects; virtual;

    { Render these additional screen effects, defined by explicit TGLSLProgram
      instead of by AddScreenEffect call.
      This is internal, only to be used by TCastleViewport.
      @exclude }
    function InternalExtraGetScreenEffects(const Index: Integer): TGLSLProgram; virtual;
    { @exclude }
    function InternalExtraScreenEffectsCount: Integer; virtual;
    { @exclude }
    function InternalExtraScreenEffectsNeedDepth: Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Add or remove a ScreenEffect node that defines new shader screen effect.
      See https://castle-engine.io/x3d_extensions_screen_effects.php .

      In the simple case, you can pass here an instance of TScreenEffectNode.
      In a general case, you can passs here any X3D node you want,
      just remember that only the TScreenEffectNode inside will be rendered.
      You can e.g. pass a TGroupNode with TScreenEffectNode and TTimeSensorNode
      as children,
      and use TTimeSensorNode to provide time to your shader uniform parameter.
      Or you can e.g. pass an X3D graph loaded from X3D file
      using @link(X3DLoad.LoadNode), this way you can define effects
      inside an external X3D file, e.g. like this:

      @longCode(#
      ScreenEffects.AddScreenEffect(
        LoadNode('castle-data:/screen_effects_scene.x3dv'));
      #)

      If you're looking for inspirations what to put in screen_effects_scene.x3dv,
      see https://github.com/castle-engine/demo-models/tree/master/screen_effects .
      See also CGE example in "examples/screen_effects_demo/" directory
      ( https://github.com/castle-engine/castle-engine/tree/master/examples/screen_effects_demo )
      that shows more screen effects code,
      and shows how to load or construct X3D node graph with ScreenEffect.

      The memory management of the node added here is automatic:
      the added screen effect becomes owned by the internal X3D Group node.
      It's reference-count is increased at AddScreenEffect, and decreased
      at RemoveScreenEffect, and it is automatically freed when reference-count
      drops to zero.

      This means that, if you create the provided Node by code,
      and don't add it anywhere else, then the node will be freed
      automatically at our destructor (if you call AddScreenEffect
      and do not call RemoveScreenEffect on it),
      or at RemoveScreenEffect call.
      If you don't want this, use @link(TX3DNode.KeepExistingBegin)
      to manage the node destruction yourself.

      Note that the given Node should not be used by other TCastlScene instances.
      In general, a node should not be present in more than one TCastlScene instance,
      and we already insert the node into an internal TCastlScene instance.
      Use TX3DNode.DeepCopy if necessary to duplicate node into multiple scenes.

      Note that you can enable/disable the effect using @link(TScreenEffectNode.Enabled),
      or enable/disable all using @link(ScreenEffectsEnable).
      You do not need to remove the node by @link(RemoveScreenEffect) if you only want
      to disable it temporarily.
    }
    procedure AddScreenEffect(const Node: TAbstractChildNode);
    procedure RemoveScreenEffect(const Node: TAbstractChildNode);

    procedure BeforeRender; override;
    procedure Render; override;
    procedure RenderOverChildren; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    procedure GLContextClose; override;

    { Scale time passing inside TimeSensor nodes you add as part of AddScreenEffect.
      May be 0 to stop time passing.
      This has deliberately long name, instead of simple TimeScale,
      to make it clear that it's completely independent from
      @link(TCastleAbstractRootTransform.TimeScale). }
    property ScreenEffectsTimeScale: Single
      read FScreenEffectsTimeScale write FScreenEffectsTimeScale {$ifdef FPC}default 1{$endif};

    { Enable or disable all screen effects added by AddScreenEffect.
      When this is @true, particular screen effects can still be enabled/disabled
      using @link(TScreenEffectNode.Enabled).
      When this is @false, all effects are disabled, regardless of
      @link(TScreenEffectNode.Enabled). }
    property ScreenEffectsEnable: Boolean
      read FScreenEffectsEnable write FScreenEffectsEnable default true;

    { Make the screen effects rendering resources ready (e.g. link shaders). }
    procedure PrepareResources;
  end;

implementation

uses CastleUtils, CastleLog, CastleRenderContext, CastleInternalGLUtils;

function ScreenEffectVertex: string;
begin
  Result := {$I screen_effect.vs.inc};
end;

function ScreenEffectFragment(const Depth: boolean): string;
begin
  Result := '';
  if Depth then
    Result := Result + ('#define DEPTH' +NL);
  if GLFeatures.FBOMultiSampling then
  begin
    if GLFeatures.CurrentMultiSampling > 1 then
      Result := Result +
        '#define MULTI_SAMPLING' +NL +
        '#define MULTI_SAMPLING_' + IntToStr(GLFeatures.CurrentMultiSampling) +NL;
    if not (GLFeatures.CurrentMultiSampling in [1, 2, 4, 8, 16]) then
      WritelnWarning('Screen Effects', Format('Our GLSL library for screen effects is not prepared for your number of samples (anti-aliasing): %d. ' + 'This may indicate that your GPU is very new or very weird. Please submit this as a bug (see https://castle-engine.io/forum.php for links to forum, bug tracker and more), citing this message. For now, screen effects will not work.',
        [GLFeatures.CurrentMultiSampling]));
  end;
  Result := Result + ({$I screen_effect_library.glsl.inc} + NL);
end;

{ TGLSLScreenEffect ---------------------------------------------------------- }

constructor TGLSLScreenEffect.Create;
begin
  inherited;
  UniformMissing := umIgnore;
  Name := 'TGLSLScreenEffect';
end;

procedure TGLSLScreenEffect.Link;
var
  VS, FS: String;
begin
  if FScreenEffectShader = '' then
    raise Exception.Create('TGLSLScreenEffect shader not assigned by AttachScreenEffectShader method');
  {$warnings off} // using deprecated below, which should be internal
  VS := ScreenEffectVertex;
  FS := ScreenEffectFragment(NeedsDepth) + FScreenEffectShader;
  {$warnings on}
  AttachVertexShader(VS);
  AttachFragmentShader(FS);
  inherited;
end;

{ TCastleScreenEffects ------------------------------------------------------- }

constructor TCastleScreenEffects.Create(AOwner: TComponent);
begin
  inherited;
  FScreenEffectsTimeScale := 1;
  FScreenEffectsEnable := true;
end;

destructor TCastleScreenEffects.Destroy;
begin
  inherited;
end;

procedure TCastleScreenEffects.AddScreenEffect(const Node: TAbstractChildNode);
begin
  if ScreenEffectsScene = nil then
  begin
    ScreenEffectsRoot := TX3DRootNode.Create;

    { We create TCastleScene that is used only for rendering screen effects.
      This way we can make all "support" things for screen effects work,
      e.g. textures will be initialized using CastleInternalRenderer,
      TimeSensor will work and can be fed into shader uniform etc. }
    ScreenEffectsScene := TCastleScene.Create(Self);
    ScreenEffectsScene.Name := 'ScreenEffectsScene';
    ScreenEffectsScene.Load(ScreenEffectsRoot, true);
    ScreenEffectsScene.ProcessEvents := true;

    (* We setup World with camera knowledge to make some ProximitySensors working,
       like a dummy "ProximitySensors { size 10000 10000 10000 }". *)
    World := TCastleRootTransform.Create(Self);
    World.Add(ScreenEffectsScene);
    World.MainCamera := TCastleCamera.Create(Self);
    World.Add(World.MainCamera); // necessary to be able to query camera world coords
  end;

  { Note that AddChildren by default has AllowDuplicates=true,
    and enables multiple instances of this node on a list. That's good. }
  ScreenEffectsRoot.AddChildren(Node);

  { Send InternalCameraChanged now, to activate ProximitySensors inside new nodes. }
  ScreenEffectsScene.InternalCameraChanged;
end;

procedure TCastleScreenEffects.RemoveScreenEffect(const Node: TAbstractChildNode);
begin
  if ScreenEffectsRoot <> nil then
  begin
    ScreenEffectsRoot.FdChildren.Remove(Node);
    ScreenEffectsRoot.FdChildren.Changed;
  end;
end;

function TCastleScreenEffects.InternalExtraGetScreenEffects(const Index: Integer): TGLSLProgram;
begin
  raise EInternalError.Create('Override InternalExtraGetScreenEffects if you override InternalExtraScreenEffectsCount. In TCastleScreenEffects, InternalExtraScreenEffectsCount returns zero, so InternalExtraGetScreenEffects should never be called');
  Result := nil; // silence FPC warnings about undefined result
end;

function TCastleScreenEffects.InternalExtraScreenEffectsCount: Integer;
begin
  Result := 0;
end;

function TCastleScreenEffects.InternalExtraScreenEffectsNeedDepth: Boolean;
begin
  Result := false;
end;

function TCastleScreenEffects.GetScreenEffectsCount: Cardinal;
begin
  Result := 0;
  if ScreenEffectsEnable then
  begin
    if ScreenEffectsScene <> nil then
      { TCastleScene already implemented logic to only count screen effects
        that are enabled, and their GLSL code linked successfully.
        Cool, we depend on it. }
      Result := Result + ScreenEffectsScene.ScreenEffectsCount;
    Result := Result + InternalExtraScreenEffectsCount;
  end;
end;

function TCastleScreenEffects.GetScreenEffectsNeedDepth: Boolean;
begin
  Result := ScreenEffectsEnable and (
    ((ScreenEffectsScene <> nil) and ScreenEffectsScene.ScreenEffectsNeedDepth) or
    InternalExtraScreenEffectsNeedDepth
  );
end;

function TCastleScreenEffects.GetScreenEffect(const Index: Integer): TGLSLProgram;
var
  SceneEffects: Integer;
begin
  if ScreenEffectsScene <> nil then
    SceneEffects := ScreenEffectsScene.ScreenEffectsCount
  else
    SceneEffects := 0;

  if Index < SceneEffects then
    Result := ScreenEffectsScene.ScreenEffects(Index)
  else
    Result := InternalExtraGetScreenEffects(Index - SceneEffects);
end;

procedure TCastleScreenEffects.Render;
var
  SR: TRectangle;

  { Calculate
    RenderScreenEffects (whether we actually do anything about screen effects),
    CurrentScreenEffectsCount,
    CurrentScreenEffectsNeedDepth.
  }
  procedure CheckScreenEffects;
  begin
    { Check do we have any screen effects to show.

      Save GetScreenEffectsCount (and later GetScreenEffectsNeedDepth) result,
      to not recalculate it,
      and make the rest of code stable --- this way we know
      CurrentScreenEffectsCount and later CurrentScreenEffectsNeedDepth
      values are constant, even if user modifies FScreenEffects in the middle. }
    CurrentScreenEffectsCount := GetScreenEffectsCount;
    RenderScreenEffects := CurrentScreenEffectsCount <> 0;
    if not RenderScreenEffects then Exit; // no need to do anything else

    { all the checks below should pass on a modern GPU }
    SR := RenderRect.Round;
    RenderScreenEffects :=
      (not GLFeatures.EnableFixedFunction) and
      GLFeatures.UseMultiTexturing and
      { check IsTextureSized, to gracefully work (without screen effects)
        on old desktop OpenGL that does not support NPOT textures. }
      IsTextureSized(SR.Width, SR.Height, tsAny);
    if not RenderScreenEffects then Exit; // no need to do anything else

    { check the need to render depth buffer to a texture. }
    CurrentScreenEffectsNeedDepth := GetScreenEffectsNeedDepth;
    if CurrentScreenEffectsNeedDepth and not GLFeatures.TextureDepth then
    begin
      { We support only screen effects that do not require depth.
        TODO: It would be cleaner to still enable screen effects not using
        depth (and only them), instead of just disabling all screen effects. }
      RenderScreenEffects := false;
      Exit;
    end;
  end;

  { Create and setup new OpenGL texture for screen effects.
    Depends on ScreenEffectTextureWidth, ScreenEffectTextureHeight being set. }
  function CreateScreenEffectTexture(const Depth: boolean): TGLuint;

    { Create new OpenGL texture for screen effect.
      Calls glTexImage2D or glTexImage2DMultisample
      (depending on multi-sampling requirements).

      - image contents are always unallocated (pixels = nil for glTexImage2D).
        For screen effects, we never need to load initial image contents,
        and we also do not have to care about pixel packing.
      - level for mipmaps is always 0
      - border is always 0
      - image target is ScreenEffectTextureTarget
      - size is ScreenEffectTextureWidth/Height }
    procedure TexImage2D(const InternalFormat: TGLint;
      const Format, AType: TGLenum);
    begin
      {$ifndef OpenGLES}
      if (GLFeatures.CurrentMultiSampling > 1) and GLFeatures.FBOMultiSampling then
        glTexImage2DMultisample(ScreenEffectTextureTarget,
          GLFeatures.CurrentMultiSampling, InternalFormat,
          ScreenEffectTextureWidth,
          ScreenEffectTextureHeight,
          { fixedsamplelocations = TRUE are necessary in case we use
            this with cbColor mode, where FBO will also have renderbuffer
            for depth (and maybe stencil). In this case,
            https://www.opengl.org/registry/specs/ARB/texture_multisample.txt
            says that

              if the attached images are a mix of
              renderbuffers and textures, the value of
              TEXTURE_FIXED_SAMPLE_LOCATIONS must be TRUE for all attached
              textures.

            which implies that this parameter must be true.
            See https://sourceforge.net/p/castle-engine/tickets/22/ . }
          GL_TRUE) else
      {$endif}
        glTexImage2D(ScreenEffectTextureTarget, 0, InternalFormat,
          ScreenEffectTextureWidth,
          ScreenEffectTextureHeight, 0, Format, AType, nil);
    end;

  begin
    glGenTextures(1, @Result);
    glBindTexture(ScreenEffectTextureTarget, Result);
    {$ifndef OpenGLES}
    { for multisample texture, these cannot be configured (OpenGL makes
      "invalid enumerant" error) }
    if ScreenEffectTextureTarget <> GL_TEXTURE_2D_MULTISAMPLE then
    {$endif}
    begin
      { TODO: NEAREST or LINEAR?
        Allow to config this at each screen effect,
        and optionally create another version? }
      SetTextureFilter(ScreenEffectTextureTarget, TextureFilter(minNearest, magNearest));
      glTexParameteri(ScreenEffectTextureTarget, GL_TEXTURE_WRAP_S, GLFeatures.CLAMP_TO_EDGE);
      glTexParameteri(ScreenEffectTextureTarget, GL_TEXTURE_WRAP_T, GLFeatures.CLAMP_TO_EDGE);
    end;
    if Depth then
    begin
      {$ifndef OpenGLES}
      // TODO-es What do we use here? See TGLRenderToTexture TODO at similar place
      if GLFeatures.ShadowVolumesPossible and GLFeatures.PackedDepthStencil then
        TexImage2D(GL_DEPTH24_STENCIL8_EXT, GL_DEPTH_STENCIL_EXT, GL_UNSIGNED_INT_24_8_EXT) else
      {$endif}
        TexImage2D(GL_DEPTH_COMPONENT, GL_DEPTH_COMPONENT,
          { On OpenGLES, using GL_UNSIGNED_BYTE will result in FBO failing
            with INCOMPLETE_ATTACHMENT.
            http://www.khronos.org/registry/gles/extensions/OES/OES_depth_texture.txt
            allows only GL_UNSIGNED_SHORT or GL_UNSIGNED_INT for depth textures. }
          {$ifdef OpenGLES} GL_UNSIGNED_SHORT {$else} GL_UNSIGNED_BYTE {$endif});
      //glTexParameteri(ScreenEffectTextureTarget, GL_TEXTURE_COMPARE_MODE_ARB, GL_NONE);
      //glTexParameteri(ScreenEffectTextureTarget, GL_DEPTH_TEXTURE_MODE_ARB, GL_LUMINANCE);
    end else
      TexImage2D({$ifdef OpenGLES} GL_RGB {$else} GL_RGB8 {$endif},
        GL_RGB, GL_UNSIGNED_BYTE);

    TextureMemoryProfiler.Allocate(Result, 'screen-contents', '', { TODO } 0, false,
      ScreenEffectTextureWidth, ScreenEffectTextureHeight, 1);
  end;

  procedure BeginRenderingToTexture;
  begin
    { We need a temporary texture, for screen effect. }
    if (ScreenEffectTextureDest = 0) or
       (ScreenEffectTextureSrc = 0) or
       (CurrentScreenEffectsNeedDepth <> (ScreenEffectTextureDepth <> 0)) or
       (ScreenEffectRTT = nil) or
       (ScreenEffectTextureWidth  <> SR.Width ) or
       (ScreenEffectTextureHeight <> SR.Height) then
    begin
      glFreeTexture(ScreenEffectTextureDest);
      glFreeTexture(ScreenEffectTextureSrc);
      glFreeTexture(ScreenEffectTextureDepth);
      FreeAndNil(ScreenEffectRTT);

      {$ifndef OpenGLES}
      if (GLFeatures.CurrentMultiSampling > 1) and GLFeatures.FBOMultiSampling then
        ScreenEffectTextureTarget := GL_TEXTURE_2D_MULTISAMPLE else
      {$endif}
        ScreenEffectTextureTarget := GL_TEXTURE_2D;

      ScreenEffectTextureWidth  := SR.Width;
      ScreenEffectTextureHeight := SR.Height;
      { We use two textures: ScreenEffectTextureDest is the destination
        of framebuffer, ScreenEffectTextureSrc is the source to render.

        Although for some effects one texture (both src and dest) is enough.
        But when you have > 1 effect and one of the effects has non-local
        operations (they read color values that can be modified by operations
        of the same shader, so it's undefined (depends on how shaders are
        executed in parallel) which one is first) then the artifacts are
        visible. For example, use view3dscene "Edge Detect" effect +
        any other effect. }
      ScreenEffectTextureDest := CreateScreenEffectTexture(false);
      ScreenEffectTextureSrc := CreateScreenEffectTexture(false);
      if CurrentScreenEffectsNeedDepth then
        ScreenEffectTextureDepth := CreateScreenEffectTexture(true);

      { create new TGLRenderToTexture (usually, framebuffer object) }
      ScreenEffectRTT := TGLRenderToTexture.Create(
        ScreenEffectTextureWidth, ScreenEffectTextureHeight);
      ScreenEffectRTT.SetTexture(ScreenEffectTextureDest, ScreenEffectTextureTarget);
      ScreenEffectRTT.CompleteTextureTarget := ScreenEffectTextureTarget;
      { use the same multi-sampling strategy as container }
      ScreenEffectRTT.MultiSampling := GLFeatures.CurrentMultiSampling;
      if CurrentScreenEffectsNeedDepth then
      begin
        ScreenEffectRTT.Buffer := tbColorAndDepth;
        ScreenEffectRTT.DepthTexture := ScreenEffectTextureDepth;
        ScreenEffectRTT.DepthTextureTarget := ScreenEffectTextureTarget;
      end else
        ScreenEffectRTT.Buffer := tbColor;
      ScreenEffectRTT.Stencil := GLFeatures.ShadowVolumesPossible;
      ScreenEffectRTT.GLContextOpen;

      WritelnLog('Screen effects', Format('Created texture for screen effects, with size %d x %d, with depth texture: %s',
        [ ScreenEffectTextureWidth,
          ScreenEffectTextureHeight,
          BoolToStr(CurrentScreenEffectsNeedDepth, true) ]));
    end;

    RenderContext.Viewport := SR;
    { Any subquequent changes to RenderContext.Viewport should by shifted,
      such that point SR.LeftBottom is actually at (0,0). }
    RenderContext.ViewportDelta := -SR.LeftBottom;
    { Note: the effective glViewport is now Rectangle(0, 0, SR.Width, SR.Height). }

    ScreenEffectRTT.RenderBegin;
    ScreenEffectRTT.SetTexture(ScreenEffectTextureDest, ScreenEffectTextureTarget);
  end;

begin
  inherited;
  CheckScreenEffects;
  if RenderScreenEffects then
    BeginRenderingToTexture;
  RenderWithoutScreenEffects;
end;

procedure TCastleScreenEffects.RenderWithoutScreenEffects;
begin
end;

procedure TCastleScreenEffects.RenderOverChildren;
var
  SR: TRectangle;

  procedure RenderWithScreenEffectsCore;

    procedure RenderOneEffect(Shader: TGLSLProgram);
    var
      BoundTextureUnits: Cardinal;
      AttribVertex, AttribTexCoord: TGLSLAttribute;
    begin
      if ScreenPointVbo = 0 then
      begin
        { generate and fill ScreenPointVbo. It's contents are constant. }
        glGenBuffers(1, @ScreenPointVbo);
        ScreenPoint[0].TexCoord := Vector2(0, 0);
        ScreenPoint[0].Position := Vector2(-1, -1);
        ScreenPoint[1].TexCoord := Vector2(1, 0);
        ScreenPoint[1].Position := Vector2( 1, -1);
        ScreenPoint[2].TexCoord := Vector2(1, 1);
        ScreenPoint[2].Position := Vector2( 1,  1);
        ScreenPoint[3].TexCoord := Vector2(0, 1);
        ScreenPoint[3].Position := Vector2(-1,  1);
        glBindBuffer(GL_ARRAY_BUFFER, ScreenPointVbo);
        glBufferData(GL_ARRAY_BUFFER, SizeOf(ScreenPoint), @(ScreenPoint[0]), GL_STATIC_DRAW);
      end;

      if ScreenPointVao = nil then
        ScreenPointVao := TVertexArrayObject.Create(nil);

      RenderContext.CurrentProgram := Shader;
      RenderContext.CurrentVao := ScreenPointVao;

      glBindBuffer(GL_ARRAY_BUFFER, ScreenPointVbo);

      glActiveTexture(GL_TEXTURE0); // GLFeatures.UseMultiTexturing is already checked
      glBindTexture(ScreenEffectTextureTarget, ScreenEffectTextureSrc);
      BoundTextureUnits := 1;

      if CurrentScreenEffectsNeedDepth then
      begin
        glActiveTexture(GL_TEXTURE1);
        glBindTexture(ScreenEffectTextureTarget, ScreenEffectTextureDepth);
        Inc(BoundTextureUnits);
      end;

      Shader.Uniform('screen').SetValue(0);
      if CurrentScreenEffectsNeedDepth then
        Shader.Uniform('screen_depth').SetValue(1);
      Shader.Uniform('screen_width').SetValue(TGLint(ScreenEffectTextureWidth));
      Shader.Uniform('screen_height').SetValue(TGLint(ScreenEffectTextureHeight));

      { Note that we ignore SetupUniforms result --- if some texture
        could not be bound, it will be undefined for shader.
        I don't see anything much better to do now. }
      Shader.SetupUniforms(BoundTextureUnits);

      { Note that there's no need to worry about Rect.Left or Rect.Bottom,
        here or inside RenderWithScreenEffectsCore, because we're already within
        RenderContext.Viewport that takes care of this. }

      AttribVertex := Shader.Attribute('vertex');
      AttribVertex.EnableArrayVector2(ScreenPointVao, SizeOf(TScreenPoint),
        OffsetUInt(ScreenPoint[0].Position, ScreenPoint[0]));
      AttribTexCoord := Shader.Attribute('tex_coord');
      AttribTexCoord.EnableArrayVector2(ScreenPointVao, SizeOf(TScreenPoint),
        OffsetUInt(ScreenPoint[0].TexCoord, ScreenPoint[0]));

      glDrawArrays(GL_TRIANGLE_FAN, 0, 4);

      AttribVertex.DisableArray;
      AttribTexCoord.DisableArray;
      glBindBuffer(GL_ARRAY_BUFFER, 0);
    end;

  var
    I: Integer;
  begin
    { Render all except the last screen effects: from texture
      (ScreenEffectTextureDest/Src) and to texture (using ScreenEffectRTT) }

    { The effective glViewport should be now Rectangle(0, 0, SR.Width, SR.Height).
      Although our "Render" implementation already set it
      (somewhat indirectly, using ViewportDelta),
      but something since then could have changed the Viewport
      (e.g. ControlRenderBegin done right before C.RenderOverChildren in CastleGLContainer).
      So set it as we need, again.
      And this time we don't need to deal with ViewportDelta, just zero it. }
    RenderContext.ViewportDelta := TVector2Integer.Zero;
    RenderContext.Viewport := Rectangle(0, 0, SR.Width, SR.Height);

    for I := 0 to CurrentScreenEffectsCount - 2 do
    begin
      ScreenEffectRTT.RenderBegin;
      ScreenEffectRTT.SetTexture(ScreenEffectTextureDest, ScreenEffectTextureTarget);
      RenderOneEffect(GetScreenEffect(I));
      ScreenEffectRTT.RenderEnd;

      SwapValues(ScreenEffectTextureDest, ScreenEffectTextureSrc);
    end;

    { the last effect gets a texture, and renders straight into screen }
    RenderContext.ViewportDelta := TVector2Integer.Zero;
    RenderContext.Viewport := RenderRect.Round;
    RenderOneEffect(GetScreenEffect(CurrentScreenEffectsCount - 1));
  end;

  procedure EndRenderingToTexture;
  begin
    SR := RenderRect.Round;

    ScreenEffectRTT.RenderEnd;

    SwapValues(ScreenEffectTextureDest, ScreenEffectTextureSrc);

    OrthoProjection(FloatRectangle(0, 0, SR.Width, SR.Height));
    RenderWithScreenEffectsCore;
  end;

begin
  inherited;
  if RenderScreenEffects then
    EndRenderingToTexture;
end;

procedure TCastleScreenEffects.Update(const SecondsPassed: Single; var HandleInput: boolean);
var
  RemoveItem: TRemoveType;
begin
  inherited;
  if (ScreenEffectsScene <> nil) and ScreenEffectsEnable then
  begin
    RemoveItem := rtNone;
    ScreenEffectsScene.Update(SecondsPassed * ScreenEffectsTimeScale, RemoveItem);
    { We ignore RemoveItem --- ScreenEffectsScene cannot be removed.
      Also, this is always TCastleScene that should not change RemoveItem ever. }
  end;
end;

procedure TCastleScreenEffects.PrepareResources;
begin
  if ScreenEffectsScene <> nil then
    { We depend here on undocumented TCastleScene.PrepareResources behavior:
      when there is no prRenderSelf or prRenderClones,
      then PrepareParams (3rd argument below) is unused, and may be nil. }
    ScreenEffectsScene.PrepareResources([prScreenEffects], nil);
end;

procedure TCastleScreenEffects.BeforeRender;
begin
  inherited;
  PrepareResources;
end;

procedure TCastleScreenEffects.GLContextClose;
begin
  glFreeTexture(ScreenEffectTextureDest);
  glFreeTexture(ScreenEffectTextureSrc);
  glFreeTexture(ScreenEffectTextureDepth);
  ScreenEffectTextureTarget := 0; //< clear, for safety
  FreeAndNil(ScreenEffectRTT);
  glFreeBuffer(ScreenPointVbo);
  FreeAndNil(ScreenPointVao);
  inherited;
end;

end.
