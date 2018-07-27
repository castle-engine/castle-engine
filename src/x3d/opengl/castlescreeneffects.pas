{
  Copyright 2010-2018 Michalis Kamburelis.

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
  {$ifdef CASTLE_OBJFPC} CastleGL, {$else} GL, GLExt, {$endif}
  CastleVectors, CastleGLShaders, CastleUIControls, X3DNodes, CastleGLImages,
  CastleRectangles;

{ Standard GLSL vertex shader for screen effect.
  @bold(In your own programs, it's usually easier to use TGLSLScreenEffect,
  and then this function is not necessary). }
function ScreenEffectVertex: string;

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
    { uaIgnore is a good idea here, in case some uniform variable
      from ScreenEffectFragment code may be left unused. }
    Shader.UniformNotFoundAction := uaIgnore;
  #)

*)
function ScreenEffectFragment(const Depth: boolean): string;

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

    { In this class, UniformNotFoundAction is by default uaIgnore, since it's
      normal that screen effect doesn't use some of it's uniform variables. }
    property UniformNotFoundAction default uaIgnore;

    { Attach GLSL code for the screen effect (executed as part of fragment shader).
      See https://castle-engine.io/x3d_extensions_screen_effects.php . }
    property ScreenEffectShader: string read FScreenEffectShader write FScreenEffectShader;

    procedure Link; override;
  end;

  { Control that applies shader screen effects (post-processing)
    on the rendering done by children and (when this class is used as an ancestor)
    it's descendants.

    It can be used to apply screen effects over any UI control,
    like @link(TCastleSceneManager), @link(TCastleViewport),
    @link(TCastleButton), @link(TCastleImageControl) and so on.
    Simply place the desired control as child of this control.

    To make it easier to apply effects on
    @link(TCastleSceneManager) and @link(TCastleViewport),
    they already descend from this class. So, while you can, you don't need to wrap
    @link(TCastleSceneManager) instance inside another @link(TCastleScreenEffects)
    instance. You can instead directly call @link(AddScreenEffect) on your
    @link(TCastleSceneManager) instance.
  }
  TCastleScreenEffects = class(TUIControlSizeable)
  strict private
    type
      TScreenPoint = packed record
        Position: TVector2;
        TexCoord: TVector2;
      end;
    var
      FScreenEffects: TGroupNode;

      { Valid only between Render and RenderOverChildren calls. }
      RenderScreenEffects: Boolean;
      OldViewport: TRectangle;

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
      ScreenPoint: packed array [0..3] of TScreenPoint;

    function GetScreenEffectsCount: Cardinal;
    function GetScreenEffectsNeedDepth: Boolean;
    function GetScreenEffect(const Index: Integer): TGLSLProgram;
    { Prepare Node.Shader. }
    procedure PrepareScreenEffect(const Node: TScreenEffectNode);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Add or remove a ScreenEffect node that defines new shader screen effect.
      See https://castle-engine.io/x3d_extensions_screen_effects.php .

      The memory management of the node added here is automatic:
      the added screen effect becomes owned by the internal X3D Group node.
      It's reference-count is increased at AddScreenEffect, and decreased
      at RemoveScreenEffect, and it is automatically freed when reference-count
      drops to zero.

      This means that, if you create the TScreenEffectNode by code,
      and don't add it anywhere else, then the node will be freed
      automatically at our destructor (if you call AddScreenEffect
      and do not call RemoveScreenEffect on it),
      or at RemoveScreenEffect call.

      In general, the node may be shared by some scene
      (TCastleScene with TX3DRootNode),
      it may also be shared by a couple of TCastleScreenEffects instances,
      and it will be correctly freed once.
      If you don't want this, use e.g. @link(TX3DNode.KeepExistingBegin)
      to manage the node free yourself.
    }
    procedure AddScreenEffect(const Node: TScreenEffectNode);
    procedure RemoveScreenEffect(const Node: TScreenEffectNode);

    procedure Render; override;
    procedure RenderOverChildren; override;
  end;

implementation

uses CastleUtils, CastleGLUtils, CastleLog;

function ScreenEffectVertex: string;
begin
  Result := {$I screen_effect.vs.inc};
end;

function ScreenEffectFragment(const Depth: boolean): string;
begin
  Result := '';
  if Depth then
    Result += '#define DEPTH' +NL;
  if GLFeatures.FBOMultiSampling then
  begin
    if GLFeatures.CurrentMultiSampling > 1 then
      Result +=
        '#define MULTI_SAMPLING' +NL +
        '#define MULTI_SAMPLING_' + IntToStr(GLFeatures.CurrentMultiSampling) +NL;
    if not (GLFeatures.CurrentMultiSampling in [1, 2, 4, 8, 16]) then
      WritelnWarning('Screen Effects', Format('Our GLSL library for screen effects is not prepared for your number of samples (anti-aliasing): %d. This may indicate that your GPU is very new or very weird. Please submit this as a bug (see https://castle-engine.io/forum.php for links to forum, bug tracker and more), citing this message. For now, screen effects will not work.',
        [GLFeatures.CurrentMultiSampling]));
  end;
  Result += {$I screen_effect_library.glsl.inc} + NL;
end;

{ TGLSLScreenEffect ---------------------------------------------------------- }

constructor TGLSLScreenEffect.Create;
begin
  inherited;
  UniformNotFoundAction := uaIgnore;
end;

procedure TGLSLScreenEffect.Link;
begin
  if FScreenEffectShader = '' then
    raise Exception.Create('TGLSLScreenEffect shader not assigned by AttachScreenEffectShader method');
  AttachVertexShader(ScreenEffectVertex);
  AttachFragmentShader(ScreenEffectFragment(NeedsDepth) + FScreenEffectShader);
  inherited;
end;

{ TCastleScreenEffects ------------------------------------------------------- }

constructor TCastleScreenEffects.Create(AOwner: TComponent);
begin
  inherited;
  FScreenEffects := TGroupNode.Create;
end;

destructor TCastleScreenEffects.Destroy;
begin
  FreeAndNil(FScreenEffects);
  inherited;
end;

procedure TCastleScreenEffects.AddScreenEffect(const Node: TScreenEffectNode);
begin
  { Do not add using AddChildren, use FdChildren.Add, to enable multiple
    instances of this node on a list. }
  FScreenEffects.FdChildren.Add(Node);
end;

procedure TCastleScreenEffects.RemoveScreenEffect(const Node: TScreenEffectNode);
begin
  FScreenEffects.FdChildren.Remove(Node);
end;

function TCastleScreenEffects.GetScreenEffectsCount: Cardinal;
begin
  // TODO: only enabled count (with Shader <> nil and Enabled, and call PrepareScreenEffect earlier)
  Result := FScreenEffects.FdChildren.Count;
end;

function TCastleScreenEffects.GetScreenEffectsNeedDepth: Boolean;
var
  SE: TScreenEffectNode;
  I: Integer;
begin
  Result := false;
  for I := 0 to FScreenEffects.FdChildren.Count - 1 do
  begin
    SE := FScreenEffects.FdChildren.Items[I] as TScreenEffectNode;
    if SE.Enabled and SE.NeedsDepth then
      Result := true;
  end;
end;

function TCastleScreenEffects.GetScreenEffect(const Index: Integer): TGLSLProgram;
var
  Node: TScreenEffectNode;
begin
  // TODO: get ith enabled (with Shader <> nil and Enabled, and call PrepareScreenEffect earlier)
  Node := FScreenEffects.FdChildren.Items[Index] as TScreenEffectNode;
  PrepareScreenEffect(Node);
  Result := Node.Shader as TGLSLProgram;
  Assert(Result <> nil); // TODO this can happen for now
end;

procedure TCastleScreenEffects.PrepareScreenEffect(const Node: TScreenEffectNode);
var
  ShaderProgram: TGLSLScreenEffect;
  ScreenEffectShader: string;
  ComposedShader: TComposedShaderNode;
  Part: TShaderPartNode;
  I: Integer;
begin
  if not Node.ShaderLoaded then
  begin
    Assert(Node.Shader = nil);
    Node.ShaderLoaded := true;
    if Node.FdEnabled.Value then
    begin
      { make sure that textures inside shaders are prepared }
      // TODO: This doesn't initialize textures for program.
      // PrepareIDeclsList(Node.FdShaders.Items, Node.StateForShaderPrepare);

      // TODO: This uses Node.Shader as TGLSLScreenEffect,
      // contrary to renderer using for TShader. This is dirty, and may cause
      // problems when this is reused.

      { calculate ComposedShader, send event isSelected }
      for I := 0 to Node.FdShaders.Count - 1 do
      begin
        ComposedShader := Node.FdShaders.GLSLShader(I);
        if ComposedShader <> nil then
        begin
          ComposedShader.EventIsSelected.Send(true);
          Break;
        end else
        if Node.FdShaders[I] is TAbstractShaderNode then
          TAbstractShaderNode(Node.FdShaders[I]).EventIsSelected.Send(false);
      end;

      if ComposedShader <> nil then
      begin
        { calculate ScreenEffectShader }
        ScreenEffectShader := '';
        for I := 0 to ComposedShader.FdParts.Count - 1 do
        begin
          if ComposedShader.FdParts[I] is TShaderPartNode then
          begin
            Part := TShaderPartNode(ComposedShader.FdParts[I]);
            ScreenEffectShader := ScreenEffectShader + NL + Part.Contents;
          end;
        end;

        // TODO: nothing frees it now.
        ShaderProgram := TGLSLScreenEffect.Create;
        ShaderProgram.ScreenEffectShader := ScreenEffectShader;
        ShaderProgram.NeedsDepth := Node.NeedsDepth;

        try
          ShaderProgram.Link;
        except on E: EGLSLError do
          begin
            FreeAndNil(ShaderProgram);
            WritelnWarning('VRML/X3D', Format('Cannot use GLSL shader for ScreenEffect: %s',
              [E.Message]));
          end;
        end;

        Node.Shader := ShaderProgram;
      end;
    end;
  end;
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
    SR := ScreenRect;
    RenderScreenEffects :=
      GLFeatures.VertexBufferObject { for screen quad } and
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
      { TODO: NEAREST or LINEAR? Allow to config this and eventually change
        before each screen effect? }
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

      if Log then
        WritelnLog('Screen effects', Format('Created texture for screen effects, with size %d x %d, with depth texture: %s',
          [ ScreenEffectTextureWidth,
            ScreenEffectTextureHeight,
            BoolToStr(CurrentScreenEffectsNeedDepth, true) ]));
    end;

    { We have to adjust RenderContext.Viewport.
      It will be restored from RenderOverChildren right before actually
      rendering to screen. }
    OldViewport := RenderContext.Viewport;
    RenderContext.Viewport := Rectangle(0, 0, SR.Width, SR.Height);

    ScreenEffectRTT.RenderBegin;
    ScreenEffectRTT.SetTexture(ScreenEffectTextureDest, ScreenEffectTextureTarget);
  end;

begin
  inherited;
  CheckScreenEffects;
  if RenderScreenEffects then
    BeginRenderingToTexture;
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

      TGLSLProgram.Current := Shader;
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
      AttribVertex.EnableArrayVector2(SizeOf(TScreenPoint),
        OffsetUInt(ScreenPoint[0].Position, ScreenPoint[0]));
      AttribTexCoord := Shader.Attribute('tex_coord');
      AttribTexCoord.EnableArrayVector2(SizeOf(TScreenPoint),
        OffsetUInt(ScreenPoint[0].TexCoord, ScreenPoint[0]));

      glDrawArrays(GL_TRIANGLE_FAN, 0, 4);

      AttribVertex.DisableArray;
      AttribTexCoord.DisableArray;
      glBindBuffer(GL_ARRAY_BUFFER, 0);
      glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
    end;

  var
    I: Integer;
  begin
    { Render all except the last screen effects: from texture
      (ScreenEffectTextureDest/Src) and to texture (using ScreenEffectRTT) }
    for I := 0 to CurrentScreenEffectsCount - 2 do
    begin
      ScreenEffectRTT.RenderBegin;
      ScreenEffectRTT.SetTexture(ScreenEffectTextureDest, ScreenEffectTextureTarget);
      RenderOneEffect(GetScreenEffect(I));
      ScreenEffectRTT.RenderEnd;

      SwapValues(ScreenEffectTextureDest, ScreenEffectTextureSrc);
    end;

    { Restore RenderContext.Viewport }
    RenderContext.Viewport := OldViewport;

    { the last effect gets a texture, and renders straight into screen }
    RenderOneEffect(GetScreenEffect(CurrentScreenEffectsCount - 1));
  end;

  procedure EndRenderingToTexture;
  begin
    SR := ScreenRect;

    ScreenEffectRTT.RenderEnd;

    SwapValues(ScreenEffectTextureDest, ScreenEffectTextureSrc);

    if GLFeatures.EnableFixedFunction then
    begin
      {$ifndef OpenGLES}
      glPushAttrib(GL_ENABLE_BIT);
      glDisable(GL_LIGHTING);
      glDisable(GL_DEPTH_TEST);

      glActiveTexture(GL_TEXTURE0);
      glDisable(GL_TEXTURE_2D);
      if ScreenEffectTextureTarget <> GL_TEXTURE_2D_MULTISAMPLE then
        glEnable(ScreenEffectTextureTarget);

      if CurrentScreenEffectsNeedDepth then
      begin
        glActiveTexture(GL_TEXTURE1);
        glDisable(GL_TEXTURE_2D);
        if ScreenEffectTextureTarget <> GL_TEXTURE_2D_MULTISAMPLE then
          glEnable(ScreenEffectTextureTarget);
      end;
      {$endif}
    end;

    OrthoProjection(FloatRectangle(0, 0, SR.Width, SR.Height));
    RenderWithScreenEffectsCore;

    if GLFeatures.EnableFixedFunction then
    begin
      {$ifndef OpenGLES}
      if CurrentScreenEffectsNeedDepth then
      begin
        glActiveTexture(GL_TEXTURE1);
        if ScreenEffectTextureTarget <> GL_TEXTURE_2D_MULTISAMPLE then
          glDisable(ScreenEffectTextureTarget); // TODO: should be done by glPopAttrib, right? enable_bit contains it?
      end;

      glActiveTexture(GL_TEXTURE0);
      if ScreenEffectTextureTarget <> GL_TEXTURE_2D_MULTISAMPLE then
        glDisable(ScreenEffectTextureTarget); // TODO: should be done by glPopAttrib, right? enable_bit contains it?

      { at the end, we left active texture as default GL_TEXTURE0 }

      glPopAttrib;
      {$endif}
    end;
  end;

begin
  inherited;
  if RenderScreenEffects then
    EndRenderingToTexture;
end;

end.
