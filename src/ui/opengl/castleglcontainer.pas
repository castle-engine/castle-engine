{
  Copyright 2009-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Container for 2D controls able to render using OpenGL (TGLContainer). }
unit CastleGLContainer;

{$I castleconf.inc}

interface

uses Classes,
  CastleUIControls, CastleGLUtils, CastleRectangles, CastleColors,
  CastleImages;

type
  { Container for controls providing an OpenGL rendering.
    This class is internally used by TCastleWindowCustom and TCastleControlCustom.
    It is not useful from the outside, unless you want to implement
    your own container provider similar to TCastleWindowCustom / TCastleControlCustom. }
  TGLContainer = class abstract(TUIContainer)
  strict private
    FContext: TRenderContext;
    procedure RenderControlCore(const C: TUIControl;
      const ViewportRect: TRectangle;
      var SomeControlHasRenderStyle2D: boolean;
      const FilterRenderStyle: TRenderStyle);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Context: TRenderContext read FContext;
    procedure EventRender; override;
    procedure EventClose(const OpenWindowsCount: Cardinal); override;

    { Render a TUIControl (along with all it's children).

      This method can be used to render UI control into an image,
      @link(TGLImage), when it is surrounded by
      @link(TGLImage.RenderToImageBegin)
      and @link(TGLImage.RenderToImageEnd).
      See example ../../../examples/3d_rendering_processing/render_3d_to_image.lpr.

      It can also be used with more low-level @link(TGLRenderToTexture).
      See example ../../../examples/3d_rendering_processing/render_3d_to_texture_and_use_as_quad.lpr.

      This is a good method to render the UI control off-screen.
      It can render any UI control, including e.g. TCastleSceneManager
      with 3D stuff inside TCastleScene.

      The contents of the @link(Controls) list doesn't matter for this method.
      In particular, it doesn't matter if the Control (given as a parameter)
      is present on the list of current @link(Controls).
      This method explicitly renders the given Control parameter (and it's children),
      nothing more, nothing less.

      More details what this method does:

      @unorderedList(
        @item(Temporarily sets
          @link(TInputListener.Container Control.Container), if needed.)

        @item(Makes sure OpenGL resources of the control are initialized.
          If needed, it calls
          @link(TUIControl.GLContextOpen Control.GLContextOpen) and
          @link(TUIControl.GLContextClose Control.GLContextClose)
          around.
          This is needed when you want to perform off-screen rendering,
          but the control's OpenGL resources are not initialized yet,
          e.g. because it is not present on the @link(Controls) list.

          Note that doing this repeatedly may be a slowdown
          (how much, it depends on the actual TUIControl
          -- some controls do nothing in TUIControl.GLContextOpen,
          some controls do a lot).
          If you want to repeatedly call @link(RenderControl) on the
          same Control, it is more efficient
          to first explicitly create it's OpenGL resources,
          e.g. by calling
          @link(TUIControl.GLContextOpen Control.GLContextOpen) explicitly.
          Or adding the control to the @link(Controls) list.
        )

        @item(Calls @link(TInputListener.Resize Control.Resize),
          required by some controls (like scene manager) to know viewport size.)

        @item(Calls @link(TUIControl.BeforeRender Control.BeforeRender),
          required by some controls (like scene manager)
          to prepare resources (like generated textures,
          important for mirrors for screenshots in batch mode).)
      )
    }
    procedure RenderControl(const Control: TUIControl;
      const ViewportRect: TRectangle);

    { Save screen by rendering the window contents to the back buffer. }
    function SaveScreen(const SaveRect: TRectangle): TRGBImage; override;
  end;

{ Render control contents to an RGBA image, using off-screen rendering.
  The background behind the control is filled with BackgroundColor
  (which may be transparent, e.g. with alpha = 0).

  The rendering is done using off-screen FBO.
  Which means that you can request any size, you are @bold(not) limited
  to your current window / control size.

  Make sure that the control is nicely positioned to fill the ViewportRect.
  Usually you want to adjust control size and position,
  and disable UI scaling (set TUIControl.EnableUIScaling = @false
  if you use TUIContainer.UIScaling).

  This is the @italic(easiest) way to make off-screen rendering,
  i.e. to render 3D things (like TCastleScene or TCastleSceneManager)
  into an image. This is @italic(not the fastest way), as it creates
  new TGLRenderToTexture instance each time,
  and it grabs the image contents to CPU.
  If you want a faster approach, use @link(TGLContainer.RenderControl)
  and render into @link(TGLImage) using @link(TGLImage.RenderToImageBegin)
  and @link(TGLImage.RenderToImageEnd).
}
function RenderControlToImage(const Container: TGLContainer;
  const Control: TUIControl;
  const ViewportRect: TRectangle;
  const BackgroundColor: TCastleColor): TRGBAlphaImage;

implementation

uses SysUtils,
  {$ifdef CASTLE_OBJFPC} CastleGL, {$else} GL, GLExt, {$endif}
  CastleVectors, CastleGLImages;

procedure ControlRenderBegin(const ViewportRect: TRectangle);
begin
  if GLFeatures.EnableFixedFunction then
  begin
    { Set state that is guaranteed for Render2D calls,
      but TUIControl.Render cannot change it carelessly. }
    {$ifndef OpenGLES}
    glDisable(GL_LIGHTING);
    glDisable(GL_FOG);
    {$endif}
    GLEnableTexture(CastleGLUtils.etNone);
  end;

  glDisable(GL_DEPTH_TEST);

  RenderContext.Viewport := ViewportRect;
  OrthoProjection(FloatRectangle(0, 0, ViewportRect.Width, ViewportRect.Height));

  if GLFeatures.EnableFixedFunction then
  begin
    { Set OpenGL state that may be changed carelessly, and has some
      guaranteed value, for Render2d calls. }
    {$ifndef OpenGLES} glLoadIdentity; {$endif}
    {$warnings off}
    CastleGLUtils.WindowPos := Vector2Integer(0, 0);
    {$warnings on}
  end;
end;

{ TGLContainer --------------------------------------------------------------- }

constructor TGLContainer.Create(AOwner: TComponent);
begin
  inherited;
  FContext := TRenderContext.Create;
end;

destructor TGLContainer.Destroy;
begin
  if RenderContext = FContext then
    RenderContext := nil;
  FreeAndNil(FContext);
  inherited;
end;

procedure TGLContainer.RenderControlCore(const C: TUIControl;
  const ViewportRect: TRectangle;
  var SomeControlHasRenderStyle2D: boolean;
  const FilterRenderStyle: TRenderStyle);
var
  I: Integer;
begin
  if C.GetExists then
  begin
    { We check C.GLInitialized, because it may happen that a control
      did not receive GLContextOpen yet, in case we cause some rendering
      during TUIContainer.EventOpen (e.g. because some TUIControl.GLContextOpen
      calls Window.Screenshot, so everything is rendered
      before even the rest of controls received TUIControl.GLContextOpen).
      See castle_game_engine/tests/testcontainer.pas . }

    if C.GLInitialized then
    begin
      {$warnings off} // knowingly looking at deprecated RenderStyle, to keep it working
      if C.RenderStyle = FilterRenderStyle then
      {$warnings on}
      begin
        ControlRenderBegin(Rect);
        C.Render;
      end;
      {$warnings off} // knowingly looking at deprecated RenderStyle, to keep it working
      if C.RenderStyle = rs2D then
      {$warnings on}
        SomeControlHasRenderStyle2D := true;
    end;

    for I := 0 to C.ControlsCount - 1 do
      RenderControlCore(C.Controls[I], ViewportRect,
        SomeControlHasRenderStyle2D, FilterRenderStyle);

    {$warnings off} // knowingly looking at deprecated RenderStyle, to keep it working
    if C.GLInitialized and (C.RenderStyle = FilterRenderStyle) then
    {$warnings on}
    begin
      ControlRenderBegin(Rect);
      C.RenderOverChildren;
    end;
  end;
end;

procedure TGLContainer.EventRender;

  procedure RenderEverything(const FilterRenderStyle: TRenderStyle; out SomeControlHasRenderStyle2D: boolean);
  var
    I: Integer;
  begin
    SomeControlHasRenderStyle2D := false;

    { draw controls in "to" order, back to front }
    for I := 0 to Controls.Count - 1 do
      RenderControlCore(Controls[I], Rect, SomeControlHasRenderStyle2D, FilterRenderStyle);

    if TooltipVisible and (Focus.Count <> 0) then
    begin
      {$warnings off} // knowingly looking at deprecated RenderStyle, to keep it working
      if Focus.Last.TooltipStyle = FilterRenderStyle then
      {$warnings on}
      begin
        ControlRenderBegin(Rect);
        Focus.Last.TooltipRender;
      end;
      {$warnings off} // knowingly looking at deprecated RenderStyle, to keep it working
      if Focus.Last.TooltipStyle = rs2D then
      {$warnings on}
        SomeControlHasRenderStyle2D := true;
    end;

    {$warnings off} // knowingly looking at deprecated RenderStyle, to keep it working
    if RenderStyle = FilterRenderStyle then
    {$warnings on}
    begin
      ControlRenderBegin(Rect);
      if Assigned(OnRender) then OnRender(Self);
    end;
    {$warnings off} // knowingly looking at deprecated RenderStyle, to keep it working
    if RenderStyle = rs2D then
    {$warnings on}
      SomeControlHasRenderStyle2D := true;
  end;

var
  SomeControlHasRenderStyle2D, Dummy: boolean;
begin
  RenderEverything(rs3D, SomeControlHasRenderStyle2D);
  if SomeControlHasRenderStyle2D then
    RenderEverything(rs2D, Dummy);
end;

procedure TGLContainer.RenderControl(const Control: TUIControl;
  const ViewportRect: TRectangle);
var
  SomeControlHasRenderStyle2D, Dummy, NeedsContainerSet, NeedsGLOpen: boolean;
  OldContainer: TUIContainer;
begin
  NeedsContainerSet := Control.Container <> Self;
  NeedsGLOpen := not Control.GLInitialized;

  { TODO: calling the methods below is not recursive,
    it will not prepare the children correctly. }
  if NeedsContainerSet then
  begin
    OldContainer := Control.Container;
    Control.Container := Self;
  end;
  if NeedsGLOpen then
    Control.GLContextOpen;
  Control.Resize;
  Control.BeforeRender;

  SomeControlHasRenderStyle2D := false;
  RenderControlCore(Control, ViewportRect, SomeControlHasRenderStyle2D, rs3D);
  if SomeControlHasRenderStyle2D then
    RenderControlCore(Control, ViewportRect, Dummy, rs2D);

  { TODO: calling the methods below is not recursive,
    it will not unprepare the children correctly. }
  if NeedsContainerSet then
    Control.Container := OldContainer;
  if NeedsGLOpen then
    Control.GLContextClose;
end;

procedure TGLContainer.EventClose(const OpenWindowsCount: Cardinal);
begin
  inherited;
  if OpenWindowsCount = 1 then
  begin
    { recreate FContext instance, to reset every variable when context is closed }
    if RenderContext = FContext then
      RenderContext := nil;
    FreeAndNil(FContext);
    FContext := TRenderContext.Create;
  end;
end;

function TGLContainer.SaveScreen(const SaveRect: TRectangle): TRGBImage;
begin
  EventBeforeRender;
  EventRender;
  { This is correct if we use double-buffer. }
  Result := SaveScreen_NoFlush(SaveRect, cbBack);
end;

{ global routines ------------------------------------------------------------ }

function RenderControlToImage(const Container: TGLContainer;
  const Control: TUIControl;
  const ViewportRect: TRectangle;
  const BackgroundColor: TCastleColor): TRGBAlphaImage;

  function CreateTargetTexture(const W, H: Integer): TGLTextureId;
  var
    InitialImage: TCastleImage;
  begin
    InitialImage := TRGBAlphaImage.Create(W, H);
    try
      InitialImage.URL := 'generated:/temporary-render-to-texture';
      InitialImage.Clear(Vector4Byte(255, 0, 255, 255));
      Result := LoadGLTexture(InitialImage,
        TextureFilter(minNearest, magNearest),
        Texture2DClampToEdge, nil, true);
    finally FreeAndNil(InitialImage) end;
  end;

var
  W, H: Integer;
  RenderToTexture: TGLRenderToTexture;
  TargetTexture: TGLTextureId;
begin
  W := ViewportRect.Width;
  H := ViewportRect.Height;
  RenderToTexture := TGLRenderToTexture.Create(W, H);
  try
    // RenderToTexture.Buffer := tbNone;
    // RenderToTexture.ColorBufferAlpha := true;

    RenderToTexture.Buffer := tbColor;
    TargetTexture := CreateTargetTexture(W, H);
    RenderToTexture.SetTexture(TargetTexture, GL_TEXTURE_2D);
    RenderToTexture.GLContextOpen;
    RenderToTexture.RenderBegin;

    { actually render }
    RenderContext.Clear([cbColor], BackgroundColor);
    Container.RenderControl(Control, ViewportRect);

    RenderToTexture.RenderEnd;

    Result := TRGBAlphaImage.Create(W, H);
    SaveTextureContents(Result, TargetTexture);

    RenderToTexture.GLContextClose;

    glFreeTexture(TargetTexture);
  finally FreeAndNil(RenderToTexture) end;
end;

end.
