{
  Copyright 2022-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Container class TCastleContainerEasy,
  that is easier to use than TCastleContainer.
  In particular it relies on

  - private OpenGL(ES) context handled by TGLContext descendants

  - with requirements expressed by public TGLContextRequirements.

  TODO: It is our long-term plan to merge TCastleContainerEasy capabilities
  into TCastleContainer. Thus, the implementation done here will be shared
  by TCastleWindow (all backends) and LCL TCastleControl,
  unifying them more.
  Right now, TCastleWindow and LCL TCastleControl do some things manually
  for historic purposes, e.g. they expose GL context requirements
  like TCastleWindow.DepthBits,
  and they expose callbacks like TCastleWindow.OnRender that we don't advise to use
  (use TUIState instead or TCastleUserInterface.OnRender to do rendering).
  Nowadays we know that

  - API of classes like TCastleWindow / TCastleControl should be simpler and just expose
    Container for everything reasonable.

  - Container also should delegate to TUIState and in turn to TCastleUserInterface
    for everything reasonable.

  TODO: Rename FContext:TGLContext here to not confuse with Container.Context:TRenderContext.
}
unit CastleInternalContainer;

{$I castleconf.inc}

interface

uses Classes, Generics.Collections,
  CastleRectangles, CastleImages,
  CastleUIControls, CastleInternalContextBase, CastleInternalContextWgl;

type
  TCastleContainerEasy = class(TCastleContainer)
  strict private
    FRequirements: TGLContextRequirements;
    FContext: TGLContextWGL;
    FGLInitialized: Boolean;
    FAutoRedisplay: Boolean;
    { Copy of Requirements.DoubleBuffer when the context was created. }
    FEffectiveDoubleBuffer: Boolean;
    procedure MakeContextCurrent;
    procedure SetAutoRedisplay(const Value: Boolean);
    procedure DoUpdate;
  protected
    { Adjust context parameters right before usage. }
    procedure AdjustContext(const AContext: TGLContextWGL); virtual;

    { Call these methods from final components that wrap TCastleContainerEasy,
      like TCastleControl, TCastleWindow. }
    procedure CreateContext;
    procedure DestroyContext;
    procedure DoRender;

    class var
      { "Updating" means that the mechanism to call DoUpdateEverything
        continuosly is set up. }
      UpdatingEnabled: Boolean;

    class procedure DoUpdateEverything;
    class procedure UpdatingEnable; virtual; abstract;
    class procedure UpdatingDisable; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GLInitialized: boolean; override;
    function SaveScreen(const SaveRect: TRectangle): TRGBImage; overload; override;

    { Should we automatically redraw the window all the time,
      without the need for an @link(Invalidate) call.
      If @true (the default), rendering and other processing will
      be called constantly.

      If your game may have a still screen (nothing animates),
      then this approach is a little unoptimal, as we use CPU and GPU
      for drawing, when it's not needed. In such case, you can set this
      property to @false, and make sure that you call
      @link(Invalidate) always when you need to redraw the screen.
      Note that the engine components always call @link(Invalidate) when
      necessary, so usually you should only call it yourself if you provide
      a custom @link(OnRender) implementation. }
    property AutoRedisplay: Boolean read FAutoRedisplay write SetAutoRedisplay
      default true;

    { Context required parameters. }
    property Requirements: TGLContextRequirements read FRequirements;
  end;

  TCastleContainerEasyList = {$ifdef FPC}specialize{$endif} TObjectList<TCastleContainerEasy>;

implementation

uses SysUtils,
  CastleRenderContext, CastleGLUtils, CastleApplicationProperties, CastleGLImages,
  CastleGLVersion, CastleTimeUtils;

var
  { All TCastleContainerEasy instances created.

    We use this to share OpenGL contexts,
    as all OpenGL contexts in our engine must share OpenGL resources
    (our OnGLContextOpen and such callbacks depend on it,
    and it makes implementation much easier). }
  ContainersList: TCastleContainerEasyList;

  { Tracks how many containers on ContainersList have GL context initialized. }
  ContainersOpen: Cardinal;

  LastLimitFPSTime: TTimerResult;

constructor TCastleContainerEasy.Create(AOwner: TComponent);
begin
  inherited;

  FRequirements := TGLContextRequirements.Create(Self);
  FRequirements.Name := 'Requirements';
  FRequirements.SetSubComponent(true);

  FContext := TGLContextWGL.Create;
  FContext.WindowCaption := 'Castle'; // TODO: invented, check it is OK, with MultiSampling > 1
  FContext.WndClassName := 'Castle'; // TODO: invented, check it is OK, with MultiSampling > 1

  FAutoRedisplay := true;

  ContainersList.Add(Self);
end;

destructor TCastleContainerEasy.Destroy;
begin
  FreeAndNil(FContext);
  if ContainersList <> nil then
    ContainersList.Remove(Self);
  inherited;
end;

procedure TCastleContainerEasy.MakeContextCurrent;
begin
  RenderContext := Context;
  FContext.MakeCurrent;
end;

function TCastleContainerEasy.SaveScreen(const SaveRect: TRectangle): TRGBImage;

  { Color buffer where we draw, and from which it makes sense to grab pixels. }
  function SaveScreenBuffer: TColorBuffer;
  begin
    if FEffectiveDoubleBuffer then
      Result := cbBack
    else
      Result := cbFront;
  end;

begin
  MakeContextCurrent;
  EventBeforeRender;
  EventRender;
  Result := SaveScreen_NoFlush(Rect, SaveScreenBuffer);
end;

procedure TCastleContainerEasy.CreateContext;
begin
  if not FGLInitialized then
  begin
    FGLInitialized := true;

    AdjustContext(FContext);

    FContext.ContextCreate(FRequirements);

    FEffectiveDoubleBuffer := Requirements.DoubleBuffer;

    // initialize CGE OpenGL resources
    MakeContextCurrent;
    GLInformationInitialize;
    RenderContext.Viewport := Rect;
    ApplicationProperties._GLContextEarlyOpen;

    Inc(ContainersOpen);
    // Note that this will cause ApplicationProperties._GLContextOpen if necessary
    EventOpen(ContainersOpen);
    EventResize;
    Invalidate;

    if (not (csDesigning in ComponentState)) and (not UpdatingEnabled) then
    begin
      UpdatingEnabled := true;
      UpdatingEnable;
    end;
  end;
end;

procedure TCastleContainerEasy.AdjustContext(const AContext: TGLContextWGL);
begin
end;

procedure TCastleContainerEasy.DestroyContext;
begin
  if FGLInitialized then
  begin
    EventClose(ContainersOpen);
    Dec(ContainersOpen);
    FGLInitialized := false;

    if UpdatingEnabled and (ContainersOpen = 0) then
    begin
      UpdatingEnabled := false;
      UpdatingDisable;
    end;
  end;
  inherited;
end;

function TCastleContainerEasy.GLInitialized: boolean;
begin
  Result := FGLInitialized;
end;

procedure TCastleContainerEasy.DoRender;
begin
  MakeContextCurrent;

  EventBeforeRender;
  Fps._RenderBegin;
  try
    EventRender;
    if GLVersion.BuggySwapNonStandardViewport then
      RenderContext.Viewport := Rect;
    FContext.SwapBuffers;

    // Note that calling Invalidate from RenderContext is not allowed,
    // it doesn't play OK with LCL or VCL.
    // if AutoRedisplay then Invalidate;
  finally Fps._RenderEnd end;
end;

procedure TCastleContainerEasy.DoUpdate;
begin
  if AutoRedisplay then
    Invalidate;

  { Update event also requires that proper OpenGL context is current.

    This matters because OpenGL resources may be used durign update,
    e.g. TCastleScene.Update will update auto-generated textures,
    doing e.g. TGLGeneratedCubeMapTextureNode.Update.
    This should run in proper OpenGL context.
    Esp. as not all resources must be shared between contexts:
    FBO are not shared in new OpenGL versions, see
    https://stackoverflow.com/questions/4385655/is-it-possible-to-share-an-opengl-framebuffer-object-between-contexts-threads

    Testcase: open examples/mobile/simple_3d_demo/ in editor,
    open main design,
    click on previews with GeneratedCubeMap like castle_with_lights_and_camera.wrl .
    Without this fix, we'll have an OpenGL error.

    Doing MakeCurrent here is consistent with TCastleWindow.DoUpdate . }
  MakeContextCurrent;
  EventUpdate;
end;

procedure TCastleContainerEasy.SetAutoRedisplay(const Value: boolean);
begin
  if FAutoRedisplay <> Value then
  begin
    FAutoRedisplay := value;
    if Value then
      Invalidate;
  end;
end;

class procedure TCastleContainerEasy.DoUpdateEverything;

  procedure DoLimitFPS;
  var
    NowTime: TTimerResult;
    TimeRemainingFloat: Single;
  begin
    if ApplicationProperties.LimitFPS > 0 then
    begin
      NowTime := Timer;

      { When this is run for the 1st time, LastLimitFPSTime is zero,
        so NowTime - LastLimitFPSTime is huge, so we will not do any Sleep
        and only update LastLimitFPSTime.

        For the same reason, it is not a problem if you do not call DoLimitFPS
        often enough (for example, you do a couple of ProcessMessage calls
        without DoLimitFPS for some reason), or when user temporarily sets
        LimitFPS to zero and then back to 100.0.
        In every case, NowTime - LastLimitFPSTime will be large, and no sleep
        will happen. IOW, in the worst case --- we will not limit FPS,
        but we will *never* slow down the program when it's not really necessary. }

      TimeRemainingFloat :=
        { how long I should wait between _LimitFPS calls }
        1 / ApplicationProperties.LimitFPS -
        { how long I actually waited between _LimitFPS calls }
        TimerSeconds(NowTime, LastLimitFPSTime);
      { Don't do Sleep with too small values.
        It's better to have larger FPS values than limit,
        than to have them too small. }
      if TimeRemainingFloat > 0.001 then
      begin
        Sleep(Round(1000 * TimeRemainingFloat));
        LastLimitFPSTime := Timer;
      end else
        LastLimitFPSTime := NowTime;
    end;
  end;

var
  I: Integer;
  C: TCastleContainerEasy;
begin
  for I := ContainersList.Count - 1 downto 0 do
  begin
    C := ContainersList[I];
    if C.GLInitialized then
      C.DoUpdate;
  end;
  ApplicationProperties._Update;
  DoLimitFPS;
end;

initialization
  ContainersList := TCastleContainerEasyList.Create(false);
  //TODO: InitializeClipboard;
  //TODO: OnMainContainer := @TCastleControl(nil).GetMainContainer;
finalization
  //TODO: OnMainContainer := nil;
  FreeAndNil(ContainersList);
end.
