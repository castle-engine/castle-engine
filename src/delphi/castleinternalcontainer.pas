{
  Copyright 2022-2023 Michalis Kamburelis.

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
  (use TCastleView instead or TCastleUserInterface.OnRender to do rendering).
  Nowadays we know that

  - API of classes like TCastleWindow / TCastleControl should be simpler and just expose
    Container for everything reasonable.

  - Container also should delegate to TCastleView and in turn to TCastleUserInterface
    for everything reasonable.

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
    { Internal platform-specific context data and initialization/finalization.
      This is in contrast to TCastleContainer.Context, that is public
      and manages context properties that are cross-platform and available
      for all OpenGL(ES). }
    FPlatformContext: TGLContext;
    FGLInitialized: Boolean;
    FAutoRedisplay: Boolean;
    { Copy of Requirements.DoubleBuffer when the context was created. }
    FEffectiveDoubleBuffer: Boolean;
    FDesignUrl: String;
    FDesignLoaded: TCastleUserInterface;
    FDesignLoadedOwner: TComponent;
    procedure MakeContextCurrent;
    procedure SetAutoRedisplay(const Value: Boolean);
    procedure DoUpdate;
    procedure SetDesignUrl(const Value: String);
    procedure LoadDesign;
    procedure UnLoadDesign;
  protected
    { Adjust context parameters right before usage. }
    procedure AdjustContext(const PlatformContext: TGLContext); virtual;

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

    { When the DesignUrl is set you can use this method to find
      loaded components. Like this:

      @longCode(#
      MyButton := MyCastleControl.DesignedComponent('MyButton') as TCastleButton;
      #)

      When the name is not found, raises exception (unless Required is @false,
      then it returns @nil).

      @seealso DesignUrl }
    function DesignedComponent(const ComponentName: String;
      const Required: Boolean = true): TComponent;
  published
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

    { Load and show the design (.castle-user-interface file).
      You can reference the loaded components by name using @link(DesignedComponent).

      If you have more complicated control flow,
      we recommend to leave this property empty, and split your management
      into a number of states (TCastleView) instead.
      In this case, load design using TCastleView.DesignUrl.
      This property makes it however easy to use .castle-user-interface
      in simple cases, e.g. when TCastleControl just shows one UI.

      The design loaded here is visible also at design-time,
      when editing the form in Lazarus/Delphi.
      Though we have to way to edit it now in Lazarus/Delphi (you have to use CGE editor
      to edit the design), so it is just a preview in this case.

      See https://castle-engine.io/control_on_form for documentation how to use TCastleControl. }
    property DesignUrl: String read FDesignUrl write SetDesignUrl;
  end;

  TCastleContainerEasyList = {$ifdef FPC}specialize{$endif} TObjectList<TCastleContainerEasy>;

implementation

uses SysUtils,
  CastleRenderContext, CastleGLUtils, CastleApplicationProperties, CastleGLImages,
  CastleGLVersion, CastleTimeUtils, CastleUtils, CastleLog, CastleURIUtils,
  CastleComponentSerialize, CastleInternalDelphiUtils, CastleFilesUtils;

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

  FPlatformContext := TGLContextWGL.Create;

  FAutoRedisplay := true;

  ContainersList.Add(Self);
end;

destructor TCastleContainerEasy.Destroy;
begin
  UnLoadDesign;
  FreeAndNil(FPlatformContext);
  if ContainersList <> nil then
    ContainersList.Remove(Self);
  inherited;
end;

procedure TCastleContainerEasy.MakeContextCurrent;
begin
  RenderContext := Context;
  FPlatformContext.MakeCurrent;
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

  function AnyOtherOpenContext: TGLContext;
  var
    C: TCastleContainerEasy;
  begin
    for C in ContainersList do
      if (C <> Self) and C.GLInitialized then
        Exit(C.FPlatformContext);
    Result := nil;
  end;

begin
  if not FGLInitialized then
  begin
    FGLInitialized := true;

    // In CGE, all open contexts should share GL resources
    FPlatformContext.SharedContext := AnyOtherOpenContext;

    AdjustContext(FPlatformContext);

    FPlatformContext.ContextCreate(FRequirements);

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

    // do this even at design-time, to allow animating in Delphi IDE
    if {(not (csDesigning in ComponentState)) and} (not UpdatingEnabled) then
    begin
      UpdatingEnabled := true;
      UpdatingEnable;
    end;
  end;
end;

procedure TCastleContainerEasy.AdjustContext(const PlatformContext: TGLContext);
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
  Fps.InternalRenderBegin;
  try
    EventRender;
    if GLVersion.BuggySwapNonStandardViewport then
      RenderContext.Viewport := Rect;
    FPlatformContext.SwapBuffers;

    // Note that calling Invalidate from RenderContext is not allowed,
    // it doesn't play OK with LCL or VCL.
    // if AutoRedisplay then Invalidate;
  finally Fps.InternalRenderEnd end;
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

procedure TCastleContainerEasy.LoadDesign;

{ Note: implementation of LoadDesign, UnLoadDesign and friends follow similar
  methods in TCastleView. Here they are much simplified, as we have no concept
  of "started" / "stopped", so no DesignPreload too. }

  procedure FixApplicationDataInIDE;
  var
    ProjectPath: String;
  begin
    if CastleDesignMode and
       (csDesigning in ComponentState) and
       Assigned(OnGetDesignTimeProjectPath) then
    begin
      ProjectPath := OnGetDesignTimeProjectPath();

      { Override ApplicationData interpretation, and castle-data:/xxx URL meaning. }
      ApplicationDataOverride := FilenameToURISafe(
        InclPathDelim(ProjectPath) + 'data' + PathDelim);
    end;
  end;

var
  OldCastleApplicationMode: TCastleApplicationMode;
begin
  if DesignUrl <> '' then
  begin
    { Make sure InternalCastleApplicationMode is correct, to
      - not e.g. do physics in Lazarus/Delphi form designer.
      - not show design-time stuff in DesignUrl loaded in CGE editor "help->system information".

      Note that we restore later InternalCastleApplicationMode.
      This way we avoid changing InternalCastleApplicationMode for future loads,
      when TCastleControl is used inside castle-editor.
      Testcase:
        in CGE editor:
        - open tools/castle-editor project
        - double click on demo design in data/demo_animation/
        - open help->system information (this uses TCastleControl too, with DesignUrl assigned)
        - close help->system information
        - close design
        - reopen design
    }
    OldCastleApplicationMode := InternalCastleApplicationMode;
    try
      if csDesigning in ComponentState then
        InternalCastleApplicationMode := appDesign
      else
        InternalCastleApplicationMode := appRunning;
      FixApplicationDataInIDE; // in case DesignUrl uses castle-data: protocol, which is most often the case

      FDesignLoadedOwner := TComponent.Create(nil);
      try
        FDesignLoaded := UserInterfaceLoad(DesignUrl, FDesignLoadedOwner);
      except
        { If loading design file failed, and we're inside form designer,
          merely report a warning. This allows deserializing LFMs with broken URLs. }
        on E: Exception do
        begin
          if CastleDesignMode then // looks at InternalCastleApplicationMode
          begin
            WritelnWarning('TCastleControl', 'Failed to load design "%s": %s', [
              URIDisplay(DesignUrl),
              ExceptMessage(E)
            ]);
            Exit;
          end else
            raise;
        end;
      end;
      Controls.InsertFront(FDesignLoaded);
    finally
      InternalCastleApplicationMode := OldCastleApplicationMode;
    end;
  end;
end;

procedure TCastleContainerEasy.UnLoadDesign;
begin
  FreeAndNil(FDesignLoadedOwner);
  FDesignLoaded := nil; // freeing FDesignLoadedOwner must have freed this too
end;

procedure TCastleContainerEasy.SetDesignUrl(const Value: String);
begin
  if FDesignUrl <> Value then
  begin
    UnLoadDesign;
    FDesignUrl := Value;
    LoadDesign;
  end;
end;

function TCastleContainerEasy.DesignedComponent(const ComponentName: String;
  const Required: Boolean = true): TComponent;
begin
  if FDesignLoaded <> nil then
    Result := FDesignLoadedOwner.FindComponent(ComponentName)
  else
    Result := nil;

  if Required and (Result = nil) then
    raise EComponentNotFound.CreateFmt('Cannot find component named "%s" in design "%s"', [
      ComponentName,
      URIDisplay(DesignUrl)
    ]);
end;

initialization
  {
  // This is some way to debug behavior at design-time when this code runs as part of Delphi IDE.
  // Uncomment to just have WritelnLog / WritelnWarning be recorded in the indicated file.
  LogFileName := 'c:/cygwin64/home/michalis/log2.txt';
  InitializeLog;
  }

  ContainersList := TCastleContainerEasyList.Create(false);
  //TODO: InitializeClipboard;
  //TODO: OnMainContainer := @TCastleControl(nil).GetMainContainer;
finalization
  //TODO: OnMainContainer := nil;
  FreeAndNil(ContainersList);
end.
