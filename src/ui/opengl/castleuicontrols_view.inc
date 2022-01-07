{
  Copyright 2010-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

//type

  { "View" represents the current state of your application user interface.
    See https://castle-engine.io/manual_2d_user_interface.php#section_ui_state
    for an overview of using TCastleView.

    In a given container (like a window), in the simple case only one TCastleView is @italic(current) at a given time,
    and it can be get or set using the @link(TCastleContainer.View) property.
    In more complex cases, you can use @link(TCastleContainer.PushView) and @link(TCastleContainer.PopView)
    to build a stack of views, and in effect multiple views are active at the same time.
    All of the views on stack are @italic(started), but only the top-most is @italic(resumed).

    Each TCastleView has @link(Start) and @link(Stop)
    methods that you can override to perform work when view becomes
    part of the current view stack, or stops being part of it.
    You can also override @link(Resume) and @link(Pause) methods,
    to perform work when the view becomes the top-most view or is no longer
    the top-most view. The distinction becomes important once you play
    around with pushing/popping view.

    To define view user interface:

    @orderedList(
      @item(It is simplest to set @link(DesignUrl)
        to the design file you created using @url(https://castle-engine.io/manual_editor.php CGE editor).
        Such user interface controls will be created right before @link(Start)
        and destroyed right after @link(Stop) (so the view UI always "resets"
        when view starts).)

      @item(You can always create more UI controls and add them to the view at any point.
        The view is a @link(TCastleUserInterface) descendant and you can add UI to it
        just by using @link(TCastleUserInterface.InsertFront).

        UI children can be added anytime you want, e.g. in @link(Start) or in overridden
        constructor.

        UI children can be removed or destroyed anytime you want as well.
        You can use @link(FreeAtStop) as an owner for anything you want to be automatically
        destroyed at @link(Stop).)
    )

    Current view is placed on the list of container controls.
    This way view is notified about UI events, and can react to them.
    Note that our children will handle events @italic(before) the view itself
    is notified about them, following @link(TCastleUserInterface) events behavior.
    This way view can:

    @unorderedList(
      @item(React to @link(TCastleUserInterface.Press Press),
        @link(TCastleUserInterface.Release Release) of keys or mouse buttons,)

      @item(do something continuos in @link(TCastleUserInterface.Update Update).)
    )

    See the TCastleUserInterface class for a lot of useful methods that you can
    override in your view descendants to capture various events. }
  TCastleView = class(TCastleUserInterface)
  private
    FStartContainer: TCastleContainer;
    FStartContainerObserver: TFreeNotificationObserver;
    FInterceptInput, FFreeWhenStopped: boolean;
    FFreeAtStop: TComponent;
    FWaitingForRender: TNotifyEventList;
    FCallBeforeUpdate: TNotifyEventList;

    { Design* private fields }
    FDesignUrl: String;
    FDesignLoaded: TCastleUserInterface;
    FDesignLoadedOwner: TComponent;
    FDesignPreload: Boolean;
    FDesignPreloadedSerialized: TSerializedComponent;
    FDesignPreloaded: TCastleUserInterface;
    FDesignPreloadedOwner: TComponent;

    procedure InternalStart(const AContainer: TCastleContainer);
    procedure InternalStop;
    procedure StartContainerFreeNotification(const Sender: TFreeNotificationObserver);
    { Stop yourself and remove from FViewStack, if present there. }
    procedure StopIfStarted;

    { Design* private routines }
    procedure SetDesignUrl(const Value: String);
    procedure SetDesignPreload(const Value: Boolean);
    procedure LoadDesign;
    procedure UnLoadDesign;
    procedure PreloadDesign;
    procedure UnPreloadDesign;

    class function GetCurrent: TCastleView; static;
    class procedure SetCurrent(const Value: TCastleView); static;
    class function GetCurrentTop: TCastleView; static;
    class function GetStateStack(const Index: Integer): TCastleView; static;

    { Container in which view is started (if view is started)
      or on which we predict it will be started (if view is not started yet).

      If view is not started yet,
      this is
      @link(TCastleApplication.MainWindow Application.MainWindow)
      if you use CastleWindow or
      @link(TCastleControlBase.MainControl) if you use CastleControl.

      When the view has started, then @link(Container) property (from
      ancestor, see TCastleUserInterface.Container) is equal to this. }
    function PredictedContainer: TCastleContainer;
  protected
    { The container size is always known: we assume that view will be part
      of GetMainContainer, if the view not yet started.

      So the view knows container size, regardless if we are between Start / Stop,
      regardless if the TCastleView is already added to some TCastleContainer.Children.
      This makes all other routines, like ParentRect, EffectiveRect, EffectiveWidth,
      EffectiveHeight also work. }
    function ContainerWidth: Cardinal; override;
    function ContainerHeight: Cardinal; override;
    function ContainerRect: TRectangle; override;
    function ContainerSizeKnown: boolean; override;

    function StateContainer: TCastleContainer; deprecated 'use StartContainer to get the container in which we are already started, use GetMainContainer to get global central container';

    { Container in which view is started (if view is started)
      or nil (if view is not started yet).

      This is just like @link(Container) property (from
      ancestor, see TCastleUserInterface.Container) but it is
      already assigned even during @link(Start) and @link(Stop). }
    property StartContainer: TCastleContainer read FStartContainer;

    { Assign this component as owner for your controls,
      to make them freed during nearest @link(Stop). }
    function FreeAtStop: TComponent;
  public
    { When @true, view operations will send a log to CastleLog. }
    class var Log: boolean;

    { Create an instance of the view.
      You willl typically create one instance of each view class
      (like TViewMain, TViewPlay) at the application initialization
      (e.g. in Application.OnInitialize callback), like

      @longCode(#
        ViewMainMenu := TViewMainMenu.Create(Application);
        ViewPlay := TViewPlay.Create(Application);
      #)

      Later you switch between views using @link(TCastleContainer.View) or
      @link(TCastleContainer.PushView) or
      @link(TCastleContainer.PopView),
      like this:

      @longCode(#
        Container.View := ViewMain;
      #)

      See https://castle-engine.io/manual_2d_user_interface.php and numerous engine examples.
    }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Create the instance TCastleView that will be automatically freed when
      the view is stopped.
      This allows alternative usage of views (as opposed to the ones described in @link(Create)
      docs), where you create short-lived instances of view classes.
      Use it like this:

      @longCode(#
        Container.View := TViewMain.CreateUntilStopped;
      #)

      The advantages:

      @unorderedList(
        @item(You don't need to worry
          that some view field value will "survive" with an invalid value after
          @link(Stop). So you don't need to clear everything in @link(Stop)
          or initialize everything in @link(Start), instead you can depend
          that @link(Start) happens only once right after the constructor,
          so the instance fields are clear.)

        @item(You avoid having global variables, keeping singletons of each view class.
          So the code is a little safer.)

        @item(You can reintroduce your own constructor to require some parameters,
          instead of exposing view parameters as public fields/properties.)
      )

      The disadvantage is that you cannot store in view fields anything
      that should "survive" the view @link(Stop).
      You can instead use "class variables" in view class, or any global variable.
    }
    constructor CreateUntilStopped;

    { Executed when view becomes active, it's now part of the view stack.

      Started view is part of the ViewStack, and will soon become
      running (top-most on the stack). When the view is set to be current,
      by @code(Container.View := MyView), this happens:

      @orderedList(
        @item(MyView is pushed as the top-most view on view stack.)
        @item(MyView.Start is called.)
        @item(MyView is added to the @link(TCastleContainer.Controls) list
          (using the container where you set @link(TCastleContainer.PushView),
          @link(TCastleContainer.View)).
          This also means that the view methods GLContextOpen and Resize are called
          (as for all normal TCastleUserInterface instances).)
        @item(MyStar.Resume is called.)
      ) }
    procedure Start; virtual;

    { Executed when view is no longer active, no longer part of view stack.

      When the view stops becoming active, this happens:

      @orderedList(
        @item(MyView.Pause is called.)
        @item(MyView is removed from the
          @code(TCastleContainer.Controls) list
          (using the container to which we were added).
          So the view method GLContextClose is called
          (as for all normal TCastleUserInterface instances).)
        @item(MyView.Stop is called.)
        @item(MyView is removed from the on view stack.)
      )

      This is always called to finalize the started view.
      When the view is destroyed, it's @link(Pause) and @link(Stop)
      are called too, so you can use this method to reliably finalize whatever
      you initialized in @link(Start). }
    procedure Stop; virtual;

    { Executed when view is now the top-most view. See @link(Start) and @link(Stop)
      docs about view lifecycle methods.
      This is called after @link(Start), it is also called
      when you pop another view, making this view the top-most. }
    procedure Resume; virtual;

    { Executed when view is no longer the top-most view. See @link(Start) and @link(Stop)
      docs about view lifecycle methods.
      This is called before @link(Stop), it is also called
      when another view is pushed over this view, so this stops
      being the the top-most view. }
    procedure Pause; virtual;

    procedure Finish; virtual; deprecated 'use Stop';

    { View is right now part of the view stack, which means
      it's between @link(Start) and @link(Stop) calls.
      The view is added to the stack before the @link(Start) call,
      and removed after the @link(Stop) call, so this returns @true
      during all the methods --- @link(Start), @link(Resume), @link(Pause), @link(Stop). }
    function Active: boolean;

    { Prevents passing mouse/keyboard events to the UI views underneath.

      More precisely, when this property is @true, then the
      @link(Press), @link(Release) and @link(Motion) events are marked as
      "handled" in this UI view. This means that they will not be processed
      further, by UI controls under this view, in particular by UI views
      that are underneath this view in @italic(view stack) (created
      by @link(Push) method). They will also not be passed to final container
      (TCastleWindowBase, TCastleControlBase) callbacks like
      TCastleWindowBase.OnPress (as these callbacks are always used at the end,
      when nothing else handled the event). }
    property InterceptInput: boolean read FInterceptInput write FInterceptInput
      default false;

    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
    function Motion(const Event: TInputMotion): boolean; override;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
    procedure Render; override;
    function UIScale: Single; override;

    { Load and show a user interface from a .castle-user-interface file,
      designed in Castle Game Engine Editor.

      This is an utility method, loading a UI in a typical way into the TCastleView.
      It is not the only way to load a .castle-user-interface file,
      a more general approach is to use @link(UserInterfaceLoad) from
      @link(CastleComponentSerialize) unit, and manually call InsertFront
      to add it to the view UI.

      The loaded UI is returned under the @code(Ui) parameter,
      unless you use an overloaded version that omits this parameter.

      It is owned by @code(UiOwner), which is useful to find the components
      by name, like
      @longCode(#
        ButtonQuit := UiOwner.FindRequiredComponent('ButtonQuit') as TCastleButton;
      #)

      The UiOwner, in turn, is owned by the FinalOwner.
      You typically use this method inside overridden @link(Start),
      and as FinalOwner you pass @link(FreeAtStop) -- this way the user interface
      is freed when the view is stopped.
    }
    procedure InsertUserInterface(const ADesignUrl: String;
      const FinalOwner: TComponent;
      out Ui: TCastleUserInterface; out UiOwner: TComponent); overload; deprecated 'instead of this, set DesignUrl in constructor';
    procedure InsertUserInterface(const ADesignUrl: String;
      const FinalOwner: TComponent; out UiOwner: TComponent); overload; deprecated 'instead of this, set DesignUrl in constructor';

    { Wait until the render event happens (to redraw current view),
      and then call Event.

      The scheduled Event will be called at the @link(Update) time.

      If the view stopped before the scheduled events fired,
      then the remaining events are ignored.
      That is, the scheduled events are cleared every time you start the view.

      One use-case of this is to show a loading view,
      where you load things, but also update the loading progress from time to time.
      Be careful in this case to not call this @italic(too often),
      as then your loading time will be tied to rendering time.
      For example, when the monitor refresh rate is 60, and the "vertical sync"
      is "on", then the render events happen at most 60 times per second.
      So if you call WaitForRenderAndCall too often during loading,
      you may spend more time waiting for render event (each WaitForRenderAndCall
      taking 1/60 of second) than doing actual loading. }
    procedure WaitForRenderAndCall(const Event: TNotifyEvent);

    { Load a designed user interface (from .castle-user-interface file)
      when this view is started.
      You typically set this property in overridden constructor, and in effect
      the given design file will be loaded right before @link(Start) and it will be freed
      right after @link(Stop).

      Typical use-case looks like this:

      @longCode(#
      constructor TViewPlay.Create(AOwner: TComponent);
      begin
        inherited;
        DesignUrl := 'castle-data:/gameviewplay.castle-user-interface';
        // DesignPreload := true; // to make future "Container.View := ViewPlay" faster
      end;

      procedure TViewPlay.Start;
      begin
        inherited;
        MyButton := DesignedComponent('MyButton') as TCastleButton;
      end;
      #)

      You can also modify this property when the view has already started
      (after @link(Start) and before @link(Stop)) in which case
      the previous design will be freed and new one will be loaded immediately.

      Set this to empty string (default) to mean "no design should be loaded".

      Note that this is not the only way to load a .castle-user-interface file.
      A more general approach is to use @link(UserInterfaceLoad),
      and call @link(InsertFront) to add it to the view UI.
      Using this property just adds some comfortable automatic behavior
      (view is freed at stop, and you can use comfortable
      @link(DesignedComponent) or @link(DesignPreload)).

      @seealso DesignPreload
      @seealso DesignedComponent
    }
    property DesignUrl: String read FDesignUrl write SetDesignUrl;

    { Preload the design file (specified in @link(DesignUrl)) as soon as possible,
      making starting the view much faster.
      Using this property means that we "preload" the design file,
      to cache the referenced images / TCastleScene instances etc. as soon as possible,
      to make the future loading of this design (when view starts) very quick.

      Typically you set this property in overridden TCastleView constructor,
      right after (or before, it doesn't matter) setting DesignUrl.
      It will mean that constructor takes a longer time (which typically means
      that Application.OnInitialize takes a longer time) but in exchange
      future starting of the view (when you do e.g. @code(Container.View := ViewXxx)
      or @code(Container.PushView(ViewXxx)) will be much faster.

      No functional difference should be visible, regardless of the @link(DesignPreload)
      value. Internally the designed component is added/removed from view at the same time.
      So think of this property as pure optimization -- you decide whether to slow down
      the view constructor, or view start.

      @seealso DesignUrl }
    property DesignPreload: Boolean read FDesignPreload write SetDesignPreload default false;

    { When the DesignUrl is set, and the view is started, you can use this method to find
      loaded components. Like this:

      @longCode(#
      MyButton := DesignedComponent('MyButton') as TCastleButton;
      #)

      See @link(DesignUrl) for full usage example.

      @seealso DesignUrl }
    function DesignedComponent(const ComponentName: String): TComponent;

    { TODO: This doesn't work with FPC 3.2.0, see implementation comments.
      It is also not that useful -- unlike Unity "GameObject.GetComponent<T>(): T",
      in this case T would not be used as searching criteria, so it would
      be just another way to express "as" cast. }
    // generic function DesignedComponent<T: TComponent>(const ComponentName: String): T; overload;

    class property Current: TCastleView read GetCurrent write SetCurrent; deprecated 'use Container.View';
    class property CurrentTop: TCastleView read GetCurrentTop; deprecated 'use Container.FrontView';
    class procedure Push(const NewState: TCastleView); deprecated 'use Container.PushView';
    class procedure Pop; overload; deprecated 'use Container.PopView';
    class procedure Pop(const CurrentTopMostState: TCastleView); overload; deprecated 'use Container.PopView';
    class function StateStackCount: Integer; deprecated 'use Container.ViewStackCount';
    class property StateStack [const Index: Integer]: TCastleView read GetStateStack; deprecated 'use Container.ViewStack';
  published
    { TCastleView control makes most sense when it is FullSize,
      filling the whole window.

      This way it always captures events on the whole container.
      And the child controls (anchored to this)
      behave like anchored to the whole container. }
    property FullSize default true;
  end;

  TCastleViewList = class({$ifdef FPC}specialize{$endif} TObjectList<TCastleView>);

{$endif read_interface}

{$ifdef read_implementation}

procedure ErrorDesignLoaded;
begin
  raise Exception.Create('DesignedComponent can only be used if the desing was loaded, which means that TCastleView has started and DesignUrl is not empty');
end;

function RequireMainContainer: TCastleContainer;
begin
  Result := GetMainContainer;
  if Result = nil then
    raise Exception.Create('Assign Application.MainWindow (if you use CastleWindow) or TCastleControl.MainControl (if you use CastleControl) before starting TCastleView');
end;


{ TSerializedComponentHelper ------------------------------------------------- }

type
  { Helper methods extending TSerializedComponent.
    Do not use TSerializedComponentHelper from CastleViewport,
    to not complicate unit dependencies. }
  TSerializedComponentHelper = class helper for TSerializedComponent
    { Instantiate component.
      Using this is equivalent to using global @link(UserInterfaceLoad),
      but it is much faster if you want to instantiate the same file many times. }
    function UserInterfaceLoad(const Owner: TComponent): TCastleUserInterface;
  end;

function TSerializedComponentHelper.UserInterfaceLoad(const Owner: TComponent): TCastleUserInterface;
begin
  Result := ComponentLoad(Owner) as TCastleUserInterface;
end;

{ TCastleView --------------------------------------------------------------------- }

function TCastleView.FreeAtStop: TComponent;
begin
  if FFreeAtStop = nil then
    FFreeAtStop := TComponent.Create(Self);
  Result := FFreeAtStop;
end;

procedure TCastleView.InternalStart(const AContainer: TCastleContainer);
var
  TimeStart: TCastleProfilerTime;
begin
  TimeStart := Profiler.Start('Started view ' + Name + ': ' + ClassName);

  FWaitingForRender.Clear;
  FCallBeforeUpdate.Clear;

  { typically, the Start method will initialize some stuff,
    making the 1st SecondsPassed non-representatively large. }
  AContainer.Fps.ZeroNextSecondsPassed;

  FStartContainer := AContainer;
  FStartContainerObserver.Observed := FStartContainer;

  LoadDesign;

  Start;
  { actually insert, this will also call GLContextOpen and Resize.
    However, check first that we're still the current view,
    to safeguard from the fact that Start changed view
    (like the loading view, that changes to play view immediately in start). }
  if AContainer.FViewStack.IndexOf(Self) <> -1 then
    AContainer.Controls.InsertFront(Self);

  Profiler.Stop(TimeStart, Log);
end;

procedure TCastleView.InternalStop;
begin
  // when container is csDestroying, the FStartContainer.Controls may be invalid
  if not (csDestroying in FStartContainer.ComponentState) then
    FStartContainer.Controls.Remove(Self);

  Stop;

  {$warnings off}
  Finish;
  {$warnings on}

  UnLoadDesign;

  FStartContainer := nil;
  FStartContainerObserver.Observed := nil;
  FreeAndNil(FFreeAtStop);

  if Log then
    WritelnLog('TCastleView', 'Stopped: ' + Name + ': ' + ClassName);
end;

procedure TCastleView.StartContainerFreeNotification(
  const Sender: TFreeNotificationObserver);
begin
  { If FStartContainer is being freed, we need to stop now.
    Otherwise later, InternalStop would surely fail, as it would try
    to access invalid FStartContainer reference.
    See https://github.com/castle-engine/castle-engine/issues/307 . }
  StopIfStarted;

  { Through InternalStop, above always sets FStartContainer to nil }
  Assert(FStartContainer = nil);
end;

function TCastleView.StateContainer: TCastleContainer;
begin
  Result := PredictedContainer;
end;

function TCastleView.PredictedContainer: TCastleContainer;
begin
  if FStartContainer <> nil then
    { between Start and Stop, be sure to return the same thing
      from PredictedContainer method. Also makes it working when Application
      is nil when destroying view from CastleWindow finalization. }
    Result := FStartContainer
  else
  begin
    Result := GetMainContainer;
    if Result = nil then
      raise Exception.Create('Assign Application.MainWindow (if you use CastleWindow) or TCastleControl.MainControl (if you use CastleControl) before starting TCastleView');
  end;
end;

constructor TCastleView.Create(AOwner: TComponent);
begin
  inherited;
  FullSize := true;
  FWaitingForRender := TNotifyEventList.Create;
  FCallBeforeUpdate := TNotifyEventList.Create;
  FStartContainerObserver := TFreeNotificationObserver.Create(Self);
  FStartContainerObserver.OnFreeNotification := {$ifdef FPC}@{$endif}StartContainerFreeNotification;
end;

constructor TCastleView.CreateUntilStopped;
begin
  Create(nil);
  FFreeWhenStopped := true;
end;

procedure TCastleView.StopIfStarted;
begin
  if (FStartContainer <> nil) and
     (FStartContainer.FViewStack <> nil) and
     (FStartContainer.FViewStack.IndexOf(Self) <> -1) then
  begin
    if FStartContainer.FViewStack.Last = Self then
      Pause;
    InternalStop;
    FStartContainer.FViewStack.Remove(Self);
    { deallocate empty FViewStack. Doing this here allows to deallocate
      FViewStack only once all views finished gracefully. }
    if FStartContainer.FViewStack.Count = 0 then
      FreeAndNil(FStartContainer.FViewStack);
  end;
end;

destructor TCastleView.Destroy;
begin
  StopIfStarted;
  UnLoadDesign;
  UnPreloadDesign;
  FreeAndNil(FWaitingForRender);
  FreeAndNil(FCallBeforeUpdate);
  inherited;
end;

procedure TCastleView.Resume;
begin
  if Log then
    WritelnLog('TCastleView', 'Resuming: ' + Name + ': ' + ClassName);
end;

procedure TCastleView.Pause;
begin
  if Log then
    WritelnLog('TCastleView', 'Paused: ' + Name + ': ' + ClassName);
end;

procedure TCastleView.Start;
begin
  { Do not place here any logic, to make sure TCastleView works reliably even when
    descendant doesn't have "inherited" call.
    While we don't guarantee that our classes generally work OK in such situations
    (you should always call "inherited" unless documented otherwise),
    but in this special case we try to make it so, otherwise sometimes hard-to-debug
    errors occur. }
end;

procedure TCastleView.Stop;
begin
  { Do not place here any logic, to make sure TCastleView works reliably even when
    descendant doesn't have "inherited" call.
    Same as in Start. }
end;

procedure TCastleView.Finish;
begin
end;

function TCastleView.Active: boolean;
begin
  Result := FStartContainer <> nil;
end;

function TCastleView.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  Result := Result or InterceptInput;
end;

function TCastleView.Release(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  Result := Result or InterceptInput;
end;

function TCastleView.Motion(const Event: TInputMotion): boolean;
begin
  Result := inherited;
  Result := Result or InterceptInput;
end;

procedure TCastleView.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
begin
  inherited;

  { do not allow controls underneath to handle input }
  if InterceptInput then
    HandleInput := false;

  if FCallBeforeUpdate.Count <> 0 then // optimize away common case
  begin
    { In case of using CreateUntilStopped view, you cannot change view
      in the middle of FCallBeforeUpdate.ExecuteAll,
      as it would free the array that is being iterated over. }
    if FFreeWhenStopped then Inc(FStartContainer.FDisableStackChange);
    try
      FCallBeforeUpdate.ExecuteAll(Self);
      FCallBeforeUpdate.Clear;
    finally
      if FFreeWhenStopped then Dec(FStartContainer.FDisableStackChange);
    end;
  end;
end;

procedure TCastleView.InsertUserInterface(const ADesignUrl: String;
  const FinalOwner: TComponent;
  out Ui: TCastleUserInterface; out UiOwner: TComponent);
begin
  UiOwner := TComponent.Create(FinalOwner);
  Ui := UserInterfaceLoad(ADesignUrl, UiOwner);
  InsertFront(Ui);
end;

procedure TCastleView.InsertUserInterface(const ADesignUrl: String;
  const FinalOwner: TComponent; out UiOwner: TComponent);
var
  Ui: TCastleUserInterface;
begin
  {$warnings off} // using deprecated in deprecated
  InsertUserInterface(ADesignUrl, FinalOwner, Ui, UiOwner);
  {$warnings on}
  // ignore the returned Ui reference
end;

procedure TCastleView.Render;
begin
  inherited;
  FCallBeforeUpdate.AddRange(FWaitingForRender);
  FWaitingForRender.Clear;
end;

procedure TCastleView.WaitForRenderAndCall(const Event: TNotifyEvent);
begin
  FWaitingForRender.Add(Event);
end;

procedure TCastleView.LoadDesign;
begin
  if DesignUrl <> '' then
  begin
    FDesignLoadedOwner := TComponent.Create(nil);
    if FDesignPreloadedSerialized <> nil then
      FDesignLoaded := FDesignPreloadedSerialized.UserInterfaceLoad(FDesignLoadedOwner)
    else
      FDesignLoaded := UserInterfaceLoad(DesignUrl, FDesignLoadedOwner);
    InsertFront(FDesignLoaded);
  end;
end;

procedure TCastleView.UnLoadDesign;
begin
  FreeAndNil(FDesignLoadedOwner);
  FDesignLoaded := nil; // freeing FDesignLoadedOwner must have freed this too
end;

procedure TCastleView.PreloadDesign;
begin
  if DesignUrl <> '' then
  begin
    // load FDesignPreloadedSerialized to be able to faster load design, without parsing JSON
    FDesignPreloadedSerialized := TSerializedComponent.Create(DesignUrl);
    // load FDesignPreloaded to cache all that's possible,
    // like images used inside TCastleImageControl or TCastleScene.
    FDesignPreloadedOwner := TComponent.Create(nil);
    FDesignPreloaded := FDesignPreloadedSerialized.UserInterfaceLoad(FDesignPreloadedOwner);
  end;
end;

procedure TCastleView.UnPreloadDesign;
begin
  { Note that it doesn't matter whether this FDesignPreloaded was used to load
    currently created FDesignLoaded or not.
    So FDesignLoaded* and FDesignPreloadedxx* are completely independent.
    This makes it easier to think about them. }
  FreeAndNil(FDesignPreloadedOwner);
  FDesignPreloaded := nil;// freeing FDesignPreloadedOwner must have freed this too
  FreeAndNil(FDesignPreloadedSerialized);
end;

procedure TCastleView.SetDesignUrl(const Value: String);
begin
  if FDesignUrl <> Value then
  begin
    UnLoadDesign;
    UnPreloadDesign;
    FDesignUrl := Value;
    if DesignPreload then
      PreloadDesign; // do this before LoadDesign, as LoadDesign may use it
    if Active then
      LoadDesign;
  end;
end;

procedure TCastleView.SetDesignPreload(const Value: Boolean);
begin
  if FDesignPreload <> Value then
  begin
    UnPreloadDesign;
    FDesignPreload := Value;
    if FDesignPreload then
      PreloadDesign;
  end;
end;

(*
{ FPC 3.2.0 error:
    Error: function header doesn't match the previous declaration "DesignedComponent$1(const AnsiString):T;"
    Error: Found declaration: DesignedComponent$1(const AnsiString):T;
}
generic function TCastleView.DesignedComponent<T: TComponent>(const ComponentName: String): T;
begin
  if FDesignLoaded = nil then
    ErrorDesignLoaded;
  Result := FDesignLoaded.FindRequiredComponent(ComponentName) as T;
end;

{ FPC 3.2.0 error:
    Error: Global Generic template references static symtable
  This works OK in a smaller testcase. }
generic function TCastleView.DesignedComponent<T>(const ComponentName: String): T;
begin
  if FDesignLoaded = nil then
    ErrorDesignLoaded;
  // Dirty typecast to TClass needed, otherwise we get error
  //   Error: Class or interface type expected, but got "T"
  // The error is valid -- which is why our first option was generic with constraints
  // <T: TComponent> yet it failed.
  Result := FDesignLoaded.FindRequiredComponent(ComponentName) as TClass(T);
end;
*)

function TCastleView.DesignedComponent(const ComponentName: String): TComponent;
begin
  if FDesignLoaded = nil then
    ErrorDesignLoaded;
  Result := FDesignLoadedOwner.FindRequiredComponent(ComponentName);
end;

function TCastleView.ContainerWidth: Cardinal;
begin
  Result := PredictedContainer.Width;
end;

function TCastleView.ContainerHeight: Cardinal;
begin
  Result := PredictedContainer.Height;
end;

function TCastleView.ContainerRect: TRectangle;
begin
  Result := PredictedContainer.Rect;
end;

function TCastleView.ContainerSizeKnown: boolean;
begin
  Result := true;
end;

function TCastleView.UIScale: Single;
begin
  if EnableUIScaling then
    Result := PredictedContainer.UIScale
  else
    Result := 1.0;
end;

class function TCastleView.GetCurrent: TCastleView;
begin
  Result := RequireMainContainer.View;
end;

class function TCastleView.GetCurrentTop: TCastleView;
begin
  Result := RequireMainContainer.FrontView;
end;

class procedure TCastleView.SetCurrent(const Value: TCastleView);
begin
  RequireMainContainer.View := Value;
end;

class procedure TCastleView.Push(const NewState: TCastleView);
begin
  RequireMainContainer.Push(NewState);
end;

class procedure TCastleView.Pop;
begin
  RequireMainContainer.Pop;
end;

class procedure TCastleView.Pop(const CurrentTopMostState: TCastleView);
begin
  RequireMainContainer.Pop(CurrentTopMostState);
end;

class function TCastleView.StateStackCount: Integer;
begin
  Result := RequireMainContainer.ViewStackCount;
end;

class function TCastleView.GetStateStack(const Index: Integer): TCastleView;
begin
  Result := RequireMainContainer.ViewStack[Index];
end;

{$endif read_implementation}
