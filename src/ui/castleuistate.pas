{
  Copyright 2015-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ UI state (TUIState). }
unit CastleUIState;

{$I castleconf.inc}

interface

uses Classes, Generics.Collections,
  CastleConfig, CastleKeysMouse, CastleImages, CastleUIControls, CastleClassUtils,
  CastleGLImages, CastleVectors, CastleRectangles, CastleComponentSerialize;

type
  TUIStateList = class;

  { UI state, to manage the state of your game UI.
    See also
    https://castle-engine.io/manual_2d_user_interface.php#section_ui_state
    for an overview of using TUIState.

    In simple cases, only one state is @italic(current) at a given time,
    and it can be get or set using the @link(TUIState.Current) property.
    In more complex cases, you can use @link(TUIState.Push) and @link(TUIState.Pop)
    to build a stack of states, and in effect multiple states are active at the same time.
    All of the states on stack are @italic(started), but only the top-most is @italic(resumed).

    Each state has @link(Start) and @link(Stop)
    methods that you can override to perform work when state becomes
    part of the current state stack, or stops being part of it.
    You can also override @link(Resume) and @link(Pause) methods,
    to perform work when the state becomes the top-most state or is no longer
    the top-most state. The distinction becomes important once you play
    around with pushing/popping states.

    To define state user interface:

    @orderedList(
      @item(It is simplest to set @link(DesignUrl)
        to the design file you created using @url(https://castle-engine.io/manual_editor.php CGE editor).
        Such user interface controls will be created right before @link(Start)
        and destroyed right after @link(Stop) (so the state UI always "resets"
        when state starts).)

      @item(You can always create more UI controls and add them to the state at any point.
        The state is a @link(TCastleUserInterface) descendant and you can add UI to it
        just by using @link(TCastleUserInterface.InsertFront).

        UI children can be added anytime you want, e.g. in @link(Start) or in overridden
        constructor.

        UI children can be removed or destroyed anytime you want as well.
        You can use @link(FreeAtStop) as an owner for anything you want to be automatically
        destroyed at @link(Stop).)
    )

    Current state is placed on the list of container controls.
    This way state is notified about UI events, and can react to them.
    Note that our children will handle events @italic(before) the state itself
    is notified about them, following @link(TCastleUserInterface) events behavior.
    This way state can:

    @unorderedList(
      @item(React to @link(TCastleUserInterface.Press Press),
        @link(TCastleUserInterface.Release Release) of keys or mouse buttons,)

      @item(do something continuos in @link(TCastleUserInterface.Update Update).)
    )

    See the TCastleUserInterface class for a lot of useful methods that you can
    override in your state descendants to capture various events. }
  TUIState = class(TCastleUserInterface)
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

    procedure InternalStart;
    procedure InternalStop;
    procedure StartContainerFreeNotification(const Sender: TFreeNotificationObserver);
    { Stop yourself and remove from FStateStack, if present there. }
    procedure StopIfStarted;

    { Design* private routines }
    procedure SetDesignUrl(const Value: String);
    procedure SetDesignPreload(const Value: Boolean);
    procedure LoadDesign;
    procedure UnLoadDesign;
    procedure PreloadDesign;
    procedure UnPreloadDesign;

    class var FStateStack: TUIStateList;
    class var FDisableStackChange: Cardinal;
    class function GetCurrent: TUIState; static;
    class procedure SetCurrent(const Value: TUIState); static;
    class function GetCurrentTop: TUIState; static;
    class function GetStateStack(const Index: Integer): TUIState; static;
  protected
    { As the state knows about the container it will be put in (StateContainer),
      the state size is always known.
      It is known regardless if we are between Start / Stop,
      regardless if the state is already added to some Container.Items.
      This makes all other routines, like ParentRect, EffectiveRect, EffectiveWidth,
      EffectiveHeight also work. }
    function ContainerWidth: Cardinal; override;
    function ContainerHeight: Cardinal; override;
    function ContainerRect: TRectangle; override;
    function ContainerSizeKnown: boolean; override;

    { Container on which state works. By default, this is
      @link(TCastleApplication.MainWindow Application.MainWindow)
      if you use CastleWindow or
      @link(TCastleControl.MainControl) if you use CastleControl.
      When the state is current, then @link(Container) property (from
      ancestor, see TCastleUserInterface.Container) is equal to this. }
    function StateContainer: TCastleContainer; virtual;

    { Position on @code(StateContainer.Controls) where we insert this state.
      By default, state is inserted as the front-most control, so position is equal
      to @code(StateContainer.Controls.Count). }
    function InsertAtPosition: Integer; virtual;

    { Assign this component as owner for your controls,
      to make them freed during nearest @link(Stop). }
    function FreeAtStop: TComponent;
  public
    { When @true, state operations will send a log to CastleLog. }
    class var Log: boolean;

    { Current state. Simply assign to this property to change the current state.

      In case multiple states are active (only possible
      if you used the @link(Push) method), this property returns the @italic(bottom) state
      (use @link(CurrentTop) to get @italic(top) state).
      Setting this property resets whole state stack.

      @bold(When is it allowed to change the state?)

      While in theory you can change current state stack (assigning @link(TUIState.Current)
      or using @link(TUIState.Push) / @link(TUIState.Pop)) at any moment,
      but remember that stopping the state frees also the state UI.
      So you should not change the current state stack within events/overriden methods
      of classes like TCastleUserInterface, TCastleTransform, TCastleBehavior
      that could be destroyed by the state stop.

      The simpler advise is: @italic(Assign to @link(TUIState.Current) or use @link(TUIState.Push) / @link(TUIState.Pop)
      only from the overridden TUIState methods.
      Like TMyState.Update or TMyState.Press).

      Note that you cannot change current state stack when another change is in progress.
      That is, you cannot change state from within TMyState.Start/Resume/Pause/Stop.
    }
    class property Current: TUIState read GetCurrent write SetCurrent;

    { The top-most state.

      In case you used @link(Push), this returns the top-most (most recently pushed) state.

      If there is only one (or none) state, e.g. because you never used @link(Push),
      then this property returns the same thing as @link(Current). }
    class property CurrentTop: TUIState read GetCurrentTop;

    { Pushing the state adds it at the top of the state stack,
      this makes new state to be displayed on top of previous ones.

      The state known as @link(Current) is conceptually at the bottom of state stack, always.
      When it is nil, then pushing new state sets the @link(Current) state.
      Otherwise @link(Current) state is left as-it-is, new state is added on top. }
    class procedure Push(const NewState: TUIState);

    { Pop the current top-most state, reversing the @link(Push) operation. }
    class procedure Pop; overload;

    { Pop the current top-most state, reversing the @link(Push) operation,
      also checking whether the current top-most state is as expected.

      Makes a warning, and does nothing, if the current top-most state
      is different than indicated. This is usually a safer (more chance
      to easily catch bugs) version of Pop than the parameter-less version. }
    class procedure Pop(const CurrentTopMostState: TUIState); overload;

    { Count of states in the state stack.
      State stack is managed using Start / Push / Pop. }
    class function StateStackCount: Integer;

    { Access any state within the state stack.
      Use with indexes between 0 and StateStackCount - 1.
      State stack is managed using Start / Push / Pop. }
    class property StateStack [const Index: Integer]: TUIState read GetStateStack;

    { Create an instance of the state.
      You willl typically create one instance of each state class
      (like TStateMain, TStatePlay) at the application initialization
      (e.g. in Application.OnInitialize callback), like

      @longCode(#
        StateMainMenu := TStateMainMenu.Create(Application);
        StatePlay := TStatePlay.Create(Application);
      #)

      Later you switch between states using @link(Current) or @link(Push) or @link(Pop),
      like this:

      @longCode(#
        TUIState.Current := StateMain;
      #)

      See https://castle-engine.io/manual_2d_user_interface.php and numerous engine examples.
    }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Create the instance TUIState that will be automatically freed when
      the state is stopped.
      This allows alternative usage of states (as opposed to the ones described in @link(Create)
      docs), where you create short-lived instances of state classes.
      Use it like this:

      @longCode(#
        TUIState.Current := TStateMain.CreateUntilStopped;
      #)

      The advantages:

      @unorderedList(
        @item(You don't need to worry
          that some state field value will "survive" with an invalid value after
          @link(Stop). So you don't need to clear everything in @link(Stop)
          or initialize everything in @link(Start), instead you can depend
          that @link(Start) happens only once right after the constructor,
          so the instance fields are clear.)

        @item(You avoid having global variables, keeping singletons of each state class.
          So the code is a little safer.)

        @item(You can reintroduce your own constructor to require some parameters,
          instead of exposing state parameters as public fields/properties.)
      )

      The disadvantage is that you cannot store in state fields anything
      that should "survive" the state @link(Stop).
      You can instead use "class variables" in state class, or any global variable.
    }
    constructor CreateUntilStopped;

    { Executed when state becomes active, it's now part of the state stack.

      Started state is part of the StateStack, and will soon become
      running (top-most on the stack). When the state is set to be current,
      by @code(TUIState.Current := MyState), this happens:

      @orderedList(
        @item(MyState is pushed as the top-most state on state stack.)
        @item(MyState.Start is called.)
        @item(MyState is added to the @code(StateContainer.Controls) list,
          so the state methods GLContextOpen and Resize are called
          (as for all normal TCastleUserInterface instances).)
        @item(MyStar.Resume is called.)
      ) }
    procedure Start; virtual;

    { Executed when state is no longer active, no longer part of state stack.

      When the state stops becoming active, this happens:

      @orderedList(
        @item(MyState.Pause is called.)
        @item(MyState is removed from the
          @code(StateContainer.Controls) list.
          So the state method GLContextClose is called
          (as for all normal TCastleUserInterface instances).)
        @item(MyState.Stop is called.)
        @item(MyState is removed from the on state stack.)
      )

      This is always called to finalize the started state.
      When the state is destroyed, it's @link(Pause) and @link(Stop)
      are called too, so you can use this method to reliably finalize whatever
      you initialized in @link(Start). }
    procedure Stop; virtual;

    { Executed when state is now the top-most state. See @link(Start) and @link(Stop)
      docs about state lifecycle methods.
      This is called after @link(Start), it is also called
      when you pop another state, making this state the top-most. }
    procedure Resume; virtual;

    { Executed when state is no longer the top-most state. See @link(Start) and @link(Stop)
      docs about state lifecycle methods.
      This is called before @link(Stop), it is also called
      when another state is pushed over this state, so this stops
      being the the top-most state. }
    procedure Pause; virtual;

    procedure Finish; virtual; deprecated 'use Stop';

    { State is right now part of the state stack, which means
      it's between @link(Start) and @link(Stop) calls.
      The state is added to the stack before the @link(Start) call,
      and removed after the @link(Stop) call, so this returns @true
      during all the methods --- @link(Start), @link(Resume), @link(Pause), @link(Stop). }
    function Active: boolean;

    { Prevents passing mouse/keyboard events to the states underneath.

      More precisely, when this property is @true, then the
      @link(Press), @link(Release) and @link(Motion) events are marked as
      "handled" in this UI state. This means that they will not be processed
      further, by UI controls under this state, in particular by UI states
      that are underneath this state in @italic(state stack) (created
      by @link(Push) method). They will also not be passed to final container
      (TCastleWindow, TCastleControl) callbacks like
      TCastleWindow.OnPress (as these callbacks are always used at the end,
      when nothing else handled the event).

      Note that setting this to @true means that calling @code(inherited)
      from your @link(Press) overridden implementation will always return @true,
      as if the ancestor handled all the items. For this reason,
      in such case you should not immediately Exit when @code(inherited) is @true.
      You should just ignore the ancestor result, like this:

      @longCode(#
      function TMyState.Press(const Event: TInputPressRelease): Boolean;
      begin
        Result := inherited;
        // ignore the ancestor result, as we use InterceptInput, so ancestor always returns true
        // if Result the Exit;

        if Event.IsMouseButton(buttonLeft) then
        begin
          ...
          Exit(true);
        end;
      end;
      #)
    }
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

      This is an utility method, loading a UI in a typical way into the TUIState.
      It is not the only way to load a .castle-user-interface file,
      a more general approach is to use @link(UserInterfaceLoad) from
      @link(CastleComponentSerialize) unit, and manually call InsertFront
      to add it to the state UI.

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
      is freed when the state is stopped.
    }
    procedure InsertUserInterface(const ADesignUrl: String;
      const FinalOwner: TComponent;
      out Ui: TCastleUserInterface; out UiOwner: TComponent); overload; deprecated 'instead of this, set DesignUrl in constructor';
    procedure InsertUserInterface(const ADesignUrl: String;
      const FinalOwner: TComponent; out UiOwner: TComponent); overload; deprecated 'instead of this, set DesignUrl in constructor';

    { Wait until the render event happens (to redraw current state),
      and then call Event.

      The scheduled Event will be called at the @link(Update) time.

      If the state stopped before the scheduled events fired,
      then the remaining events are ignored.
      That is, the scheduled events are cleared every time you start the state.

      One use-case of this is to show a loading state,
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
      when this state is started.
      You typically set this property in overridden constructor, and in effect
      the given design file will be loaded right before @link(Start) and it will be freed
      right after @link(Stop).

      Typical use-case looks like this:

      @longCode(#
      constructor TStatePlay.Create(AOwner: TComponent);
      begin
        inherited;
        DesignUrl := 'castle-data:/gamestateplay.castle-user-interface';
        // DesignPreload := true; // to make future "TUIState.Current := StatePlay" faster
      end;

      procedure TStatePlay.Start;
      begin
        inherited;
        MyButton := DesignedComponent('MyButton') as TCastleButton;
      end;
      #)

      You can also modify this property when the state has already started
      (after @link(Start) and before @link(Stop)) in which case
      the previous design will be freed and new one will be loaded immediately.

      Set this to empty string (default) to mean "no design should be loaded".

      Note that this is not the only way to load a .castle-user-interface file.
      A more general approach is to use @link(UserInterfaceLoad),
      and call @link(InsertFront) to add it to the state UI.
      Using this property just adds some comfortable automatic behavior
      (state is freed at stop, and you can use comfortable
      @link(DesignedComponent) or @link(DesignPreload)).

      @seealso DesignPreload
      @seealso DesignedComponent
    }
    property DesignUrl: String read FDesignUrl write SetDesignUrl;

    { Preload the design file (specified in @link(DesignUrl)) as soon as possible,
      making starting the state much faster.
      Using this property means that we "preload" the design file,
      to cache the referenced images / TCastleScene instances etc. as soon as possible,
      to make the future loading of this design (when state starts) very quick.

      Typically you set this property in overridden TUIState constructor,
      right after (or before, it doesn't matter) setting DesignUrl.
      It will mean that constructor takes a longer time (which typically means
      that Application.OnInitialize takes a longer time) but in exchange
      future starting of the state (when you do e.g. @code(TUIState.Current := StateXxx)
      or @code(TUIState.Push(StateXxx)) will be much faster.

      No functional difference should be visible, regardless of the @link(DesignPreload)
      value. Internally the designed component is added/removed from state at the same time.
      So think of this property as pure optimization -- you decide whether to slow down
      the state constructor, or state start.

      @seealso DesignUrl }
    property DesignPreload: Boolean read FDesignPreload write SetDesignPreload default false;

    { When the DesignUrl is set, and the state is started, you can use this method to find
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
  published
    { TUIState control makes most sense when it is FullSize,
      filling the whole window.

      This way it always captures events on the whole container.
      And the child controls (anchored to this)
      behave like anchored to the whole container. }
    property FullSize default true;
  end;

  TUIStateList = class({$ifdef FPC}specialize{$endif} TObjectList<TUIState>);

implementation

uses SysUtils,
  CastleFilesUtils, CastleUtils, CastleTimeUtils, CastleLog;

{ Changing state during state Start/Stop/Push/Pop is not reliable,
  e.g. doing TUIState.Push during another TUIState.Push will not result
  in proper stack. }
procedure ErrorStackChangeDisabled(const Message: String);
begin
  raise EInternalError.Create(Message);
end;

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

{ TUIState --------------------------------------------------------------------- }

class function TUIState.GetCurrent: TUIState;
begin
  if (FStateStack = nil) or
     (FStateStack.Count = 0) then
    Result := nil else
    Result := FStateStack[0];
end;

class function TUIState.GetCurrentTop: TUIState;
begin
  if (FStateStack = nil) or
     (FStateStack.Count = 0) then
    Result := nil
  else
    Result := FStateStack[FStateStack.Count - 1];
end;

class procedure TUIState.SetCurrent(const Value: TUIState);
begin
  { exit early if there's nothing to do }
  if (StateStackCount = 0) and (Value = nil) then
    Exit;
  if (StateStackCount = 1) and (FStateStack[0] = Value) then
    Exit;

  if FDisableStackChange <> 0 then
    ErrorStackChangeDisabled('Cannot change TUIState.Current from inside of TUIState.Start/Resume/Pause/Stop');

  { Remove and finish topmost state.
    The loop is written to work even when some state Stop method
    changes states. }
  while StateStackCount <> 0 do
    Pop;
  { deallocate empty FStateStack }
  if Value = nil then
    FreeAndNil(FStateStack);

  Push(Value);
end;

class procedure TUIState.Push(const NewState: TUIState);
begin
  if NewState <> nil then
  begin
    if FDisableStackChange <> 0 then
      ErrorStackChangeDisabled('Cannot call TUIState.Push from inside of TUIState.Start/Resume/Pause/Stop');

    Inc(FDisableStackChange);
    try
      { pause previous top-most state }
      if (FStateStack <> nil) and
         (FStateStack.Count <> 0) then
        FStateStack.Last.Pause;

      { create FStateStack on demand now }
      if FStateStack = nil then
        FStateStack := TUIStateList.Create(false);
      FStateStack.Add(NewState);
      NewState.InternalStart;
      NewState.Resume;
    finally
      Dec(FDisableStackChange)
    end;
  end;
end;

class procedure TUIState.Pop;
var
  TopState: TUIState;
begin
  if FDisableStackChange <> 0 then
    ErrorStackChangeDisabled('Cannot call TUIState.Pop from inside of TUIState.Start/Resume/Pause/Stop');

  Inc(FDisableStackChange);
  try
    TopState := FStateStack.Last;
    TopState.Pause;
    TopState.InternalStop;
    if TopState = FStateStack.Last then
      FStateStack.Delete(FStateStack.Count - 1)
    else
      WritelnWarning('State', 'Topmost state is no longer topmost after its Stop method. Do not change state stack from state Stop methods.');

    if TopState.FFreeWhenStopped then
      FreeAndNil(TopState);

    { resume new top-most state }
    if (FStateStack <> nil) and
       (FStateStack.Count <> 0) then
      FStateStack.Last.Resume;
  finally
    Dec(FDisableStackChange)
  end;
end;

class procedure TUIState.Pop(const CurrentTopMostState: TUIState);
begin
  if (FStateStack = nil) or (FStateStack.Count = 0) then
  begin
    WritelnWarning('State', 'Cannot pop UI state, that stack is empty');
    Exit;
  end;
  if FStateStack.Last <> CurrentTopMostState then
  begin
    WritelnWarning('State', 'Cannot pop UI state, top-most state is expected to be ' + CurrentTopMostState.ClassName + ', but is ' + FStateStack.Last.ClassName);
    Exit;
  end;

  Pop;
end;

class function TUIState.StateStackCount: Integer;
begin
  if FStateStack = nil then
    Result := 0 else
    Result := FStateStack.Count;
end;

class function TUIState.GetStateStack(const Index: Integer): TUIState;
begin
  if FStateStack = nil then
    raise EInternalError.CreateFmt('TUIState.GetStateStack: state stack is empty, cannot get state index %d',
      [Index]);
  Result := FStateStack[Index];
end;

function TUIState.InsertAtPosition: Integer;
begin
  Result := StateContainer.Controls.Count;
end;

function TUIState.FreeAtStop: TComponent;
begin
  if FFreeAtStop = nil then
    FFreeAtStop := TComponent.Create(Self);
  Result := FFreeAtStop;
end;

procedure TUIState.InternalStart;
var
  TimeStart: TCastleProfilerTime;
begin
  TimeStart := Profiler.Start('Started state ' + Name + ': ' + ClassName);

  FWaitingForRender.Clear;
  FCallBeforeUpdate.Clear;

  { typically, the Start method will initialize some stuff,
    making the 1st SecondsPassed non-representatively large. }
  StateContainer.Fps.ZeroNextSecondsPassed;

  FStartContainer := StateContainer;
  FStartContainerObserver.Observed := FStartContainer;

  LoadDesign;

  Start;
  { actually insert, this will also call GLContextOpen and Resize.
    However, check first that we're still the current state,
    to safeguard from the fact that Start changed state
    (like the loading state, that changes to play state immediately in start). }
  if FStateStack.IndexOf(Self) <> -1 then
    StateContainer.Controls.Insert(InsertAtPosition, Self);

  Profiler.Stop(TimeStart, Log);
end;

procedure TUIState.InternalStop;
begin
  // when container is csDestroying, the StateContainer.Controls may be invalid
  if not (csDestroying in StateContainer.ComponentState) then
    StateContainer.Controls.Remove(Self);

  Stop;

  {$warnings off}
  Finish;
  {$warnings on}

  UnLoadDesign;

  FStartContainer := nil;
  FStartContainerObserver.Observed := nil;
  FreeAndNil(FFreeAtStop);

  if Log then
    WritelnLog('UIState', 'Stopped state ' + Name + ': ' + ClassName);
end;

procedure TUIState.StartContainerFreeNotification(
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

function TUIState.StateContainer: TCastleContainer;
begin
  if FStartContainer <> nil then
    { between Start and Stop, be sure to return the same thing
      from StateContainer method. Also makes it working when Application
      is nil when destroying state from CastleWindow finalization. }
    Result := FStartContainer
  else
  begin
    Result := GetMainContainer;
    if Result = nil then
      raise Exception.Create('Assign Application.MainWindow (if you use CastleWindow) or TCastleControl.MainControl (if you use CastleControl) before starting TUIState');
  end;
end;

constructor TUIState.Create(AOwner: TComponent);
begin
  inherited;
  FullSize := true;
  FWaitingForRender := TNotifyEventList.Create;
  FCallBeforeUpdate := TNotifyEventList.Create;
  FStartContainerObserver := TFreeNotificationObserver.Create(Self);
  FStartContainerObserver.OnFreeNotification := {$ifdef FPC}@{$endif}StartContainerFreeNotification;
end;

constructor TUIState.CreateUntilStopped;
begin
  Create(nil);
  FFreeWhenStopped := true;
end;

procedure TUIState.StopIfStarted;
begin
  if (FStateStack <> nil) and
     (FStateStack.IndexOf(Self) <> -1) then
  begin
    if FStateStack.Last = Self then
      Pause;
    InternalStop;
    FStateStack.Remove(Self);
    { deallocate empty FStateStack. Doing this here allows to deallocate
      FStateStack only once all states finished gracefully. }
    if FStateStack.Count = 0 then
      FreeAndNil(FStateStack);
  end;
end;

destructor TUIState.Destroy;
begin
  StopIfStarted;
  UnLoadDesign;
  UnPreloadDesign;
  FreeAndNil(FWaitingForRender);
  FreeAndNil(FCallBeforeUpdate);
  inherited;
end;

procedure TUIState.Resume;
begin
  if Log then
    WritelnLog('UIState', 'Resuming state ' + Name + ': ' + ClassName);
end;

procedure TUIState.Pause;
begin
  if Log then
    WritelnLog('UIState', 'Paused state ' + Name + ': ' + ClassName);
end;

procedure TUIState.Start;
begin
  { Do not place here any logic, to make sure TUIState works reliably even when
    descendant doesn't have "inherited" call.
    While we don't guarantee that our classes generally work OK in such situations
    (you should always call "inherited" unless documented otherwise),
    but in this special case we try to make it so, otherwise sometimes hard-to-debug
    errors occur. }
end;

procedure TUIState.Stop;
begin
  { Do not place here any logic, to make sure TUIState works reliably even when
    descendant doesn't have "inherited" call.
    Same as in Start. }
end;

procedure TUIState.Finish;
begin
end;

function TUIState.Active: boolean;
begin
  Result := (FStateStack <> nil) and
            (FStateStack.IndexOf(Self) <> -1);
end;

function TUIState.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  Result := Result or InterceptInput;
end;

function TUIState.Release(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  Result := Result or InterceptInput;
end;

function TUIState.Motion(const Event: TInputMotion): boolean;
begin
  Result := inherited;
  Result := Result or InterceptInput;
end;

procedure TUIState.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
begin
  inherited;

  { do not allow controls underneath to handle input }
  if InterceptInput then
    HandleInput := false;

  if FCallBeforeUpdate.Count <> 0 then // optimize away common case
  begin
    { In case of using CreateUntilStopped state, you cannot change state
      in the middle of FCallBeforeUpdate.ExecuteAll,
      as it would free the array that is being iterated over. }
    if FFreeWhenStopped then Inc(FDisableStackChange);
    try
      FCallBeforeUpdate.ExecuteAll(Self);
      FCallBeforeUpdate.Clear;
    finally
      if FFreeWhenStopped then Dec(FDisableStackChange);
    end;
  end;
end;

procedure TUIState.InsertUserInterface(const ADesignUrl: String;
  const FinalOwner: TComponent;
  out Ui: TCastleUserInterface; out UiOwner: TComponent);
begin
  UiOwner := TComponent.Create(FinalOwner);
  Ui := UserInterfaceLoad(ADesignUrl, UiOwner);
  InsertFront(Ui);
end;

procedure TUIState.InsertUserInterface(const ADesignUrl: String;
  const FinalOwner: TComponent; out UiOwner: TComponent);
var
  Ui: TCastleUserInterface;
begin
  {$warnings off} // using deprecated in deprecated
  InsertUserInterface(ADesignUrl, FinalOwner, Ui, UiOwner);
  {$warnings on}
  // ignore the returned Ui reference
end;

procedure TUIState.Render;
begin
  inherited;
  FCallBeforeUpdate.AddRange(FWaitingForRender);
  FWaitingForRender.Clear;
end;

procedure TUIState.WaitForRenderAndCall(const Event: TNotifyEvent);
begin
  FWaitingForRender.Add(Event);
end;

procedure TUIState.LoadDesign;
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

procedure TUIState.UnLoadDesign;
begin
  FreeAndNil(FDesignLoadedOwner);
  FDesignLoaded := nil; // freeing FDesignLoadedOwner must have freed this too
end;

procedure TUIState.PreloadDesign;
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

procedure TUIState.UnPreloadDesign;
begin
  { Note that it doesn't matter whether this FDesignPreloaded was used to load
    currently created FDesignLoaded or not.
    So FDesignLoaded* and FDesignPreloadedxx* are completely independent.
    This makes it easier to think about them. }
  FreeAndNil(FDesignPreloadedOwner);
  FDesignPreloaded := nil;// freeing FDesignPreloadedOwner must have freed this too
  FreeAndNil(FDesignPreloadedSerialized);
end;

procedure TUIState.SetDesignUrl(const Value: String);
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

procedure TUIState.SetDesignPreload(const Value: Boolean);
begin
  if FDesignPreload <> Value then
  begin
    UnPreloadDesign;
    FDesignPreload := Value;
    if FDesignPreload then
      PreloadDesign;
  end;
end;

procedure ErrorDesignLoaded;
begin
  raise Exception.Create('DesignedComponent can only be used if the desing was loaded, which means that TUIState has started and DesignUrl is not empty');
end;

(*
{ FPC 3.2.0 error:
    Error: function header doesn't match the previous declaration "DesignedComponent$1(const AnsiString):T;"
    Error: Found declaration: DesignedComponent$1(const AnsiString):T;
}
generic function TUIState.DesignedComponent<T: TComponent>(const ComponentName: String): T;
begin
  if FDesignLoaded = nil then
    ErrorDesignLoaded;
  Result := FDesignLoaded.FindRequiredComponent(ComponentName) as T;
end;

{ FPC 3.2.0 error:
    Error: Global Generic template references static symtable
  This works OK in a smaller testcase. }
generic function TUIState.DesignedComponent<T>(const ComponentName: String): T;
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

function TUIState.DesignedComponent(const ComponentName: String): TComponent;
begin
  if FDesignLoaded = nil then
    ErrorDesignLoaded;
  Result := FDesignLoadedOwner.FindRequiredComponent(ComponentName);
end;

function TUIState.ContainerWidth: Cardinal;
begin
  Result := StateContainer.Width;
end;

function TUIState.ContainerHeight: Cardinal;
begin
  Result := StateContainer.Height;
end;

function TUIState.ContainerRect: TRectangle;
begin
  Result := StateContainer.Rect;
end;

function TUIState.ContainerSizeKnown: boolean;
begin
  Result := true;
end;

function TUIState.UIScale: Single;
begin
  if EnableUIScaling then
    Result := StateContainer.UIScale
  else
    Result := 1.0;
end;

end.
