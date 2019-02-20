{
  Copyright 2015-2018 Michalis Kamburelis.

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
  CastleConfig, CastleKeysMouse, CastleImages, CastleUIControls,
  CastleGLImages, CastleVectors, CastleRectangles;

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
    The names are deliberaly similar to Android lifecycle callback names.

    You can add/remove state-specific UI controls in various ways.
    You can add them in the constructor of this state (and then free in destructor),
    or add them in @link(Start), free in @link(Stop).

    @orderedList(
      @item(It's simplest and best to add/keep children controls as real
        children of the current state, so add them
        using methods @link(TCastleUserInterface.InsertFront) and
        @link(TCastleUserInterface.InsertBack).)

      @item(Eventually, for special tricks, you can add controls that are
        conceptually the state "children" directly to the
        @code(StateContainer.Controls) list.
        This allows to keep some children on the @code(StateContainer.Controls)
        list for a longer
        time (not only when this state is active), which may be useful for optimization,
        to not reinitialize GL resources too often.
        To do this, add controls using
        @code(StateContainer.Controls.InsertFront(...)), remove them by
        @code(StateContainer.Controls.Remove(...)),
        and make sure to override @link(InsertAtPosition) method such that state instance
        is inserted in @code(StateContainer.Controls) right behind your UI.)
    )

    Current state is also placed on the list of container controls.
    This way state is notified
    about UI events, and can react to them. Since state-specific UI
    should always be at the front of us, or our children,
    so in case of events that can be "handled"
    (like TCastleUserInterface.Press, TCastleUserInterface.Release events)
    the state-specific UI controls will handle them @italic(before)
    the state itself (if you override TCastleUserInterface.Press or such in state,
    be sure to call @code(inherited) first, to make sure it really
    happens).

    This way state can

    @unorderedList(
      @item(catch press/release and similar events, when no other
        state-specific control handled them,)
      @item(catch update, GL context open/close and other useful events,)
      @item(can have it's own render function, to directly draw UI.)
    )

    See the TCastleUserInterface class for a lot of useful methods that you can
    override in your state descendants to capture various events. }
  TUIState = class(TCastleUserInterface)
  private
    FStartContainer: TUIContainer;
    FInterceptInput: boolean;
    FFreeAtStop: TComponent;
    procedure InternalStart;
    procedure InternalStop;

    class var FStateStack: TUIStateList;
    class var FDisableStackChange: Cardinal;
    class function GetCurrent: TUIState; static;
    class procedure SetCurrent(const Value: TUIState); static;
    class function GetCurrentTop: TUIState; static;
    class function GetStateStack(const Index: Integer): TUIState; static;
  protected
    { Container on which state works. By default, this is
      @link(TCastleApplication.MainWindow Application.MainWindow)
      if you use CastleWindow or
      @link(TCastleControlBase.MainControl) if you use CastleControl.
      When the state is current, then @link(Container) property (from
      ancestor, see TCastleUserInterface.Container) is equal to this. }
    function StateContainer: TUIContainer; virtual;

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

    { Current state. In case multiple states are active (only possible
      if you used @link(Push) method), this is the bottom state
      (use @link(CurrentTop) to get top state).
      Setting this resets whole state stack. }
    class property Current: TUIState read GetCurrent write SetCurrent;
    class property CurrentTop: TUIState read GetCurrentTop;

    { Pushing the state adds it at the top of the state stack.

      The state known as @link(Current) is conceptually at the bottom of state stack, always.
      When it is nil, then pushing new state sets the @link(Current) state.
      Otherwise @link(Current) state is left as-it-is, new state is added on top. }
    class procedure Push(const NewState: TUIState);

    { Pop the current top-most state, whatever it is. }
    class procedure Pop;

    { Pop the top-most state, checking it is as expected.
      Makes a warning, and does nothing, if the current top-most state
      is different than indicated. This is usually a safer (more chance
      to easily catch bugs) version of Pop than the parameter-less version. }
    class procedure Pop(const CurrentTopMostState: TUIState);

    class function StateStackCount: Integer;
    class property StateStack [const Index: Integer]: TUIState read GetStateStack;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { State becomes active, it's now part of the state stack.

      Started state is part of the StateStack, and will soon become
      running (top-most on the stack). When the state is set to be current,
      by @code(TUIState.Current := MyState), this happens:

      @orderedList(
        @item(MyStart is pushed as the top-most state on state stack.)
        @item(MyStart.Start is called.)
        @item(MyStart is added to the @code(StateContainer.Controls) list,
          so the state methods GLContextOpen and Resize are called
          (as for all normal TCastleUserInterface instances).)
        @item(MyStar.Resume is called.)
      ) }
    procedure Start; virtual;

    { State is no longer active, no longer part of state stack.

      When the state stops becoming active, this happens:

      @orderedList(
        @item(MyStart.Pause is called.)
        @item(MyStart is removed from the
          @code(StateContainer.Controls) list.
          So the state method GLContextClose is called
          (as for all normal TCastleUserInterface instances).)
        @item(MyStart.Stop is called.)
        @item(MyStart is removed from the on state stack.)
      )

      This is always called to finalize the started state.
      When the state is destroyed, it's @link(Pause) and @link(Stop)
      are called too, so you can use this method to reliably finalize whatever
      you initialized in @link(Start). }
    procedure Stop; virtual;

    { State is now the top-most state. See @link(Start) and @link(Stop)
      docs about state lifecycle methods.
      This is called after @link(Start), it is also called
      when you pop another state, making this state the top-most. }
    procedure Resume; virtual;

    { State is no longer the top-most state. See @link(Start) and @link(Stop)
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

    { Prevents passing mouse/keyboard events to the UI states underneath.

      More precisely, when this property is @true, then the
      @link(Press), @link(Release) and @link(Motion) events are marked as
      "handled" in this UI state. This means that they will not be processed
      further, by UI controls under this state, in particular by UI states
      that are underneath this state in @italic(state stack) (created
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
    procedure InsertUserInterface(const DesignUrl: String;
      const FinalOwner: TComponent;
      out Ui: TCastleUserInterface; out UiOwner: TComponent);
    procedure InsertUserInterface(const DesignUrl: String;
      const FinalOwner: TComponent; out UiOwner: TComponent);
  published
    { TUIState control makes most sense when it is FullSize,
      filling the whole window.

      This way it always captures events on the whole container.
      And the child controls (anchored to this)
      behave like anchored to the whole container. }
    property FullSize default true;
  end;

  TUIStateList = class(specialize TObjectList<TUIState>);

implementation

uses SysUtils,
  CastleFilesUtils, CastleUtils, CastleTimeUtils, CastleLog,
  CastleComponentSerialize;

{ TODO: Change this into always exception in the future.
  Changing state during state Start/Stop/Push/Pop is not reliable,
  e.g. doing TUIState.Push during another TUIState.Push will not result
  in proper stack. }
procedure ErrorStackChangeDisabled(const Message: String);
begin
  {$ifdef DEBUG}
  raise EInternalError.Create(Message);
  {$else}
  WritelnWarning('TUIState', Message);
  {$endif}
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
begin
  { typically, the Start method will initialize some stuff,
    making the 1st SecondsPassed non-representatively large. }
  StateContainer.Fps.ZeroNextSecondsPassed;

  if Log then
    WritelnLog('UIState', 'Starting state ' + Name + ':' + ClassName);
  Start;
  { actually insert, this will also call GLContextOpen and Resize.
    However, check first that we're still the current state,
    to safeguard from the fact that Start changed state
    (like the loading state, that changes to play state immediately in start). }
  if FStateStack.IndexOf(Self) <> -1 then
    StateContainer.Controls.Insert(InsertAtPosition, Self);
end;

procedure TUIState.InternalStop;
begin
  StateContainer.Controls.Remove(Self);
  Stop;
  if Log then
    WritelnLog('UIState', 'Stopped state ' + Name + ':' + ClassName);
end;

function TUIState.StateContainer: TUIContainer;
begin
  if FStartContainer <> nil then
    { between Start and Stop, be sure to return the same thing
      from StateContainer method. Also makes it working when Application
      is nil when destroying state from CastleWindow finalization. }
    Result := FStartContainer
  else
  begin
    if not Assigned(OnMainContainer) then
      raise Exception.Create('OnMainContainer not assigned. Use CastleWindow or CastleControl unit before starting TUIState');
    Result := OnMainContainer();
    if Result = nil then
      raise Exception.Create('Assign Application.MainWindow (if you use CastleWindow) or TCastleControl.MainControl (if you use CastleControl) before starting TUIState');
  end;
end;

constructor TUIState.Create(AOwner: TComponent);
begin
  inherited;
  FullSize := true;
end;

destructor TUIState.Destroy;
begin
  { finish yourself and remove from FStateStack, if present there }
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

  inherited;
end;

procedure TUIState.Resume;
begin
  if Log then
    WritelnLog('UIState', 'Resuming state ' + Name + ':' + ClassName);
end;

procedure TUIState.Pause;
begin
  if Log then
    WritelnLog('UIState', 'Paused state ' + Name + ':' + ClassName);
end;

procedure TUIState.Start;
begin
  FStartContainer := StateContainer;
end;

procedure TUIState.Stop;
begin
  {$warnings off}
  Finish;
  {$warnings on}

  FStartContainer := nil;
  FreeAndNil(FFreeAtStop);
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
  { do not allow controls underneath to handle input }
  if InterceptInput then
    HandleInput := false;
end;

procedure TUIState.InsertUserInterface(const DesignUrl: String;
  const FinalOwner: TComponent;
  out Ui: TCastleUserInterface; out UiOwner: TComponent);
begin
  UiOwner := TComponent.Create(FinalOwner);
  Ui := UserInterfaceLoad(DesignUrl, UiOwner);
  InsertFront(Ui);
end;

procedure TUIState.InsertUserInterface(const DesignUrl: String;
  const FinalOwner: TComponent; out UiOwner: TComponent);
var
  Ui: TCastleUserInterface;
begin
  InsertUserInterface(DesignUrl, FinalOwner, Ui, UiOwner);
  // ignore the returned Ui reference
end;

end.
