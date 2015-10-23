{
  Copyright 2015-2015 Michalis Kamburelis.

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

{ When defined, the non-OpenGL image data is kept loaded in memory.
  This uses more RAM, but allows faster GLContextOpen/Close on the state,
  as images do not need to be loaded from disk again. }
{ $define KEEP_LOADED_DATA_IMAGES}
{$ifdef ANDROID} {$undef KEEP_LOADED_DATA_IMAGES} {$endif}
{$ifdef iOS} {$undef KEEP_LOADED_DATA_IMAGES} {$endif}

interface

uses Classes, FGL,
  CastleConfig, CastleKeysMouse, CastleImages, CastleUIControls,
  CastleGLImages, CastleVectors, CastleRectangles;

type
  TUIStateList = class;

  TDataImageId = Integer;

  { UI state, a useful singleton to manage the state of your game UI.

    Only one state is @italic(current) at a given time, it can
    be get or set using the TUIState.Current property.
    (Unless you use TUIState.Push, in which case you build a stack
    of states, all of them are available at the same time.)

    Each state has comfortable @link(Start) and @link(Finish)
    methods that you can override to perform work when state becomes
    current, or stops being current.

    You can add/remove state-specific UI controls in various ways.
    You can add them in the constructor of this state (and then free in destructor),
    or add them in @link(Start), free in @link(Finish).

    @orderedList(
      @item(It's simplest and best to add/keep children controls as real
        children of the current state, so add them
        using methods @link(TUIControl.InsertFront) and
        @link(TUIControl.InsertBack).)

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
    so in case of events that
    can be "handled" (like TUIControl.Press, TUIControl.Release events)
    the state is notified about them only if no other state-specific
    UI control handled them.

    TODO: control children don't work like that now.

    This way state can

    @unorderedList(
      @item(catch press/release and similar events, when no other
        state-specific control handled them,)
      @item(catch update, GL context open/close and other useful events,)
      @item(can have it's own render function, to directly draw UI.)
    )

    See the TUIControl class for a lot of useful methods that you can
    override in your state descendants to capture various events. }
  TUIState = class(TUIControl)
  private
  type
    TDataImage = class
      URL: string;
      Image: TEncodedImage;
      GLImage: TGLImage;
      destructor Destroy; override;
    end;
    TDataImageList = specialize TFPGObjectList<TDataImage>;
  var
    FDataImages: TDataImageList;
    FStartContainer: TUIContainer;
    procedure InternalStart;
    procedure InternalFinish;

    class var FStateStack: TUIStateList;
    class function GetCurrent: TUIState; static;
    class procedure SetCurrent(const Value: TUIState); static;
    class function GetStateStack(const Index: Integer): TUIState; static;
  protected
    { Adds image to the list of automatically loaded images for this state.
      Path is automatically wrapped in ApplicationData(Path) to get URL.
      The OpenGL image resource (TGLImage) is loaded when GL context
      is active, available under DataGLImage(Id).
      Where Id is the return value of this method. }
    function AddDataImage(const Path: string): TDataImageId;
    function DataGLImage(const Id: TDataImageId): TGLImage;
    function DataImageRect(const Id: TDataImageId; const Scale: Single): TRectangle;

    { Container on which state works. By default, this is Application.MainWindow.
      When the state is current, then @link(Container) property (from
      ancestor, see TUIControl.Container) is equal to this. }
    function StateContainer: TUIContainer; virtual;

    { Position on @code(StateContainer.Controls) where we insert this state.
      By default, state is inserted as the front-most control, so position is equal
      to @code(StateContainer.Controls.Count). }
    function InsertAtPosition: Integer; virtual;
  public
    { Current state. In case multiple states are active (only possible
      if you used @link(Push) method), this is the bottom state.
      Setting this resets whole state stack. }
    class property Current: TUIState read GetCurrent write SetCurrent;

    { Pushing the state adds it above the @link(Current) state.

      The current state is conceptually at the bottom of state stack, always.
      When it is nil, then pushing new state sets the @link(Current) state.
      Otherwise @link(Current) state is left as-it-is, new state is added on top. }
    class procedure Push(const NewState: TUIState);
    class procedure Pop;

    class function StateStackCount: Integer;
    class property StateStack [const Index: Integer]: TUIState read GetStateStack;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { State becomes current.
      This is called right before adding the state to the
      @code(StateContainer.Controls) list, so the state methods
      GLContextOpen and Resize will be called next (as for all
      normal TUIControl). }
    procedure Start; virtual;

    { State is no longer current.
      This is called after removing the state from the
      @code(StateContainer.Controls) list.

      This is always called to finalize the started state.
      When the current state is destroyed, it's @link(Finish) is called
      too. So you can use this method to reliably finalize whatever
      you initialized in @link(Start). }
    procedure Finish; virtual;

    function Rect: TRectangle; override;
    procedure GLContextOpen; override;
    procedure GLContextClose; override;
  end;

  TUIStateList = class(specialize TFPGObjectList<TUIState>);

implementation

uses SysUtils,
  CastleWindow, CastleWarnings, CastleFilesUtils, CastleUtils,
  CastleTimeUtils, CastleLog;

{ TUIState.TDataImage ---------------------------------------------------------- }

destructor TUIState.TDataImage.Destroy;
begin
  FreeAndNil(Image);
  FreeAndNil(GLImage);
  inherited;
end;

{ TUIState --------------------------------------------------------------------- }

class function TUIState.GetCurrent: TUIState;
begin
  if (FStateStack = nil) or
     (FStateStack.Count = 0) then
    Result := nil else
    Result := FStateStack[0];
end;

class procedure TUIState.SetCurrent(const Value: TUIState);
begin
  { exit early if there's nothing to do }
  if (StateStackCount = 0) and (Value = nil) then
    Exit;
  if (StateStackCount = 1) and (FStateStack[0] = Value) then
    Exit;

  { Remove and finish topmost state.
    The loop is written to work even when some state Finish method
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
    { create FStateStack on demand now }
    if FStateStack = nil then
      FStateStack := TUIStateList.Create(false);
    FStateStack.Add(NewState);
    NewState.InternalStart;
  end;
end;

class procedure TUIState.Pop;
var
  TopState: TUIState;
begin
  TopState := FStateStack.Last;
  TopState.InternalFinish;
  if TopState = FStateStack.Last then
    FStateStack.Delete(FStateStack.Count - 1) else
    OnWarning(wtMinor, 'State', 'Topmost state is no longer topmost after its Finish method. Do not change state stack from state Finish methods.');
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

procedure TUIState.InternalStart;
begin
  Start;
  { actually insert, this will also call GLContextOpen  and Resize.
    However, check first that we're still the current state,
    to safeguard from the fact that Start changed state
    (like the loading state, that changes to play state immediately in start). }
  if FStateStack.IndexOf(Self) <> -1 then
    StateContainer.Controls.Insert(InsertAtPosition, Self);
end;

procedure TUIState.InternalFinish;
begin
  StateContainer.Controls.Remove(Self);
  Finish;
end;

function TUIState.StateContainer: TUIContainer;
begin
  if FStartContainer <> nil then
    { between Start and Finish, be sure to return the same thing
      from StateContainer method. Also makes it working when Application
      is nil when destroying state from CastleWindow finalization. }
    Result := FStartContainer else
    Result := Application.MainWindow.Container;
end;

constructor TUIState.Create(AOwner: TComponent);
begin
  inherited;
  FDataImages := TDataImageList.Create;
end;

destructor TUIState.Destroy;
begin
  { finish yourself and remove from FStateStack, if present there }
  if (FStateStack <> nil) and
     (FStateStack.IndexOf(Self) <> -1) then
  begin
    InternalFinish;
    FStateStack.Remove(Self);
    { deallocate empty FStateStack. Doing this here allows to deallocate
      FStateStack only once all states finished gracefully. }
    if FStateStack.Count = 0 then
      FreeAndNil(FStateStack);
  end;

  FreeAndNil(FDataImages);
  inherited;
end;

procedure TUIState.Start;
begin
  FStartContainer := StateContainer;
end;

procedure TUIState.Finish;
begin
  FStartContainer := nil;
end;

const
  { Shift data image id from 0, to avoid accidentally using uninitialized zero
    value to get the information for 1st image. This way passing 0 to DataGLImage
    or DataImageRect will always fail. }
  ShiftDataImageId = 10;

function TUIState.AddDataImage(const Path: string): TDataImageId;
var
  DI: TDataImage;
begin
  DI := TDataImage.Create;
  DI.URL := ApplicationData(Path);
  {$ifdef KEEP_LOADED_DATA_IMAGES}
  DI.Image := LoadEncodedImage(DI.URL, []);
  {$endif}
  if GLInitialized then
  begin
    {$ifndef KEEP_LOADED_DATA_IMAGES}
    DI.Image := LoadEncodedImage(DI.URL, []);
    {$endif}
    DI.GLImage := TGLImage.Create(DI.Image, true);
    {$ifndef KEEP_LOADED_DATA_IMAGES}
    FreeAndNil(DI.Image);
    {$endif}
  end;

  Result := FDataImages.Add(DI) + ShiftDataImageId;
end;

{ Do not make this public, to make outside code work
  regardless of KEEP_LOADED_DATA_IMAGES defined.
function TUIState.DataImage(const Index: TDataImageId): TCastleImage;
begin
  Result := FDataImages[Index - ShiftDataImageId].Image;
end;
}

function TUIState.DataGLImage(const Id: TDataImageId): TGLImage;
begin
  Result := FDataImages[Id - ShiftDataImageId].GLImage;
end;

function TUIState.DataImageRect(const Id: TDataImageId; const Scale: Single): TRectangle;
begin
  Result := FDataImages[Id - ShiftDataImageId].GLImage.Rect;
  Result.Width := Round(Result.Width * Scale);
  Result.Height := Round(Result.Height * Scale);
end;

function TUIState.Rect: TRectangle;
begin
  { 1. always capture events on whole container
    2. make child controls (anchored to us) behave like anchored to whole window. }
  Result := ParentRect;
end;

procedure TUIState.GLContextOpen;
var
  I: Integer;
  DI: TDataImage;
  StartTime: TProcessTimerResult;
begin
  inherited;
  if FDataImages.Count <> 0 then
  begin
    StartTime := ProcessTimerNow;
    for I := 0 to FDataImages.Count - 1 do
    begin
      DI := FDataImages[I];
      if DI.GLImage = nil then
      begin
        {$ifndef KEEP_LOADED_DATA_IMAGES}
        DI.Image := LoadEncodedImage(DI.URL, []);
        {$endif}
        DI.GLImage := TGLImage.Create(DI.Image, true);
        {$ifndef KEEP_LOADED_DATA_IMAGES}
        FreeAndNil(DI.Image);
        {$endif}
      end;
    end;
    WritelnLog('Loading', Format('Loading time of %d data images for state %s: %f',
      [FDataImages.Count, ClassName,
       ProcessTimerSeconds(ProcessTimerNow, StartTime)]));
  end;
end;

procedure TUIState.GLContextClose;
var
  I: Integer;
begin
  if FDataImages <> nil then
    for I := 0 to FDataImages.Count - 1 do
      FreeAndNil(FDataImages[I].GLImage);
  inherited;
end;

end.
