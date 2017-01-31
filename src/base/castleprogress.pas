{
  Copyright 2002-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Progress bar functionality (TProgress, global variable Progress). }
unit CastleProgress;

{$I castleconf.inc}

{ Define this only for testing }
{ $define TESTING_PROGRESS_DELAY}

interface

uses SysUtils, CastleUtils, CastleTimeUtils;

type
  TProgress = class;

  { Abstract user interface of the progress bar.
    See @link(TProgress) for information how to use progress bars. }
  TProgressUserInterface = class
  private
    FBarYPosition: Single;
    FImage: TObject;
    FOwnsImage: boolean;
    procedure SetImage(const Value: TObject);
  public
    const
      DefaultBarYPosition = 0.5;

    constructor Create;
    destructor Destroy; override;

    { Image displayed as a background of the progress bar.

      Not all progress bar interfaces support it, some simply ignore it.
      You can leave it @nil, then the interface will use whatever is suitable
      (e.g. capture screen contents each time the progress bar starts).

      Whether the image assigned here is "owned" (that is, "automatically
      freed") by TProgressUserInterface instance depends on OwnsImage.
      In any case, we don't modify the image
      (if we need to resize it to fit the screen size,
      we do it on a temporary copy).

      The type of this must be @link(CastleImages.TRGBImage), but it cannot
      be declared as such here, we want this unit to be part of base
      units, not dependent on images. }
    property Image: TObject read FImage write SetImage;
    property OwnsImage: boolean read FOwnsImage write FOwnsImage default false;

    { Vertical position of the displayed progress bar.
      This feature is supposed to indicate a suitable free space on the
      background @link(Image) where we can nicely fit the progress bar UI.

      Not all progress bar interfaces support it, some simply ignore it.

      0 means the middle of progress bar is at the bottom of the image,
      1 means at the top. 0.5 indicates the middle, and it's the default. }
    property BarYPosition: Single read FBarYPosition write FBarYPosition
      default DefaultBarYPosition;

    { Deprecated name for BarYPosition. }
    property ImageBarYPosition: Single read FBarYPosition write FBarYPosition
      default DefaultBarYPosition; deprecated;

    { Show progress bar. }
    procedure Init(Progress: TProgress); virtual; abstract;
    { Update progress bar (because Progress.Position changed). }
    procedure Update(Progress: TProgress); virtual; abstract;
    { Hide progress bar. }
    procedure Fini(Progress: TProgress); virtual; abstract;
  end;

  { Progress bar functionality.

    This provides the functionality of a progress bar (everything that
    wants to signal progress should call @link(Progress) methods),
    but not the actual user interface. The user interface is "pluggable",
    that is you assign something to the Progress.UserInterface property.
    See the units:

    @unorderedList(
      @itemSpacing Compact
      @item(CastleWindowProgress --- show progress bar in OpenGL window)
      @item(CastleProgressConsole --- show progress bar on StdErr)
      @item(And you can also implement progress handling yourself,
        e.g. using Lazarus form or using Lazarus progress bar on existing form.)
    )

    This way any unit that implements some lengthy operation can call
    appropriate functions of the @link(Progress) object. And the final program
    can choose how it wants to show that progress to user (in console?
    in OpenGL window? etc.).

    Usage example:

    @longcode(#
      Progress.UserInterface := ... some TProgressUserInterface instance ...;
      ...
      Progress.Init(100, 'Doing something time-consuming, please wait');
      try
        for i := 1 to 100 do
        begin
          ... do something ...
          Progress.Step;
        end;
      finally Progress.Fini; end;
    #)

    Using @code("try ... finally ... end") above is not strictly required,
    but is strongly suggested. Rule of thumb says to always call
    Progress.Fini when you called Progress.Init.

    The @link(TProgress.Step) is implemented such that you don't have to
    worry about calling it too often. We will not update the interface
    (@link(TProgressUserInterface.Update)) too often,
    see TProgress.UpdatePart and TProgress.UpdateDelay for details.

    This unit creates one instance of the class @link(TProgress): @link(Progress).
    Usually this is what you want to use. For complicated cases,
    you can create and pass around more instances
    (e.g. from different threads, each @link(TProgress) object displaying
    it's state in a separate window.) }
  TProgress = class
  private
    FUserInterface: TProgressUserInterface;
    FUpdatePart: Cardinal;
    FUpdateDelay: TFloatTime;

    FMax, FPosition: Cardinal;
    { Variables below are meaningfull only if Active.

      When UserInterfaceDelayed, this is the time and position (always 0)
      of the TProgress.Init call.
      When not UserInterfaceDelayed, this is the time and position
      of the last TProgress.Init or TProgress.Update call. }
    LastUpdatePos: Cardinal;
    LastUpdateTime: TTimerResult;
    UserInterfaceDelayed: boolean;

    FTitle: string;
    FActive: boolean;
    procedure SetPosition(const Value: Cardinal);
  public
    const
      { }
      DefaultUpdatePart = {$ifdef TESTING_PROGRESS_DELAY} 100000000 {$else} 100 {$endif};
      DefaultUpdateDelay = {$ifdef TESTING_PROGRESS_DELAY} 0 {$else} 0.25 {$endif};

    property UserInterface: TProgressUserInterface
      read FUserInterface write FUserInterface;

    { Define how often to redraw interface (TProgressUserInterface.Update).
      Position must change by (1/UpdatePart) * Max and at the same time
      at least UpdateDelay seconds must pass to redraw.

      This allows you to call @link(Step) very often, without worrying
      that you cause redraw too often (which would cause slowdown).

      UpdateDelay default value is DefaultUpdateDelay.
      @groupBegin }
    property UpdatePart: Cardinal read FUpdatePart write FUpdatePart
      default DefaultUpdatePart;
    property UpdateDelay: TFloatTime read FUpdateDelay write FUpdateDelay;
    { @groupEnd }

    { Current Position of the progress bar.
      Always >= 0 and <= @link(Max).

      You can set this property only when @link(Active).
      Setting it to something > @link(Max) will be silently clamped to @link(Max).
      You can only increase it (trying to decrease it will be silently
      ignored, which is useful if your position information is only an
      approximation).
      In other words, setthing this property is equivalent
      to appropriate @link(Step) call. }
    property Position: Cardinal read FPosition write SetPosition;
    property Max: Cardinal read FMax;
    property Title: string read FTitle;

    { Are we between Init and Fini calls.
      Init changes Active to true, Fini changes Active to false. }
    property Active: boolean read FActive;

    { Start the progress bar.
      You can call Init only when Active = false (that is, you
      cannot Init while another progress is working).
      Initializes @link(Max), @link(Title), sets @link(Position) to 0 and
      changes @link(Active) to true.

      UserInterface must be initialized (non-nil) when calling
      Init, and you cannot change UserInterface when progress is Active
      (i.e. before you call Fini).

      If DelayUserInterface is set to @true, a very useful optimization
      is performed: TProgress.Init will not
      immediately result in TProgressUserInterface.Init call.
      Instead, actual initialization of the interface will be delayed
      until some TProgress.Update, when UpdateDelay time will pass.

      The advantage of DelayUserInterface is that if
      an operation will take a very short time, we will not waste
      time on possibly lengthy initialization of the progress bar
      interface. For example, CastleWindowProgress may have to capture OpenGL screen
      at the initialization, which takes a noticeable fraction of second
      by itself. So it's not sensible to init CastleWindowProgress if an entire
      operation between Progress.Init and Fini will take only 0.001 of second..

      The only downside of DelayUserInterface is that it's not applicable
      to an operation with very few steps (e.g. 1) that may take a long time.
      If a time between Init and the first Update or Fini is really large,
      the progress bar will not be visible. }
    procedure Init(AMax: Cardinal; const ATitle: string;
      const DelayUserInterface: boolean = false);

    { Increments progress bar @link(Position) by @code(StepSize).
      Use only when @link(Active), that is between @link(Init) and @link(Fini)
      calls.

      @link(Position) always stays <= @link(Max) (you can depend on this
      when implementaing TProgressUserInterface descendants).
      But it is legal to try to raise @link(Position) above
      @link(Max) by using this method, we will silently clamp @link(Position)
      to @link(Max).
      This is usefull when given @link(Max) was only an approximation of needed
      steps. }
    procedure Step(StepSize: Cardinal = 1);

    { Finish progress bar.
      You can call it only when Active = true (that is, if you called Init
      before). Fini changes Active to false.

      Note that it's perfectly legal to call Fini before Position
      reaches Max (it's sensible e.g. when you're allowing user to break
      some lenghty operation, or when Max was only an approximation
      of steps needed). }
    procedure Fini;

    constructor Create;
  end;

var
  { Global progress bar instance.
    Created in initialization of this unit, freed in finalization. }
  Progress: TProgress;

type
  TProgressNullInterface = class(TProgressUserInterface)
  public
    procedure Init(Progress: TProgress); override;
    procedure Update(Progress: TProgress); override;
    procedure Fini(Progress: TProgress); override;
  end;

var
  { A special progress user interface, that simply doesn't show progress anywhere.

    If you set Progress.UserInterface to this,
    then progress Init/Update/Fini will work --- but will not be displayed
    anywhere. This is done at the initialization of this unit,
    so you can safely use progress bars even before real interface
    is initialized.

    Created in initialization, freed in finalization. }
  ProgressNullInterface: TProgressNullInterface;

implementation

{ TProgressUserInterface ----------------------------------------------------- }

constructor TProgressUserInterface.Create;
begin
  inherited;
  FBarYPosition := DefaultBarYPosition;
end;

destructor TProgressUserInterface.Destroy;
begin
  if OwnsImage then
    FreeAndNil(FImage) else
    FImage := nil;
  inherited;
end;

procedure TProgressUserInterface.SetImage(const Value: TObject);
begin
  if FImage <> Value then
  begin
    if OwnsImage then
      FreeAndNil(FImage);
    FImage := Value;
  end;
end;

{ TProgress ------------------------------------------------------------------ }

procedure TProgress.Init(AMax: Cardinal; const ATitle: string;
  const DelayUserInterface: boolean);
begin
  Check(not Active, 'TProgress.Init error: progress is already active');
  FActive := true;

  Check(UserInterface <> nil,
    'TProgress.Init error: UserInterface not initialized');

  FPosition := 0;

  { Max(AMax, 1) secures us against AMax <= 0 values.

    (Otherwise, it would have to be secured at many places when calling
    Progress.Init, as sometimes AMax <= 0 values values can naturally
    occur. Consider e.g. building octree, when the VRML scene turns out
    to be empty.)

    The idea is that AMax <= 0 means that actually operation is already
    finished. So we'll set Max to 1 (to allow UserInterface to display it,
    since user interface can display only Max >= 1 values)
    and we'll do Step(1) immediately at the end of TProgress.Init,
    to show to user that operation is already done. }
  FMax := CastleUtils.Max(AMax, 1);

  FTitle := ATitle;

  { Calling UserInterface.Init updates LastUpdatePos and LastUpdateTime,
    just like calling UserInterface.Update. }
  LastUpdatePos := FPosition;
  LastUpdateTime := Timer;

  UserInterfaceDelayed := DelayUserInterface;

  if not UserInterfaceDelayed then
  try
    UserInterface.Init(Self);
  except
    { In case of problems within UserInterface.Init, call Fini
      to change our state to not Active. }
    Fini;
    raise;
  end;

  { This means that AMax < Max(AMax, 1), in other words: AMax <= 0.
    Then show to user that this operation actually finished. }
  try
    if AMax < Max then Step;
  except
    { In case of problems within UserInterface.Init, call Fini
      to change our state to not Active. }
    Fini;
    raise;
  end;
end;

procedure TProgress.Step(StepSize: Cardinal);
begin
  Assert(Active, 'TProgress.Step error: progress is not active');

  FPosition := FPosition + StepSize;
  if Position > Max then FPosition := Max;

  if UserInterfaceDelayed then
  begin
    { Either actually init user interface, or resign from calling
      UserInterface.Update. }
    if TimerSeconds(Timer, LastUpdateTime) > UpdateDelay then
    begin
      UserInterface.Init(Self);
      UserInterfaceDelayed := false;
    end else
      Exit;
  end;

  if ((Position - LastUpdatePos) / Max > 1 / UpdatePart) and
     (TimerSeconds(Timer, LastUpdateTime) > UpdateDelay) then
  begin
    LastUpdatePos := FPosition;
    LastUpdateTime := Timer;
    UserInterface.Update(Self);

    {$ifdef TESTING_PROGRESS_DELAY}
    Sleep(10);
    {$endif}
  end;
end;

procedure TProgress.SetPosition(const Value: Cardinal);
begin
  if Value > Position then
    Step(Value - Position);
end;

procedure TProgress.Fini;
begin
  Check(Active, 'TProgress.Fini error: progress is not active');
  FActive := false;

  if not UserInterfaceDelayed then
  begin
    { update to reflect the current state of Position, if needed.
      Note that this does NOT mean that at the end Position is = Max.
      Noone ever guarantees that -- you can call Fini before Position
      reaches Max. }
    if LastUpdatePos < Position then
      UserInterface.Update(Self);

    UserInterface.Fini(Self);
  end;
end;

constructor TProgress.Create;
begin
  inherited;
  UpdatePart := DefaultUpdatePart;
  UpdateDelay := DefaultUpdateDelay;
  FActive := false;
end;

{ TProgressNullInterface ----------------------------------------------------- }

procedure TProgressNullInterface.Init(Progress: TProgress);
begin
end;

procedure TProgressNullInterface.Update(Progress: TProgress);
begin
end;

procedure TProgressNullInterface.Fini(Progress: TProgress);
begin
end;

{ initialization / finalization ---------------------------------------------- }

initialization
  Progress := TProgress.Create;
  ProgressNullInterface := TProgressNullInterface.Create;
  { initialize Progress.UserInterface to null interface,
    this way Progress.Init etc. may be always safely called }
  Progress.UserInterface := ProgressNullInterface;
finalization
  FreeAndNil(Progress);
  FreeAndNil(ProgressNullInterface);
end.
