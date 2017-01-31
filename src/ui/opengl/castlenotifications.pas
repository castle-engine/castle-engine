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

{ Notifications displayed in the OpenGL window (TCastleNotifications). }
unit CastleNotifications;

{$I castleconf.inc}

interface

uses FGL,
  CastleUIControls, Classes, SysUtils, CastleUtils, CastleControls,
  CastleFonts, CastleTimeUtils, CastleVectors, CastleStringUtils,
  CastleColors, CastleRectangles;

type
  { Notifications displayed on the screen.
    Each message disappears after a short time.
    Suitable for game messages like "Picked up 20 ammo".
    Call @link(Show) to display a message.

    This is a TUIControl descendant, so to use it --- just add it
    to TCastleWindowCustom.Controls or TCastleControlCustom.Controls.
    Use TUIControl anchors to automatically position it on the screen,
    for example:

    @longCode(#
      Notifications := TCastleNotifications.Create(Owner);
      Notifications.Anchor(hpMiddle);
      Notifications.Anchor(vpMiddle);
      Notifications.TextAlign := hpMiddle; // looks best, when anchor is also in the middle
    #) }
  TCastleNotifications = class(TUIControlFont)
  private
    type
      TNotification = class
        Text: string;
        Time: TTimerResult; {< appear time }
        Width: Integer;
        Color: TCastleColor;
      end;
      TNotificationList = class(specialize TFPGObjectList<TNotification>)
        procedure DeleteFirst(DelCount: Integer);
      end;
    var
    { Messages, ordered from oldest (new mesages are added at the end).}
    Messages: TNotificationList;
    FColor: TCastleColor;
    FMaxMessages: integer;
    FTimeout, FFade: Single;
    FHistory: TCastleStringList;
    FCollectHistory: boolean;
    FTextAlignment: THorizontalPosition;
  public
    const
      DefaultMaxMessages = 4;
      DefaultTimeout = 5.0;
      DefaultFade = 1.0;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Show new message. An overloaded version that takes a single string will
      detect newlines in the string automatically so a message may be multi-line.
      The messages will be automatically broken to fit on the screen width with
      given font.
      @groupBegin }
    procedure Show(const s: string); overload;
    procedure Show(s: TStringList); overload;
    { @groupEnd }

    { Clear all messages. }
    procedure Clear;

    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;

    procedure Render; override;
    function Rect: TRectangle; override;

    { Color used to draw subsequent messages. Default value is white. }
    property Color: TCastleColor read FColor write FColor;

    { All the messages passed to @link(Show), collected only if CollectHistory.
      May be @nil when not CollectHistory. }
    property History: TCastleStringList read FHistory;
  published
    { How many message lines should be visible on the screen, at maximum.  }
    property MaxMessages: integer
      read FMaxMessages write FMaxMessages default DefaultMaxMessages;

    { How long a given message should be visible on the screen, in seconds.
      Message stops being visible when this timeout passed,
      or when we need more space for new messages (see MaxMessages). }
    property Timeout: Single
      read FTimeout write FTimeout default DefaultTimeout;

    property Fade: Single read FFade write FFade default DefaultFade;

    { Turn this on to have all the messages you pass to @link(Show) be collected
      inside @link(History) string list. @link(History) is expanded by @link(Show),
      it is cleared by @link(Clear), just like the notifications on screen.
      However, unlike the visible messages, it has unlimited size
      (messages there are not removed when MaxMessages or @link(Timeout)
      take action), and messages inside are not broken to honour screen width.

      This is useful if you want to show the player a history of messages
      (in case they missed the message in game). }
    property CollectHistory: boolean read FCollectHistory write FCollectHistory
      default false;

    { Alignment of the text inside. }
    property TextAlignment: THorizontalPosition
      read FTextAlignment write FTextAlignment default hpLeft;
  end;

procedure Register;

implementation

uses CastleLog;

procedure Register;
begin
  RegisterComponents('Castle', [TCastleNotifications]);
end;

{ TNotificationList ---------------------------------------------------------- }

procedure TCastleNotifications.TNotificationList.DeleteFirst(DelCount: Integer);
var
  I: Integer;
begin
  { Could be optimized better, but this is simple and works correctly
    with TFPGObjectList.FreeObjects = true management.
    This is called only for really small DelCount values, so no problem. }
  for I := 1 to DelCount do Delete(0);
end;

{ TCastleNotifications ------------------------------------------------------- }

constructor TCastleNotifications.Create(AOwner: TComponent);
begin
  inherited;
  Messages := TNotificationList.Create;
  FHistory := TCastleStringList.Create;

  MaxMessages := DefaultMaxMessages;
  Timeout := DefaultTimeout;
  Fade := DefaultFade;
  FColor := White;
end;

destructor TCastleNotifications.Destroy;
begin
  FreeAndNil(Messages);
  FreeAndNil(FHistory);
  inherited;
end;

procedure TCastleNotifications.Show(S: TStringList);

  procedure AddStrings(S: TStrings);
  var
    N: TNotification;
    i: integer;
  begin
    { Below could be optimized. But we use this only for a small number
      of messages, so no need to. }
    for i := 0 to S.Count - 1 do
    begin
      if Messages.Count = MaxMessages then Messages.Delete(0);
      N := TNotification.Create;
      N.Text := S[i];
      N.Time := Timer;
      N.Width := Font.TextWidth(N.Text);
      N.Color := Color;
      Messages.Add(N);
    end;
  end;

var
  Broken: TStringList;
begin
  if Log then
    WritelnLog('Notification', S.Text);

  { before Notifications are part of some Controls list,
    we don't know about Parent or Container size. }
  if ContainerSizeKnown then
  begin
    Broken := TStringList.Create;
    try
      Font.BreakLines(S, Broken, ParentRect.Width);
      AddStrings(Broken);
    finally Broken.Free end;
  end else
    AddStrings(S);

  if CollectHistory then
    History.AddList(S);

  VisibleChange;
end;

procedure TCastleNotifications.Show(const s: string);
var
  Strs: TStringList;
begin
  Strs := TStringList.Create;
  try
    Strs.Text := s;
    Show(strs);
  finally Strs.Free end;
end;

procedure TCastleNotifications.Clear;
begin
  Messages.Clear;
  if CollectHistory then
    History.Clear;
  VisibleChange;
end;

function TCastleNotifications.Rect: TRectangle;
var
  I: integer;
begin
  Result := Rectangle(LeftBottomScaled, 0, Font.RowHeight * Messages.Count);
  for I := 0 to Messages.Count - 1 do
    Result.Width := Max(Result.Width, Messages[I].Width);
end;

procedure TCastleNotifications.Render;
var
  I: integer;
  SR: TRectangle;
begin
  SR := ScreenRect;
  for I := 0 to Messages.Count - 1 do
  begin
    Font.PrintRect(Rectangle(SR.Left,
      SR.Bottom + (Messages.Count - 1 - I) * Font.RowHeight,
      SR.Width, Font.RowHeight), Messages[i].Color, Messages[i].Text,
      TextAlignment, vpBottom);
  end;
end;

procedure TCastleNotifications.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
var
  TimerNow: TTimerResult;
  TimeoutToFade: TFloatTime;
  I: integer;
  C: TCastleColor;
begin
  inherited;
  TimerNow := Timer;
  TimeoutToFade := Timeout - Fade;
  for I := Messages.Count - 1 downto 0 do
    if TimerSeconds(TimerNow, Messages[I].Time) > Timeout then
    begin { delete messages 0..I }
      Messages.DeleteFirst(I + 1);
      VisibleChange;
      break;
    end else
    if TimerSeconds(TimerNow, Messages[I].Time) > TimeoutToFade then
    begin
      C := Messages[I].Color;
      C[3] := MapRange(TimerSeconds(TimerNow, Messages[I].Time),
        TimeoutToFade, Timeout, 1, 0);
      Messages[I].Color := C;
      VisibleChange;
    end;
end;

end.
