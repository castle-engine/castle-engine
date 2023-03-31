{
  Copyright 2002-2023 Michalis Kamburelis.

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

uses SysUtils, Classes, Generics.Collections,
  CastleUIControls, CastleUtils, CastleControls,
  CastleFonts, CastleTimeUtils, CastleVectors, CastleStringUtils,
  CastleColors, CastleRectangles, CastleClassUtils;

type
  { Notifications displayed on the screen.
    Each message disappears after a short time.
    Suitable for game messages like "Picked up 20 ammo".
    Call @link(Show) to display a message.

    This is a TCastleUserInterface descendant, so to use it --- just add it
    to TCastleWindow.Controls or TCastleControl.Controls.
    Use TCastleUserInterface anchors to automatically position it on the screen,
    for example:

    @longCode(#
      Notifications := TCastleNotifications.Create(Owner);
      Notifications.Anchor(hpMiddle);
      Notifications.Anchor(vpMiddle);
      Notifications.TextAlignment := hpMiddle; // looks best, when anchor is also in the middle
    #) }
  TCastleNotifications = class(TCastleUserInterfaceFont)
  private
    type
      TNotification = class
        Text: string;
        Time: TTimerResult; {< appear time }
        Width: Single;
        Color: TCastleColor;
      end;
      TNotificationList = class({$ifdef FPC}specialize{$endif} TObjectList<TNotification>)
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
    FDesignTestMessagesInterval: Single;
    FDesignTestMessagesTimeout: TFloatTime;
  protected
    procedure PreferredSize(var PreferredWidth, PreferredHeight: Single); override;
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
    function PropertySections(const PropertyName: String): TPropertySections; override;

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
      or when we need more space for new messages (see MaxMessages).

      Note that the @link(Fade) time is already included in this value,
      so a message is normally visible only for the @code(Timeout - Fade) seconds,
      and then it starts fading out. }
    property Timeout: Single
      read FTimeout write FTimeout {$ifdef FPC}default DefaultTimeout{$endif};

    property Fade: Single read FFade write FFade {$ifdef FPC}default DefaultFade{$endif};

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

    { In design mode (within CGE editor) we can spawn test messages (so you can see how
      the notifications look). This property determines how often we do it.
      0 means to not do it. }
    property DesignTestMessagesInterval: Single
      read FDesignTestMessagesInterval write FDesignTestMessagesInterval
      {$ifdef FPC}default 1{$endif};

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastlenotifications_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

implementation

uses Math,
  CastleLog, CastleComponentSerialize;

{ TNotificationList ---------------------------------------------------------- }

procedure TCastleNotifications.TNotificationList.DeleteFirst(DelCount: Integer);
var
  I: Integer;
begin
  { Could be optimized better, but this is simple and works correctly
    with TObjectList.FreeObjects = true management.
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
  FDesignTestMessagesInterval := 1;
  FDesignTestMessagesTimeout := FDesignTestMessagesInterval;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastlenotifications_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleNotifications.Destroy;
begin
  FreeAndNil(Messages);
  FreeAndNil(FHistory);

  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastlenotifications_persistent_vectors.inc}
  {$undef read_implementation_destructor}
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
  //WritelnLog('Notification', Trim(S.Text)); // too verbose by default

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
    History.AddRange(S);

  VisibleChange([chRectangle]);
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
  VisibleChange([chRectangle]);
end;

procedure TCastleNotifications.PreferredSize(var PreferredWidth, PreferredHeight: Single);
var
  I: integer;
begin
  inherited;
  PreferredWidth := 0;
  PreferredHeight := Font.Height * Messages.Count;
  for I := 0 to Messages.Count - 1 do
    PreferredWidth := Max(PreferredWidth, Messages[I].Width);
end;

procedure TCastleNotifications.Render;
var
  SR: TFloatRectangle;

  procedure ShowMessage(const MessageIndex, MessagesCount: Integer;
    const MessageColor: TCastleColor; const Text: String);
  begin
    Font.PrintRect(FloatRectangle(SR.Left,
      SR.Bottom + (MessagesCount - 1 - MessageIndex) * Font.Height,
      SR.Width, Font.Height), MessageColor, Text,
      TextAlignment, vpBottom);
  end;

var
  I: integer;
begin
  SR := RenderRect;
  for I := 0 to Messages.Count - 1 do
    ShowMessage(I, Messages.Count, Messages[I].Color, Messages[I].Text);
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
      VisibleChange([chRectangle]);
      break;
    end else
    if TimerSeconds(TimerNow, Messages[I].Time) > TimeoutToFade then
    begin
      C := Messages[I].Color;
      C.W := MapRange(TimerSeconds(TimerNow, Messages[I].Time),
        TimeoutToFade, Timeout, 1, 0);
      Messages[I].Color := C;
      VisibleChange([chRectangle]);
    end;

  if CastleDesignMode and (FDesignTestMessagesInterval <> 0) then
  begin
    FDesignTestMessagesTimeout := FDesignTestMessagesTimeout - SecondsPassed;
    if FDesignTestMessagesTimeout < 0 then
    begin
      Show('Test Message in Design Mode at ' + DateTimeToAtStr(CastleNow));
      FDesignTestMessagesTimeout := FDesignTestMessagesInterval;
    end;
  end;
end;

function TCastleNotifications.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'MaxMessages') or
     (PropertyName = 'Timeout') or
     (PropertyName = 'Fade') or
     (PropertyName = 'CollectHistory') or
     (PropertyName = 'TextAlignment') or
     (PropertyName = 'DesignTestMessagesInterval') or
     (PropertyName = 'ColorPersistent') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlenotifications_persistent_vectors.inc}
{$undef read_implementation_methods}

initialization
  RegisterSerializableComponent(TCastleNotifications, 'Notifications');
end.
