{
  Copyright 2002-2012 Michalis Kamburelis.

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

interface

uses GL, GLU, UIControls, Classes, SysUtils, CastleUtils, CastleGLUtils,
  OpenGLBmpFonts, OpenGLFonts, CastleTimeUtils, VectorMath, CastleStringUtils,
  FGL, CastleColors;

type
  THorizontalPosition = (hpLeft, hpMiddle, hpRight);
  TVerticalPosition = (vpDown, vpMiddle, vpUp);

const
  DefaultMaxMessages = 10;
  DefaultMessagesTimeout = 5000;
  DefaultHorizontalPosition = hpMiddle;
  DefaultVerticalPosition = vpDown;
  DefaultHorizontalMargin = 10;
  DefaultVerticalMargin = 1;

type
  { Internal type. @exclude }
  TNotification = class
    Text: string;
    Time: TMilisecTime; {< appear time }
  end;

  { Internal type. @exclude }
  TNotificationList = class(specialize TFPGObjectList<TNotification>)
    procedure DeleteFirst(DelCount: Integer);
  end;

  { Notifications displayed in the OpenGL window.
    The idea is to display messages about something happening
    at the bottom / top of the screen. These messages disappear by themselves
    after some short time.

    Similar to older FPS games messages, e.g. DOOM, Quake, Duke Nukem 3D.
    Suitable for game messages like "Picked up 20 ammo"
    or "Player Foo joined game".

    This is a TUIControl descendant, so to use it --- just add it
    to TCastleWindowCustom.Controls or TCastleControlCustom.Controls.
    Call @link(Show) to display a message. }
  TCastleNotifications = class(TUIControl)
  private
    { Messages, ordered from oldest (new mesages are added at the end).}
    Messages: TNotificationList;
    FHorizontalPosition: THorizontalPosition;
    FVerticalPosition: TVerticalPosition;
    FFont: TGLBitmapFont_Abstract;
    FColor: TVector3Single;
    FMaxMessages: integer;
    FTimeout: TMilisecTime;
    FHorizontalMargin, FVerticalMargin: Integer;
    FHistory: TCastleStringList;
    FCollectHistory: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure GLContextOpen; override;
    procedure GLContextClose; override;

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

    procedure Idle(const CompSpeed: Single;
      const HandleMouseAndKeys: boolean;
      var LetOthersHandleMouseAndKeys: boolean); override;

    procedure Draw; override;
    function DrawStyle: TUIControlDrawStyle; override;

    { Color used to draw messages. Default value is yellow. }
    property Color: TVector3Single read FColor write FColor;

    { Font used to draw messages. Read-only for now, in the future
      you should be allowed to change it. }
    property Font: TGLBitmapFont_Abstract read FFont;

    { All the messages passed to @link(Show), collected only if CollectHistory.
      May be @nil when not CollectHistory. }
    property History: TCastleStringList read FHistory;
  published
    { How many message lines should be visible on the screen, at maximum.  }
    property MaxMessages: integer
      read FMaxMessages write FMaxMessages default DefaultMaxMessages;

    { How long a given message should be visible on the screen, in miliseconds.
      Message stops being visible when this timeout passed,
      or when we need more space for new messages (see MaxMessages). }
    property Timeout: TMilisecTime
      read FTimeout write FTimeout default DefaultMessagesTimeout;

    property HorizontalPosition: THorizontalPosition read FHorizontalPosition
      write FHorizontalPosition default DefaultHorizontalPosition;
    property VerticalPosition: TVerticalPosition read FVerticalPosition
      write FVerticalPosition default DefaultVerticalPosition;

    { Margins, in pixels, from the border of the container (window or such).
      @groupBegin }
    property HorizontalMargin: Integer read FHorizontalMargin write FHorizontalMargin
      default DefaultHorizontalMargin;
    property VerticalMargin: Integer read FVerticalMargin write FVerticalMargin
      default DefaultVerticalMargin;
    { @groupEnd }

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
  end;

procedure Register;

implementation

uses BFNT_BitstreamVeraSans_Unit, CastleLog;

procedure Register;
begin
  RegisterComponents('Castle', [TCastleNotifications]);
end;

procedure TNotificationList.DeleteFirst(DelCount: Integer);
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
  Timeout := DefaultMessagesTimeout;
  FHorizontalPosition := DefaultHorizontalPosition;
  FVerticalPosition := DefaultVerticalPosition;
  FHorizontalMargin := DefaultHorizontalMargin;
  FVerticalMargin := DefaultVerticalMargin;
  FColor := Yellow3Single;
end;

destructor TCastleNotifications.Destroy;
begin
  FreeAndNil(Messages);
  FreeAndNil(FHistory);
  inherited;
end;

procedure TCastleNotifications.GLContextOpen;
begin
  inherited;
  if FFont = nil then
    FFont := TGLBitmapFont.Create(@BFNT_BitstreamVeraSans);
end;

procedure TCastleNotifications.GLContextClose;
begin
  FreeAndNil(FFont);
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
      N.Time := GetTickCount;
      Messages.Add(N);
    end;
  end;

var
  Broken: TStringList;
begin
  if Log then
    WriteLog('Time message', S.Text);

  { TODO: It's a bummer that we need Font created to make BreakLines,
    while BreakLines only really uses font metrics (doesn't need OpenGL
    font resources). }
  if ContainerSizeKnown and (Font <> nil) then
  begin
    Broken := TStringList.Create;
    try
      Font.BreakLines(s, Broken, ContainerWidth - HorizontalMargin * 2);
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
  strs: TStringList;
begin
  strs := TStringList.Create;
  try
    strs.Text := s;
    Show(strs);
  finally strs.Free end;
end;

procedure TCastleNotifications.Clear;
begin
  Messages.Clear;
  if CollectHistory then
    History.Clear;
  VisibleChange;
end;

procedure TCastleNotifications.Draw;
var
  i: integer;
  x, y: integer;
begin
  if DrawStyle = dsNone then Exit;

  glLoadIdentity;
  glColorv(Color);
  for i := 0 to Messages.Count-1 do
  begin
    { calculate x relative to 0..ContainerWidth, then convert to 0..GLMaxX }
    case HorizontalPosition of
      hpLeft  : x := HorizontalMargin;
      hpRight : x :=  ContainerWidth-Font.TextWidth(messages[i].Text) - HorizontalMargin;
      hpMiddle: x := (ContainerWidth-Font.TextWidth(messages[i].Text)) div 2;
    end;

    { calculate y relative to 0..ContainerHeight, then convert to 0..GLMaxY }
    case VerticalPosition of
      vpDown  : y := (Messages.Count-i-1) * Font.RowHeight + Font.Descend + VerticalMargin;
      vpMiddle: y := (ContainerHeight - Messages.Count * Font.RowHeight) div 2 + i*Font.RowHeight;
      vpUp   :  y :=  ContainerHeight-(i+1)*Font.RowHeight - VerticalMargin;
    end;

    { draw Text at position x, y }
    glRasterPos2i(x, y);
    Font.Print(Messages[i].Text);
  end;
end;

function TCastleNotifications.DrawStyle: TUIControlDrawStyle;
begin
  if GetExists and (Messages.Count <> 0) then
    Result := ds2D else
    Result := dsNone;
end;

procedure TCastleNotifications.Idle(const CompSpeed: Single;
  const HandleMouseAndKeys: boolean;
  var LetOthersHandleMouseAndKeys: boolean);
{ Check which messages should time out. }
var
  gtc: TMilisecTime;
  i: integer;
begin
  gtc := GetTickCount;
  for i := Messages.Count - 1 downto 0 do
    if TimeTickSecondLater(Messages[i].Time, gtc, Timeout) then
    begin { delete messages 0..I }
      Messages.DeleteFirst(I + 1);
      VisibleChange;
      break;
    end;

  { let controls under handle keys/mouse,
    because we don't do anything with them by default. }
  LetOthersHandleMouseAndKeys := true;
end;

end.
