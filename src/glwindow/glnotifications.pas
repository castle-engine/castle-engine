{
  Copyright 2002-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Notifications displayed in the OpenGL window (TGLNotifications). }
unit GLNotifications;

interface

uses GL, GLU, GLWindow, Classes, SysUtils, KambiUtils, KambiGLUtils,
  OpenGLBmpFonts, OpenGLFonts, KambiTimeUtils, VectorMath;

{$define read_interface}

type
  { Internal TMessageStruct type. @exclude }
  TMessageStruct = record
    Text: string;
    Time: TMilisecTime; {< appear time }
  end;
  { @exclude }
  PMessageStruct = ^TMessageStruct;

  TDynArrayItem_1 = TMessageStruct;
  PDynArrayItem_1 = PMessageStruct;
  {$define DYNARRAY_1_IS_STRUCT}
  {$define DYNARRAY_1_IS_INIT_FINI_TYPE}
  {$I dynarray_1.inc}
  TDynMessageStructArray = TDynArray_1;

  THorizPosition = (hpLeft, hpMiddle, hpRight);
  TVertPosition = (vpDown, vpMiddle, vpUp);

  { Notifications displayed in the OpenGL window.
    The idea is to display messages about something happening
    at the bottom / top of the screen. These messages disappear by themselves
    after some short time.

    Similar to older FPS games messages, e.g. DOOM, Quake, Duke Nukem 3D.
    Suitable for game messages like "Picked up 20 ammo" or
    "Player Foo joined game".

    To use this:
    @unorderedList(
      @itemSpacing Compact
      @item(Create TGLNotifications instance, usually right when OpenGL
        is initialized (in TGLWindow.OnOpen).)
      @item(Destroy this instance usually right when OpenGL context
        is closed (in TGLWindow.OnClose).)
      @item(In your TGLWindow.OnDraw callback, when the 2D projection
        is active, draw it by @link(Draw2D) method.)
      @item(In your TGLWindow.OnIdle callback, call our @link(Idle) method.)
      @item(Finally, submit messages by calling our @link(Show Show('message'))
        method.)
    )

    TODO: rewrite this as TUIControl, then it will be easier to add it
    to window. Also, it will not be dependent on GLWindow then. }
  TGLNotifications = class
  private
    { Messages, ordered from oldest (new mesages are added at the end).}
    Messages: TDynMessageStructArray;
    FHorizMessgPosition: THorizPosition;
    FVertMessgPosition: TVertPosition;
    FDisplayPixelWidth: integer;
    FMessageFont: TGLBitmapFont_Abstract;
    FColor: TVector3Single;
    FMaxMessages: integer;
    FMessageTimeout: TMilisecTime;
    FWindow: TGLWindow;
    procedure PostRedisplayMessages;
  public
    { How many messages should be visible on the screen, at maximum.  }
    property MaxMessages: integer
      read FMaxMessages write FMaxMessages default 10;

    { How long a given message should be visible on the screen.
      Message stops being visible when it's visible for MessageTimeout
      time, or when we need more space for new messages (see MaxMessages). }
    property MessageTimeout: TMilisecTime
      read FMessageTimeout write FMessageTimeout default 5000;

    { Window where we send PostRedisplay when needed.
      May be @nil if you don't want us to send notifications when
      new message appears / old disappears (for example, maybe you use
      TGLWindow.AutoRedisplay = true). }
    property Window: TGLWindow read FWindow write FWindow;

    property HorizMessgPosition: THorizPosition read FHorizMessgPosition;
    property VertMessgPosition: TVertPosition read FVertMessgPosition;

    { When positive (> 0) then we know in advance the width of the screen.
      (PixelWidth for Draw2D.) This allows us to automatically break
      too long messages, during @link(Show), to make them fit. }
    property DisplayPixelWidth: integer read FDisplayPixelWidth;

    { New message. A version that takes a single string will detect
      newlines in the string (NL) automatically so a message may be multi-line. }
    procedure Show(const s: string); overload;
    procedure Show(s: TStrings); overload;

    { Clear all messages. }
    procedure Clear;

    procedure Idle;

    { Draw messages. May be called only when current matrix is modelview,
      it modifies the matrix (surround with glPushMatrix / glPopMatrix
      if you want).

      Assumes that the available screen is 0..GLMaxX, 0..GLMaxY.
      X values grow to the right, Y grow up.

      PixelWidth / PixelHeight say what is the size in pixels
      of GLMaxX, GLMaxY area. (We use bitmap fonts, so we have to know this.) }
    procedure Draw2D(GLMaxX, GLMaxY, PixelWidth, PixelHeight: integer);

    { Will Draw2D actually draw anything.
      This just checks whether we have any messages that we want to
      display --- if not, then you don't need to call Draw2D,
      as it will not do anything.

      This function is useful if you want to draw some background under
      TGLNotifications, like in "The Castle". Then you want to know whether
      the background is needed or not. }
    function DrawNeeded: boolean;

    { Color used to draw messages. Default value is yellow. }
    property Color: TVector3Single read FColor write FColor;

    { Font used to draw messages. Read-only for now, in the future
      you should be allowed to change it. }
    property MessageFont: TGLBitmapFont_Abstract read FMessageFont;

    { Create resources for displaying OpenGL notifications.
      You have to call constructor and destructor when the OpenGL
      context where the messages will be displayed is active. }
    constructor Create(AWindow: TGLwindow; AHorizMessgPosition: THorizPosition;
      AVertMessgPosition: TVertPosition; ADisplayPixelWidth: integer);
    destructor Destroy; override;
  end;

{$undef read_interface}

implementation

uses BFNT_BitstreamVeraSans_Unit, KambiLog;

{$define read_implementation}
{$I dynarray_1.inc}

{ TGLNotifications ------------------------------------------------------- }

const HorizMargin = 10; { marginesy wyswietlania, w pixelach }
      VertMargin = 1;

constructor TGLNotifications.Create(AWindow: TGLwindow;
  AHorizMessgPosition: THorizPosition; AVertMessgPosition: TVertPosition;
  ADisplayPixelWidth: integer);
begin
 inherited Create;
 Messages := TDynMessageStructArray.Create;
 MaxMessages := 10;
 MessageTimeout := 5000;
 Window := AWindow;
 FHorizMessgPosition := AHorizMessgPosition;
 FVertMessgPosition := AVertMessgPosition;
 FDisplayPixelWidth := ADisplayPixelWidth;

 FMessageFont := TGLBitmapFont.Create(@BFNT_BitstreamVeraSans);
 FColor := Yellow3Single;
end;

destructor TGLNotifications.Destroy;
begin
 FreeAndNil(FMessageFont);

 FreeAndNil(Messages);
 inherited;
end;

procedure TGLNotifications.PostRedisplayMessages;
begin
 if Window <> nil then Window.PostRedisplay;
end;

procedure TGLNotifications.Show(s: TStrings);

  procedure AddStrings(s: TStrings);
  var
    ms: TMessageStruct;
    i: integer;
  begin
    { Below could be optimized. But we use this only for a small number
      of messages, so no need to. }
    for i := 0 to s.Count-1 do
    begin
      if Messages.Count = MaxMessages then Messages.Delete(0, 1);
      ms.Text := s[i];
      ms.Time := GetTickCount;
      Messages.Add(ms);
    end;
  end;

var
  broken: TStringList;
begin
  if Log then
    WriteLog('Time message', S.Text);

  if DisplayPixelWidth > 0 then
  begin
    broken := TStringList.Create;
    try
      messageFont.BreakLines(s, broken, DisplayPixelWidth - HorizMargin*2);
      AddStrings(broken);
    finally broken.Free end;
  end else
    AddStrings(s);
  PostRedisplayMessages;
end;

procedure TGLNotifications.Show(const s: string);
var
  strs: TStringList;
begin
  strs := TStringList.Create;
  try
    strs.Text := s;
    Show(strs);
  finally strs.Free end;
end;

procedure TGLNotifications.Clear;
begin
  Messages.Clear;
  PostRedisplayMessages;
end;

procedure TGLNotifications.Draw2D(GLMaxX, GLMaxY, PixelWidth, PixelHeight: integer);
var
  i: integer;
  x, y: integer;
begin
  glLoadIdentity;
  glColorv(Color);
  for i := 0 to Messages.Count-1 do
  begin
    { calculate x relative to 0..PixelWidth, then convert to 0..GLMaxX }
    case HorizMessgPosition of
      hpLeft: x := HorizMargin;
      hpRight: x := PixelWidth-messageFont.TextWidth(messages.Items[i].Text)-HorizMargin;
      hpMiddle: x:=(PixelWidth-messageFont.TextWidth(messages.Items[i].Text)) div 2;
    end;
    x := x * GLMaxX div PixelWidth;

    { calculate y relative to 0..PixelHeight, then convert to 0..GLMaxY }
    case VertMessgPosition of
      vpDown: y:=(Messages.Count-i-1) * messageFont.RowHeight + messageFont.Descend + VertMargin;
      vpMiddle: y:=(PixelHeight - Messages.Count * messageFont.RowHeight) div 2 + i*messageFont.RowHeight;
      vpUp: y := PixelHeight-(i+1)*messageFont.RowHeight - VertMargin;
    end;
    y := y * GLMaxY div PixelHeight;

    { draw Text at position x, y }
    glRasterPos2i(x, y);
    messageFont.print(messages.Items[i].Text);
  end;
end;

function TGLNotifications.DrawNeeded: boolean;
begin
  Result := Messages.Count <> 0;
end;

procedure TGLNotifications.Idle;
{ Check which messages should time out. }
var
  gtc: TMilisecTime;
  i: integer;
begin
  gtc := GetTickCount;
  for i := Messages.Count-1 downto 0 do
    if TimeTickSecondLater(messages.Items[i].Time, gtc, MessageTimeout) then
    begin { delete messages 0..i }
      Messages.Delete(0, i+1);
      PostRedisplayMessages;
      break;
    end;
end;

end.
