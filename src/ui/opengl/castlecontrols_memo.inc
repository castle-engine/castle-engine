{%MainUnit castlecontrols.pas}
{
  Copyright 2017-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Edit box to input a single line of text.
    The contents are in the @link(Text) property, you can get or set them
    at any time.

    Note that you should set a suitable
    @link(Width TCastleUserInterface.Width) of the edit box,
    to nicely display a typical content.
    The height is by default auto-calculated (see @link(AutoSizeHeight))
    looking at font size (and other properties, like padding and frame size),
    and usually is reasonable automatically.
  }

  { TCastleMemo }

  TCastleMemo = class(TCastleUserInterfaceFont)
  strict private
    FIndex:integer;
    FIndexColumn:integer;
BottomText:Single;
    FPasswordChar: Char;
    FText: Tstrings;
    FPaddingHorizontal, FPaddingVertical, FPadding: Single;
    FBackgroundColor, FFocusedColor, FUnfocusedColor: TCastleColor;
    FPlaceholderColor: TCastleColor;
    FFrame: boolean;
    LifeTime: TFloatTime;
    FMaxLength: Cardinal;
    FAllowedChars: TSetOfChars;
    FCaptureAllInput: boolean;
    FAutoOnScreenKeyboard: Boolean;
    Scissor: TScissor;
    FAutoSizeHeight: Boolean;
    FPlaceholder: String;
    FOnChange: TNotifyEvent;
    FTextTranslate, FPlaceholderTranslate: Boolean;
    FBolScroll:Boolean;
    function GetCountRow: integer;
    procedure SetIndex(Value:integer);
    procedure SetIndexColumn(Value:integer);
    procedure SetCountRow(Value:integer);
    procedure SetText(const Value: TStrings);
    procedure SetFocusedColor(const Value: TCastleColor);
    //procedure SetText(AValue: TStrings);
    procedure SetUnfocusedColor(const Value: TCastleColor);
    procedure SetPlaceholderColor(const Value: TCastleColor);
    procedure SetBackgroundColor(const Value: TCastleColor);
    procedure SetPadding(const Value: Single);
    procedure SetPaddingHorizontal(const Value: Single);
    procedure SetPaddingVertical(const Value: Single);
    procedure SetFrame(const Value: boolean);
    procedure SetAutoSizeHeight(const Value: Boolean);
    procedure SetPlaceholder(const Value: String);
    procedure SetPasswordChar(const Value: Char);
  protected
    procedure PreferredSize(var PreferredWidth, PreferredHeight: Single); override;
    function GetInternalText: String; override;
    procedure SetInternalText(const Value:String); override;
    procedure DoChange; virtual;
    procedure TranslateProperties(const TranslatePropertyEvent: TTranslatePropertyEvent); override;
    procedure OnScrollChange(Scroll:TObject);
  public
    ScrollBar:TCastleScrollViewManual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
    function CapturesEventsAtPosition(const Position: TVector2): boolean; override;
    procedure SetFocused(const Value: boolean); override;
    procedure EditorAllowResize(
      out ResizeWidth, ResizeHeight: Boolean; out Reason: String); override;
    function PropertySection(const PropertyName: String): TPropertySection; override;
    property Index:integer read FIndex write SetIndex;
     property IndexColumn:integer read FIndexColumn write SetIndexColumn;
    { Focused text color. By default it's black. }
    property FocusedColor: TCastleColor read FFocusedColor write SetFocusedColor;

    { Unfocused text color. By default it's dark gray. }
    property UnfocusedColor: TCastleColor read FUnfocusedColor write SetUnfocusedColor;

    { Color for @link(Placeholder). By default it's light gray. }
    property PlaceholderColor: TCastleColor read FPlaceholderColor write SetPlaceholderColor;

    { Background color. Fills the area under @link(Text) and the padding.
      By default it's white.
      Note that it can be transparent, in which case it will show the
      tiEdit image underneath (if @link(Frame) is @true),
      or UI control underneath (if @link(Frame) is @false). }
    property BackgroundColor: TCastleColor read FBackgroundColor write SetBackgroundColor;

    { Allowed characters that user can input.
      Note that this @italic(only restricts the user input),
      it does not prevent from programmatically setting @link(Text)
      to include disallowed characters. }
    property AllowedChars: TSetOfChars read FAllowedChars write FAllowedChars;

    { Max length of user input.
      Value of 0 (default) means "no limit".
      Note that this @italic(only restricts the user input),
      it does not prevent from programmatically setting @link(Text)
      to be longer. }
    property MaxLength: Cardinal read FMaxLength write FMaxLength;
  published
    const
      DefaultPadding = 4;
      DefaultAutoOnScreenKeyboard = false;

    { Currently input text. }
    property Text: Tstrings read FText write SetText;
    property CountRow:integer read GetCountRow write SetCountRow default 1;
    { Inside the box rectangle, padding between the borders (or frame, if @link(Frame)) and text.
      Total horizontal padding is the sum @code(PaddingHorizontal + Padding),
      total vertical padding is the sum @code(PaddingVertical + Padding).
      @groupBegin }
    property PaddingHorizontal: Single
      read FPaddingHorizontal write SetPaddingHorizontal default 0;
    property PaddingVertical: Single
      read FPaddingVertical write SetPaddingVertical default 0;
    property Padding: Single
      read FPadding write SetPadding default DefaultPadding;
    { @groupEnd }

    { Draw frame around the box. Frame uses theme image tiEdit,
      see TCastleTheme.Images if you want to customize it. }
    property Frame: boolean read FFrame write SetFrame default true;

    { Should we capture input regardless of the currently focused control.
      This is in practice only sensible if this is the only edit box currently
      visible on the screen. }
    property CaptureAllInput: boolean read FCaptureAllInput write FCaptureAllInput;

    { When true, it shows the software keyboard on Android. }
    property AutoOnScreenKeyboard: Boolean
      read FAutoOnScreenKeyboard write FAutoOnScreenKeyboard
      default DefaultAutoOnScreenKeyboard;

    property AutoSizeHeight: Boolean
      read FAutoSizeHeight write SetAutoSizeHeight default false;

    { Displayed when @link(Text) is empty. }
    property Placeholder: String
      read FPlaceholder write SetPlaceholder;

    { Event sent when @link(Text) value was changed by a user.
      Note that this is @italic(not) called when you change @link(Text)
      property programmatically. }
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    { Show given character (usually "*") instead of actual input characters.
      Useful for password input fields. }
    property PasswordChar: Char
      read FPasswordChar write SetPasswordChar default #0;

    { Should the @link(Text) be localized (translated into other languages).
      Determines if the property is enumerated by @link(TCastleComponent.TranslateProperties),
      which affects the rest of localization routines. }
    property TextTranslate: Boolean read FTextTranslate write FTextTranslate default true;

    { Should the @link(Placeholder) be localized (translated into other languages).
      Determines if the property is enumerated by @link(TCastleComponent.TranslateProperties),
      which affects the rest of localization routines. }
    property PlaceholderTranslate: Boolean read FPlaceholderTranslate write FPlaceholderTranslate default true;
  end;

 {$endif read_interface}

{$ifdef read_implementation}

{ TCastleMemo --------------------------------------------------------------- }

constructor TCastleMemo.Create(AOwner: TComponent);
begin
  inherited;
    ScrollBar:=TCastleScrollViewManual.Create(Self);
  InsertFrontIfNotExists(ScrollBar);
  ScrollBar.FullSize:=True;
    ScrollBar.KeyScrollSpeed:=0;
    ScrollBar.PageSize:=1;
    BottomText:=0;
    ScrollBar.OnChange:=@OnScrollChange;
    ScrollBar.ActiveKeyHomeEnd:=False;
    ScrollBar.ActiveCtrlHomeEnd:=True;
    ScrollBar.Exists:=False;
  FIndex:=0;
  FIndexColumn:=-1;
  FFocusedColor := Black;
  FUnfocusedColor := Vector4(0.25, 0.25, 0.25, 1);
  FPlaceholderColor := Vector4(0.75, 0.75, 0.75, 1);
  FBackgroundColor := White;
  FPadding := DefaultPadding;
  FFrame := true;
  FAllowedChars := AllChars;
  FMaxLength := 0;
  FAutoOnScreenKeyboard := DefaultAutoOnScreenKeyboard;
  Scissor := TScissor.Create;
  FAutoSizeHeight := false;
  FTextTranslate := true;
  FPlaceholderTranslate := true;
  FText:=TStringList.Create;
  Index:=FText.Add('');
  FBolScroll:=False;
end;

destructor TCastleMemo.Destroy;
begin
  FreeAndNil(Scissor);
  FreeAndNil(ScrollBar);
  Text.Free;
  inherited;
end;

procedure TCastleMemo.SetIndex(Value: integer);
var
  i:Single;
begin
if (Value>-1) and (Value<Text.Count) then
begin
  i:=Font.RowHeight+UIScale * (PaddingVertical + Padding)*2;
  if Value>FIndex then
  begin
     if BottomText+Height<=i*(Index+2) then
       BottomText:=BottomText+i;
  end else
     if (i*(Index)<=BottomText) and (BottomText>0) then
       BottomText:=BottomText-i;
  FIndex:=Value;
  end;
if IndexColumn>Length(Text[Index])-2 then IndexColumn:=Length(Text[Index])-2;
FBolScroll:=True;
if ScrollBar.CompleteSize<>(Text.Count)-(Height/i) then
  ScrollBar.CompleteSize:=(Text.Count)-(Height/i);
  ScrollBar.Scroll:=Index;
  FBolScroll:=False;
end;

function TCastleMemo.GetCountRow: integer;
begin
   Result:=Text.Count;
end;

procedure TCastleMemo.SetIndexColumn(Value: integer);
begin
  if (Value>-2) and (Value<Length(Text[Index])) then
  begin
  FIndexColumn:=Value;
  end;
end;

procedure TCastleMemo.SetCountRow(Value: integer);
var
  i:integer;
  S1:TStringList;
begin
  if Value>0 then
  begin
  s1:=TStringList.Create;
  for i:=0 to Value-1 do
  if Value<Text.Count then
    S1.Add(Text[i]) else s1.Add('');
Text.Clear;
Text.AddText(S1.Text);
s1.Free;
if Value>1 then
     ScrollBar.Exists:=True else ScrollBar.Exists:=False;
  end;
end;


procedure TCastleMemo.SetText(const Value: TStrings);
begin
  if FText <> Value then
  begin
    FText.Clear;
    FText.AddStrings (Value);
    VisibleChange([chRender]);
  end;
end;

procedure TCastleMemo.SetFocusedColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FFocusedColor, Value) then
  begin
    FFocusedColor := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleMemo.SetUnfocusedColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FUnfocusedColor, Value) then
  begin
    FUnfocusedColor := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleMemo.SetPlaceholderColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FPlaceholderColor, Value) then
  begin
    FPlaceholderColor := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleMemo.SetBackgroundColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FBackgroundColor, Value) then
  begin
    FBackgroundColor := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleMemo.SetPadding(const Value: Single);
begin
  if FPadding <> Value then
  begin
    FPadding := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleMemo.SetPaddingHorizontal(const Value: Single);
begin
  if FPaddingHorizontal <> Value then
  begin
    FPaddingHorizontal := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleMemo.SetPaddingVertical(const Value: Single);
begin
  if FPaddingVertical <> Value then
  begin
    FPaddingVertical := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleMemo.SetFrame(const Value: boolean);
begin
  if FFrame <> Value then
  begin
    FFrame := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleMemo.SetAutoSizeHeight(const Value: Boolean);
begin
  if FAutoSizeHeight <> Value then
  begin
    FAutoSizeHeight := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleMemo.PreferredSize(var PreferredWidth, PreferredHeight: Single);
var
  PaddingVerticalScaled, HeightScaled: Single;
  US: Single;
  FrameCorners: TVector4;
begin
  inherited;

  if AutoSizeHeight then
  begin
    US := UIScale;
    PaddingVerticalScaled := US * (PaddingVertical + Padding);

    HeightScaled := Font.RowHeight + 2 * PaddingVerticalScaled;
    if Frame then
    begin
      FrameCorners := Theme.Corners[tiMemo];
      HeightScaled := HeightScaled +
        US * FrameCorners[0] +
        US * FrameCorners[2];
    end;

    PreferredHeight := HeightScaled;
  end else
  begin
       US := UIScale;
    PaddingVerticalScaled := US * (PaddingVertical + Padding);

    HeightScaled := Height;
    if Frame then
    begin
      FrameCorners := Theme.Corners[tiMemo];
      HeightScaled := HeightScaled +
        US * FrameCorners[0] +
        US * FrameCorners[2];
    end;

    PreferredHeight := HeightScaled;
  end;
end;

procedure TCastleMemo.DoChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TCastleMemo.EditorAllowResize(
  out ResizeWidth, ResizeHeight: Boolean; out Reason: String);
begin
  inherited;
  if AutoSizeHeight then
  begin
    ResizeHeight := false;
    Reason := SAppendPart(Reason, NL, 'Turn off "TCastleMemo.AutoSizeHeight" to change height.');
  end;
end;

function TCastleMemo.PropertySection(const PropertyName: String): TPropertySection;
begin
  if (PropertyName = 'Text') then
    Result := psBasic
  else
    Result := inherited PropertySection(PropertyName);
end;

procedure TCastleMemo.Render;
const
  CaretSpeed = 1; //< how many blinks per second
var
  SR, RectInside: TFloatRectangle;
  PaddingHorizontalScaled, PaddingVerticalScaled: Single;
  US, TextLeft, TextBottom: Single;
  FrameCorners: TVector4;
  CaretVisible: boolean;
  S: string;
  Color: TCastleColor;
  CaretShift,HeightText: Single;
  i:integer;
begin
  inherited;

  US := UIScale;
  RectInside := RenderRect;
     if AutoSizeHeight then
     RectInside:=RectInside.GrowBottom(RectInside.Height*(Text.Count-1));
       SR := RectInside;
  if Frame then
  begin
    { make smaller RectInside, by frame size }
    FrameCorners := Theme.Corners[tiMemo];
    RectInside := RectInside.
      RemoveTop(FrameCorners[0] * US).
      RemoveRight(FrameCorners[1] * US).
      RemoveBottom(FrameCorners[2] * US).
      RemoveLeft(FrameCorners[3] * US);

    { draw frame }
    Theme.Draw(SR, tiMemo, US);
  end;

  { fill padding and area under Text with BackgroundColor }
  DrawRectangle(RectInside, BackgroundColor);

  { make smaller RectInside, by padding }
  PaddingHorizontalScaled := US * (PaddingHorizontal + Padding);
  PaddingVerticalScaled := US * (PaddingVertical + Padding);
  //RectInside := RectInside.Grow(-PaddingHorizontalScaled, -PaddingVerticalScaled);
  // calculate S, Color, CaretShift
  CaretShift := 0;
  if AutoSizeHeight then
   HeightText:=Height else
     begin
      HeightText:=(Font.RowHeight+PaddingVerticalScaled*2)*Text.Count;
       end;

  if Text<>nil then
  for i:=0 to Text.Count -1 do
  begin
    S :=Text[i];
    S:=UTF8Copy(s,0,5);
    if Focused then
    begin
    if i=Index then
      CaretShift := Font.TextWidth(Copy(s,0,IndexColumn+1));
      Color := FocusedColor
    end else
      Color := UnfocusedColor;
     S :=UTF8Copy(s,0,IndexColumn+1)+S;
  Scissor.Rect := RectInside.Round;
  Scissor.Enabled := true;

  TextLeft := RectInside.Left;
  // center in RectInside, important when AutoSize = false
  TextBottom := (RectInside.Bottom+(Height-HeightText) +
    ( HeightText/Text.Count - Font.RowHeight) / 2 + Font.Descend)+HeightText/Text.Count*(Text.Count-i-1); //+FPaddingVertical
  Font.Print(TextLeft, BottomText+TextBottom, Color, S);

  { Note that we have to display Caret even when Text = '' and S = Placeholder. }
  if Focused  and (i=Index) then
  begin
    CaretVisible := FloatModulo(LifeTime * CaretSpeed, 1.0) < 0.5;
    if CaretVisible then
      Font.Print(TextLeft + CaretShift,BottomText+ TextBottom, FocusedColor, CaretChar);
  end;
  end;
  Scissor.Enabled := false;
end;

procedure TCastleMemo.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
begin
  inherited;
  LifeTime := LifeTime + SecondsPassed;
  { do not allow other controls to handle key presses
    (and e.g. move camera when "A" is pressed down), TCastleMemo handles all keys }
  if ExclusiveEvents then
    HandleInput := false;
end;

function TCastleMemo.Press(const Event: TInputPressRelease): boolean;
var
  s:string;
  i:integer;
begin
  Result := inherited;
  if Result or (not GetExists) then Exit;

  if Event.IsKey(CharBackSpace) or Event.IsKey(K_BackSpace) then
  begin
      s:=Text[index];
      if s<>'' then
      begin

      Delete(s,indexColumn+1-UTF8Length(Text[index][IndexColumn]),UTF8Length(Text[index][IndexColumn]));
      IndexColumn:=IndexColumn-UTF8Length(Text[index][IndexColumn]);
      Text[index]:=s+Text[index][IndexColumn];

      end else if Index>0 then
      Text.Delete(index);

      begin
      Index:=Index-1;
       IndexColumn:=IndexColumn-UTF8Length(Text[index][IndexColumn]);
       end;
      DoChange;
    Result := ExclusiveEvents;
  end else
    if Event.IsKey(K_Delete) then
  begin
      s:=Text[index];
      if s<>'' then
          begin
      Delete(s,indexColumn+1,Length(Text[Index][IndexColumn+1]));
      Text[index]:=s;
      end else  if Index<Text.Count-1 then
      begin
      if Text.Count>1 then
      Text.Delete(Index);
      end;
      DoChange;
    Result := ExclusiveEvents;
  end else
  if Event.IsKey(CtrlC) then
  begin
      Clipboard.AsText := Text[Index];
    Result := ExclusiveEvents;
  end else
  if Event.IsKey(CtrlX) then
  begin
    if Text[Index] <> '' then
    begin
      Clipboard.AsText := Text[Index];
      Text[Index] := '';
      DoChange;
    end;
    Result := ExclusiveEvents;
  end else
  if Event.IsKey(CtrlV) then
  begin
    if (Clipboard.AsText.IndexOf(#13)>-1) then
    Text.AddText(Clipboard.AsText)
    else
    Text[Index]:=Text[Index].Insert(IndexColumn+1,Clipboard.AsText);
    DoChange;
    Result := ExclusiveEvents;
  end else
  if Event.IsKey(keyUp) then
  begin
  index:=Index-1;
  end
  else
    if Event.IsKey(keyDown) then
    begin
 index:=Index+1;
    end
  else
  if Event.IsKey(keyLeft) then
  IndexColumn:=indexColumn-Length(Text[Index][IndexColumn-1])
  else
    if Event.IsKey(keyRight) then
  IndexColumn:=IndexColumn+Length(Text[Index][IndexColumn+1])
  else
    if Event.IsKey(keyHome) and (mkCtrl  in Event.ModifiersDown) then
    begin
     index:=0;
     IndexColumn:=0;
    end
    else
     if Event.IsKey(KeyEnd) and (mkCtrl  in Event.ModifiersDown) then
     begin
    index:=Text.Count-1;
      IndexColumn:=Length(Text[Index])-1;
     end
    else
         if Event.IsKey(KeyEnd) then
    IndexColumn:=Length(Text[Index])-1
    else
    if Event.IsKey(keyHome) then
    IndexColumn:=-1
    else
  if Event.IsKey(K_Enter) then
  begin
  i:=UTF8Length(UTF8Copy(Text[index],IndexColumn,UTF8Length(text[index])-IndexColumn));
    Text.Insert(Index+1,UTF8Copy(Text[index],IndexColumn,Length(text[index])-IndexColumn));   //UTF8Copy(Text[Index],IndexColumn+2,Length(Text[index])-IndexColumn+Length(Text[Index][Length(Text[Index])])));
    s:=Text[index];
    Delete(s,IndexColumn+2,utf8Length(Text[index])-IndexColumn+1);
    Text[index]:=s;
    Index:=Index+i;
    IndexColumn:=-1;
    DoChange;
  end
  else
  if (Event.EventType = itKey) and
     (Event.KeyString <> '') and
     (Event.KeyCharacter in AllowedChars) and
     ((MaxLength = 0) or (Length(Text[Index]) < MaxLength)) and
     { Do not add unprintable special characters, like Enter and Escape.
       TCastleMemo doesn't process them,
       but also users don't expect these keys are something added to the edit field.
       Note: The range CtrlA..CtrlZ includes CharEnter. }
     (not (Event.KeyCharacter in [CtrlA..CtrlZ, CharEscape])) then
  begin
    s:=Text[index];
    Insert(Event.KeyString,s,IndexColumn+1);
    Text[index]:=s;
    IndexColumn:=IndexColumn+1;
    DoChange;
    Result := ExclusiveEvents;
  end;
 end;

function TCastleMemo.CapturesEventsAtPosition(const Position: TVector2): boolean;
begin
  if CaptureAllInput then
    Result := true // always capture
  else
    Result := inherited CapturesEventsAtPosition(Position);
end;

procedure TCastleMemo.SetFocused(const Value: boolean);
begin
  inherited;

  if FAutoOnScreenKeyboard then
    Messaging.Send(['change-keyboard-state', TMessaging.BoolToStr(Value)])
end;

procedure TCastleMemo.SetPlaceholder(const Value: String);
begin
  if FPlaceholder <> Value then
  begin
    FPlaceholder := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleMemo.SetPasswordChar(const Value: Char);
begin
  if FPasswordChar <> Value then
  begin
    FPasswordChar := Value;
    VisibleChange([chRender]);
  end;
end;

function TCastleMemo.GetInternalText: String;
begin
  Result := Text.Text;
end;

procedure TCastleMemo.SetInternalText(const Value: String);
begin
  Text.Text := Value;
end;

procedure TCastleMemo.TranslateProperties(
  const TranslatePropertyEvent: TTranslatePropertyEvent);
var
  S: String;
begin
  if TextTranslate and (Text[Index] <> '') then
  begin
    S := Text[Index];
    TranslatePropertyEvent(Self, 'Text', S);
    Text[Index] := S;
  end;

  if PlaceholderTranslate and (Placeholder <> '') then
  begin
    S := Placeholder;
    TranslatePropertyEvent(Self, 'Placeholder', S);
    Placeholder := S;
  end;
end;

procedure TCastleMemo.OnScrollChange(Scroll: TObject);
var
  i:Single;
begin
  if FBolScroll=False then
  begin
  i:=Font.RowHeight+UIScale * (PaddingVertical + Padding)*2;
  FIndex:=round(ScrollBar.Scroll);
  BottomText:=index*i;

  end;
end;

{$endif read_implementation}
