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
   type

    { TMemoIndexs }

    TMemoIndexs = record
      Index1, IndexColumn1 ,Index2 ,IndexColumn2: integer;
    end;
  { TCastleMemo }

  TCastleMemo = class(TCastleUserInterfaceFont)
  strict private
    TextLeft: Single;
    FIndex, FLastIndex: integer;
    FIndexColumn,FLastIndexColumn: integer;
    BottomText, LeftText: Single;
    FPasswordChar: Char;
    FText: TstringList;
    FPaddingHorizontal, FPaddingVertical, FPadding: Single;
    FBackgroundColor, FFocusedColor, FUnfocusedColor: TCastleColor;
    FPlaceholderColor: TCastleColor;
    FFrame: boolean;
    LifeTime: TFloatTime;
    FMaxLength: Cardinal;
    FAllowedChars: TSetOfChars;
    FCaptureAllInput: boolean;
    FAutoOnScreenKeyboard: Boolean;
    FAutoSizeHeight: Boolean;
    FPlaceholder: String;
    FOnChange: TNotifyEvent;
    FTextTranslate, FPlaceholderTranslate: Boolean;
    FBolScroll: Boolean;
    FSelectedColor: TCastleColor;
    UndoSystem: TMemoUndo;
    LastSizeWidth, LastSizeHeight: Single;
    function GetText: TStringList;
    procedure LinesChange(Sender: TObject);
    procedure SetLines(AValue: TStringList);
    procedure SetSelectedColor(const Value:TCastleColor);
    function GetCountRow: integer;
    procedure SetIndex(Value:integer);
    procedure SetIndexColumn(Value:integer);
    procedure SetLastIndex(Value:integer);
    procedure SetLastIndexColumn(Value:integer);
    procedure SetCountRow(Value:integer);
    procedure SetText(const Value: TStringList);
    function GetSelectText:string;
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
    function WordWrap(index1:integer=-1):integer;
  protected
    FTextChar: TBooleanList;  //if False-Enter is word wrap, True - is real Enter #13
    procedure PreferredSize(var PreferredWidth, PreferredHeight: Single); override;
    function GetInternalText: String; override;
    procedure SetInternalText(const Value:String); override;
    procedure DoChange; virtual;
    procedure TranslateProperties(const TranslatePropertyEvent: TTranslatePropertyEvent); override;
    procedure OnScrollChange(Scroll:TObject);
  public
    ScrollBar, ScrollBarHorizontal: TCastleScrollViewManual;
    RectInside: TFloatRectangle;
    BolSelect, SelectShift: boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function LineToStrings(out Chars:TBooleanList; var Line:string):String;
    procedure Render; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
    function Motion(const Event: TInputMotion): boolean; override;
    function CapturesEventsAtPosition(const Position: TVector2): boolean; override;
    procedure SetFocused(const Value: boolean); override;
    procedure EditorAllowResize(
      out ResizeWidth, ResizeHeight: Boolean; out Reason: String); override;
    procedure Delete(index1:integer);
    property SelectedText: string read GetSelectText;// write SetSelectText;
    procedure SetCaret(index1, indexColumn1: integer; index2: integer = -1; indexColumn2: Integer = -1);
    function GetCaret:TMemoIndexs;
    procedure Append(index1, indexColumn1: integer; Text_: String);
    function PropertySection(const PropertyName: String): TPropertySection; override;
    procedure DeleteSelectText;
    property Text: TstringList read FText write SetText;
    property Index:integer read FIndex write SetIndex;
    property LastIndex:integer read FLastIndex write SetLastIndex;
     property IndexColumn:integer read FIndexColumn write SetIndexColumn;
    property LastIndexColumn:integer read FLastIndexColumn write SetLastIndexColumn;
    { Focused text color. By default it's black. }
    property FocusedColor: TCastleColor read FFocusedColor write SetFocusedColor;

    {Selected rectangle color. By default it's blue.}
    property SelectedColor:TCastleColor read FSelectedColor write SetSelectedColor;
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

    property Lines:TStringList read GetText write SetLines;
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
  ScrollBar:= TCastleScrollViewManual.Create(Self);
  InsertFrontIfNotExists(ScrollBar);
  ScrollBar.FullSize:=True;
  ScrollBar.KeyScrollSpeed:=0;
  ScrollBar.PageSize:=1;
  ScrollBar.OnChange:=@OnScrollChange;
  ScrollBar.ActiveKeyHomeEnd:=False;
  ScrollBar.ActiveCtrlHomeEnd:=True;
  ScrollBar.SetTransient;
  BottomText:=0;
  LeftText:=0;
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
  FAutoSizeHeight := false;
  FTextTranslate := true;
  FPlaceholderTranslate := true;
  FText:=TStringList.Create;
  Index:=FText.Add('');
  FBolScroll:=False;
  BolSelect:=False;
  LastIndex:=-1 ;
  FTextChar:=TBooleanList.Create;
  FSelectedColor:=Blue;
  UndoSystem:= TMemoUndo.Create;
  UndoSystem.Memo^:= Self;
//  WordWrap(-1);
end;


destructor TCastleMemo.Destroy;
begin
  FreeAndNil(ScrollBar);
  FTextChar.Free;
  Text.Free;
  inherited;
end;

function TCastleMemo.LineToStrings(out Chars: TBooleanList; var Line: string
  ): String;
var
  i:integer;
  s1:TStringList;
begin
    i:=0;
    s1:=TStringList.Create;
    s1.AddText(Line);
    Result:='';
    if s1.Count>1 then
    begin
      for i:=0 to s1.Count-1 do
       begin
          FTextChar.Add(False);
          Result:=Result+s1[i];
       end;
    end else Result:=Line;
    s1.Free;
end;

procedure TCastleMemo.SetIndex(Value: integer);
var
  i:Single;
begin
i:= Font.RowHeight + UIScale * (PaddingVertical + Padding)*2;
if (Value > -1) and (Value < Text.Count) then
begin
  if Value > FIndex then
  begin
     if BottomText+RectInside.Height <= i*(Index+2) then
       BottomText:= BottomText+i;
  end else
     if (i*(Index) <= BottomText) and (BottomText > 0) then
       BottomText:= BottomText-i;
  FIndex:= Value;
  end else
  begin
   if Value > Text.Count-1 then Index:= Text.Count-1;
   if Value < 0 then Index:= 0;
   Exit;
  end;
  if IndexColumn > Utf8Length(Text[Index])-1 then
     IndexColumn:= UTF8Length(Text[Index])-1;
  FBolScroll:= True;
  if ScrollBar.CompleteSize <> (Text.Count)-(RectInside.Height/i) then
    ScrollBar.CompleteSize:= (Text.Count)-(RectInside.Height/i); //round((Text.Count+1)-(Height/i))
  ScrollBar.Scroll:= Index -(RectInside.Height / i);  //Text.Count-Index)/(Height/i);
  if (ScrollBar.Scroll < 0) or (Index = 0) then ScrollBar.Scroll:= 0;
  if ScrollBar.Scroll > ScrollBar.CompleteSize then ScrollBar.Scroll:= ScrollBar.CompleteSize;
  FBolScroll:= False;
end;

procedure TCastleMemo.SetSelectedColor(const Value: TCastleColor);
begin
  FSelectedColor:=Value;
end;

function TCastleMemo.GetText: TStringList;
var
  i,j:integer;
  ff:TStringList;
begin
  ff:=TStringList.Create;
  ff.Assign(Text);
  i:= 0;
  while i < FTextChar.Count do
    begin
     j:=i;
     while FTextChar[i]=False do
     begin
       ff[j]:=ff[j]+ff[j+1];
       ff.Delete(j+1);
       i:=i+1;
       if FTextChar.Count<=i then
         break;
    end;
    i:= i + 1;
   end;
 // ff.OnChange:= @LinesChange;
  Result:=TStringList.Create;
  Result.Assign(ff);
  Result.OnChange:= @LinesChange;
  ff.Free;
end;

procedure TCastleMemo.LinesChange(Sender: TObject);
var
  i: integer;
begin
  if Sender <> nil then
  begin
   WritelnLog('Change');
   SetLines(TStringList(Sender));
  end;
end;

procedure TCastleMemo.SetLines(AValue: TStringList);
var
  i:integer;
begin
  Text.Assign(AValue);
  FTextChar.Clear;
  if Text.Count > 2 then
  begin
    for i:= 0 to Text.Count-2 do
      FTextChar.Add(True);
  end else
  if Text.Count > 1 then  FTextChar.Add(True);
  WritelnLog('Change1');
  WordWrap(-1);
end;

function TCastleMemo.GetCountRow: integer;
begin
   Result:=Text.Count;
end;

procedure TCastleMemo.SetIndexColumn(Value: integer);
begin
  if (Value > -2) and (Value < UTF8Length(Text[Index])) then
  begin
   FIndexColumn:= Value;
  end else begin
   if (Value < -2) then  FIndexColumn:= -1;
   if (Value > UTF8Length(Text[Index])-1) then FIndexColumn:= UTF8Length(Text[Index]) - 1;
  end;
end;

procedure TCastleMemo.SetLastIndex(Value: integer);
var
  i:Single;
begin
if (Value>-2) and (Value<Text.Count) then
begin
 { i:=Font.RowHeight + UIScale * (PaddingVertical + Padding)*2;
  if Value>FLastIndex then
  begin
     if BottomText+Height<=i*(Index+2) then
       BottomText:=BottomText+i;
  end else
     if (i*(Index)<=BottomText) and (BottomText>0) then
       BottomText:=BottomText-i;  }
  FLastIndex:=Value;
  WritelnLog('Assigned: ' +IntToStr(FLastIndex));
end else Exit;
  if LastIndexColumn > Utf8Length(Text[Index])-1 then
     LastIndexColumn:=UTF8Length(Text[Index])-1;
end;

procedure TCastleMemo.SetLastIndexColumn(Value: integer);
begin
  if LastIndex<>-1 then
    if (Value>-2) and (Value<UTF8Length(Text[LastIndex])) then
  begin
  FLastIndexColumn:=Value;
  end else begin
  if (Value<-2) then  FIndexColumn:=-1;
  if (Value>UTF8Length(Text[LastIndex])-1) then FLastIndexColumn:=UTF8Length(Text[LastIndex])-1;
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
  end;
end;


procedure TCastleMemo.SetText(const Value: TStringList);
begin
  if FText <> Value then
  begin
    FText.Clear;
    FText.AddStrings (Value);
    VisibleChange([chRender]);
    DoChange;
  end;
end;

function TCastleMemo.GetSelectText: string;
var
  i,j: integer;
  Caret: TMemoIndexs;
  Lines1: TStringList;
begin
  Caret:= GetCaret;
  WritelnLog('GGF');
  if (Caret.Index2 > -1) and (Caret.Index2 < Lines.Count) and (Caret.Index1 < Lines.Count) and (BolSelect) then
  begin
    Lines1:= TStringList.Create;
    Lines1.Assign(Lines);
    if Caret.Index1 <> Caret.Index2 then
    begin
      if Caret.Index1 > Caret.Index2 then
      begin
        Result:= UTF8Copy(Lines1[Caret.Index2], Caret.IndexColumn2 + 2, UTF8Length(Lines1[Caret.Index2]) - Caret.IndexColumn2 + 2);
        for i:= Caret.Index2+1 to  Caret.Index1-1 do
            Result:= Result + #13 + Lines1[i];
        Result:= Result+#13+UTF8Copy(Lines1[Caret.Index1], 1, Caret.IndexColumn1 + 1);
      end
      else begin
        Result:= UTF8Copy(Lines1[Caret.Index1], Caret.IndexColumn1 + 2,UTF8Length(Lines1[Caret.Index1])-Caret.IndexColumn1 + 2);
        for i:= Caret.Index1+1 to  Caret.Index2-1 do
            Result:= Result+#13+Lines1[i];
        Result:= Result+#13+UTF8Copy(Lines1[Caret.Index2], 1, Caret.IndexColumn2+1);
      end;
    end else
    if Caret.IndexColumn1 > Caret.IndexColumn2 then
       Result:= UTF8Copy(Lines1[Caret.Index1], Caret.IndexColumn2 + 2, Caret.IndexColumn1 - Caret.IndexColumn2)
    else Result:= UTF8Copy(Lines1[Caret.Index1], Caret.IndexColumn1 + 2, Caret.IndexColumn2 - Caret.IndexColumn1);
    Lines.Assign(Lines1);
    Lines1.Free;
end else Result:='';
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
var
  i:integer;
begin
 index:= index;
 WordWrap(index);
 if index-1 > 0 then WordWrap(index-1);
 {if FTextChar.Count >= FText.Count then
   for i:= 0 to FTextChar.Count-FText.Count do
     FTextChar.Delete(FText.Count-1);
 WritelnLog('Count_'+IntToStr(FTextChar.Count));
 if FTextChar.Count < FText.Count-1 then
  for i:= 0 to FText.Count-FTextChar.Count do
    FTextChar.Add(False);                 }
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

procedure TCastleMemo.Delete(index1: integer);
begin
 Text.Delete(index1);
 if (index1-1<Text.Count) and (index1<>0) then
    FTextChar.Delete(index1-1);
end;

procedure TCastleMemo.SetCaret(index1, indexColumn1: integer; index2: integer;
  indexColumn2: Integer);
var
  i, j, k: integer;
begin
   j:=0;
   k:= 0;
   if index1 <> -2 then
  begin
   if index1<>0 then
    for i:=0 to FTextChar.Count-1 do
    begin
      if FTextChar[i] = False then j:=j+1 else k:=k+1;
      if k >= index1 then break;
    end;
    Index:= index1 + j;
  end;

  if indexColumn1 <> -2 then
  begin
    j:=UTF8Length(Text[Index]);
    IndexColumn:= -1;
    for i:=Index to Text.Count-1 do
    begin
      if indexColumn1 <= j then
      begin
        IndexColumn:= indexColumn1 - (j - UTF8Length(Text[i]));
        break;
      end
      else
        if i+1 < Text.Count then j:= j + UTF8Length(Text[i+1])
        else
        begin

         IndexColumn:= indexColumn1 - (j - UTF8Length(Text[i]));
         break;
        end;
      if FTextChar.Count > i then
        if FTextChar[i] = False then Index:= Index + 1
        else
          begin
            IndexColumn:= indexColumn1 - (j - UTF8Length(Text[i]));
            break;
          end;
      end;
  end;

  if index2 <> -1 then
  begin
    if index2 <> -2 then
    begin
      BolSelect:= True;
      j:= 0;
      k:= 0;
      if index1<>0 then
        for i:=0 to FTextChar.Count-1 do
        begin
          if FTextChar[i] = False then j:=j+1 else k:=k+1;
          if k >= index2 then break;
        end;
      LastIndex:= index2 + j;
    end;

    if indexColumn2 <> -2 then
    begin
      j:=UTF8Length(Text[LastIndex]);
      LastIndexColumn:=0;
      for i:=LastIndex to Text.Count-1 do
      begin
        if indexColumn2 <= j then
        begin
        LastIndexColumn:= indexColumn2 - (j - UTF8Length(Text[i])) - 1;
        break;
        end
        else
          if i+1<Text.Count then j:= j + UTF8Length(Text[i+1])
          else
          begin
             LastIndexColumn:= indexColumn2 - (j - UTF8Length(Text[i])) - 1;
             break;
          end;
        if FTextChar.Count > i then
          if FTextChar[i] = False then LastIndex:= LastIndex + 1
          else
            begin
            LastIndexColumn:= indexColumn2 - (j - UTF8Length(Text[i])) - 1;
            break;
            end;
      end;
    end;
  end else BolSelect:= False;
end;

function TCastleMemo.GetCaret: TMemoIndexs;
var
  i, j, j1 ,k, k1: integer;
begin
  j1:= 0;
  k:= 0;
  j:= 0;
  k1:=0;
  if Index <> 0 then
  begin
    for i:=0 to FTextChar.Count-1 do
    begin
      if FTextChar[i] = False then
      begin
       k:= k+1;
       k1:= k1+1;
      end
      else begin
       j1:= j1+1;
       k1:= 0;
      end;
      if k + j1 >= index then break;
    end;
    j:= k+j1;
    for i:= j1+k-1 downto 0 do
      begin
        if FTextChar[i] = True then break else begin
        k1:= k1 - 1;
        k:= k - 1;
        end;
      end;
    if i = 0 then k1:=0;
    Result.Index1:= j1 + k1;
  end else
  begin
    j:=0;
    Result.Index1:= 0;
    k:= 0;
    j1:= 0;
  end;

  j1:= 0;
  Result.IndexColumn1:= IndexColumn;
  if FTextChar.Count > 0 then
    for i:= Result.Index1+k to j do
    begin
      WritelnLog('G '+IntToStr(i));
      if i-1 > FTextChar.Count-1 then break;
      if i-1 >= Result.Index1+k then
      begin
        if FTextChar[i-1] = False then j1:= j1 + UTF8Length(Text[i])
        else
        begin
          if i-1 > -1 then
            j1:= j1 - UTF8Length(Text[i]);
          break;
        end;
      end else j1:= UTF8Length(Text[i]);
      if i-1 > FTextChar.Count-1 then break;
      if  (i = j) then
      begin
       j1:= j1 - UTF8Length(Text[i]);
       break;
      end;
    end;
  Result.IndexColumn1:= j1 + IndexColumn;

 if LastIndex <> -1 then
 begin
   j1:= 0;
  k:= 0;
  j:= 0;
  if LastIndex <> 0 then
  begin
    for i:=0 to FTextChar.Count-1 do
    begin
      if FTextChar[i] = False then
      begin
       k:= k+1;
       k1:= k1+1;
      end
      else begin
       j1:= j1+1;
       k1:= 0;
      end;
      if k + j1 >= LastIndex then break;
    end;
    j:= k;
    for i:= j1+k-1 downto 0 do
      begin
        if FTextChar[i] = True then break else k1:= k1 - 1;
      end;
    Result.Index2:= j1 + k1;
  end else
  begin
    j:=0;
    Result.Index2:= 0;
    k:= 0;
    j1:= 0;
  end;

  WritelnLog('YY '+IntToStr(k)+' '+IntToStr(j1)+' '+IntToStr(k1)+' '+IntToStr(j));
  j1:= 0;
  Result.IndexColumn2:= LastIndexColumn;
  if FTextChar.Count > 0 then
    for i:= Result.Index2 to k + j do
    begin
     WritelnLog(IntToStr(i)+' '+IntToStr(j1));
      if i > FTextChar.Count-1 then break;
      if FTextChar[i] = False then j1:= j1 + UTF8Length(Text[i])
      else
      begin
        if i-1 > -1 then
         j1:= j1 - UTF8Length(Text[i-1]);
        WritelnLog('break1');
        break;
      end;
      if i = FTextChar.Count-1 then break;
      if (i = k + j) then
      begin
       j1:= j1 - UTF8Length(Text[i]);
               WritelnLog('break2');
       break;
      end;
    end;
  Result.IndexColumn2:= j1  + LastIndexColumn;
 end else
 begin
  Result.IndexColumn2:=-1;
  Result.Index2:=-1;
 end;
end;

procedure TCastleMemo.Append(index1, indexColumn1: integer; Text_: String);
var
  i: integer;
  s1: TStringList;
  s: String;
begin
  if LastIndex <> -1 then
    begin
       DeleteSelectText;
       LastIndex:= -1;
    end;
  i:= 0;
  s1:= TStringList.Create;
  s1.AddText(Text_);
  if s1.Count > 1 then
  begin
    s1[Text.Count-1]:= s1[Text.Count-1] + UTF8Copy(Text[Index1], IndexColumn1 + 2, UTF8Length(Text[Index1]) - IndexColumn1 + 2);
    s1.Clear;
    s1.AddText(Text_);
    s1[s1.Count-1]:= s1[s1.Count-1] + UTF8Copy(Text[Index1], IndexColumn1 + 2,UTF8Length(Text[Index1]) - IndexColumn1 + 1);
    Text[Index1]:= UTF8Copy(Text[Index1], 1, IndexColumn1+1);
    Text[Index1]:= Text[Index1] + s1[0];
    if s1.Count > 2 then
    begin
      for i:= 1 to s1.Count-1 do
      begin
         Text.Insert(Index1 + i, s1[i]);
         FTextChar.Insert(Index1 + i - 1, True);
      end;
      for i:= 0 to s1.Count-1 do
          WordWrap(Index1 + i);
    end;
  end else
  begin
    s:= Text[Index1];
    UTF8Insert(Clipboard.AsText, s, IndexColumn1 + 2);
    Text[Index1]:= s;
  end;
  DoChange;
end;

function TCastleMemo.PropertySection(const PropertyName: String): TPropertySection;
begin
  if (PropertyName = 'Text') then
    Result := psBasic
  else
    Result := inherited PropertySection(PropertyName);
end;

procedure TCastleMemo.DeleteSelectText;
var
  i:integer;
  s:String;
begin
 WritelnLog('DeleteSelectText');
  if LastIndex <> -1 then
  begin
   WritelnLog('DeleteSelectText '+IntToStr(Index)+' LI '+IntToStr(LastIndex));
   if (Index = 0) and (LastIndex = Text.Count-1) then
    if (IndexColumn = -1) and (LastIndexColumn = UTF8Length(Text[LastIndex])) then
    begin
     Text.Clear;
     Text.Add('');
    end;
       if Index = LastIndex then
        begin
         s:=Text[Index];
         if IndexColumn > LastIndexColumn then
         begin
           UTF8Delete(s,LastIndexColumn+2,IndexColumn-LastIndexColumn);
           IndexColumn:= LastIndexColumn;
         end  else UTF8Delete(s,IndexColumn+2,LastIndexColumn-IndexColumn);
         Text[index]:=s;
         LastIndex:= -1;
        end else
        if Index > LastIndex then
        begin
          s:=Text[LastIndex];
          UTF8Delete(s,LastIndexColumn+2,UTF8Length(Text[LastIndex])-LastIndexColumn+2);
          Text[LastIndex]:=s;
          WritelnLog('Index_ADelete_'+IntToStr(Index));
          if FTextChar.Count>Index then
          begin
            FTextChar[LastIndex]:=FTextChar[Index];
            WritelnLog('LastIndex_Change_'+BoolToStr(FTextChar[LastIndex],True));
            WritelnLog('Index_Delete_'+IntToStr(Index));
            FTextChar.Delete(index);
          end;
          if Index - LastIndex > 2 then
          begin
           for i:= LastIndex+1 to Index-1 do
           begin
              Text.Delete(LastIndex+1);
              if FTextChar.Count>LastIndex+1 then
              FTextChar.Delete(LastIndex+1);
              WritelnLog('LastIndex+1_Delete_'+IntToStr(LastIndex+1));
           end;
          end;
          Index:= LastIndex+1;
          s:=Text[Index];
          UTF8Delete(s,1,IndexColumn+1);
          if Text.Count=1 then Text[0]:='' else
              Text.Delete(LastIndex+1);
          WritelnLog('LastIndex'+IntToStr(LastIndex)+'Index_'+IntToStr(Index));
          if (LastIndex < FTextChar.Count) and (LastIndex > 0) then
          begin
            FTextChar.Delete(LastIndex);
            FTextChar[LastIndex-1]:= False;
            WritelnLog('LastIndex_ADelete_'+IntToStr(LastIndex));
          end;
          Text[LastIndex]:=Text[LastIndex]+s;

          Index:=LastIndex;
          IndexColumn:=LastIndexColumn;
          LastIndex:=-1;
       end else
       begin
          s:=Text[Index];
          UTF8Delete(s,IndexColumn+2,UTF8Length(Text[Index])-IndexColumn+1);
          Text[Index]:=s;
          if FTextChar.Count>LastIndex then
          begin
            FTextChar[index]:=FTextChar[LastIndex];
            FTextChar.Delete(LastIndex);
          end;
          if LastIndex-Index>2 then
          begin
           for i:= Index+1 to LastIndex-1 do
           begin
              Text.Delete(Index+1);
              if FTextChar.Count > Index+1 then FTextChar.Delete(Index+1);
           end;
          end
          else;
          FLastIndex:=Index+1;
          if Index < FTextChar.Count then FTextChar.delete(Index);
          if Text.Count>Index then
          begin
            s:=Text[LastIndex];
            UTF8Delete(s,1,LastIndexColumn+1);
            Text[Index]:=Text[Index]+s;
          end;
          Text.Delete(LastIndex);
          if LastIndex<FTextChar.Count then
          begin
          FTextChar.Delete(LastIndex-1);
          FTextChar[LastIndex-1]:=False;
          end;
          if Text.Count=0 then Text.Add('');
          //LastIndexColumn:=-1;
          LastIndex:=-1;
       end;
end;
  DoChange;
end;

procedure TCastleMemo.Render;
const
  CaretSpeed = 1; //< how many blinks per second
var
  SR, MainSelectRect,AddSelectRec,AddLastSelectRec: TFloatRectangle;
  PaddingHorizontalScaled, PaddingVerticalScaled: Single;
  US,  TextBottom: Single;
  FrameCorners: TVector4;
  CaretVisible: boolean;
  S, S1: string;
  Color: TCastleColor;
  CaretShift,HeightText: Single;
  i,j,k:integer;
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
  // calculate S, Color, CaretShift
  if (LastSizeHeight<>RectInside.Height) or (LastSizeWidth<>RectInside.Width) then
    begin
    WordWrap(-1);
    LastSizeHeight:=RectInside.Height;
    LastSizeWidth:=RectInside.Width;
    end;

  if LastIndex<>-1 then
  begin
   MainSelectRect.Left:=TextLeft;
   if Index=LastIndex then
   begin
     AddSelectRec.Bottom:=(RectInside.Bottom+RectInside.Height)-(Font.RowHeight+PaddingVerticalScaled*2)*(Index+1)+BottomText+1;
     if IndexColumn>LastIndexColumn then
     begin
      AddSelectRec.Left:=TextLeft+Font.TextWidth(UTF8Copy(Text[LastIndex],1,LastIndexColumn+1));
      AddSelectRec:=AddSelectRec.GrowTop((Font.RowHeight+PaddingVerticalScaled*2)).GrowRight(Font.TextWidth(UTF8Copy(Text[Index],LastIndexColumn+2,IndexColumn-LastIndexColumn)));
     end else
     begin
      AddSelectRec.Left:=TextLeft+Font.TextWidth(UTF8Copy(Text[Index],1,IndexColumn+1));
      AddSelectRec:=AddSelectRec.GrowTop((Font.RowHeight+PaddingVerticalScaled*2)).GrowRight(Font.TextWidth(UTF8Copy(Text[LastIndex],IndexColumn+2,LastIndexColumn-IndexColumn)));
     end;
   end
  else if Index>LastIndex then
  begin
    MainSelectRect.Bottom:=(RectInside.Bottom+RectInside.Height)-(Font.RowHeight+PaddingVerticalScaled*2)*(Index)+BottomText+1;
    MainSelectRect:=MainSelectRect.GrowTop(((Font.RowHeight+PaddingVerticalScaled*2)*(Index-LastIndex-1))).GrowRight(RectInside.Width);

    AddSelectRec.Bottom:=(RectInside.Bottom+RectInside.Height)-(Font.RowHeight+PaddingVerticalScaled*2)*(Index+1)+BottomText+1;
    AddSelectRec.Left:=TextLeft;
    AddSelectRec:=AddSelectRec.GrowTop((Font.RowHeight+PaddingVerticalScaled*2)).GrowRight(Font.TextWidth(UTF8Copy(Text[Index],1,IndexColumn+1)));

    AddLastSelectRec.Bottom:=(RectInside.Bottom+RectInside.Height)-(Font.RowHeight+PaddingVerticalScaled*2)*(LastIndex+1)+BottomText;
    AddLastSelectRec.Left:=TextLeft+Font.TextWidth(UTF8Copy(Text[LastIndex],1,LastIndexColumn+1));
    AddLastSelectRec:=AddLastSelectRec.GrowTop((Font.RowHeight+PaddingVerticalScaled*2)).GrowRight(RectInside.Width-Font.TextWidth(UTF8Copy(Text[LastIndex],1,LastIndexColumn+1)));
  end
  else
  begin
    MainSelectRect.Bottom:=(RenderRect.Bottom+RectInside.Height)-(Font.RowHeight+PaddingVerticalScaled*2)*(LastIndex)+BottomText+1;
    MainSelectRect:=MainSelectRect.GrowTop((Font.RowHeight+PaddingVerticalScaled*2)*(LastIndex-Index-1)+1).GrowRight(RectInside.Width);

    AddSelectRec.Bottom:=(RenderRect.Bottom+RectInside.Height)-(Font.RowHeight+PaddingVerticalScaled*2)*(Index+1)+BottomText;
    AddSelectRec.Left:=TextLeft+Font.TextWidth(UTF8Copy(Text[Index],1,IndexColumn+1));
    AddSelectRec:=AddSelectRec.GrowTop((Font.RowHeight+PaddingVerticalScaled*2)).GrowRight(RectInside.Width-Font.TextWidth(UTF8Copy(Text[Index],1,IndexColumn+1)));

    AddLastSelectRec.Bottom:=(RenderRect.Bottom+RectInside.Height)-(Font.RowHeight+PaddingVerticalScaled*2)*(LastIndex+1)+BottomText;
    AddLastSelectRec.Left:=TextLeft;
    AddLastSelectRec:=AddLastSelectRec.GrowTop((Font.RowHeight+PaddingVerticalScaled*2)+1).GrowRight(Font.TextWidth(UTF8Copy(Text[LastIndex],1,LastIndexColumn+1)));
  end;

  if MainSelectRect.Bottom<RectInside.Bottom then
     MainSelectRect:=MainSelectRect.RemoveBottom(RectInside.Bottom-MainSelectRect.Bottom)
  else if MainSelectRect.Bottom+MainSelectRect.Height>RectInside.Bottom+RectInside.Height then
    MainSelectRect:=MainSelectRect.RemoveTop((MainSelectRect.Bottom+MainSelectRect.Height)-(RectInside.Bottom+RectInside.Height));
    if AddSelectRec.Bottom<RectInside.Bottom then
     AddSelectRec:=AddSelectRec.RemoveBottom(RectInside.Bottom-AddSelectRec.Bottom)
  else if AddSelectRec.Bottom+AddSelectRec.Height>RectInside.Bottom+RectInside.Height then
    AddSelectRec:=AddSelectRec.RemoveTop((AddSelectRec.Bottom+AddSelectRec.Height)-(RectInside.Bottom+RectInside.Height));
    if AddSelectRec.Bottom<RectInside.Bottom then
     AddSelectRec:=AddSelectRec.RemoveBottom(RectInside.Bottom-AddSelectRec.Bottom)
  else if AddSelectRec.Bottom+AddSelectRec.Height>RectInside.Bottom+RectInside.Height then
    AddSelectRec:=AddSelectRec.RemoveTop((AddSelectRec.Bottom+AddSelectRec.Height)-(RectInside.Bottom+RectInside.Height));
     if AddLastSelectRec.Bottom<RectInside.Bottom then
     AddLastSelectRec:=AddLastSelectRec.RemoveBottom(RectInside.Bottom-AddLastSelectRec.Bottom)
  else if AddLastSelectRec.Bottom+AddLastSelectRec.Height>RectInside.Bottom+RectInside.Height then
    AddLastSelectRec:=AddLastSelectRec.RemoveTop((AddLastSelectRec.Bottom+AddLastSelectRec.Height)-(RectInside.Bottom+RectInside.Height));

   DrawRectangle(MainSelectRect,SelectedColor);
   DrawRectangle(AddSelectRec,SelectedColor);
   DrawRectangle(AddLastSelectRec,SelectedColor);
  end;
  CaretShift := 0;
  if AutoSizeHeight then
   HeightText:=Height else
     begin
      HeightText:=(Font.RowHeight+PaddingVerticalScaled*2)*Text.Count;
       end;
  if Text<>nil then
  for i:=0 to Text.Count -1 do
  begin
  if ((Font.RowHeight + UIScale * (PaddingVertical + Padding)*2)*(i+1)>=BottomText) and (BottomText+RectInside.Height>=(Font.RowHeight + UIScale * (PaddingVertical + Padding)*2)*(i+1)) then
  begin
    S :=Text[i];
    if Focused then
    begin
    if i=Index then
      CaretShift := Font.TextWidth(UTF8Copy(s,1,IndexColumn+1));
    Color := FocusedColor
    end else
      Color := UnfocusedColor;

  TextLeft := RectInside.Left;
  // center in RectInside, important when AutoSize = false
  TextBottom := (RectInside.Bottom+(RectInside.Height-HeightText) +
    ( HeightText/Text.Count - Font.RowHeight) / 2 + Font.Descend)+HeightText/Text.Count*(Text.Count-i-1); //+FPaddingVertical

  Font.Print(TextLeft+LeftText, BottomText+TextBottom, Color, S);
  if (FTextChar.Count>i) and (i>-1) then
    if FTextChar[i] then
    Font.Print(TextLeft+LeftText+Font.TextWidth(S)+2, BottomText+TextBottom, Color, 'T')
    else Font.Print(TextLeft+LeftText+Font.TextWidth(S)+2, BottomText+TextBottom, Color, 'F');
     if Focused  and (i = Index) then
  begin
    CaretVisible := FloatModulo(LifeTime * CaretSpeed, 1.0) < 0.5;
    if CaretVisible then
      Font.Print(TextLeft + CaretShift+LeftText,BottomText+TextBottom, FocusedColor, CaretChar);
  end;
  end;
end;
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

function TCastleMemo.Motion(const Event: TInputMotion): boolean;
var
  i:integer;
begin
  Result:=inherited Motion(Event);
  if BolSelect then
  begin
    if mbLeft in Event.Pressed then
    begin
      if Text.Count>1 then
      begin
       for i:=1 to round(Text.Count) do
       begin
         if RenderRect.Bottom + RectInside.Height-Event.Position.Y<i*(Font.RowHeight + UIScale * (PaddingVertical + Padding)*2)-BottomText then
         begin
         WritelnLog('Motion Assign');
           LastIndex:= i-1;
           break;
         end;
       end;

      if (LastIndex <= Text.Count+1-round(RectInside.Height/(Font.RowHeight + UIScale * (PaddingVertical + Padding)*2)/2)) then
        if Event.Position.Y<5+RenderRect.Bottom then
           BottomText:= BottomText+Font.RowHeight + UIScale * (PaddingVertical + Padding)*2;
      if BottomText > 0 then
          if Event.Position.Y > RenderRect.Bottom+RenderRect.Height-7 then
            BottomText:=BottomText-Font.RowHeight + UIScale * (PaddingVertical + Padding)*2;
      if (LastIndex>-1) and (LastIndex<Text.Count) then
          for i:=UTF8Length(Text[LastIndex]) downto 0 do
          begin
           if Event.Position.X-RenderRect.Left>Font.TextWidth(UTF8Copy(Text[LastIndex],1,i)) then
           begin
              LastIndexColumn:= i - 1;
              break;
           end;
          end;
     {   if (LastIndex = Index) and (IndexColumn = LastIndexColumn) then
         LastIndex:=-1;}
    end;
    end;
  end else
  begin
    BolSelect:= False;
    LastIndex:= -1;
    LastIndexColumn:= -1;
  end;
end;

function TCastleMemo.Press(const Event: TInputPressRelease): boolean;
var
  s:string;
  s1:TStrings;
  i, j:integer;
  bool: boolean;
  Caret: TMemoIndexs;
begin
  Result := inherited;
  if Result or (not GetExists) then Exit;
  s:='';
  bool:= False;
  Caret:= GetCaret;
  if Event.IsKey(keyLeft) or Event.IsKey(keyRight) or Event.IsKey(keyUp) or Event.IsKey(keyDown) then
   if NOT(mkShift in Event.ModifiersDown) then
    if LastIndex <> -1 then
     begin
      Index:= LastIndex;
      IndexColumn:= LastIndexColumn;
      LastIndex:= -1;
      BolSelect:= False;
     end;
  ////
  if Event.IsKey('G') then WriteUndo(UndoSystem) else
     ////
  if Event.IsMouseButton(mbLeft) then
  begin
    if Text.Count=0 then Text.Add('');
    if NOT(mkShift in Event.ModifiersDown) then
    begin
      for i:=1 to Text.Count do
      begin
       if RenderRect.Bottom+RectInside.Height-Event.Position.Y < i*(Font.RowHeight + UIScale * (PaddingVertical + Padding)*2)-BottomText then
       begin
         Index:=i-1;
         break;
       end;
       if i = Text.Count then Index:= i;
      end;
      for i:=UTF8Length(Text[Index]) downto 0 do
       if Event.Position.X-RenderRect.Left > Font.TextWidth(UTF8Copy(Text[Index],1,i)) then
       begin
          IndexColumn:= i-1;
          LastIndex:= -1;
          LastIndexColumn:= -1;
          break;
      end;
    end
    else
    begin
     for i:=1 to round(Text.Count) do
       if RenderRect.Bottom+RectInside.Height-Event.Position.Y < i*(Font.RowHeight + UIScale * (PaddingVertical + Padding)*2)-BottomText then
       begin
         BolSelect:=True;
         WritelnLog('Press Assign');
         LastIndex:= i-1;
         break;
       end;
      for i:=UTF8Length(Text[LastIndex]) downto 0 do
       if Event.Position.X-RenderRect.Left > Font.TextWidth(UTF8Copy(Text[LastIndex],1,i)) then
       begin
          BolSelect:= True;
          LastIndexColumn:= i-1;
          break;
       end;
    end;
    BolSelect:=True;
  end else
  if  Event.IsKey(keyShift) then
  begin
   if BolSelect = False then
    begin
    WritelnLog(BoolToStr(BolSelect,True));
      LastIndex:= Index;
      LastIndexColumn:= IndexColumn;
      BolSelect:= True;
    end;
    BolSelect:= True;
  end
  else
  if Event.IsKey(CharBackSpace) or Event.IsKey(K_BackSpace) then
  begin
    if LastIndex <> -1 then
    begin
      WritelnLog('GG');
      UndoSystem.ChangeAct(bool, Caret.index1, Caret.indexcolumn1, Caret.Index2, Caret.IndexColumn2, SelectedText, atDelete);
      WritelnLog('GG');
      DeleteSelectText;
    end
    else
    begin
     s:=Text[index];
     if IndexColumn <> -1 then
      begin
        UTF8Delete(s, indexColumn+1, 1);
        UndoSystem.ChangeAct(bool, Caret.Index1, Caret.IndexColumn1-1, -1, -1, UTF8Copy(Text[Index],IndexColumn+1,1), atDelete);
        Text[index]:= s;//+Text[index][IndexColumn];
        IndexColumn:= IndexColumn - 1;
        WritelnLog('Deleted IndexColumn<>-1');
      end else
      if Index > 0 then
       begin
        i:= UTF8Length(Text[Index-1])-1;
        if FTextChar[index-1] = False then
         begin
         s:=Text[index-1];
          UTF8Delete(s,UTF8Length(Text[index-1]),1);
          UndoSystem.ChangeAct(bool, Caret.Index1, UTF8Length(Text[index-1]), -1, -1, UTF8Copy(Text[index-1],UTF8Length(Text[index-1])-1,1), atDelete);
          Text[index-1]:=s;
         end;
       {if FTextChar[Index-1] then
          begin  }
         if Text[index] <> '' then
           Text[Index-1]:=Text[Index-1]+Text[Index];
          //WritelnLog(Text[index]);
           UndoSystem.AddAct(bool, Caret.index1, Caret.IndexColumn1, -1, -1, Text[Index], atDeleteLine);
           Text.Delete(index);
         // end;
        FTextChar.Delete(index-1);
        Index:=Index-1;
        IndexColumn:=i;
        end;
      WritelnLog('IndexColumn:    '+IntToStr(IndexColumn));
     end;
    DoChange;
    Result := ExclusiveEvents;
  end else
    if Event.IsKey(CharDelete) or Event.IsKey(K_Delete) then
  begin
    if LastIndex <> -1 then
    begin
      UndoSystem.ChangeAct(bool, Caret.Index1, Caret.IndexColumn1, Caret.Index2, Caret.IndexColumn2, SelectedText, atDelete);
      DeleteSelectText;
      bool:=true;
    end
    else
    begin
      s:=Text[index];
      if IndexColumn < UTF8Length(S)-1 then
      begin
        UTF8Delete(s, indexColumn+2, UTF8Length(UTF8Copy(Text[Index], IndexColumn+2, 1)));
        UndoSystem.AddAct(Caret.Index1, Caret.IndexColumn1, UTF8Copy(Text[Index],IndexColumn+2, 1), atDelete);
        Text[index]:= s;
      end
      else
      if (Text.Count > Index + 1) and (Text.Count > 1) then
        if s='' then  begin
          Text.Delete(Index);
          FTextChar.Delete(index);
          UndoSystem.AddAct(Caret.Index1, Caret.IndexColumn1 + 2, '', atDeleteLine);
        end
        else
        begin
         Text[Index]:= Text[Index] + Text[Index+1];
         UndoSystem.AddAct(Caret.Index1, Caret.IndexColumn1+2, Text[Index+1], atDelete);
         Text.Delete(Index+1);
         if FTextChar.Count > index then
            FTextChar.Delete(index);
        end;
    DoChange;
    Result := ExclusiveEvents;
    end;
  end else
  if Event.IsKey(CtrlC) then
  begin
    Clipboard.SetAsText(AnsiToUtf8(SelectedText));
    Result := ExclusiveEvents;
  end else
  if Event.IsKey(CtrlX) then
  begin
      Clipboard.AsText := SelectedText;
      UndoSystem.AddAct(False, Caret.Index2, Caret.IndexColumn2,Caret.Index1, Caret.IndexColumn1, SelectedText, atDelete);
      DeleteSelectText;
      DoChange;
    Result := ExclusiveEvents;
  end else
  if Event.IsKey(CtrlA) then
  begin
    Index:= 0;
    IndexColumn:= -1;
    LastIndex:= Text.Count-1;
    LastIndexColumn:= UTF8Length(Text[LastIndex]);
    BolSelect:= True;
  end
  else
  if Event.IsKey(CtrlV) then
  begin
    if LastIndex <> -1 then
    begin
       DeleteSelectText;
       UndoSystem.AddAct(False, Caret.Index1, Caret.IndexColumn1, Caret.Index2, Caret.IndexColumn2, SelectedText, atDelete);
       bool:= True;
       LastIndex:= -1;
       WritelnLog('True');
    end;
    s:=Clipboard.AsText;
    i:=0;
    s1:=TStringList.Create;
    s1.AddText(s);
    if s1.Count > 1 then
    begin
      s1[Text.Count-1]:=s1[Text.Count-1]+UTF8Copy(Text[Index],IndexColumn+2,UTF8Length(Text[index])-IndexColumn+2);
      s1.Clear;
      s1.AddText(s);
      s1[s1.Count-1]:= s1[s1.Count-1] + UTF8Copy(Text[Index],IndexColumn+2,UTF8Length(Text[Index]) - indexColumn+1);
      Text[Index]:= UTF8Copy(Text[Index],1,IndexColumn+1);
      Text[Index]:= Text[Index] + s1[0];
      UndoSystem.AddAct(bool, index, indexColumn,-1,-1, Clipboard.AsText, atAdd);
      if s1.Count > 2 then
      begin
        for i:= 1 to s1.Count-1 do
        begin
           Text.Insert(Index+i,s1[i]);
           FTextChar.Insert(index+i-1,True);
        end;
        for i:= 0 to s1.Count-1 do
            index:=index + WordWrap(index+i);
       { s:= Text[Index+i];
        UTF8Insert(s1[i+1], s, IndexColumn+2);
        Text[Index+i]:= s; }
        index:= Index+i;
        UndoSystem.ChangeAct(bool,-1, -1, Caret.Index1, Caret.IndexColumn1+UTF8Length(s)-1);
      end;
    end else
    begin
      s:= Text[Index];
      UTF8Insert(Clipboard.AsText,s,IndexColumn + 2);
      Text[Index]:= s;
      UndoSystem.AddAct(bool, Caret.Index1, Caret.IndexColumn1, Caret.index1,Caret.IndexColumn1 + 1 +UTF8Length(Clipboard.AsText), Clipboard.AsText, atAdd);
    end;
    IndexColumn:= IndexColumn+UTF8Length(s);
    //UndoSystem.ChangeAct(bool,-1, -1, Index, IndexColumn);
    DoChange;
    Result := ExclusiveEvents;
  end else
  if Event.IsKey(keyUp) or Event.IsKey(keyNumpadUp) then
    if mkShift in Event.ModifiersDown then
    begin
       if LastIndex > 0 then
        LastIndex:= LastIndex - 1;
    end
    else
     begin
      if Index > 0 then
       index:= Index - 1;
      BolSelect:= False;
      LastIndex:= -1;
     end
  else
  if Event.IsKey(keyDown)or Event.IsKey(keyNumpadDown) then
    if mkShift in Event.ModifiersDown then
    begin
       if LastIndex < Text.Count-1 then
        LastIndex:= LastIndex + 1;
    end
    else
    begin
      if Index < Text.Count-1 then
        index:= Index + 1;
      BolSelect:= False;
      LastIndex:= -1;
    end
  else
  if Event.IsKey(keyLeft) or Event.IsKey(keyNumpadLeft) then
    if mkShift in Event.ModifiersDown then
    begin
      WritelnLog('ShiftLeft');
     if LastIndexColumn <> -1 then
      LastIndexColumn:= LastIndexColumn - 1
     else
       if LastIndex > 0 then
       begin
        LastIndex:= LastIndex - 1;
        LastIndexColumn:= UTF8Length(Text[LastIndex]);
       end;
    end
    else
    begin
      if IndexColumn <> -1 then
        IndexColumn:= indexColumn - 1
      else
        if Index > 0 then
        begin
          Index:= Index - 1;
          IndexColumn:= UTF8Length(Text[index]);
        end;
      BolSelect:= False;
      LastIndex:= -1;
    end
  else
  if Event.IsKey(keyRight) or Event.IsKey(keyNumpadRight) then
    if mkShift in Event.ModifiersDown then
    begin
     if LastIndexColumn < UTF8Length(Text[LastIndex])-1 then
      LastIndexColumn:= LastIndexColumn + 1
     else
       if LastIndex < Text.Count-1 then
       begin
        LastIndex:= LastIndex + 1;
        LastIndexColumn:= -1;
       end;
    end
    else
    begin
      if IndexColumn < UTF8Length(Text[Index]) then
        IndexColumn:= indexColumn + 1
      else
        if Index < Text.Count-1 then
        begin
          Index:= Index - 1;
          IndexColumn:= -1;
        end;
      BolSelect:= False;
      LastIndex:= -1;
    end
  else
  if (Event.IsKey(keyHome) or Event.IsKey(keyNumpadHome)) and (Event.IsKey(keyCtrl)) then
    if mkShift in Event.ModifiersDown then
    begin
      LastIndex:= 0;
      LastIndexColumn:= -1;
    end else
    begin
      index:=0;
      IndexColumn:=-1;
      BolSelect:= False;
      LastIndex:= -1;
    end
  else
   if (Event.IsKey(keyEnd) or Event.IsKey(keyNumpadEnd)) and (Event.IsKey(keyCtrl)) then
     if mkShift in Event.ModifiersDown then
      begin
        LastIndex:= Text.Count - 1;
        LastIndexColumn:= UTF8Length(Text[LastIndex]);
      end else
      begin
        index:= Text.Count - 1;
        IndexColumn:= UTF8Length(Text[Index]);
        BolSelect:= False;
        LastIndex:= -1;
      end
  else
  if Event.IsKey(KeyEnd) or Event.IsKey(keyNumpadEnd) then
    if mkShift in Event.ModifiersDown then
      LastIndexColumn:= UTF8Length(Text[LastIndex])
    else
    begin
      IndexColumn:= UTF8Length(Text[Index]);
      BolSelect:= False;
      LastIndex:= -1;
    end
  else
  if Event.IsKey(keyHome) or Event.IsKey(keyNumpadHome) then
    if mkShift in Event.ModifiersDown then
       LastIndexColumn:= -1
    else
    begin
      IndexColumn:= -1;
      BolSelect:= False;
      LastIndex:= -1;
    end
  else
  if Event.IsKey(K_Enter) then
  begin
    i:=UTF8Length(UTF8Copy(Text[index],IndexColumn,UTF8Length(text[index])-IndexColumn));
    Text.Insert(Index+1,UTF8Copy(Text[index],IndexColumn+2,UTF8Length(text[index])-IndexColumn));   //UTF8Copy(Text[Index],IndexColumn+2,Length(Text[index])-IndexColumn+Length(Text[Index][Length(Text[Index])])));
    s:=Text[index];
    Utf8Delete(s,IndexColumn+2,utf8Length(Text[index]) - IndexColumn+1);
    Text[index]:=s;
    FTextChar.Insert(index,True);
    UndoSystem.AddAct(Index + 1, IndexColumn, Text[index], atEnter);
    Index:=Index+1;
    IndexColumn:=-1;
    DoChange;
  end
  else
  if (Event.EventType = itKey) and
     (Event.KeyString <> '') and
     (Event.KeyCharacter in AllowedChars) and
     ((MaxLength = 0) or (UTF8Length(Text[Index]) < MaxLength)) and
     { Do not add unprintable special characters, like Enter and Escape.
       TCastleMemo doesn't process them,
       but also users don't expect these keys are something added to the edit field.
       Note: The range CtrlA..CtrlZ includes CharEnter. }
     (not (Event.KeyCharacter in [CtrlA..CtrlZ, CharEscape])) then
  begin
     if LastIndex<>-1 then
      DeleteSelectText;
    s:=Text[index];
    UTF8Insert(Event.KeyString,s,IndexColumn+2);
    Text[index]:=s;
    UndoSystem.AddAct(index,indexcolumn,Event.KeyString,atAdd);
    IndexColumn:=IndexColumn+1;

    DoChange;
    Result := ExclusiveEvents;

  end else
  if Event.IsKey(CtrlZ) then
  begin
    UndoSystem.Undo;
    WritelnLog('Undo');
  end
  else
  if Event.IsKey(CtrlY) then
  begin
   UndoSystem.Rendo;
  end
  else BolSelect:= False ;
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

function TCastleMemo.WordWrap(index1: integer): integer;
var
  i,j,j1,j2:integer;
  s:string;
  MemoIndexs: TMemoIndexs;
begin
  j1:=0;
  j:=0;
  Result:=0;
  if index1 = -1 then
  begin
   if Text.Count > 0 then
   begin
    j1:= index;
    j2:= IndexColumn;
    index:= 0;
      if Text.Count > 1 then
      begin
        for i:= 0 to Text.Count-1 do
        begin
          index:= index + WordWrap(index+i);
        end;
      end else WordWrap(0);
    index:= j1;
    indexcolumn:= j2;
   end;
  end
  else
  if index1 < Text.Count then
  if Text[index1] <> '' then
  begin
  MemoIndexs:= GetCaret;
  j:= index1 + 1;
  if FTextChar.Count > j-1 then
    while FTextChar[j-1] = False do
    begin
         Text[index1]:= Text[index1]+Text[j];
         FTextChar.Delete(j-1);
         Text.Delete(j);
         if FTextChar.Count <= j-1 then
             break;
    end;
  j:=0;
  if  Text[index1].IndexOf(' ') <> -1 then
  begin
    while Text[index1].IndexOf(' ',j,UTF8Length(Text[index1])-j)<>-1 do
       begin
       j1:=j;
       j:=Text[index1].IndexOf(' ',j,UTF8Length(Text[index1])-j)+1;
       if Font.TextWidth(UTF8Copy(Text[index1],1,Text[index1].IndexOf(' ',j,UTF8Length(Text[index1])-j)))>RectInside.Width-ScrollBar.EffectiveScrollBarWidth then
          begin
               Text.Insert(index1+1,UTF8Copy(Text[index1],j+1,UTF8Length(Text[index1])-j1+1));
               s:=Text[index1];
               UTF8Delete(s,j+1,UTF8Length(Text[index1])-j);
               Text[index1]:=s;

               FTextChar.Insert(index1,False);
               Result:=Result+WordWrap(index1+1)+1;
               if (index = index1) and (IndexColumn > j) then
               begin
                  index:=index1+1;
                  Result:=Result-1;
               end;
               break;
          end;

       if j >= UTF8Length(Text[index1]) then break;
       end;
    end else if Font.TextWidth(Text[index])>RectInside.Width-ScrollBar.EffectiveScrollBarWidth then
      for i:=0 to UTF8Length(Text[index])-1 do
      if Font.TextWidth(UTF8Copy(Text[index],1,i))>RectInside.Width-ScrollBar.EffectiveScrollBarWidth then
       begin
       Text.Insert(index1+1,UTF8Copy(Text[index1],i,UTF8Length(Text[index1])-i+1));
       s:=Text[index1];
       UTF8Delete(s,i,UTF8Length(Text[index1])-i+1);
       Text[index1]:= s;
       FTextChar.insert(index1,False);
       Result:=Result + WordWrap(index1+1) + 1;

       if index = index1 then  index:= index1 + 1;
       break;
       end;
    SetCaret(MemoIndexs.Index1, MemoIndexs.IndexColumn1, -2, -2);
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
  indexVar:integer;
begin
  if FBolScroll=False then
    begin
      i:=Font.RowHeight+UIScale * (PaddingVertical + Padding)*2;
      if round(ScrollBar.Scroll-RectInside.Height/i)>0 then
      begin
      {if  round(ScrollBar.Scroll-RectInside.Height/i)<Text.Count-(RectInside.Height/i) then   }
      indexVar:=round(ScrollBar.CompleteSize/ScrollBar.Scroll);{ else
      indexVar:=round(Text.Count-(RectInside.Height/i)); }
      end
      else
      indexVar:=0;
      BottomText:=indexVar*i;
    end else
    begin
      if ScrollBar.Scroll<0 then ScrollBar.Scroll:=0;
      if ScrollBar.Scroll>ScrollBar.CompleteSize then ScrollBar.Scroll:=ScrollBar.CompleteSize;
    end;
end;

{$endif read_implementation}



{$ifdef read_interface}


  { TMemoUndo }

  TMemoUndo = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TStructList<TMemoUndoAction>)
    public
    Memo:^TCastleMemo;
    IndexAct:integer;

    procedure ChangeAct(NotNew:Boolean; index,indexColumn:integer;LastIndex:integer=-1;LastIndexColumn:integer=-1; Text:string=''; ActionType:TMemoUndoActionType=atAdd);
    procedure AddAct(AddBol: boolean; index,indexColumn:integer;LastIndex:integer=-1;LastIndexColumn:integer=-1;
      Text:string=''; ActionType:TMemoUndoActionType=atAdd);
    procedure AddAct(index,indexColumn:integer;Text:string;ActionType:TMemoUndoActionType);
    constructor Create; overload;
    destructor Destroy; override;
    procedure Undo;
    procedure Rendo;
    function ActionFleep(Action:TMemoUndoAction):TMemoUndoAction;
    procedure Action(index,indexColumn:integer;LastIndex:integer=-1;LastIndexColumn:integer=-1;
      Text:string='';ActionType:TMemoUndoActionType=atAdd);
    procedure Action(UndoAction:TMemoUndoAction);
    procedure Action1(UndoAction:TMemoUndoAction);
  end;
{$endif read_interface}

{$ifdef read_implementation}

procedure TMemoUndo.ChangeAct(NotNew: Boolean; index, indexColumn: integer;
  LastIndex: integer; LastIndexColumn: integer; Text: string;
  ActionType: TMemoUndoActionType);
var
  A:TMemoUndoAction;
begin
  if NotNew then
  begin
    if IndexAct < Count then
    begin
      WritelnLog('Ff'+IntToStr(IndexAct));
      A:= self[IndexAct];
      if index <> -2 then A.Index:= Index;
      if LastIndex <> -2 then A.LastIndex:= LastIndex;
      if indexColumn <> -2 then A.IndexColumn:= IndexColumn;
      if LastIndexColumn <> -2 then A.LastIndexColumn:= LastIndexColumn;
      if ActionType <> A.ActionType then A.ActionType:= ActionType;
      if Text <> '' then A.Text:= A.Text + Text;
      Self[IndexAct]:= A;
    end;
  end else AddAct(False,index, indexColumn, LastIndex, LastIndexColumn, Text, ActionType);
end;

procedure TMemoUndo.AddAct(AddBol: boolean; index, indexColumn: integer;
LastIndex: integer; LastIndexColumn: integer; Text: string;
ActionType: TMemoUndoActionType);
 var
   A:TMemoUndoAction;
   i:integer;
   P: TMemoUndoActPointer;
 begin
   WritelnLog('AddAct!  |   ');
    if AddBol then
    begin
       WritelnLog('HI' + IntToStr(IndexAct) + 'Count:' + IntToStr(Count));
      // WritelnLog('Index1_'+IntToStr(index)+'_IndexColumn1_'+IntToStr(indexColumn)+'_Text_'+Text);
       WritelnLog('_LastIndex_'+IntToStr(LastIndex)+'_LastIndexColumn_'+IntToStr(LastIndexColumn)+'|||||||||||');
       A:= self[IndexAct-1];
       A.index1:= Index;
       A.indexColumn1:= indexColumn;
       A.LastIndex1:= LastIndex;
       A.LastIndexColumn1:= LastIndexColumn;
       A.ActionType1:= ActionType;
       A.Text1:= Text;
       self[IndexAct-1]:= A;
    end
    else
    begin
      A.Index:= index;
      A.IndexColumn:= indexColumn;
      A.ActionType:= ActionType;
      A.LastIndex:= LastIndex;
      A.LastIndexColumn:= LastIndexColumn;
      A.Text:= Text;
      A.Index1:= -1;
      WritelnLog('AddMain_' + IntToStr(IndexAct) + 'Count:' + IntToStr(Count));
     // WritelnLog('Index1_'+IntToStr(index)+'_IndexColumn1_'+IntToStr(indexColumn)+'_Text_'+Text);
       WritelnLog('_LastIndex_'+IntToStr(LastIndex)+'_LastIndexColumn_'+IntToStr(LastIndexColumn)+'|||||||||||');

      if ActionType = atAdd then WritelnLog('add') else WritelnLog('delete');
      if IndexAct < Count then
       for i:= IndexAct to Count-1 do
           Delete(IndexAct); //+1
      Self.add(A);
      IndexAct:= IndexAct+1;
      WritelnLog(IntToStr(IndexAct));
      WritelnLog(IntToStr(Count));
    end;
 end;

 procedure TMemoUndo.AddAct(index, indexColumn: integer; Text: string;
   ActionType: TMemoUndoActionType);
 begin
   AddAct(False, index, indexColumn, -1, -1, Text, ActionType);
 end;

 constructor TMemoUndo.Create;
 begin
   inherited Create;
   IndexAct:=0;
   New(Memo);
 end;

 destructor TMemoUndo.Destroy;
begin
  Clear;
  inherited Destroy;
  Dispose(Memo);
end;

 procedure TMemoUndo.Undo;
 var
   AFleep:TMemoUndoAction;
   j:integer;
 begin
   WritelnLog('IndexAct'+IntToStr(IndexAct-1));
   if (Count > IndexAct-1) and (IndexAct-1 > -1) then
   begin
    if self[IndexAct-1].ActionType = atAdd then WritelnLog('Type Add') else
    if self[IndexAct-1].ActionType = atDelete then WritelnLog('Type Delete') else
    if self[IndexAct-1].ActionType = atEnter then WritelnLog('Type Enter');
    if self[IndexAct-1].ActionType1 = atAdd then WritelnLog('Type Add1') else
    if self[IndexAct-1].ActionType1 = atDelete then WritelnLog('Type Delete1') else
    if self[IndexAct-1].ActionType1 = atEnter then WritelnLog('Type Enter1');
    // WritelnLog(self[IndexAct-1].Text + '  ' + self[IndexAct-1].Text1);
     WritelnLog('IndexAct'+IntToStr(IndexAct-1));
     //for j:= 0 to Self.Count-1 do
      //WritelnLog('Index:'+IntToStr(self[j].Index)+' IndexColumn:'+IntToStr(self[j].IndexColumn)+' LastIndex:'+IntToStr(self[j].LastIndex)+' LastIndexColumn:'+IntToStr(self[j].LastIndexColumn)+' Text:'+self[j].Text);
     AFleep:= ActionFleep(self[IndexAct-1]);
     Action1(AFleep);
     Action(AFleep);
     {if Self[IndexAct - 1].SecondAction_onf then
         Action(ActionFleep(self[IndexAct-1].SecondAction));  }
     IndexAct:= IndexAct-1;
   end;
 end;

 procedure TMemoUndo.Rendo;
 begin
    if (Count > IndexAct)and(IndexAct > -1) then
   begin
   if self[IndexAct].ActionType = atAdd then WritelnLog('Type Add') else
    if self[IndexAct].ActionType = atDelete then WritelnLog('Type Delete') else
    if self[IndexAct].ActionType = atEnter then WritelnLog('Type Enter');
//  WritelnLog(self[IndexAct].Text);
  WritelnLog('IndexAct'+IntToStr(IndexAct));
  Action(self[IndexAct]);
  IndexAct:=IndexAct+1;
   end;
 end;

function TMemoUndo.ActionFleep(Action: TMemoUndoAction): TMemoUndoAction;
 begin
   Result:=Action;
   if Result.ActionType = atAdd then Result.ActionType:= atDelete
    else if Result.ActionType = atDelete then Result.ActionType:= atAdd
     else if Result.ActionType = atEnter then Result.ActionType:= atDeleteLine
      else if Result.ActionType = atDeleteLine then Result.ActionType:= atEnter;

   if Result.ActionType1 = atAdd then Result.ActionType1:= atDelete
    else if Result.ActionType1 = atDelete then Result.ActionType1:= atAdd
     else if Result.ActionType1 = atEnter then Result.ActionType1:= atDeleteLine
      else if Result.ActionType1 = atDeleteLine then Result.ActionType1:= atEnter;
 end;

 procedure TMemoUndo.Action(index, indexColumn: integer; LastIndex: integer;
    LastIndexColumn: integer; Text: string; ActionType: TMemoUndoActionType);
  var
    s:string;
    Lines:TStringList;
 begin
   if ActionType = atAdd then
    begin
      Lines:=TStringList.Create;
      Lines.Assign(Memo^.Lines);
      WritelnLog('ffffffff   '+Text);
      WritelnLog(IntToStr(LastIndex));
      //Memo^.Text.Insert(Memo^.Text.Count,IntToStr());
      if LastIndex <> -1 then
      begin
        if NOT(Index < Lines.Count) then
          Lines.Insert(Index,'');
        if index = LastIndex then
        begin
          s:= Lines[Index];
          if indexColumn < LastIndexColumn then
          begin
             UTF8Insert(Text, s, indexColumn + 2);
             Lines[Index]:= s;
             Memo^.Lines.Assign(Lines);
             Memo^.SetCaret(index, LastIndexColumn+1, -1, -1);
          end
          else
          begin
             UTF8Insert(Text, s, LastIndexColumn + 2);
             Lines[Index]:= s;
             Memo^.Lines.Assign(Lines);
             Memo^.SetCaret(index, IndexColumn+1, -1, -1);
          end;
          WritelnLog(UTF8Copy(s,UTF8Length(s)-10,10));
        end;
        WritelnLog('adding'+IntToStr(Memo^.indexColumn));
      end
      else
      begin
        if NOT(Index < Lines.Count) then
          Lines.Insert(Index,'');
         s:= Lines[Index];
        UTF8Insert(Text, s, indexColumn + 2);
        Lines[Index]:= s;
        Memo^.Lines.Assign(Lines);
        Memo^.SetCaret(index, indexColumn+1, -1, -1);
      end;
      Lines.Free;
    end
   else
   if ActionType = atDelete then
    begin
     if LastIndex < 0 then
     begin
      Lines:=TStringList.Create;
      Lines.Assign(Memo^.Lines);
     // Memo^.SetCaret(index, IndexColumn-UTF8Length(Text)+1);
      s:= Lines[Index];
      UTF8Delete(s, indexColumn+2, 1);
      Lines[Index]:=s;
      Memo^.Lines.Assign(Lines);
      Memo^.SetCaret(index, IndexColumn-UTF8Length(Text)+1);
      WritelnLog('delete');
      Lines.Free;
    end else
    begin
      Memo^.SetCaret(index, indexColumn, LastIndex, LastIndexColumn);
      Memo^.BolSelect:= True;
      WritelnLog('Index_'+IntToStr(Memo^.index)+'LastIndex_'+IntToStr(Memo^.LastIndex));
      WritelnLog('Index_'+IntToStr(Memo^.IndexColumn)+'LastIndex_'+IntToStr(Memo^.LastIndexColumn));
      Memo^.DeleteSelectText;
      WritelnLog('delete');
    end;
    end else if ActionType=atEnter then
     begin
        Lines.Insert(Index,UTF8Copy(Lines[index-1],IndexColumn+2,UTF8Length(Lines[index-1])-IndexColumn));
        s:= Lines[index-1];
        Utf8Delete(s,IndexColumn+2,utf8Length(Lines[index-1])-IndexColumn+1);
        Lines[index-1]:= s;
        Memo^.FTextChar.Insert(index-1,True);
        Memo^.Index:= Index+1;
        Memo^.IndexColumn:= -1;
        WritelnLog('addingIndex');
     end else if ActionType = atDeleteLine then
     begin
      Memo^.LastIndex:= index - 1;
      Memo^.LastIndexColumn:= indexColumn;
      Memo^.Index:= index;
      Memo^.IndexColumn:= -1;
      Memo^.BolSelect:= True;
      Memo^.DeleteSelectText;
      Memo^.BolSelect:= False;
      WritelnLog('DeleteLine!!!'+IntToStr(Index)+'gg'+IntToStr(indexColumn)+'ff'+IntToStr(index-1)+'ff'+IntToStr(-1));
     end;
    end;

  procedure TMemoUndo.Action(UndoAction: TMemoUndoAction);
  begin
   Action(UndoAction.Index,UndoAction.IndexColumn, UndoAction.LastIndex,UndoAction.LastIndexColumn, UndoAction.Text, UndoAction.ActionType);
  end;

  procedure TMemoUndo.Action1(UndoAction: TMemoUndoAction);
  var
    A:TMemoUndoAction;
  begin
    if UndoAction.index1 <> -1 then
    begin
      A.ActionType:= UndoAction.ActionType1;
      A.Index:= UndoAction.Index1;
      A.IndexColumn:= UndoAction.IndexColumn1;
      A.LastIndex:= UndoAction.LastIndex1;
      A.LastIndexColumn:= UndoAction.LastIndexColumn1;
      A.Text:= UndoAction.Text1;
      Action(A);
    end;
  end;

{$endif read_implementation}
