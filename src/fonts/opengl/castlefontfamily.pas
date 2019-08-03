{
  Copyright 2016-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Font family, with different subfonts for bold/italic variants (TFontFamily). }
unit CastleFontFamily;

{$I castleconf.inc}

interface

uses Classes, Generics.Collections,
  CastleColors, CastleVectors, CastleRectangles, CastleFonts;

type
  { Font family, with different subfonts for normal, bold, italic, bold+italic
    variants. Used together with TRichText, that has powerful methods to render
    and wrap text with HTML elements (<b>, <i> etc. inside).
    During such processing and rendering, it automatically uses the correct
    subfont. It's closely tied with the TRichText class.

    For simple operations, it simply uses the subfont indicated by the @link(Bold)
    and @italic(Italic) properties. By default they are @false, and then we simply
    use @link(RegularFont).
    This class can be treated as a font itself, since it has all the measuring
    and rendering commands you expect from a font (and you could
    even use it as a subfont of another TFontFamily --- weird but works,
    in which case the "TFontFamily used as a subfont" just acts as a proxy
    for the Regular subfont).

    Similar to TCustomizedFont, it can also change the subfont size.
    Simply set the @code(Size) property of this instance to non-zero
    to force the specific size of all the underlying subfonts.
    You can also change the subfont outline, if CustomizeOutline is used.
    The underlying font properties remain unchanged for subfonts
    (so they can be still used for other purposes,
    directly or by other TCustomizedFont or TFontFamily wrappers). }
  TFontFamily = class(TCastleFont)
  strict private
    FRegularFont, FBoldFont, FItalicFont, FBoldItalicFont: TCastleFont;
    FBold, FItalic: boolean;
    FCustomizeOutline: boolean;
    procedure SetRegularFont(const Value: TCastleFont);
    procedure SetBoldFont(const Value: TCastleFont);
    procedure SetItalicFont(const Value: TCastleFont);
    procedure SetBoldItalicFont(const Value: TCastleFont);
    procedure SubFontCustomizeBegin;
    procedure SubFontCustomizeEnd;
  private
    function SubFont(const ABold, AItalic: boolean): TCastleFont;
    function SubFont: TCastleFont;
  strict protected
    procedure GLContextClose; override;
    procedure Measure(out ARowHeight, ARowHeightBase, ADescend: Single); override;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property RegularFont: TCastleFont read FRegularFont write SetRegularFont;
    property BoldFont: TCastleFont read FBoldFont write SetBoldFont;
    property ItalicFont: TCastleFont read FItalicFont write SetItalicFont;
    property BoldItalicFont: TCastleFont read FBoldItalicFont write SetBoldItalicFont;

    property Bold: boolean read FBold write FBold default false;
    property Italic: boolean read FItalic write FItalic default false;

    procedure PrepareResources; override;
    procedure Print(const X, Y: Single; const Color: TCastleColor;
      const S: string); override;
    function TextWidth(const S: string): Single; override;
    function TextHeight(const S: string): Single; override;
    function TextHeightBase(const S: string): Single; override;
    function TextMove(const S: string): TVector2; override;
    function EffectiveSize: Single; override;

    { Should we customize the outline of the underlying font. }
    property CustomizeOutline: boolean read FCustomizeOutline write FCustomizeOutline default false;
  end;

  { @exclude Internal type for TRichText }
  TTextCommand = (
    tcBold, tcBoldEnd,
    tcItalic, tcItalicEnd,
    tcFontColor, tcFontSize, tcFontEnd,
    tcSmall, tcSmallEnd
  );

  TTextProperty = class;

  { Line of text with processing commands.
    @exclude Internal type for TRichText. }
  TTextLine = class(specialize TObjectList<TTextProperty>)
  strict private
    FWidthKnown: boolean;
    FWidth: Single;
    FFont: TFontFamily;
  public
    type
      TFontState = class
        Color: TCastleColor;
        Size: Single;
      end;
      TFontStateList = specialize TObjectList<TFontState>;

      TPrintState = class
        Color: TCastleColor;
        DefaultSize: Single;
        { Created on demand, only when some tcFontXxx command is seen. }
        FontStack: TFontStateList;
        { Note that Bold and Italic are expressed as integers,
          to smoothly handle <b> embeded in <b>. The value <> 0 indicates
          to use the given variant. }
        Bold, Italic: Cardinal;
        { The default Font.Bold, Font.Italic before whole TRichText processing. }
        RestoreBold, RestoreItalic: boolean;
        destructor Destroy; override;
      end;

    constructor Create(const AFont: TFontFamily);
    function Width(const State: TPrintState): Single;
    function DisplayChars(const State: TPrintState): Cardinal;
    function KnownWidth: Single;
    { Render line of text at given position. }
    procedure Print(const State: TPrintState; X0, Y0: Single;
      var MaxDisplayChars: Integer);
  end;

  { @exclude Internal type for TRichText }
  TTextProperty = class abstract
    procedure Print(const Font: TFontFamily;
      const State: TTextLine.TPrintState; var X0: Single; const Y0: Single;
      var MaxDisplayChars: Integer); virtual; abstract;
    function Wrap(const Font: TFontFamily; const State: TTextLine.TPrintState;
      var CurrentWidth: Single; const MaxWidth: Single;
      const CurrentLine: TTextLine; const CurrentPropertyIndex: Integer): TTextLine; virtual; abstract;
    function Width(const Font: TFontFamily; const State: TTextLine.TPrintState): Single; virtual; abstract;
    function DisplayChars(const Font: TFontFamily; const State: TTextLine.TPrintState): Cardinal; virtual; abstract;
  end;

  { @exclude Internal type for TRichText }
  TTextPropertyString = class(TTextProperty)
    S: string;
    procedure Print(const Font: TFontFamily;
      const State: TTextLine.TPrintState; var X0: Single; const Y0: Single;
      var MaxDisplayChars: Integer); override;
    { If there's a need to break, then:
      - this property is modified (cut),
      - CurrentLine is modified (cut),
      - new line is returned,
      - CurrentWidth is undefined.
      Otherwise, this moves forward, increasing CurrentWidth. }
    function Wrap(const Font: TFontFamily; const State: TTextLine.TPrintState;
      var CurrentWidth: Single; const MaxWidth: Single;
      const CurrentLine: TTextLine; const CurrentPropertyIndex: Integer): TTextLine; override;
    function Width(const Font: TFontFamily; const State: TTextLine.TPrintState): Single; override;
    function DisplayChars(const Font: TFontFamily; const State: TTextLine.TPrintState): Cardinal; override;
  end;

  { @exclude Internal type for TRichText }
  TTextPropertyCommand = class(TTextProperty)
  strict private
    procedure UpdateState(const Font: TFontFamily;
      const State: TTextLine.TPrintState);
  public
    Command: TTextCommand;
    Color: TCastleColor;
    HtmlSize: Integer;
    PercentSize: Single;
    procedure Print(const Font: TFontFamily;
      const State: TTextLine.TPrintState; var X0: Single; const Y0: Single;
      var MaxDisplayChars: Integer); override;
    function Wrap(const Font: TFontFamily; const State: TTextLine.TPrintState;
      var CurrentWidth: Single; const MaxWidth: Single;
      const CurrentLine: TTextLine; const CurrentPropertyIndex: Integer): TTextLine; override;
    function Width(const Font: TFontFamily; const State: TTextLine.TPrintState): Single; override;
    function DisplayChars(const Font: TFontFamily; const State: TTextLine.TPrintState): Cardinal; override;
  end;

  { Multi-line text with processing commands
    (like "change font to bold now"). Used to render "rich text",
    which is text that may contain (a subset of) HTML.

    Note that TRichText instance is always tied to a corresponding
    TFontFamily used to render it. @bold(Through the lifetime of TRichText,
    we assume that size and other properties of this font remain constant.) }
  TRichText = class(specialize TObjectList<TTextLine>)
  strict private
    FWidthKnown: boolean;
    { Known max line width, e.g. calculated by @link(Wrap).
      Using this allows to avoid recalculating this many times,
      e.g. @link(Wrap) always calculates this as a by-product of it's work. }
    FWidth: Single;
    FFont: TFontFamily;
    FOwnsFont: boolean;
    procedure SetTextWithoutHtml(Text: TStrings);
    procedure SetTextWithHtml(Text: TStrings);
    procedure AddTextWithHtml(const S: string);
    function BeginProcessing(const InitialColor: TCastleColor): TTextLine.TPrintState;
    procedure EndProcessing(var State: TTextLine.TPrintState);
  public
    constructor Create(const AFont: TCastleFont;
      const Text: TStrings; const Html: boolean);
    constructor Create(const AFont: TCastleFont;
      const S: string; const Html: boolean);
    destructor Destroy; override;
    function Width: Single;
    procedure Wrap(const MaxWidth: Single);
    procedure Print(const X, Y: Single; const Color: TCastleColor;
      const ALineSpacing: Single;
      const TextHorizontalAlignment: THorizontalPosition = hpLeft;
      MaxDisplayChars: Integer = -1);
    function DisplayChars: Cardinal;
  end;

implementation

uses SysUtils, StrUtils, Math,
  CastleUtils, CastleStringUtils, CastleLog, CastleUnicode;

{ TFontFamily ------------------------------------------------------------ }

constructor TFontFamily.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TFontFamily.Destroy;
begin
  // this will free FXxxFont if needed
  RegularFont := nil;
  BoldFont := nil;
  ItalicFont := nil;
  BoldItalicFont := nil;
  inherited;
end;

procedure TFontFamily.SetRegularFont(const Value: TCastleFont);
begin
  if FRegularFont <> Value then
  begin
    if FRegularFont <> nil then
      FRegularFont.RemoveFreeNotification(Self);
    FRegularFont := Value;
    if FRegularFont <> nil then
      FRegularFont.FreeNotification(Self);
  end;
end;

procedure TFontFamily.SetBoldFont(const Value: TCastleFont);
begin
  if FBoldFont <> Value then
  begin
    if FBoldFont <> nil then
      FBoldFont.RemoveFreeNotification(Self);
    FBoldFont := Value;
    if FBoldFont <> nil then
      FBoldFont.FreeNotification(Self);
  end;
end;

procedure TFontFamily.SetItalicFont(const Value: TCastleFont);
begin
  if FItalicFont <> Value then
  begin
    if FItalicFont <> nil then
      FItalicFont.RemoveFreeNotification(Self);
    FItalicFont := Value;
    if FItalicFont <> nil then
      FItalicFont.FreeNotification(Self);
  end;
end;

procedure TFontFamily.SetBoldItalicFont(const Value: TCastleFont);
begin
  if FBoldItalicFont <> Value then
  begin
    if FBoldItalicFont <> nil then
      FBoldItalicFont.RemoveFreeNotification(Self);
    FBoldItalicFont := Value;
    if FBoldItalicFont <> nil then
      FBoldItalicFont.FreeNotification(Self);
  end;
end;

procedure TFontFamily.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  { set to nil by SetXxxFont to clean nicely }
  if (Operation = opRemove) and (AComponent = FRegularFont) then
    RegularFont := nil;
  if (Operation = opRemove) and (AComponent = FBoldFont) then
    BoldFont := nil;
  if (Operation = opRemove) and (AComponent = FItalicFont) then
    ItalicFont := nil;
  if (Operation = opRemove) and (AComponent = FBoldItalicFont) then
    BoldItalicFont := nil;
end;

procedure TFontFamily.PrepareResources;
begin
  if FRegularFont <> nil then
    FRegularFont.PrepareResources;
  if FBoldFont <> nil then
    FBoldFont.PrepareResources;
  if FItalicFont <> nil then
    FItalicFont.PrepareResources;
  if FBoldItalicFont <> nil then
    FBoldItalicFont.PrepareResources;
end;

procedure TFontFamily.GLContextClose;
begin
  if FRegularFont <> nil then
    FRegularFont.GLContextClose;
  if FBoldFont <> nil then
    FBoldFont.GLContextClose;
  if FItalicFont <> nil then
    FItalicFont.GLContextClose;
  if FBoldItalicFont <> nil then
    FBoldItalicFont.GLContextClose;
end;

procedure TFontFamily.SubFontCustomizeBegin;
begin
  if (Size <> 0) or CustomizeOutline then
  begin
    SubFont.PushProperties;
    if Size <> 0 then
      SubFont.Size := Size;
    if CustomizeOutline then
    begin
      SubFont.Outline := Outline;
      SubFont.OutlineColor := OutlineColor;
      SubFont.OutlineHighQuality := OutlineHighQuality;
    end;
  end;
end;

procedure TFontFamily.SubFontCustomizeEnd;
begin
  if Size <> 0 then
    SubFont.PopProperties;
end;

procedure TFontFamily.Print(const X, Y: Single; const Color: TCastleColor;
  const S: string);
begin
  SubFontCustomizeBegin;
  SubFont.Print(X, Y, Color, S);
  SubFontCustomizeEnd;
end;

function TFontFamily.TextWidth(const S: string): Single;
begin
  SubFontCustomizeBegin;
  Result := SubFont.TextWidth(S);
  SubFontCustomizeEnd;
end;

function TFontFamily.TextHeight(const S: string): Single;
begin
  SubFontCustomizeBegin;
  Result := SubFont.TextHeight(S);
  SubFontCustomizeEnd;
end;

function TFontFamily.TextHeightBase(const S: string): Single;
begin
  SubFontCustomizeBegin;
  Result := SubFont.TextHeightBase(S);
  SubFontCustomizeEnd;
end;

function TFontFamily.TextMove(const S: string): TVector2;
begin
  SubFontCustomizeBegin;
  Result := SubFont.TextMove(S);
  SubFontCustomizeEnd;
end;

function TFontFamily.SubFont(const ABold, AItalic: boolean): TCastleFont;
begin
  if ABold and AItalic and (BoldItalicFont <> nil) then
    Result := BoldItalicFont else
  if ABold and (BoldFont <> nil) then
    Result := BoldFont else
  if AItalic and (ItalicFont <> nil) then
    Result := ItalicFont else
  if RegularFont <> nil then
    Result := RegularFont else
    raise Exception.Create('You must set at least RegularFont of TFontFamily to use it for processing and rendering');
end;

function TFontFamily.SubFont: TCastleFont;
begin
  Result := SubFont(Bold, Italic);
end;

function TFontFamily.EffectiveSize: Single;
begin
  if Size <> 0 then
    Result := Size
  else
    Result := SubFont.EffectiveSize;
end;

procedure TFontFamily.Measure(out ARowHeight, ARowHeightBase, ADescend: Single);
begin
  { See TCustomizedFont.Measure for explanation why we need to override
    Measure. }
  SubFontCustomizeBegin;
  SubFont.Measure(ARowHeight, ARowHeightBase, ADescend);
  SubFontCustomizeEnd;
end;

{ TPrintState ---------------------------------------------------------------- }

destructor TTextLine.TPrintState.Destroy;
begin
  FreeAndNil(FontStack);
  inherited;
end;

{ TTextPropertyString -------------------------------------------------------- }

procedure TTextPropertyString.Print(const Font: TFontFamily;
  const State: TTextLine.TPrintState; var X0: Single; const Y0: Single;
  var MaxDisplayChars: Integer);
var
  L: Integer;
begin
  if MaxDisplayChars <> -1 then
  begin
    L := UTF8Length(S);
    if MaxDisplayChars >= L then
    begin
      MaxDisplayChars -= L;
      Font.Print(X0, Y0, State.Color, S);
    end else
    begin
      Font.Print(X0, Y0, State.Color, UTF8Copy(S, 1, MaxDisplayChars));
      MaxDisplayChars := 0;
    end;
  end else
    Font.Print(X0, Y0, State.Color, S);
  X0 += Font.TextWidth(S);
end;

function TTextPropertyString.Wrap(const Font: TFontFamily; const State: TTextLine.TPrintState;
  var CurrentWidth: Single; const MaxWidth: Single;
  const CurrentLine: TTextLine; const CurrentPropertyIndex: Integer): TTextLine;

  { Split line in the middle of this TTextPropertyString. }
  procedure BreakLine(const PropWidthBytes: Integer);
  var
    NewProp: TTextPropertyString;
    ExtractedProp: TTextProperty;
    BreakOutput1, BreakOutput2: string;
    P: Integer;
  begin
    { We have to break this line now. }
    P := BackCharsPos(WhiteSpaces, Copy(S, 1, PropWidthBytes));
    if P > 0 then
    begin
      BreakOutput1 := Copy(S, 1, P - 1);
      BreakOutput2 := SEnding(S, P + 1) { break at pos p, delete p-th char }
    end else
    begin
      { Code in case we don't find a whitespace on which to break.

        Note: we used to have here logic like this:

          if LineIsNonEmptyAlready then // calculated by looking at CurrentWidth <> 0 when TTextPropertyString.Wrap starts
            BreakOutput1 := ''
          else
            BreakOutput1 := Copy(S, 1, PropWidthBytes);

        In effect, if we have "blablah<b>foobar</b>", we would prefer to break
        "blablah" + newline + "<b>foobar</b>" (if this would make both lines fit in MaxWidth)
        instead of "blablah<b>foo" + newline + "bar</b>".
        But it seemed incorrect: in any case we are breaking in the middle of
        the word (not at space),
        so why do we favor the place where font style (bold/non-bold) changes?
        In particular it looked incorrect in Japanese, where there are no spaces
        between words, and any place is a valid breaking point.
        So the line should should be typically long in Japanese and it should
        be broken where it doesn't fit (not earlier, when bold/non-bold changes).
      }
      BreakOutput1 := Copy(S, 1, PropWidthBytes);
      BreakOutput2 := SEnding(S, Length(BreakOutput1) + 1);
    end;

    { now leave BreakOutput1 in this line, and add BreakOutput2
      (along with remaining properties on this line) to new line }
    S := BreakOutput1;

    NewProp := TTextPropertyString.Create;
    NewProp.S := BreakOutput2;

    Result := TTextLine.Create(Font);
    Result.Add(NewProp);

    while CurrentLine.Count > CurrentPropertyIndex + 1 do
    begin
      ExtractedProp := CurrentLine.Items[CurrentPropertyIndex + 1];
      CurrentLine.Extract(ExtractedProp);
      Result.Add(ExtractedProp);
    end;
  end;

var
  PropWidthBytes: Integer;
  C: TUnicodeChar;
  SPtr: PChar;
  CharLen: Integer;
begin
  Result := nil;
  SPtr := PChar(S);

  { If S is empty, there's no need to break it. }
  C := UTF8CharacterToUnicode(SPtr, CharLen);
  if (C > 0) and (CharLen > 0) then
  begin
    { the first character C is always considered to fit into BreakOutput1,
      regardless of MaxWidth --- after all, we have to place this character
      somewhere (otherwise we would have to reject this character
      or raise an error). }
    Inc(SPtr, CharLen);
    PropWidthBytes := CharLen;
    CurrentWidth += Font.TextWidth(UnicodeToUTF8(C));

    C := UTF8CharacterToUnicode(SPtr, CharLen);
    while (C > 0) and (CharLen > 0) and
          (CurrentWidth + Font.TextWidth(UnicodeToUTF8(C)) <= MaxWidth) do
    begin
      Inc(SPtr, CharLen);
      PropWidthBytes += CharLen;
      CurrentWidth += Font.TextWidth(UnicodeToUTF8(C));

      C := UTF8CharacterToUnicode(SPtr, CharLen);
    end;

    if (C > 0) and (CharLen > 0) then // then above loop stopped because we have to break
      BreakLine(PropWidthBytes);
  end;
end;

function TTextPropertyString.Width(const Font: TFontFamily; const State: TTextLine.TPrintState): Single;
begin
  Result := Font.TextWidth(S);
end;

function TTextPropertyString.DisplayChars(const Font: TFontFamily; const State: TTextLine.TPrintState): Cardinal;
begin
  Result := UTF8Length(S);
end;

{ TTextPropertyCommand -------------------------------------------------------- }

procedure TTextPropertyCommand.Print(const Font: TFontFamily;
  const State: TTextLine.TPrintState; var X0: Single; const Y0: Single;
  var MaxDisplayChars: Integer);
begin
  UpdateState(Font, State);
end;

function TTextPropertyCommand.Wrap(const Font: TFontFamily; const State: TTextLine.TPrintState;
  var CurrentWidth: Single; const MaxWidth: Single;
  const CurrentLine: TTextLine; const CurrentPropertyIndex: Integer): TTextLine;
begin
  UpdateState(Font, State);
  Result := nil;
end;

function TTextPropertyCommand.Width(const Font: TFontFamily; const State: TTextLine.TPrintState): Single;
begin
  UpdateState(Font, State);
  Result := 0;
end;

function TTextPropertyCommand.DisplayChars(const Font: TFontFamily; const State: TTextLine.TPrintState): Cardinal;
begin
  UpdateState(Font, State);
  Result := 0;
end;

procedure TTextPropertyCommand.UpdateState(const Font: TFontFamily;
  const State: TTextLine.TPrintState);
const
  { Chosen constant seems to match Google Chrome's <small> sizing.
    Firefox seems to have more complex equation for <small>
    (effect of <small> seems to depend non-linearly on the parent size). }
  SizeSmaller = 0.825;
  SizeLarger = 1 / SizeSmaller;

  { Convert HTML size as for <font>
    https://developer.mozilla.org/en-US/docs/Web/HTML/Element/font to our font size. }
  function HtmlSizeToMySize(const HtmlSize: Integer; const DefaultSize: Single): Single;
  begin
    if HtmlSize < 3 then
      Result := DefaultSize * Power(SizeSmaller, 3 - HtmlSize) else
    if HtmlSize > 3 then
      Result := DefaultSize * Power(SizeLarger, HtmlSize - 3) else
      Result := DefaultSize;
  end;

var
  FontState: TTextLine.TFontState;
begin
  case Command of
    tcBold:
      begin
        Inc(State.Bold);
        Font.Bold := State.Bold <> 0;
      end;
    tcBoldEnd:
      if State.Bold = 0 then
        WritelnWarning('HTML', 'Mismatched </b>') else
      begin
        Dec(State.Bold);
        Font.Bold := State.Bold <> 0;
      end;
    tcItalic:
      begin
        Inc(State.Italic);
        Font.Italic := State.Italic <> 0;
      end;
    tcItalicEnd:
      if State.Italic = 0 then
        WritelnWarning('HTML', 'Mismatched </i>') else
      begin
        Dec(State.Italic);
        Font.Italic := State.Italic <> 0;
      end;
    tcFontColor:
      begin
        if State.FontStack = nil then
          State.FontStack := TTextLine.TFontStateList.Create;
        FontState := TTextLine.TFontState.Create;
        FontState.Color := State.Color;
        FontState.Size := Font.EffectiveSize;
        State.FontStack.Add(FontState);

        State.Color := Color;
      end;
    tcFontSize:
      begin
        if State.FontStack = nil then
          State.FontStack := TTextLine.TFontStateList.Create;
        FontState := TTextLine.TFontState.Create;
        FontState.Color := State.Color;
        FontState.Size := Font.EffectiveSize;
        State.FontStack.Add(FontState);

        if HtmlSize <> 0 then
          Font.Size := HtmlSizeToMySize(HtmlSize, State.DefaultSize) else
          Font.Size := PercentSize * State.DefaultSize;
      end;
    tcSmall:
      begin
        if State.FontStack = nil then
          State.FontStack := TTextLine.TFontStateList.Create;
        FontState := TTextLine.TFontState.Create;
        FontState.Color := State.Color;
        FontState.Size := Font.EffectiveSize;
        State.FontStack.Add(FontState);

        Font.Size := Font.EffectiveSize * SizeSmaller;
      end;
    tcFontEnd, tcSmallEnd:
      if (State.FontStack = nil) or (State.FontStack.Count = 0) then
        WritelnWarning('HTML', 'Mismatched </font> or </small>') else
      begin
        FontState := State.FontStack.Last;
        State.Color := FontState.Color;
        Font.Size := FontState.Size;
        State.FontStack.Delete(State.FontStack.Count - 1); // remove last, popping from stack
      end;
    else raise EInternalError.Create('TTextPropertyCommand.Print unknown enum');
  end;
end;

{ TTextLine ------------------------------------------------------------------ }

constructor TTextLine.Create(const AFont: TFontFamily);
begin
  inherited Create(true);
  FFont := AFont;
end;

function TTextLine.Width(const State: TPrintState): Single;
var
  I: Integer;
begin
  { note that this cannot just return FWidth, even if FWidthKnown,
    because we still have to iterate over our properties to update State. }
  FWidthKnown := true;
  FWidth := 0;
  for I := 0 to Count - 1 do
    FWidth += Items[I].Width(FFont, State);
  Result := FWidth;
end;

function TTextLine.DisplayChars(const State: TPrintState): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Result += Items[I].DisplayChars(FFont, State);
end;

function TTextLine.KnownWidth: Single;
begin
  if not FWidthKnown then
    raise Exception.Create('Line width not known yet');
  Result := FWidth;
end;

procedure TTextLine.Print(const State: TPrintState; X0, Y0: Single;
  var MaxDisplayChars: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Print(FFont, State, X0, Y0, MaxDisplayChars);
end;

{ TRichText ------------------------------------------------------------------ }

constructor TRichText.Create(const AFont: TCastleFont;
  const Text: TStrings; const Html: boolean);
begin
  inherited Create(true);

  if AFont is TFontFamily then
  begin
    FFont := AFont as TFontFamily;
    FOwnsFont := false;
  end else
  begin
    FFont := TFontFamily.Create(nil);
    FFont.RegularFont := AFont;
    FOwnsFont := true;
    { Do not make this warning by default? Too talkative sometimes,
      esp. at every Print call,
      and as HTML may be useful without TFontFamily sometimes.

    if Html then
      WritelnWarning('HTML', 'Rendering HTML text with simple font (' +
        AFont.ClassName + ':' + AFont.Name +
        '), without bold/italic variants. <b> and <i> will not have any effect');
    }
  end;

  if Html then
    SetTextWithHtml(Text) else
    SetTextWithoutHtml(Text);
end;

constructor TRichText.Create(const AFont: TCastleFont;
  const S: string; const Html: boolean);
var
  Text: TStringList;
begin
  Text := TStringList.Create;
  try
    Text.Text := S;
    Create(AFont, Text, Html);
  finally FreeAndNil(Text) end;
end;

destructor TRichText.Destroy;
begin
  if FOwnsFont then
    FreeAndNil(FFont);
  inherited;
end;

procedure TRichText.SetTextWithoutHtml(Text: TStrings);
var
  PropS: TTextPropertyString;
  I: Integer;
begin
  Count := Text.Count;
  for I := 0 to Text.Count - 1 do
  begin
    PropS := TTextPropertyString.Create;
    PropS.S := Text[I];
    Items[I] := TTextLine.Create(FFont);
    Items[I].Add(PropS);
  end;
end;

procedure TRichText.SetTextWithHtml(Text: TStrings);
var
  I: Integer;
begin
  Clear;
  Capacity := Text.Count;
  for I := 0 to Text.Count - 1 do
    AddTextWithHtml(Text[I]);
end;

procedure TRichText.AddTextWithHtml(const S: string);
var
  TextLine: TTextLine;

  { Test is given substring at position I within larger string S.
    Does it fast (without creating a temporary copy for a subtring from S).
    Case-sensitive. }
  function SubstringStartsHere(const S: string; const I: Integer;
    const Substring: string; out NextChar: Integer): boolean;
  var
    J, SubstringLength, SIndex: Integer;
  begin
    SubstringLength := Length(Substring);
    NextChar := I + SubstringLength;
    if NextChar > Length(S) + 1 then
      Exit(false); // Substring too long
    SIndex := I;
    for J := 1 to SubstringLength do
    begin
      if Substring[J] <> S[SIndex] then
        Exit(false);
      Inc(SIndex);
    end;
    Result := true;
  end;

  function EntityFound(const S: string; const I: Integer;
    out NextChar: Integer): TTextPropertyString;
  begin
    if SubstringStartsHere(S, I, '&amp;', NextChar) then
    begin
      Result := TTextPropertyString.Create;
      Result.S := '&';
    end else
    if SubstringStartsHere(S, I, '&lt;', NextChar) then
    begin
      Result := TTextPropertyString.Create;
      Result.S := '<';
    end else
    if SubstringStartsHere(S, I, '&gt;', NextChar) then
    begin
      Result := TTextPropertyString.Create;
      Result.S := '>';
    end else
    if SubstringStartsHere(S, I, '&apos;', NextChar) then
    begin
      Result := TTextPropertyString.Create;
      Result.S := '''';
    end else
    if SubstringStartsHere(S, I, '&quot;', NextChar) then
    begin
      Result := TTextPropertyString.Create;
      Result.S := '"';
    end else
      Result := nil;
  end;

  { @raises EConvertError In case of invalid color hexadecimal value. }
  function ReadFontColor(const S: string; const I: Integer;
    out NextChar: Integer; out Color: TCastleColor): boolean;
  var
    EndPos: Integer;
  const
    HexDigits = ['0'..'9', 'a'..'f', 'A'..'F'];
  begin
    EndPos := PosEx('">', S, I);
    NextChar := EndPos + 2;
    { we have to find ">, and have exactly 6 or 8 characters for color }
    Result := (EndPos = I + 6) or (EndPos = I + 8);

    Color[0] := StrHexToInt(Copy(S, I    , 2)) / 255;
    Color[1] := StrHexToInt(Copy(S, I + 2, 2)) / 255;
    Color[2] := StrHexToInt(Copy(S, I + 4, 2)) / 255;
    if EndPos = I + 8 then
      Color[3] := StrHexToInt(Copy(S, I + 6, 2)) / 255 else
      Color[3] := 1.0;
  end;

  function ReadFontSize(const S: string; const I: Integer;
    out NextChar: Integer; out HtmlSize: Integer; out PercentSize: Single): boolean;
  var
    EndPos: Integer;
    SizeRead: Int64;
    NumStr: string;
  begin
    EndPos := PosEx('">', S, I);
    NextChar := EndPos + 2;
    Result := EndPos <> 0;
    SizeRead := 0;
    PercentSize := 0;

    NumStr := Copy(S, I, EndPos - I);

    if IsSuffix('%', NumStr, false) then
    begin
      if not TryStrToFloat(SuffixRemove('%', NumStr, false), PercentSize) then
        Exit(false);
      PercentSize := PercentSize / 100;
    end else
    begin
      if not TryStrToInt64(NumStr, SizeRead) then
        Exit(false);
      if S[I] in ['+', '-'] then
        { size is relative to 3.
          See https://developer.mozilla.org/en-US/docs/Web/HTML/Element/font }
        HtmlSize := Clamped(3 + SizeRead, 1, 7)
      else
        HtmlSize := Clamped(SizeRead, 1, 7);
    end;
  end;

  function CommandFound(const S: string; const I: Integer;
    out NextChar: Integer): TTextPropertyCommand;
  begin
    if SubstringStartsHere(S, I, '<b>', NextChar) then
    begin
      Result := TTextPropertyCommand.Create;
      Result.Command := tcBold;
    end else
    if SubstringStartsHere(S, I, '</b>', NextChar) then
    begin
      Result := TTextPropertyCommand.Create;
      Result.Command := tcBoldEnd;
    end else
    if SubstringStartsHere(S, I, '<i>', NextChar) then
    begin
      Result := TTextPropertyCommand.Create;
      Result.Command := tcItalic;
    end else
    if SubstringStartsHere(S, I, '</i>', NextChar) then
    begin
      Result := TTextPropertyCommand.Create;
      Result.Command := tcItalicEnd;
    end else
    if SubstringStartsHere(S, I, '<font color="#', NextChar) then
    begin
      Result := TTextPropertyCommand.Create;
      Result.Command := tcFontColor;
      if not ReadFontColor(S, NextChar, NextChar, Result.Color) then
        FreeAndNil(Result); // resign, not correct
    end else
    if SubstringStartsHere(S, I, '<font size="', NextChar) then
    begin
      Result := TTextPropertyCommand.Create;
      Result.Command := tcFontSize;
      if not ReadFontSize(S, NextChar, NextChar, Result.HtmlSize, Result.PercentSize) then
        FreeAndNil(Result); // resign, not correct
    end else
    if SubstringStartsHere(S, I, '</font>', NextChar) then
    begin
      Result := TTextPropertyCommand.Create;
      Result.Command := tcFontEnd;
    end else
    if SubstringStartsHere(S, I, '<small>', NextChar) then
    begin
      Result := TTextPropertyCommand.Create;
      Result.Command := tcSmall;
    end else
    if SubstringStartsHere(S, I, '</small>', NextChar) then
    begin
      Result := TTextPropertyCommand.Create;
      Result.Command := tcSmallEnd;
    end else
      Result := nil;
  end;

  function CommentFound(const S: string; const I: Integer;
    out NextChar: Integer): TTextPropertyString;
  var
    EndPos: Integer;
  begin
    Result := nil;
    if SubstringStartsHere(S, I, '<!--', NextChar) then
    begin
      EndPos := PosEx('-->', S, NextChar);
      if EndPos <> 0 then
      begin
        NextChar := EndPos + 3;
        Result := TTextPropertyString.Create; // with empty S
      end;
    end;
  end;

  function SpecialFound(const S: string; const I: Integer;
    out NextChar: Integer): TTextProperty;
  begin
    if S[I] = '&' then
      Result := EntityFound(S, I, NextChar) else
    if S[I] = '<' then
    begin
      if SCharIs(S, I + 1, '!') then
        Result := CommentFound(S, I, NextChar) else
      { simply ignore the </p>, we handle <p> correctly already }
      if SubstringStartsHere(S, I, '</p>', NextChar) then
      begin
        Result := TTextPropertyString.Create; // with empty S
      end else
        Result := CommandFound(S, I, NextChar);
    end else
      Result := nil;
  end;

  procedure AddRemainingString(const I, Done: Integer);
  var
    PropS: TTextPropertyString;
  begin
    if Done + 1 < I then
    begin
      PropS := TTextPropertyString.Create;
      PropS.S := Copy(S, Done + 1, I - 1 - Done);
      TextLine.Add(PropS);
    end;
  end;

var
  I, Done, NextChar, SLength: Integer;
  SLowerCase: string;
  PropSpecial: TTextProperty;
begin
  TextLine := TTextLine.Create(FFont);

  { we search lowercase substrings inside this, thus we ignore case automatically
    in the rest of the processing code }
  SLowerCase := AnsiLowerCase(S);

  Done := 0;
  I := 1;
  SLength := Length(S);

  while I <= SLength do
  begin
    { for speed, quickly test and move on if this isn't any special character }
    if S[I] in ['<', '&'] then
    begin
      { handle line-breaking elements specially here }
      if SubstringStartsHere(SLowerCase, I, '<p>', NextChar) then
      begin
        AddRemainingString(I, Done);
        Add(TextLine); TextLine := TTextLine.Create(FFont);
        Add(TextLine); TextLine := TTextLine.Create(FFont);
        Done := NextChar - 1;
        I := NextChar;
      end else
      if SubstringStartsHere(SLowerCase, I, '<br>', NextChar) or
         SubstringStartsHere(SLowerCase, I, '<br/>', NextChar) or
         SubstringStartsHere(SLowerCase, I, '<br />', NextChar) then
      begin
        AddRemainingString(I, Done);
        Add(TextLine); TextLine := TTextLine.Create(FFont);
        Done := NextChar - 1;
        I := NextChar;
      end else
      { handle entities and commands,
        that break a text into multiple TTextProperty instances }
      begin
        PropSpecial := SpecialFound(SLowerCase, I, NextChar);
        if PropSpecial <> nil then
        begin
          AddRemainingString(I, Done);
          TextLine.Add(PropSpecial);
          Done := NextChar - 1;
          I := NextChar;
        end else
          Inc(I);
      end;
    end else
      Inc(I);
  end;

  AddRemainingString(I, Done);
  Add(TextLine);
end;

function TRichText.BeginProcessing(const InitialColor: TCastleColor): TTextLine.TPrintState;
begin
  Result := TTextLine.TPrintState.Create;

  { set FFont into Bold = Italic = false state }
  Result.RestoreBold := FFont.Bold;
  Result.RestoreItalic := FFont.Italic;
  FFont.Bold := false;
  FFont.Italic := false;

  Result.Color := InitialColor;
  Result.DefaultSize := FFont.EffectiveSize;
end;

procedure TRichText.EndProcessing(var State: TTextLine.TPrintState);
begin
  FFont.Bold := State.RestoreBold;
  FFont.Italic := State.RestoreItalic;

  FreeAndNil(State);
end;

procedure TRichText.Print(const X, Y: Single; const Color: TCastleColor;
  const ALineSpacing: Single;
  const TextHorizontalAlignment: THorizontalPosition;
  MaxDisplayChars: Integer);
var
  X0, Y0: Single;
  LineSpacing: Single;

  function XPos(const Line: Integer): Single;
  begin
    case TextHorizontalAlignment of
      hpLeft  : Result := X0;
      hpMiddle: Result := X0 - Items[Line].KnownWidth / 2;
      hpRight : Result := X0 - Items[Line].KnownWidth;
      else raise EInternalError.Create('TRichText.Print: TextHorizontalAlignment unknown');
    end;
  end;

var
  RowHeight: Single;

  function YPos(const Line: Integer): Single;
  begin
    Result := (Count - 1 - Line) * (RowHeight + LineSpacing) + Y0;
  end;

var
  State: TTextLine.TPrintState;
  I: Integer;
begin
  X0 := X;
  Y0  := Y;
  LineSpacing := ALineSpacing;

  { force calculating FWidth for all lines }
  if TextHorizontalAlignment in [hpMiddle, hpRight] then
    Width;

  State := BeginProcessing(Color);
  try
    RowHeight := FFont.RowHeight;
    for I := 0 to Count - 1 do
      Items[I].Print(State, XPos(I), YPos(I), MaxDisplayChars);
  finally EndProcessing(State) end;
end;

function TRichText.Width: Single;
var
  I: Integer;
  State: TTextLine.TPrintState;
begin
  if not FWidthKnown then
  begin
    FWidthKnown := true;
    FWidth := 0;
    State := BeginProcessing(Black { any color });
    try
      for I := 0 to Count - 1 do
        MaxVar(FWidth, Items[I].Width(State));
    finally EndProcessing(State) end;
  end;
  Result := FWidth;
end;

function TRichText.DisplayChars: Cardinal;
var
  I: Integer;
  State: TTextLine.TPrintState;
begin
  Result := 0;
  State := BeginProcessing(Black { any color });
  try
    for I := 0 to Count - 1 do
      Result += Items[I].DisplayChars(State);
  finally EndProcessing(State) end;
end;

procedure TRichText.Wrap(const MaxWidth: Single);
var
  I, J: Integer;
  LineWidth: Single;
  Line, NewLine: TTextLine;
  State: TTextLine.TPrintState;
begin
  State := BeginProcessing(Black { any color });
  try
    I := 0;
    { note that the current Count will be changing, as we will split our own strings }
    while I < Count do
    begin
      Line := Items[I];
      LineWidth := 0;
      for J := 0 to Line.Count - 1 do
      begin
        NewLine := Line[J].Wrap(FFont, State, LineWidth, Ceil(MaxWidth), Line, J);
        if NewLine <> nil then
        begin
          { next I loop iteration will break the next line,
            which is the remainder of current line }
          Insert(I + 1, NewLine);
          Break; // Break from for J loop
        end;
      end;
      Inc(I);
    end;
  finally EndProcessing(State) end;
end;

end.
