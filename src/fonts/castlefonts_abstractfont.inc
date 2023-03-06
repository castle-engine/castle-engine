{%MainUnit castlefonts.pas}
{
  Copyright 2001-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

type
  { Abstract class for a font that can be used to render text. }
  TCastleAbstractFont = class abstract(TCastleComponent)
  strict private
  type
    TSavedProperties = class
      Size: Single;
      Outline: Cardinal;
      OutlineColor: TCastleColor;
      OutlineHighQuality: boolean;
      TargetImage: TCastleImage;
    end;
    TSavedPropertiesList = {$ifdef FPC}specialize{$endif} TObjectList<TSavedProperties>;
  var
    FMeasuredSize, FMeasuredHeight, FMeasuredCapHeight, FMeasuredDescenderHeight: Single;
    FMeasureHeight, FMeasureCapHeight, FMeasureDescenderHeight: String;
    FOutline: Cardinal;
    FOutlineColor: TCastleColor;
    FOutlineHighQuality: boolean;
    FTargetImage: TCastleImage;
    FPropertiesStack: TSavedPropertiesList;
    FSize: Single;
    FFontSizeNotifications: TNotifyEventList;
    procedure MakeMeasure;
    procedure GLContextCloseEvent(Sender: TObject);
  {$ifdef FPC}strict{$endif} protected
    { Calculate properties based on measuring the current font
      (with current @link(Size)).
      The default implementation in TCastleAbstractFont looks at TextHeight of sample texts
      to determine the parameter values. }
    procedure Measure(out AHeight, ACapHeight, ADescenderHeight: Single); virtual;
    procedure GLContextClose; virtual;
    procedure SetSize(const Value: Single); virtual;
    { Call this when the measurement of font possibly changed, and anything cached
      should be recalculated by calling TextWidth, TextHeight etc. again. }
    procedure FontSizesChanged;
    procedure SetOutline(const Value: Cardinal);
    procedure SetMeasureHeight(const Value: String);
    procedure SetMeasureCapHeight(const Value: String);
    procedure SetMeasureDescenderHeight(const Value: String);
    function StoreMeasureHeight: Boolean;
    function StoreMeasureCapHeight: Boolean;
    function StoreMeasureDescenderHeight: Boolean;
  public
    const
      DefaultMeasureHeight = 'AOHIypq';
      DefaultMeasureCapHeight = 'HI';
      DefaultMeasureDescenderHeight = 'ya';

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { If the font is loaded, it can actually display and measure some characters. }
    function FontLoaded: Boolean; virtual; abstract;

    { Desired font size to use when rendering (@link(Print)) and measuring
      (@link(TextWidth), @link(TextHeight) and related).
      Should be always > 0.

      The font size should correspond to the font height, which you can query using @link(Height).
      So in general @link(Size) and @link(Height) should be equal.
      That said, it is up to each specific font file to have correct size that satisfies above.
      This property, @name, gets and sets the size, as understood by FreeType font size.
      On the other hand, the @link(CapHeight) and @link(Height) properties
      actually query the letters height. }
    property Size: Single read FSize write SetSize;

    { Draw text at the current WindowPos, and move
      the WindowPos at the end. This way you can immediately
      call another PrintAndMove again, to add something at the end.

      It is not adviced to use it, as using the global WindowPos leads
      sooner or later to messy in code, that has to deal with global state.
      If you need to know how to move after printing text, use TextMove.

      May require 1 free slot on the attributes stack.
      May only be called when current matrix is modelview.
      Doesn't modify any OpenGL state or matrix, except it moves raster position. }
    procedure PrintAndMove(const s: string); deprecated 'use Print(X, Y, ...), and move the (X, Y) yourself based on TextMove, instead of this';

    { Draw text at the given position with given color.
      If the last Color component is not 1, the text is rendered
      with blending.

      Overloaded version without X, Y uses WindowPos (but doesn't modify
      it, in contrast to PrintAndMove).
      Overloaded version without Color uses CurrentColor,
      last color set by glColorv.
      It is not adviced to use overloaded versions without X, Y or Color
      --- using global state leads to messy code.
      You should upgrade your code to use the version that gets X,Y,Color
      explicitly.

      May require 1 free slot on the attributes stack.
      May only be called when current matrix is modelview.
      Doesn't modify any OpenGL state or matrix, except it moves raster position. }
    procedure Print(const X, Y: Single; const Color: TCastleColor;
      const S: string); overload; virtual; abstract;
    procedure Print(const Pos: TVector2Integer; const Color: TCastleColor;
      const S: string); overload;
    procedure Print(const Pos: TVector2; const Color: TCastleColor;
      const S: string); overload;

    procedure Print(const X, Y: Single; const S: string); overload; deprecated 'instead of this, use Print overload that takes explicit X,Y,Color parameters';
    procedure Print(const s: string); overload; deprecated 'instead of this, use Print overload that takes explicit X,Y,Color parameters';

    { Print text, aligning within given rectangle.

      Hint: Use TRectangle.Grow(-10) or similar to align within a rectangle
      with padding. }
    procedure PrintRect(const Rect: TRectangle; const Color: TCastleColor;
      const S: string;
      const HorizontalAlignment: THorizontalPosition;
      const VerticalAlignment: TVerticalPosition); overload;
    procedure PrintRect(const Rect: TFloatRectangle; const Color: TCastleColor;
      const S: string;
      const HorizontalAlignment: THorizontalPosition;
      const VerticalAlignment: TVerticalPosition); overload;

    { Print text, aligning within given rectangle.
      Newlines within the text will be automatically honored,
      the text will be rendered as multiple lines.
      See @link(PrintStrings) for description of parameters Html,
      LineSpacing, TextHorizontalAlignment. }
    procedure PrintRectMultiline(const Rect: TRectangle; const Color: TCastleColor;
      const S: string;
      const HorizontalAlignment: THorizontalPosition;
      const VerticalAlignment: TVerticalPosition;
      const Html: boolean;
      const LineSpacing: Integer;
      const TextHorizontalAlignment: THorizontalPosition = hpLeft); overload;
    procedure PrintRectMultiline(const Rect: TFloatRectangle; const Color: TCastleColor;
      const S: string;
      const HorizontalAlignment: THorizontalPosition;
      const VerticalAlignment: TVerticalPosition;
      const Html: boolean;
      const LineSpacing: Integer;
      const TextHorizontalAlignment: THorizontalPosition = hpLeft); overload;

    { The font may require some OpenGL resources for drawing.
      You can explicitly create them using PrepareResources (although it is never
      needed, resources will be automatically created if needed).
      There's no public method to explicitly destroy them,
      they are always destroyed automatically.
      @groupBegin }
    procedure PrepareResources; virtual;
    { @groupEnd }

    function TextWidth(const S: string): Single; virtual; abstract;
    function TextHeight(const S: string): Single; virtual; abstract;
    { The height (above the baseline) of the text.
      This doesn't take into account height of the text below the baseline
      (for example letter "y" has the tail below the baseline in most fonts). }
    function TextHeightBase(const S: string): Single; virtual; abstract;
    function TextMove(const S: string): TVector2; virtual; abstract;
    function TextSize(const S: string): TVector2;

    function RowHeight: Single; deprecated 'use Height';
    function RowHeightBase: Single; deprecated 'use CapHeight';
    function Descend: Single; deprecated 'use DescenderHeight';

    { Maximum height of font characters, taking into account whole descent and ascent.

      By default calculated as height of "AOHIypq", see the @link(MeasureHeight).

      See about standard font measurements:
      https://en.wikipedia.org/wiki/Typeface#Font_metrics ,
      https://en.wikipedia.org/wiki/Metric_typographic_units . }
    function Height: Single;

    { Height above the baseline of font characters.

      Compared to @link(Height), this does not take into account the characters
      size below the baseline, and it doesn't take into account some possible "overflow"
      on the top of some characters.

      By default calculated as height of "HI", see the MeasureCapHeight.

      The intention is that, while this doesn't represent the whole possible font height,
      but it often represents the perceived by user height of the block of text,
      as fonts are designed to make descents / overflows not stand out.

      See about standard font measurements:
      https://en.wikipedia.org/wiki/Typeface#Font_metrics ,
      https://en.wikipedia.org/wiki/Metric_typographic_units .
      About "cap height" in particular:
      https://en.wikipedia.org/wiki/Cap_height
      https://fonts.google.com/knowledge/glossary/cap_height }
    function CapHeight: Single;

    { How low the font characters may go below the baseline.
      By default calculated as a difference between "y" and "a" characters height,
      see the MeasureDescenderHeight.

      See about standard font measurements:
      https://en.wikipedia.org/wiki/Typeface#Font_metrics ,
      https://en.wikipedia.org/wiki/Metric_typographic_units . }
    function DescenderHeight: Single;

    { Break lines (possibly break one long string into more strings)
      to fit the text with given MaxLineWidth.

      This takes into account current font information (works also
      for non-monospace fonts, of course), and converts your Unbroken
      text into Broken text, such that TextWidth of the longest Broken
      line fits within MaxLineWidth.

      Tries to break on white characters. If not possible (there's
      a long stream of non-white characters that really has to be broken),
      it will break in the middle of normal (non-white) characters.
      The only situation when we have to fail, and the resulting
      Broken text is wider than required MaxLineWidth, is when
      @italic(a single character in your font) is wider than MaxLineWidth.
      In such case, there's really no solution, and we'll just let such
      character stay.

      If you use the overloaded version where Unbroken is just a string,
      then note that already existing newlines (NL) inside Unbroken
      will be correctly preserved.

      If you use the overloaded version with separate Unbroken and
      Broken parameters, then the previous Broken contents are not modified.
      We only append to Broken new strings, coming from Unbroken text.
      The overloaded version that takes only Broken parameter
      (no Unbroken parameter) simply modifies it's Broken parameter
      (from the line FirstToBreak).

      @groupBegin }
    procedure BreakLines(const unbroken: string; broken: TStrings; MaxLineWidth: Single); overload;
    procedure BreakLines(unbroken, broken: TStrings; MaxLineWidth: Single); overload;
    procedure BreakLines(broken: TStrings; MaxLineWidth: Single; FirstToBreak: integer); overload;
    { @groupEnd }

    { Largest width of the line of text in given list.

      @param(Html Indicates that strings inside SList use a subset of HTML,
        the same ones as interpreted by PrintStrings.
        If your SList uses these elements (for example, you plan to call later
        PrintStrings with the same SList and Html = @true) then make
        sure you pass Html = @true to this method.
        Otherwise, MaxTextWidth will treat HTML markup (like @code(<font ...>))
        like a normal text, usually making the width incorrectly large.)
    }
    function MaxTextWidth(SList: TStrings; const Html: boolean = false): Single;

    { Print all strings from the list.

      @param(X0 The X position of the whole text block.
        It's exact interpretation depends on TextHorizontalAlignment value.)

      @param(Y0 The bottom position of the whole text block.
        That is, this is the bottom position of the last string.)

      @param(Color The color of the text. Alpha value of the color is honored,
        value < 1 renders partially-transparent text.

        Overloaded and deprecated versions without
        explicit Color parameter use CurrentColor.)

      @param(Strs The text to display. Can be given as either TStringList,
        or a simple array of strings.)

      @param(Html Enable a subset of HTML to mark font changes inside the text.
        See the example examples/fonts/html_text_demo.html for a demo, supported
        HTML constructs now are:
        @unorderedList(
          @item <b> (bold)
          @item <i> (italic)
          @item <font color="#rrggbb">, <font color="#rrggbbaa"> (change color, with or without alpha)
          @item <font size="xxx">, <small> (change size)
          @item <br> <br/> <br /> <p> (newlines; paragraph makes 2 newlines)
          @item &amp; &lt; &gt; &apos; &quot; (entities)
          @item <!-- xxx --> (comments)
        )
      )

      @param(LineSpacing Extra space between lines.
        Distance between each line is determined by Height + LineSpacing
        pixels.

        Note that LineSpacing can be < 0 (as well as > 0),
        this may be sometimes useful if you really want to squeeze
        more text into the available space. Still, make sure that
        (Height + LineSpacing) is > 0.)

      @groupBegin }
    procedure PrintStrings(const X0, Y0: Single; const Color: TCastleColor;
      const Strs: TStrings; const Html: boolean;
      const LineSpacing: Single;
      const TextHorizontalAlignment: THorizontalPosition = hpLeft); overload;
    procedure PrintStrings(const X0, Y0: Single; const Color: TCastleColor;
      const Strs: array of string; const Html: boolean;
      const LineSpacing: Single;
      const TextHorizontalAlignment: THorizontalPosition = hpLeft); overload;
    procedure PrintStrings(const Strs: TStrings;
      const Html: boolean; const LineSpacing: Single;
      const X0: Single = 0; const Y0: Single = 0); overload; deprecated 'instead of this, use PrintStrings version that takes explicit Color parameter';
    procedure PrintStrings(const Strs: array of string;
      const Html: boolean; const LineSpacing: Single;
      const X0: Single = 0; const Y0: Single = 0); overload; deprecated 'instead of this, use PrintStrings version that takes explicit Color parameter';
    { @groupEnd }

    { Print the string, broken such that it fits within MaxLineWidth.
      The string is broken into many lines using BreakLines,
      so the original newlines insides are correctly used,
      and the length of lines fits inside MaxLineWidth.

      The strings are printed on the screen, just like by PrintStrings.
      If PositionsFirst then the X0, Y0 determine
      the position of the first (top) line, otherwise they determine
      the position of the last (bottom) line.

      LineSpacing has the same meaning as for PrintStrings:
      it adds an additional space between lines (if positive) or forces
      the lines to be more tightly squeezed (if negative). Always make
      sure that (Height + LineSpacing) > 0.

      Returns the number of lines printed, that is the number of lines
      after breaking the text into lines. This may be useful e.g. to calculate
      the height of the printed text.

      May require 1 free slot on the attributes stack.
      May only be called when current matrix is modelview.
      Doesn't modify any OpenGL state or matrix.

      Overloaded and deprecated version without
      explicit Color parameter uses CurrentColor.

      Overloaded version that takes rectangle as a parameter can
      align the resulting string box within the rectangle.

      @groupBegin }
    function PrintBrokenString(const Rect: TRectangle; const Color: TCastleColor;
      const S: string;
      const LineSpacing: Single;
      const AlignHorizontal: THorizontalPosition;
      const AlignVertical: TVerticalPosition;
      const Html: boolean = false): Integer; overload;
    function PrintBrokenString(const Rect: TFloatRectangle; const Color: TCastleColor;
      const S: string;
      const LineSpacing: Single;
      const AlignHorizontal: THorizontalPosition;
      const AlignVertical: TVerticalPosition;
      const Html: boolean = false): Integer; overload;
    function PrintBrokenString(X0, Y0: Single; const Color: TCastleColor;
      const S: string; const MaxLineWidth: Single;
      const PositionsFirst: boolean;
      const LineSpacing: Single;
      const Html: boolean = false): Integer; overload;
    function PrintBrokenString(const S: string;
      const MaxLineWidth, X0, Y0: Single;
      const PositionsFirst: boolean;
      const LineSpacing: Single): Integer; overload; deprecated 'instead of this, use PrintBrokenString that takes explicit Color parameter';
    { @groupEnd }

    { Outline size around the normal text.
      Note that the current implementation is very simple, it will only
      look sensible for small outline values (like 1 or 2).

      Note that outline size, in pixels, is not scaled along with font size.
      Which makes sense: whether you load TTF with size 10 and then set size 20,
      or you load TTF with size 40 and then set size 20
      --- the font is internally scaled differently, but the resulting sizes
      and outline sizes remain the same.

      @seealso OutlineHighQuality }
    property Outline: Cardinal read FOutline write SetOutline default 0;

    { Optionally force better outline quality. Used only if Outline <> 0.
      High quality outline looks better, but is about 2x more expensive to draw.
      @seealso Outline }
    property OutlineHighQuality: boolean
      read FOutlineHighQuality write FOutlineHighQuality default false;

    { Outline color, used only if Outline <> 0. Default is black.
      @seealso Outline }
    property OutlineColor: TCastleColor read FOutlineColor write FOutlineColor;

    { Save draw properties to a stack. Saves:
      @link(Size), @link(Outline), @link(OutlineColor), @link(OutlineHighQuality),
      @link(TargetImage). }
    procedure PushProperties;
    procedure PopProperties;

    { Non-zero font size. Usually same thing as @link(Size), but in case of proxy
      font classes (like TCustomizedFont and TCastleFontFamily) it makes sure to
      never return zero (which, in case of font proxies,
      is allowed value for @link(Size) and means "use underlying font size"). }
    function EffectiveSize: Single; virtual;

    function RealSize: Single; deprecated 'use EffectiveSize';

    { The image where we render the font.
      Usually (when this is @nil) our rendering routines render to the screen
      (or the current FBO, if you use @link(TGLRenderToTexture)).
      By setting this to non-nil, you make the rendering by done on CPU
      (without libraries like OpenGL/OpenGLES), and the text is drawn on the given image.

      The PushProperties and PopProperties methods save/restore this.

      TODO: Font scaling (normally done by TCastleFont and TCastleBitmapFont
      if you change @link(Size) from default)
      is not done when drawing font to an image.
      So don't touch the @link(Size)
      if you plan on rendering to image. Also, when using HTML tags,
      do not change the font size by them.
      Otherwise TextWidth / TextHeight will be unsynchronized
      with what @link(Print) actually does --- so not only your font will
      remain constant size, also it will overlap with itself.
      This will get fixed one day --- TCastleImage.Draw just needs to support scaling.
    }
    property TargetImage: TCastleImage read FTargetImage write FTargetImage;

    { Add notification when FontSizesChanged occurs. }
    procedure AddFontSizeChangeNotification(const Notify: TNotifyEvent);
    { Remove notification when FontSizesChanged occurs. }
    procedure RemoveFontSizeChangeNotification(const Notify: TNotifyEvent);
  published
    { Use these characters to measure font @link(Height). }
    property MeasureHeight: String read FMeasureHeight write SetMeasureHeight
      stored StoreMeasureHeight;

    { Use these characters to measure font @link(CapHeight).
      Note that we only measure the height above baseline (Y = 0),
      so even when MeasureHeight and MeasureCapHeight are equal,
      the resulting @link(Height) and @link(CapHeight) may differ. }
    property MeasureCapHeight: String read FMeasureCapHeight write SetMeasureCapHeight
      stored StoreMeasureCapHeight;

    { Use these characters to measure font @link(DescenderHeight).

      This string must be empty (meaning that descend is zero)
      or has exactly 2 characters.
      The descender height is measured as height of 1st char - height of 2nd char.

      See DefaultMeasureDescenderHeight . }
    property MeasureDescenderHeight: String read FMeasureDescenderHeight write SetMeasureDescenderHeight
      stored StoreMeasureDescenderHeight;
  end;

  TGLBitmapFontAbstract = TCastleAbstractFont deprecated 'use TCastleAbstractFont';

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleAbstractFont ------------------------------------------------------}

constructor TCastleAbstractFont.Create(AOwner: TComponent);
begin
  inherited;
  FMeasuredSize := -1; // not measured at all
  FOutlineColor := Black;
  FMeasureHeight := DefaultMeasureHeight;
  FMeasureCapHeight := DefaultMeasureCapHeight;
  FMeasureDescenderHeight := DefaultMeasureDescenderHeight;
  ApplicationProperties.OnGLContextCloseObject.Add({$ifdef FPC}@{$endif}GLContextCloseEvent);
end;

destructor TCastleAbstractFont.Destroy;
begin
  if ApplicationProperties <> nil then
    ApplicationProperties.OnGLContextCloseObject.Remove({$ifdef FPC}@{$endif}GLContextCloseEvent);
  GLContextClose;
  FreeAndNil(FPropertiesStack);
  FreeAndNil(FFontSizeNotifications);
  inherited;
end;

procedure TCastleAbstractFont.SetSize(const Value: Single);
begin
  if FSize <> Value then
  begin
    Assert(not IsInfinite(Value));
    FSize := Value;
  end;
end;

procedure TCastleAbstractFont.GLContextCloseEvent(Sender: TObject);
begin
  GLContextClose;
end;

procedure TCastleAbstractFont.PrepareResources;
begin
end;

procedure TCastleAbstractFont.GLContextClose;
begin
end;

function TCastleAbstractFont.TextSize(const S: string): TVector2;
begin
  Result := Vector2(TextWidth(S), TextHeight(S));
end;

procedure TCastleAbstractFont.Print(const Pos: TVector2Integer;
  const Color: TCastleColor; const S: string);
begin
  Print(Pos[0], Pos[1], Color, S);
end;

procedure TCastleAbstractFont.Print(const Pos: TVector2;
  const Color: TCastleColor; const S: string);
begin
  Print(Pos[0], Pos[1], Color, S);
end;

procedure TCastleAbstractFont.Print(const s: string);
begin
  { Deprecated method uses other deprecated method here, don't warn }
  {$warnings off}
  Print(WindowPos[0], WindowPos[1], CurrentColor, S);
  {$warnings on}
end;

procedure TCastleAbstractFont.PrintAndMove(const s: string);
var
  M: TVector2;
begin
  { Deprecated method uses other deprecated method here, don't warn }
  {$warnings off}
  Print(S);
  M := TextMove(S);
  WindowPos := WindowPos + Vector2Integer(Round(M.X), Round(M.Y));
  {$warnings on}
end;

procedure TCastleAbstractFont.Print(const X, Y: Single; const S: string);
begin
  { Deprecated method uses other deprecated method here, don't warn }
  {$warnings off}
  Print(X, Y, CurrentColor, S);
  {$warnings on}
end;

procedure TCastleAbstractFont.PrintRectMultiline(const Rect: TFloatRectangle; const Color: TCastleColor;
  const S: string;
  const HorizontalAlignment: THorizontalPosition;
  const VerticalAlignment: TVerticalPosition;
  const Html: boolean;
  const LineSpacing: Integer;
  const TextHorizontalAlignment: THorizontalPosition);
var
  Strings: TStringList;
  ThisRect: TFloatRectangle;
  X: Single;
begin
  Strings := TStringList.Create;
  try
    Strings.Text := S;
    if Strings.Count <> 0 then
    begin
      ThisRect := FloatRectangle(0, 0, MaxTextWidth(Strings, Html),
        Strings.Count * (LineSpacing + Height) - LineSpacing);
      ThisRect := ThisRect.
        Align(HorizontalAlignment, Rect, HorizontalAlignment).
        Align(VerticalAlignment, Rect, VerticalAlignment);
      case TextHorizontalAlignment of
        hpLeft   : X := ThisRect.Left;
        hpMiddle : X := ThisRect.Center[0];
        hpRight  : X := ThisRect.Right;
        {$ifndef COMPILER_CASE_ANALYSIS}
        else raise EInternalError.Create('TextHorizontalAlignment? in TCastleAbstractFont.PrintRectMultiline');
        {$endif}
      end;
      PrintStrings(X, ThisRect.Bottom, Color, Strings,
        Html, LineSpacing, TextHorizontalAlignment);
    end;
  finally FreeAndNil(Strings) end;
end;

procedure TCastleAbstractFont.PrintRectMultiline(const Rect: TRectangle; const Color: TCastleColor;
  const S: string;
  const HorizontalAlignment: THorizontalPosition;
  const VerticalAlignment: TVerticalPosition;
  const Html: boolean;
  const LineSpacing: Integer;
  const TextHorizontalAlignment: THorizontalPosition);
begin
  PrintRectMultiline(FloatRectangle(Rect), Color, S,
    HorizontalAlignment, VerticalAlignment, Html, LineSpacing, TextHorizontalAlignment);
end;

procedure TCastleAbstractFont.PrintRect(
  const Rect: TFloatRectangle; const Color: TCastleColor;
  const S: string;
  const HorizontalAlignment: THorizontalPosition;
  const VerticalAlignment: TVerticalPosition);
var
  ThisRect: TFloatRectangle;
begin
  ThisRect :=
    FloatRectangle(0, 0, TextWidth(S), TextHeight(S)).
    Align(HorizontalAlignment, Rect, HorizontalAlignment).
    Align(VerticalAlignment, Rect, VerticalAlignment);
  Print(ThisRect.Left, ThisRect.Bottom, Color, S);
end;

procedure TCastleAbstractFont.PrintRect(
  const Rect: TRectangle; const Color: TCastleColor;
  const S: string;
  const HorizontalAlignment: THorizontalPosition;
  const VerticalAlignment: TVerticalPosition);
begin
  PrintRect(FloatRectangle(Rect), Color, S, HorizontalAlignment, VerticalAlignment);
end;

procedure TCastleAbstractFont.BreakLines(const unbroken: string;
  broken: TStrings; MaxLineWidth: Single);
var
  unbrokenlist: TStringList;
begin
  unbrokenlist := TStringList.Create;
  try
    Strings_SetText(unbrokenlist, unbroken);
    BreakLines(unbrokenlist, broken, MaxLineWidth);
  finally unbrokenlist.Free end;
end;

procedure TCastleAbstractFont.BreakLines(unbroken, broken: TStrings;
  MaxLineWidth: Single);
var
  i, FirstToBreak: Integer;
begin
  FirstToBreak := broken.count;
  for I := 0 to unbroken.count-1 do broken.Append(unbroken[i]);
  BreakLines(broken, MaxLineWidth, FirstToBreak);
end;

procedure TCastleAbstractFont.BreakLines(broken: TStrings;
  MaxLineWidth: Single; FirstToBreak: integer);
var
  I: Integer;
  LineWidthBytes: Integer;
  LineWidth: Single;
  P: Integer;
  BreakInput, BreakOutput1, BreakOutput2, HardBreakOutput: string;
  C: TUnicodeChar;
  {$ifdef FPC}
  BreakInputPtr: PChar;
  CharLen: Integer;
  {$else}
  TextIndex: Integer;
  NextTextIndex: Integer;
  TextLength: Integer;
  {$endif}
begin
  { break the strings on Broken list.
    We look at MaxLineWidth in pixels, taking into account that font
    does not have to be mono-spaced.  }

  I := FirstToBreak;
  { instead of "for" use "while" because Broken.Count will be changing }
  while I < Broken.Count do
  begin
    BreakInput := Broken[I];

    { Try breaking line number I. If it's empty, there's no need to break it. }
    {$ifdef FPC}
    BreakInputPtr := PChar(BreakInput);

    C := UTF8CharacterToUnicode(BreakInputPtr, CharLen);
    if (C > 0) and (CharLen > 0) then
    {$else}
    TextIndex := 1;
    TextLength := Length(BreakInput);
    if TextLength > 0 then
    {$endif}
    begin
      { the first character C is always considered to fit into BreakOutput1,
        regardless of MaxLineWidth --- after all, we have to place this character
        somewhere (otherwise we would have to reject this character
        or raise an error). }
      {$ifdef FPC}
      Inc(BreakInputPtr, CharLen);
      LineWidthBytes := CharLen;
      {$else}
      C := GetUTF32Char(BreakInput, TextIndex, NextTextIndex);
      TextIndex := NextTextIndex;
      LineWidthBytes := TextIndex {TODO: maybe *2 to gets bytes};
      {$endif}
      LineWidth := TextWidth({$ifdef FPC}UnicodeToUTF8{$else}ConvertFromUtf32{$endif}(C));

      {$ifdef FPC}
      C := UTF8CharacterToUnicode(BreakInputPtr, CharLen);
      while (C > 0) and (CharLen > 0) and
            (LineWidth + TextWidth(UnicodeToUTF8(C)) <= MaxLineWidth) do
      {$else}
      while (TextIndex <= TextLength) and
            (LineWidth + TextWidth(ConvertFromUtf32(C)) <= MaxLineWidth) do
      {$endif}
      begin
        {$ifdef FPC}
        Inc(BreakInputPtr, CharLen);
        LineWidthBytes += CharLen;
        {$else}
        C := GetUTF32Char(BreakInput, TextIndex, NextTextIndex);
        Inc(LineWidthBytes, NextTextIndex - TextIndex); {TODO: maybe *2 to gets bytes};
        TextIndex := NextTextIndex;
        {$endif}
        LineWidth := LineWidth + TextWidth({$ifdef FPC}UnicodeToUTF8{$else}ConvertFromUtf32{$endif}(C));

        {$ifdef FPC}
        C := UTF8CharacterToUnicode(BreakInputPtr, CharLen);
        {$endif}
      end;

      {$ifdef FPC}
      if (C > 0) and (CharLen > 0) then // then above loop stopped because we have to break
      {$else}
      if TextIndex <= TextLength then // TODO: check that carefully
      {$endif}
      begin
        { HardBreakOutput is BreakOutput1 in case we don't find a whitespace
          on which to break }
        HardBreakOutput := Copy(BreakInput, 1, LineWidthBytes);
        { We have to break this line now. }
        P := BackCharsPos(WhiteSpaces, HardBreakOutput);
        if P > 0 then
        begin
          BreakOutput1 := Copy(BreakInput, 1, P - 1);
          BreakOutput2 := SEnding(BreakInput, P + 1) { break at pos p, delete p-th char }
        end else
        begin
          BreakOutput1 := HardBreakOutput;
          BreakOutput2 := SEnding(BreakInput, Length(HardBreakOutput) + 1);
        end;
        Broken[I] := BreakOutput1;
        Broken.Insert(I + 1, BreakOutput2); // next iteration will break BreakOutput2
      end;
    end;

    Inc(I);
  end;
end;

function TCastleAbstractFont.MaxTextWidth(SList: TStrings; const Html: boolean): Single;
var
  I: Integer;
  Text: TRichText;
begin
  if not Html then
  begin
    { simple and fast implementation in case TRichText not needed }
    Result := 0;
    for I := 0 to SList.Count-1 do
      MaxVar(Result, TextWidth(SList[I]));
  end else
  begin
    Text := TRichText.Create(Self, SList, Html);
    try
      Result := Text.Width;
    finally FreeAndNil(Text) end;
  end;
end;

procedure TCastleAbstractFont.PrintStrings(const X0, Y0: Single;
  const Color: TCastleColor; const Strs: TStrings;
  const Html: boolean; const LineSpacing: Single;
  const TextHorizontalAlignment: THorizontalPosition);

  function XPos(const Line: Integer; const S: string): Single;
  begin
    case TextHorizontalAlignment of
      hpLeft  : Result := X0;
      hpMiddle: Result := X0 - TextWidth(S) / 2;
      hpRight : Result := X0 - TextWidth(S);
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('TCastleAbstractFont.PrintStrings: TextHorizontalAlignment unknown');
      {$endif}
    end;
  end;

  function YPos(const Line: Integer): Single;
  begin
    Result := (Strs.Count - 1 - Line) * (Height + LineSpacing) + Y0;
  end;

var
  Line: Integer;
  Text: TRichText;
  S: String;
  Y: Single;
  HasScissorRect: Boolean;
  ScissorRect: TRectangle;
begin
  if not Html then
  begin
    { simple and fast implementation in case TRichText not needed }

    { Check RenderContext <> nil, as this may be used for TCastleImage
      drawing, testcase: font_draw_over_image.
      TODO: better account for TCastleImage drawing,
      it may be possible even when RenderContext <> nil,
      and we should ignore scissors then. }
    HasScissorRect := (RenderContext <> nil) and
      RenderContext.FinalScissor(ScissorRect) and
      (not ScissorRect.IsEmpty);

    for Line := 0 to Strs.Count - 1 do
    begin
      Y := YPos(Line);
      { Reject entire line, if it would be clipped anyway. }
      if HasScissorRect and
         ( (Y > ScissorRect.Top) or
           (Y + Height < ScissorRect.Bottom)
         ) then
        Continue;
      S := Strs[Line];
      Print(XPos(Line, S), Y, Color, S);
    end;
  end else
  begin
    Text := TRichText.Create(Self, Strs, Html);
    try
      Text.Print(X0, Y0, Color, LineSpacing, TextHorizontalAlignment);
    finally FreeAndNil(Text) end;
  end;
end;

procedure TCastleAbstractFont.PrintStrings(const X0, Y0: Single;
  const Color: TCastleColor; const Strs: array of string;
  const Html: boolean; const LineSpacing: Single;
  const TextHorizontalAlignment: THorizontalPosition);
var
  SList: TStringList;
begin
  SList := TStringList.Create;
  try
    AddStrArrayToStrings(Strs, SList);
    PrintStrings(X0, Y0, Color, SList, Html, LineSpacing, TextHorizontalAlignment);
  finally FreeAndNil(SList) end;
end;

procedure TCastleAbstractFont.PrintStrings(const Strs: TStrings;
  const Html: boolean; const LineSpacing: Single;
  const X0: Single; const Y0: Single);
begin
  { Deprecated stuff uses other deprecated stuff here, don't warn }
  {$warnings off}
  PrintStrings(X0, Y0, CurrentColor, Strs, Html, LineSpacing);
  {$warnings on}
end;

procedure TCastleAbstractFont.PrintStrings(const Strs: array of string;
  const Html: boolean; const LineSpacing: Single; const X0: Single;
  const Y0: Single);
var
  SList: TStringList;
begin
  { Deprecated stuff uses other deprecated stuff here, don't warn }
  {$warnings off}
  SList := TStringList.Create;
  try
    AddStrArrayToStrings(Strs, SList);
    PrintStrings(X0, Y0, CurrentColor, SList, Html, LineSpacing);
  finally SList.Free end;
  {$warnings on}
end;

function TCastleAbstractFont.PrintBrokenString(X0, Y0: Single;
  const Color: TCastleColor; const S: string; const MaxLineWidth: Single;
  const PositionsFirst: boolean; const LineSpacing: Single;
  const Html: boolean): Integer;
var
  Text: TRichText;
begin
  Text := TRichText.Create(Self, S, Html);
  try
    Text.Wrap(MaxLineWidth);
    if PositionsFirst then
      Y0 := Y0 - (Text.Count-1) * (Height + LineSpacing);
    Text.Print(X0, Y0, Color, LineSpacing);
    Result := Text.Count;
  finally FreeAndNil(Text) end;
end;

function TCastleAbstractFont.PrintBrokenString(const Rect: TFloatRectangle;
  const Color: TCastleColor; const S: string;
  const LineSpacing: Single;
  const AlignHorizontal: THorizontalPosition;
  const AlignVertical: TVerticalPosition;
  const Html: boolean): Integer;
var
  Text: TRichText;
  X0, Y0, BrokenHeight: Single;
begin
  Text := TRichText.Create(Self, S, Html);
  try
    Text.Wrap(Rect.Width);
    { calculate X0 based on Rect and Text.Width }
    case AlignHorizontal of
      hpLeft  : X0 := Rect.Left;
      hpMiddle: X0 := Rect.Left + (Rect.Width - Text.Width) / 2;
      hpRight : X0 := Rect.Right - Text.Width;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('PrintBrokenString.AlignHorizontal?');
      {$endif}
    end;
    { calculate Y0 based on Rect and BrokenHeight }
    BrokenHeight := Text.Count * (LineSpacing + Height);
    case AlignVertical of
      vpBottom: Y0 := Rect.Bottom;
      vpMiddle: Y0 := Rect.Bottom + (Rect.Height - BrokenHeight) / 2;
      vpTop   : Y0 := Rect.Top - BrokenHeight;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('PrintBrokenString.AlignVertical?');
      {$endif}
    end;
    Text.Print(X0, Y0, Color, LineSpacing);
    Result := Text.Count;
  finally FreeAndNil(Text) end;
end;

function TCastleAbstractFont.PrintBrokenString(const Rect: TRectangle; const Color: TCastleColor;
  const S: string;
  const LineSpacing: Single;
  const AlignHorizontal: THorizontalPosition;
  const AlignVertical: TVerticalPosition;
  const Html: boolean = false): Integer;
begin
  Result := PrintBrokenString(FloatRectangle(Rect), Color, S, LineSpacing,
    AlignHorizontal, AlignVertical, Html);
end;

function TCastleAbstractFont.PrintBrokenString(const S: string;
  const MaxLineWidth, X0, Y0: Single;
  const PositionsFirst: boolean;
  const LineSpacing: Single): Integer; deprecated;
begin
  { Deprecated stuff uses other deprecated stuff here, don't warn }
  {$warnings off}
  Result := PrintBrokenString(X0, Y0, CurrentColor, S, MaxLineWidth,
    PositionsFirst, LineSpacing);
  {$warnings on}
end;

procedure TCastleAbstractFont.Measure(out AHeight, ACapHeight, ADescenderHeight: Single);
var
  MeasureDescenderHeightLen: Integer;
begin
  AHeight := TextHeight(MeasureHeight);
  ACapHeight := TextHeightBase(MeasureCapHeight);

  MeasureDescenderHeightLen := StringLength(MeasureDescenderHeight);
  if MeasureDescenderHeightLen = 2 then
  begin
    ADescenderHeight := Max(0,
      TextHeight(StringCopy(MeasureDescenderHeight, 1, 1)) -
      TextHeight(StringCopy(MeasureDescenderHeight, 2, 1))
    );
  end else
  begin
    ADescenderHeight := 0;
    if MeasureDescenderHeightLen <> 0 then
      WritelnWarning('MeasureDescenderHeight must have 0 or 2 Unicode characters, but has %d: "%s"', [
        MeasureDescenderHeightLen,
        MeasureDescenderHeight
      ]);
  end;
end;

procedure TCastleAbstractFont.MakeMeasure;
begin
  if FMeasuredSize <> Size then
  begin
    Measure(FMeasuredHeight, FMeasuredCapHeight, FMeasuredDescenderHeight);
    FMeasuredSize := Size;
  end;
end;

function TCastleAbstractFont.RowHeight: Single;
begin
  Result := Height;
end;

function TCastleAbstractFont.RowHeightBase: Single;
begin
  Result := CapHeight;
end;

function TCastleAbstractFont.Descend: Single;
begin
  Result := DescenderHeight;
end;

function TCastleAbstractFont.Height: Single;
begin
  MakeMeasure;
  Result := FMeasuredHeight;
end;

function TCastleAbstractFont.CapHeight: Single;
begin
  MakeMeasure;
  Result := FMeasuredCapHeight;
end;

function TCastleAbstractFont.DescenderHeight: Single;
begin
  MakeMeasure;
  Result := FMeasuredDescenderHeight;
end;

procedure TCastleAbstractFont.PushProperties;
var
  SavedProperites: TSavedProperties;
begin
  if FPropertiesStack = nil then
    FPropertiesStack := TSavedPropertiesList.Create;

  SavedProperites := TSavedProperties.Create;
  SavedProperites.Size := Size;
  SavedProperites.Outline := Outline;
  SavedProperites.OutlineColor := OutlineColor;
  SavedProperites.OutlineHighQuality := OutlineHighQuality;
  SavedProperites.TargetImage := TargetImage;
  FPropertiesStack.Add(SavedProperites);
end;

procedure TCastleAbstractFont.PopProperties;
var
  SavedProperites: TSavedProperties;
begin
  if (FPropertiesStack = nil) or (FPropertiesStack.Count = 0) then
    raise Exception.Create('Cannot do TCastleAbstractFont.PopProperties, stack empty. Every PopProperties should match previous PushProperties');

  SavedProperites := FPropertiesStack.Last;
  Size         := SavedProperites.Size;
  Outline      := SavedProperites.Outline;
  OutlineColor := SavedProperites.OutlineColor;
  OutlineHighQuality := SavedProperites.OutlineHighQuality;
  TargetImage  := SavedProperites.TargetImage;
  FPropertiesStack.Delete(FPropertiesStack.Count - 1);
end;

function TCastleAbstractFont.EffectiveSize: Single;
begin
  Result := Size;
end;

function TCastleAbstractFont.RealSize: Single;
begin
  Result := EffectiveSize;
end;

procedure TCastleAbstractFont.SetOutline(const Value: Cardinal);
begin
  if FOutline <> Value then
  begin
    FOutline := Value;
    // invalidate measurements, like Height, as they depend on Outline
    FMeasuredSize := -1;
  end;
end;

procedure TCastleAbstractFont.FontSizesChanged;
begin
  FMeasuredSize := -1; // not measured at all
  if FFontSizeNotifications <> nil then
    FFontSizeNotifications.ExecuteAll(Self);
end;

procedure TCastleAbstractFont.AddFontSizeChangeNotification(const Notify: TNotifyEvent);
begin
  if FFontSizeNotifications = nil then
    FFontSizeNotifications := TNotifyEventList.Create;
  FFontSizeNotifications.Add(Notify);
end;

procedure TCastleAbstractFont.RemoveFontSizeChangeNotification(const Notify: TNotifyEvent);
begin
  if FFontSizeNotifications <> nil then // may be nil when destroying, and we make notification
    FFontSizeNotifications.Remove(Notify);
end;

procedure TCastleAbstractFont.SetMeasureHeight(const Value: String);
begin
  if FMeasureHeight <> Value then
  begin
    FMeasureHeight := Value;
    FMeasuredSize := -1; // invalidate measurements
  end;
end;

procedure TCastleAbstractFont.SetMeasureCapHeight(const Value: String);
begin
  if FMeasureCapHeight <> Value then
  begin
    FMeasureCapHeight := Value;
    FMeasuredSize := -1; // invalidate measurements
  end;
end;

procedure TCastleAbstractFont.SetMeasureDescenderHeight(const Value: String);
begin
  if FMeasureDescenderHeight <> Value then
  begin
    FMeasureDescenderHeight := Value;
    FMeasuredSize := -1; // invalidate measurements
  end;
end;

function TCastleAbstractFont.StoreMeasureHeight: Boolean;
begin
  Result := FMeasureHeight <> DefaultMeasureHeight;
end;

function TCastleAbstractFont.StoreMeasureCapHeight: Boolean;
begin
  Result := FMeasureCapHeight <> DefaultMeasureCapHeight;
end;

function TCastleAbstractFont.StoreMeasureDescenderHeight: Boolean;
begin
  Result := FMeasureDescenderHeight <> DefaultMeasureDescenderHeight;
end;

{$endif read_implementation}
