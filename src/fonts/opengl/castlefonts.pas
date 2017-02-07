{
  Copyright 2001-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ 2D fonts (TCastleFont, TTextureFont, TSimpleTextureFont). }
unit CastleFonts;

{$I castleconf.inc}

interface

uses Classes, FGL, CastleGLImages, CastleStringUtils, CastleColors,
  CastleVectors, CastleTextureFontData, CastleImages, CastleUnicode,
  CastleRectangles, CastleApplicationProperties;

type
  { Abstract class for 2D font. }
  TCastleFont = class abstract(TComponent)
  strict private
  type
    TSavedProperties = class
      Scale: Single;
      Outline: Cardinal;
      OutlineColor: TCastleColor;
      OutlineHighQuality: boolean;
      TargetImage: TCastleImage;
    end;
    TSavedPropertiesList = specialize TFPGObjectList<TSavedProperties>;
  var
    MeasureDone: boolean;
    FRowHeight, FRowHeightBase, FDescend: Integer;
    FScale: Single;
    FOutline: Cardinal;
    FOutlineColor: TCastleColor;
    FOutlineHighQuality: boolean;
    FTargetImage: TCastleImage;
    FPropertiesStack: TSavedPropertiesList;
    procedure MakeMeasure;
    procedure GLContextCloseEvent(Sender: TObject);
  strict protected
    { Calculate properties based on measuring the font.
      The default implementation in TCastleFont looks at TextHeight of sample texts
      to determine the parameter values. }
    procedure Measure(out ARowHeight, ARowHeightBase, ADescend: Integer); virtual;
    { Call when data calculated by Measure changed,
      because TextWidth / TextHeight results changed (but not by Scale,
      that is taken care of automatically). }
    procedure InvalidateMeasure;
    procedure SetScale(const Value: Single); virtual;
    function GetSize: Single; virtual; abstract;
    procedure SetSize(const Value: Single); virtual; abstract;
    procedure GLContextClose; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

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
    procedure Print(const X, Y: Integer; const Color: TCastleColor;
      const S: string); overload; virtual; abstract;
    procedure Print(const Pos: TVector2Integer; const Color: TCastleColor;
      const S: string); overload;

    procedure Print(const X, Y: Integer; const S: string); overload; deprecated 'instead of this, use Print overload that takes explicit X,Y,Color parameters';
    procedure Print(const s: string); overload; deprecated 'instead of this, use Print overload that takes explicit X,Y,Color parameters';

    { Print text, aligning within given rectangle.

      Hint: Use TRectangle.Grow(-10) or similar to align within a rectangle
      with padding. }
    procedure PrintRect(const Rect: TRectangle; const Color: TCastleColor;
      const S: string;
      const HorizontalAlignment: THorizontalPosition;
      const VerticalAlignment: TVerticalPosition);

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
      const TextHorizontalAlignment: THorizontalPosition = hpLeft);

    { The font may require some OpenGL resources for drawing.
      You can explicitly create them using PrepareResources (although it is never
      needed, resources will be automatically created if needed).
      There's no public method to explicitly destroy them,
      they are always destroyed automatically.
      @groupBegin }
    procedure PrepareResources; virtual;
    { @groupEnd }

    function TextWidth(const S: string): Integer; virtual; abstract;
    function TextHeight(const S: string): Integer; virtual; abstract;
    { The height (above the baseline) of the text.
      This doesn't take into account height of the text below the baseline
      (for example letter "y" has the tail below the baseline in most fonts). }
    function TextHeightBase(const S: string): Integer; virtual; abstract;
    function TextMove(const S: string): TVector2Integer; virtual; abstract;
    function TextSize(const S: string): TVector2Integer;

    { Height of a row of text in this font.
      This may be calculated as simply @code(TextHeight('Wy')) for most
      normal fonts. }
    function RowHeight: Integer;

    { Height (above the baseline) of a row of text in this font.
      Similar to TextHeightBase and TextHeight,
      note that RowHeightBase is generally smaller than RowHeight,
      because RowHeightBase doesn't care how low the letter may go below
      the baseline. }
    function RowHeightBase: Integer;

    { How low the text may go below the baseline. }
    function Descend: Integer;

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
    procedure BreakLines(const unbroken: string; broken: TStrings; MaxLineWidth: integer); overload;
    procedure BreakLines(unbroken, broken: TStrings; MaxLineWidth: integer); overload;
    procedure BreakLines(broken: TStrings; MaxLineWidth: Integer; FirstToBreak: integer); overload;
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
    function MaxTextWidth(SList: TStrings; const Html: boolean = false): Integer;

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
        Distance between each line is determined by RowHeight + LineSpacing
        pixels.

        Note that LineSpacing can be < 0 (as well as > 0),
        this may be sometimes useful if you really want to squeeze
        more text into the available space. Still, make sure that
        (RowHeight + LineSpacing) is > 0.)

      @groupBegin }
    procedure PrintStrings(const X0, Y0: Integer; const Color: TCastleColor;
      const Strs: TStrings; const Html: boolean;
      const LineSpacing: Integer;
      const TextHorizontalAlignment: THorizontalPosition = hpLeft); overload;
    procedure PrintStrings(const X0, Y0: Integer; const Color: TCastleColor;
      const Strs: array of string; const Html: boolean;
      const LineSpacing: Integer;
      const TextHorizontalAlignment: THorizontalPosition = hpLeft); overload;
    procedure PrintStrings(const Strs: TStrings;
      const Html: boolean; const LineSpacing: Integer;
      const X0: Integer = 0; const Y0: Integer = 0); overload; deprecated 'instead of this, use PrintStrings version that takes explicit Color parameter';
    procedure PrintStrings(const Strs: array of string;
      const Html: boolean; const LineSpacing: Integer;
      const X0: Integer = 0; const Y0: Integer = 0); overload; deprecated 'instead of this, use PrintStrings version that takes explicit Color parameter';
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
      sure that (RowHeight + LineSpacing) > 0.

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
      const LineSpacing: Integer;
      const AlignHorizontal: THorizontalPosition;
      const AlignVertical: TVerticalPosition;
      const Html: boolean = false): Integer;
    function PrintBrokenString(X0, Y0: Integer; const Color: TCastleColor;
      const S: string; const MaxLineWidth: Integer;
      const PositionsFirst: boolean;
      const LineSpacing: Integer;
      const Html: boolean = false): Integer;
    function PrintBrokenString(const S: string;
      const MaxLineWidth, X0, Y0: Integer;
      const PositionsFirst: boolean;
      const LineSpacing: Integer): Integer; deprecated 'instead of this, use PrintBrokenString that takes explicit Color parameter';
    { @groupEnd }

    property Scale: Single read FScale write SetScale;

    { Adjust font size by scaling. Underneath, it scales the font using
      @link(Scale) property, to adjust it to requested size. }
    property Size: Single read GetSize write SetSize;

    { Outline size around the normal text.
      Note that the current implementation is very simple, it will only
      look sensible for small outline values (like 1 or 2).

      TOO: Note that outline size, in pixels, is @bold(right now) not scaled by
      font scale. Which may be sensible (this way you can change font "base size"
      when generating texture freely, as long as you always set font
      @link(Size) in your code explicitly, and things will keep looking the same
      --- bacause Scale only matters as a multiplier of original "base size").
      It also helps our poor outline implementation --- it may look bad
      at non-integer values.
      But it has disadvantages: UI scaling (@link(TUIContainer.UIScaling))
      doesn't affect outline size now.
      This will be fixed in the future, please speak up on Castle Game Engine
      forum if interested.

      @seealso OutlineHighQuality }
    property Outline: Cardinal read FOutline write FOutline default 0;

    { Optionally force better outline quality. Used only if Outline <> 0.
      High quality outline looks better, but is about 2x more expensive to draw.
      @seealso Outline }
    property OutlineHighQuality: boolean
      read FOutlineHighQuality write FOutlineHighQuality default false;

    { Outline color, used only if Outline <> 0. Default is black.
      @seealso Outline }
    property OutlineColor: TCastleColor read FOutlineColor write FOutlineColor;

    { Save draw properties to a stack. Saves:
      @link(Scale) (synchronized with @link(Size)),
      @link(Outline), @link(OutlineColor), @link(OutlineHighQuality),
      @link(TargetImage). }
    procedure PushProperties;
    procedure PopProperties;

    { Actual font-size. Usually same thing as @link(Size), but in case of proxy
      font classes (like TCustomizedFont) it makes sure to never return zero
      (which, in case of font proxies, means "use underlying font size"). }
    function RealSize: Single; virtual;

    { The image where we render the font.
      Usually (when this is @nil) our rendering routines render to the screen
      (or the current FBO, if you use @link(TGLRenderToTexture)).
      By setting this to non-nil, you make the rendering by done on CPU
      (without libraries like OpenGL/OpenGLES), and the text is drawn on the given image.

      The PushProperties and PopProperties methods save/restore this.

      @bold(TODO: Font scaling is not done when drawing font to an image.)
      So leave the @link(Scale) at 1 and don't touch the @link(Size)
      if you plan on rendering to image. Also, when using HTML tags,
      do not change the font size by them.
      Otherwise TextWidth / TextHeight will be unsynchronized
      with what @link(Print) actually does --- so not only your font will
      remain constant size, also it will overlap with itself.
      This may get fixed one day --- TCastleImage.Draw just needs to support scaling.
    }
    property TargetImage: TCastleImage read FTargetImage write FTargetImage;
  end;

  { @deprecated Deprecated name for TCastleFont. }
  TGLBitmapFontAbstract = TCastleFont deprecated;

  { 2D font using a texture initialized from a FreeType font file.

    This can load a font file, or it can use ready data in TTextureFontData.
    The latter allows to use this for fonts embedded in a Pascal source code,
    since our texture-font-to-pascal can convert a font ttf to a unit that defines
    ready TTextureFontData instance. }
  TTextureFont = class(TCastleFont)
  strict private
    FFont: TTextureFontData;
    FOwnsFont: boolean;
    GLImage: TGLImageCore;
    GlyphsScreenRects, GlyphsImageRects: TFloatRectangleList;
    function GetSmoothScaling: boolean;
  strict protected
    procedure SetScale(const Value: Single); override;
    function GetSize: Single; override;
    procedure SetSize(const Value: Single); override;
    procedure GLContextClose; override;
  public
    { The default component constructor. If you construct font
      this way, @bold(you must call @link(Load) before doing anything else
      with the font). }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    {$ifdef HAS_FREE_TYPE}
    { Create by reading a FreeType font file, like ttf.

      Providing charaters list as @nil means that we only create glyphs
      for SimpleAsciiCharacters, which includes only the basic ASCII characters.
      The ACharacters instance @italic(does not) become owned by this object,
      so remember to free it after calling this constructor. }
    constructor Create(const URL: string;
      const ASize: Integer; const AnAntiAliased: boolean;
      const ACharacters: TUnicodeCharList = nil); reintroduce;
    procedure Load(const URL: string;
      const ASize: Integer; const AnAntiAliased: boolean;
      const ACharacters: TUnicodeCharList = nil);

    constructor Create(const URL: string;
      const ASize: Integer; const AnAntiAliased: boolean;
      const ACharacters: TSetOfChars); deprecated;
    {$endif}

    { Create from a ready TTextureFontData instance.
      @param(Data TTextureFontData instance containing loaded image
        and glyphs parameters.)
      @param(OwnsData If @true, the Data instance becomes owned
        by this class (will be freed in our constructor).
        Usually you @italic(do not) want this, since usually you pass Data
        from a unit generated by texture-font-to-pascal. In this case,
        the finalization of CastleTextureFont_Xxx unit will already free
        the TTextureFontData instance.) }
    constructor Create(const Data: TTextureFontData;
      const OwnsData: boolean = false); reintroduce;
    procedure Load(const Data: TTextureFontData;
      const OwnsData: boolean = false);
    procedure PrepareResources; override;
    procedure Print(const X, Y: Integer; const Color: TCastleColor;
      const S: string); override;
    function TextWidth(const S: string): Integer; override;
    function TextHeight(const S: string): Integer; override;
    function TextHeightBase(const S: string): Integer; override;
    function TextMove(const S: string): TVector2Integer; override;
  end;

  { @deprecated Deprecated name, use TTextureFont now. }
  TGLBitmapFont = TTextureFont deprecated;

  { 2D font using a texture to define character images
    with constant width and height.

    This class has some assumptions about how the font image looks like:
    the characters are drawn in ASCII order, starting from space, on an image.
    Derive your own descendants of TCastleFont to have more flexibility,
    see the implementation of this class --- it is quite simple.
    Or use TTextureFont that can read data from a FreeType (like ttf) font file.

    See e.g. castle_game_engine/examples/fonts/data/sonic_asalga_0.png
    how to prepare an image for use with such font.
    You can find more such fonts on the Internet, see
    e.g. http://opengameart.org/content/sonic-font and
    http://opengameart.org/content/null-terminator. }
  TSimpleTextureFont = class(TCastleFont)
  strict private
    GLImage: TGLImageCore;
    Image: TCastleImage;
    ImageCols, ImageRows,
      CharMargin, CharDisplayMargin, CharWidth, CharHeight: Integer;
    GlyphsScreenRects, GlyphsImageRects: TFloatRectangleList;
    function ScaledCharWidth: Integer;
    function ScaledCharHeight: Integer;
    function ScaledCharDisplayMargin: Integer;
    function GetSmoothScaling: boolean;
  strict protected
    procedure SetScale(const Value: Single); override;
    function GetSize: Single; override;
    procedure SetSize(const Value: Single); override;
    procedure GLContextClose; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Load font from given image.
      @param AImage Image data, becomes owned by this class.
      @param ACharMargin There is a margin in the image between rows and cols.
      @param(ACharDisplayMargin We can display some spacing between characters.
        This is independent from CharMargin and image contents.) }
    procedure Load(AImage: TCastleImage;
      const AImageCols, AImageRows, ACharMargin, ACharDisplayMargin: Integer);
    procedure PrepareResources; override;
    procedure Print(const X, Y: Integer; const Color: TCastleColor;
      const S: string); override;
    function TextWidth(const S: string): Integer; override;
    function TextHeight(const S: string): Integer; override;
    function TextHeightBase(const S: string): Integer; override;
    function TextMove(const S: string): TVector2Integer; override;
  end;

  { Font that uses @italic(another) TCastleFont for rendering and sizing,
    but modifies the underlying font size (by simple scaling).
    Simply set the @code(Size) property of this instance to non-zero
    to force the specific size.

    The underlying font properties remain unchanged
    (so it can be still used for other purposes,
    directly or by other TCustomizedFont wrappers).

    @italic(Do not get / set the @code(Scale) property of this instance),
    it will not do anything in current implementation and should always
    stay equal to 1. }
  TCustomizedFont = class(TCastleFont)
  strict private
    FSourceFont: TCastleFont;
    // Note that we leave inherited Scale at == 1, always.
    FSize: Single;
    procedure SetSourceFont(const Value: TCastleFont);
  strict protected
    function GetSize: Single; override;
    procedure SetSize(const Value: Single); override;
    procedure GLContextClose; override;
    procedure Measure(out ARowHeight, ARowHeightBase, ADescend: Integer); override;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property SourceFont: TCastleFont read FSourceFont write SetSourceFont;

    procedure PrepareResources; override;
    procedure Print(const X, Y: Integer; const Color: TCastleColor;
      const S: string); override;
    function TextWidth(const S: string): Integer; override;
    function TextHeight(const S: string): Integer; override;
    function TextHeightBase(const S: string): Integer; override;
    function TextMove(const S: string): TVector2Integer; override;
    function RealSize: Single; override;
  end;

implementation

uses SysUtils, Math,
  CastleClassUtils, CastleGLUtils, CastleUtils, CastleFontFamily;

{ TCastleFont ------------------------------------------------------}

constructor TCastleFont.Create(AOwner: TComponent);
begin
  inherited;
  FScale := 1;
  FOutlineColor := Black;
  ApplicationProperties.OnGLContextCloseObject.Add(@GLContextCloseEvent);
end;

destructor TCastleFont.Destroy;
begin
  if ApplicationProperties <> nil then
    ApplicationProperties.OnGLContextCloseObject.Remove(@GLContextCloseEvent);
  GLContextClose;
  FreeAndNil(FPropertiesStack);
  inherited;
end;

procedure TCastleFont.GLContextCloseEvent(Sender: TObject);
begin
  GLContextClose;
end;

procedure TCastleFont.PrepareResources;
begin
end;

procedure TCastleFont.GLContextClose;
begin
end;

function TCastleFont.TextSize(const S: string): TVector2Integer;
begin
  Result := Vector2Integer(TextWidth(S), TextHeight(S));
end;

procedure TCastleFont.Print(const Pos: TVector2Integer;
  const Color: TCastleColor; const S: string);
begin
  Print(Pos[0], Pos[1], Color, S);
end;

procedure TCastleFont.Print(const s: string);
begin
  { Deprecated method uses other deprecated method here, don't warn }
  {$warnings off}
  Print(WindowPos[0], WindowPos[1], CurrentColor, S);
  {$warnings on}
end;

procedure TCastleFont.PrintAndMove(const s: string);
begin
  { Deprecated method uses other deprecated method here, don't warn }
  {$warnings off}
  Print(S);
  WindowPos := WindowPos + TextMove(S);
  {$warnings on}
end;

procedure TCastleFont.Print(const X, Y: Integer; const S: string);
begin
  { Deprecated method uses other deprecated method here, don't warn }
  {$warnings off}
  Print(X, Y, CurrentColor, S);
  {$warnings on}
end;

procedure TCastleFont.PrintRectMultiline(const Rect: TRectangle; const Color: TCastleColor;
  const S: string;
  const HorizontalAlignment: THorizontalPosition;
  const VerticalAlignment: TVerticalPosition;
  const Html: boolean;
  const LineSpacing: Integer;
  const TextHorizontalAlignment: THorizontalPosition);
var
  Strings: TStringList;
  ThisRect: TRectangle;
  X: Integer;
begin
  Strings := TStringList.Create;
  try
    Strings.Text := S;
    if Strings.Count <> 0 then
    begin
      ThisRect := Rectangle(0, 0, MaxTextWidth(Strings, Html),
        Strings.Count * (LineSpacing + RowHeight) - LineSpacing);
      ThisRect := ThisRect.
        Align(HorizontalAlignment, Rect, HorizontalAlignment).
        Align(VerticalAlignment, Rect, VerticalAlignment);
      case TextHorizontalAlignment of
        hpLeft   : X := ThisRect.Left;
        hpMiddle : X := ThisRect.Center[0];
        hpRight  : X := ThisRect.Right;
        else raise EInternalError.Create('TextHorizontalAlignment? in TCastleFont.PrintRectMultiline');
      end;
      PrintStrings(X, ThisRect.Bottom, Color, Strings,
        Html, LineSpacing, TextHorizontalAlignment);
    end;
  finally FreeAndNil(Strings) end;
end;

procedure TCastleFont.PrintRect(const Rect: TRectangle; const Color: TCastleColor;
  const S: string;
  const HorizontalAlignment: THorizontalPosition;
  const VerticalAlignment: TVerticalPosition);
var
  ThisRect: TRectangle;
begin
  ThisRect :=
    Rectangle(0, 0, TextWidth(S), TextHeight(S)).
    Align(HorizontalAlignment, Rect, HorizontalAlignment).
    Align(VerticalAlignment, Rect, VerticalAlignment);
  Print(ThisRect.Left, ThisRect.Bottom, Color, S);
end;

procedure TCastleFont.BreakLines(const unbroken: string;
  broken: TStrings; MaxLineWidth: integer);
var
  unbrokenlist: TStringList;
begin
  unbrokenlist := TStringList.Create;
  try
    Strings_SetText(unbrokenlist, unbroken);
    BreakLines(unbrokenlist, broken, MaxLineWidth);
  finally unbrokenlist.Free end;
end;

procedure TCastleFont.BreakLines(unbroken, broken: TStrings;
  MaxLineWidth: integer);
var
  i, FirstToBreak: Integer;
begin
  FirstToBreak := broken.count;
  for I := 0 to unbroken.count-1 do broken.Append(unbroken[i]);
  BreakLines(broken, MaxLineWidth, FirstToBreak);
end;

procedure TCastleFont.BreakLines(broken: TStrings;
  MaxLineWidth: Integer; FirstToBreak: integer);
var
  I: Integer;
  LineWidth, LineWidthBytes: Integer;
  P: Integer;
  BreakInput, BreakOutput1, BreakOutput2, HardBreakOutput: string;
  C: TUnicodeChar;
  BreakInputPtr: PChar;
  CharLen: Integer;
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
    BreakInputPtr := PChar(BreakInput);

    C := UTF8CharacterToUnicode(BreakInputPtr, CharLen);
    if (C > 0) and (CharLen > 0) then
    begin
      { the first character C is always considered to fit into BreakOutput1,
        regardless of MaxLineWidth --- after all, we have to place this character
        somewhere (otherwise we would have to reject this character
        or raise an error). }
      Inc(BreakInputPtr, CharLen);
      LineWidthBytes := CharLen;
      LineWidth := TextWidth(UnicodeToUTF8(C));

      C := UTF8CharacterToUnicode(BreakInputPtr, CharLen);
      while (C > 0) and (CharLen > 0) and
            (LineWidth + TextWidth(UnicodeToUTF8(C)) <= MaxLineWidth) do
      begin
        Inc(BreakInputPtr, CharLen);
        LineWidthBytes += CharLen;
        LineWidth += TextWidth(UnicodeToUTF8(C));

        C := UTF8CharacterToUnicode(BreakInputPtr, CharLen);
      end;

      if (C > 0) and (CharLen > 0) then // then above loop stopped because we have to break
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

function TCastleFont.MaxTextWidth(SList: TStrings; const Html: boolean): Integer;
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

procedure TCastleFont.PrintStrings(const X0, Y0: Integer;
  const Color: TCastleColor; const Strs: TStrings;
  const Html: boolean; const LineSpacing: Integer;
  const TextHorizontalAlignment: THorizontalPosition);

  function XPos(const Line: Integer; const S: string): Integer;
  begin
    case TextHorizontalAlignment of
      hpLeft  : Result := X0;
      hpMiddle: Result := X0 - TextWidth(S) div 2;
      hpRight : Result := X0 - TextWidth(S);
      else EInternalError.Create('TCastleFont.PrintStrings: TextHorizontalAlignment unknown');
    end;
  end;

  function YPos(const Line: Integer): Integer;
  begin
    Result := (Strs.Count - 1 - Line) * (RowHeight + LineSpacing) + Y0;
  end;

var
  S: string;
  Line: Integer;
  Text: TRichText;
begin
  if not Html then
  begin
    { simple and fast implementation in case TRichText not needed }
    for Line := 0 to Strs.Count - 1 do
    begin
      S := Strs[Line];
      Print(XPos(Line, S), YPos(Line), Color, S);
    end;
  end else
  begin
    Text := TRichText.Create(Self, Strs, Html);
    try
      Text.Print(X0, Y0, Color, LineSpacing, TextHorizontalAlignment);
    finally FreeAndNil(Text) end;
  end;
end;

procedure TCastleFont.PrintStrings(const X0, Y0: Integer;
  const Color: TCastleColor; const Strs: array of string;
  const Html: boolean; const LineSpacing: Integer;
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

procedure TCastleFont.PrintStrings(const Strs: TStrings;
  const Html: boolean; const LineSpacing: Integer;
  const X0: Integer; const Y0: Integer);
begin
  { Deprecated stuff uses other deprecated stuff here, don't warn }
  {$warnings off}
  PrintStrings(X0, Y0, CurrentColor, Strs, Html, LineSpacing);
  {$warnings on}
end;

procedure TCastleFont.PrintStrings(const Strs: array of string;
  const Html: boolean; const LineSpacing: Integer; const X0: Integer;
  const Y0: Integer);
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

function TCastleFont.PrintBrokenString(X0, Y0: Integer;
  const Color: TCastleColor; const S: string; const MaxLineWidth: Integer;
  const PositionsFirst: boolean; const LineSpacing: Integer;
  const Html: boolean): Integer;
var
  Text: TRichText;
begin
  Text := TRichText.Create(Self, S, Html);
  try
    Text.Wrap(MaxLineWidth);
    if PositionsFirst then
      Y0 -= (Text.Count-1) * (RowHeight + LineSpacing);
    Text.Print(X0, Y0, Color, LineSpacing);
    Result := Text.Count;
  finally FreeAndNil(Text) end;
end;

function TCastleFont.PrintBrokenString(const Rect: TRectangle;
  const Color: TCastleColor; const S: string;
  const LineSpacing: Integer;
  const AlignHorizontal: THorizontalPosition;
  const AlignVertical: TVerticalPosition;
  const Html: boolean): Integer;
var
  Text: TRichText;
  X0, Y0, BrokenHeight: Integer;
begin
  Text := TRichText.Create(Self, S, Html);
  try
    Text.Wrap(Rect.Width);
    { calculate X0 based on Rect and Text.Width }
    case AlignHorizontal of
      hpLeft  : X0 := Rect.Left;
      hpMiddle: X0 := Rect.Left + (Rect.Width - Text.Width) div 2;
      hpRight : X0 := Rect.Right - Text.Width;
      else raise EInternalError.Create('PrintBrokenString.AlignHorizontal?');
    end;
    { calculate Y0 based on Rect and BrokenHeight }
    BrokenHeight := Text.Count * (LineSpacing + RowHeight);
    case AlignVertical of
      vpBottom: Y0 := Rect.Bottom;
      vpMiddle: Y0 := Rect.Bottom + (Rect.Height - BrokenHeight) div 2;
      vpTop   : Y0 := Rect.Top - BrokenHeight;
      else raise EInternalError.Create('PrintBrokenString.AlignVertical?');
    end;
    Text.Print(X0, Y0, Color, LineSpacing);
    Result := Text.Count;
  finally FreeAndNil(Text) end;
end;

function TCastleFont.PrintBrokenString(const S: string;
  const MaxLineWidth, X0, Y0: Integer;
  const PositionsFirst: boolean;
  const LineSpacing: Integer): Integer; deprecated;
begin
  { Deprecated stuff uses other deprecated stuff here, don't warn }
  {$warnings off}
  Result := PrintBrokenString(X0, Y0, CurrentColor, S, MaxLineWidth,
    PositionsFirst, LineSpacing);
  {$warnings on}
end;

procedure TCastleFont.Measure(out ARowHeight, ARowHeightBase, ADescend: Integer);
var
  OldScale: Single;
begin
  OldScale := Scale;
  Scale := 1;
  ARowHeight := TextHeight('Wy');
  ARowHeightBase := TextHeightBase('W');
  ADescend := Max(0, TextHeight('y') - TextHeight('a'));
  Scale := OldScale;
end;

procedure TCastleFont.MakeMeasure;
begin
  if not MeasureDone then
  begin
    Measure(FRowHeight, FRowHeightBase, FDescend);
    MeasureDone := true;
  end;
end;

procedure TCastleFont.InvalidateMeasure;
begin
  MeasureDone := false;
end;

function TCastleFont.RowHeight: Integer;
begin
  MakeMeasure;
  Result := Round(FRowHeight * Scale);
end;

function TCastleFont.RowHeightBase: Integer;
begin
  MakeMeasure;
  Result := Round(FRowHeightBase * Scale);
end;

function TCastleFont.Descend: Integer;
begin
  MakeMeasure;
  Result := Round(FDescend * Scale);
end;

procedure TCastleFont.SetScale(const Value: Single);
begin
  FScale := Value;
end;

procedure TCastleFont.PushProperties;
var
  SavedProperites: TSavedProperties;
begin
  if FPropertiesStack = nil then
    FPropertiesStack := TSavedPropertiesList.Create;

  SavedProperites := TSavedProperties.Create;
  SavedProperites.Scale := Scale;
  SavedProperites.Outline := Outline;
  SavedProperites.OutlineColor := OutlineColor;
  SavedProperites.OutlineHighQuality := OutlineHighQuality;
  SavedProperites.TargetImage := TargetImage;
  FPropertiesStack.Add(SavedProperites);
end;

procedure TCastleFont.PopProperties;
var
  SavedProperites: TSavedProperties;
begin
  if (FPropertiesStack = nil) or (FPropertiesStack.Count = 0) then
    raise Exception.Create('Cannot do TCastleFont.PopProperties, stack empty. Every PopProperties should match previous PushProperties');

  SavedProperites := FPropertiesStack.Last;
  Scale        := SavedProperites.Scale;
  Outline      := SavedProperites.Outline;
  OutlineColor := SavedProperites.OutlineColor;
  OutlineHighQuality := SavedProperites.OutlineHighQuality;
  TargetImage  := SavedProperites.TargetImage;
  FPropertiesStack.Delete(FPropertiesStack.Count - 1);
end;

function TCastleFont.RealSize: Single;
begin
  Result := Size;
end;

{ TTextureFont --------------------------------------------------------------- }

constructor TTextureFont.Create(AOwner: TComponent);
begin
  inherited;
  GlyphsScreenRects := TFloatRectangleList.Create;
  GlyphsImageRects  := TFloatRectangleList.Create;
end;

destructor TTextureFont.Destroy;
begin
  Load(nil); // free previous FFont data
  FreeAndNil(GlyphsScreenRects);
  FreeAndNil(GlyphsImageRects);
  inherited;
end;

{$ifdef HAS_FREE_TYPE}
constructor TTextureFont.Create(const URL: string;
  const ASize: Integer; const AnAntiAliased: boolean;
  const ACharacters: TUnicodeCharList);
begin
  Create(TComponent(nil));
  Load(URL, ASize, AnAntiAliased, ACharacters);
end;

procedure TTextureFont.Load(const URL: string;
  const ASize: Integer; const AnAntiAliased: boolean;
  const ACharacters: TUnicodeCharList);
begin
  Load(TTextureFontData.Create(URL, ASize, AnAntiAliased, ACharacters), true);
end;

constructor TTextureFont.Create(const URL: string;
  const ASize: Integer; const AnAntiAliased: boolean;
  const ACharacters: TSetOfChars);
var
  Chars: TUnicodeCharList;
  C: char;
begin
  Chars := TUnicodeCharList.Create;
  try
    for C in ACharacters do
      Chars.Add(Ord(C));
    Create(URL, ASize, AnAntiAliased, Chars);
  finally FreeAndNil(Chars) end;
end;
{$endif}

constructor TTextureFont.Create(const Data: TTextureFontData; const OwnsData: boolean);
begin
  Create(TComponent(nil));
  Load(Data, OwnsData);
end;

procedure TTextureFont.Load(const Data: TTextureFontData; const OwnsData: boolean);
begin
  if FOwnsFont then
    FreeAndNil(FFont) else
    FFont := nil;

  FOwnsFont := OwnsData;
  FFont := Data;
end;

function TTextureFont.GetSmoothScaling: boolean;
begin
  Result := Scale <> 1;
end;

procedure TTextureFont.PrepareResources;
begin
  inherited;
  if GLImage = nil then
    GLImage := TGLImageCore.Create(FFont.Image, GetSmoothScaling);
end;

procedure TTextureFont.GLContextClose;
begin
  FreeAndNil(GLImage);
  inherited;
end;

const
  { These many glyphs will be allocated always.
    This avoids reallocating memory if you just render strings shorter than this.

    This affects 2 arrays, and SizeOf(TFloatRectangle) should be 4 * 4.
    So it costs 32 bytes per item. }
  MinimumGlyphsAllocated = 100;

procedure TTextureFont.Print(const X, Y: Integer; const Color: TCastleColor;
  const S: string);
var
  ScreenX, ScreenY: Single;
  G: TTextureFontData.TGlyph;
  GlyphsToRender: Integer;

  procedure GlyphDraw(const OutlineMoveX, OutlineMoveY: Integer);
  var
    ScreenRect, ImageRect: PFloatRectangle;
  begin
    if TargetImage <> nil then
    begin
      TargetImage.DrawFrom(FFont.Image,
        Round(ScreenX - G.X * Scale + OutlineMoveX * Outline),
        Round(ScreenY - G.Y * Scale + OutlineMoveY * Outline),
        G.ImageX,
        G.ImageY,
        G.Width,
        G.Height);
    end else
    begin
      Assert(GlyphsToRender < GlyphsScreenRects.Count);

      ScreenRect := GlyphsScreenRects.Ptr(GlyphsToRender);
      ScreenRect^.Left   := ScreenX - G.X * Scale + OutlineMoveX * Outline;
      ScreenRect^.Bottom := ScreenY - G.Y * Scale + OutlineMoveY * Outline;
      ScreenRect^.Width  := G.Width  * Scale;
      ScreenRect^.Height := G.Height * Scale;

      ImageRect := GlyphsImageRects.Ptr(GlyphsToRender);
      ImageRect^.Left   := G.ImageX;
      ImageRect^.Bottom := G.ImageY;
      ImageRect^.Width  := G.Width;
      ImageRect^.Height := G.Height;

      Inc(GlyphsToRender);
    end;
  end;

var
  C: TUnicodeChar;
  TextPtr: PChar;
  CharLen, GlyphsPerChar: Integer;
begin
  if TargetImage = nil then
  begin
    PrepareResources;

    { allocate the necessary glyphs at start.
      This allows to quickly fill them later.
      Note that we possibly allocate too much, because Length(S) may be > UTF8Length(S)
      (because of multi-byte characters), and also because some characters do not have glyphs.
      That's OK, we'll calculate real GlyphsToRender when iterating. }
    if Outline = 0 then
      GlyphsPerChar := 1 else
    if OutlineHighQuality then
      GlyphsPerChar := 8 else
      GlyphsPerChar := 4;
    GlyphsScreenRects.Count := Max(MinimumGlyphsAllocated, GlyphsPerChar * Length(S));
    GlyphsImageRects .Count := Max(MinimumGlyphsAllocated, GlyphsPerChar * Length(S));
  end;

  { first pass, to render Outline.

    This could be done better by being done together with non-outline pass,
    by filling the alternative place in Glyph arrays, such that outline and non-outline data
    don't collide.
    It would be 1. faster (don't iterate over S two times), 2. less code duplication. }
  if Outline <> 0 then
  begin
    GlyphsToRender := 0;
    ScreenX := X;
    ScreenY := Y;
    if TargetImage <> nil then
      FFont.Image.ColorWhenTreatedAsAlpha := Vector3Byte(Vector3SingleCut(OutlineColor)); // ignore Color[3] for now

    TextPtr := PChar(S);
    C := UTF8CharacterToUnicode(TextPtr, CharLen);
    while (C > 0) and (CharLen > 0) do
    begin
      Inc(TextPtr, CharLen);

      G := FFont.Glyph(C);
      if G <> nil then
      begin
        if (G.Width <> 0) and (G.Height <> 0) then
        begin
          GlyphDraw(0, 0);
          GlyphDraw(0, 2);
          GlyphDraw(2, 2);
          GlyphDraw(2, 0);

          if OutlineHighQuality then
          begin
            GlyphDraw(1, 0);
            GlyphDraw(1, 2);
            GlyphDraw(0, 1);
            GlyphDraw(2, 1);
          end;
        end;
        ScreenX += G.AdvanceX * Scale + Outline * 2;
        ScreenY += G.AdvanceY * Scale;
      end;

      C := UTF8CharacterToUnicode(TextPtr, CharLen);
    end;

    if TargetImage = nil then
    begin
      GLImage.Color := OutlineColor;
      GLImage.Draw(
        PFloatRectangle(GlyphsScreenRects.List),
        PFloatRectangle(GlyphsImageRects.List), GlyphsToRender);
    end;
  end;

  GlyphsToRender := 0;
  ScreenX := X;
  ScreenY := Y;
  if TargetImage <> nil then
    FFont.Image.ColorWhenTreatedAsAlpha := Vector3Byte(Vector3SingleCut(Color)); // ignore Color[3] for now

  TextPtr := PChar(S);
  C := UTF8CharacterToUnicode(TextPtr, CharLen);
  while (C > 0) and (CharLen > 0) do
  begin
    Inc(TextPtr, CharLen);

    G := FFont.Glyph(C);
    if G <> nil then
    begin
      if (G.Width <> 0) and (G.Height <> 0) then
        if Outline <> 0 then
          GlyphDraw(1, 1) else
          GlyphDraw(0, 0);
      ScreenX += G.AdvanceX * Scale + Outline * 2;
      ScreenY += G.AdvanceY * Scale;
    end;

    C := UTF8CharacterToUnicode(TextPtr, CharLen);
  end;

  if TargetImage = nil then
  begin
    GLImage.Color := Color;
    GLImage.Draw(
      PFloatRectangle(GlyphsScreenRects.List),
      PFloatRectangle(GlyphsImageRects.List), GlyphsToRender);
  end;
end;

function TTextureFont.TextWidth(const S: string): Integer;
begin
  Result := Round(FFont.TextWidth(S) * Scale);
  if Outline <> 0 then
    Result += Outline * 2 * UTF8Length(S);
end;

function TTextureFont.TextHeight(const S: string): Integer;
begin
  Result := Round(FFont.TextHeight(S) * Scale) + Outline * 2;
end;

function TTextureFont.TextHeightBase(const S: string): Integer;
begin
  Result := Round(FFont.TextHeightBase(S) * Scale) + Outline * 2;
end;

function TTextureFont.TextMove(const S: string): TVector2Integer;
begin
  Result := FFont.TextMove(S);
  Result[0] := Round(Result[0] * Scale);
  if Outline <> 0 then
    Result[0] += Outline * 2 * UTF8Length(S);
  Result[1] := Round(Result[1] * Scale);
end;

procedure TTextureFont.SetScale(const Value: Single);
begin
  inherited;
  if GLImage <> nil then
    GLImage.SmoothScaling := GetSmoothScaling;
end;

function TTextureFont.GetSize: Single;
begin
  Result := FFont.Size * Scale;
end;

procedure TTextureFont.SetSize(const Value: Single);
begin
  Scale := Value / FFont.Size;
end;

{ TSimpleTextureFont --------------------------------------------------------- }

constructor TSimpleTextureFont.Create(AOwner: TComponent);
begin
  inherited;
  GlyphsScreenRects := TFloatRectangleList.Create;
  GlyphsImageRects  := TFloatRectangleList.Create;
end;

destructor TSimpleTextureFont.Destroy;
begin
  FreeAndNil(GlyphsScreenRects);
  FreeAndNil(GlyphsImageRects);
  FreeAndNil(Image);
  inherited;
end;

procedure TSimpleTextureFont.Load(AImage: TCastleImage;
  const AImageCols, AImageRows, ACharMargin, ACharDisplayMargin: Integer);
begin
  FreeAndNil(Image);

  Image := AImage;

  ImageCols := AImageCols;
  ImageRows := AImageRows;
  CharMargin := ACharMargin;
  CharWidth := Image.Width div ImageCols - CharMargin;
  CharHeight := Image.Height div ImageRows - CharMargin;
  CharDisplayMargin := ACharDisplayMargin;
end;

function TSimpleTextureFont.ScaledCharWidth: Integer;
begin
  Result := Round(CharWidth * Scale) + Outline * 2;
end;

function TSimpleTextureFont.ScaledCharHeight: Integer;
begin
  Result := Round(CharHeight * Scale) + Outline * 2;
end;

function TSimpleTextureFont.ScaledCharDisplayMargin: Integer;
begin
  Result := Round(CharDisplayMargin * Scale);
end;

function TSimpleTextureFont.GetSmoothScaling: boolean;
begin
  Result := Scale <> 1;
end;

procedure TSimpleTextureFont.PrepareResources;
begin
  inherited;
  if GLImage = nil then
    GLImage := TGLImageCore.Create(Image, GetSmoothScaling);
end;

procedure TSimpleTextureFont.GLContextClose;
begin
  FreeAndNil(GLImage);
  inherited;
end;

procedure TSimpleTextureFont.Print(const X, Y: Integer; const Color: TCastleColor;
  const S: string);
var
  GlyphsToRender: Integer;

  procedure GlyphDraw(const ScreenRect, ImageRect: TFloatRectangle);
  begin
    if TargetImage = nil then
    begin
      Assert(GlyphsToRender < GlyphsScreenRects.Count);
      GlyphsScreenRects.List^[GlyphsToRender] := ScreenRect;
      GlyphsImageRects .List^[GlyphsToRender] := ImageRect;
      Inc(GlyphsToRender);
    end else
    begin
      TargetImage.DrawFrom(Image,
        Round(ScreenRect.Left),
        Round(ScreenRect.Bottom),
        Round(ImageRect.Left),
        Round(ImageRect.Bottom),
        Round(ImageRect.Width),
        Round(ImageRect.Height));
    end;
  end;

var
  ImageX, ImageY: Single;
  CharIndex, ScreenX, ScreenY: Integer;
  C: TUnicodeChar;
  TextPtr: PChar;
  I, CharLen: Integer;
begin
  if TargetImage = nil then
  begin
    PrepareResources;
    GlyphsScreenRects.Count := Max(MinimumGlyphsAllocated, Length(S));
    GlyphsImageRects .Count := Max(MinimumGlyphsAllocated, Length(S));
    GLImage.Color := Color;
  end;

  GlyphsToRender := 0;

  TextPtr := PChar(S);
  C := UTF8CharacterToUnicode(TextPtr, CharLen);
  I := 0;
  while (C > 0) and (CharLen > 0) do
  begin
    Inc(TextPtr, CharLen);

    CharIndex := C - Ord(' ');
    ImageX := CharIndex mod ImageCols;
    ImageY := CharIndex div ImageCols;
    if ImageY < ImageRows then
    begin
      ImageX := ImageX * (CharWidth + CharMargin);
      ImageY := Image.Height - (ImageY + 1) * (CharHeight + CharMargin);
      ScreenX := ScaledCharDisplayMargin div 2 + X + I * (ScaledCharWidth + ScaledCharDisplayMargin);
      ScreenY := ScaledCharDisplayMargin div 2 + Y;
      Inc(I);

      { TODO: this ignores Outline and related properties now, always renders like Outline = 0. }

      GlyphDraw(
        FloatRectangle(ScreenX, ScreenY, ScaledCharWidth, ScaledCharHeight),
        FloatRectangle(ImageX, ImageY, CharWidth, CharHeight));
    end;

    C := UTF8CharacterToUnicode(TextPtr, CharLen);
  end;

  if TargetImage = nil then
  begin
    GLImage.Color := Color;
    GLImage.Draw(
      PFloatRectangle(GlyphsScreenRects.List),
      PFloatRectangle(GlyphsImageRects.List), GlyphsToRender);
  end;
end;

function TSimpleTextureFont.TextWidth(const S: string): Integer;
begin
  Result := Length(S) * (ScaledCharWidth + ScaledCharDisplayMargin);
end;

function TSimpleTextureFont.TextHeight(const S: string): Integer;
begin
  Result := ScaledCharHeight + ScaledCharDisplayMargin;
end;

function TSimpleTextureFont.TextHeightBase(const S: string): Integer;
begin
  Result := ScaledCharHeight + ScaledCharDisplayMargin;
end;

function TSimpleTextureFont.TextMove(const S: string): TVector2Integer;
begin
  Result := Vector2Integer(TextWidth(S), TextHeight(S));
end;

procedure TSimpleTextureFont.SetScale(const Value: Single);
begin
  inherited;
  if GLImage <> nil then
    GLImage.SmoothScaling := GetSmoothScaling;
end;

function TSimpleTextureFont.GetSize: Single;
begin
  Result := CharHeight * Scale;
end;

procedure TSimpleTextureFont.SetSize(const Value: Single);
begin
  Scale := Value / CharHeight;
end;

{ TCustomizedFont ------------------------------------------------------------ }

constructor TCustomizedFont.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TCustomizedFont.Destroy;
begin
  SourceFont := nil; // this will free FSourceFont if needed
  inherited;
end;

function TCustomizedFont.GetSize: Single;
begin
  Result := FSize;
end;

procedure TCustomizedFont.SetSize(const Value: Single);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    InvalidateMeasure;
  end;
end;

procedure TCustomizedFont.SetSourceFont(const Value: TCastleFont);
begin
  if FSourceFont <> Value then
  begin
    if FSourceFont <> nil then
      FSourceFont.RemoveFreeNotification(Self);
    FSourceFont := Value;
    if FSourceFont <> nil then
      FSourceFont.FreeNotification(Self);
  end;
end;

procedure TCustomizedFont.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) and (AComponent = FSourceFont) then
  begin
    { set to nil by SetSourceFont to clean nicely }
    SourceFont := nil;
  end;
end;

procedure TCustomizedFont.PrepareResources;
begin
  if FSourceFont <> nil then
    FSourceFont.PrepareResources;
end;

procedure TCustomizedFont.GLContextClose;
begin
  if FSourceFont <> nil then
    FSourceFont.GLContextClose;
end;

procedure TCustomizedFont.Print(const X, Y: Integer; const Color: TCastleColor;
  const S: string);
begin
  if Size <> 0 then
  begin
    FSourceFont.PushProperties;
    FSourceFont.Size := Size;
  end;
  FSourceFont.Print(X, Y, Color, S);
  if Size <> 0 then
    FSourceFont.PopProperties;
end;

function TCustomizedFont.TextWidth(const S: string): Integer;
begin
  if Size <> 0 then
  begin
    FSourceFont.PushProperties;
    FSourceFont.Size := Size;
  end;
  Result := FSourceFont.TextWidth(S);
  if Size <> 0 then
    FSourceFont.PopProperties;
end;

function TCustomizedFont.TextHeight(const S: string): Integer;
begin
  if Size <> 0 then
  begin
    FSourceFont.PushProperties;
    FSourceFont.Size := Size;
  end;
  Result := FSourceFont.TextHeight(S);
  if Size <> 0 then
    FSourceFont.PopProperties;
end;

function TCustomizedFont.TextHeightBase(const S: string): Integer;
begin
  if Size <> 0 then
  begin
    FSourceFont.PushProperties;
    FSourceFont.Size := Size;
  end;
  Result := FSourceFont.TextHeightBase(S);
  if Size <> 0 then
    FSourceFont.PopProperties;
end;

function TCustomizedFont.TextMove(const S: string): TVector2Integer;
begin
  if Size <> 0 then
  begin
    FSourceFont.PushProperties;
    FSourceFont.Size := Size;
  end;
  Result := FSourceFont.TextMove(S);
  if Size <> 0 then
    FSourceFont.PopProperties;
end;

function TCustomizedFont.RealSize: Single;
begin
  if Size <> 0 then
    Result := Size else
    Result := SourceFont.RealSize;
end;

procedure TCustomizedFont.Measure(out ARowHeight, ARowHeightBase, ADescend: Integer);
begin
  { in usual circumstances, overriding Measure in TCustomizedFont is not needed,
    the default implementation would work OK. But if the FSourceFont has some custom
    override fot Measure(), than we need to call it here, otherwise wrapping
    a font in TCustomizedFont (which is what e.g. TCastleLabel does when it has some
    size) would not use the user Measure implementation. }
  FSourceFont.Measure(ARowHeight, ARowHeightBase, ADescend);
  { our Scale is always 1, to scale the resulting sizes manually now }
  if Size <> 0 then
  begin
    ARowHeight     := Round(ARowHeight     * Size / FSourceFont.Size);
    ARowHeightBase := Round(ARowHeightBase * Size / FSourceFont.Size);
    ADescend       := Round(ADescend       * Size / FSourceFont.Size);
  end;
end;

end.
