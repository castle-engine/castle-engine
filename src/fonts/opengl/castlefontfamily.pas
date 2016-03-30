{
  Copyright 2016-2016 Michalis Kamburelis.

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

uses Classes, FGL,
  CastleColors, CastleVectors, CastleRectangles, CastleFonts;

type
  { Font family, with different subfonts for normal, bold, italic, bold+italic
    variants. It implements the powerful methods to render and wrap text
    with HTML tags (<b>, <i>, <p>, <font color="#rrbbgg">).
    During such processing and rendering, it automatically uses the correct
    subfont. It's closely tied with the TRichText class.

    For simple operations, it simply uses the @link(Regular) subfont.
    It can be treated as a font itself, since it has all the measuring
    and rendering commands you expect from a font (and you could
    even use it as a subfont of another TFontFamily --- weird but works,
    in which case the "TFontFamily used as a subfont" just acts as a proxy
    for the Regular subfont).

    Similar to TCustomizedFont, it can also change the subfont size.
    Simply set the @code(Size) property of this instance to non-zero
    to force the specific size of all the underlying subfonts.
    The underlying font properties remain unchanged for subfonts
    (so they can be still used for other purposes,
    directly or by other TCustomizedFont or TFontFamily wrappers).

    @italic(Do not get / set the @code(Scale) property of this instance),
    it will not do anything in current implementation and should always
    stay equal to 1.
    TODO: remove the Scale property of TCastleFont,
    or make it only a protected read-only property? }
  TFontFamily = class(TCastleFont)
  strict private
    FRegularFont, FBoldFont, FItalicFont, FBoldItalicFont: TCastleFont;
    // Note that we leave inherited Scale at == 1, always.
    FSize: Single;
    procedure SetRegularFont(const Value: TCastleFont);
    procedure SetBoldFont(const Value: TCastleFont);
    procedure SetItalicFont(const Value: TCastleFont);
    procedure SetBoldItalicFont(const Value: TCastleFont);
    function SubFont(const Bold, Italic: boolean): TCastleFont;
  strict protected
    function GetSize: Single; override;
    procedure SetSize(const Value: Single); override;
    procedure GLContextClose; override;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property RegularFont: TCastleFont read FRegularFont write SetRegularFont;
    property BoldFont: TCastleFont read FBoldFont write SetBoldFont;
    property ItalicFont: TCastleFont read FItalicFont write SetItalicFont;
    property BoldItalicFont: TCastleFont read FBoldItalicFont write SetBoldItalicFont;

    procedure PrepareResources; override;
    procedure Print(const X, Y: Integer; const Color: TCastleColor;
      const S: string); override;
    function TextWidth(const S: string): Integer; override;
    function TextHeight(const S: string): Integer; override;
    function TextHeightBase(const S: string): Integer; override;
    function TextMove(const S: string): TVector2Integer; override;
  end;

  TTextCommand = (
    tcBold, tcBoldEnd, tcItalic, tcItalicEnd, tcFontColor, tcParagraph);
  TTextProperty = class abstract;
  TTextPropertyString = class(TTextProperty)
    S: string;
  end;
  TTextPropertyCommand = class(TTextProperty)
    Command: TTextCommand;
    Color: TCastleColor;
  end;

  { Line of text with processing commands. }
  TTextLine = class(specialize TFPGObjectList<TTextProperty>)
  strict private
    FLineWidthKnown: boolean;
    FLineWidth: Cardinal;
    FFont: TFontFamily;
  public
    //function LineWidth: Cardinal;

    { Render line of text at given position.

      The values of Color, Bold, Italic specify the initial state
      at the beginning of the line, they may get modified while rendering
      the line. Note that Bold and Italic are expressed as integers,
      to smoothly handle <b> embeded in <b>. They value <> 0 indicates
      to use the given variant. }
    // procedure Print(const Font: TFontFamily;
    //   const X0, Y0: Integer; var Color: TCastleColor;
    //   var Bold, Italic: Cardinal);
    constructor Create(const AFont: TFontFamily);
  end;

  { Multi-line text with processing commands
    (like "change font to bold now"). Used to render "rich text",
    which is text that may contain (a subset of) HTML tags.

    Note that TRichText instance is always tied to a corresponding
    TFontFamily used to render it. @bold(Through the lifetime of TRichText,
    we assume that size and other properties of this font remain constant.) }
  TRichText = class(specialize TFPGObjectList<TTextLine>)
  strict private
    FMaxLineWidthKnown: boolean;
    { Known max line width, e.g. calculated by @link(Wrap).
      Using this allows to avoid recalculating this many times,
      e.g. @link(Wrap) always calculates this as a by-product of it's work. }
    FMaxLineWidth: Cardinal;
    FFont: TFontFamily;
  public
    constructor Create(const AFont: TFontFamily;
      const Text: TStrings; const Tags: boolean);
    //procedure Wrap(const Font: TFontFamily; const MaxLineWidth: Cardinal);
    //function MaxLineWidth: Cardinal;
  end;

implementation

uses SysUtils;

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

function TFontFamily.GetSize: Single;
begin
  Result := FSize;
end;

procedure TFontFamily.SetSize(const Value: Single);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    InvalidateMeasure;
  end;
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

procedure TFontFamily.Print(const X, Y: Integer; const Color: TCastleColor;
  const S: string);
begin
  if Size <> 0 then
  begin
    SubFont(false, false).PushProperties;
    SubFont(false, false).Size := Size;
  end;
  SubFont(false, false).Print(X, Y, Color, S);
  if Size <> 0 then
    SubFont(false, false).PopProperties;
end;

function TFontFamily.TextWidth(const S: string): Integer;
begin
  if Size <> 0 then
  begin
    SubFont(false, false).PushProperties;
    SubFont(false, false).Size := Size;
  end;
  Result := SubFont(false, false).TextWidth(S);
  if Size <> 0 then
    SubFont(false, false).PopProperties;
end;

function TFontFamily.TextHeight(const S: string): Integer;
begin
  if Size <> 0 then
  begin
    SubFont(false, false).PushProperties;
    SubFont(false, false).Size := Size;
  end;
  Result := SubFont(false, false).TextHeight(S);
  if Size <> 0 then
    SubFont(false, false).PopProperties;
end;

function TFontFamily.TextHeightBase(const S: string): Integer;
begin
  if Size <> 0 then
  begin
    SubFont(false, false).PushProperties;
    SubFont(false, false).Size := Size;
  end;
  Result := SubFont(false, false).TextHeightBase(S);
  if Size <> 0 then
    SubFont(false, false).PopProperties;
end;

function TFontFamily.TextMove(const S: string): TVector2Integer;
begin
  if Size <> 0 then
  begin
    SubFont(false, false).PushProperties;
    SubFont(false, false).Size := Size;
  end;
  Result := SubFont(false, false).TextMove(S);
  if Size <> 0 then
    SubFont(false, false).PopProperties;
end;

function TFontFamily.SubFont(const Bold, Italic: boolean): TCastleFont;
begin
  if Bold and Italic and (BoldItalicFont <> nil) then
    Result := BoldItalicFont else
  if Bold and (BoldFont <> nil) then
    Result := BoldFont else
  if Italic and (ItalicFont <> nil) then
    Result := ItalicFont else
  if RegularFont <> nil then
    Result := RegularFont else
    raise Exception.Create('You must set at least RegularFont of TFontFamily to use it for processing and rendering');
end;

{ TTextLine ------------------------------------------------------------------ }

constructor TTextLine.Create(const AFont: TFontFamily);
begin
  inherited Create;
  FFont := AFont;
end;

{ TRichText ------------------------------------------------------------------ }

constructor TRichText.Create(const AFont: TFontFamily;
  const Text: TStrings; const Tags: boolean);
begin
  inherited Create;
  FFont := AFont;
end;

end.
