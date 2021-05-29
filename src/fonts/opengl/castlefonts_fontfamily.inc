{%MainUnit castlefonts.pas}
{
  Copyright 2016-2021 Michalis Kamburelis.

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
  { Font family, with different subfonts for normal, bold, italic, bold+italic
    variants. Used this together with @link(TCastleLabel.Html) that allows
    to render HTML text with varying font styles, changing @link(Bold) and @link(Italic)
    state during rendering.
    Or you can use this for normal font rendering (that doesn't change
    font style in the middle, e.g. TCastleLabel with TCastleLabel.Html = @false)
    and just enjoy simple Boolean properties
    @link(Bold) and @link(Italic) to change the font style yourself at runtime or design-time.

    This class can be treated as a font itself, since it has all the measuring
    and rendering commands you expect from a font (and you could
    even use it as a subfont of another TCastleFontFamily --- weird but works,
    in which case the "TCastleFontFamily used as a subfont" just acts as a proxy
    for the Regular subfont).

    Similar to TCustomizedFont, it can also change the subfont size.
    Simply set the @code(Size) property of this instance to non-zero
    to force the specific size of all the underlying subfonts.
    You can also change the subfont outline, if CustomizeOutline is used.
    The underlying font properties remain unchanged for subfonts
    (so they can be still used for other purposes,
    directly or by other TCustomizedFont or TCastleFontFamily wrappers). }
  TCastleFontFamily = class(TCastleAbstractFont)
  strict private
    FRegularFont, FBoldFont, FItalicFont, FBoldItalicFont: TCastleAbstractFont;
    FBold, FItalic: boolean;
    FCustomizeOutline: boolean;
    procedure SetRegularFont(const Value: TCastleAbstractFont);
    procedure SetBoldFont(const Value: TCastleAbstractFont);
    procedure SetItalicFont(const Value: TCastleAbstractFont);
    procedure SetBoldItalicFont(const Value: TCastleAbstractFont);
    procedure SubFontCustomizeBegin;
    procedure SubFontCustomizeEnd;
  private
    function SubFont(const ABold, AItalic: boolean): TCastleAbstractFont;
    function SubFont: TCastleAbstractFont;
  strict protected
    procedure GLContextClose; override;
    procedure Measure(out ARowHeight, ARowHeightBase, ADescend: Single); override;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property RegularFont: TCastleAbstractFont read FRegularFont write SetRegularFont;
    property BoldFont: TCastleAbstractFont read FBoldFont write SetBoldFont;
    property ItalicFont: TCastleAbstractFont read FItalicFont write SetItalicFont;
    property BoldItalicFont: TCastleAbstractFont read FBoldItalicFont write SetBoldItalicFont;

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

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleFontFamily ------------------------------------------------------------ }

constructor TCastleFontFamily.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TCastleFontFamily.Destroy;
begin
  // this will free FXxxFont if needed
  RegularFont := nil;
  BoldFont := nil;
  ItalicFont := nil;
  BoldItalicFont := nil;
  inherited;
end;

procedure TCastleFontFamily.SetRegularFont(const Value: TCastleAbstractFont);
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

procedure TCastleFontFamily.SetBoldFont(const Value: TCastleAbstractFont);
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

procedure TCastleFontFamily.SetItalicFont(const Value: TCastleAbstractFont);
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

procedure TCastleFontFamily.SetBoldItalicFont(const Value: TCastleAbstractFont);
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

procedure TCastleFontFamily.Notification(AComponent: TComponent; Operation: TOperation);
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

procedure TCastleFontFamily.PrepareResources;
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

procedure TCastleFontFamily.GLContextClose;
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

procedure TCastleFontFamily.SubFontCustomizeBegin;
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

procedure TCastleFontFamily.SubFontCustomizeEnd;
begin
  if Size <> 0 then
    SubFont.PopProperties;
end;

procedure TCastleFontFamily.Print(const X, Y: Single; const Color: TCastleColor;
  const S: string);
begin
  SubFontCustomizeBegin;
  SubFont.Print(X, Y, Color, S);
  SubFontCustomizeEnd;
end;

function TCastleFontFamily.TextWidth(const S: string): Single;
begin
  SubFontCustomizeBegin;
  Result := SubFont.TextWidth(S);
  SubFontCustomizeEnd;
end;

function TCastleFontFamily.TextHeight(const S: string): Single;
begin
  SubFontCustomizeBegin;
  Result := SubFont.TextHeight(S);
  SubFontCustomizeEnd;
end;

function TCastleFontFamily.TextHeightBase(const S: string): Single;
begin
  SubFontCustomizeBegin;
  Result := SubFont.TextHeightBase(S);
  SubFontCustomizeEnd;
end;

function TCastleFontFamily.TextMove(const S: string): TVector2;
begin
  SubFontCustomizeBegin;
  Result := SubFont.TextMove(S);
  SubFontCustomizeEnd;
end;

function TCastleFontFamily.SubFont(const ABold, AItalic: boolean): TCastleAbstractFont;
begin
  if ABold and AItalic and (BoldItalicFont <> nil) then
    Result := BoldItalicFont else
  if ABold and (BoldFont <> nil) then
    Result := BoldFont else
  if AItalic and (ItalicFont <> nil) then
    Result := ItalicFont else
  if RegularFont <> nil then
    Result := RegularFont else
    raise Exception.Create('You must set at least RegularFont of TCastleFontFamily to use it for processing and rendering');
end;

function TCastleFontFamily.SubFont: TCastleAbstractFont;
begin
  Result := SubFont(Bold, Italic);
end;

function TCastleFontFamily.EffectiveSize: Single;
begin
  if Size <> 0 then
    Result := Size
  else
    Result := SubFont.EffectiveSize;
end;

procedure TCastleFontFamily.Measure(out ARowHeight, ARowHeightBase, ADescend: Single);
begin
  { See TCustomizedFont.Measure for explanation why we need to override
    Measure. }
  SubFontCustomizeBegin;
  SubFont.Measure(ARowHeight, ARowHeightBase, ADescend);
  SubFontCustomizeEnd;
end;

{$endif read_implementation}
