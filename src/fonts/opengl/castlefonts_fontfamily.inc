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
    to render HTML text with varying font styles.
    Or you can use this for normal font rendering (that doesn't change
    font style in the middle, e.g. TCastleLabel with TCastleLabel.Html = @false)
    and just enjoy simple Boolean properties
    @link(DefaultBold) and @link(DefaultItalic) to change the font style yourself at runtime or design-time.

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
    FRegular, FBold, FItalic, FBoldItalic: TCastleAbstractFont;
    FDefaultBold, FDefaultItalic: boolean;
    FCustomizeOutline: boolean;
    procedure SetRegular(const Value: TCastleAbstractFont);
    procedure SetBold(const Value: TCastleAbstractFont);
    procedure SetItalic(const Value: TCastleAbstractFont);
    procedure SetBoldItalic(const Value: TCastleAbstractFont);
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

    property DefaultBold: boolean read FDefaultBold write FDefaultBold default false;
    property DefaultItalic: boolean read FDefaultItalic write FDefaultItalic default false;

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

    property RegularFont: TCastleAbstractFont read FRegular write SetRegular; deprecated 'use Regular';
    property BoldFont: TCastleAbstractFont read FBold write SetBold; deprecated 'use Bold';
    property ItalicFont: TCastleAbstractFont read FItalic write SetItalic; deprecated 'use Italic';
    property BoldItalicFont: TCastleAbstractFont read FBoldItalic write SetBoldItalic; deprecated 'use BoldItalic';
  published
    property Regular: TCastleAbstractFont read FRegular write SetRegular;
    property Bold: TCastleAbstractFont read FBold write SetBold;
    property Italic: TCastleAbstractFont read FItalic write SetItalic;
    property BoldItalic: TCastleAbstractFont read FBoldItalic write SetBoldItalic;
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
  Regular := nil;
  Bold := nil;
  Italic := nil;
  BoldItalic := nil;
  inherited;
end;

procedure TCastleFontFamily.SetRegular(const Value: TCastleAbstractFont);
begin
  if FRegular <> Value then
  begin
    if FRegular <> nil then
      FRegular.RemoveFreeNotification(Self);
    FRegular := Value;
    if FRegular <> nil then
      FRegular.FreeNotification(Self);
  end;
end;

procedure TCastleFontFamily.SetBold(const Value: TCastleAbstractFont);
begin
  if FBold <> Value then
  begin
    if FBold <> nil then
      FBold.RemoveFreeNotification(Self);
    FBold := Value;
    if FBold <> nil then
      FBold.FreeNotification(Self);
  end;
end;

procedure TCastleFontFamily.SetItalic(const Value: TCastleAbstractFont);
begin
  if FItalic <> Value then
  begin
    if FItalic <> nil then
      FItalic.RemoveFreeNotification(Self);
    FItalic := Value;
    if FItalic <> nil then
      FItalic.FreeNotification(Self);
  end;
end;

procedure TCastleFontFamily.SetBoldItalic(const Value: TCastleAbstractFont);
begin
  if FBoldItalic <> Value then
  begin
    if FBoldItalic <> nil then
      FBoldItalic.RemoveFreeNotification(Self);
    FBoldItalic := Value;
    if FBoldItalic <> nil then
      FBoldItalic.FreeNotification(Self);
  end;
end;

procedure TCastleFontFamily.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  { set to nil by SetXxxFont to clean nicely }
  if (Operation = opRemove) and (AComponent = FRegular) then
    Regular := nil;
  if (Operation = opRemove) and (AComponent = FBold) then
    Bold := nil;
  if (Operation = opRemove) and (AComponent = FItalic) then
    Italic := nil;
  if (Operation = opRemove) and (AComponent = FBoldItalic) then
    BoldItalic := nil;
end;

procedure TCastleFontFamily.PrepareResources;
begin
  if FRegular <> nil then
    FRegular.PrepareResources;
  if FBold <> nil then
    FBold.PrepareResources;
  if FItalic <> nil then
    FItalic.PrepareResources;
  if FBoldItalic <> nil then
    FBoldItalic.PrepareResources;
end;

procedure TCastleFontFamily.GLContextClose;
begin
  if FRegular <> nil then
    FRegular.GLContextClose;
  if FBold <> nil then
    FBold.GLContextClose;
  if FItalic <> nil then
    FItalic.GLContextClose;
  if FBoldItalic <> nil then
    FBoldItalic.GLContextClose;
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
  if ABold and AItalic and (BoldItalic <> nil) then
    Result := BoldItalic
  else
  if ABold and (Bold <> nil) then
    Result := Bold
  else
  if AItalic and (Italic <> nil) then
    Result := Italic
  else
  if Regular <> nil then
    Result := Regular
  else
    raise Exception.Create('You must set at least Regular of TCastleFontFamily to use it for processing and rendering');
end;

function TCastleFontFamily.SubFont: TCastleAbstractFont;
begin
  Result := SubFont(DefaultBold, DefaultItalic);
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
