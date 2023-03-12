{%MainUnit castlefonts.pas}
{
  Copyright 2016-2023 Michalis Kamburelis.

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
  { Font family, with possible different subfonts for
    @link(Regular), @link(Bold), @link(Italic), @link(BoldItalic) variants.

    It can be generally used everywhere where we allow TCastleAbstractFont,
    and in most cases it will just act like the @link(Regular) variant.
    But it is especialy useful together with @link(TCastleLabel.Html) = @true.
    In such case, using it as @link(TCastleUserInterfaceFont.CustomFont TCastleLabel.CustomFont)
    will allow to render HTML text with varying font styles, this way bold/italic requested
    by HTML like @code(<b>something bold</b> <i>something italic</i>) will be visible.

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
    FInternalBold, FInternalItalic: boolean;
    FCustomizeOutline: boolean;
    procedure SetRegular(const Value: TCastleAbstractFont);
    procedure SetBold(const Value: TCastleAbstractFont);
    procedure SetItalic(const Value: TCastleAbstractFont);
    procedure SetBoldItalic(const Value: TCastleAbstractFont);
    procedure SubFontCustomizeBegin(const SF: TCastleAbstractFont);
    procedure SubFontCustomizeEnd(const SF: TCastleAbstractFont);
    procedure SubFontFontSizeChange(Sender: TObject);
  private
    { Font for given ABold/AItalic combination.
      Does what it can to fallback on Regular font
      (e.g. when you request ABold = AItalic = @true and BoldItalic is @nil)
      but returns @nil if even Regular font is @nil. }
    function SubFont(const ABold, AItalic: boolean): TCastleAbstractFont; overload;
    function SubFont: TCastleAbstractFont; overload;
  {$ifdef FPC}strict{$endif} protected
    procedure GLContextClose; override;
    procedure Measure(out AHeight, ACapHeight, ADescenderHeight: Single); override;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    function FontLoaded: Boolean; override;

    property InternalBold: boolean read FInternalBold write FInternalBold default false;
    property InternalItalic: boolean read FInternalItalic write FInternalItalic default false;

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

    {$ifdef FPC}
    property RegularFont: TCastleAbstractFont read FRegular write SetRegular; deprecated 'use Regular';
    property BoldFont: TCastleAbstractFont read FBold write SetBold; deprecated 'use Bold';
    property ItalicFont: TCastleAbstractFont read FItalic write SetItalic; deprecated 'use Italic';
    property BoldItalicFont: TCastleAbstractFont read FBoldItalic write SetBoldItalic; deprecated 'use BoldItalic';
    {$endif FPC}
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
  // detach free notifications
  Regular := nil;
  Bold := nil;
  Italic := nil;
  BoldItalic := nil;
  inherited;
end;

function TCastleFontFamily.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'Regular') or
     (PropertyName = 'Bold') or
     (PropertyName = 'Italic') or
     (PropertyName = 'BoldItalic') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleFontFamily.SetRegular(const Value: TCastleAbstractFont);
begin
  if FRegular <> Value then
  begin
    if FRegular <> nil then
    begin
      FRegular.RemoveFontSizeChangeNotification({$ifdef FPC}@{$endif}SubFontFontSizeChange);
      FRegular.RemoveFreeNotification(Self);
    end;
    FRegular := Value;
    if FRegular <> nil then
    begin
      FRegular.AddFontSizeChangeNotification({$ifdef FPC}@{$endif}SubFontFontSizeChange);
      FRegular.FreeNotification(Self);
    end;
  end;
end;

procedure TCastleFontFamily.SetBold(const Value: TCastleAbstractFont);
begin
  if FBold <> Value then
  begin
    if FBold <> nil then
    begin
      FBold.RemoveFontSizeChangeNotification({$ifdef FPC}@{$endif}SubFontFontSizeChange);
      FBold.RemoveFreeNotification(Self);
    end;
    FBold := Value;
    if FBold <> nil then
    begin
      FBold.AddFontSizeChangeNotification({$ifdef FPC}@{$endif}SubFontFontSizeChange);
      FBold.FreeNotification(Self);
    end;
  end;
end;

procedure TCastleFontFamily.SetItalic(const Value: TCastleAbstractFont);
begin
  if FItalic <> Value then
  begin
    if FItalic <> nil then
    begin
      FItalic.RemoveFontSizeChangeNotification({$ifdef FPC}@{$endif}SubFontFontSizeChange);
      FItalic.RemoveFreeNotification(Self);
    end;
    FItalic := Value;
    if FItalic <> nil then
    begin
      FItalic.AddFontSizeChangeNotification({$ifdef FPC}@{$endif}SubFontFontSizeChange);
      FItalic.FreeNotification(Self);
    end;
  end;
end;

procedure TCastleFontFamily.SetBoldItalic(const Value: TCastleAbstractFont);
begin
  if FBoldItalic <> Value then
  begin
    if FBoldItalic <> nil then
    begin
      FBoldItalic.RemoveFontSizeChangeNotification({$ifdef FPC}@{$endif}SubFontFontSizeChange);
      FBoldItalic.RemoveFreeNotification(Self);
    end;
    FBoldItalic := Value;
    if FBoldItalic <> nil then
    begin
      FBoldItalic.AddFontSizeChangeNotification({$ifdef FPC}@{$endif}SubFontFontSizeChange);
      FBoldItalic.FreeNotification(Self);
    end;
  end;
end;

procedure TCastleFontFamily.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  { set to nil by setter to detach notifications }
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

procedure TCastleFontFamily.SubFontCustomizeBegin(const SF: TCastleAbstractFont);
begin
  Assert(SF <> nil, 'TCastleFontFamily.SubFontCustomizeBegin should only be called with SF (from SubFont) <> nil');
  if (Size <> 0) or CustomizeOutline then
  begin
    SF.PushProperties;
    if Size <> 0 then
      SF.Size := Size;
    if CustomizeOutline then
    begin
      SF.Outline := Outline;
      SF.OutlineColor := OutlineColor;
      SF.OutlineHighQuality := OutlineHighQuality;
    end;
  end;
end;

procedure TCastleFontFamily.SubFontCustomizeEnd(const SF: TCastleAbstractFont);
begin
  Assert(SF <> nil, 'TCastleFontFamily.SubFontCustomizeEnd should only be called with SF (from SubFont) <> nil');
  if Size <> 0 then
    SF.PopProperties;
end;

procedure TCastleFontFamily.Print(const X, Y: Single; const Color: TCastleColor;
  const S: string);
var
  SF: TCastleAbstractFont;
begin
  SF := SubFont;
  if SF = nil then
    Exit; // subfonts not set yet
  SubFontCustomizeBegin(SF);
  SF.Print(X, Y, Color, S);
  SubFontCustomizeEnd(SF);
end;

function TCastleFontFamily.TextWidth(const S: string): Single;
var
  SF: TCastleAbstractFont;
begin
  SF := SubFont;
  if SF = nil then
    Exit(0); // subfonts not set yet
  SubFontCustomizeBegin(SF);
  Result := SF.TextWidth(S);
  SubFontCustomizeEnd(SF);
end;

function TCastleFontFamily.TextHeight(const S: string): Single;
var
  SF: TCastleAbstractFont;
begin
  SF := SubFont;
  if SF = nil then
    Exit(0); // subfonts not set yet
  SubFontCustomizeBegin(SF);
  Result := SF.TextHeight(S);
  SubFontCustomizeEnd(SF);
end;

function TCastleFontFamily.TextHeightBase(const S: string): Single;
var
  SF: TCastleAbstractFont;
begin
  SF := SubFont;
  if SF = nil then
    Exit(0); // subfonts not set yet
  SubFontCustomizeBegin(SF);
  Result := SF.TextHeightBase(S);
  SubFontCustomizeEnd(SF);
end;

function TCastleFontFamily.TextMove(const S: string): TVector2;
var
  SF: TCastleAbstractFont;
begin
  SF := SubFont;
  if SF = nil then
    Exit(TVector2.Zero); // subfonts not set yet
  SubFontCustomizeBegin(SF);
  Result := SF.TextMove(S);
  SubFontCustomizeEnd(SF);
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
    Result := nil;
    // We have to gracefully accept unset font, it is a possible case esp. in editor but also at runtime
    //raise Exception.Create('You must set at least Regular of TCastleFontFamily to use it for processing and rendering');
end;

function TCastleFontFamily.SubFont: TCastleAbstractFont;
begin
  Result := SubFont(InternalBold, InternalItalic);
end;

function TCastleFontFamily.EffectiveSize: Single;
var
  SF: TCastleAbstractFont;
begin
  if Size <> 0 then
    Result := Size
  else
  begin
    SF := SubFont;
    if SF = nil then
      Exit(0); // subfonts not set yet
    Result := SF.EffectiveSize;
  end;
end;

procedure TCastleFontFamily.Measure(out AHeight, ACapHeight, ADescenderHeight: Single);
var
  SF: TCastleAbstractFont;
begin
  SF := SubFont;
  if SF = nil then
  begin
    AHeight := 0;
    ACapHeight := 0;
    ADescenderHeight := 0;
    Exit; // subfonts not set yet
  end;

  { See TCustomizedFont.Measure for explanation why we need to override
    Measure. }
  SubFontCustomizeBegin(SF);
  SF.Measure(AHeight, ACapHeight, ADescenderHeight);
  SubFontCustomizeEnd(SF);
end;

procedure TCastleFontFamily.SubFontFontSizeChange(Sender: TObject);
begin
  FontSizesChanged; // invalidate our MakeMeasure calculations
end;

function TCastleFontFamily.FontLoaded: Boolean;
begin
  Result := (Regular <> nil) and Regular.FontLoaded;
end;

{$endif read_implementation}
