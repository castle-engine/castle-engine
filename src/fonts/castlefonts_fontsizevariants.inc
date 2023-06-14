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
  { Font that uses @italic(another) TCastleAbstractFont for rendering and sizing,
    but modifies the underlying font size.
    Simply set the @code(Size) property of this instance to non-zero
    to force the specific size.

    The underlying font properties remain unchanged
    (so it can be still used for other purposes,
    directly or by other TCustomizedFont wrappers). }
  TCustomizedFont = class(TCastleAbstractFont)
  strict private
    FSourceFont: TCastleAbstractFont;
    FAlternativeSizes: TComponentList;
    { Font for current Size, from BestSourceFont.
      May be @nil if no font alternative yet loaded. }
    SubFont: TCastleAbstractFont;
    { Set SubFont to one of SourceFont of FAlternativeSizes,
      depending on the current Size. }
    procedure UpdateSubFont;
    procedure SubFontCustomizeBegin;
    procedure SubFontCustomizeEnd;
    procedure SetSourceFont(const Value: TCastleAbstractFont);
    procedure SubFontFontSizeChange(Sender: TObject);
  strict protected
    procedure Measure(out AHeight, ACapHeight, ADescenderHeight: Single); override;
    procedure GLContextClose; override;
    procedure SetSize(const Value: Single); override;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property SourceFont: TCastleAbstractFont read FSourceFont write SetSourceFont;

    procedure PrepareResources; override;
    procedure Print(const X, Y: Single; const Color: TCastleColor;
      const S: string); override;
    function TextWidth(const S: string): Single; override;
    function TextHeight(const S: string): Single; override;
    function TextHeightBase(const S: string): Single; override;
    function TextMove(const S: string): TVector2; override;
    function EffectiveSize: Single; override;
    function FontLoaded: Boolean; override;

    { Add any number of alternative source fonts.
      Before actually using them for rendering,
      we always choose the one with size most matching our desired @link(Size).
      This way you can e.g. load the same font in sizes 10, 50, 100,
      and have good quality font rendering in various sizes. }
    procedure AddAlternativeSourceFont(const ASourceFont: TCastleAbstractFont);

    { Load the same font to a number of textures with different sizes.
      At rendering, we will automatically use the best size.
      This sets @link(SourceFont) and @link(AddAlternativeSourceFont).

      This allows to achieve better look than TTexturedFont with one size. }
    procedure Load(const URL: string;
      const ASizes: array of Integer; const AnAntiAliased: boolean;
      const ACharacters: TUnicodeCharList = nil);

    { Return SourceFont or one of the fonts added by @link(AddAlternativeSourceFont),
      to have the font with @link(TCastleAbstractFont.Size) closest to the given ASize. }
    function BestSourceFont(const ASize: Single): TCastleAbstractFont;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCustomizedFont ------------------------------------------------------------ }

constructor TCustomizedFont.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TCustomizedFont.Destroy;
begin
  SourceFont := nil; // this will free FSourceFont if needed
  FreeAndNil(FAlternativeSizes);
  inherited;
end;

procedure TCustomizedFont.SubFontFontSizeChange(Sender: TObject);
begin
  FontSizesChanged; // invalidate our MakeMeasure calculations
end;

procedure TCustomizedFont.SetSourceFont(const Value: TCastleAbstractFont);
begin
  if FSourceFont <> Value then
  begin
    if FSourceFont <> nil then
    begin
      FSourceFont.RemoveFontSizeChangeNotification({$ifdef FPC}@{$endif}SubFontFontSizeChange);
      FSourceFont.RemoveFreeNotification(Self);
    end;
    FSourceFont := Value;
    if FSourceFont <> nil then
    begin
      FSourceFont.AddFontSizeChangeNotification({$ifdef FPC}@{$endif}SubFontFontSizeChange);
      FSourceFont.FreeNotification(Self);
    end;

    { Recalculate SubFont, since fonts changed.

      TODO: This will not fire when a font on FAlternativeSizes list is freed,
      SubFont may be left pointing to one of freed fonts.

      TODO: Also SubFontFontSizeChange is not fired when something on FAlternativeSizes
      changes size. }
    UpdateSubFont;
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
var
  I: Integer;
begin
  if FSourceFont <> nil then
    FSourceFont.PrepareResources;

  if FAlternativeSizes <> nil then
  begin
    for I := 0 to FAlternativeSizes.Count - 1 do
      TCastleAbstractFont(FAlternativeSizes[I]).PrepareResources;
  end;
end;

procedure TCustomizedFont.GLContextClose;
var
  I: Integer;
begin
  if FSourceFont <> nil then
    FSourceFont.GLContextClose;

  if FAlternativeSizes <> nil then
  begin
    for I := 0 to FAlternativeSizes.Count - 1 do
      TCastleAbstractFont(FAlternativeSizes[I]).GLContextClose;
  end;
end;

procedure TCustomizedFont.SetSize(const Value: Single);
begin
  if Size <> Value then
  begin
    inherited SetSize(Value);
    UpdateSubFont;
  end;
end;

function TCustomizedFont.BestSourceFont(const ASize: Single): TCastleAbstractFont;
var
  SizeDist, NewSizeDist: Single;
  AltFont: TCastleAbstractFont;
  I: Integer;
begin
  Result := SourceFont;

  if (FAlternativeSizes <> nil) and (ASize <> 0) then
  begin
    { At destruction it can happen that SourceFont is nil,
      while FAlternativeSizes is non-nil. }
    if SourceFont <> nil then
      SizeDist := Abs(SourceFont.Size - ASize)
    else
      SizeDist := MaxSingle;

    for I := 0 to FAlternativeSizes.Count - 1 do
    begin
      AltFont := TCastleAbstractFont(FAlternativeSizes[I]);
      NewSizeDist := Abs(AltFont.Size - ASize);
      if NewSizeDist < SizeDist then
      begin
        Result := AltFont;
        SizeDist := NewSizeDist;
      end;
    end;
  end;

  // Writeln('for size ', Size:1:2, ' using font with size ', Result.Size:1:2);
end;

procedure TCustomizedFont.UpdateSubFont;
begin
  SubFont := BestSourceFont(Size);
end;

procedure TCustomizedFont.SubFontCustomizeBegin;
begin
  Assert(SubFont <> nil, 'TCustomizedFont.SubFontCustomizeBegin should only be called when SubFont <> nil');
  if Size <> 0 then
  begin
    SubFont.PushProperties;
    SubFont.Size := Size;
  end;
end;

procedure TCustomizedFont.SubFontCustomizeEnd;
begin
  Assert(SubFont <> nil, 'TCustomizedFont.SubFontCustomizeEnd should only be called when SubFont <> nil');
  if Size <> 0 then
    SubFont.PopProperties;
end;

procedure TCustomizedFont.Print(const X, Y: Single; const Color: TCastleColor;
  const S: string);
begin
  if SubFont = nil then
    Exit; // subfonts not set yet
  SubFontCustomizeBegin;
  SubFont.Print(X, Y, Color, S);
  SubFontCustomizeEnd;
end;

function TCustomizedFont.TextWidth(const S: string): Single;
begin
  { One may think that only for rendering (Print) we have to use SubFont,
    for sizing it's enough to use SourceFont (since all alternatives should
    be the same font, just scaled).

    In practice it's important to use best font (always SubFont),
    since the underlying fonts data have integer sizes,
    so taking sizes of a different font always introduces some imprecision,
    this is visible by slightly misaligned labels etc. }

  if SubFont = nil then
    Exit(0); // subfonts not set yet
  SubFontCustomizeBegin;
  Result := SubFont.TextWidth(S);
  SubFontCustomizeEnd;
end;

function TCustomizedFont.TextHeight(const S: string): Single;
begin
  if SubFont = nil then
    Exit(0); // subfonts not set yet
  SubFontCustomizeBegin;
  Result := SubFont.TextHeight(S);
  SubFontCustomizeEnd;
end;

function TCustomizedFont.TextHeightBase(const S: string): Single;
begin
  if SubFont = nil then
    Exit(0); // subfonts not set yet
  SubFontCustomizeBegin;
  Result := SubFont.TextHeightBase(S);
  SubFontCustomizeEnd;
end;

function TCustomizedFont.TextMove(const S: string): TVector2;
begin
  if SubFont = nil then
    Exit(TVector2.Zero); // subfonts not set yet
  SubFontCustomizeBegin;
  Result := SubFont.TextMove(S);
  SubFontCustomizeEnd;
end;

function TCustomizedFont.EffectiveSize: Single;
begin
  if Size <> 0 then
    Result := Size
  else
  begin
    if SubFont = nil then
      Exit(0); // subfonts not set yet
    Result := SourceFont.EffectiveSize;
  end;
end;

procedure TCustomizedFont.Measure(out AHeight, ACapHeight, ADescenderHeight: Single);
begin
  { In usual circumstances, overriding Measure in TCustomizedFont is not needed.
    The default implementation of ancestor would work OK,
    by calling TextHeight to measure "Wy" for Height,
    and TCustomizedFont.TextHeight refers to FSourceFont.TextHeight already.

    But if the FSourceFont has some custom override for Measure,
    like TLargeDigitsFont.Measure in testcastlefonts.pas
    that returns constant instead of measuring "Wy" height for Height
    (because TLargeDigitsFont doesn't have "Wy" letters)
    then it would be ignored.
    This would cause problems when wrapping a font in TCustomizedFont
    (which is what e.g. TCastleLabel does when it has some size),
    wrapping TLargeDigitsFont in TCustomizedFont would ignore the
    TLargeDigitsFont.Measure override.

    So instead we implement our Measure by calling FSourceFont.Measure. }

  if SubFont = nil then
  begin
    AHeight := 0;
    ACapHeight := 0;
    ADescenderHeight := 0;
    Exit; // subfonts not set yet
  end;

  SubFontCustomizeBegin;
  SubFont.Measure(AHeight, ACapHeight, ADescenderHeight);
  SubFontCustomizeEnd;
end;

procedure TCustomizedFont.AddAlternativeSourceFont(const ASourceFont: TCastleAbstractFont);
begin
  if FAlternativeSizes = nil then
    FAlternativeSizes := TComponentList.Create(false);
  FAlternativeSizes.Add(ASourceFont);
  UpdateSubFont;
end;

procedure TCustomizedFont.Load(const URL: string;
  const ASizes: array of Integer; const AnAntiAliased: boolean;
  const ACharacters: TUnicodeCharList);
var
  F: TCastleFont;
  I: Integer;
begin
  Assert(Length(ASizes) > 0);

  // clear previous
  SourceFont := nil;
  if FAlternativeSizes <> nil then
    FAlternativeSizes.Clear;

  for I := 0 to Length(ASizes) - 1 do
  begin
    F := TCastleFont.Create(Self);
    {$warnings off}
    { Using deprecated, as we have to better alternative to pass ACharacters: TUnicodeCharList.
      In the future TCustomizedFont may be deprecated, we need to have alternative sizes
      at TCastleFont probably. }
    F.Load(URL, ASizes[I], AnAntiAliased, ACharacters);
    {$warnings on}
    if SourceFont = nil then
      SourceFont := F
    else
      AddAlternativeSourceFont(F);
  end;
end;

function TCustomizedFont.FontLoaded: Boolean;
begin
  Result := (SourceFont <> nil) and SourceFont.FontLoaded;
end;

{$endif read_implementation}
