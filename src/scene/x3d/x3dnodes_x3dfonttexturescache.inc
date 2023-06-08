{%MainUnit x3dnodes.pas}
{
  Copyright 2002-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  TPixelTextureNode = class;

  { Cache for font texture nodes (TPixelTextureNode that we need for each font).
    Descends from TX3DNodesCache, so can also cache textures, movies, any X3D nodes. }
  TX3DFontTexturesCache = class(TX3DNodesCache)
  strict private
    type
      { @exclude Internal for TX3DFontTexturesCache. }
      TCachedFontTexture = class
      private
        Font: TCastleFont;
        Blending: Boolean;
        References: Cardinal;
        FontTexture: TPixelTextureNode;
      end;

      TCachedFontTextureList = {$ifdef FPC}specialize{$endif} TObjectList<TCachedFontTexture>;

    var
      CachedFontTextures: TCachedFontTextureList;
  public
    constructor Create;
    destructor Destroy; override;

    { Return TPixelTextureNode to draw given font configuration. }
    function LoadFontTexture(const Font: TCastleFont;
      const Blending: boolean): TPixelTextureNode;

    procedure FreeFontTexture(var FontTexture: TPixelTextureNode);

    function Empty: boolean; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TX3DFontTexturesCache ------------------------------------------------------------ }

constructor TX3DFontTexturesCache.Create;
begin
  inherited;
  CachedFontTextures := TCachedFontTextureList.Create;
end;

destructor TX3DFontTexturesCache.Destroy;
begin
  if CachedFontTextures <> nil then
  begin
    Assert(CachedFontTextures.Count = 0, ' Some references to font textures still exist when freeing TX3DFontTexturesCache');
    FreeAndNil(CachedFontTextures);
  end;
  inherited;
end;

function TX3DFontTexturesCache.LoadFontTexture(const Font: TCastleFont;
  const Blending: boolean): TPixelTextureNode;
var
  I: Integer;
  C: TCachedFontTexture;
  Image: TGrayscaleImage;
begin
  {$warnings off} // using FontData, will be internal in the future
  if Font.FontData = nil then
    raise Exception.Create('Only loaded fonts can be used with TX3DFontTexturesCache.LoadFontTexture');
  Image := Font.FontData.Image;
  {$warnings on}

  for I := 0 to CachedFontTextures.Count - 1 do
  begin
    C := CachedFontTextures[I];
    if (C.Font = Font) and
       (C.Blending = Blending) then
    begin
      Inc(C.References);

      if Log then
        WritelnLog('++', 'Font texture %s, blending %s : %d', [
          URIDisplay(Image.URL),
          BoolToStr(Blending, true),
          C.References
        ]);

      Exit(C.FontTexture);
    end;
  end;

  Result := TPixelTextureNode.Create;
  Result.KeepExistingBegin; // never free it using ref-counting
  { although repeat=TRUE seems counter-intuitive, in fact:
    - We *can* use it, since CastleTextureFontData surrounds each glyph from
      the right and top with GlyphPadding, so we will not accidentally pick
      other glyph by slightly going outside the texture area.
    - We *want* to use it actually, for EXTRA_GLYPH_SPACE. This way glyphs
      on the very left and bottom border of the font texture will also
      have a transparent pixel row to the left / bottom. }
  Result.RepeatS := true;
  Result.RepeatT := true;
  { convert grayscale image with TreatAsAlpha to TGrayscaleAlphaImage }
  Result.FdImage.Value := TGrayscaleAlphaImage.Create;
  Result.FdImage.Value.Assign(Image);
  if Blending then
    Result.AlphaChannel := acBlending
  else
    Result.AlphaChannel := acTest;

  C := TCachedFontTexture.Create;
  CachedFontTextures.Add(C);
  C.References := 1;
  C.Font := Font;
  C.Blending := Blending;
  C.FontTexture := Result;

  if Log then
    WritelnLog('++', 'Font texture %s, blending %s : %d', [
      URIDisplay(Image.URL),
      BoolToStr(Blending, true),
      1
    ]);
end;

procedure TX3DFontTexturesCache.FreeFontTexture(var FontTexture: TPixelTextureNode);
var
  I: Integer;
  C: TCachedFontTexture;
begin
  if FontTexture = nil then Exit;

  for I := 0 to CachedFontTextures.Count - 1 do
  begin
    C := CachedFontTextures[I];
    if C.FontTexture = FontTexture then
    begin
      if Log then
        {$warnings off} // using FontData, will be internal in the future
        WritelnLog('--', 'Font texture %s, blending %s : %d', [
          URIDisplay(C.Font.FontData.Image.URL),
          BoolToStr(C.Blending, true),
          C.References - 1
        ]);
        {$warnings on}

      FontTexture := nil;

      if C.References = 1 then
      begin
        { Simple FreeAndNil would also be OK, as this node should never
          be a child of another node in X3D graph.
          (Being a TTextNode.FontTextureNode value doesn't make it a normal child,
          i.e. TX3DNode internal child-parent references are not set.)
          But for safety we use "nice way" with KeepExistingEnd + FreeIfUnusedAndNil. }
        C.FontTexture.KeepExistingEnd;
        FreeIfUnusedAndNil(C.FontTexture);

        CachedFontTextures.Delete(I);
        CheckEmpty;
      end else
        Dec(C.References);

      Exit;
    end;
  end;

  raise EInternalError.CreateFmt('FreeFontTexture: no reference found for font texture %s',
    [PointerToStr(FontTexture)]);
end;

function TX3DFontTexturesCache.Empty: boolean;
begin
  Result := (inherited Empty) and (CachedFontTextures.Count = 0);
end;

{$endif read_implementation}
