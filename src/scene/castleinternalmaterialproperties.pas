{
  Copyright 2007-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Material and auto-generated texture properties from material_properties.xml }
unit CastleInternalMaterialProperties;

{$I castleconf.inc}

interface

uses Classes, DOM, Generics.Collections,
  CastleUtils, CastleClassUtils, {$ifndef CASTLE_STRICT_CLI} CastleSoundEngine, {$endif}
  CastleStringUtils, CastleImages, CastleFindFiles, CastleInternalAutoGenerated;

type
  TCompressionsMap = {$ifdef FPC}specialize{$endif} TDictionary<TTextureCompression, TCastlePlatforms>;

  TTextureCompressionsToGenerate = class
    { Compressions.Keys are the compressed formats to generate.
      For each compression format, we also specify the platforms on
      which is should be distributed and used. }
    Compressions: TCompressionsMap;
    { In addition to Compressions.Keys,
      generate also the most suitable variant of DXTn compression. }
    DxtAutoDetect: Boolean;
    DxtAutoDetectPlatforms: TCastlePlatforms;
    constructor Create;
    destructor Destroy; override;
    { HasSomeCompression means that some compression is indicated
      (DxtAutoDetect is true, or Compressions are not empty). }
    //function HasSomeCompression: Boolean; // unused
  end;

  { How to scale texture. }
  TScale = record
    { The scale (as in ScaledWidth := Width / (2 ^ Scale.Value).
      Should be intepreted just like TextureLoadingScale.
      See https://castle-engine.io/creating_data_auto_generated_textures.php#section_texture_scale }
    Value: Byte;
    { List of platforms for which this scale should be packaged. }
    Platforms: TCastlePlatforms;
  end;
  TScalesList = {$ifdef FPC}specialize{$endif} TList<TScale>;

  TAutoGeneratedTextures = class
  strict private
    const
      PathsIgnoreCase = true;
    var
    FAutoProcessImageUrls: Boolean;
    IncludePaths: TCastleStringList; // absolute URLs
    IncludePathsRecursive: TBooleanList;
    ExcludePaths: TCastleStringList;
    { necessary for Exclude with relative dirs, like "entites/*", to work }
    FBaseUrl: String;
    FCompressedFormatsToGenerate: TTextureCompressionsToGenerate;
    GatheringResult: TCastleStringList;
    FScales: TScalesList;
    FPreferredOutputFormat: String;
    FTrivialUncompressedConvert: Boolean;
    FMipmapsLevel: Cardinal;
    FAutoGeneratedAtLoad: TAutoGenerated;
    procedure GatherCallback(const FileInfo: TFileInfo; var StopSearch: Boolean);
    procedure LoadImageEvent(var Url: String);
    function IsAbsoluteUrlMatchingRelativeMask(const Url, Mask: String): Boolean;
  private
    { Largest Scale.Value for this texture. }
    function LargestScaleValue: Byte;
    function TextureUrlMatches(const Url: String): Boolean;
    function AutoGeneratedTextures: TCastleStringList;
    function CompressedFormatsGenerated: TCompressionsMap;
    property PreferredOutputFormat: String read FPreferredOutputFormat;
  public
    constructor Create(const Element: TDOMElement; const BaseUrl: String;
      const AnAutoProcessImageUrls: Boolean; const AnAutoGeneratedAtLoad: TAutoGenerated);
    destructor Destroy; override;

    { For given texture (absolute) URL and compression and scaling,
      return the proper (absolute) URL of auto-compressed and auto-downscaled counterpart.
      Scaling is defined just like TextureLoadingScale. }
    function GeneratedTextureUrl(const Url: String;
      const UseCompression: Boolean; const TextureCompression: TTextureCompression;
      const Scaling: Cardinal): String;

    { Automatic compression formats generated for this texture.
      May return @nil, if there are no compressed formats to generate for this texture.
      The resulting class instance cannot be modified, and it is owned by this TMaterialProperties
      instance, so don't free it yourself. }
    property CompressedFormatsToGenerate: TTextureCompressionsToGenerate
      read FCompressedFormatsToGenerate;

    { Determine the platforms for which the original texture should be packaged }
    function OriginalPlatforms: TCastlePlatforms;

    { Which scales to generate for this texture. Read-only. }
    property Scales: TScalesList read FScales;

    { Perform trivial conversion (that does not compress, does not downscale)
      for this texture. }
    property TrivialUncompressedConvert: Boolean read FTrivialUncompressedConvert;

    property MipmapsLevel: Cardinal read FMipmapsLevel;
  end;

  TAutoGeneratedTexturesList = {$ifdef FPC}specialize{$endif} TObjectList<TAutoGeneratedTextures>;

  { Store information about auto-generated textures.
    See https://castle-engine.io/creating_data_auto_generated_textures.php .

    You have to load an XML file by setting
    @link(TMaterialProperties.Url MaterialProperties.Url) property.
  }
  TMaterialProperties = class
  strict private
    FAutoGeneratedTexturesList: TAutoGeneratedTexturesList;
    FUrl: String;
    FAutoProcessImageUrls: Boolean;
    { Information about available (already created) compressed/downscaled textures.
      Obtained from CastleAutoGenerated.xml at the moment of TMaterialProperties.Create
      (so, at application start in practice). }
    FAutoGeneratedAtLoad: TAutoGenerated;
    procedure SetUrl(const Value: String);
  public
    constructor Create(const AnAutoProcessImageUrls: Boolean);
    destructor Destroy; override;

    { Load material properties from given XML file.
      Set this to empty string to unload previously loaded properties.
      See Castle1 and fps_game data for examples how this looks like,
      in @code(material_properties.xml). }
    property Url: String read FUrl write SetUrl;

    { Get the Urls of all textures that should have automatically
      generated GPU-compressed and downscaled counterparts.
      Returns a list of absolute Urls.
      This actually searches on disk, right now, to find the texture list,
      applying the include/exclude rules specified in material_properties.xml file.

      This is to be used by "castle-engine auto-generate-textures"
      tool, or similar tools.

      The objects on this list refer to TAutoGeneratedTextures objects
      that define @italic(how) to process this texture.

      Caller is responsible for freeing the returned TCastleStringList list. }
    function AutoGeneratedTextures: TCastleStringList;
  end;

{ Material and texture properties, see @link(TMaterialProperties).
  Automatically loaded from castle-data:/material_properties.xml at OpenGL initialization. }
function MaterialProperties: TMaterialProperties;

implementation

uses SysUtils, XMLRead, StrUtils, Math,
  CastleXmlUtils, CastleFilesUtils, CastleApplicationProperties, CastleTextureImages,
  CastleUriUtils, CastleDownload, CastleLog;

const
  MaxScale = 32;

{ TTextureCompressionsToGenerate ----------------------------------------------------------- }

constructor TTextureCompressionsToGenerate.Create;
begin
  inherited;
  Compressions := TCompressionsMap.Create;
end;

destructor TTextureCompressionsToGenerate.Destroy;
begin
  FreeAndNil(Compressions);
  inherited;
end;

// function TTextureCompressionsToGenerate.HasSomeCompression: Boolean;
// begin
//   Result := (Compressions.Count <> 0) or DxtAutoDetect;
// end;

{ TAutoGeneratedTextures ----------------------------------------------------- }

constructor TAutoGeneratedTextures.Create(
  const Element: TDOMElement; const BaseUrl: String;
  const AnAutoProcessImageUrls: Boolean;
  const AnAutoGeneratedAtLoad: TAutoGenerated);

  function PlatformsOfTextureCompression(const Element: TDOMElement): TCastlePlatforms;
  var
    PlatformsElement: TDOMElement;
    I: TXMLElementIterator;
  begin
    PlatformsElement := Element.Child('platforms', false);
    if PlatformsElement <> nil then
    begin
      Result := [];
      I := PlatformsElement.ChildrenIterator('platform');
      try
        while I.GetNext do
          Include(Result, StrToPlatform(Trim(I.Current.TextData)));
      finally FreeAndNil(I) end;
    end else
      Result := AllPlatforms;
  end;

  function StrToScale(const AString: String): Cardinal;
  begin
    Result := StrToInt(AString);
    if Result < 1 then
      raise Exception.CreateFmt('Error reading scale value "%s", the scale must be >= 1.', [AString]);
    if Result > MaxScale then
      raise Exception.CreateFmt('Error reading scale value "%s", the scale must be <= %d.', [AString, MaxScale]);
  end;

var
  ChildElements: TXMLElementIterator;
  ChildElement, CompressElement, ScalesElement, PlatformsElement, PreferredOutputFormatElement, MipmapsElement: TDOMElement;
  TextureCompressionName: String;
  ASmallestScale: Cardinal;
  ScalesIterator, PlatformsIterator: TXMLElementIterator;
  Scale: TScale;
  I: Integer;
begin
  inherited Create;
  FScales := TScalesList.Create;
  FAutoProcessImageUrls := AnAutoProcessImageUrls;
  IncludePaths := TCastleStringList.Create;
  IncludePathsRecursive := TBooleanList.Create;
  ExcludePaths := TCastleStringList.Create;
  FCompressedFormatsToGenerate := TTextureCompressionsToGenerate.Create;
  FBaseUrl := BaseUrl;
  FAutoGeneratedAtLoad := AnAutoGeneratedAtLoad;

  { read from XML }

  ChildElements := Element.ChildrenIterator('include');
  try
    while ChildElements.GetNext do
    begin
      ChildElement := ChildElements.Current;
      IncludePaths.Add(ChildElement.AttributeUrl('path', BaseUrl));
      IncludePathsRecursive.Add(ChildElement.AttributeBooleanDef('recursive', false));
    end;
  finally FreeAndNil(ChildElements) end;

  ChildElements := Element.ChildrenIterator('exclude');
  try
    while ChildElements.GetNext do
    begin
      ChildElement := ChildElements.Current;
      ExcludePaths.Add(ChildElement.AttributeString('path'));
    end;
  finally FreeAndNil(ChildElements) end;

  // calculate FCompressedFormatsToGenerate
  CompressElement := Element.ChildElement('compress', false);
  if CompressElement <> nil then
  begin
    ChildElements := CompressElement.ChildrenIterator('format');
    try
      while ChildElements.GetNext do
      begin
        TextureCompressionName := ChildElements.Current.AttributeString('name');
        if LowerCase(TextureCompressionName) = 'dxt_autodetect' then
        begin
          FCompressedFormatsToGenerate.DxtAutoDetect := true;
          FCompressedFormatsToGenerate.DxtAutoDetectPlatforms := PlatformsOfTextureCompression(ChildElements.Current);
        end else
          FCompressedFormatsToGenerate.Compressions.Add(
            StringToTextureCompression(TextureCompressionName),
            PlatformsOfTextureCompression(ChildElements.Current)
          );
      end;
    finally FreeAndNil(ChildElements) end;
  end;

  ScalesElement := Element.ChildElement('scale', false);
  if ScalesElement <> nil then
  begin
    { Deprecated "scale" element }
    WriteLnWarning('AutoGeneratedTextures', 'MaterialProperties contains "scale" node which is deprecated, use "scales" instead and list all scales.');
    if ScalesElement.HasAttribute('smallest') then
    begin
      WriteLnWarning('AutoGeneratedTextures', 'MaterialProperties contains "smallest" attribute which is deprecated.');
      ASmallestScale := ScalesElement.AttributeCardinalDef('smallest', 1);
      if not Between(ASmallestScale, 1, MaxScale) then
        raise Exception.CreateFmt('Invalid scale smallest value "%d" (must be an integer number within 1..%d)', [ASmallestScale, MaxScale]);
      for I := 1 to ASmallestScale do
      begin
        Scale.Platforms := AllPlatforms;
        Scale.Value := I;
        FScales.Add(Scale);
      end;
    end;
  end;

  ScalesElement := Element.ChildElement('scales', false);
  if ScalesElement <> nil then
  begin
    ScalesIterator := ScalesElement.ChildrenIterator('scale');
    try
      while ScalesIterator.GetNext do
      begin
        Scale.Value := StrToScale(ScalesIterator.Current.AttributeString('value'));
        Scale.Platforms := AllPlatforms;

        PlatformsElement := ScalesIterator.Current.ChildElement('platforms', false);
        if PlatformsElement <> nil then
        begin
          Scale.Platforms := [];
          PlatformsIterator := PlatformsElement.ChildrenIterator('platform');
          try
            while PlatformsIterator.GetNext do
              Include(Scale.Platforms, StrToPlatform(Trim(PlatformsIterator.Current.TextData)));
          finally
            FreeAndNil(PlatformsIterator);
          end;
        end;

        FScales.Add(Scale);
      end;
    finally
      FreeAndNil(ScalesIterator);
    end;
  end;

  // no <scale> or <scales> in material_properties.xml file -> do not downscale
  if FScales.Count = 0 then
  begin
    Scale.Value := 1;
    Scale.Platforms := AllPlatforms;
    FScales.Add(Scale);
  end;

  PreferredOutputFormatElement := Element.ChildElement('preferred_output_format', false);
  if PreferredOutputFormatElement <> nil then
    FPreferredOutputFormat := PreferredOutputFormatElement.AttributeStringDef('extension', '')
  else
    FPreferredOutputFormat := '.png';

  FTrivialUncompressedConvert := Element.ChildElement('trivial_uncompressed_convert', false) <> nil;

  MipmapsElement := Element.ChildElement('mipmaps', false);
  if MipmapsElement <> nil then
  begin
    FMipmapsLevel := MipmapsElement.AttributeCardinalDef('level', 0);
    { Mipmaps level = 0 and 1 are equivalent.
      Make it easier and just always express them as 0 in rest of API. }
    if FMipmapsLevel = 1 then
      FMipmapsLevel := 0;
  end else
    FMipmapsLevel := 0;

  if FAutoProcessImageUrls then
    AddLoadImageListener({$ifdef FPC}@{$endif} LoadImageEvent);
end;

function TAutoGeneratedTextures.CompressedFormatsGenerated: TCompressionsMap;
begin
  { TODO: for now, the DxtAutoDetect texture will not be used at all.
    The actual value of CompressedFormatsGenerated
    should come from auto_generated.xml. }
  Result := FCompressedFormatsToGenerate.Compressions;
end;

destructor TAutoGeneratedTextures.Destroy;
begin
  FreeAndNil(FScales);
  FreeAndNil(IncludePaths);
  FreeAndNil(IncludePathsRecursive);
  FreeAndNil(ExcludePaths);
  FreeAndNil(FCompressedFormatsToGenerate);
  if FAutoProcessImageUrls then
    RemoveLoadImageListener({$ifdef FPC}@{$endif} LoadImageEvent);
  inherited;
end;

function TAutoGeneratedTextures.LargestScaleValue: Byte;
var
  Scale: TScale;
begin
  Result := 1;
  for Scale in FScales do
    if Scale.Value > Result then
      Result := Scale.Value;
end;

procedure TAutoGeneratedTextures.GatherCallback(const FileInfo: TFileInfo; var StopSearch: Boolean);
begin
  if (Pos('/' + TAutoGenerated.AutoGeneratedDirName + '/', FileInfo.Url) = 0) and
     IsImageMimeType(UriMimeType(FileInfo.Url), false, false) then
    GatheringResult.Add(FileInfo.Url);
end;

function TAutoGeneratedTextures.IsAbsoluteUrlMatchingRelativeMask(
  const Url, Mask: String): Boolean;
var
  U: String;
begin
  U := PrefixRemove(ExtractUriPath(FBaseUrl), Url, PathsIgnoreCase);
  Result := IsWild(U, Mask, PathsIgnoreCase);
end;

function TAutoGeneratedTextures.
  AutoGeneratedTextures: TCastleStringList;

  procedure Exclude(const ExcludePathMask: String; const Urls: TCastleStringList);
  var
    I: Integer;
  begin
    I := 0;
    while I < Urls.Count do
    begin
      // Writeln('Excluding ExcludePathMask ' + ExcludePathMask +
      //   ' from ' + PrefixRemove(ExtractUriPath(FBaseUrl), Urls[I], PathsIgnoreCase));
      if IsAbsoluteUrlMatchingRelativeMask(Urls[I], ExcludePathMask) then
        Urls.Delete(I) else
        Inc(I);
    end;
  end;

var
  I: Integer;
  FindOptions: TFindFilesOptions;
begin
  Result := TCastleStringList.Create;
  GatheringResult := Result;

  for I := 0 to IncludePaths.Count - 1 do
  begin
    if IncludePathsRecursive[I] then
      FindOptions := [ffRecursive] else
      { not recursive, so that e.g. <include path="my_texture.png" />
        or <include path="subdir/my_texture.png" />
        should not include *all* my_texture.png files inside. }
      FindOptions := [];
    FindFiles(IncludePaths[I], false, {$ifdef FPC}@{$endif} GatherCallback, FindOptions);
  end;

  GatheringResult := nil;

  for I := 0 to ExcludePaths.Count - 1 do
    Exclude(ExcludePaths[I], Result);
end;

function TAutoGeneratedTextures.TextureUrlMatches(const Url: String): Boolean;

  { Check is Url not excluded. }
  function CheckNotExcluded: Boolean;
  var
    I: Integer;
  begin
    for I := 0 to ExcludePaths.Count - 1 do
      if IsAbsoluteUrlMatchingRelativeMask(Url, ExcludePaths[I]) then
        Exit(false);
    Result := true;
  end;

var
  UrlName, UrlPath: String;
  I: Integer;
  IncludePath, IncludeMask: String;
  PathMatches: Boolean;
begin
  Result := false;
  UrlPath := ExtractUriPath(Url);
  UrlName := ExtractUriName(Url);
  for I := 0 to IncludePaths.Count - 1 do
  begin
    IncludePath := ExtractUriPath(IncludePaths[I]);
    IncludeMask := ExtractUriName(IncludePaths[I]);
    if IncludePathsRecursive[I] then
      PathMatches := IsPrefix(IncludePath, UrlPath, PathsIgnoreCase) else
      PathMatches := AnsiSameText(IncludePath, UrlPath); { assume PathsIgnoreCase=true }
    if PathMatches and IsWild(UrlName, IncludeMask, PathsIgnoreCase) then
    begin
      Result := CheckNotExcluded;
      Exit;
    end;
  end;
end;

procedure TAutoGeneratedTextures.LoadImageEvent(
  var Url: String);

  { Returns @true if the texture has square size.
    @false otherwise.
    If it's unknown, returns @false too.

    This uses CastleAutoGenerated.xml information to work without any file reading.
    If the Url is in our data, and it is in CastleAutoGenerated.xml,
    then we check it's size according to CastleAutoGenerated.xml. }
  function IsSquareTexture(const Url: String): Boolean;
  var
    Texture: TAutoGenerated.TTexture;
    WasInsideData: Boolean;
    UrlInData: String;
  begin
    UrlInData := RelativeToCastleDataUrl(Url, WasInsideData);
    if not WasInsideData then
      Exit(false);
    Texture := FAutoGeneratedAtLoad.Texture(UrlInData, [], false);
    Result := (Texture <> nil) and (Texture.Width = Texture.Height);
  end;

  { Texture has GPU-compressed and/or downscaled counterpart, according to include/exclude
    variables. So try to replace Url with something compressed and downscaled. }
  procedure ReplaceUrl;
  var
    C: TTextureCompression;
    Scale: Cardinal;
    CompressionPair: {$ifdef FPC}TCompressionsMap.TDictionaryPair{$else}
      TPair<TTextureCompression, TCastlePlatforms>{$endif};
  begin
    { Do not warn about it, just as we don't warn when TextureLoadingScale = 2
      but we're loading image not mentioned in <auto_generated_textures>.
    if TextureLoadingScale > SmallestScale then
      raise Exception.CreateFmt('Invalid TextureLoadingScale %d, we do not have such downscaled images. You should add or modify the <scale smallest=".." /> declaration in "material_properties.xml", and make sure thar you load the "material_properties.xml" early enough.',
        [TextureLoadingScale]); }
    Scale := Min(LargestScaleValue, TextureLoadingScale);

    if not SupportedTextureCompressionKnown then
      WritelnWarning('MaterialProperties', 'Cannot determine whether to use auto-generated (GPU compressed and/or downscaled) texture version for ' + Url + ' because the image is loaded before GPU capabilities are known')
    else
    begin
      {$ifdef CASTLE_IOS}
      if not IsSquareTexture(Url) then
      begin
        WritelnWarning('MaterialProperties', 'Not using GPU compressed (and potentially downscaled) version for ' + Url + ' because on iOS non-square compressed textures are not supported');
      end else
      {$endif}
      for CompressionPair in CompressedFormatsGenerated do
      begin
        C := CompressionPair.Key;
        if (C in SupportedTextureCompression) and (Platform in CompressionPair.Value) then
        begin
          Url := GeneratedTextureUrl(Url, true, C, Scale);
          WritelnLog('MaterialProperties', 'Using GPU compressed (and potentially downscaled) alternative ' + Url);
          Exit;
        end;
      end;
    end;

    { no GPU compression supported; still, maybe we should use a downscaled alternative }
    if (Scale <> 1) or TrivialUncompressedConvert then
    begin
      Url := GeneratedTextureUrl(Url, false, Low(TTextureCompression), Scale);
      WritelnLog('MaterialProperties', 'Using alternative ' + Url);
    end;
  end;

begin
  if TextureUrlMatches(Url) then
    ReplaceUrl;
end;

function TAutoGeneratedTextures.GeneratedTextureUrl(
  const Url: String;
  const UseCompression: Boolean; const TextureCompression: TTextureCompression;
  const Scaling: Cardinal): String;
begin
  Result := ExtractUriPath(Url) + TAutoGenerated.AutoGeneratedDirName + '/';
  if UseCompression then
    Result := Result + LowerCase(TextureCompressionToString(TextureCompression)) + '/'
  else
    Result := Result + 'uncompressed/';
  if Scaling <> 1 then
    Result := Result + 'downscaled_' + IntToStr(Scaling) + '/';
  Result := Result + ExtractUriName(Url);
  if UseCompression then
    Result := Result + TextureCompressionInfo[TextureCompression].FileExtension
  else
    Result := Result + PreferredOutputFormat;
end;

function TAutoGeneratedTextures.OriginalPlatforms: TCastlePlatforms;
var
  Scale: TScale;
begin
  Result := [];
  for Scale in Scales do
    if Scale.Value = 1 then
      Result := Result + Scale.Platforms;
end;

{ TMaterialProperties ---------------------------------------------------------- }

constructor TMaterialProperties.Create(const AnAutoProcessImageUrls: Boolean);
begin
  inherited Create;
  FAutoProcessImageUrls := AnAutoProcessImageUrls;
  FAutoGeneratedTexturesList := TAutoGeneratedTexturesList.Create({ owns objects } true);

  FAutoGeneratedAtLoad := TAutoGenerated.Create;
  FAutoGeneratedAtLoad.LoadFromFile('castle-data:/CastleAutoGenerated.xml');
end;

destructor TMaterialProperties.Destroy;
begin
  FreeAndNil(FAutoGeneratedAtLoad);
  FreeAndNil(FAutoGeneratedTexturesList);
  inherited;
end;

procedure TMaterialProperties.SetUrl(const Value: String);
var
  Config: TXMLDocument;
  Elements: TXMLElementIterator;
  Stream: TStream;
begin
  FUrl := Value;

  FAutoGeneratedTexturesList.Clear;

  if Url = '' then Exit;

  Stream := Download(Url);
  try
    ReadXMLFile(Config, Stream, Url);
  finally FreeAndNil(Stream) end;

  try
    Check(Config.DocumentElement.TagName = 'properties',
      'Root node of material properties file must be <properties>');

    Elements := Config.DocumentElement.ChildrenIterator('auto_generated_textures');
    try
      while Elements.GetNext do
      begin
        FAutoGeneratedTexturesList.Add(
          TAutoGeneratedTextures.Create(Elements.Current, Url, FAutoProcessImageUrls,
            FAutoGeneratedAtLoad));
      end;
    finally FreeAndNil(Elements); end;
  finally
    SysUtils.FreeAndNil(Config);
  end;
end;

function TMaterialProperties.AutoGeneratedTextures: TCastleStringList;
var
  S: TCastleStringList;
  I, J: Integer;
begin
  Result := TCastleStringList.Create;
  try
    for I := 0 to FAutoGeneratedTexturesList.Count - 1 do
    begin
      S := FAutoGeneratedTexturesList[I].AutoGeneratedTextures;
      try
        for J := 0 to S.Count - 1 do
        begin
          if Result.IndexOf(S[J]) <> -1 then
            WritelnWarning('MaterialProperties', Format('The texture URL "%s" is under the influence of more than one <auto_generated_textures> rule. Use <include> and <exclude> to avoid it',
              [S[J]]))
          else
            Result.AddObject(S[J], FAutoGeneratedTexturesList[I]);
        end;
      finally FreeAndNil(S) end;
    end;
  except FreeAndNil(Result); raise end;
end;

{ globals -------------------------------------------------------------------- }

var
  FMaterialProperties: TMaterialProperties;

function MaterialProperties: TMaterialProperties;
begin
  if FMaterialProperties = nil then
    FMaterialProperties := TMaterialProperties.Create(true);
  Result := FMaterialProperties;
end;

procedure LoadMaterialProperties;
begin
  if UriFileExists('castle-data:/material_properties.xml') then
  begin
    WritelnLog('Found and loading castle-data:/material_properties.xml');
    MaterialProperties.Url := 'castle-data:/material_properties.xml';
  end;
end;

initialization
  ApplicationProperties.OnGLContextEarlyOpen.Add({$ifdef FPC}@{$endif} LoadMaterialProperties);
finalization
  FreeAndNil(FMaterialProperties);
end.
