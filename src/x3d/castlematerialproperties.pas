{
  Copyright 2007-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Material and texture properties from external files (TMaterialProperty,
  global MaterialProperties collection). }
unit CastleMaterialProperties;

{$I castleconf.inc}

interface

uses Classes, DOM, FGL,
  CastleUtils, CastleClassUtils, CastleSoundEngine, CastleStringUtils,
  CastleImages, CastleFindFiles;

type
  { Information for a particular material. }
  TMaterialProperty = class
  strict private
    FTextureBaseName: string;
    FFootstepsSound: TSoundType;
    FToxic: boolean;
    FToxicDamageConst, FToxicDamageRandom, FToxicDamageTime: Single;
    FNormalMap: string;
    FAlphaChannel: string;
  private
    procedure LoadFromDOMElement(Element: TDOMElement; const BaseURL: string);
  public
    { Texture basename to associate this property will all appearances
      using given texture. For now, this is the only way to associate
      property, but more are possible in the future (like MaterialNodeName). }
    property TextureBaseName: string read FTextureBaseName write FTextureBaseName;

    { Footsteps sound to make when player is walking on this material.
      stNone is no information is available. }
    property FootstepsSound: TSoundType read FFootstepsSound write FFootstepsSound;

    { Is the floor toxic when walking on it.
      @groupBegin }
    property Toxic: boolean read FToxic write FToxic;
    property ToxicDamageConst: Single read FToxicDamageConst write FToxicDamageConst;
    property ToxicDamageRandom: Single read FToxicDamageRandom write FToxicDamageRandom;
    property ToxicDamageTime: Single read FToxicDamageTime write FToxicDamageTime;
    { @groupEnd }

    { Normal map texture URL. This is a simple method to activate bump mapping,
      equivalent to using normalMap field in an Appearance node of VRML/X3D, see
      http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_bump_mapping .

      In case both VRML/X3D Appearance specifies normalMap and we have
      NormalMap defined here, the VRML/X3D Appearance is used. }
    property NormalMap: string read FNormalMap write FNormalMap;

    { Override alpha channel type for diffuse texture.
      The meaning and allowed values for this are the same as for
      alphaChannel field for texture nodes, see
      http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_alpha_channel_detection .
      Empty value (default) doesn't change the alpha channel type
      (set in VRML/X3D or auto-detected). }
    property AlphaChannel: string read FAlphaChannel write FAlphaChannel;
  end;

  { Store information that is naturally associated with a given material
    or texture in an external file. Documentation and example of such
    file is on  http://castle-engine.sourceforge.net/creating_data_material_properties.php .
    Right now this allows to define things like:

    @unorderedList(
      @itemSpacing compact
      @item footsteps,
      @item toxic ground (hurts player),
      @item bump mapping (normal maps and height maps for given texture),
      @item texture GPU-compressed alternatives.
    )

    In the future, it should be possible to express all these properties
    in pure VRML/X3D (inside Appearance / Material / ImageTexture nodes).
    Right now, you can do this with bump mapping, see
    http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_bump_mapping ,
    but not footsteps or toxic ground.
    In the future it should also be possible to express these properties
    in 3D authoring software (like Blender), and easily export them
    to appropriate VRML/X3D nodes.
    For now, this TMaterialProperty allows us to easily customize materials
    in a way that is not possible in Blender.

    Using an external file for material properties has also long-term
    advantages: it can be shared across many 3D models, for example
    you can define footsteps sound for all grounds using the @code(grass.png)
    textures, in all levels, at once.

    You have to load an XML file by setting
    @link(TMaterialProperties.URL MaterialProperties.URL) property.
  }
  TMaterialProperties = class(specialize TFPGObjectList<TMaterialProperty>)
  strict private
    type
      TAutoCompressedTextures = class
      strict private
        const
          PathsIgnoreCase = true;
        var
        FAutoProcessImageURLs: boolean;
        IncludePaths: TCastleStringList; // absolute URLs
        IncludePathsRecursive: TBooleanList;
        ExcludePaths: TCastleStringList;
        { necessary for Exclude with relative dirs, like "entites/*", to work }
        FBaseURL: string;
        FFormats: TTextureCompressions;
        GatheringResult: TCastleStringList;
        procedure GatherCallback(const FileInfo: TFileInfo; var StopSearch: boolean);
        procedure LoadImageEvent(var URL: string);
        function IsAbsoluteURLMatchingRelativeMask(const URL, Mask: string): boolean;
      public
        constructor Create(const Element: TDOMElement; const BaseURL: string; const AnAutoProcessImageURLs: boolean);
        destructor Destroy; override;
        function AutoCompressedTextures: TCastleStringList;
        class function CompressedTextureURL(const URL: string;
          const TextureCompression: TTextureCompression): string;
        property Formats: TTextureCompressions read FFormats;
      end;
    var
    FAutoCompressedTextures: TAutoCompressedTextures;
    FURL: string;
    FAutoProcessImageURLs: boolean;
    procedure SetURL(const Value: string);
  public
    const
      AutoCompressedDirName = 'auto_compressed';

    constructor Create(const AnAutoProcessImageURLs: boolean);
    destructor Destroy; override;

    { Load material properties from given XML file.
      Set this to empty string to unload previously loaded properties.
      See Castle1 and fps_game data for examples how this looks like,
      in @code(material_properties.xml). }
    property URL: string read FURL write SetURL;

    property FileName: string read FURL write SetURL; deprecated 'use URL';

    { Find material properties for given texture basename.
      Returns @nil if no material properties are found
      (in particular, if @link(URL) was not set yet). }
    function FindTextureBaseName(const TextureBaseName: string): TMaterialProperty;

    { Get the URLs of all textures that should have automatically
      generated GPU-compressed counterparts.
      Returns a list of absolute URLs.

      This is to be used by "castle-engine auto-compress-textures"
      tool, or similar tools.

      Caller is responsible for freeing the returned TCastleStringList list. }
    function AutoCompressedTextures: TCastleStringList;

    { For given texture (absolute) URL and compression,
      return the proper URL of auto-compressed counterpart. }
    class function AutoCompressedTextureURL(const TextureURL: string;
      const TextureCompression: TTextureCompression): string;

    { Automatic formats used for all AutoCompressedTextures. }
    function AutoCompressedTextureFormats: TTextureCompressions;
  end;

{ Material and texture properties, see @link(TMaterialProperties).
  Set the @link(TMaterialProperties.URL URL) property
  to load material properties from XML file. }
function MaterialProperties: TMaterialProperties;

implementation

uses SysUtils, XMLRead, StrUtils,
  CastleXMLUtils, CastleFilesUtils, X3DNodes, CastleGLUtils,
  CastleURIUtils, CastleDownload, CastleWarnings, CastleLog;

{ TMaterialProperty --------------------------------------------------------- }

procedure TMaterialProperty.LoadFromDOMElement(Element: TDOMElement; const BaseURL: string);
var
  FootstepsSoundName: string;
  ToxicDamage: TDOMElement;
  I: TXMLElementIterator;
begin
  if not Element.AttributeString('texture_base_name', FTextureBaseName) then
    raise Exception.Create('<properties> element must have "texture_base_name" attribute');

  FootstepsSoundName := '';
  if Element.AttributeString('footsteps_sound', FootstepsSoundName) and
     (FootstepsSoundName <> '') then
    FFootstepsSound := SoundEngine.SoundFromName(FootstepsSoundName) else
    FFootstepsSound := stNone;

  if Element.AttributeString('normal_map', FNormalMap) and (FNormalMap <> '') then
    FNormalMap := CombineURI(BaseURL, FNormalMap) else
    FNormalMap := '';

  if not Element.AttributeString('alpha_channel', FAlphaChannel) then
    FAlphaChannel := '';

  I := Element.ChildrenIterator;
  try
    while I.GetNext do
      if I.Current.TagName = 'toxic' then
      begin
        FToxic := true;
        ToxicDamage := I.Current.ChildElement('damage');
        if not ToxicDamage.AttributeSingle('const', FToxicDamageConst) then
          FToxicDamageConst := 0;
        if not ToxicDamage.AttributeSingle('random', FToxicDamageRandom) then
          FToxicDamageRandom := 0;
        if not ToxicDamage.AttributeSingle('time', FToxicDamageTime) then
          FToxicDamageTime := 0;
      end else
        raise Exception.CreateFmt('Unknown element inside <property>: "%s"',
          [I.Current.TagName]);
  finally FreeAndNil(I) end;
end;

{ TMaterialProperties.TAutoCompressedTextures -------------------------------- }

constructor TMaterialProperties.TAutoCompressedTextures.Create(
  const Element: TDOMElement; const BaseURL: string;
  const AnAutoProcessImageURLs: boolean);
var
  ChildElements: TXMLElementIterator;
  ChildElement, FormatsElement: TDOMElement;
begin
  inherited Create;
  FAutoProcessImageURLs := AnAutoProcessImageURLs;
  IncludePaths := TCastleStringList.Create;
  IncludePathsRecursive := TBooleanList.Create;
  ExcludePaths := TCastleStringList.Create;
  FBaseURL := BaseURL;

  { read from XML }

  ChildElements := Element.ChildrenIterator('include');
  try
    while ChildElements.GetNext do
    begin
      ChildElement := ChildElements.Current;
      IncludePaths.Add(ChildElement.AttributeURL('path', BaseURL));
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

  FormatsElement := Element.ChildElement('formats', false);
  if FormatsElement <> nil then
  begin
    ChildElements := FormatsElement.ChildrenIterator('format');
    try
      while ChildElements.GetNext do
        Include(FFormats, StringToTextureCompression(
          ChildElements.Current.AttributeString('name')));
    finally FreeAndNil(ChildElements) end;
  end;

  if FAutoProcessImageURLs then
    AddLoadImageListener(@LoadImageEvent);
end;

destructor TMaterialProperties.TAutoCompressedTextures.Destroy;
begin
  FreeAndNil(IncludePaths);
  FreeAndNil(IncludePathsRecursive);
  FreeAndNil(ExcludePaths);
  if FAutoProcessImageURLs then
    RemoveLoadImageListener(@LoadImageEvent);
  inherited;
end;

procedure TMaterialProperties.TAutoCompressedTextures.GatherCallback(const FileInfo: TFileInfo; var StopSearch: boolean);
begin
  if (Pos('/' + AutoCompressedDirName + '/', FileInfo.URL) = 0) and
     IsImageMimeType(URIMimeType(FileInfo.URL), false, false) then
    GatheringResult.Add(FileInfo.URL);
end;

function TMaterialProperties.TAutoCompressedTextures.IsAbsoluteURLMatchingRelativeMask(
  const URL, Mask: string): boolean;
var
  U: string;
begin
  U := PrefixRemove(ExtractURIPath(FBaseURL), URL, PathsIgnoreCase);
  Result := IsWild(U, Mask, PathsIgnoreCase);
end;

function TMaterialProperties.TAutoCompressedTextures.
  AutoCompressedTextures: TCastleStringList;

  procedure Exclude(const ExcludePathMask: string; const URLs: TCastleStringList);
  var
    I: Integer;
  begin
    I := 0;
    while I < URLs.Count do
    begin
      // Writeln('Excluding ExcludePathMask ' + ExcludePathMask +
      //   ' from ' + PrefixRemove(ExtractURIPath(FBaseURL), URLs[I], PathsIgnoreCase));
      if IsAbsoluteURLMatchingRelativeMask(URLs[I], ExcludePathMask) then
        URLs.Delete(I) else
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
    FindFiles(IncludePaths[I], false, @GatherCallback, FindOptions);
  end;

  GatheringResult := nil;

  for I := 0 to ExcludePaths.Count - 1 do
    Exclude(ExcludePaths[I], Result);
end;

procedure TMaterialProperties.TAutoCompressedTextures.LoadImageEvent(
  var URL: string);
var
  URLName, URLPath: string;

  { Texture has GPU-compressed counterpart, according to include/exclude
    variables. So try to replace URL with something compressed. }
  procedure ReplaceURL;
  var
    C: TTextureCompression;
  begin
    if GLFeatures = nil then
      OnWarning(wtMinor, 'GPUCompression', 'Cannot determine whether to use GPU compressed version for ' + URL + ' because the image is loaded before GPU capabilities are known') else
    for C in Formats do
      if C in GLFeatures.TextureCompression then
      begin
        URL := CompressedTextureURL(URL, C);
        WritelnLog('GPUCompression', 'Using compressed alternative ' + URL);
        Exit;
      end;
  end;

  { Check is URL not excluded, and eventually call ReplaceURL. }
  procedure CheckExcludeAndReplaceURL;
  var
    I: Integer;
  begin
    for I := 0 to ExcludePaths.Count - 1 do
      if IsAbsoluteURLMatchingRelativeMask(URL, ExcludePaths[I]) then
        Exit;
    ReplaceURL;
  end;

var
  I: Integer;
  IncludePath, IncludeMask: string;
  PathMatches: boolean;
begin
  URLPath := ExtractURIPath(URL);
  URLName := ExtractURIName(URL);
  for I := 0 to IncludePaths.Count - 1 do
  begin
    IncludePath := ExtractURIPath(IncludePaths[I]);
    IncludeMask := ExtractURIName(IncludePaths[I]);
    if IncludePathsRecursive[I] then
      PathMatches := IsPrefix(IncludePath, URLPath, PathsIgnoreCase) else
      PathMatches := AnsiSameText(IncludePath, URLPath); { assume PathsIgnoreCase=true }
    if PathMatches and IsWild(URLName, IncludeMask, PathsIgnoreCase) then
    begin
      CheckExcludeAndReplaceURL;
      Exit;
    end;
  end;
end;

class function TMaterialProperties.TAutoCompressedTextures.CompressedTextureURL(
  const URL: string;
  const TextureCompression: TTextureCompression): string;
begin
  Result := ExtractURIPath(URL) + AutoCompressedDirName + '/' +
    LowerCase(TextureCompressionToString(TextureCompression)) + '/' +
    ExtractURIName(URL) + '.dds';
end;

{ TMaterialProperties ---------------------------------------------------------- }

constructor TMaterialProperties.Create(const AnAutoProcessImageURLs: boolean);
begin
  inherited Create({ owns objects } true);
  FAutoProcessImageURLs := AnAutoProcessImageURLs;
end;

destructor TMaterialProperties.Destroy;
begin
  FreeAndNil(FAutoCompressedTextures);
  inherited;
end;

procedure TMaterialProperties.SetURL(const Value: string);
var
  Config: TXMLDocument;
  Elements: TXMLElementIterator;
  MaterialProperty: TMaterialProperty;
  Stream: TStream;
  AutoCompressedTexturesElement: TDOMElement;
begin
  FURL := Value;

  Clear;
  FreeAndNil(FAutoCompressedTextures);

  if URL = '' then Exit;

  Stream := Download(URL);
  try
    ReadXMLFile(Config, Stream, URL);
  finally FreeAndNil(Stream) end;

  try
    Check(Config.DocumentElement.TagName = 'properties',
      'Root node of material properties file must be <properties>');

    Elements := Config.DocumentElement.ChildrenIterator('property');
    try
      while Elements.GetNext do
      begin
        MaterialProperty := TMaterialProperty.Create;
        Add(MaterialProperty);
        MaterialProperty.LoadFromDOMElement(Elements.Current, AbsoluteURI(URL));
      end;
    finally FreeAndNil(Elements); end;

    AutoCompressedTexturesElement := Config.DocumentElement.ChildElement('auto_compressed_textures', false);
    if AutoCompressedTexturesElement <> nil then
      FAutoCompressedTextures := TAutoCompressedTextures.Create(
        AutoCompressedTexturesElement, URL, FAutoProcessImageURLs);
  finally
    SysUtils.FreeAndNil(Config);
  end;
end;

function TMaterialProperties.FindTextureBaseName(const TextureBaseName: string): TMaterialProperty;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if SameText(Items[I].TextureBaseName, TextureBaseName) then
      Exit(Items[I]);
  Result := nil;
end;

function TMaterialProperties.AutoCompressedTextures: TCastleStringList;
begin
  if FAutoCompressedTextures <> nil then
    Result := FAutoCompressedTextures.AutoCompressedTextures else
    Result := TCastleStringList.Create;
end;

class function TMaterialProperties.AutoCompressedTextureURL(const TextureURL: string;
  const TextureCompression: TTextureCompression): string;
begin
  Result := TAutoCompressedTextures.CompressedTextureURL(TextureURL,
    TextureCompression);
end;

function TMaterialProperties.AutoCompressedTextureFormats: TTextureCompressions;
begin
  if FAutoCompressedTextures <> nil then
    Result := FAutoCompressedTextures.Formats else
    Result := [];
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

finalization
  FreeAndNil(FMaterialProperties);
end.
