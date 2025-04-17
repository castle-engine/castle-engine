{
  Copyright 2018-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Reading CastleSettings.xml ( https://castle-engine.io/manual_castle_settings.php ). }
unit CastleInternalSettings;

{$I castleconf.inc}

interface

uses SysUtils, Classes, Generics.Collections, DOM, Contnrs,
  CastleUtils, CastleClassUtils, CastleFonts, CastleRectangles, CastleTimeUtils,
  CastleUIControls;

type
  TWarmupCache = class;
  TWarmupCacheFormat = class;

  EInvalidSettingsXml = class(Exception);

  TWarmupCacheFormatEvent = procedure (const Cache: TWarmupCache;
    const Element: TDOMElement; const ElementBaseUrl: String) of object;

  { Anything that can be preloaded into the TWarmupCache.
    E.g. a texture, sound, model. }
  TWarmupCacheFormat = class
    { XML element (in CastleSettings.xml) name indicating this format. }
    Name: String;
    Event: TWarmupCacheFormatEvent;
  end;

  TWarmupCacheFormatList = class({$ifdef FPC}specialize{$endif} TObjectList<TWarmupCacheFormat>)
  private
    function CallRegisteredFormat(const Cache: TWarmupCache;
      const Element: TDOMElement; const ElementBaseUrl: String): Boolean;
  public
    procedure RegisterFormat(const Name: String; const Event: TWarmupCacheFormatEvent);
  end;

  { Used by SettingsLoad, an instance of this will be created and owner by container.
    In the future this may be available publicly (in non-internal unit) to have
    "warmpup cache" available for any period of time (right now, it is only for the
    lifetime of the container). }
  TWarmupCache = class(TComponent)
  strict private
    FOwnedObjects: TObjectList;
  private
    procedure ReadElement(const Element: TDOMElement; const ElementBaseUrl: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { General-purpose container for objects that should be owned by this
      cache. May be used by TWarmupCacheFormatEvent implementation,
      if you create something that should be owned by cache. }
    property OwnedObjects: TObjectList read FOwnedObjects;
  end;

{ Register new TWarmupCacheFormat using @code(WarmupCacheFormats.RegisterFormat). }
function WarmupCacheFormats: TWarmupCacheFormatList;

{ Load CastleSettings.xml ( https://castle-engine.io/manual_castle_settings.php )
  into a container.
  This also creates a TWarmupCache that is owned by the container. }
procedure SettingsLoad(const Container: TCastleContainer; const SettingsUrl: String);

implementation

uses Math, TypInfo,
  CastleLog, CastleXmlUtils, CastleStringUtils, CastleGLImages,
  CastleUnicode, CastleUriUtils{$ifdef FPC}, CastleLocalizationGetText{$endif};

{ TWarmupCacheFormatList ----------------------------------------------------- }

function TWarmupCacheFormatList.CallRegisteredFormat(const Cache: TWarmupCache;
  const Element: TDOMElement; const ElementBaseUrl: String): Boolean;
var
  Format: TWarmupCacheFormat;
begin
  for Format in Self do
    if Format.Name = Element.TagName8 then
    begin
      Format.Event(Cache, Element, ElementBaseUrl);
      Exit(true);
    end;
  Result := false;
end;

procedure TWarmupCacheFormatList.RegisterFormat(const Name: String;
  const Event: TWarmupCacheFormatEvent);
var
  Format: TWarmupCacheFormat;
begin
  Assert(Assigned(Event));

  Format := TWarmupCacheFormat.Create;
  Add(Format);
  Format.Name := Name;
  Format.Event := Event;
end;

{ TImageUiCache -------------------------------------------------------------- }

type
  TImageUiCache = class
    class procedure Event(const Cache: TWarmupCache;
      const Element: TDOMElement; const ElementBaseUrl: String);
  end;

class procedure TImageUiCache.Event(const Cache: TWarmupCache;
  const Element: TDOMElement; const ElementBaseUrl: String);
var
  URL: String;
  Image: TCastleImagePersistent;
begin
  URL := Element.AttributeURL('url', ElementBaseUrl);
  Image := TCastleImagePersistent.Create;
  Cache.OwnedObjects.Add(Image);
  Image.URL := URL; // loads the image
end;

{ TWarmupCache --------------------------------------------------------------- }

constructor TWarmupCache.Create(AOwner: TComponent);
begin
  inherited;
  FOwnedObjects := TObjectList.Create(true);
end;

destructor TWarmupCache.Destroy;
begin
  FreeAndNil(FOwnedObjects);
  inherited;
end;

procedure TWarmupCache.ReadElement(const Element: TDOMElement; const ElementBaseUrl: String);
begin
  if not WarmupCacheFormats.CallRegisteredFormat(Self, Element, ElementBaseUrl) then
    raise EInvalidSettingsXml.CreateFmt('Not recognized warmup cache element "%s"',
      [Element.TagName8]);
end;

{ globals -------------------------------------------------------------------- }

var
  FWarmupCacheFormats: TWarmupCacheFormatList;

function WarmupCacheFormats: TWarmupCacheFormatList;
begin
  if FWarmupCacheFormats = nil then
  begin
    FWarmupCacheFormats := TWarmupCacheFormatList.Create(true);
    // register formats implemented in this unit
    FWarmupCacheFormats.RegisterFormat('image_ui', {$ifdef FPC}@{$endif}TImageUiCache{$ifdef FPC}(nil){$endif}.Event);
  end;
  Result := FWarmupCacheFormats;
end;

procedure SettingsLoad(const Container: TCastleContainer; const SettingsUrl: String);

  function UIScalingToString(const UIScaling: TUIScaling): String;
  begin
    Result := SEnding(GetEnumName(TypeInfo(TUIScaling), Ord(UIScaling)), 3);
  end;

  function UIScalingFromString(const S: String): TUIScaling;
  begin
    for Result := Low(TUIScaling) to High(TUIScaling) do
      if S = UIScalingToString(Result) then
        Exit;
    raise EInvalidSettingsXml.CreateFmt('Not a valid value for UIScaling: %s', [S]);
  end;

type
  TDynIntegerArray = array of Integer;

  function ParseIntegerList(const S: String): TDynIntegerArray;
  var
    IntegerList: TIntegerList;
    SeekPos: Integer;
    Token: String;
  begin
    IntegerList := TIntegerList.Create;
    try
      SeekPos := 1;
      repeat
        Token := NextToken(S, SeekPos);
        if Token = '' then Break;
        IntegerList.Add(StrToInt(Token));
      until false;

      if IntegerList.Count = 0 then
        raise EInvalidSettingsXml.Create('sizes_at_load parameter is an empty list in CastleSettings.xml');

      Result := TDynIntegerArray(IntegerList.ToArray);
    finally FreeAndNil(IntegerList) end;
  end;

  procedure ReadWarmupCache(E: TDOMElement);
  var
    Cache: TWarmupCache;
    I: TXMLElementIterator;
    TimeStart: TCastleProfilerTime;
  begin
    TimeStart := Profiler.Start('Warming up cache (following CastleSettings.xml contents)');
    try
      Cache := TWarmupCache.Create(Container);
      I := E.ChildrenIterator;
      try
        while I.GetNext do
          Cache.ReadElement(I.Current, SettingsUrl);
      finally FreeAndNil(I) end;
    finally Profiler.Stop(TimeStart) end;
  end;

  function LoadFontSettings(const FontElement: TDOMElement): TCastleAbstractFont;
  var
    NewFontUrl: String;
    NewFontSize, NewFontOptimalSize: Cardinal;
    NewFontAntiAliased: Boolean;
    AllSizesAtLoadStr: String;
    AllSizesAtLoad: TDynIntegerArray;
    UnicodeCharList: TUnicodeCharList;
    RawString: String;
    S: String;
    StringList: TCastleStringList;
  begin
    if FontElement <> nil then
    begin
      NewFontUrl := FontElement.AttributeURL('url', SettingsUrl);
      NewFontSize := FontElement.AttributeCardinalDef('size', 20);
      NewFontAntiAliased := FontElement.AttributeBooleanDef('anti_aliased', true);

      { Load additional character list for the font }
      UnicodeCharList := TUnicodeCharList.Create;
      { providing only_sample_text="true" will force loading only characters
        explicitly specified, e.g. by sample_text="..." field or other means }
      if not FontElement.AttributeBooleanDef('only_sample_text', false) then
        UnicodeCharList.Add(SimpleAsciiCharacters);
      UnicodeCharList.Add(FontElement.AttributeStringDef('sample_text', ''));
      { Load characters from one or several translation files }
      // TODO: Delphi Localization
      {$ifdef FPC}
      RawString := FontElement.AttributeStringDef('sample_get_text_mo', '');
      if RawString <> '' then
      begin
        StringList := CreateTokens(RawString, WhiteSpaces + [',']);
        for S in StringList do
          AddTranslatedCharacters(CombineURI(SettingsUrl, S), UnicodeCharList);
        FreeAndNil(StringList);
      end;
      {$endif}
      { Loads a comma/whitespace separated list of UTF8 characters decimal code }
      RawString := FontElement.AttributeStringDef('sample_code', '');
      if RawString <> '' then
      begin
        StringList := CreateTokens(RawString, WhiteSpaces + [',']);
        for S in StringList do
          UnicodeCharList.Add(StrToInt(S));
        FreeAndNil(StringList);
      end;

      if UnicodeCharList.Count = 0 then
        raise EInvalidSettingsXml.Create('No characters were loaded for font at ' + NewFontUrl);

      if FontElement.AttributeString('sizes_at_load', AllSizesAtLoadStr) then
      begin
        AllSizesAtLoad := ParseIntegerList(AllSizesAtLoadStr);
        Result := TCustomizedFont.Create(Container);
        TCustomizedFont(Result).Load(NewFontUrl, AllSizesAtLoad, NewFontAntiAliased, UnicodeCharList);
      end else
      begin
        NewFontOptimalSize := FontElement.AttributeCardinalDef('size_at_load', NewFontSize);
        Result := TCastleFont.Create(Container);
        TCastleFont(Result).OptimalSize := NewFontOptimalSize;
        TCastleFont(Result).AntiAliased := NewFontAntiAliased;
        TCastleFont(Result).LoadBasicCharacters := false; // if needed, they are included in UnicodeCharList
        TCastleFont(Result).LoadCharacters := UnicodeCharList.ToString;
        TCastleFont(Result).URL := NewFontUrl;
      end;
      Result.Size := NewFontSize;
      FreeAndNil(UnicodeCharList);
    end else
      Result := nil;
  end;

const
  DefaultUIScaling = usNone;
  DefaultUIReferenceWidth = 0;
  DefaultUIReferenceHeight = 0;
var
  SettingsDoc: TXMLDocument;
  E: TDOMElement;

  NewDefaultFont: TCastleAbstractFont;
  NewFontFamily: TCastleFontFamily;

  NewUIScaling: TUIScaling;
  NewUIReferenceWidth, NewUIReferenceHeight: Single;
begin
  // initialize defaults
  NewUIScaling := DefaultUIScaling;
  NewUIReferenceWidth := DefaultUIReferenceWidth;
  NewUIReferenceHeight := DefaultUIReferenceHeight;
  NewDefaultFont := nil;

  SettingsDoc := URLReadXML(SettingsUrl);
  try
    if SettingsDoc.DocumentElement.TagName8 <> 'castle_settings' then
      raise EInvalidSettingsXml.Create('The root element must be <castle_settings>');

    E := SettingsDoc.DocumentElement.Child('ui_scaling', false);
    if E <> nil then
    begin
      NewUIScaling := UIScalingFromString(
        E.AttributeStringDef('mode', UIScalingToString(DefaultUIScaling)));
      NewUIReferenceWidth :=
        E.AttributeSingleDef('reference_width', DefaultUIReferenceWidth);
      NewUIReferenceHeight :=
        E.AttributeSingleDef('reference_height', DefaultUIReferenceHeight);
    end;

    E := SettingsDoc.DocumentElement.Child('default_font', false);
    if E <> nil then
    begin
      if E.HasAttribute('url') then
        NewDefaultFont := LoadFontSettings(E)
      else
      begin
        NewFontFamily := TCastleFontFamily.Create(Container);
        { Don't assign name, to enable users to call LoadSettings
          multiple times on the same container -- the names shouldn't clash. }
        // NewFontFamily.Name := 'CastleInternalDefaultFontFamily';
        NewFontFamily.Regular := LoadFontSettings(E.Child('regular', false));
        NewFontFamily.Bold := LoadFontSettings(E.Child('bold', false));
        NewFontFamily.Italic := LoadFontSettings(E.Child('italic', false));
        NewFontFamily.BoldItalic := LoadFontSettings(E.Child('bold_italic', false));
        if NewFontFamily.Regular = nil then
          raise EInvalidSettingsXml.Create('The <default_font> specified in CastleSettings.xml does not have "url" attribute (for simple fonts, like TCastleFont, that are not TCastleFontFamily) nor a <regular> variant (for TCastleFontFamily fonts)');
        NewDefaultFont := NewFontFamily;
      end;
    end;

    E := SettingsDoc.DocumentElement.Child('warmup_cache', false);
    if E <> nil then
      ReadWarmupCache(E);
  finally FreeAndNil(SettingsDoc) end;

  Container.UIScaling := NewUIScaling;
  Container.UIReferenceWidth := NewUIReferenceWidth;
  Container.UIReferenceHeight := NewUIReferenceHeight;
  Container.DefaultFont := NewDefaultFont;
end;

{$ifndef FPC}initialization{$endif}

finalization
  FreeAndNil(FWarmupCacheFormats);
end.
