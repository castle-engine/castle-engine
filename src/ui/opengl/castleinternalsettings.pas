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

uses SysUtils, Classes, Generics.Collections,
  CastleUtils, CastleClassUtils, CastleFonts, CastleRectangles, CastleTimeUtils,
  CastleUIControls;

type
  TWarmupCache = class;
  TWarmupCacheFormat = class;

  EInvalidSettings = class(Exception);

  TWarmupCacheFormatEvent = procedure (const Cache: TWarmupCache;
    const Format: TWarmupCacheFormat; const Element: TDOMElement);

  { Anything that can be preloaded into the TWarmupCache.
    E.g. a texture, sound, model. }
  TWarmupCacheFormat = class
    ElementName: String;
    Event: TWarmupCacheFormatEvent;
  end;

  TWarmupCacheFormatList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TList<TWarmupCacheFormat>)
  private
    function CallRegisteredFormat(const Cache: TWarmupCache;
      const Element: TDOMElement): Boolean;
  public
    procedure RegisterFormat(const Name: String; const Event: TWarmupCacheFormatEvent);
  end;

  { Used by SettingsLoad, an instance of this will be created and owner by container.
    In the future this may be available publicly (in non-internal unit) to have
    "warmpup cache" available for any period of time (right now, it is only for the
    lifetime of the container). }
  TWarmupCache = class(TComponent)
  private
    procedure ReadElement(const Element: TDOMElement);
  public
    { Cache image contents in TCastleImagePersistent. }
    procedure AddImageUi(const URL: String);
  end;

{ Register new TWarmupCacheFormat using @code(WarmupCacheFormats.RegisterFormat). }
function WarmupCacheFormats: TWarmupCacheFormatList;

{ Load CastleSettings.xml ( https://castle-engine.io/manual_castle_settings.php )
  into a container.
  This also creates a TWarmupCache that is owned by the container. }
procedure SetttingsLoad(const Container: TUIContainer; const SettingsUrl: String);

implementation

uses DOM, Math, TypInfo,
  CastleLog, CastleXMLUtils, CastleStringUtils;

{ TWarmupCacheFormatList ----------------------------------------------------- }

function TWarmupCacheFormatList.CallRegisteredFormat(const Cache: TWarmupCache;
  const Element: TDOMElement): Boolean;
var
  Format: TWarmupCacheFormat;
begin
  for Format in Self do
    if Format.ElementName = Element.TagName8 then
    begin
      Format.Event(Cache, Format, Element);
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
  Format.ElementName := Name;
  Format.Event := Event;
end;

{ TWarmupCache --------------------------------------------------------------- }

constructor TWarmupCache.Create(AOwner: TComponent);
begin
  inherited;
  OwnedObjects := TObjectList.Create(true);
end;

destructor TWarmupCache.Destroy;
begin
  FreeAndNil(OwnedObjects);
  inherited;
end;

procedure TWarmupCache.AddImageUi(const URL: String);
begin
  Image := TCastleImagePersistent.Create;
  OwnedObjects.Add(Image);
  Image.URL := URL; // loads the image
end;

procedure TWarmupCache.ReadElement(const Element: TDOMElement);
begin
  if Element.TagName8 = 'image_ui' then
    AddImageUi(Element.AttributeURL('url'))
  else
  if not CallRegisteredFormat(Self, Element) then
    raise Exception.
end;

{ globals -------------------------------------------------------------------- }

var
  FWarmupCacheFormats: TWarmupCacheFormatList;

function WarmupCacheFormats: TWarmupCacheFormatList;
begin
  if FWarmupCacheFormats = nil then
    FWarmupCacheFormats := TWarmupCacheFormatList.Create;
  Result := FWarmupCacheFormats;
end;

procedure SetttingsLoad(const Container: TUIContainer; const SettingsUrl: String);

  function UIScalingToString(const UIScaling: TUIScaling): String;
  begin
    Result := SEnding(GetEnumName(TypeInfo(TUIScaling), Ord(UIScaling)), 3);
  end;

  function UIScalingFromString(const S: String): TUIScaling;
  begin
    for Result := Low(TUIScaling) to High(TUIScaling) do
      if S = UIScalingToString(Result) then
        Exit;
    raise Exception.CreateFmt('Not a valid value for UIScaling: %s', [S]);
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
        raise Exception.Create('sizes_at_load parameter is an empty list in CastleSettings.xml');

      Result := IntegerList.ToArray;
    finally FreeAndNil(IntegerList) end;
  end;

const
  DefaultUIScaling = usNone;
  DefaultUIReferenceWidth = 0;
  DefaultUIReferenceHeight = 0;
var
  SettingsDoc: TXMLDocument;
  E: TDOMElement;

  // font stuff
  DefaultFontUrl: String;
  DefaultFontSize, DefaultFontLoadSize: Cardinal;
  DefaultFontAntiAliased: Boolean;
  NewDefaultFont: TCastleFont;
  AllSizesAtLoadStr: String;
  AllSizesAtLoad: TDynIntegerArray;

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
      raise Exception.Create('The root element must be <castle_settings>');

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
      DefaultFontUrl := E.AttributeURL('url', SettingsUrl);
      DefaultFontSize := E.AttributeCardinalDef('size', 20);
      DefaultFontAntiAliased := E.AttributeBooleanDef('anti_aliased', true);

      if E.AttributeString('sizes_at_load', AllSizesAtLoadStr) then
      begin
        AllSizesAtLoad := ParseIntegerList(AllSizesAtLoadStr);
        NewDefaultFont := TCustomizedFont.Create(Container);
        TCustomizedFont(NewDefaultFont).Load(DefaultFontUrl, AllSizesAtLoad, DefaultFontAntiAliased);
      end else
      begin
        DefaultFontLoadSize := E.AttributeCardinalDef('size_at_load', DefaultFontSize);
        NewDefaultFont := TTextureFont.Create(Container);
        TTextureFont(NewDefaultFont).Load(DefaultFontUrl, DefaultFontLoadSize, DefaultFontAntiAliased);
      end;
      NewDefaultFont.Size := DefaultFontSize;
    end;
  finally FreeAndNil(SettingsDoc) end;

  Container.UIScaling := NewUIScaling;
  Container.UIReferenceWidth := NewUIReferenceWidth;
  Container.UIReferenceHeight := NewUIReferenceHeight;
  Container.DefaultFont := NewDefaultFont;
end;

finalization
  FreeAndNil(FWarmupCacheFormats);
end.
