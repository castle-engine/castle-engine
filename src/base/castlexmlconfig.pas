{
  Copyright 2006-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Storing configuration files in XML (TCastleConfig). }
unit CastleXMLConfig;

interface

uses CastleUtils, CastleXMLCfgInternal, DOM,
  CastleVectors, CastleKeysMouse, CastleGenericLists, SysUtils, Classes;

type
  EMissingAttribute = class(Exception);

  TCastleConfig = class;

  TCastleConfigEvent = procedure (const Config: TCastleConfig) of object;

  TCastleConfigEventList = class(specialize TGenericStructList<TCastleConfigEvent>)
  public
    { Call all items. }
    procedure ExecuteAll(const Config: TCastleConfig);
  end;

  { Store configuration in XML format.

    This is a descendant of TXMLConfig that adds various small extensions:
    float types (GetFloat, SetFloat, SetDeleteFloat),
    vector types, key (TKey) types,
    PathElement utility. }
  TCastleConfig = class(TXMLConfig)
  private
    FOnLoad, FOnSave: TCastleConfigEventList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Internal notes: At the beginning I made the float methods
      to overload existing names (GetValue, SetValue etc.).

      But this turned out to be a *very* bad idea: integers are
      casted to floats automatically, and this means that it's too
      easy to use integer getter to read a value that may be float.
      Consider that default value for some float parameter is of integer type
      (e.g. because it was declared as an integer, because you forget to
      write "0.0" instead of "0" etc.). Then

        MyValue := GetValue('float_param', 0);

      will choose GetValue that interprets given value as an integer.
      If you perviously stored a float value there
      (like by SetValue('float_param', 3.14)) then the GetValue above
      will compile but fail miserably at runtime }

    { }
    function GetFloat(const APath: string;
      const ADefaultValue: Float): Float;
    procedure SetFloat(const APath: string;
      const AValue: Float);
    procedure SetDeleteFloat(const APath: string;
      const AValue, ADefaultValue: Float);

    function GetValue(const APath: string;
      const ADefaultValue: TVector2Single): TVector2Single; overload;
    procedure SetValue(const APath: string;
      const AValue: TVector2Single); overload;
    procedure SetDeleteValue(const APath: string;
      const AValue, ADefaultValue: TVector2Single); overload;

    function GetValue(const APath: string;
      const ADefaultValue: TVector3Single): TVector3Single; overload;
    procedure SetValue(const APath: string;
      const AValue: TVector3Single); overload;
    procedure SetDeleteValue(const APath: string;
      const AValue, ADefaultValue: TVector3Single); overload;

    function GetValue(const APath: string;
      const ADefaultValue: TVector4Single): TVector4Single; overload;
    procedure SetValue(const APath: string;
      const AValue: TVector4Single); overload;
    procedure SetDeleteValue(const APath: string;
      const AValue, ADefaultValue: TVector4Single); overload;

    function GetValue(const APath: string;
      const ADefaultValue: TKey): TKey; overload;
    procedure SetValue(const APath: string;
      const AValue: TKey); overload;
    procedure SetDeleteValue(const APath: string;
      const AValue, ADefaultValue: TKey); overload;

    { For a given path, return corresponding DOM element of XML tree.
      This is useful if you want to mix XMLConfig style operations
      on the file and then use some real DOM functions to more directly
      operate/read on XML document.

      Note that for paths that you pass to various SetValue versions,
      the last path component is the attribute name. You do not pass
      this here. Path passed here should end with the name of final
      element.

      Path passed here may but doesn't have to be terminated by a final slash.
      In fact, for now the path is just splitted using slash character
      as a separator, so a path like @code(/some////path/) is equivalent
      to a path like (some/path). But don't depend on this behavior.

      If there is no such element:
      when RaiseExceptionWhenMissing=@true, raises exception.
      when RaiseExceptionWhenMissing=@false, returns @nil.

      Remember that XMLConfig idea of XML document is limited.
      That's intentional (XMLConfig is supposed to offer only a simple limited
      XML access), and this means that some XML trees may confuse XMLConfig.
      For example, if there are two elements with the same TagName as a children
      of the same element: XMLConfig will (probably ?) just always ignore
      the second one. Which means that if you use this method to change
      some XML content, you should be careful when accessing this content
      from regular XMLConfig Get/SetValue methods. }
    function PathElement(const APath: string;
      const RaiseExceptionWhenMissing: boolean = false): TDOMElement;

    { For a given path, return corresponding children elements of a given
      DOM element of XML tree. For example, you have an XML like this:

@preformatted(
<?xml version="1.0" encoding="UTF-8"?>
<CONFIG>
  <game_configuration>
    <locations>
      <location name="location_1st">...</location>
      <location name="location_2nd">...</location>
    </locations>
  </game_configuration>
</CONFIG>
)

      You could use @code(PathElement('game_configuration/locations'))
      to get the @code(<locations>) DOM element.
      Or you could use this method @code(PathChildren('game_configuration/locations',
        'location')) to get a list of <location> elements.

      Raises exception if element indicated by APath does not exist.
      (But it is OK if it is empty.)
      Never returns @nil. }
    function PathChildren(const APath: string; const ChildName: string): TDOMNodeList;

    { Read an URL from an XML attribute.
      The attribute in an XML file may be an absolute or relative URL,
      (we will look at own TXMLConfig.FileName directory to resolve relative
      URLs). The returned URL is always an absolute URL.

      If EmptyIfNoAttribute, then this will just set URL to ''
      if appropriate XML attribute not found. Otherwise
      (when EmptyIfNoAttribute = @false, this is default),
      error will be raised.

      @raises(EMissingAttribute If EmptyIfNoAttribute = @false and no such attribute.) }
    function GetURL(const APath: string;
      const EmptyIfNoAttribute: boolean = false): string;

    { Get a value, as a string. Value must exist and cannot be empty in XML file.

      @raises(EMissingAttribute If value doesn't exist or is empty in XML file.) }
    function GetNonEmptyValue(const APath: string): string;

    procedure NotModified;

    { Called at @link(Load). }
    property OnLoad: TCastleConfigEventList read FOnLoad;

    { Called at @link(Save). }
    property OnSave: TCastleConfigEventList read FOnSave;

    { Load the current configuration of the engine components.
      Sets @link(TXMLConfig.URL), loading the appropriate file to our properties,
      and then calls the OnLoad callbacks to allow all engine components
      read their settings.

      Accepts URL as parameter, converting it to a local filename
      under the hood.

      The overloaded parameter-less version chooses
      a suitable filename for storing per-program user preferences.
      It uses ApplicationName to pick a filename that is unique
      to your application (usually you want to assign OnGetApplicationName
      callback to set your name, unless you're fine with default determination
      that looks at stuff like ParamStr(0)).
      See FPC OnGetApplicationName docs.
      It uses @link(ApplicationConfig) to determine location of this file.

      The overloaded version with TStream parameter loads from a stream.
      URL is set to empty.

      @groupBegin }
    procedure Load(const AURL: string);
    procedure Load;
    procedure Load(const Stream: TStream);
    { @groupEnd }

    { Save the configuration of all engine components.
      Calls the OnSave callbacks to allow all engine components
      to store their settings in our properties, and then flushes
      them to disk (using @link(TXMLConfig.URL) property)
      by inherited Flush method.

      The overloaded version with TStream parameter saves to a stream.
      If does not use inherited Flush method, instead it always unconditionally
      dumps contents to stream.

      @groupBegin }
    procedure Save;
    procedure Save(const Stream: TStream);
    { @groupEnd }
  end;

procedure Register;

implementation

uses CastleStringUtils, CastleFilesUtils, CastleLog, CastleURIUtils;

procedure Register;
begin
  RegisterComponents('Castle', [TCastleConfig]);
end;

{ TCastleConfigEventList ----------------------------------------------------- }

procedure TCastleConfigEventList.ExecuteAll(const Config: TCastleConfig);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I](Config);
end;

{ TCastleConfig -------------------------------------------------------------- }

constructor TCastleConfig.Create(AOwner: TComponent);
begin
  inherited;
  FOnLoad := TCastleConfigEventList.Create;
  FOnSave := TCastleConfigEventList.Create;
end;

destructor TCastleConfig.Destroy;
begin
  FreeAndNil(FOnLoad);
  FreeAndNil(FOnSave);
  inherited;
end;

function TCastleConfig.GetFloat(const APath: string;
  const ADefaultValue: Float): Float;
var
  ResultString: string;
begin
  ResultString := GetValue(APath, FloatToStr(ADefaultValue));
  Result := StrToFloatDef(ResultString, ADefaultValue);
end;

procedure TCastleConfig.SetFloat(const APath: string;
  const AValue: Float);
begin
  SetValue(APath, FloatToStr(AValue));
end;

procedure TCastleConfig.SetDeleteFloat(const APath: string;
  const AValue, ADefaultValue: Float);
begin
  SetDeleteValue(APath, FloatToStr(AValue), FloatToStr(ADefaultValue));
end;

const
  VectorComponentPaths: array [0..3] of string =
  ('/x', '/y', '/z', '/w');

function TCastleConfig.GetValue(const APath: string;
  const ADefaultValue: TVector2Single): TVector2Single;
var
  I: Integer;
begin
  for I := 0 to High(ADefaultValue) do
    Result[I] := GetFloat(APath + VectorComponentPaths[I], ADefaultValue[I]);
end;

procedure TCastleConfig.SetValue(const APath: string;
  const AValue: TVector2Single);
var
  I: Integer;
begin
  for I := 0 to High(AValue) do
    SetFloat(APath + VectorComponentPaths[I], AValue[I]);
end;

procedure TCastleConfig.SetDeleteValue(const APath: string;
  const AValue, ADefaultValue: TVector2Single);
var
  I: Integer;
begin
  for I := 0 to High(AValue) do
    SetDeleteFloat(APath + VectorComponentPaths[I], AValue[I], ADefaultValue[I]);
end;

function TCastleConfig.GetValue(const APath: string;
  const ADefaultValue: TVector3Single): TVector3Single;
var
  I: Integer;
begin
  for I := 0 to High(ADefaultValue) do
    Result[I] := GetFloat(APath + VectorComponentPaths[I], ADefaultValue[I]);
end;

procedure TCastleConfig.SetValue(const APath: string;
  const AValue: TVector3Single);
var
  I: Integer;
begin
  for I := 0 to High(AValue) do
    SetFloat(APath + VectorComponentPaths[I], AValue[I]);
end;

procedure TCastleConfig.SetDeleteValue(const APath: string;
  const AValue, ADefaultValue: TVector3Single);
var
  I: Integer;
begin
  for I := 0 to High(AValue) do
    SetDeleteFloat(APath + VectorComponentPaths[I], AValue[I], ADefaultValue[I]);
end;

function TCastleConfig.GetValue(const APath: string;
  const ADefaultValue: TVector4Single): TVector4Single;
var
  I: Integer;
begin
  for I := 0 to High(ADefaultValue) do
    Result[I] := GetFloat(APath + VectorComponentPaths[I], ADefaultValue[I]);
end;

procedure TCastleConfig.SetValue(const APath: string;
  const AValue: TVector4Single);
var
  I: Integer;
begin
  for I := 0 to High(AValue) do
    SetFloat(APath + VectorComponentPaths[I], AValue[I]);
end;

procedure TCastleConfig.SetDeleteValue(const APath: string;
  const AValue, ADefaultValue: TVector4Single);
var
  I: Integer;
begin
  for I := 0 to High(AValue) do
    SetDeleteFloat(APath + VectorComponentPaths[I], AValue[I], ADefaultValue[I]);
end;

function TCastleConfig.GetValue(const APath: string;
  const ADefaultValue: TKey): TKey;
begin
  Result := StrToKey(GetValue(APath, KeyToStr(ADefaultValue)), ADefaultValue);
end;

procedure TCastleConfig.SetValue(const APath: string;
  const AValue: TKey);
begin
  SetValue(APath, KeyToStr(AValue));
end;

procedure TCastleConfig.SetDeleteValue(const APath: string;
  const AValue, ADefaultValue: TKey);
begin
  SetDeleteValue(APath, KeyToStr(AValue), KeyToStr(ADefaultValue));
end;

function TCastleConfig.PathElement(const APath: string;
  const RaiseExceptionWhenMissing: boolean): TDOMElement;

  { Find a children element, nil if not found. }
  function FindElementChildren(Element: TDOMElement;
    const ElementName: string): TDOMElement;
  var
    Node: TDOMNode;
  begin
    Node := Element.FindNode(ElementName);
    if (Node <> nil) and (Node.NodeType = ELEMENT_NODE) then
      Result := Node as TDOMElement else
      Result := nil;
  end;

var
  SeekPos: Integer;
  PathComponent: string;
begin
  Result := Doc.DocumentElement;
  SeekPos := 1;
  while Result <> nil do
  begin
    PathComponent := NextToken(APath, SeekPos, ['/']);
    if PathComponent = '' then break;
    Result := FindElementChildren(Result, PathComponent);
  end;

  if (Result = nil) and RaiseExceptionWhenMissing then
    raise Exception.CreateFmt('Missing element "%s" in file "%s"', [APath, URL]);
end;

function TCastleConfig.PathChildren(const APath: string;
  const ChildName: string): TDOMNodeList;
begin
  Result := PathElement(APath, true).GetElementsByTagName(ChildName);
end;

function TCastleConfig.GetURL(const APath: string;
  const EmptyIfNoAttribute: boolean): string;
begin
  Result := GetValue(APath, '');
  if Result = '' then
  begin
    if not EmptyIfNoAttribute then
      raise EMissingAttribute.CreateFmt('Missing attribute "%s" in XML file', [APath]);
  end else
    Result := CombineURI(URL, Result);
end;

function TCastleConfig.GetNonEmptyValue(const APath: string): string;
begin
  Result := GetValue(APath, '');
  if Result = '' then
    raise EMissingAttribute.CreateFmt('Missing attribute "%s" in XML file', [APath]);
end;

procedure TCastleConfig.NotModified;
begin
  FModified := false;
end;

procedure TCastleConfig.Load(const AURL: string);
begin
  URL := AURL;
  OnLoad.ExecuteAll(Self);

  { This is used for various files (not just user preferences,
    also resource.xml files), and logging this gets too talkative for now.
  if Log then
    WritelnLog('Config', 'Loading configuration from "%s"', [AURL]); }
end;

procedure TCastleConfig.Load;
begin
  Load(ApplicationConfig(ApplicationName + '.conf'));
end;

procedure TCastleConfig.Save;
begin
  OnSave.ExecuteAll(Self);
  Flush;

  if Log and (URL <> '') then
    WritelnLog('Config', 'Saving configuration to "%s"', [URL]);
end;

procedure TCastleConfig.Load(const Stream: TStream);
begin
  WritelnLog('Config', 'Loading configuration from stream');
  LoadFromStream(Stream);
  OnLoad.ExecuteAll(Self);
end;

procedure TCastleConfig.Save(const Stream: TStream);
begin
  OnSave.ExecuteAll(Self);
  SaveToStream(Stream);
  WritelnLog('Config', 'Saving configuration to stream');
end;

end.
