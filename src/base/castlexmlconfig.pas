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

uses SysUtils, Classes, DOM,
  CastleUtils, CastleXMLCfgInternal, CastleXMLUtils, CastleVectors,
  CastleGenericLists, CastleColors;

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
    Adds various Castle Game Engine extensions to the ancestor TXMLConfig
    class:

    @unorderedList(
      @item(load/save from an URL or a TStream (not just a filename),)
      @item(load/save to the default config file location (for user preferences),)
      @item(load/save more types (floats, vectors, colors, URLs,
        multiline text...),)
      @item(PathElement utility, to use powerful DOM functions when needed
        to process something more complex,)
      @item(encrypt/descrypt contents, just use BlowFishKeyPhrase property
        (this is actually built-in in our modified TXMLConfig).)
    )

    See http://castle-engine.sourceforge.net/tutorial_user_prefs.php
    for more documentation. }
  TCastleConfig = class(TXMLConfig)
  private
    FOnLoad, FOnSave: TCastleConfigEventList;
    FLoaded: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Get a @italic(required) integer attribute, raise exception if missing or invalid.
      @raises(EMissingAttribute If the attribute is missing or empty.)
      @raises(EConvertError If the attribute exists but has invalid format.) }
    function GetInteger(const APath: String): Integer; overload;

    { Get a @italic(required) boolean attribute, raise exception if missing or invalid.
      @raises(EMissingAttribute If the attribute is missing or empty.)
      @raises(EConvertError If the attribute exists but has invalid format.) }
    function GetBoolean(const APath: String): Boolean; overload;

    { Internal notes about GetFloat / SetFloat:
      At the beginning I made the float methods
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
      will compile but fail miserably at runtime.
    }

    { Float values reading/writing to config file.

      Note: for powerful reading of float expressions,
      consider using @code(GetFloatExpression) instead of @code(GetFloat).
      It can read expressions like @code("3.0 * 2.0") or @code("sin(2.0)").
      Use CastleScriptConfig unit to introduce
      necessary class helper for this, see @link(TCastleConfigScriptHelper.GetFloatExpression).

      @raises(EMissingAttribute Raised by GetFloat(string) (overloaded
        version without the ADefaultValue parameter) if the attribute is missing.)

      @groupBegin }
    function GetFloat(const APath: string;
      const ADefaultValue: Float): Float;
    function GetFloat(const APath: string): Float;
    procedure SetFloat(const APath: string;
      const AValue: Float);
    procedure SetDeleteFloat(const APath: string;
      const AValue, ADefaultValue: Float);
    { @groupEnd }

    { 2D, 3D, 4D vectors reading/writing to config file.

      They should be expressed in XML like

      @preformatted(<myVector x="1" y="2" z="3" w="4" />)

      You can read such vector by

      @longCode(# GetVector('example/path/to/myVector', Vector4Single(0, 0, 0, 0)); #)

      @groupBegin }
    function GetVector2(const APath: string;
      const ADefaultValue: TVector2Single): TVector2Single; overload;
    function GetVector2(const APath: string): TVector2Single; overload;
    procedure SetVector2(const APath: string;
      const AValue: TVector2Single); overload;
    procedure SetDeleteVector2(const APath: string;
      const AValue, ADefaultValue: TVector2Single); overload;

    function GetVector3(const APath: string;
      const ADefaultValue: TVector3Single): TVector3Single; overload;
    function GetVector3(const APath: string): TVector3Single; overload;
    procedure SetVector3(const APath: string;
      const AValue: TVector3Single); overload;
    procedure SetDeleteVector3(const APath: string;
      const AValue, ADefaultValue: TVector3Single); overload;

    function GetVector4(const APath: string;
      const ADefaultValue: TVector4Single): TVector4Single; overload;
    function GetVector4(const APath: string): TVector4Single; overload;
    procedure SetVector4(const APath: string;
      const AValue: TVector4Single); overload;
    procedure SetDeleteVector4(const APath: string;
      const AValue, ADefaultValue: TVector4Single); overload;

    function GetValue(const APath: string;
      const ADefaultValue: TVector2Single): TVector2Single;
      overload; deprecated 'use GetVector2';
    procedure SetValue(const APath: string;
      const AValue: TVector2Single);
      overload; deprecated 'use SetVector2';
    procedure SetDeleteValue(const APath: string;
      const AValue, ADefaultValue: TVector2Single);
      overload; deprecated 'use SetDeleteVector2';

    function GetValue(const APath: string;
      const ADefaultValue: TVector3Single): TVector3Single;
      overload; deprecated 'use GetVector3';
    procedure SetValue(const APath: string;
      const AValue: TVector3Single);
      overload; deprecated 'use SetVector3';
    procedure SetDeleteValue(const APath: string;
      const AValue, ADefaultValue: TVector3Single);
      overload; deprecated 'use SetDeleteVector3';

    function GetValue(const APath: string;
      const ADefaultValue: TVector4Single): TVector4Single;
      overload; deprecated 'use GetVector4';
    procedure SetValue(const APath: string;
      const AValue: TVector4Single);
      overload; deprecated 'use SetVector4';
    procedure SetDeleteValue(const APath: string;
      const AValue, ADefaultValue: TVector4Single);
      overload; deprecated 'use SetDeleteVector4';
    { @groupEnd }

    { Colors reading/writing to config file.

      This is very similar to 3D / 4D vector reading/writing to config file,
      however

      @orderedList(
        @itemSpacing Compact
        @item(attribute names are better for colors
          (@italic(red, green, blue, alpha) instead of @italic(x, y, z, w)),)
        @item(we allow alternative color specification as hex.)
        @item(and we limit component values to 0..1 range.)
      )

      They should be expressed in XML like

@preformatted(
<myColor red="1" green="0.5" blue="0.25" alpha="1" />
<myColorRGB red="1" green="0.5" blue="0.25" />
)

      or as hex colors (see @link(HexToColor)) like

@preformatted(
<myColor hex="ff804011" />
<myColorRGB hex="ff8040" />
)

      You can read such colors by

@longCode(#
Color := GetColor('example/path/to/myColor', Black);
ColorRGB := GetColorRGB('example/path/to/myColorRGB', BlackRGB);
#)

      @groupBegin }
    function GetColorRGB(const APath: string;
      const ADefaultColor: TCastleColorRGB): TCastleColorRGB; overload;
    function GetColorRGB(const APath: string): TCastleColorRGB; overload;
    procedure SetColorRGB(const APath: string;
      const AColor: TCastleColorRGB); overload;
    procedure SetDeleteColorRGB(const APath: string;
      const AColor, ADefaultColor: TCastleColorRGB); overload;

    function GetColor(const APath: string;
      const ADefaultColor: TCastleColor): TCastleColor; overload;
    function GetColor(const APath: string): TCastleColor; overload;
    procedure SetColor(const APath: string;
      const AColor: TCastleColor); overload;
    procedure SetDeleteColor(const APath: string;
      const AColor, ADefaultColor: TCastleColor); overload;
    { @groupEnd }

    { For a given path, return corresponding DOM element of XML tree.
      This is useful if you want to mix XMLConfig style operations
      on the file and then use some real DOM functions to more directly
      operate/read on XML document.

      Note that for paths that you pass to various
      SetValue / SetColor / SetFloat / SetVector versions,
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
      from regular XMLConfig GetValue / SetValue methods. }
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
      deprecated 'use PathChildrenIterator';

    { For a given path, return iterator for elements of a given name.

      For example, assume you have an XML like this:

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

      You can process it like this:

@longCode(#
var
  I: TXMLElementIterator;
begin
  I := PathChildrenIterator('game_configuration/locations', 'location');
  try
    while I.GetNext do
    begin
      // ... here goes your code to process I.Current ...
    end;
  finally FreeAndNil(I) end;
end;
#)

      Raises exception if element indicated by APath does not exist.
      (But it is OK if it is empty.)
      Never returns @nil. }
    function PathChildrenIterator(const APath: string;
      const ChildName: string): TXMLElementIterator;

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

    { Read string from a text content of given element.
      The text may be multiline, line endings are guaranteed to be converted
      to current OS newlines. }
    function GetMultilineText(const APath: string; const DefaultValue: string): string;

    { Get a value, as a string. Value must exist and cannot be empty in XML file.

      @raises(EMissingAttribute If value doesn't exist or is empty in XML file.) }
    function GetNonEmptyValue(const APath: string): string;

    procedure NotModified;

    { Listeners, automatically called at the @link(Load) or @link(Save)
      calls.

      @bold(If the config file is already loaded when you call
      AddLoadListener, then the Listener is called immediately.)
      This is useful to make sure that Listener is always called,
      regardless of the order. (Regardless if you call Config.Load
      or Config.AddLoadListener first.)

      @groupBegin }
    procedure AddLoadListener(const Listener: TCastleConfigEvent);
    procedure AddSaveListener(const Listener: TCastleConfigEvent);
    procedure RemoveLoadListener(const Listener: TCastleConfigEvent);
    procedure RemoveSaveListener(const Listener: TCastleConfigEvent);
    { @groupEnd }

    { Load the current persistent data (user preferences, savegames etc.).
      All the versions load the appropriate file,
      and call all the listeners (from AddLoadListener)
      to allow the engine (and your own) components to read our settings.

      @unorderedList(
        @item(The overloaded parameter-less version chooses
          a default filename for storing application user preferences.

          It uses ApplicationName to pick a filename that is unique
          to your application (usually you want to assign OnGetApplicationName
          callback to set your name, unless you're fine with default determination
          that looks at stuff like ParamStr(0)).
          See FPC OnGetApplicationName docs.
          It uses @link(ApplicationConfig) to determine location of this file.

          In case the default config is corrupted, it automatically
          catches the exception and loads an empty config.
          It also stores the URL in this case to make sure that following
          @link(Save) call with overwrite the default config location with a good one.
          This is useful in case user somehow corrupted the config file on disk.)

        @item(The overloaded version with URL parameter
          sets @code(TXMLConfig.URL), loading the file from given URL.
          As always, URL may be just a simple filename,
          or an URL with 'file://' protocol, to just load a file from
          the local filesystem.)

        @item(The overloaded version with TStream parameter loads from a stream.
          URL is set to PretendURL (just pass empty string if you don't
          want to be able to save it back).)
      )

      @groupBegin }
    procedure Load(const AURL: string);
    procedure Load;
    procedure Load(const Stream: TStream; const PretendURL: string);
    procedure LoadFromString(const Data: string; const PretendURL: string);
    //procedure LoadFromBase64(const Base64Contents: string);
    { @groupEnd }

    { Load empty config. This loads a clear content, without any saved
      settings, but it takes care to set @link(Loaded) to @true,
      run OnLoad listeners and so on. Useful if your default config
      is broken for some reason (e.g. file corruption),
      but you want to override it and just get into a state
      where config is considered loaded.  }
    procedure LoadEmpty(const PretendURL: string);

    property Loaded: boolean read FLoaded;

    { Save the configuration of all engine components.
      Calls the OnSave callbacks to allow all engine components
      to store their settings in our properties.

      @unorderedList(
        @item(The overloaded parameter-less version flushes
          the changes to disk, thus saving them back to the file from which
          they were read (in @code(TXMLConfig.URL) property).)

        @item(The overloaded version with TStream parameter saves to a stream.
          If does not use inherited Flush method, instead it always
          unconditionally dumps contents to stream.)
      )

      @groupBegin }
    procedure Save;
    procedure Save(const Stream: TStream);
    function SaveToString: string;
    //function SaveToBase64: string;
    { @groupEnd }
  end;

procedure Register;

implementation

uses //Base64,
  CastleStringUtils, CastleFilesUtils, CastleLog, CastleURIUtils,
  CastleWarnings;

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

function TCastleConfig.GetInteger(const APath: String): Integer;
begin
  Result := StrToInt(GetNonEmptyValue(APath));
end;

function TCastleConfig.GetBoolean(const APath: String): Boolean;
var
  S: String;
begin
  S := GetNonEmptyValue(APath);
  if AnsiCompareText(S, 'TRUE') = 0 then
    Result := true else
  if AnsiCompareText(s, 'FALSE') = 0 then
    Result := false else
    raise EConvertError.CreateFmt('Invalid boolean value "%s" in XML attribute "%s"',
      [S, APath]);
end;

{ get/set floats ------------------------------------------------------------ }

function TCastleConfig.GetFloat(const APath: string;
  const ADefaultValue: Float): Float;
var
  ResultString: string;
begin
  ResultString := GetValue(APath, FloatToStr(ADefaultValue));
  Result := StrToFloatDef(ResultString, ADefaultValue);
end;

function TCastleConfig.GetFloat(const APath: string): Float;
begin
  Result := StrToFloat(GetNonEmptyValue(APath));
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

{ get/set vectors ------------------------------------------------------------ }

const
  VectorComponentPaths: array [0..3] of string =
  ('/x', '/y', '/z', '/w');

function TCastleConfig.GetVector2(const APath: string;
  const ADefaultValue: TVector2Single): TVector2Single;
var
  I: Integer;
begin
  for I := 0 to High(ADefaultValue) do
    Result[I] := GetFloat(APath + VectorComponentPaths[I], ADefaultValue[I]);
end;

function TCastleConfig.GetVector2(const APath: string): TVector2Single;
var
  I: Integer;
begin
  for I := 0 to High(Result) do
    Result[I] := GetFloat(APath + VectorComponentPaths[I]);
end;

procedure TCastleConfig.SetVector2(const APath: string;
  const AValue: TVector2Single);
var
  I: Integer;
begin
  for I := 0 to High(AValue) do
    SetFloat(APath + VectorComponentPaths[I], AValue[I]);
end;

procedure TCastleConfig.SetDeleteVector2(const APath: string;
  const AValue, ADefaultValue: TVector2Single);
var
  I: Integer;
begin
  for I := 0 to High(AValue) do
    SetDeleteFloat(APath + VectorComponentPaths[I], AValue[I], ADefaultValue[I]);
end;

function TCastleConfig.GetVector3(const APath: string;
  const ADefaultValue: TVector3Single): TVector3Single;
var
  I: Integer;
begin
  for I := 0 to High(ADefaultValue) do
    Result[I] := GetFloat(APath + VectorComponentPaths[I], ADefaultValue[I]);
end;

function TCastleConfig.GetVector3(const APath: string): TVector3Single;
var
  I: Integer;
begin
  for I := 0 to High(Result) do
    Result[I] := GetFloat(APath + VectorComponentPaths[I]);
end;

procedure TCastleConfig.SetVector3(const APath: string;
  const AValue: TVector3Single);
var
  I: Integer;
begin
  for I := 0 to High(AValue) do
    SetFloat(APath + VectorComponentPaths[I], AValue[I]);
end;

procedure TCastleConfig.SetDeleteVector3(const APath: string;
  const AValue, ADefaultValue: TVector3Single);
var
  I: Integer;
begin
  for I := 0 to High(AValue) do
    SetDeleteFloat(APath + VectorComponentPaths[I], AValue[I], ADefaultValue[I]);
end;

function TCastleConfig.GetVector4(const APath: string;
  const ADefaultValue: TVector4Single): TVector4Single;
var
  I: Integer;
begin
  for I := 0 to High(ADefaultValue) do
    Result[I] := GetFloat(APath + VectorComponentPaths[I], ADefaultValue[I]);
end;

function TCastleConfig.GetVector4(const APath: string): TVector4Single;
var
  I: Integer;
begin
  for I := 0 to High(Result) do
    Result[I] := GetFloat(APath + VectorComponentPaths[I]);
end;

procedure TCastleConfig.SetVector4(const APath: string;
  const AValue: TVector4Single);
var
  I: Integer;
begin
  for I := 0 to High(AValue) do
    SetFloat(APath + VectorComponentPaths[I], AValue[I]);
end;

procedure TCastleConfig.SetDeleteVector4(const APath: string;
  const AValue, ADefaultValue: TVector4Single);
var
  I: Integer;
begin
  for I := 0 to High(AValue) do
    SetDeleteFloat(APath + VectorComponentPaths[I], AValue[I], ADefaultValue[I]);
end;

{ deprecated get/set on vectors ---------------------------------------------- }

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

{ get/set colors ------------------------------------------------------------- }

const
  ColorComponentPaths: array [0..3] of string =
  ('/red', '/green', '/blue', '/alpha');
  HexPath = '/hex';

function TCastleConfig.GetColorRGB(const APath: string;
  const ADefaultColor: TCastleColorRGB): TCastleColorRGB;
var
  I: Integer;
  Hex: string;
begin
  Hex := GetValue(APath + HexPath, '');
  if Hex <> '' then
    Result := HexToColorRGB(Hex) else
  begin
    Hex := GetValue(APath + HexPath, '');
    if Hex <> '' then
      Result := HexToColorRGB(Hex) else
    begin
      for I := 0 to High(ADefaultColor) do
        Result[I] := Clamped(GetFloat(APath + ColorComponentPaths[I], ADefaultColor[I]), 0.0, 1.0);
    end;
  end;
end;

function TCastleConfig.GetColorRGB(const APath: string): TCastleColorRGB;
var
  I: Integer;
  Hex: string;
begin
  Hex := GetValue(APath + HexPath, '');
  if Hex <> '' then
    Result := HexToColorRGB(Hex) else
  begin
    Hex := GetValue(APath, '');
    if Hex <> '' then
      Result := HexToColorRGB(Hex) else
    begin
      for I := 0 to High(Result) do
        Result[I] := Clamped(GetFloat(APath + ColorComponentPaths[I]), 0.0, 1.0);
    end;
  end;
end;

procedure TCastleConfig.SetColorRGB(const APath: string;
  const AColor: TCastleColorRGB);
var
  I: Integer;
begin
  SetValue(APath + HexPath, ColorRGBToHex(AColor));
  for I := 0 to High(AColor) do
    DeleteValue(APath + ColorComponentPaths[I]);
end;

procedure TCastleConfig.SetDeleteColorRGB(const APath: string;
  const AColor, ADefaultColor: TCastleColorRGB);
var
  I: Integer;
begin
  SetDeleteValue(APath + HexPath, ColorRGBToHex(AColor), ColorRGBToHex(ADefaultColor));
  for I := 0 to High(AColor) do
    DeleteValue(APath + ColorComponentPaths[I]);
end;

function TCastleConfig.GetColor(const APath: string;
  const ADefaultColor: TCastleColor): TCastleColor;
var
  I: Integer;
  Hex: string;
begin
  Hex := GetValue(APath + HexPath, '');
  if Hex <> '' then
    Result := HexToColor(Hex) else
  begin
    Hex := GetValue(APath, '');
    if Hex <> '' then
      Result := HexToColor(Hex) else
    begin
      for I := 0 to High(ADefaultColor) do
        Result[I] := Clamped(GetFloat(APath + ColorComponentPaths[I], ADefaultColor[I]), 0.0, 1.0);
    end;
  end;
end;

function TCastleConfig.GetColor(const APath: string): TCastleColor;
var
  I: Integer;
  Hex: string;
begin
  Hex := GetValue(APath + HexPath, '');
  if Hex <> '' then
    Result := HexToColor(Hex) else
  begin
    Hex := GetValue(APath, '');
    if Hex <> '' then
      Result := HexToColor(Hex) else
    begin
      for I := 0 to High(Result) do
        Result[I] := Clamped(GetFloat(APath + ColorComponentPaths[I]), 0.0, 1.0);
    end;
  end;
end;

procedure TCastleConfig.SetColor(const APath: string;
  const AColor: TCastleColor);
var
  I: Integer;
begin
  SetValue(APath + HexPath, ColorToHex(AColor));
  for I := 0 to High(AColor) do
    DeleteValue(APath + ColorComponentPaths[I]);
end;

procedure TCastleConfig.SetDeleteColor(const APath: string;
  const AColor, ADefaultColor: TCastleColor);
var
  I: Integer;
begin
  SetDeleteValue(APath + HexPath, ColorToHex(AColor), ColorToHex(ADefaultColor));
  for I := 0 to High(AColor) do
    DeleteValue(APath + ColorComponentPaths[I]);
end;

{ others --------------------------------------------------------------------- }

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

function TCastleConfig.PathChildrenIterator(const APath: string;
  const ChildName: string): TXMLElementIterator;
begin
  Result := TXMLElementFilteringIterator.Create(
    PathElement(APath, true), ChildName);
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

function TCastleConfig.GetMultilineText(const APath: string;
  const DefaultValue: string): string;
var
  E: TDOMElement;
begin
  E := PathElement(APath, false);
  if E = nil then
    Result := DefaultValue else
    Result := E.TextContent;
  { convert all to Unix-line endings }
  StringReplaceAllVar(Result, #13, '', false);
  { in case we're not on Unix, convert to current line endings }
  {$warnings off} { don't warn about dead code on OSes where NL = #10 }
  if #10 <> NL then
    StringReplaceAllVar(Result, #10, NL, false);
  {$warnings on}
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
  FOnLoad.ExecuteAll(Self);
  FLoaded := true;

  { This is used for various files (not just user preferences,
    also resource.xml files). Logging this may get talkative, but it's also
    useful for now... }
  WritelnLog('Config', 'Loading configuration from "%s"', [AURL]);
end;

procedure TCastleConfig.Load;
var
  LoadURL: string;
begin
  LoadURL := ApplicationConfig(ApplicationName + '.conf');
  try
    Load(LoadURL);
  except
    on E: Exception do
    begin
      OnWarning(wtMajor, 'UserConfig', 'User config corrupted (will load defaults): ' + E.Message);
      LoadEmpty(LoadURL);
    end;
  end;
end;

procedure TCastleConfig.Save;
begin
  FOnSave.ExecuteAll(Self);
  Flush;
  if Log and (URL <> '') then
    WritelnLog('Config', 'Saving configuration to "%s"', [URL]);
end;

procedure TCastleConfig.Load(const Stream: TStream; const PretendURL: string);
begin
  WritelnLog('Config', 'Loading configuration from stream');
  LoadFromStream(Stream, PretendURL);
  FOnLoad.ExecuteAll(Self);
  FLoaded := true;
end;

procedure TCastleConfig.Save(const Stream: TStream);
begin
  FOnSave.ExecuteAll(Self);
  SaveToStream(Stream);
  WritelnLog('Config', 'Saving configuration to stream');
end;

procedure TCastleConfig.AddLoadListener(const Listener: TCastleConfigEvent);
begin
  if Loaded then
    Listener(Self);
  FOnLoad.Add(Listener);
end;

procedure TCastleConfig.AddSaveListener(const Listener: TCastleConfigEvent);
begin
  FOnSave.Add(Listener);
end;

procedure TCastleConfig.RemoveLoadListener(const Listener: TCastleConfigEvent);
begin
  FOnLoad.Remove(Listener);
end;

procedure TCastleConfig.RemoveSaveListener(const Listener: TCastleConfigEvent);
begin
  FOnSave.Remove(Listener);
end;

{ // Should work, but was never tested
procedure TCastleConfig.LoadFromBase64(const Base64Contents: string);
var
  Base64Decode: TBase64DecodingStream;
  InputStream: TStringStream;
begin
  InputStream := TStringStream.Create(Base64Contents);
  try
    Base64Decode := TBase64DecodingStream.Create(InputStream, bdmMIME);
    try
      Load(Base64Decode);
    finally FreeAndNil(Base64Decode) end;
  finally FreeAndNil(InputStream) end;
end;
}

procedure TCastleConfig.LoadFromString(const Data: string; const PretendURL: string);
var
  InputStream: TStringStream;
begin
  InputStream := TStringStream.Create(Data);
  try
    Load(InputStream, PretendURL);
  finally FreeAndNil(InputStream) end;
end;

procedure TCastleConfig.LoadEmpty(const PretendURL: string);
const
  EmptyConfig = '<?xml version="1.0" encoding="utf-8"?>' + LineEnding +
    '<CONFIG>' + LineEnding +
    '</CONFIG>';
begin
  LoadFromString(EmptyConfig, PretendURL);
end;

{ // Should work, but was never tested
function TCastleConfig.SaveToBase64: string;
var
  Base64Encode: TBase64EncodingStream;
  ResultStream: TStringStream;
begin
  ResultStream := TStringStream.Create('');
  try
    Base64Encode := TBase64EncodingStream.Create(ResultStream);
    try
      Save(Base64Encode);
      Result := ResultStream.DataString;
    finally FreeAndNil(Base64Encode) end;
  finally FreeAndNil(ResultStream) end;
end;
}

function TCastleConfig.SaveToString: string;
var
  ResultStream: TStringStream;
begin
  ResultStream := TStringStream.Create('');
  try
    Save(ResultStream);
    Result := ResultStream.DataString;
  finally FreeAndNil(ResultStream) end;
end;

end.
