{
  Copyright 2006-2022 Michalis Kamburelis.

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

{$I castleconf.inc}

interface

uses SysUtils, Classes, DOM, Generics.Collections,
  CastleUtils, CastleXMLCfgInternal, CastleXMLUtils, CastleVectors, CastleColors;

type
  EMissingAttribute = class(Exception);

  TCastleConfig = class;

  TCastleConfigEvent = procedure (const Config: TCastleConfig) of object;

  TCastleConfigEventList = class({$ifdef FPC}specialize{$endif} TList<TCastleConfigEvent>)
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
      @item(PathElement and MakePathElement utility, to use powerful DOM
        functions when needed to process something more complex,)
      @item(encrypt/descrypt contents, just use BlowFishKeyPhrase property
        (this is actually built-in in our modified TXMLConfig).)
    )

    See https://castle-engine.io/manual_user_prefs.php
    for more documentation. }
  TCastleConfig = class(TXMLConfig)
  private
    FOnLoad, FOnSave: TCastleConfigEventList;
    FLoaded: boolean;
    { Load empty config. This loads a clear content, without any saved
      settings, but it takes care to set @link(IsLoaded) to @true,
      run OnLoad listeners and so on. Useful if your default config
      is broken for some reason (e.g. file corruption),
      but you want to override it and just get into a state
      where config is considered loaded.  }
    procedure LoadEmpty(const PretendURL: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Integer values reading/writing to config file.
      @raises(EMissingAttribute Raised by GetInteger(string) (overloaded
      version without the ADefaultValue parameter) if the attribute is missing.)
      @raises(EConvertError If the attribute exists but has invalid format.) }
    function GetInteger(const APath: string;
      const ADefaultValue: Integer): Integer; overload;
    function GetInteger(const APath: String): Integer; overload;

    { Get a @italic(required) boolean attribute, raise exception if missing or invalid.
      @raises(EMissingAttribute If the attribute is missing or empty.)
      @raises(EConvertError If the attribute exists but has invalid format.) }
    function GetBoolean(const APath: String): Boolean; overload;

    { Get a @italic(required, non-empty) string value.
      Value must exist and cannot be empty in XML file.
      @raises(EMissingAttribute If value doesn't exist or is empty in XML file.) }
    function GetStringNonEmpty(const APath: string): string;

    function GetNonEmptyValue(const APath: string): string; deprecated 'use GetStringNonEmpty';

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
      Use CastleScriptXML unit to introduce
      necessary class helper for this, see @link(TCastleConfigScriptHelper.GetFloatExpression).

      @raises(EMissingAttribute Raised by GetFloat(string) (overloaded
        version without the ADefaultValue parameter) if the attribute is missing.)

      @groupBegin }
    function GetFloat(const APath: string;
      const ADefaultValue: Float): Float; overload;
    function GetFloat(const APath: string): Float; overload;
    procedure SetFloat(const APath: string;
      const AValue: Float);
    procedure SetDeleteFloat(const APath: string;
      const AValue, ADefaultValue: Float);
    { @groupEnd }

    { Int64 values reading/writing to config file.
      @raises(EMissingAttribute Raised by GetInt64(string) (overloaded
        version without the ADefaultValue parameter) if the attribute is missing.) }
    function GetInt64(const APath: string;
      const ADefaultValue: Int64): Int64; overload;
    function GetInt64(const APath: string): Int64; overload;
    procedure SetInt64(const APath: string;
      const AValue: Int64);
    procedure SetDeleteInt64(const APath: string;
      const AValue, ADefaultValue: Int64);
    { @groupEnd }

    { 2D, 3D, 4D vectors reading/writing to config file.

      They should be expressed in XML like

      @preformatted(<myVector x="1" y="2" z="3" w="4" />)

      You can read such vector by

      @longCode(# GetVector('example/path/to/myVector', Vector4(0, 0, 0, 0)); #)

      @groupBegin }
    function GetVector2(const APath: string;
      const ADefaultValue: TVector2): TVector2; overload;
    function GetVector2(const APath: string): TVector2; overload;
    procedure SetVector2(const APath: string;
      const AValue: TVector2); overload;
    procedure SetDeleteVector2(const APath: string;
      const AValue, ADefaultValue: TVector2); overload;

    function GetVector3(const APath: string;
      const ADefaultValue: TVector3): TVector3; overload;
    function GetVector3(const APath: string): TVector3; overload;
    procedure SetVector3(const APath: string;
      const AValue: TVector3); overload;
    procedure SetDeleteVector3(const APath: string;
      const AValue, ADefaultValue: TVector3); overload;

    function GetVector4(const APath: string;
      const ADefaultValue: TVector4): TVector4; overload;
    function GetVector4(const APath: string): TVector4; overload;
    procedure SetVector4(const APath: string;
      const AValue: TVector4); overload;
    procedure SetDeleteVector4(const APath: string;
      const AValue, ADefaultValue: TVector4); overload;

    function GetValue(const APath: string;
      const ADefaultValue: TVector2): TVector2;
      overload; deprecated 'use GetVector2';
    procedure SetValue(const APath: string;
      const AValue: TVector2);
      overload; deprecated 'use SetVector2';
    procedure SetDeleteValue(const APath: string;
      const AValue, ADefaultValue: TVector2);
      overload; deprecated 'use SetDeleteVector2';

    function GetValue(const APath: string;
      const ADefaultValue: TVector3): TVector3;
      overload; deprecated 'use GetVector3';
    procedure SetValue(const APath: string;
      const AValue: TVector3);
      overload; deprecated 'use SetVector3';
    procedure SetDeleteValue(const APath: string;
      const AValue, ADefaultValue: TVector3);
      overload; deprecated 'use SetDeleteVector3';

    function GetValue(const APath: string;
      const ADefaultValue: TVector4): TVector4;
      overload; deprecated 'use GetVector4';
    procedure SetValue(const APath: string;
      const AValue: TVector4);
      overload; deprecated 'use SetVector4';
    procedure SetDeleteValue(const APath: string;
      const AValue, ADefaultValue: TVector4);
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
      from regular XMLConfig GetValue / SetValue methods.

      Note that if you modify the DOM contents this way,
      you must manually call MarkModified afterwards, to make sure
      it will get saved later. }
    function PathElement(const APath: string;
      const RaiseExceptionWhenMissing: boolean = false): TDOMElement;

    { Similar to PathElement,
      but creates the necessary elements along the way as needed. }
    function MakePathElement(const APath: string): TDOMElement;

    {$ifdef FPC}
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
    {$endif}

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
    function GetMultilineText(const APath: string; const DefaultValue: string): string; overload;

    { Read @italic(required, non-empty) string from a text content of given element.
      The text may be multiline, line endings are guaranteed to be converted
      to current OS newlines. }
    function GetMultilineText(const APath: string): string; overload;

    procedure NotModified;
    procedure MarkModified;

    function Document: TDOMDocument;

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

    property IsLoaded: boolean read FLoaded;

    { Load the current persistent data (user preferences, savegames etc.).

      All these methods call the listeners (from AddLoadListener).
      All these methods update the @link(IsLoaded) property and the URL property.
      All these methods are secured to never raise exception in case of a currupted
      config file -- in this case, they silently load an empty config
      (but keep the new URL, so that following
      @link(Save) call with overwrite the config location with a good save.)

      @italic(Never call the ancestor load / save methods,
      like TXMLConfig.LoadFromStream and TXMLConfig.SaveToStream, to be on the safe side.
      Use the methods below to load / save user config.)

      @unorderedList(
        @item(The overloaded @code(Load) parameter-less version chooses
          a default filename for storing application user preferences.

          It uses ApplicationName to pick a filename that is unique
          to your application. Usually you want to assign
          @link(TCastleApplicationProperties.ApplicationName ApplicationProperties.ApplicationName),
          unless you're fine with the default name derived from ParamStr(0).

          It uses @link(ApplicationConfig) to determine location of this file.)

        @item(The overloaded @code(Load) version with URL
          sets @code(TXMLConfig.URL), loading the file from given URL.
          The URL may use any supported protocol,
          see https://castle-engine.io/manual_network.php .
          It can also just be a filename. )

        @item(The overloaded @code(Load) version with TStream loads from a stream.
          URL is set to PretendURL (just pass empty string if you don't
          need to be able to save it back).)
      )

      @groupBegin }
    procedure Load; overload;
    procedure Load(const AURL: string); overload;
    procedure Load(const Stream: TStream; const PretendURL: string); overload;
    procedure LoadFromString(const Data: string; const PretendURL: string);
    { @groupEnd }

    { Save the user persistent configuration.
      Calls all the listeners (registered by AddSaveListener).

      @unorderedList(
        @item(The overloaded parameter-less version flushes
          the changes to disk, thus saving them back to the file from which
          they were read (in @code(TXMLConfig.URL) property).)

        @item(The overloaded version with TStream parameter saves to a stream.
          If does not use inherited Flush method, instead it always
          unconditionally dumps contents to stream.)
      )

      @groupBegin }
    procedure Save; overload;
    procedure Save(const Stream: TStream); overload;
    function SaveToString: string;
    { @groupEnd }
  end;

implementation

uses //Base64,
  CastleStringUtils, CastleFilesUtils, CastleLog, CastleURIUtils;

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

function TCastleConfig.GetInteger(const APath: string;
  const ADefaultValue: Integer): Integer;
var
  ResultString: string;
begin
  ResultString := GetValue(APath, IntToStr(ADefaultValue));
  Result := StrToIntDef(ResultString, ADefaultValue);
end;

function TCastleConfig.GetInteger(const APath: String): Integer;
begin
  Result := StrToInt(GetStringNonEmpty(APath));
end;

function TCastleConfig.GetBoolean(const APath: String): Boolean;
var
  S: String;
begin
  S := GetStringNonEmpty(APath);
  if AnsiCompareText(S, 'TRUE') = 0 then
    Result := true else
  if AnsiCompareText(s, 'FALSE') = 0 then
    Result := false else
    raise EConvertError.CreateFmt('Invalid boolean value "%s" in XML attribute "%s"',
      [S, APath]);
end;

function TCastleConfig.GetStringNonEmpty(const APath: string): string;
begin
  Result := GetValue(APath, '');
  if Result = '' then
    raise EMissingAttribute.CreateFmt('Missing attribute "%s" in XML file', [APath]);
end;

function TCastleConfig.GetNonEmptyValue(const APath: string): string;
begin
  Result := GetStringNonEmpty(APath);
end;

{ get/set floats ------------------------------------------------------------ }

function TCastleConfig.GetFloat(const APath: string;
  const ADefaultValue: Float): Float;
var
  ResultString: string;
begin
  ResultString := GetValue(APath, FloatToStrDot(ADefaultValue));
  Result := StrToFloatDefDot(ResultString, ADefaultValue);
end;

function TCastleConfig.GetFloat(const APath: string): Float;
begin
  Result := StrToFloatDot(GetStringNonEmpty(APath));
end;

procedure TCastleConfig.SetFloat(const APath: string;
  const AValue: Float);
begin
  SetValue(APath, FloatToStrDot(AValue));
end;

procedure TCastleConfig.SetDeleteFloat(const APath: string;
  const AValue, ADefaultValue: Float);
begin
  SetDeleteValue(APath, FloatToStrDot(AValue), FloatToStrDot(ADefaultValue));
end;

{ get/set Int64s ------------------------------------------------------------ }

function TCastleConfig.GetInt64(const APath: string;
  const ADefaultValue: Int64): Int64;
var
  ResultString: string;
begin
  ResultString := GetValue(APath, IntToStr(ADefaultValue));
  Result := StrToInt64Def(ResultString, ADefaultValue);
end;

function TCastleConfig.GetInt64(const APath: string): Int64;
begin
  Result := StrToInt64(GetStringNonEmpty(APath));
end;

procedure TCastleConfig.SetInt64(const APath: string;
  const AValue: Int64);
begin
  SetValue(APath, IntToStr(AValue));
end;

procedure TCastleConfig.SetDeleteInt64(const APath: string;
  const AValue, ADefaultValue: Int64);
begin
  SetDeleteValue(APath, IntToStr(AValue), IntToStr(ADefaultValue));
end;

{ get/set vectors ------------------------------------------------------------ }

const
  VectorComponentPaths: array [0..3] of string =
  ('/x', '/y', '/z', '/w');

function TCastleConfig.GetVector2(const APath: string;
  const ADefaultValue: TVector2): TVector2;
var
  I: Integer;
begin
  for I := 0 to ADefaultValue.Count - 1 do
    Result.Data[I] := GetFloat(APath + VectorComponentPaths[I], ADefaultValue.Data[I]);
end;

function TCastleConfig.GetVector2(const APath: string): TVector2;
var
  I: Integer;
begin
  for I := 0 to Result.Count - 1 do
    Result.Data[I] := GetFloat(APath + VectorComponentPaths[I]);
end;

procedure TCastleConfig.SetVector2(const APath: string;
  const AValue: TVector2);
var
  I: Integer;
begin
  for I := 0 to AValue.Count - 1 do
    SetFloat(APath + VectorComponentPaths[I], AValue.Data[I]);
end;

procedure TCastleConfig.SetDeleteVector2(const APath: string;
  const AValue, ADefaultValue: TVector2);
var
  I: Integer;
begin
  for I := 0 to AValue.Count - 1 do
    SetDeleteFloat(APath + VectorComponentPaths[I], AValue.Data[I], ADefaultValue.Data[I]);
end;

function TCastleConfig.GetVector3(const APath: string;
  const ADefaultValue: TVector3): TVector3;
var
  I: Integer;
begin
  for I := 0 to ADefaultValue.Count - 1 do
    Result.Data[I] := GetFloat(APath + VectorComponentPaths[I], ADefaultValue.Data[I]);
end;

function TCastleConfig.GetVector3(const APath: string): TVector3;
var
  I: Integer;
begin
  for I := 0 to Result.Count - 1 do
    Result.Data[I] := GetFloat(APath + VectorComponentPaths[I]);
end;

procedure TCastleConfig.SetVector3(const APath: string;
  const AValue: TVector3);
var
  I: Integer;
begin
  for I := 0 to AValue.Count - 1 do
    SetFloat(APath + VectorComponentPaths[I], AValue.Data[I]);
end;

procedure TCastleConfig.SetDeleteVector3(const APath: string;
  const AValue, ADefaultValue: TVector3);
var
  I: Integer;
begin
  for I := 0 to AValue.Count - 1 do
    SetDeleteFloat(APath + VectorComponentPaths[I], AValue.Data[I], ADefaultValue.Data[I]);
end;

function TCastleConfig.GetVector4(const APath: string;
  const ADefaultValue: TVector4): TVector4;
var
  I: Integer;
begin
  for I := 0 to ADefaultValue.Count - 1 do
    Result.Data[I] := GetFloat(APath + VectorComponentPaths[I], ADefaultValue.Data[I]);
end;

function TCastleConfig.GetVector4(const APath: string): TVector4;
var
  I: Integer;
begin
  for I := 0 to Result.Count - 1 do
    Result.Data[I] := GetFloat(APath + VectorComponentPaths[I]);
end;

procedure TCastleConfig.SetVector4(const APath: string;
  const AValue: TVector4);
var
  I: Integer;
begin
  for I := 0 to AValue.Count - 1 do
    SetFloat(APath + VectorComponentPaths[I], AValue.Data[I]);
end;

procedure TCastleConfig.SetDeleteVector4(const APath: string;
  const AValue, ADefaultValue: TVector4);
var
  I: Integer;
begin
  for I := 0 to AValue.Count - 1 do
    SetDeleteFloat(APath + VectorComponentPaths[I], AValue.Data[I], ADefaultValue.Data[I]);
end;

{ deprecated get/set on vectors ---------------------------------------------- }

function TCastleConfig.GetValue(const APath: string;
  const ADefaultValue: TVector2): TVector2;
var
  I: Integer;
begin
  for I := 0 to ADefaultValue.Count - 1 do
    Result.Data[I] := GetFloat(APath + VectorComponentPaths[I], ADefaultValue.Data[I]);
end;

procedure TCastleConfig.SetValue(const APath: string;
  const AValue: TVector2);
var
  I: Integer;
begin
  for I := 0 to AValue.Count - 1 do
    SetFloat(APath + VectorComponentPaths[I], AValue.Data[I]);
end;

procedure TCastleConfig.SetDeleteValue(const APath: string;
  const AValue, ADefaultValue: TVector2);
var
  I: Integer;
begin
  for I := 0 to AValue.Count - 1 do
    SetDeleteFloat(APath + VectorComponentPaths[I], AValue.Data[I], ADefaultValue.Data[I]);
end;

function TCastleConfig.GetValue(const APath: string;
  const ADefaultValue: TVector3): TVector3;
var
  I: Integer;
begin
  for I := 0 to ADefaultValue.Count - 1 do
    Result.Data[I] := GetFloat(APath + VectorComponentPaths[I], ADefaultValue.Data[I]);
end;

procedure TCastleConfig.SetValue(const APath: string;
  const AValue: TVector3);
var
  I: Integer;
begin
  for I := 0 to AValue.Count - 1 do
    SetFloat(APath + VectorComponentPaths[I], AValue.Data[I]);
end;

procedure TCastleConfig.SetDeleteValue(const APath: string;
  const AValue, ADefaultValue: TVector3);
var
  I: Integer;
begin
  for I := 0 to AValue.Count - 1 do
    SetDeleteFloat(APath + VectorComponentPaths[I], AValue.Data[I], ADefaultValue.Data[I]);
end;

function TCastleConfig.GetValue(const APath: string;
  const ADefaultValue: TVector4): TVector4;
var
  I: Integer;
begin
  for I := 0 to ADefaultValue.Count - 1 do
    Result.Data[I] := GetFloat(APath + VectorComponentPaths[I], ADefaultValue.Data[I]);
end;

procedure TCastleConfig.SetValue(const APath: string;
  const AValue: TVector4);
var
  I: Integer;
begin
  for I := 0 to AValue.Count - 1 do
    SetFloat(APath + VectorComponentPaths[I], AValue.Data[I]);
end;

procedure TCastleConfig.SetDeleteValue(const APath: string;
  const AValue, ADefaultValue: TVector4);
var
  I: Integer;
begin
  for I := 0 to AValue.Count - 1 do
    SetDeleteFloat(APath + VectorComponentPaths[I], AValue.Data[I], ADefaultValue.Data[I]);
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
      for I := 0 to ADefaultColor.Count - 1 do
        Result.Data[I] := Clamped(GetFloat(APath + ColorComponentPaths[I], ADefaultColor.Data[I]), 0.0, 1.0);
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
      for I := 0 to Result.Count - 1 do
        Result.Data[I] := Clamped(GetFloat(APath + ColorComponentPaths[I]), 0.0, 1.0);
    end;
  end;
end;

procedure TCastleConfig.SetColorRGB(const APath: string;
  const AColor: TCastleColorRGB);
var
  I: Integer;
begin
  SetValue(APath + HexPath, ColorRGBToHex(AColor));
  for I := 0 to AColor.Count - 1 do
    DeleteValue(APath + ColorComponentPaths[I]);
end;

procedure TCastleConfig.SetDeleteColorRGB(const APath: string;
  const AColor, ADefaultColor: TCastleColorRGB);
var
  I: Integer;
begin
  SetDeleteValue(APath + HexPath, ColorRGBToHex(AColor), ColorRGBToHex(ADefaultColor));
  for I := 0 to AColor.Count - 1 do
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
      for I := 0 to ADefaultColor.Count - 1 do
        Result.Data[I] := Clamped(GetFloat(APath + ColorComponentPaths[I], ADefaultColor.Data[I]), 0.0, 1.0);
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
      for I := 0 to Result.Count - 1 do
        Result.Data[I] := Clamped(GetFloat(APath + ColorComponentPaths[I]), 0.0, 1.0);
    end;
  end;
end;

procedure TCastleConfig.SetColor(const APath: string;
  const AColor: TCastleColor);
var
  I: Integer;
begin
  SetValue(APath + HexPath, ColorToHex(AColor));
  for I := 0 to AColor.Count - 1 do
    DeleteValue(APath + ColorComponentPaths[I]);
end;

procedure TCastleConfig.SetDeleteColor(const APath: string;
  const AColor, ADefaultColor: TCastleColor);
var
  I: Integer;
begin
  SetDeleteValue(APath + HexPath, ColorToHex(AColor), ColorToHex(ADefaultColor));
  for I := 0 to AColor.Count - 1 do
    DeleteValue(APath + ColorComponentPaths[I]);
end;

{ others --------------------------------------------------------------------- }

function TCastleConfig.PathElement(const APath: string;
  const RaiseExceptionWhenMissing: boolean): TDOMElement;
var
  SeekPos: Integer;
  PathComponent: string;
begin
  Result := Document.DocumentElement;
  SeekPos := 1;
  while Result <> nil do
  begin
    PathComponent := NextToken(APath, SeekPos, ['/']);
    if PathComponent = '' then break;
    Result := Result.ChildElement(PathComponent, false);
  end;

  if (Result = nil) and RaiseExceptionWhenMissing then
    raise Exception.CreateFmt('Missing element "%s" in file "%s"', [APath, URL]);
end;

function TCastleConfig.MakePathElement(const APath: string): TDOMElement;
var
  SeekPos: Integer;
  PathComponent: string;
  NewResult: TDOMElement;
begin
  Result := Document.DocumentElement;
  SeekPos := 1;
  { only exits by break, for consistency with PathElement implementation above }
  while true do
  begin
    PathComponent := NextToken(APath, SeekPos, ['/']);
    if PathComponent = '' then break;
    NewResult := Result.ChildElement(PathComponent, false);
    { create child if necessary }
    if NewResult = nil then
    begin
      NewResult := Document.CreateElement({$ifdef FPC}UTF8Decode({$endif}PathComponent{$ifdef FPC}){$endif});
      Result.AppendChild(NewResult);
    end;
    Result := NewResult;
  end;
end;

{$ifdef FPC}
function TCastleConfig.PathChildren(const APath: string;
  const ChildName: string): TDOMNodeList;
begin
  Result := PathElement(APath, true).GetElementsByTagName(UTF8Decode(ChildName));
end;
{$endif}


function TCastleConfig.PathChildrenIterator(const APath: string;
  const ChildName: string): TXMLElementIterator;
begin
  Result := PathElement(APath, true).ChildrenIterator(ChildName);
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
    Result := UTF8Encode(E.TextContent);
  { convert all to Unix-line endings }
  StringReplaceAllVar(Result, #13, '', false);
  { in case we're not on Unix, convert to current line endings }
  {$warnings off} { don't warn about dead code on OSes where NL = #10 }
  if #10 <> NL then
    StringReplaceAllVar(Result, #10, NL, false);
  {$warnings on}
end;

function TCastleConfig.GetMultilineText(const APath: string): string;
begin
  Result := GetMultilineText(APath, '');
  if Result = '' then
    raise EMissingAttribute.CreateFmt('Missing multi-line text context of element "%s" in XML file', [APath]);
end;

procedure TCastleConfig.NotModified;
begin
  FModified := false;
end;

procedure TCastleConfig.MarkModified;
begin
  FModified := true;
end;

function TCastleConfig.Document: TDOMDocument;
begin
  Result := Doc;
end;

{ loading and saving --------------------------------------------------------- }

procedure TCastleConfig.AddLoadListener(const Listener: TCastleConfigEvent);
begin
  if IsLoaded then
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

procedure TCastleConfig.Load;
begin
  Load(ApplicationConfig(ApplicationName + '.conf'));
end;

procedure TCastleConfig.Load(const AURL: string);
begin
  try
    URL := AURL; // use ancestor method to load
  except
    on E: Exception do
    begin
      WritelnWarning('UserConfig', 'User config in "' + AURL + '" corrupted (will load defaults): ' + E.Message);
      LoadEmpty(AURL);
      Exit;
    end;
  end;

  FOnLoad.ExecuteAll(Self);
  FLoaded := true;
  { This is used for various files (not just user preferences,
    also resource.xml files). Logging this may get talkative, but it's also
    useful for now... }
  WritelnLog('Config', 'Loaded configuration from "%s"', [AURL]);
end;

procedure TCastleConfig.Load(const Stream: TStream; const PretendURL: string);
begin
  try
    LoadFromStream(Stream, PretendURL); // use ancestor method to load
  except
    on E: Exception do
    begin
      WritelnWarning('UserConfig', 'User config in stream corrupted (will load defaults): ' + E.Message);
      LoadEmpty(PretendURL);
      Exit;
    end;
  end;

  FOnLoad.ExecuteAll(Self);
  FLoaded := true;
  WritelnLog('Config', 'Loaded configuration from stream, pretending the URL is "%s"', [PretendURL]);
end;

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

procedure TCastleConfig.Save;
begin
  FOnSave.ExecuteAll(Self);
  if Flush then //use ancestor method to save
    WriteLnLog('Config', 'Saving configuration to "%s"', [URIDisplay(URL)])
  else
  if URL <> '' then
    WriteLnLog('Config', 'No changes in config to save "%s"', [URIDisplay(URL)])
  else
    WriteLnWarning('Config', 'Configuration was not saved, because no URL is specified. Call TCastleConfig.Load or explicitly set TCastleConfig.URL before calling TCastleConfig.Save.');
end;

procedure TCastleConfig.Save(const Stream: TStream);
begin
  FOnSave.ExecuteAll(Self);
  SaveToStream(Stream); // use ancestor method to save
  WritelnLog('Config', 'Saving configuration to stream');
end;

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
