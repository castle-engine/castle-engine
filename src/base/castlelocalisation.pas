unit CastleLocalisation;

{$I castleconf.inc}

interface

uses
  Classes, Generics.Collections, {$ifdef ANDROID}JNI,{$endif}
  CastleStringUtils;

type
  TLanguage = {$ifdef CASTLE_OBJFPC}specialize{$endif} TDictionary<String, String>;

type
  TCastleLocalisation = class
    protected
      const DefaultLanguage = 'en';
    protected
      FLanguage: TLanguage;
      FLanguageURL: String;
      function Get(AKey: String): String;
    public
      constructor Create; virtual;
      destructor Destroy; override;
      function SystemLanguage(const ADefaultLanguage: String = DefaultLanguage): String;
      procedure LoadLanguage(const ALanguageURL: String);
    public
      property Items[AKey: String]: String read Get; default;
  end;

{$ifdef ANDROID}
  { Export this function from your Android library. }
  function Java_net_sourceforge_castleengine_MainActivity_jniLanguage(Env: PJNIEnv; This: jobject; JavaToNative: jstring): jstring; cdecl;
{$endif}

var
  Localisation: TCastleLocalisation; //Singleton.

implementation

{$warnings off}
  uses
    SysUtils, StrUtils, DOM, XMLRead, {$ifdef MSWINDOWS}Windows,{$endif}
    CastleXMLUtils, CastleURIUtils, CastleUtils, CastleDownload;
{$warnings on}

var
  MobileSystemLanguage: String;

{$ifdef ANDROID}
  function Java_net_sourceforge_castleengine_MainActivity_jniLanguage(Env: PJNIEnv; This: jobject; JavaToNative: jstring): jstring; cdecl;
  var
    JavaToNativeStr: PChar;
    Dummy: JBoolean;
  begin
    Result := Env^^.NewStringUTF(Env, nil);

    if (JavaToNative <> nil) and (Env^^.GetStringUTFLength(Env, JavaToNative) <> 0) then
    begin
      Dummy := 0;
      JavaToNativeStr := Env^^.GetStringUTFChars(Env, JavaToNative,{$ifdef VER2}Dummy{$else}@Dummy{$endif});
      try
        MobileSystemLanguage := AnsiString(JavaToNativeStr); // will copy characters
      finally
        Env^^.ReleaseStringUTFChars(Env, JavaToNative, JavaToNativeStr) end;
    end;
  end;
{$endif ANDROID}

//////////////////////////
//Constructor/Destructor//
//////////////////////////

constructor TCastleLocalisation.Create;
begin
  FLanguage := TLanguage.Create;
end;

destructor TCastleLocalisation.Destroy;
begin
  FreeAndNil(FLanguage);
end;

/////////////////////
//Private/Protected//
/////////////////////

function TCastleLocalisation.Get(AKey: String): String;
begin
  Result := AKey; //When no translation is found, return the key.
  FLanguage.TryGetValue(AKey, Result);
end;

//////////////
////Public////
//////////////

function TCastleLocalisation.SystemLanguage(const ADefaultLanguage: String = DefaultLanguage): String;
  {$ifdef MSWINDOWS}
    function GetLocaleInformation(Flag: integer): string;
    var
      pcLCA: array[0..20] of char;
    begin
      if (GetLocaleInfo(LOCALE_SYSTEM_DEFAULT, Flag, pcLCA, 19) <= 0) then
      begin
        pcLCA[0] := #0;
      end;
      Result := pcLCA;
    end;
  {$endif}
begin
  {$ifdef MSWINDOWS}
    Result := GetLocaleInformation(LOCALE_SISO639LANGNAME);
  {$else}
    {$ifdef ANDROID}
      Result := MobileSystemLanguage;
    {$else}
      Result := Copy(GetEnvironmentVariable('LANG'), 1, 2);
    {$endif}
  {$endif}

  if Result = '' then
    Result := ADefaultLanguage;
end;

procedure TCastleLocalisation.LoadLanguage(const ALanguageURL: String);
var
  FileURLAbsolute: string;
  Stream: TStream;
  LanguageXML: TXMLDocument;
  I: TXMLElementIterator;
  LOnUpdateLocalisationEvent: TOnUpdateLocalisationEvent;
begin
  if FLanguageURL = ALanguageURL then Exit;
  FLanguageURL := ALanguageURL;

  FLanguage.Clear;

  if ALanguageURL = '' then Exit; //If there's no language XML file, then that's it, no more localisation.

  FileURLAbsolute := AbsoluteURI(ALanguageURL); //This should be an absolute URL so we doesn't depend on the current directory.

  Stream := Download(FileURLAbsolute);
  try
    ReadXMLFile(LanguageXML, Stream, FileURLAbsolute);
  finally
    Stream.Free;
  end;

  try
    Check(LanguageXML.DocumentElement.TagName = 'strings', 'Root node of local/index.xml must be <strings>');

    I := LanguageXML.DocumentElement.ChildrenIterator;
    try
      while I.GetNext do
      begin
        Check(I.Current.TagName = 'string', 'Each child of local/index.xml root node must be the <string> element');

        FLanguage.AddOrSetValue(I.Current.AttributeString('key'), I.Current.AttributeString('value'));
      end;
    finally
      I.Free;
    end;
  finally
    LanguageXML.Free;
  end;

end;

initialization
  Localisation := TCastleLocalisation.Create;

finalization
  FreeAndNil(Localisation);

end.