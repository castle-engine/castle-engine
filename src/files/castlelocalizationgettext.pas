{
  Copyright 2019-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Utilities for localizing CGE applications using GetText. }
unit CastleLocalizationGetText;

interface

uses SysUtils, Classes, GetText,
  CastleClassUtils, CastleUnicode;

type
  ETranslationEmptyId = class(Exception);

{ Generate text to insert to POT (PO template) to allow translating given text.
  Depending on MsgidFromId:
  @unorderedList(
    @item(@true (default):

      msgid is Id, msgstr is EnglishText in output.
      This looks non-standard, but is sometimes more comfortable:
      @unorderedList(
        @item EnglishText may be empty.
        @item EnglishText may be repeated many times in the same output file.
        @item Changing EnglishText later doesn't require updating all PO files, as EnglishText is not used as the key.
      )
    )

    @item(@false:

      msgid is EnglishText, msgstr is empty, and we place special comment in the output to preserve Id.
      This is more standard, following typical GetText usage,
      and some software UI may assume that msgid is an English text.
    )
  ) }
function GetTextPoEntry(const Context, Id, EnglishText: String;
  const MsgidFromId: Boolean = true): String;

{ Generate GetText PO file contents that can be used as a basis for translators
  to localize this component. The component may be a hierarchy of user interface
  and transformations, just like for @link(CastleComponentSerialize).

  A typical workflow of translation is:

  @orderedList(

    @item(Generate a file containing everything to translate in your components
      by calling something like
      @code(GenerateGetTextPo('castle-data:/gui/*.castle-user-interface');).)

    @item(Translate the PO files and generate MO files using GetText tools.)

    @item(In game, after determining user language,
      call e.g. @code(TranslateAllDesigns('castle-data:/translations/user_interface.ja.mo');)
      Where "ja" stands for Japanese.)
  )

  The argument of this routine can be a TComponent hierarchy,
  which can be a TCastleUserInterface or TCastleTransform hierarchy
  designed in the CGE editor or in code.

  The argument can also be an URL of a design (loaded using CastleSerializeComponent)
  or a mask (using wildcards * and ?) to load multiple designs.
  The design basename (part of URL without extension and path)
  is the prefix for all identifiers inside.

  The file will include all translatable properties in the given component hierarchy,
  with their current values (by convention in English).
  By convention you store this file as "xxx.pot" or ""xxx.en.po" and then
  translate it e.g. to "xxz.ja.po" which you can use to load Japanese translations
  by TranslateDesigns . }
function GenerateGetTextPo(const C: TComponent; const GroupName: String): String;
function GenerateGetTextPo(const UrlMask: String): String;

type
  { TMOFile descendant that allows iterating through all strings. }
  TCastleMOFile = class(TMOFile)
  private
    function GetKey(const AIndex: Cardinal): String;
    function GetValue(const AIndex: Cardinal): String;
  public
    constructor Create(const Stream: TStream);
    constructor Create(const Url: String);

    property Count: Cardinal read StringCount;
    property Keys[const AIndex: Cardinal]: String read GetKey;
    property Values[const AIndex: Cardinal]: String read GetValue;

    { Does the given id exist in MO file.
      Using it is much slower than just attempting to translate using
      @code(Translate) method, but allows to recognize when a key is missing
      (as opposed to when a key translation is empty). }
    function ContainsKey(const TranslationId: String): Boolean;
    function ContainsKeyWithContext(const Context, Id: String): Boolean;

    function TranslateWithContext(const Context, Id: String): String;
  end;

{ Load GetText MO file from and URL. }
function LoadGetTextMo(const Url: String): TCastleMOFile;
  deprecated 'use TCastleMOFile.Create';

{ Extract from MO file all unique characters in translated strings,
  add them to Characters. }
procedure AddTranslatedCharacters(const Url: String; const Characters: TUnicodeCharList);

{ Translate all future designs (component hierarchies) loaded using @link(CastleComponentSerialize)
  using the given GetText MO file.
  This is the simplest way to translate user interface designed using CGE Editor.

  The GetText PO file with suitable identifiers should be earlier generated using
  @link(GenerateGetTextPo), translated, and processed to MO format using GetText
  tools.

  Calling this routine again will override the effect of the previous call.
  That is, at a given time, only one MO file is "active" and automatically translates
  all the loaded designs.

  @seealso TranslateDesign }
procedure TranslateAllDesigns(const GetTextMoUrl: String);

{ Translate all possible properties in the given component hierarchy
  with given translation file. Missing translations will cause a warning in
  the log (indicating that your translation file is outdated and
  doesn't reflect all UI stuff anymore).

  If all your designs are loaded from files (using @link(CastleComponentSerialize))
  then usually it's more comfortable to use @link(TranslateAllDesigns)
  instead of this routine. This routine is however more flexible,
  and allows to translate any component hierarchy.

  @seealso TranslateAllDesigns }
procedure TranslateDesign(const C: TComponent; const GroupName: String; const GetTextMo: TCastleMOFile);

{ Use this instead of FPC TranslateResourceStrings.
  This supports URL to MO file, and works cross-platform. }
procedure CastleTranslateResourceStrings(const GetTextMoUrl: String);

implementation

uses CastleUtils, CastleStringUtils, CastleFindFiles, CastleComponentSerialize,
  CastleURIUtils, CastleLog, CastleDownload;

const
  PoContextDelimiter = #4;

{ GetTextPoEntry ------------------------------------------------------------- }

function GetTextPoEntry(const Context, Id, EnglishText: String;
  const MsgidFromId: Boolean): String;

  function StringToPo(const S: String): String;
  begin
    Result := S;
    // convert #13#10 -> #10, i.e. make line endings Unix
    Result := SDeleteChars(Result, [#13]);
    Result := SReplacePatterns(Result, ['\', #10, '"'], ['\\', '\n', '\"'], false);
    Result := '"' + Result + '"';
  end;

begin
  Result := '';

  { place Id as "reference" (usually source code filename,
    line number etc. when xgettext generates the file) }
  if Id = '' then
    raise ETranslationEmptyId.CreateFmt('Empty id for "%s"', [EnglishText]);
  if Pos(' ', Id) <> 0 then
    raise ETranslationEmptyId.CreateFmt('Translation id contains space "%s"', [Id]);

  if MsgidFromId then
  begin
    if Context <> '' then
      Result += 'msgctxt ' + StringToPo(Context) + NL;
    Result += 'msgid ' + StringToPo(Id) + NL;
    Result += 'msgstr ' + StringToPo(EnglishText) + NL;
  end else
  begin
    // When English text is empty, it is not translatable
    if EnglishText = '' then Exit;

    Result += '#: ' + Id + NL;

    if Context <> '' then
      Result += 'msgctxt ' + StringToPo(Context) + NL;
    Result += 'msgid ' + StringToPo(EnglishText) + NL;
    Result += 'msgstr ' + StringToPo('') + NL;
  end;

  Result += NL;
end;

{ GenerateGetTextPo(TComponent) and helper ----------------------------------------------- }

type
  TComponentHandler = class
    GroupName: String;
    Output: String;
    procedure TranslateProperty(const Sender: TCastleComponent;
      const PropertyName: String; var PropertyValue: String);
  end;

procedure TComponentHandler.TranslateProperty(const Sender: TCastleComponent;
  const PropertyName: String; var PropertyValue: String);
begin
  Assert(Sender.Name <> '');
  Assert(PropertyName <> '');
  Output := Output + GetTextPoEntry('castle-components',
    GroupName + '.' + Sender.Name + '.' + PropertyName,
    PropertyValue);
end;

function GenerateGetTextPo(const C: TComponent; const GroupName: String): String;
var
  Handler: TComponentHandler;
begin
  Check(GroupName <> '', 'GenerateGetTextPo requires non-empty GroupName');
  Handler := TComponentHandler.Create;
  try
    Handler.GroupName := GroupName;
    TranslateProperties(C, @Handler.TranslateProperty);
    Result := Handler.Output;
  finally FreeAndNil(Handler) end;
end;

{ GenerateGetTextPo(string) -------------------------------------------------- }

type
  TFilesHandler = class
    Output: String;
    procedure FoundFile(const FileInfo: TFileInfo; var StopSearch: boolean);
  end;

procedure TFilesHandler.FoundFile(const FileInfo: TFileInfo; var StopSearch: boolean);
var
  GroupName: String;
  C, COwner: TComponent;
begin
  COwner := TComponent.Create(nil);
  try
    C := ComponentLoad(FileInfo.URL, COwner);
    GroupName := DeleteURIExt(ExtractURIName(FileInfo.URL));
    Output := Output + GenerateGetTextPo(C, GroupName);
  finally FreeAndNil(COwner) end;
end;

function GenerateGetTextPo(const UrlMask: String): String;
var
  Handler: TFilesHandler;
  FilesCount: Cardinal;
begin
  Handler := TFilesHandler.Create;
  try
    FilesCount := FindFiles(UrlMask, false, @Handler.FoundFile, [ffRecursive]);
    WriteLnLog('Generated GetText PO for %d files matching "%s"', [FilesCount, UrlMask]);
    Result := Handler.Output;
  finally FreeAndNil(Handler) end;
end;

{ LoadGetTextMo -------------------------------------------------------------- }

function LoadGetTextMo(const Url: String): TCastleMOFile;
var
  S: TStream;
begin
  S := Download(Url);
  try
    Result := TCastleMOFile.Create(S);
  finally FreeAndNil(S) end;
end;

{ AddTranslatedCharacters ---------------------------------------------------- }

procedure AddTranslatedCharacters(const Url: String; const Characters: TUnicodeCharList);
var
  Mo: TCastleMOFile;
  I: Integer;
begin
  Mo := LoadGetTextMo(Url);
  try
    for I := 0 to Mo.Count - 1 do
      Characters.Add(Mo.Values[I]);
  finally FreeAndNil(Mo) end;
end;

{ TranslateAllDesigns -------------------------------------------------------- }

var
  TranslateAllDesignsMo: TCastleMOFile;

procedure TranslateDesignCallback(const C: TComponent; const GroupName: String);
begin
  if TranslateAllDesignsMo = nil then
    Exit; // in case this is called after finalization of this unit
  TranslateDesign(C, GroupName, TranslateAllDesignsMo);
end;

procedure TranslateAllDesigns(const GetTextMoUrl: String);
begin
  FreeAndNil(TranslateAllDesignsMo);
  TranslateAllDesignsMo := LoadGetTextMo(GetTextMoUrl);
  OnInternalTranslateDesign := @TranslateDesignCallback;
end;

{ TranslateDesign ------------------------------------------------------------ }

type
  TTranslateDesignHelper = class
    GroupName: String;
    Mo: TCastleMOFile;
    procedure TranslateProperty(const Sender: TCastleComponent;
      const PropertyName: String; var PropertyValue: String);
  end;

procedure TTranslateDesignHelper.TranslateProperty(const Sender: TCastleComponent;
  const PropertyName: String; var PropertyValue: String);
var
  TranslationId: String;
begin
  Assert(Sender.Name <> '');
  Assert(PropertyName <> '');
  TranslationId := 'castle-components' + PoContextDelimiter +
    GroupName + '.' + Sender.Name + '.' + PropertyName;
  if not Mo.ContainsKey(TranslationId) then
  begin
    WritelnWarning('Translation id "%s" is not present in the GetText MO file. Translation was done for an outdated UI file, add new keys to the GetText PO files and translate them and recreate MO file.',
      [TranslationId]);
    Exit;
  end;
  PropertyValue := Mo.Translate(TranslationId);
end;

procedure TranslateDesign(const C: TComponent; const GroupName: String; const GetTextMo: TCastleMOFile);
var
  Helper: TTranslateDesignHelper;
begin
  Helper := TTranslateDesignHelper.Create;
  try
    Helper.Mo := GetTextMo;
    Helper.GroupName := GroupName;
    TranslateProperties(C, @Helper.TranslateProperty);
  finally FreeAndNil(Helper) end;
end;

{ CastleTranslateResourceStrings --------------------------------------------- }

procedure CastleTranslateResourceStrings(const GetTextMoUrl: String);
var
  Mo: TCastleMOFile;
begin
  Mo := LoadGetTextMo(GetTextMoUrl);
  try
    TranslateResourceStrings(Mo);
  finally FreeAndNil(Mo) end;
end;

{ TCastleMOFile ------------------------------------------------------------ }

constructor TCastleMOFile.Create(const Stream: TStream);
begin
  inherited Create(Stream);
end;

constructor TCastleMOFile.Create(const Url: String);
var
  S: TStream;
begin
  S := Download(Url);
  try
    Create(S);
  finally FreeAndNil(S) end;
end;

function TCastleMOFile.GetKey(const AIndex: Cardinal): String;
begin
  Result := OrigStrings^[AIndex];
end;

function TCastleMOFile.GetValue(const AIndex: Cardinal): String;
begin
  Result := TranslStrings^[AIndex];
end;

function TCastleMOFile.ContainsKey(const TranslationId: String): Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Keys[I] = TranslationId then
      Exit(true);
  Result := false;
end;

function TCastleMOFile.ContainsKeyWithContext(const Context, Id: String): Boolean;
begin
  Result := ContainsKey(Context + PoContextDelimiter + Id);
end;

function TCastleMOFile.TranslateWithContext(const Context, Id: String): String;
begin
  Result := Translate(Context + PoContextDelimiter + Id);
end;

finalization
  FreeAndNil(TranslateAllDesignsMo);
end.
