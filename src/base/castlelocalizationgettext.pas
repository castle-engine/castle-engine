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

uses SysUtils, Classes,
  CastleClassUtils;

type
  ETranslationEmptyId = class(Exception);

{ Generate text to insert to POT (PO template) to allow translating given text.
  Depending on MsgidFromId:
  @unoderedList(
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

implementation

uses CastleUtils, CastleStringUtils, CastleFindFiles, CastleComponentSerialize,
  CastleURIUtils, CastleLog;

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

end.
