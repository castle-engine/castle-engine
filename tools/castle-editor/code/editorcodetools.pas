{
  Copyright 2021-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Utilities to operate on Pascal source code. }
unit EditorCodeTools;

interface

uses SysUtils,
  ToolManifest;

{ Find unit where view initialization takes place.
  Returns '' if not found.
  Returns relative filename (relative to project path) if found. }
function FindUnitToInitializeView(const Manifest: TCastleManifest): String;

{ Add view initialization to given unit.
  Assumes that it was a unit returned by FindUnitToInitializeView,
  so it passed basic checks implemented there.
  Raises exception if unit cannot be modified for some reason
  (e.g. it was broken in the meantime, between
  FindUnitToInitializeView and AddInitializeView calls,
  or we have no permissions to write there). }
procedure AddInitializeView(const UnitFileAbsolute: String;
  const ViewUnitName, ViewClassName, ViewVariableName: String);

implementation

uses CastleStringUtils, CastleFilesUtils, CastleURIUtils, CastleLog, CastleUtils;

const
  CommentViewCreateBegin = '{$region ''Castle View Creation''}';
  CommentViewCreateEnd   = '{$endregion ''Castle View Creation''}';
  CommentView2CreateBegin = '{$region ''Castle State Creation''}';
  CommentView2CreateEnd   = '{$endregion ''Castle State Creation''}';
  CommentInitializationUsesBegin = '{$region ''Castle Initialization Uses''}';
  CommentInitializationUsesEnd   = '{$endregion ''Castle Initialization Uses''}';

function CheckUnitToInitializeView(const FileName: String): Boolean;
var
  Content: String;
begin
  try
    Content := FileToString(FilenameToURISafe(FileName));
  except
    on E: Exception do
    begin
      WritelnWarning('Could not open main unit to initialize views "%s": %s', [
        FileName,
        E.Message
      ]);
      Exit(false);
    end;
  end;

  Result :=
    ( (Pos(CommentViewCreateBegin, Content) <> 0) or (Pos(CommentView2CreateBegin, Content) <> 0) ) and
    ( (Pos(CommentViewCreateEnd, Content) <> 0) or (Pos(CommentView2CreateEnd, Content) <> 0) ) and
    (Pos(CommentInitializationUsesBegin, Content) <> 0) and
    (Pos(CommentInitializationUsesEnd, Content) <> 0);
end;

function FindUnitToInitializeView(const Manifest: TCastleManifest): String;
var
  UnitNames: TCastleStringList;
  AUnitName, UnitFileNameAbsolute, UnitFileNameRelative: String;
begin
  UnitNames := CreateTokens(Manifest.GameUnits, WhiteSpaces + [',']);
  try
    for AUnitName in UnitNames do
    begin
      UnitFileNameAbsolute := Manifest.SearchPascalUnit(AUnitName);
      if (UnitFileNameAbsolute <> '') and
         CheckUnitToInitializeView(UnitFileNameAbsolute) then
      begin
        UnitFileNameRelative := ExtractRelativePath(Manifest.Path, UnitFileNameAbsolute);
        Exit(UnitFileNameRelative);
      end;
    end;

    Result := ''; // not found
  finally FreeAndNil(UnitNames) end;
end;

procedure AddInitializeView(const UnitFileAbsolute: String;
  const ViewUnitName, ViewClassName, ViewVariableName: String);
var
  UnitFileUrl: String;
  Content: String;
  P: SizeInt;
begin
  UnitFileUrl := FilenameToURISafe(UnitFileAbsolute);
  Content := FileToString(UnitFileUrl);

  P := Pos(CommentViewCreateEnd, Content);
  if P = 0 then
    P := Pos(CommentView2CreateEnd, Content);
  if P = 0 then
    raise Exception.CreateFmt('Cannot modify unit file "%s", does not contain comment "%s".', [
      UnitFileAbsolute,
      CommentViewCreateEnd
    ]);
  Insert(Format('%s := %s.Create(Application);' + NL + '  ', [
    ViewVariableName,
    ViewClassName
  ]), Content, P);

  P := Pos(CommentInitializationUsesEnd, Content);
  if P = 0 then
    raise Exception.CreateFmt('Cannot modify unit file "%s", does not contain comment "%s".', [
      UnitFileAbsolute,
      CommentInitializationUsesEnd
    ]);
  Insert(Format(', %s' + NL + '  ', [
    ViewUnitName
  ]), Content, P);

  StringToFile(UnitFileUrl, Content);
end;

end.
