// -*- compile-command: "castle-engine compile --mode=debug && castle-engine run" -*-
{
  Copyright 2021-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Check Castle Game Engine Lazarus (.lpk) and Delphi (.dpk, .dproj) packages.
  See README.md for more description. }

uses SysUtils, DOM,
  CastleXmlUtils, CastleUtils, CastleFindFiles, CastleStringUtils, CastleParameters,
  CastleLog, CastleApplicationProperties, CastleDownload,
  PackageUtils;

var
  CgePath: String = '../../../';
  CgePathExpanded: String;
  HasWarnings: Boolean = false;

const
  TryFixing = false;

type
  EInvalidPackage = class(Exception);

{ Report warning.
  Continue the work, but the test will exit with non-zero status signalling invalid package.
  This way we report many problems in one go. }
procedure PackageWarning(const S: String; const Args: array of const);
begin
  WriteLnWarning(Format(S, Args));
  HasWarnings := true;
end;

{ TPackage ------------------------------------------------------------------- }

type
  { Abstract class that represents Lazarus or Delphi package.

    Descendants:
    - Must implement constructor to read PackageFileName file and fill Files.
      May also extend ConsideredFilesMask.
    - May override ProposeFix. }
  TPackage = class
  strict private
    RequiredFilesList: TCastleStringList;
    FPackageFileName: String;
    procedure GatherRequiredFiles(const FileInfo: TFileInfo; var StopSearch: Boolean);
    procedure ExcludeFromRequiredFiles(const FileInfo: TFileInfo; var StopSearch: Boolean);
  protected
    { List of filenames.
      Relative to CGE root.
      Use only slash (/) as path separator, for simplicity -- convert backslashes
      to slashes when filling this list. }
    Files: TCastleStringList;

    { Filename masks (like *.pas) that must be present in packages
      if found in CGE sources.
      By default this contains just *.pas, and this is good enough for Delphi packages
      that only include units.
      For Lazarus packages, they should include also *.inc and some other files,
      so descendant TLazarusPackage extends this. }
    ConsideredFilesMask: TCastleStringList;

    { If TryFixing, this will overwrite PackageFileName adding the missing units.

      This automatic fix is not perfect, so beware! Known issues:

      - It is implemented now only for Lazarus packages, not Delphi.
      - It only adds missing files. Doesn't remove files that should not be in package.
      - It can mess some initial LPK XML stuff, causing unnecessary edits. Revert them.
      - Generated UnitName follows filename, so it is all lowercase.
      - It doesn't add to units (because it doesn't know which are platform-specific)
        <AddToUsesPkgSection Value="False"/>

      Descendants: you can override this.
      Do not call inherited when overriding (since this implementation makes
      warning).
      You can assume when this is called that TryFixing = true.
    }
    procedure ProposeFix(const MissingFiles: TCastleStringList); virtual;
  public
    property PackageFileName: String read FPackageFileName;
    constructor Create(const APackageFileName: String);
    destructor Destroy; override;

    { Check that package
      - contains all files from RequiredFiles (except ExcludedFromRequiredFiles)
      - doesn't contain any files not in RequiredFiles (except ExcludedFromRequiredFiles) + OptionalFiles
    }
    procedure CheckFiles(const RequiredFiles, ExcludedFromRequiredFiles, OptionalFiles: array of String);
  end;

constructor TPackage.Create(const APackageFileName: String);
begin
  inherited Create;
  Files := TCastleStringList.Create;
  Files.CaseSensitive := FileNameCaseSensitive;

  ConsideredFilesMask := TCastleStringList.Create;
  //ConsideredFilesMask.CaseSensitive := FileNameCaseSensitive; // doesn't matter
  ConsideredFilesMask.Append('*.pas');

  FPackageFileName := APackageFileName;
end;

destructor TPackage.Destroy;
begin
  FreeAndNil(Files);
  FreeAndNil(ConsideredFilesMask);
  inherited;
end;

procedure TPackage.GatherRequiredFiles(const FileInfo: TFileInfo; var StopSearch: Boolean);
var
  FileName, CgePrefix: String;
begin
  FileName := FileInfo.AbsoluteName;
  FileName := SReplaceChars(FileName, PathDelim, '/'); // replace backslashes with slashes on Windows

  CgePrefix := CgePathExpanded;
  if not IsPrefix(CgePrefix, FileName, not FileNameCaseSensitive) then
    PackageWarning('All found files must be in CGE root, invalid: %s', [FileName]);
  FileName := PrefixRemove(CgePrefix, FileName, not FileNameCaseSensitive);

  RequiredFilesList.Append(FileName);
end;

procedure TPackage.ExcludeFromRequiredFiles(const FileInfo: TFileInfo; var StopSearch: Boolean);
var
  FileName, CgePrefix: String;
  I: Integer;
begin
  FileName := FileInfo.AbsoluteName;
  FileName := SReplaceChars(FileName, PathDelim, '/'); // replace backslashes with slashes on Windows

  CgePrefix := CgePathExpanded;
  if not IsPrefix(CgePrefix, FileName, not FileNameCaseSensitive) then
    PackageWarning('All found files must be in CGE root, invalid: %s', [FileName]);
  FileName := PrefixRemove(CgePrefix, FileName, not FileNameCaseSensitive);

  I := RequiredFilesList.IndexOf(FileName);
  if I = -1 then
    raise EInternalError.Create('File found in ExcludeFromRequiredFiles, but not in RequiredFiles: ' + FileName);
  RequiredFilesList.Delete(I);
end;

procedure TPackage.CheckFiles(const RequiredFiles, ExcludedFromRequiredFiles, OptionalFiles: array of String);

  function InsideOptionalFiles(const FileName: String): Boolean;
  var
    I: Integer;
  begin
    for I := 0 to High(OptionalFiles) do
      if IsPrefix(OptionalFiles[I] + PathDelim, FileName, not FileNameCaseSensitive) or
         IsPrefix(OptionalFiles[I] + '/', FileName, not FileNameCaseSensitive) then // accept / also on Windows
        Exit(true);
    Result := false;
  end;

var
  I, FilesIndex: Integer;
  FindPath, Mask: String;
  MissingFiles: TCastleStringList;
begin
  RequiredFilesList := TCastleStringList.Create;
  RequiredFilesList.CaseSensitive := FileNameCaseSensitive;
  for I := 0 to High(RequiredFiles) do
  begin
    FindPath := CgePathExpanded + RequiredFiles[I] + PathDelim;
    for Mask in ConsideredFilesMask do
      FindFiles(FindPath, Mask, false, @GatherRequiredFiles, [ffRecursive]);
  end;
  Writeln('Found required files on disk: ', RequiredFilesList.Count);

  { remove ExcludedFromRequiredFiles }
  for I := 0 to High(ExcludedFromRequiredFiles) do
  begin
    FindPath := CgePathExpanded + ExcludedFromRequiredFiles[I] + PathDelim;
    for Mask in ConsideredFilesMask do
      FindFiles(FindPath, Mask, false, @ExcludeFromRequiredFiles, [ffRecursive]);
  end;
  Writeln('Required files after removing exclusions: ', RequiredFilesList.Count);

  { verify that all RequiredFilesList are in package, remove them from Files }
  MissingFiles := TCastleStringList.Create;
  try
    for I := 0 to RequiredFilesList.Count - 1 do
    begin
      FilesIndex := Files.IndexOf(RequiredFilesList[I]);
      if FilesIndex = -1 then
      begin
        PackageWarning('Required file "%s" is not present in package "%s"', [
          RequiredFilesList[I],
          FPackageFileName
        ]);
        MissingFiles.Add(RequiredFilesList[I]);
      end else
        Files.Delete(FilesIndex);
    end;

    if (MissingFiles.Count <> 0) and TryFixing then
      ProposeFix(MissingFiles);
  finally FreeAndNil(MissingFiles) end;

  { verify that rest of Files is in OptionalFiles }
  for I := 0 to Files.Count - 1 do
    if not InsideOptionalFiles(Files[I]) then
      PackageWarning('File "%s" in package "%s" is neither in OptionalFiles or RequiredFiles of this package, or it is in package but not existing on disk', [
        Files[I],
        FPackageFileName
      ]);

  FreeAndNil(RequiredFilesList);
end;

procedure TPackage.ProposeFix(const MissingFiles: TCastleStringList);
begin
  PackageWarning('Automatic fixing not implemented for this package type %s', [
    ClassName
  ]);
end;

{ TLazarusPackage ------------------------------------------------------------ }

type
  { Represents Lazarus package (.lpk). }
  TLazarusPackage = class(TPackage)
  protected
    procedure ProposeFix(const MissingFiles: TCastleStringList); override;
  public
    constructor Create(const APackageFileName: String);
  end;

constructor TLazarusPackage.Create(const APackageFileName: String);
var
  Doc: TXMLDocument;
  FilesElement, FileElement: TDOMElement;
  I, FilesCount: Cardinal;
  FileName: String;
begin
  inherited;

  ConsideredFilesMask.Append('*.inc');
  ConsideredFilesMask.Append('*.image_data');
  ConsideredFilesMask.Append('*.lrs');

  Doc := URLReadXML(PackageFileName);
  try
    FilesElement := Doc.DocumentElement.Child('Package').Child('Files');
    FilesCount := FilesElement.AttributeCardinal('Count');
    for I := 1 to FilesCount do
    begin
      FileElement := FilesElement.Child('Item' + IntToStr(I));
      FileName := FileElement.Child('Filename').AttributeString('Value');
      if not IsPrefix('../', FileName, not FileNameCaseSensitive) then
        PackageWarning('All filenames in lpk must be in CGE root, invalid: %s', [FileName]);
      FileName := PrefixRemove('../', FileName, not FileNameCaseSensitive);
      Files.Append(FileName);
    end;
  finally FreeAndNil(Doc) end;

  Writeln(Format('LPK %s: %d files', [PackageFileName, Files.Count]));
end;

procedure TLazarusPackage.ProposeFix(const MissingFiles: TCastleStringList);
var
  Doc: TXMLDocument;
  FilesElement, FileElement,
    FileFilenameElement, FileTypeElement, FileUnitNameElement: TDOMElement;
  FilesCount: Cardinal;
  MissingFile: String;
begin
  Assert(TryFixing);

  Doc := URLReadXML(PackageFileName);
  try
    FilesElement := Doc.DocumentElement.Child('Package').Child('Files');
    FilesCount := FilesElement.AttributeCardinal('Count');
    for MissingFile in MissingFiles do
    begin
      Inc(FilesCount); // ItemXxx numbering is 1-based, so increment FilesCount first
      FileElement := FilesElement.CreateChild('Item' + IntToStr(FilesCount));
      FileFilenameElement := FileElement.CreateChild('Filename');
      FileFilenameElement.AttributeSet('Value', '../' + MissingFile);
      if ExtractFileExt(MissingFile) = '.inc' then
      begin
        FileTypeElement := FileElement.CreateChild('Type');
        FileTypeElement.AttributeSet('Value', 'Include');
      end;
      if ExtractFileExt(MissingFile) = '.pas' then
      begin
        FileUnitNameElement := FileElement.CreateChild('UnitName');
        FileUnitNameElement.AttributeSet('Value', DeleteFileExt(ExtractFileName(MissingFile)));
      end;
    end;
    FilesElement.AttributeSet('Count', FilesCount);
    URLWriteXML(Doc, PackageFileName);
  finally FreeAndNil(Doc) end;
end;

{ TDelphiPackage ------------------------------------------------------------ }

type
  { Represents Delphi package (.dpk and .dproj, PackageFileName should point to .dpk). }
  TDelphiPackage = class(TPackage)
  public
    constructor Create(const APackageFileName: String);
  end;

constructor TDelphiPackage.Create(const APackageFileName: String);

  procedure ReadDpk;
  var
    Reader: TTextReader;
    Line, FoundFileName: String;
    Matches: TCastleStringList;
  begin
    Matches := TCastleStringList.Create;
    try
      Reader := TTextReader.Create(PackageFileName);
      try
        while not Reader.Eof do
        begin
          Line := Reader.Readln;
          Matches.Clear;
          if StringMatchesRegexp(Line, '^ (.*) in ''(.*)'',$', Matches) then
          begin
            Check(Matches.Count = 3);
            FoundFileName := Matches[2];

            // replace backslashes with slashes on Windows
            FoundFileName := SReplaceChars(FoundFileName, PathDelim, '/');

            // strip prefix, to make it relative to CGE root
            if not IsPrefix('../../', FoundFileName, not FileNameCaseSensitive) then
              PackageWarning('All filenames in dpk must be in CGE root, invalid: %s', [FoundFileName]);
            FoundFileName := PrefixRemove('../../', FoundFileName, not FileNameCaseSensitive);

            Files.Append(FoundFileName);
          end;
        end;
      finally FreeAndNil(Reader) end;
    finally FreeAndNil(Matches) end;
  end;

begin
  inherited;

  ReadDpk;
  // ReadDproj; TODO - read and make sure matches DPK

  Writeln(Format('DPK (and DPROJ) %s: %d files', [PackageFileName, Files.Count]));
end;

{ main routine --------------------------------------------------------------- }

var
  Package: TPackage;
begin
  ApplicationProperties.ApplicationName := 'check_packages';
  ApplicationProperties.Version := CastleEngineVersion;
  ApplicationProperties.OnWarning.Add(@ApplicationProperties.WriteWarningOnConsole);

  Parameters.CheckHighAtMost(1);
  if Parameters.High = 1 then
    CgePath := Parameters[1];

  CgePathExpanded := InclPathDelim(ExpandFileName(CgePath));
  CgePathExpanded := SReplaceChars(CgePathExpanded, PathDelim, '/'); // replace backslashes with slashes on Windows
  Writeln('Checking CGE in directory: ', CgePathExpanded);

  Package := TLazarusPackage.Create(CgePathExpanded + 'packages' + PathDelim + 'castle_base.lpk');
  try
    Package.CheckFiles([
      'src/common_includes',
      'src/transform',
      'src/audio',
      'src/base',
      'src/base_rendering',
      'src/castlescript',
      'src/files',
      'src/fonts',
      'src/images',
      'src/physics',
      'src/services',
      'src/ui',
      'src/scene',
      'src/deprecated_units'
    ],
    [
      'src/base/android',
      'src/files/indy'
    ],
    [
      'src/vampyre_imaginglib'
    ]);
  finally FreeAndNil(Package) end;

  Package := TLazarusPackage.Create(CgePathExpanded + 'packages' + PathDelim + 'castle_window.lpk');
  try
    Package.CheckFiles([
      'src/window'
    ],
    [ ],
    [ ]);
  finally FreeAndNil(Package) end;

  Package := TLazarusPackage.Create(CgePathExpanded + 'packages' + PathDelim + 'alternative_castle_window_based_on_lcl.lpk');
  try
    Package.CheckFiles([
      'src/window'
    ],
    [ ],
    [ ]);
  finally FreeAndNil(Package) end;

  Package := TLazarusPackage.Create(CgePathExpanded + 'packages' + PathDelim + 'castle_components.lpk');
  try
    Package.CheckFiles([
      'src/lcl'
    ],
    [],
    [ ]);
  finally FreeAndNil(Package) end;

  Package := TLazarusPackage.Create(CgePathExpanded + 'packages' + PathDelim + 'castle_indy.lpk');
  try
    Package.CheckFiles([
      'src/files/indy'
    ],
    [ ],
    [ ]);
  finally FreeAndNil(Package) end;

  Package := TLazarusPackage.Create(CgePathExpanded + 'packages' + PathDelim + 'castle_editor_components.lpk');
  try
    Package.CheckFiles([
      'tools/castle-editor/components'
    ],
    [
      'tools/castle-editor/components/mbColorLib/examples'
    ],
    [ ]);
  finally FreeAndNil(Package) end;

  Package := TDelphiPackage.Create(CgePathExpanded + 'packages' + PathDelim + 'delphi' + PathDelim + 'castle_engine.dpk');
  try
    Package.CheckFiles([
      'src/common_includes',
      'src/transform',
      'src/audio',
      'src/base',
      'src/base_rendering',
      'src/castlescript',
      'src/files',
      'src/fonts',
      'src/images',
      'src/physics',
      'src/services',
      'src/ui',
      'src/scene',
      // Delphi specific:
      'src/delphi',
      'src/compatibility/delphi-only'
      // TODO: not in package, but maybe they should be?
      // 'src/deprecated_units'
    ],
    [
      'src/base/android',
      'src/files/indy'
    ],
    [
      'src/vampyre_imaginglib'
    ]);
  finally FreeAndNil(Package) end;

  if HasWarnings then
  begin
    Writeln('Some package problems reported above, exiting with status 1');
    Halt(1);
  end else
    Writeln('All packages OK');
end.
