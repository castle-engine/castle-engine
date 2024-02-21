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

program check_packages;

{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

{ Check Castle Game Engine Lazarus (.lpk) and Delphi (.dpk, .dproj) packages.
  See README.md for more description. }

uses SysUtils, DOM, Classes,
  CastleXmlUtils, CastleUtils, CastleFindFiles, CastleStringUtils, CastleParameters,
  CastleLog, CastleApplicationProperties, CastleDownload,
  PackageUtils;

var
  CgePath: String = '../../../';
  CgePathExpanded: String;
  WarningsCount: Cardinal = 0;

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
  Inc(WarningsCount);
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
      You can assume when this is called that TryFixing = true. }
    procedure ProposeFix(const MissingFiles: TCastleStringList); virtual;
  public
    property PackageFileName: String read FPackageFileName;
    constructor Create(const APackageFileName: String);
    destructor Destroy; override;

    { Check that package contains all files from
      RequiredFiles (except ExcludedFromRequiredFiles).

      Moreover, check it doesn't contain anything additional,
      except OptionalFiles.

      All the lists can contain

      - directories, end with / (in which case all Pascal files
        in this directory are considered,  following ConsideredFilesMask,
        by default just *.pas)

      - or mask for files (to indicate a specific file, or files
        if you use wildcards * and ?).

      Use only slash, never backslash (including PathDelim which may be backslash
      on Windows), as path separator.
      We make things easier at certain places in this implementation,
      by just working only with slashes. }
    procedure CheckFiles(const RequiredFiles, ExcludedFromRequiredFiles,
      OptionalFiles: array of String);
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
  FileName := SReplaceChars(FileName, '\', '/'); // replace backslashes with slashes

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
  FileName := SReplaceChars(FileName, '\', '/'); // replace backslashes with slashes

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
    begin
      if Pos('\', OptionalFiles[I]) <> 0 then
        raise Exception.CreateFmt('OptionalFiles must use only slashes, not backslashes: %s', [OptionalFiles[I]]);

      if IsPrefix(OptionalFiles[I], FileName, not FileNameCaseSensitive) then
        Exit(true);
    end;
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
    if Pos('\', RequiredFiles[I]) <> 0 then
      raise Exception.CreateFmt('RequiredFiles must use only slashes, not backslashes: %s', [RequiredFiles[I]]);

    FindPath := CgePathExpanded + RequiredFiles[I];
    if IsSuffix('/', FindPath) then
    begin
      for Mask in ConsideredFilesMask do
        FindFiles(FindPath, Mask, false, {$ifdef FPC}@{$endif} GatherRequiredFiles, [ffRecursive]);
    end else
      // just use this mask with FindFiles, to find matching files
      FindFiles(FindPath, false, {$ifdef FPC}@{$endif} GatherRequiredFiles, [ffRecursive]);
  end;
  Writeln('Found required files on disk: ', RequiredFilesList.Count);

  { remove ExcludedFromRequiredFiles }
  for I := 0 to High(ExcludedFromRequiredFiles) do
  begin
    if Pos('\', ExcludedFromRequiredFiles[I]) <> 0 then
      raise Exception.CreateFmt('ExcludedFromRequiredFiles must use only slashes, not backslashes: %s', [ExcludedFromRequiredFiles[I]]);

    FindPath := CgePathExpanded + ExcludedFromRequiredFiles[I];
    if IsSuffix('/', FindPath) then
    begin
      for Mask in ConsideredFilesMask do
        FindFiles(FindPath, Mask, false, {$ifdef FPC}@{$endif} ExcludeFromRequiredFiles, [ffRecursive]);
    end else
      // just use this mask with FindFiles, to find matching files
      FindFiles(FindPath, false, {$ifdef FPC}@{$endif} ExcludeFromRequiredFiles, [ffRecursive]);
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

const
  { Delphi packages may have various default platforms.
    For ease of installation, we require it to be Win32
    (the platform you need to install in Delphi IDE). }
  RequiredDefaultPlatform = 'Win32';

type
  { Represents Delphi package (.dpk and .dproj, PackageFileName should point to .dpk). }
  TDelphiPackage = class(TPackage)
  strict private
    FDefaultPlatform: String;
  public
    property DefaultPlatform: String read FDefaultPlatform;
    constructor Create(const APackageFileName: String);
  end;

constructor TDelphiPackage.Create(const APackageFileName: String);

  function FixFileNameFromPackage(const FileName: String): String;
  begin
    Result := FileName;

    // replace backslashes with slashes
    Result := SReplaceChars(Result, '\', '/');

    // strip prefix, to make it relative to CGE root
    if not IsPrefix('../../', Result, not FileNameCaseSensitive) then
      PackageWarning('All filenames in dpk must be in CGE root, invalid: %s', [Result]);
    Result := PrefixRemove('../../', Result, not FileNameCaseSensitive);
  end;

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
          if StringMatchesRegexp(Line, '^ (.*) in ''(.*)''', Matches) then
          begin
            Check(Matches.Count = 3);
            FoundFileName := Matches[2];
            FoundFileName := FixFileNameFromPackage(FoundFileName);
            Files.Append(FoundFileName);
          end;
        end;
      finally FreeAndNil(Reader) end;
    finally FreeAndNil(Matches) end;

    Writeln(Format('DPK %s: %d files', [PackageFileName, Files.Count]));
  end;

  procedure ReadDefaultPlatformFromDproj(const DocumentRoot: TDOMElement);
  var
    PropertyGroupIterator: TXMLElementIterator;
    PropertyGroupElement: TDOMElement;
  begin
    { Choose first PropertyGroup from the XML document }
    PropertyGroupIterator := DocumentRoot.ChildrenIterator('PropertyGroup');
    if not PropertyGroupIterator.GetNext then
      raise EInvalidPackage.Create('No PropertyGroup in DPROJ file');
    PropertyGroupElement := PropertyGroupIterator.Current;

    FDefaultPlatform := PropertyGroupElement.Child('Platform').TextData;
    if DefaultPlatform <> RequiredDefaultPlatform then
      PackageWarning('DefaultPlatform is "%s", but we require "%s"', [
        DefaultPlatform,
        RequiredDefaultPlatform
      ]);
  end;

  procedure ReadDproj;
  var
    Doc: TXMLDocument;
    ItemGroupElement, DccReferenceElement: TDOMElement;
    I: TXMLElementIterator;
    FileName, DprojFileName: String;
    AltFiles: TCastleStringList;
  begin
    AltFiles := TCastleStringList.Create;
    try
      DprojFileName := ChangeFileExt(PackageFileName, '.dproj');
      Doc := UrlReadXml(DprojFileName);
      try
        ItemGroupElement := Doc.DocumentElement.Child('ItemGroup');
        I := ItemGroupElement.ChildrenIterator('DCCReference');
        try
          while I.GetNext do
          begin
            DccReferenceElement := I.Current;
            FileName := DccReferenceElement.AttributeString('Include');
            if ExtractFileExt(FileName) <> '.dcp' then
            begin
              FileName := FixFileNameFromPackage(FileName);
              AltFiles.Append(FileName);
            end;
          end;
        finally FreeAndNil(I) end;

        ReadDefaultPlatformFromDproj(Doc.DocumentElement);
      finally FreeAndNil(Doc) end;

      CompareFilesLists(Files, AltFiles, Format('Files in DPK (%s) and DPROJ differ (%s)', [
        PackageFileName,
        DprojFileName
      ]));

      Writeln(Format('DPROJ %s: %d files', [DprojFileName, AltFiles.Count]));
    finally FreeAndNil(AltFiles) end;
  end;

begin
  inherited;
  ReadDpk;
  ReadDproj;
end;

{ main routine --------------------------------------------------------------- }

var
  Package: TPackage;
begin
  ApplicationProperties.ApplicationName := 'check_packages';
  ApplicationProperties.Version := CastleEngineVersion;
  ApplicationProperties.OnWarning.Add({$ifdef FPC}@{$endif} ApplicationProperties.WriteWarningOnConsole);

  Parameters.CheckHighAtMost(1);
  if Parameters.High = 1 then
    CgePath := Parameters[1];

  CgePathExpanded := InclPathDelim(ExpandFileName(CgePath));
  CgePathExpanded := SReplaceChars(CgePathExpanded, '\', '/'); // replace backslashes with slashes
  Writeln('Checking CGE in directory: ', CgePathExpanded);

  Package := TLazarusPackage.Create(CgePathExpanded + 'packages/castle_base.lpk');
  try
    Package.CheckFiles([
      'src/common_includes/',
      'src/transform/',
      'src/audio/',
      'src/base/',
      'src/base_rendering/',
      'src/castlescript/',
      'src/files/',
      'src/fonts/',
      'src/images/',
      'src/physics/',
      'src/services/',
      'src/ui/',
      'src/scene/',
      'src/deprecated_units/'
    ],
    [
      'src/base/android/',
      'src/files/indy/'
    ],
    [
      'src/vampyre_imaginglib/'
    ]);
  finally FreeAndNil(Package) end;

  Package := TLazarusPackage.Create(CgePathExpanded + 'packages/castle_window.lpk');
  try
    Package.CheckFiles([
      'src/window/'
    ],
    [ ],
    [ ]);
  finally FreeAndNil(Package) end;

  Package := TLazarusPackage.Create(CgePathExpanded + 'packages/alternative_castle_window_based_on_lcl.lpk');
  try
    Package.CheckFiles([
      'src/window/'
    ],
    [ ],
    [ ]);
  finally FreeAndNil(Package) end;

  Package := TLazarusPackage.Create(CgePathExpanded + 'packages/castle_components.lpk');
  try
    Package.CheckFiles([
      'src/lcl/'
    ],
    [],
    [ ]);
  finally FreeAndNil(Package) end;

  Package := TLazarusPackage.Create(CgePathExpanded + 'packages/castle_indy.lpk');
  try
    Package.CheckFiles([
      'src/files/indy/'
    ],
    [ ],
    [ ]);
  finally FreeAndNil(Package) end;

  Package := TLazarusPackage.Create(CgePathExpanded + 'packages/castle_editor_components.lpk');
  try
    Package.CheckFiles([
      'tools/castle-editor/components/'
    ],
    [
      'tools/castle-editor/components/mbColorLib/examples/'
    ],
    [ ]);
  finally FreeAndNil(Package) end;

  Package := TDelphiPackage.Create(CgePathExpanded + 'packages/delphi/castle_engine.dpk');
  try
    Package.CheckFiles([
      'src/common_includes/',
      'src/transform/',
      'src/audio/',
      'src/base/',
      'src/base_rendering/',
      'src/castlescript/',
      'src/files/',
      'src/fonts/',
      'src/images/',
      'src/physics/',
      'src/services/',
      'src/ui/',
      'src/scene/',
      // Delphi specific:
      'src/delphi/',
      'src/compatibility/delphi-only/'
      // TODO: not in package, but maybe they should be?
      // 'src/deprecated_units/'
    ],
    [
      'src/delphi/castleinternaldelphidesign.pas',
      'src/base/android/',
      'src/files/indy/',

      // This is in castle_engine_vcl package
      'src/delphi/vcl.castlecontrol.pas',

      // This is in castle_engine_fmx package
      'src/delphi/*fmx*.pas',

      // Internal units only for FPC
      'src/audio/castleinternalsoxsoundbackend.pas',
      'src/base/castleinternalgzio.pas',

      // TODO: FMOD is not yet supported with Delphi
      'src/audio/fmod/castlefmodsoundbackend.pas',
      'src/audio/fmod/castleinternalfmod.pas',

      // TODO: CGE localization is not yet supported with Delphi, uses FPC GetText
      'src/base/castlesystemlanguage.pas',
      'src/files/castlelocalizationgettext.pas',

      // TODO: OpenGLES (mobile) is not yet supported with Delphi
      'src/base_rendering/castlegles.pas',

      // TODO: CastleScript is not yet fully supported with Delphi
      'src/castlescript/castlescriptxml.pas',

      // TODO: Joysticks on Linux are not yet supported with Delphi
      'src/ui/castleinternaljoystickslinux.pas',

      // This is VCL-specific with Delphi, maybe in the future will be in some VCL package.
      // It also makes warnings about dispinterface not being portable.
      // It's only for 3dconnexion devices -- it is uncertain do we even want to maintain it in CGE core.
      'src/ui/windows/castleinternaltdxinput_tlb.pas',

      // This is only supported with FPC, but planned to be removed from CGE
      'src/scene/castleraytracer.pas'
    ],
    [
      'src/vampyre_imaginglib/'
    ]);
  finally FreeAndNil(Package) end;

  Package := TDelphiPackage.Create(CgePathExpanded + 'packages/delphi/castle_engine_design.dpk');
  try
    Package.CheckFiles([
      'src/delphi/castleinternaldelphidesign.pas'
    ],
    [ ],
    [ ]);
  finally FreeAndNil(Package) end;

  Package := TDelphiPackage.Create(CgePathExpanded + 'packages/delphi/castle_engine_window.dpk');
  try
    Package.CheckFiles([
      'src/window/'
    ],
    [
      // TODO: not in package, but maybe they should be?
      'src/window/deprecated_units/',

      // Only for CASTLE_WINDOW_XLIB, available only with FPC, and that's OK -- it's not a default for Linux
      'src/window/unix/castleinternalxlib.pas'
    ],
    [ ]);
  finally FreeAndNil(Package) end;

  Package := TDelphiPackage.Create(CgePathExpanded + 'packages/delphi/castle_engine_vcl.dpk');
  try
    Package.CheckFiles([
      'src/delphi/vcl.castlecontrol.pas'
    ],
    [ ],
    [ ]);
  finally FreeAndNil(Package) end;

  Package := TDelphiPackage.Create(CgePathExpanded + 'packages/delphi/castle_engine_fmx.dpk');
  try
    Package.CheckFiles([
      'src/delphi/*fmx*.pas'
    ],
    [ ],
    [ ]);
  finally FreeAndNil(Package) end;

  if WarningsCount <> 0 then
  begin
    Writeln(Format('%d package problems found (see report above), exiting with status 1', [WarningsCount]));
    Halt(1);
  end else
    Writeln('All packages OK');
end.
