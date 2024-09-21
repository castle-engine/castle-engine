{
  Copyright 2024-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Detect unused files in data. }
unit ToolProjectUnusedData;

{$I castleconf.inc}

interface

uses Generics.Collections, Classes,
  CastleFindFiles;

type
  TDetectUnusedData = class
  public
    type
      TResource = class;
      TResourceList = {$ifdef FPC}specialize{$endif} TObjectList<TResource>;

      { File (in data or not) that may be referred to by other files. }
      TResource = class
        { Absolute filename.}
        FileName: String;
        { Filename relative to the project;s path. }
        RelativeFileName: String;
        Size: Int64;
        Contents: String;
        ContentsLoaded: Boolean;
        Users: TResourceList;
        UsedInProject: Boolean;
        destructor Destroy; override;
      end;

  strict private
    AllData: TResourceList;
    AllCode: TResourceList;
    FUnusedData: TResourceList;
    FProjectPath: String;
    procedure ScanDataFile(const FileInfo: TFileInfo; var StopSearch: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ScanProject(const ProjectPath, ProjectDataPath: String;
      const ProjectPascalFiles: TStringList);
    property UnusedData: TResourceList read FUnusedData;
    procedure SortUnusedDataBySize;
  end;

implementation

uses SysUtils, Generics.Defaults,
  CastleImages, CastleFilesUtils, CastleUriUtils;

{ TDetectUnusedData.TResource ------------------------------------------------ }

destructor TDetectUnusedData.TResource.Destroy;
begin
  FreeAndNil(Users);
  inherited;
end;

{ TDetectUnusedData ---------------------------------------------------------- }

constructor TDetectUnusedData.Create;
begin
  inherited;
  AllData := TResourceList.Create(true);
  AllCode := TResourceList.Create(true);
  FUnusedData := TResourceList.Create(false);
end;

destructor TDetectUnusedData.Destroy;
begin
  FreeAndNil(FUnusedData);
  FreeAndNil(AllData);
  FreeAndNil(AllCode);
  inherited;
end;

procedure TDetectUnusedData.ScanDataFile(const FileInfo: TFileInfo; var StopSearch: boolean);

  { For efficiency, we don't even analyze some files, that cannot refer to other files. }
  function CanReferToOtherFiles(const FileName: String): Boolean;
  begin
    Result := not (
      // all our image formats are simple, then are self-contained
      IsImageMimeType(UriMimeType(FileName), false, false) or
      // glTF .bin files do not refer to other files
      (ExtractFileExt(FileName) = '.bin') or
      // audio files are self-contained
      (ExtractFileExt(FileName) = '.wav') or
      (ExtractFileExt(FileName) = '.ogg')
    );
  end;

var
  Res: TResource;
begin
  Res := TResource.Create;
  Res.FileName := FileInfo.AbsoluteName;
  Res.RelativeFileName := ExtractRelativePath(FProjectPath, Res.FileName);
  Res.Size := FileInfo.Size;
  if CanReferToOtherFiles(Res.FileName) then
  begin
    Res.ContentsLoaded := true;
    Res.Contents := FileToString(Res.FileName);
  end;
  AllData.Add(Res);
end;

procedure TDetectUnusedData.ScanProject(const ProjectPath, ProjectDataPath: String;
  const ProjectPascalFiles: TStringList);

  { Initialize AllData by scanning all files in ProjectDataPath. }
  procedure ScanData;
  begin
    AllData.Clear;
    FindFiles(ProjectDataPath, '*', false, {$ifdef FPC}@{$endif} ScanDataFile,
      [ffRecursive]);
    Writeln('Found ', AllData.Count, ' data files.');
  end;

  { Initialize AllCode. }
  procedure ScanCode;
  var
    PascalFile: String;
    Res: TResource;
  begin
    AllCode.Clear;

    // collect AllCode
    for PascalFile in ProjectPascalFiles do
    begin
      Res := TResource.Create;
      Res.RelativeFileName := PascalFile;
      Res.FileName := CombinePaths(ProjectPath, PascalFile);
      Res.ContentsLoaded := true;
      Res.Contents := FileToString(Res.FileName);
      // Res.Size := FileSize(Res.FileName); // size of files in AllCode is not used
      // Res.Users := ... ; // we don't analyze dependencies of AllCode
      Res.UsedInProject := true;
      AllCode.Add(Res);
    end;
    WriteLn('Found ', AllCode.Count, ' Pascal files.');
  end;

  { Fill Users of all items in AllData.
    This effectively makes a edges of dependency graph. }
  procedure DetectUsage;

    { We do an incredibly simple heuristics here: just check if the filename
      appears in another file's contents.
      This obviously assumes some things:

      - That filename stuff is encoded as "just text", UTF-8, in the user.

      - That filename stuff is a single string, not constructed.
        E.g. if Pascal file will do
          DesignUrl := 'castle-data:/game' + view.castle-user-interface'
        ... then we will not detect that gameview.castle-user-interface is used.

      This is just "good enough" in practical use-cases, but absolutely not perfect.
      If above assumptions fail, we will not detect some usages.

      OTOH, we may also "over-zealously" detect some usages:

      - We just look for ExtractFileName(Used.FileName).
        This is simpler (means we don't need to care how each file format
        resolves relative paths) but it means we can easily classify some
        files as "used" when they are actually not.
    }
    function UsedByContents(const Used, PotentialUser: TResource; const UsedBaseName: String): Boolean;
    begin
      Result := PotentialUser.ContentsLoaded and
        (Pos(UsedBaseName, PotentialUser.Contents) <> 0)
    end;

    function UsedByNames(const Used, PotentialUser: TResource; const UsedExt: String): Boolean;
    begin
      if UsedExt = '.atlas' then
        // special rule for .atlas files: used by corresponding .json files
        Result := ChangeFileExt(Used.FileName, '.json') = PotentialUser.FileName
      else
        Result := false;
    end;

    procedure AddUsers(const Used: TResource; const AllPotentialUsers: TResourceList);
    var
      PotentialUser: TResource;
      UsedBaseName, UsedExt: String;
    begin
      UsedBaseName := ExtractFileName(Used.FileName);
      UsedExt := LowerCase(ExtractFileExt(Used.FileName));
      for PotentialUser in AllPotentialUsers do
        if (Used <> PotentialUser) and
           ( UsedByContents(Used, PotentialUser, UsedBaseName) or
             UsedByNames(Used, PotentialUser, UsedExt)
           ) then
          Used.Users.Add(PotentialUser);
    end;

  var
    Used: TResource;
  begin
    for Used in AllData do
    begin
      Used.Users := TResourceList.Create(false);
      AddUsers(Used, AllData);
      AddUsers(Used, AllCode);
    end;
    Writeln('Established edges of dependency graph (what file uses what).');
  end;

  { Detect UsedInProject of all items in AllData.
    This relies on Users (in AllData) being already filled. }
  procedure DetectUsedInProject;
  const
    MaxDepth = 16;

    procedure UpdateUsedInProject(const R: TResource; const CurrentDepth: Integer);
    var
      User: TResource;
    begin
      if CurrentDepth = 0 then
        Exit; // avoid infinite recursion, in case of cycles in dependencies

      if R.UsedInProject then
        Exit; // nothing more to do
      for User in R.Users do
      begin
        UpdateUsedInProject(User, CurrentDepth - 1);
        if User.UsedInProject then
        begin
          R.UsedInProject := true;
          Exit;
        end;
      end;
    end;

  var
    R: TResource;
  begin
    for R in AllData do
      UpdateUsedInProject(R, MaxDepth);
  end;

  { Initialize UnusedData contents (based on UsedInProject flags in AllData). }
  procedure FillUnusedData;
  var
    R: TResource;
  begin
    for R in AllData do
      { We never list data/README.txt as "unused", although it actually is unused
        in probably all projects. But

        - It is too beneficial for devs (it communicates important info).
        - It is present in all projects created by our templates.
        - It is tiny, one file -- should not matter.

        Note that we don't mark it early as UsedInProject -- in case
        it mentions some other file, it still *does not* make this other file
        used in project. }
      if (not R.UsedInProject) and
         (R.RelativeFileName <> 'data/README.txt') and
         (R.RelativeFileName <> 'data' + PathDelim + 'README.txt') then
        FUnusedData.Add(R);
  end;

begin
  FProjectPath := ProjectPath;
  ScanData;
  ScanCode;
  DetectUsage;
  DetectUsedInProject;
  FillUnusedData;
end;

function CompareResourceSize(
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif}
  Left, Right: TDetectUnusedData.TResource): Integer;
begin
  Result := Right.Size - Left.Size;
end;

procedure TDetectUnusedData.SortUnusedDataBySize;
type
  TResourceComparer = specialize TComparer<TResource>;
begin
  FUnusedData.Sort(TResourceComparer.Construct(@CompareResourceSize));
end;

end.
