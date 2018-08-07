unit ChooseProjectForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, Menus,
  CastleDialogs, CastleLCLRecentFiles;

type
  { Choose project (new or existing). }

  { TChooseProject }

  TChooseProject = class(TForm)
    ButtonOpenRecent: TBitBtn;
    ButtonNew: TBitBtn;
    ButtonOpen: TBitBtn;
    OpenProject: TCastleOpenDialog;
    ImageLogo: TImage;
    LabelTitle: TLabel;
    PopupMenuRecentProjects: TPopupMenu;
    procedure ButtonNewClick(Sender: TObject);
    procedure ButtonOpenClick(Sender: TObject);
    procedure ButtonOpenRecentClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    RecentProjects: TCastleRecentFiles;
    procedure MenuItemRecentClick(Sender: TObject);
  public

  end;

var
  ChooseProject: TChooseProject;

implementation

{$R *.lfm}

uses CastleConfig, CastleLCLUtils, CastleURIUtils, CastleUtils,
  CastleFindFiles, CastleStringUtils, CastleFilesUtils,
  ProjectUtils, EditorUtils, NewProjectForm, ToolUtils;

{ TTemplateCopyProcess ------------------------------------------------------------ }

type
  TTemplateCopyProcess = class
    TemplateUrl: String;
    ProjectDirUrl: String;
    Macros: TStringStringMap;
    procedure FoundFile(const FileInfo: TFileInfo; var StopSearch: Boolean);
  end;

procedure TTemplateCopyProcess.FoundFile(const FileInfo: TFileInfo; var StopSearch: Boolean);
var
  Contents, RelativeUrl, TargetUrl, TargetFileName: String;
begin
  if FileInfo.Directory and SpecialDirName(FileInfo.Name) then
    Exit;

  { Ignore case at IsPrefix / PrefixRemove calls,
    in case it's not case-sensitive file-system, then the case in theory
    can differ. }
  if not IsPrefix(TemplateUrl, FileInfo.URL, true) then
    raise Exception.CreateFmt('Unexpected: %s is not a prefix of %s, report a bug',
      [TemplateUrl, FileInfo.URL]);
  RelativeUrl := PrefixRemove(TemplateUrl, FileInfo.URL, true);
  TargetUrl := CombineURI(ProjectDirUrl, RelativeUrl);
  TargetFileName := URIToFilenameSafe(TargetUrl);

  if FileInfo.Directory then
  begin
    // create directory
    if not ForceDirectories(TargetFileName) then
      raise Exception.CreateFmt('Cannot create directory "%s"', [TargetFileName]);
  end else
  if (URIMimeType(FileInfo.URL) = 'application/xml') or
     (URIMimeType(FileInfo.URL) = 'text/plain') then
  begin
    // copy text file, replacing macros
    Contents := FileToString(FileInfo.URL);
    Contents := SReplacePatterns(Contents, Macros, false);
    StringToFile(TargetFileName, Contents);
  end else
  begin
    // simply copy other file types (e.g. sample png images in project templates)
    CheckCopyFile(URIToFilenameSafe(FileInfo.URL), TargetFileName);
  end;
end;

{ TChooseProject ------------------------------------------------------------- }

procedure TChooseProject.ButtonOpenClick(Sender: TObject);
begin
  if OpenProject.Execute then
  begin
    RecentProjects.Add(OpenProject.URL, false);
    ProjectOpen(OpenProject.URL);
  end;
end;

procedure TChooseProject.ButtonNewClick(Sender: TObject);

  procedure ShowAndError(const Message: String);
  begin
    Show;
    ErrorBox(Message);
  end;

  procedure CopyTemplate(const ProjectDirUrl: String);
  var
    TemplateUrl, ProjectName, ProjectQualifiedName, ProjectPascalName: String;
    CopyProcess: TTemplateCopyProcess;
    Macros: TStringStringMap;
  begin
    // TODO Choose template name according to chosen template
    TemplateUrl := ApplicationData('project_templates/empty/files/');

    ProjectName := NewProject.EditProjectName.Text;
    ProjectQualifiedName := 'com.mycompany.' + SDeleteChars(ProjectName, ['-']);
    ProjectPascalName := SReplaceChars(ProjectName, AllChars - ['a'..'z', 'A'..'Z', '0'..'9'], '_');

    Macros := TStringStringMap.Create;
    try
      Macros.Add('${PROJECT_NAME}', ProjectName);
      Macros.Add('${PROJECT_QUALIFIED_NAME}', ProjectQualifiedName);
      Macros.Add('${PROJECT_PASCAL_NAME}', ProjectPascalName);

      CopyProcess := TTemplateCopyProcess.Create;
      try
        CopyProcess.TemplateUrl := TemplateUrl;
        CopyProcess.ProjectDirUrl := ProjectDirUrl;
        CopyProcess.Macros := Macros;
        FindFiles(TemplateUrl, '*', true, @CopyProcess.FoundFile, [ffRecursive]);
      finally FreeAndNil(CopyProcess) end;
    finally FreeAndNil(Macros) end;
  end;

  procedure GenerateProgramWithBuildTool(const ProjectDirUrl: String);
  var
    BuildToolExe, BuildToolOutput: String;
    BuildToolStatus: integer;
  begin
    BuildToolExe := FindExe('castle-engine');
    if BuildToolExe = '' then
    begin
      WarningBox('Cannot find build tool (castle-engine) on $PATH environment variable. You will need to manually run "castle-engine generate-program" within project''s directory.');
      Exit;
    end;

    MyRunCommandIndir(URIToFilenameSafe(ProjectDirUrl), BuildToolExe,
      ['generate-program'], BuildToolOutput, BuildToolStatus);
    if BuildToolStatus <> 0 then
    begin
      WarningBox(Format('Generating program with the build tool failed with status code %d and output: "%s"',
        [BuildToolStatus, BuildToolOutput]));
      Exit;
    end;
  end;

var
  ProjectDir, ProjectDirUrl, ManifestUrl: String;
begin
  Hide;
  if NewProject.ShowModal = mrOK then
  begin
    Show; // TODO only for now

    // Create project dir
    ProjectDir := InclPathDelim(NewProject.EditLocation.Text) +
      NewProject.EditProjectName.Text;
    ProjectDirUrl := FilenameToURISafe(InclPathDelim(ProjectDir));
    if not ForceDirectories(ProjectDir) then
    begin
      ShowAndError(Format('Cannot create directory "%s".', [ProjectDir]));
      Exit;
    end;

    // Fill project dir
    CopyTemplate(ProjectDirUrl);

    // Generate rest using build tool
    GenerateProgramWithBuildTool(ProjectDirUrl);

    // Open new project
    ManifestUrl := CombineURI(ProjectDirUrl, 'CastleEngineManifest.xml');
    try
      ProjectOpen(ManifestUrl);
      RecentProjects.Add(ManifestUrl, false);
    except
      on E: Exception do
      begin
        ShowAndError(ExceptMessage(E));
        Exit;
      end;
    end;
  end else
    Show;
end;

procedure TChooseProject.ButtonOpenRecentClick(Sender: TObject);
var
  MenuItem: TMenuItem;
  I: Integer;
  Url: String;
begin
  PopupMenuRecentProjects.Items.Clear;
  for I := 0 to RecentProjects.URLs.Count - 1 do
  begin
    Url := RecentProjects.URLs[I];
    MenuItem := TMenuItem.Create(Self);
    MenuItem.Caption := SQuoteLCLCaption(Url);
    MenuItem.Tag := I;
    MenuItem.OnClick := @MenuItemRecentClick;
    PopupMenuRecentProjects.Items.Add(MenuItem);
  end;
  PopupMenuRecentProjects.PopupComponent := ButtonOpenRecent;
  PopupMenuRecentProjects.Popup;
end;

procedure TChooseProject.FormCreate(Sender: TObject);
begin
  UserConfig.Load;
  RecentProjects := TCastleRecentFiles.Create(Self);
  RecentProjects.LoadFromConfig(UserConfig);
  //  RecentProjects.NextMenuItem := ; // unused for now
end;

procedure TChooseProject.FormDestroy(Sender: TObject);
begin
  RecentProjects.SaveToConfig(UserConfig);
  UserConfig.Save;
end;

procedure TChooseProject.FormShow(Sender: TObject);
begin
  ButtonOpenRecent.Enabled := RecentProjects.URLs.Count <> 0;
end;

procedure TChooseProject.MenuItemRecentClick(Sender: TObject);
var
  Url: String;
begin
  Url := RecentProjects.URLs[(Sender as TMenuItem).Tag];
  ProjectOpen(Url);
end;

end.

