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
  ProjectUtils, EditorUtils, NewProjectForm;

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

var
  ProjectDir, Url: String;
begin
  Hide;
  if NewProject.ShowModal = mrOK then
  begin
    Show; // TODO only for now

    ProjectDir := InclPathDelim(NewProject.EditLocation.Text) +
      NewProject.EditProjectName.Text;
    if not ForceDirectories(ProjectDir) then
    begin
      ShowAndError(Format('Cannot create directory "%s".', [ProjectDir]));
      Exit;
    end;

    // TODO - actually create the project content

    Url := CombineURI(FilenameToURISafe(NewProject.EditLocation.Text),
      'CastleEngineManifest.xml');
    if not URIFileExists(Url) then
    begin
      ShowAndError(Format('Cannot read CastleEngineManifest.xml in project directory "%s".',
        [ProjectDir]));
      Exit;
    end;

    RecentProjects.Add(OpenProject.URL, false);
    ProjectOpen(Url);
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

