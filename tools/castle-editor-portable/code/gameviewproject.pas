{
  Copyright 2025-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Edit a chosen project. }
unit GameViewProject;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize, CastleInternalInspector,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleFindFiles;

type
  { Edit a chosen project. }
  TViewProject = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    ButtonCloseProject: TCastleButton;
    ButtonCloseDesign: TCastleButton;
    ButtonViewTemplate: TCastleButton;
    ContainerDesignView, ContainerOpenView, ContainerLoadedDesign: TCastleUserInterface;
    FactoryButtonView: TCastleComponentFactory;
    ListOpenExistingView: TCastleVerticalGroup;
  private
    FInspector: TCastleInspector;
    ListOpenExistingViewStr: TStringList;
    procedure ClickCloseProject(Sender: TObject);
    procedure ClickCloseDesign(Sender: TObject);
    procedure ClickOpenView(Sender: TObject);
    procedure ListOpenExistingViewAddFile(const FileInfo: TFileInfo;
      var StopSearch: boolean);
    procedure ProposeOpenDesign(const OpenDesignUrl: String);
    procedure ListViewsRefresh;
  public
    // set before starting the project
    // Absolute project path, as directory name (not URL), ending with path delimiter.
    ProjectPath: String;
    // Absolute project path, as URL, ending with path delimiter.
    ProjectPathUrl: String;
    // Full URL to the project manifest, including final filename.
    ProjectManifestUrl: String;
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewProject: TViewProject;

implementation

uses SysUtils,
  CastleStringUtils, CastleUriUtils, CastleUtils,
  ToolEditorUtils,
  GameViewChooseProject;

{ TViewProject ----------------------------------------------------------------- }

constructor TViewProject.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewproject.castle-user-interface';
end;

procedure TViewProject.Start;
begin
  inherited;
  ButtonCloseProject.OnClick := {$ifdef FPC}@{$endif} ClickCloseProject;
  ButtonCloseDesign.OnClick := {$ifdef FPC}@{$endif} ClickCloseDesign;

  // a bit simplified inspector, to have room for rest
  TCastleInspector.PersistentState.RectLogExists := false;
  TCastleInspector.PersistentState.RectProfilerExists := false;
  FInspector := TCastleInspector.Create(FreeAtStop);
  ContainerDesignView.InsertFront(FInspector);

  // at the beginning, show UI to choose design
  ButtonCloseDesign.Exists := false;
  ContainerDesignView.Exists := false;
  ContainerOpenView.Exists := true;

  FactoryButtonView.LoadFromComponent(ButtonViewTemplate);
  // note that ButtonViewTemplate children remain existing, doesn't matter
  FreeAndNil(ButtonViewTemplate);

  ListOpenExistingViewStr := TStringList.Create;

  ListViewsRefresh;
end;

procedure TViewProject.Stop;
begin
  FreeAndNil(ListOpenExistingViewStr);
  inherited;
end;

procedure TViewProject.ListOpenExistingViewAddFile(const FileInfo: TFileInfo; var StopSearch: boolean);
begin
  ListOpenExistingViewStr.Append(FileInfo.AbsoluteName);
end;

type
  TButtonViewDesign = class(TPersistent)
  published
    LabelViewName, LabelViewFile, LabelLastModified: TCastleLabel;
  end;

procedure TViewProject.ListViewsRefresh;

  function ShortDesignName(const S: String): String;
  begin
    Result := DeleteFileExt(ExtractFileName(S));
    Result := PrefixRemove('gameview', Result, true);
    Result := PrefixRemove('gamestate', Result, true);
    Result := SuffixRemove('.castle-user-interface', Result, true);
  end;

var
  DesignFileName, ProjectDataUrl: String;
  ButtonViewDesign: TButtonViewDesign;
  I: Integer;
  ButtonView: TCastleButton;
begin
  { calculate ListOpenExistingViewStr contents }
  ListOpenExistingViewStr.Clear;
  { Search in ProjectDataUrl, not ProjectPathUrl, as all designs should be part of data
    to be possible to open them at runtime.
    This also avoids finding stuff in castle-engine-output, which is possible,
    e.g. after "castle-engine package --target=android" the castle-engine-output contains
    some temporary data with copies of design files -- and we *do not* want to show them here. }
  ProjectDataUrl := CombineUri(ProjectPathUrl, 'data/');
  if UriExists(ProjectDataUrl) <> ueNotExists then
  begin
    FindFiles(ProjectDataUrl, 'gameview*.castle-user-interface', false, @ListOpenExistingViewAddFile, [ffRecursive]);
    // support deprecated names
    FindFiles(ProjectDataUrl, 'gamestate*.castle-user-interface', false, @ListOpenExistingViewAddFile, [ffRecursive]);
  end;
  { without sorting, the order would be ~random (as FindFiles enumarates).
    Note that we sort including the subdirectory names, which is good,
    we want files in the same subdirectory to be together. }
  ListOpenExistingViewStr.Sort;

  { copy ListOpenExistingViewStr contents -> ListOpenExistingView GUI contents }
  ListOpenExistingView.ClearControls; // TODO: also free items, use TCastleListBox
  for I := 0 to ListOpenExistingViewStr.Count -1 do
  begin
    DesignFileName := ListOpenExistingViewStr[I];
    ButtonViewDesign := TButtonViewDesign.Create;
    try
      ButtonView := FactoryButtonView.ComponentLoad(FreeAtStop, ButtonViewDesign) as TCastleButton;
      ButtonView.Tag := I;
      ButtonView.OnClick := {$ifdef FPC}@{$endif} ClickOpenView;
      ListOpenExistingView.InsertFront(ButtonView);

      ButtonViewDesign.LabelViewName.Caption := ShortDesignName(DesignFileName);
      ButtonViewDesign.LabelViewFile.Caption := ExtractRelativePath(ProjectPath, DesignFileName);
      ButtonViewDesign.LabelLastModified.Caption := FileDateTimeStr(DesignFileName);
    finally FreeAndNil(ButtonViewDesign) end;
  end;
end;

procedure TViewProject.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TViewProject.ClickCloseProject(Sender: TObject);
begin
  Container.View := ViewChooseProject;
end;

procedure TViewProject.ClickCloseDesign(Sender: TObject);
begin
  ContainerLoadedDesign.ClearControls; // to free memory, observers of URL etc.
  ListViewsRefresh;
  ButtonCloseDesign.Exists := false;
  ContainerDesignView.Exists := false;
  ContainerOpenView.Exists := true;
end;

procedure TViewProject.ClickOpenView(Sender: TObject);
var
  DesignFileName, OpenDesignUrl: String;
  ViewIndex: Integer;
begin
  ViewIndex := (Sender as TComponent).Tag;
  DesignFileName := ListOpenExistingViewStr[ViewIndex];
  OpenDesignUrl := FilenameToUriSafe(DesignFileName);
  ProposeOpenDesign(OpenDesignUrl);
end;

procedure TViewProject.ProposeOpenDesign(const OpenDesignUrl: String);
begin
  ButtonCloseDesign.Exists := true;
  ContainerDesignView.Exists := true;
  ContainerOpenView.Exists := false;

  // TODO: do rest of design opening, remove test blue rect
end;

end.
