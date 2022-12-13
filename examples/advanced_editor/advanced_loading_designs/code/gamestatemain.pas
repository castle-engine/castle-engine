{
  Copyright 2020-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main state, where most of the application logic takes place. }
unit GameStateMain;

interface

uses Classes,
  CastleUIState, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleNotifications, CastleViewport;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    { Components designed using CGE editor, loaded from gamestatemain.castle-user-interface. }
    LabelFps: TCastleLabel;
    DesignedButton1: TCastleDesign;
    DesignedButton1Button: TCastleButton;
    DesignedButton2: TCastleDesign;
    DesignedButton2Button: TCastleButton;
    Notifications: TCastleNotifications;
    ScrollViewTable: TCastleScrollView;
    VerticalGroupTable: TCastleVerticalGroup;
    ViewportForSoldiers: TCastleViewport;

    procedure ClickDesignedButton1(Sender: TObject);
    procedure ClickDesignedButton2(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils,
  CastleVectors, CastleTransform;

{ TStateMain ----------------------------------------------------------------- }

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;

procedure TStateMain.Start;

  procedure AddTableRow(const RowTemplate: TSerializedComponent; const ProjectName, ProjectWebsite, ProjectInfo: String);
  var
    Row: TCastleUserInterface;
    RowOwner: TComponent;
    LabelProjectName: TCastleLabel;
    LabelProjectWebsite: TCastleLabel;
    EditProjectInfo: TCastleEdit;
  begin
    RowOwner := TComponent.Create(FreeAtStop);
    Row := RowTemplate.UserInterfaceLoad(RowOwner);
    Row.FullSize := false;
    Row.Width := 1000;
    Row.Height := 50;

    LabelProjectName := RowOwner.FindRequiredComponent('LabelProjectName') as TCastleLabel;
    LabelProjectWebsite := RowOwner.FindRequiredComponent('LabelProjectWebsite') as TCastleLabel;
    EditProjectInfo := RowOwner.FindRequiredComponent('EditProjectInfo') as TCastleEdit;

    LabelProjectName.Caption := ProjectName;
    LabelProjectWebsite.Caption := ProjectWebsite;
    EditProjectInfo.Text := ProjectInfo;

    VerticalGroupTable.InsertFront(Row);
  end;

  procedure AddSoldierWithCape(const Template: TSerializedComponent; const Translation: TVector3);
  var
    SoldierInstance: TCastleTransform;
    SoldierOwner: TComponent;
  begin
    SoldierOwner := TComponent.Create(FreeAtStop);
    SoldierInstance := Template.TransformLoad(SoldierOwner);
    SoldierInstance.Translation := Translation;
    ViewportForSoldiers.Items.Add(SoldierInstance);
  end;

var
  TableRowTemplate: TSerializedComponent;
  SoldierWithCapeTemplate: TSerializedComponent;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  DesignedButton1 := DesignedComponent('DesignedButton1') as TCastleDesign;
  DesignedButton2 := DesignedComponent('DesignedButton2') as TCastleDesign;
  Notifications := DesignedComponent('Notifications') as TCastleNotifications;
  ScrollViewTable := DesignedComponent('ScrollViewTable') as TCastleScrollView;
  VerticalGroupTable := DesignedComponent('VerticalGroupTable') as TCastleVerticalGroup;
  ViewportForSoldiers := DesignedComponent('ViewportForSoldiers') as TCastleViewport;

  { Find the components inside TCastleDesigns -- need another FindRequiredComponent call }
  DesignedButton1Button := DesignedButton1.DesignedComponent('RootButton') as TCastleButton;
  DesignedButton2Button := DesignedButton2.DesignedComponent('RootButton') as TCastleButton;

  { Attach events }
  DesignedButton1Button.OnClick := {$ifdef FPC}@{$endif} ClickDesignedButton1;
  DesignedButton2Button.OnClick := {$ifdef FPC}@{$endif} ClickDesignedButton2;

  { Load and instantiate table row UI many times }
  TableRowTemplate := TSerializedComponent.Create('castle-data:/table_row.castle-user-interface');
  try
    AddTableRow(TableRowTemplate, 'Castle Game Engine', 'https://castle-engine.io/', 'Game engine');
    AddTableRow(TableRowTemplate, 'Free Pascal Compiler', 'https://www.freepascal.org/', 'Pascal compiler');
    AddTableRow(TableRowTemplate, 'Lazarus', 'https://www.lazarus-ide.org/', 'IDE for FPC');
    AddTableRow(TableRowTemplate, 'Blender', 'https://www.blender.org/', '3D creation');
    AddTableRow(TableRowTemplate, 'OpenGL', 'https://www.khronos.org/opengl/', 'Rendering API');
    AddTableRow(TableRowTemplate, 'Debian', 'https://www.debian.org/', 'Debian GNU/Linux distro');
  finally
    FreeAndNil(TableRowTemplate);
  end;

  { Load and instantiate soldier with cape many times }
  SoldierWithCapeTemplate := TSerializedComponent.Create('castle-data:/soldier_with_cape.castle-transform');
  try
    AddSoldierWithCape(SoldierWithCapeTemplate, Vector3(0, 0, 0));
    AddSoldierWithCape(SoldierWithCapeTemplate, Vector3(1.5, 0, 0));
    AddSoldierWithCape(SoldierWithCapeTemplate, Vector3(3, 0, 0));
  finally
    FreeAndNil(SoldierWithCapeTemplate);
  end;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TStateMain.ClickDesignedButton1(Sender: TObject);
begin
  Notifications.Show('Clicked button 1');
end;

procedure TStateMain.ClickDesignedButton2(Sender: TObject);
begin
  Notifications.Show('Clicked button 2');
end;

end.
