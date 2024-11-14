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

{ Main view, where most of the application logic takes place. }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleIfc;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    ButtonNew, ButtonLoad, ButtonSave, ButtonAddWall, ButtonModifyWall: TCastleButton;
    IfcScene: TCastleScene;
  private
    IfcFile: TIfcFile;
    IfcMapping: TCastleIfcMapping;

    { Create new IfcMapping instance and update what IfcScene shows,
      based on IfcFile contents.
      Use this after completely changing the IfcFile contents
      (like loading new file, or creating new file). }
    procedure NewIfcMapping(const NewIfcFile: TIfcFile);

    procedure ClickNew(Sender: TObject);
    procedure ClickLoad(Sender: TObject);
    procedure ClickSave(Sender: TObject);
    procedure ClickAddWall(Sender: TObject);
    procedure ClickModifyWall(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleIfc, CastleUtils, CastleUriUtils, CastleWindow;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;
  ClickNew(nil);
end;

procedure TViewMain.Stop;
begin
  FreeAndNil(IfcFile);
  FreeAndNil(IfcMapping);
  inherited;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TViewMain.NewIfcMapping(const NewIfcFile: TIfcFile);
begin
  FreeAndNil(IfcMapping);

  IfcMapping := TCastleIfcMapping.Create;
  { The 'castle-data:/' below will be used as base URL to resolve any texture
    URLs inside IFC. As they are not possible in this demo for now,
    this value doesn't really matter. }
  IfcMapping.Load(IfcFile, 'castle-data:/');

  IfcScene.Load(IfcMapping.RootNode, true);
end;

procedure TViewMain.ClickNew(Sender: TObject);
begin
  FreeAndNil(IfcFile);

  IfcFile := TIfcFile.Create;
  IfcFile.EncodingType := 'IFC.JSON';
  IfcFile.Version := '0.0.1';
  IfcFile.SchemaIdentifier := 'IFC4';
  IfcFile.OriginatingSystem := 'Castle Game Engine ' + CastleEngineVersion;

  NewIfcMapping(IfcFile);
end;

const
  IfcFileFilter = 'IFC JSON (*.ifcjson)|*.ifcjson|All Files|*';

procedure TViewMain.ClickLoad(Sender: TObject);
var
  Url: string;
begin
  Url := 'castle-data:/';
  if Application.MainWindow.FileDialog('Load IFC file', Url, true, IfcFileFilter) then
  begin
    FreeAndNil(IfcFile);
    IfcFile := IfcJsonLoad(Url);
    NewIfcMapping(IfcFile);
  end;
end;

procedure TViewMain.ClickSave(Sender: TObject);
var
  Url: string;
begin
  Url := 'castle-data:/';
  if Application.MainWindow.FileDialog('Save IFC file', Url, false, IfcFileFilter) then
  begin
    IfcJsonSave(IfcFile, Url);
  end;
end;

procedure TViewMain.ClickAddWall(Sender: TObject);
begin
  // IfcFile.Project.... // TODO
  IfcMapping.Update(IfcFile);
end;

procedure TViewMain.ClickModifyWall(Sender: TObject);
begin
  // IfcFile.Project.... // TODO
  IfcMapping.Update(IfcFile);
end;

end.
