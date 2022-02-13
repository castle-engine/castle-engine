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

uses SysUtils, Classes, Generics.Collections,
  CastleUIState, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleViewport, CastleScene, CastleResources, CastleCreatures,
  CastleLevels, CastleVectors;

type
  TStateMain = class;

  { Button that spawns a creature with specified TCreatureResource. }
  TResourceButton = class(TCastleButton)
  public
    OwnerState: TStateMain;
    ButtonResource: TCreatureResource;
    procedure DoClick; override;
  end;

  TResourceButtonList = {$ifdef FPC}specialize{$endif} TObjectList<TResourceButton>;

  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    { Components designed using CGE editor, loaded from gamestatemain.castle-user-interface. }
    LabelFps: TCastleLabel;
    MainViewport: TCastleViewport;
    ButtonLoadResourceXml: TCastleButton;
    CheckboxShowDebug: TCastleCheckbox;
    GroupResources: TCastleVerticalGroup;

    { Other private stuff }
    Level: TLevel;
    CurrentCreature: TCreature;
    CurrentResource: TCreatureResource;
    ResourceButtons: TResourceButtonList; //< all current TResourceButton instances
    { Remember this to make repeated usage of FileDialog in ClickButtonLoadResourceXml
      more comfortable, it will start with the last chosen directory. }
    LastLoadedResourceXml: string;

    procedure ClickButtonLoadResourceXml(Sender: TObject);
    procedure ChangedCheckboxShowDebug(Sender: TObject);
    { Create buttons in ResourceButtons and GroupResources to reflect current Resources. }
    procedure UpdateResourceButtons;
    procedure SetCreatureResource(const NewCreatureResource: TCreatureResource);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  StateMain: TStateMain;

implementation

uses CastleURIUtils, CastleWindow;

{ TResourceButton ------------------------------------------------------------ }

procedure TResourceButton.DoClick;
begin
  OwnerState.SetCreatureResource(ButtonResource);
end;

{ TStateMain ----------------------------------------------------------------- }

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;

procedure TStateMain.Start;

  { An example of creating a resource (TStillCreatureResource in this case)
    without resource.xml file. You just create an instance of TXxxResource by hand,
    fill the properties you need, and add it to global Resources list. }
  procedure TestAddingResourceByCode;
  var
    Res: TStillCreatureResource;
  begin
    Res := TStillCreatureResource.Create(nil);
    Res.Name := 'KnightCreatedFromCodeTest';
    Res.ModelURL := 'castle-data:/knight_single_gltf/knight.gltf';
    Res.Animations.FindName('idle').AnimationName := 'Idle';
    Resources.Add(Res);
  end;

begin
  inherited;

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  MainViewport := DesignedComponent('MainViewport') as TCastleViewport;
  ButtonLoadResourceXml := DesignedComponent('ButtonLoadResourceXml') as TCastleButton;
  CheckboxShowDebug := DesignedComponent('CheckboxShowDebug') as TCastleCheckbox;
  GroupResources := DesignedComponent('GroupResources') as TCastleVerticalGroup;

  ButtonLoadResourceXml.OnClick := {$ifdef FPC}@{$endif}ClickButtonLoadResourceXml;
  CheckboxShowDebug.OnChange := {$ifdef FPC}@{$endif}ChangedCheckboxShowDebug;

  { initialize global Resources list contents }
  Resources.LoadFromFiles;
  TestAddingResourceByCode;

  { Prepare (load animations) for all resources }
  Resources.Prepare(MainViewport.PrepareParams, 'resources');

  { Level refers to a Viewport.
    We need to create Level, as creature can be spawned only within a Level,
    using CurrentResource.CreateCreature(Level, ...),
    placing it inside "Viewport.Items". }
  Level := TLevel.Create(FreeAtStop);
  Level.Viewport := MainViewport;

  ResourceButtons := TResourceButtonList.Create(true);
  UpdateResourceButtons;
  SetCreatureResource(Resources.FindName('KnightSingleGltf') as TCreatureResource);
end;

procedure TStateMain.Stop;
begin
  FreeAndNil(ResourceButtons);
  inherited;
end;

procedure TStateMain.SetCreatureResource(const NewCreatureResource: TCreatureResource);
var
  I: Integer;
begin
  FreeAndNil(CurrentCreature); // remove previous creature

  { CreateCreature creates TCreature instance and adds it to Viewport.Items }
  CurrentResource := NewCreatureResource;
  CurrentCreature := CurrentResource.CreateCreature(Level,
    { Translation } Vector3(0, 0, 0),
    { Direction } Vector3(0, 0, 1));

  { update Pressed of buttons }
  for I := 0 to ResourceButtons.Count - 1 do
    ResourceButtons[I].Pressed := ResourceButtons[I].ButtonResource = CurrentResource;
end;

procedure TStateMain.UpdateResourceButtons;
var
  ResButton: TResourceButton;
  I: Integer;
begin
  { easily destroy all existing buttons using the XxxButtons list,
    destroying them also automatically removed them from Window.Controls list }
  ResourceButtons.Clear;

  for I := 0 to Resources.Count - 1 do
  begin
    ResButton := TResourceButton.Create(nil);
    ResButton.ButtonResource := Resources[I] as TCreatureResource;
    ResButton.Caption := 'Spawn creature ' + ResButton.ButtonResource.Name;
    ResButton.Toggle := true;
    ResButton.OwnerState := Self;
    ResourceButtons.Add(ResButton);
    GroupResources.InsertFront(ResButton);
  end;
  if Resources.Count = 0 then
    raise Exception.CreateFmt('No resources found. Make sure we search in proper path (current data path is detected as "%s")',
      [ResolveCastleDataURL('castle-data:/')]);
end;

procedure TStateMain.ClickButtonLoadResourceXml(Sender: TObject);
begin
  if Application.MainWindow.FileDialog('Resource file to load',
    LastLoadedResourceXml, true,
    'All Files|*|*Resource files (resource.xml)|resource.xml|') then
  begin
    Resources.AddFromFile(LastLoadedResourceXml);
    { directly prepare new resource }
    Resources.Prepare(MainViewport.PrepareParams, 'resources');

    UpdateResourceButtons;
    SetCreatureResource(Resources.Last as TCreatureResource); // newly added resource is the last, activate it
  end;
end;

procedure TStateMain.ChangedCheckboxShowDebug(Sender: TObject);
begin
  // show bounding box of the creatures
  TCreature.RenderDebug := CheckboxShowDebug.Checked;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

end.
