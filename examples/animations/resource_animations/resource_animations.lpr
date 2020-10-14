{
  Copyright 2009-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Play the animations of resources (creatures/items). }
uses SysUtils, Generics.Collections,
  CastleFilesUtils, CastleWindow, CastleResources, CastleScene,
  CastleProgress, CastleWindowProgress, CastleControls, CastleUIControls,
  CastleUtils, CastleTransform, CastleCreatures, CastleLog, CastleCameras,
  CastleURIUtils, CastleViewport, CastleLevels, CastleVectors;

var
  Window: TCastleWindowBase;
  Viewport: TCastleViewport;
  BaseScene: TCastleScene;
  Level: TLevel;
  CurrentCreature: TCreature;
  CurrentResource: TCreatureResource;
  ResourceButtonsGroup: TCastleVerticalGroup;

procedure UpdateResourceButtons; forward;
procedure SetCreatureResource(const NewCreatureResource: TCreatureResource); forward;

{ TResourceButton ------------------------------------------------------------ }

type
  TResourceButton = class(TCastleButton)
  public
    ButtonResource: TCreatureResource;
    procedure DoClick; override;
  end;

  TResourceButtonList = specialize TObjectList<TResourceButton>;

procedure TResourceButton.DoClick;
begin
  SetCreatureResource(ButtonResource);
end;

var
  ResourceButtons: TResourceButtonList;

{ TLoadResourceButton -------------------------------------------------------- }

type
  TLoadResourceButton = class(TCastleButton)
  public
    { remember this only to make repeated usage of FileDialog more comfortable }
    LastChosenURL: string;
    procedure DoClick; override;
  end;

procedure TLoadResourceButton.DoClick;
begin
  if Window.FileDialog('Resource file to load', LastChosenURL, true,
    'All Files|*|*Resource files (resource.xml)|resource.xml|') then
  begin
    Resources.AddFromFile(LastChosenURL);
    { directly prepare new resource }
    Resources.Prepare(Viewport.PrepareParams, 'resources');

    UpdateResourceButtons;
    SetCreatureResource(Resources.Last as TCreatureResource); // newly added resource is the last, activate it
  end;
end;

var
  LoadResourceButton: TLoadResourceButton;

{ helper functions ----------------------------------------------------------- }

procedure SetCreatureResource(const NewCreatureResource: TCreatureResource);
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

{ Create buttons in ResourceButtons and ResourceButtonsGroup to reflect current Resources. }
procedure UpdateResourceButtons;
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
    ResourceButtons.Add(ResButton);
    ResourceButtonsGroup.InsertFront(ResButton);
  end;
  if Resources.Count = 0 then
    raise Exception.CreateFmt('No resources found. Make sure we search in proper path (current data path is detected as "%s")',
      [ResolveCastleDataURL('castle-data:/')]);
end;

{ An example of creating a resource (TStillCreatureResource in this case)
  without resource.xml file. You just create an instance of TXxxResource by hand,
  fill the properties you need, and add it to global Resources list. }
procedure TestAddingResourceByCode;
var
  Res: TStillCreatureResource;
begin
  Res := TStillCreatureResource.Create('KnightCreatedFromCodeTest');
  Res.ModelURL := 'castle-data:/knight_single_gltf/knight.gltf';
  Res.Animations.FindName('idle').AnimationName := 'Idle';
  Resources.Add(Res);
end;

{ Main program --------------------------------------------------------------- }

begin
  InitializeLog;

  Window := TCastleWindowBase.Create(Application);
  Application.MainWindow := Window;
  Progress.UserInterface := WindowProgressInterface;
  Window.Open;

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Window.Controls.InsertFront(Viewport);

  Resources.LoadFromFiles;

  TestAddingResourceByCode;

  { load basic 3D scene where creature is shown. This isn't necessary,
    but it's an easy way to add a camera with headlight,
    and some grid to help with orientation. }
  BaseScene := TCastleScene.Create(Application);
  BaseScene.Load('castle-data:/base.gltf');
  Viewport.Items.MainScene := BaseScene;
  Viewport.Items.Add(BaseScene);
  Viewport.Items.UseHeadlight := hlOn;

  Viewport.AssignDefaultCamera; // derive camera from base.gltf (in MainScene)

  Viewport.Navigation := TCastleExamineNavigation.Create(Application);

  { Prepare (load animations) for all resources.
    In a normal game, you would not call this directly, instead you would
    depend on TLevel.Load doing this for you. }
  Resources.Prepare(Viewport.PrepareParams, 'resources');

  { Level refers to a Viewport.
    We need to create Level, as creature can be spawned only within a Level,
    using CurrentResource.CreateCreature(Level, ...),
    placing it inside "Viewport.Items". }
  Level := TLevel.Create(Application);
  Level.Viewport := Viewport;

  ResourceButtonsGroup := TCastleVerticalGroup.Create(Application);
  ResourceButtonsGroup.Anchor(hpLeft, 10);
  ResourceButtonsGroup.Anchor(vpTop, -10);
  ResourceButtonsGroup.Spacing := 10;
  Window.Controls.InsertFront(ResourceButtonsGroup);

  LoadResourceButton := TLoadResourceButton.Create(Application);
  LoadResourceButton.Caption := 'Add resource...';
  ResourceButtonsGroup.InsertFront(LoadResourceButton);

  ResourceButtons := TResourceButtonList.Create(true);
  UpdateResourceButtons;
  SetCreatureResource(Resources.FindName('KnightSingleGltf') as TCreatureResource);

  Application.Run;

  FreeAndNil(ResourceButtons);
end.
