{
  Copyright 2009-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Play the animations of resources (creatures/items). }

uses SysUtils, FGL, CastleFilesUtils, CastleWindow, CastleResources, CastleScene,
  CastleProgress, ProgressUnit, CastleControls, UIControls, CastleUtils, Base3D,
  CastleSoundEngine;

var
  BaseScene: TCastleScene;
  Window: TCastleWindow;
  Resource: T3DResource;
  Animation: T3DResourceAnimation;

{ 3D things ------------------------------------------------------------------ }

type
  { Display current Animation in a loop.

    In a normal game, you would never display T3DResourceAnimation like this.
    Actually, in a normal game, you would never deal directly with
    T3DResourceAnimation. Instead, in a game, resource logic
    (like TWalkAttackCreature implementation) chooses appropriate animation
    (and whether it's looping), based on creature state (whether it's dead,
    attacking and such), and the animation is displayed because
    creature/item is already a part of 3D world.

    Here, to directly show the animation, we go around the normal CastleCreatures
    and CastleItems mechanisms, and we directly get the animation. }
  TLoopAnimation = class(T3DList)
  public
    Time: Single;
    function GetChild: T3D; override;
    procedure Idle(const CompSpeed: Single; var RemoveMe: TRemoveType); override;
  end;

function TLoopAnimation.GetChild: T3D;
begin
  if not (GetExists and Resource.Prepared) then Exit;
  Result := Animation.Scene(Time, true);
end;

procedure TLoopAnimation.Idle(const CompSpeed: Single; var RemoveMe: TRemoveType);
begin
  Time += CompSpeed;
  VisibleChangeHere([vcVisibleGeometry]);
end;

var
  LoopAnimation: TLoopAnimation;

{ 2D things (buttons) -------------------------------------------------------- }

type
  TUpdateCurrentResource = (
    ucpActivateFirst,
    ucpActivateLast,
    ucpKeep);

procedure Resize(Window: TCastleWindowBase); forward;
procedure UpdateButtons(const UpdateCurrentResource: TUpdateCurrentResource); forward;

type
  TButtonSwitchResource = class(TCastleButton)
  public
    ResourceName: string;
    procedure DoClick; override;
  end;

procedure TButtonSwitchResource.DoClick;
begin
  Resource := Resources.FindName(ResourceName);
  UpdateButtons(ucpKeep);
end;

type
  TButtonSwitchAnimation = class(TCastleButton)
  public
    AnimationName: string;
    procedure DoClick; override;
  end;

procedure TButtonSwitchAnimation.DoClick;
begin
  Animation := Resource.Animations.FindName(AnimationName);
  LoopAnimation.Time := 0;
end;

type
  TLoadResourceButton = class(TCastleButton)
  public
    { remember this only to make repeated usage of FileDialog more comfortable }
    LastChosenFileName: string;
    procedure DoClick; override;
  end;

procedure TLoadResourceButton.DoClick;
begin
  if Window.FileDialog('Resource file to load', LastChosenFileName, true,
    'All Files|*|*Resource files (resource.xml)|resource.xml|') then
  begin
    Resources.LoadResourceFile(LastChosenFileName);
    { directly prepare new resource }
    Resources.Prepare(
      Window.SceneManager.Items.BaseLights,
      Window.SceneManager.Items.GravityUp, 'resources');
    UpdateButtons(ucpActivateLast);
  end;
end;

type
  TCastleButtonList = specialize TFPGObjectList<TCastleButton>;

var
  ResButtons, AnimButtons: TCastleButtonList;
  LoadResourceButton: TLoadResourceButton;

procedure UpdateButtons(const UpdateCurrentResource: TUpdateCurrentResource);
var
  ResButton: TButtonSwitchResource;
  AnimButton: TButtonSwitchAnimation;
  DefaultAnim: T3DResourceAnimation;
  I: Integer;
begin
  { easily destroy all existing buttons using the XxxButtons list,
    destroying them also automatically removed them from Window.Controls list }
  ResButtons.Clear;
  AnimButtons.Clear;

  for I := 0 to Resources.Count - 1 do
  begin
    ResButton := TButtonSwitchResource.Create(nil);
    ResButton.Caption := Resources[I].Name;
    ResButton.ResourceName := Resources[I].Name;
    ResButtons.Add(ResButton);
    Window.Controls.Add(ResButton);
  end;
  if Resources.Count = 0 then
    raise Exception.CreateFmt('No resources found. Make sure we search in proper path (current data path is detected as "%s")', [ProgramDataPath]);
  case UpdateCurrentResource of
    ucpActivateFirst: Resource := Resources.First;
    ucpActivateLast : Resource := Resources.Last;
  end;

  DefaultAnim := nil;
  for I := 0 to Resource.Animations.Count - 1 do
    if Resource.Animations[I].Defined then
    begin
      if DefaultAnim = nil then
        DefaultAnim := Resource.Animations[I];
      AnimButton := TButtonSwitchAnimation.Create(nil);
      AnimButton.Caption := Resource.Animations[I].Name;
      AnimButton.AnimationName := Resource.Animations[I].Name;
      AnimButtons.Add(AnimButton);
      Window.Controls.Add(AnimButton);
    end;
  if DefaultAnim = nil then
    raise Exception.CreateFmt('No (defined) animation found in resource "%s"', [Resource.Name]);
  Animation := DefaultAnim;

  { update buttons sizes and positions using Resize }
  Resize(Window);
end;

procedure Resize(Window: TCastleWindowBase);
const
  Margin = 8;
var
  MaxWidth: Cardinal;
  Bottom, I: Integer;
begin
  MaxWidth := 0;
  for I := 0 to ResButtons.Count - 1 do
    MaxTo1st(MaxWidth, ResButtons[I].Width);

  Bottom := Window.Height;
  for I := 0 to ResButtons.Count - 1 do
  begin
    Bottom -= Margin + ResButtons[I].Height;
    ResButtons[I].Bottom := Bottom;
    ResButtons[I].Left := Margin;
    ResButtons[I].AutoSizeWidth := false;
    ResButtons[I].Width := MaxWidth;
  end;

  Bottom -= Margin * 2 + ResButtons[I].Height;
  LoadResourceButton.Bottom := Bottom;
  LoadResourceButton.Left := Margin;

  MaxWidth := 0;
  for I := 0 to AnimButtons.Count - 1 do
    MaxTo1st(MaxWidth, AnimButtons[I].Width);

  Bottom := Window.Height;
  for I := 0 to AnimButtons.Count - 1 do
  begin
    Bottom -= Margin + AnimButtons[I].Height;
    AnimButtons[I].Bottom := Bottom;
    AnimButtons[I].Left := Window.Width - Margin - MaxWidth;
    AnimButtons[I].AutoSizeWidth := false;
    AnimButtons[I].Width := MaxWidth;
  end;
end;

{ Main program --------------------------------------------------------------- }

begin
  Window := TCastleWindow.Create(Application);
  WindowProgressInterface.Window := Window;
  Progress.UserInterface := WindowProgressInterface;

  { otherwise if you use "Add resource..." button and load a resource
    using sounds, we may get errors about undefined sound names. }
  IgnoreAllMissingSounds := true;

  Resources.LoadFromFiles;

  { load basic 3D scene where creature is shown. This isn't necessary,
    but it's an easy way to add a camera with headlight,
    and some grid to help with orientation. }
  BaseScene := TCastleScene.Create(Application);
  BaseScene.Load(ProgramDataPath + 'data' + PathDelim + 'base.x3d');
  { turn on headlight, as base.x3d exported from Blender has always headlight=false }
  BaseScene.NavigationInfoStack.Top.FdHeadlight.Send(true);
  Window.SceneManager.MainScene := BaseScene;
  Window.SceneManager.Items.Add(BaseScene);

  { Open the window, as OpenGL context must be ready before Resources.Prepare. }
  Window.Open;

  { Prepare (load animations) for all resources.
    In a normal game, you would not call this directly, instead you would
    depend on TGameSceneManager.LoadLevel doing this for you. }
  Resources.Prepare(
    Window.SceneManager.Items.BaseLights,
    Window.SceneManager.Items.GravityUp, 'resources');

  LoadResourceButton := TLoadResourceButton.Create(Application);
  LoadResourceButton.Caption := 'Add resource...';
  Window.Controls.Add(LoadResourceButton);
  ResButtons := TCastleButtonList.Create(true);
  AnimButtons := TCastleButtonList.Create(true);
  UpdateButtons(ucpActivateFirst);

  LoopAnimation := TLoopAnimation.Create(Application);
  Window.SceneManager.Items.Add(LoopAnimation);

  Window.OnResize := @Resize;

  Application.Run;

  FreeAndNil(ResButtons);
  FreeAndNil(AnimButtons);
end.
