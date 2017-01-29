{
  Copyright 2009-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Play the animations of resources (creatures/items). }

uses SysUtils, FGL,
  CastleFilesUtils, CastleWindow, CastleResources, CastleScene,
  CastleProgress, CastleWindowProgress, CastleControls, CastleUIControls,
  CastleUtils, Castle3D, CastleSoundEngine, CastleCreatures, CastleLog;

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
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;

function TLoopAnimation.GetChild: T3D;
begin
  if not (GetExists and Resource.Prepared) then Exit;
  Result := Animation.Scene(Time, true);
end;

procedure TLoopAnimation.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  Time += SecondsPassed;
  VisibleChangeHere([vcVisibleGeometry]);
end;

var
  LoopAnimation: TLoopAnimation;

{ 2D things (buttons) -------------------------------------------------------- }

type
  TUpdateCurrentResource = (
    ucpActivateFirst,
    ucpActivateLast,
    ucpUpdateOnlyAnimations);

procedure Resize(Container: TUIContainer); forward;
procedure UpdateButtons(const UpdateCurrentResource: TUpdateCurrentResource); forward;

type
  TResourceButton = class(TCastleButton)
  public
    ButtonResource: T3DResource;
    procedure DoClick; override;
  end;

  TAnimationButton = class(TCastleButton)
  public
    ButtonAnimation: T3DResourceAnimation;
    procedure DoClick; override;
  end;

  TLoadResourceButton = class(TCastleButton)
  public
    { remember this only to make repeated usage of FileDialog more comfortable }
    LastChosenURL: string;
    procedure DoClick; override;
  end;

  TResourceButtonList = specialize TFPGObjectList<TResourceButton>;
  TAnimationButtonList = specialize TFPGObjectList<TAnimationButton>;

var
  ResButtons: TResourceButtonList;
  AnimButtons: TAnimationButtonList;
  LoadResourceButton: TLoadResourceButton;

procedure TResourceButton.DoClick;
var
  I: Integer;
begin
  Resource := ButtonResource;
  { update Pressed of buttons }
  for I := 0 to ResButtons.Count - 1 do
    ResButtons[I].Pressed := ResButtons[I].ButtonResource = Resource;
  { load buttons for animations of currently selected Resource }
  UpdateButtons(ucpUpdateOnlyAnimations);
end;

procedure TAnimationButton.DoClick;
var
  I: Integer;
begin
  Animation := ButtonAnimation;
  { update Pressed of buttons }
  for I := 0 to AnimButtons.Count - 1 do
    AnimButtons[I].Pressed := AnimButtons[I].ButtonAnimation = Animation;
  { reset time }
  LoopAnimation.Time := 0;
end;

procedure TLoadResourceButton.DoClick;
begin
  if Window.FileDialog('Resource file to load', LastChosenURL, true,
    'All Files|*|*Resource files (resource.xml)|resource.xml|') then
  begin
    Resources.AddFromFile(LastChosenURL);
    { directly prepare new resource }
    Resources.Prepare(
      Window.SceneManager.Items.BaseLights,
      Window.SceneManager.Items.GravityUp, 'resources');
    UpdateButtons(ucpActivateLast);
  end;
end;

procedure UpdateButtons(const UpdateCurrentResource: TUpdateCurrentResource);
var
  ResButton: TResourceButton;
  AnimButton: TAnimationButton;
  I: Integer;
begin
  if UpdateCurrentResource <> ucpUpdateOnlyAnimations then
  begin
    { easily destroy all existing buttons using the XxxButtons list,
      destroying them also automatically removed them from Window.Controls list }
    ResButtons.Clear;

    for I := 0 to Resources.Count - 1 do
    begin
      ResButton := TResourceButton.Create(nil);
      ResButton.ButtonResource := Resources[I];
      ResButton.Caption := ResButton.ButtonResource.Name;
      ResButton.Toggle := true;
      ResButtons.Add(ResButton);
      Window.Controls.InsertFront(ResButton);
    end;
    if Resources.Count = 0 then
      raise Exception.CreateFmt('No resources found. Make sure we search in proper path (current data path is detected as "%s")', [ApplicationData('')]);
    case UpdateCurrentResource of
      ucpActivateFirst:
        begin
          Resource := Resources.First;
          ResButtons.First.Pressed := true;
        end;
      ucpActivateLast :
        begin
          Resource := Resources.Last;
          ResButtons.Last.Pressed := true;
        end;
    end;
  end;

  AnimButtons.Clear;
  Animation := nil;
  for I := 0 to Resource.Animations.Count - 1 do
    if Resource.Animations[I].Defined then
    begin
      AnimButton := TAnimationButton.Create(nil);
      AnimButton.ButtonAnimation := Resource.Animations[I];
      AnimButton.Caption := AnimButton.ButtonAnimation.Name;
      AnimButton.Toggle := true;
      if Animation = nil then
      begin
        Animation := Resource.Animations[I];
        AnimButton.Pressed := true;
      end;
      AnimButtons.Add(AnimButton);
      Window.Controls.InsertFront(AnimButton);
    end;
  if Animation = nil then
    raise Exception.CreateFmt('No (defined) animation found in resource "%s"', [Resource.Name]);

  { update buttons sizes and positions using Resize }
  Resize(Window.Container);
end;

procedure Resize(Container: TUIContainer);
const
  Margin = 8;
var
  MaxWidth: Cardinal;
  Bottom, I: Integer;
begin
  MaxWidth := 0;
  for I := 0 to ResButtons.Count - 1 do
    MaxVar(MaxWidth, ResButtons[I].CalculatedWidth);

  Bottom := Window.Height;
  for I := 0 to ResButtons.Count - 1 do
  begin
    Bottom -= Margin + ResButtons[I].CalculatedHeight;
    ResButtons[I].Bottom := Bottom;
    ResButtons[I].Left := Margin;
    ResButtons[I].AutoSizeWidth := false;
    ResButtons[I].Width := MaxWidth;
  end;

  Bottom -= Margin * 2 + ResButtons[I].CalculatedHeight;
  LoadResourceButton.Bottom := Bottom;
  LoadResourceButton.Left := Margin;

  MaxWidth := 0;
  for I := 0 to AnimButtons.Count - 1 do
    MaxVar(MaxWidth, AnimButtons[I].CalculatedWidth);

  Bottom := Window.Height;
  for I := 0 to AnimButtons.Count - 1 do
  begin
    Bottom -= Margin + AnimButtons[I].CalculatedHeight;
    AnimButtons[I].Bottom := Bottom;
    AnimButtons[I].Left := Window.Width - Margin - MaxWidth;
    AnimButtons[I].AutoSizeWidth := false;
    AnimButtons[I].Width := MaxWidth;
  end;
end;

{ TestAddingResourceByCode --------------------------------------------------- }

{ An example of creating a resource (TStillCreatureResource in this case)
  without resource.xml file. You just create an instance of TXxxResource by hand,
  fill the properties you need, and add it to global Resources list. }
procedure TestAddingResourceByCode;
var
  Res: TStillCreatureResource;
begin
  Res := TStillCreatureResource.Create('KnightCreatedFromCodeTest');
  Res.Animations.FindName('idle').URL := ApplicationData('knight_multiple_castle_anim_frames/idle.castle-anim-frames');
  Res.Animations.FindName('die').URL := ApplicationData('knight_multiple_castle_anim_frames/die.castle-anim-frames');
  Resources.Add(Res);
end;

{ Main program --------------------------------------------------------------- }

begin
  InitializeLog;

  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window;
  Progress.UserInterface := WindowProgressInterface;

  Resources.LoadFromFiles;

  TestAddingResourceByCode;

  { load basic 3D scene where creature is shown. This isn't necessary,
    but it's an easy way to add a camera with headlight,
    and some grid to help with orientation. }
  BaseScene := TCastleScene.Create(Application);
  BaseScene.Load(ApplicationData('base.x3d'));
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
  Window.Controls.InsertFront(LoadResourceButton);
  ResButtons := TResourceButtonList.Create(true);
  AnimButtons := TAnimationButtonList.Create(true);
  UpdateButtons(ucpActivateFirst);

  LoopAnimation := TLoopAnimation.Create(Application);
  Window.SceneManager.Items.Add(LoopAnimation);

  Window.OnResize := @Resize;

  Application.Run;

  FreeAndNil(ResButtons);
  FreeAndNil(AnimButtons);
end.
