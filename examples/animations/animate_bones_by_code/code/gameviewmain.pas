{
  Copyright 2022-2025 Michalis Kamburelis.

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
  CastleVectors, CastleComponentSerialize, CastleTimeUtils, CastleSceneCore,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene,
  CastleViewport, CastleTransform, X3DNodes;

type
  TAnimationMode = (
    amNone,
    { Animate most of the body by playing 'walk' animation designed in Blender. }
    amWalk,
    { Animate head by changing TransformNeck.Rotation in Pascal code. }
    amHead,
    { Both amWalk and amHead at the same time. }
    amBoth
  );

  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    LabelSkinUsesShaders: TCastleLabel;
    CheckboxCastShadows: TCastleCheckbox;
    SceneHumanoidNoSkinnedAnim, SceneHumanoidWithSkinnedAnim: TCastleScene;
    ButtonPlayWalk, ButtonPlayHead, ButtonPlayBoth: TCastleButton;
    ButtonNoSkinnedAnim, ButtonWithSkinnedAnim, ButtonTestCrowd: TCastleButton;
    MainViewport: TCastleViewport;
    FactoryCharacterForCrowd: TCastleComponentFactory;
    PlaneGround: TCastlePlane;
  private
    SceneHumanoid: TCastleScene; //< Either SceneHumanoidNoSkinnedAnim or SceneHumanoidWithSkinnedAnim
    TransformNeck: TTransformNode;
    HeadAnimationTime: TFloatTime;
    AnimationMode: TAnimationMode;
    SkinNode: TSkinNode;
    CrowdScenes: TCastleSceneList;
    procedure DebugLogNodeName(Node: TX3DNode);
    procedure ClickPlayWalk(Sender: TObject);
    procedure ClickPlayHead(Sender: TObject);
    procedure ClickPlayBoth(Sender: TObject);
    procedure ClickNoSkinnedAnim(Sender: TObject);
    procedure ClickWithSkinnedAnim(Sender: TObject);
    procedure CheckboxCastShadowsChange(Sender: TObject);
    procedure ClickTestCrowd(Sender: TObject);
    { Switch UI and variables to use
      SceneHumanoidNoSkinnedAnim or SceneHumanoidWithSkinnedAnim }
    procedure ChooseSkinnedAnim(const UseSkinnedAnim: boolean);
    procedure ChooseAnimationMode(const NewAnimationMode: TAnimationMode);
    { Remove all X3D ROUTEs (used by animations) that can affect this Joint. }
    procedure RemoveAnimationsAffectingJoint(
      const Scene: TCastleScene; const Joint: TTransformNode);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

  VerboseLog: Boolean = false;

implementation

uses SysUtils,
  CastleLog, CastleUtils, CastleStringUtils;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.DebugLogNodeName(Node: TX3DNode);
var
  ParentNode: TX3DNode;
  ParentNodeStr, AllParents: String;
  I: Integer;
begin
  AllParents := '';
  if Node.ParentFieldsCount = 0 then
    AllParents := '(no parent)' // possible for root node in non-skinned animation
  else
  begin
    for I := 0 to Node.ParentFieldsCount - 1 do
    begin
      ParentNode := Node.ParentFields[I].ParentNode as TX3DNode;
      ParentNodeStr := ParentNode.NiceName;
      AllParents := SAppendPart(AllParents, ', ', ParentNodeStr);
    end;
  end;

  WritelnLog('Found transformation (possible joint) node: %s, parents: %s', [
    Node.NiceName,
    AllParents
  ]);
end;

procedure TViewMain.Start;
begin
  inherited;

  ButtonPlayWalk.OnClick := {$ifdef FPC}@{$endif} ClickPlayWalk;
  ButtonPlayHead.OnClick := {$ifdef FPC}@{$endif} ClickPlayHead;
  ButtonPlayBoth.OnClick := {$ifdef FPC}@{$endif} ClickPlayBoth;
  ButtonNoSkinnedAnim.OnClick := {$ifdef FPC}@{$endif} ClickNoSkinnedAnim;
  ButtonWithSkinnedAnim.OnClick := {$ifdef FPC}@{$endif} ClickWithSkinnedAnim;
  ButtonTestCrowd.OnClick := {$ifdef FPC}@{$endif} ClickTestCrowd;
  CheckboxCastShadows.OnChange := {$ifdef FPC}@{$endif} CheckboxCastShadowsChange;

  { Remove any animation affecting Neck from SceneHumanoidWithSkinnedAnim.
    This simple solution avoids tweaking animation on the Blender side,
    we just "disconnect" it from changing the Neck on the engine side.
    And we need to do this, to show amBoth mode on SceneHumanoidWithSkinnedAnim,
    where Pascal code animates Neck. }
  RemoveAnimationsAffectingJoint(SceneHumanoidWithSkinnedAnim,
    SceneHumanoidWithSkinnedAnim.Node('Neck') as TTransformNode);

  { In normal usage, you don't need to access the TSkinNode inside.
    We access it only to debug whether InternalUsesShaders is true. }
  SkinNode := SceneHumanoidWithSkinnedAnim.Node('HumanArmature') as TSkinNode;

  // change all UI and variables to use SceneHumanoidWithSkinnedAnim and hide SceneHumanoidNoSkinnedAnim
  ClickWithSkinnedAnim(nil);

  CrowdScenes := TCastleSceneList.Create(false);
end;

procedure TViewMain.Stop;
begin
  FreeAndNil(CrowdScenes);
  inherited;
end;

procedure TViewMain.ClickNoSkinnedAnim(Sender: TObject);
begin
  ChooseSkinnedAnim(false);
end;

procedure TViewMain.ClickWithSkinnedAnim(Sender: TObject);
begin
  ChooseSkinnedAnim(true);
end;

procedure TViewMain.ChooseSkinnedAnim(const UseSkinnedAnim: boolean);
begin
  if UseSkinnedAnim then
    SceneHumanoid := SceneHumanoidWithSkinnedAnim
  else
    SceneHumanoid := SceneHumanoidNoSkinnedAnim;

  { for debug: write to log transform node names }
  if VerboseLog then
    SceneHumanoid.RootNode.EnumerateNodes(TTransformNode,
      {$ifdef FPC}@{$endif} DebugLogNodeName, false);

  TransformNeck := SceneHumanoid.Node('Neck') as TTransformNode;

  SceneHumanoidWithSkinnedAnim.Exists := UseSkinnedAnim;
  SceneHumanoidNoSkinnedAnim.Exists := not UseSkinnedAnim;

  ButtonWithSkinnedAnim.Pressed := UseSkinnedAnim;
  ButtonNoSkinnedAnim.Pressed := not UseSkinnedAnim;

  LabelSkinUsesShaders.Exists := UseSkinnedAnim;

  // reinitialize AnimationMode, to e.g. start 'walk' on new model
  ChooseAnimationMode(AnimationMode);
end;

procedure TViewMain.ClickPlayWalk(Sender: TObject);
begin
  ChooseAnimationMode(amWalk);
end;

procedure TViewMain.ClickPlayHead(Sender: TObject);
begin
  ChooseAnimationMode(amHead);
end;

procedure TViewMain.ClickPlayBoth(Sender: TObject);
begin
  ChooseAnimationMode(amBoth);
end;

procedure TViewMain.ChooseAnimationMode(const NewAnimationMode: TAnimationMode);
begin
  if NewAnimationMode in [amWalk, amBoth] then
    SceneHumanoid.AutoAnimation := 'walk'
  else
    SceneHumanoid.AutoAnimation := ''; // stop walking

  AnimationMode := NewAnimationMode;

  // update buttons
  ButtonPlayWalk.Pressed := AnimationMode = amWalk;
  ButtonPlayHead.Pressed := AnimationMode = amHead;
  ButtonPlayBoth.Pressed := AnimationMode = amBoth;
end;

procedure TViewMain.CheckboxCastShadowsChange(Sender: TObject);
var
  Scene: TCastleScene;
begin
  SceneHumanoidNoSkinnedAnim.CastShadows := CheckboxCastShadows.Checked;
  SceneHumanoidWithSkinnedAnim.CastShadows := CheckboxCastShadows.Checked;
  for Scene in CrowdScenes do
    Scene.CastShadows := CheckboxCastShadows.Checked;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);

  function AnimateKeyFrames(const TimeNow: TFloatTime;
    const KeyTimes: array of Single;
    const KeyValues: array of Single): Single;
  var
    T: Single;
    Range: Integer;
  begin
    Assert(High(KeyTimes) = High(KeyValues));
    Assert(High(KeyTimes) >= 1); // we need at least 2 keyframes
    Assert(KeyTimes[0] = 0); // this logic is only prepared for case when KeyTimes[0] = 0

    T := FloatModulo(TimeNow, KeyTimes[High(KeyTimes)]);
    for Range := 1 to High(KeyTimes) do // TODO: do binary search, knowing that KeyTimes are sorted - faster
      if KeyTimes[Range] >= T then
        Break;

    Result := MapRange(T,
      KeyTimes[Range - 1], KeyTimes[Range],
      KeyValues[Range - 1], KeyValues[Range]);
  end;

begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  LabelSkinUsesShaders.Caption :=
    'Skin is calculated using shaders (faster, but not always possible): ' +
    BoolToStr(SkinNode.InternalUsesShaders, true);

  if AnimationMode in [amHead, amBoth] then
  begin
    HeadAnimationTime := HeadAnimationTime + SecondsPassed;
    TransformNeck.Rotation := Vector4(
      // rotation axis
      1, 0, 0,
      // rotation angle
      AnimateKeyFrames(HeadAnimationTime,
        [    0,  0.5,     1],
        [-Pi/6, Pi/6, -Pi/6]
      )
    );
  end;
end;

type
  { Utility for TViewMain.RemoveAnimationsAffectingJoint }
  TRemoveAnimationsAffectingJointUtility = class
  public
    Joint: TTransformNode;
    procedure Handler(Node: TX3DNode);
  end;

procedure TRemoveAnimationsAffectingJointUtility.Handler(Node: TX3DNode);
var
  Route: TX3DRoute;
  I: Integer;
begin
  I := 0;
  while I < Node.RoutesCount do
  begin
    Route := Node.Routes[I];
    if Route.DestinationNode = Joint then
    begin
      if VerboseLog then
        WritelnLog('Removing ROUTE affecting %s.%s (from %s.%s)', [
          Route.DestinationNode.NiceName,
          Route.DestinationEvent.X3DName,
          Route.SourceNode.NiceName,
          Route.SourceEvent.X3DName
        ]);
      Node.RemoveRoute(I);
    end else
      Inc(I);
  end;
end;

procedure TViewMain.RemoveAnimationsAffectingJoint(
  const Scene: TCastleScene; const Joint: TTransformNode);
var
  Utility: TRemoveAnimationsAffectingJointUtility;
begin
  if Scene.RootNode = nil then
    Exit; // nothing to do on empty scene

  Utility := TRemoveAnimationsAffectingJointUtility.Create;
  try
    Utility.Joint := Joint;
    Scene.RootNode.EnumerateNodes(TX3DNode,
      {$ifdef FPC}@{$endif} Utility.Handler, false);
  finally FreeAndNil(Utility) end;
end;

procedure TViewMain.ClickTestCrowd(Sender: TObject);

{ Create clones of TCastleScene using TCastleComponentFactory
  (see https://castle-engine.io/reuse_design ).
  They all play an animation (at different time) to experiment how many
  (different) skinned animations we can handle without performance problems.

  We also create even more copies of it using TCastleTransformReference
  (such copy is linked to the same scene instance, so it's even more efficient,
  but has to show the same animation, same animation time).
  Just to show we may make a real crowd.
}

  // Add to each instance position, to make them look less regular.
  {
  function RandomShift: TVector3;
  const
    Spread = 0.5;
  begin
    Result := Vector3(
      RandomFloatRange(-Spread, Spread),
      0,
      RandomFloatRange(-Spread, Spread)
    );
  end;
  }

const
  InstancesCount = 8; // We will make InstancesCount^2 new instances
  ReferencesCount = 3; // Each of InstancesCount^2 will have ReferencesCount^2-1 additional references
  InstanceShift: TVector3 = (X: 2; Y: 0; Z: 0); // shift to avoid colliding with SceneHumanoidWithSkinnedAnim
  InstanceSpread: TVector3 = (X: 1.5; Y: 0; Z: -1.5);
  { Animations of female model that look good as looping animations. }
  FemaleLoopingAnimations: array [0..7] of String = (
    'Female_Clapping',
    'Female_Jump',
    'Female_Punch',
    'Female_Roll',
    'Female_Run',
    'Female_RunningJump',
    'Female_SwordSlash',
    'walk'
  );
var
  X, Z, TX, TZ: Integer;
  NewScene: TCastleScene;
  NewCharacter: TCastleTransform;
  NewReference: TCastleTransformReference;
  Params: TPlayAnimationParameters;
begin
  { TODO: Turn off shadows for crowd.
    Reason: shadow volumes now disable skinned animation on GPU, making
    it prohibitively slow for crowd (you can toggle CheckboxCastShadows to see it).
    We will solve this by making shadow maps easier to set up, https://castle-engine.io/roadmap#shadow_maps }
  CheckboxCastShadows.Checked := false;
  CheckboxCastShadowsChange(nil);

  for X := 0 to InstancesCount - 1 do
    for Z := 0 to InstancesCount - 1 do
    begin
      NewCharacter := FactoryCharacterForCrowd.ComponentLoad(FreeAtStop) as TCastleTransform;
      NewScene := NewCharacter[0] as TCastleScene; // we know it's TCastleScene
      CrowdScenes.Add(NewScene);

      NewScene.Translation := {RandomShift +}
        InstanceShift +
        InstanceSpread * Vector3(X, 0, Z);
      NewScene.CastShadows := CheckboxCastShadows.Checked;

      //NewScene.PlayAnimation('walk', true);
      Params := TPlayAnimationParameters.Create;
      try
        Params.Name := //'walk';
          // choose random animation, to show that we can play different animations
          //NewScene.AnimationsList[Random(NewScene.AnimationsList.Count)];
          FemaleLoopingAnimations[Random(Length(FemaleLoopingAnimations))];
        Params.Loop := true;
        { Randomize start a bit, to have each instance start at different time,
          to show this is more powerful than TCastleTransformReference
          as all instances of TCastleTransformReference can play different
          animation. }
        Params.InitialTime := Random * NewScene.AnimationDuration(Params.Name) * 0.75;
        NewScene.PlayAnimation(Params);
      finally FreeAndNil(Params) end;

      MainViewport.Items.Add(NewScene);

      // create ReferencesCount^2-1 references to NewScene
      for TX := 0 to ReferencesCount - 1 do
        for TZ := 0 to ReferencesCount - 1 do
        begin
          if (TX = 0) and (TZ = 0) then
            Continue; // original instance, not a reference

          NewReference := TCastleTransformReference.Create(FreeAtStop);
          NewReference.Reference := NewScene;
          NewReference.Scale := NewScene.Scale;
          NewReference.Translation := {RandomShift +}
            InstanceShift +
            InstanceSpread * Vector3(X, 0, Z) +
            Vector3(
              InstancesCount * Abs(InstanceSpread.X),
              0,
              -InstancesCount * Abs(InstanceSpread.Z)
            ) * Vector3(TX, 0, TZ);
          MainViewport.Items.Add(NewReference);
        end;
    end;

  PlaneGround.Size := Vector2(
    ReferencesCount * InstancesCount * Abs(InstanceSpread.X) * 2,
    ReferencesCount * InstancesCount * Abs(InstanceSpread.Z) * 2);

  // calling it multiple times would look bad with overlapping characters
  ButtonTestCrowd.Enabled := false;

  WritelnLog('Added crowd with %d characters. %d of them have different animation time', [
    Sqr(InstancesCount) * Sqr(ReferencesCount),
    Sqr(InstancesCount)
  ]);
end;

end.
