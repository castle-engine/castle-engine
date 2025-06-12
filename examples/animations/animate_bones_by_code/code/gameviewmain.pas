{
  Copyright 2022-2023 Michalis Kamburelis.

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
  CastleVectors, CastleComponentSerialize, CastleTimeUtils,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene,
  X3DNodes;

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
    SceneHumanoidNoSkinnedAnim, SceneHumanoidWithSkinnedAnim: TCastleScene;
    ButtonPlayWalk, ButtonPlayHead, ButtonPlayBoth: TCastleButton;
    ButtonNoSkinnedAnim, ButtonWithSkinnedAnim: TCastleButton;
  private
    SceneHumanoid: TCastleScene; //< Either SceneHumanoidNoSkinnedAnim or SceneHumanoidWithSkinnedAnim
    TransformNeck: TTransformNode;
    HeadAnimationTime: TFloatTime;
    AnimationMode: TAnimationMode;
    procedure DebugLogNodeName(Node: TX3DNode);
    procedure ClickPlayWalk(Sender: TObject);
    procedure ClickPlayHead(Sender: TObject);
    procedure ClickPlayBoth(Sender: TObject);
    procedure ClickNoSkinnedAnim(Sender: TObject);
    procedure ClickWithSkinnedAnim(Sender: TObject);
    { Switch UI and variables to use
      SceneHumanoidNoSkinnedAnim or SceneHumanoidWithSkinnedAnim }
    procedure ChooseSkinnedAnim(const UseSkinnedAnim: boolean);
    procedure ChooseAnimationMode(const NewAnimationMode: TAnimationMode);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

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

  WritelnLog('Found node: %s, parents: %s', [
    Node.X3DName,
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

  // change all UI and variables to use SceneHumanoidWithSkinnedAnim and hide SceneHumanoidNoSkinnedAnim
  ClickWithSkinnedAnim(nil);
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

  { for debug: write to log all transform nodes }
  SceneHumanoid.RootNode.EnumerateNodes(TTransformNode,
    {$ifdef FPC}@{$endif} DebugLogNodeName, false);

  TransformNeck := SceneHumanoid.Node('Neck') as TTransformNode;

  SceneHumanoidWithSkinnedAnim.Exists := UseSkinnedAnim;
  SceneHumanoidNoSkinnedAnim.Exists := not UseSkinnedAnim;

  ButtonWithSkinnedAnim.Pressed := UseSkinnedAnim;
  ButtonNoSkinnedAnim.Pressed := not UseSkinnedAnim;

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

end.
