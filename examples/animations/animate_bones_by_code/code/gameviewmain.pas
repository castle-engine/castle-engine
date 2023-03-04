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
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    SceneHumanoid: TCastleScene;
    ButtonPlayWalk, ButtonPlayHead, ButtonPlayBoth: TCastleButton;
  private
    { Other fields }
    TransformNeck: TTransformNode;
    HeadAnimation: Boolean; //< whether we play animation of TransformNeck, controled by code
    HeadAnimationTime: TFloatTime;
    procedure DebugLogNodeName(Node: TX3DNode);
    procedure ClickPlayWalk(Sender: TObject);
    procedure ClickPlayHead(Sender: TObject);
    procedure ClickPlayBoth(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleLog, CastleUtils;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.DebugLogNodeName(Node: TX3DNode);
var
  ParentNode: TX3DNode;
  ParentNodeStr: String;
begin
  case Node.ParentFieldsCount of
    0: ParentNodeStr := '(no parent)'; // possible for root node
    1: begin
         ParentNode := Node.ParentFields[0].ParentNode as TX3DNode;
         ParentNodeStr := ParentNode.X3DName;
         if ParentNodeStr = '' then
           ParentNodeStr := '(unnamed)';
       end;
    else
       ParentNodeStr := '(multiple parents)';
  end;

  WritelnLog('Found node: %s, parent: %s', [
    Node.X3DName,
    ParentNodeStr
  ]);
end;

procedure TViewMain.Start;
begin
  inherited;

  { for debug: write to log all transform nodes }
  SceneHumanoid.RootNode.EnumerateNodes(TTransformNode, {$ifdef FPC}@{$endif} DebugLogNodeName, false);

  ButtonPlayWalk.OnClick := {$ifdef FPC}@{$endif} ClickPlayWalk;
  ButtonPlayHead.OnClick := {$ifdef FPC}@{$endif} ClickPlayHead;
  ButtonPlayBoth.OnClick := {$ifdef FPC}@{$endif} ClickPlayBoth;

  TransformNeck := SceneHumanoid.Node('Neck') as TTransformNode;
end;

procedure TViewMain.ClickPlayWalk(Sender: TObject);
begin
  SceneHumanoid.AutoAnimation := 'walk';
  HeadAnimation := false;
end;

procedure TViewMain.ClickPlayHead(Sender: TObject);
begin
  SceneHumanoid.AutoAnimation := ''; // stop walking
  HeadAnimation := true;
end;

procedure TViewMain.ClickPlayBoth(Sender: TObject);
begin
  SceneHumanoid.AutoAnimation := 'walk';
  HeadAnimation := true;
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

  if HeadAnimation then
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
