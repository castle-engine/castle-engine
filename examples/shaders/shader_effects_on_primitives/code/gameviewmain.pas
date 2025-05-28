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

{ Main view, where most of the application logic takes place. }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize, CastleScene,
  CastleUIControls, CastleControls, CastleKeysMouse, X3DNodes;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    BoxForShader: TCastleBox;
    SphereForShader: TCastleSphere;
    ImageForShader: TCastleImageTransform;
    ButtonEffectNone, ButtonEffectColor, ButtonEffectEnlarge, ButtonEffectBoth: TCastleButton;
  private
    { Each scene (so also each primitive) must use different TEffectNode instance,
      we we keep 3 copies of each:
      for BoxForShader, SphereForShader, and ImageForShader. }
    EffectColor, EffectEnlarge: array [0..2] of TEffectNode;
    procedure ClickEffectNone(Sender: TObject);
    procedure ClickEffectColor(Sender: TObject);
    procedure ClickEffectEnlarge(Sender: TObject);
    procedure ClickEffectBoth(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleRenderOptions;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;

  procedure CreateEffectColor;
  var
    Effect: TEffectNode;
    EffectPart: TEffectPartNode;
    I: Integer;
  begin
    Effect := TEffectNode.Create;
    Effect.Language := slGLSL;

    EffectPart := TEffectPartNode.Create;
    EffectPart.ShaderType := stFragment;
    EffectPart.SetUrl(['castle-data:/shaders/color.fs']);
    Effect.SetParts([EffectPart]);

    EffectColor[0] := Effect;
    EffectColor[1] := Effect.DeepCopy as TEffectNode;
    EffectColor[2] := Effect.DeepCopy as TEffectNode;

    { We will free it manually, to avoid refcounting interfering when we
      add / remove effects on components. (Refcounting would detect
      the effect nodes as unused and free them.) }
    for I := 0 to 2 do
      EffectColor[I].WaitForRelease;
  end;

  procedure CreateEffectEnlarge;
  var
    Effect: TEffectNode;
    EffectPart: TEffectPartNode;
    I: Integer;
  begin
    Effect := TEffectNode.Create;
    Effect.Language := slGLSL;

    EffectPart := TEffectPartNode.Create;
    EffectPart.ShaderType := stVertex;
    EffectPart.SetUrl(['castle-data:/shaders/enlarge.vs']);
    Effect.SetParts([EffectPart]);

    EffectEnlarge[0] := Effect;
    EffectEnlarge[1] := Effect.DeepCopy as TEffectNode;
    EffectEnlarge[2] := Effect.DeepCopy as TEffectNode;

    { We will free it manually, to avoid refcounting interfering when we
      add / remove effects on components. (Refcounting would detect
      the effect nodes as unused and free them.) }
    for I := 0 to 2 do
      EffectEnlarge[I].WaitForRelease;
  end;

begin
  inherited;
  CreateEffectColor;
  CreateEffectEnlarge;
  ButtonEffectNone.OnClick := {$ifdef FPC}@{$endif} ClickEffectNone;
  ButtonEffectColor.OnClick := {$ifdef FPC}@{$endif} ClickEffectColor;
  ButtonEffectEnlarge.OnClick := {$ifdef FPC}@{$endif} ClickEffectEnlarge;
  ButtonEffectBoth.OnClick := {$ifdef FPC}@{$endif} ClickEffectBoth;
end;

procedure TViewMain.Stop;
var
  I: Integer;
begin
  inherited;
  // free the effect nodes
  for I := 0 to 2 do
  begin
    NodeRelease(EffectColor[I]);
    NodeRelease(EffectEnlarge[I]);
  end;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TViewMain.ClickEffectNone(Sender: TObject);
begin
  BoxForShader.SetEffects([]);
  SphereForShader.SetEffects([]);
  ImageForShader.SetEffects([]);
end;

procedure TViewMain.ClickEffectColor(Sender: TObject);
begin
  BoxForShader.SetEffects([EffectColor[0]]);
  SphereForShader.SetEffects([EffectColor[1]]);
  ImageForShader.SetEffects([EffectColor[2]]);
end;

procedure TViewMain.ClickEffectEnlarge(Sender: TObject);
begin
  BoxForShader.SetEffects([EffectEnlarge[0]]);
  SphereForShader.SetEffects([EffectEnlarge[1]]);
  ImageForShader.SetEffects([EffectEnlarge[2]]);
end;

procedure TViewMain.ClickEffectBoth(Sender: TObject);
begin
  BoxForShader.SetEffects([EffectColor[0], EffectEnlarge[0]]);
  SphereForShader.SetEffects([EffectColor[1], EffectEnlarge[1]]);
  ImageForShader.SetEffects([EffectColor[2], EffectEnlarge[2]]);
end;

end.
