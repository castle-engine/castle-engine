{
  Copyright 2026 Popa Marius Adrian.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Shader effects on light sources.

  The design (data/gameviewmain.castle-user-interface) has three lamps
  (TCastlePointLight), each lighting its own floor tile.
  Here we add to each lamp an Effect, using TCastleAbstractLight.SetEffects.
  All effects have the same GLSL code (data/shaders/rings_light.fs):
  the light shines in rings around the lamp.
  Each effect has its own value of the uniform "ring_count",
  so every tile shows a different number of rings. }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleScene, X3DNodes, X3DFields;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelInfo: TCastleLabel;
    Lamp1, Lamp2, Lamp3: TCastlePointLight;
  private
    { The "ring_count" field of each lamp's effect (a uniform in the shader). }
    RingCount: array [0..2] of TSFFloat;
    Animate: Boolean;
    Time: Single;
    function CreateRingsEffect(const ARingCount: Single;
      out RingCountField: TSFFloat): TEffectNode;
    procedure UpdateInfo;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, Math,
  CastleUtils, CastleRenderOptions;

const
  { Rings of each lamp (see rings_light.fs). }
  BaseRingCount: array [0..2] of Single = (1, 2, 4);

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

function TViewMain.CreateRingsEffect(const ARingCount: Single;
  out RingCountField: TSFFloat): TEffectNode;
var
  Part: TEffectPartNode;
begin
  Result := TEffectNode.Create;
  Result.Language := slGLSL;
  { Functions to convert between eye and world space,
    used by rings_light.fs. }
  Result.SetShaderLibraries(['castle-shader:/EyeWorldSpace.glsl']);

  { Fields of the effect are uniforms in the shader. }
  RingCountField := TSFFloat.Create(Result, true, 'ring_count', ARingCount);
  Result.AddCustomField(RingCountField);

  Part := TEffectPartNode.Create;
  Part.ShaderType := stFragment;
  Part.SetUrl(['castle-data:/shaders/rings_light.fs']);
  Result.SetParts([Part]);
end;

procedure TViewMain.UpdateInfo;
begin
  LabelInfo.Caption :=
    'Three lamps with the same shader effect (rings around the lamp)' + NL +
    'and different values of the "ring_count" uniform.' + NL +
    Format('Animate the ring counts: %s (press Space)', [Iff(Animate, 'yes', 'no')]) + NL +
    'Drag with mouse to rotate the view.';
end;

procedure TViewMain.Start;
begin
  inherited;
  Lamp1.SetEffects([CreateRingsEffect(BaseRingCount[0], RingCount[0])]);
  Lamp2.SetEffects([CreateRingsEffect(BaseRingCount[1], RingCount[1])]);
  Lamp3.SetEffects([CreateRingsEffect(BaseRingCount[2], RingCount[2])]);
  UpdateInfo;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
var
  I: Integer;
begin
  inherited;
  if Animate then
  begin
    Time := Time + SecondsPassed;
    { Use Send to update the uniform value. }
    for I := 0 to High(RingCount) do
      RingCount[I].Send(BaseRingCount[I] * (1 + 0.3 * Sin(Time * 2)));
  end;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsKey(keySpace) then
  begin
    Animate := not Animate;
    UpdateInfo;
    Exit(true);
  end;
end;

end.
