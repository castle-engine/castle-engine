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

{ Shader effect on a light source, using a texture.

  The design (data/gameviewmain.castle-user-interface) has two scenes:
  - LightScene (data/projector_light.x3dv, with CastGlobalLights = true)
    has a point light with an Effect that multiplies the light color by an image
    (stained glass window, window panes or blinds) projected downwards from the light.
    The images are ImageTexture nodes in fields of the Effect node.
  - LitScene (data/lit_objects.x3dv) has the objects lit by it.

  Here we move the light around and change the image. }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleScene, X3DFields;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelInfo: TCastleLabel;
    LightScene: TCastleScene;
  private
    { Fields of the light's effect (uniforms in the shader). }
    FieldLightPosition: TSFVec3f;
    FieldImageIndex: TSFInt32;
    LightTime: Single;
    LightMoves: Boolean;
    procedure UpdateLight;
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

uses SysUtils,
  CastleUtils;

const
  LightHeight = 6.0;
  LightOrbit = 1.5;
  ImageNames: array [0..2] of String = (
    'stained glass',
    'window panes',
    'blinds'
  );

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.UpdateLight;
begin
  LightScene.Translation := Vector3(
    Cos(LightTime * 0.4) * LightOrbit, LightHeight, Sin(LightTime * 0.4) * LightOrbit);
  { The light is at (0, 0, 0) in LightScene,
    so its world position is the LightScene translation.
    Send, not just set the value, to update the shader uniform. }
  FieldLightPosition.Send(LightScene.Translation);
end;

procedure TViewMain.UpdateInfo;
begin
  LabelInfo.Caption :=
    'A point light with a shader effect: a colored image projected from the light.' + NL +
    'The images are textures in fields of the light''s Effect node.' + NL +
    Format('Image: %s (press T to change)', [ImageNames[FieldImageIndex.Value]]) + NL +
    Format('Light moves: %s (press Space)', [Iff(LightMoves, 'yes', 'no')]) + NL +
    'Drag with mouse to rotate the view.';
end;

procedure TViewMain.Start;
begin
  inherited;
  FieldLightPosition := LightScene.Field('LightEffect', 'light_position') as TSFVec3f;
  FieldImageIndex := LightScene.Field('LightEffect', 'image_index') as TSFInt32;
  LightMoves := true;
  UpdateLight;
  UpdateInfo;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  if LightMoves then
  begin
    LightTime := LightTime + SecondsPassed;
    UpdateLight;
  end;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsKey(keyT) then
  begin
    FieldImageIndex.Send((FieldImageIndex.Value + 1) mod Length(ImageNames));
    UpdateInfo;
    Exit(true);
  end;

  if Event.IsKey(keySpace) then
  begin
    LightMoves := not LightMoves;
    UpdateInfo;
    Exit(true);
  end;
end;

end.
