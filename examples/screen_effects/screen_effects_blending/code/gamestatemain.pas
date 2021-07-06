{
  Copyright 2021-2021 Yevhen Loza, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
{ Main state of the example }
unit GameStateMain;

interface

uses Classes,
  CastleUIState, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, X3DNodes, X3DLoad;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    LabelFps: TCastleLabel;
    BurnRoot: TX3DRootNode;
    BurnEffect: TScreenEffectNode;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  StateMain: TStateMain;

implementation

uses
  SysUtils,
  CastleScreenEffects, CastleImages, CastleVectors;

{ TStateMain ----------------------------------------------------------------- }

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;


procedure TStateMain.Start;
var
  SimpleBackground: TCastleSimpleBackground;
  ScreenEffect: TCastleScreenEffects;
  ImageControl: TCastleImageControl;
begin
  inherited;
  { Create the screen effect and load appropriate shader }
  ScreenEffect := TCastleScreenEffects.Create(Self);
  ScreenEffect.FullSize := true;
  ScreenEffect.Blending := true;
  BurnRoot := LoadNode('castle-data:/shaders/burn.x3dv');
  BurnEffect := BurnRoot.FindNode('MyScreenEffect') as TScreenEffectNode;
  ScreenEffect.AddScreenEffect(BurnEffect);
  InsertFront(ScreenEffect);

  { SimpleBackground is required to clear the background buffer
    otherwise its content will be undefined }
  SimpleBackground := TCastleSimpleBackground.Create(Self);
  SimpleBackground.Color := Vector4(1, 0, 0, 0);
  SimpleBackground.FullSize := true;
  ScreenEffect.InsertFront(SimpleBackground);

  { Add two images, that cover each other (because they have opaque center)
    And after that the resulting image is made transparent through the shader }
  ImageControl := TCastleImageControl.Create(Self);
  ImageControl.HorizontalAnchorDelta := 128;
  ImageControl.VerticalAnchorDelta := 128;
  ImageControl.Url := 'castle-data:/cge1.png';
  ScreenEffect.InsertFront(ImageControl);

  ImageControl := TCastleImageControl.Create(Self);
  ImageControl.Url := 'castle-data:/cge1.png';
  ImageControl.HorizontalAnchorDelta := 256;
  ImageControl.VerticalAnchorDelta := 256;
  ScreenEffect.InsertFront(ImageControl);

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
end;

procedure TStateMain.Stop;
begin
  BurnRoot.Free;
  inherited;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

end.
