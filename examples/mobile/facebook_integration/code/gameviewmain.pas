{
  Copyright 2020-2025 Michalis Kamburelis.

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
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    ButtonFacebookLogin: TCastleButton;
    ButtonFacebookLogAchievedLevel: TCastleButton;
  private
    procedure ClickFacebookLogin(Sender: TObject);
    procedure ClickFacebookLogAchievedLevel(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleFacebook;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;

  // TFacebook.LoginButton only supported on iOS now
  ButtonFacebookLogin.Exists := {$ifdef CASTLE_IOS} true {$else} false {$endif};
  ButtonFacebookLogin.OnClick := {$ifdef FPC}@{$endif} ClickFacebookLogin;

  // TFacebook.LogAchievedLevel only supported on Android now
  ButtonFacebookLogAchievedLevel.Exists := {$ifdef ANDROID} true {$else} false {$endif};
  ButtonFacebookLogAchievedLevel.OnClick := {$ifdef FPC}@{$endif} ClickFacebookLogAchievedLevel;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TViewMain.ClickFacebookLogin(Sender: TObject);
begin
  TFacebook.LoginButton;
end;

procedure TViewMain.ClickFacebookLogAchievedLevel(Sender: TObject);
begin
  TFacebook.LogAchievedLevel('test_level_name');
end;

end.
