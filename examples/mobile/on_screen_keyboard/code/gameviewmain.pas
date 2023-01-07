{
  Copyright 2022 Andrzej Kilijanski, Michalis Kamburelis.

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
  CastleUIControls, CastleControls, CastleKeysMouse, CastleAds, CastleOpenDocument;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    ButtonShow: TCastleButton;
    ButtonHide: TCastleButton;
    Edit1: TCastleEdit;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;

    procedure ButtonShowClick(Sender: TObject);
    procedure ButtonHideClick(Sender: TObject);
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, CastleWindow;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;
  ButtonShow.OnClick := {$ifdef FPC}@{$endif}ButtonShowClick;
  ButtonHide.OnClick := {$ifdef FPC}@{$endif}ButtonHideClick;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TViewMain.ButtonShowClick(Sender: TObject);
begin
  Edit1.Focused := true;
end;

procedure TViewMain.ButtonHideClick(Sender: TObject);
begin
  Edit1.Focused := false;
end;

end.
