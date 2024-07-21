{
  Copyright 2024-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Show mobile controls help. }
unit GameViewControlsHelp;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse;

type
  TViewControlsHelp = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonOK: TCastleButton;
  private
    procedure ClickMenu(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  end;

var
  ViewControlsHelp: TViewControlsHelp;

implementation

constructor TViewControlsHelp.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewcontrolshelp.castle-user-interface';
end;

procedure TViewControlsHelp.Start;
begin
  inherited;
  ButtonOK.OnClick := {$ifdef FPC}@{$endif}ClickMenu;
  InterceptInput := true;
end;

procedure TViewControlsHelp.ClickMenu(Sender: TObject);
begin
  Container.PopView(Self);
end;

end.
