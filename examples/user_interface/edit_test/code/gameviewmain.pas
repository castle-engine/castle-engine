{
  Copyright 2016-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main state. }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse;

type
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonCopyText: TCastleButton;
    EditNumbers, Edit1, Edit2: TCastleEdit;
  private
    procedure ButtonCopyTextClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  end;

var
  ViewMain: TViewMain;

implementation

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;
  ButtonCopyText.OnClick := {$ifdef FPC}@{$endif} ButtonCopyTextClick;

  EditNumbers.AllowedChars := ['0'..'9'];
  EditNumbers.MaxLength := 9;
end;

procedure TViewMain.ButtonCopyTextClick(Sender: TObject);
begin
  Edit2.Text := Edit1.Text;
end;

end.
