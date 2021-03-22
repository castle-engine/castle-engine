{
  Copyright 2021-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Dialog to configure new unit properties (TNewUnitForm). }
unit FormNewUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  ButtonPanel;

type
  { Dialog to configure new unit properties. }
  TNewUnitForm = class(TForm)
    ButtonUnitFile: TButton;
    ButtonPanel1: TButtonPanel;
    ButtonStateFile: TButton;
    ComboUnitType: TComboBox;
    EditStateFile: TEdit;
    EditUnitName: TEdit;
    EditUnitFile: TEdit;
    EditClassName: TEdit;
    LabelStateFile: TLabel;
    LabelUnitName: TLabel;
    LabelCreateUnit: TLabel;
    LabelUnitFile: TLabel;
    LabelClassName: TLabel;
    procedure ButtonStateFileClick(Sender: TObject);
    procedure ButtonUnitFileClick(Sender: TObject);
  private

  public

  end;

var
  NewUnitForm: TNewUnitForm;

implementation

{$R *.lfm}

procedure TNewUnitForm.ButtonUnitFileClick(Sender: TObject);
begin
  // TODO: allow to choose directory only (filename is determined by lowercase(unit name) + .pas
  // only within the project
  // TODO: start EditUnitFile with directory from where user clicked the "New Unit" menu item
end;

procedure TNewUnitForm.ButtonStateFileClick(Sender: TObject);
begin
  // TODO: allow to choose directory and filename, but only within castle-data:/
end;

end.

