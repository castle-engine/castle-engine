{
  Copyright 2019-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Form to configure CGE editor (@link(TPreferencesForm)). }
unit FormPreferences;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls,
  ExtCtrls, ButtonPanel;

type
  TPreferencesForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    DirectoryEditFpc: TDirectoryEdit;
    DirectoryEditLazarus: TDirectoryEdit;
    LabelFpc: TLabel;
    LabelInstructions0: TLabel;
    LabelLazarus: TLabel;
    LabelInstructions1: TLabel;
    LabelTitle: TLabel;
    LabelInstructions2: TLabel;
    LabelFpcAutoDetected: TLabel;
    LabelLazarusAutoDetected: TLabel;
    LabelLazarusWebsite: TLabel;
    procedure FormShow(Sender: TObject);
    procedure LabelLazarusWebsiteClick(Sender: TObject);
  private

  public

  end;

var
  PreferencesForm: TPreferencesForm;

implementation

uses CastleOpenDocument;

{$R *.lfm}

{ TPreferencesForm }

procedure TPreferencesForm.LabelLazarusWebsiteClick(Sender: TObject);
begin
  OpenURL('https://www.lazarus-ide.org/');
end;

procedure TPreferencesForm.FormShow(Sender: TObject);
begin
  // TODO: fill
  //LabelFpcAutoDetected
  //LabelLazarusAutoDetected
  //DirectoryEditFpc
  //DirectoryEditLazarus
end;

end.

