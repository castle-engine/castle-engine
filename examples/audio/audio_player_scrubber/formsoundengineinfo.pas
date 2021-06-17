{
  Copyright 2017-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Sound engine information form. }
unit FormSoundEngineInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TSoundEngineInfoForm = class(TForm)
    ButtonClose: TButton;
    MemoInfo: TMemo;
    procedure ButtonCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  SoundEngineInfoForm: TSoundEngineInfoForm;

implementation

uses CastleSoundEngine;

{$R *.lfm}

procedure TSoundEngineInfoForm.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TSoundEngineInfoForm.FormShow(Sender: TObject);
begin
  if not SoundEngine.IsContextOpen then
    SoundEngine.ContextOpen;
  MemoInfo.Lines.Text := SoundEngine.Information;
end;

end.
