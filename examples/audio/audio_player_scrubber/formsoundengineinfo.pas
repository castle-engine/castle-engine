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

{ TSoundEngineInfoForm }

procedure TSoundEngineInfoForm.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TSoundEngineInfoForm.FormShow(Sender: TObject);
begin
  if not SoundEngine.ALInitialized then
    SoundEngine.ALContextOpen;
  MemoInfo.Lines.Text := SoundEngine.Information;
end;

end.

