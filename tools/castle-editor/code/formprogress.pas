{
  Copyright 2023-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Show download progress.
  Use only through TProgressForm.WaitFor method. }
unit FormProgress;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, CastleDownload;

type
  EDownloadCancelled = class(Exception);

  TProgressForm = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    ButtonCancel: TButton;
    LabelInfo: TLabel;
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    procedure ButtonCancelClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
  public
    WaitingFor: TCastleDownload;
    { Show progress of download, waiting for download to finish.
      Make exception if download fails or is cancelled. }
    class procedure WaitFor(const Description: String; const Download: TCastleDownload);
  end;

implementation

uses CastleApplicationProperties;

{$R *.lfm}

procedure TProgressForm.ButtonCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
  Close;
end;

procedure TProgressForm.Timer1Timer(Sender: TObject);
begin
  { We don't do this in ApplicationProperties1Idle because
    it isn't done when TCastleControl installs own idle handler that
    always sets Done:=false. }

  if WaitingFor.Status = dsDownloading then
  begin
    if WaitingFor.TotalBytes <> -1 then
      ProgressBar1.Position := Round(ProgressBar1.Max *
        WaitingFor.DownloadedBytes / WaitingFor.TotalBytes);

    // to process downloading
    ApplicationProperties._Update;
  end else
  begin
    ModalResult := mrOK;
    Close;
  end;
end;

class procedure TProgressForm.WaitFor(const Description: String; const Download: TCastleDownload);
var
  Form: TProgressForm;
begin
  Form := TProgressForm.Create(Application);
  try
    Form.LabelInfo.Caption := Description;
    Form.ProgressBar1.Max := 100;
    Form.ProgressBar1.Position := 0;
    Form.WaitingFor := Download;
    Form.ShowModal;

    case Download.Status of
      dsSuccess: ;
      dsError: raise Exception.CreateFmt('Download failed: %s', [Download.ErrorMessage]);
      else raise EDownloadCancelled.Create('Download cancelled');
    end;
  finally
    // TODO: closing before freeing necessary?
    if Form <> nil then
    begin
      Form.OnCloseQuery := nil;
      Form.Close;
    end;
    FreeAndNil(Form);
  end;
end;

end.

