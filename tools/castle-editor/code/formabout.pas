unit FormAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons;

type
  TAboutForm = class(TForm)
    BitBtn1: TBitBtn;
    ImageLogo: TImage;
    LabelWebsite: TLabel;
    LabelName: TLabel;
    LabelVersion: TLabel;
    LabelCopyright: TLabel;
    LabelWebsite1: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LabelWebsite1Click(Sender: TObject);
    procedure LabelWebsiteClick(Sender: TObject);
  private

  public

  end;

var
  AboutForm: TAboutForm;

implementation

uses CastleOpenDocument, CastleUtils;

{$R *.lfm}

procedure TAboutForm.LabelWebsiteClick(Sender: TObject);
begin
  OpenUrl('https://castle-engine.io/');
end;

procedure TAboutForm.LabelWebsite1Click(Sender: TObject);
begin
  OpenUrl('https://patreon.com/castleengine/');
end;

procedure TAboutForm.BitBtn1Click(Sender: TObject);
begin
  Close;
end;

procedure TAboutForm.FormCreate(Sender: TObject);
var
  VersionMultiline: String;
begin
  { When a snapshot is packed by pack_release.sh,
    CastleEngineVersion is long, and contains a commit information.
    Display it nicer. }
  VersionMultiline := StringReplace(CastleEngineVersion,
    ' (commit', NL + '(commit', [rfReplaceAll, rfIgnoreCase]);
  LabelVersion.Caption := 'Version: ' + VersionMultiline;
end;

end.

