unit AboutForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Ani,
  FMX.Filter.Effects, FMX.Effects, FMX.StdCtrls, FMX.Controls.Presentation,

  Imaging,
  DemoUtils;

type
  TFormAbout = class(TForm)
    ImgLogo: TImage;
    PanelBack: TPanel;
    Timer: TTimer;
    BtnOk: TButton;
    Label1: TLabel;
    LabVersion: TLabel;
    LabWebsite: TLabel;
    Effect: TWaveEffect;
    WaveAnim: TFloatAnimation;
    procedure FormCreate(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure WaveAnimFinish(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormAbout: TFormAbout;

implementation

{$R *.fmx}

procedure TFormAbout.WaveAnimFinish(Sender: TObject);
begin
  Effect.Enabled := False;
  Close;
end;

procedure TFormAbout.FormCreate(Sender: TObject);
begin
  ImgLogo.Bitmap.LoadFromFile(GetDataDir + PathDelim + 'LogoAlpha.png');
  LabVersion.Text := LabVersion.Text + GetVersionStr;
end;

procedure TFormAbout.BtnOkClick(Sender: TObject);
begin
  if Effect.Enabled then
    Exit;
  Effect.Enabled := True;
  WaveAnim.Start;
end;

end.
