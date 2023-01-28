unit FormView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ExtDlgs,

  Imaging,
  ImagingComponents;

type
  TViewForm = class(TForm)
    Image: TImage;
    PnlBle: TPanel;
    Button1: TButton;
    DlgSave: TSavePictureDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Bitmap: TImagingBitmap;
  end;

var
  ViewForm: TViewForm;

implementation

uses FormMain;

{$R *.dfm}

procedure TViewForm.Button1Click(Sender: TObject);
begin
  DlgSave.Filter := GetImageFileFormatsFilter(False);
  DlgSave.FileName := ChangeFileExt(ExtractFileName(MainForm.FileName), '');
  DlgSave.FilterIndex := GetFileNameFilterIndex(MainForm.FileName, False);
  if DlgSave.Execute then
  begin
    DlgSave.FileName := ChangeFileExt(DlgSave.FileName, '.' + GetFilterIndexExtension(DlgSave.FilterIndex, False));
    MainForm.Resampled.SaveToFile(DlgSave.FileName);
    MainForm.FileName := DlgSave.FileName;
  end;
end;

procedure TViewForm.FormCreate(Sender: TObject);
begin
  Bitmap := TImagingBitmap.Create;
  ViewForm.Image.Picture.Bitmap := Bitmap;
end;

procedure TViewForm.FormDestroy(Sender: TObject);
begin
  Bitmap.Free;
end;

procedure TViewForm.FormShow(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

end.
