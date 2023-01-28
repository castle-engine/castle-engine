unit FormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons, ExtDlgs,

  ImagingTypes,
  Imaging,
  ImagingClasses,
  ImagingComponents,
  hq2x;

type
  TMainForm = class(TForm)
    Panel1: TPanel;
    Image: TImage;
    Label1: TLabel;
    EdFileName: TEdit;
    Button1: TButton;
    Btn2x: TButton;
    Btn3x: TButton;
    Btn4x: TButton;
    Button2: TButton;
    DlgOpen: TOpenPictureDialog;
    procedure Button2Click(Sender: TObject);
    procedure Btn2xClick(Sender: TObject);
    procedure Btn3xClick(Sender: TObject);
    procedure Btn4xClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Source: TSingleImage;
    Resampled: TSingleImage;
    FileName: string;
    Bitmap: TImagingBitmap;

    procedure ResampleFile(Magnification: Integer);
    procedure ShowImage;
  end;

var
  MainForm: TMainForm;

const
  AboutMsg = 'Original hq2x, hq3x, hq4x: Maxim Stepin' + SLineBreak +
             'Pascal translation: Jeremy Darling' + SLineBreak +
             'Imaging hq demo: Marek Mauder';

implementation

uses FormView;

{$R *.dfm}

procedure TMainForm.Btn2xClick(Sender: TObject);
begin
  ResampleFile(2);
end;

procedure TMainForm.Btn3xClick(Sender: TObject);
begin
  ResampleFile(3);
end;

procedure TMainForm.Btn4xClick(Sender: TObject);
begin
  ResampleFile(4);
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  ValidFile: Boolean;
begin
  DlgOpen.Filter := GetImageFileFormatsFilter(True);
  if DlgOpen.Execute then
  begin
    EdFileName.Text := DlgOpen.FileName;
    FileName := DlgOpen.FileName;

    ValidFile := FileExists(FileName) and
      (Imaging.DetermineFileFormat(FileName) <> '');
    Btn2x.Enabled := ValidFile;
    Btn3x.Enabled := ValidFile;
    Btn4x.Enabled := ValidFile;

    if ValidFile then
    begin
      Source.LoadFromFile(FileName);
      Image.SetBounds(Image.Left, Image.Top, Source.Width, Source.Height);
      Image.Picture.Bitmap.Assign(Source);
    end;
  end;
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  ShowMessage(AboutMsg);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Source := TSingleImage.Create;
  Resampled := TSingleImage.Create;
  Bitmap := TImagingBitmap.Create;
  Image.Picture.Bitmap := Bitmap;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Source.Free;
  Resampled.Free;
  Bitmap.Free;
end;

procedure TMainForm.ResampleFile(Magnification: Integer);
begin
  if not (Magnification in [2, 3, 4]) then
  begin
    ShowMessage('Requested magnification not supported');
    Exit;
  end;

  if Magnification in [3, 4] then
  begin
    ShowMessage('Requested magnification not YET supported');
    Exit;
  end;

  Source.Format := ifR5G6B5;
  Resampled.CreateFromParams(Source.Width * Magnification, Source.Height * Magnification, ifX8R8G8B8);
  try
    case Magnification of
      2: hq2x_32(Source.Bits, Resampled.Bits, Source.Width, Source.Height, Resampled.Width * 4);
      3: ;
      4: ;
    end;
  except
    ShowMessage('Error during resampling');
  end;

  ShowImage;
end;

procedure TMainForm.ShowImage;
begin
  ViewForm.Image.SetBounds(0, ViewForm.Image.Top, Resampled.Width, Resampled.Height);
  ViewForm.Image.Picture.Bitmap.Assign(Resampled);
  ViewForm.ShowModal;
end;

end.
