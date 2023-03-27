unit test1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, jpeg, ExtCtrls, FileCtrl, ComCtrls;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Panel1: TPanel;
    DirectoryListBox1: TDirectoryListBox;
    FileListBox1: TFileListBox;
    Panel3: TPanel;
    DriveComboBox1: TDriveComboBox;
    Scale: TComboBox;
    PixelFormat: TComboBox;
    ColorSpace: TComboBox;
    Performance: TComboBox;
    ProgressiveDisplay: TCheckBox;
    IncrementalDisplay: TCheckBox;
    procedure FileListBox1DblClick(Sender: TObject);
    procedure SetJPEGOptions(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ProgressUpdate(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FileListBox1DblClick(Sender: TObject);
begin
  try
    Image1.Picture.LoadFromFile(FileListbox1.Filename);
  except
    on EInvalidGraphic do
      Image1.Picture.Graphic := nil;
  end;
  SetJPEGOptions(self);
end;


procedure TForm1.SetJPEGOptions(Sender: TObject);
var
  Temp: Boolean;
begin
  Temp := Image1.Picture.Graphic is TJPEGImage;
  if Temp then
    with TJPEGImage(Image1.Picture.Graphic) do
    begin
      PixelFormat := TJPEGPixelFormat(Self.PixelFormat.ItemIndex);
      Scale := TJPEGScale(Self.Scale.ItemIndex);
      Grayscale := Boolean(Colorspace.ItemIndex);
      Performance := TJPEGPerformance(Self.Performance.ItemIndex);
      ProgressiveDisplay := Self.ProgressiveDisplay.Checked;
    end;
  Scale.Enabled := Temp;
  PixelFormat.Enabled := Temp;
  Colorspace.Enabled := Temp;
  Performance.Enabled := Temp;
  ProgressiveDisplay.Enabled := Temp
    and TJPEGImage(Image1.Picture.Graphic).ProgressiveEncoding;
  Image1.IncrementalDisplay := IncrementalDisplay.Checked;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Scale.ItemIndex := 0;
  PixelFormat.ItemIndex := 0;
  Colorspace.ItemIndex := 0;
  Performance.ItemIndex := 0;
  FileListbox1.Mask := '*.jpg;*.bmp;*.wmf;*.emf;*.ico';
  Image1.OnProgress := ProgressUpdate;
end;

procedure TForm1.ProgressUpdate(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if Stage = psRunning then
    Caption := Format('%d%%',[PercentDone])
  else
    Caption := 'Form1';
end;


end.
