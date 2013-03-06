unit mainf; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ExtDlgs;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnChooseSrcImage: TButton;
    CheckAvgRect: TCheckBox;
    CheckBWColors: TCheckBox;
    LabelRectWidth: TLabel;
    LabelRectWidth1: TLabel;
    OpenPictureDialog1: TOpenPictureDialog;
    SpinRectWidth: TSpinEdit;
    SpinRectHeight: TSpinEdit;
    procedure BtnChooseSrcImageClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

uses ImageRectsUtils, CastleLCLUtils, CastleImages, CastleUtils;

{$R *.lfm}

{ TForm1 }

procedure TForm1.BtnChooseSrcImageClick(Sender: TObject);
var
  OutFileName: string;
begin
  FileFiltersToDialog(LoadImage_FileFilters, OpenPictureDialog1);
  if OpenPictureDialog1.Execute then
  begin
    AvgRect := CheckAvgRect.Checked;
    BWColors := CheckBWColors.Checked;
    OutFileName := AppendToFilename(OpenPictureDialog1.FileName, '_out');
    DoImageRects(OpenPictureDialog1.FileName,
      OutFileName, SpinRectWidth.Value, SpinRectHeight.Value);
    ShowMessage('Wygenerowano "' + OutFileName + '".');
  end;
end;

end.

