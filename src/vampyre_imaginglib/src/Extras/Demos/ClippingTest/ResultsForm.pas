unit ResultsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls,
  ImagingComponents, StdCtrls;

type
  TResultForm = class(TForm)
    ImageMy: TImage;
    ImageWin: TImage;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageMyMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageWinMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  public
    MyBitmap, WinBitmap: TImagingBitmap;
  end;

var
  ResultForm: TResultForm;

implementation

{$R *.dfm}

procedure TResultForm.FormCreate(Sender: TObject);
begin
  MyBitmap := TImagingBitmap.Create;
  WinBitmap := TImagingBitmap.Create;

  ImageMy.Picture.Graphic := MyBitmap;
  ImageWin.Picture.Graphic := WinBitmap;
end;

procedure TResultForm.FormDestroy(Sender: TObject);
begin
  MyBitmap.Free;
  WinBitmap.Free;
end;

procedure TResultForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Close;
end;

procedure TResultForm.ImageMyMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Close;
end;

procedure TResultForm.ImageWinMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Close;
end;

end.
