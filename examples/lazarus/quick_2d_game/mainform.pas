unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  CastleControl, CastleGLImages, CastleFilesUtils, CastleKeysMouse;

type

  { TForm1 }

  TForm1 = class(TForm)
    CastleControl1: TCastleControlBase;
    procedure CastleControl1Press(Sender: TObject;
      const Event: TInputPressRelease);
    procedure CastleControl1Render(Sender: TObject);
    procedure CastleControl1Update(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    Image: TDrawableImage;
    X, Y: Single;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Image := TDrawableImage.Create('castle-data:/my_image.png');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Image);
end;

procedure TForm1.CastleControl1Render(Sender: TObject);
begin
  Image.Draw(X, Y);
end;

procedure TForm1.CastleControl1Press(Sender: TObject;
  const Event: TInputPressRelease);
begin
  if Event.IsKey(keySpace) then
    Y := Y - 200.0;
end;

procedure TForm1.CastleControl1Update(Sender: TObject);
var
  SecondsPassed: Single;
begin
  SecondsPassed := CastleControl1.Fps.SecondsPassed;
  Y := Y + SecondsPassed * 100.0;
  if CastleControl1.Pressed[keyArrowLeft] then
    X := X - SecondsPassed * 200.0;
  if CastleControl1.Pressed[keyArrowRight] then
    X := X + SecondsPassed * 200.0;
end;

end.

