unit GraphicsTestUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, ExtCtrls, CastleRandom;

type
  TForm1 = class(TForm)
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

uses SysUtils;

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
var ix, iy: Integer;
    Rnd: TCastleRandom;
begin
  Rnd := TCastleRandom.Create;
  try
    for ix := 0 to Image1.Width do
      for iy := 0 to Image1.Height do
        with Image1.Canvas do begin
          Brush.Color := Round(Rnd.random(255))+256*Round(Rnd.Random(255))+65536*Round(Rnd.Random(255));
          FillRect(ix,iy,ix+1,iy+1);
        end;
  finally
    FreeAndNil(Rnd);
  end;
end;

end.

