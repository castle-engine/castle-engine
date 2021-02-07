unit TestHashUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls,

  CastleRandom;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Image1: TImage;
    Label1: TLabel;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.Button1Click(Sender: TObject);
var V: LongWord;
    s: String;
begin
  V := StringToHash(Edit1.Text);
  Label1.Caption := 'Random (0..1) = ' + FloatToStrDot(V/MaxInt/2);

  s := '';
  while Length(s)<32 do begin
    s := IntToStr(V mod 2)+s;
    V := V shr 1
  end;

  Memo1.Lines.Add(s);
end;

procedure TForm1.Button2Click(Sender: TObject);
var ix,iy: Integer;
    s: String;
    C: Byte;
begin
  for ix := 0 to Image1.Width do
    for iy := 0 to Image1.Height do with Image1.Canvas do begin
      s := IntToStr(ix)+'x'+IntToStr(iy); //generate a String different for each pixel and highly inhomogeneous
      C := Round(255*(StringToHash(s)/MaxInt/2));
      Brush.Color := C+256*C+65536*C;
      FillRect(ix,iy,ix+1,iy+1);
    end;

end;



end.

