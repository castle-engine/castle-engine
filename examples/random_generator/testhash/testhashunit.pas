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

uses CastleUtils;

{$R *.lfm}

procedure TForm1.Button1Click(Sender: TObject);
var v: LongWord;
    s: string;
begin
  v := StringToHash(Edit1.Text);
  Label1.Caption := 'Random (0..1) = ' + FloatToStrDot(v/MaxInt/2);

  s := '';
  while Length(s)<32 do begin
    s := IntToStr(v mod 2)+s;
    v := v shr 1
  end;

  Memo1.Lines.Add(s);
end;

procedure TForm1.Button2Click(Sender: TObject);
var ix,iy: integer;
    s: string;
    c: byte;
begin
  for ix := 0 to Image1.Width do
    for iy := 0 to Image1.Height do with Image1.Canvas do begin
      s := IntToStr(ix)+'x'+IntToStr(iy); //generate a string different for each pixel and highly inhomogeneous
      c := Round(255*(StringToHash(s)/MaxInt/2));
      Brush.Color := c+256*c+65536*c;
      FillRect(ix,iy,ix+1,iy+1);
    end;

end;



end.

