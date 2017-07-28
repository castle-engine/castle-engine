unit testhashunit;

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
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.Button1Click(Sender: TObject);
var v: LongWord;
    s: string;
begin
  v := StringToHash(edit1.text);
  label1.caption := 'Random (0..1) = ' + floatToStr(v/MaxInt/2);

  s := '';
  while length(s)<32 do begin
    s := inttostr(v mod 2)+s;
    v := v shr 1
  end;

  Memo1.lines.add(s);
end;

procedure TForm1.Button2Click(Sender: TObject);
var ix,iy: integer;
    s: string;
    c: byte;
begin
  for ix := 0 to image1.width do
    for iy := 0 to image1.height do with image1.canvas do begin
      s := inttostr(ix)+'x'+inttostr(iy); //generate a string different for each pixel and highly inhomogeneous
      c := round(255*(StringToHash(s)/maxint/2));
      brush.color := c+256*c+65536*c;
      fillrect(ix,iy,ix+1,iy+1);
    end;

end;



end.

