unit GraphicsTestUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, {Controls, Graphics,} ExtCtrls{, SysUtils, FileUtil, Dialogs},CastleRandom;

type

  { TForm1 }

  TForm1 = class(TForm)
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses SysUtils;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var ix, iy: integer;
    RND: TCastleRandom;
begin
  RND := TCastleRandom.create;
  try
    for ix := 0 to image1.width do
      for iy := 0 to image1.height do
        with Image1.canvas do begin
          brush.color := round(RND.random(255))+256*round(RND.random(255))+65536*round(RND.random(255));
          fillrect(ix,iy,ix+1,iy+1);
        end;
  finally
    FreeAndNil(RND);
  end;
end;

end.

