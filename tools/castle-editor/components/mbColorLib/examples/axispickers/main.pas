unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, RAxisColorPicker, RColorPicker, GColorPicker,
  GAxisColorPicker, BColorPicker, BAxisColorPicker, mbColorPreview;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    mbColorPreview1: TmbColorPreview;
    PageControl1: TPageControl;
    Panel1: TPanel;
    RAxisColorPicker1: TRAxisColorPicker;
    BAxisColorPicker1: TBAxisColorPicker;
    GAxisColorPicker1: TGAxisColorPicker;
    RColorPicker1: TRColorPicker;
    BColorPicker1: TBColorPicker;
    GColorPicker1: TGColorPicker;
    PanelBLUE: TPanel;
    PanelGREEN: TPanel;
    PanelRED: TPanel;
    PgRED: TTabSheet;
    PgGREEN: TTabSheet;
    PgBLUE: TTabSheet;
    procedure BAxisColorPicker1Change(Sender: TObject);
    procedure BColorPicker1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GAxisColorPicker1Change(Sender: TObject);
    procedure GColorPicker1Change(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure PanelBLUEPaint(Sender: TObject);
    procedure PanelGREENPaint(Sender: TObject);
    procedure PanelREDPaint(Sender: TObject);
    procedure RAxisColorPicker1Change(Sender: TObject);
    procedure RColorPicker1Change(Sender: TObject);
  private
    procedure UpdatePreview;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  LclIntf, Types, GraphUtil, HTMLColors;

{ TForm1 }

procedure TForm1.BAxisColorPicker1Change(Sender: TObject);
begin
  BColorPicker1.SelectedColor := BAxisColorPicker1.SelectedColor;
  UpdatePreview;
end;

procedure TForm1.BColorPicker1Change(Sender: TObject);
begin
  BAxisColorPicker1.SelectedColor := BColorPicker1.SelectedColor;
  UpdatePreview;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  RAxisColorPicker1.SelectedColor := clRed;
  GAxisColorPicker1.SelectedColor := clGreen;
  BAxisColorPicker1.SelectedColor := clBlue;
  UpdatePreview;
end;

procedure TForm1.GAxisColorPicker1Change(Sender: TObject);
begin
  GColorPicker1.SelectedColor := GAxisColorPicker1.SelectedColor;
  UpdatePreview;
end;

procedure TForm1.GColorPicker1Change(Sender: TObject);
begin
  GAxisColorPicker1.SelectedColor := GColorPicker1.SelectedColor;
  UpdatePreview;
end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin
  UpdatePreview;
end;

// On BlueAxisPicker, x is RED, y is GREEN
procedure TForm1.PanelBLUEPaint(Sender: TObject);
var
  x, y: Integer;
  Rr, Rg, Rb: TSize;
begin
  PanelBLUE.Canvas.Font.Assign(PanelRED.Font);

  Rr := PanelBLUE.Canvas.TextExtent('red');
  Rg := PanelBLUE.Canvas.TextExtent('green');
  Rb := PanelBLUE.Canvas.TextExtent('blue');

  x := BAxisColorPicker1.Left + (BAxisColorPicker1.Width - Rr.CX) div 2;
  y := BAxisColorPicker1.Top + BAxisColorPicker1.Height + 4;
  PanelBLUE.Canvas.TextOut(x, y, 'red');
  x := x + Rr.CX div 2;
  y := y + Rr.CY div 2;
  DrawArrow(PanelBLUE.Canvas, sdRight, Point(x + 16, y), 16, atArrows);

  PanelBLUE.Canvas.Font.Orientation := 900;

  x := BColorPicker1.Left - Rb.CY - 4;
  y := BColorPicker1.Top + (BColorPicker1.Height + Rb.CX) div 2;
  PanelBLUE.Canvas.TextOut(x, y, 'blue');
  x := x + Rb.CY * 3 div 4;
  y := y - Rb.CX - 16;
  DrawArrow(PanelBLUE.Canvas, sdUp, Point(x, y + 8), 16, atArrows);

  x := BAxisColorPicker1.Left - Rg.CY - 4;
  y := BAxisColorPicker1.Top + (BAxisColorPicker1.Height + Rg.CX) div 2;
  PanelBLUE.Canvas.TextOut(x, y, 'green');
  x := x + Rg.CY * 3 div 4;
  y := y - Rg.CX - 16;
  DrawArrow(PanelBLUE.Canvas, sdUp, Point(x, y + 8), 16, atArrows);
end;

// on GreenAxisPicker, x is BLUE, y is RED
procedure TForm1.PanelGREENPaint(Sender: TObject);
var
  x, y: Integer;
  Rr, Rg, Rb: TSize;
begin
  PanelGREEN.Canvas.Font.Assign(PanelGREEN.Font);

  Rr := PanelGREEN.Canvas.TextExtent('red');
  Rg := PanelGREEN.Canvas.TextExtent('green');
  Rb := PanelGREEN.Canvas.TextExtent('blue');

  x := GAxisColorPicker1.Left + (GAxisColorPicker1.Width - Rb.CX) div 2;
  y := GAxisColorPicker1.Top + GAxisColorPicker1.Height + 4;
  PanelGREEN.Canvas.TextOut(x, y, 'blue');
  x := x + Rb.CX div 2;
  y := y + Rb.CY div 2;
  DrawArrow(PanelGREEN.Canvas, sdRight, Point(x + 16, y), 16, atArrows); //Solid);

  PanelGREEN.Canvas.Font.Orientation := 900;

  x := GColorPicker1.Left - Rg.CY - 4;
  y := GColorPicker1.Top + (GColorPicker1.Height + Rg.CX) div 2;
  PanelGREEN.Canvas.TextOut(x, y, 'green');
  x := x + Rg.CY * 3 div 4;
  y := y - Rg.CX - 16;
  DrawArrow(PanelGREEN.Canvas, sdUp, Point(x, y + 8), 16, atArrows);

  x := GAxisColorPicker1.Left - Rr.CY - 4;
  y := GAxisColorPicker1.Top + (GAxisColorPicker1.Height + Rr.CX) div 2;
  PanelGREEN.Canvas.TextOut(x, y, 'red');
  x := x + Rr.CY * 3 div 4;
  y := y - Rr.CX - 16;
  DrawArrow(PanelGREEN.Canvas, sdUp, Point(x, y + 8), 16, atArrows);
end;

procedure TForm1.PanelREDPaint(Sender: TObject);
var
  x, y: Integer;
  Rr, Rg, Rb: TSize;
begin
  PanelRED.Canvas.Font.Assign(PanelRED.Font);

  Rr := PanelRED.Canvas.TextExtent('red');
  Rg := PanelRED.Canvas.TextExtent('green');
  Rb := PanelRED.Canvas.TextExtent('blue');

  x := RAxisColorPicker1.Left + (RAxisColorPicker1.Width - Rb.CX) div 2;
  y := RAxisColorPicker1.Top + RAxisColorPicker1.Height + 4;
  PanelRED.Canvas.TextOut(x, y, 'blue');
  x := x + Rb.CX div 2;
  y := y + Rb.CY div 2;
  DrawArrow(PanelRED.Canvas, sdRight, Point(x + 16, y), 16, atArrows); //Solid);

  PanelRED.Canvas.Font.Orientation := 900;

  x := RColorPicker1.Left - Rr.CY - 4;
  y := RColorPicker1.Top + (RColorPicker1.Height + Rr.CX) div 2;
  PanelRED.Canvas.TextOut(x, y, 'red');
  x := x + Rr.CY * 3 div 4;
  y := y - Rr.CX - 16;
  DrawArrow(PanelRED.Canvas, sdUp, Point(x, y + 8), 16, atArrows);

  x := RAxisColorPicker1.Left - Rg.CY - 4;
  y := RAxisColorPicker1.Top + (RAxisColorPicker1.Height + Rg.CX) div 2;
  PanelRED.Canvas.TextOut(x, y, 'green');
  x := x + Rg.CY * 3 div 4;
  y := y - Rg.CX - 16;
  DrawArrow(PanelRED.Canvas, sdUp, Point(x, y + 8), 16, atArrows);
end;

procedure TForm1.RAxisColorPicker1Change(Sender: TObject);
begin
  RColorPicker1.SelectedColor := RAxisColorPicker1.SelectedColor;
  UpdatePreview;
end;

procedure TForm1.RColorPicker1Change(Sender: TObject);
begin
  RAXisColorPicker1.SelectedColor := RColorPicker1.SelectedColor;
  UpdatePreview;
end;

procedure TForm1.UpdatePreview;
begin
  case PageControl1.ActivePageindex of
    0: mbColorPreview1.Color := RColorPicker1.SelectedColor;
    1: mbColorPreview1.Color := GColorPicker1.SelectedColor;
    2: mbColorPreview1.Color := BColorPicker1.SelectedColor;
  end;
  Label1.Caption := Format('R=%d G=%d B=%d HTML=#%s', [
    GetRValue(mbColorPreview1.Color),
    GetGValue(mbColorPreview1.Color),
    GetBValue(mbColorPreview1.Color),
    ColorToHex(mbColorPreview1.Color)
  ]);
end;
end.

