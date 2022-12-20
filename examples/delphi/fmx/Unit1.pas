{
  Copyright 2022-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Form unit. }
unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Platform.Win,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo,
  CastleGLVersion, CastleGLUtils, CastleGLContextWGL, CastleFmxOpenGlControl;

type
  TTestCgeControl = class(TForm)
    Memo1: TMemo;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure GlPaint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    CastleControl: TCastleFmxOpenGlControl;
  public
    { Public declarations }
  end;

var
  TestCgeControl: TTestCgeControl;

implementation

{$R *.fmx}

uses Windows, FMX.Presentation.Win,
  CastleRenderOptions, CastleRectangles, CastleColors, CastleRenderContext,
  CastleApplicationProperties, CastleTimeUtils, CastleVectors,
  CastleControls;

procedure TTestCgeControl.FormCreate(Sender: TObject);
var
  TimeStart: TTimerResult;
begin
  TimeStart := Timer;

  CastleControl := TCastleFmxOpenGlControl.Create(Self);
  CastleControl.Parent := Self;
  CastleControl.Position.X := 20;
  CastleControl.Position.Y := 20;
  CastleControl.Width := 400;
  CastleControl.Height := 500;

  CastleControl.GLContextOpen;

  Memo1.Lines.Add(Format('Initialized OpenGL(ES) context in %f secs', [
    TimeStart.ElapsedTime
  ]));
  Memo1.Lines.Add(GLInformationString);

  CastleControl.OnGlPaint := GlPaint;
  Invalidate;
end;

procedure TTestCgeControl.GlPaint(Sender: TObject);
begin
  RenderContext.Clear([cbColor], Vector4(0.2, 0.2, 0.2, 1));
  DrawRectangle(FloatRectangle(10, 10, 100, 200), Yellow);
  FallbackFont.Print(30, 30, Green, FormatDateTime('yyyy-mm-dd, hh:nn:ss', Now));
end;

procedure TTestCgeControl.Timer1Timer(Sender: TObject);
begin
  Invalidate;
end;

end.
