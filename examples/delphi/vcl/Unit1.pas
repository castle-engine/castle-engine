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
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  CastleVclOpenGlControl,
  CastleGLVersion, CastleGLUtils, CastleGLContextWGL;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    CastleControl: TCastleVclOpenGlControl;
    Requirements: TGLContextRequirements;
    Context: TGLContextWGL;
    procedure GlPaint(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses CastleRenderOptions, CastleRectangles, CastleColors, CastleRenderContext,
  CastleControls, CastleApplicationProperties, CastleVectors, CastleTimeUtils;

procedure TForm1.FormCreate(Sender: TObject);
var
  TimeStart: TTimerResult;
begin
  TimeStart := Timer;

  CastleControl := TCastleVclOpenGlControl.Create(Self);
  CastleControl.Parent := Self;
  CastleControl.Left := 50;
  CastleControl.Top := 50;
  CastleControl.Width := 400;
  CastleControl.Height := 500;

  Requirements := TGLContextRequirements.Create;
  Requirements.DoubleBuffer := true;
  Requirements.DepthBits := 24;
  Requirements.StencilBits := DefaultStencilBits;
  Requirements.MultiSampling := 1;

  Context := TGLContextWGL.Create;
  Context.WndPtr := CastleControl.Handle;
  Context.h_Dc := GetWindowDC(Context.WndPtr); // TODO: get it internally?
  Context.WindowCaption := 'TestCgeControl';
  Context.WndClassName := 'Castle'; // TODO: invented

  Context.ContextCreate(Requirements);

  Memo1.Lines.Add('Got GL context, h_GLRc: ' + IntToStr(Context.h_GLRc));

  Context.MakeCurrent;

  ApplicationProperties._GLContextEarlyOpen;
  ApplicationProperties._GLContextOpen;

  GLInformationInitialize;

  // CGE needs this to be assigned, typically done by container
  RenderContext := TRenderContext.Create;
  RenderContext.Viewport := Rectangle(0, 0, CastleControl.Width, CastleControl.Height);
  // TODO: update viewport if panel size changes

  Memo1.Lines.Add(Format('Initialized OpenGL(ES) context in %f secs', [
    TimeStart.ElapsedTime
  ]));
  Memo1.Lines.Add(GLInformationString);

  CastleControl.Context := Context;
  CastleControl.OnGlPaint := GlPaint;
  CastleControl.Invalidate;
end;

procedure TForm1.GlPaint(Sender: TObject);
begin
  RenderContext.Clear([cbColor], Vector4(0.2, 0.2, 0.2, 1));
  DrawRectangle(FloatRectangle(10, 10, 100, 200), Yellow);
  FallbackFont.Print(30, 30, Green, FormatDateTime('yyyy-mm-dd, hh:nn:ss', Now));
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if CastleControl <> nil then
    CastleControl.Invalidate;
end;

end.
