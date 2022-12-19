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
  CastleGLVersion, CastleGLUtils, CastleGLContextWGL;

type
  TTestCgeControl = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Panel1Paint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure Panel1Painting(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
  private
    Requirements: TGLContextRequirements;
    Context: TGLContextWGL;
  public
    { Public declarations }
  end;

var
  TestCgeControl: TTestCgeControl;

implementation

{$R *.fmx}

uses Windows,
  CastleRenderOptions, CastleRectangles, CastleColors, CastleRenderContext;

procedure TTestCgeControl.Button1Click(Sender: TObject);
begin
  Requirements := TGLContextRequirements.Create;
  Requirements.DoubleBuffer := true;
  Requirements.DepthBits := 24;
  Requirements.StencilBits := DefaultStencilBits;
  Requirements.MultiSampling := 1;

  Context := TGLContextWGL.Create;
  // TODO: get it for a panel, not whole form?
  Context.WndPtr := WindowHandleToPlatform(Handle).Wnd;
  Context.h_Dc := GetWindowDC(Context.WndPtr); // TODO: get it internally?
  Context.WindowCaption := 'TestCgeControl';
  Context.WndClassName := 'Castle'; // TODO: invented

  Context.ContextCreate(Requirements);

  Memo1.Lines.Add('Got GL context, h_GLRc: ' + IntToStr(Context.h_GLRc));

  Context.MakeCurrent;

    // CGE needs this to be assigned, typically done by container
  RenderContext := TRenderContext.Create;
  RenderContext.Viewport := Rectangle(0, 0, Round(Panel1.Width), Round(Panel1.Height));

  GLInformationInitialize;
  Memo1.Lines.Add(GLInformationString);
end;

procedure TTestCgeControl.FormPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
  if Context <> nil then
  begin
    Memo1.Lines.Add('Form Paint');

    Context.MakeCurrent;

    // TODO: render at proper moment, in a loop
    DrawRectangle(FloatRectangle(10, 10, 100, 200), Blue);
    Context.SwapBuffers;
  end;
end;

procedure TTestCgeControl.Panel1Paint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
  if Context <> nil then
  begin
    Memo1.Lines.Add('Panel Paint');

    Context.MakeCurrent;

    // TODO: render at proper moment, in a loop
    DrawRectangle(FloatRectangle(10, 10, 100, 200), Yellow);
    Context.SwapBuffers;
  end;
end;

procedure TTestCgeControl.Panel1Painting(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
  if Context <> nil then
  begin
    Memo1.Lines.Add('Panel Painting');

    Context.MakeCurrent;

    // TODO: render at proper moment, in a loop
    DrawRectangle(FloatRectangle(10, 10, 100, 200), Red);
    Context.SwapBuffers;
  end;
end;

end.
