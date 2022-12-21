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
  CastleGLUtils, Fmx.CastleControl;

type
  TTestCgeControl = class(TForm)
    Memo1: TMemo;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    CastleControl: TCastleControl;
  public
    { Public declarations }
  end;

var
  TestCgeControl: TTestCgeControl;

implementation

{$R *.fmx}

uses Windows, FMX.Presentation.Win,
  CastleRenderOptions, CastleRectangles, CastleColors, CastleRenderContext,
  CastleVectors, CastleControls, CastleUIControls;

type
  TMyUi = class(TCastleUserInterface)
    procedure Render; override;
    procedure GLContextOpen; override;
  end;

procedure TTestCgeControl.FormCreate(Sender: TObject);
begin
  CastleControl := TCastleControl.Create(Self);
  CastleControl.Parent := Self;
  CastleControl.Position.X := 20;
  CastleControl.Position.Y := 20;
  CastleControl.Width := 300;
  CastleControl.Height := 400;

  CastleControl.Container.Controls.InsertFront(TMyUi.Create(Self));
end;

procedure TTestCgeControl.Timer1Timer(Sender: TObject);
begin
  Invalidate; // TODO: should not be needed
end;

{ TMyUi --------------------------------------------------------------------- }

procedure TMyUi.GLContextOpen;
begin
  inherited;
  TestCgeControl.Memo1.Lines.Add(GLInformationString);
end;

procedure TMyUi.Render;
begin
  inherited;
  RenderContext.Clear([cbColor], Vector4(0.2, 0.2, 0.2, 1));
  DrawRectangle(FloatRectangle(10, 10, 100, 200), Yellow);
  FallbackFont.Print(30, 30, Green, FormatDateTime('yyyy-mm-dd, hh:nn:ss', Now));
end;

end.
