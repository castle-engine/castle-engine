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
  Vcl.CastleControl,
  CastleGLVersion, CastleGLUtils;

type
  TForm1 = class(TForm)
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
  Form1: TForm1;

implementation

{$R *.dfm}

uses CastleRenderOptions, CastleRectangles, CastleColors, CastleRenderContext,
  CastleControls, CastleApplicationProperties, CastleVectors, CastleUIControls;

type
  TMyUi = class(TCastleUserInterface)
    procedure Render; override;
    procedure GLContextOpen; override;
  end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CastleControl := TCastleControl.Create(Self);
  CastleControl.Parent := Self;
  CastleControl.Left := 50;
  CastleControl.Top := 50;
  CastleControl.Width := 400;
  CastleControl.Height := 500;

  CastleControl.Container.Controls.InsertFront(TMyUi.Create(Self));
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  CastleControl.Invalidate; // TODO: should not be needed
end;

{ TMyUi --------------------------------------------------------------------- }

procedure TMyUi.GLContextOpen;
begin
  inherited;
  Form1.Memo1.Lines.Add(GLInformationString);
end;

procedure TMyUi.Render;
begin
  inherited;
  RenderContext.Clear([cbColor], Vector4(0.2, 0.2, 0.2, 1));
  DrawRectangle(FloatRectangle(10, 10, 100, 200), Blue);
  FallbackFont.Print(30, 30, Green, FormatDateTime('yyyy-mm-dd, hh:nn:ss', Now));
end;

end.
