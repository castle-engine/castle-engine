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
  CastleGLVersion, CastleGLUtils, CastleControls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Timer1: TTimer;
    Button3D: TButton;
    Button2D: TButton;
    ButtonUI: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button3DClick(Sender: TObject);
    procedure Button2DClick(Sender: TObject);
    procedure ButtonUIClick(Sender: TObject);
  private
    DesignUi: TCastleDesign;
    CastleControl: TCastleControl;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses CastleRenderOptions, CastleRectangles, CastleColors, CastleRenderContext,
  CastleApplicationProperties, CastleVectors, CastleUIControls;

type
  TMyRenderTest = class(TCastleUserInterface)
    procedure Render; override;
    procedure GLContextOpen; override;
  end;

procedure TForm1.Button2DClick(Sender: TObject);
begin
  DesignUi.Url := 'castle-data:/test_2d.castle-user-interface';
end;

procedure TForm1.Button3DClick(Sender: TObject);
begin
  DesignUi.Url := 'castle-data:/main.castle-user-interface';
end;

procedure TForm1.ButtonUIClick(Sender: TObject);
begin
  DesignUi.Url := 'castle-data:/test_ui.castle-user-interface';
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CastleControl := TCastleControl.Create(Self);
  CastleControl.Parent := Self;
  //  CastleControl.Left := 50;
  //  CastleControl.Top := 50;
  //  CastleControl.Width := 400;
  //  CastleControl.Height := 500;
  CastleControl.Align := alClient;

  // adding a design (made in CGE editor) using TCastleDesign
  DesignUi := TCastleDesign.Create(Self);
  DesignUi.Url := 'castle-data:/main.castle-user-interface';
  DesignUi.FullSize := true;
  CastleControl.Container.Controls.InsertFront(DesignUi);

  // adding a component created by code, doing manual rendering in TMyRenderTest.Render
  CastleControl.Container.Controls.InsertFront(TMyRenderTest.Create(Self));
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  CastleControl.Invalidate; // TODO: should not be needed
end;

{ TMyUi --------------------------------------------------------------------- }

procedure TMyRenderTest.GLContextOpen;
begin
  inherited;
  Form1.Memo1.Lines.Add(GLInformationString);
end;

procedure TMyRenderTest.Render;
begin
  inherited;
  DrawRectangle(FloatRectangle(5, 5, 10, 10), Blue);
  FallbackFont.Print(30, 30, Green, FormatDateTime('yyyy-mm-dd, hh:nn:ss', Now));
end;

end.
