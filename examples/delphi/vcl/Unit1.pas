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
    Button3D: TButton;
    Button2D: TButton;
    ButtonUI: TButton;
    LabelFps: TLabel;
    Timer1: TTimer;
    CastleControl: TCastleControl;
    procedure FormCreate(Sender: TObject);
    procedure Button3DClick(Sender: TObject);
    procedure Button2DClick(Sender: TObject);
    procedure ButtonUIClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses CastleRenderOptions, CastleRectangles, CastleColors, CastleRenderContext,
  CastleApplicationProperties, CastleVectors, CastleUIControls, CastleKeysMouse,
  CastleCameras;

{ TMyRenderTest --------------------------------------------------------------------- }

type
  TMyRenderTest = class(TCastleUserInterface)
    procedure Render; override;
    procedure GLContextOpen; override;
    //function Press(const Event: TInputPressRelease): Boolean; override;
  end;

procedure TMyRenderTest.GLContextOpen;
begin
  inherited;
  Form1.Memo1.Lines.Add(GLInformationString);
end;

procedure TMyRenderTest.Render;
begin
  inherited;
  DrawRectangle(FloatRectangle(5, 5, 10, 10), Blue);
  FallbackFont.Print(30, 5, Yellow, FormatDateTime('yyyy-mm-dd, hh:nn:ss', Now));
end;

{
function TMyRenderTest.Press(const Event: TInputPressRelease): Boolean;
var
  WalkNav: TCastleWalkNavigation;
begin
  Result := inherited;

  if Event.IsMouseButton(buttonRight) then
  begin
    WalkNav := Form1.CastleControl.Container.DesignedComponent(
      'WalkNavigation1', false) as TCastleWalkNavigation;
    if WalkNav <> nil then
    begin
      WalkNav.MouseLook := not WalkNav.MouseLook;
      Exit(true);
    end;
  end;
end;
}

{ TForm1 --------------------------------------------------------------------- }

procedure TForm1.Button2DClick(Sender: TObject);
begin
  CastleControl.Container.DesignUrl := 'castle-data:/test_2d.castle-user-interface';
end;

procedure TForm1.Button3DClick(Sender: TObject);
begin
  CastleControl.Container.DesignUrl := 'castle-data:/test_3d.castle-user-interface';
end;

procedure TForm1.ButtonUIClick(Sender: TObject);
begin
  CastleControl.Container.DesignUrl := 'castle-data:/test_ui.castle-user-interface';
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // adding a component created by code, doing manual rendering in TMyRenderTest.Render
  CastleControl.Container.Controls.InsertFront(TMyRenderTest.Create(Self));
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  LabelFps.Caption := 'FPS: ' + CastleControl.Container.Fps.ToString;
end;

end.
