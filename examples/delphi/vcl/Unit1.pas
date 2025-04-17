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
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
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
  CastleCameras, CastleLog;

{ TUiTest --------------------------------------------------------------------- }

type
  { Define a TCastleUserInterface descendant using code,
    to showcase various features from Pascal:

    - observe when GL context is available to do something (here: get OpenGL info)

    - do direct rendering by overriding TUiTest.Render
      (you can even use OpenGL(ES) directly there)

    - handle keys and mouse presses using CGE API.
  }
  TUiTest = class(TCastleUserInterface)
    constructor Create(Owner: TComponent); override;
    procedure Render; override;
    procedure GLContextOpen; override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

constructor TUiTest.Create(Owner: TComponent);
begin
  inherited;
  // keep in front, to not be obscured by designs we load using CastleControl.Container.DesignUrl
  KeepInFront := true;
end;

procedure TUiTest.GLContextOpen;
begin
  inherited;
  Form1.Memo1.Lines.Add(GLInformationString);
end;

procedure TUiTest.Render;
begin
  inherited;
  DrawRectangle(FloatRectangle(5, 5, 10, 10), Blue);
  FallbackFont.Print(30, 5, Yellow, FormatDateTime('yyyy-mm-dd, hh:nn:ss', Now));
end;

function TUiTest.Press(const Event: TInputPressRelease): Boolean;
var
  WalkNav: TCastleWalkNavigation;
begin
  Result := inherited;

  { See https://castle-engine.io/log where to find log output,
    it is also in Delphi Events window. }
  WritelnLog('Press received ' + Event.ToString);

  { To test it, change design to remove TCastleExamineNavigation and add
    WalkNavigation1: TCastleWalkNavigation. }
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
var
  UiTest: TUiTest;
begin
  // Call this to have UI scaling, same as in editor
  CastleControl.Container.LoadSettings('castle-data:/CastleSettings.xml');

  InitializeLog;

  UiTest := TUiTest.Create(Self);
  UiTest.FullSize := true;
  CastleControl.Container.Controls.InsertFront(UiTest);
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  { If you want to handle arrow keys in CastleControl,
    you have to set KeyPreview=true on form,
    and redirect them here to the CastleControl.
    Otherwise arrow keys are used for navigation by Windows,
    and the CastleControl will never even see them.

    In this demo, you can test arrow keys and space by using
    them to rotate in 3D view.
    You can also see key presses received by CGE in Delphi Events window.
  }
  CastleControl.PreviewFormKeyDown(Key, Shift);
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  CastleControl.PreviewFormKeyUp(Key, Shift);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  LabelFps.Caption := 'FPS: ' + CastleControl.Container.Fps.ToString;
end;

end.
