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
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo,
  Fmx.CastleControl,
  CastleGLUtils, CastleControls, CastleUIControls;

type
  TTestCgeControl = class(TForm)
    Timer1: TTimer;
    PanelSideBar: TPanel;
    Memo1: TMemo;
    Button2D: TButton;
    ButtonUI: TButton;
    Button3D: TButton;
    LabelFps: TLabel;
    CastleControl: TCastleControl;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button3DClick(Sender: TObject);
    procedure Button2DClick(Sender: TObject);
    procedure ButtonUIClick(Sender: TObject);
  public
    { Public declarations }
  end;

var
  TestCgeControl: TTestCgeControl;

implementation

{$R *.fmx}

uses
  CastleRenderOptions, CastleRectangles, CastleColors, CastleRenderContext,
  CastleVectors;

{ TUiTest -------------------------------------------------------------- }

type
  TUiTest = class(TCastleUserInterface)
    constructor Create(Owner: TComponent); override;
    procedure Render; override;
    procedure GLContextOpen; override;
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
  TestCgeControl.Memo1.Lines.Add(GLInformationString);
end;

procedure TUiTest.Render;
begin
  inherited;
  DrawRectangle(FloatRectangle(5, 5, 10, 10), Yellow);
  FallbackFont.Print(30, 5, Green, FormatDateTime('yyyy-mm-dd, hh:nn:ss', Now));
end;

{ TTestCgeControl ------------------------------------------------------------ }

procedure TTestCgeControl.Button2DClick(Sender: TObject);
begin
  CastleControl.Container.DesignUrl := 'castle-data:/test_2d.castle-user-interface';
end;

procedure TTestCgeControl.Button3DClick(Sender: TObject);
begin
  CastleControl.Container.DesignUrl := 'castle-data:/test_3d.castle-user-interface';
end;

procedure TTestCgeControl.ButtonUIClick(Sender: TObject);
begin
  CastleControl.Container.DesignUrl := 'castle-data:/test_ui.castle-user-interface';
end;

procedure TTestCgeControl.FormCreate(Sender: TObject);
begin
  // Call this to have UI scaling, same as in editor
  CastleControl.Container.LoadSettings('castle-data:/CastleSettings.xml');

  CastleControl.Container.DesignUrl := 'castle-data:/test_3d.castle-user-interface';

  // adding a component created by code, doing manual rendering in TUiTest.Render
  CastleControl.Container.Controls.InsertFront(TUiTest.Create(Self));
end;

procedure TTestCgeControl.Timer1Timer(Sender: TObject);
begin
  LabelFps.Text := 'FPS: ' + CastleControl.Container.Fps.ToString;
end;

end.
