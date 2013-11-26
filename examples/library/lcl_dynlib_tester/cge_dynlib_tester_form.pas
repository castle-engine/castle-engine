{
  Copyright 2008-2013 Jan Adamec, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------

  This is another test project for our library. Instead of including library
  units directly, it uses the compiled dynamic library.

  Note: prior running the project, compile and copy the shared library
  (in src/library/) to a place where it can be loaded:

  - Windows: copy castleengine.dll to this project folder
    (where the executable file is generated), or anywhere on $PATH.

  - Unix: copy libcastleengine.so (or libcastleengine.dylib on Mac OS X)
    to any directory listed on $LD_LIBRARY_PATH.
    For example you can set LD_LIBRARY_PATH to contain empty directory,
    and then you can just copy to the current directory (from which you
    run the program).
    Or you can explicitly list the directory with libcastleengine.so,
    by doing this (in your shell, or even in your ~/.bashrc or similar file):

    export LD_LIBRARY_PATH="$LD_LIBRARY_PATH":<path-to-cge>/castle_game_engine/src/library/
}
unit cge_dynlib_tester_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, OpenGLContext, Forms, Controls, Graphics,
  Dialogs, types;

type

  { TForm1 }

  TForm1 = class(TForm)
    OpenGLControl1: TOpenGLControl;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure OpenGLControl1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure OpenGLControl1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure OpenGLControl1MouseDown(Sender: TObject; Button: Controls.TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenGLControl1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure OpenGLControl1MouseUp(Sender: TObject; Button: Controls.TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenGLControl1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure OpenGLControl1Paint(Sender: TObject);
    procedure IdleFunc(Sender: TObject; var Done: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  LCLType, castlelib_dynloader, ctypes;

{$R *.lfm}

function OpenGlLibraryCallback(eCode, iParam1, iParam2: cInt32):cInt32; cdecl;
begin
  case eCode of
    ecgelibNeedsDisplay: Form1.OpenGLControl1.Invalidate;
    ecgelibSetMouseCursor:
      begin
        case iParam1 of
          ecgecursorNone: Form1.OpenGLControl1.Cursor := crNone;
          ecgecursorWait: Form1.OpenGLControl1.Cursor := crHourGlass;
          ecgecursorHand: Form1.OpenGLControl1.Cursor := crHandPoint;
          ecgecursorText: Form1.OpenGLControl1.Cursor := crIBeam;
          else Form1.OpenGLControl1.Cursor := crDefault;
        end;
      end;
  end;
  Result := 0;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  sFile: String;
begin
  OpenGLControl1.MakeCurrent();
  Application.OnIdle := @IdleFunc;
  CGE_Init();
  CGE_SetRenderParams(OpenGLControl1.Width, OpenGLControl1.Height);
  CGE_SetLibraryCallbackProc(@OpenGlLibraryCallback);
  CGE_SetUserInterfaceInfo(ecgeuiTouch, 96);
  sFile := '../../../../demo_models/navigation/type_walk.wrl';
  //sFile := '../../../examples/shadow_fields/models/humanoid_stand.wrl';
  CGE_LoadSceneFromFile(@sFile[1]);

  OpenGLControl1.Invalidate;
  ActiveControl := OpenGLControl1;   // set focus in order to receive keydowns
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  OpenGLControl1.Width := Width-OpenGLControl1.Left*2;
  OpenGLControl1.Height := Height-OpenGLControl1.Top*2;
  CGE_SetRenderParams(OpenGLControl1.Width, OpenGLControl1.Height);
end;

procedure TForm1.OpenGLControl1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  { TODO }
end;

procedure TForm1.OpenGLControl1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  { TODO }
end;

function ShiftToCgeShift(Shift: TShiftState): integer;
var
  uiShift: Integer;
begin
  uiShift := 0;
  if ssShift in Shift then uiShift := uiShift or ecgessShift;
  if ssCtrl in Shift then uiShift := uiShift or ecgessCtrl;
  if ssAlt in Shift then uiShift := uiShift or ecgessAlt;
  Result := uiShift;
end;

procedure TForm1.OpenGLControl1MouseDown(Sender: TObject; Button: Controls.TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  CGE_OnMouseDown(x, y, Button=mbLeft, ShiftToCgeShift(Shift));
end;

procedure TForm1.OpenGLControl1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  CGE_OnMouseMove(x, y, ShiftToCgeShift(Shift));
end;

procedure TForm1.OpenGLControl1MouseUp(Sender: TObject; Button: Controls.TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  CGE_OnMouseUp(x, y, Button=mbLeft, ShiftToCgeShift(Shift));
end;

procedure TForm1.OpenGLControl1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  CGE_OnMouseWheel(WheelDelta, true, ShiftToCgeShift(Shift));
  Handled := true;
end;

procedure TForm1.IdleFunc(Sender: TObject; var Done: Boolean);
begin
  CGE_OnIdle();
  Done:=false;
end;

procedure TForm1.OpenGLControl1Paint(Sender: TObject);
begin
  OpenGLControl1.MakeCurrent();
  CGE_Render();
  OpenGLControl1.SwapBuffers;
end;

end.

