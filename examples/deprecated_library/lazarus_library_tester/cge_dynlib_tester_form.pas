{
  Copyright 2008-2018 Jan Adamec, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------

  This is a test project for our library in src/deprecated_library/.
  It uses library API (through the castlelib_dynloader unit),
  and uses a compiled dynamic library with the engine.

  THIS IS *NOT* HOW YOU SHOULD USUALLY USE THE ENGINE from FPC/Lazarus.
  If you use FPC/Lazarus to make your game, then usually you want to simply
  use the engine units (and Lazarus packages). For example, unit CastleControl
  gives you a nice Lazarus component readily integrated with the engine,
  CastleWindow gives you a window (without LCL dependency) integrated with
  engine. Countless other engine units give you useful things
  (like CastleScene, CastleViewport, CastleVectors... see the engine manual).

  Using the engine units directly gives you a complete object-oriented API
  in ObjectPascal to do everything :) The C library API (exposed in
  castlelib_dynloader) offers only a small subset of engine functionality.
  The library is useful to access the engine from other programming languages.

  HOW TO RUN THIS: prior to running this project,
  compile and copy the shared library
  (in src/deprecated_library/) to a place where it can be loaded:

  - Windows: copy castleengine.dll to this project folder
    (where the executable file is generated), or anywhere on $PATH.

  - Unix: copy libcastleengine.so (or libcastleengine.dylib on macOS)
    to any directory listed on $LD_LIBRARY_PATH.
    For example you can set LD_LIBRARY_PATH to contain empty directory,
    and then you can just copy to the current directory (from which you
    run the program).
    Or you can explicitly list the directory with libcastleengine.so,
    by doing this (in your shell, or even in your ~/.bashrc or similar file):

    export LD_LIBRARY_PATH="$LD_LIBRARY_PATH":"$CASTLE_ENGINE_PATH"/src/deprecated_library/
}
unit cge_dynlib_tester_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, OpenGLContext, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, Types;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnNavWalk: TToggleBox;
    BtnNavFly: TToggleBox;
    BtnNavExamine: TToggleBox;
    BtnNavTurntable: TToggleBox;
    BtnScreenshot: TButton;
    BtnOpen: TButton;
    CbViewpoints: TComboBox;
    OpenDialog1: TOpenDialog;
    OpenGLControl1: TOpenGLControl;
    Panel1: TPanel;
    procedure BtnOpenClick(Sender: TObject);
    procedure BtnScreenshotClick(Sender: TObject);
    procedure BtnWalkClick(Sender: TObject);
    procedure CbViewpointsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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
    procedure UpdateUIAfterOpen;
    procedure UpdateNavigationButtons;
    procedure FillViewpoints;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  LCLType, castlelib_dynloader, ctypes;

var
  bIgnoreNotifications: boolean;

{$R *.lfm}

function OpenGlLibraryCallback(eCode, iParam1, iParam2: cInt32; szParam: pcchar):cInt32; cdecl;
begin
  case eCode of
    ecgelibNeedsDisplay: Form1.OpenGLControl1.Invalidate;
    ecgelibSetMouseCursor:
      case iParam1 of
        ecgecursorNone: Form1.OpenGLControl1.Cursor := crNone;
        ecgecursorWait: Form1.OpenGLControl1.Cursor := crHourGlass;
        ecgecursorHand: Form1.OpenGLControl1.Cursor := crHandPoint;
        ecgecursorText: Form1.OpenGLControl1.Cursor := crIBeam;
        else Form1.OpenGLControl1.Cursor := crDefault;
      end;
    ecgelibNavigationTypeChanged: Form1.UpdateNavigationButtons;
  end;
  Result := 0;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  sFile: String;
begin
  bIgnoreNotifications := false;

  OpenGLControl1.MakeCurrent();
  Application.OnIdle := @IdleFunc;
  CGE_Initialize(PCChar(PChar(GetAppConfigDir(false))));
  CGE_Open(ecgeofLog, OpenGLControl1.Width, OpenGLControl1.Height, 96);
  CGE_SetLibraryCallbackProc(@OpenGlLibraryCallback);
  CGE_SetUserInterface(true);
  sFile := 'data/bridge_level/bridge_final.x3dv';
  CGE_LoadSceneFromFile(@sFile[1]);
  UpdateUIAfterOpen;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  CGE_Close();
  CGE_Finalize();
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  OpenGLControl1.Width := Width-OpenGLControl1.Left*2;
  OpenGLControl1.Height := Height-OpenGLControl1.Top*2;
  CGE_Resize(OpenGLControl1.Width, OpenGLControl1.Height);
end;

{ Convert Key (Lazarus key code) to Castle Game Engine library kcge_Xxx constant.
  Returns kcge_None if not possible. }
function KeyLCLToCastleLibrary(const Key: Word; const Shift: TShiftState): CInt32;
begin
  { Note: We have almost the same conversion (LCL -> CGE TKey) already implemented
    in KeyLCLToCastle in CastleLCLUtils.
    But this example application deliberately does not use any CGE units,
    it only uses CGE as a shared library through the API in castlelib_dynloader.
    So we duplicate here logic of KeyLCLToCastle. }

  Result := kcge_None;
  case Key of
    VK_BACK:       Result := kcge_BackSpace;
    VK_TAB:        Result := kcge_Tab;
    VK_RETURN:     Result := kcge_Enter;
    VK_SHIFT:      Result := kcge_Shift;
    VK_CONTROL:    Result := kcge_Ctrl;
    VK_MENU:       Result := kcge_Alt;
    VK_ESCAPE:     Result := kcge_Escape;
    VK_SPACE:      Result := kcge_Space;
    VK_PRIOR:      Result := kcge_PageUp;
    VK_NEXT:       Result := kcge_PageDown;
    VK_END:        Result := kcge_End;
    VK_HOME:       Result := kcge_Home;
    VK_LEFT:       Result := kcge_Left;
    VK_UP:         Result := kcge_Up;
    VK_RIGHT:      Result := kcge_Right;
    VK_DOWN:       Result := kcge_Down;
    VK_INSERT:     Result := kcge_Insert;
    VK_DELETE:     Result := kcge_Delete;
    VK_ADD:        Result := kcge_Numpad_Plus;
    VK_SUBTRACT:   Result := kcge_Numpad_Minus;
    VK_SNAPSHOT:   Result := kcge_PrintScreen;
    VK_NUMLOCK:    Result := kcge_NumLock;
    VK_SCROLL:     Result := kcge_ScrollLock;
    VK_CAPITAL:    Result := kcge_CapsLock;
    VK_PAUSE:      Result := kcge_Pause;
    VK_OEM_COMMA:  Result := kcge_Comma;
    VK_OEM_PERIOD: Result := kcge_Period;
    VK_NUMPAD0:    Result := kcge_Numpad_0;
    VK_NUMPAD1:    Result := kcge_Numpad_1;
    VK_NUMPAD2:    Result := kcge_Numpad_2;
    VK_NUMPAD3:    Result := kcge_Numpad_3;
    VK_NUMPAD4:    Result := kcge_Numpad_4;
    VK_NUMPAD5:    Result := kcge_Numpad_5;
    VK_NUMPAD6:    Result := kcge_Numpad_6;
    VK_NUMPAD7:    Result := kcge_Numpad_7;
    VK_NUMPAD8:    Result := kcge_Numpad_8;
    VK_NUMPAD9:    Result := kcge_Numpad_9;
    VK_CLEAR:      Result := kcge_Numpad_Begin;
    VK_MULTIPLY:   Result := kcge_Numpad_Multiply;
    VK_DIVIDE:     Result := kcge_Numpad_Divide;
    VK_OEM_MINUS:  Result := kcge_Minus;
    VK_OEM_PLUS:
      if ssShift in Shift then
        Result := kcge_Plus
      else
        Result := kcge_Equal;
    Ord('0') .. Ord('9'):
      Result := Ord(kcge_0)  + Ord(Key) - Ord('0');
    Ord('A') .. Ord('Z'):
      Result := Ord(kcge_A)  + Ord(Key) - Ord('A');
    VK_F1 .. VK_F12:
      Result := Ord(kcge_F1) + Ord(Key) - VK_F1;
  end;
end;

procedure TForm1.OpenGLControl1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  KeyCge: CInt32;
begin
  { Test CGE_MoveToViewpoint by switching to 1st, 2nd or 3rd viewpoint
    when you press 1,2,3 keys.
    Hold Shift to switch with smooth animation instead of instantly.

    Example test model for this: demo-models/navigation/viewpoints_various_tests.x3dv
    (switch between 1st and 3rd viewpoints there, note that 2nd viewpoint there equals 1st). }
  case Key of
    VK_1: CGE_MoveToViewpoint(0, ssShift in Shift);
    VK_2: CGE_MoveToViewpoint(1, ssShift in Shift);
    VK_3: CGE_MoveToViewpoint(2, ssShift in Shift);
  end;

  KeyCge := KeyLCLToCastleLibrary(Key, Shift);
  if KeyCge <> kcge_None then
    CGE_KeyDown(KeyCge);
end;

procedure TForm1.OpenGLControl1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  KeyCge: CInt32;
begin
  KeyCge := KeyLCLToCastleLibrary(Key, Shift);
  if KeyCge <> kcge_None then
    CGE_KeyUp(KeyCge);
end;

const
  { Because of deprecated CastleKeysMouse conflict (deprecated CastleKeysMouse.mbLeft),
    we need to write "Controls.mbLeft" not just "mbLeft".
    But we cannot use inside TForm1 method, as there it is a property that calls GetControl. }
  LeftMouseButton = Controls.mbLeft;

procedure TForm1.OpenGLControl1MouseDown(Sender: TObject; Button: Controls.TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  CGE_MouseDown(x, OpenGLControl1.Height - 1 - y, Button=LeftMouseButton, 0);
end;

procedure TForm1.OpenGLControl1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  CGE_Motion(x, OpenGLControl1.Height - 1 - y, 0);
end;

procedure TForm1.OpenGLControl1MouseUp(Sender: TObject; Button: Controls.TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  CGE_MouseUp(x, OpenGLControl1.Height - 1 - y, Button=LeftMouseButton, 0, true);
end;

procedure TForm1.OpenGLControl1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  CGE_MouseWheel(WheelDelta, true);
  Handled := true;
end;

procedure TForm1.IdleFunc(Sender: TObject; var Done: Boolean);
begin
  CGE_Update();
  Done:=false;
end;

procedure TForm1.OpenGLControl1Paint(Sender: TObject);
begin
  OpenGLControl1.MakeCurrent();
  CGE_Render();
  OpenGLControl1.SwapBuffers;
end;

procedure TForm1.BtnScreenshotClick(Sender: TObject);
var
  sFile: string;
  csFile: array[0..260] of char;
begin
  sFile := ExtractFilePath(Application.ExeName) + 'cge_test_screenshot.png';
  StrPCopy(csFile, sFile);
  CGE_SaveScreenshotToFile(@csFile[0]);
end;

procedure TForm1.BtnOpenClick(Sender: TObject);
var
  csFile: array[0..260] of char;
begin
  { Note about OpenDialog1 (TOpenDialog) usage:
    In a "real" Castle Game Engine application using LCL, we recommend to use
    component TCastleOpen3DDialog (from castle_components.lpk) 
    to have a dialog box to select a file to load in TCastleScene.
    However, in case of this application, it deliberately *does not* use CGE in a normal
    way (through Lazarus packages or Pascal units), it only accesses CGE as a shared library.
    That's why we decided to define OpenDialog1 as TOpenDialog, not TCastleOpen3DDialog. }
    
  if OpenDialog1.Execute then
  begin
    StrPCopy(csFile, OpenDialog1.Filename);

    CGE_LoadSceneFromFile(@csFile[0]);

    UpdateUIAfterOpen;
  end;
end;

procedure TForm1.UpdateUIAfterOpen;
begin
  OpenGLControl1.Invalidate;
  FillViewpoints;
  ActiveControl := OpenGLControl1;   // set focus in order to receive keydowns
  UpdateNavigationButtons;
end;

procedure TForm1.BtnWalkClick(Sender: TObject);
begin
  if bIgnoreNotifications then exit;

  CGE_SetNavigationType((Sender as TToggleBox).Tag);
  UpdateNavigationButtons;
end;

procedure TForm1.UpdateNavigationButtons;
var
  iType: integer;
  bOldIgnore: boolean;
begin
  bOldIgnore := bIgnoreNotifications;
  bIgnoreNotifications := true;

  iType := CGE_GetNavigationType();
  BtnNavWalk.Checked := (iType = ecgenavWalk);
  BtnNavFly.Checked := (iType = ecgenavFly);
  BtnNavExamine.Checked := (iType = ecgenavExamine);
  BtnNavTurntable.Checked := (iType = ecgenavTurntable);

  bIgnoreNotifications := bOldIgnore;
end;

procedure TForm1.FillViewpoints;
var
  i, nCount: integer;
  csName: array[0..260] of char;
  bOldIgnore: boolean;
begin
  bOldIgnore := bIgnoreNotifications;
  bIgnoreNotifications := true;

  CbViewpoints.Items.Clear;
  nCount := CGE_GetViewpointsCount();
  for i := 0 to nCount-1 do
  begin
    CGE_GetViewpointName(i, @csName[0], 260);
    CbViewpoints.Items.Add(csName);
  end;

  bIgnoreNotifications := bOldIgnore;
end;

procedure TForm1.CbViewpointsChange(Sender: TObject);
begin
  if bIgnoreNotifications then exit;

  CGE_MoveToViewpoint(CbViewpoints.ItemIndex, true);
end;

end.
