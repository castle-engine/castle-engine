unit cge_lib_tester_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, OpenGLContext, Forms, Controls, Graphics,
  Dialogs, CastleFrame, CastleKeysMouse, CastleLCLUtils, types;

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
    aCastleFrame: TCastleFrame;
    nCurrentViewpoint: integer;
  end;

var
  Form1: TForm1;

implementation

uses
  LCLType, ctypes, CastleControls, CastleGLUtils, CastleCameras;

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
  OglInfo: string;
begin
  nCurrentViewpoint := 0;

  OpenGLControl1.MakeCurrent();
  Application.OnIdle := @IdleFunc;
  aCastleFrame := TCastleFrame.Create(nil);
  aCastleFrame.GLContextOpen;
  aCastleFrame.SetRenderSize(OpenGLControl1.Width, OpenGLControl1.Height);
  aCastleFrame.SetLibraryCallbackProc(@OpenGlLibraryCallback);
  aCastleFrame.UserInterface := euiTouch;

  aCastleFrame.Load('../../../../demo_models/navigation/type_walk.wrl');
  //aCastleFrame.Load('../../../examples/shadow_fields/models/humanoid_stand.wrl');
  //aCastleFrame.Load('mojedata/Image3D.wrl');

  OglInfo := GLInformationString;
  //Application.MessageBox(PChar(OglInfo), 'GL Info', 0);

  OpenGLControl1.Invalidate;
  ActiveControl := OpenGLControl1;   // set focus in order to receive keydowns
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  OpenGLControl1.Width := Width-OpenGLControl1.Left*2;
  OpenGLControl1.Height := Height-OpenGLControl1.Top*2;
  aCastleFrame.SetRenderSize(OpenGLControl1.Width, OpenGLControl1.Height);
end;

procedure TForm1.OpenGLControl1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  MyKey: TKey;
  Ch: char;
begin
  KeyLCLToCastle(Key, Shift, MyKey, Ch);

  if Key = VK_F3 then        // previous veiwpoint
  begin
     if nCurrentViewpoint > 0 then
       Dec(nCurrentViewpoint)
     else
       nCurrentViewpoint := aCastleFrame.MainScene.ViewpointsCount-1;
     aCastleFrame.MainScene.MoveToViewpoint(nCurrentViewpoint);
     Exit;
  end;
  if Key = VK_F4 then        // next viewpoint
  begin
     if nCurrentViewpoint < aCastleFrame.MainScene.ViewpointsCount-1 then
       Inc(nCurrentViewpoint)
     else
       nCurrentViewpoint := 0;
     aCastleFrame.MainScene.MoveToViewpoint(nCurrentViewpoint);
     Exit;
  end;
  if Key = VK_F5 then        // rebind current viewpoint
  begin
     aCastleFrame.MainScene.MoveToViewpoint(nCurrentViewpoint);
     Exit;
  end;
  if Key = VK_F6 then        // add new viewpoint
  begin
     aCastleFrame.MainScene.AddViewpointFromCamera(aCastleFrame.Camera, 'New View');
     nCurrentViewpoint := aCastleFrame.MainScene.ViewpointsCount-1;
     Exit;
  end;
  if Key = VK_F then
  begin
     aCastleFrame.SetNavigationType(ntFly);
     Exit;
  end;

  { Do not change focus by arrow keys, this would breaks our handling of arrows
    over TCastleControl. We can prevent Lazarus from interpreting these
    keys as focus-changing (actually, Lazarus tells widget managet that these
    are already handled) by setting them to zero.
    Note: our MyKey/Ch (passed to KeyDownEvent) are calculated earlier,
    so they will correctly capture arrow keys. }
  if (Key = VK_Down) or
     (Key = VK_Up) or
     (Key = VK_Right) or
     (Key = VK_Left) then
    Key := 0;

  if (MyKey <> K_None) or (Ch <> #0) then
    if aCastleFrame.OnKeyDown(MyKey, Ch) then
      Key := 0; // handled
end;

procedure TForm1.OpenGLControl1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  MyKey: TKey;
  Ch: char;
begin
  KeyLCLToCastle(Key, Shift, MyKey, Ch);

  { Do not change focus by arrow keys, this breaks our handling of them.
    See KeyDown for more comments. }
  if (Key = VK_Down) or
     (Key = VK_Up) or
     (Key = VK_Right) or
     (Key = VK_Left) then
    Key := 0;

  if (MyKey <> K_None) or (Ch <> #0) then
    if aCastleFrame.OnKeyUp(MyKey, Ch) then
      Key := 0; // handled
end;

procedure TForm1.OpenGLControl1MouseDown(Sender: TObject; Button: Controls.TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  MyButton: CastleKeysMouse.TMouseButton;
begin
  if MouseButtonLCLToCastle(Button, MyButton) then
    aCastleFrame.OnMouseDown(MyButton, Shift, X, Y);
end;

procedure TForm1.OpenGLControl1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  aCastleFrame.OnMouseMove(Shift, X, Y);
end;

procedure TForm1.OpenGLControl1MouseUp(Sender: TObject; Button: Controls.TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  MyButton: CastleKeysMouse.TMouseButton;
begin
  if MouseButtonLCLToCastle(Button, MyButton) then
    aCastleFrame.OnMouseUp(MyButton, Shift, X, Y);
end;

procedure TForm1.OpenGLControl1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
    Handled := aCastleFrame.OnMouseWheel(WheelDelta/120, true);
end;

procedure TForm1.IdleFunc(Sender: TObject; var Done: Boolean);
begin
  aCastleFrame.Update;
  Done:=false;
end;

procedure TForm1.OpenGLControl1Paint(Sender: TObject);
begin
  OpenGLControl1.MakeCurrent();
  aCastleFrame.Paint();
  OpenGLControl1.SwapBuffers;
end;

end.

