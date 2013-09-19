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
  end;

var
  Form1: TForm1;

implementation

uses
  LCLType;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  OpenGLControl1.MakeCurrent();
  Application.OnIdle := @IdleFunc;
  aCastleFrame := TCastleFrame.Create(nil);
  aCastleFrame.GLContextOpen;
  aCastleFrame.SetRenderSize(OpenGLControl1.Width, OpenGLControl1.Height);
  aCastleFrame.Load('../../../examples/shadow_fields/models/humanoid_stand.wrl');
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
  OpenGLControl1.Invalidate;
  Done:=false;
end;

procedure TForm1.OpenGLControl1Paint(Sender: TObject);
begin
  OpenGLControl1.MakeCurrent();
  aCastleFrame.Paint();
  OpenGLControl1.SwapBuffers;
end;

end.

