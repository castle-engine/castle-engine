unit MainF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  KambiVRMLBrowser, OpenGLFonts, GL, UIControls;

type
  TForm1 = class(TForm)
    Browser: TKamVRMLBrowser;
    procedure BrowserDraw(Sender: TObject);
    procedure BrowserGLContextClose(Sender: TObject);
    procedure BrowserGLContextInit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    GLSampleImage, GLSampleImageAlpha: TGLuint;
    ClickableRect: TUIControl;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses VRMLErrors, VRMLScene, KambiGLUtils, OpenGLBmpFonts,
  BFNT_BitstreamVeraSans_Unit, GLImages, Areas, KeysMouse, KambiUtils;

{ TClickableRect ------------------------------------------------------------- }

type
  { Clickable rectangular area on 2D screen.

    Thanks to making this rectangle a TUIControl descendant,
    and placing it on Browser.Controls list, VRML scene sensors
    (like TouchSensor) will not intercept mouse clicks. That is, yellow
    rectangle obscures anything clickable on VRML scene (like TouchSensor)
    underneath. }
  TClickableRect = class(TUIControl)
  private
    GLFont: TGLBitmapFont_Abstract;
  public
    function MouseDown(const Button: KeysMouse.TMouseButton): boolean; override;
    function DrawStyle: TUIControlDrawStyle; override;
    procedure Draw(const Focused: boolean); override;
    function PositionInside(const X, Y: Integer): boolean; override;
    procedure GLContextInit; override;
    procedure GLContextClose; override;
  end;

function TClickableRect.MouseDown(const Button: KeysMouse.TMouseButton): boolean;
begin
  ShowMessage('Clicked yellow rect !');
  Result := true;
end;

function TClickableRect.DrawStyle: TUIControlDrawStyle;
begin
  Result := ds2D;
end;

procedure TClickableRect.Draw(const Focused: boolean);
begin
  glColor3f(1, 1, 0);
  glRectf(10, 10, ContainerWidth - 10, 100);

  glColor3f(0.2, 0.2, 0.2);
  glRasterPos2i(20, 80);
  GLFont.Print('Sample text drawn on GL area. Yellow rect is clickable.');
end;

function TClickableRect.PositionInside(const X, Y: Integer): boolean;
begin
  { This is the same area that is drawn by glRectf. }
  Result := Between(X, 10, ContainerWidth - 10) and
            Between(Y, ContainerHeight - 100, ContainerHeight - 10);
end;

procedure TClickableRect.GLContextInit;
begin
  inherited;
  GLFont := TGLBitmapFont.Create(@BFNT_BitstreamVeraSans);
end;

procedure TClickableRect.GLContextClose;
begin
  FreeAndNil(GLFont);
  inherited;
end;

{ TForm1 --------------------------------------------------------------------- }

procedure TForm1.FormCreate(Sender: TObject);
begin
  { For simplicity, we just ignore warnings and load VRML from hardcoded path. }
  VRMLWarning := @VRMLWarning_Ignore;
  Browser.Load(
     '../../../../kambi_vrml_test_suite/vrml_2/castle_with_lights_and_camera.wrl'
    {'../../../../kambi_vrml_test_suite/x3d/touch_sensor_tests.x3dv'});
  Browser.Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Browser.Scene.ProcessEvents := true;

  ClickableRect := TClickableRect.Create(Self);
  Browser.Controls.Insert(0, ClickableRect);
end;

procedure Draw2D(Draw2DData: Pointer);
begin
  glLoadIdentity;

  glRasterPos2i(10, 150);
  glCallList(Form1.GLSampleImage);

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);
    glRasterPos2i(310, 150);
    glCallList(Form1.GLSampleImageAlpha);
  glDisable(GL_BLEND);
end;

procedure TForm1.BrowserDraw(Sender: TObject);
begin
  glPushAttrib(GL_ENABLE_BIT);
    glDisable(GL_LIGHTING);
    glProjectionPushPopOrtho2D(@Draw2d, nil, 0, Browser.Width, 0, Browser.Height);
  glPopAttrib;
end;

procedure TForm1.BrowserGLContextInit(Sender: TObject);
begin
  GLSampleImage := LoadImageToDisplayList('sample_image.png', [], [], 0, 0);
  GLSampleImageAlpha := LoadImageToDisplayList('sample_image_with_alpha.png', [], [], 0, 0);
end;

procedure TForm1.BrowserGLContextClose(Sender: TObject);
begin
  { Calling glFreeDisplayList is not needed here (when GL context is closed,
    all display lists are freed anyway). I just do it for elegance. }
  glFreeDisplayList(GLSampleImage);
  glFreeDisplayList(GLSampleImageAlpha);
end;

initialization
  {$I mainf.lrs}
end.

