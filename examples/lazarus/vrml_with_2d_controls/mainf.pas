unit MainF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  KambiVRMLBrowser, GL, GLButtons;

type
  TForm1 = class(TForm)
    Browser: TKamVRMLBrowser;
    SampleButton: TKamGLButton;
    procedure BrowserDraw(Sender: TObject);
    procedure BrowserGLContextClose(Sender: TObject);
    procedure BrowserGLContextInit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BrowserResize(Sender: TObject);
    procedure SampleButtonClick(Sender: TObject);
  private
    GLSampleImage, GLSampleImageAlpha: TGLuint;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses VRMLErrors, VRMLScene, KambiGLUtils, GLImages, Areas, KeysMouse, KambiUtils;

{ TForm1 --------------------------------------------------------------------- }

procedure TForm1.FormCreate(Sender: TObject);
begin
  { For simplicity, we just ignore warnings and load VRML from hardcoded path. }
  VRMLWarning := @VRMLWarning_Ignore;
  Browser.Load(
     '../../../../kambi_vrml_test_suite/vrml_2/castle_with_lights_and_camera.wrl'
     //'../../../../kambi_vrml_test_suite/x3d/touch_sensor_tests.x3dv'
     //'../../../../kambi_vrml_test_suite/x3d/anchor_test.x3dv'
     );
  Browser.Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Browser.Scene.ProcessEvents := true;

  { Thanks to using this button as a TUIControl descendant
    (placing it on Browser.Controls list), VRML scene sensors
    (like TouchSensor) will not intercept mouse clicks. That is, button
    obscures anything clickable on VRML scene (like TouchSensor)
    underneath. }
  Browser.Controls.Insert(0, SampleButton);
  { TODO: setting this in object inspector is messed up with "magic"
    non-visual components's Left. }
  SampleButton.Left := 10;
end;

procedure TForm1.BrowserResize(Sender: TObject);
begin
  SampleButton.Width := Browser.Width - 20;
end;

procedure TForm1.SampleButtonClick(Sender: TObject);
begin
  ShowMessage('Button clicked !');
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

