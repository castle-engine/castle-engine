unit MainF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  KambiVRMLBrowser, OpenGLFonts, GL;

type

  { TForm1 }

  TForm1 = class(TForm)
    Browser: TKamVRMLBrowser;
    procedure BrowserClick(Sender: TObject);
    procedure BrowserDraw(Sender: TObject);
    procedure BrowserGLContextClose(Sender: TObject);
    procedure BrowserGLContextInit(Sender: TObject);
    procedure BrowserResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    GLFont: TGLBitmapFont_Abstract;
    GLSampleImage, GLSampleImageAlpha: TGLuint;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

uses VRMLErrors, VRMLScene, KambiGLUtils, OpenGLBmpFonts,
  BFNT_BitstreamVeraSans_Unit, GLImages, Areas;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  { For simplicity, we just ignore warnings and load VRML from hardcoded path. }
  VRMLNonFatalError := @VRMLNonFatalError_Ignore;
  Browser.Load(
     '../../../../kambi_vrml_test_suite/vrml_2/castle_with_lights_and_camera.wrl'
    {'../../../../kambi_vrml_test_suite/x3d/touch_sensor_tests.x3dv'});
  Browser.Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Browser.Scene.ProcessEvents := true;
end;

procedure Draw2D(Draw2DData: Pointer);
begin
  glLoadIdentity;

  glColor3f(1, 1, 0);
  glRectf(10, 10, Form1.Browser.Width - 10, 100);

  glColor3f(0.2, 0.2, 0.2);
  glRasterPos2i(20, 80);
  Form1.GLFont.Print('Sample text drawn on GL area. Yellow rect is clickable.');

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

procedure TForm1.BrowserClick(Sender: TObject);
begin
  { Since Browser.IgnoreAreas contain our yellow rect area, we can use
    it here: FindArea tells us if our click hits this area. }
  if Browser.IgnoreAreas.FindArea(Browser.MouseX, Browser.MouseY) = 0 then
    ShowMessage('Clicked yellow rect !');
end;

procedure TForm1.BrowserGLContextInit(Sender: TObject);
begin
  GLFont := TGLBitmapFont.Create(@BFNT_BitstreamVeraSans);
  GLSampleImage := LoadImageToDisplayList('sample_image.png', [], [], 0, 0);
  GLSampleImageAlpha := LoadImageToDisplayList('sample_image_with_alpha.png', [], [], 0, 0);
end;

procedure TForm1.BrowserResize(Sender: TObject);
begin
  { This is the same area that is drawn by glRectf. It has Y coords reverted
    by "Browser.Height - ", since this is expected by IgnoreAreas (it requires
    points in the same coord system as MouseX/Y).

    Using Browser.IgnoreAreas is needed, otherwise VRML scene sensors
    (like TouchSensor) could intercept mouse clicks (that is, when you
    would click on your yellow rectangle, you could also click on TouchSensor
    under it).

    This depends on Browser.Width/Height, so it's safest to redo this in
    Browser.OnResize. }
  Browser.IgnoreAreas.Count := 0;
  Browser.IgnoreAreas.AppendItem(AreaCorners(10, Browser.Height - 10,
    Browser.Width - 10, Browser.Height - 100));
end;

procedure TForm1.BrowserGLContextClose(Sender: TObject);
begin
  FreeAndNil(GLFont);

  { Calling glFreeDisplayList is not needed here (when GL context is closed,
    all display lists are freed anyway). I just do it for elegance. }
  glFreeDisplayList(GLSampleImage);
  glFreeDisplayList(GLSampleImageAlpha);
end;

initialization
  {$I mainf.lrs}

end.

