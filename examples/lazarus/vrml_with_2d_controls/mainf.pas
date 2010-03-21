unit MainF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  KambiVRMLBrowser, GL, GLControls;

type
  TForm1 = class(TForm)
    Browser: TKamVRMLBrowser;
    SampleButton: TKamGLButton;
    SampleImage: TKamGLImage;
    SampleImageAlpha: TKamGLImage;
    procedure FormCreate(Sender: TObject);
    procedure BrowserResize(Sender: TObject);
    procedure SampleButtonClick(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

uses VRMLErrors, VRMLScene, KambiUtils;

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
    obscures anything clickable on VRML scene (like TouchSensor) underneath.

    (If you would like to change this, you can set SampleButton.ExlusiveEvents
    to false.)

    (Images (TKamGLImage) actually also obscure scene underneath, but since
    they do not handle any keys or mouse by default, they let themn through
    to 3d scene. This could be changed by overriding their MouseDown etc.) }
  Browser.Controls.Insert(0, SampleButton);
  Browser.Controls.Insert(0, SampleImage);
  Browser.Controls.Insert(0, SampleImageAlpha);

  { Load images (do not do this at design-time, as relative path to the images
    may be not right then) }
  SampleImage.FileName := 'sample_image.png';
  SampleImageAlpha.FileName := 'sample_image_with_alpha.png';
end;

procedure TForm1.BrowserResize(Sender: TObject);
begin
  SampleButton.Width := Browser.Width - 20;
end;

procedure TForm1.SampleButtonClick(Sender: TObject);
begin
  ShowMessage('Button clicked !');
end;

initialization
  {$I mainf.lrs}
end.

