unit MainF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  CastleGLControl, GL, GLControls, GLMenu;

type

  { TForm1 }

  TForm1 = class(TForm)
    Browser: TCastleControl;
    GLMenu1: TCastleMenu;
    SampleButton: TCastleButton;
    SampleImage: TCastleImage;
    SampleImageAlpha: TCastleImage;
    ButtonImageFixSize1: TCastleButton;
    ButtonImageFixSize2: TCastleButton;
    ButtonImageFixSize3: TCastleButton;
    ButtonImageFixSize4: TCastleButton;
    ButtonImage1: TCastleButton;
    ButtonImage2: TCastleButton;
    ButtonImage3: TCastleButton;
    ButtonImage4: TCastleButton;
    procedure FormCreate(Sender: TObject);
    procedure BrowserResize(Sender: TObject);
    procedure GLMenu1Click(Sender: TObject);
    procedure SampleButtonClick(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

uses CastleSceneCore, CastleUtils, Images, VectorMath;

{ TForm1 --------------------------------------------------------------------- }

procedure TForm1.FormCreate(Sender: TObject);
begin
  { For simplicity, we just load VRML/X3D from hardcoded path. }
  Browser.Load('../../vrml/models/bridge_final.x3dv');
  Browser.MainScene.Spatial := [ssRendering, ssDynamicCollisions];
  Browser.MainScene.ProcessEvents := true;

  { Thanks to using this button as a TUIControl descendant
    (placing it on Browser.Controls list), VRML scene sensors
    (like TouchSensor) will not intercept mouse clicks. That is, button
    obscures anything clickable on VRML scene (like TouchSensor) underneath.

    (If you would like to change this, you can set SampleButton.ExlusiveEvents
    to false.)

    (Images (TCastleImage) actually also obscure scene underneath, but since
    they do not handle any keys or mouse by default, they let themn through
    to 3d scene. This could be changed by overriding their MouseDown etc.) }

  Browser.Controls.Insert(0, SampleButton);
  Browser.Controls.Insert(0, SampleImage);
  Browser.Controls.Insert(0, SampleImageAlpha);
  Browser.Controls.Insert(0, ButtonImageFixSize1);
  Browser.Controls.Insert(0, ButtonImageFixSize2);
  Browser.Controls.Insert(0, ButtonImageFixSize3);
  Browser.Controls.Insert(0, ButtonImageFixSize4);
  Browser.Controls.Insert(0, ButtonImage1);
  Browser.Controls.Insert(0, ButtonImage2);
  Browser.Controls.Insert(0, ButtonImage3);
  Browser.Controls.Insert(0, ButtonImage4);
  Browser.Controls.Insert(0, GLMenu1);

  { load button's images. Actually, load it only once, and use the same
    reference (leaving OwnsImage = false) on other buttons. }
  ButtonImageFixSize1.Image := LoadImage('sample_button_icon.png', [], []);
  ButtonImageFixSize1.OwnsImage := true;
  ButtonImageFixSize2.Image := ButtonImageFixSize1.Image;
  ButtonImageFixSize3.Image := ButtonImageFixSize1.Image;
  ButtonImageFixSize4.Image := ButtonImageFixSize1.Image;

  ButtonImage1.Image := ButtonImageFixSize1.Image;
  ButtonImage2.Image := ButtonImageFixSize1.Image;
  ButtonImage3.Image := ButtonImageFixSize1.Image;
  ButtonImage4.Image := ButtonImageFixSize1.Image;

  { Load images. We do not set FileName at design-time,
    as we want to use relative paths, which may be invalid at design-time
    (depends on Lazarus current dir). }
  SampleImage.FileName := 'sample_image.png';
  SampleImageAlpha.FileName := 'sample_image_with_alpha.png';

  GLMenu1.Position := Vector2Integer(400, 150);
end;

procedure TForm1.BrowserResize(Sender: TObject);
begin
  SampleButton.Width := Browser.Width - 20;
end;

procedure TForm1.GLMenu1Click(Sender: TObject);
begin
  ShowMessage(Format('Clicked menu item %d.', [GLMenu1.CurrentItem]));
end;

procedure TForm1.SampleButtonClick(Sender: TObject);
begin
  ShowMessage('Button clicked !');
end;

initialization
  {$I mainf.lrs}
end.

