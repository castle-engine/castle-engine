unit MainF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  CastleControl, CastleControls, CastleOnScreenMenu;

type
  TForm1 = class(TForm)
    Browser: TCastleControl;
    OnScreenMenu1: TCastleOnScreenMenu;
    SampleButton: TCastleButton;
    SampleImage: TCastleImageControl;
    SampleImageAlpha: TCastleImageControl;
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
    procedure OnScreenMenu1Click(Sender: TObject);
    procedure SampleButtonClick(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

uses CastleSceneCore, CastleUtils, CastleImages, CastleVectors,
  CastleControlsImages;

{ TForm1 --------------------------------------------------------------------- }

procedure TForm1.FormCreate(Sender: TObject);
begin
  { For simplicity, we just load 3D model from hardcoded path. }
  Browser.Load('../../3d_rendering_processing/data/bridge_final.x3dv');
  Browser.MainScene.Spatial := [ssRendering, ssDynamicCollisions];
  Browser.MainScene.ProcessEvents := true;

  { We can customize the look of all standard 2D controls by changing Theme.
    Below we change the tooltip image to have rounded corners,
    using a predefined TooltipRounded image from CastleControlsImages
    unit. You could also use load and use your own image,
    e.g. by LoadImage. }
  Theme.Images[tiTooltip] := TooltipRounded;
  Theme.Corners[tiTooltip] := Vector4Integer(9, 9, 9, 9);

  { Thanks to using this button as a TUIControl descendant
    (placing it on Browser.Controls list), VRML scene sensors
    (like TouchSensor) will not intercept mouse clicks. That is, button
    obscures anything clickable on VRML scene (like TouchSensor) underneath.

    (If you would like to change this, you can set SampleButton.ExlusiveEvents
    to false.)

    (Images (TCastleImageControl) actually also obscure scene underneath, but since
    they do not handle any keys or mouse by default, they let themn through
    to 3d scene. This could be changed by overriding their
    TCastleImageControl.Press etc.) }

  Browser.Controls.InsertFront(SampleButton);
  Browser.Controls.InsertFront(SampleImage);
  Browser.Controls.InsertFront(SampleImageAlpha);
  Browser.Controls.InsertFront(ButtonImageFixSize1);
  Browser.Controls.InsertFront(ButtonImageFixSize2);
  Browser.Controls.InsertFront(ButtonImageFixSize3);
  Browser.Controls.InsertFront(ButtonImageFixSize4);
  Browser.Controls.InsertFront(ButtonImage1);
  Browser.Controls.InsertFront(ButtonImage2);
  Browser.Controls.InsertFront(ButtonImage3);
  Browser.Controls.InsertFront(ButtonImage4);
  Browser.Controls.InsertFront(OnScreenMenu1);

  { load button's images. Actually, load it only once, and use the same
    reference (leaving OwnsImage = false) on other buttons. }
  ButtonImageFixSize1.Image := LoadImage('sample_button_icon.png');
  ButtonImageFixSize1.OwnsImage := true;
  ButtonImageFixSize2.Image := ButtonImageFixSize1.Image;
  ButtonImageFixSize3.Image := ButtonImageFixSize1.Image;
  ButtonImageFixSize4.Image := ButtonImageFixSize1.Image;

  ButtonImage1.Image := ButtonImageFixSize1.Image;
  ButtonImage2.Image := ButtonImageFixSize1.Image;
  ButtonImage3.Image := ButtonImageFixSize1.Image;
  ButtonImage4.Image := ButtonImageFixSize1.Image;

  { Load images. We do not set URL at design-time,
    as we want to use relative paths, which may be invalid at design-time
    (depends on Lazarus current dir). }
  SampleImage.URL := 'sample_image.png';
  SampleImageAlpha.URL := 'sample_image_with_alpha.png';

  OnScreenMenu1.Add('one');
  OnScreenMenu1.Add('two');
  OnScreenMenu1.Add('three');
end;

procedure TForm1.BrowserResize(Sender: TObject);
begin
  SampleButton.Width := Browser.Width - 20;
end;

procedure TForm1.OnScreenMenu1Click(Sender: TObject);
begin
  ShowMessage(Format('Clicked menu item %d.', [OnScreenMenu1.CurrentItem]));
end;

procedure TForm1.SampleButtonClick(Sender: TObject);
begin
  ShowMessage('Button clicked !');
end;

initialization
  {$I mainf.lrs}
end.

