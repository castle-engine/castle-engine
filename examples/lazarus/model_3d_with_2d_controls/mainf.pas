{
  Copyright 2014-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main form. }
unit MainF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  CastleControl, CastleControls, CastleOnScreenMenu;

type
  TForm1 = class(TForm)
    Browser: TCastleControl;
    procedure FormCreate(Sender: TObject);
    procedure BrowserResize(Sender: TObject);
    procedure OnScreenMenu1Click(Sender: TObject);
    procedure SampleButtonClick(Sender: TObject);
  private
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
    Edit: TCastleEdit;
  end;

var
  Form1: TForm1;

implementation

uses CastleSceneCore, CastleUtils, CastleImages, CastleVectors,
  CastleControlsImages, CastleUIControls;

{ TForm1 --------------------------------------------------------------------- }

procedure TForm1.FormCreate(Sender: TObject);

  // TODO: Replace this code using CGE editor
  procedure AddUserInterface;
  begin
    SampleButton := TCastleButton.Create(Self);
    SampleButton.Bottom := 10;
    SampleButton.Left := 10;
    SampleButton.Width := 100;
    SampleButton.Height := 100;
    SampleButton.AutoSize := false;
    SampleButton.OnClick := @SampleButtonClick;
    SampleButton.Caption := 'Sample clickable button on GL area.';
    SampleButton.ImageLayout := ilTop;
    Browser.Controls.InsertFront(SampleButton);

    SampleImage := TCastleImageControl.Create(Self);
    SampleImage.Bottom := 150;
    SampleImage.Left := 20;
    { Load image. Using relative path. }
    SampleImage.URL := 'sample_image.png';
    Browser.Controls.InsertFront(SampleImage);

    SampleImageAlpha := TCastleImageControl.Create(Self);
    SampleImageAlpha.Bottom := 150;
    SampleImageAlpha.Left := 200;
    SampleImageAlpha.URL := 'sample_image_with_alpha.png';
    Browser.Controls.InsertFront(SampleImageAlpha);

    ButtonImageFixSize1 := TCastleButton.Create(Self);
    ButtonImageFixSize1.Bottom := 320;
    ButtonImageFixSize1.Tooltip := 'Sample tooltip over a button';
    ButtonImageFixSize1.Width := 240;
    ButtonImageFixSize1.Height := 100;
    ButtonImageFixSize1.AutoSize := false;
    ButtonImageFixSize1.Caption := 'Image top';
    ButtonImageFixSize1.ImageLayout := ilTop;
    { Load button image. Here we set OwnsImage = true, so the image
      will be freed when ButtonImageFixSize1 is freed.
      On other buttons we will use the same image reference,
      and other buttons will have OwnsImage = false. }
    ButtonImageFixSize1.Image := LoadImage('sample_button_icon.png');
    ButtonImageFixSize1.OwnsImage := true;
    Browser.Controls.InsertFront(ButtonImageFixSize1);

    ButtonImageFixSize2 := TCastleButton.Create(Self);
    ButtonImageFixSize2.Bottom := 320;
    ButtonImageFixSize2.Tooltip := 'Sample tooltip over a button';
    ButtonImageFixSize2.Width := 240;
    ButtonImageFixSize2.Height := 100;
    ButtonImageFixSize2.AutoSize := false;
    ButtonImageFixSize2.Caption := 'Image bottom';
    ButtonImageFixSize2.ImageLayout := ilBottom;
    ButtonImageFixSize2.Left := 250;
    ButtonImageFixSize2.Image := ButtonImageFixSize1.Image;
    Browser.Controls.InsertFront(ButtonImageFixSize2);

    ButtonImageFixSize3 := TCastleButton.Create(Self);
    ButtonImageFixSize3.Bottom := 320;
    ButtonImageFixSize3.Tooltip := 'Sample tooltip over a button';
    ButtonImageFixSize3.Width := 240;
    ButtonImageFixSize3.Height := 100;
    ButtonImageFixSize3.AutoSize := false;
    ButtonImageFixSize3.Caption := 'Image left';
    ButtonImageFixSize3.Left := 500;
    ButtonImageFixSize3.Image := ButtonImageFixSize1.Image;
    Browser.Controls.InsertFront(ButtonImageFixSize3);

    ButtonImageFixSize4 := TCastleButton.Create(Self);
    ButtonImageFixSize4.Bottom := 320;
    ButtonImageFixSize4.Tooltip := 'Sample tooltip over a button';
    ButtonImageFixSize4.Width := 240;
    ButtonImageFixSize4.Height := 100;
    ButtonImageFixSize4.AutoSize := false;
    ButtonImageFixSize4.Caption := 'Image right';
    ButtonImageFixSize4.ImageLayout := ilRight;
    ButtonImageFixSize4.Left := 750;
    ButtonImageFixSize4.Image := ButtonImageFixSize1.Image;
    Browser.Controls.InsertFront(ButtonImageFixSize4);

    ButtonImage1 := TCastleButton.Create(Self);
    ButtonImage1.Bottom := 420;
    ButtonImage1.Tooltip := 'Sample tooltip over a button';
    ButtonImage1.Width := 122;
    ButtonImage1.Height := 36;
    ButtonImage1.Caption := 'Image top';
    ButtonImage1.ImageLayout := ilTop;
    ButtonImage1.Image := ButtonImageFixSize1.Image;
    Browser.Controls.InsertFront(ButtonImage1);

    ButtonImage2 := TCastleButton.Create(Self);
    ButtonImage2.Bottom := 420;
    ButtonImage2.Tooltip := 'Sample tooltip over a button';
    ButtonImage2.Width := 162;
    ButtonImage2.Height := 36;
    ButtonImage2.Caption := 'Image bottom';
    ButtonImage2.ImageLayout := ilBottom;
    ButtonImage2.Left := 250;
    ButtonImage2.Image := ButtonImageFixSize1.Image;
    Browser.Controls.InsertFront(ButtonImage2);

    ButtonImage3 := TCastleButton.Create(Self);
    ButtonImage3.Bottom := 420;
    ButtonImage3.Tooltip := 'Sample tooltip over a button';
    ButtonImage3.Width := 122;
    ButtonImage3.Height := 36;
    ButtonImage3.Caption := 'Image left';
    ButtonImage3.Left := 500;
    ButtonImage3.Image := ButtonImageFixSize1.Image;
    Browser.Controls.InsertFront(ButtonImage3);

    ButtonImage4 := TCastleButton.Create(Self);
    ButtonImage4.Bottom := 420;
    ButtonImage4.Tooltip := 'Sample tooltip over a button';
    ButtonImage4.Width := 137;
    ButtonImage4.Height := 36;
    ButtonImage4.Caption := 'Image right';
    ButtonImage4.ImageLayout := ilRight;
    ButtonImage4.Left := 750;
    ButtonImage4.Image := ButtonImageFixSize1.Image;
    Browser.Controls.InsertFront(ButtonImage4);

    OnScreenMenu1 := TCastleOnScreenMenu.Create(Self);
    OnScreenMenu1.Bottom := 140;
    OnScreenMenu1.Left := 400;
    OnScreenMenu1.Add('one', @OnScreenMenu1Click);
    OnScreenMenu1.Add('two', @OnScreenMenu1Click);
    OnScreenMenu1.Add('three', @OnScreenMenu1Click);
    Browser.Controls.InsertFront(OnScreenMenu1);

    Edit := TCastleEdit.Create(Self);
    Edit.Anchor(vpTop, -10);
    Edit.Anchor(hpRight, -10);
    Edit.Width := 200;
    Browser.Controls.InsertFront(Edit);
  end;

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

  AddUserInterface;
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
