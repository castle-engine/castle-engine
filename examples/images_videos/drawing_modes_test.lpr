{
  Copyright 2016-2017 Eugene Loza, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test drawing one TCastleImage on another (using TCastleImage.DrawFrom)
  with various blending modes. }
program drawing_modes_test;

{$Apptype GUI}

uses Classes, SysUtils, TypInfo,
  CastleWindow, CastleImages, CastleFilesUtils, CastleControls, CastleUIControls,
  CastleColors, CastleStringUtils;

type
  TSetOfImages = class(TComponent)
  public
    data: array[0..4, 0..4] of TCastleImageControl;
    procedure Reset(const DrawMode: TDrawMode);
    procedure ChangeModeClick(Sender: TObject);
  end;

var
  Window: TCastleWindowCustom;

  RGBA1, RGBA2: TCastleImage;
  GA1, GA2: TCastleImage;
  RGB1, RGB2: TCastleImage;
  G1, G2: TCastleImage;
  Legend, NotApplicable: TCastleImage;

  SetOfImages: TSetOfImages;

{$R+}{$Q+}

procedure TSetOfImages.Reset(const DrawMode: TDrawMode);
var
  i, j: integer;
  source, dest: TCastleImage;
begin
  for i := 0 to 4 do
    for j := 0 to 4 do
    begin
      { In case this method is called for the 2nd time
        (because the button to change drawing mode was pressed),
        we need to free the previous TCastleImageControl.
        Otherwise, it would remain visible in Window.Controls
        (there would be no memory leak though, as it's owned by Self). }
      FreeAndNil(data[i,j]);
      data[i,j] := TCastleImageControl.Create(Self);
    end;

  data[0,0].image := Legend; data[0,0].OwnsImage := false;
  data[1,0].image := RGBA1;  data[1,0].OwnsImage := false;
  data[2,0].image := GA1;    data[2,0].OwnsImage := false;
  data[3,0].image := RGB1;   data[3,0].OwnsImage := false;
  data[4,0].image := G1;     data[4,0].OwnsImage := false;
  data[0,1].image := RGBA2;  data[0,1].OwnsImage := false;
  data[0,2].image := GA2;    data[0,2].OwnsImage := false;
  data[0,3].image := RGB2;   data[0,3].OwnsImage := false;
  data[0,4].image := G2;     data[0,4].OwnsImage := false;

  for i := 1 to 4 do
    for j := 1 to 4 do
    begin
      try
        data[i,j].image := data[i,0].image.MakeCopy;
        Dest := data[i,j].image;
        Source := data[0,j].image;
        Dest.DrawFrom(Source, 0, 0, 0, 0, Source.width, Source.height, DrawMode);
      except
        data[i,j].image := NotApplicable;
        { We will free NotApplicable image at the end of program, this is better than
          relying on TCastleImageControl.OwnsImage mechanism,
          as it will correctly free it regardless if it's used here or not. }
        data[i,j].OwnsImage := false;
      end;
    end;

  for i := 0 to 4 do
    for j := 0 to 4 do
    begin
      data[i,j].Anchor(vpTop, -data[i,j].image.height * j);
      data[i,j].Anchor(hpLeft, data[i,j].image.width * i);
      { InsertBack, not InsertFront, to be behind the labels and buttons. }
      Window.Controls.InsertBack(data[i,j]);
    end;
end;

procedure TSetOfImages.ChangeModeClick(Sender: TObject);
begin
  Reset(TDrawMode((Sender as TCastleButton).Tag));
end;

const
  ImageSize = 128;

procedure AddLabels(const Horizontal: boolean);
var
  Labels: array [0..3] of TCastleLabel;
  I: Integer;
begin
  for I := 0 to 3 do
  begin
    Labels[I] := TCastleLabel.Create(Application);
    Labels[I].Color := White;
    Labels[I].Alignment := hpMiddle;
    Labels[I].Outline := 1;
    Labels[I].OutlineColor := Black;
    if Horizontal then
    begin
      Labels[I].Anchor(hpMiddle, hpLeft, ImageSize * (I + 1) + ImageSize div 2);
      Labels[I].Anchor(vpMiddle, vpTop, - ImageSize div 2);
    end else
    begin
      Labels[I].Anchor(hpMiddle, hpLeft, ImageSize div 2);
      Labels[I].Anchor(vpMiddle, vpTop, - ImageSize * (I + 1) - ImageSize div 2);
    end;
    Window.Controls.InsertFront(Labels[I]);
  end;
  Labels[0].Caption := 'RGB' + LineEnding + '+ Alpha';
  Labels[1].Caption := 'Grayscale' + LineEnding + '+ Alpha';
  Labels[2].Caption := 'RGB';
  Labels[3].Caption := 'Grayscale';
end;

procedure AddButtons;
var
  DrawMode: TDrawMode;
  DrawModeName: string;
  B: TCastleButton;
begin
  for DrawMode := Low(DrawMode) to High(DrawMode) do
  begin
    B := TCastleButton.Create(Application);
    { convert enum to string using GetEnumName }
    DrawModeName := SEnding(GetEnumName(TypeInfo(TDrawMode), Ord(DrawMode)), 3);
    B.Caption := DrawModeName;
    B.AutoSizeWidth := false;
    B.Width := 200;
    B.Anchor(hpRight, -20);
    B.Anchor(vpBottom, 20 + Ord(DrawMode) * 50);
    B.Tag := Ord(DrawMode); // pass DrawMode to ChangeModeClick in the Tag
    B.OnClick := @SetOfImages.ChangeModeClick;
    Window.Controls.InsertFront(B);
  end;
end;

begin
  Window := TCastleWindow.Create(Application);

  RGBA1 := LoadImage(ApplicationData('1RGBA.png'), [TRGBAlphaImage]) as TRGBAlphaImage;
  RGBA2 := LoadImage(ApplicationData('2RGBA.png'), [TRGBAlphaImage]) as TRGBAlphaImage;
  GA1 := LoadImage(ApplicationData('1GA.png'), [TGrayscaleAlphaImage]) as TGrayscaleAlphaImage;
  GA2 := LoadImage(ApplicationData('2GA.png'), [TGrayscaleAlphaImage]) as TGrayscaleAlphaImage;
  RGB1 := LoadImage(ApplicationData('1RGB.png'), [TRGBImage]) as TRGBImage;
  RGB2 := LoadImage(ApplicationData('2RGB.png'), [TRGBImage]) as TRGBImage;
  G1 := LoadImage(ApplicationData('1G.png'), [TGrayscaleImage]) as TGrayscaleImage;
  G2 := LoadImage(ApplicationData('2G.png'), [TGrayscaleImage]) as TGrayscaleImage;
  Legend := LoadImage(ApplicationData('Legend.png'), [TGrayscaleImage]) as TGrayscaleImage;
  NotApplicable := LoadImage(ApplicationData('na.png'), [TGrayscaleImage]) as TGrayscaleImage;

  Window.Height := ImageSize * 5;
  Window.Width := ImageSize * 5 + 240;
  SetOfImages := TSetOfImages.Create(Application);
  SetOfImages.Reset(dmBlend);

  AddLabels(false);
  AddLabels(true);
  AddButtons;

  Window.Open;
  Application.Run;

  FreeAndNil(RGBA1);
  FreeAndNil(RGBA2);
  FreeAndNil(GA1);
  FreeAndNil(GA2);
  FreeAndNil(RGB1);
  FreeAndNil(RGB2);
  FreeAndNil(G1);
  FreeAndNil(G2);
  FreeAndNil(Legend);
  FreeAndNil(NotApplicable);
end.
