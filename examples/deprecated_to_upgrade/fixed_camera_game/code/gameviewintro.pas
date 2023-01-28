{
  Copyright 2007-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ View showing the intro images (TViewIntro). }
unit GameViewIntro;

interface

uses Classes, Generics.Collections,
  CastleGLImages, CastleUIControls, CastleTimeUtils, CastleKeysMouse,
  CastleControls;

type
  { View showing the intro images. }
  TViewIntro = class(TCastleView)
  strict private
    type
      TIntroPart = class
        CorrodeDuration: TFloatTime;
        Image: TDrawableImage;
        ImageCorroded: TDrawableImage;
        destructor Destroy; override;
      end;
      TIntroPartList = {$ifdef FPC}specialize{$endif} TObjectList<TIntroPart>;
    var
      IntroPart: Integer;
      IntroPartTime: Single;
      IntroParts: TIntroPartList;
      InfoLabel: TCastleLabel;
    procedure NextIntroPart;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; override;
    procedure Render; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
  end;

var
  ViewIntro: TViewIntro;

implementation

uses SysUtils, DOM,
  CastleFilesUtils, CastleVectors,
  CastleGLUtils, CastleImages, CastleSoundEngine,
  CastleStringUtils, CastleXMLUtils,
  CastleColors, CastleApplicationProperties, CastleRenderOptions,
  GameConfiguration, GameSound, GameViewMainMenu;

{ TViewIntro.TIntroPart ----------------------------------------------------- }

destructor TViewIntro.TIntroPart.Destroy;
begin
  FreeAndNil(Image);
  FreeAndNil(ImageCorroded);
  inherited;
end;

{ TViewIntro ---------------------------------------------------------------- }

constructor TViewIntro.Create(AOwner: TComponent);

  procedure InitializeIntroParts;
  var
    Element: TDOMElement;
    ImageName: string;
    I: TXMLElementIterator;
    Part: TIntroPart;
  begin
    IntroParts := TIntroPartList.Create(true);

    I := GameConfig.PathChildrenIterator('intro/parts', 'part');
    try
      while I.GetNext do
      begin
        Part := TIntroPart.Create;
        IntroParts.Add(Part);

        Element := I.Current;

        { calculate Part durations }
        Part.CorrodeDuration := Element.AttributeFloat('corrode_duration');

        { calculate Part.Image and ImageCorroded }
        ImageName := Element.AttributeString('image');
        Part.Image := TDrawableImage.Create(
          'castle-data:/images/' + ImageName + '.png', [TRGBImage]);
        Part.ImageCorroded := TDrawableImage.Create(
          'castle-data:/images/' + ImageName + '_corroded.png', [TRGBImage]);
      end;
    finally FreeAndNil(I) end;

    if IntroParts.Count = 0 then
      raise Exception.Create('No intro parts defined in data/index.xml');
  end;

begin
  inherited;
  InitializeIntroParts;
end;

destructor TViewIntro.Destroy;
begin
  FreeAndNil(IntroParts);
  inherited;
end;

procedure TViewIntro.Start;
begin
  inherited;
  IntroPart := 0;
  IntroPartTime := 0.0;

  SoundEngine.LoopingChannel[0].Sound := AllSounds.SoundIntroMusic;

  InfoLabel := TCastleLabel.Create(FreeAtStop);
  InfoLabel.Color := White;
  InfoLabel.Anchor(hpRight, -5);
  InfoLabel.Anchor(vpTop, -5);
  InsertFront(InfoLabel);
end;

procedure TViewIntro.NextIntroPart;
begin
  Inc(IntroPart);
  IntroPartTime := 0.0;

  if IntroPart >= IntroParts.Count then
    Container.View := ViewMainMenu;
end;

procedure TViewIntro.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  IntroPartTime := IntroPartTime + SecondsPassed;
  InfoLabel.Caption := Format('(%d / %d) Press [Space] or click / tap to continue',
    [IntroPart + 1, IntroParts.Count]);
end;

procedure TViewIntro.Render;
var
  Corrosion: Single;
begin
  inherited;

  if IntroPartTime >= IntroParts[IntroPart].CorrodeDuration then
  begin
    IntroParts[IntroPart].ImageCorroded.Draw(Container.Rect);
  end else
  begin
    Corrosion := IntroPartTime / IntroParts[IntroPart].CorrodeDuration;

    IntroParts[IntroPart].Image.Alpha := acBlending;
    IntroParts[IntroPart].Image.BlendingSourceFactor := bsConstantColor;
    IntroParts[IntroPart].Image.BlendingDestinationFactor := bdOne;
    IntroParts[IntroPart].Image.BlendingConstantColor :=
      Vector4(1 - Corrosion, 1 - Corrosion, 1 - Corrosion, 1);
    IntroParts[IntroPart].Image.Draw(Container.Rect);

    IntroParts[IntroPart].ImageCorroded.Alpha := acBlending;
    IntroParts[IntroPart].ImageCorroded.BlendingSourceFactor := bsConstantColor;
    IntroParts[IntroPart].ImageCorroded.BlendingDestinationFactor := bdOne;
    IntroParts[IntroPart].ImageCorroded.BlendingConstantColor :=
      Vector4(Corrosion, Corrosion, Corrosion, 1);
    IntroParts[IntroPart].ImageCorroded.Draw(Container.Rect);
  end;
end;

function TViewIntro.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;

  if Event.IsKey(CharEscape) then
  begin
    Container.View := ViewMainMenu;
    Result := true;
  end else
  if Event.IsMouseButton(buttonLeft) or Event.IsKey(keySpace) then
  begin
    NextIntroPart;
    Result := true;
  end;
end;

end.
