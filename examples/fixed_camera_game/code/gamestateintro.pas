{
  Copyright 2007-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ State showing the intro images (TStateIntro). }
unit GameStateIntro;

interface

uses Classes, Generics.Collections,
  CastleGLImages, CastleUIState, CastleTimeUtils, CastleKeysMouse,
  CastleControls;

type
  { State showing the intro images. }
  TStateIntro = class(TUIState)
  strict private
    type
      TIntroPart = class
        CorrodeDuration: TFloatTime;
        Image: TGLImage;
        ImageCorroded: TGLImage;
        destructor Destroy; override;
      end;
      TIntroPartList = specialize TObjectList<TIntroPart>;
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
  StateIntro: TStateIntro;

implementation

uses SysUtils, DOM,
  CastleFilesUtils, CastleVectors,
  CastleGLUtils, CastleImages, CastleSoundEngine,
  CastleUIControls, CastleStringUtils, CastleXMLUtils,
  CastleColors, CastleApplicationProperties,
  GameConfiguration, GameSound, GameStateMainMenu;

{ TStateIntro.TIntroPart ----------------------------------------------------- }

destructor TStateIntro.TIntroPart.Destroy;
begin
  FreeAndNil(Image);
  FreeAndNil(ImageCorroded);
  inherited;
end;

{ TStateIntro ---------------------------------------------------------------- }

constructor TStateIntro.Create(AOwner: TComponent);

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
        Part.Image := TGLImage.Create(
          ApplicationData('images/' + ImageName + '.png'), [TRGBImage]);
        Part.ImageCorroded := TGLImage.Create(
          ApplicationData('images/' + ImageName + '_corroded.png'), [TRGBImage]);
      end;
    finally FreeAndNil(I) end;

    if IntroParts.Count = 0 then
      raise Exception.Create('No intro parts defined in data/index.xml');
  end;

begin
  inherited;
  InitializeIntroParts;
end;

destructor TStateIntro.Destroy;
begin
  FreeAndNil(IntroParts);
  inherited;
end;

procedure TStateIntro.Start;
begin
  inherited;
  IntroPart := 0;
  IntroPartTime := 0.0;

  SoundEngine.MusicPlayer.Sound := stIntroMusic;

  InfoLabel := TCastleLabel.Create(FreeAtStop);
  InfoLabel.Color := White;
  InfoLabel.Anchor(hpRight, -5);
  InfoLabel.Anchor(vpTop, -5);
  InsertFront(InfoLabel);
end;

procedure TStateIntro.NextIntroPart;
begin
  Inc(IntroPart);
  IntroPartTime := 0.0;

  if IntroPart >= IntroParts.Count then
    TUIState.Current := StateMainMenu;
end;

procedure TStateIntro.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  IntroPartTime := IntroPartTime + SecondsPassed;
  InfoLabel.Caption := Format('(%d / %d) Press [Space] or click / tap to continue',
    [IntroPart + 1, IntroParts.Count]);
end;

procedure TStateIntro.Render;
var
  Corrosion: Single;
begin
  inherited;

  RenderContext.Clear([cbColor], Black);

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

function TStateIntro.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;

  if Event.IsKey(CharEscape) then
  begin
    TUIState.Current := StateMainMenu;
    Result := true;
  end else
  if Event.IsMouseButton(mbLeft) or Event.IsKey(K_Space) then
  begin
    NextIntroPart;
    Result := true;
  end;
end;

end.
