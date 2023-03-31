{
  Copyright 2022-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main view, where most of the application logic takes place. }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleViewport, CastleScene;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    MainViewport: TCastleViewport;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleUtils, CastleImages;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
const
  { The calculation of ImagePosition below assumes that all images have
    the same width and height (you can play with Image.Size if you want to resize them),
    and that they should be arranged in isometric fashion -- that is,
    images in each row have alternating shift. }
  TileWidth = 70;
  TileHeight = 36;

  { URL of a random ground image. }
  function RandomGround: String;
  begin
    Result := 'castle-data:/map_objects/ground/soil_' + IntToStr(RandomIntRangeInclusive(1, 7)) + '_ta.png';
  end;

  { URL of a random tree image. }
  function RandomTree: String;
  begin
    case Random(2) of
      0: Result := 'castle-data:/map_objects/trees/kambi_birch_01_mature_autumn_thc.png';
      1: Result := 'castle-data:/map_objects/trees/kambi_birch_01_mature_winter_thc.png';
      else raise EInternalError.Create('Unexpected random result in RandomTree');
    end;
  end;

  { Return position of the middle of tile (X,Y).
    This position is in the coordinates inside MainViewport world,
    so you can use it to set TCastleTransform.Translation, like TCastleImageTransform.Translation. }
  function TilePosition(const X, Y: Integer): TVector2;
  begin
    { This is simplest and good for image layout as a simple grid. }
    //Result := Vector2(X * TileWidth, Y * TileHeight);

    { This makes isometric image layout. }
    Result := Vector2(
      X * TileWidth + Iff(Odd(Y), TileWidth / 2, 0),
      Y * TileHeight / 2);

    { Change the Result to point to the middle of the tile. }
    Result := Result + Vector2(TileWidth / 2, TileHeight / 2);
  end;

  { Create TCastleImageTransform and add it to the viewport.

    The image is loaded from given Url.

    The image is placed on tile X, Y. The middle of the tile will match the Pivot
    point of the image.

    The front-to-back order is determined by Z. Larger Z -> in front. }
  procedure AddImage(const Url: String; const X, Y: Integer; const Pivot: TVector2;
    const Z: Single);
  var
    Image: TCastleImageTransform;
  begin
    Image := TCastleImageTransform.Create(FreeAtStop);
    Image.Url := Url;
    Image.AlphaChannel := acTest;
    Image.Translation := Vector3(TilePosition(X, Y), Z);
    Image.Pivot := Pivot;

    { Using SmoothScaling=false as our sample images are small
      and seem to look better as pixel-art.

      But everything would also work with SmoothScaling=true.
      Note: We have applied on images alpha bleeding
      (see https://castle-engine.io/manual_alpha_bleeding.php)
      using castle-view-image.
      This means they have good transition between opaque and transparent pixels,
      which is necessary for SmoothScaling=true.

      In the end these images just seem better as pixel-art (SmoothScaling=false).
      But both options work -- it's a matter of visual taste at this point. }
    Image.SmoothScaling := false;

    MainViewport.Items.Add(Image);
  end;

const
  TilesCountX = 10;
  TilesCountY = 20;
var
  X, Y: Integer;
begin
  inherited;

  { Fill the map with random ground + occasional trees }
  for X := 0 to TilesCountX - 1 do
    for Y := 0 to TilesCountY - 1 do
    begin
      AddImage(RandomGround, X, Y, Vector2(0.5, 0.5), 0);
      if Random(10) = 0 then
        AddImage(RandomTree, X, Y, Vector2(0.5, 0.1), 1);
    end;

  { This call is only necessary if you use some transformations with blending.
    Our AddImage doesn't add such transformations (it always sets acTest),
    still we do this call in case you add other things to the viewport.
    See https://castle-engine.io/blending for explanation why is this necessary (for now). }
  MainViewport.Items.SortBackToFront2D;

  { Place camera such that it looks at the center tile initially }
  MainViewport.Camera.Translation := Vector3(
    TilePosition(TilesCountX div 2, TilesCountY div 2),
    100);
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { This virtual method is executed when user presses
    a key, a mouse button, or touches a touch-screen.

    Note that each UI control has also events like OnPress and OnClick.
    These events can be used to handle the "press", if it should do something
    specific when used in that UI control.
    The TViewMain.Press method should be used to handle keys
    not handled in children controls.
  }

  // Use this to handle keys:
  {
  if Event.IsKey(keyXxx) then
  begin
    // DoSomething;
    Exit(true); // key was handled
  end;
  }
end;

end.
