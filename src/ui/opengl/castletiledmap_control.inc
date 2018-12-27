{
  Copyright 2015-2018 Tomasz Wojtyś, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Display a 2D tiled map (TMX) (TCastleTiledMapControl class). }

{$ifdef read_interface}

type
  { Display a 2D tiled map (TMX). }
  TCastleTiledMapControl = class(TCastleUserInterface)
  strict private
    FTiledMap: TTiledMap;
    FURL: String;
    FObjectsVisible: Boolean;
    FOrigin: TVector2;
    FScale: Single;
    FSmoothScaling: Boolean;
    procedure RenderObjects;
    procedure LoadTilesetsImages;
    { Free images created by LoadTilesetsImages. }
    procedure FreeTilesetsImages;
    { Free everything created by SetURL. }
    procedure Close;
    procedure SetURL(const Value: string);
    procedure SetOrigin(const Value: TVector2);
    procedure SetScale(const Value: Single);
    procedure SetSmoothScaling(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;

    { Load given TMX file to memory and creates structures. }
    procedure LoadFromFile(const AURL: string); deprecated 'set URL instead of calling LoadFromFile';

    { Origin determines the map position at control's left-bottom corner.
      Increasing Origin.X moves visually the map to the left,
      increasing Origin.Y moves it down. }
    property Origin: TVector2 read FOrigin write SetOrigin;
  published
    property ClipChildren default true;

    { URL (or just filename) of the Tiled map file (TMX). }
    property URL: String read FURL write SetURL;

    property Scale: Single read FScale write SetScale default 1;

    // TODO: not implemented yet
    // property ObjectsVisible: Boolean read FObjectsVisible write FObjectsVisible;

    { How are images scaled (because of UI scaling, because of @link(Scale)).
      @true means we use nice "bilinear" filter.
      @false means we use pixelated "nearest" filter. }
    property SmoothScaling: Boolean read FSmoothScaling write SetSmoothScaling
      default true;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastletiledmapcontrol_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

{$endif read_interface}

{$ifdef read_implementation}

constructor TCastleTiledMapControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ClipChildren := true;
  FObjectsVisible := false;
  FScale := 1;
  FSmoothScaling := true;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastletiledmapcontrol_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleTiledMapControl.Destroy;
begin
  Close;

  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastletiledmapcontrol_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

procedure TCastleTiledMapControl.RenderObjects;
var
  Lay: TTiledMap.TLayer;
begin
  { Object groups. }
  for Lay in FTiledMap.Layers do
  begin
    // if Lay is TTiledMap.TObjectGroupLayer then
    // TODO: draw TObjectGroupLayer here
    // TODO: when to draw TImageLayer?
  end;
end;

procedure TCastleTiledMapControl.LoadTilesetsImages;
var
  Tileset: TTiledMap.TTileset;
  Spr: TSprite;
begin
  if FTiledMap <> nil then
    for Tileset in FTiledMap.Tilesets do
    begin
      //todo: use transparent color (implement in TSprite?)
      if Tileset.Columns = 0 then
        Tileset.Columns := Tileset.Image.Width div Tileset.TileWidth;
      if Tileset.TileCount = 0 then
        Tileset.TileCount := (Tileset.Image.Height div Tileset.TileHeight) * Tileset.Columns;
      if (Tileset.TileCount = 0) or (Tileset.Columns = 0) then
      begin
        WriteLnWarning('Empty Tileset');
        Continue;
      end;
      Spr := TSprite.Create(
        Tileset.Image.URL, Tileset.TileCount,
        Tileset.Columns, Tileset.TileCount div Tileset.Columns,
        SmoothScaling, false);
      { Note that we use here Tileset.TileWidth/Height,
        which may difer from FTiledMap.TileWidth/Height.
        This is important in case of e.g. "perspective_walls.tmx" test
        from https://github.com/bjorn/tiled/tree/master/examples . }
      Spr.DrawingWidth := Tileset.TileWidth;
      Spr.DrawingHeight := Tileset.TileHeight;
      Spr.FrameWidth := Tileset.TileWidth;
      Spr.FrameHeight := Tileset.TileHeight;
      Spr.LeftMargin := Tileset.Margin;
      Spr.TopMargin := Tileset.Margin;
      Spr.HorizontalSpacing := Tileset.Spacing;
      Spr.VerticalSpacing := Tileset.Spacing;
      Spr.VerticalSpacingBottom := true;
      // Necessary when TiledMap is scaled (because of Scale or UIScaling)
      Spr.RenderingFrameSafeMargin := true;
      Tileset.RendererData := Spr;
    end;
end;

procedure TCastleTiledMapControl.FreeTilesetsImages;
var
  Tileset: TTiledMap.TTileset;
begin
  if FTiledMap <> nil then
    for Tileset in FTiledMap.Tilesets do
      FreeAndNil(Tileset.RendererData);
end;

procedure TCastleTiledMapControl.Close;
begin
  FreeTilesetsImages;
  FreeAndNil(FTiledMap);
end;

procedure TCastleTiledMapControl.LoadFromFile(const AURL: string);
begin
  URL := AURL;
end;

procedure TCastleTiledMapControl.SetURL(const Value: string);
begin
  if FURL <> Value then
  begin
    FURL := Value;
    Close;
    if Value <> '' then
      FTiledMap := TTiledMap.Create(Value);
    LoadTilesetsImages;
  end;
end;

procedure TCastleTiledMapControl.Render;
var
  RR: TFloatRectangle;

  { Return TSprite to render this tile,
    with sprite's Frame, HorizontalFlip, VerticalFlip, DiagonalFlip already set.
    May return @nil if nothing to render. }
  function TileSprite(const Layer: TTiledMap.TLayer; const X, Y: Integer): TSprite;
  var
    Index: Integer;
    GID, Dat: Cardinal;
    Tileset: TTiledMap.TTileset;
  const
    HorizontalFlag = $80000000;
    VerticalFlag   = $40000000;
    DiagonalFlag   = $20000000;
    ClearFlag      = $1FFFFFFF;
  begin
    Result := nil;

    Index := X + (FTiledMap.Height - 1 - Y) * FTiledMap.Width;
    Dat := Layer.Data.Data[Index];
    GID := Dat and ClearFlag;
    if GID = 0 then Exit;

    Tileset := FTiledMap.GIDToTileset(GID);
    Result := Tileset.RendererData as TSprite;
    if Result = nil then Exit;

    Result.Frame := GID - Tileset.FirstGID;
    Result.HorizontalFlip := Dat and HorizontalFlag > 0;
    Result.VerticalFlip := Dat and VerticalFlag > 0;
    // TODO: test flipping diagonally
    Result.DiagonalFlip := Dat and DiagonalFlag > 0;
  end;

  procedure RenderTile(const Layer: TTiledMap.TLayer; const X, Y: Integer;
    const RenderX, RenderY: Single);
  var
    Spr: TSprite;
    SpriteScreenRect: TFloatRectangle;
  begin
    Spr := TileSprite(Layer, X, Y);
    if Spr = nil then Exit;

    SpriteScreenRect := FloatRectangle(
      RenderX,
      RenderY,
      Spr.DrawingWidth,
      Spr.DrawingHeight
    ).
    // Apply Origin and Scale
    Translate(-Origin).
    ScaleAround0(Scale).
    // Apply UIScale and RenderRect (anchors etc.)
    // to honor standard TCastleUserInterface conventions
    ScaleAround0(UIScale).
    Translate(RR.LeftBottom);

    // TODO: use Layer opacity
    // TODO: use Layer offset

    Spr.Draw(SpriteScreenRect);
  end;

  procedure RenderOrthogonal;
  var
    X, Y: Integer;
    Layer: TTiledMap.TLayer;
  begin
    for Layer in FTiledMap.Layers do
    begin
      if not Layer.Visible then Continue;
      if (Layer is TTiledMap.TObjectGroupLayer) or
         (Layer is TTiledMap.TImageLayer) then
        Continue;

      // TODO: use map render order

      for Y := FTiledMap.Height - 1 downto 0 do
        for X := 0 to FTiledMap.Width - 1 do
          RenderTile(Layer, X, Y,
            X * FTiledMap.TileWidth,
            Y * FTiledMap.TileHeight);
    end;
  end;

  procedure RenderIsometric;
  var
    X, Y, YFromTop: Integer;
    Layer: TTiledMap.TLayer;
  begin
    for Layer in FTiledMap.Layers do
    begin
      if not Layer.Visible then Continue;
      if (Layer is TTiledMap.TObjectGroupLayer) or
         (Layer is TTiledMap.TImageLayer) then
        Continue;

      // TODO: use map render order

      for Y := FTiledMap.Height - 1 downto 0 do
        for X := 0 to FTiledMap.Width - 1 do
        begin
          YFromTop := FTiledMap.Height - 1 - Y;
          RenderTile(Layer, X, Y,
            (FTiledMap.Width  - 1 + X - YFromTop) * FTiledMap.TileWidth  / 2,
            (FTiledMap.Height - 2 + Y - X       ) * FTiledMap.TileHeight / 2);
        end;
    end;
  end;

  procedure RenderIsometricStaggered;
  var
    X, Y: Integer;
    Layer: TTiledMap.TLayer;
    RenderX, RenderY: Single;
  begin
    for Layer in FTiledMap.Layers do
    begin
      if not Layer.Visible then Continue;
      if (Layer is TTiledMap.TObjectGroupLayer) or
         (Layer is TTiledMap.TImageLayer) then
        Continue;

      // TODO: use map render order
      // TODO: right now this assumes Stagger Axis = Y

      for Y := FTiledMap.Height - 1 downto 0 do
        for X := 0 to FTiledMap.Width - 1 do
        begin
          RenderX := X * FTiledMap.TileWidth;
          if (not Odd(Y)) xor (FTiledMap.StaggerIndex <> siOdd) then
            RenderX += FTiledMap.TileWidth / 2;
          RenderY := (Y - 1) * FTiledMap.TileHeight / 2;
          RenderTile(Layer, X, Y, RenderX, RenderY);
        end;
    end;
  end;

  procedure RenderHexagonal;
  var
    X, Y: Integer;
    Layer: TTiledMap.TLayer;
    RenderX, RenderY, RowIncreaseY: Single;
  begin
    for Layer in FTiledMap.Layers do
    begin
      if not Layer.Visible then Continue;
      if (Layer is TTiledMap.TObjectGroupLayer) or
         (Layer is TTiledMap.TImageLayer) then
        Continue;

      // TODO: use map render order
      // TODO: right now this assumes Stagger Axis = Y

      RowIncreaseY := FTiledMap.TileHeight -
        (FTiledMap.TileHeight - FTiledMap.HexSideLength) / 2;

      for Y := FTiledMap.Height - 1 downto 0 do
        for X := 0 to FTiledMap.Width - 1 do
        begin
          RenderX := X * FTiledMap.TileWidth;
          if (not Odd(Y)) xor (FTiledMap.StaggerIndex <> siOdd) then
            RenderX += FTiledMap.TileWidth / 2;
          RenderY := Y * RowIncreaseY;
          RenderTile(Layer, X, Y, RenderX, RenderY);
        end;
    end;
  end;

begin
  inherited Render;
  if FTiledMap = nil then Exit;

  RR := RenderRect;

  if FTiledMap.BackgroundColor[3] <> 0 then
    DrawRectangle(RR, FTiledMap.BackgroundColor);

  case FTiledMap.Orientation of
    moOrthogonal: RenderOrthogonal;
    moIsometric: RenderIsometric;
    moIsometricStaggered: RenderIsometricStaggered;
    moHexagonal: RenderHexagonal;
    // As a fallback, render unsupported modes as orthogonal
    else RenderOrthogonal;
  end;

  if FObjectsVisible then RenderObjects;
end;

procedure TCastleTiledMapControl.SetOrigin(const Value: TVector2);
begin
  if not TVector2.PerfectlyEquals(FOrigin, Value) then
  begin
    FOrigin := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleTiledMapControl.SetScale(const Value: Single);
begin
  if FScale <> Value then
  begin
    FScale := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleTiledMapControl.SetSmoothScaling(const Value: Boolean);

  procedure ChangeExistingImages;
  var
    Tileset: TTiledMap.TTileset;
  begin
    if FTiledMap <> nil then
      for Tileset in FTiledMap.Tilesets do
        if Tileset.RendererData <> nil then
          (Tileset.RendererData as TSprite).SmoothScaling := SmoothScaling;
  end;

begin
  if FSmoothScaling <> Value then
  begin
    FSmoothScaling := Value;
    ChangeExistingImages;
    VisibleChange([chRectangle]);
  end;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastletiledmapcontrol_persistent_vectors.inc}
{$undef read_implementation_methods}

{$endif read_implementation}
