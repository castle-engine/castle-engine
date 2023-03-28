{%MainUnit castletiledmap.pas}
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

{ Display a map created in Tiled (https://www.mapeditor.org/) (TCastleTiledMapControl class). }

{$ifdef read_interface}

type
  { Display a map created in Tiled (https://www.mapeditor.org/). }
  TCastleTiledMapControl = class(TCastleUserInterface)
  strict private
    FMap: TCastleTiledMapData;
    FURL: String;
    FObjectsVisible: Boolean;
    FOrigin: TVector2;
    FScale: Single;
    FSmoothScaling, FSmoothScalingSafeBorder: Boolean;
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
    procedure SetSmoothScalingSafeBorder(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Load given TMX file to memory and creates structures. }
    procedure LoadFromFile(const AURL: string); deprecated 'set URL instead of calling LoadFromFile';

    { Origin determines the map position at control's left-bottom corner.
      Increasing Origin.X moves visually the map to the left,
      increasing Origin.Y moves it down. }
    property Origin: TVector2 read FOrigin write SetOrigin;

    { Currently loaded map (from @link(URL)), if any.
      @nil if no map is loaded (when @link(URL) is empty). Read-only. }
    property Map: TCastleTiledMapData read FMap;

    { Detect tile under given position.

      The interpretation of Position depends on ContainerCoordinates,
      and is similar to e.g. @link(TCastleViewport.PositionToCameraPlane):

      @unorderedList(
        @item(When ContainerCoordinates = @true,
          then Position is relative to the whole container
          (like TCastleWindow or TCastleControl).

          And it is expressed in real device coordinates,
          just like @link(TInputPressRelease.Position)
          when mouse is being clicked, or like @link(TInputMotion.Position)
          when mouse is moved.
        )

        @item(When ContainerCoordinates = @false,
          then Position is relative to this UI control.

          And it is expressed in coordinates after UI scaling.
          IOW, if the size of this control is @link(Width) = 100,
          then Position.X between 0 and 100 reflects the visible range of this control.
        )
      )

      Note that you can query the positions that are outside of this UI control,
      or even outside of the container.
      They will return correct map tiles, as if the map was rendered there.
      If you want to avoid this, you can e.g. check whether Position
      is inside the @link(RenderRect) (if it's a position in container coordinates,
      with ContainerCoordinates = @true).

      This method returns @false if the position is outside of the map.
      Valid map tiles are from (0, 0) (lower-left) to
      (@link(TCastleTiledMapData.Width) - 1, @link(TCastleTiledMapData.Height) - 1) (upper-right). }
    function PositionToTile(const Position: TVector2; const ContainerCoordinates: Boolean;
      out TilePosition: TVector2Integer): Boolean;

    { Rectangle where the given map tile is rendered.
      Useful to render something over a TCastleTiledMapControl,
      e.g. a pawn in a game.

      In some way, this method is an inverse of the @link(PositionToTile) method.
      It converts map (tile) coordinates back into coordinates useful to display
      something.
      The ContainerCoordinates value determines the coordinate system of the resulting
      rectangle. It's meaning is similar to the ContainerCoordinates parameter
      of the @link(PositionToTile) method.

      @unorderedList(
        @item(When ContainerCoordinates = @true,
          the resulting rectangle is relative to the whole container
          (like TCastleWindow or TCastleControl).
          And is expressed in real device coordinates.

          These are the same coordinates as expressed by @link(RenderRect)
          method. You can pass the resulting rectangle straight
          to @link(DrawRectangle), or @link(TDrawableImage.Draw), or @link(TSprite.Draw),
          to draw something over this tile.
        )

        @item(When ContainerCoordinates = @false,
          the resulting rectangle is specified in local coordinates of this
          control, and is expressed in coordinates after UI scaling.

          These are the same coordinates as used by children controls (TCastleUserInterface)
          or the TCastleTiledMapControl.
          E.g. you can insert a TCastleImageControl as a child of
          TCastleTiledMapControl, and set it's position/size this way,
          to draw an image over this tile.
        )
      )

      This method works for any TilePosition, even outside of valid map positions.
      Returns TFloatRectangle.Empty is @link(Map) not loaded (@link(URL) not set).

      Some maps may use tilesets in which image size is different
      than map tile size. That is, TCastleTiledMapData.TTileset.TileWidth may be different
      than TCastleTiledMapData.TileWidth for some tilesets.
      The simpler version of this method,
      without ForceTileWidth/Height, just returns the size of the map tile looking at
      (TCastleTiledMapData.TileWidth, TCastleTiledMapData.TileHeight), since it doesn't know
      the tileset you use.

      You can pass ForceTileWidth and ForceTileHeight to set particular size.
    }
    function TileRectangle(const TilePosition: TVector2Integer;
      const ContainerCoordinates: Boolean): TFloatRectangle; overload;
    function TileRectangle(const TilePosition: TVector2Integer;
      const ContainerCoordinates: Boolean;
      const ForceTileWidth, ForceTileHeight: Single): TFloatRectangle; overload;
  published
    property ClipChildren default true;

    { URL (or just filename) of the Tiled map file (TMX). }
    property URL: String read FURL write SetURL;

    property Scale: Single read FScale write SetScale {$ifdef FPC}default 1{$endif};

    { Show objects like polylines and points on the map. }
    property ObjectsVisible: Boolean read FObjectsVisible write FObjectsVisible default true;

    { How are images scaled (because of UI scaling, because of @link(Scale)).
      @true means we use nice "bilinear" filter.
      @false means we use pixelated "nearest" filter.

      Beware that using @true, and scaling the map, requires preparing
      the tileset (atlas of tile images) in a proper way. The space
      around each tile must have at least 1-pixel margin
      filled with the same color and alpha ("color bleeding"),
      otherwise bad colors will creep in because of bilinear filtering.
      Alternatively, you can use @link(SmoothScalingSafeBorder).
    }
    property SmoothScaling: Boolean read FSmoothScaling write SetSmoothScaling
      default false;

    { In case of @link(SmoothScaling),
      prevent the color outside of rendered frame from creeping in,
      by stretching the rendered frame a bit.
      See @link(TSprite.SmoothScalingSafeBorder).

      It is better to prepare the sprite (tileset) images
      with some spacing between tiles, and color bleeding,
      instead of using this property.
      This property workarounds the fact that some sprite (tileset) images
      lack proper spacing and color bleeding, but it distorts the image
      a bit, so it's no longer pixel-perfect. }
    property SmoothScalingSafeBorder: Boolean read FSmoothScalingSafeBorder write SetSmoothScalingSafeBorder
      default false;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastletiledmapcontrol_persistent_vectors.inc}
  {$undef read_interface_class}
  end {$ifdef FPC}deprecated 'use TCastleTiledMap in a viewport'{$endif};

{$endif read_interface}

{$ifdef read_implementation}

constructor TCastleTiledMapControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ClipChildren := true;
  FObjectsVisible := true;
  FScale := 1;
  FSmoothScaling := false;
  FSmoothScalingSafeBorder := false;

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
  Lay: TCastleTiledMapData.TLayer;              // ObjectGroupLayer
  TiledObj: TCastleTiledMapData.TTiledObject;   // one TiledObject
  TiledObjPoints: array of TVector2;  // holds points of TiledObject as array of TVector2

  { Transform point from Tiled position to our render position.
    This is similar to TCastleTiledMapControl.TileRectangle, but works only for simple point. }
  function RenderPosition(V: TVector2; const ContainerCoordinates: Boolean = true): TVector2;
  begin
    V.Y := Map.Height * Map.TileHeight - V.Y; // convert to be relative to bottom
    // Apply Origin and Scale
    Result := (V - Origin) * Scale;
    if ContainerCoordinates then
    begin
      // Apply UI scaling and anchors to get position on the final container
      Result := LocalToContainerPosition(Result);
    end;
  end;

  { Converts all TVector2's from a TVector2List to and array of TVector2.
    ArrIndex is the index of a TVector2 in the array, ListIndex is the TVector2
    in the list. }
  procedure ConvertAndScalePoints(const ArrIndex, ListIndex: Cardinal);
  begin
    TiledObjPoints[ArrIndex] := RenderPosition(
      TiledObj.Points.Items[ListIndex] + TiledObj.Position + Lay.Offset);
  end;

  { Prepare and render TiledObject polyline and polygon. }
  procedure RenderPolylinePolygon;
  var
    I: Cardinal;
  begin
    { Prepare points for DrawPrimitive2D function. Convert from vector list
      to array of vectors. }
    SetLength(TiledObjPoints, TiledObj.Points.Count);
    for I := 0 to High(TiledObjPoints) do
    begin
      ConvertAndScalePoints(I, I);
    end;

    { If TiledObject is a polygon, for rendering of last line, a point has to be
      added manually to close the polygon. }
    if TiledObj.Primitive = topPolygon then
    begin
      SetLength(TiledObjPoints, TiledObj.Points.Count + 1);
      ConvertAndScalePoints(High(TiledObjPoints), 0);
    end;

    { Draw polygon or polyline TiledObject. }
    DrawPrimitive2D(pmLineStrip, TiledObjPoints, Orange, bsSrcAlpha,
      bdOneMinusSrcAlpha, false, 1, 1);
  end;

  procedure RenderPoint;
  var
    TiledObjPoint: array [0..0] of TVector2;   // array with 1 element
  begin
    { Prepare point. }
    TiledObjPoint[0] := RenderPosition(Lay.Offset + TiledObj.Position);

    { Draw point TiledObject. }
    DrawPrimitive2D(pmPoints, TiledObjPoint, Orange, bsSrcAlpha,
      bdOneMinusSrcAlpha, false, 1, 1);
  end;

  procedure RenderRectangle;
  var
    TiledObjRect: TFloatRectangle;
    LeftBottom, RightTop: TVector2;
  begin
    { Prepare rectangle.
      The "Lay.Offset + TiledObj.Position" indicates left-top corner of the rectangle in Tiled. }
    LeftBottom := RenderPosition(Lay.Offset + TiledObj.Position + Vector2(0, TiledObj.Height));
    RightTop := RenderPosition(Lay.Offset + TiledObj.Position + Vector2(TiledObj.Width, 0));
    TiledObjRect := FloatRectangle(
      LeftBottom.X,
      LeftBottom.Y,
      RightTop.X - LeftBottom.X,
      RightTop.Y - LeftBottom.Y
    );

    { Draw rectangle TiledObject. }
    DrawRectangleOutline(TiledObjRect, Orange, 1, bsSrcAlpha,
      bdOneMinusSrcAlpha, false);
  end;

begin
  { Object groups. }
  for Lay in Map.Layers do
  begin
    if (Lay is TCastleTiledMapData.TObjectGroupLayer) and Lay.Visible then
    begin
      for TiledObj in (Lay as TCastleTiledMapData.TObjectGroupLayer).Objects do
      begin
        if TiledObj.Visible then
        begin
          case TiledObj.Primitive of
            topPolyline, topPolygon: RenderPolylinePolygon;
            topPoint: RenderPoint;
            topRectangle: RenderRectangle;
            // TODO: handle ellipse
            else;
          end;
        end;
      end;
    end;
  end;
  // TODO: when to draw TImageLayer?
end;

procedure TCastleTiledMapControl.LoadTilesetsImages;
var
  Tileset: TCastleTiledMapData.TTileset;
  Spr: TSprite;
begin
  if Map <> nil then
    for Tileset in Map.Tilesets do
    begin
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
        which may difer from Map.TileWidth/Height.
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
      Spr.SmoothScalingSafeBorder := SmoothScalingSafeBorder;
      Tileset.RendererData := Spr;
    end;
end;

procedure TCastleTiledMapControl.FreeTilesetsImages;
var
  Tileset: TCastleTiledMapData.TTileset;
begin
  if Map <> nil then
    for Tileset in Map.Tilesets do
      FreeAndNil(Tileset.RendererData);
end;

procedure TCastleTiledMapControl.Close;
begin
  FreeTilesetsImages;
  FreeAndNil(FMap);
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
      FMap := TCastleTiledMapData.Create(Value);
    LoadTilesetsImages;
  end;
end;

procedure TCastleTiledMapControl.Render;
var
  RR: TFloatRectangle;

  { Return TSprite to render this tile,
    with sprite's Frame, HorizontalFlip, VerticalFlip, DiagonalFlip already set.
    May return @nil if nothing to render. }
  function TileSprite(const Layer: TCastleTiledMapData.TLayer; const X, Y: Integer): TSprite;
  var
    Tileset: TCastleTiledMapData.TTileset;
    Frame: Integer;
    HorizontalFlip, VerticalFlip, DiagonalFlip: Boolean;
  begin
    Result := nil;

    if Map.TileRenderData(Vector2Integer(X, Y), Layer,
      Tileset, Frame, HorizontalFlip, VerticalFlip, DiagonalFlip) then
    begin
      Result := Tileset.RendererData as TSprite;
      if Result = nil then Exit;

      Result.Frame := Frame;
      Result.HorizontalFlip := HorizontalFlip;
      Result.VerticalFlip := VerticalFlip;
      { TODO: this makes diagonal flip too late, effectively *after* HorizontalFlip and VerticalFlip.
        Tiled expects (https://doc.mapeditor.org/en/stable/reference/tmx-map-format/):

        - The diagonal flip (x/y axis swap) is done first,
        - followed by the horizontal
        - and vertical flips.
      }
      if DiagonalFlip then
        Result.Rotation := Pi / 2
      else
        Result.Rotation := 0;
    end;
  end;

  procedure RenderTile(const Layer: TCastleTiledMapData.TLayer; const X, Y: Integer);
  var
    Spr: TSprite;
    SpriteRenderRect: TFloatRectangle;
  begin
    Spr := TileSprite(Layer, X, Y);
    if Spr = nil then Exit;

    // TODO: use Layer opacity
    // TODO: use Layer offset

    SpriteRenderRect := TileRectangle(Vector2Integer(X, Y), true,
      Spr.DrawingWidth, // corresponds to Tileset.TileWidth
      Spr.DrawingHeight // corresponds to Tileset.TileHeight
    );

    { Don't render off-screen images, this makes big difference for huge maps where
      most content if off-screen,
      testcase: examples/tiled/map_viewer/data/maps/desert_big.tmx .
      We rely on ClipChildren = true here: Tiled map rendering is always
      limited to RR. }
    if RR.Collides(SpriteRenderRect) then
      Spr.Draw(SpriteRenderRect);
  end;

var
  X, Y: Integer;
  Layer: TCastleTiledMapData.TLayer;
begin
  inherited Render;
  if Map = nil then Exit;

  { This routine benefits a lot from TDrawableImage automatic batching,
    testcase: examples/tiled/map_viewer/data/maps/desert_big.tmx }
  TDrawableImage.BatchingBegin;
  try
    RR := RenderRect;

    if Map.BackgroundColor[3] <> 0 then
      DrawRectangle(RR, Map.BackgroundColor);

    for Layer in Map.Layers do
    begin
      if not Layer.Visible then Continue;
      if (Layer is TCastleTiledMapData.TObjectGroupLayer) or  // TODO: See below, it would be better to render TObjectGroupLayer here, instead of special RenderObjects call
         (Layer is TCastleTiledMapData.TImageLayer) then
        Continue;

      // TODO: use Map.RenderOrder

      for Y := Map.Height - 1 downto 0 do
        for X := 0 to Map.Width - 1 do
          RenderTile(Layer, X, Y);
    end;

    { TODO: This is Redundant? Better to render the layer at cond. above at
      (Layer is TCastleTiledMapData.TObjectGroupLayer). Would make redundant property
      ObjectsVisible obsolete. This property is not found in the tmx tile specs.,
      but covered by TLayer.Visible already. See:
      https://doc.mapeditor.org/en/stable/reference/tmx-map-format/#objectgroup }
    if FObjectsVisible then
      RenderObjects;

  finally TDrawableImage.BatchingEnd end;
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
    Tileset: TCastleTiledMapData.TTileset;
  begin
    if Map <> nil then
      for Tileset in Map.Tilesets do
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

procedure TCastleTiledMapControl.SetSmoothScalingSafeBorder(const Value: Boolean);

  procedure ChangeExistingImages;
  var
    Tileset: TCastleTiledMapData.TTileset;
  begin
    if Map <> nil then
      for Tileset in Map.Tilesets do
        if Tileset.RendererData <> nil then
          (Tileset.RendererData as TSprite).SmoothScalingSafeBorder :=
            SmoothScalingSafeBorder;
  end;

begin
  if FSmoothScalingSafeBorder <> Value then
  begin
    FSmoothScalingSafeBorder := Value;
    ChangeExistingImages;
    VisibleChange([chRectangle]);
  end;
end;

function TCastleTiledMapControl.PositionToTile(const Position: TVector2;
  const ContainerCoordinates: Boolean;
  out TilePosition: TVector2Integer): Boolean;
var
  LocalPosition: TVector2;
begin
  if Map = nil then Exit(false);

  if ContainerCoordinates then
    LocalPosition := ContainerToLocalPosition(Position)
  else
    LocalPosition := Position;

  Result := Map.PositionToTile((LocalPosition / Scale) + Origin, TilePosition);
end;

function TCastleTiledMapControl.TileRectangle(const TilePosition: TVector2Integer;
  const ContainerCoordinates: Boolean): TFloatRectangle;
begin
  if Map = nil then
    Result := TFloatRectangle.Empty
  else
    Result := TileRectangle(TilePosition, ContainerCoordinates, Map.TileWidth, Map.TileHeight);
end;

function TCastleTiledMapControl.TileRectangle(const TilePosition: TVector2Integer;
  const ContainerCoordinates: Boolean;
  const ForceTileWidth, ForceTileHeight: Single): TFloatRectangle;
begin
  if Map = nil then
    Result := TFloatRectangle.Empty
  else
  begin
    Result.LeftBottom := Map.TileRenderPosition(TilePosition);
    Result.Width := ForceTileWidth;
    Result.Height := ForceTileHeight;
    Result := Result.
      // Apply Origin and Scale
      Translate(-Origin).
      ScaleAround0(Scale);
    if ContainerCoordinates then
    begin
      Result := Result.
        // Apply UIScale and RenderRect (anchors etc.)
        // to honor standard TCastleUserInterface conventions
        ScaleAround0(UIScale).
        Translate(RenderRect.LeftBottom);
    end;
  end;
end;

function TCastleTiledMapControl.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'URL') or
     (PropertyName = 'SmoothScaling') or
     (PropertyName = 'SmoothScalingSafeBorder') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastletiledmapcontrol_persistent_vectors.inc}
{$undef read_implementation_methods}

{$endif read_implementation}
