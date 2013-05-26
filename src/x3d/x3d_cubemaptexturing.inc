{
  Copyright 2008-2013 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { }
  TAbstractEnvironmentTextureNode = class(TAbstractTextureNode)
  public
    procedure CreateNode; override;
  end;

  TComposedCubeMapTextureNode = class(TAbstractEnvironmentTextureNode)
  private
    FAlphaChannelData: TAlphaChannel;
  protected
    function AlphaChannelData: TAlphaChannel; override;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdBack: TSFNode;
    public property FdBack: TSFNode read FFdBack;

    private FFdBottom: TSFNode;
    public property FdBottom: TSFNode read FFdBottom;

    private FFdFront: TSFNode;
    public property FdFront: TSFNode read FFdFront;

    private FFdLeft: TSFNode;
    public property FdLeft: TSFNode read FFdLeft;

    private FFdRight: TSFNode;
    public property FdRight: TSFNode read FFdRight;

    private FFdTop: TSFNode;
    public property FdTop: TSFNode read FFdTop;

    private FFdTextureProperties: TSFNode;
    public property FdTextureProperties: TSFNode read FFdTextureProperties;

    { Make sure all 6 sides are loaded.
      Make sure that all 6 fields for each cube map side are assigned,
      are TAbstractTexture2DNode instance (X3D spec requires it),
      load them, and check are they TCastleImage (not just TEncodedImage;
      loading S3TC cube map sides to OpenGL is not available).

      Also calculate AlphaChannel for the whole cube map.
      Our AlphaChannel method will reflect the state from
      last call of this. }
    function LoadSides: boolean;
  end;

  { Helper for all VRML/X3D generated texture nodes. }
  TGeneratedTextureHandler = class
  private
    FUpdateNeeded: boolean;
  public
    constructor Create;

    { When update contains "ALWAYS", you can check this to know if really
      something visible changed since last update.
      If not, then you do not have to update the texture --- no point, since
      it would look exactly like the current one.

      Scene classes (TCastleSceneCore, TCastleScene, TGLRenderer)
      take care to set this field. After each actual update of the texture,
      it's set to @false. Each time something visible affecting the look
      of this texture possibly changed, it's set to @true. }
    property UpdateNeeded: boolean read FUpdateNeeded write FUpdateNeeded
      default true;
  public
    FdUpdate: TSFString;
  end;

  TGeneratedCubeMapTextureNode = class(TAbstractEnvironmentTextureNode)
  private
    FGeneratedTextureHandler: TGeneratedTextureHandler;
  public
    procedure CreateNode; override;
    destructor Destroy; override;

    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdUpdate: TSFString;
    public property FdUpdate: TSFString read FFdUpdate;

    private FFdSize: TSFInt32;
    public property FdSize: TSFInt32 read FFdSize;

    private FFdTextureProperties: TSFNode;
    public property FdTextureProperties: TSFNode read FFdTextureProperties;

    property GeneratedTextureHandler: TGeneratedTextureHandler
      read FGeneratedTextureHandler;
  end;

  TImageCubeMapTextureNode = class(TAbstractEnvironmentTextureNode, IAbstractUrlObject)
  private
    FAlphaChannelData: TAlphaChannel;
  protected
    function AlphaChannelData: TAlphaChannel; override;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdUrl: TMFString;
    public property FdUrl: TMFString read FFdUrl;

    private FFdTextureProperties: TSFNode;
    public property FdTextureProperties: TSFNode read FFdTextureProperties;

    { Load cube environment map from DDS image.

      In case of problems, will make OnWarning.
      This includes situations when url cannot be loaded for whatever reason.
      Also problems when url contains valid DDS image,
      but not describing cube map with all 6 sides.

      If all URLs failed, will return @nil.

      Although the loaded image is not saved here, we do save the AlphaChannel
      type. Our AlphaChannel method will reflect last loaded image. }
    function LoadImage: TDDSImage;
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TAbstractEnvironmentTextureNode.CreateNode;
begin
  inherited;
end;

procedure TComposedCubeMapTextureNode.CreateNode;
begin
  inherited;

  { TODO: changes to nodes inside will not be properly caught,
    they should result in chTextureRendererProperties on us. }

  FFdBack := TSFNode.Create(Self, 'back', [TAbstractX3DTexture2DNode]);
   FdBack.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdBack);

  FFdBottom := TSFNode.Create(Self, 'bottom', [TAbstractX3DTexture2DNode]);
   FdBottom.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdBottom);

  FFdFront := TSFNode.Create(Self, 'front', [TAbstractX3DTexture2DNode]);
   FdFront.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdFront);

  FFdLeft := TSFNode.Create(Self, 'left', [TAbstractX3DTexture2DNode]);
   FdLeft.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdLeft);

  FFdRight := TSFNode.Create(Self, 'right', [TAbstractX3DTexture2DNode]);
   FdRight.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdRight);

  FFdTop := TSFNode.Create(Self, 'top', [TAbstractX3DTexture2DNode]);
   FdTop.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdTop);

  { X3D spec 3.2 doesn't specify this, but it's natural,
    instantreality also uses this. }
  FFdTextureProperties := TSFNode.Create(Self, 'textureProperties', [TTexturePropertiesNode]);
   FdTextureProperties.Exposed := false;
   FdTextureProperties.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdTextureProperties);
end;

class function TComposedCubeMapTextureNode.ClassNodeTypeName: string;
begin
  Result := 'ComposedCubeMapTexture';
end;

class function TComposedCubeMapTextureNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TComposedCubeMapTextureNode.LoadSides: boolean;

  { Checks is given side has non-nil valid node class,
    and then if image there can be loaded. }
  function SideLoaded(SideField: TSFNode): boolean;
  var
    SideTex: TAbstractTexture2DNode;
  begin
    Result :=
      (SideField.Value <> nil) and
      (SideField.Value is TAbstractTexture2DNode);
    if Result then
    begin
      SideTex := TAbstractTexture2DNode(SideField.Value);
      Result := SideTex.IsTextureImage;

      if Result and not (SideTex.TextureImage is TCastleImage) then
      begin
        OnWarning(wtMinor, 'VRML/X3D', 'ComposedCubeMapTexture cannot contain S3TC images, as we have to rotate images within, and we cannot do this (fast) with compressed textures');
        Result := false;
      end;
    end;

    { If any slice has full-range alpha, then assume whole texture does. }
    AlphaMaxTo1st(FAlphaChannelData, SideTex.AlphaChannel);
  end;

begin
  FAlphaChannelData := acNone;
  Result :=
    SideLoaded(FdBack) and
    SideLoaded(FdBottom) and
    SideLoaded(FdFront) and
    SideLoaded(FdLeft) and
    SideLoaded(FdRight) and
    SideLoaded(FdTop);
end;

function TComposedCubeMapTextureNode.AlphaChannelData: TAlphaChannel;
begin
  Result := FAlphaChannelData;
end;

constructor TGeneratedTextureHandler.Create;
begin
  inherited;
  FUpdateNeeded := true;
end;

type
  { SFString for "update" field on rendered textures
    (like GeneratedCubeMapTexture).

    This causes appropriate @link(Changes): [chRedisplay] if value <> 'NONE'.
    Then necessary things will be done automatically
    at next UpdateGeneratedTextures call, so nothing besides chRedisplay
    is required.

    Note we do not pass chVisibleGeometry, chVisibleNonGeometry, or such.
    So VisibleChangeHere will be called with [].
    That's logical --- only the change of "update" field doesn't visibly
    change anything on the scene. This means that if you change "update"
    to "ALWAYS", but no visible change was registered since last update
    of the texture, the texture will not be actually immediately
    regenerated --- correct optimization!

    If value is 'NONE', nothing needs to be done. }
  TSFStringUpdate = class(TSFString)
  protected
    function ExposedEventsFieldClass: TX3DFieldClass; override;
  public
    function ExecuteChanges: TX3DChanges; override;
  end;

function TSFStringUpdate.ExposedEventsFieldClass: TX3DFieldClass;
begin
  Result := TSFString;
end;

function TSFStringUpdate.ExecuteChanges: TX3DChanges;
begin
  if Value <> 'NONE' then
    Result := [chRedisplay] else
    Result := [];
end;

procedure TGeneratedCubeMapTextureNode.CreateNode;
begin
  inherited;

  FFdUpdate := TSFStringUpdate.Create(Self, 'update', 'NONE');
  Fields.Add(FFdUpdate);
  { X3D specification comment: ["NONE"|"NEXT_FRAME_ONLY"|"ALWAYS"] }

  FFdSize := TSFInt32.Create(Self, 'size', 128, { MustBeNonnegative } true);
   FdSize.Exposed := false;
   FdSize.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdSize);
  { X3D specification comment: (0,Inf) }

  FFdTextureProperties := TSFNode.Create(Self, 'textureProperties', [TTexturePropertiesNode]);
   FdTextureProperties.Exposed := false;
   FdTextureProperties.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdTextureProperties);

  FGeneratedTextureHandler := TGeneratedTextureHandler.Create;
  FGeneratedTextureHandler.FdUpdate := FdUpdate;
end;

destructor TGeneratedCubeMapTextureNode.Destroy;
begin
  FreeAndNil(FGeneratedTextureHandler);
  inherited;
end;

class function TGeneratedCubeMapTextureNode.ClassNodeTypeName: string;
begin
  Result := 'GeneratedCubeMapTexture';
end;

class function TGeneratedCubeMapTextureNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TImageCubeMapTextureNode.CreateNode;
begin
  inherited;

  FFdUrl := TMFString.Create(Self, 'url', []);
  { The image loaded by LoadImage method isn't saved here.
    So we don't need chTextureImage in case of change,
    instead just let renderer load the texture again. }
   FdUrl.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdUrl);
  { X3D specification comment: [URI] }

  FFdTextureProperties := TSFNode.Create(Self, 'textureProperties', [TTexturePropertiesNode]);
   FdTextureProperties.Exposed := false;
   FdTextureProperties.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdTextureProperties);
end;

class function TImageCubeMapTextureNode.ClassNodeTypeName: string;
begin
  Result := 'ImageCubeMapTexture';
end;

class function TImageCubeMapTextureNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TImageCubeMapTextureNode.LoadImage: TDDSImage;
var
  I: Integer;
  FullUrl: string;
begin
  Result := TDDSImage.Create;
  try
    for I := 0 to FdUrl.Items.Count - 1 do
    begin
      FullUrl := PathFromBaseUrl(FdUrl.Items[I]);

      if not TDDSImage.MatchesURL(FullUrl) then
      begin
        OnWarning(wtMinor, 'VRML/X3D', Format('Only DDS format is supported for ImageCubeMapTexture node, but URL is "%s"', [FullUrl]));
        Continue;
      end;

      try
        Result.LoadFromFile(FullUrl);
      except
        on E: Exception do
        begin
          Result.Close;
          OnWarning(wtMinor, 'VRML/X3D', Format('Error when loading DDS file "%s": %s', [FullUrl, E.Message]));
          Continue;
        end;
      end;

      FAlphaChannelData := Result.Images[0].AlphaChannel;

      if Result.DDSType <> dtCubeMap then
      begin
        Result.Close;
        OnWarning(wtMinor, 'VRML/X3D', Format('DDS image "%s" given for ImageCubeMapTexture doesn''t describe a cube map texture', [FullUrl]));
        Continue;
      end;

      if Result.CubeMapSides <> AllDDSCubeMapSides then
      begin
        Result.Close;
        OnWarning(wtMinor, 'VRML/X3D', Format('DDS image "%s" given for ImageCubeMapTexture doesn''t contain all cube map sides', [FullUrl]));
        Continue;
      end;

      Exit;
    end;

    { If we got here, then no URL was good. So set Result to @nil. }
    FreeAndNil(Result);
  except FreeAndNil(Result); raise end;
end;

function TImageCubeMapTextureNode.AlphaChannelData: TAlphaChannel;
begin
  Result := FAlphaChannelData;
end;

procedure RegisterCubeMapTexturingNodes;
begin
  NodesManager.RegisterNodeClasses([
    TComposedCubeMapTextureNode,
    TGeneratedCubeMapTextureNode,
    TImageCubeMapTextureNode
  ]);
end;

{$endif read_implementation}
