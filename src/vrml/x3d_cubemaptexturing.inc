{
  Copyright 2008-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { }
  TNodeX3DEnvironmentTextureNode = class(TNodeX3DTextureNode)
  public
    procedure CreateNode; override;
  end;

  TNodeComposedCubeMapTexture = class(TNodeX3DEnvironmentTextureNode)
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

      Scene classes (TVRMLScene, TVRMLGLScene, TVRMLGLRenderers)
      take care to set this field. After each actual update of the texture,
      it's set to @false. Each time something visible affecting the look
      of this texture possibly changed, it's set to @true. }
    property UpdateNeeded: boolean read FUpdateNeeded write FUpdateNeeded
      default true;
  public
    FdUpdate: TSFString;
  end;

  TNodeGeneratedCubeMapTexture = class(TNodeX3DEnvironmentTextureNode)
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

  TNodeImageCubeMapTexture = class(TNodeX3DEnvironmentTextureNode, INodeX3DUrlObject)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdUrl: TMFString;
    public property FdUrl: TMFString read FFdUrl;

    private FFdTextureProperties: TSFNode;
    public property FdTextureProperties: TSFNode read FFdTextureProperties;

    { Load cube environment map from DDS image.

      In case of problems, will make VRMLWarning.
      This includes situations when url cannot be loaded for whatever reason.
      Also problems when url contains valid DDS image,
      but not describing cube map with all 6 sides.

      If all URLs failed, will return @nil. }
    function LoadImage: TDDSImage;
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TNodeX3DEnvironmentTextureNode.CreateNode;
begin
  inherited;
end;

procedure TNodeComposedCubeMapTexture.CreateNode;
begin
  inherited;

  { TODO: changes to nodes inside will not be properly caught,
    they should result in chTextureRendererProperties on us. }

  FFdBack := TSFNode.Create(Self, 'back', [TNodeX3DTexture2DNode]);
   FdBack.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdBack);

  FFdBottom := TSFNode.Create(Self, 'bottom', [TNodeX3DTexture2DNode]);
   FdBottom.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdBottom);

  FFdFront := TSFNode.Create(Self, 'front', [TNodeX3DTexture2DNode]);
   FdFront.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdFront);

  FFdLeft := TSFNode.Create(Self, 'left', [TNodeX3DTexture2DNode]);
   FdLeft.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdLeft);

  FFdRight := TSFNode.Create(Self, 'right', [TNodeX3DTexture2DNode]);
   FdRight.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdRight);

  FFdTop := TSFNode.Create(Self, 'top', [TNodeX3DTexture2DNode]);
   FdTop.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdTop);

  { X3D spec 3.2 doesn't specify this, but it's natural,
    instantreality also uses this. }
  FFdTextureProperties := TSFNode.Create(Self, 'textureProperties', [TNodeTextureProperties]);
   FdTextureProperties.Exposed := false;
   FdTextureProperties.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdTextureProperties);
end;

class function TNodeComposedCubeMapTexture.ClassNodeTypeName: string;
begin
  Result := 'ComposedCubeMapTexture';
end;

class function TNodeComposedCubeMapTexture.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
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
    function ExposedEventsFieldClass: TVRMLFieldClass; override;
  public
    function Changes: TVRMLChanges; override;
  end;

function TSFStringUpdate.ExposedEventsFieldClass: TVRMLFieldClass;
begin
  Result := TSFString;
end;

function TSFStringUpdate.Changes: TVRMLChanges;
begin
  if Value <> 'NONE' then
    Result := [chRedisplay] else
    Result := [];
end;

procedure TNodeGeneratedCubeMapTexture.CreateNode;
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

  FFdTextureProperties := TSFNode.Create(Self, 'textureProperties', [TNodeTextureProperties]);
   FdTextureProperties.Exposed := false;
   FdTextureProperties.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdTextureProperties);

  FGeneratedTextureHandler := TGeneratedTextureHandler.Create;
  FGeneratedTextureHandler.FdUpdate := FdUpdate;
end;

destructor TNodeGeneratedCubeMapTexture.Destroy;
begin
  FreeAndNil(FGeneratedTextureHandler);
  inherited;
end;

class function TNodeGeneratedCubeMapTexture.ClassNodeTypeName: string;
begin
  Result := 'GeneratedCubeMapTexture';
end;

class function TNodeGeneratedCubeMapTexture.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeImageCubeMapTexture.CreateNode;
begin
  inherited;

  FFdUrl := TMFString.Create(Self, 'url', []);
  { The image loaded by LoadImage method isn't saved here.
    So we don't need chTextureImage in case of change,
    instead just let renderer load the texture again. }
   FdUrl.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdUrl);
  { X3D specification comment: [URI] }

  FFdTextureProperties := TSFNode.Create(Self, 'textureProperties', [TNodeTextureProperties]);
   FdTextureProperties.Exposed := false;
   FdTextureProperties.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdTextureProperties);
end;

class function TNodeImageCubeMapTexture.ClassNodeTypeName: string;
begin
  Result := 'ImageCubeMapTexture';
end;

class function TNodeImageCubeMapTexture.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TNodeImageCubeMapTexture.LoadImage: TDDSImage;
var
  I: Integer;
  FullUrl: string;
begin
  Result := TDDSImage.Create;
  try
    for I := 0 to FdUrl.Items.Count - 1 do
    begin
      FullUrl := PathFromWWWBasePath(FdUrl.Items[I]);

      if FileExtToImageFormatDef(ExtractFileExt(FullUrl),
        false, false, ifBMP) <> ifDDS then
      begin
        VRMLWarning(vwIgnorable, Format('Only DDS format is supported for ImageCubeMapTexture node, but URL is "%s"', [FullUrl]));
        Continue;
      end;

      try
        Result.LoadFromFile(FullUrl);
      except
        on E: Exception do
        begin
          Result.Close;
          VRMLWarning(vwIgnorable, Format('Error when loading DDS file "%s": %s', [FullUrl, E.Message]));
          Continue;
        end;
      end;

      if Result.DDSType <> dtCubeMap then
      begin
        Result.Close;
        VRMLWarning(vwIgnorable, Format('DDS image "%s" given for ImageCubeMapTexture doesn''t describe a cube map texture', [FullUrl]));
        Continue;
      end;

      if Result.CubeMapSides <> AllDDSCubeMapSides then
      begin
        Result.Close;
        VRMLWarning(vwIgnorable, Format('DDS image "%s" given for ImageCubeMapTexture doesn''t contain all cube map sides', [FullUrl]));
        Continue;
      end;

      Exit;
    end;

    { If we got here, then no URL was good. So set Result to @nil. }
    FreeAndNil(Result);
  except FreeAndNil(Result); raise end;
end;

procedure RegisterCubeMapTexturingNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeComposedCubeMapTexture,
    TNodeGeneratedCubeMapTexture,
    TNodeImageCubeMapTexture
  ]);
end;

{$endif read_implementation}
