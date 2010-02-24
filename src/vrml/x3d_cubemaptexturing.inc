{
  Copyright 2008-2010 Michalis Kamburelis.

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
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
  end;

  TNodeComposedCubeMapTexture = class(TNodeX3DEnvironmentTextureNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdback: TSFNode;
    public property Fdback: TSFNode read FFdback;

    private FFdbottom: TSFNode;
    public property Fdbottom: TSFNode read FFdbottom;

    private FFdfront: TSFNode;
    public property Fdfront: TSFNode read FFdfront;

    private FFdleft: TSFNode;
    public property Fdleft: TSFNode read FFdleft;

    private FFdright: TSFNode;
    public property Fdright: TSFNode read FFdright;

    private FFdtop: TSFNode;
    public property Fdtop: TSFNode read FFdtop;

    private FFdtextureProperties: TSFNode;
    public property FdtextureProperties: TSFNode read FFdtextureProperties;
  end;

  { Helper class common to all VRML/X3D generated texture nodes. }
  TGeneratedTextureHandler = class
  private
    FUpdateNeeded: boolean;
  public
    constructor Create;

    { When update contains "ALWAYS", you can check this to know if really
      something visible changed since last update.
      If not, then you do not have to update the texture --- no point, since
      it would look exactly like the current one.

      Scene classes (TVRMLScene, TVRMLGLScene, TVRMLOpenGLRenderers)
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
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    destructor Destroy; override;

    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdupdate: TSFString;
    public property Fdupdate: TSFString read FFdupdate;

    private FFdsize: TSFInt32;
    public property Fdsize: TSFInt32 read FFdsize;

    private FFdtextureProperties: TSFNode;
    public property FdtextureProperties: TSFNode read FFdtextureProperties;

    property GeneratedTextureHandler: TGeneratedTextureHandler
      read FGeneratedTextureHandler;
  end;

  TNodeImageCubeMapTexture = class(TNodeX3DEnvironmentTextureNode, INodeX3DUrlObject)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;

    private FFdtextureProperties: TSFNode;
    public property FdtextureProperties: TSFNode read FFdtextureProperties;

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
constructor TNodeX3DEnvironmentTextureNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;
end;

constructor TNodeComposedCubeMapTexture.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdback := TSFNode.Create(Self, 'back', [TNodeX3DTexture2DNode]);
  Fields.Add(FFdback);

  FFdbottom := TSFNode.Create(Self, 'bottom', [TNodeX3DTexture2DNode]);
  Fields.Add(FFdbottom);

  FFdfront := TSFNode.Create(Self, 'front', [TNodeX3DTexture2DNode]);
  Fields.Add(FFdfront);

  FFdleft := TSFNode.Create(Self, 'left', [TNodeX3DTexture2DNode]);
  Fields.Add(FFdleft);

  FFdright := TSFNode.Create(Self, 'right', [TNodeX3DTexture2DNode]);
  Fields.Add(FFdright);

  FFdtop := TSFNode.Create(Self, 'top', [TNodeX3DTexture2DNode]);
  Fields.Add(FFdtop);

  { X3D spec 3.2 doesn't specify this, but it's natural,
    instantreality also uses this. }
  FFdtextureProperties := TSFNode.Create(Self, 'textureProperties', [TNodeTextureProperties]);
  FFdtextureProperties.Exposed := false;
  Fields.Add(FFdtextureProperties);
end;

class function TNodeComposedCubeMapTexture.ClassNodeTypeName: string;
begin
  Result := 'ComposedCubeMapTexture';
end;

function TNodeComposedCubeMapTexture.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
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

constructor TNodeGeneratedCubeMapTexture.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdupdate := TSFString.Create(Self, 'update', 'NONE');
  Fields.Add(FFdupdate);
  { X3D specification comment: ["NONE"|"NEXT_FRAME_ONLY"|"ALWAYS"] }

  FFdsize := TSFInt32.Create(Self, 'size', 128, { MustBeNonnegative } true);
  FFdsize.Exposed := false;
  Fields.Add(FFdsize);
  { X3D specification comment: (0,Inf) }

  FFdtextureProperties := TSFNode.Create(Self, 'textureProperties', [TNodeTextureProperties]);
  FFdtextureProperties.Exposed := false;
  Fields.Add(FFdtextureProperties);

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

function TNodeGeneratedCubeMapTexture.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeGeneratedCubeMapTexture.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeImageCubeMapTexture.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdurl := TMFString.Create(Self, 'url', []);
  Fields.Add(FFdurl);
  { X3D specification comment: [URI] }

  FFdtextureProperties := TSFNode.Create(Self, 'textureProperties', [TNodeTextureProperties]);
  FFdtextureProperties.Exposed := false;
  Fields.Add(FFdtextureProperties);
end;

class function TNodeImageCubeMapTexture.ClassNodeTypeName: string;
begin
  Result := 'ImageCubeMapTexture';
end;

function TNodeImageCubeMapTexture.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
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
