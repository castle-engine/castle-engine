{
  Copyright 2002-2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{$ifdef read_interface}
  TNodeX3DTextureCoordinateNode = class(TNodeX3DGeometricPropertyNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
  end;

  TNodeX3DTextureNode = class(TNodeX3DAppearanceChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
  end;

  { Common texture node for all VRML / X3D texture nodes.

    For X3D, this descends from X3DTextureNode and is an ancestor
    for X3DTexture2DNode, so X3D hierarchy is nicely preserved. }
  TVRMLTextureNode = class(TNodeX3DTextureNode)
  private
    { This is always <> nil.
      We use only IsNull to indicate whether we have or have not a texture here. }
    FTextureImage: TImage;
    { Non-nil only if FTextureImage should be freed using
      TextureImageUsedCache.LoadImage_DecReference. }
    TextureImageUsedCache: TImagesCache;
    FIsTextureLoaded: boolean;
    procedure SetIsTextureLoaded(Value: boolean);

    FImagesCache: TImagesCache;

    procedure FreeAndNilTextureImage;
  protected
    { This loads actual image. It must return newly created TImage
      instance if texture could be loaded, or nil if no texture could
      be loaded. You do not care in this method about things like
      IsImageLoaded --- this method should just always,
      unconditionally, make everything it can do to load texture.

      You can use VRMLNonFatalError inside, so we're prepared
      that this may even exit with exception (since VRMLNonFatalError
      can raise exception).

      You have to return TRGBImage or TAlphaImage here, see TextureImage docs.

      Set CacheUsed to @true if you loaded the image using ImagesCache.

      Also, set FTextureUsedFullUrl here. }
    function LoadTextureImage(out CacheUsed: boolean): TImage; virtual; abstract;
    FTextureUsedFullUrl: string;
  public
    constructor Create(const ANodeName: string;
      const AWWWBasePath: string); override;
    destructor Destroy; override;

    { Texture image, inline or loaded from URL.
      First call to TextureImage will automatically
      load the texture, so in simple situations you really
      don't need to do anything. Just use TextureImage when you want,
      and things will just work.

      Be sure to set ImagesCache before loading the texture.
      TODO: allow ImagesCache to be @nil.

      This is never @nil. If the texture is not available for any reason,
      it's indicated by TextureImage.IsNull. IsTextureImage is a shortcut
      for "not TextureImage.IsNull" (so note that calling IsTextureImage also
      may cause texture load).

      IsTextureLoaded says whether the texture is already loaded.
      Since the texture will be loaded automatically, you're usually
      not interested in this property. You can read it to e.g. predict
      if next TextureImage call may take a long time. (You know that
      if IsTextureLoaded = @true then TextureImage just returns ready
      image instantly).

      You can also set IsTextureLoaded.
      Setting to @true means that you request the texture to be loaded @italic(now),
      if it's not loaded already. Setting to @false may be useful if you want
      to release resources (e.g. when you want to keep TNodeTexture instance
      loaded but you know that you will not need TextureImage anymore).
      You can also set it to @false and then back to @true if you want to
      request reloading the texture from URL (e.g. if you suspect that
      the URL contents changed).

      Note that separate IsTextureLoaded and IsTextureImage state
      means that if the texture loading will fail (e.g.
      "texture file foo.png does not exist"), it will be reported
      to VRMLNonFatalError only once, not at every TextureImage call.
      This is obviously important if you use VRMLNonFatalError to report
      warnings for user.

      TextureImage class is always in (TRGBImage, TAlphaImage)
      simply because only these formats are accepted by KambiGLUtils.
      TODO: this is not nice, we do not want any OpenGL dependency in this unit,
      even such "ideological" one. }
    function TextureImage: TImage;
    function IsTextureImage: boolean;
    property IsTextureLoaded: boolean
      read FIsTextureLoaded write SetIsTextureLoaded;
    property ImagesCache: TImagesCache read FImagesCache write FImagesCache;

    { Krotki opis tego jak zdefiniowana jest tekstura. none jesli nie
      zdefiniowana, jakie jest filename, jakie jest inline. NIE okresla
      jak i jaka tekstura jest zaladowana. }
    function TextureDescription: string; virtual; abstract;

    function RepeatS: boolean; virtual; abstract;
    function RepeatT: boolean; virtual; abstract;

    { Once the image is loaded, this is set to the URL that was used to load
      this image, or '' if no URL was used. No URL was used may mean that
      no image was valid, or inlined image was used.

      This is always a full, expanded (i.e. not relative) URL. }
    property TextureUsedFullUrl: string read FTextureUsedFullUrl;
  end;

  TNodeX3DTexture2DNode = class(TVRMLTextureNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFdrepeatS: TSFBool;
    public property FdrepeatS: TSFBool read FFdrepeatS;

    private FFdrepeatT: TSFBool;
    public property FdrepeatT: TSFBool read FFdrepeatT;

    private FFdtextureProperties: TSFNode;
    public property FdtextureProperties: TSFNode read FFdtextureProperties;
  end;

  TNodeX3DTextureTransformNode = class(TNodeX3DAppearanceChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
  end;

  TNodeImageTexture = class(TNodeX3DTexture2DNode, INodeX3DUrlObject)
  protected
    function LoadTextureImage(out CacheUsed: boolean): TImage; override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;

    function TextureDescription: string; override;
    function RepeatS: boolean; override;
    function RepeatT: boolean; override;
  end;

  TNodeMovieTexture = class(TNodeX3DTexture2DNode, INodeX3DSoundSourceNode, INodeX3DUrlObject)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFddescription: TSFString;
    public property Fddescription: TSFString read FFddescription;

    private FFdloop: TSFBool;
    public property Fdloop: TSFBool read FFdloop;

    private FFdpauseTime: TSFTime;
    public property FdpauseTime: TSFTime read FFdpauseTime;

    private FFdresumeTime: TSFTime;
    public property FdresumeTime: TSFTime read FFdresumeTime;

    private FFdspeed: TSFFloat;
    public property Fdspeed: TSFFloat read FFdspeed;

    private FFdstartTime: TSFTime;
    public property FdstartTime: TSFTime read FFdstartTime;

    private FFdstopTime: TSFTime;
    public property FdstopTime: TSFTime read FFdstopTime;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;

    { Event: SFTime, out } { }
    private FEventduration_changed: TVRMLEvent;
    public property Eventduration_changed: TVRMLEvent read FEventduration_changed;

    { Event: SFTime, out } { }
    private FEventelapsedTime: TVRMLEvent;
    public property EventelapsedTime: TVRMLEvent read FEventelapsedTime;

    { Event: SFBool, out } { }
    private FEventisActive: TVRMLEvent;
    public property EventisActive: TVRMLEvent read FEventisActive;

    { Event: SFBool, out } { }
    private FEventisPaused: TVRMLEvent;
    public property EventisPaused: TVRMLEvent read FEventisPaused;
  end;

  TNodeMultiTexture = class(TNodeX3DTextureNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdalpha: TSFFloat;
    public property Fdalpha: TSFFloat read FFdalpha;

    private FFdcolor: TSFColor;
    public property Fdcolor: TSFColor read FFdcolor;

    private FFdfunction: TMFString;
    public property Fdfunction: TMFString read FFdfunction;

    private FFdmode: TMFString;
    public property Fdmode: TMFString read FFdmode;

    private FFdsource: TMFString;
    public property Fdsource: TMFString read FFdsource;

    private FFdtexture: TMFNode;
    public property Fdtexture: TMFNode read FFdtexture;
  end;

  TNodeMultiTextureCoordinate = class(TNodeX3DTextureCoordinateNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdtexCoord: TMFNode;
    public property FdtexCoord: TMFNode read FFdtexCoord;
  end;

  TNodeMultiTextureTransform = class(TNodeX3DTextureTransformNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdtextureTransform: TMFNode;
    public property FdtextureTransform: TMFNode read FFdtextureTransform;
  end;

  TNodePixelTexture = class(TNodeX3DTexture2DNode)
  protected
    function LoadTextureImage(out CacheUsed: boolean): TImage; override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdimage: TSFImage;
    public property Fdimage: TSFImage read FFdimage;

    function TextureDescription: string; override;
    function RepeatS: boolean; override;
    function RepeatT: boolean; override;
  end;

  TNodeTextureCoordinate = class(TNodeX3DTextureCoordinateNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdpoint: TMFVec2f;
    public property Fdpoint: TMFVec2f read FFdpoint;
  end;

  TNodeTextureCoordinateGenerator = class(TNodeX3DTextureCoordinateNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdmode: TSFString;
    public property Fdmode: TSFString read FFdmode;

    private FFdparameter: TMFFloat;
    public property Fdparameter: TMFFloat read FFdparameter;
  end;

  TNodeTextureProperties = class(TNodeX3DNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdanisotropicDegree: TSFFloat;
    public property FdanisotropicDegree: TSFFloat read FFdanisotropicDegree;

    private FFdborderColor: TSFColorRGBA;
    public property FdborderColor: TSFColorRGBA read FFdborderColor;

    private FFdborderWidth: TSFInt32;
    public property FdborderWidth: TSFInt32 read FFdborderWidth;

    private FFdboundaryModeS: TSFString;
    public property FdboundaryModeS: TSFString read FFdboundaryModeS;

    private FFdboundaryModeT: TSFString;
    public property FdboundaryModeT: TSFString read FFdboundaryModeT;

    private FFdboundaryModeR: TSFString;
    public property FdboundaryModeR: TSFString read FFdboundaryModeR;

    private FFdmagnificationFilter: TSFString;
    public property FdmagnificationFilter: TSFString read FFdmagnificationFilter;

    private FFdminificationFilter: TSFString;
    public property FdminificationFilter: TSFString read FFdminificationFilter;

    private FFdtextureCompression: TSFString;
    public property FdtextureCompression: TSFString read FFdtextureCompression;

    private FFdtexturePriority: TSFFloat;
    public property FdtexturePriority: TSFFloat read FFdtexturePriority;

    private FFdgenerateMipMaps: TSFBool;
    public property FdgenerateMipMaps: TSFBool read FFdgenerateMipMaps;
  end;

  TNodeTextureTransform = class(TNodeX3DTextureTransformNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdcenter: TSFVec2f;
    public property Fdcenter: TSFVec2f read FFdcenter;

    private FFdrotation: TSFFloat;
    public property Fdrotation: TSFFloat read FFdrotation;

    private FFdscale: TSFVec2f;
    public property Fdscale: TSFVec2f read FFdscale;

    private FFdtranslation: TSFVec2f;
    public property Fdtranslation: TSFVec2f read FFdtranslation;

    function Matrix: TMatrix4Single;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TNodeX3DTextureCoordinateNode ---------------------------------------------- }

constructor TNodeX3DTextureCoordinateNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  { X3D XML spec doesn't specify containerField for abstract X3D classes.
    texCoord seems most sensible for this case. }
  DefaultContainerField := 'texCoord';
end;

{ TNodeX3DTextureNode -------------------------------------------------------- }

constructor TNodeX3DTextureNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'texture';
end;

{ TVRMLTextureNode ----------------------------------------------------------- }

constructor TVRMLTextureNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;
  FTextureImage := TRGBImage.Create;
  TextureImageUsedCache := nil;
  FIsTextureLoaded := false;
end;

destructor TVRMLTextureNode.Destroy;
begin
  FreeAndNilTextureImage;
  inherited;
end;

procedure TVRMLTextureNode.FreeAndNilTextureImage;
begin
  if TextureImageUsedCache <> nil then
  begin
    TextureImageUsedCache.LoadImage_DecReference(FTextureImage);
    TextureImageUsedCache := nil;
  end else
    FreeAndNil(FTextureImage);
end;

function TVRMLTextureNode.TextureImage: TImage;
begin
  { Setting IsTextureLoaded property will initialize FTextureImage. }
  IsTextureLoaded := true;

  Result := FTextureImage;
end;

procedure TVRMLTextureNode.SetIsTextureLoaded(Value: boolean);

  procedure DoLoadTexture;
  var
    UsedCache: boolean;
  begin
    Assert(ImagesCache <> nil, 'ImagesCache must be set before loading the texture');

    FreeAndNilTextureImage;

    try
      FTextureImage := LoadTextureImage(UsedCache);
      if UsedCache then
        TextureImageUsedCache := ImagesCache;
    except
      { Even if LoadTextureImage exits with exception, FTextureImage should
        always remain non-nil.
        This is all because I just changed Images unit interface to class-like
        and I want to do minimal changes to VRMLNodes unit to not break
        anything. TODO -- this will be solved better in the future, by simply
        allowing TextureImage to be nil at any time. }
      FTextureImage := TRGBImage.Create;
      raise;
    end;

    { LoadTextureImage may raise exception, or it may return @nil in case of
      trouble (this depends on VRMLNonFatalError, so we can't assume anything).
      In case it returned nil, comments above still apply:
      FTextureImage must not be nil. }

    if FTextureImage = nil then
      FTextureImage := TRGBImage.Create;
  end;

begin
  if Value <> FIsTextureLoaded then
  begin
    if Value then
    begin
      { actually load the texture }
      DoLoadTexture;
    end else
    begin
      { unload the texture }
      FreeAndNilTextureImage;
      FTextureImage := TRGBImage.Create;
    end;

    FIsTextureLoaded := Value;
  end;
end;

function TVRMLTextureNode.IsTextureImage: boolean;
begin
  result := not TextureImage.IsNull;
end;

{ TNodeX3DTexture2DNode ------------------------------------------------------ }

constructor TNodeX3DTexture2DNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdrepeatS := TSFBool.Create('repeatS', true);
  FFdrepeatS.Exposed := false;
  Fields.Add(FFdrepeatS);

  FFdrepeatT := TSFBool.Create('repeatT', true);
  FFdrepeatT.Exposed := false;
  Fields.Add(FFdrepeatT);

  FFdtextureProperties := TSFNode.Create(Self, 'textureProperties', [TNodeTextureProperties]);
  FFdtextureProperties.Exposed := false;
  Fields.Add(FFdtextureProperties);
end;

constructor TNodeX3DTextureTransformNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'textureTransform';
end;

constructor TNodeImageTexture.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdurl := TMFString.Create('url', []);
  Fields.Add(FFdurl);
  { X3D specification comment: [URI] }
end;

class function TNodeImageTexture.ClassNodeTypeName: string;
begin
  Result := 'ImageTexture';
end;

function TNodeImageTexture.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeImageTexture.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TNodeImageTexture.LoadTextureImage(out CacheUsed: boolean): TImage;
var
  I: Integer;
  FullUrl: string;
begin
  Result := nil;
  CacheUsed := false;
  FTextureUsedFullUrl := '';

  for I := 0 to FdUrl.Count - 1 do
  begin
    FullUrl := PathFromWWWBasePath(FdUrl.Items[I]);
    try
      Result := ImagesCache.LoadImage_IncReference(FullUrl);
      CacheUsed := true;
      FTextureUsedFullUrl := FullUrl;
      Break;
    except
      on E: Exception do
        { pamietajmy ze VRMLNonFatalError moze spowodowac rzucenie wyjatku
          (chociaz nie musi) }
        VRMLNonFatalError('Exception ' + E.ClassName +
          ' occured when trying to load ' +
          'texture from filename "' + FullUrl + '" : ' + E.Message);
    end;
  end;
end;

function TNodeImageTexture.TextureDescription: string;
begin
  if TextureUsedFullUrl <> '' then
    Result := 'file "' + TextureUsedFullUrl + '"' else
    Result := 'none';
end;

function TNodeImageTexture.RepeatS: boolean;
begin
  Result := FdRepeatS.Value;
end;

function TNodeImageTexture.RepeatT: boolean;
begin
  Result := FdRepeatT.Value;
end;

constructor TNodeMovieTexture.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFddescription := TSFString.Create('description', '');
  Fields.Add(FFddescription);

  FFdloop := TSFBool.Create('loop', false);
  Fields.Add(FFdloop);

  FFdpauseTime := TSFTime.Create('pauseTime', 0);
  Fields.Add(FFdpauseTime);
  { X3D specification comment: (-Inf,Inf) }

  FFdresumeTime := TSFTime.Create('resumeTime', 0);
  Fields.Add(FFdresumeTime);
  { X3D specification comment: (-Inf,Inf) }

  FFdspeed := TSFFloat.Create('speed', 1.0);
  Fields.Add(FFdspeed);
  { X3D specification comment: (-Inf,Inf) }

  FFdstartTime := TSFTime.Create('startTime', 0);
  Fields.Add(FFdstartTime);
  { X3D specification comment: (-Inf,Inf) }

  FFdstopTime := TSFTime.Create('stopTime', 0);
  Fields.Add(FFdstopTime);
  { X3D specification comment: (-Inf,Inf) }

  FFdurl := TMFString.Create('url', []);
  Fields.Add(FFdurl);
  { X3D specification comment: [URI] }

  FEventduration_changed := TVRMLEvent.Create('duration_changed', TSFTime, false);
  Events.Add(FEventduration_changed);

  FEventelapsedTime := TVRMLEvent.Create('elapsedTime', TSFTime, false);
  Events.Add(FEventelapsedTime);

  FEventisActive := TVRMLEvent.Create('isActive', TSFBool, false);
  Events.Add(FEventisActive);

  FEventisPaused := TVRMLEvent.Create('isPaused', TSFBool, false);
  Events.Add(FEventisPaused);
end;

class function TNodeMovieTexture.ClassNodeTypeName: string;
begin
  Result := 'MovieTexture';
end;

function TNodeMovieTexture.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeMovieTexture.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeMultiTexture.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdalpha := TSFFloat.Create('alpha', 1);
  Fields.Add(FFdalpha);
  { X3D specification comment: [0,1] }

  FFdcolor := TSFColor.Create('color', Vector3Single(1, 1, 1));
  Fields.Add(FFdcolor);
  { X3D specification comment: [0,1] }

  FFdfunction := TMFString.Create('function', []);
  Fields.Add(FFdfunction);

  FFdmode := TMFString.Create('mode', []);
  Fields.Add(FFdmode);

  FFdsource := TMFString.Create('source', []);
  Fields.Add(FFdsource);

  FFdtexture := TMFNode.Create(Self, 'texture', [TNodeX3DTextureNode]);
  Fields.Add(FFdtexture);
end;

class function TNodeMultiTexture.ClassNodeTypeName: string;
begin
  Result := 'MultiTexture';
end;

function TNodeMultiTexture.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeMultiTexture.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeMultiTextureCoordinate.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdtexCoord := TMFNode.Create(Self, 'texCoord', [TNodeX3DTextureCoordinateNode]);
  Fields.Add(FFdtexCoord);
end;

class function TNodeMultiTextureCoordinate.ClassNodeTypeName: string;
begin
  Result := 'MultiTextureCoordinate';
end;

function TNodeMultiTextureCoordinate.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeMultiTextureCoordinate.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeMultiTextureTransform.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdtextureTransform := TMFNode.Create(Self, 'textureTransform', [TNodeX3DTextureTransformNode]);
  Fields.Add(FFdtextureTransform);
end;

class function TNodeMultiTextureTransform.ClassNodeTypeName: string;
begin
  Result := 'MultiTextureTransform';
end;

function TNodeMultiTextureTransform.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeMultiTextureTransform.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodePixelTexture.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdimage := TSFImage.Create('image', nil);
  Fields.Add(FFdimage);
  { X3D specification comment: 0 0 }
end;

class function TNodePixelTexture.ClassNodeTypeName: string;
begin
  Result := 'PixelTexture';
end;

function TNodePixelTexture.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodePixelTexture.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TNodePixelTexture.LoadTextureImage(out CacheUsed: boolean): TImage;
begin
  Result := nil;
  CacheUsed := false;

  if not FdImage.Value.IsNull then
    Result := FdImage.Value.MakeCopy;
end;

function TNodePixelTexture.TextureDescription: string;
begin
  if not FdImage.Value.IsNull then
    result := Format('inlined (width = %d; height = %d; with alpha = %s)',
      [ FdImage.Value.Width, FdImage.Value.Height,
        BoolToStr[FdImage.Value is TAlphaImage] ]) else
    result := 'none';
end;

function TNodePixelTexture.RepeatS: boolean;
begin
  Result := FdRepeatS.Value;
end;

function TNodePixelTexture.RepeatT: boolean;
begin
  Result := FdRepeatT.Value;
end;

constructor TNodeTextureCoordinate.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdpoint := TMFVec2f.Create('point', []);
  Fields.Add(FFdpoint);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TNodeTextureCoordinate.ClassNodeTypeName: string;
begin
  Result := 'TextureCoordinate';
end;

function TNodeTextureCoordinate.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeTextureCoordinate.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeTextureCoordinateGenerator.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdmode := TSFString.Create('mode', 'SPHERE');
  Fields.Add(FFdmode);
  { X3D specification comment: [see Table 18.6] }

  FFdparameter := TMFFloat.Create('parameter', []);
  Fields.Add(FFdparameter);
  { X3D specification comment: [see Table 18.6] }
end;

class function TNodeTextureCoordinateGenerator.ClassNodeTypeName: string;
begin
  Result := 'TextureCoordinateGenerator';
end;

function TNodeTextureCoordinateGenerator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeTextureCoordinateGenerator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeTextureProperties.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdanisotropicDegree := TSFFloat.Create('anisotropicDegree', 1.0);
  Fields.Add(FFdanisotropicDegree);
  { X3D specification comment: [1,Inf) }

  FFdborderColor := TSFColorRGBA.Create('borderColor', Vector4Single(0, 0, 0, 0));
  Fields.Add(FFdborderColor);
  { X3D specification comment: [0,1] }

  FFdborderWidth := TSFInt32.Create('borderWidth', 0);
  Fields.Add(FFdborderWidth);
  { X3D specification comment: [0,1] }

  FFdboundaryModeS := TSFString.Create('boundaryModeS', 'REPEAT');
  Fields.Add(FFdboundaryModeS);
  { X3D specification comment: [see Table 18.7] }

  FFdboundaryModeT := TSFString.Create('boundaryModeT', 'REPEAT');
  Fields.Add(FFdboundaryModeT);
  { X3D specification comment: [see Table 18.7] }

  FFdboundaryModeR := TSFString.Create('boundaryModeR', 'REPEAT');
  Fields.Add(FFdboundaryModeR);
  { X3D specification comment: [see Table 18.7] }

  FFdmagnificationFilter := TSFString.Create('magnificationFilter', 'FASTEST');
  Fields.Add(FFdmagnificationFilter);
  { X3D specification comment: [see Table 18.8] }

  FFdminificationFilter := TSFString.Create('minificationFilter', 'FASTEST');
  Fields.Add(FFdminificationFilter);
  { X3D specification comment: [see Table 18.9] }

  FFdtextureCompression := TSFString.Create('textureCompression', 'FASTEST');
  Fields.Add(FFdtextureCompression);
  { X3D specification comment: [see Table 18.10] }

  FFdtexturePriority := TSFFloat.Create('texturePriority', 0);
  Fields.Add(FFdtexturePriority);
  { X3D specification comment: [0,1] }

  FFdgenerateMipMaps := TSFBool.Create('generateMipMaps', false);
  FFdgenerateMipMaps.Exposed := false;
  Fields.Add(FFdgenerateMipMaps);

  { X3D XML spec (edition 2) mistakenyl claims it should be
    "lineProperties", which is nonsense... I set this to "textureProperties". }
  DefaultContainerField := 'textureProperties';
end;

class function TNodeTextureProperties.ClassNodeTypeName: string;
begin
  Result := 'TextureProperties';
end;

function TNodeTextureProperties.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeTextureProperties.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeTextureTransform.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcenter := TSFVec2f.Create('center', Vector2Single(0, 0));
  Fields.Add(FFdcenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdrotation := TSFFloat.Create('rotation', 0);
  Fields.Add(FFdrotation);
  { X3D specification comment: (-Inf,Inf) }

  FFdscale := TSFVec2f.Create('scale', Vector2Single(1, 1));
  Fields.Add(FFdscale);
  { X3D specification comment: (-Inf,Inf) }

  FFdtranslation := TSFVec2f.Create('translation', Vector2Single(0, 0));
  Fields.Add(FFdtranslation);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TNodeTextureTransform.ClassNodeTypeName: string;
begin
  Result := 'TextureTransform';
end;

function TNodeTextureTransform.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeTextureTransform.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TNodeTextureTransform.Matrix: TMatrix4Single;
begin
  { Note: don't be fooled by a little confusing VRML 2.0 spec
    wording for TextureTransform, that suggests that VRML 2.0
    TextureTransform should multiply matrices in reversed
    order than VRML 1.0. I'm talking about the paragraph

       In matrix transformation notation, where Tc is the
       untransformed texture coordinate, Tc' is the transformed
       texture coordinate, C (center), T (translation),
       R (rotation), and S (scale) are the intermediate
       transformation matrices,

         Tc' = -C × S × R × C × T × Tc

    VRML TextureTransform node transforms texture *coordinates*.
    OpenGL texture matrix transforms texture *coordinates*.
    And above paragraph says about texture *coordinates*.
    So I'm either ultra-dumb or ultra-smart, but for me
    the above paragraph is wrong: it says precisely that I should load
    the *reversed* matrix of what I'm actually loading.
    If I would follow this, I would actually *not* be
    conforming to the rest of TextureTransform description in VRML spec.

    Am I the only one fooled by this ? No:
    [http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4320634]

    So what's the truth (i.e. the correct order, consistent
    with the whole rest of TextureTransform specification and with
    other implementations) ?
    Actually the order is the same for
    both VRML 1.0 and 2.0, confirmed by experience (reversed
    order just will not work correctly, e.g. TextureTransform
    with rotation and center 0.5 0.5 fields specified will
    not behave correctly) and other implementations
    (see [http://search.cpan.org/src/LUKKA/FreeWRL-0.14/VRMLFunc.xs]
    function TextureTransform_Rend). }

  Result :=
    TranslationMatrix( Vector3Single(
      FdTranslation.Value[0] + FdCenter.Value[0],
      FdTranslation.Value[1] + FdCenter.Value[1], 0));
  Result := MultMatrices(Result,
    RotationMatrixRad(FdRotation.Value, Vector3Single(0, 0, 1)));
  Result := MultMatrices(Result,
    ScalingMatrix(
      Vector3Single( FdScale.Value[0], FdScale.Value[1], 1 )));
  Result := MultMatrices(Result,
    TranslationMatrix(
      Vector3Single( -FdCenter.Value[0], -FdCenter.Value[1], 0 )));
end;

procedure RegisterTexturingNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeImageTexture,
    TNodeMovieTexture,
    TNodeMultiTexture,
    TNodeMultiTextureCoordinate,
    TNodeMultiTextureTransform,
    TNodePixelTexture,
    TNodeTextureCoordinate,
    TNodeTextureCoordinateGenerator,
    TNodeTextureProperties,
    TNodeTextureTransform
  ]);
end;

{$endif read_implementation}
