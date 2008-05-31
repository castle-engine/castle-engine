{
  Copyright 2008 Michalis Kamburelis.

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

  TNodeX3DTexture2DNode = class(TNodeX3DTextureNode)
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
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;
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
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdimage: TSFImage;
    public property Fdimage: TSFImage read FFdimage;
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
  end;

{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeX3DTextureCoordinateNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'children';
end;

constructor TNodeX3DTextureNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'children';
end;

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

  FFdtextureProperties := TSFNode.Create(Self, 'textureProperties', [TextureProperties]);
  FFdtextureProperties.Exposed := false;
  Fields.Add(FFdtextureProperties);

  DefaultContainerField := 'children';
end;

constructor TNodeX3DTextureTransformNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'children';
end;

constructor TNodeImageTexture.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdurl := TMFString.Create('url', []);
  Fields.Add(FFdurl);
  { X3D specification comment: [URI] }

  DefaultContainerField := 'children';
end;

class function TNodeImageTexture.ClassNodeTypeName: string;
begin
  Result := 'ImageTexture';
end;

function TNodeImageTexture.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeImageTexture.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
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

  DefaultContainerField := 'children';
end;

class function TNodeMovieTexture.ClassNodeTypeName: string;
begin
  Result := 'MovieTexture';
end;

function TNodeMovieTexture.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeMovieTexture.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
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

  FFdtexture := TMFNode.Create(Self, 'texture', [X3DTextureNode]);
  Fields.Add(FFdtexture);

  DefaultContainerField := 'children';
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

  FFdtexCoord := TMFNode.Create(Self, 'texCoord', [X3DTextureCoordinateNode]);
  Fields.Add(FFdtexCoord);

  DefaultContainerField := 'children';
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

  FFdtextureTransform := TMFNode.Create(Self, 'textureTransform', [X3DTextureTransformNode]);
  Fields.Add(FFdtextureTransform);

  DefaultContainerField := 'children';
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

  FFdimage := TSFImage.Create('image', 0);
  Fields.Add(FFdimage);
  { X3D specification comment: 0 0 }

  DefaultContainerField := 'children';
end;

class function TNodePixelTexture.ClassNodeTypeName: string;
begin
  Result := 'PixelTexture';
end;

function TNodePixelTexture.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodePixelTexture.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeTextureCoordinate.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdpoint := TMFVec2f.Create('point', []);
  Fields.Add(FFdpoint);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeTextureCoordinate.ClassNodeTypeName: string;
begin
  Result := 'TextureCoordinate';
end;

function TNodeTextureCoordinate.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeTextureCoordinate.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
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

  DefaultContainerField := 'children';
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

  DefaultContainerField := 'children';
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

  DefaultContainerField := 'children';
end;

class function TNodeTextureTransform.ClassNodeTypeName: string;
begin
  Result := 'TextureTransform';
end;

function TNodeTextureTransform.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeTextureTransform.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
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
