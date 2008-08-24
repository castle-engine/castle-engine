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
  TNodeX3DTexture3DNode = class(TNodeX3DTextureNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFdrepeatS: TSFBool;
    public property FdrepeatS: TSFBool read FFdrepeatS;

    private FFdrepeatT: TSFBool;
    public property FdrepeatT: TSFBool read FFdrepeatT;

    private FFdrepeatR: TSFBool;
    public property FdrepeatR: TSFBool read FFdrepeatR;

    private FFdtextureProperties: TSFNode;
    public property FdtextureProperties: TSFNode read FFdtextureProperties;
  end;

  TNodeComposedTexture3D = class(TNodeX3DTexture3DNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdtexture: TMFNode;
    public property Fdtexture: TMFNode read FFdtexture;
  end;

  TNodeImageTexture3D = class(TNodeX3DTexture3DNode, INodeX3DUrlObject)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;
  end;

  TNodePixelTexture3D = class(TNodeX3DTexture3DNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdimage: TMFInt32;
    public property Fdimage: TMFInt32 read FFdimage;
  end;

  TNodeTextureCoordinate3D = class(TNodeX3DTextureCoordinateNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdpoint: TMFVec3f;
    public property Fdpoint: TMFVec3f read FFdpoint;
  end;

  TNodeTextureCoordinate4D = class(TNodeX3DTextureCoordinateNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdpoint: TMFVec4f;
    public property Fdpoint: TMFVec4f read FFdpoint;
  end;

  TNodeTextureTransformMatrix3D = class(TNodeX3DTextureTransformNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdmatrix: TSFMatrix4f;
    public property Fdmatrix: TSFMatrix4f read FFdmatrix;
  end;

  TNodeTextureTransform3D = class(TNodeX3DTextureTransformNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdcenter: TSFVec3f;
    public property Fdcenter: TSFVec3f read FFdcenter;

    private FFdrotation: TSFRotation;
    public property Fdrotation: TSFRotation read FFdrotation;

    private FFdscale: TSFVec3f;
    public property Fdscale: TSFVec3f read FFdscale;

    private FFdtranslation: TSFVec3f;
    public property Fdtranslation: TSFVec3f read FFdtranslation;
  end;

{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeX3DTexture3DNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdrepeatS := TSFBool.Create(Self, 'repeatS', false);
  FFdrepeatS.Exposed := false;
  Fields.Add(FFdrepeatS);

  FFdrepeatT := TSFBool.Create(Self, 'repeatT', false);
  FFdrepeatT.Exposed := false;
  Fields.Add(FFdrepeatT);

  FFdrepeatR := TSFBool.Create(Self, 'repeatR', false);
  FFdrepeatR.Exposed := false;
  Fields.Add(FFdrepeatR);

  FFdtextureProperties := TSFNode.Create(Self, 'textureProperties', [TNodeTextureProperties]);
  FFdtextureProperties.Exposed := false;
  Fields.Add(FFdtextureProperties);
end;

constructor TNodeComposedTexture3D.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdtexture := TMFNode.Create(Self, 'texture', [TNodeX3DTexture2DNode]);
  Fields.Add(FFdtexture);
end;

class function TNodeComposedTexture3D.ClassNodeTypeName: string;
begin
  Result := 'ComposedTexture3D';
end;

function TNodeComposedTexture3D.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeComposedTexture3D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeImageTexture3D.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdurl := TMFString.Create(Self, 'url', []);
  Fields.Add(FFdurl);
  { X3D specification comment: [URI] }
end;

class function TNodeImageTexture3D.ClassNodeTypeName: string;
begin
  Result := 'ImageTexture3D';
end;

function TNodeImageTexture3D.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeImageTexture3D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodePixelTexture3D.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdimage := TMFInt32.Create(Self, 'image', [0, 0, 0, 0]);
  Fields.Add(FFdimage);
end;

class function TNodePixelTexture3D.ClassNodeTypeName: string;
begin
  Result := 'PixelTexture3D';
end;

function TNodePixelTexture3D.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodePixelTexture3D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeTextureCoordinate3D.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdpoint := TMFVec3f.Create(Self, 'point', []);
  Fields.Add(FFdpoint);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TNodeTextureCoordinate3D.ClassNodeTypeName: string;
begin
  Result := 'TextureCoordinate3D';
end;

function TNodeTextureCoordinate3D.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeTextureCoordinate3D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeTextureCoordinate4D.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdpoint := TMFVec4f.Create(Self, 'point', []);
  Fields.Add(FFdpoint);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TNodeTextureCoordinate4D.ClassNodeTypeName: string;
begin
  Result := 'TextureCoordinate4D';
end;

function TNodeTextureCoordinate4D.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeTextureCoordinate4D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeTextureTransformMatrix3D.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdmatrix := TSFMatrix4f.Create(Self, 'matrix', IdentityMatrix4Single);
  Fields.Add(FFdmatrix);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TNodeTextureTransformMatrix3D.ClassNodeTypeName: string;
begin
  Result := 'TextureTransformMatrix3D';
end;

function TNodeTextureTransformMatrix3D.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeTextureTransformMatrix3D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeTextureTransform3D.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcenter := TSFVec3f.Create(Self, 'center', Vector3Single(0, 0, 0));
  Fields.Add(FFdcenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdrotation := TSFRotation.Create(Self, 'rotation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdrotation);
  { X3D specification comment: (-Inf,Inf) }

  FFdscale := TSFVec3f.Create(Self, 'scale', Vector3Single(1, 1, 1));
  Fields.Add(FFdscale);
  { X3D specification comment: (-Inf,Inf) }

  FFdtranslation := TSFVec3f.Create(Self, 'translation', Vector3Single(0, 0, 0));
  Fields.Add(FFdtranslation);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TNodeTextureTransform3D.ClassNodeTypeName: string;
begin
  Result := 'TextureTransform3D';
end;

function TNodeTextureTransform3D.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeTextureTransform3D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterTexturing3DNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeComposedTexture3D,
    TNodeImageTexture3D,
    TNodePixelTexture3D,
    TNodeTextureCoordinate3D,
    TNodeTextureCoordinate4D,
    TNodeTextureTransformMatrix3D,
    TNodeTextureTransform3D
  ]);
end;

{$endif read_implementation}
