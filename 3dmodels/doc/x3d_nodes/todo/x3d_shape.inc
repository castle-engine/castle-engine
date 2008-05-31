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
  TNodeX3DAppearanceChildNode = class(TNodeX3DNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
  end;

  TNodeX3DAppearanceNode = class(TNodeX3DNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
  end;

  TNodeX3DMaterialNode = class(TNodeX3DAppearanceChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
  end;

  TNodeX3DShapeNode = class(TNodeX3DChildNode, INodeX3DBoundedObject)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFdappearance: TSFNode;
    public property Fdappearance: TSFNode read FFdappearance;

    private FFdgeometry: TSFNode;
    public property Fdgeometry: TSFNode read FFdgeometry;

    private FFdbboxCenter: TSFVec3f;
    public property FdbboxCenter: TSFVec3f read FFdbboxCenter;

    private FFdbboxSize: TSFVec3f;
    public property FdbboxSize: TSFVec3f read FFdbboxSize;
  end;

  TNodeAppearance = class(TNodeX3DAppearanceNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdfillProperties: TSFNode;
    public property FdfillProperties: TSFNode read FFdfillProperties;

    private FFdlineProperties: TSFNode;
    public property FdlineProperties: TSFNode read FFdlineProperties;

    private FFdmaterial: TSFNode;
    public property Fdmaterial: TSFNode read FFdmaterial;

    private FFdshaders: TMFNode;
    public property Fdshaders: TMFNode read FFdshaders;

    private FFdtexture: TSFNode;
    public property Fdtexture: TSFNode read FFdtexture;

    private FFdtextureTransform: TSFNode;
    public property FdtextureTransform: TSFNode read FFdtextureTransform;
  end;

  TNodeFillProperties = class(TNodeX3DAppearanceChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdfilled: TSFBool;
    public property Fdfilled: TSFBool read FFdfilled;

    private FFdhatchColor: TSFColor;
    public property FdhatchColor: TSFColor read FFdhatchColor;

    private FFdhatched: TSFBool;
    public property Fdhatched: TSFBool read FFdhatched;

    private FFdhatchStyle: TSFInt32;
    public property FdhatchStyle: TSFInt32 read FFdhatchStyle;
  end;

  TNodeLineProperties = class(TNodeX3DAppearanceChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdapplied: TSFBool;
    public property Fdapplied: TSFBool read FFdapplied;

    private FFdlinetype: TSFInt32;
    public property Fdlinetype: TSFInt32 read FFdlinetype;

    private FFdlinewidthScaleFactor: TSFFloat;
    public property FdlinewidthScaleFactor: TSFFloat read FFdlinewidthScaleFactor;
  end;

  TNodeMaterial = class(TNodeX3DMaterialNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdambientIntensity: TSFFloat;
    public property FdambientIntensity: TSFFloat read FFdambientIntensity;

    private FFddiffuseColor: TSFColor;
    public property FddiffuseColor: TSFColor read FFddiffuseColor;

    private FFdemissiveColor: TSFColor;
    public property FdemissiveColor: TSFColor read FFdemissiveColor;

    private FFdshininess: TSFFloat;
    public property Fdshininess: TSFFloat read FFdshininess;

    private FFdspecularColor: TSFColor;
    public property FdspecularColor: TSFColor read FFdspecularColor;

    private FFdtransparency: TSFFloat;
    public property Fdtransparency: TSFFloat read FFdtransparency;
  end;

  TNodeShape = class(TNodeX3DShapeNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeTwoSidedMaterial = class(TNodeX3DMaterialNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdambientIntensity: TSFFloat;
    public property FdambientIntensity: TSFFloat read FFdambientIntensity;

    private FFdbackAmbientIntensity: TSFFloat;
    public property FdbackAmbientIntensity: TSFFloat read FFdbackAmbientIntensity;

    private FFdbackDiffuseColor: TSFColor;
    public property FdbackDiffuseColor: TSFColor read FFdbackDiffuseColor;

    private FFdbackEmissiveColor: TSFColor;
    public property FdbackEmissiveColor: TSFColor read FFdbackEmissiveColor;

    private FFdbackShininess: TSFFloat;
    public property FdbackShininess: TSFFloat read FFdbackShininess;

    private FFdbackSpecularColor: TSFColor;
    public property FdbackSpecularColor: TSFColor read FFdbackSpecularColor;

    private FFdbackTransparency: TSFFloat;
    public property FdbackTransparency: TSFFloat read FFdbackTransparency;

    private FFddiffuseColor: TSFColor;
    public property FddiffuseColor: TSFColor read FFddiffuseColor;

    private FFdemissiveColor: TSFColor;
    public property FdemissiveColor: TSFColor read FFdemissiveColor;

    private FFdshininess: TSFFloat;
    public property Fdshininess: TSFFloat read FFdshininess;

    private FFdseparateBackColor: TSFBool;
    public property FdseparateBackColor: TSFBool read FFdseparateBackColor;

    private FFdspecularColor: TSFColor;
    public property FdspecularColor: TSFColor read FFdspecularColor;

    private FFdtransparency: TSFFloat;
    public property Fdtransparency: TSFFloat read FFdtransparency;
  end;

{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeX3DAppearanceChildNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'children';
end;

constructor TNodeX3DAppearanceNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'children';
end;

constructor TNodeX3DMaterialNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'children';
end;

constructor TNodeX3DShapeNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdappearance := TSFNode.Create(Self, 'appearance', [X3DAppearanceNode]);
  Fields.Add(FFdappearance);

  FFdgeometry := TSFNode.Create(Self, 'geometry', [X3DGeometryNode]);
  Fields.Add(FFdgeometry);

  FFdbboxCenter := TSFVec3f.Create('bboxCenter', Vector3Single(0, 0, 0));
  FFdbboxCenter.Exposed := false;
  Fields.Add(FFdbboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdbboxSize := TSFVec3f.Create('bboxSize', Vector3Single(-1, -1, -1));
  FFdbboxSize.Exposed := false;
  Fields.Add(FFdbboxSize);
  { X3D specification comment: [0,Inf) or -1 -1 -1 }

  DefaultContainerField := 'children';
end;

constructor TNodeAppearance.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdfillProperties := TSFNode.Create(Self, 'fillProperties', [FillProperties]);
  Fields.Add(FFdfillProperties);

  FFdlineProperties := TSFNode.Create(Self, 'lineProperties', [LineProperties]);
  Fields.Add(FFdlineProperties);

  FFdmaterial := TSFNode.Create(Self, 'material', [X3DMaterialNode]);
  Fields.Add(FFdmaterial);

  FFdshaders := TMFNode.Create(Self, 'shaders', [X3DShaderNode]);
  Fields.Add(FFdshaders);

  FFdtexture := TSFNode.Create(Self, 'texture', [X3DTextureNode]);
  Fields.Add(FFdtexture);

  FFdtextureTransform := TSFNode.Create(Self, 'textureTransform', [X3DTextureTransformNode]);
  Fields.Add(FFdtextureTransform);

  DefaultContainerField := 'children';
end;

class function TNodeAppearance.ClassNodeTypeName: string;
begin
  Result := 'Appearance';
end;

function TNodeAppearance.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeAppearance.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeFillProperties.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdfilled := TSFBool.Create('filled', true);
  Fields.Add(FFdfilled);

  FFdhatchColor := TSFColor.Create('hatchColor', Vector3Single(1, 1, 1));
  Fields.Add(FFdhatchColor);
  { X3D specification comment: [0,1] }

  FFdhatched := TSFBool.Create('hatched', true);
  Fields.Add(FFdhatched);

  FFdhatchStyle := TSFInt32.Create('hatchStyle', 1);
  Fields.Add(FFdhatchStyle);
  { X3D specification comment: [0,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeFillProperties.ClassNodeTypeName: string;
begin
  Result := 'FillProperties';
end;

function TNodeFillProperties.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeFillProperties.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeLineProperties.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdapplied := TSFBool.Create('applied', true);
  Fields.Add(FFdapplied);

  FFdlinetype := TSFInt32.Create('linetype', 1);
  Fields.Add(FFdlinetype);
  { X3D specification comment: [1,Inf) }

  FFdlinewidthScaleFactor := TSFFloat.Create('linewidthScaleFactor', 0);
  Fields.Add(FFdlinewidthScaleFactor);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeLineProperties.ClassNodeTypeName: string;
begin
  Result := 'LineProperties';
end;

function TNodeLineProperties.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeLineProperties.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeMaterial.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdambientIntensity := TSFFloat.Create('ambientIntensity', 0.2);
  Fields.Add(FFdambientIntensity);
  { X3D specification comment: [0,1] }

  FFddiffuseColor := TSFColor.Create('diffuseColor', Vector3Single(0.8, 0.8, 0.8));
  Fields.Add(FFddiffuseColor);
  { X3D specification comment: [0,1] }

  FFdemissiveColor := TSFColor.Create('emissiveColor', Vector3Single(0, 0, 0));
  Fields.Add(FFdemissiveColor);
  { X3D specification comment: [0,1] }

  FFdshininess := TSFFloat.Create('shininess', 0.2);
  Fields.Add(FFdshininess);
  { X3D specification comment: [0,1] }

  FFdspecularColor := TSFColor.Create('specularColor', Vector3Single(0, 0, 0));
  Fields.Add(FFdspecularColor);
  { X3D specification comment: [0,1] }

  FFdtransparency := TSFFloat.Create('transparency', 0);
  Fields.Add(FFdtransparency);
  { X3D specification comment: [0,1] }

  DefaultContainerField := 'children';
end;

class function TNodeMaterial.ClassNodeTypeName: string;
begin
  Result := 'Material';
end;

function TNodeMaterial.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeMaterial.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeShape.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'children';
end;

class function TNodeShape.ClassNodeTypeName: string;
begin
  Result := 'Shape';
end;

function TNodeShape.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeShape.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeTwoSidedMaterial.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdambientIntensity := TSFFloat.Create('ambientIntensity', 0.2);
  Fields.Add(FFdambientIntensity);
  { X3D specification comment: [0,1] }

  FFdbackAmbientIntensity := TSFFloat.Create('backAmbientIntensity', 0.2);
  Fields.Add(FFdbackAmbientIntensity);
  { X3D specification comment: [0,1] }

  FFdbackDiffuseColor := TSFColor.Create('backDiffuseColor', Vector3Single(0.8, 0.8, 0.8));
  Fields.Add(FFdbackDiffuseColor);
  { X3D specification comment: [0,1] }

  FFdbackEmissiveColor := TSFColor.Create('backEmissiveColor', Vector3Single(0, 0, 0));
  Fields.Add(FFdbackEmissiveColor);
  { X3D specification comment: [0,1] }

  FFdbackShininess := TSFFloat.Create('backShininess', 0.2);
  Fields.Add(FFdbackShininess);
  { X3D specification comment: [0,1] }

  FFdbackSpecularColor := TSFColor.Create('backSpecularColor', Vector3Single(0, 0, 0));
  Fields.Add(FFdbackSpecularColor);
  { X3D specification comment: [0,1] }

  FFdbackTransparency := TSFFloat.Create('backTransparency', 0);
  Fields.Add(FFdbackTransparency);
  { X3D specification comment: [0,1] }

  FFddiffuseColor := TSFColor.Create('diffuseColor', Vector3Single(0.8, 0.8, 0.8));
  Fields.Add(FFddiffuseColor);
  { X3D specification comment: [0,1] }

  FFdemissiveColor := TSFColor.Create('emissiveColor', Vector3Single(0, 0, 0));
  Fields.Add(FFdemissiveColor);
  { X3D specification comment: [0,1] }

  FFdshininess := TSFFloat.Create('shininess', 0.2);
  Fields.Add(FFdshininess);
  { X3D specification comment: [0,1] }

  FFdseparateBackColor := TSFBool.Create('separateBackColor', false);
  Fields.Add(FFdseparateBackColor);

  FFdspecularColor := TSFColor.Create('specularColor', Vector3Single(0, 0, 0));
  Fields.Add(FFdspecularColor);
  { X3D specification comment: [0,1] }

  FFdtransparency := TSFFloat.Create('transparency', 0);
  Fields.Add(FFdtransparency);
  { X3D specification comment: [0,1] }

  DefaultContainerField := 'children';
end;

class function TNodeTwoSidedMaterial.ClassNodeTypeName: string;
begin
  Result := 'TwoSidedMaterial';
end;

function TNodeTwoSidedMaterial.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeTwoSidedMaterial.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterShapeNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeAppearance,
    TNodeFillProperties,
    TNodeLineProperties,
    TNodeMaterial,
    TNodeShape,
    TNodeTwoSidedMaterial
  ]);
end;

{$endif read_implementation}
