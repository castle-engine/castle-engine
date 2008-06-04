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
  TNodeX3DAppearanceChildNode = class(TNodeX3DNode)
  end;

  TNodeX3DAppearanceNode = class(TNodeX3DNode)
  end;

  TNodeX3DMaterialNode = class(TNodeX3DAppearanceChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
  end;

  TNodeAppearance = class;
  TNodeTextureTransform = class;
  TNodeMaterial_2 = class;
  TNodeComposedShader = class;

  TNodeX3DShapeNode = class(TNodeX3DChildNode, INodeX3DBoundedObject)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
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

    procedure BeforeTraverse(var State: TVRMLGraphTraverseState); override;
    procedure AfterTraverse(var State: TVRMLGraphTraverseState); override;

    { This is a shortcut for FdAppearance.Value.
      If anything makes this impossible (Apperance field is NULL, or wrong class)
      then returns @nil. }
    function Appearance: TNodeAppearance;

    { This is a shortcut for FdAppearance.Value.FdTexture.Value.
      If anything makes this impossible (Apperance field is NULL,
      or Appearance.Texture field is NULL, or wrong node class is
      passed as Appearance or Texture node), then returns nil. }
    function Texture: TVRMLTextureNode;

    { This is like @link(Texture), but it returns normalMap field
      of KambiAppearance. }
    function NormalMap: TVRMLTextureNode;

    { This is like @link(Texture), but it returns heightMap field
      of KambiAppearance. }
    function HeightMap: TVRMLTextureNode;

    function HeightMapScale: Single;

    { This is like @link(Texture), but it returns TextureTransform
      of Apperance. }
    function TextureTransform: TNodeTextureTransform;

    { This is like @link(Texture), but it returns Material
      of Apperance. }
    function Material: TNodeMaterial_2;
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

    { This returns FdShaders.Items[Index], if it's a shader node suitable for GLSL
      shader. Otherwise returns @nil. This checks the class of
      FdShaders.Items[Index] and it's @code(language) field. }
    function GLSLShader(Index: Integer): TNodeComposedShader;
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

  TNodeMaterial_2 = class(TNodeX3DMaterialNode)
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

    { Kambi extensions below, TODO: should be separated into
      different KambiMaterial node. } { }

    private FFdFogImmune: TSFBool;
    public property FdFogImmune: TSFBool read FFdFogImmune;

    private FFdMirror: TSFFloat;
    public property FdMirror: TSFFloat read FFdMirror;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    { Opacity is just a 1 - FdTransparency.Value.
      Defined for your comfort --- for
      OpenGL you will usually want to pass Opacity, not Transparency. }
    function Opacity: Single;

    { ShininessExp is just 128 * FdShininess.Value, this is the "real"
      exponent indicated by shininess field value.
      Defined for your comfort --- for any graphic library you will usually
      want to pass the "real" exponent given by this function, not just
      value of shininess field. }
    function ShininessExp: Single;
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
constructor TNodeX3DMaterialNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  { This is not actually specified anywhere (X3D XML encoding spec
    doesn't specify containerField for abstract X3DXxxNode classes)
    but it seems most sensible. }

  DefaultContainerField := 'material';
end;

constructor TNodeX3DShapeNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdappearance := TSFNode.Create(Self, 'appearance', [TNodeX3DAppearanceNode]);
  Fields.Add(FFdappearance);

  FFdgeometry := TSFNode.Create(Self, 'geometry', [TNodeX3DGeometryNode]);
  Fields.Add(FFdgeometry);

  FFdbboxCenter := TSFVec3f.Create('bboxCenter', Vector3Single(0, 0, 0));
  FFdbboxCenter.Exposed := false;
  Fields.Add(FFdbboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdbboxSize := TSFVec3f.Create('bboxSize', Vector3Single(-1, -1, -1));
  FFdbboxSize.Exposed := false;
  Fields.Add(FFdbboxSize);
  { X3D specification comment: [0,Inf) or -1 -1 -1 }
end;

procedure TNodeX3DShapeNode.DirectEnumerateActive(
  Func: TEnumerateChildrenFunction);
begin
  if FdGeometry.Value <> nil then
  begin
    { According to VRML spec, when geometry is NULL then object is not
      drawn so appearance doesn't matter. }
    if FdAppearance.Value <> nil then
      Func(Self, FdAppearance.Value);
    Func(Self, FdGeometry.Value);
  end;
end;

procedure TNodeX3DShapeNode.BeforeTraverse(var State: TVRMLGraphTraverseState);
begin
  inherited;
  State.ParentShape := Self;
end;

procedure TNodeX3DShapeNode.AfterTraverse(var State: TVRMLGraphTraverseState);
begin
  State.ParentShape := nil;
  inherited;
end;

function TNodeX3DShapeNode.Appearance: TNodeAppearance;
begin
  if (FdAppearance.Value <> nil) and
     (FdAppearance.Value is TNodeAppearance) then
    Result := TNodeAppearance(FdAppearance.Value) else
    Result := nil;
end;

function TNodeX3DShapeNode.Texture: TVRMLTextureNode;
var
  A: TNodeAppearance;
begin
  Result := nil;
  if (FdAppearance.Value <> nil) and
     (FdAppearance.Value is TNodeAppearance) then
  begin
    A := TNodeAppearance(FdAppearance.Value);
    if (A.FdTexture.Value <> nil) and
       (A.FdTexture.Value is TVRMLTextureNode) then
      Result := TVRMLTextureNode(A.FdTexture.Value);
  end;
end;

function TNodeX3DShapeNode.NormalMap: TVRMLTextureNode;
var
  A: TNodeKambiAppearance;
begin
  Result := nil;
  if (FdAppearance.Value <> nil) and
     (FdAppearance.Value is TNodeKambiAppearance) then
  begin
    A := TNodeKambiAppearance(FdAppearance.Value);
    if (A.FdNormalMap.Value <> nil) and
       (A.FdNormalMap.Value is TVRMLTextureNode) then
      Result := TVRMLTextureNode(A.FdNormalMap.Value);
  end;
end;

function TNodeX3DShapeNode.HeightMap: TVRMLTextureNode;
var
  A: TNodeKambiAppearance;
begin
  Result := nil;
  if (FdAppearance.Value <> nil) and
     (FdAppearance.Value is TNodeKambiAppearance) then
  begin
    A := TNodeKambiAppearance(FdAppearance.Value);
    if (A.FdHeightMap.Value <> nil) and
       (A.FdHeightMap.Value is TVRMLTextureNode) then
      Result := TVRMLTextureNode(A.FdHeightMap.Value);
  end;
end;

function TNodeX3DShapeNode.HeightMapScale: Single;
var
  A: TNodeKambiAppearance;
begin
  if (FdAppearance.Value <> nil) and
     (FdAppearance.Value is TNodeKambiAppearance) then
  begin
    A := TNodeKambiAppearance(FdAppearance.Value);
    Result := A.FdHeightMapScale.Value;
  end else
    Result := DefaultHeightMapScale;
end;

function TNodeX3DShapeNode.TextureTransform: TNodeTextureTransform;
var
  A: TNodeAppearance;
begin
  Result := nil;
  if (FdAppearance.Value <> nil) and
     (FdAppearance.Value is TNodeAppearance) then
  begin
    A := TNodeAppearance(FdAppearance.Value);
    if (A.FdTextureTransform.Value <> nil) and
       (A.FdTextureTransform.Value is TNodeTextureTransform) then
      Result := TNodeTextureTransform(A.FdTextureTransform.Value);
  end;
end;

function TNodeX3DShapeNode.Material: TNodeMaterial_2;
var
  A: TNodeAppearance;
begin
  Result := nil;
  if (FdAppearance.Value <> nil) and
     (FdAppearance.Value is TNodeAppearance) then
  begin
    A := TNodeAppearance(FdAppearance.Value);
    if (A.FdMaterial.Value <> nil) and
       (A.FdMaterial.Value is TNodeMaterial_2) then
      Result := TNodeMaterial_2(A.FdMaterial.Value);
  end;
end;

constructor TNodeAppearance.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdfillProperties := TSFNode.Create(Self, 'fillProperties', [TNodeFillProperties]);
  Fields.Add(FFdfillProperties);

  FFdlineProperties := TSFNode.Create(Self, 'lineProperties', [TNodeLineProperties]);
  Fields.Add(FFdlineProperties);

  FFdmaterial := TSFNode.Create(Self, 'material', [TNodeX3DMaterialNode]);
  Fields.Add(FFdmaterial);

  FFdshaders := TMFNode.Create(Self, 'shaders', [TNodeX3DShaderNode]);
  Fields.Add(FFdshaders);

  FFdtexture := TSFNode.Create(Self, 'texture', [TNodeX3DTextureNode]);
  Fields.Add(FFdtexture);

  FFdtextureTransform := TSFNode.Create(Self, 'textureTransform', [TNodeX3DTextureTransformNode]);
  Fields.Add(FFdtextureTransform);

  { In edition 2 of X3D XML encoding, this is empty... but in earlier
    versions, this was "appearance" and this seems more sensible,
    Appearance node may only occur within Shape.appearance field
    so it should definitely have DefaultContainerField set. }
  DefaultContainerField := 'appearance';
end;

class function TNodeAppearance.ClassNodeTypeName: string;
begin
  Result := 'Appearance';
end;

function TNodeAppearance.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeAppearance.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TNodeAppearance.GLSLShader(Index: Integer): TNodeComposedShader;
begin
  if FdShaders.Items[Index] is TNodeComposedShader then
  begin
    Result := TNodeComposedShader(FdShaders.Items[Index]);
    if not ((Result.FdLanguage.Value = '') or
            (Result.FdLanguage.Value = 'GLSL')) then
      Result := nil;
  end else
    Result := nil;
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

  DefaultContainerField := 'fillProperties';
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

  DefaultContainerField := 'lineProperties';
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

constructor TNodeMaterial_2.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdambientIntensity := TSFFloat.Create('ambientIntensity', DefaultMaterial_2AmbientIntensity);
  Fields.Add(FFdambientIntensity);
  { X3D specification comment: [0,1] }

  FFddiffuseColor := TSFColor.Create('diffuseColor', DefaultMaterialDiffuseColor);
  Fields.Add(FFddiffuseColor);
  { X3D specification comment: [0,1] }

  FFdemissiveColor := TSFColor.Create('emissiveColor', DefaultMaterialEmissiveColor);
  Fields.Add(FFdemissiveColor);
  { X3D specification comment: [0,1] }

  FFdshininess := TSFFloat.Create('shininess', DefaultMaterialShininess);
  Fields.Add(FFdshininess);
  { X3D specification comment: [0,1] }

  FFdspecularColor := TSFColor.Create('specularColor', DefaultMaterialSpecularColor);
  Fields.Add(FFdspecularColor);
  { X3D specification comment: [0,1] }

  FFdtransparency := TSFFloat.Create('transparency', DefaultMaterialTransparency);
  Fields.Add(FFdtransparency);
  { X3D specification comment: [0,1] }

  FFdfogImmune := TSFBool.Create('fogImmune', false);
  Fields.Add(FFdfogImmune);

  FFdmirror := TSFFloat.Create('mirror', DefaultMaterialMirror);
  Fields.Add(FFdmirror);
end;

class function TNodeMaterial_2.ClassNodeTypeName: string;
begin
  Result := 'Material';
end;

function TNodeMaterial_2.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeMaterial_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TNodeMaterial_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 2;
end;

function TNodeMaterial_2.Opacity: Single;
begin
  Result := 1- FdTransparency.Value;
end;

function TNodeMaterial_2.ShininessExp: Single;
begin
  Result := Clamped(FdShininess.Value * 128.0, 0.0, 128.0);
end;

constructor TNodeShape.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;
end;

class function TNodeShape.ClassNodeTypeName: string;
begin
  Result := 'Shape';
end;

function TNodeShape.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeShape.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
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
    TNodeMaterial_2,
    TNodeShape,
    TNodeTwoSidedMaterial
  ]);
end;

{$endif read_implementation}
