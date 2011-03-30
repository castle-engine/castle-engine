{
  Copyright 2002-2011 Michalis Kamburelis.

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
  TNodeX3DAppearanceChildNode = class(TNodeX3DNode)
  end;

  TNodeX3DAppearanceNode = class(TNodeX3DNode)
  end;

  TNodeX3DMaterialNode = class(TNodeX3DAppearanceChildNode)
  public
    procedure CreateNode; override;
  end;

  TNodeAppearance = class;
  TNodeX3DTextureTransformNode = class;
  TNodeMaterial_2 = class;
  TNodeComposedShader = class;
  TNodeLineProperties = class;

  TNodeX3DShapeNode = class(TNodeX3DChildNode, INodeX3DBoundedObject)
  private
    function GetAppearance: TNodeAppearance;
    procedure SetAppearance(const Value: TNodeAppearance);
    function GetMaterial: TNodeMaterial_2;
    procedure SetMaterial(const Value: TNodeMaterial_2);
    function GetTexture: TNodeX3DTextureNode;
    procedure SetTexture(const Value: TNodeX3DTextureNode);
    function GetTextureTransform: TNodeX3DTextureTransformNode;
    procedure SetTextureTransform(const Value: TNodeX3DTextureTransformNode);
    function GetLineProperties: TNodeLineProperties;
    procedure SetLineProperties(const Value: TNodeLineProperties);
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    procedure CreateNode; override;

    private FFdappearance: TSFNode;
    public property Fdappearance: TSFNode read FFdappearance;

    private FFdgeometry: TSFNode;
    public property Fdgeometry: TSFNode read FFdgeometry;

    private FFdbboxCenter: TSFVec3f;
    public property FdbboxCenter: TSFVec3f read FFdbboxCenter;

    private FFdbboxSize: TSFVec3f;
    public property FdbboxSize: TSFVec3f read FFdbboxSize;

    procedure BeforeTraverse(StateStack: TVRMLGraphTraverseStateStack); override;
    procedure AfterTraverse(StateStack: TVRMLGraphTraverseStateStack); override;

    { Apperance of this shape.

      This is a comfortable shortcut for FdAppearance.Value,
      checking whether it's class is appropriate. }
    property Appearance: TNodeAppearance read GetAppearance write SetAppearance;

    { The texture of this shape.
      This is a shortcut for accessing "texture" field of Apperance,
      that is @code(TNodeAppearance(FdAppearance.Value).FdTexture.Value),
      checking whether nodes are set to appropriate types along the way.

      When setting this to something non-nil, we make sure
      to also set Appearance to something non-nil. }
    property Texture: TNodeX3DTextureNode read GetTexture write SetTexture;

    { Texture transformation of this shape.
      Similar to @link(Texture), this is just a comfortable property
      for getting and setting the appropriate Appearance field,
      checking class types along the way. }
    property TextureTransform: TNodeX3DTextureTransformNode
      read GetTextureTransform write SetTextureTransform;

    { Get / set the (simple, one-sided) material of this shape.
      This is a shortcut for accessing "material" field of Apperance,
      that is @code(TNodeAppearance(FdAppearance.Value).FdMaterial.Value),
      checking whether nodes are set to appropriate types along the way.

      When setting this material to something non-nil, we make sure
      to also set Appearance to something non-nil. }
    property Material: TNodeMaterial_2 read GetMaterial write SetMaterial;

    { Line properties of this shape. This is a clean, type-safe way
      for accessing Appearance.lineProperties field of this shape. }
    property LineProperties: TNodeLineProperties
      read GetLineProperties write SetLineProperties;

    private FFdoctreeTriangles: TSFNode;
    public property FdoctreeTriangles: TSFNode read FFdoctreeTriangles;
  end;

  TMFNodeShaders = class(TMFNode)
  protected
    function ExposedEventsFieldClass: TVRMLFieldClass; override;
  public
    { Returns Items[Index], if it's a shader node suitable
      for GLSL  shader. Otherwise returns @nil. Checks the class of
      Items[Index] and it's @code(language) field. }
    function GLSLShader(Index: Integer): TNodeComposedShader;
  end;

  TNodeAppearance = class(TNodeX3DAppearanceNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    procedure CreateNode; override;
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

    private FFdshaders: TMFNodeShaders;
    public property Fdshaders: TMFNodeShaders read FFdshaders;

    private FFdtexture: TSFNode;
    public property Fdtexture: TSFNode read FFdtexture;

    private FFdtextureTransform: TSFNode;
    public property FdtextureTransform: TSFNode read FFdtextureTransform;

    private FFdReceiveShadows: TMFNode;
    public property FdReceiveShadows: TMFNode read FFdReceiveShadows;

    private FFdShadowCaster: TSFBool;
    public property FdShadowCaster: TSFBool read FFdShadowCaster;

    private FFdEffects: TMFNode;
    public property FdEffects: TMFNode read FFdEffects;

    private FFdNormalMap: TSFNode;
    public property FdNormalMap: TSFNode read FFdNormalMap;

    private FFdHeightMap: TSFNode;
    public property FdHeightMap: TSFNode read FFdHeightMap;

    private FFdHeightMapScale: TSFFloat;
    public property FdHeightMapScale: TSFFloat read FFdHeightMapScale;

    { blendMode property.

      This is modelled after Avalon extension on [http://www.instantreality.org/].
      See [http://www.instantreality.org/documentation/nodetype/ManagedAppearance/]
      and [http://www.instantreality.org/documentation/nodetype/BlendMode/].
      It allows you to use BlendMode nodes, which I consider very useful. }
    private FFdBlendMode: TSFNode;
    public property FdBlendMode: TSFNode read FFdBlendMode;
  end;

  TNodeFillProperties = class(TNodeX3DAppearanceChildNode)
  public
    procedure CreateNode; override;
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
    procedure CreateNode; override;
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

  TVRMLMaterialInfo_2 = class;

  TNodeMaterial_2 = class(TNodeX3DMaterialNode)
  public
    procedure CreateNode; override;
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

    { Create material information based on this node.
      Returned TVRMLMaterialInfo is valid only as long as this node instance. }
    function MaterialInfo: TVRMLMaterialInfo_2;

    private FFdReflSpecular: TMFColor;
    public property FdReflSpecular: TMFColor read FFdReflSpecular;

    private FFdReflDiffuse: TMFColor;
    public property FdReflDiffuse: TMFColor read FFdReflDiffuse;

    private FFdTransSpecular: TMFColor;
    public property FdTransSpecular: TMFColor read FFdTransSpecular;

    private FFdTransDiffuse: TMFColor;
    public property FdTransDiffuse: TMFColor read FFdTransDiffuse;

    private FFdReflSpecularExp: TSFFloat;
    public property FdReflSpecularExp: TSFFloat read FFdReflSpecularExp;

    private FFdTransSpecularExp: TSFFloat;
    public property FdTransSpecularExp: TSFFloat read FFdTransSpecularExp;
  end;

  { VRML material information, usable for all VRML versions.

    VRML 1.0 and VRML >= 2.0 materials work a little differently:
    VRML 1.0 has arrays of values, e.g. an array of diffuse color,
    while VRML >= 2.0 is really a single material, e.g. one diffuse color.
    This class is designed to hide these differences as much as possible:
    it provides a simple common interface for getting material properties.

    Note that it doesn't try to abstract all material properties ---
    there are some features that are simply fundamentally different
    in how VRML 1.0 and >= 2.0 materials should be treated, and they
    cannot be abstracted under a common interface here. }
  TVRMLMaterialInfo = class
  protected
    { Calculate physical material properties using standard VRML material
      fields. See [http://127.0.0.1/~michalis/vrmlengine/kambi_vrml_extensions.php#section_ext_material_phong_brdf_fields].

      These should be used by descendants implementation,
      for example CalculateReflSpecular should be used by descendant
      ReflSpecular when material node doesn't specify any value
      in FdReflSpecular field.

      @groupBegin }
    procedure CalculateReflSpecular(var V: TVector3Single);
    procedure CalculateReflDiffuse(var V: TVector3Single);
    procedure CalculateTransSpecular(var V: TVector3Single);
    procedure CalculateTransDiffuse(var V: TVector3Single);
    { @groupEnd }
  public
    function DiffuseColor: TVector3Single; virtual; abstract;
    function Mirror: Single; virtual; abstract;
    function Transparency: Single; virtual; abstract;

    function ReflSpecular: TVector3Single; virtual; abstract;
    function ReflDiffuse: TVector3Single; virtual; abstract;
    function TransSpecular: TVector3Single; virtual; abstract;
    function TransDiffuse: TVector3Single; virtual; abstract;

    function ReflSpecularExp: Single; virtual; abstract;
    function TransSpecularExp: Single; virtual; abstract;
  end;

  TVRMLMaterialInfo_2 = class(TVRMLMaterialInfo)
  private
    FNode: TNodeMaterial_2;
  public
    constructor Create(Node: TNodeMaterial_2);

    function DiffuseColor: TVector3Single; override;
    function Mirror: Single; override;
    function Transparency: Single; override;

    function ReflSpecular: TVector3Single; override;
    function ReflDiffuse: TVector3Single; override;
    function TransSpecular: TVector3Single; override;
    function TransDiffuse: TVector3Single; override;

    function ReflSpecularExp: Single; override;
    function TransSpecularExp: Single; override;
  end;

  TNodeShape = class(TNodeX3DShapeNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeTwoSidedMaterial = class(TNodeX3DMaterialNode)
  public
    procedure CreateNode; override;
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
procedure TNodeX3DMaterialNode.CreateNode;
begin
  inherited;

  { This is not actually specified anywhere (X3D XML encoding spec
    doesn't specify containerField for abstract X3DXxxNode classes)
    but it seems most sensible. }

  DefaultContainerField := 'material';
end;

procedure TNodeX3DShapeNode.CreateNode;
begin
  inherited;

  FFdappearance := TSFNode.Create(Self, 'appearance', [TNodeX3DAppearanceNode]);
   Fdappearance.ChangesAlways := [chEverything];
  Fields.Add(FFdappearance);

  FFdgeometry := TSFNode.Create(Self, 'geometry', [TNodeX3DGeometryNode]);
   Fdgeometry.ChangesAlways := [chEverything];
  Fields.Add(FFdgeometry);

  FFdbboxCenter := TSFVec3f.Create(Self, 'bboxCenter', Vector3Single(0, 0, 0));
   FdbboxCenter.Exposed := false;
  Fields.Add(FFdbboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdbboxSize := TSFVec3f.Create(Self, 'bboxSize', Vector3Single(-1, -1, -1));
   FdbboxSize.Exposed := false;
  Fields.Add(FFdbboxSize);
  { X3D specification comment: [0,Inf) or -1 -1 -1 }

  FFdoctreeTriangles := TSFNode.Create(Self, 'octreeTriangles', [TNodeKambiOctreeProperties]);
   FdoctreeTriangles.Exposed := false;
   FdoctreeTriangles.ChangesAlways := [chEverything];
  Fields.Add(FFdoctreeTriangles);
end;

procedure TNodeX3DShapeNode.DirectEnumerateActive(
  Func: TEnumerateChildrenFunction);
begin
  { According to VRML spec, when geometry is NULL then object is not
    drawn so appearance doesn't matter. }

  if FdGeometry.CurrentChildAllowed and
     (FdGeometry.Value <> nil) then
  begin
    FdAppearance.EnumerateValid(Func);

    { We could also do now
        FdGeometry.EnumerateValid(Func);
      but actually we already checked conditions, so we can just call Func
      directly. }
    Func(Self, FdGeometry.Value);
  end;
end;

procedure TNodeX3DShapeNode.BeforeTraverse(StateStack: TVRMLGraphTraverseStateStack);
begin
  inherited;
  StateStack.Top.ShapeNode := Self;
end;

procedure TNodeX3DShapeNode.AfterTraverse(StateStack: TVRMLGraphTraverseStateStack);
begin
  StateStack.Top.ShapeNode := nil;
  inherited;
end;

function TNodeX3DShapeNode.GetAppearance: TNodeAppearance;
begin
  if (FdAppearance.Value <> nil) and
     (FdAppearance.Value is TNodeAppearance) then
    Result := TNodeAppearance(FdAppearance.Value) else
    Result := nil;
end;

procedure TNodeX3DShapeNode.SetAppearance(const Value: TNodeAppearance);
begin
  FdAppearance.Value := Value;
end;

function TNodeX3DShapeNode.GetMaterial: TNodeMaterial_2;
var
  App: TNodeAppearance;
begin
  App := Appearance;
  if (App <> nil) and
     (App.FdMaterial.Value <> nil) and
     (App.FdMaterial.Value is TNodeMaterial_2) then
    Result := TNodeMaterial_2(App.FdMaterial.Value) else
    Result := nil;
end;

procedure TNodeX3DShapeNode.SetMaterial(const Value: TNodeMaterial_2);
var
  App: TNodeAppearance;
begin
  App := Appearance;
  if App <> nil then
    App.FdMaterial.Value := Value else
  if Value <> nil then
  begin
    App := TNodeAppearance.Create('', WWWBasePath);
    App.Scene := Scene;
    Appearance := App;
    App.FdMaterial.Value := Value;
  end;
end;

function TNodeX3DShapeNode.GetTexture: TNodeX3DTextureNode;
var
  App: TNodeAppearance;
begin
  App := Appearance;
  if (App <> nil) and
     (App.FdTexture.Value <> nil) and
     (App.FdTexture.Value is TNodeX3DTextureNode) then
    Result := TNodeX3DTextureNode(App.FdTexture.Value) else
    Result := nil;
end;

procedure TNodeX3DShapeNode.SetTexture(const Value: TNodeX3DTextureNode);
var
  App: TNodeAppearance;
begin
  App := Appearance;
  if App <> nil then
    App.FdTexture.Value := Value else
  if Value <> nil then
  begin
    App := TNodeAppearance.Create('', WWWBasePath);
    App.Scene := Scene;
    Appearance := App;
    App.FdTexture.Value := Value;
  end;
end;

function TNodeX3DShapeNode.GetTextureTransform: TNodeX3DTextureTransformNode;
var
  App: TNodeAppearance;
begin
  App := Appearance;
  if (App <> nil) and
     (App.FdTextureTransform.Value <> nil) and
     (App.FdTextureTransform.Value is TNodeX3DTextureTransformNode) then
    Result := TNodeX3DTextureTransformNode(App.FdTextureTransform.Value) else
    Result := nil;
end;

procedure TNodeX3DShapeNode.SetTextureTransform(const Value: TNodeX3DTextureTransformNode);
var
  App: TNodeAppearance;
begin
  App := Appearance;
  if App <> nil then
    App.FdTextureTransform.Value := Value else
  if Value <> nil then
  begin
    App := TNodeAppearance.Create('', WWWBasePath);
    App.Scene := Scene;
    Appearance := App;
    App.FdTextureTransform.Value := Value;
  end;
end;

function TNodeX3DShapeNode.GetLineProperties: TNodeLineProperties;
var
  App: TNodeAppearance;
begin
  App := Appearance;
  if (App <> nil) and
     (App.FdLineProperties.Value <> nil) and
     (App.FdLineProperties.Value is TNodeLineProperties) then
    Result := TNodeLineProperties(App.FdLineProperties.Value) else
    Result := nil;
end;

procedure TNodeX3DShapeNode.SetLineProperties(const Value: TNodeLineProperties);
var
  App: TNodeAppearance;
begin
  App := Appearance;
  if App <> nil then
    App.FdLineProperties.Value := Value else
  if Value <> nil then
  begin
    App := TNodeAppearance.Create('', WWWBasePath);
    App.Scene := Scene;
    Appearance := App;
    App.FdLineProperties.Value := Value;
  end;
end;

procedure TNodeAppearance.CreateNode;
begin
  inherited;

  FFdfillProperties := TSFNode.Create(Self, 'fillProperties', [TNodeFillProperties]);
   FdfillProperties.ChangesAlways := [chEverything];
  Fields.Add(FFdfillProperties);

  FFdlineProperties := TSFNode.Create(Self, 'lineProperties', [TNodeLineProperties]);
   FdlineProperties.ChangesAlways := [chEverything];
  Fields.Add(FFdlineProperties);

  FFdmaterial := TSFNode.Create(Self, 'material', [TNodeX3DMaterialNode]);
   Fdmaterial.ChangesAlways := [chEverything];
  Fields.Add(FFdmaterial);

  FFdshaders := TMFNodeShaders.Create(Self, 'shaders', [TNodeX3DShaderNode]);
   Fdshaders.ChangesAlways := [chEverything];
  Fields.Add(FFdshaders);

  FFdtexture := TSFNode.Create(Self, 'texture', [TNodeX3DTextureNode]);
   Fdtexture.ChangesAlways := [chEverything];
  Fields.Add(FFdtexture);

  FFdtextureTransform := TSFNode.Create(Self, 'textureTransform', [TNodeX3DTextureTransformNode]);
   FdtextureTransform.ChangesAlways := [chEverything];
  Fields.Add(FFdtextureTransform);

  FFdReceiveShadows := TMFNode.Create(Self, 'receiveShadows', [TNodeX3DLightNode]);
   FdReceiveShadows.Exposed := false;
   FdReceiveShadows.ChangesAlways := [chEverything];
  Fields.Add(FFdReceiveShadows);

  FFdShadowCaster := TSFBool.Create(Self, 'shadowCaster', true);
   FdShadowCaster.ChangesAlways := [chShadowCasters];
  Fields.Add(FFdShadowCaster);

  FFdnormalMap := TSFNode.Create(Self, 'normalMap', [TNodeX3DTextureNode]);
   FdnormalMap.ChangesAlways := [chEverything];
  Fields.Add(FFdnormalMap);

  FFdheightMap := TSFNode.Create(Self, 'heightMap', [TNodeX3DTextureNode]);
   FdheightMap.ChangesAlways := [chEverything];
  Fields.Add(FFdheightMap);

  FFdheightMapScale := TSFFloat.Create(Self, 'heightMapScale', DefaultHeightMapScale);
   FdheightMapScale.ChangesAlways := [chEverything];
  Fields.Add(FFdheightMapScale);

  FFdblendMode := TSFNode.Create(Self, 'blendMode', [TNodeBlendMode]);
   FdblendMode.ChangesAlways := [chEverything];
  Fields.Add(FFdblendMode);

  FFdEffects := TMFNode.Create(Self, 'effects', [TNodeEffect]);
   FdEffects.Exposed := false;
   FdEffects.ChangesAlways := [chEverything];
  Fields.Add(FFdEffects);

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

function TMFNodeShaders.ExposedEventsFieldClass: TVRMLFieldClass;
begin
  Result := TMFNode;
end;

function TMFNodeShaders.GLSLShader(Index: Integer): TNodeComposedShader;
begin
  if Items[Index] is TNodeComposedShader then
  begin
    Result := TNodeComposedShader(Items[Index]);
    if not ((Result.FdLanguage.Value = '') or
            (Result.FdLanguage.Value = 'GLSL')) then
      Result := nil;
  end else
    Result := nil;
end;

procedure TNodeAppearance.DirectEnumerateActive(
  Func: TEnumerateChildrenFunction);
begin
  FFdfillProperties.EnumerateValid(Func);
  FFdlineProperties.EnumerateValid(Func);
  FFdmaterial.EnumerateValid(Func);
  FFdshaders.EnumerateValid(Func);
  FFdtexture.EnumerateValid(Func);
  FFdtextureTransform.EnumerateValid(Func);
end;

procedure TNodeFillProperties.CreateNode;
begin
  inherited;

  FFdfilled := TSFBool.Create(Self, 'filled', true);
  Fields.Add(FFdfilled);

  FFdhatchColor := TSFColor.Create(Self, 'hatchColor', Vector3Single(1, 1, 1));
  Fields.Add(FFdhatchColor);
  { X3D specification comment: [0,1] }

  FFdhatched := TSFBool.Create(Self, 'hatched', true);
  Fields.Add(FFdhatched);

  FFdhatchStyle := TSFInt32.Create(Self, 'hatchStyle', 1);
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

procedure TNodeLineProperties.CreateNode;
begin
  inherited;

  FFdapplied := TSFBool.Create(Self, 'applied', true);
  Fields.Add(FFdapplied);

  FFdlinetype := TSFInt32.Create(Self, 'linetype', 1);
  Fields.Add(FFdlinetype);
  { X3D specification comment: [1,Inf) }

  FFdlinewidthScaleFactor := TSFFloat.Create(Self, 'linewidthScaleFactor', 0);
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

procedure TNodeMaterial_2.CreateNode;
begin
  inherited;

  FFdambientIntensity := TSFFloat.Create(Self, 'ambientIntensity', DefaultMaterial_2AmbientIntensity);
   FdambientIntensity.ChangesAlways := [chMaterial2];
  Fields.Add(FFdambientIntensity);
  { X3D specification comment: [0,1] }

  FFddiffuseColor := TSFColor.Create(Self, 'diffuseColor', DefaultMaterialDiffuseColor);
   FddiffuseColor.ChangesAlways := [chMaterial2];
  Fields.Add(FFddiffuseColor);
  { X3D specification comment: [0,1] }

  FFdemissiveColor := TSFColor.Create(Self, 'emissiveColor', DefaultMaterialEmissiveColor);
   FdemissiveColor.ChangesAlways := [chMaterial2];
  Fields.Add(FFdemissiveColor);
  { X3D specification comment: [0,1] }

  FFdshininess := TSFFloat.Create(Self, 'shininess', DefaultMaterialShininess);
   Fdshininess.ChangesAlways := [chMaterial2];
  Fields.Add(FFdshininess);
  { X3D specification comment: [0,1] }

  FFdspecularColor := TSFColor.Create(Self, 'specularColor', DefaultMaterialSpecularColor);
   FdspecularColor.ChangesAlways := [chMaterial2];
  Fields.Add(FFdspecularColor);
  { X3D specification comment: [0,1] }

  FFdtransparency := TSFFloat.Create(Self, 'transparency', DefaultMaterialTransparency);
   Fdtransparency.ChangesAlways := [chMaterial2, chUseBlending];
  Fields.Add(FFdtransparency);
  { X3D specification comment: [0,1] }

  FFdfogImmune := TSFBool.Create(Self, 'fogImmune', false);
   FdfogImmune.ChangesAlways := [chMaterial2];
  Fields.Add(FFdfogImmune);

  FFdmirror := TSFFloat.Create(Self, 'mirror', DefaultMaterialMirror);
   Fdmirror.ChangesAlways := [chMaterial2];
  Fields.Add(FFdmirror);

  FFdreflSpecular := TMFColor.Create(Self, 'reflSpecular', []);
   FdreflSpecular.ChangesAlways := [chMaterial2];
  Fields.Add(FFdreflSpecular);

  FFdreflDiffuse := TMFColor.Create(Self, 'reflDiffuse', []);
   FdreflDiffuse.ChangesAlways := [chMaterial2];
  Fields.Add(FFdreflDiffuse);

  FFdtransSpecular := TMFColor.Create(Self, 'transSpecular', []);
   FdtransSpecular.ChangesAlways := [chMaterial2];
  Fields.Add(FFdtransSpecular);

  FFdtransDiffuse := TMFColor.Create(Self, 'transDiffuse', []);
   FdtransDiffuse.ChangesAlways := [chMaterial2];
  Fields.Add(FFdtransDiffuse);

  FFdreflSpecularExp := TSFFloat.Create(Self, 'reflSpecularExp', DefaultMaterialReflSpecularExp);
   FdreflSpecularExp.ChangesAlways := [chMaterial2];
  Fields.Add(FFdreflSpecularExp);

  FFdtransSpecularExp := TSFFloat.Create(Self, 'transSpecularExp', DefaultMaterialTransSpecularExp);
   FdtransSpecularExp.ChangesAlways := [chMaterial2];
  Fields.Add(FFdtransSpecularExp);
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

function TNodeMaterial_2.MaterialInfo: TVRMLMaterialInfo_2;
begin
  Result := TVRMLMaterialInfo_2.Create(Self);
end;

{ TVRMLMaterialInfo ---------------------------------------------------------- }

procedure TVRMLMaterialInfo.CalculateReflSpecular(var V: TVector3Single);
begin
  V[0] := Mirror;
  V[1] := V[0];
  V[2] := V[0];
end;

procedure TVRMLMaterialInfo.CalculateReflDiffuse(var V: TVector3Single);
begin
  V := DiffuseColor;
end;

procedure TVRMLMaterialInfo.CalculateTransSpecular(var V: TVector3Single);
begin
  V[0] := Transparency;
  V[1] := V[0];
  V[2] := V[0];
end;

procedure TVRMLMaterialInfo.CalculateTransDiffuse(var V: TVector3Single);
begin
  V := VectorScale(DiffuseColor, Transparency);
end;

{ TVRMLMaterialInfo_2 -------------------------------------------------------- }

constructor TVRMLMaterialInfo_2.Create(Node: TNodeMaterial_2);
begin
  inherited Create;
  FNode := Node;
end;

function TVRMLMaterialInfo_2.DiffuseColor: TVector3Single;
begin
  Result := FNode.FdDiffuseColor.Value;
end;

function TVRMLMaterialInfo_2.Mirror: Single;
begin
  Result := FNode.FdMirror.Value;
end;

function TVRMLMaterialInfo_2.Transparency: Single;
begin
  Result := FNode.FdTransparency.Value;
end;

function TVRMLMaterialInfo_2.ReflSpecular: TVector3Single;
var
  A: TDynVector3SingleArray;
begin
  A := FNode.FdReflSpecular.Items;
  if A.Count = 0 then
    CalculateReflSpecular(Result) else
    Result := A.Items[0];
end;

function TVRMLMaterialInfo_2.ReflDiffuse: TVector3Single;
var
  A: TDynVector3SingleArray;
begin
  A := FNode.FdReflDiffuse.Items;
  if A.Count = 0 then
    CalculateReflDiffuse(Result) else
    Result := A.Items[0];
end;

function TVRMLMaterialInfo_2.TransSpecular: TVector3Single;
var
  A: TDynVector3SingleArray;
begin
  A := FNode.FdTransSpecular.Items;
  if A.Count = 0 then
    CalculateTransSpecular(Result) else
    Result := A.Items[0];
end;

function TVRMLMaterialInfo_2.TransDiffuse: TVector3Single;
var
  A: TDynVector3SingleArray;
begin
  A := FNode.FdTransDiffuse.Items;
  if A.Count = 0 then
    CalculateTransDiffuse(Result) else
    Result := A.Items[0];
end;

function TVRMLMaterialInfo_2.ReflSpecularExp: Single;
begin
  Result := FNode.FdReflSpecularExp.Value;
end;

function TVRMLMaterialInfo_2.TransSpecularExp: Single;
begin
  Result := FNode.FdTransSpecularExp.Value;
end;

{ TNodeShape ----------------------------------------------------------------- }

procedure TNodeShape.CreateNode;
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

procedure TNodeTwoSidedMaterial.CreateNode;
begin
  inherited;

  FFdambientIntensity := TSFFloat.Create(Self, 'ambientIntensity', 0.2);
   FdambientIntensity.ChangesAlways := [chMaterial2];
  Fields.Add(FFdambientIntensity);
  { X3D specification comment: [0,1] }

  FFdbackAmbientIntensity := TSFFloat.Create(Self, 'backAmbientIntensity', 0.2);
   FdbackAmbientIntensity.ChangesAlways := [chMaterial2];
  Fields.Add(FFdbackAmbientIntensity);
  { X3D specification comment: [0,1] }

  FFdbackDiffuseColor := TSFColor.Create(Self, 'backDiffuseColor', Vector3Single(0.8, 0.8, 0.8));
   FdbackDiffuseColor.ChangesAlways := [chMaterial2];
  Fields.Add(FFdbackDiffuseColor);
  { X3D specification comment: [0,1] }

  FFdbackEmissiveColor := TSFColor.Create(Self, 'backEmissiveColor', Vector3Single(0, 0, 0));
   FdbackEmissiveColor.ChangesAlways := [chMaterial2];
  Fields.Add(FFdbackEmissiveColor);
  { X3D specification comment: [0,1] }

  FFdbackShininess := TSFFloat.Create(Self, 'backShininess', 0.2);
   FdbackShininess.ChangesAlways := [chMaterial2];
  Fields.Add(FFdbackShininess);
  { X3D specification comment: [0,1] }

  FFdbackSpecularColor := TSFColor.Create(Self, 'backSpecularColor', Vector3Single(0, 0, 0));
   FdbackSpecularColor.ChangesAlways := [chMaterial2];
  Fields.Add(FFdbackSpecularColor);
  { X3D specification comment: [0,1] }

  FFdbackTransparency := TSFFloat.Create(Self, 'backTransparency', 0);
   FdbackTransparency.ChangesAlways := [chMaterial2, chUseBlending];
  Fields.Add(FFdbackTransparency);
  { X3D specification comment: [0,1] }

  FFddiffuseColor := TSFColor.Create(Self, 'diffuseColor', Vector3Single(0.8, 0.8, 0.8));
   FddiffuseColor.ChangesAlways := [chMaterial2];
  Fields.Add(FFddiffuseColor);
  { X3D specification comment: [0,1] }

  FFdemissiveColor := TSFColor.Create(Self, 'emissiveColor', Vector3Single(0, 0, 0));
   FdemissiveColor.ChangesAlways := [chMaterial2];
  Fields.Add(FFdemissiveColor);
  { X3D specification comment: [0,1] }

  FFdshininess := TSFFloat.Create(Self, 'shininess', 0.2);
   Fdshininess.ChangesAlways := [chMaterial2];
  Fields.Add(FFdshininess);
  { X3D specification comment: [0,1] }

  FFdseparateBackColor := TSFBool.Create(Self, 'separateBackColor', false);
   FdseparateBackColor.ChangesAlways := [chMaterial2];
  Fields.Add(FFdseparateBackColor);

  FFdspecularColor := TSFColor.Create(Self, 'specularColor', Vector3Single(0, 0, 0));
   FdspecularColor.ChangesAlways := [chMaterial2];
  Fields.Add(FFdspecularColor);
  { X3D specification comment: [0,1] }

  FFdtransparency := TSFFloat.Create(Self, 'transparency', 0);
   Fdtransparency.ChangesAlways := [chMaterial2, chUseBlending];
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
