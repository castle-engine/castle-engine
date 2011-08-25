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
  TAbstractX3DAppearanceChildNode = class(TAbstractX3DNode)
  end;

  TAbstractX3DAppearanceNode = class(TAbstractX3DNode)
  end;

  TAbstractX3DMaterialNode = class(TAbstractX3DAppearanceChildNode)
  public
    procedure CreateNode; override;
  end;

  TAppearanceNode = class;
  TAbstractX3DTextureTransformNode = class;
  TMaterialNode = class;
  TComposedShaderNode = class;
  TLinePropertiesNode = class;

  TAbstractX3DShapeNode = class(TAbstractX3DChildNode, IAbstractX3DBoundedObject)
  private
    function GetAppearance: TAppearanceNode;
    procedure SetAppearance(const Value: TAppearanceNode);
    function GetMaterial: TMaterialNode;
    procedure SetMaterial(const Value: TMaterialNode);
    function GetTexture: TAbstractX3DTextureNode;
    procedure SetTexture(const Value: TAbstractX3DTextureNode);
    function GetTextureTransform: TAbstractX3DTextureTransformNode;
    procedure SetTextureTransform(const Value: TAbstractX3DTextureTransformNode);
    function GetLineProperties: TLinePropertiesNode;
    procedure SetLineProperties(const Value: TLinePropertiesNode);
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    procedure CreateNode; override;

    private FFdAppearance: TSFNode;
    public property FdAppearance: TSFNode read FFdAppearance;

    private FFdGeometry: TSFNode;
    public property FdGeometry: TSFNode read FFdGeometry;

    private FFdBboxCenter: TSFVec3f;
    public property FdBboxCenter: TSFVec3f read FFdBboxCenter;

    private FFdBboxSize: TSFVec3f;
    public property FdBboxSize: TSFVec3f read FFdBboxSize;

    procedure BeforeTraverse(StateStack: TVRMLGraphTraverseStateStack); override;
    procedure AfterTraverse(StateStack: TVRMLGraphTraverseStateStack); override;

    { Apperance of this shape.

      This is a comfortable shortcut for FdAppearance.Value,
      checking whether it's class is appropriate. }
    property Appearance: TAppearanceNode read GetAppearance write SetAppearance;

    { The texture of this shape.
      This is a shortcut for accessing "texture" field of Apperance,
      that is @code(TAppearanceNode(FdAppearance.Value).FdTexture.Value),
      checking whether nodes are set to appropriate types along the way.

      When setting this to something non-nil, we make sure
      to also set Appearance to something non-nil. }
    property Texture: TAbstractX3DTextureNode read GetTexture write SetTexture;

    { Texture transformation of this shape.
      Similar to @link(Texture), this is just a comfortable property
      for getting and setting the appropriate Appearance field,
      checking class types along the way. }
    property TextureTransform: TAbstractX3DTextureTransformNode
      read GetTextureTransform write SetTextureTransform;

    { Get / set the (simple, one-sided) material of this shape.
      This is a shortcut for accessing "material" field of Apperance,
      that is @code(TAppearanceNode(FdAppearance.Value).FdMaterial.Value),
      checking whether nodes are set to appropriate types along the way.

      When setting this material to something non-nil, we make sure
      to also set Appearance to something non-nil. }
    property Material: TMaterialNode read GetMaterial write SetMaterial;

    { Line properties of this shape. This is a clean, type-safe way
      for accessing Appearance.lineProperties field of this shape. }
    property LineProperties: TLinePropertiesNode
      read GetLineProperties write SetLineProperties;

    private FFdOctreeTriangles: TSFNode;
    public property FdOctreeTriangles: TSFNode read FFdOctreeTriangles;
  end;

  TMFNodeShaders = class(TMFNode)
  protected
    function ExposedEventsFieldClass: TVRMLFieldClass; override;
  public
    { Returns Items[Index], if it's a shader node suitable
      for GLSL  shader. Otherwise returns @nil. Checks the class of
      Items[Index] and it's @code(language) field. }
    function GLSLShader(Index: Integer): TComposedShaderNode;
  end;

  TAppearanceNode = class(TAbstractX3DAppearanceNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdFillProperties: TSFNode;
    public property FdFillProperties: TSFNode read FFdFillProperties;

    private FFdLineProperties: TSFNode;
    public property FdLineProperties: TSFNode read FFdLineProperties;

    private FFdMaterial: TSFNode;
    public property FdMaterial: TSFNode read FFdMaterial;

    private FFdShaders: TMFNodeShaders;
    public property FdShaders: TMFNodeShaders read FFdShaders;

    private FFdTexture: TSFNode;
    public property FdTexture: TSFNode read FFdTexture;

    private FFdTextureTransform: TSFNode;
    public property FdTextureTransform: TSFNode read FFdTextureTransform;

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

  TFillPropertiesNode = class(TAbstractX3DAppearanceChildNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdFilled: TSFBool;
    public property FdFilled: TSFBool read FFdFilled;

    private FFdHatchColor: TSFColor;
    public property FdHatchColor: TSFColor read FFdHatchColor;

    private FFdHatched: TSFBool;
    public property FdHatched: TSFBool read FFdHatched;

    private FFdHatchStyle: TSFInt32;
    public property FdHatchStyle: TSFInt32 read FFdHatchStyle;
  end;

  TLinePropertiesNode = class(TAbstractX3DAppearanceChildNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdApplied: TSFBool;
    public property FdApplied: TSFBool read FFdApplied;

    private FFdLinetype: TSFInt32;
    public property FdLinetype: TSFInt32 read FFdLinetype;

    private FFdLinewidthScaleFactor: TSFFloat;
    public property FdLinewidthScaleFactor: TSFFloat read FFdLinewidthScaleFactor;
  end;

  TVRMLMaterialInfo = class;

  TMaterialNode = class(TAbstractX3DMaterialNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdAmbientIntensity: TSFFloat;
    public property FdAmbientIntensity: TSFFloat read FFdAmbientIntensity;

    private FFdDiffuseColor: TSFColor;
    public property FdDiffuseColor: TSFColor read FFdDiffuseColor;

    private FFdEmissiveColor: TSFColor;
    public property FdEmissiveColor: TSFColor read FFdEmissiveColor;

    private FFdShininess: TSFFloat;
    public property FdShininess: TSFFloat read FFdShininess;

    private FFdSpecularColor: TSFColor;
    public property FdSpecularColor: TSFColor read FFdSpecularColor;

    private FFdTransparency: TSFFloat;
    public property FdTransparency: TSFFloat read FFdTransparency;

    { Kambi extensions below, TODO: should be separated into
      different KambiMaterial node. } { }

    private FFdFogImmune: TSFBool;
    public property FdFogImmune: TSFBool read FFdFogImmune;

    private FFdMirror: TSFFloat;
    public property FdMirror: TSFFloat read FFdMirror;

    class function ForVRMLVersion(const Version: TVRMLVersion): boolean;
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
    function MaterialInfo: TVRMLMaterialInfo;

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
  TMaterialNode_2 = TMaterialNode;

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
  TVRMLMaterialInfoAbstract = class
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

  TVRMLMaterialInfo = class(TVRMLMaterialInfoAbstract)
  private
    FNode: TMaterialNode;
  public
    constructor Create(Node: TMaterialNode);

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

  TShapeNode = class(TAbstractX3DShapeNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TTwoSidedMaterialNode = class(TAbstractX3DMaterialNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdAmbientIntensity: TSFFloat;
    public property FdAmbientIntensity: TSFFloat read FFdAmbientIntensity;

    private FFdBackAmbientIntensity: TSFFloat;
    public property FdBackAmbientIntensity: TSFFloat read FFdBackAmbientIntensity;

    private FFdBackDiffuseColor: TSFColor;
    public property FdBackDiffuseColor: TSFColor read FFdBackDiffuseColor;

    private FFdBackEmissiveColor: TSFColor;
    public property FdBackEmissiveColor: TSFColor read FFdBackEmissiveColor;

    private FFdBackShininess: TSFFloat;
    public property FdBackShininess: TSFFloat read FFdBackShininess;

    private FFdBackSpecularColor: TSFColor;
    public property FdBackSpecularColor: TSFColor read FFdBackSpecularColor;

    private FFdBackTransparency: TSFFloat;
    public property FdBackTransparency: TSFFloat read FFdBackTransparency;

    private FFdDiffuseColor: TSFColor;
    public property FdDiffuseColor: TSFColor read FFdDiffuseColor;

    private FFdEmissiveColor: TSFColor;
    public property FdEmissiveColor: TSFColor read FFdEmissiveColor;

    private FFdShininess: TSFFloat;
    public property FdShininess: TSFFloat read FFdShininess;

    private FFdSeparateBackColor: TSFBool;
    public property FdSeparateBackColor: TSFBool read FFdSeparateBackColor;

    private FFdSpecularColor: TSFColor;
    public property FdSpecularColor: TSFColor read FFdSpecularColor;

    private FFdTransparency: TSFFloat;
    public property FdTransparency: TSFFloat read FFdTransparency;
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TAbstractX3DMaterialNode.CreateNode;
begin
  inherited;

  { This is not actually specified anywhere (X3D XML encoding spec
    doesn't specify containerField for abstract X3DXxxNode classes)
    but it seems most sensible. }

  DefaultContainerField := 'material';
end;

procedure TAbstractX3DShapeNode.CreateNode;
begin
  inherited;

  FFdAppearance := TSFNode.Create(Self, 'appearance', [TAbstractX3DAppearanceNode]);
   FdAppearance.ChangesAlways := [chEverything];
  Fields.Add(FFdAppearance);

  FFdGeometry := TSFNode.Create(Self, 'geometry', [TAbstractX3DGeometryNode]);
   FdGeometry.ChangesAlways := [chEverything];
  Fields.Add(FFdGeometry);

  FFdBboxCenter := TSFVec3f.Create(Self, 'bboxCenter', Vector3Single(0, 0, 0));
   FdBboxCenter.Exposed := false;
  Fields.Add(FFdBboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdBboxSize := TSFVec3f.Create(Self, 'bboxSize', Vector3Single(-1, -1, -1));
   FdBboxSize.Exposed := false;
  Fields.Add(FFdBboxSize);
  { X3D specification comment: [0,Inf) or -1 -1 -1 }

  FFdOctreeTriangles := TSFNode.Create(Self, 'octreeTriangles', [TKambiOctreePropertiesNode]);
   FdOctreeTriangles.Exposed := false;
   FdOctreeTriangles.ChangesAlways := [chEverything];
  Fields.Add(FFdOctreeTriangles);
end;

procedure TAbstractX3DShapeNode.DirectEnumerateActive(
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

procedure TAbstractX3DShapeNode.BeforeTraverse(StateStack: TVRMLGraphTraverseStateStack);
begin
  inherited;
  StateStack.Top.ShapeNode := Self;
end;

procedure TAbstractX3DShapeNode.AfterTraverse(StateStack: TVRMLGraphTraverseStateStack);
begin
  StateStack.Top.ShapeNode := nil;
  inherited;
end;

function TAbstractX3DShapeNode.GetAppearance: TAppearanceNode;
begin
  if (FdAppearance.Value <> nil) and
     (FdAppearance.Value is TAppearanceNode) then
    Result := TAppearanceNode(FdAppearance.Value) else
    Result := nil;
end;

procedure TAbstractX3DShapeNode.SetAppearance(const Value: TAppearanceNode);
begin
  FdAppearance.Value := Value;
end;

function TAbstractX3DShapeNode.GetMaterial: TMaterialNode;
var
  App: TAppearanceNode;
begin
  App := Appearance;
  if (App <> nil) and
     (App.FdMaterial.Value <> nil) and
     (App.FdMaterial.Value is TMaterialNode) then
    Result := TMaterialNode(App.FdMaterial.Value) else
    Result := nil;
end;

procedure TAbstractX3DShapeNode.SetMaterial(const Value: TMaterialNode);
var
  App: TAppearanceNode;
begin
  App := Appearance;
  if App <> nil then
    App.FdMaterial.Value := Value else
  if Value <> nil then
  begin
    App := TAppearanceNode.Create('', WWWBasePath);
    App.Scene := Scene;
    Appearance := App;
    App.FdMaterial.Value := Value;
  end;
end;

function TAbstractX3DShapeNode.GetTexture: TAbstractX3DTextureNode;
var
  App: TAppearanceNode;
begin
  App := Appearance;
  if (App <> nil) and
     (App.FdTexture.Value <> nil) and
     (App.FdTexture.Value is TAbstractX3DTextureNode) then
    Result := TAbstractX3DTextureNode(App.FdTexture.Value) else
    Result := nil;
end;

procedure TAbstractX3DShapeNode.SetTexture(const Value: TAbstractX3DTextureNode);
var
  App: TAppearanceNode;
begin
  App := Appearance;
  if App <> nil then
    App.FdTexture.Value := Value else
  if Value <> nil then
  begin
    App := TAppearanceNode.Create('', WWWBasePath);
    App.Scene := Scene;
    Appearance := App;
    App.FdTexture.Value := Value;
  end;
end;

function TAbstractX3DShapeNode.GetTextureTransform: TAbstractX3DTextureTransformNode;
var
  App: TAppearanceNode;
begin
  App := Appearance;
  if (App <> nil) and
     (App.FdTextureTransform.Value <> nil) and
     (App.FdTextureTransform.Value is TAbstractX3DTextureTransformNode) then
    Result := TAbstractX3DTextureTransformNode(App.FdTextureTransform.Value) else
    Result := nil;
end;

procedure TAbstractX3DShapeNode.SetTextureTransform(const Value: TAbstractX3DTextureTransformNode);
var
  App: TAppearanceNode;
begin
  App := Appearance;
  if App <> nil then
    App.FdTextureTransform.Value := Value else
  if Value <> nil then
  begin
    App := TAppearanceNode.Create('', WWWBasePath);
    App.Scene := Scene;
    Appearance := App;
    App.FdTextureTransform.Value := Value;
  end;
end;

function TAbstractX3DShapeNode.GetLineProperties: TLinePropertiesNode;
var
  App: TAppearanceNode;
begin
  App := Appearance;
  if (App <> nil) and
     (App.FdLineProperties.Value <> nil) and
     (App.FdLineProperties.Value is TLinePropertiesNode) then
    Result := TLinePropertiesNode(App.FdLineProperties.Value) else
    Result := nil;
end;

procedure TAbstractX3DShapeNode.SetLineProperties(const Value: TLinePropertiesNode);
var
  App: TAppearanceNode;
begin
  App := Appearance;
  if App <> nil then
    App.FdLineProperties.Value := Value else
  if Value <> nil then
  begin
    App := TAppearanceNode.Create('', WWWBasePath);
    App.Scene := Scene;
    Appearance := App;
    App.FdLineProperties.Value := Value;
  end;
end;

procedure TAppearanceNode.CreateNode;
begin
  inherited;

  FFdFillProperties := TSFNode.Create(Self, 'fillProperties', [TFillPropertiesNode]);
   FdFillProperties.ChangesAlways := [chEverything];
  Fields.Add(FFdFillProperties);

  FFdLineProperties := TSFNode.Create(Self, 'lineProperties', [TLinePropertiesNode]);
   FdLineProperties.ChangesAlways := [chEverything];
  Fields.Add(FFdLineProperties);

  FFdMaterial := TSFNode.Create(Self, 'material', [TAbstractX3DMaterialNode]);
   FdMaterial.ChangesAlways := [chEverything];
  Fields.Add(FFdMaterial);

  FFdShaders := TMFNodeShaders.Create(Self, 'shaders', [TAbstractX3DShaderNode]);
   FdShaders.ChangesAlways := [chEverything];
  Fields.Add(FFdShaders);

  FFdTexture := TSFNode.Create(Self, 'texture', [TAbstractX3DTextureNode]);
   FdTexture.ChangesAlways := [chEverything];
  Fields.Add(FFdTexture);

  FFdTextureTransform := TSFNode.Create(Self, 'textureTransform', [TAbstractX3DTextureTransformNode]);
   FdTextureTransform.ChangesAlways := [chEverything];
  Fields.Add(FFdTextureTransform);

  FFdReceiveShadows := TMFNode.Create(Self, 'receiveShadows', [TAbstractX3DLightNode]);
   FdReceiveShadows.Exposed := false;
   FdReceiveShadows.ChangesAlways := [chEverything];
  Fields.Add(FFdReceiveShadows);

  FFdShadowCaster := TSFBool.Create(Self, 'shadowCaster', true);
   FdShadowCaster.ChangesAlways := [chShadowCasters];
  Fields.Add(FFdShadowCaster);

  FFdNormalMap := TSFNode.Create(Self, 'normalMap', [TAbstractX3DTextureNode]);
   FdNormalMap.ChangesAlways := [chEverything];
  Fields.Add(FFdNormalMap);

  FFdHeightMap := TSFNode.Create(Self, 'heightMap', [TAbstractX3DTextureNode]);
   FdHeightMap.ChangesAlways := [chEverything];
  Fields.Add(FFdHeightMap);

  FFdHeightMapScale := TSFFloat.Create(Self, 'heightMapScale', DefaultHeightMapScale);
   FdHeightMapScale.ChangesAlways := [chEverything];
  Fields.Add(FFdHeightMapScale);

  FFdBlendMode := TSFNode.Create(Self, 'blendMode', [TBlendModeNode]);
   FdBlendMode.ChangesAlways := [chEverything];
  Fields.Add(FFdBlendMode);

  FFdEffects := TMFNode.Create(Self, 'effects', [TEffectNode]);
   FdEffects.Exposed := false;
   FdEffects.ChangesAlways := [chEverything];
  Fields.Add(FFdEffects);

  { In edition 2 of X3D XML encoding, this is empty... but in earlier
    versions, this was "appearance" and this seems more sensible,
    Appearance node may only occur within Shape.appearance field
    so it should definitely have DefaultContainerField set. }
  DefaultContainerField := 'appearance';
end;

class function TAppearanceNode.ClassNodeTypeName: string;
begin
  Result := 'Appearance';
end;

class function TAppearanceNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TMFNodeShaders.ExposedEventsFieldClass: TVRMLFieldClass;
begin
  Result := TMFNode;
end;

function TMFNodeShaders.GLSLShader(Index: Integer): TComposedShaderNode;
begin
  if Items[Index] is TComposedShaderNode then
  begin
    Result := TComposedShaderNode(Items[Index]);
    if not ((Result.FdLanguage.Value = '') or
            (Result.FdLanguage.Value = 'GLSL')) then
      Result := nil;
  end else
    Result := nil;
end;

procedure TAppearanceNode.DirectEnumerateActive(
  Func: TEnumerateChildrenFunction);
begin
  FFdFillProperties.EnumerateValid(Func);
  FFdLineProperties.EnumerateValid(Func);
  FFdMaterial.EnumerateValid(Func);
  FFdShaders.EnumerateValid(Func);
  FFdTexture.EnumerateValid(Func);
  FFdTextureTransform.EnumerateValid(Func);
end;

procedure TFillPropertiesNode.CreateNode;
begin
  inherited;

  FFdFilled := TSFBool.Create(Self, 'filled', true);
  Fields.Add(FFdFilled);

  FFdHatchColor := TSFColor.Create(Self, 'hatchColor', Vector3Single(1, 1, 1));
  Fields.Add(FFdHatchColor);
  { X3D specification comment: [0,1] }

  FFdHatched := TSFBool.Create(Self, 'hatched', true);
  Fields.Add(FFdHatched);

  FFdHatchStyle := TSFInt32.Create(Self, 'hatchStyle', 1);
  Fields.Add(FFdHatchStyle);
  { X3D specification comment: [0,Inf) }

  DefaultContainerField := 'fillProperties';
end;

class function TFillPropertiesNode.ClassNodeTypeName: string;
begin
  Result := 'FillProperties';
end;

class function TFillPropertiesNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TLinePropertiesNode.CreateNode;
begin
  inherited;

  FFdApplied := TSFBool.Create(Self, 'applied', true);
  Fields.Add(FFdApplied);

  FFdLinetype := TSFInt32.Create(Self, 'linetype', 1);
  Fields.Add(FFdLinetype);
  { X3D specification comment: [1,Inf) }

  FFdLinewidthScaleFactor := TSFFloat.Create(Self, 'linewidthScaleFactor', 0);
  Fields.Add(FFdLinewidthScaleFactor);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'lineProperties';
end;

class function TLinePropertiesNode.ClassNodeTypeName: string;
begin
  Result := 'LineProperties';
end;

class function TLinePropertiesNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TMaterialNode.CreateNode;
begin
  inherited;

  FFdAmbientIntensity := TSFFloat.Create(Self, 'ambientIntensity', DefaultMaterialAmbientIntensity);
   FdAmbientIntensity.ChangesAlways := [chMaterial2];
  Fields.Add(FFdAmbientIntensity);
  { X3D specification comment: [0,1] }

  FFdDiffuseColor := TSFColor.Create(Self, 'diffuseColor', DefaultMaterialDiffuseColor);
   FdDiffuseColor.ChangesAlways := [chMaterial2];
  Fields.Add(FFdDiffuseColor);
  { X3D specification comment: [0,1] }

  FFdEmissiveColor := TSFColor.Create(Self, 'emissiveColor', DefaultMaterialEmissiveColor);
   FdEmissiveColor.ChangesAlways := [chMaterial2];
  Fields.Add(FFdEmissiveColor);
  { X3D specification comment: [0,1] }

  FFdShininess := TSFFloat.Create(Self, 'shininess', DefaultMaterialShininess);
   FdShininess.ChangesAlways := [chMaterial2];
  Fields.Add(FFdShininess);
  { X3D specification comment: [0,1] }

  FFdSpecularColor := TSFColor.Create(Self, 'specularColor', DefaultMaterialSpecularColor);
   FdSpecularColor.ChangesAlways := [chMaterial2];
  Fields.Add(FFdSpecularColor);
  { X3D specification comment: [0,1] }

  FFdTransparency := TSFFloat.Create(Self, 'transparency', DefaultMaterialTransparency);
   FdTransparency.ChangesAlways := [chMaterial2, chUseBlending];
  Fields.Add(FFdTransparency);
  { X3D specification comment: [0,1] }

  FFdFogImmune := TSFBool.Create(Self, 'fogImmune', false);
   FdFogImmune.ChangesAlways := [chMaterial2];
  Fields.Add(FFdFogImmune);

  FFdMirror := TSFFloat.Create(Self, 'mirror', DefaultMaterialMirror);
   FdMirror.ChangesAlways := [chMaterial2];
  Fields.Add(FFdMirror);

  FFdReflSpecular := TMFColor.Create(Self, 'reflSpecular', []);
   FdReflSpecular.ChangesAlways := [chMaterial2];
  Fields.Add(FFdReflSpecular);

  FFdReflDiffuse := TMFColor.Create(Self, 'reflDiffuse', []);
   FdReflDiffuse.ChangesAlways := [chMaterial2];
  Fields.Add(FFdReflDiffuse);

  FFdTransSpecular := TMFColor.Create(Self, 'transSpecular', []);
   FdTransSpecular.ChangesAlways := [chMaterial2];
  Fields.Add(FFdTransSpecular);

  FFdTransDiffuse := TMFColor.Create(Self, 'transDiffuse', []);
   FdTransDiffuse.ChangesAlways := [chMaterial2];
  Fields.Add(FFdTransDiffuse);

  FFdReflSpecularExp := TSFFloat.Create(Self, 'reflSpecularExp', DefaultMaterialReflSpecularExp);
   FdReflSpecularExp.ChangesAlways := [chMaterial2];
  Fields.Add(FFdReflSpecularExp);

  FFdTransSpecularExp := TSFFloat.Create(Self, 'transSpecularExp', DefaultMaterialTransSpecularExp);
   FdTransSpecularExp.ChangesAlways := [chMaterial2];
  Fields.Add(FFdTransSpecularExp);
end;

class function TMaterialNode.ClassNodeTypeName: string;
begin
  Result := 'Material';
end;

class function TMaterialNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TMaterialNode.ForVRMLVersion(const Version: TVRMLVersion): boolean;
begin
  Result := Version.Major >= 2;
end;

function TMaterialNode.Opacity: Single;
begin
  Result := 1- FdTransparency.Value;
end;

function TMaterialNode.ShininessExp: Single;
begin
  Result := Clamped(FdShininess.Value * 128.0, 0.0, 128.0);
end;

function TMaterialNode.MaterialInfo: TVRMLMaterialInfo;
begin
  Result := TVRMLMaterialInfo.Create(Self);
end;

{ TVRMLMaterialInfoAbstract ---------------------------------------------------------- }

procedure TVRMLMaterialInfoAbstract.CalculateReflSpecular(var V: TVector3Single);
begin
  V[0] := Mirror;
  V[1] := V[0];
  V[2] := V[0];
end;

procedure TVRMLMaterialInfoAbstract.CalculateReflDiffuse(var V: TVector3Single);
begin
  V := DiffuseColor;
end;

procedure TVRMLMaterialInfoAbstract.CalculateTransSpecular(var V: TVector3Single);
begin
  V[0] := Transparency;
  V[1] := V[0];
  V[2] := V[0];
end;

procedure TVRMLMaterialInfoAbstract.CalculateTransDiffuse(var V: TVector3Single);
begin
  V := VectorScale(DiffuseColor, Transparency);
end;

{ TVRMLMaterialInfo -------------------------------------------------------- }

constructor TVRMLMaterialInfo.Create(Node: TMaterialNode);
begin
  inherited Create;
  FNode := Node;
end;

function TVRMLMaterialInfo.DiffuseColor: TVector3Single;
begin
  Result := FNode.FdDiffuseColor.Value;
end;

function TVRMLMaterialInfo.Mirror: Single;
begin
  Result := FNode.FdMirror.Value;
end;

function TVRMLMaterialInfo.Transparency: Single;
begin
  Result := FNode.FdTransparency.Value;
end;

function TVRMLMaterialInfo.ReflSpecular: TVector3Single;
var
  A: TVector3SingleList;
begin
  A := FNode.FdReflSpecular.Items;
  if A.Count = 0 then
    CalculateReflSpecular(Result) else
    Result := A.L[0];
end;

function TVRMLMaterialInfo.ReflDiffuse: TVector3Single;
var
  A: TVector3SingleList;
begin
  A := FNode.FdReflDiffuse.Items;
  if A.Count = 0 then
    CalculateReflDiffuse(Result) else
    Result := A.L[0];
end;

function TVRMLMaterialInfo.TransSpecular: TVector3Single;
var
  A: TVector3SingleList;
begin
  A := FNode.FdTransSpecular.Items;
  if A.Count = 0 then
    CalculateTransSpecular(Result) else
    Result := A.L[0];
end;

function TVRMLMaterialInfo.TransDiffuse: TVector3Single;
var
  A: TVector3SingleList;
begin
  A := FNode.FdTransDiffuse.Items;
  if A.Count = 0 then
    CalculateTransDiffuse(Result) else
    Result := A.L[0];
end;

function TVRMLMaterialInfo.ReflSpecularExp: Single;
begin
  Result := FNode.FdReflSpecularExp.Value;
end;

function TVRMLMaterialInfo.TransSpecularExp: Single;
begin
  Result := FNode.FdTransSpecularExp.Value;
end;

{ TShapeNode ----------------------------------------------------------------- }

procedure TShapeNode.CreateNode;
begin
  inherited;
end;

class function TShapeNode.ClassNodeTypeName: string;
begin
  Result := 'Shape';
end;

class function TShapeNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TTwoSidedMaterialNode.CreateNode;
begin
  inherited;

  FFdAmbientIntensity := TSFFloat.Create(Self, 'ambientIntensity', 0.2);
   FdAmbientIntensity.ChangesAlways := [chMaterial2];
  Fields.Add(FFdAmbientIntensity);
  { X3D specification comment: [0,1] }

  FFdBackAmbientIntensity := TSFFloat.Create(Self, 'backAmbientIntensity', 0.2);
   FdBackAmbientIntensity.ChangesAlways := [chMaterial2];
  Fields.Add(FFdBackAmbientIntensity);
  { X3D specification comment: [0,1] }

  FFdBackDiffuseColor := TSFColor.Create(Self, 'backDiffuseColor', Vector3Single(0.8, 0.8, 0.8));
   FdBackDiffuseColor.ChangesAlways := [chMaterial2];
  Fields.Add(FFdBackDiffuseColor);
  { X3D specification comment: [0,1] }

  FFdBackEmissiveColor := TSFColor.Create(Self, 'backEmissiveColor', Vector3Single(0, 0, 0));
   FdBackEmissiveColor.ChangesAlways := [chMaterial2];
  Fields.Add(FFdBackEmissiveColor);
  { X3D specification comment: [0,1] }

  FFdBackShininess := TSFFloat.Create(Self, 'backShininess', 0.2);
   FdBackShininess.ChangesAlways := [chMaterial2];
  Fields.Add(FFdBackShininess);
  { X3D specification comment: [0,1] }

  FFdBackSpecularColor := TSFColor.Create(Self, 'backSpecularColor', Vector3Single(0, 0, 0));
   FdBackSpecularColor.ChangesAlways := [chMaterial2];
  Fields.Add(FFdBackSpecularColor);
  { X3D specification comment: [0,1] }

  FFdBackTransparency := TSFFloat.Create(Self, 'backTransparency', 0);
   FdBackTransparency.ChangesAlways := [chMaterial2, chUseBlending];
  Fields.Add(FFdBackTransparency);
  { X3D specification comment: [0,1] }

  FFdDiffuseColor := TSFColor.Create(Self, 'diffuseColor', Vector3Single(0.8, 0.8, 0.8));
   FdDiffuseColor.ChangesAlways := [chMaterial2];
  Fields.Add(FFdDiffuseColor);
  { X3D specification comment: [0,1] }

  FFdEmissiveColor := TSFColor.Create(Self, 'emissiveColor', Vector3Single(0, 0, 0));
   FdEmissiveColor.ChangesAlways := [chMaterial2];
  Fields.Add(FFdEmissiveColor);
  { X3D specification comment: [0,1] }

  FFdShininess := TSFFloat.Create(Self, 'shininess', 0.2);
   FdShininess.ChangesAlways := [chMaterial2];
  Fields.Add(FFdShininess);
  { X3D specification comment: [0,1] }

  FFdSeparateBackColor := TSFBool.Create(Self, 'separateBackColor', false);
   FdSeparateBackColor.ChangesAlways := [chMaterial2];
  Fields.Add(FFdSeparateBackColor);

  FFdSpecularColor := TSFColor.Create(Self, 'specularColor', Vector3Single(0, 0, 0));
   FdSpecularColor.ChangesAlways := [chMaterial2];
  Fields.Add(FFdSpecularColor);
  { X3D specification comment: [0,1] }

  FFdTransparency := TSFFloat.Create(Self, 'transparency', 0);
   FdTransparency.ChangesAlways := [chMaterial2, chUseBlending];
  Fields.Add(FFdTransparency);
  { X3D specification comment: [0,1] }
end;

class function TTwoSidedMaterialNode.ClassNodeTypeName: string;
begin
  Result := 'TwoSidedMaterial';
end;

class function TTwoSidedMaterialNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterShapeNodes;
begin
  NodesManager.RegisterNodeClasses([
    TAppearanceNode,
    TFillPropertiesNode,
    TLinePropertiesNode,
    TMaterialNode,
    TShapeNode,
    TTwoSidedMaterialNode
  ]);
end;

{$endif read_implementation}
