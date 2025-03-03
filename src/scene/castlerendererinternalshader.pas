{
  Copyright 2010-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Setting up OpenGL shaders (TShader). Internal for CastleRenderer. @exclude }
unit CastleRendererInternalShader;

{$I castleconf.inc}

interface

uses Generics.Collections,
  CastleVectors, CastleGLShaders, CastleColors,
  X3DTime, X3DFields, X3DNodes, CastleUtils, CastleBoxes,
  CastleRendererInternalTextureEnv, CastleStringUtils, CastleRenderOptions,
  CastleShapes, CastleRectangles, CastleTransform, CastleInternalGeometryArrays;

{$define read_interface}

type
  TShader = class;
  TShaderSource = class;
  TX3DShaderProgram = class;

  TTextureType = (tt2D, ttCubeMap, tt3D, ttShader);

  TTexGenerationComponent = (tgEye, tgObject);
  TTexGenerationComplete = (tgSphere, tgNormal, tgReflection, tgcMirrorPlane);
  TTexComponent = 0..3;

  TFogCoordinateSource = (
    { Fog is determined by depth (distance to camera). }
    fcDepth,
    { Fog is determined by explicit coordinate (per-vertex castle_FogCoord attribute). }
    fcPassedCoordinate);

  TBoundingBoxEvent = function: TBox3D of object;

  { GLSL program that may be used by the X3D renderer.
    Provides some extra features, above the standard TGLSLProgram,
    but does not require to link the shader using TShader algorithm. }
  TX3DShaderProgramBase = class(TGLSLProgram)
  public
    { Uniforms initialized after linking.
      Initializing them only once after linking allows the mesh renderer to go fast. }
    UniformCastle_ModelViewMatrix,
    UniformCastle_ProjectionMatrix,
    UniformCastle_NormalMatrix,
    UniformCastle_MaterialDiffuseAlpha,
    UniformCastle_MaterialBaseAlpha,
    UniformCastle_MaterialEmissiveAlpha,
    UniformCastle_MaterialShininess,
    UniformCastle_MaterialEmissive,
    UniformCastle_MaterialAmbient,
    UniformCastle_MaterialSpecular,
    UniformCastle_MaterialMetallic,
    UniformCastle_MaterialRoughness,
    UniformCastle_GlobalAmbient,
    UniformCastle_UnlitColor: TGLSLUniform;

    { Attributes initialized after linking.
      Initializing them only once after linking allows the mesh renderer to go fast. }
    AttributeCastle_Vertex,
    AttributeCastle_Normal,
    AttributeCastle_Tangent,
    AttributeCastle_ColorPerVertex,
    AttributeCastle_FogCoord: TGLSLAttribute;

    procedure Link; override;
  end;

  {$I castlerendererinternalshader_hash.inc}
  {$I castlerendererinternalshader_light.inc}
  {$I castlerendererinternalshader_texture.inc}
  {$I castlerendererinternalshader_shadowmap.inc}
  {$I castlerendererinternalshader_mirrorplane.inc}
  {$I castlerendererinternalshader_surfacetexture.inc}
  {$I castlerendererinternalshader_bumpmapping.inc}

  { GLSL program integrated with VRML/X3D and TShader.
    Allows to bind uniform values from VRML/X3D fields,
    and to observe VRML/X3D events and automatically update uniform values.
    Also allows to initialize and check program by TShader.LinkProgram,
    and get a hash of it by TShader.CodeHash. }
  TX3DShaderProgram = class(TX3DShaderProgramBase)
  private
    { Events where we registered our EventReceive method. }
    EventsObserved: TX3DEventList;

    { Current state of lights uniforms for this shader. }
    FLightUniformsList: TLightUniformsList;

    { Set uniform variable from VRML/X3D field value.
      Uniform name is contained in UniformName. UniformValue indicates
      uniform type and new value (UniformValue.Name is not used).

      Do not pass here SFNode / MFNode fields (these should be added to
      UniformsTextures).

      @raises(EGLSLUniformInvalid When uniform variable name
        or type are invalid.

        Caller should always catch this and change into WritelnWarning.

        X3D spec "OpenGL shading language (GLSL) binding" says
        "If the name is not available as a uniform variable in the
        provided shader source, the values of the node shall be ignored"
        (although it says when talking about "Vertex attributes",
        seems they mixed attributes and uniforms meaning in spec?).

        So invalid uniform names should be always catched.
        We also catch type mismatches.) }
    procedure SetUniformFromField(const UniformName: String;
      const UniformValue: TX3DField;
      const AUniformMissing: TUniformMissing;
      const EnableDisable: boolean);

    procedure EventReceive(const Event: TX3DEvent; const Value: TX3DField;
      const Time: TX3DTime);

    { Set uniform shader variable from VRML/X3D field (exposed or not).
      We also start observing an exposed field or eventIn,
      and will automatically update uniform value when we receive an event. }
    procedure BindNonTextureUniform(
      const FieldOrEvent: TX3DInterfaceDeclaration;
      const AUniformMissing: TUniformMissing;
      const EnableDisable: boolean);
  protected
    { Nodes that have interface declarations with textures for this shader. }
    UniformsTextures: TX3DFieldList;
  public
    constructor Create;
    destructor Destroy; override;

    { Set and observe uniform variables from given Node.InterfaceDeclarations.

      Non-texture fields are set immediately.
      Non-texture fields, and also events, become observed by this shader,
      and automatically updated when changed.

      Texture fields have to be updated by descendant (like TX3DGLSLProgram),
      using the UniformsTextures list. These methods add fields to this list.
      @groupBegin }
    procedure BindUniforms(const Node: TX3DNode; const EnableDisable: boolean); overload;
    procedure BindUniforms(const Nodes: TX3DNodeList; const EnableDisable: boolean); overload;
    { @groupEnd }
  end;

  TShaderSource = class
  private
    FSource: array [TShaderType] of TCastleStringList;
    function GetSource(const AType: TShaderType): TCastleStringList;
  public
    constructor Create;
    destructor Destroy; override;
    property Source [const AType: TShaderType]: TCastleStringList read GetSource; default;

    { Append AppendCode to our code.
      Has some special features:

      - Doesn't use AppendCode[DontAppendFirstPart][0]
        (we use this for now only with texture and light shaders,
        which treat AppendCode[stVertex / stFragment][0] specially).

      - Doesn't add anything to given type, if it's already empty.
        For our internal base shaders, vertex and fragment are never empty.
        When they are empty, this means that user assigned ComposedShader,
        but depends on fixed-function pipeline to do part of the job. }
    procedure Append(AppendCode: TShaderSource; const DontAppendFirstPart: TShaderType);
  end;

  TDynamicUniform = class abstract
  public
    Name: String;
    { Declaration to put at the top of the shader code.
      Must end with newline. May be empty if you do it directly yourself. }
    Declaration: String;
    procedure SetUniform(AProgram: TX3DShaderProgram); virtual; abstract;
  end;

  TDynamicUniformSingle = class(TDynamicUniform)
  public
    Value: Single;
    procedure SetUniform(AProgram: TX3DShaderProgram); override;
  end;

  TDynamicUniformVec3 = class(TDynamicUniform)
  public
    Value: TVector3;
    procedure SetUniform(AProgram: TX3DShaderProgram); override;
  end;

  TDynamicUniformVec4 = class(TDynamicUniform)
  public
    Value: TVector4;
    procedure SetUniform(AProgram: TX3DShaderProgram); override;
  end;

  TDynamicUniformMat4 = class(TDynamicUniform)
  public
    Value: TMatrix4;
    procedure SetUniform(AProgram: TX3DShaderProgram); override;
  end;

  TDynamicUniformInteger = class(TDynamicUniform)
  public
    Value: Integer;
    procedure SetUniform(AProgram: TX3DShaderProgram); override;
  end;

  TDynamicUniformList = {$ifdef FPC}specialize{$endif} TObjectList<TDynamicUniform>;

  { Possible ways to implement clip planes. }
  TClipPlaneAlgorithm = (
    {$ifndef OpenGLES}
    { Use glClipPlane, glEnable(GL_CLIP_PLANE*) calls.

      These are deprecated in newer OpenGL versions.

      The only thing GLSL needs to do is to set gl_ClipVertex
      to a vertex position in eye space. This way it will work both when
      shaders are used for rendering and when not.
    }
    cpFixedFunction,

    { Vertex shader must calculate gl_ClipDistance[] for each plane.
      The clipping must also be enabled by glEnable(GL_CLIP_DISTANCE*).

      This works in new OpenGL >= 3.1 without deprecated stuff.

      This requires
      - OpenGL >= 3.0 (for "glEnable with GL_CLIP_DISTANCE*" in OpenGL API),
      - and again OpenGL >= 3.0 (for GLSL >= 1.30 that includes "gl_ClipDistance"
        built-in).
      - we actuallly bump it to 3.1, so that CastleGLShaders will add a #version,
        which is required for gl_ClipDistance access.

      See
      https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/gl_ClipDistance.xhtml
      https://www.khronos.org/registry/OpenGL/specs/gl/GLSLangSpec.1.40.pdf ,
      https://www.gamedev.net/forums/topic/625559-gl_clipvertex-alternative/ }
    cpClipDistance,
    {$endif}

    { We pass the necessary information and do discard in fragment shader.

      This works everywhere where we have shaders,
      including in OpenGLES 2
      (without EXT_clip_cull_distance.txt, which is only since OpenGLES 3).
      So we write to varying castle_ClipDistance[] (exactly like gl_ClipDistance)
      and then we discard in fragment shader fragments with distance < 0.
    }
    cpDiscard
  );

  { Create appropriate shader and at the same time set OpenGL parameters
    for fixed-function rendering. Once everything is set up,
    you can create TX3DShaderProgram instance
    and initialize it by LinkProgram here, then enable it if you want.
    Or you can simply allow the fixed-function pipeline to work.

    This is used internally by TRenderer. It isn't supposed to be used
    directly by other code. }
  TShader = class
  private
    { When adding new field, remember to clear it in Clear method. }
    { List of effect nodes that determine uniforms of our program. }
    UniformsNodes: TX3DNodeList;
    TextureCoordGen, FragmentEnd: String;
    ClipPlanesCount: Cardinal;
    FShadowSampling: TShadowSampling;
    Source: TShaderSource;
    PlugIdentifiers: Cardinal;
    LightShaders: TLightShaders;
    TextureShaders: TTextureCoordinateShaderList;
    ShadowMapShaders: TShadowMapShaderList;
    FCodeHash: TShaderCodeHash;
    CodeHashFinalized: boolean;
    SelectedNode: TComposedShaderNode;
    WarnMissingPlugs: boolean;
    FShapeRequiresShaders: boolean;
    FBumpMappingShader: TBumpMappingShader;
    FSurfaceTextureShaders: TSurfaceTextureShaderList;
    FFogEnabled: boolean;
    FFogType: TFogType;
    FFogColor: TVector3;
    FFogVisibilityRange: Single;
    FFogCoordinateSource: TFogCoordinateSource;
    HasGeometryMain: boolean;
    TextureMatrix: TCardinalList;
    NeedsCameraInverseMatrix: Boolean;
    NeedsMirrorPlaneTexCoords: Boolean;
    NeedsNormalsForTexGen: Boolean;
    FPhongShading: boolean;
    FLightingModel: TLightingModel;
    FAlphaTest: Boolean;
    UsesShadowMaps: Boolean;

    { We have to optimize the most often case of TShader usage,
      when the shader is not needed or is already prepared.

      - Enabling shader features should not do anything time-consuming,
        as it's done every frame. This means that we cannot construct
        complete shader source code on the fly, as this would mean
        slowdown at every frame for every shape.
        So enabling a feature merely records the demand for this feature.

      - It must also set ShapeRequiresShaders := true, if needed.
      - It must also update FCodeHash, if needed (if final shader code or
        uniform value changes). Can be done immediately, or inside
        CodeHashFinalize (the latter is more comfortable if it may change
        repeatedly and you don't want temporary values to be added to hash).
      - Actually adding this feature to shader source may be done at LinkProgram.
    }
    AppearanceEffects: TMFNode;
    GroupEffects: TX3DNodeList;
    Lighting: Boolean;
    ColorPerVertexType: TColorPerVertexType;
    ColorPerVertexMode: TColorMode;

    FShapeBoundingBoxInWorldKnown: Boolean;
    FShapeBoundingBoxInWorld: TBox3D;

    FClipPlaneAlgorithm: TClipPlaneAlgorithm;

    procedure EnableEffects(Effects: TMFNode;
      const Code: TShaderSource = nil;
      const ForwardDeclareInFinalShader: boolean = false); overload;
    procedure EnableEffects(Effects: TX3DNodeList;
      const Code: TShaderSource = nil;
      const ForwardDeclareInFinalShader: boolean = false); overload;

    { Special form of Plug. It inserts the PlugValue source code directly
      at the position of given plug comment (no function call
      or anything is added). It also assumes that PlugName occurs only once
      in the Code, for speed.

      Returns if plug code was inserted (always @true when
      InsertAtBeginIfNotFound). }
    function PlugDirectly(Code: TCastleStringList;
      const CodeIndex: Cardinal;
      const PlugName, PlugValue: String;
      const InsertAtBeginIfNotFound: boolean): boolean;

    { Make symbol DefineName to be defined for all GLSL parts of
      Source[ShaderType]. }
    procedure Define(const DefineName: String; const ShaderType: TShaderType;
      const PlugEarly: Boolean = false);

    function DeclareShadowFunctions: String;
  public
    { Material parameters for current shape.
      Must be set before EnableLight, and be constant later. }
    Material: TMaterialInfo;

    { Uniforms that will be set on this shader every frame (not just once after linking). }
    DynamicUniforms: TDynamicUniformList;

    { We require a callback, instead of always requiring TBox3D result, because
      in many cases, we will not need to call it (so we don't need to recalculate
      TShape.LocalBoundingBox every frame for a changing shape).

      Should return bbox in scene coordinate system (not in world coordinate system).

      Use ShapeBoundingBoxInWorld to get the box easily. }
    ShapeBoundingBoxInSceneEvent: TBoundingBoxEvent;

    { Camera * scene transformation (without the shape transformation).

      In case GLFeatures.EnableFixedFunction = true, these are also the contents
      of current OpenGL modelview matrix,
      at TRenderer.RenderShapeClipPlanes and earlier.
      At TRenderer.RenderShapeCreateMeshRenderer and later,
      the OpenGL modelview matrix contains also shape transformation,
      so it's different than SceneModelView. }
    SceneModelView: TMatrix4;

    { Scene transformation (without the shape transformation). }
    SceneTransform: TMatrix4;

    { Assign this if you used EnableTexGen with tgMirrorPlane
      to setup correct uniforms. }
    MirrorPlaneUniforms: TMirrorPlaneUniforms;

    RenderingCamera: TRenderingCamera; //< Set this after construction.

    MainTextureMapping: Integer;

    ColorSpaceLinear: Boolean;

    { In case MultiTexture is used to render this, this is MultiTexture.color+alpha value. }
    MultiTextureColor: TCastleColor;

    constructor Create;
    destructor Destroy; override;

    { Detect PLUG_xxx functions within PlugValue,
      look for matching @code(/* PLUG: xxx ...*/) declarations in both CompleteCode
      and the final shader source.

      For every plug declaration,
      @unorderedList(
        @item(insert the appropriate call to the plug function,)
        @item(and insert forward declaration of the plug function.)
      )

      Also, always insert the PlugValue (which should be variable and functions
      declarations) as another part of the CompleteCode.

      EffectPartType determines which type of CompleteCode is used.

      When CompleteCode = nil then we assume code of the final shader
      (private Source field).

      ForwardDeclareInFinalShader should be used only when Code is not nil.
      It means that forward declarations for Code[0] will be inserted
      into final shader code, not into Code[0]. This is useful if your
      Code[0] is special, and it will be pasted directly (not as plug)
      into final shader code.

      Inserts calls right before the magic @code(/* PLUG ...*/) comments,
      this way many Plug calls that defined the same PLUG_xxx function
      will be called in the same order.

      Doesn't do anything if in the final shader given type (EffectPartType)
      has empty code. This indicates that we used ComposedShader, and this type
      has no source code (so it should be done by fixed-function pipeline).
      Adding our own plug would be bad in this case, as we would create shader
      without main(). }
    procedure Plug(const EffectPartType: TShaderType; PlugValue: String;
      CompleteCode: TShaderSource = nil;
      const ForwardDeclareInFinalShader: boolean = false);

    { Add fragment and vertex shader code, link.
      @raises EGLSLError In case of troubles with linking. }
    procedure LinkProgram(AProgram: TX3DShaderProgram;
      const ShapeNiceName: String);

    { Add a fallback vertex + fragment shader code and link.
      Use this when normal LinkProgram failed, but you want to have
      *any* shader anyway.
      @raises EGLSLError In case of troubles with linking. }
    procedure LinkFallbackProgram(AProgram: TX3DShaderProgram);

    { Calculate the hash of all the current TShader settings,
      that is the hash of GLSL program code initialized by this shader
      LinkProgram. You should use this only when the GLSL program source
      is completely initialized (all TShader settings are set).

      It can be used to decide when the shader GLSL program needs
      to be regenerated, shared etc. }
    function CodeHash: TShaderCodeHash;

    procedure EnableTexture(const TextureUnit: Cardinal;
      const TextureType: TTextureType; const Node: TAbstractSingleTextureNode;
      const Env: TTextureEnv);
    procedure EnableTexGen(const TextureUnit: Cardinal;
      const Generation: TTexGenerationComponent; const Component: TTexComponent;
      const Plane: TVector4); overload;
    procedure EnableTexGen(const TextureUnit: Cardinal;
      const Generation: TTexGenerationComplete;
      const TransformToWorldSpace: boolean = false); overload;
    { Disable fixed-function texgen of given texture unit.
      Guarantees to also set active texture unit to TexUnit (if multi-texturing
      available at all). }
    procedure DisableTexGen(const TextureUnit: Cardinal);
    procedure EnableTextureTransform(const TextureUnit: Cardinal;
      const Matrix: TMatrix4);
    { Enable clip plane.

      The Plane equation must be given in "scene coordinates".
      IOW, with shape transformation matrix (from X3D Transform nodes) applied,
      but scene matrix (TCastleScene transformation) not applied,
      and "camera matrix" not applied.

      The ClipPlaneIndex must always be one more than previous one
      on this TShape instance, since it's creation or Initialize call.
      So you can only call EnableClipPlane with successive integers, from 0. }
    procedure EnableClipPlane(const ClipPlaneIndex: Cardinal;
      const Plane: TVector4);
    procedure DisableClipPlane(const ClipPlaneIndex: Cardinal);
    procedure EnableAlphaTest(const AlphaCutoff: Single);
    procedure EnableBumpMapping(const BumpMapping: TBumpMapping;
      const NormalMapTextureUnit, NormalMapTextureCoordinatesId: Cardinal;
      const NormalMapScale: Single;
      const HeightMapInAlpha: boolean; const HeightMapScale: Single);
    { Enable surface texture.
      "Surface texture" is a term for a texture that
      doesn't contribute main color (so it is not Material.diffuseTexture,
      PhysicalMaterial.baseTexture, UnlitMaterial.emissiveTexture),
      and is specified in a special material slot (in CommonSurfaceShader
      or in new Material / PhysicalMaterial / UnlitMaterial xxxTexture fields
      in X3D 4.0). }
    procedure EnableSurfaceTexture(const SurfaceTexture: TSurfaceTexture;
      const TextureUnit, TextureCoordinatesId: Cardinal;
      const UniformTextureName, PlugCode: String);
    { Enable light source. Remember to set MaterialXxx before calling this. }
    procedure EnableLight(const Number: Cardinal; Light: PLightInstance);
    procedure EnableFog(const FogType: TFogType;
      const FogCoordinateSource: TFogCoordinateSource;
      const FogColor: TVector3; const FogVisibilityRange: Single);
    { Modify some fog parameters, relevant only if fog already enabled.
      Used by FogCoordinate, that changes some fog settings,
      but does not change fog color.  }
    procedure ModifyFog(const FogType: TFogType;
      const FogCoordinateSource: TFogCoordinateSource;
      const FogVisibilityRange: Single);
    function EnableCustomShaderCode(const Shaders: TMFNode;
      out Node: TComposedShaderNode): boolean;
    procedure EnableAppearanceEffects(Effects: TMFNode);
    procedure EnableGroupEffects(Effects: TX3DNodeList);
    procedure EnableLighting;
    procedure EnableColorPerVertex(const AMode: TColorMode; const AColorType: TColorPerVertexType);
    procedure EnableShadowMap(const TextureUnit: Cardinal;
      const ShadowMap: TGeneratedShadowMapNode);

    property ShadowSampling: TShadowSampling
      read FShadowSampling write FShadowSampling;
    property ShapeRequiresShaders: boolean read FShapeRequiresShaders
      write FShapeRequiresShaders;
    property LightingModel: TLightingModel
      read FLightingModel write FLightingModel;

    { Clear instance, bringing it to the state after creation.
      You must call Intialize afterwards. }
    procedure Clear;

    { Initialize the instance and PhongShading.
      For now, PhongShading must be set early (and cannot be changed later),
      as it determines the initial shader templates that may be used before linking. }
    procedure Initialize(const APhongShading: boolean);

    property PhongShading: boolean read FPhongShading;

    { Set uniforms that should be set each time before using shader
      (because changes to their values may happen at any time,
      and they do not cause rebuilding the shader). }
    procedure SetDynamicUniforms(AProgram: TX3DShaderProgram);

    { Add a screen effect GLSL code. }
    procedure AddScreenEffectCode(const Depth: boolean);

    { Shader needs normals, for lighting calculation or tex coord generation. }
    function NeedsNormals: Boolean;

    { Current shape bbox, in world coordinates. }
    function ShapeBoundingBoxInWorld: TBox3D;

    { Is alpha testing enabled by EnableAlphaTest. }
    property AlphaTest: Boolean read FAlphaTest;
  end;

{ Derive UniformMissing behavior for fields within given node.
  Accepts Node = @nil too. }
function UniformMissingFromNode(const Node: TX3DNode): TUniformMissing;

{ Find PLUG_xxx function inside PlugValue.
  Returns xxx (the part after PLUG_),
  and DeclaredParameters (of this plug function). Or '' if not found.

  This is public in this unit for the sake of testing. }
function FindPlugName(const PlugValue: String;
  out DeclaredParameters: String): String;

{$undef read_interface}

implementation

uses SysUtils, StrUtils,
  {$ifdef OpenGLES} CastleGLES, {$else} CastleGL, {$endif}
  CastleGLUtils, CastleLog, CastleGLVersion, CastleInternalScreenEffects,
  CastleScreenEffects, CastleInternalX3DLexer, CastleInternalGLUtils;

{$define read_implementation}
{$I castlerendererinternalshader_hash.inc}
{$I castlerendererinternalshader_light.inc}
{$I castlerendererinternalshader_texture.inc}
{$I castlerendererinternalshader_shadowmap.inc}
{$I castlerendererinternalshader_mirrorplane.inc}
{$I castlerendererinternalshader_surfacetexture.inc}
{$I castlerendererinternalshader_bumpmapping.inc}

{$ifndef OpenGLES}
var
  { By default (when this is false),
    desktop OpenGL either use gl_ClipVertex or gl_ClipDistance.
    Set this to true to implement clip planes without any built-in GLSL
    variables. This makes no practical sense on desktop OpenGL (is slower),
    but allows to test this GLSL code on desktop OpenGL.

    This is a global variable only to silence compiler warnings about
    unreachable code, in practice we use this as a constant. }
  ForceOpenGLESClipPlanes: Boolean = false;
{$endif}

function UniformMissingFromNode(const Node: TX3DNode): TUniformMissing;
begin
  if Node is TEffectNode then
    Result := TEffectNode(Node).UniformMissing
  else
  if Node is TComposedShaderNode then
    Result := TComposedShaderNode(Node).UniformMissing
  else
    Result := umWarning;
end;

{ String helpers ------------------------------------------------------------- }

{ MoveTo do not warn about incorrect PLUG_ declarations, only return @false
  on them. That's because FindPlugName should just ignore them.
  But we log them --- maybe they will be useful
  in case there's some problem with FindPlugName.

  Testcase: opening
  castle-model-viewer-mobile/data/demo/teapot (fresnel and toon shader).x3dv
  in castle-model-viewer-mobile.
}

function MoveToOpeningParen(const S: String; var P: Integer): boolean;
begin
  Result := true;
  repeat
    Inc(P);

    if P > Length(S) then
    begin
      WritelnLog('VRML/X3D', 'PLUG declaration unexpected end (no opening parenthesis "(") ');
      Exit(false);
    end;

    if (S[P] <> '(') and
       not CharInSet(S[P], WhiteSpaces) then
    begin
      WritelnLog('VRML/X3D', Format('PLUG declaration unexpected character "%s" (expected opening parenthesis "(") in "%s"',
        [S[P], S]));
      Exit(false);
    end;
  until S[P] = '(';
 end;

function MoveToMatchingParen(const S: String; var P: Integer): boolean;
var
  ParenLevel: Cardinal;
begin
  Result := true;
  ParenLevel := 1;

  repeat
    Inc(P);
    if P > Length(S) then
    begin
      WritelnLog('VRML/X3D', 'PLUG declaration unexpected end (no closing parenthesis ")")');
      Exit(false);
    end;

    if S[P] = '(' then
      Inc(ParenLevel) else
    if S[P] = ')' then
      Dec(ParenLevel);
  until ParenLevel = 0;
end;

function FindPlugName(const PlugValue: String;
  out DeclaredParameters: String): String;
const
  PlugPrefix = 'PLUG_';
  IdentifierChars = ['0'..'9', 'a'..'z', 'A'..'Z', '_'];
var
  P, PBegin, DPBegin, DPEnd, SearchStart: Integer;
begin
  SearchStart := 1;
  repeat
    P := PosEx(PlugPrefix, PlugValue, SearchStart);
    if P = 0 then Exit('');

    { if code below will decide that it's an incorrect PLUG_ definition,
      it will do Continue, and we will search again from the next position. }
    SearchStart := P + Length(PlugPrefix);

    { There must be whitespace before PLUG_ }
    if (P > 1) and (not CharInSet(PlugValue[P - 1], WhiteSpaces)) then Continue;
    P := P + Length(PlugPrefix);
    PBegin := P;
    { There must be at least one identifier char after PLUG_ }
    if (P > Length(PlugValue)) or
        (not CharInSet(PlugValue[P], IdentifierChars)) then Continue;
    repeat
      Inc(P);
    until (P > Length(PlugValue)) or (not CharInSet(PlugValue[P], IdentifierChars));
    { Skip whitespace after PLUG_xxx and before "(". }
    while (P <= Length(PlugValue)) and CharInSet(PlugValue[P], WhiteSpaces) do
      Inc(P);
    { There must be "(" now }
    if (P > Length(PlugValue)) or (PlugValue[P] <> '(') then
      Continue;

    // Trim needed, because P may include some trailing whitespace
    Result := Trim(CopyPos(PlugValue, PBegin, P - 1));

    DPBegin := P - 1;
    if not MoveToOpeningParen(PlugValue, DPBegin) then Continue;
    DPEnd := DPBegin;
    if not MoveToMatchingParen(PlugValue, DPEnd) then Continue;

    DeclaredParameters := CopyPos(PlugValue, DPBegin, DPEnd);
    { if you managed to get here, then we have correct Result and DeclaredParameters }
    Exit;
  until false;
end;

{ In OpenGL, each part (separate compilation) has to declare it's variables
  (uniforms, attributes etc.). It also has to declare the used procedures
  from other compilation units.

  In OpenGLES, all parts are glued into one, and their declarations cannot
  be repeated (or there will be compilation error).
  The used procedures may be declated (a forward declaration),
  but the forward declarations cannot be repeated (although it may depend
  on mobile GPU).

  This function wraps a declaration in suitable #ifdef
  such that it will only be declared once.
  The given Name is anything unique -- usually the variable or procedure name.

  Declaration should not (but may) end with newline.
  The result always ends with newline. }
function DeclareOnce(const Name: String; const Declaration: String): String;
begin
  {$ifndef OpenGLES}
  Result := Declaration + NL;
  {$else}
  Result :=
    '#ifndef ' + Name + '_defined' + NL +
    '#define ' + Name + '_defined' + NL +
    Declaration + NL +
    '#endif' + NL;
  {$endif}
end;

{ TShaderSource -------------------------------------------------------------- }

constructor TShaderSource.Create;
var
  SourceType: TShaderType;
begin
  inherited;
  for SourceType := Low(SourceType) to High(SourceType) do
    FSource[SourceType] := TCastleStringList.Create;
end;

destructor TShaderSource.Destroy;
var
  SourceType: TShaderType;
begin
  for SourceType := Low(SourceType) to High(SourceType) do
    FreeAndNil(FSource[SourceType]);
  inherited;
end;

function TShaderSource.GetSource(const AType: TShaderType): TCastleStringList;
begin
  Result := FSource[AType];
end;

procedure TShaderSource.Append(AppendCode: TShaderSource; const DontAppendFirstPart: TShaderType);
var
  T: TShaderType;
  I: Integer;
begin
  for T := Low(T) to High(T) do
    if Source[T].Count <> 0 then
      for I := Iff(T = DontAppendFirstPart, 1, 0) to AppendCode[T].Count - 1 do
        Source[T].Add(AppendCode[T][I]);
end;

{ TX3DShaderProgramBase ------------------------------------------------------ }

procedure TX3DShaderProgramBase.Link;
begin
  inherited;

  UniformCastle_ModelViewMatrix       := Uniform('castle_ModelViewMatrix'      , umIgnore);
  UniformCastle_ProjectionMatrix      := Uniform('castle_ProjectionMatrix'     , umIgnore);
  UniformCastle_NormalMatrix          := Uniform('castle_NormalMatrix'         , umIgnore);
  UniformCastle_MaterialDiffuseAlpha  := Uniform('castle_MaterialDiffuseAlpha' , umIgnore);
  UniformCastle_MaterialBaseAlpha     := Uniform('castle_MaterialBaseAlpha'    , umIgnore);
  UniformCastle_MaterialEmissiveAlpha := Uniform('castle_MaterialEmissiveAlpha', umIgnore);
  UniformCastle_MaterialShininess     := Uniform('castle_MaterialShininess'    , umIgnore);
  UniformCastle_MaterialEmissive      := Uniform('castle_MaterialEmissive'     , umIgnore);
  UniformCastle_MaterialAmbient       := Uniform('castle_MaterialAmbient'      , umIgnore);
  UniformCastle_MaterialSpecular      := Uniform('castle_MaterialSpecular'     , umIgnore);
  UniformCastle_MaterialMetallic      := Uniform('castle_MaterialMetallic'     , umIgnore);
  UniformCastle_MaterialRoughness     := Uniform('castle_MaterialRoughness'    , umIgnore);
  UniformCastle_GlobalAmbient         := Uniform('castle_GlobalAmbient'        , umIgnore);
  UniformCastle_UnlitColor            := Uniform('castle_UnlitColor'           , umIgnore);

  AttributeCastle_Vertex         := AttributeOptional('castle_Vertex');
  AttributeCastle_Normal         := AttributeOptional('castle_Normal');
  AttributeCastle_Tangent        := AttributeOptional('castle_Tangent');
  AttributeCastle_ColorPerVertex := AttributeOptional('castle_ColorPerVertex');
  AttributeCastle_FogCoord       := AttributeOptional('castle_FogCoord');
end;

{ TX3DShaderProgram ------------------------------------------------------- }

constructor TX3DShaderProgram.Create;
begin
  inherited;
  EventsObserved := TX3DEventList.Create(false);
  UniformsTextures := TX3DFieldList.Create(false);
  FLightUniformsList := TLightUniformsList.Create(true);
end;

destructor TX3DShaderProgram.Destroy;
var
  I: Integer;
begin
  if EventsObserved <> nil then
  begin
    for I := 0 to EventsObserved.Count - 1 do
      EventsObserved[I].RemoveNotification({$ifdef FPC}@{$endif}EventReceive);
    FreeAndNil(EventsObserved);
  end;
  FreeAndNil(UniformsTextures);
  FreeAndNil(FLightUniformsList);
  inherited;
end;

procedure TX3DShaderProgram.BindNonTextureUniform(
  const FieldOrEvent: TX3DInterfaceDeclaration;
  const AUniformMissing: TUniformMissing;
  const EnableDisable: boolean);
var
  UniformField: TX3DField;
  UniformEvent, ObservedEvent: TX3DEvent;
begin
  UniformField := FieldOrEvent.Field;
  UniformEvent := FieldOrEvent.Event;

  { Set initial value for this GLSL uniform variable,
    from X3D field or exposedField }

  if UniformField <> nil then
    { Ok, we have a field with a value (interface declarations with
      fields inside ComposedShader / Effect always have a value).
      So set GLSL uniform variable from this field. }
    SetUniformFromField(UniformField.X3DName, UniformField, AUniformMissing, EnableDisable);

  { Allow future changing of this GLSL uniform variable,
    from VRML eventIn or exposedField }

  { calculate ObservedEvent }
  ObservedEvent := nil;
  if (UniformField <> nil) and UniformField.Exposed then
    ObservedEvent := UniformField.ExposedEvents[false] else
  if (UniformEvent <> nil) and UniformEvent.InEvent then
    ObservedEvent := UniformEvent;

  if ObservedEvent <> nil then
  begin
    ObservedEvent.AddNotification({$ifdef FPC}@{$endif}EventReceive);
    EventsObserved.Add(ObservedEvent);
  end;
end;

procedure TX3DShaderProgram.SetUniformFromField(
  const UniformName: String; const UniformValue: TX3DField;
  const AUniformMissing: TUniformMissing;
  const EnableDisable: Boolean);
var
  TempF: TSingleList;
  TempVec2f: TVector2List;
  TempVec3f: TVector3List;
  TempVec4f: TVector4List;
  TempMat3f: TMatrix3List;
  TempMat4f: TMatrix4List;
begin
  { program must be active to set uniform values. }
  if EnableDisable then
    Enable;

  if UniformValue is TSFBool then
    SetUniform(UniformName, TSFBool(UniformValue).Value, AUniformMissing) else
  if UniformValue is TSFLong then
    { Handling of SFLong also takes care of SFInt32. }
    SetUniform(UniformName, TSFLong(UniformValue).Value, AUniformMissing) else
  if UniformValue is TSFVec2f then
    SetUniform(UniformName, TSFVec2f(UniformValue).Value, AUniformMissing) else

  (*
  { Old approach: Check TSFColor first, otherwise TSFVec3f would also catch and handle
    TSFColor. And we don't want this: for GLSL, color is passed
    as vec4 (so says the spec, I guess that the reason is that for GLSL most
    input/output colors are vec4).

    New approach: That's just nonsense in X3D spec.
    We now pass SFColor as vec3, it can fallback TSFVec3f clause. }

  // if UniformValue is TSFColor then
  //   SetUniform(UniformName, Vector4(TSFColor(UniformValue).Value, 1.0), AUniformMissing) else
  *)

  if UniformValue is TSFVec3f then
    SetUniform(UniformName, TSFVec3f(UniformValue).Value, AUniformMissing) else
  if UniformValue is TSFVec4f then
    SetUniform(UniformName, TSFVec4f(UniformValue).Value, AUniformMissing) else
  if UniformValue is TSFRotation then
    SetUniform(UniformName, TSFRotation(UniformValue).Value, AUniformMissing) else
  if UniformValue is TSFMatrix3f then
    SetUniform(UniformName, TSFMatrix3f(UniformValue).Value, AUniformMissing) else
  if UniformValue is TSFMatrix4f then
    SetUniform(UniformName, TSFMatrix4f(UniformValue).Value, AUniformMissing) else
  if UniformValue is TSFFloat then
    SetUniform(UniformName, TSFFloat(UniformValue).Value, AUniformMissing) else
  if UniformValue is TSFDouble then
    { SFDouble also takes care of SFTime }
    SetUniform(UniformName, TSFDouble(UniformValue).Value, AUniformMissing) else

  { Double-precision vector and matrix types.

    Note that X3D spec specifies only mapping for SF/MFVec3d, 4d
    (not specifying any mapping for SF/MFVec2d, and all matrix types).
    And it specifies that they map to types float3, float4 ---
    which are not valid types in GLSL?

    So I simply ignore non-sensible specification, and take
    the reasonable approach: support all double-precision vectors and matrices,
    just like single-precision. }
  if UniformValue is TSFVec2d then
    SetUniform(UniformName, Vector2(TSFVec2d(UniformValue).Value), AUniformMissing) else
  if UniformValue is TSFVec3d then
    SetUniform(UniformName, Vector3(TSFVec3d(UniformValue).Value), AUniformMissing) else
  if UniformValue is TSFVec4d then
    SetUniform(UniformName, Vector4(TSFVec4d(UniformValue).Value), AUniformMissing) else
  if UniformValue is TSFMatrix3d then
    SetUniform(UniformName, Matrix3(TSFMatrix3d(UniformValue).Value), AUniformMissing) else
  if UniformValue is TSFMatrix4d then
    SetUniform(UniformName, Matrix4(TSFMatrix4d(UniformValue).Value), AUniformMissing) else

  { Now repeat this for array types }
  if UniformValue is TMFBool then
    SetUniform(UniformName, TMFBool(UniformValue).Items, AUniformMissing) else
  if UniformValue is TMFLong then
    SetUniform(UniformName, TMFLong(UniformValue).Items, AUniformMissing) else
  if UniformValue is TMFVec2f then
    SetUniform(UniformName, TMFVec2f(UniformValue).Items, AUniformMissing) else
  (* Old approach: follow X3D spec, and map MFColor to vec4[].
     New approach: ignore nonsense X3D spec, and map MFColor to vec3[].
     Just allow TMFColor to fallback to TMFVec3f.
  if UniformValue is TMFColor then
  begin
    TempVec4f := TMFColor(UniformValue).Items.ToVector4(1.0);
    try
      SetUniform(UniformName, TempVec4f, AUniformMissing);
    finally FreeAndNil(TempVec4f) end;
  end else
  *)
  if UniformValue is TMFVec3f then
    SetUniform(UniformName, TMFVec3f(UniformValue).Items, AUniformMissing) else
  if UniformValue is TMFVec4f then
    SetUniform(UniformName, TMFVec4f(UniformValue).Items, AUniformMissing) else
  if UniformValue is TMFRotation then
    SetUniform(UniformName, TMFRotation(UniformValue).Items, AUniformMissing) else
  if UniformValue is TMFMatrix3f then
    SetUniform(UniformName, TMFMatrix3f(UniformValue).Items, AUniformMissing) else
  if UniformValue is TMFMatrix4f then
    SetUniform(UniformName, TMFMatrix4f(UniformValue).Items, AUniformMissing) else
  if UniformValue is TMFFloat then
    SetUniform(UniformName, TMFFloat(UniformValue).Items, AUniformMissing) else
  if UniformValue is TMFDouble then
  begin
    TempF := TMFDouble(UniformValue).Items.ToSingle;
    try
      SetUniform(UniformName, TempF, AUniformMissing);
    finally FreeAndNil(TempF) end;
  end else
  if UniformValue is TMFVec2d then
  begin
    TempVec2f := TMFVec2d(UniformValue).Items.ToVector2;
    try
      SetUniform(UniformName, TempVec2f, AUniformMissing);
    finally FreeAndNil(TempVec2f) end;
  end else
  if UniformValue is TMFVec3d then
  begin
    TempVec3f := TMFVec3d(UniformValue).Items.ToVector3;
    try
      SetUniform(UniformName, TempVec3f, AUniformMissing);
    finally FreeAndNil(TempVec3f) end;
  end else
  if UniformValue is TMFVec4d then
  begin
    TempVec4f := TMFVec4d(UniformValue).Items.ToVector4;
    try
      SetUniform(UniformName, TempVec4f, AUniformMissing);
    finally FreeAndNil(TempVec4f) end;
  end else
  if UniformValue is TMFMatrix3d then
  begin
    TempMat3f := TMFMatrix3d(UniformValue).Items.ToMatrix3;
    try
      SetUniform(UniformName, TempMat3f, AUniformMissing);
    finally FreeAndNil(TempMat3f) end;
  end else
  if UniformValue is TMFMatrix4d then
  begin
    TempMat4f := TMFMatrix4d(UniformValue).Items.ToMatrix4;
    try
      SetUniform(UniformName, TempMat4f, AUniformMissing);
    finally FreeAndNil(TempMat4f) end;
  end else

  (*
  if (UniformValue is TSFNode) or
     (UniformValue is TMFNode) then
  begin
    { Nothing to do, these will be set by TGLSLRenderer.Enable.
      Right now, these are never passed here. }
  end else
  *)

    { TODO: other field types, full list is in X3D spec in
      "OpenGL shading language (GLSL) binding".
      Remaining:
      SF/MFImage }
    WritelnWarning('VRML/X3D', 'Setting uniform GLSL variable from X3D field type "' + UniformValue.X3DType + '" not supported');

  if EnableDisable then
    { TODO: this should restore previously bound program }
    Disable;
end;

procedure TX3DShaderProgram.EventReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
var
  UniformName: String;
  Scene: TX3DEventsEngine;
begin
  if Event.ParentExposedField = nil then
    UniformName := Event.X3DName
  else
    UniformName := Event.ParentExposedField.X3DName;

  SetUniformFromField(UniformName, Value, UniformMissingFromNode(Event.ParentNode as TX3DNode), true);

  { Although ExposedEvents implementation already sends notification
    about changes to Scene, we can also get here
    by eventIn invocation (which doesn't trigger
    Scene.InternalChangedField, since it doesn't change a field...).
    So we should explicitly do VisibleChangeHere here, to make sure
    it gets called when uniform changed. }
  if Event.ParentNode <> nil then
  begin
    Scene := (Event.ParentNode as TX3DNode).Scene;
    if Scene <> nil then
      Scene.VisibleChangeHere([vcVisibleGeometry, vcVisibleNonGeometry]);
  end;
end;

procedure TX3DShaderProgram.BindUniforms(const Node: TX3DNode;
  const EnableDisable: boolean);
var
  I: Integer;
  IDecl: TX3DInterfaceDeclaration;
begin
  Assert(Node.HasInterfaceDeclarations <> []);
  Assert(Node.InterfaceDeclarations <> nil);
  for I := 0 to Node.InterfaceDeclarations.Count - 1 do
  begin
    IDecl := Node.InterfaceDeclarations[I];
    if (IDecl.Field <> nil) and
       ((IDecl.Field is TSFNode) or
        (IDecl.Field is TMFNode)) then
      UniformsTextures.Add(IDecl.Field)
    else
      BindNonTextureUniform(IDecl, UniformMissingFromNode(Node), EnableDisable);
  end;
end;

procedure TX3DShaderProgram.BindUniforms(const Nodes: TX3DNodeList;
  const EnableDisable: boolean);
var
  I: Integer;
begin
  for I := 0 to Nodes.Count - 1 do
    BindUniforms(Nodes[I], EnableDisable);
end;

{ TDynamicUniformSingle ------------------------------------------------------ }

procedure TDynamicUniformSingle.SetUniform(AProgram: TX3DShaderProgram);
begin
  AProgram.SetUniform(Name, Value);
end;

{ TDynamicUniformVec3 -------------------------------------------------------- }

procedure TDynamicUniformVec3.SetUniform(AProgram: TX3DShaderProgram);
begin
  AProgram.SetUniform(Name, Value);
end;

{ TDynamicUniformVec4 -------------------------------------------------------- }

procedure TDynamicUniformVec4.SetUniform(AProgram: TX3DShaderProgram);
begin
  AProgram.SetUniform(Name, Value);
end;

{ TDynamicUniformMat4 -------------------------------------------------------- }

procedure TDynamicUniformMat4.SetUniform(AProgram: TX3DShaderProgram);
begin
  AProgram.SetUniform(Name, Value);
end;

{ TDynamicUniformInteger -------------------------------------------------------- }

procedure TDynamicUniformInteger.SetUniform(AProgram: TX3DShaderProgram);
begin
  AProgram.SetUniform(Name, Value);
end;

{ TShader ---------------------------------------------------------------- }

function InsertIntoString(const Base: String; const P: Integer; const S: String): String;
begin
  Result := Copy(Base, 1, P - 1) + S + SEnding(Base, P);
end;

const
  DefaultVertexShader: array [ { phong shading } boolean ] of string = (
    {$I main_shading_gouraud.vs.inc},
    {$I main_shading_phong.vs.inc}
  );
  DefaultFragmentShader: array [ { phong shading } boolean ] of string = (
    {$I main_shading_gouraud.fs.inc},
    {$I main_shading_phong.fs.inc}
  );
  DefaultGeometryShader = {$I geometry_shader_utils.gs.inc};

  // TODO: fix this to pass color in new shaders (not using deprecated gl_FrontColor, gl_BackColor)
  (*
  GeometryShaderPassColors =
    '#version 150 compatibility' +NL+

    'void PLUG_geometry_vertex_set(const int index)' +NL+
    '{' +NL+
    '  gl_FrontColor = gl_in[index].gl_FrontColor;' +NL+
    '  gl_BackColor  = gl_in[index].gl_BackColor;' +NL+
    '}' +NL+

    'void PLUG_geometry_vertex_zero()' +NL+
    '{' +NL+
    '  gl_FrontColor = vec4(0.0);' +NL+
    '  gl_BackColor  = vec4(0.0);' +NL+
    '}' +NL+

    'void PLUG_geometry_vertex_add(const int index, const float scale)' +NL+
    '{' +NL+
    '  gl_FrontColor += gl_in[index].gl_FrontColor * scale;' +NL+
    '  gl_BackColor  += gl_in[index].gl_BackColor  * scale;' +NL+
    '}' +NL;
  *)

constructor TShader.Create;
begin
  inherited;
  Source := TShaderSource.Create;
  LightShaders := TLightShaders.Create;
  TextureShaders := TTextureCoordinateShaderList.Create;
  ShadowMapShaders := TShadowMapShaderList.Create;
  UniformsNodes := TX3DNodeList.Create(false);
  DynamicUniforms := TDynamicUniformList.Create(true);
  TextureMatrix := TCardinalList.Create;

  WarnMissingPlugs := true;
end;

destructor TShader.Destroy;
begin
  FreeAndNil(UniformsNodes);
  FreeAndNil(LightShaders);
  FreeAndNil(TextureShaders);
  FreeAndNil(ShadowMapShaders);
  FreeAndNil(Source);
  FreeAndNil(DynamicUniforms);
  FreeAndNil(TextureMatrix);
  inherited;
end;

procedure TShader.Clear;
var
  ShaderType: TShaderType;
begin
  for ShaderType := Low(TShaderType) to High(TShaderType) do
    Source[ShaderType].Clear;

  WarnMissingPlugs := true;
  HasGeometryMain := false;

  { The rest of fields just restored to default clear state.
    Note: use "Count := 0" instead of "Clear", this allows to keep Capacity of TList<T>.Clear,
    which will hopefully avoid some resizing and make it faster. }
  UniformsNodes.Count := 0;
  TextureCoordGen := '';
  ClipPlanesCount := 0;
  FragmentEnd := '';
  FShadowSampling := Low(TShadowSampling);
  PlugIdentifiers := 0;
  LightShaders.Count := 0;
  TextureShaders.Count := 0;
  ShadowMapShaders.Count := 0;
  FCodeHash.Clear;
  CodeHashFinalized := false;
  SelectedNode := nil;
  FShapeRequiresShaders := false;
  FBumpMappingShader.Clear;
  FSurfaceTextureShaders.Clear;
  FFogEnabled := false;
  { No need to reset, will be set when FFogEnabled := true
  FFogType := Low(TFogType);
  FFogCoordinateSource := Low(TFogCoordinateSource); }
  AppearanceEffects := nil;
  GroupEffects := nil;
  Lighting := false;
  ColorPerVertexType := ctNone;
  ColorPerVertexMode := cmReplace;
  FPhongShading := false;
  ShapeBoundingBoxInSceneEvent := nil;
  FShapeBoundingBoxInWorldKnown := false;
  Material := nil;
  DynamicUniforms.Count := 0;
  TextureMatrix.Count := 0;
  NeedsCameraInverseMatrix := false;
  NeedsMirrorPlaneTexCoords := false;
  NeedsNormalsForTexGen := false;
  RenderingCamera := nil;
  FLightingModel := lmPhong;
  MainTextureMapping := -1;
  ColorSpaceLinear := false;
  MultiTextureColor := White;
  FAlphaTest := false;
  UsesShadowMaps := false;
end;

procedure TShader.Initialize(const APhongShading: boolean);
begin
  FPhongShading := APhongShading;
  FCodeHash.AddInteger(Ord(PhongShading) * 877);

  Source[stVertex].Count := 1;
  Source[stVertex][0] := DefaultVertexShader[PhongShading];
  Source[stFragment].Count := 1;
  Source[stFragment][0] := DefaultFragmentShader[PhongShading];
  Source[stGeometry].Count := 1;
  Source[stGeometry][0] := DefaultGeometryShader;
end;

procedure TShader.Plug(const EffectPartType: TShaderType; PlugValue: String;
  CompleteCode: TShaderSource; const ForwardDeclareInFinalShader: boolean);

  function FindPlugOccurrence(const CommentBegin, Code: String;
    const CodeSearchBegin: Integer; out PBegin, PEnd: Integer): boolean;
  begin
    Result := false;
    PBegin := PosEx(CommentBegin, Code, CodeSearchBegin);
    if PBegin <> 0 then
    begin
      PEnd := PosEx('*/', Code, PBegin + Length(CommentBegin));
      Result :=  PEnd <> 0;
      if not Result then
        WritelnWarning('VRML/X3D', Format('Plug comment "%s" not properly closed, treating like not declared',
          [CommentBegin]));
    end;
  end;

  procedure InsertIntoCode(Code: TCastleStringList;
    const CodeIndex, P: Integer; const S: String);
  begin
    Code[CodeIndex] := InsertIntoString(Code[CodeIndex], P, S);
  end;

var
  PlugName, ProcedureName, PlugForwardDeclaration: String;

  function LookForPlugDeclaration(CodeForPlugDeclaration: TCastleStringList): boolean;
  var
    AnyOccurrencesInThisCodeIndex: boolean;
    PBegin, PEnd, CodeSearchBegin, CodeIndex: Integer;
    CommentBegin, Parameters, Declaration: String;
  begin
    CommentBegin := '/* PLUG: ' + PlugName + ' ';
    Result := false;
    for CodeIndex := 0 to CodeForPlugDeclaration.Count - 1 do
    begin
      CodeSearchBegin := 1;
      AnyOccurrencesInThisCodeIndex := false;
      while FindPlugOccurrence(CommentBegin, CodeForPlugDeclaration[CodeIndex],
        CodeSearchBegin, PBegin, PEnd) do
      begin
        Parameters := Trim(CopyPos(CodeForPlugDeclaration[CodeIndex], PBegin + Length(CommentBegin), PEnd - 1));
        Declaration := ProcedureName + Parameters + ';' + NL;
        InsertIntoCode(CodeForPlugDeclaration, CodeIndex, PBegin, Declaration);

        { do not find again the same plug comment by FindPlugOccurrence }
        CodeSearchBegin := PEnd + Length(Declaration);

        AnyOccurrencesInThisCodeIndex := true;
        Result := true;
      end;

      if AnyOccurrencesInThisCodeIndex then
      begin
        { added "plugged_x" function must be forward declared first.
          Otherwise it could be defined after it is needed, or inside different
          compilation unit. }
        if ForwardDeclareInFinalShader and (CodeIndex = 0) then
          PlugDirectly(Source[EffectPartType], CodeIndex, '/* PLUG-DECLARATIONS */', PlugForwardDeclaration, true) else
          PlugDirectly(CodeForPlugDeclaration, CodeIndex, '/* PLUG-DECLARATIONS */', PlugForwardDeclaration, true);
      end;
    end;
  end;

var
  Code: TCastleStringList;
  PlugDeclaredParameters: String;
  AnyOccurrences: boolean;
begin
  if CompleteCode = nil then
    CompleteCode := Source;
  Code := CompleteCode[EffectPartType];

  { if the final shader code is empty (on this type) then don't insert anything
    (avoid creating shader without main()).

    For geometry shaders (EffectPartType = stGeometry),
    this check actually does nothing. Geometry shaders always have at least
    our code defining geometry_xxx functions, so they are never empty. }
  if Source[EffectPartType].Count = 0 then
    Exit;

  HasGeometryMain := HasGeometryMain or
    ( (EffectPartType = stGeometry) and (Pos('main()', PlugValue) <> 0) );

  repeat
    PlugName := FindPlugName(PlugValue, PlugDeclaredParameters);
    if PlugName = '' then Break;

    { When using some special plugs, we need to do define some symbols. }
    if PlugName = 'texture_coord_shift' then
      PlugDirectly(Source[stFragment], 0, '/* PLUG-DECLARATIONS */',
        '#define HAS_TEXTURE_COORD_SHIFT', false);
    if PlugName = 'texture_apply' then
      WritelnWarning('Using PLUG_texture_apply is deprecated. Upgrade to ' +
        '1. PLUG_main_texture_apply (to do something after main texture application; but remember that it happens *before* lighting application in case of Phong shading), ' +
        '2. or PLUG_fragment_modify (to do final fragment color adjustment)');

    { PlugName is not needed below to make this unique,
      but it makes reading shader code easier. }
    ProcedureName := 'plugged_' + IntToStr(PlugIdentifiers) + '_' + PlugName;
    StringReplaceAllVar(PlugValue, 'PLUG_' + PlugName, ProcedureName, false);
    Inc(PlugIdentifiers);

    PlugForwardDeclaration := DeclareOnce(ProcedureName,
      'void ' + ProcedureName + PlugDeclaredParameters + ';');

    AnyOccurrences := LookForPlugDeclaration(Code);
    { If the plug declaration not found in Code, then try to find it in
      the final shader. This happens if your Code is special for given
      light/texture effect, and you try to use a plug that
      is not special to the light/texture effect. For example,
      using PLUG_vertex_object_space inside a X3DTextureNode.effects. }
    if (not AnyOccurrences) and
       (Code <> Source[EffectPartType]) then
      AnyOccurrences := LookForPlugDeclaration(Source[EffectPartType]);

    if (not AnyOccurrences) and WarnMissingPlugs then
      WritelnWarning('VRML/X3D', Format('Plug name "%s" not declared (in shader type "%s")',
        [PlugName, ShaderTypeName[EffectPartType]]));
  until false;

  { regardless if any (and how many) plug points were found,
    always insert PlugValue into Code }
  Code.Add(PlugValue);
end;

function TShader.PlugDirectly(Code: TCastleStringList;
  const CodeIndex: Cardinal;
  const PlugName, PlugValue: String;
  const InsertAtBeginIfNotFound: boolean): boolean;
var
  P: Integer;
begin
  Result := false;

  if CodeIndex < Code.Count then
  begin
    P := Pos(PlugName, Code[CodeIndex]);
    if P <> 0 then
    begin
      Code[CodeIndex] := InsertIntoString(Code[CodeIndex], P, PlugValue + NL);
      Result := true;
    end else
    if InsertAtBeginIfNotFound then
    begin
      Code[CodeIndex] := PlugValue + NL + Code[CodeIndex];
      Result := true;
    end;
  end;

  if (not Result) and WarnMissingPlugs then
    WritelnWarning('VRML/X3D', Format('Plug point "%s" not found', [PlugName]));
end;

procedure TShader.Define(const DefineName: String; const ShaderType: TShaderType;
  const PlugEarly: Boolean = false);
var
  Declaration, PlugComment: String;
  Code: TCastleStringList;
  {$ifndef OpenGLES}
  I: Integer;
  {$endif}
begin
  Declaration := '#define ' + DefineName;
  Code := Source[ShaderType];
  PlugComment := Iff(PlugEarly, '/* PLUG-DECLARATIONS-EARLY */', '/* PLUG-DECLARATIONS */');

  {$ifdef OpenGLES}
  { Do not add it to all Source[stXxx], as then GLSL compiler
    will say "COLOR_PER_VERTEX macro redefinition",
    because we glue all parts for OpenGLES. }
  if Code.Count > 0 then
    PlugDirectly(Code, 0, PlugComment, Declaration, true);
  {$else}
  for I := 0 to Code.Count - 1 do
    PlugDirectly(Code, I, PlugComment, Declaration, true);
  {$endif}
end;

procedure TShader.EnableEffects(Effects: TMFNode;
  const Code: TShaderSource;
  const ForwardDeclareInFinalShader: boolean);
begin
  EnableEffects(Effects.InternalItems, Code, ForwardDeclareInFinalShader);
end;

procedure TShader.EnableEffects(Effects: TX3DNodeList;
  const Code: TShaderSource;
  const ForwardDeclareInFinalShader: boolean);

  procedure EnableEffect(Effect: TEffectNode);

    procedure EnableEffectPart(Part: TEffectPartNode);
    var
      Contents: String;
    begin
      Contents := Part.Contents;
      if Contents <> '' then
      begin
        Plug(Part.ShaderType, Contents, Code, ForwardDeclareInFinalShader);
        { Right now, for speed, we do not call EnableEffects, or even Plug,
          before LinkProgram. At which point ShapeRequiresShaders
          is already known true. }
        Assert(ShapeRequiresShaders);
      end;
    end;

  var
    I: Integer;
  begin
    if not Effect.FdEnabled.Value then Exit;

    if not (Effect.Language in [slDefault, slGLSL]) then
    begin
      WritelnWarning('VRML/X3D', Format('Unknown shading language "%s" for Effect node',
        [Effect.FdLanguage.Value]));
      Exit;
    end;

    for I := 0 to Effect.FdParts.Count - 1 do
      if Effect.FdParts[I] is TEffectPartNode then
        EnableEffectPart(TEffectPartNode(Effect.FdParts[I]));

    UniformsNodes.Add(Effect);
  end;

var
  I: Integer;
begin
  for I := 0 to Effects.Count - 1 do
    if Effects[I] is TEffectNode then
      EnableEffect(TEffectNode(Effects[I]));
end;

procedure TShader.LinkProgram(AProgram: TX3DShaderProgram;
  const ShapeNiceName: String);
var
  TextureApply, TextureColorDeclare, TextureCoordInitialize, TextureCoordMatrix,
    TextureAttributeDeclare, TextureVaryingDeclareVertex, TextureVaryingDeclareFragment, TextureUniformsDeclare,
    GeometryVertexDeclare, GeometryVertexSet, GeometryVertexZero, GeometryVertexAdd: String;
  TextureUniformsSet: Boolean;

const
  Structures: array [TLightingModel] of String = (
    {$I lighting_model_phong_structures.glsl.inc},
    {$I lighting_model_physical_structures.glsl.inc},
    {$I lighting_model_unlit_structures.glsl.inc}
  );

  procedure EnableLightingModel;
  const
    LightingModelCode: array [TLightingModel, { PhongShading } Boolean] of String = (
      (
        {$I lighting_model_phong_shading_gouraud.vs.inc},
        {$I lighting_model_phong_shading_phong.fs.inc}
      ),
      (
        {$I lighting_model_physical_shading_gouraud.vs.inc},
        {$I lighting_model_physical_shading_phong.fs.inc}
      ),
      (
        {$I lighting_model_unlit_shading_gouraud.vs.inc},
        {$I lighting_model_unlit_shading_phong.fs.inc}
      )
    );
  var
    LightingStage: TShaderType;
    LightShader: TLightShader;
  begin
    { Just like in EnableLights, don't add lights code in some cases }
    if (Source[stFragment].Count = 0) or
       (Source[stVertex].Count = 0) or
       (SelectedNode <> nil) then
      Exit;

    if PhongShading then
      LightingStage := stFragment
    else
      LightingStage := stVertex;

    Source[LightingStage][0] := StringReplace(Source[LightingStage][0],
      '/* CASTLE-LIGHTING-MODEL */',
      LightingModelCode[LightingModel, PhongShading], []);

    Source[LightingStage][0] := StringReplace(Source[LightingStage][0],
      '/* PLUG-DECLARATIONS */',
      Structures[LightingModel] + '/* PLUG-DECLARATIONS */', []);

    for LightShader in LightShaders do
      LightShader.LightingModel := LightingModel;
  end;

  procedure RequireTextureCoordinateForSurfaceTextures;

    { Make sure TextureShaders has an item
      with TextureUnit = given TextureCoordinateId. }
    procedure RequireTextureCoordinateId(const TextureCoordinateId: Cardinal);
    var
      I: Integer;
      TexCoordShader: TTextureCoordinateShader;
    begin
      for I := 0 to TextureShaders.Count - 1 do
        if TextureShaders[I].TextureUnit = TextureCoordinateId then
          Exit;

      { item with necessary TextureUnit not found, so create it }
      TexCoordShader := TTextureCoordinateShader.Create;
      TexCoordShader.HasMatrixTransform := TextureMatrix.IndexOf(TextureCoordinateId) <> -1;
      TexCoordShader.TextureUnit := TextureCoordinateId;
      TextureShaders.Add(TexCoordShader);

      { Note that we don't call

          TexCoordShader.Prepare(FCodeHash);

        to change the hash at this point. It is not needed (the fact that
        we use bump mapping or some "surface texture" was already
        recorded in the hash), and changing hash at this point
        could have bad consequences. }
    end;

  var
    SurfaceTexture: TSurfaceTexture;
  begin
    if FBumpMappingShader.BumpMapping <> bmNone then
      RequireTextureCoordinateId(FBumpMappingShader.NormalMapTextureCoordinatesId);

    if MainTextureMapping <> -1 then
      RequireTextureCoordinateId(MainTextureMapping);

    for SurfaceTexture := Low(TSurfaceTexture) to High(TSurfaceTexture) do
      if FSurfaceTextureShaders.Items[SurfaceTexture].Enable then
        RequireTextureCoordinateId(
          FSurfaceTextureShaders.Items[SurfaceTexture].TextureCoordinatesId);
  end;

  procedure EnableTextures;
  var
    I: Integer;
  begin
    TextureApply := '';
    TextureColorDeclare := '';
    TextureCoordInitialize := '';
    TextureCoordMatrix := '';
    TextureAttributeDeclare := '';
    TextureVaryingDeclareVertex := '';
    TextureVaryingDeclareFragment := '';
    TextureUniformsDeclare := '';
    GeometryVertexDeclare := '';
    GeometryVertexSet := '';
    GeometryVertexZero := '';
    GeometryVertexAdd := '';
    TextureUniformsSet := true;

    for I := 0 to TextureShaders.Count - 1 do
      TextureShaders[I].Enable(MainTextureMapping, MultiTextureColor,
        TextureApply, TextureColorDeclare,
        TextureCoordInitialize, TextureCoordMatrix,
        TextureAttributeDeclare, TextureVaryingDeclareVertex, TextureVaryingDeclareFragment, TextureUniformsDeclare,
        GeometryVertexDeclare, GeometryVertexSet, GeometryVertexZero, GeometryVertexAdd);
  end;

  { Applies to shader necessary clip plane code, using ClipPlanes value. }
  procedure EnableClipPlanes;
  var
    I: Integer;
    PlaneName: String;
    PlugVertexDeclarations, PlugVertexImplementation, PlugFragmentImplementation: String;
  begin
    { This routine closely cooperates with method EnableClipPlane to set
      up the necessary OpenGL(ES) state.
      See TClipPlaneAlgorithm for comments what and why we do. }

    if ClipPlanesCount <> 0 then
    begin
      case FClipPlaneAlgorithm of
        {$ifndef OpenGLES}
        cpFixedFunction:
          begin
            Plug(stVertex,
              'void PLUG_vertex_eye_space(const in vec4 vertex_eye, const in vec3 normal_eye)' +NL+
              '{' +NL+
              '  gl_ClipVertex = vertex_eye;' +NL+
              '}');
          end;

        cpClipDistance:
          begin
            PlugVertexDeclarations := '';
            PlugVertexImplementation := '';

            for I := 0 to ClipPlanesCount - 1 do
            begin
              PlaneName := 'castle_ClipPlane' + IntToStr(I);
              PlugVertexDeclarations := PlugVertexDeclarations +
                'uniform vec4 ' + PlaneName + ';' + NL;
              PlugVertexImplementation := PlugVertexImplementation +
                '  gl_ClipDistance[' + IntToStr(I) + '] = dot(' + PlaneName + ', vertex_eye);' + NL;
            end;

            Plug(stVertex,
              PlugVertexDeclarations +
              'void PLUG_vertex_eye_space(const in vec4 vertex_eye, const in vec3 normal_eye)' +NL+
              '{' +NL+
              PlugVertexImplementation +
              '}');
          end;
        {$endif}

        cpDiscard:
          begin
            PlugVertexDeclarations := '';
            PlugVertexImplementation := '';
            PlugFragmentImplementation := '';

            for I := 0 to ClipPlanesCount - 1 do
            begin
              PlaneName := 'castle_ClipPlane' + IntToStr(I);
              PlugVertexDeclarations := PlugVertexDeclarations +
                'uniform vec4 ' + PlaneName + ';' + NL;
              PlugVertexImplementation := PlugVertexImplementation +
                '  castle_ClipDistance[' + IntToStr(I) + '] = dot(' + PlaneName + ', vertex_eye);' + NL;
              PlugFragmentImplementation := PlugFragmentImplementation +
                '  if (castle_ClipDistance[' + IntToStr(I) + '] < 0.0) discard;' + NL;
            end;

            Plug(stVertex,
              'varying float castle_ClipDistance[' + IntToStr(ClipPlanesCount) + '];' +NL+
              PlugVertexDeclarations +
              'void PLUG_vertex_eye_space(const in vec4 vertex_eye, const in vec3 normal_eye)' +NL+
              '{' +NL+
              PlugVertexImplementation +
              '}');

            Plug(stFragment,
              'varying float castle_ClipDistance[' + IntToStr(ClipPlanesCount) + '];' +NL+
              'void PLUG_main_texture_apply(inout vec4 fragment_color, const in vec3 normal)' +NL+
              '{' +NL+
              PlugFragmentImplementation +
              '}');
          end;
      end;
    end;

    (* TODO: make this work with geometry shaders: (instead of 0, add each index)
    ClipPlaneGeometryPlug :=
      '#version 150 compatibility' +NL+
      'void PLUG_geometry_vertex_set(const int index)' +NL+
      '{' +NL+
      '  gl_ClipDistance[0] = gl_in[index].gl_ClipDistance[0];' +NL+
      '}' +NL+
      'void PLUG_geometry_vertex_zero()' +NL+
      '{' +NL+
      '  gl_ClipDistance[0] = 0.0;' +NL+
      '}' +NL+
      'void PLUG_geometry_vertex_add(const int index, const float scale)' +NL+
      '{' +NL+
      '  gl_ClipDistance[0] += gl_in[index].gl_ClipDistance[0] * scale;' +NL+
      '}' +NL;
    *)
  end;

  { Applies effects from various strings here.
    This also finalizes applying textures. }
  procedure EnableInternalEffects;
  const
    ShadowMapsFunctions: array [TShadowSampling] of string =
    (                               {$I shadow_map_common.fs.inc},
     '#define PCF4'          + NL + {$I shadow_map_common.fs.inc},
     '#define PCF4_BILINEAR' + NL + {$I shadow_map_common.fs.inc},
     '#define PCF16'         + NL + {$I shadow_map_common.fs.inc},
     {$I variance_shadow_map_common.fs.inc});
    ToneMappingFunctions = {$I tone_mapping.fs.inc};
  var
    UniformsDeclare, TextureApplyPoint: String;
    I: Integer;
  begin
    PlugDirectly(Source[stVertex], 0, '/* PLUG: vertex_eye_space',
      TextureCoordInitialize + TextureCoordGen + TextureCoordMatrix, false);

    TextureApplyPoint := 'main_texture_apply';
    PlugDirectly(Source[stFragment], 0, '/* PLUG: ' + TextureApplyPoint,
      TextureColorDeclare + TextureApply, false);

    PlugDirectly(Source[stFragment], 0, '/* PLUG: fragment_end', FragmentEnd, false);

    PlugDirectly(Source[stGeometry], 0, '/* PLUG-DECLARATIONS'         , GeometryVertexDeclare, false);
    PlugDirectly(Source[stGeometry], 0, '/* PLUG: geometry_vertex_set' , GeometryVertexSet    , false);
    PlugDirectly(Source[stGeometry], 0, '/* PLUG: geometry_vertex_zero', GeometryVertexZero   , false);
    PlugDirectly(Source[stGeometry], 0, '/* PLUG: geometry_vertex_add' , GeometryVertexAdd    , false);

    UniformsDeclare := '';
    for I := 0 to DynamicUniforms.Count - 1 do
      UniformsDeclare := UniformsDeclare + DynamicUniforms[I].Declaration;
    if NeedsCameraInverseMatrix then
      UniformsDeclare := UniformsDeclare + 'uniform mat4 castle_CameraInverseMatrix;' + NL;

    if not (
      PlugDirectly(Source[stFragment], 0, '/* PLUG-DECLARATIONS */',
        TextureVaryingDeclareFragment + NL +
        TextureUniformsDeclare + NL +
        Iff(UsesShadowMaps, DeclareShadowFunctions, ''),
        false) and
      PlugDirectly(Source[stVertex], 0, '/* PLUG-DECLARATIONS */',
        UniformsDeclare +
        TextureAttributeDeclare + NL + TextureVaryingDeclareVertex,
        false) ) then
    begin
      { When we cannot find /* PLUG-DECLARATIONS */, it also means we have
        base shader from ComposedShader. In this case, forcing
        TextureXxxDeclare at the beginning of shader code
        (by InsertAtBeginIfNotFound) would be bad (in case ComposedShader
        has some #version at the beginning). So we choose the safer route
        to *not* integrate our texture handling with ComposedShader.

        We also remove uniform values for textures, to avoid
        "unused castle_texture_%d" warning. Setting TextureUniformsSet
        will make it happen. }
      TextureUniformsSet := false;
    end;

    { Don't add to empty Source[stFragment], in case ComposedShader
      doesn't want any fragment shader.
      Only add if we're not using shaders from custom ComposedShader
      (SelectedNode not set). }
    if (Source[stFragment].Count <> 0) and
       (SelectedNode = nil) then
    begin
      if UsesShadowMaps then
        Source[stFragment].Add(ShadowMapsFunctions[ShadowSampling]);
      Source[stFragment].Add(ToneMappingFunctions);
    end;
  end;

var
  PassLightsUniforms: boolean;

  procedure EnableLights;
  var
    LightShader: TLightShader;
    LightShaderCode: TShaderSource;
    LightingStage: TShaderType;
    LightShaderCodeBase: String;
  begin
    PassLightsUniforms := false;

    { If we have no fragment/vertex shader (means that we used ComposedShader
      node without one shader) then don't add any code.
      Otherwise we would create a shader without any main() inside.

      Source.Append later also has some safeguard against this,
      but we need to check it earlier (to avoid plugging LightShaderBack),
      and check them both (as vertex and fragment code cooperates,
      so we need both or none).

      Also don't add anything in case we're rendering a custom ComposedShader node. }
    if (Source[stFragment].Count = 0) or
       (Source[stVertex].Count = 0) or
       (SelectedNode <> nil) then
      Exit;

    if Lighting then
    begin
      Source[stFragment][0] := '#define LIT' + NL + Source[stFragment][0];
      Source[stVertex  ][0] := '#define LIT' + NL + Source[stVertex  ][0];

      PassLightsUniforms := true;

      if PhongShading then
        LightingStage := stFragment
      else
        LightingStage := stVertex;

      for LightShader in LightShaders do
      begin
        LightShaderCode := LightShader.Code;

        // Plug the "base" GLSL code of the light (with PLUG_add_light)
        LightShaderCodeBase := LightShaderCode[LightingStage][0];
        LightShaderCodeBase :=
          // when "separate compilation units" are used (desktop OpenGL), need to redeclare structures
          {$ifndef OpenGLES} Structures[LightingModel] + {$endif}
          LightShaderCodeBase;
        Plug(LightingStage, LightShaderCodeBase);

        { Append the rest of LightShader, it may contain shadow maps utilities
          and light plugs. }
        Source.Append(LightShaderCode, LightingStage);
      end;
    end else
    begin
      // TODO: fix this to pass color in new shaders (not using deprecated gl_FrontColor, gl_BackColor)
      // Plug(stGeometry, GeometryShaderPassColors);
    end;
  end;

  { Define COLOR_PER_VERTEX* symbols for GLSL. }
  procedure EnableShaderColorPerVertex;
  begin
    if ColorPerVertexType <> ctNone then
    begin
      { TODO: need to pass castle_ColorPerVertexFragment onward?
      Plug(stGeometry, GeometryShaderPassColors);
      }

      Define('COLOR_PER_VERTEX', stVertex);
      Define('COLOR_PER_VERTEX', stFragment);
      case ColorPerVertexMode of
        cmReplace:
          begin
            Define('COLOR_PER_VERTEX_REPLACE', stVertex);
            Define('COLOR_PER_VERTEX_REPLACE', stFragment);
          end;
        cmModulate:
          begin
            Define('COLOR_PER_VERTEX_MODULATE', stVertex);
            Define('COLOR_PER_VERTEX_MODULATE', stFragment);
          end;
      end;
      case ColorPerVertexType of
        ctNone: ;
        ctRgb:
          begin
            Define('COLOR_PER_VERTEX_RGB', stVertex);
            Define('COLOR_PER_VERTEX_RGB', stFragment);
          end;
        ctRgbAlpha:
          begin
            Define('COLOR_PER_VERTEX_RGB_ALPHA', stVertex);
            Define('COLOR_PER_VERTEX_RGB_ALPHA', stFragment);
          end;
      end;
    end;
  end;

  procedure EnableShaderFog;
  var
    FogFactor, FogUniforms, CoordinateSource: String;
  begin
    if FFogEnabled then
    begin
      {$ifdef CASTLE_WEBGL}
      { Shader fails compilation with
          EGLSLShaderCompileError: Vertex shader not compiled:
          0(13) : error C1503: undefined variable "_ucastle_FogFragCoord" }
      WritelnWarning('WebGL', 'Fog is not supported with WebGL yet');
      Exit;
      {$endif}

      case FFogCoordinateSource of
        fcDepth           : CoordinateSource := '-vertex_eye.z';
        fcPassedCoordinate: CoordinateSource := 'castle_FogCoord';
        {$ifndef COMPILER_CASE_ANALYSIS}
        else raise EInternalError.Create('TShader.EnableShaderFog:FogCoordinateSource?');
        {$endif}
      end;

      Plug(stVertex,
        'attribute float castle_FogCoord;' +NL+
        'varying float castle_FogFragCoord;' + NL+
        'void PLUG_vertex_eye_space(const in vec4 vertex_eye, const in vec3 normal_eye)' +NL+
        '{' +NL+
        '  castle_FogFragCoord = ' + CoordinateSource + ';' +NL+
        '}');

      FogUniforms := 'uniform float castle_FogVisibilityRange;';

      // See equations in https://www.web3d.org/specifications/X3Dv4Draft/ISO-IEC19775-1v4-CD1/Part01/components/lighting.html#t-foginterpolant
      case FFogType of
        ftLinear:
          begin
            FogFactor := '1.0 - castle_FogFragCoord / castle_FogVisibilityRange';
          end;
        ftExponential:
          begin
            FogFactor := '(castle_FogFragCoord < castle_FogVisibilityRange ? exp(-castle_FogFragCoord / (castle_FogVisibilityRange - castle_FogFragCoord)) : 0.0)';
          end;
        {$ifndef COMPILER_CASE_ANALYSIS}
        else raise EInternalError.Create('TShader.EnableShaderFog:FogType?');
        {$endif}
      end;

      Plug(stFragment,
        'varying float castle_FogFragCoord;' + NL+
        'uniform vec3 castle_FogColor;' +NL+
        FogUniforms + NL +
        'void PLUG_fog_apply(inout vec4 fragment_color, const vec3 normal_eye_fragment)' +NL+
        '{' +NL+
        '  fragment_color.rgb = mix(castle_FogColor, fragment_color.rgb, ' +NL+
        '    clamp(' + FogFactor + ', 0.0, 1.0));' +NL+
        '}');
    end;
  end;

  procedure SetupUniformsOnce;
  var
    I: Integer;
  begin
    AProgram.Enable;

    if TextureUniformsSet then
    begin
      for I := 0 to TextureShaders.Count - 1 do
        if (TextureShaders[I] is TTextureShader) and
           (TTextureShader(TextureShaders[I]).UniformName <> '') then
          AProgram.SetUniform(TTextureShader(TextureShaders[I]).UniformName,
                              TTextureShader(TextureShaders[I]).UniformValue);
    end;

    FBumpMappingShader.SetUniformsOnce(AProgram);
    FSurfaceTextureShaders.SetUniformsOnce(AProgram);

    AProgram.BindUniforms(UniformsNodes, false);

    if PassLightsUniforms then
      for I := 0 to LightShaders.Count - 1 do
        LightShaders[I].SetUniformsOnce(AProgram);

    ShadowMapShaders.SetUniformsOnce(AProgram);

    AProgram.Disable;
  end;

  procedure EnableMirrorPlaneTexCoords;
  begin
    if NeedsMirrorPlaneTexCoords then
      Define('CASTLE_NEEDS_MIRROR_PLANE_TEX_COORDS', stVertex);
  end;

  procedure PrepareCommonCode;
  begin
    if Source[stVertex].Count > 0 then
      Source[stVertex][0] := StringReplace(Source[stVertex][0],
        '/* CASTLE-COMMON-CODE */', {$I common.vs.inc}, [rfReplaceAll]);
    if Source[stFragment].Count > 0 then
      Source[stFragment][0] := StringReplace(Source[stFragment][0],
        '/* CASTLE-COMMON-CODE */', {$I common.fs.inc}, [rfReplaceAll]);
  end;

var
  ShaderType: TShaderType;
  GeometryInputSize: String;
  I: Integer;
begin
  EnableLightingModel; // do this early, as later EnableLights may assume it's done
  ShadowMapShaders.GenerateCode(Self);
  RequireTextureCoordinateForSurfaceTextures;
  EnableTextures;
  EnableClipPlanes;
  EnableInternalEffects;
  EnableLights;
  EnableShaderColorPerVertex;
  PrepareCommonCode; // must be before FBumpMappingShader.GenerateCode to define PLUG texture_coord_shift
  FBumpMappingShader.GenerateCode(Self);
  FSurfaceTextureShaders.GenerateCode(Self);
  EnableShaderFog;
  if AppearanceEffects <> nil then
    EnableEffects(AppearanceEffects);
  if GroupEffects <> nil then
    EnableEffects(GroupEffects);
  EnableMirrorPlaneTexCoords;

  if HasGeometryMain then
  begin
    Define('HAS_GEOMETRY_SHADER', stFragment);
    if GLVersion.VendorType = gvATI then
      GeometryInputSize := 'gl_in.length()' else
      GeometryInputSize := '';
    { Replace CASTLE_GEOMETRY_INPUT_SIZE }
    for I := 0 to Source[stGeometry].Count - 1 do
      Source[stGeometry][I] := StringReplace(Source[stGeometry][I],
        'CASTLE_GEOMETRY_INPUT_SIZE', GeometryInputSize, [rfReplaceAll]);
  end else
    Source[stGeometry].Clear;

  if GLVersion.BuggyGLSLFrontFacing then
    Define('CASTLE_BUGGY_FRONT_FACING', stFragment);
  if GLVersion.BuggyGLSLReadVarying then
    Define('CASTLE_BUGGY_GLSL_READ_VARYING', stVertex);
  if GLVersion.BuggyGLSLBumpMappingNumSteps then
    Define('CASTLE_BUGGY_BUMP_MAPPING_NUM_STEPS', stFragment);
  if ColorSpaceLinear then
    Define('CASTLE_GAMMA_CORRECTION', stFragment);
  if NeedsNormals then
    Define('CASTLE_HAS_NORMALS', stVertex, true);
  case ToneMapping of
    tmNone: ;
    tmUncharted:
      begin
        Define('CASTLE_TONE_MAPPING', stFragment);
        Define('CASTLE_TONE_MAPPING_UNCHARTED', stFragment);
      end;
    tmHejlRichard:
      begin
        Define('CASTLE_TONE_MAPPING', stFragment);
        Define('CASTLE_TONE_MAPPING_HEJLRICHARD', stFragment);
      end;
    tmACES:
      begin
        Define('CASTLE_TONE_MAPPING', stFragment);
        Define('CASTLE_TONE_MAPPING_ACES', stFragment);
      end;
  end;

  try
    if (Source[stVertex].Count = 0) and
       (Source[stFragment].Count = 0) then
      raise EGLSLError.Create('No vertex and no fragment shader for GLSL program');

    for ShaderType := Low(ShaderType) to High(ShaderType) do
      AProgram.AttachShader(ShaderType, Source[ShaderType]);
    AProgram.Name := 'TShader:Shape:' + ShapeNiceName;
    AProgram.Link;

    if SelectedNode <> nil then
      SelectedNode.EventIsValid.Send(true);
  except
    if SelectedNode <> nil then
      SelectedNode.EventIsValid.Send(false);
    raise;
  end;

  { Note that user X3D uniform values go through SetUniformFromField,
    which passes UniformMissing based on node configuration
    (TEffectNode.UniformMissing, TComposedShaderNode.UniformMissing).

    So setting AProgram.UniformMissing below only controls what happens on our
    built-in uniform values.

    Missing uniform name should be ignored, as it's normal in some cases:
    When all the lights are off (including headlight) then normal vectors
    are unused, and so the normalmap texture is unused. }
  AProgram.UniformMissing := umIgnore;

  { set uniforms that will not need to be updated at each SetupUniforms call }
  SetupUniformsOnce;
end;

procedure TShader.LinkFallbackProgram(AProgram: TX3DShaderProgram);
const
  VS = {$I fallback.vs.inc};
  FS = {$I fallback.fs.inc};
begin
  AProgram.Name := 'TShader:Fallback';
  AProgram.AttachShader(stVertex, VS);
  AProgram.AttachShader(stFragment, FS);
  AProgram.Link;

  AProgram.UniformMissing := umIgnore;
end;

function TShader.CodeHash: TShaderCodeHash;

  { Add to FCodeHash some stuff that must be added at the end,
    since it can be changed back (replacing previous values) during TShader
    lifetime. }
  procedure CodeHashFinalize;
  begin
    FCodeHash.AddInteger(Ord(ShadowSampling) * 1009);
    FCodeHash.AddInteger(Ord(LightingModel) * 503);
    FCodeHash.AddInteger(Ord(ColorSpaceLinear) * 347);
    FCodeHash.AddInteger(Ord(ToneMapping) * 331);
    FCodeHash.AddInteger(Ord(MainTextureMapping) * 839);
    FCodeHash.AddFloat(MultiTextureColor.X, 5821);
    FCodeHash.AddFloat(MultiTextureColor.Y, 5827);
    FCodeHash.AddFloat(MultiTextureColor.Z, 5839);
    FCodeHash.AddFloat(MultiTextureColor.W, 5843);
  end;

begin
  if not CodeHashFinalized then
  begin
    CodeHashFinalize;
    CodeHashFinalized := true;
  end;
  Result := FCodeHash;
end;

procedure TShader.EnableTexture(const TextureUnit: Cardinal;
  const TextureType: TTextureType;
  const Node: TAbstractSingleTextureNode;
  const Env: TTextureEnv);
var
  TextureShader: TTextureShader;
begin
  { Enable for fixed-function pipeline }
  if GLFeatures.EnableFixedFunction then
  begin
    if GLFeatures.UseMultiTexturing then
      glActiveTexture(GL_TEXTURE0 + TextureUnit);
    case TextureType of
      tt2D     : GLEnableTexture(et2D);
      ttCubeMap: GLEnableTexture(etCubeMap);
      tt3D     : GLEnableTexture(et3D);
      ttShader : GLEnableTexture(etNone);
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('TextureEnableDisable?');
      {$endif}
    end;
  end;

  { Enable for shader pipeline }

  TextureShader := TTextureShader.Create;
  TextureShader.HasMatrixTransform := (TextureMatrix.IndexOf(TextureUnit) <> -1);
  TextureShader.TextureUnit := TextureUnit;
  TextureShader.TextureType := TextureType;
  TextureShader.Node := Node;
  TextureShader.Env := Env;
  TextureShader.Shader := Self;

  { Change csMaterial to csPreviousTexture on the 1st texture slot.
    This fixes the problem of TTextureShader.TextureEnvMix:

    - it has reliable definition of csPreviousTexture,
      that also works for the 1st texture slot
      (and means there "material base/alpha color", as we want).

    - it has hacky definition of csMaterial,
      that just returns hardcoded (0.8,0.8,0.8,1).
      This is good enough for rare cases when MultiTexture really uses
      source=DIFFUSE/SPECULAR.
      It is bad for the common case when this is the 1st texture slot
      (maybe even the only texture slot! when not using MultiTexture)
      that should be multiplied by material/per-vertex color.
  }
  if TextureShaders.Count = 0 then
  begin
    if TextureShader.Env.Source[cRGB] = csMaterial then
      TextureShader.Env.Source[cRGB] := csPreviousTexture;
    if TextureShader.Env.Source[cAlpha] = csMaterial then
      TextureShader.Env.Source[cAlpha] := csPreviousTexture;
  end;

  TextureShaders.Add(TextureShader);

  if (TextureType = ttShader) or
     (Node.FdEffects.Count <> 0) or
     { MultiTexture.function requires shaders }
     (Env.TextureFunction <> tfNone) then
    ShapeRequiresShaders := true;

  TextureShader.Prepare(FCodeHash);
end;

procedure TShader.EnableTexGen(const TextureUnit: Cardinal;
  const Generation: TTexGenerationComplete;
  const TransformToWorldSpace: boolean);
var
  TexCoordName: String;
begin
  { Enable for fixed-function pipeline }
  if GLFeatures.UseMultiTexturing then
    glActiveTexture(GL_TEXTURE0 + TextureUnit);
  { Rest of code for fixed-function pipeline
    (glTexGeni and glEnable(GL_TEXTURE_GEN_*)) is below }

  TexCoordName := TTextureShader.CoordName(TextureUnit);

  FCodeHash.AddInteger(
    1303 * (Ord(Generation) + 1) +
    1307 * (TextureUnit + 1));

  { Enable for fixed-function and shader pipeline }
  case Generation of
    tgSphere:
      begin
        if GLFeatures.EnableFixedFunction then
        begin
          {$ifndef OpenGLES}
          glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
          glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
          glEnable(GL_TEXTURE_GEN_S);
          glEnable(GL_TEXTURE_GEN_T);
          {$endif}
        end;
        TextureCoordGen := TextureCoordGen + Format('%s.st = castle_generate_tex_coords_sphere(castle_vertex_eye, castle_normal_eye);',
          [TexCoordName]);
        NeedsNormalsForTexGen := true;
      end;
    tgNormal:
      begin
        if GLFeatures.EnableFixedFunction then
        begin
          {$ifndef OpenGLES}
          glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP_ARB);
          glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP_ARB);
          glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP_ARB);
          glEnable(GL_TEXTURE_GEN_S);
          glEnable(GL_TEXTURE_GEN_T);
          glEnable(GL_TEXTURE_GEN_R);
          {$endif}
        end;
        TextureCoordGen := TextureCoordGen + Format('%s.xyz = castle_normal_eye;' + NL,
          [TexCoordName]);
        NeedsNormalsForTexGen := true;
      end;
    tgReflection:
      begin
        if GLFeatures.EnableFixedFunction then
        begin
          {$ifndef OpenGLES}
          glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP_ARB);
          glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP_ARB);
          glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP_ARB);
          glEnable(GL_TEXTURE_GEN_S);
          glEnable(GL_TEXTURE_GEN_T);
          glEnable(GL_TEXTURE_GEN_R);
          {$endif}
        end;
        { Negate reflect result --- just like for demo_models/water/water_reflections_normalmap.fs }
        TextureCoordGen := TextureCoordGen + Format('%s.xyz = -reflect(-vec3(castle_vertex_eye), castle_normal_eye);' + NL,
          [TexCoordName]);
        NeedsNormalsForTexGen := true;
      end;
    tgcMirrorPlane:
      begin
        NeedsMirrorPlaneTexCoords := true;
        NeedsCameraInverseMatrix := true;
        TextureCoordGen := TextureCoordGen + Format('%s.xyz = castle_generate_tex_coords_mirror_plane(castle_CameraInverseMatrix * castle_vertex_eye);' + NL,
          [TexCoordName]);
      end;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TShader.EnableTexGen:Generation?');
    {$endif}
  end;

  if TransformToWorldSpace then
  begin
    TextureCoordGen := TextureCoordGen + Format('%s.w = 0.0; %0:s = castle_CameraInverseMatrix * %0:s;' + NL,
      [TexCoordName]);
    NeedsCameraInverseMatrix := true;
    FCodeHash.AddInteger(263);
  end;
end;

procedure TShader.EnableTexGen(const TextureUnit: Cardinal;
  const Generation: TTexGenerationComponent; const Component: TTexComponent;
  const Plane: TVector4);
const
  PlaneComponentNames: array [TTexComponent] of char = ('S', 'T', 'R', 'Q');
  { Note: R changes to p ! }
  VectorComponentNames: array [TTexComponent] of char = ('s', 't', 'p', 'q');
var
  PlaneName, CoordSource, TexCoordName: String;
  Uniform: TDynamicUniformVec4;
begin
  { Enable for fixed-function pipeline }
  if GLFeatures.UseMultiTexturing then
    glActiveTexture(GL_TEXTURE0 + TextureUnit);

  if GLFeatures.EnableFixedFunction then
  begin
    {$ifndef OpenGLES}
    case Component of
      0: glEnable(GL_TEXTURE_GEN_S);
      1: glEnable(GL_TEXTURE_GEN_T);
      2: glEnable(GL_TEXTURE_GEN_R);
      3: glEnable(GL_TEXTURE_GEN_Q);
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('TShader.EnableTexGen:Component?');
      {$endif}
    end;
    {$endif}
  end;

  { Enable for shader pipeline.
    See helpful info about simulating glTexGen in GLSL in:
    http://www.mail-archive.com/osg-users@lists.openscenegraph.org/msg14238.html }

  case Generation of
    tgEye   : begin PlaneName := 'EyePlane'   ; CoordSource := 'castle_vertex_eye'; end;
    tgObject: begin PlaneName := 'ObjectPlane'; CoordSource := 'vertex_object' ; end;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TShader.EnableTexGen:Generation?');
    {$endif}
  end;

  PlaneName := 'castle_' + PlaneName + PlaneComponentNames[Component] +
    Format('%d', [TextureUnit]);

  Uniform := TDynamicUniformVec4.Create;
  Uniform.Name := PlaneName;
  Uniform.Declaration := 'uniform vec4 ' + PlaneName + ';' + NL;
  Uniform.Value := Plane;
  DynamicUniforms.Add(Uniform);

  TexCoordName := TTextureShader.CoordName(TextureUnit);
  TextureCoordGen := TextureCoordGen + Format('%s.%s = dot(%s, %s);' + NL,
    [TexCoordName, VectorComponentNames[Component], CoordSource, PlaneName]);
  FCodeHash.AddInteger(1319 * (TextureUnit + 1) * (Ord(Generation) + 1) * (Component + 1));
end;

procedure TShader.DisableTexGen(const TextureUnit: Cardinal);
begin
  {$ifndef OpenGLES}
  if GLFeatures.EnableFixedFunction then
  begin
    { Disable for fixed-function pipeline }
    if GLFeatures.UseMultiTexturing then
      glActiveTexture(GL_TEXTURE0 + TextureUnit);
    glDisable(GL_TEXTURE_GEN_S);
    glDisable(GL_TEXTURE_GEN_T);
    glDisable(GL_TEXTURE_GEN_R);
    glDisable(GL_TEXTURE_GEN_Q);
  end;
  {$endif}
end;

procedure TShader.EnableTextureTransform(const TextureUnit: Cardinal;
  const Matrix: TMatrix4);
var
  Uniform: TDynamicUniformMat4;
begin
  { pass the uniform value with transformation to shader }
  Uniform := TDynamicUniformMat4.Create;
  Uniform.Name := TTextureShader.MatrixName(TextureUnit);
  Uniform.Declaration := 'uniform mat4 ' + Uniform.Name + ';' + NL;
  Uniform.Value := Matrix;
  DynamicUniforms.Add(Uniform);

  { multiply by the uniform value in shader }
  TextureMatrix.Add(TextureUnit);

  FCodeHash.AddInteger(1973 * (TextureUnit + 1));
end;

procedure TShader.EnableClipPlane(const ClipPlaneIndex: Cardinal;
  const Plane: TVector4);
var
  Uniform: TDynamicUniformVec4;
begin
  { Note: This method closely cooperates with EnableClipPlanes,
    that adds necessary GLSL code to make it all work. }

  Assert(ClipPlanesCount = ClipPlaneIndex);
  Inc(ClipPlanesCount);

  { This effectively adds 2003 * ClipPlanesCount to hash. }
  FCodeHash.AddInteger(2003);

  {$ifndef OpenGLES}
  if GLFeatures.EnableFixedFunction or (not GLFeatures.Version_3_1) then
  begin
    FClipPlaneAlgorithm := cpFixedFunction;

    CastleGlClipPlane(GL_CLIP_PLANE0 + ClipPlaneIndex, Vector4Double(Plane));
    glEnable(GL_CLIP_PLANE0 + ClipPlaneIndex);
  end else

  if not ForceOpenGLESClipPlanes then
  begin
    FClipPlaneAlgorithm := cpClipDistance;

    glEnable(GL_CLIP_DISTANCE0 + ClipPlaneIndex);

    Uniform := TDynamicUniformVec4.Create;
    Uniform.Name := 'castle_ClipPlane' + IntToStr(ClipPlaneIndex);
    { We leave Uniform.Declaration empty,
      because we only declare them inside the necessary plug in EnableClipPlanes. }
    { Convert Plane from world space -> eye space. }
    Uniform.Value := PlaneTransform(Plane, SceneModelView);
    DynamicUniforms.Add(Uniform);
  end else

  {$endif}
  begin
    FClipPlaneAlgorithm := cpDiscard;

    Uniform := TDynamicUniformVec4.Create;
    Uniform.Name := 'castle_ClipPlane' + IntToStr(ClipPlaneIndex);
    { We leave Uniform.Declaration empty,
      because we only declare them inside the necessary plug in EnableClipPlanes. }
    { Convert Plane from world space -> eye space. }
    Uniform.Value := PlaneTransform(Plane, SceneModelView);
    DynamicUniforms.Add(Uniform);
  end;
end;

procedure TShader.DisableClipPlane(const ClipPlaneIndex: Cardinal);
begin
  {$ifndef OpenGLES}
  case FClipPlaneAlgorithm of
    cpFixedFunction:
      glDisable(GL_CLIP_PLANE0 + ClipPlaneIndex);
    cpClipDistance:
      glDisable(GL_CLIP_DISTANCE0 + ClipPlaneIndex);
    else ;
  end;
  {$endif}
end;

procedure TShader.EnableAlphaTest(const AlphaCutoff: Single);
var
  AlphaCutoffStr: String;
begin
  FAlphaTest := true;

  { Convert float to be a valid GLSL constant.
    Make sure to use dot, and a fixed notation. }
  AlphaCutoffStr := FloatToStrFDot(AlphaCutoff, ffFixed, { ignored } 0, 4);

  FragmentEnd := FragmentEnd +
    'if (gl_FragColor.a < ' + AlphaCutoffStr + ')' + NL +
    '  discard;' + NL;

  FCodeHash.AddInteger(2011);
  FCodeHash.AddFloat(AlphaCutoff, 2017);
end;

procedure TShader.EnableBumpMapping(const BumpMapping: TBumpMapping;
  const NormalMapTextureUnit, NormalMapTextureCoordinatesId: Cardinal;
  const NormalMapScale: Single;
  const HeightMapInAlpha: boolean; const HeightMapScale: Single);
begin
  FBumpMappingShader.BumpMapping := BumpMapping;
  FBumpMappingShader.NormalMapTextureUnit := NormalMapTextureUnit;
  FBumpMappingShader.NormalMapTextureCoordinatesId := NormalMapTextureCoordinatesId;
  FBumpMappingShader.NormalMapScale := NormalMapScale;
  FBumpMappingShader.HeightMapInAlpha := HeightMapInAlpha;
  FBumpMappingShader.HeightMapScale := HeightMapScale;
  FBumpMappingShader.PrepareHash(FCodeHash);

  if BumpMapping <> bmNone then
    ShapeRequiresShaders := true;
end;

procedure TShader.EnableSurfaceTexture(const SurfaceTexture: TSurfaceTexture;
  const TextureUnit, TextureCoordinatesId: Cardinal;
  const UniformTextureName, PlugCode: String);
begin
  FSurfaceTextureShaders.Items[SurfaceTexture].Enable := true;
  FSurfaceTextureShaders.Items[SurfaceTexture].TextureUnit := TextureUnit;
  FSurfaceTextureShaders.Items[SurfaceTexture].TextureCoordinatesId := TextureCoordinatesId;
  FSurfaceTextureShaders.Items[SurfaceTexture].UniformTextureName := UniformTextureName;
  FSurfaceTextureShaders.Items[SurfaceTexture].PlugCode := PlugCode;

  ShapeRequiresShaders := true;

  FSurfaceTextureShaders.Items[SurfaceTexture].PrepareHash(FCodeHash, SurfaceTexture);
end;

procedure TShader.EnableLight(const Number: Cardinal; Light: PLightInstance);
var
  LightShader: TLightShader;
begin
  LightShader := TLightShader.Create;
  LightShader.Number := Number;
  LightShader.Light := Light;
  LightShader.Node := Light^.Node;
  LightShader.Shader := Self;
  LightShader.RenderingCamera := RenderingCamera;

  LightShaders.Add(LightShader);

  if Light^.Node.FdEffects.Count <> 0 then
    ShapeRequiresShaders := true;

  LightShader.Prepare(FCodeHash, LightShaders.Count - 1);
end;

procedure TShader.EnableFog(const FogType: TFogType;
  const FogCoordinateSource: TFogCoordinateSource;
  const FogColor: TVector3; const FogVisibilityRange: Single);
var
  UColor: TDynamicUniformVec3;
  USingle: TDynamicUniformSingle;
begin
  FFogEnabled := true;
  FFogType := FogType;
  FFogCoordinateSource := FogCoordinateSource;
  FFogColor := FogColor;
  FFogVisibilityRange := FogVisibilityRange;
  FCodeHash.AddInteger(
    67 * (Ord(FFogType) + 1) +
    709 * (Ord(FFogCoordinateSource) + 1));

  if FFogEnabled then
  begin
    USingle := TDynamicUniformSingle.Create;
    USingle.Name := 'castle_FogVisibilityRange';
    USingle.Value := FFogVisibilityRange;
    DynamicUniforms.Add(USingle);

    UColor := TDynamicUniformVec3.Create;
    UColor.Name := 'castle_FogColor';
    UColor.Value := FFogColor;
    { We leave UColor.Declaration empty, just like USingle.Declaration above,
      because we only declare them inside this plug
      (which is a separate compilation unit for desktop OpenGL). }
    DynamicUniforms.Add(UColor);
  end;
end;

procedure TShader.ModifyFog(const FogType: TFogType;
  const FogCoordinateSource: TFogCoordinateSource;
  const FogVisibilityRange: Single);
begin
  { Do not enable fog, or change it's color. Only work if fog already enabled. }
  FFogType := FogType;
  FFogCoordinateSource := FogCoordinateSource;
  FFogVisibilityRange := FogVisibilityRange;

  FCodeHash.AddInteger(
    431 * (Ord(FFogType) + 1) +
    433 * (Ord(FFogCoordinateSource) + 1));
end;

function TShader.EnableCustomShaderCode(const Shaders: TMFNode;
  out Node: TComposedShaderNode): boolean;
var
  I, J: Integer;
  Part: TShaderPartNode;
  PartSource: String;
  PartType, SourceType: TShaderType;
begin
  Result := false;
  for I := 0 to Shaders.Count - 1 do
  begin
    if (Shaders[I] is TComposedShaderNode) and
       (TComposedShaderNode(Shaders[I]).Language in [slDefault, slGLSL]) then
      Node := TComposedShaderNode(Shaders[I])
    else
      Node := nil;

    if Node <> nil then
    begin
      Result := true;

      { Clear whole Source }
      for SourceType := Low(SourceType) to High(SourceType) do
        if SourceType <> stGeometry then
          Source[SourceType].Count := 0;

      { Iterate over Node.FdParts, looking for vertex shaders
        and fragment shaders. }
      for J := 0 to Node.FdParts.Count - 1 do
        if Node.FdParts[J] is TShaderPartNode then
        begin
          Part := TShaderPartNode(Node.FdParts[J]);
          PartSource := Part.Contents;
          if PartSource <> '' then
          begin
            PartType := Part.ShaderType;
            Source[PartType].Add(PartSource);
            if PartType = stGeometry then
              HasGeometryMain := true;
          end;
        end;

      Node.EventIsSelected.Send(true);

      UniformsNodes.Add(Node);

      { For sending isValid to this node later }
      SelectedNode := Node;

      { Ignore missing plugs, as our plugs are (probably) not found there }
      WarnMissingPlugs := false;

      ShapeRequiresShaders := true;

      { We add to FCodeHash custom shader node.

        We don't add the source code (all PartSource), we just add node
        reference, for reasoning see TShaderCodeHash.AddEffects (equal
        source code may still mean different uniforms).
        Also, adding a node reference is faster that calculating string hash.

        Note that our original shader code (from glsl/template*)
        is never added to hash --- there's no need, after all it's
        always constant. }
      FCodeHash.AddPointer(Node);

      Break;
    end else
    if Shaders[I] is TAbstractShaderNode then
      TAbstractShaderNode(Shaders[I]).EventIsSelected.Send(false);
  end;
end;

procedure TShader.EnableAppearanceEffects(Effects: TMFNode);
begin
  AppearanceEffects := Effects;
  if AppearanceEffects.Count <> 0 then
  begin
    ShapeRequiresShaders := true;
    FCodeHash.AddEffects(AppearanceEffects);
  end;
end;

procedure TShader.EnableGroupEffects(Effects: TX3DNodeList);
begin
  GroupEffects := Effects;
  if GroupEffects.Count <> 0 then
  begin
    ShapeRequiresShaders := true;
    FCodeHash.AddEffects(GroupEffects);
  end;
end;

procedure TShader.EnableLighting;
begin
  Lighting := true;
  FCodeHash.AddInteger(7);
end;

procedure TShader.EnableColorPerVertex(const AMode: TColorMode; const AColorType: TColorPerVertexType);
begin
  Assert(AColorType <> ctNone);
  { This will cause appropriate shader later }
  ColorPerVertexType := AColorType;
  ColorPerVertexMode := AMode;
  FCodeHash.AddInteger(
    (Ord(AMode) + 1) * 29 +
    (Ord(AColorType) + 1) * 601
  );
end;

function TShader.DeclareShadowFunctions: String;
const
  ShadowDeclare: array [boolean { vsm? }] of string =
  ('float shadow(sampler2DShadow shadowMap, const vec4 shadowMapCoord, const in float size);',
   'float shadow(sampler2D       shadowMap, const vec4 shadowMapCoord, const in float size);');
begin
  Result := ShadowDeclare[ShadowSampling = ssVarianceShadowMaps];
end;

procedure TShader.SetDynamicUniforms(AProgram: TX3DShaderProgram);
var
  I: Integer;
begin
  for I := 0 to LightShaders.Count - 1 do
    LightShaders[I].SetDynamicUniforms(AProgram);
  for I := 0 to DynamicUniforms.Count - 1 do
    DynamicUniforms[I].SetUniform(AProgram);

  if NeedsCameraInverseMatrix then
  begin
    RenderingCamera.InverseMatrixNeeded;
    AProgram.SetUniform('castle_CameraInverseMatrix', RenderingCamera.InverseMatrix);
  end;

  if NeedsMirrorPlaneTexCoords and (MirrorPlaneUniforms <> nil) then
    MirrorPlaneUniforms.SetDynamicUniforms(AProgram);

  ShadowMapShaders.SetDynamicUniforms(AProgram, RenderingCamera);
end;

procedure TShader.AddScreenEffectCode(const Depth: boolean);
var
  VS, FS: String;
begin
  VS := ScreenEffectVertex;
  FS := ScreenEffectFragment(Depth);

  { Fragment shader code is required, as FS doesn't define main().
    Raising exception here prevents getting later a less clear exception
    message from OpenGL(ES),
    see https://github.com/castle-engine/castle-engine/issues/509 .

    Note that EGLSLError will be captured and turned into warning
    by TScreenEffectResource.PrepareCore .
    This is desirable for castle-model-viewer, as it should display only a warning
    for invalid X3D models. }

  if Source[stFragment].Count = 0 then
    raise EGLSLError.Create('No fragment shader code provided for a screen effect');

  Source[stVertex].Insert(0, VS);

  { For OpenGLES, ScreenEffectLibrary must be 1st shader,
    and it will be glued with the user shader code.
    So we glue it also on desktop OpenGL, for consistency
    (so e.g. you should never repeat "uniform screen_width...").  }

  Assert(Source[stFragment].Count <> 0); // make check it above
  Source[stFragment][0] := FS + Source[stFragment][0];
end;

function TShader.NeedsNormals: Boolean;
begin
  // optimize lmUnlit: no need for normals data
  Result := (LightingModel <> lmUnlit) or NeedsNormalsForTexGen;
end;

function TShader.ShapeBoundingBoxInWorld: TBox3D;
begin
  if not FShapeBoundingBoxInWorldKnown then
  begin
    FShapeBoundingBoxInWorldKnown := true;
    FShapeBoundingBoxInWorld := ShapeBoundingBoxInSceneEvent().Transform(SceneTransform);
  end;
  Result := FShapeBoundingBoxInWorld;
end;

procedure TShader.EnableShadowMap(const TextureUnit: Cardinal;
  const ShadowMap: TGeneratedShadowMapNode);
var
  ShadowMapShader: TShadowMapShader;
begin
  ShapeRequiresShaders := true;
  UsesShadowMaps := true;

  { This method doesn't actually generate shader code to set uniforms,
    it only adds information to ShadowMapShaders to do it later. }
  ShadowMapShader.ShadowMap := ShadowMap;
  ShadowMapShader.TextureUnit := TextureUnit;
  ShadowMapShaders.Add(ShadowMapShader);

  ShadowMapShader.PrepareHash(FCodeHash);
end;

end.
