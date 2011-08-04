{
  Copyright 2010-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Setting up OpenGL shading (TVRMLShader).

  Internal for VRMLGLRenderer. @exclude }
unit VRMLShader;

interface

uses VectorMath, GLShaders, FGL {$ifdef VER2_2}, FGLObjectList22 {$endif},
  VRMLShadowMaps, VRMLTime, VRMLFields, VRMLNodes, KambiUtils, Boxes3D,
  VRMLGLRendererTextureEnv;

type
  TTextureType = (tt2D, tt2DShadow, ttCubeMap, tt3D, ttShader);

  TTexGenerationComponent = (tgEye, tgObject);
  TTexGenerationComplete = (tgSphere, tgNormal, tgReflection);
  TTexComponent = 0..3;

  TFogType = (ftLinear, ftExp);
  TFogCoordinateSource = (
    { Fog is determined by depth (distance to camera). }
    fcDepth,
    { Fog is determined by explicit coordinate (per-vertex glFogCoord*). }
    fcPassedCoordinate);

  TShaderCodeHash = object
  private
    Sum, XorValue: LongWord;
    procedure AddInteger(const I: Integer);
    procedure AddFloat(const F: Single);
    procedure AddPointer(Ptr: Pointer);
    procedure AddEffects(Nodes: TVRMLNodeList);
  public
    function ToString: string;
    procedure Clear;
  end;

  { GLSL program integrated with VRML/X3D and TVRMLShader.
    Allows to bind uniform values from VRML/X3D fields,
    and to observe VRML/X3D events and automatically update uniform values.
    Also allows to initialize and check program by TVRMLShader.LinkProgram,
    and get a hash of it by TVRMLShader.CodeHash. }
  TVRMLShaderProgram = class(TGLSLProgram)
  private
    { Events where we registered our EventReceive method. }
    EventsObserved: TVRMLEventList;

    { Set uniform variable from VRML/X3D field value.
      Uniform name is contained in UniformName. UniformValue indicates
      uniform type and new value (UniformValue.Name is not used).

      Do not pass here SFNode / MFNode fields (these should be added to
      UniformsTextures).

      @raises(EGLSLUniformInvalid When uniform variable name
        or type are invalid.

        Caller should always catch this and change into OnWarning.

        X3D spec "OpenGL shading language (GLSL) binding" says
        "If the name is not available as a uniform variable in the
        provided shader source, the values of the node shall be ignored"
        (although it says when talking about "Vertex attributes",
        seems they mixed attributes and uniforms meaning in spec?).

        So invalid uniform names should be always catched.
        We also catch type mismatches.) }
    procedure SetUniformFromField(const UniformName: string;
      const UniformValue: TVRMLField; const EnableDisable: boolean);

    procedure EventReceive(Event: TVRMLEvent; Value: TVRMLField;
      const Time: TVRMLTime);

    { Set uniform shader variable from VRML/X3D field (exposed or not).
      We also start observing an exposed field or eventIn,
      and will automatically update uniform value when we receive an event. }
    procedure BindNonTextureUniform(
      const FieldOrEvent: TVRMLInterfaceDeclaration;
      const EnableDisable: boolean);
  protected
    { Nodes that have interface declarations with textures for this shader. }
    UniformsTextures: TVRMLFieldList;
  public
    constructor Create;
    destructor Destroy; override;

    { Set and observe uniform variables from given Node.InterfaceDeclarations.

      Non-texture fields are set immediately.
      Non-texture fields and events are then observed by this shader,
      and automatically updated when changed.

      Texture fields have to be updated by descendant (like TVRMLGLSLProgram),
      using the UniformsTextures list. These methods add fields to this list.
      @groupBegin }
    procedure BindUniforms(const Node: TVRMLNode; const EnableDisable: boolean);
    procedure BindUniforms(const Nodes: TVRMLNodeList; const EnableDisable: boolean);
    { @groupEnd }
  end;

  TVRMLShader = class;

  TShaderSource = class
  private
    FSource: array [TShaderType] of TDynStringArray;
    function GetSource(const AType: TShaderType): TDynStringArray;
  public
    constructor Create;
    destructor Destroy; override;
    property Source [AType: TShaderType]: TDynStringArray read GetSource; default;

    { Append AppendCode to our code.
      Has some special features:

      - Doesn't use AppendCode[stFragment][0]
        (we use this for now only with texture and light shaders,
        which treat AppendCode[stFragment][0] specially).

      - Doesn't add anything to given type, if it's already empty.
        For our internal base shaders, vertex and fragment are never empty.
        When they are empty, this means that user assigned ComposedShader,
        but depends on fixed-function pipeline to do part of the job. }
    procedure Append(AppendCode: TShaderSource);
  end;

  { Internal for TLightShader. @exclude }
  TLightDefine = (
    ldTypePosiional,
    ldTypeSpot,
    ldHasAttenuation,
    ldHasRadius,
    ldHasAmbient,
    ldHasSpecular,
    ldHasBeamWidth,
    ldHasSpotExponent);

  TLightShader = class
  private
    Number: Cardinal;
    Node: TNodeX3DLightNode;
    Light: PLightInstance;
    Shader: TVRMLShader;
    { Code calculated (on demand, when method called) using above vars. }
    FCode: TShaderSource;
    LightUniformName1: string;
    LightUniformValue1: Single;
    LightUniformName2: string;
    LightUniformValue2: Single;
    { Calculated by Prepare. Stored as TLightDefine array,
      since TLightShader.Prepare is executed very often and must be fast.
      Only TLightShader.Code actually changes it to a string. }
    Defines: array [0..9] of TLightDefine;
    DefinesCount: Cardinal;
  public
    destructor Destroy; override;
    { Prepare some stuff for Code generation, update Hash for this light shader. }
    procedure Prepare(var Hash: TShaderCodeHash; const LightNumber: Cardinal);
    function Code: TShaderSource;
  end;

  TLightShaders = class(specialize TFPGObjectList<TLightShader>)
  private
    function Find(const Node: TNodeX3DLightNode; out Shader: TLightShader): boolean;
  end;

  TTextureShader = class
  private
    TextureUnit: Cardinal;
    TextureType: TTextureType;
    Node: TNodeX3DTextureNode;
    Env: TTextureEnv;
    ShadowMapSize: Cardinal;
    ShadowLight: TNodeX3DLightNode;
    ShadowVisualizeDepth: boolean;
    Shader: TVRMLShader;

    { Uniform to set for this texture. May be empty. }
    UniformName: string;
    UniformValue: LongInt;

    { Mix texture colors into fragment color, based on TTextureEnv specification. }
    class function TextureEnvMix(const AEnv: TTextureEnv;
      const FragmentColor, CurrentTexture: string): string;
  public
    { Update Hash for this light shader. }
    procedure Prepare(var Hash: TShaderCodeHash);
    procedure Enable(var TextureApply, TextureColorDeclare,
      TextureCoordInitialize, TextureCoordMatrix, TextureUniformsDeclare: string);
  end;

  TTextureShaders = specialize TFPGObjectList<TTextureShader>;

  TBumpMapping = (bmNone, bmBasic, bmParallax, bmSteepParallax, bmSteepParallaxShadowing);

  { Create appropriate shader and at the same time set OpenGL parameters
    for fixed-function rendering. Once everything is set up,
    you can create TVRMLShaderProgram instance
    and initialize it by LinkProgram here, then enable it if you want.
    Or you can simply allow the fixed-function pipeline to work.

    This is used internally by TVRMLGLRenderer. It isn't supposed to be used
    directly by other code. }
  TVRMLShader = class
  private
    { When adding new field, remember to clear it in Clear method. }
    { List of effect nodes that determine uniforms of our program. }
    UniformsNodes: TVRMLNodeList;
    TextureCoordGen, ClipPlane, FragmentEnd: string;
    FPercentageCloserFiltering: TPercentageCloserFiltering;
    FVarianceShadowMaps: boolean;
    Source: TShaderSource;
    PlugIdentifiers: Cardinal;
    LightShaders: TLightShaders;
    TextureShaders: TTextureShaders;
    FCodeHash: TShaderCodeHash;
    CodeHashFinalized: boolean;
    SelectedNode: TNodeComposedShader;
    WarnMissingPlugs: boolean;
    FShapeRequiresShaders: boolean;
    FBumpMapping: TBumpMapping;
    FNormalMapTextureUnit: Cardinal;
    FHeightMapInAlpha: boolean;
    FHeightMapScale: Single;
    FFogEnabled: boolean;
    FFogType: TFogType;
    FFogCoordinateSource: TFogCoordinateSource;

    { We have to optimize the most often case of TVRMLShader usage,
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
    GroupEffects: TVRMLNodeList;
    Lighting, MaterialFromColor: boolean;

    procedure EnableEffects(Effects: TMFNode;
      const Code: TShaderSource = nil;
      const ForwardDeclareInFinalShader: boolean = false);
    procedure EnableEffects(Effects: TVRMLNodeList;
      const Code: TShaderSource = nil;
      const ForwardDeclareInFinalShader: boolean = false);

    { Special form of Plug. It inserts the PlugValue source code directly
      at the position of given plug comment (no function call
      or anything is added). It also assumes that PlugName occurs only once
      in the Code, for speed.

      Returns if plug code was inserted (always @true when
      InsertAtBeginIfNotFound). }
    function PlugDirectly(Code: TDynStringArray;
      const CodeIndex: Cardinal;
      const PlugName, PlugValue: string;
      const InsertAtBeginIfNotFound: boolean): boolean;

    function DeclareShadowFunctions: string;
  public
    ShapeBoundingBox: TBox3D;
    { Specular material color. Must be set before EnableLight, and be constant
      later. }
    MaterialSpecularColor: TVector3Single;

    constructor Create;
    destructor Destroy; override;

    { Detect defined PLUG_xxx functions within PlugValue,
      insert calls to them into given Code,
      insert forward declarations of their calls into Code too
      (this allows to work with separate compilation units, and also makes
      procedure declaration order less important),
      insert the PlugValue (which should be variable and functions declarations)
      into code of final shader (determined by EffectPartType).

      When Code = nil then we assume code of final shader.
      So we insert a call to plugged_x function, and PlugValue (defining this
      plugged_x function) into the same final shader source
      (determined by EffectPartType).

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
    procedure Plug(const EffectPartType: TShaderType; PlugValue: string;
      CompleteCode: TShaderSource = nil;
      const ForwardDeclareInFinalShader: boolean = false);

    { Add fragment and vertex shader code, link.
      @raises EGLSLError In case of troubles with linking. }
    procedure LinkProgram(AProgram: TVRMLShaderProgram);

    { Calculate the hash of all the current TVRMLShader settings,
      that is the hash of GLSL program code initialized by this shader
      LinkProgram. You should use this only when the GLSL program source
      is completely initialized (all TVRMLShader settings are set).

      It can be used to decide when the shader GLSL program needs
      to be regenerated, shared etc. }
    function CodeHash: TShaderCodeHash;

    procedure EnableTexture(const TextureUnit: Cardinal;
      const TextureType: TTextureType; const Node: TNodeX3DTextureNode;
      const Env: TTextureEnv;
      const ShadowMapSize: Cardinal = 0;
      const ShadowLight: TNodeX3DLightNode = nil;
      const ShadowVisualizeDepth: boolean = false);
    procedure EnableTexGen(const TextureUnit: Cardinal;
      const Generation: TTexGenerationComponent; const Component: TTexComponent);
    procedure EnableTexGen(const TextureUnit: Cardinal;
      const Generation: TTexGenerationComplete);
    procedure DisableTexGen(const TextureUnit: Cardinal);
    procedure EnableClipPlane(const ClipPlaneIndex: Cardinal);
    procedure DisableClipPlane(const ClipPlaneIndex: Cardinal);
    procedure EnableAlphaTest;
    procedure EnableBumpMapping(const BumpMapping: TBumpMapping;
      const NormalMapTextureUnit: Cardinal;
      const HeightMapInAlpha: boolean; const HeightMapScale: Single);
    { Enable light source.
      Remember to set MaterialSpecularColor before calling this. }
    procedure EnableLight(const Number: Cardinal; Light: PLightInstance);
    { It is Ok to enable this more than once, last EnableFog determines
      the fog settings. TFogCoordinateRenderer.RenderCoordinateBegin for
      direct fog coordinate relies on this. }
    procedure EnableFog(const FogType: TFogType;
      const FogCoordinateSource: TFogCoordinateSource);
    function EnableCustomShaderCode(Shaders: TMFNodeShaders;
      out Node: TNodeComposedShader): boolean;
    procedure EnableAppearanceEffects(Effects: TMFNode);
    procedure EnableGroupEffects(Effects: TVRMLNodeList);
    procedure EnableLighting;
    procedure EnableMaterialFromColor;

    property PercentageCloserFiltering: TPercentageCloserFiltering
      read FPercentageCloserFiltering write FPercentageCloserFiltering;

    property VarianceShadowMaps: boolean read FVarianceShadowMaps
      write FVarianceShadowMaps;

    property ShapeRequiresShaders: boolean read FShapeRequiresShaders
      write FShapeRequiresShaders;

    { Clear instance, bringing it to the state after creation. }
    procedure Clear;
  end;

operator = (const A, B: TShaderCodeHash): boolean;

implementation

uses SysUtils, GL, GLExt, KambiStringUtils, KambiGLUtils, KambiWarnings,
  KambiLog, StrUtils, Base3D, GLVersionUnit;

{ TODO: a way to turn off using fixed-function pipeline completely
  will be needed some day. Currently, some functions here call
  fixed-function glEnable... stuff.

  TODO: some day, avoid using predefined OpenGL state variables.
  Use only shader uniforms. Right now, we allow some state to be assigned
  using direct normal OpenGL fixed-function functions in VRMLGLRenderer,
  and our shaders just use it.
}

{ String helpers ------------------------------------------------------------- }

function MoveToOpeningParen(const S: string; var P: Integer): boolean;
begin
  Result := true;
  repeat
    Inc(P);

    if P > Length(S) then
    begin
      OnWarning(wtMinor, 'VRML/X3D', 'PLUG declaration unexpected end (no opening parenthesis "(")');
      Exit(false);
    end;

    if (S[P] <> '(') and
       not (S[P] in WhiteSpaces) then
    begin
      OnWarning(wtMinor, 'VRML/X3D', Format('PLUG declaration unexpected character "%s" (expected opening parenthesis "(")',
        [S[P]]));
      Exit(false);
    end;
  until S[P] = '(';
 end;

function MoveToMatchingParen(const S: string; var P: Integer): boolean;
var
  ParenLevel: Cardinal;
begin
  Result := true;
  ParenLevel := 1;

  repeat
    Inc(P);
    if P > Length(S) then
    begin
      OnWarning(wtMinor, 'VRML/X3D', 'PLUG declaration unexpected end (no closing parenthesis ")")');
      Exit(false);
    end;

    if S[P] = '(' then
      Inc(ParenLevel) else
    if S[P] = ')' then
      Dec(ParenLevel);
  until ParenLevel = 0;
end;

{ TShaderCodeHash ------------------------------------------------------------ }

{$include norqcheckbegin.inc}

(* Smart, but not used:
procedure TShaderCodeHash.AddString(const S: string);
var
  PS: PLongWord;
  Last: LongWord;
  I: Integer;
begin
  PS := PLongWord(S);

  for I := 1 to Length(S) div 4 do
  begin
    Sum += PS^;
    XorValue := XorValue xor PS^;
    Inc(PS);
  end;

  if Length(S) mod 4 = 0 then
  begin
    Last := 0;
    Move(S[(Length(S) div 4) * 4 + 1], Last, Length(S) mod 4);
    Sum += Last;
    XorValue := XorValue xor Last;
  end;
end;
*)

procedure TShaderCodeHash.AddPointer(Ptr: Pointer);
begin
  { This will cut the pointer on non-32bit processors.
    But that's not a problem --- we just want it for hash,
    taking the least significant 32 bits from pointer is OK for this. }
  Sum += LongWord(PtrUInt(Ptr));
  XorValue := XorValue xor LongWord(PtrUInt(Ptr));
end;

procedure TShaderCodeHash.AddInteger(const I: Integer);
begin
  Sum += I;
end;

procedure TShaderCodeHash.AddFloat(const F: Single);
begin
  Sum += Round(F * 100000);
end;

{$include norqcheckend.inc}

procedure TShaderCodeHash.AddEffects(Nodes: TVRMLNodeList);
var
  I: Integer;
begin
  { We add to hash actual Effect node references (pointers), this way ensuring
    that to share the same shader, effect nodes must be the same.
    Merely equal GLSL source code is not enough (because effects with equal
    source code may still have different uniform values, and sharing them
    would not be handled correctly here --- we set uniform values on change,
    not every time before rendering shape). }
  for I := 0 to Nodes.Count - 1 do
    if (Nodes[I] is TNodeEffect) and
       TNodeEffect(Nodes[I]).FdEnabled.Value then
      AddPointer(Nodes[I]);
end;

function TShaderCodeHash.ToString: string;
begin
  Result := IntToStr(Sum) + '/' + IntToStr(XorValue);
end;

procedure TShaderCodeHash.Clear;
begin
  Sum := 0;
  XorValue := 0;
end;

operator = (const A, B: TShaderCodeHash): boolean;
begin
  Result := (A.Sum = B.Sum) and (A.XorValue = B.XorValue);
end;

{ TShaderSource -------------------------------------------------------------- }

constructor TShaderSource.Create;
var
  SourceType: TShaderType;
begin
  inherited;
  for SourceType := Low(SourceType) to High(SourceType) do
    FSource[SourceType] := TDynStringArray.Create;
end;

destructor TShaderSource.Destroy;
var
  SourceType: TShaderType;
begin
  for SourceType := Low(SourceType) to High(SourceType) do
    FreeAndNil(FSource[SourceType]);
  inherited;
end;

function TShaderSource.GetSource(const AType: TShaderType): TDynStringArray;
begin
  Result := FSource[AType];
end;

procedure TShaderSource.Append(AppendCode: TShaderSource);
var
  T: TShaderType;
  I: Integer;
begin
  for T := Low(T) to High(T) do
    if Source[T].Count <> 0 then
      for I := Iff(T = stFragment, 1, 0) to AppendCode[T].Count - 1 do
        Source[T].Add(AppendCode[T][I]);
end;

{ TLightShader --------------------------------------------------------------- }

destructor TLightShader.Destroy;
begin
  FreeAndNil(FCode);
  inherited;
end;

const
  LightDefines: array [TLightDefine] of record
    Name: string;
    Hash: LongWord;
  end =
  ( (Name: 'LIGHT_TYPE_POSITIONAL'  ; Hash: 107; ),
    (Name: 'LIGHT_TYPE_SPOT'        ; Hash: 109; ),
    (Name: 'LIGHT_HAS_ATTENUATION'  ; Hash: 113; ),
    (Name: 'LIGHT_HAS_RADIUS'       ; Hash: 127; ),
    (Name: 'LIGHT_HAS_AMBIENT'      ; Hash: 131; ),
    (Name: 'LIGHT_HAS_SPECULAR'     ; Hash: 137; ),
    (Name: 'LIGHT_HAS_BEAM_WIDTH'   ; Hash: 139; ),
    (Name: 'LIGHT_HAS_SPOT_EXPONENT'; Hash: 149; )
  );

procedure TLightShader.Prepare(var Hash: TShaderCodeHash; const LightNumber: Cardinal);

  procedure Define(const D: TLightDefine);
  begin
    Assert(DefinesCount <= High(Defines), 'Too many light #defines, increase High(TLightShader.Defines)');
    Defines[DefinesCount] := D;
    Inc(DefinesCount);
    Hash.AddInteger(LightDefines[D].Hash * (LightNumber + 1));
  end;

begin
  DefinesCount := 0;
  Hash.AddInteger(101);

  if Node is TVRMLPositionalLightNode then
  begin
    Define(ldTypePosiional);
    if Node is TNodeSpotLight_1 then
    begin
      Define(ldTypeSpot);
      if TNodeSpotLight_1(Node).SpotExp <> 0 then
        Define(ldHasSpotExponent);
    end else
    if Node is TNodeSpotLight then
    begin
      Define(ldTypeSpot);
      if TNodeSpotLight(Node).FdBeamWidth.Value <
         TNodeSpotLight(Node).FdCutOffAngle.Value then
      begin
        Define(ldHasBeamWidth);
        LightUniformName1 := 'kambi_light_%d_beam_width';
        LightUniformValue1 := TNodeSpotLight(Node).FdBeamWidth.Value;
        Hash.AddFloat(LightUniformValue1);
      end;
    end;

    if TVRMLPositionalLightNode(Node).HasAttenuation then
      Define(ldHasAttenuation);

    if TVRMLPositionalLightNode(Node).HasRadius and
      { Do not activate per-pixel checking of light radius,
        if we know (by bounding box test below)
        that the whole shape is completely within radius. }
      (Box3DPointMaxDistance(Shader.ShapeBoundingBox,
        Light^.Location, -1) > Light^.Radius) then
    begin
      Define(ldHasRadius);
      LightUniformName2 := 'kambi_light_%d_radius';
      LightUniformValue2 := Light^.Radius;
      { Uniform value comes from this Node's property,
        so this cannot be shared with other light nodes,
        that may have not synchronized radius value.

        (Note: We could instead add radius value to the hash.
        Then this shader could be shared between all light nodes with
        the same radius value --- however, if radius changed,
        then the shader would have to be recreated, even if the same
        light node was used.) }
      Hash.AddPointer(Node);
    end;
  end;
  if Node.FdAmbientIntensity.Value <> 0 then
    Define(ldHasAmbient);
  if not PerfectlyZeroVector(Shader.MaterialSpecularColor) then
    Define(ldHasSpecular);
end;

function TLightShader.Code: TShaderSource;

  { Convert Defines list into a string of GLSL code. }
  function DefinesStr: string;
  var
    I: Integer;
  begin
    Result := '';
    for I := 0 to DefinesCount - 1 do
      Result += '#define ' + LightDefines[Defines[I]].Name + NL;
  end;

begin
  if FCode = nil then
  begin
    FCode := TShaderSource.Create;

    FCode[stFragment].Add(
      DefinesStr + StringReplace({$I template_add_light.glsl.inc},
      'light_number', IntToStr(Number), [rfReplaceAll]));

    if Node <> nil then
      Shader.EnableEffects(Node.FdEffects, FCode);
  end;

  Result := FCode;
end;

{ TLightShaders -------------------------------------------------------------- }

function TLightShaders.Find(const Node: TNodeX3DLightNode; out Shader: TLightShader): boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Node = Node then
    begin
      Shader := Items[I];
      Exit(true);
    end;
  Shader := nil;
  Result := false;
end;

{ TVRMLShaderProgram ------------------------------------------------------- }

constructor TVRMLShaderProgram.Create;
begin
  inherited;
  EventsObserved := TVRMLEventList.Create(false);
  UniformsTextures := TVRMLFieldList.Create(false);
end;

destructor TVRMLShaderProgram.Destroy;
var
  I: Integer;
begin
  if EventsObserved <> nil then
  begin
    for I := 0 to EventsObserved.Count - 1 do
      EventsObserved[I].RemoveHandler(@EventReceive);
    FreeAndNil(EventsObserved);
  end;
  FreeAndNil(UniformsTextures);
  inherited;
end;

procedure TVRMLShaderProgram.BindNonTextureUniform(
  const FieldOrEvent: TVRMLInterfaceDeclaration;
  const EnableDisable: boolean);
var
  UniformField: TVRMLField;
  UniformEvent, ObservedEvent: TVRMLEvent;
begin
  UniformField := FieldOrEvent.Field;
  UniformEvent := FieldOrEvent.Event;

  { Set initial value for this GLSL uniform variable,
    from VRML field or exposedField }

  if UniformField <> nil then
  try
    { Ok, we have a field with a value (interface declarations with
      fields inside ComposedShader / Effect always have a value).
      So set GLSL uniform variable from this field. }
    SetUniformFromField(UniformField.Name, UniformField, EnableDisable);
  except
    { We capture EGLSLUniformInvalid, converting it to OnWarning and exit.
      This way we will not add this field to EventsObserved. }
    on E: EGLSLUniformInvalid do
    begin
      OnWarning(wtMinor, 'VRML/X3D', E.Message);
      Exit;
    end;
  end;

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
    ObservedEvent.OnReceive.Add(@EventReceive);
    EventsObserved.Add(ObservedEvent);
  end;
end;

procedure TVRMLShaderProgram.SetUniformFromField(
  const UniformName: string; const UniformValue: TVRMLField;
  const EnableDisable: boolean);
var
  TempF: TDynSingleArray;
  TempVec2f: TDynVector2SingleArray;
  TempVec3f: TDynVector3SingleArray;
  TempVec4f: TDynVector4SingleArray;
  TempMat3f: TDynMatrix3SingleArray;
  TempMat4f: TDynMatrix4SingleArray;
begin
  { program must be active to set uniform values. }
  if EnableDisable then
    Enable;

  if UniformValue is TSFBool then
    SetUniform(UniformName, TSFBool(UniformValue).Value, true) else
  if UniformValue is TSFLong then
    { Handling of SFLong also takes care of SFInt32. }
    SetUniform(UniformName, TSFLong(UniformValue).Value, true) else
  if UniformValue is TSFVec2f then
    SetUniform(UniformName, TSFVec2f(UniformValue).Value, true) else
  { Check TSFColor first, otherwise TSFVec3f would also catch and handle
    TSFColor. And we don't want this: for GLSL, color is passed
    as vec4 (so says the spec, I guess that the reason is that for GLSL most
    input/output colors are vec4). }
  if UniformValue is TSFColor then
    SetUniform(UniformName, Vector4Single(TSFColor(UniformValue).Value, 1.0), true) else
  if UniformValue is TSFVec3f then
    SetUniform(UniformName, TSFVec3f(UniformValue).Value, true) else
  if UniformValue is TSFVec4f then
    SetUniform(UniformName, TSFVec4f(UniformValue).Value, true) else
  if UniformValue is TSFRotation then
    SetUniform(UniformName, TSFRotation(UniformValue).Value, true) else
  if UniformValue is TSFMatrix3f then
    SetUniform(UniformName, TSFMatrix3f(UniformValue).Value, true) else
  if UniformValue is TSFMatrix4f then
    SetUniform(UniformName, TSFMatrix4f(UniformValue).Value, true) else
  if UniformValue is TSFFloat then
    SetUniform(UniformName, TSFFloat(UniformValue).Value, true) else
  if UniformValue is TSFDouble then
    { SFDouble also takes care of SFTime }
    SetUniform(UniformName, TSFDouble(UniformValue).Value, true) else

  { Double-precision vector and matrix types.

    Note that X3D spec specifies only mapping for SF/MFVec3d, 4d
    (not specifying any mapping for SF/MFVec2d, and all matrix types).
    And it specifies that they map to types float3, float4 ---
    which are not valid types in GLSL?

    So I simply ignore non-sensible specification, and take
    the reasonable approach: support all double-precision vectors and matrices,
    just like single-precision. }
  if UniformValue is TSFVec2d then
    SetUniform(UniformName, Vector2Single(TSFVec2d(UniformValue).Value), true) else
  if UniformValue is TSFVec3d then
    SetUniform(UniformName, Vector3Single(TSFVec3d(UniformValue).Value), true) else
  if UniformValue is TSFVec4d then
    SetUniform(UniformName, Vector4Single(TSFVec4d(UniformValue).Value), true) else
  if UniformValue is TSFMatrix3d then
    SetUniform(UniformName, Matrix3Single(TSFMatrix3d(UniformValue).Value), true) else
  if UniformValue is TSFMatrix4d then
    SetUniform(UniformName, Matrix4Single(TSFMatrix4d(UniformValue).Value), true) else

  { Now repeat this for array types }
  if UniformValue is TMFBool then
    SetUniform(UniformName, TMFBool(UniformValue).Items, true) else
  if UniformValue is TMFLong then
    SetUniform(UniformName, TMFLong(UniformValue).Items, true) else
  if UniformValue is TMFVec2f then
    SetUniform(UniformName, TMFVec2f(UniformValue).Items, true) else
  if UniformValue is TMFColor then
  begin
    TempVec4f := TMFColor(UniformValue).Items.ToVector4Single(1.0);
    try
      SetUniform(UniformName, TempVec4f, true);
    finally FreeAndNil(TempVec4f) end;
  end else
  if UniformValue is TMFVec3f then
    SetUniform(UniformName, TMFVec3f(UniformValue).Items, true) else
  if UniformValue is TMFVec4f then
    SetUniform(UniformName, TMFVec4f(UniformValue).Items, true) else
  if UniformValue is TMFRotation then
    SetUniform(UniformName, TMFRotation(UniformValue).Items, true) else
  if UniformValue is TMFMatrix3f then
    SetUniform(UniformName, TMFMatrix3f(UniformValue).Items, true) else
  if UniformValue is TMFMatrix4f then
    SetUniform(UniformName, TMFMatrix4f(UniformValue).Items, true) else
  if UniformValue is TMFFloat then
    SetUniform(UniformName, TMFFloat(UniformValue).Items, true) else
  if UniformValue is TMFDouble then
  begin
    TempF := TMFDouble(UniformValue).Items.ToSingle;
    try
      SetUniform(UniformName, TempF, true);
    finally FreeAndNil(TempF) end;
  end else
  if UniformValue is TMFVec2d then
  begin
    TempVec2f := TMFVec2d(UniformValue).Items.ToVector2Single;
    try
      SetUniform(UniformName, TempVec2f, true);
    finally FreeAndNil(TempVec2f) end;
  end else
  if UniformValue is TMFVec3d then
  begin
    TempVec3f := TMFVec3d(UniformValue).Items.ToVector3Single;
    try
      SetUniform(UniformName, TempVec3f, true);
    finally FreeAndNil(TempVec3f) end;
  end else
  if UniformValue is TMFVec4d then
  begin
    TempVec4f := TMFVec4d(UniformValue).Items.ToVector4Single;
    try
      SetUniform(UniformName, TempVec4f, true);
    finally FreeAndNil(TempVec4f) end;
  end else
  if UniformValue is TMFMatrix3d then
  begin
    TempMat3f := TMFMatrix3d(UniformValue).Items.ToMatrix3Single;
    try
      SetUniform(UniformName, TempMat3f, true);
    finally FreeAndNil(TempMat3f) end;
  end else
  if UniformValue is TMFMatrix4d then
  begin
    TempMat4f := TMFMatrix4d(UniformValue).Items.ToMatrix4Single;
    try
      SetUniform(UniformName, TempMat4f, true);
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
    OnWarning(wtMajor, 'VRML/X3D', 'Setting uniform GLSL variable from X3D field type "' + UniformValue.VRMLTypeName + '" not supported');

  if EnableDisable then
    { TODO: this should restore previously bound program }
    Disable;
end;

procedure TVRMLShaderProgram.EventReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
var
  UniformName: string;
  Scene: TVRMLEventsEngine;
begin
  if Event.ParentExposedField = nil then
    UniformName := Event.Name else
    UniformName := Event.ParentExposedField.Name;

  try
    SetUniformFromField(UniformName, Value, true);
  except
    { We capture EGLSLUniformInvalid, converting it to OnWarning.
      This way we remove this event from OnReceive list. }
    on E: EGLSLUniformInvalid do
    begin
      OnWarning(wtMinor, 'VRML/X3D', E.Message);
      Event.RemoveHandler(@EventReceive);
      EventsObserved.Remove(Event);
      Exit;
    end;
  end;

  { Although ExposedEvents implementation already sends notification
    about changes to Scene, we can also get here
    by eventIn invocation (which doesn't trigger
    Scene.ChangedField, since it doesn't change a field...).
    So we should explicitly do VisibleChangeHere here, to make sure
    it gets called when uniform changed. }
  if Event.ParentNode <> nil then
  begin
    Scene := (Event.ParentNode as TVRMLNode).Scene;
    if Scene <> nil then
      Scene.VisibleChangeHere([vcVisibleGeometry, vcVisibleNonGeometry]);
  end;
end;

procedure TVRMLShaderProgram.BindUniforms(const Node: TVRMLNode;
  const EnableDisable: boolean);
var
  I: Integer;
  IDecl: TVRMLInterfaceDeclaration;
begin
  Assert(Node.HasInterfaceDeclarations <> []);
  Assert(Node.InterfaceDeclarations <> nil);
  for I := 0 to Node.InterfaceDeclarations.Count - 1 do
  begin
    IDecl := Node.InterfaceDeclarations[I];
    if (IDecl.Field <> nil) and
       ((IDecl.Field is TSFNode) or
        (IDecl.Field is TMFNode)) then
      UniformsTextures.Add(IDecl.Field) else
      BindNonTextureUniform(IDecl, EnableDisable);
  end;
end;

procedure TVRMLShaderProgram.BindUniforms(const Nodes: TVRMLNodeList;
  const EnableDisable: boolean);
var
  I: Integer;
begin
  for I := 0 to Nodes.Count - 1 do
    BindUniforms(Nodes[I], EnableDisable);
end;

{ TTextureShader ------------------------------------------------------------- }

procedure TTextureShader.Prepare(var Hash: TShaderCodeHash);
var
  IntHash: LongWord;
begin
{$include norqcheckbegin.inc}
  IntHash :=
    1 +
    181 * Ord(TextureType) +
    191 * ShadowMapSize +
    193 * Ord(ShadowVisualizeDepth) +
    Env.Hash;
  if ShadowLight <> nil then
    IntHash += PtrUInt(ShadowLight);
  Hash.AddInteger(179 * (TextureUnit + 1) * IntHash);
  { Don't directly add Node to the Hash, it would prevent a lot of sharing.
    Node is only used to get effects. }
  Hash.AddEffects(Node.FdEffects.Items);
{$include norqcheckend.inc}
end;

class function TTextureShader.TextureEnvMix(const AEnv: TTextureEnv;
  const FragmentColor, CurrentTexture: string): string;
begin
  if AEnv.Disabled then Exit('');

  case AEnv.Combine[cRGB] of
    GL_REPLACE : Result := FragmentColor + ' = '  + CurrentTexture + ';';
    GL_ADD     : Result := FragmentColor + ' += ' + CurrentTexture + ';';
    GL_SUBTRACT: Result := FragmentColor + ' = '  + CurrentTexture + ' - ' + FragmentColor + ';';
    else { assume GL_MODULATE }
                 Result := FragmentColor + ' *= ' + CurrentTexture + ';';
  end;

  { TODO: this handles only a subset of possible values:
    - different combine values on RGB/alpha not handled yet.
      We just check Env.Combine[cRGB], and assume it's equal Env.Combine[cAlpha].
    - Scale is ignored (assumed 1)
    - CurrentTextureArgument, SourceArgument ignored (assumed ta0, ta1)
    - Source ignored (assumed csPreviousTexture, which is in FragmentColor)
    - many Combine values ignored (treated like modulate),
      and so also NeedsConstantColor and InterpolateAlphaSource are ignored.
    - moreover, shader pipeline has a chance to implement more parameters
      of multi-texturing. In fact, complete support for X3D MultiTexture
      should be doable, including stuff too difficult / not possible
      in fixed-function, like MultiTexture.function.
  }
end;

procedure TTextureShader.Enable(var TextureApply, TextureColorDeclare,
  TextureCoordInitialize, TextureCoordMatrix, TextureUniformsDeclare: string);
const
  SamplerFromTextureType: array [TTextureType] of string =
  ('sampler2D', 'sampler2DShadow', 'samplerCube', 'sampler3D', '');
var
  TextureSampleCall, TexCoordName: string;
  ShadowLightShader: TLightShader;
  Code: TShaderSource;
  SamplerType: string;
begin
  if TextureType <> ttShader then
  begin
    UniformName := Format('kambi_texture_%d', [TextureUnit]);
    UniformValue := TextureUnit;
  end else
    UniformName := '';

  TexCoordName := Format('gl_TexCoord[%d]', [TextureUnit]);

  TextureCoordInitialize += Format('%s = gl_MultiTexCoord%d;' + NL,
    [TexCoordName, TextureUnit]);
  if not (GLVersion.BuggyShaderShadowMap and (TextureType = tt2DShadow)) then
    TextureCoordMatrix += Format('%s = gl_TextureMatrix[%d] * %0:s;' + NL,
      [TexCoordName, TextureUnit]);

  if (TextureType = tt2DShadow) and
      ShadowVisualizeDepth then
  begin
    { visualizing depth map requires a little different approach:
      - we use shadow_depth() instead of shadow() function
      - we *set* gl_FragColor, not modulate it, to ignore previous textures
      - we call "return" after, to ignore following textures
      - the sampler is sampler2D, not sampler2DShadow
      - also, we use gl_FragColor (while we should use fragment_color otherwise),
        because we don't care about previous texture operations and
        we want to return immediately. }
    TextureSampleCall := 'vec4(vec3(shadow_depth(%s, %s)), gl_FragColor.a)';
    TextureApply += Format('gl_FragColor = ' + TextureSampleCall + ';' + NL +
      'return;',
      [UniformName, TexCoordName]);
    TextureUniformsDeclare += Format('uniform sampler2D %s;' + NL,
      [UniformName]);
  end else
  begin
    SamplerType := SamplerFromTextureType[TextureType];
    { For variance shadow maps, use normal sampler2D, not sampler2DShadow }
    if Shader.VarianceShadowMaps and (TextureType = tt2DShadow) then
      SamplerType := 'sampler2D';

    if (TextureType = tt2DShadow) and
       (ShadowLight <> nil) and
       Shader.LightShaders.Find(ShadowLight, ShadowLightShader) then
    begin
      Shader.Plug(stFragment, Format(
        'uniform %s %s;' +NL+
        '%s' +NL+
        'void PLUG_light_scale(inout float scale, const in vec3 normal_eye, const in vec3 light_dir, const in gl_LightSourceParameters light_source, const in gl_LightProducts light_products, const in gl_MaterialParameters material)' +NL+
        '{' +NL+
        '  scale *= shadow(%s, gl_TexCoord[%d], %d.0);' +NL+
        '}',
        [SamplerType, UniformName,
         Shader.DeclareShadowFunctions,
         UniformName, TextureUnit, ShadowMapSize]),
        ShadowLightShader.Code);
    end else
    begin
      if TextureColorDeclare = '' then
        TextureColorDeclare := 'vec4 texture_color;' + NL;
      case TextureType of
        tt2D:
          { texture2DProj reasoning:
            Most of the time, 'texture2D(%s, %s.st)' would be enough.
            But we may get 4D tex coords (that is, with last component <> 1)
            - through TextureCoordinate4D
            - through projected texture mapping, when using perspective light
              (spot light) or perspective viewpoint.

            TextureUnit = 0 check reasoning:
            Even when HAS_TEXTURE_COORD_SHIFT is defined (PLUG_texture_coord_shift
            was used), use it only for 0th texture unit. Parallax bump mapping
            calculates the shift, assuming that transformations to tangent space
            follow 0th texture coordinates. Also, for parallax bump mapping,
            we have to assume the 0th texture has simple 2D coords (not 4D). }
          if TextureUnit = 0 then
            TextureSampleCall := NL+
              '#ifdef HAS_TEXTURE_COORD_SHIFT' +NL+
              '  texture2D(%0:s, texture_coord_shifted(%1:s.st))' +NL+
              '#else' +NL+
              '  texture2DProj(%0:s, %1:s)' +NL+
              '#endif' + NL else
            TextureSampleCall := 'texture2DProj(%0:s, %1:s)';
        tt2DShadow: TextureSampleCall := 'vec4(vec3(shadow(%s, %s, ' +IntToStr(ShadowMapSize) + '.0)), fragment_color.a)';
        ttCubeMap : TextureSampleCall := 'textureCube(%s, %s.xyz)';
        { For 3D textures, remember we may get 4D tex coords
          through TextureCoordinate4D, so we have to use texture3DProj }
        tt3D      : TextureSampleCall := 'texture3DProj(%s, %s)';
        ttShader  : TextureSampleCall := 'vec4(1.0, 0.0, 1.0, 1.0)';
        else raise EInternalError.Create('TVRMLShader.EnableTexture:TextureType?');
      end;

      Code := TShaderSource.Create;
      try
        if TextureType <> ttShader then
          Code[stFragment].Add(Format(
            'texture_color = ' + TextureSampleCall + ';' +NL+
            '/* PLUG: texture_color (texture_color, %0:s, %1:s) */' +NL,
            [UniformName, TexCoordName])) else
          Code[stFragment].Add(Format(
            'texture_color = ' + TextureSampleCall + ';' +NL+
            '/* PLUG: texture_color (texture_color, %0:s) */' +NL,
            [TexCoordName]));

        Shader.EnableEffects(Node.FdEffects, Code, true);

        { Add generated Code to Shader.Source. Code[stFragment][0] for texture
          is a little special, we add it to TextureApply that
          will be directly placed within the source. }
        TextureApply += Code[stFragment][0];
        Shader.Source.Append(Code);
      finally FreeAndNil(Code) end;

      TextureApply += TextureEnvMix(Env, 'fragment_color', 'texture_color') + NL;

      if TextureType <> ttShader then
        TextureUniformsDeclare += Format('uniform %s %s;' + NL,
          [SamplerType, UniformName]);
    end;
  end;
end;

{ TVRMLShader ---------------------------------------------------------------- }

function InsertIntoString(const Base: string; const P: Integer; const S: string): string;
begin
  Result := Copy(Base, 1, P - 1) + S + SEnding(Base, P);
end;

const
  DefaultVertexShader = {$I template.vs.inc};
  DefaultFragmentShader = {$I template.fs.inc};

constructor TVRMLShader.Create;
begin
  inherited;

  Source := TShaderSource.Create;
  Source[stVertex].Add(DefaultVertexShader);
  Source[stFragment].Add(DefaultFragmentShader);

  LightShaders := TLightShaders.Create;
  TextureShaders := TTextureShaders.Create;
  UniformsNodes := TVRMLNodeList.Create(false);

  WarnMissingPlugs := true;
end;

destructor TVRMLShader.Destroy;
begin
  FreeAndNil(UniformsNodes);
  FreeAndNil(LightShaders);
  FreeAndNil(TextureShaders);
  FreeAndNil(Source);
  inherited;
end;

procedure TVRMLShader.Clear;
begin
  Source[stVertex].Count := 1;
  Source[stVertex][0] := DefaultVertexShader;
  Source[stFragment].Count := 1;
  Source[stFragment][0] := DefaultFragmentShader;
  { TODO: Source[stGeometry].Count := 0; }
  WarnMissingPlugs := true;

  { the rest of fields just restored to default clear state }
  UniformsNodes.Clear;
  TextureCoordGen := '';
  ClipPlane := '';
  FragmentEnd := '';
  FPercentageCloserFiltering := Low(TPercentageCloserFiltering);
  FVarianceShadowMaps := false;
  PlugIdentifiers := 0;
  LightShaders.Count := 0;
  TextureShaders.Count := 0;
  FCodeHash.Clear;
  CodeHashFinalized := false;
  SelectedNode := nil;
  FShapeRequiresShaders := false;
  FBumpMapping := Low(TBumpMapping);
  FNormalMapTextureUnit := 0;
  FHeightMapInAlpha := false;
  FHeightMapScale := 0;
  FFogEnabled := false;
  { No need to reset, will be set when FFogEnabled := true
  FFogType := Low(TFogType);
  FFogCoordinateSource := Low(TFogCoordinateSource); }
  AppearanceEffects := nil;
  GroupEffects := nil;
  Lighting := false;
  MaterialFromColor := false;
  ShapeBoundingBox := EmptyBox3D;
  MaterialSpecularColor := ZeroVector3Single;
end;

procedure TVRMLShader.Plug(const EffectPartType: TShaderType; PlugValue: string;
  CompleteCode: TShaderSource; const ForwardDeclareInFinalShader: boolean);
const
  PlugPrefix = 'PLUG_';

  { Find PLUG_xxx function inside PlugValue.
    Returns xxx (the part after PLUG_),
    and DeclaredParameters (or this plug function). Or '' if not found. }
  function FindPlugName(const PlugValue: string;
    out DeclaredParameters: string): string;
  const
    IdentifierChars = ['0'..'9', 'a'..'z', 'A'..'Z', '_'];
  var
    P, PBegin, DPBegin, DPEnd: Integer;
  begin
    Result := ''; { assume failure }
    P := Pos(PlugPrefix, PlugValue);
    if P <> 0 then
    begin
      { There must be whitespace before PLUG_ }
      if (P > 1) and (not (PlugValue[P - 1] in WhiteSpaces)) then Exit;
      P += Length(PlugPrefix);
      PBegin := P;
      { There must be at least one identifier char after PLUG_ }
      if (P > Length(PlugValue)) or
         (not (PlugValue[P] in IdentifierChars)) then Exit;
      repeat
        Inc(P);
      until (P > Length(PlugValue)) or (not (PlugValue[P] in IdentifierChars));
      { There must be a whitespace or ( after PLUG_xxx }
      if (P > Length(PlugValue)) or (not (PlugValue[P] in (WhiteSpaces + ['(']))) then
        Exit;

      Result := CopyPos(PlugValue, PBegin, P - 1);

      DPBegin := P - 1;
      if not MoveToOpeningParen(PlugValue, DPBegin) then Exit('');
      DPEnd := DPBegin;
      if not MoveToMatchingParen(PlugValue, DPEnd) then Exit('');

      DeclaredParameters := CopyPos(PlugValue, DPBegin, DPEnd);
    end;
  end;

var
  Code: TDynStringArray;

  procedure InsertIntoCode(const CodeIndex, P: Integer; const S: string);
  begin
    Code[CodeIndex] := InsertIntoString(Code[CodeIndex], P, S);
  end;

  function FindPlugOccurence(const CommentBegin, Code: string;
    const CodeSearchBegin: Integer; out PBegin, PEnd: Integer): boolean;
  begin
    Result := false;
    PBegin := PosEx(CommentBegin, Code, CodeSearchBegin);
    if PBegin <> 0 then
    begin
      PEnd := PosEx('*/', Code, PBegin + Length(CommentBegin));
      Result :=  PEnd <> 0;
      if not Result then
        OnWarning(wtMinor, 'VRML/X3D', Format('Plug comment "%s" not properly closed, treating like not declared',
          [CommentBegin]));
    end;
  end;

var
  PBegin, PEnd, CodeSearchBegin, CodeIndex: Integer;
  Parameter, PlugName, ProcedureName, CommentBegin, PlugDeclaredParameters,
    PlugForwardDeclaration: string;
  CompleteCodeForPlugValue: TShaderSource;
  CodeForPlugValue: TDynStringArray;
  AnyOccurences, AnyOccurencesInThisCodeIndex: boolean;
begin
  CompleteCodeForPlugValue := Source;

  if CompleteCode = nil then
    CompleteCode := CompleteCodeForPlugValue;

  Code := CompleteCode[EffectPartType];
  CodeForPlugValue := CompleteCodeForPlugValue[EffectPartType];

  { if the final shader code is empty (on this type) then don't insert anything
    (avoid creating shader without main()). }
  if CodeForPlugValue.Count = 0 then
    Exit;

  repeat
    PlugName := FindPlugName(PlugValue, PlugDeclaredParameters);
    if PlugName = '' then Break;

    { When using some special plugs, we need to do define some symbols. }
    if (PlugName = 'vertex_object_space_change') and
       (Code = Source[stVertex]) then
      PlugDirectly(Source[stVertex], 0, '/* PLUG-DECLARATIONS */',
        '#define VERTEX_OBJECT_SPACE_CHANGED', false) else
    if (PlugName = 'texture_coord_shift') and
       (Code = Source[stFragment]) then
      PlugDirectly(Source[stFragment], 0, '/* PLUG-DECLARATIONS */',
        '#define HAS_TEXTURE_COORD_SHIFT', false);

    CommentBegin := '/* PLUG: ' + PlugName + ' ';

    ProcedureName := 'plugged_' + IntToStr(PlugIdentifiers);
    StringReplaceAllTo1st(PlugValue, 'PLUG_' + PlugName, ProcedureName, false);
    Inc(PlugIdentifiers);

    PlugForwardDeclaration := 'void ' + ProcedureName + PlugDeclaredParameters + ';' + NL;

    AnyOccurences := false;
    for CodeIndex := 0 to Code.Count - 1 do
    begin
      CodeSearchBegin := 1;
      AnyOccurencesInThisCodeIndex := false;
      while FindPlugOccurence(CommentBegin, Code[CodeIndex], CodeSearchBegin, PBegin, PEnd) do
      begin
        Parameter := Trim(CopyPos(Code[CodeIndex], PBegin + Length(CommentBegin), PEnd - 1));
        InsertIntoCode(CodeIndex, PBegin, ProcedureName + Parameter + ';' + NL);

        { do not find again the same plug comment by FindPlugOccurence }
        CodeSearchBegin := PEnd;

        AnyOccurencesInThisCodeIndex := true;
        AnyOccurences := true;
      end;

      if AnyOccurencesInThisCodeIndex then
      begin
        { added "plugged_x" function must be forward declared first.
          Otherwise it could be defined after it is needed, or inside different
          compilation unit. }
        if ForwardDeclareInFinalShader and (CodeIndex = 0) then
          PlugDirectly(CodeForPlugValue, CodeIndex, '/* PLUG-DECLARATIONS */', PlugForwardDeclaration, true) else
          PlugDirectly(Code            , CodeIndex, '/* PLUG-DECLARATIONS */', PlugForwardDeclaration, true);
      end;
    end;

    if (not AnyOccurences) and WarnMissingPlugs then
      OnWarning(wtMinor, 'VRML/X3D', Format('Plug name "%s" not declared', [PlugName]));
  until false;

  { regardless if any (and how many) plug points were found,
    always insert PlugValue into CodeForPlugValue }
  CodeForPlugValue.Add(PlugValue);
end;

function TVRMLShader.PlugDirectly(Code: TDynStringArray;
  const CodeIndex: Cardinal;
  const PlugName, PlugValue: string;
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
    OnWarning(wtMinor, 'VRML/X3D', Format('Plug point "%s" not found', [PlugName]));
end;

procedure TVRMLShader.EnableEffects(Effects: TMFNode;
  const Code: TShaderSource;
  const ForwardDeclareInFinalShader: boolean);
begin
  EnableEffects(Effects.Items, Code, ForwardDeclareInFinalShader);
end;

procedure TVRMLShader.EnableEffects(Effects: TVRMLNodeList;
  const Code: TShaderSource;
  const ForwardDeclareInFinalShader: boolean);

  procedure EnableEffect(Effect: TNodeEffect);

    procedure EnableEffectPart(Part: TNodeEffectPart);
    var
      Contents: string;
      PartType: TShaderType;
    begin
      Contents := Part.LoadContents;
      if (Contents <> '') and Part.FdType.GetValue(PartType) then
      begin
        Plug(PartType, Contents, Code, ForwardDeclareInFinalShader);
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

    if Effect.FdLanguage.Value <> 'GLSL' then
    begin
      OnWarning(wtMinor, 'VRML/X3D', Format('Unknown shading language "%s" for Effect node',
        [Effect.FdLanguage.Value]));
      Exit;
    end;

    for I := 0 to Effect.FdParts.Count - 1 do
      if Effect.FdParts[I] is TNodeEffectPart then
        EnableEffectPart(TNodeEffectPart(Effect.FdParts[I]));

    UniformsNodes.Add(Effect);
  end;

var
  I: Integer;
begin
  for I := 0 to Effects.Count - 1 do
    if Effects[I] is TNodeEffect then
      EnableEffect(TNodeEffect(Effects[I]));
end;

procedure TVRMLShader.LinkProgram(AProgram: TVRMLShaderProgram);
var
  TextureApply, TextureColorDeclare, TextureCoordInitialize,
    TextureCoordMatrix, TextureUniformsDeclare: string;
  TextureUniformsSet: boolean;

  procedure EnableTextures;
  var
    I: Integer;
  begin
    TextureApply := '';
    TextureColorDeclare := '';
    TextureCoordInitialize := '';
    TextureCoordMatrix := '';
    TextureUniformsDeclare := '';
    TextureUniformsSet := true;

    for I := 0 to TextureShaders.Count - 1 do
      TextureShaders[I].Enable(TextureApply, TextureColorDeclare,
        TextureCoordInitialize, TextureCoordMatrix, TextureUniformsDeclare);
  end;

  { Applies effects from various strings here.
    This also finalizes applying textures. }
  procedure EnableInternalEffects;
  const
    PCFDefine: array [TPercentageCloserFiltering] of string =
    ( '', '#define PCF4', '#define PCF4_BILINEAR', '#define PCF16' );
    ShadowMapsFunctions: array [boolean { vsm? }] of string =
    ({$I shadow_map_common.fs.inc}, {$I variance_shadow_map_common.fs.inc});
  begin
    PlugDirectly(Source[stVertex], 0, '/* PLUG: vertex_eye_space',
      TextureCoordInitialize + TextureCoordGen + TextureCoordMatrix + ClipPlane, false);
    PlugDirectly(Source[stFragment], 0, '/* PLUG: texture_apply',
      TextureColorDeclare + TextureApply, false);
    PlugDirectly(Source[stFragment], 0, '/* PLUG: fragment_end', FragmentEnd, false);

    if not PlugDirectly(Source[stFragment], 0, '/* PLUG-DECLARATIONS */',
      TextureUniformsDeclare + NL +
      DeclareShadowFunctions, false) then
    begin
      { When we cannot find /* PLUG-DECLARATIONS */, it also means we have
        base shader from ComposedShader. In this case, forcing
        TextureUniformsDeclare at the beginning of shader code
        (by InsertAtBeginIfNotFound) would be bad (in case ComposedShader
        has some #version at the beginning). So we choose the safer route
        to *not* integrate our texture handling with ComposedShader.

        We also remove uniform values for textures, to avoid
        "unused kambi_texture_%d" warning. Setting TextureUniformsSet
        will make it happen. }
      TextureUniformsSet := false;
    end;

    { Don't add to empty Source[stFragment], in case ComposedShader
      doesn't want any fragment shader }
    if Source[stFragment].Count <> 0 then
      Source[stFragment].Add(PCFDefine[PercentageCloserFiltering] + NL +
        ShadowMapsFunctions[VarianceShadowMaps]);
  end;

var
  PassLightsUniforms: boolean;

  procedure EnableLights;
  var
    I: Integer;
    LightShaderBack, LightShaderFront: string;
  begin
    PassLightsUniforms := false;

    { If we have no fragment/vertex shader (means that we used ComposedShader
      node without one shader) then don't add any code.
      Otherwise we would create a shader without any main() inside.

      Source.Append later also has some safeguard against this,
      but we need to check it earlier (to avoid plugging LightShaderBack),
      and check them both (as vertex and fragment code cooperates,
      so we need both or none). }
    if (Source[stFragment].Count = 0) or
       (Source[stVertex].Count = 0) then
      Exit;

    if Lighting then
    begin
      Source[stFragment][0] := '#define LIT' + NL + Source[stFragment][0];
      Source[stVertex  ][0] := '#define LIT' + NL + Source[stVertex  ][0];

      PassLightsUniforms := true;

      for I := 0 to LightShaders.Count - 1 do
      begin
        LightShaderBack  := LightShaders[I].Code[stFragment][0];
        LightShaderFront := LightShaderBack;

        LightShaderBack := StringReplace(LightShaderBack,
          'gl_SideLightProduct', 'gl_BackLightProduct' , [rfReplaceAll]);
        LightShaderFront := StringReplace(LightShaderFront,
          'gl_SideLightProduct', 'gl_FrontLightProduct', [rfReplaceAll]);

        LightShaderBack := StringReplace(LightShaderBack,
          'add_light_contribution_side', 'add_light_contribution_back' , [rfReplaceAll]);
        LightShaderFront := StringReplace(LightShaderFront,
          'add_light_contribution_side', 'add_light_contribution_front', [rfReplaceAll]);

        Plug(stFragment, LightShaderBack);
        Plug(stFragment, LightShaderFront);

        Source.Append(LightShaders[I].Code);
      end;
    end;
  end;

var
  BumpMappingUniformName1: string;
  BumpMappingUniformValue1: LongInt;
  BumpMappingUniformName2: string;
  BumpMappingUniformValue2: Single;

  procedure EnableShaderBumpMapping;
  const
    SteepParallaxDeclarations: array [boolean] of string = ('',
      'float kambi_bm_height;' +NL+
      'vec2 kambi_parallax_tex_coord;' +NL
    );

    SteepParallaxShift: array [boolean] of string = (
      { Classic parallax bump mapping }
      'float height = (texture2D(kambi_normal_map, tex_coord).a - 1.0/2.0) * kambi_parallax_bm_scale;' +NL+
      'tex_coord += height * v_to_eye.xy /* / v_to_eye.z*/;' +NL,

      { Steep parallax bump mapping }
      '/* At smaller view angles, much more iterations needed, otherwise ugly' +NL+
      '   aliasing arifacts quickly appear. */' +NL+
      'float num_steps = mix(30.0, 10.0, v_to_eye.z);' +NL+
      'float step = 1.0 / num_steps;' +NL+

      { Should we remove "v_to_eye.z" below, i.e. should we apply
        "offset limiting" ? In works about steep parallax mapping,
        v_to_eye.z is present, and in sample steep parallax mapping
        shader they suggest that it doesn't really matter.
        My tests confirm this, so I leave v_to_eye.z component. }

      'vec2 delta = -v_to_eye.xy * kambi_parallax_bm_scale / (v_to_eye.z * num_steps);' +NL+
      'float height = 1.0;' +NL+
      'kambi_bm_height = texture2D(kambi_normal_map, tex_coord).a;' +NL+

      { It's known problem that NVidia GeForce FX 5200 fails here with

           error C5011: profile does not support "while" statements
           and "while" could not be unrolled.

        I could workaround this problem (by using
          for (int i = 0; i < steep_steps_max; i++)
        loop and
          if (! (kambi_bm_height < height)) break;
        , this is possible to unroll). But it turns out that this still
        (even with steep_steps_max = 1) works much too slow on this hardware...
        so I simply fallback to non-steep version of parallax mapping
        if this doesn't compile. TODO: we no longer retry with steep? }

      'while (kambi_bm_height < height)' +NL+
      '{' +NL+
      '  height -= step;' +NL+
      '  tex_coord += delta;' +NL+
      '  kambi_bm_height = texture2D(kambi_normal_map, tex_coord).a;' +NL+
      '}' +NL+

      { Save for SteepParallaxShadowing }
      'kambi_parallax_tex_coord = tex_coord;'
    );

    SteepParallaxShadowing =
      'uniform float kambi_parallax_bm_scale;' +NL+
      'uniform sampler2D kambi_normal_map;' +NL+
      'varying vec3 kambi_light_direction_tangent_space;' +NL+

      'float kambi_bm_height;' +NL+
      'vec2 kambi_parallax_tex_coord;' +NL+

      { This has to be done after PLUG_texture_coord_shift (done from PLUG_texture_apply),
        as we depend that global kambi_bm_height/kambi_parallax_tex_coord
        are already set correctly. }
      'void PLUG_steep_parallax_shadow_apply(inout vec4 fragment_color)' +NL+
      '{' +NL+
      '  vec3 light_dir = normalize(kambi_light_direction_tangent_space);' +NL+

      '  /* We basically do the same thing as when we calculate tex_coord' +NL+
      '     with steep parallax mapping.' +NL+
      '     Only now we increment height, and we use light_dir instead of' +NL+
      '     v_to_eye. */' +NL+
      '  float num_steps = mix(30.0, 10.0, light_dir.z);' +NL+

      '  float step = 1.0 / num_steps;' +NL+

      '  vec2 delta = light_dir.xy * kambi_parallax_bm_scale / (light_dir.z * num_steps);' +NL+

      '  /* Do the 1st step always, otherwise initial height = shadow_map_height' +NL+
      '     and we would be considered in our own shadow. */' +NL+
      '  float height = kambi_bm_height + step;' +NL+
      '  vec2 shadow_texture_coord = kambi_parallax_tex_coord + delta;' +NL+
      '  float shadow_map_height = texture2D(kambi_normal_map, shadow_texture_coord).a;' +NL+

      '  while (shadow_map_height < height && height < 1.0)' +NL+
      '  {' +NL+
      '    height += step;' +NL+
      '    shadow_texture_coord += delta;' +NL+
      '    shadow_map_height = texture2D(kambi_normal_map, shadow_texture_coord).a;' +NL+
      '  }' +NL+

      '  if (shadow_map_height >= height)' +NL+
      '  {' +NL+
      '    /* TODO: setting appropriate light contribution to 0 would be more correct. But for now, this self-shadowing is hacky, always from light source 0, and after the light calculation is actually done. */' +NL+
      '    fragment_color.rgb /= 2.0;' +NL+
      '  }' +NL+
      '}';

  var
    VertexEyeBonusDeclarations, VertexEyeBonusCode: string;
  begin
    if FBumpMapping = bmNone then Exit;

    VertexEyeBonusDeclarations := '';
    VertexEyeBonusCode := '';

    if FHeightMapInAlpha and (FBumpMapping >= bmParallax) then
    begin
      { parallax bump mapping }
      Plug(stFragment,
        'uniform float kambi_parallax_bm_scale;' +NL+
        'uniform sampler2D kambi_normal_map;' +NL+
        'varying vec3 kambi_vertex_to_eye_in_tangent_space;' +NL+
        SteepParallaxDeclarations[FBumpMapping >= bmSteepParallax] +
        NL+
        'void PLUG_texture_coord_shift(inout vec2 tex_coord)' +NL+
        '{' +NL+
        { We have to normalize kambi_vertex_to_eye_in_tangent_space again, just like normal vectors. }
        '  vec3 v_to_eye = normalize(kambi_vertex_to_eye_in_tangent_space);' +NL+
        SteepParallaxShift[FBumpMapping >= bmSteepParallax] +
        '}');
      VertexEyeBonusDeclarations :=
        'varying vec3 kambi_vertex_to_eye_in_tangent_space;' +NL;
      VertexEyeBonusCode :=
        'mat3 object_to_tangent_space = transpose(kambi_tangent_to_object_space);' +NL+
        'mat3 eye_to_object_space = mat3(gl_ModelViewMatrix[0][0], gl_ModelViewMatrix[1][0], gl_ModelViewMatrix[2][0],' +NL+
        '                                gl_ModelViewMatrix[0][1], gl_ModelViewMatrix[1][1], gl_ModelViewMatrix[2][1],' +NL+
        '                                gl_ModelViewMatrix[0][2], gl_ModelViewMatrix[1][2], gl_ModelViewMatrix[2][2]);' +NL+
        'mat3 eye_to_tangent_space = object_to_tangent_space * eye_to_object_space;' +NL+
        { Theoretically faster implementation below, not fully correct ---
          assume that transpose is enough to invert this matrix. Tests proved:
          - results seem the same
          - but it's not really faster. }
        { 'mat3 eye_to_tangent_space = transpose(kambi_tangent_to_eye_space);' +NL+ }
        'kambi_vertex_to_eye_in_tangent_space = normalize(eye_to_tangent_space * (-vec3(vertex_eye)) );' +NL;

      BumpMappingUniformName2 := 'kambi_parallax_bm_scale';
      BumpMappingUniformValue2 := FHeightMapScale;

      if FBumpMapping >= bmSteepParallaxShadowing then
      begin
        Plug(stFragment, SteepParallaxShadowing);
        VertexEyeBonusDeclarations +=
          'varying vec3 kambi_light_direction_tangent_space;' +NL;
        VertexEyeBonusCode +=
          { We only cast shadow from gl_LightSource[0]. }
          'vec3 light_dir = gl_LightSource[0].position.xyz;' +NL+
          '/* We assume gl_LightSource[0].position.w = 1 (if not 0). */' +NL+
          'if (gl_LightSource[0].position.w != 0.0)' +NL+
          '  light_dir -= vec3(vertex_eye);' +NL+
          'light_dir = normalize(light_dir);' +NL+

          'kambi_light_direction_tangent_space = eye_to_tangent_space * light_dir;' +NL;
      end;
    end;

    Plug(stVertex,
      '#version 120' +NL+ { version 120 needed for transpose() }
      'attribute mat3 kambi_tangent_to_object_space;' +NL+
      'varying mat3 kambi_tangent_to_eye_space;' +NL+
      VertexEyeBonusDeclarations +
      NL+
      'void PLUG_vertex_eye_space(const in vec4 vertex_eye, const in vec3 normal_eye)' +NL+
      '{' +NL+
      '  kambi_tangent_to_eye_space = gl_NormalMatrix * kambi_tangent_to_object_space;' +NL+
      VertexEyeBonusCode +
      '}');

    Plug(stFragment,
      'varying mat3 kambi_tangent_to_eye_space;' +NL+
      'uniform sampler2D kambi_normal_map;' +NL+
      NL+
      'void PLUG_fragment_eye_space(const vec4 vertex, inout vec3 normal_eye_fragment)' +NL+
      '{' +NL+
      '  /* Read normal from the texture, this is the very idea of bump mapping.' +NL+
      '     Unpack normals, they are in texture in [0..1] range and I want in [-1..1].' +NL+
      '     Our normal map is always indexed using gl_TexCoord[0] (this way' +NL+
      '     we depend on already correct gl_TexCoord[0], multiplied by TextureTransform' +NL+
      '     and such). */' +NL+
      '  vec3 normal_tangent = texture2D(kambi_normal_map, gl_TexCoord[0].st).xyz * 2.0 - vec3(1.0);' +NL+

      '  /* We have to take two-sided lighting into account here, in tangent space.' +NL+
      '     Simply negating whole normal in eye space (like we do without bump mapping)' +NL+
      '     would not work good, check e.g. insides of demo_models/bump_mapping/room_for_parallax_final.wrl. */' +NL+
      '  if (gl_FrontFacing)' +NL+
      '    /* Avoid AMD bug http://forums.amd.com/devforum/messageview.cfm?catid=392&threadid=148827&enterthread=y' +NL+
      '       It causes both (gl_FrontFacing) and (!gl_FrontFacing) to be true...' +NL+
      '       To minimize the number of problems, never use "if (!gl_FrontFacing)",' +NL+
      '       only "if (gl_FrontFacing)".' +NL+
      '       See template.fs for more comments.' +NL+
      '    */ ; else' +NL+
      '    normal_tangent.z = -normal_tangent.z;' +NL+

      '  normal_eye_fragment = normalize(kambi_tangent_to_eye_space * normal_tangent);' +NL+
      '}');

    BumpMappingUniformName1 := 'kambi_normal_map';
    BumpMappingUniformValue1 := FNormalMapTextureUnit;
  end;

  procedure EnableShaderMaterialFromColor;
  begin
    if MaterialFromColor then
    begin
      Plug(stVertex,
        'void PLUG_vertex_eye_space(const in vec4 vertex_eye, const in vec3 normal_eye)' +NL+
        '{' +NL+
        '  gl_FrontColor = gl_Color;' +NL+
        '  gl_BackColor = gl_Color;' +NL+
        '}');

        { Sidenote: What happens without this? That is, what's in OpenGL
          gl_Front/BackLightProducts (used by normal shader code) when
          glEnable(GL_COLOR_MATERIAL) was called
          --- the value from glMaterial call, or the value from glColor
          (or color array)? IOW, is glEnable(GL_COLOR_MATERIAL) automatically
          already applied for shader uniforms?

          Looks like it's undefined:
          - NVidia GeForce 450 GTS (kocury) behaves like an undefined
            color (from some previous shape) leaked on the current shape.

            (Although for MaterialFromColor, we always have lit shape,
            with set glMaterial, and set glColor (or color array).
            Although we set glColor (or color array) after enabling
            GL_COLOR_MATERIAL, which means material color is undefined
            for a short time, but it's always defined before actual glDraw*
            call.)

            Looks like NVidia just doesn't know (like me :) what to put
            inside uniform gl_Front/BackLightProducts, so it just doesn't
            change it at all.

          - Radeon X1600 (fglrx, chantal) behaves like GL_COLOR_MATERIAL
            doesn't affect shader. gl_Front/BackLightProducts contain
            (it seems) values from glMaterial (multiplied by light),
            never glColor. }

      Plug(stFragment,
        'void PLUG_material_light_diffuse(inout vec4 diffuse, const in vec4 vertex_eye, const in vec3 normal_eye, const in gl_LightSourceParameters light_source, const in gl_MaterialParameters material)' +NL+
        '{' +NL+
        '  diffuse = light_source.diffuse * gl_Color;' +NL+
        '}' +NL+
        NL+
        'void PLUG_lighting_apply(inout vec4 fragment_color, const vec4 vertex_eye, const vec3 normal_eye_fragment)' +NL+
        '{' +NL+
        '  fragment_color.a = gl_Color.a;' +NL+
        '}');
    end;
  end;

  procedure EnableShaderFog;
  var
    FogFactor, CoordinateSource: string;
  begin
    if FFogEnabled then
    begin
      case FFogCoordinateSource of
        fcDepth           : CoordinateSource := 'vertex_eye.z';
        fcPassedCoordinate: CoordinateSource := 'gl_FogCoord';
        else raise EInternalError.Create('TVRMLShader.EnableShaderFog:FogCoordinateSource?');
      end;

      Plug(stVertex,
        'void PLUG_vertex_eye_space(const in vec4 vertex_eye, const in vec3 normal_eye)' +NL+
        '{' +NL+
        '  gl_FogFragCoord = ' + CoordinateSource + ';' +NL+
        '}');

      case FFogType of
        ftLinear: FogFactor := '(gl_Fog.end - gl_FogFragCoord) * gl_Fog.scale';
        ftExp   : FogFactor := 'exp(-gl_Fog.density * gl_FogFragCoord)';
        else raise EInternalError.Create('TVRMLShader.EnableShaderFog:FogType?');
      end;

      Plug(stFragment,
        'void PLUG_fog_apply(inout vec4 fragment_color, const vec3 normal_eye_fragment)' +NL+
        '{' +NL+
        '  fragment_color.rgb = mix(fragment_color.rgb, gl_Fog.color.rgb,' +NL+
        '    clamp(1.0 - ' + FogFactor + ', 0.0, 1.0));' +NL+
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
        if TextureShaders[I].UniformName <> '' then
          AProgram.SetUniform(TextureShaders[I].UniformName,
                              TextureShaders[I].UniformValue);
    end;

    if BumpMappingUniformName1 <> '' then
      AProgram.SetUniform(BumpMappingUniformName1,
                          BumpMappingUniformValue1);

    if BumpMappingUniformName2 <> '' then
      AProgram.SetUniform(BumpMappingUniformName2,
                          BumpMappingUniformValue2);

    AProgram.BindUniforms(UniformsNodes, false);

    if PassLightsUniforms then
      for I := 0 to LightShaders.Count - 1 do
      begin
        if LightShaders[I].LightUniformName1 <> '' then
          AProgram.SetUniform(Format(
            LightShaders[I].LightUniformName1, [I]),
            LightShaders[I].LightUniformValue1);
        if LightShaders[I].LightUniformName2 <> '' then
          AProgram.SetUniform(Format(
            LightShaders[I].LightUniformName2, [I]),
            LightShaders[I].LightUniformValue2);
      end;

    AProgram.Disable;
  end;

var
  I: Integer;
begin
  EnableTextures;
  EnableInternalEffects;
  EnableLights;
  EnableShaderMaterialFromColor;
  EnableShaderBumpMapping;
  EnableShaderFog;
  if AppearanceEffects <> nil then
    EnableEffects(AppearanceEffects);
  if GroupEffects <> nil then
    EnableEffects(GroupEffects);

  if Log and LogShaders then
  begin
    for I := 0 to Source[stVertex].Count - 1 do
      WritelnLogMultiline(Format('Generated GLSL vertex shader[%d]', [I]),
        Source[stVertex][I]);
    for I := 0 to Source[stFragment].Count - 1 do
      WritelnLogMultiline(Format('Generated GLSL fragment shader[%d]', [I]),
        Source[stFragment][I]);
  end;

  try
    if (Source[stVertex].Count = 0) and
       (Source[stFragment].Count = 0) then
      raise EGLSLError.Create('No vertex and no fragment shader for GLSL program');

    for I := 0 to Source[stVertex].Count - 1 do
      AProgram.AttachVertexShader(Source[stVertex][I]);
    for I := 0 to Source[stFragment].Count - 1 do
      AProgram.AttachFragmentShader(Source[stFragment][I]);
    AProgram.Link(true);

    if SelectedNode <> nil then
      SelectedNode.EventIsValid.Send(true);
  except
    if SelectedNode <> nil then
      SelectedNode.EventIsValid.Send(false);
    raise;
  end;

  { All user VRML/X3D uniform values go through SetUniformFromField,
    that always raises exception on invalid names/types, regardless
    of UniformNotFoundAction / UniformTypeMismatchAction values.

    So settings below only control what happens on our uniform values.
    - Missing uniform name should be ignored, as it's normal in some cases:
      - When ShadowVisualizeDepth is used, almost everything (besides
        the single visualized shadow map) is unused.
      - When all the lights are off (including headlight) then normal vectors
        are unused, and so the normalmap texture is unused.

      Avoid producing any warnings in this case, as this is normal situation.
      Actually needed at least on NVidia GeForce 450 GTS (proprietary OpenGL
      under Linux), on ATI (tested proprietary OpenGL drivers under Linux and Windows)
      this doesn't seem needed (less aggressive removal of unused vars).

    - Invalid types should always be reported (in debug mode, as OpenGL errors,
      this is fastest). We carefully code to always specify correct types
      for our uniform variables. }
  AProgram.UniformNotFoundAction := uaIgnore;
  AProgram.UniformTypeMismatchAction := utGLError;

  { set uniforms that will not need to be updated at each SetupUniforms call }
  SetupUniformsOnce;
end;

function TVRMLShader.CodeHash: TShaderCodeHash;

  { Add to FCodeHash some stuff that must be added at the end,
    since it can be changed back (replacing previous values) during TVRMLShader
    lifetime. }
  procedure CodeHashFinalize;
  begin
    FCodeHash.AddInteger(Ord(PercentageCloserFiltering) * 1009);
    FCodeHash.AddInteger(Ord(VarianceShadowMaps) * 1823);
  end;

begin
  if not CodeHashFinalized then
  begin
    CodeHashFinalize;
    CodeHashFinalized := true;
  end;
  Result := FCodeHash;
end;

procedure TVRMLShader.EnableTexture(const TextureUnit: Cardinal;
  const TextureType: TTextureType;
  const Node: TNodeX3DTextureNode;
  const Env: TTextureEnv;
  const ShadowMapSize: Cardinal;
  const ShadowLight: TNodeX3DLightNode;
  const ShadowVisualizeDepth: boolean);
var
  TextureShader: TTextureShader;
begin
  { Enable for fixed-function pipeline }
  if GLUseMultiTexturing then
    glActiveTextureARB(GL_TEXTURE0 + TextureUnit);
  case TextureType of
    tt2D, tt2DShadow:
      begin
        glEnable(GL_TEXTURE_2D);
        if GL_ARB_texture_cube_map then glDisable(GL_TEXTURE_CUBE_MAP_ARB);
        if GL_EXT_texture3D        then glDisable(GL_TEXTURE_3D_EXT);
      end;
    ttCubeMap:
      begin
        glDisable(GL_TEXTURE_2D);
        if GL_ARB_texture_cube_map then glEnable(GL_TEXTURE_CUBE_MAP_ARB);
        if GL_EXT_texture3D        then glDisable(GL_TEXTURE_3D_EXT);
      end;
    tt3D:
      begin
        glDisable(GL_TEXTURE_2D);
        if GL_ARB_texture_cube_map then glDisable(GL_TEXTURE_CUBE_MAP_ARB);
        if GL_EXT_texture3D        then glEnable(GL_TEXTURE_3D_EXT);
      end;
    ttShader:
      begin
        glDisable(GL_TEXTURE_2D);
        if GL_ARB_texture_cube_map then glDisable(GL_TEXTURE_CUBE_MAP_ARB);
        if GL_EXT_texture3D        then glDisable(GL_TEXTURE_3D_EXT);
      end;
    else raise EInternalError.Create('TextureEnableDisable?');
  end;

  { Enable for shader pipeline }

  TextureShader := TTextureShader.Create;
  TextureShader.TextureUnit := TextureUnit;
  TextureShader.TextureType := TextureType;
  TextureShader.Node := Node;
  TextureShader.Env := Env;
  TextureShader.ShadowMapSize := ShadowMapSize;
  TextureShader.ShadowLight := ShadowLight;
  TextureShader.ShadowVisualizeDepth := ShadowVisualizeDepth;
  TextureShader.Shader := Self;

  TextureShaders.Add(TextureShader);

  if (TextureType in [ttShader, tt2DShadow]) or
     (Node.FdEffects.Count <> 0) then
    ShapeRequiresShaders := true;

  TextureShader.Prepare(FCodeHash);
end;

procedure TVRMLShader.EnableTexGen(const TextureUnit: Cardinal;
  const Generation: TTexGenerationComplete);
begin
  { Enable for fixed-function pipeline }
  if GLUseMultiTexturing then
    glActiveTextureARB(GL_TEXTURE0 + TextureUnit);
  { glEnable(GL_TEXTURE_GEN_*) below }

  { Enable for shader pipeline }
  case Generation of
    tgSphere:
      begin
        glEnable(GL_TEXTURE_GEN_S);
        glEnable(GL_TEXTURE_GEN_T);
        TextureCoordGen += Format(
          { Sphere mapping in GLSL adapted from
            http://www.ozone3d.net/tutorials/glsl_texturing_p04.php#part_41
            by Jerome Guinot aka 'JeGX', many thanks! }
          'vec3 r = reflect( normalize(vec3(kambi_vertex_eye)), kambi_normal_eye );' + NL +
          'float m = 2.0 * sqrt( r.x*r.x + r.y*r.y + (r.z+1.0)*(r.z+1.0) );' + NL +
          '/* Using 1.0 / 2.0 instead of 0.5 to workaround fglrx bugs */' + NL +
          'gl_TexCoord[%d].st = r.xy / m + vec2(1.0, 1.0) / 2.0;',
          [TextureUnit]);
        FCodeHash.AddInteger(1301 * (TextureUnit + 1));
      end;
    tgNormal:
      begin
        glEnable(GL_TEXTURE_GEN_S);
        glEnable(GL_TEXTURE_GEN_T);
        glEnable(GL_TEXTURE_GEN_R);
        TextureCoordGen += Format('gl_TexCoord[%d].xyz = kambi_normal_eye;' + NL,
          [TextureUnit]);
        FCodeHash.AddInteger(1303 * (TextureUnit + 1));
      end;
    tgReflection:
      begin
        glEnable(GL_TEXTURE_GEN_S);
        glEnable(GL_TEXTURE_GEN_T);
        glEnable(GL_TEXTURE_GEN_R);
        { Negate reflect result --- just like for demo_models/water/water_reflections_normalmap.fs }
        TextureCoordGen += Format('gl_TexCoord[%d].xyz = -reflect(-vec3(kambi_vertex_eye), kambi_normal_eye);' + NL,
          [TextureUnit]);
        FCodeHash.AddInteger(1307 * (TextureUnit + 1));
      end;
    else raise EInternalError.Create('TVRMLShader.EnableTexGen:Generation?');
  end;
end;

procedure TVRMLShader.EnableTexGen(const TextureUnit: Cardinal;
  const Generation: TTexGenerationComponent; const Component: TTexComponent);
const
  PlaneComponentNames: array [TTexComponent] of char = ('S', 'T', 'R', 'Q');
  { Note: R changes to p ! }
  VectorComponentNames: array [TTexComponent] of char = ('s', 't', 'p', 'q');
var
  PlaneName, CoordSource: string;
begin
  { Enable for fixed-function pipeline }
  if GLUseMultiTexturing then
    glActiveTextureARB(GL_TEXTURE0 + TextureUnit);
  case Component of
    0: glEnable(GL_TEXTURE_GEN_S);
    1: glEnable(GL_TEXTURE_GEN_T);
    2: glEnable(GL_TEXTURE_GEN_R);
    3: glEnable(GL_TEXTURE_GEN_Q);
    else raise EInternalError.Create('TVRMLShader.EnableTexGen:Component?');
  end;

  { Enable for shader pipeline.
    See helpful info about simulating glTexGen in GLSL in:
    http://www.mail-archive.com/osg-users@lists.openscenegraph.org/msg14238.html }

  case Generation of
    tgEye   : begin PlaneName := 'gl_EyePlane'   ; CoordSource := 'kambi_vertex_eye'; end;
    tgObject: begin PlaneName := 'gl_ObjectPlane'; CoordSource := 'vertex_object' ; end;
    else raise EInternalError.Create('TVRMLShader.EnableTexGen:Generation?');
  end;

  TextureCoordGen += Format('gl_TexCoord[%d].%s = dot(%s, %s%s[%0:d]);' + NL,
    [TextureUnit, VectorComponentNames[Component],
     CoordSource, PlaneName, PlaneComponentNames[Component]]);
  FCodeHash.AddInteger(1319 * (TextureUnit + 1) * (Ord(Generation) + 1) * (Component + 1));
end;

procedure TVRMLShader.DisableTexGen(const TextureUnit: Cardinal);
begin
  { Disable for fixed-function pipeline }
  if GLUseMultiTexturing then
    glActiveTextureARB(GL_TEXTURE0 + TextureUnit);
  glDisable(GL_TEXTURE_GEN_S);
  glDisable(GL_TEXTURE_GEN_T);
  glDisable(GL_TEXTURE_GEN_R);
  glDisable(GL_TEXTURE_GEN_Q);
end;

procedure TVRMLShader.EnableClipPlane(const ClipPlaneIndex: Cardinal);
begin
  glEnable(GL_CLIP_PLANE0 + ClipPlaneIndex);
  if ClipPlane = '' then
    ClipPlane := 'gl_ClipVertex = kambi_vertex_eye;';
end;

procedure TVRMLShader.DisableClipPlane(const ClipPlaneIndex: Cardinal);
begin
  glDisable(GL_CLIP_PLANE0 + ClipPlaneIndex);
end;

procedure TVRMLShader.EnableAlphaTest;
begin
  { Enable for fixed-function pipeline }
  glEnable(GL_ALPHA_TEST);

  { Enable for shader pipeline. We know alpha comparison is always < 0.5 }
  FragmentEnd +=
    '/* Do the trick with 1.0 / 2.0, instead of comparing with 0.5, to avoid fglrx bugs */' + NL +
    'if (2.0 * gl_FragColor.a < 1.0)' + NL +
    '  discard;' + NL;
end;

procedure TVRMLShader.EnableBumpMapping(const BumpMapping: TBumpMapping;
  const NormalMapTextureUnit: Cardinal;
  const HeightMapInAlpha: boolean; const HeightMapScale: Single);
begin
  FBumpMapping := BumpMapping;
  FNormalMapTextureUnit := NormalMapTextureUnit;
  FHeightMapInAlpha := HeightMapInAlpha;
  FHeightMapScale := HeightMapScale;

  if FBumpMapping <> bmNone then
  begin
    ShapeRequiresShaders := true;
    FCodeHash.AddInteger(47 * (
      Ord(FBumpMapping) +
      FNormalMapTextureUnit +
      Ord(FHeightMapInAlpha)));
    FCodeHash.AddFloat(FHeightMapScale);
  end;
end;

procedure TVRMLShader.EnableLight(const Number: Cardinal; Light: PLightInstance);
var
  LightShader: TLightShader;
begin
  LightShader := TLightShader.Create;
  LightShader.Number := Number;
  LightShader.Light := Light;
  LightShader.Node := Light^.Node;
  LightShader.Shader := Self;

  LightShaders.Add(LightShader);

  if Light^.Node.FdEffects.Count <> 0 then
    ShapeRequiresShaders := true;

  LightShader.Prepare(FCodeHash, LightShaders.Count - 1);
end;

procedure TVRMLShader.EnableFog(const FogType: TFogType;
  const FogCoordinateSource: TFogCoordinateSource);
begin
  FFogEnabled := true;
  FFogType := FogType;
  FFogCoordinateSource := FogCoordinateSource;
  FCodeHash.AddInteger(
    67 * (Ord(FFogType) + 1) +
    709 * (Ord(FFogCoordinateSource) + 1));
end;

function TVRMLShader.EnableCustomShaderCode(Shaders: TMFNodeShaders;
  out Node: TNodeComposedShader): boolean;
var
  I, J: Integer;
  Part: TNodeShaderPart;
  PartSource: String;
  PartType, SourceType: TShaderType;
begin
  Result := false;
  for I := 0 to Shaders.Count - 1 do
  begin
    Node := Shaders.GLSLShader(I);
    if Node <> nil then
    begin
      Result := true;

      { Clear whole Source }
      for SourceType := Low(SourceType) to High(SourceType) do
        Source[SourceType].Count := 0;

      { Iterate over Node.FdParts, looking for vertex shaders
        and fragment shaders. }
      for J := 0 to Node.FdParts.Count - 1 do
        if Node.FdParts[J] is TNodeShaderPart then
        begin
          Part := TNodeShaderPart(Node.FdParts[J]);
          PartSource := Part.LoadContents;
          if (PartSource <> '') and Part.FdType.GetValue(PartType) then
            Source[PartType].Add(PartSource);
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
    if Shaders[I] is TNodeX3DShaderNode then
      TNodeX3DShaderNode(Shaders[I]).EventIsSelected.Send(false);
  end;
end;

procedure TVRMLShader.EnableAppearanceEffects(Effects: TMFNode);
begin
  AppearanceEffects := Effects;
  if AppearanceEffects.Count <> 0 then
  begin
    ShapeRequiresShaders := true;
    FCodeHash.AddEffects(AppearanceEffects.Items);
  end;
end;

procedure TVRMLShader.EnableGroupEffects(Effects: TVRMLNodeList);
begin
  GroupEffects := Effects;
  if GroupEffects.Count <> 0 then
  begin
    ShapeRequiresShaders := true;
    FCodeHash.AddEffects(GroupEffects);
  end;
end;

procedure TVRMLShader.EnableLighting;
begin
  Lighting := true;
  FCodeHash.AddInteger(7);
end;

procedure TVRMLShader.EnableMaterialFromColor;
begin
  { glColorMaterial is already set by TVRMLGLRenderer.RenderBegin }
  glEnable(GL_COLOR_MATERIAL);

  { This will cause appropriate shader later }
  MaterialFromColor := true;
  FCodeHash.AddInteger(29);
end;

function TVRMLShader.DeclareShadowFunctions: string;
const
  ShadowDeclare: array [boolean { vsm? }] of string =
  ('float shadow(sampler2DShadow shadowMap, const vec4 shadowMapCoord, const in float size);',
   'float shadow(sampler2D       shadowMap, const vec4 shadowMapCoord, const in float size);');
  ShadowDepthDeclare =
   'float shadow_depth(sampler2D shadowMap, const vec4 shadowMapCoord);';
begin
  Result := ShadowDeclare[VarianceShadowMaps] + NL + ShadowDepthDeclare;
end;

end.
