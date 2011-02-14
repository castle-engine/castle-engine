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

uses GLShaders, FGL, VRMLShadowMaps, VRMLTime, VRMLFields, VRMLNodes;

type
  { Uniform value type, for TUniform. }
  TUniformType = (utLongInt, utSingle);

  { Uniform value to set after binding shader. }
  TUniform = class
    Name: string;
    AType: TUniformType;
    Value: record
      case Integer of
        utLongInt: (LongInt: LongInt);
        utSingle: (Single: Single);
    end;
  end;
  TUniformsList = specialize TFPGObjectList<TUniform>;

  TTextureType = (tt2D, tt2DShadow, ttCubeMap, tt3D);

  TTexGenerationComponent = (tgEye, tgObject);
  TTexGenerationComplete = (tgSphere, tgNormal, tgReflection);
  TTexComponent = 0..3;

  { GLSL program integrated with VRML/X3D. Adds ability to bind uniform
    values from VRML/X3D fields, and to observe VRML/X3D events
    and automatically update uniform values. }
  TVRMLGLSLBaseProgram = class(TGLSLProgram)
  private
    { Events where we registered our EventReceive method. }
    EventsObserved: TVRMLEventsList;

    { Set uniform variable from VRML/X3D field value.
      Uniform name is contained in UniformName. UniformValue indicates
      uniform type and new value (UniformValue.Name is not used).

      This ignores SFNode / MFNode fields (these will be set elsewhere). }
    procedure SetUniformFromField(const UniformName: string;
      const UniformValue: TVRMLField; const EnableDisable: boolean);

    procedure EventReceive(Event: TVRMLEvent; Value: TVRMLField;
      const Time: TVRMLTime);

    { Set uniform shader variable from VRML/X3D field (exposed or not).
      We also start observing an exposed field or eventIn,
      and will automatically update uniform value when we receive an event. }
    procedure BindUniform(const FieldOrEvent: TVRMLInterfaceDeclaration);
  public
    constructor Create;
    destructor Destroy; override;

    { Set and observe uniform variables from given Node.InterfaceDeclarations. }
    procedure BindUniforms(const Node: TVRMLNode);
  end;

  TVRMLShaderProgram = class(TVRMLGLSLBaseProgram)
  private
    { State of TVRMLShader when creating this shader.
      Used to decide when shader needs to be regenerated. }
    LightsEnabled: Cardinal;
    PercentageCloserFiltering: TPercentageCloserFiltering;
    VisualizeDepthMap: boolean;
  end;

  TLightShader = class
  private
    Code: string;
    Node: TNodeX3DLightNode;
  end;
  TLightShaders = class(specialize TFPGObjectList<TLightShader>)
  private
    function Find(const Node: TNodeX3DLightNode; out Shader: TLightShader): boolean;
  end;

  { Create appropriate shader and at the same time set OpenGL parameters
    for fixed-function rendering. Once everything is set up,
    you can use the @link(CreateProgram) to create and link a program
    (that you should then enable), or simply allow the fixed-function
    pipeline to work.

    This is used internally by TVRMLGLRenderer. It isn't supposed to be used
    directly by other code. }
  TVRMLShader = class
  private
    Uniforms: TUniformsList;
    UniformsNodes: TVRMLNodesList;
    TextureApply, TextureCoordInitialize,
      TextureCoordGen, TextureCoordMatrix, FragmentShaderDeclare,
      ClipPlane, FragmentEnd: string;
    FPercentageCloserFiltering: TPercentageCloserFiltering;
    FVisualizeDepthMap: boolean;
    VertexShaderComplete: string;
    FragmentShaderComplete: string;
    PlugIdentifiers: Cardinal;
    LightsEnabled: Cardinal;
    LightShaders: TLightShaders;
  public
    constructor Create;
    destructor Destroy; override;

    { Insert a piece of code at given plugging point.
      Inserts code right before the magic @code(/* PLUG ...*/) comments,
      this way many Plug calls for the same PlugName will insert code in the same
      order.

      @param(ForceDirectInsertion Set to @true to force inserting
        PlugValue code right at the place of magic comment, without
        wrapping it inside a procedure call. This allows for unsafe code
        insertion. Effectively, this treats every magic comment like
        a "declaration".
        (Useful for some tricks, e.g. you can "return" from main
        for shadow maps visualization.))

      @param(RemovePlug Should we remove the magic comment from shader
        source, so it will not be available for further effects.) }
    procedure Plug(const PlugName: string; const PlugValue: string;
      const RemovePlug: boolean = false;
      const ForceDirectInsertion: boolean = false);

    { More flexible version of Plug, that searches and replaces within specified
      code string. It also returns success.

      Note that in case of a plug that creates new GLSL function, the new function
      is still added to the global (complete) shader code.
      That is, PlugNameDeclareProcedures is still searched in the complete shader
      code (it doesn't matter if Code equals to one of *ShaderComplete or not). }
    function PlugCustom(
      var Code: string; const PlugNameDeclareProcedures: string;
      const PlugName: string; const PlugValue: string;
      const RemovePlug, ForceDirectInsertion: boolean): boolean;

    procedure ApplyInternalEffects;
    function CreateProgram: TVRMLShaderProgram;
    procedure SetupUniforms(AProgram: TVRMLShaderProgram);

    { Given one TVRMLShaderProgram, created for the same shape by CreateProgram,
      do these program settings matching current TVRMLShader settings.
      This is used to decide when shape settings (for example,
      lights count or such) change and require regenerating the shader. }
    function ProgramSettingsEqual(AProgram: TVRMLShaderProgram): boolean;

    procedure AddUniform(Uniform: TUniform);

    procedure EnableTexture(const TextureUnit: Cardinal;
      const TextureType: TTextureType; const ShadowMapSize: Cardinal = 0;
      const ShadowLight: TNodeX3DLightNode = nil);
    procedure EnableTexGen(const TextureUnit: Cardinal;
      const Generation: TTexGenerationComponent; const Component: TTexComponent);
    procedure EnableTexGen(const TextureUnit: Cardinal;
      const Generation: TTexGenerationComplete);
    procedure DisableTexGen(const TextureUnit: Cardinal);
    procedure EnableClipPlane(const ClipPlaneIndex: Cardinal);
    procedure DisableClipPlane(const ClipPlaneIndex: Cardinal);
    procedure EnableAlphaTest;
    procedure EnableBumpMapping(const NormalMapTextureUnit: Cardinal);
    procedure EnableLight(const Number: Cardinal; Node: TNodeX3DLightNode);

    property PercentageCloserFiltering: TPercentageCloserFiltering
      read FPercentageCloserFiltering write FPercentageCloserFiltering;
    property VisualizeDepthMap: boolean
      read FVisualizeDepthMap write FVisualizeDepthMap;

    procedure EnableEffects(Effects: TMFNode);
  end;

implementation

uses SysUtils, GL, GLExt, KambiUtils, KambiStringUtils, KambiGLUtils,
  VRMLErrors, KambiLog, StrUtils, VectorMath, Base3D;

{ TODO: a way to turn off using fixed-function pipeline completely
  will be needed some day. Currently, some functions here call
  fixed-function glEnable... stuff.

  TODO: caching shader programs, using the same program if all settings
  are the same, will be needed some day. TShapeCache is not a good place
  for this, as the conditions for two shapes to share arrays/vbos
  are smaller/different (for example, two different geometry nodes
  can definitely share the same shader).

  Maybe caching should be done in this unit, or maybe in TVRMLGLRenderer
  in some TShapeShaderCache or such.

  TODO: a way to turn on/off per-pixel shading should be available.

  TODO: some day, avoid using predefined OpenGL state variables.
  Use only shader uniforms. Right now, we allow some state to be assigned
  using direct normal OpenGL fixed-function functions in VRMLGLRenderer,
  and our shaders just use it.
}

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

{ TVRMLGLSLBaseProgram ------------------------------------------------------- }

constructor TVRMLGLSLBaseProgram.Create;
begin
  inherited;
  EventsObserved := TVRMLEventsList.Create;
end;

destructor TVRMLGLSLBaseProgram.Destroy;
var
  I: Integer;
begin
  if EventsObserved <> nil then
  begin
    for I := 0 to EventsObserved.Count - 1 do
      EventsObserved[I].OnReceive.Remove(@EventReceive);
    FreeAndNil(EventsObserved);
  end;
  inherited;
end;

procedure TVRMLGLSLBaseProgram.BindUniform(const FieldOrEvent: TVRMLInterfaceDeclaration);
var
  UniformField: TVRMLField;
  UniformEvent, ObservedEvent: TVRMLEvent;
begin
  UniformField := FieldOrEvent.Field;
  UniformEvent := FieldOrEvent.Event;

  { Set initial value for this GLSL uniform variable,
    from VRML field or exposedField }

  if UniformField <> nil then
  begin
    { Ok, we have a field with a value (interface declarations with
      fields inside ComposedShader / Effect always have a value).
      So set GLSL uniform variable from this field. }
    SetUniformFromField(UniformField.Name, UniformField, true);
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

procedure TVRMLGLSLBaseProgram.SetUniformFromField(
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
    SetUniform(UniformName, TSFBool(UniformValue).Value) else
  if UniformValue is TSFLong then
    { Handling of SFLong also takes care of SFInt32. }
    SetUniform(UniformName, TSFLong(UniformValue).Value) else
  if UniformValue is TSFVec2f then
    SetUniform(UniformName, TSFVec2f(UniformValue).Value) else
  { Check TSFColor first, otherwise TSFVec3f would also catch and handle
    TSFColor. And we don't want this: for GLSL, color is passed
    as vec4 (so says the spec, I guess that the reason is that for GLSL most
    input/output colors are vec4). }
  if UniformValue is TSFColor then
    SetUniform(UniformName, Vector4Single(TSFColor(UniformValue).Value, 1.0)) else
  if UniformValue is TSFVec3f then
    SetUniform(UniformName, TSFVec3f(UniformValue).Value) else
  if UniformValue is TSFVec4f then
    SetUniform(UniformName, TSFVec4f(UniformValue).Value) else
  if UniformValue is TSFRotation then
    SetUniform(UniformName, TSFRotation(UniformValue).Value) else
  if UniformValue is TSFMatrix3f then
    SetUniform(UniformName, TSFMatrix3f(UniformValue).Value) else
  if UniformValue is TSFMatrix4f then
    SetUniform(UniformName, TSFMatrix4f(UniformValue).Value) else
  if UniformValue is TSFFloat then
    SetUniform(UniformName, TSFFloat(UniformValue).Value) else
  if UniformValue is TSFDouble then
    { SFDouble also takes care of SFTime }
    SetUniform(UniformName, TSFDouble(UniformValue).Value) else

  { Double-precision vector and matrix types.

    Note that X3D spec specifies only mapping for SF/MFVec3d, 4d
    (not specifying any mapping for SF/MFVec2d, and all matrix types).
    And it specifies that they map to types float3, float4 ---
    which are not valid types in GLSL?

    So I simply ignore non-sensible specification, and take
    the reasonable approach: support all double-precision vectors and matrices,
    just like single-precision. }
  if UniformValue is TSFVec2d then
    SetUniform(UniformName, Vector2Single(TSFVec2d(UniformValue).Value)) else
  if UniformValue is TSFVec3d then
    SetUniform(UniformName, Vector3Single(TSFVec3d(UniformValue).Value)) else
  if UniformValue is TSFVec4d then
    SetUniform(UniformName, Vector4Single(TSFVec4d(UniformValue).Value)) else
  if UniformValue is TSFMatrix3d then
    SetUniform(UniformName, Matrix3Single(TSFMatrix3d(UniformValue).Value)) else
  if UniformValue is TSFMatrix4d then
    SetUniform(UniformName, Matrix4Single(TSFMatrix4d(UniformValue).Value)) else

  { Now repeat this for array types }
  if UniformValue is TMFBool then
    SetUniform(UniformName, TMFBool(UniformValue).Items) else
  if UniformValue is TMFLong then
    SetUniform(UniformName, TMFLong(UniformValue).Items) else
  if UniformValue is TMFVec2f then
    SetUniform(UniformName, TMFVec2f(UniformValue).Items) else
  if UniformValue is TMFColor then
  begin
    TempVec4f := TMFColor(UniformValue).Items.ToVector4Single(1.0);
    try
      SetUniform(UniformName, TempVec4f);
    finally FreeAndNil(TempVec4f) end;
  end else
  if UniformValue is TMFVec3f then
    SetUniform(UniformName, TMFVec3f(UniformValue).Items) else
  if UniformValue is TMFVec4f then
    SetUniform(UniformName, TMFVec4f(UniformValue).Items) else
  if UniformValue is TMFRotation then
    SetUniform(UniformName, TMFRotation(UniformValue).Items) else
  if UniformValue is TMFMatrix3f then
    SetUniform(UniformName, TMFMatrix3f(UniformValue).Items) else
  if UniformValue is TMFMatrix4f then
    SetUniform(UniformName, TMFMatrix4f(UniformValue).Items) else
  if UniformValue is TMFFloat then
    SetUniform(UniformName, TMFFloat(UniformValue).Items) else
  if UniformValue is TMFDouble then
  begin
    TempF := TMFDouble(UniformValue).Items.ToSingle;
    try
      SetUniform(UniformName, TempF);
    finally FreeAndNil(TempF) end;
  end else
  if UniformValue is TMFVec2d then
  begin
    TempVec2f := TMFVec2d(UniformValue).Items.ToVector2Single;
    try
      SetUniform(UniformName, TempVec2f);
    finally FreeAndNil(TempVec2f) end;
  end else
  if UniformValue is TMFVec3d then
  begin
    TempVec3f := TMFVec3d(UniformValue).Items.ToVector3Single;
    try
      SetUniform(UniformName, TempVec3f);
    finally FreeAndNil(TempVec3f) end;
  end else
  if UniformValue is TMFVec4d then
  begin
    TempVec4f := TMFVec4d(UniformValue).Items.ToVector4Single;
    try
      SetUniform(UniformName, TempVec4f);
    finally FreeAndNil(TempVec4f) end;
  end else
  if UniformValue is TMFMatrix3d then
  begin
    TempMat3f := TMFMatrix3d(UniformValue).Items.ToMatrix3Single;
    try
      SetUniform(UniformName, TempMat3f);
    finally FreeAndNil(TempMat3f) end;
  end else
  if UniformValue is TMFMatrix4d then
  begin
    TempMat4f := TMFMatrix4d(UniformValue).Items.ToMatrix4Single;
    try
      SetUniform(UniformName, TempMat4f);
    finally FreeAndNil(TempMat4f) end;
  end else
  if (UniformValue is TSFNode) or
     (UniformValue is TMFNode) then
  begin
    { Nothing to do, these will be set by TGLSLRenderer.Enable }
  end else
    { TODO: other field types, full list is in X3D spec in
      "OpenGL shading language (GLSL) binding".
      Remaining:
      SF/MFImage }
    VRMLWarning(vwSerious, 'Setting uniform GLSL variable from X3D field type "' + UniformValue.VRMLTypeName + '" not supported');

  if EnableDisable then
    { TODO: this should restore previously bound program }
    Disable;
end;

procedure TVRMLGLSLBaseProgram.EventReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
var
  UniformName: string;
  EventsEngine: TVRMLEventsEngine;
begin
  if Event.ParentExposedField = nil then
    UniformName := Event.Name else
    UniformName := Event.ParentExposedField.Name;

  SetUniformFromField(UniformName, Value, true);

  { Although ExposedEvents implementation already sends notification
    about changes to EventsEngine, we can also get here
    by eventIn invocation (which doesn't trigger
    EventsEngine.ChangedField, since it doesn't change a field...).
    So we should explicitly do VisibleChangeHere here, to make sure
    it gets called when uniform changed. }
  if Event.ParentNode <> nil then
  begin
    EventsEngine := (Event.ParentNode as TVRMLNode).EventsEngine;
    if EventsEngine <> nil then
      EventsEngine.VisibleChangeHere([vcVisibleGeometry, vcVisibleNonGeometry]);
  end;
end;

procedure TVRMLGLSLBaseProgram.BindUniforms(const Node: TVRMLNode);
var
  I: Integer;
begin
  Assert(Node.HasInterfaceDeclarations <> []);
  Assert(Node.InterfaceDeclarations <> nil);
  for I := 0 to Node.InterfaceDeclarations.Count - 1 do
    BindUniform(Node.InterfaceDeclarations[I]);
end;

{ TVRMLShader ---------------------------------------------------------------- }

constructor TVRMLShader.Create;
begin
  inherited;
  VertexShaderComplete := {$I template.vs.inc};
  FragmentShaderComplete := {$I template.fs.inc};
  LightShaders := TLightShaders.Create;
end;

destructor TVRMLShader.Destroy;
begin
  FreeAndNil(Uniforms);
  FreeAndNil(UniformsNodes);
  FreeAndNil(LightShaders);
  inherited;
end;

function TVRMLShader.PlugCustom(
  var Code: string; const PlugNameDeclareProcedures: string;
  const PlugName: string; const PlugValue: string;
  const RemovePlug, ForceDirectInsertion: boolean): boolean;

  { Derive procedure call and declaration code from plug parameter and PlugValue }
  procedure PlugProcedure(const Parameter: string;
    out ProcedureCall, ProcedureDeclaration: string);

    function MoveToOpeningParen(var P: Integer): boolean;
    begin
      Result := true;
      repeat
        Inc(P);

        if P > Length(Parameter) then
        begin
          VRMLWarning(vwIgnorable, 'PLUG parameter unexpected end (not closed parenthesis)');
          Exit(false);
        end;

        if (Parameter[P] <> '(') and
           not (Parameter[P] in WhiteSpaces) then
        begin
          VRMLWarning(vwIgnorable, Format('PLUG parameter unexpected character "%s" (expected opening parenthesis "(")',
            [Parameter[P]]));
          Exit(false);
        end;
      until Parameter[P] = '(';
    end;

    function MoveToMatchingParen(var P: Integer): boolean;
    var
      ParenLevel: Cardinal;
    begin
      Result := true;
      ParenLevel := 1;

      repeat
        Inc(P);
        if P > Length(Parameter) then
        begin
          VRMLWarning(vwIgnorable, 'PLUG parameter unexpected end (not closed parenthesis)');
          Exit(false);
        end;

        if Parameter[P] = '(' then
          Inc(ParenLevel) else
        if Parameter[P] = ')' then
          Dec(ParenLevel);
      until ParenLevel = 0;
    end;

  var
    ProcedureName: string;
    CallBegin, CallEnd, DeclarationBegin, DeclarationEnd: Integer;
  begin
    ProcedureName := 'plug_' + IntToStr(PlugIdentifiers);
    Inc(PlugIdentifiers);

    CallBegin := 0;
    if not MoveToOpeningParen(CallBegin) then Exit;
    CallEnd := CallBegin;
    if not MoveToMatchingParen(CallEnd) then Exit;

    DeclarationBegin := CallEnd;
    if not MoveToOpeningParen(DeclarationBegin) then Exit;
    DeclarationEnd := DeclarationBegin;
    if not MoveToMatchingParen(DeclarationEnd) then Exit;

    ProcedureCall := ProcedureName +
      CopyPos(Parameter, CallBegin, CallEnd) + ';' + NL;
    ProcedureDeclaration := 'void ' + ProcedureName +
      CopyPos(Parameter, DeclarationBegin, DeclarationEnd) + NL +
      '{' + NL + PlugValue + NL + '}' + NL;
  end;

  procedure InsertIntoCode(const P: Integer; const S: string);
  begin
    Code := Copy(Code, 0, P - 1) + S + SEnding(Code, P);
  end;

var
  PBegin, PEnd: Integer;
  Parameter, ProcedureCall, ProcedureDeclaration: string;
  CommentBegin: string;
begin
  Result := false;
  CommentBegin := '/* PLUG: ' + PlugName + ' ';
  PBegin := Pos(CommentBegin, Code);
  if PBegin <> 0 then
  begin
    PEnd := PosEx('*/', Code, PBegin + Length(CommentBegin));
    if PEnd <> 0 then
    begin
      Result := true;
      Parameter := Trim(CopyPos(Code, PBegin + Length(CommentBegin), PEnd - 1));

      if RemovePlug then
        DeletePos(Code, PBegin, PEnd + 1);

      if Trim(PlugValue) <> '' then
      begin
        if ForceDirectInsertion or (Parameter = 'declaration') then
          InsertIntoCode(PBegin, PlugValue + NL) else
        begin
          PlugProcedure(Parameter, ProcedureCall, ProcedureDeclaration);
          InsertIntoCode(PBegin,  ProcedureCall);
          { We use recursive Plug call, to insert procedure declaration
            at appropriate place. }
          Plug(PlugNameDeclareProcedures, ProcedureDeclaration, false, true);
        end;
      end;
    end;
  end;
end;

procedure TVRMLShader.Plug(const PlugName: string; const PlugValue: string;
  const RemovePlug, ForceDirectInsertion: boolean);
begin
  if not PlugCustom(VertexShaderComplete, 'vertex-declare-procedures',
    PlugName, PlugValue, RemovePlug, ForceDirectInsertion) then
  if not PlugCustom(FragmentShaderComplete, 'fragment-declare-procedures',
    PlugName, PlugValue, RemovePlug, ForceDirectInsertion) then
    VRMLWarning(vwIgnorable, Format('Plugging point "%s" for shader code not found',
      [PlugName]));
end;

procedure TVRMLShader.ApplyInternalEffects;
const
  PCFDefine: array [TPercentageCloserFiltering] of string =
  ( '', '#define PCF4', '#define PCF4_BILINEAR', '#define PCF16' );
var
  I: Integer;
begin
  Plug('vertex-process', TextureCoordInitialize + TextureCoordGen
    + TextureCoordMatrix + ClipPlane,
    false, true);

  Plug('texture-apply', TextureApply,
    false, true);
  Plug('fragment-declare-variables',
    FragmentShaderDeclare +
    PCFDefine[PercentageCloserFiltering],
    false, true);
  Plug('fragment-declare-procedures', {$I shadow_map_common.fs.inc},
    false, true);
  Plug('fragment-end', FragmentEnd,
    false, true);

  for I := 0 to LightShaders.Count - 1 do
    if LightShaders[I] <> nil then
    begin
      Plug('add-light-contribution-back' , StringReplace(LightShaders[I].Code,
        'gl_SideLightProduct', 'gl_BackLightProduct' , [rfReplaceAll]));
      Plug('add-light-contribution-front', StringReplace(LightShaders[I].Code,
        'gl_SideLightProduct', 'gl_FrontLightProduct', [rfReplaceAll]));
    end;
end;

function TVRMLShader.CreateProgram: TVRMLShaderProgram;
begin
  if Log then
  begin
    WritelnLogMultiline('Generated GLSL vertex shader', VertexShaderComplete);
    WritelnLogMultiline('Generated GLSL fragment shader', FragmentShaderComplete);
  end;

  Result := TVRMLShaderProgram.Create;
  try
    Result.AttachVertexShader(VertexShaderComplete);
    Result.AttachFragmentShader(FragmentShaderComplete);
    Result.Link(true);

    Result.UniformNotFoundAction := uaWarning;
    Result.UniformTypeMismatchAction := utWarning;

    Result.LightsEnabled := LightsEnabled;
    Result.PercentageCloserFiltering := PercentageCloserFiltering;
    Result.VisualizeDepthMap := VisualizeDepthMap;
  except Result.Free; raise end;
end;

function TVRMLShader.ProgramSettingsEqual(AProgram: TVRMLShaderProgram): boolean;
begin
  Result := (
    (AProgram.LightsEnabled = LightsEnabled) and
    (AProgram.PercentageCloserFiltering = PercentageCloserFiltering) and
    (AProgram.VisualizeDepthMap = VisualizeDepthMap)
  );
end;

procedure TVRMLShader.SetupUniforms(AProgram: TVRMLShaderProgram);
var
  I: Integer;
begin
  if Uniforms <> nil then
    for I := 0 to Uniforms.Count - 1 do
      case Uniforms[I].AType of
        utLongInt: AProgram.SetUniform(Uniforms[I].Name, Uniforms[I].Value.LongInt);
        utSingle : AProgram.SetUniform(Uniforms[I].Name, Uniforms[I].Value.Single );
        else raise EInternalError.Create('TVRMLShader.SetupUniforms:Uniforms[I].Type?');
      end;

  if UniformsNodes <> nil then
    for I := 0 to UniformsNodes.Count - 1 do
      AProgram.BindUniforms(UniformsNodes[I]);
end;

procedure TVRMLShader.AddUniform(Uniform: TUniform);
begin
  if Uniforms = nil then
    Uniforms := TUniformsList.Create;
  Uniforms.Add(Uniform);
end;

procedure TVRMLShader.EnableTexture(const TextureUnit: Cardinal;
  const TextureType: TTextureType; const ShadowMapSize: Cardinal;
  const ShadowLight: TNodeX3DLightNode);
const
  OpenGLTextureType: array [TTextureType] of string =
  ('sampler2D', 'sampler2DShadow', 'samplerCube', 'sampler3D');
var
  Uniform: TUniform;
  TextureSampleCall: string;
  ShadowLightShader: TLightShader;
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
    else raise EInternalError.Create('TextureEnableDisable?');
  end;

  { Enable for shader pipeline }

  Uniform := TUniform.Create;
  Uniform.Name := Format('texture_%d', [TextureUnit]);
  Uniform.AType := utLongInt;
  Uniform.Value.LongInt := TextureUnit;

  AddUniform(Uniform);

  TextureCoordInitialize += Format('gl_TexCoord[%d] = gl_MultiTexCoord%0:d;' + NL,
    [TextureUnit]);
  TextureCoordMatrix += Format('gl_TexCoord[%d] = gl_TextureMatrix[%0:d] * gl_TexCoord[%0:d];' + NL,
    [TextureUnit]);

  if (TextureType = tt2DShadow) and VisualizeDepthMap then
  begin
    { visualizing depth map requires a little different approach:
      - we use shadow_depth() instead of shadow() function
      - we *set* gl_FragColor, not modulate it, to ignore previous textures
      - we return after, to ignore following textures
      - the sampler is sampler2D, not sampler2DShadow }
    TextureSampleCall := 'vec4(vec3(shadow_depth(%s, %s)), gl_FragColor.a)';
    TextureApply += Format('gl_FragColor = ' + TextureSampleCall + ';' + NL +
      'return;',
      [Uniform.Name, 'gl_TexCoord[' + IntToStr(TextureUnit) + ']']);
    FragmentShaderDeclare += Format('uniform sampler2D %s;' + NL,
      [Uniform.Name]);
  end else
  begin
    if (TextureType = tt2DShadow) and
       (ShadowLight <> nil) and
       LightShaders.Find(ShadowLight, ShadowLightShader) then
    begin
      PlugCustom(ShadowLightShader.Code, 'fragment-declare-procedures',
        'light-scale', Format('scale *= shadow(%s, gl_TexCoord[%d], %d.0);',
        [Uniform.Name, TextureUnit, ShadowMapSize]),
        false, true);
    end else
    begin
      { TODO: always modulate mode for now }
      case TextureType of
        tt2D      : TextureSampleCall := 'texture2D(%s, %s.st)';
        tt2DShadow: TextureSampleCall := 'vec4(vec3(shadow(%s, %s, ' +IntToStr(ShadowMapSize) + '.0)), gl_FragColor.a)';
        ttCubeMap : TextureSampleCall := 'textureCube(%s, %s.xyz)';
        { For 3D textures, remember we may get 4D tex coords
          through TextureCoordinate4D, so we have to use texture3DProj }
        tt3D      : TextureSampleCall := 'texture3DProj(%s, %s)';
        else raise EInternalError.Create('TVRMLShader.EnableTexture:TextureType?');
      end;
      TextureApply += Format('gl_FragColor *= ' + TextureSampleCall + ';' + NL,
        [Uniform.Name, 'gl_TexCoord[' + IntToStr(TextureUnit) + ']']);
    end;
    FragmentShaderDeclare += Format('uniform %s %s;' + NL,
      [OpenGLTextureType[TextureType], Uniform.Name]);
  end;
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
          'vec3 r = reflect( normalize(vec3(vertex_eye)), normal_eye );' + NL +
	  'float m = 2.0 * sqrt( r.x*r.x + r.y*r.y + (r.z+1.0)*(r.z+1.0) );' + NL +
          '/* Using 1.0 / 2.0 instead of 0.5 to workaround fglrx bugs */' + NL +
	  'gl_TexCoord[%d].st = r.xy / m + vec2(1.0, 1.0) / 2.0;',
          [TextureUnit]);
      end;
    tgNormal:
      begin
        glEnable(GL_TEXTURE_GEN_S);
        glEnable(GL_TEXTURE_GEN_T);
        glEnable(GL_TEXTURE_GEN_R);
        TextureCoordGen += Format('gl_TexCoord[%d].xyz = normal_eye;' + NL,
          [TextureUnit]);
      end;
    tgReflection:
      begin
        glEnable(GL_TEXTURE_GEN_S);
        glEnable(GL_TEXTURE_GEN_T);
        glEnable(GL_TEXTURE_GEN_R);
        { Negate reflect result --- just like for kambi_vrml_test_suite/x3d/water_reflections/water_reflections_normalmap.fs }
        TextureCoordGen += Format('gl_TexCoord[%d].xyz = -reflect(-vec3(vertex_eye), normal_eye);' + NL,
          [TextureUnit]);
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
  PlaneName, Source: string;
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
    tgEye   : begin PlaneName := 'gl_EyePlane'   ; Source := 'vertex_eye'; end;
    tgObject: begin PlaneName := 'gl_ObjectPlane'; Source := 'gl_Vertex' ; end;
    else raise EInternalError.Create('TVRMLShader.EnableTexGen:Generation?');
  end;

  TextureCoordGen += Format('gl_TexCoord[%d].%s = dot(%s, %s%s[%0:d]);' + NL,
    [TextureUnit, VectorComponentNames[Component],
     Source, PlaneName, PlaneComponentNames[Component]]);
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
    ClipPlane := 'gl_ClipVertex = vertex_eye;';
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

procedure TVRMLShader.EnableBumpMapping(const NormalMapTextureUnit: Cardinal);
var
  Uniform: TUniform;
begin
  Plug('vertex-declare-variables',
    'attribute mat3 tangent_to_object_space;' +NL+
    'varying mat3 tangent_to_eye_space;');

  Plug('vertex-process',
    'tangent_to_eye_space = gl_NormalMatrix * tangent_to_object_space;');

  Plug('fragment-declare-variables',
    'varying mat3 tangent_to_eye_space;' +NL+
    'uniform sampler2D tex_normal_map;');

  Plug('fragment-normal-eye',
    '/* Read normal from the texture, this is the very idea of bump mapping.' +NL+
    '   Unpack normals, they are in texture in [0..1] range and I want in [-1..1].' +NL+
    '   Our normal map is always indexed using gl_TexCoord[0] (this way' +NL+
    '   we depend on already correct gl_TexCoord[0], multiplied by TextureTransform' +NL+
    '   and such). */' +NL+
    'normal_eye_fragment = normalize(tangent_to_eye_space * (' +NL+
    '  texture2D(tex_normal_map, gl_TexCoord[0].st).xyz * 2.0 - vec3(1.0)));');

  Uniform := TUniform.Create;
  Uniform.Name := 'tex_normal_map';
  Uniform.AType := utLongInt;
  Uniform.Value.LongInt := NormalMapTextureUnit;

  AddUniform(Uniform);
end;

procedure TVRMLShader.EnableLight(const Number: Cardinal; Node: TNodeX3DLightNode);
var
  LightShader: TLightShader;
  Defines: string;
begin
  Defines := '';
  if Node <> nil then
  begin
    Defines += '#define LIGHT_TYPE_KNOWN' + NL;
    if Node is TVRMLPositionalLightNode then
    begin
      Defines += '#define LIGHT_TYPE_POSITIONAL' + NL;
      if (Node is TNodeSpotLight_1) or
         (Node is TNodeSpotLight_2) then
        Defines  += '#define LIGHT_TYPE_SPOT' + NL;
    end;
  end;

  LightShader := TLightShader.Create;
  LightShader.Code := Defines + {$I template_add_light.glsl.inc};
  StringReplaceAllTo1st(LightShader.Code, 'light_number', IntToStr(Number), false);
  LightShader.Node := Node;

  if Number >= LightShaders.Count then
    LightShaders.Count := Number + 1;
  LightShaders[Number] := LightShader;

  Inc(LightsEnabled);
end;

procedure TVRMLShader.EnableEffects(Effects: TMFNode);

  procedure EnableEffect(Effect: TNodeEffect);

    procedure EnableEffectPart(Part: TNodeEffectPart);
    var
      Contents: string;
    begin
      Contents := Part.LoadContents;
      if Contents <> '' then
        Plug(Part.FdName.Value, Contents);
    end;

  var
    I: Integer;
  begin
    if Effect.FdLanguage.Value <> 'GLSL' then
      VRMLWarning(vwIgnorable, Format('Unknown shading language "%s" for Effect node',
        [Effect.FdLanguage.Value]));

    for I := 0 to Effect.FdParts.Count - 1 do
      if Effect.FdParts[I] is TNodeEffectPart then
        EnableEffectPart(TNodeEffectPart(Effect.FdParts[I]));

    if UniformsNodes = nil then
      UniformsNodes := TVRMLNodesList.Create;
    UniformsNodes.Add(Effect);
  end;

var
  I: Integer;
begin
  for I := 0 to Effects.Count - 1 do
    if Effects[I] is TNodeEffect then
      EnableEffect(TNodeEffect(Effects[I]));
end;

end.
