{
  Copyright 2007-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ OpenGL shaders (GLSL through the TGLSLProgram,
  ARB assembly through TARBProgram descendants).

  Some common notes for all classes defined here:

  @unorderedList(
    @item(
      Creating/destroying the class instance immediately creates/destroys
      appropriate program. So be sure to create/destroy these classes only
      when you have OpenGL context available (for example, create in TCastleWindowBase.OnInit
      and destroy in TCastleWindowBase.OnClose).)

    @item(
      Upon creation, these classes check current OpenGL context abilities.
      Currently three support levels are possible for all programs:
      no support at all (old OpenGL), support through ARB extensions,
      or support built-in (newer OpenGL versions, >= 2.0).

      All three cases are automatically handled inside, so usually you
      do not have to care about these details. Note that "none" support
      (on older OpenGL version with no appropriate ARB extensions)
      means that shaders are not really initialized at all.))
}
unit GLShaders;

interface

uses SysUtils, Classes, GL, GLU, GLExt, CastleGLUtils, CastleUtils, CastleVectors,
  FGL, Shaders;

type
  { Abstract class for both ARB vertex and fragment programs. }
  TARBProgram = class
  private
    Target: TGLenum;
    FSupport: TGLSupport;
    ProgId: TGLuint;
  public
    destructor Destroy; override;

    { Bind and load program contents. }
    procedure Load(const S: string);

    { Bind and enable this program. }
    procedure Enable;

    { Bind and disable this program. }
    procedure Disable;

    property Support: TGLSupport read FSupport;

    { Returns multiline debug info about current program.
      Things like how it's supported (not at all ? ARB extension ?
      standard ?), how much of GL limit is used (number of instructions,
      temporaries etc.), whether program fits within native limits etc. }
    function DebugInfo: string; virtual;
  end;

  { Easily handle ARB vertex program. }
  TARBVertexProgram = class(TARBProgram)
  public
    constructor Create;
    function DebugInfo: string; override;

    { @abstract(What support do we get from current OpenGL context ?)
      This is much like @link(Support), but it's a class function. }
    class function ClassSupport: TGLSupport;
  end;

  { Easily handle ARB fragment program. }
  TARBFragmentProgram = class(TARBProgram)
  public
    constructor Create;
    function DebugInfo: string; override;

    { @abstract(What support do we get from current OpenGL context ?)
      This is much like @link(Support), but it's a class function. }
    class function ClassSupport: TGLSupport;
  end;

  { Common class for exceptions related to GLSL programs. }
  EGLSLError = class(Exception);

  EGLSLShaderCompileError = class(EGLSLError);
  EGLSLProgramLinkError = class(EGLSLError);
  EGLSLRunningInSoftware = class(EGLSLError);
  EGLSLAttributeNotFound = class(EGLSLError);

  EGLSLUniformInvalid = class(EGLSLError);
  EGLSLUniformNotFound = class(EGLSLUniformInvalid);
  EGLSLUniformTypeMismatch = class(EGLSLUniformInvalid);

  TGLuintList = TCardinalList;

  { What to do when GLSL uniform variable is set (TGLSLProgram.SetUniform)
    but doesn't exist in the shader. }
  TUniformNotFoundAction = (
    { Report that uniform variable not found to OnWarning. }
    uaWarning,
    { Report that uniform variable not found by raising EGLSLUniformNotFound. }
    uaException,
    { Ignore the fact that uniform variable doesn't exist in the GLSL shader.
      Do not warn anywhere. }
    uaIgnore);

  { What to do when GLSL uniform variable is set (by TGLSLProgram.SetUniform)
    to the type that doesn't match type declared in GLSL shader. }
  TUniformTypeMismatchAction = (
    { Do not catch uniform type mismatch, leaving it to OpenGL.
      This will cause OpenGL error "invalid operation" (possibly resulting
      in an exception in some later code that checks OpenGL errors).

      This is unsafe (you may get OpenGL errors later), but is also fastest.
      Other options have to detect invalid types, which means
      checking the OpenGL error state each time you set uniform value. }
    utGLError,
    { Report type mismatch to OnWarning. }
    utWarning,
    { Report type mismatch by raising EGLSLUniformTypeMismatch. }
    utException);

  { Easily handle program in GLSL (OpenGL Shading Language). }
  TGLSLProgram = class
  private
    FSupport: TGLSupport;

    { Actually, this should be TGLhandleARB for gsExtension version.
      But TGLhandleARB = TGLuint in practice, so this is not a problem. }
    ProgramId: TGLuint;
    ShaderIds: TGLuintList;

    FUniformNotFoundAction: TUniformNotFoundAction;
    FUniformTypeMismatchAction: TUniformTypeMismatchAction;
    procedure UniformNotFound(const Name: string; const ForceException: boolean);
    procedure SetUniformEnd(const UniformName: string; const ForceException: boolean);

    { Wrapper over glGetAttribLocationARB (use only if gsExtension) }
    function GetAttribLocationARB(const Name: string): TGLint;

    { Wrapper over glGetAttribLocation (use only if gsStandard) }
    function GetAttribLocation(const Name: string): TGLint;
  public
    constructor Create;
    destructor Destroy; override;

    property Support: TGLSupport read FSupport;

    { Create shader from given string, compile it and attach to current program.

      You can attach more than one  shader of given type, just
      make sure that only one main() function is among each type
      (otherwise link error will be raised later).

      If you want to explicitly get rid of old shaders, use DetachAllShaders.

      @raises(EGLSLShaderCompileError If the shader source code cannot be compiled,
        exception message contains precise description from OpenGL where
        the error is.)

      @groupBegin }
    procedure AttachVertexShader(const S: string);
    procedure AttachGeometryShader(const S: string);
    procedure AttachFragmentShader(const S: string);
    procedure AttachShader(const ShaderType: TShaderType; const S: string);
    { @groupEnd }

    procedure DetachAllShaders;

    { Link the program, this should be done after attaching all shaders
      and before actually using the program.

      @raises(EGLSLProgramLinkError If the program cannot be linked,
        exception message contains precise description from OpenGL where
        the error is.)

      @raises(EGLSLRunningInSoftware If the program will be linked
        successfully, but RequireRunningInHardware = @true and the program
        will be detected to be running in software.) }
    procedure Link(RequireRunningInHardware: boolean);

    { Enable (use) this program. Shortcut for @code(CurrentProgram := Self). }
    procedure Enable;

    { Disable this program (use the fixed function pipeline).
      Shortcut for @code(CurrentProgram := nil). }
    class procedure Disable;

    { Override this to set uniform values, in particular to
      bind the textures used by this shader, right after each @link(Enable)
      call.

      This is automatically called after every @link(Enable) by VRML renderer
      (when it renders shapes) or scene manager (when it renders screen effects).
      If you use this TGLSLProgram directly (if you call @link(Enable)
      yourself), then it's your responsibility to call this method
      explicitly, if you want shaders using it to work.

      You can set any uniform values, and generally do
      anything you want to be done each time this shader is enabled.
      In particular, you can bind textures and set corresponding uniform
      variables of them. Increase BoundTextureUnits appropriately.

      Returns @false is some texture couldn't be bound. }
    function SetupUniforms(var BoundTextureUnits: Cardinal): boolean; virtual;

    { Returns multiline debug info about current program.
      Things like how it's supported (not at all ? ARB extension ?
      standard ?), names of active uniform and attribute variables etc.

      fglrx (Radeon closed-source OpenGL drivers) are buggy (that's not
      news, I know...) and they report GL_INVALID_ENUM on some queries
      here. We detect this, and still produce nice debug message, without
      raising any exception (after all, we have to workaround fglrx bugs,
      and try to run our program anyway...). What's important for caller
      is that we have to check first whether there are any OpenGL errors
      pending, and raise exceptions on them.

      @raises EOpenGLError If any OpenGL error will be detected. }
    function DebugInfo: string;

    { This is program info log, given to you from OpenGL after the program
      is linked. }
    function ProgramInfoLog: string;

    { After the program is linked, you can check this.

      Note that this is necessarily implemented in quite hacky way (by
      looking at ProgramInfoLog), there is no way currently (AFAIK ?)
      to get this information cleanly from OpenGL.
      In case of doubts, we try to "trust" OpenGL to execute shader in hardware.
      Return @false only when we see clear indication in ProgramInfoLog that
      it'll run in software. }
    function RunningInHardware: boolean;

    { @abstract(What support do we get from current OpenGL context ?)
      This is much like @link(Support), but it's a class function. }
    class function ClassSupport: TGLSupport;

    { What to do when GLSL uniform variable is set (SetUniform)
      but doesn't exist in the shader.
      Note that OpenGL aggresively removes
      unused code and variables from the shader when compiling/linking,
      so this also happens for "declared but detected to not used" variables.

      @seealso TUniformNotFoundAction }
    property UniformNotFoundAction: TUniformNotFoundAction
      read FUniformNotFoundAction write FUniformNotFoundAction
      default uaException;

    { What to do when GLSL uniform variable is set (SetUniform)
      but is declared with an incompatible type in the shader source.
      @seealso TUniformTypeMismatchAction }
    property UniformTypeMismatchAction: TUniformTypeMismatchAction
      read FUniformTypeMismatchAction write FUniformTypeMismatchAction
      default utGLError;

    { Set appropriate uniform variable value.
      The used type must match the type of this variable in GLSL program.

      OpenGL forces some constraints on using this:

      @unorderedList(
        @item(This should be used only after linking, and re-linking clears
          (sets to zero) all uniform variables values.)

        @item(This can be used only when the program is currently used.
          So call @link(Enable) before doing SetUniform calls.

          This is required by OpenGL glUniform*
          commands. glGetUniformLocation take ProgramId as parameter, so they
          can operate on any program. But glUniform* operate only on
          active program.)

        @item(Only @italic(active) uniforms variables may be set.
          @italic(Active) means, quoting OpenGL manpages,
          a variable that is determined during the link operation
          that it may be accessed during program execution.
          In other words, when linking GLSL program, unused variables
          may be eliminated, and you cannot set them by SetUniform.

          Call DebugInfo to see what uniform variables are considered
          active for your shader.)
      )

      @raises(EGLSLUniformNotFound If the variable is not found within
        the program and UniformNotFoundAction = uaException (default)
        or ForceException.)

      @raises(EGLSLUniformTypeMismatch If the variable type doesn't
        match the type declared in shader code. Raised only if
        UniformTypeMismatchAction = utException or ForceException.

        Note that both EGLSLUniformNotFound and EGLSLUniformTypeMismatch
        may be comfortably catched by an umbrella class EGLSLUniformInvalid.)

      @groupBegin }
    procedure SetUniform(const Name: string; const Value: boolean        ; const ForceException: boolean = false);
    procedure SetUniform(const Name: string; const Value: TGLint         ; const ForceException: boolean = false);
    procedure SetUniform(const Name: string; const Value: TVector2Integer; const ForceException: boolean = false);
    procedure SetUniform(const Name: string; const Value: TVector3Integer; const ForceException: boolean = false);
    procedure SetUniform(const Name: string; const Value: TVector4Integer; const ForceException: boolean = false);
    procedure SetUniform(const Name: string; const Value: TGLfloat       ; const ForceException: boolean = false);
    procedure SetUniform(const Name: string; const Value: TVector2Single ; const ForceException: boolean = false);
    procedure SetUniform(const Name: string; const Value: TVector3Single ; const ForceException: boolean = false);
    procedure SetUniform(const Name: string; const Value: TVector4Single ; const ForceException: boolean = false);
    procedure SetUniform(const Name: string; const Value: TMatrix2Single ; const ForceException: boolean = false);
    procedure SetUniform(const Name: string; const Value: TMatrix3Single ; const ForceException: boolean = false);
    procedure SetUniform(const Name: string; const Value: TMatrix4Single ; const ForceException: boolean = false);

    procedure SetUniform(const Name: string; const Value: TBooleanList      ; const ForceException: boolean = false);
    procedure SetUniform(const Name: string; const Value: TLongIntList      ; const ForceException: boolean = false);
    procedure SetUniform(const Name: string; const Value: TSingleList       ; const ForceException: boolean = false);
    procedure SetUniform(const Name: string; const Value: TVector2SingleList; const ForceException: boolean = false);
    procedure SetUniform(const Name: string; const Value: TVector3SingleList; const ForceException: boolean = false);
    procedure SetUniform(const Name: string; const Value: TVector4SingleList; const ForceException: boolean = false);
    procedure SetUniform(const Name: string; const Value: TMatrix3SingleList; const ForceException: boolean = false);
    procedure SetUniform(const Name: string; const Value: TMatrix4SingleList; const ForceException: boolean = false);
    { @groupEnd }

    { Load and enable vertex attribute data.
      This calls glVertexAttribPointer and enables it by
      glEnableVertexAttribArray (or ARB extension equivalents),
      see OpenGL reference for details.

      The attribute name is automatically resolved to a "location".
      We add LocationOffset (useful if you want to load matrix attributes,
      when you have to load matrix columns separately, with
      LocationOffset = column index).

      @raises(EGLSLAttributeNotFound If the variable is not found within
        the program.)

      @returns(Attribute location (with LocationOffset already applied).
        You can use it with DisableVertexAttribArray.) }
    function VertexAttribPointer(const Name: string; LocationOffset: TGLint;
      Size: TGLint; AType: TGLenum; Normalized: TGLboolean; Stride: TGLsizei;
      Ptr: Pointer): TGLint;

    class procedure DisableVertexAttribArray(Location: TGLint);

    { Set attribute variable value.
      The used type must match the type of this variable in GLSL program.

      OpenGL forces some constraints on using this, see SetUniform.
      In short: use this only after linking and using the program.
      And note that attributes declared but not actually used in shader code
      may be eliminated, use DebugInfo to see which attributes are actually
      used (@italic(active) in OpenGL terminology).

      These should not be often useful. Usually, you should rather load
      attribute arrays, by VertexAttribPointer.

      @raises(EGLSLAttributeNotFound If the variable is not found within
        the program.

        Note that this is only one of the many things that can
        go wrong. And on most cases we don't raise any error,
        instead OpenGL sets it's error state and you probably want to
        call CheckGLErrors from time to time to catch them.)

      @groupBegin }
    procedure SetAttribute(const Name: string; const Value: TVector4Integer);
    procedure SetAttribute(const Name: string; const Value: TVector4Byte);
    procedure SetAttribute(const Name: string; const Value: TGLfloat);
    procedure SetAttribute(const Name: string; const Value: TVector2Single);
    procedure SetAttribute(const Name: string; const Value: TVector3Single);
    procedure SetAttribute(const Name: string; const Value: TVector4Single);
    procedure SetAttribute(const Name: string; const Value: TGLdouble);
    procedure SetAttribute(const Name: string; const Value: TVector2Double);
    procedure SetAttribute(const Name: string; const Value: TVector3Double);
    procedure SetAttribute(const Name: string; const Value: TVector4Double);
    procedure SetAttribute(const Name: string; const Value: TMatrix3Single);
    procedure SetAttribute(const Name: string; const Value: TMatrix4Single);
    { @groupEnd }
  end;

  TGLSLProgramList = specialize TFPGObjectList<TGLSLProgram>;

var
  LogShaders: boolean;

function GetCurrentProgram: TGLSLProgram;
procedure SetCurrentProgram(const Value: TGLSLProgram);

{ Currently enabled GLSL program.
  @nil if fixed-function pipeline should be used.
  Setting this property encapsulates the OpenGL glUseProgram
  (or equivalent ARB extension), additionally preventing redundant glUseProgram
  calls. }
property CurrentProgram: TGLSLProgram
  read GetCurrentProgram write SetCurrentProgram;

implementation

uses CastleStringUtils, CastleWarnings, CastleLog, GLVersionUnit;

{ Comfortable shortcut for glGetProgramivARB that always returns 1 value. }
function glGetProgramiARB(target: TGLenum; pname: TGLenum): TGLint;
begin
  glGetProgramivARB(target, pname, @Result);
end;

{ Wrapper around glGetShaderInfoLog.
  Based on Dean Ellis BasicShader.dpr, but somewhat fixed ? <> 0 not > 1. }
function GetShaderInfoLog(ShaderId: TGLuint): String;
var
  Len, Len2: TGLint;
begin
  glGetShaderiv(ShaderId, GL_INFO_LOG_LENGTH, @Len);

  if Len <> 0 then
  begin
    SetLength(Result, Len);
    glGetShaderInfoLog(ShaderId, Len, @Len2, PChar(Result));
    StringReplaceAllTo1st(Result, #0, NL);
  end else
    Result := '';
end;

{ Wrapper around glGetProgramInfoLog. }
function GetProgramInfoLog(ProgramId: TGLuint): String;
var
  Len, Len2: TGLint;
begin
  glGetProgramiv(ProgramId, GL_INFO_LOG_LENGTH, @Len);

  if Len <> 0 then
  begin
    SetLength(Result, Len);
    glGetProgramInfoLog(ProgramId, Len, @Len2, PChar(Result));
    StringReplaceAllTo1st(Result, #0, NL);
  end else
    Result := '';
end;

{ Wrapper around glGetInfoLogARB (this is for both shaders and programs). }
function GetInfoLogARB(ObjectId: TGLuint): String;
var
  Len, Len2: TGLint;
begin
  glGetObjectParameterivARB(ObjectId, GL_OBJECT_INFO_LOG_LENGTH_ARB, @Len);

  if Len <> 0 then
  begin
    SetLength(Result, Len);
    glGetInfoLogARB(ObjectId, Len, @Len2, PChar(Result));
    StringReplaceAllTo1st(Result, #0, NL);
  end else
    Result := '';
end;

var
  FCurrentProgram: TGLSLProgram;

function GetCurrentProgram: TGLSLProgram;
begin
  Result := FCurrentProgram;
end;

procedure SetCurrentProgram(const Value: TGLSLProgram);
begin
  if FCurrentProgram <> Value then
  begin
    FCurrentProgram := Value;

    if Value <> nil then
    begin
      case TGLSLProgram.ClassSupport of
        gsExtension: glUseProgramObjectARB(Value.ProgramId);
        gsStandard : glUseProgram         (Value.ProgramId);
      end;
    end else
    begin
      case TGLSLProgram.ClassSupport of
        gsExtension:
          begin
            glUseProgramObjectARB(0);
            { Workaround for fglrx bug (Radeon X1600 (chantal)).
              Reproduce: open demo_models/x3d/anchor_test.x3dv,
              and switch in view3dscene "Shaders -> Enable For Everything".
              Text should be still rendered without shaders in this case
              (we cannot currently render text through shaders).
              Without the hack below, the shader from sphere would remain
              active and text would look black. }
            if GLVersion.Fglrx then glUseProgramObjectARB(0);
          end;
        gsStandard    : glUseProgram         (0);
      end;
  end;
end;

end;

{ TARBProgram ---------------------------------------------------------------- }

destructor TARBProgram.Destroy;
begin
  glDeleteProgramsARB(1, @ProgId);
  inherited;
end;

procedure TARBProgram.Load(const S: string);
begin
  case Support of
    gsExtension:
      begin
        glBindProgramARB(Target, ProgId);
        glProgramStringARB(Target,
          GL_PROGRAM_FORMAT_ASCII_ARB, Length(S), PCharOrNil(S));
      end;
    gsStandard: { TODO };
  end;
end;

function TARBProgram.DebugInfo: string;
begin
  Result := 'support: ' + GLSupportNames[Support];

  case Support of
    gsExtension:
      begin
        Result += NL + Format(
          'Number of instructions: %d / %d' +NL+
          'Number of temporaries: %d / %d' +NL+
          'Number of program parameter bindings: %d / %d',
          [ glGetProgramiARB(Target, GL_PROGRAM_INSTRUCTIONS_ARB),
            glGetProgramiARB(Target, GL_MAX_PROGRAM_INSTRUCTIONS_ARB),
            glGetProgramiARB(Target, GL_PROGRAM_TEMPORARIES_ARB),
            glGetProgramiARB(Target, GL_MAX_PROGRAM_TEMPORARIES_ARB),
            glGetProgramiARB(Target, GL_PROGRAM_PARAMETERS_ARB),
            glGetProgramiARB(Target, GL_MAX_PROGRAM_PARAMETERS_ARB) ]);

        if glGetProgramiARB(Target, GL_PROGRAM_UNDER_NATIVE_LIMITS_ARB) <> 0 then
          Result += NL + 'Program fits within native limits, OK.' else
          Result += NL + 'Program doesn''t fit within native limits, performance/quality will suffer!';
      end;
    gsStandard: { TODO };
  end;
end;

procedure TARBProgram.Enable;
begin
  case Support of
    gsExtension:
      begin
        glBindProgramARB(Target, ProgId);
        glEnable(Target);
      end;
    gsStandard: { TODO };
  end;
end;

procedure TARBProgram.Disable;
begin
  case Support of
    gsExtension:
      begin
        glBindProgramARB(Target, ProgId);
        glDisable(Target);
      end;
    gsStandard: { TODO };
  end;
end;

{ TARBVertexProgram ---------------------------------------------------------- }

constructor TARBVertexProgram.Create;
begin
  inherited;

  Target := GL_VERTEX_PROGRAM_ARB;

  FSupport := ClassSupport;

  case Support of
    gsExtension: glGenProgramsARB(1, @ProgId);
    gsStandard: { TODO };
  end;
end;

function TARBVertexProgram.DebugInfo: string;
begin
  Result := 'Vertex program ' + inherited;
end;

class function TARBVertexProgram.ClassSupport: TGLSupport;
begin
  { TODO: gsStandard not implemented now }

  { if GL_version_2_0 then
    Result := gsStandard else }
  if GL_ARB_vertex_program then
    Result := gsExtension else
    Result := gsNone;
end;

{ TARBFragmentProgram ---------------------------------------------------------- }

constructor TARBFragmentProgram.Create;
begin
  inherited;

  Target := GL_FRAGMENT_PROGRAM_ARB;

  FSupport := ClassSupport;

  case Support of
    gsExtension: glGenProgramsARB(1, @ProgId);
    gsStandard: { TODO };
  end;
end;

function TARBFragmentProgram.DebugInfo: string;
begin
  Result := 'Fragment program ' + inherited;
end;

class function TARBFragmentProgram.ClassSupport: TGLSupport;
begin
  { TODO: gsStandard not implemented now }

  { if GL_version_2_0 then
    Result := gsStandard else }
  if GL_ARB_fragment_program then
    Result := gsExtension else
    Result := gsNone;
end;

{ TGLSLProgram --------------------------------------------------------------- }

constructor TGLSLProgram.Create;
begin
  inherited;

  FSupport := ClassSupport;

  case Support of
    gsExtension: ProgramId := glCreateProgramObjectARB();
    gsStandard : ProgramId := glCreateProgram         ();
  end;

  { ProgramId = 0 means that an error occurred. Citing GL documentation:

    gsExtension: ARB_shader_objects spec says about
      CreateProgramObjectARB(void): "If the program object
      is created successfully, a handle that can be used to reference it is
      returned .... If the creation failed the handle returned will be 0."

    gsStandard: glCreateProgram docs say
      "This function returns 0 if an error occurs creating the program object."

    Looking at glGetError, I don't see any error code there.
    So I guess that this is just like glGenLists: when zero is returned,
    I have to raise error that simply creating GLSL program failed.
  }

  if ProgramId = 0 then
    raise EGLSLError.Create('Cannot create GLSL shader program');

  ShaderIds := TGLuintList.Create;

  FUniformNotFoundAction := uaException;
  FUniformTypeMismatchAction := utGLError;
end;

destructor TGLSLProgram.Destroy;
begin
  { make sure all shaders are detached and deleted, to free all resources }

  { Destructor may be called if exception raised from constructor,
    so better check that ShaderIds was created. }
  if ShaderIds <> nil then
    DetachAllShaders;

  case Support of
    gsExtension: glDeleteObjectARB(ProgramId);
    gsStandard : glDeleteProgram  (ProgramId);
  end;

  FreeAndNil(ShaderIds);

  inherited;
end;

class function TGLSLProgram.ClassSupport: TGLSupport;
begin
  if GL_version_2_0 then
    Result := gsStandard else
  if GLUseARBGLSL then
    Result := gsExtension else
    Result := gsNone;
end;

function TGLSLProgram.ProgramInfoLog: string;
begin
  case Support of
    gsExtension: Result := GetInfoLogARB(ProgramId);
    gsStandard : Result := GetProgramInfoLog(ProgramId);
    else Result := '';
  end;
end;

function TGLSLProgram.DebugInfo: string;

  function GLShaderVariableTypeName(AType: TGLenum): string;
  const
    { Present in glext since GL_VERSION_2_1, define here to support
      older FPC versions. }
    GL_FLOAT_MAT2x3 = $8B65;
    GL_FLOAT_MAT2x4 = $8B66;
    GL_FLOAT_MAT3x2 = $8B67;
    GL_FLOAT_MAT3x4 = $8B68;
    GL_FLOAT_MAT4x2 = $8B69;
    GL_FLOAT_MAT4x3 = $8B6A;
    { Present in glext since GL_VERSION_3_1, define here to support
      older FPC versions. }
    GL_SAMPLER_2D_RECT = $8B63;
    GL_SAMPLER_2D_RECT_SHADOW = $8B64;
    GL_INT_SAMPLER_2D_RECT = $8DCD;
    GL_UNSIGNED_INT_SAMPLER_2D_RECT = $8DD5;
  begin
    case AType of
      GL_FLOAT: Result := 'FLOAT';
      GL_FLOAT_VEC2: Result := 'FLOAT_VEC2';
      GL_FLOAT_VEC3: Result := 'FLOAT_VEC3';
      GL_FLOAT_VEC4: Result := 'FLOAT_VEC4';
      GL_INT: Result := 'INT';
      GL_INT_VEC2: Result := 'INT_VEC2';
      GL_INT_VEC3: Result := 'INT_VEC3';
      GL_INT_VEC4: Result := 'INT_VEC4';
      GL_BOOL: Result := 'BOOL';
      GL_BOOL_VEC2: Result := 'BOOL_VEC2';
      GL_BOOL_VEC3: Result := 'BOOL_VEC3';
      GL_BOOL_VEC4: Result := 'BOOL_VEC4';
      GL_FLOAT_MAT2: Result := 'FLOAT_MAT2';
      GL_FLOAT_MAT3: Result := 'FLOAT_MAT3';
      GL_FLOAT_MAT4: Result := 'FLOAT_MAT4';
      GL_FLOAT_MAT2x3: Result := 'FLOAT_MAT2x3';
      GL_FLOAT_MAT2x4: Result := 'FLOAT_MAT2x4';
      GL_FLOAT_MAT3x2: Result := 'FLOAT_MAT3x2';
      GL_FLOAT_MAT3x4: Result := 'FLOAT_MAT3x4';
      GL_FLOAT_MAT4x2: Result := 'FLOAT_MAT4x2';
      GL_FLOAT_MAT4x3: Result := 'FLOAT_MAT4x3';
      GL_SAMPLER_1D: Result := 'SAMPLER_1D';
      GL_SAMPLER_2D: Result := 'SAMPLER_2D';
      GL_SAMPLER_3D: Result := 'SAMPLER_3D';
      GL_SAMPLER_CUBE: Result := 'SAMPLER_CUBE';
      GL_SAMPLER_1D_SHADOW: Result := 'SAMPLER_1D_SHADOW';
      GL_SAMPLER_2D_SHADOW: Result := 'SAMPLER_2D_SHADOW';
      GL_SAMPLER_2D_RECT: Result := 'SAMPLER_2D_RECT';
      GL_SAMPLER_2D_RECT_SHADOW: Result := 'SAMPLER_2D_RECT_SHADOW';
      GL_INT_SAMPLER_2D_RECT: Result := 'INT_SAMPLER_2D_RECT';
      GL_UNSIGNED_INT_SAMPLER_2D_RECT: Result := 'UNSIGNED_INT_SAMPLER_2D_RECT';
      else Result := Format('Unrecognized uniform type "%d"', [AType]);
    end;
  end;

  const
    SCannotGetCount = '  Cannot get variables count (probably buggy fglrx ('+
      'Radeon closed-source drivers) or Mesa)';

  { Fills the UniformNames with the names (and properties)
    of all active uniform variables available by the program.
    The program must be previously linked to use this.

    Quoting OpenGL manpage about what is @italic(active) uniform variable:
    A uniform variable (either built-in or user-defined) is
    considered active if it is determined during the link operation
    that it may be accessed during program execution.

    You may want to clear UniformNames before calling this, as this doesn't
    clear them automatically, it only appends new names.

    GetActiveUniforms was planned once to be public, but actually
    I think it fits better in DebugInfo }
  procedure GetActiveUniforms(UniformNames: TStringList);
  var
    I: Integer;
    UniformsCount, UniformMaxLength: TGLuint;
    ReturnedLength: TGLsizei;
    Size: TGLint;
    AType: TGLEnum;
    Name: string;
    ErrorCode: TGLenum;
  begin
    case Support of
      gsExtension:
        begin
          glGetProgramivARB(ProgramId, GL_OBJECT_ACTIVE_UNIFORMS_ARB, @UniformsCount);

          ErrorCode := glGetError();
          if ErrorCode = GL_INVALID_ENUM then
          begin
            UniformNames.Append(SCannotGetCount);
            Exit;
          end else
          if ErrorCode <> GL_NO_ERROR then
            raise EOpenGLError.Create(ErrorCode, 'GetActiveUniforms');

          glGetProgramivARB(ProgramId, GL_OBJECT_ACTIVE_UNIFORM_MAX_LENGTH_ARB, @UniformMaxLength);

          for I := 0 to UniformsCount - 1 do
          begin
            SetLength(Name, UniformMaxLength);
            glGetActiveUniformARB(ProgramId, I, UniformMaxLength, @ReturnedLength,
              @Size, @AType, PCharOrNil(Name));

            SetLength(Name, ReturnedLength);
            UniformNames.Append(Format('  Name: %s, type: %s, size: %d',
              [Name, GLShaderVariableTypeName(AType), Size]));
          end;
        end;

      gsStandard    :
        begin
          glGetProgramiv(ProgramId, GL_ACTIVE_UNIFORMS, @UniformsCount);

          ErrorCode := glGetError();
          if ErrorCode = GL_INVALID_ENUM then
          begin
            UniformNames.Append(SCannotGetCount);
            Exit;
          end else
          if ErrorCode <> GL_NO_ERROR then
            raise EOpenGLError.Create(ErrorCode, 'GetActiveUniforms');

          glGetProgramiv(ProgramId, GL_ACTIVE_UNIFORM_MAX_LENGTH, @UniformMaxLength);

          for I := 0 to UniformsCount - 1 do
          begin
            SetLength(Name, UniformMaxLength);
            glGetActiveUniform(ProgramId, I, UniformMaxLength, @ReturnedLength,
              @Size, @AType, PCharOrNil(Name));
            SetLength(Name, ReturnedLength);
            UniformNames.Append(Format('  Name: %s, type: %s, size: %d',
              [Name, GLShaderVariableTypeName(AType), Size]));
          end;
        end;
    end;
  end;

  procedure GetActiveAttribs(AttribNames: TStringList);
  var
    I: Integer;
    AttribsCount, AttribMaxLength: TGLuint;
    ReturnedLength: TGLsizei;
    Size: TGLint;
    AType: TGLEnum;
    Name: string;
    ErrorCode: TGLenum;
  begin
    case Support of
      gsExtension:
        begin
          glGetProgramivARB(ProgramId, GL_OBJECT_ACTIVE_ATTRIBUTES_ARB, @AttribsCount);

          ErrorCode := glGetError();
          if ErrorCode = GL_INVALID_ENUM then
          begin
            AttribNames.Append(SCannotGetCount);
            Exit;
          end else
          if ErrorCode <> GL_NO_ERROR then
            raise EOpenGLError.Create(ErrorCode, 'GetActiveAttribs');

          glGetProgramivARB(ProgramId, GL_OBJECT_ACTIVE_ATTRIBUTE_MAX_LENGTH_ARB, @AttribMaxLength);

          for I := 0 to AttribsCount - 1 do
          begin
            SetLength(Name, AttribMaxLength);
            glGetActiveAttribARB(ProgramId, I, AttribMaxLength, @ReturnedLength,
              @Size, @AType, PCharOrNil(Name));
            SetLength(Name, ReturnedLength);
            AttribNames.Append(Format('  Name: %s, type: %s, size: %d',
              [Name, GLShaderVariableTypeName(AType), Size]));
          end;
        end;

      gsStandard    :
        begin
          glGetProgramiv(ProgramId, GL_ACTIVE_ATTRIBUTES, @AttribsCount);

          ErrorCode := glGetError();
          if ErrorCode = GL_INVALID_ENUM then
          begin
            AttribNames.Append(SCannotGetCount);
            Exit;
          end else
          if ErrorCode <> GL_NO_ERROR then
            raise EOpenGLError.Create(ErrorCode, 'GetActiveAttribs');

          glGetProgramiv(ProgramId, GL_ACTIVE_ATTRIBUTE_MAX_LENGTH, @AttribMaxLength);

          for I := 0 to AttribsCount - 1 do
          begin
            SetLength(Name, AttribMaxLength);
            glGetActiveAttrib(ProgramId, I, AttribMaxLength, @ReturnedLength,
              @Size, @AType, PCharOrNil(Name));
            SetLength(Name, ReturnedLength);
            AttribNames.Append(Format('  Name: %s, type: %s, size: %d',
              [Name, GLShaderVariableTypeName(AType), Size]));
          end;
        end;
    end;
  end;

  function ShaderInfoLog(ShaderId: TGLuint): string;
  begin
    case Support of
      gsExtension: Result := GetInfoLogARB(ShaderId);
      gsStandard : Result := GetShaderInfoLog(ShaderId);
    end;
  end;

var
  S: TStringList;
  I: Integer;
begin
  Result := 'GLSL program support: ' + GLSupportNames[Support];

  CheckGLErrors('Check at the beginning of TGLSLProgram.DebugInfo');

  S := TStringList.Create;
  try
    GetActiveUniforms(S);
    Result += NL + 'Active uniforms:' + NL + S.Text;

    S.Clear;
    GetActiveAttribs(S);
    Result += NL + 'Active attribs:' + NL + S.Text;
  finally FreeAndNil(S) end;

  for I := 0 to ShaderIds.Count - 1 do
  begin
    Result += NL + Format('Shader number %d (OpenGL id %d) log:',
      [I, ShaderIds[I]]) + NL +
      ShaderInfoLog(ShaderIds[I]);
  end;

  Result += NL + 'Program info log:' + NL + ProgramInfoLog;

  Result += NL + 'Program detected as running in hardware: ' +
    BoolToStr[RunningInHardware];
end;

procedure TGLSLProgram.AttachShader(const ShaderType: TShaderType; const S: string);
var
  AType: TGLenum;

  { Some Nvidia drivers on Linux (like version 295.49
    in Debian testing on 2012-06-02) segfault when calling glCompileShader[ARB]
    with code like "const in gl_MaterialParameters material".
    Googling, at least Nvidia happens to sefault also on other pieces of GLSL.

    Although our internal GLSL code avoids using such known buggy constructs,
    but user can always supply his own shader code.
    So we surround glCompileShader[ARB] in a safeguard, to raise nice
    EGLSLShaderCompileError (that will result in simple warning and fallback
    on fixed-function pipeline) instead of crash. }
  procedure ReportBuggyCompileShader;
  begin
    raise EGLSLShaderCompileError.CreateFmt('%s shader not compiled, segmentation fault in glCompileShader call. Buggy OpenGL GLSL compiler.',
      [ShaderTypeName[ShaderType]]);
  end;

  function CreateShaderARB(const S: string): TGLuint;
  var
    SrcPtr: PChar;
    SrcLength: Cardinal;
    Compiled: TGLint;
  begin
    Result := glCreateShaderObjectARB(AType);
    SrcPtr := PChar(S);
    SrcLength := Length(S);
    glShaderSourceARB(Result, 1, @SrcPtr, @SrcLength);
    try
      glCompileShaderARB(Result);
    except
      on E: EAccessViolation do ReportBuggyCompileShader;
    end;
    glGetObjectParameterivARB(Result, GL_OBJECT_COMPILE_STATUS_ARB, @Compiled);
    if Compiled <> 1 then
      raise EGLSLShaderCompileError.CreateFmt('%s shader not compiled:' + NL + '%s',
        [ShaderTypeName[ShaderType], GetInfoLogARB(Result)]);
  end;

  { Based on Dean Ellis BasicShader.dpr }
  function CreateShader(const S: string): TGLuint;
  var
    SrcPtr: PChar;
    SrcLength: Cardinal;
    Compiled: TGLint;
  begin
    Result := glCreateShader(AType);
    SrcPtr := PChar(S);
    SrcLength := Length(S);
    glShaderSource(Result, 1, @SrcPtr, @SrcLength);
    try
      glCompileShader(Result);
    except
      on E: EAccessViolation do ReportBuggyCompileShader;
    end;
    glGetShaderiv(Result, GL_COMPILE_STATUS, @Compiled);
    { Although I generally avoid creating multiline exception messages,
      ShaderGetInfoLog naturally comes out multiline (it contains a
      couple of error messages) and it's best presented with line breaks.
      So a line break right before ShaderGetInfoLog contents looks good. }
    if Compiled <> GL_TRUE then
      raise EGLSLShaderCompileError.CreateFmt('%s shader not compiled:' + NL + '%s',
        [ShaderTypeName[ShaderType], GetShaderInfoLog(Result)]);
  end;

const
  { FPC < 2.4.4 doesn't have it defined }
  GL_GEOMETRY_SHADER = $8DD9;
var
  ShaderId: TGLuint;
begin
  { calculate AType }
  case ShaderType of
    stVertex:
      case Support of
        gsExtension: AType := GL_VERTEX_SHADER_ARB;
        gsStandard : AType := GL_VERTEX_SHADER    ;
        else Exit;
      end;
    stGeometry:
      if GLVersion.AtLeast(3, 2) and (Support = gsStandard) then
        AType := GL_GEOMETRY_SHADER else
      { otherwise, raise an error --- but only if Support <> gsNone.
        When Support = gsNone, everything should be silent NO-OP. }
      if Support <> gsNone then
        raise EGLSLShaderCompileError.Create('Geometry shaders not supported by your OpenGL version') else
        Exit;
    stFragment:
      case Support of
        gsExtension: AType := GL_FRAGMENT_SHADER_ARB;
        gsStandard : AType := GL_FRAGMENT_SHADER    ;
        else Exit;
      end;
    else raise EInternalError.Create('TGLSLProgram.AttachShader ShaderType?');
  end;

  case Support of
    gsExtension:
      begin
        ShaderId := CreateShaderARB(S);
        glAttachObjectARB(ProgramId, ShaderId);
        ShaderIds.Add(ShaderId);
      end;
    gsStandard:
      begin
        ShaderId := CreateShader(S);
        glAttachShader(ProgramId, ShaderId);
        ShaderIds.Add(ShaderId);
      end;
  end;
end;

procedure TGLSLProgram.AttachVertexShader(const S: string);
begin
  AttachShader(stVertex, S);
end;

procedure TGLSLProgram.AttachFragmentShader(const S: string);
begin
  AttachShader(stFragment, S);
end;

procedure TGLSLProgram.AttachGeometryShader(const S: string);
begin
  AttachShader(stGeometry, S);
end;

procedure TGLSLProgram.DetachAllShaders;
var
  I: Integer;
begin
  case Support of
    gsExtension:
      for I := 0 to ShaderIds.Count - 1 do
      begin
        glDetachObjectARB(ProgramId, ShaderIds[I]);
        glDeleteObjectARB(ShaderIds[I]);
      end;
    gsStandard    :
      for I := 0 to ShaderIds.Count - 1 do
      begin
        glDetachShader   (ProgramId, ShaderIds[I]);
        glDeleteShader   (ShaderIds[I]);
      end;
  end;
  ShaderIds.Count := 0;
end;

procedure TGLSLProgram.Link(RequireRunningInHardware: boolean);
var
  Linked: TGLuint;
begin
  case Support of
    gsExtension:
      begin
        glLinkProgramARB(ProgramId);
        glGetObjectParameterivARB(ProgramId, GL_OBJECT_LINK_STATUS_ARB, @Linked);
        if Linked <> 1 then
          raise EGLSLProgramLinkError.Create('GLSL program not linked' + NL +
            GetInfoLogARB(ProgramId));
      end;
    gsStandard:
      begin
        glLinkProgram(ProgramId);
        glGetProgramiv(ProgramId, GL_LINK_STATUS, @Linked);
        if Linked <> GL_TRUE then
          raise EGLSLProgramLinkError.Create('GLSL program not linked' + NL +
            GetProgramInfoLog(ProgramId));
      end;
  end;

  if RequireRunningInHardware and (not RunningInHardware) then
    raise EGLSLRunningInSoftware.CreateFmt(
      'GLSL shader linked but rejected, as it seems it will run in software.' +
      'Program info log is "%s"', [ProgramInfoLog]);

  if Log and LogShaders then
    WritelnLogMultiline('GLSL', 'GLSL program successfully linked. Information:' + NL + DebugInfo);
end;

function TGLSLProgram.RunningInHardware: boolean;
begin
  { This is good at least for capturing shaders running in software on
      Radeon X300/X550/X1050 Series (crypto on ii.324), Linux fglrx. }

  Result := Pos('shader will run in software due to the', ProgramInfoLog) = 0;
end;

procedure TGLSLProgram.Enable;
begin
  CurrentProgram := Self;
end;

class procedure TGLSLProgram.Disable;
begin
  CurrentProgram := nil;
end;

function TGLSLProgram.SetupUniforms(var BoundTextureUnits: Cardinal): boolean;
begin
  Result := true;
end;

procedure TGLSLProgram.UniformNotFound(const Name: string; const ForceException: boolean);

  function ErrMessage: string;
  begin
    Result := Format('Uniform variable "%s" not found (or not used) in the shader source code', [Name]);
  end;

begin
  if (UniformNotFoundAction = uaException) or ForceException then
    raise EGLSLUniformNotFound.Create(ErrMessage) else
  case UniformNotFoundAction of
    uaWarning: OnWarning(wtMinor, 'GLSL', ErrMessage);
    uaIgnore: ;
    else raise EInternalError.Create('UniformNotFoundAction? in TGLSLProgram.UniformNotFound');
  end;
end;

{ Wrapper over glGetUniformLocationARB (use only if gsExtension) }
{$define GetLocationCheckARB :=

  Location := glGetUniformLocationARB(ProgramId, PCharOrNil(Name));
  if Location = -1 then
  begin
    UniformNotFound(Name, ForceException);
    Exit;
  end;

  if (UniformTypeMismatchAction in [utWarning, utException]) or
     ForceException then
    CheckGLErrors('Cleaning GL errors before setting GLSL uniform:');
}

{ Wrapper over glGetUniformLocation (use only if gsStandard) }
{$define GetLocationCheck :=
  Location := glGetUniformLocation   (ProgramId, PCharOrNil(Name));
  if Location = -1 then
  begin
    UniformNotFound(Name, ForceException);
    Exit;
  end;

  if (UniformTypeMismatchAction in [utWarning, utException]) or
     ForceException then
    CheckGLErrors('Cleaning GL errors before setting GLSL uniform:');
}

procedure TGLSLProgram.SetUniformEnd(const UniformName: string; const ForceException: boolean);
var
  ErrorCode: TGLenum;

  function ErrMessage: string;
  begin
    Result := Format('Error when setting GLSL uniform variable "%s". Probably the type in the shader source code does not match with the type declared in VRML/X3D. OpenGL error (%d): %s',
      [UniformName, ErrorCode,  gluErrorString(ErrorCode)]);
  end;

begin
  if (UniformTypeMismatchAction in [utWarning, utException]) or
     ForceException then
  begin
    { Invalid glUniform call, that specifies wrong uniform variable type,
      may cause OpenGL error "invalid operation". We want to catch it.
      We cleaned GL error at the beginning of SetUniform
      (at the end of macro glGetUniformLocation*), so if there's an error
      now --- we know it's because of glUniform. }
    ErrorCode := glGetError();
    if ErrorCode <> GL_NO_ERROR then
    begin
      if ForceException or (UniformTypeMismatchAction = utException) then
        raise EGLSLUniformNotFound.Create(ErrMessage) else
        OnWarning(wtMinor, 'GLSL', ErrMessage);
    end;
  end;
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: boolean; const ForceException: boolean);
var
  Location: TGLint;
begin
  { GLSL "bool" types are set using the "i" version. From manpage:

    "Either the i or the f variants
     may be used to provide values for uniform variables of type
     bool, bvec2, bvec3, bvec4, or arrays of these. The uniform
     variable will be set to false if the input value is 0 or 0.0f,
     and it will be set to true otherwise.
    "

    Which means that I can simply call glUniform1i, with Ord(Value). }

  case Support of
    gsExtension: begin GetLocationCheckARB glUniform1iARB(Location, Ord(Value)); SetUniformEnd(Name, ForceException); end;
    gsStandard : begin GetLocationCheck    glUniform1i   (Location, Ord(Value)); SetUniformEnd(Name, ForceException); end;
  end;
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TGLint; const ForceException: boolean);
var
  Location: TGLint;
begin
  case Support of
    gsExtension: begin GetLocationCheckARB glUniform1iARB(Location, Value); SetUniformEnd(Name, ForceException); end;
    gsStandard : begin GetLocationCheck    glUniform1i   (Location, Value); SetUniformEnd(Name, ForceException); end;
  end;
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TVector2Integer; const ForceException: boolean);
var
  Location: TGLint;
begin
  case Support of
    gsExtension: begin GetLocationCheckARB glUniform2ivARB(Location, 1, @Value); SetUniformEnd(Name, ForceException); end;
    gsStandard : begin GetLocationCheck    glUniform2iv   (Location, 1, @Value); SetUniformEnd(Name, ForceException); end;
  end;
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TVector3Integer; const ForceException: boolean);
var
  Location: TGLint;
begin
  case Support of
    gsExtension: begin GetLocationCheckARB glUniform3ivARB(Location, 1, @Value); SetUniformEnd(Name, ForceException); end;
    gsStandard : begin GetLocationCheck    glUniform3iv   (Location, 1, @Value); SetUniformEnd(Name, ForceException); end;
  end;
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TVector4Integer; const ForceException: boolean);
var
  Location: TGLint;
begin
  case Support of
    gsExtension: begin GetLocationCheckARB glUniform4ivARB(Location, 1, @Value); SetUniformEnd(Name, ForceException); end;
    gsStandard : begin GetLocationCheck    glUniform4iv   (Location, 1, @Value); SetUniformEnd(Name, ForceException); end;
  end;
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TGLfloat; const ForceException: boolean);
var
  Location: TGLint;
begin
  case Support of
    gsExtension: begin GetLocationCheckARB glUniform1fARB(Location, Value); SetUniformEnd(Name, ForceException); end;
    gsStandard : begin GetLocationCheck    glUniform1f   (Location, Value); SetUniformEnd(Name, ForceException); end;
  end;
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TVector2Single; const ForceException: boolean);
var
  Location: TGLint;
begin
  case Support of
    gsExtension: begin GetLocationCheckARB glUniform2fvARB(Location, 1, @Value); SetUniformEnd(Name, ForceException); end;
    gsStandard : begin GetLocationCheck    glUniform2fv   (Location, 1, @Value); SetUniformEnd(Name, ForceException); end;
  end;
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TVector3Single; const ForceException: boolean);
var
  Location: TGLint;
begin
  case Support of
    gsExtension: begin GetLocationCheckARB glUniform3fvARB(Location, 1, @Value); SetUniformEnd(Name, ForceException); end;
    gsStandard : begin GetLocationCheck    glUniform3fv   (Location, 1, @Value); SetUniformEnd(Name, ForceException); end;
  end;
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TVector4Single; const ForceException: boolean);
var
  Location: TGLint;
begin
  case Support of
    gsExtension: begin GetLocationCheckARB glUniform4fvARB(Location, 1, @Value); SetUniformEnd(Name, ForceException); end;
    gsStandard : begin GetLocationCheck    glUniform4fv   (Location, 1, @Value); SetUniformEnd(Name, ForceException); end;
  end;
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TMatrix2Single; const ForceException: boolean);
var
  Location: TGLint;
begin
  case Support of
    gsExtension: begin GetLocationCheckARB glUniformMatrix2fvARB(Location, 1, GL_FALSE, @Value); SetUniformEnd(Name, ForceException); end;
    gsStandard : begin GetLocationCheck    glUniformMatrix2fv   (Location, 1, GL_FALSE, @Value); SetUniformEnd(Name, ForceException); end;
  end;
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TMatrix3Single; const ForceException: boolean);
var
  Location: TGLint;
begin
  case Support of
    gsExtension: begin GetLocationCheckARB glUniformMatrix3fvARB(Location, 1, GL_FALSE, @Value); SetUniformEnd(Name, ForceException); end;
    gsStandard : begin GetLocationCheck    glUniformMatrix3fv   (Location, 1, GL_FALSE, @Value); SetUniformEnd(Name, ForceException); end;
  end;
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TMatrix4Single; const ForceException: boolean);
var
  Location: TGLint;
begin
  case Support of
    gsExtension: begin GetLocationCheckARB glUniformMatrix4fvARB(Location, 1, GL_FALSE, @Value); SetUniformEnd(Name, ForceException); end;
    gsStandard : begin GetLocationCheck    glUniformMatrix4fv   (Location, 1, GL_FALSE, @Value); SetUniformEnd(Name, ForceException); end;
  end;
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TBooleanList; const ForceException: boolean);
var
  Location: TGLint;
  Ints: TLongIntList;
begin
  { We cannot pass Value.List, as Pascal booleans do not have 4 bytes
    (well, actually I could change this by compiler directive or
    by using LongBool for TBooleanList --- but for TBooleanList
    this would enlarge it 4 times, not nice).

    Unfortunately, there's no glUniform*ub (unsigned byte) or such function.

    So convert to longints. }
  Ints := Value.ToLongInt;
  try
    case Support of
      gsExtension: begin GetLocationCheckARB glUniform1ivARB(Location, Value.Count, PGLint(Ints.List)); SetUniformEnd(Name, ForceException); end;
      gsStandard : begin GetLocationCheck    glUniform1iv   (Location, Value.Count, PGLint(Ints.List)); SetUniformEnd(Name, ForceException); end;
    end;
  finally FreeAndNil(Ints) end;
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TLongIntList; const ForceException: boolean);
var
  Location: TGLint;
begin
  Assert(SizeOf(LongInt) = SizeOf(TGLint));
  case Support of
    gsExtension: begin GetLocationCheckARB glUniform1ivARB(Location, Value.Count, PGLint(Value.List)); SetUniformEnd(Name, ForceException); end;
    gsStandard : begin GetLocationCheck    glUniform1iv   (Location, Value.Count, PGLint(Value.List)); SetUniformEnd(Name, ForceException); end;
  end;
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TSingleList; const ForceException: boolean);
var
  Location: TGLint;
begin
  case Support of
    gsExtension: begin GetLocationCheckARB glUniform1fvARB(Location, Value.Count, PGLfloat(Value.List)); SetUniformEnd(Name, ForceException); end;
    gsStandard : begin GetLocationCheck    glUniform1fv   (Location, Value.Count, PGLfloat(Value.List)); SetUniformEnd(Name, ForceException); end;
  end;
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TVector2SingleList; const ForceException: boolean);
var
  Location: TGLint;
begin
  case Support of
    gsExtension: begin GetLocationCheckARB glUniform2fvARB(Location, Value.Count, PGLfloat(Value.List)); SetUniformEnd(Name, ForceException); end;
    gsStandard : begin GetLocationCheck    glUniform2fv   (Location, Value.Count, PGLfloat(Value.List)); SetUniformEnd(Name, ForceException); end;
  end;
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TVector3SingleList; const ForceException: boolean);
var
  Location: TGLint;
begin
  case Support of
    gsExtension: begin GetLocationCheckARB glUniform3fvARB(Location, Value.Count, PGLfloat(Value.List)); SetUniformEnd(Name, ForceException); end;
    gsStandard : begin GetLocationCheck    glUniform3fv   (Location, Value.Count, PGLfloat(Value.List)); SetUniformEnd(Name, ForceException); end;
  end;
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TVector4SingleList; const ForceException: boolean);
var
  Location: TGLint;
begin
  case Support of
    gsExtension: begin GetLocationCheckARB glUniform4fvARB(Location, Value.Count, PGLfloat(Value.List)); SetUniformEnd(Name, ForceException); end;
    gsStandard : begin GetLocationCheck    glUniform4fv   (Location, Value.Count, PGLfloat(Value.List)); SetUniformEnd(Name, ForceException); end;
  end;
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TMatrix3SingleList; const ForceException: boolean);
var
  Location: TGLint;
begin
  case Support of
    gsExtension: begin GetLocationCheckARB glUniformMatrix3fvARB(Location, Value.Count, GL_FALSE, PGLfloat(Value.List)); SetUniformEnd(Name, ForceException); end;
    gsStandard : begin GetLocationCheck    glUniformMatrix3fv   (Location, Value.Count, GL_FALSE, PGLfloat(Value.List)); SetUniformEnd(Name, ForceException); end;
  end;
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TMatrix4SingleList; const ForceException: boolean);
var
  Location: TGLint;
begin
  case Support of
    gsExtension: begin GetLocationCheckARB glUniformMatrix4fvARB(Location, Value.Count, GL_FALSE, PGLfloat(Value.List)); SetUniformEnd(Name, ForceException); end;
    gsStandard : begin GetLocationCheck    glUniformMatrix4fv   (Location, Value.Count, GL_FALSE, PGLfloat(Value.List)); SetUniformEnd(Name, ForceException); end;
  end;
end;

function TGLSLProgram.GetAttribLocationARB(const Name: string): TGLint;
begin
  Result := glGetAttribLocationARB(ProgramId, PCharOrNil(Name));
  if Result = -1 then
    raise EGLSLAttributeNotFound.CreateFmt('Attribute variable "%s" not found', [Name]);
end;

function TGLSLProgram.GetAttribLocation(const Name: string): TGLint;
begin
  Result := glGetAttribLocation   (ProgramId, PCharOrNil(Name));
  if Result = -1 then
    raise EGLSLAttributeNotFound.CreateFmt('Attribute variable "%s" not found', [Name]);
end;

procedure TGLSLProgram.SetAttribute(const Name: string; const Value: TVector4Integer);
begin
  case Support of
    gsExtension: glVertexAttrib4ivARB(GetAttribLocationARB(Name), @Value);
    gsStandard : glVertexAttrib4iv   (GetAttribLocation   (Name), @Value);
  end;
end;

procedure TGLSLProgram.SetAttribute(const Name: string; const Value: TVector4Byte);
begin
  case Support of
    gsExtension: glVertexAttrib4ubvARB(GetAttribLocationARB(Name), @Value);
    gsStandard : glVertexAttrib4ubv   (GetAttribLocation   (Name), @Value);
  end;
end;

procedure TGLSLProgram.SetAttribute(const Name: string; const Value: TGLfloat);
begin
  case Support of
    gsExtension: glVertexAttrib1fARB(GetAttribLocationARB(Name), Value);
    gsStandard : glVertexAttrib1f   (GetAttribLocation   (Name), Value);
  end;
end;

procedure TGLSLProgram.SetAttribute(const Name: string; const Value: TVector2Single);
begin
  case Support of
    gsExtension: glVertexAttrib2fvARB(GetAttribLocationARB(Name), @Value);
    gsStandard : glVertexAttrib2fv   (GetAttribLocation   (Name), @Value);
  end;
end;

procedure TGLSLProgram.SetAttribute(const Name: string; const Value: TVector3Single);
begin
  case Support of
    gsExtension: glVertexAttrib3fvARB(GetAttribLocationARB(Name), @Value);
    gsStandard : glVertexAttrib3fv   (GetAttribLocation   (Name), @Value);
  end;
end;

procedure TGLSLProgram.SetAttribute(const Name: string; const Value: TVector4Single);
begin
  case Support of
    gsExtension: glVertexAttrib4fvARB(GetAttribLocationARB(Name), @Value);
    gsStandard : glVertexAttrib4fv   (GetAttribLocation   (Name), @Value);
  end;
end;

procedure TGLSLProgram.SetAttribute(const Name: string; const Value: TGLdouble);
begin
  case Support of
    gsExtension: glVertexAttrib1dARB(GetAttribLocationARB(Name), Value);
    gsStandard : glVertexAttrib1d   (GetAttribLocation   (Name), Value);
  end;
end;

procedure TGLSLProgram.SetAttribute(const Name: string; const Value: TVector2Double);
begin
  case Support of
    gsExtension: glVertexAttrib2dvARB(GetAttribLocationARB(Name), @Value);
    gsStandard : glVertexAttrib2dv   (GetAttribLocation   (Name), @Value);
  end;
end;

procedure TGLSLProgram.SetAttribute(const Name: string; const Value: TVector3Double);
begin
  case Support of
    gsExtension: glVertexAttrib3dvARB(GetAttribLocationARB(Name), @Value);
    gsStandard : glVertexAttrib3dv   (GetAttribLocation   (Name), @Value);
  end;
end;

procedure TGLSLProgram.SetAttribute(const Name: string; const Value: TVector4Double);
begin
  case Support of
    gsExtension: glVertexAttrib4dvARB(GetAttribLocationARB(Name), @Value);
    gsStandard : glVertexAttrib4dv   (GetAttribLocation   (Name), @Value);
  end;
end;

procedure TGLSLProgram.SetAttribute(const Name: string; const Value: TMatrix3Single);
var
  Location: TGLint;
begin
  case Support of
    gsExtension:
      begin
        Location := GetAttribLocationARB(Name);
        glVertexAttrib3fvARB(Location    , @Value[0]);
        glVertexAttrib3fvARB(Location + 1, @Value[1]);
        glVertexAttrib3fvARB(Location + 2, @Value[2]);
      end;
    gsStandard    :
      begin
        Location := GetAttribLocation   (Name);
        glVertexAttrib3fv   (Location    , @Value[0]);
        glVertexAttrib3fv   (Location + 1, @Value[1]);
        glVertexAttrib3fv   (Location + 2, @Value[2]);
      end;
  end;
end;

procedure TGLSLProgram.SetAttribute(const Name: string; const Value: TMatrix4Single);
var
  Location: TGLint;
begin
  case Support of
    gsExtension:
      begin
        Location := GetAttribLocationARB(Name);
        glVertexAttrib4fvARB(Location    , @Value[0]);
        glVertexAttrib4fvARB(Location + 1, @Value[1]);
        glVertexAttrib4fvARB(Location + 2, @Value[2]);
        glVertexAttrib4fvARB(Location + 3, @Value[3]);
      end;
    gsStandard    :
      begin
        Location := GetAttribLocation   (Name);
        glVertexAttrib4fv   (Location    , @Value[0]);
        glVertexAttrib4fv   (Location + 1, @Value[1]);
        glVertexAttrib4fv   (Location + 2, @Value[2]);
        glVertexAttrib4fv   (Location + 3, @Value[3]);
      end;
  end;
end;

function TGLSLProgram.VertexAttribPointer(const Name: string;
  LocationOffset: TGLint;
  Size: TGLint; AType: TGLenum; Normalized: TGLboolean; Stride: TGLsizei;
  Ptr: Pointer): TGLint;
begin
  case Support of
    gsExtension:
      begin
        Result := GetAttribLocationARB(Name) + LocationOffset;
        glEnableVertexAttribArrayARB(Result);
        glVertexAttribPointerARB(Result, Size, AType, Normalized, Stride, Ptr);
      end;
    gsStandard    :
      begin
        Result := GetAttribLocation   (Name) + LocationOffset;
        glEnableVertexAttribArray   (Result);
        glVertexAttribPointer   (Result, Size, AType, Normalized, Stride, Ptr);
      end;
  end;
end;

class procedure TGLSLProgram.DisableVertexAttribArray(Location: TGLint);
begin
  case ClassSupport of
    gsExtension: glDisableVertexAttribArrayARB(Location);
    gsStandard : glDisableVertexAttribArray   (Location);
  end;
end;

end.
