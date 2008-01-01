{
  Copyright 2007 Michalis Kamburelis.

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

{ Handling OpenGL shaders (GLSL and ARB vertex / fragment programs).

  Some common notes for all classes defined here:

  @unorderedList(
    @item(
      Creating/destroying the class instance immediately creates/destroys
      appropriate program. So be sure to create/destroy these classes only
      when you have OpenGL context available (for example, create in TGLWindow.OnInit
      and destroy in TGLWindow.OnClose).)

    @item(
      Upon creation, these classes check current OpenGL context abilities.
      Currently three support levels are possible for all programs:
      no support at all (old OpenGL), support through ARB extensions,
      or support built-in (newer OpenGL versions, >= 2.0).

      All three cases are automatically handled inside, so usually you
      do not have to care about this details. Note that "none" support
      (on older OpenGL version with no appropriate ARB extensions)
      means that shaders are not really initialized at all.))
}
unit GLShaders;

interface

uses SysUtils, Classes, GL, GLU, GLExt, KambiGLUtils, KambiUtils, VectorMath;

type
  TGLSupport = (gsNone, gsARBExtension, gsStandard);

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
  EGLSLUniformNotFound = class(EGLSLError);

  TDynGLuintArray = TDynCardinalArray;

  { Easily handle program in GLSL (OpenGL Shading Language). }
  TGLSLProgram = class
  private
    FSupport: TGLSupport;

    { Actually, this should be TGLhandleARB for gsARBExtension version.
      But TGLhandleARB = TGLuint in practice, so this is not a problem. }
    ProgramId: TGLuint;
    ShaderIds: TDynGLuintArray;

    procedure AttachShader(AType: TGLenum; const S: string);

    { Wrapper over glGetUniformLocationARB (use only if gsARBExtension) }
    function GetUniformLocationARB(const Name: string): TGLint;

    { Wrapper over glGetUniformLocation (use only if gsStandard) }
    function GetUniformLocation(const Name: string): TGLint;
  public
    constructor Create;
    destructor Destroy; override;

    property Support: TGLSupport read FSupport;

    { Create shader from given string, compile it and attach to current program.

      You can attach more than one vertex or fragment shader, just
      make sure that only one main() function is among vertex shaders and
      only one among fragment shaders (otherwise Link error will be raised).

      If you want to explicitly get rid of old shaders, use DetachAllShaders.

      @raises(EGLSLShaderCompileError If the shader source code cannot be compiled,
        exception message contains precise description from OpenGL where
        the error is.)

      @groupBegin }
    procedure AttachVertexShader(const S: string);
    procedure AttachFragmentShader(const S: string);
    { @groupEnd }

    procedure DetachAllShaders;

    { Link the program, this should be done after attaching all shaders
      and before actually using the program.

      @raises(EGLSLProgramLinkError If the program cannot be linked,
        exception message contains precise description from OpenGL where
        the error is.) }
    procedure Link;

    { Enable (use) this program. }
    procedure Enable;

    { Disable this program (use the fixed function pipeline). }
    procedure Disable;

    { Returns multiline debug info about current program.
      Things like how it's supported (not at all ? ARB extension ?
      standard ?), names of active uniform variables etc. }
    function DebugInfo: string;

    { @abstract(What support do we get from current OpenGL context ?)
      This is much like @link(Support), but it's a class function. }
    class function ClassSupport: TGLSupport;

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

      @italic(Design notes:) For simplicity, this interface doesn't
      allow you to set
      vectors using different counts as their declared types in shader.
      That is, OpenGL interface has functions like

@preformatted(
  glUniform4fv(GLint location, GLsizei count, GLfloat *value)
)

      The "4" in the name must match array size declared in shader.
      The given "count" specifies how many values are passed.
      AFAIK (but not tested), it's allowed to specify different "count"
      than "4" --- ad then OpenGL "pads" the appropriate passed value.
      For SetUniform methods below, this is not possible
      ("count" as a parameter is always equal to the number in function name),
      I think that this simplifies things without actually losing any
      functionality.

      @raises(EGLSLUniformNotFound If the variable is not found within
        the program.

        Note that this is only one of the many things that can
        go wrong. And on most cases we don't raise any error,
        instead OpenGL sets it's error state and you probably want to
        call CheckGLErrors from time to time to catch them.)

      @groupBegin }
    procedure SetUniform(const Name: string; const Value: TGLint);
    procedure SetUniform(const Name: string; const Value: TVector2Integer);
    procedure SetUniform(const Name: string; const Value: TVector3Integer);
    procedure SetUniform(const Name: string; const Value: TVector4Integer);
    procedure SetUniform(const Name: string; const Value: TGLfloat);
    procedure SetUniform(const Name: string; const Value: TVector2Single);
    procedure SetUniform(const Name: string; const Value: TVector3Single);
    procedure SetUniform(const Name: string; const Value: TVector4Single);
    { @groupEnd }
  end;

implementation

uses KambiStringUtils;

const
  SupportNames: array [TGLSupport] of string =
  ( 'None', 'ARB Extension', 'Standard' );

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

{ TARBProgram ---------------------------------------------------------------- }

destructor TARBProgram.Destroy;
begin
  glDeleteProgramsARB(1, @ProgId);
  inherited;
end;

procedure TARBProgram.Load(const S: string);
begin
  case Support of
    gsARBExtension:
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
  Result := 'support: ' + SupportNames[Support];

  case Support of
    gsARBExtension:
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
    gsARBExtension:
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
    gsARBExtension:
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
    gsARBExtension: glGenProgramsARB(1, @ProgId);
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
  if Load_GL_ARB_vertex_program then
    Result := gsARBExtension else
    Result := gsNone;
end;

{ TARBFragmentProgram ---------------------------------------------------------- }

constructor TARBFragmentProgram.Create;
begin
  inherited;

  Target := GL_FRAGMENT_PROGRAM_ARB;

  FSupport := ClassSupport;

  case Support of
    gsARBExtension: glGenProgramsARB(1, @ProgId);
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
  if Load_GL_ARB_fragment_program then
    Result := gsARBExtension else
    Result := gsNone;
end;

{ TGLSLProgram --------------------------------------------------------------- }

constructor TGLSLProgram.Create;
begin
  inherited;

  FSupport := ClassSupport;

  case Support of
    gsARBExtension: ProgramId := glCreateProgramObjectARB();
    gsStandard    : ProgramId := glCreateProgram         ();
  end;

  ShaderIds := TDynGLuintArray.Create;
end;

destructor TGLSLProgram.Destroy;
begin
  { make sure all shaders are detached and deleted, to free all resources }
  DetachAllShaders;

  case Support of
    gsARBExtension: glDeleteObjectARB(ProgramId);
    gsStandard    : glDeleteProgram  (ProgramId);
  end;

  FreeAndNil(ShaderIds);

  inherited;
end;

class function TGLSLProgram.ClassSupport: TGLSupport;
begin
  if GL_version_2_0 then
    Result := gsStandard else
  if Load_GL_ARB_shader_objects and
     Load_GL_ARB_vertex_shader and
     Load_GL_ARB_fragment_shader and
     Load_GL_ARB_shading_language_100 then
    Result := gsARBExtension else
    Result := gsNone;
end;

function TGLSLProgram.DebugInfo: string;

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

    function GLUniformTypeName(AType: TGLenum): string;
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
        { These are only for GL >= 2.1, will be uncommented when GLExt
          binding will be updated to GL 2.1. }
        //GL_FLOAT_MAT2x3: Result := 'FLOAT_MAT2x3';
        //GL_FLOAT_MAT2x4: Result := 'FLOAT_MAT2x4';
        //GL_FLOAT_MAT3x2: Result := 'FLOAT_MAT3x2';
        //GL_FLOAT_MAT3x4: Result := 'FLOAT_MAT3x4';
        //GL_FLOAT_MAT4x2: Result := 'FLOAT_MAT4x2';
        //GL_FLOAT_MAT4x3: Result := 'FLOAT_MAT4x3';
        GL_SAMPLER_1D: Result := 'SAMPLER_1D';
        GL_SAMPLER_2D: Result := 'SAMPLER_2D';
        GL_SAMPLER_3D: Result := 'SAMPLER_3D';
        GL_SAMPLER_CUBE: Result := 'SAMPLER_CUBE';
        GL_SAMPLER_1D_SHADOW: Result := 'SAMPLER_1D_SHADOW';
        GL_SAMPLER_2D_SHADOW: Result := 'SAMPLER_2D_SHADOW';
        else Result := Format('Unrecognized uniform type "%d"', [AType]);
      end;
    end;

  var
    I: Integer;
    UniformsCount, UniformMaxLength: TGLuint;
    ReturnedLength: TGLsizei;
    Size: TGLint;
    AType: TGLEnum;
    Name: string;
  begin
    case Support of
      gsARBExtension:
        begin
          glGetProgramivARB(ProgramId, GL_OBJECT_ACTIVE_UNIFORMS_ARB, @UniformsCount);
          glGetProgramivARB(ProgramId, GL_OBJECT_ACTIVE_UNIFORM_MAX_LENGTH_ARB, @UniformMaxLength);

          for I := 0 to UniformsCount - 1 do
          begin
            SetLength(Name, UniformMaxLength);
            glGetActiveUniformARB(ProgramId, I, UniformMaxLength, @ReturnedLength,
              @Size, @AType, PCharOrNil(Name));
            SetLength(Name, ReturnedLength);
            UniformNames.Append(Format('  Name: %s, type: %s, size: %d',
              [Name, GLUniformTypeName(AType), Size]));
          end;
        end;

      gsStandard    :
        begin
          glGetProgramiv(ProgramId, GL_ACTIVE_UNIFORMS, @UniformsCount);
          glGetProgramiv(ProgramId, GL_ACTIVE_UNIFORM_MAX_LENGTH, @UniformMaxLength);

          for I := 0 to UniformsCount - 1 do
          begin
            SetLength(Name, UniformMaxLength);
            glGetActiveUniform(ProgramId, I, UniformMaxLength, @ReturnedLength,
              @Size, @AType, PCharOrNil(Name));
            SetLength(Name, ReturnedLength);
            UniformNames.Append(Format('  Name: %s, type: %s, size: %d',
              [Name, GLUniformTypeName(AType), Size]));
          end;
        end;
    end;
  end;

var
  S: TStringList;
begin
  Result := 'GLSL program support: ' + SupportNames[Support];

  S := TStringList.Create;
  try
    GetActiveUniforms(S);
    Result += NL + 'Active uniforms:' + NL + S.Text;
  finally FreeAndNil(S) end;
end;

procedure TGLSLProgram.AttachShader(AType: TGLenum; const S: string);

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
    glCompileShaderARB(Result);
    glGetObjectParameterivARB(Result, GL_OBJECT_COMPILE_STATUS_ARB, @Compiled);
    if Compiled <> 1 then
      raise EGLSLShaderCompileError.CreateFmt('Shader not compiled:' + NL + '%s',
        [GetInfoLogARB(Result)]);
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
    glCompileShader(Result);
    glGetShaderiv(Result, GL_COMPILE_STATUS, @Compiled);
    { Although I generally avoid creating multiline exception messages,
      ShaderGetInfoLog naturally comes out multiline (it contains a
      couple of error messages) and it's best presented with line breaks.
      So a line break right before ShaderGetInfoLog contents looks good. }
    if Compiled <> GL_TRUE then
      raise EGLSLShaderCompileError.CreateFmt('Shader not compiled:' + NL + '%s',
        [GetShaderInfoLog(Result)]);
  end;

var
  ShaderId: TGLuint;
begin
  case Support of
    gsARBExtension:
      begin
        ShaderId := CreateShaderARB(S);
        glAttachObjectARB(ProgramId, ShaderId);
        ShaderIds.AppendItem(ShaderId);
      end;
    gsStandard:
      begin
        ShaderId := CreateShader(S);
        glAttachShader(ProgramId, ShaderId);
        ShaderIds.AppendItem(ShaderId);
      end;
  end;
end;

procedure TGLSLProgram.AttachVertexShader(const S: string);
begin
  case Support of
    gsARBExtension: AttachShader(GL_VERTEX_SHADER_ARB, S);
    gsStandard    : AttachShader(GL_VERTEX_SHADER    , S);
  end;
end;

procedure TGLSLProgram.AttachFragmentShader(const S: string);
begin
  case Support of
    gsARBExtension: AttachShader(GL_FRAGMENT_SHADER_ARB, S);
    gsStandard    : AttachShader(GL_FRAGMENT_SHADER    , S);
  end;
end;

procedure TGLSLProgram.DetachAllShaders;
var
  I: Integer;
begin
  case Support of
    gsARBExtension:
      for I := 0 to ShaderIds.Count - 1 do
      begin
        glDetachObjectARB(ProgramId, ShaderIds.Items[I]);
        glDeleteObjectARB(ShaderIds.Items[I]);
      end;
    gsStandard    :
      for I := 0 to ShaderIds.Count - 1 do
      begin
        glDetachShader   (ProgramId, ShaderIds.Items[I]);
        glDeleteShader   (ShaderIds.Items[I]);
      end;
  end;
  ShaderIds.Count := 0;
end;

procedure TGLSLProgram.Link;
var
  Linked: TGLuint;
begin
  case Support of
    gsARBExtension:
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
end;

procedure TGLSLProgram.Enable;
begin
  case Support of
    gsARBExtension: glUseProgramObjectARB(ProgramId);
    gsStandard    : glUseProgram         (ProgramId);
  end;
end;

procedure TGLSLProgram.Disable;
begin
  case Support of
    gsARBExtension: glUseProgramObjectARB(0);
    gsStandard    : glUseProgram         (0);
  end;
end;

function TGLSLProgram.GetUniformLocationARB(const Name: string): TGLint;
begin
  Result := glGetUniformLocationARB(ProgramId, PCharOrNil(Name));
  if Result = -1 then
    raise EGLSLUniformNotFound.CreateFmt('Uniform variable "%s" not found', [Name]);
end;

function TGLSLProgram.GetUniformLocation(const Name: string): TGLint;
begin
  Result := glGetUniformLocation   (ProgramId, PCharOrNil(Name));
  if Result = -1 then
    raise EGLSLUniformNotFound.CreateFmt('Uniform variable "%s" not found', [Name]);
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TGLint);
begin
  case Support of
    gsARBExtension: glUniform1iARB(GetUniformLocationARB(Name), Value);
    gsStandard    : glUniform1i   (GetUniformLocation   (Name), Value);
  end;
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TVector2Integer);
begin
  case Support of
    gsARBExtension: glUniform2ivARB(GetUniformLocationARB(Name), 2, @Value);
    gsStandard    : glUniform2iv   (GetUniformLocation   (Name), 2, @Value);
  end;
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TVector3Integer);
begin
  case Support of
    gsARBExtension: glUniform3ivARB(GetUniformLocationARB(Name), 3, @Value);
    gsStandard    : glUniform3iv   (GetUniformLocation   (Name), 3, @Value);
  end;
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TVector4Integer);
begin
  case Support of
    gsARBExtension: glUniform4ivARB(GetUniformLocationARB(Name), 4, @Value);
    gsStandard    : glUniform4iv   (GetUniformLocation   (Name), 4, @Value);
  end;
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TGLfloat);
begin
  case Support of
    gsARBExtension: glUniform1fARB(GetUniformLocationARB(Name), Value);
    gsStandard    : glUniform1f   (GetUniformLocation   (Name), Value);
  end;
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TVector2Single);
begin
  case Support of
    gsARBExtension: glUniform2fvARB(GetUniformLocationARB(Name), 2, @Value);
    gsStandard    : glUniform2fv   (GetUniformLocation   (Name), 2, @Value);
  end;
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TVector3Single);
begin
  case Support of
    gsARBExtension: glUniform3fvARB(GetUniformLocationARB(Name), 3, @Value);
    gsStandard    : glUniform3fv   (GetUniformLocation   (Name), 3, @Value);
  end;
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TVector4Single);
begin
  case Support of
    gsARBExtension: glUniform4fvARB(GetUniformLocationARB(Name), 4, @Value);
    gsStandard    : glUniform4fv   (GetUniformLocation   (Name), 4, @Value);
  end;
end;

end.
