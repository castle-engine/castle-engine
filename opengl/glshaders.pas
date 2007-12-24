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

uses SysUtils, GL, GLU, GLExt, KambiGLUtils;

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
  end;

  { Easily handle ARB fragment program. }
  TARBFragmentProgram = class(TARBProgram)
  public
    constructor Create;
    function DebugInfo: string; override;
  end;

  EShaderError = class(Exception);

  { Easily handle program in GLSL (OpenGL Shading Language). }
  TGLSLProgram = class
  private
    FSupport: TGLSupport;

    { Actually, this should be TGLhandleARB for gsARBExtension version.
      But TGLhandleARB = TGLuint in practice, so this is not a problem. }
    VertexShaderId, FragmentShaderId, ProgramId: TGLuint;

    procedure AttachShader(var ShaderId: TGLuint; AType: TGLenum; const S: string);
  public
    constructor Create;
    destructor Destroy; override;

    property Support: TGLSupport read FSupport;

    { Create shader from given string, compile it and attach to current program.

      @raises EShaderError(If the shader source code cannot be compiled,
        exception message contains precise description from OpenGL where
        the error is.)

      @groupBegin }
    procedure AttachVertexShader(const S: string);
    procedure AttachFragmentShader(const S: string);
    { @groupEnd }

    { Link the program, this should be done after attaching all shaders
      and before actually using the program. }
    procedure Link;

    { Enable (use) this program. }
    procedure Enable;

    { Disable this program (use the fixed function pipeline). }
    procedure Disable;

    function DebugInfo: string;
  end;

implementation

uses KambiUtils, KambiStringUtils;

const
  SupportNames: array [TGLSupport] of string =
  ( 'None', 'ARB Extension', 'Standard' );

{ Comfortable shortcut for glGetProgramivARB that always returns 1 value. }
function glGetProgramiARB(target: TGLenum; pname: TGLenum): TGLint;
begin
  glGetProgramivARB(target, pname, @Result);
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

  { TODO: gsStandard not implemented now }

  { if GL_version_2_0 then
    FSupport := gsStandard else }
  if Load_GL_ARB_vertex_program then
    FSupport := gsARBExtension else
    FSupport := gsNone;

  case Support of
    gsARBExtension: glGenProgramsARB(1, @ProgId);
    gsStandard: { TODO };
  end;
end;

function TARBVertexProgram.DebugInfo: string;
begin
  Result := 'Vertex program ' + inherited;
end;

{ TARBFragmentProgram ---------------------------------------------------------- }

constructor TARBFragmentProgram.Create;
begin
  inherited;

  Target := GL_FRAGMENT_PROGRAM_ARB;

  { TODO: gsStandard not implemented now }

  { if GL_version_2_0 then
    FSupport := gsStandard else }
  if Load_GL_ARB_fragment_program then
    FSupport := gsARBExtension else
    FSupport := gsNone;

  case Support of
    gsARBExtension: glGenProgramsARB(1, @ProgId);
    gsStandard: { TODO };
  end;
end;

function TARBFragmentProgram.DebugInfo: string;
begin
  Result := 'Fragment program ' + inherited;
end;

{ TGLSLProgram --------------------------------------------------------------- }

constructor TGLSLProgram.Create;
begin
  inherited;

  if GL_version_2_0 then
    FSupport := gsStandard else
  if Load_GL_ARB_shader_objects and
     Load_GL_ARB_vertex_shader and
     Load_GL_ARB_fragment_shader and
     Load_GL_ARB_shading_language_100 then
    FSupport := gsARBExtension else
    FSupport := gsNone;

  case Support of
    gsARBExtension: ProgramId := glCreateProgramObjectARB();
    gsStandard    : ProgramId := glCreateProgram         ();
  end;
end;

destructor TGLSLProgram.Destroy;
begin
  case Support of
    gsARBExtension: glDeleteObjectARB(ProgramId);
    gsStandard    : glDeleteProgram  (ProgramId);
  end;
end;

function TGLSLProgram.DebugInfo: string;
begin
  Result := 'GLSL program support: ' + SupportNames[Support];
end;

procedure TGLSLProgram.AttachShader(
  var ShaderId: TGLuint; AType: TGLenum; const S: string);

  { Based on Dean Ellis BasicShader.dpr }
  function CreateShader(const S: string): TGLuint;

    { Based on Dean Ellis BasicShader.dpr, but somewhat fixed ? <> 0 not > 1. }
    function ShaderGetInfoLog(Shader: TGLuint): String;
    var
      Len, Len2: TGLint;
    begin
      glGetShaderiv(Shader, GL_INFO_LOG_LENGTH, @Len);

      if Len <> 0 then
      begin
        SetLength(Result, Len);
        glGetShaderInfoLog(Shader, Len, @Len2, PChar(Result));
        StringReplaceAllTo1st(Result, #0, NL);
      end else
        Result := '';
    end;

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
    if Compiled <> GL_TRUE then
      raise EShaderError.CreateFmt('Shader not compiled: %s',
        [ShaderGetInfoLog(Result)]);
  end;

  function CreateShaderARB(const S: string): TGLuint;

    { Based on Dean Ellis BasicShader.dpr, but somewhat fixed ? <> 0 not > 1. }
    function ShaderGetInfoLog(Shader: GLuint): String;
    begin
      Result := 'TODO: implement ShaderGetInfoLog for ARB path';
    end;

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

    { TODO } Compiled := GL_TRUE;

    if Compiled <> GL_TRUE then
      raise EShaderError.CreateFmt('Shader not compiled: %s',
        [ShaderGetInfoLog(Result)]);
  end;

begin
  case Support of
    gsARBExtension:
      begin
        if ShaderId <> 0 then
          glDetachObjectARB(ProgramId, ShaderId);
        ShaderId := CreateShaderARB(S);
        glAttachObjectARB(ProgramId, ShaderId);
      end;
    gsStandard:
      begin
        if ShaderId <> 0 then
          glDetachShader(ProgramId, ShaderId);
        ShaderId := CreateShader(S);
        glAttachShader(ProgramId, ShaderId);
      end;
  end;
end;

procedure TGLSLProgram.AttachVertexShader(const S: string);
begin
  case Support of
    gsARBExtension: AttachShader(VertexShaderId, GL_VERTEX_SHADER_ARB, S);
    gsStandard    : AttachShader(VertexShaderId, GL_VERTEX_SHADER    , S);
  end;
end;

procedure TGLSLProgram.AttachFragmentShader(const S: string);
begin
  case Support of
    gsARBExtension: AttachShader(FragmentShaderId, GL_FRAGMENT_SHADER_ARB, S);
    gsStandard    : AttachShader(FragmentShaderId, GL_FRAGMENT_SHADER    , S);
  end;
end;

procedure TGLSLProgram.Link;
begin
  case Support of
    gsARBExtension: glLinkProgramARB(ProgramId);
    gsStandard    : glLinkProgram   (ProgramId);
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

end.
