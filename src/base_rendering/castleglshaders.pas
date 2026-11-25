{
  Copyright 2007-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ OpenGL shaders in GLSL language.

  @unorderedList(
    @item(
      GLSL version:

      For maximum portability, provide shader code
      that works with oldest GLSL versions, and do not declare "#version".
      This means targeting ancient GLSL 1.10 version for desktop OpenGL
      (introduced in OpenGL 2.0)
      and targeting GLSL 1.00 for OpenGLES (introduced in OpenGLES 2.0).
      This means using "attribute", "varying" keywords, and querying
      the textures using "texture2D", "textureCube" and so on.

      When compiling the shader on modern OpenGL or OpenGLES,
      we will add a modern #version statement to the shader,
      and define a few macros that make it use modern keywords
      (e.g. "attribute" will be replaced with "in" in vertex shader;
      "texture2D" call will be replaced with "texture").
      The resulting shader will not use any deprecated features,
      so it should be fine for OpenGL "core" profile (without compatibility) too.

      As of now, this means we bump the #version for the desktop OpenGL 3.1
      core profile (if OpenGL >= 3.1) and we bump the #version for
      the mobile OpenGLES 3 (if OpenGLES >= 3.0).

      Note: If you know you want to target only newer OpenGL(ES) versions,
      you can also specify #version in shader code explicitly.
      We will not add another #version then. It is your responsibility
      then to provide proper alternatives for desktop OpenGL and mobile OpenGLES,
      as they use similar but not exactly compatible GLSL versions.

      For geometry shaders, you can assume GLSL >= 1.50 (OpenGL 3.2).
      We do not support geometry shaders with older versions.

      Set InternalUpgradeGlslVersion to @false to test that shaders work also without it.
    )

    @item(
      Precision on OpenGLES:

      We automatically add "precision mediump float;" to fragment shader on OpenGLES,
      if no declaration "precision mediump/lowp/highp float;" is found in the code.
      This is practically universally needed for OpenGLES fragment shader,
      that have no default precision.

      See also WebGL recommendations:
      https://developer.mozilla.org/en-US/docs/Web/API/WebGL_API/WebGL_best_practices#be_precise_with_glsl_precision_annotations
    )

    @item(
      Creating/destroying the TGLSLProgram instance immediately creates/destroys
      appropriate program on GPU. So be sure to create/destroy it only
      when you have OpenGL context available (for example,
      create in @link(TCastleUserInterface.GLContextOpen),
      and destroy in @link(TCastleUserInterface.GLContextClose)).)

    @item(
      Upon creation, we check current OpenGL context abilities.

      Currently 2 support levels are possible:
      no support at all (ancient OpenGL) or support built-in (newer OpenGL versions, >= 2.0).
      The support for shaders using ARB extensions has been removed at 2023-01,
      as practically no GPU had it and we didn't really test it.

      Both cases are automatically handled inside, so usually you
      do not have to care about these details.)

  )
}
unit CastleGLShaders;

{ Define CASTLE_COLLECT_SHADER_SOURCE to collect in TGLSLProgram shader code as Srting, to

  - show it in case of error
  - show it before linking, if LogShaders.

  Note:

  - The resulting error message can be quite long,
    as it will contain a full GLSL code.
    That's useful if you're a developer familiar with GLSL, and able to modify it.
    But it makes the log very long and hard to read,
    which is a problem if you're not interested in this particular GLSL bug now.

  - Showing it (at linking) requires collecting shader source code,
    which uses a bit of memory and time.
    (But in practice, this is negligible, so this isn't an important argument.)
}
{$ifdef DEBUG}
  {$define CASTLE_COLLECT_SHADER_SOURCE}
{$endif}

{$I castleconf.inc}

interface

uses SysUtils, Classes, Generics.Collections,
  {$ifdef OpenGLES} CastleGLES, {$else} CastleGL, {$endif}
  CastleGLUtils, CastleUtils, CastleVectors, CastleRenderOptions,
  CastleInternalRHI;

type
  { Common class for exceptions related to GLSL programs. }
  EGLSLError = class(Exception);

  EGLSLShaderCompileError = class(EGLSLError);
  EGLSLProgramLinkError = class(EGLSLError);
  EGLSLAttributeNotFound = class(EGLSLError);
  EGLSLTransformFeedbackError = class(EGLSLError);

  TGLSLProgram = class;

  { GLSL uniform provides information to shader that is constant for a given shader execution. }
  TGLSLUniform = record
  public
    Owner: TGLSLProgram;
    Name: string;
    Location: TGLUniformLocation;

    { Calling @link(TGLSLUniform.SetValue) of this is ignored. }
    class function NotExisting: TGLSLUniform; static;

    { Set uniform variable value.
      You should get the uniform information first using the
      @link(TGLSLProgram.Uniform) call. All the documentation for
      @link(TGLSLProgram.SetUniform) applies also here, so the program must be
      linked, and enabled (it will be enabled automatically by calling this).

      This method always ignores a missing uniform.
      Rely on @link(TGLSLProgram.Uniform) to make something else (like make a warning).

      The TGLSLUniform information about a given uniform stays constant while
      the program is linked. Using the @link(TGLSLProgram.Uniform) one time, and then
      repeatedly calling @link(TGLSLUniform.SetValue), is faster than repeatedly
      calling @link(TGLSLProgram.SetUniform). Because the latter will
      effectively call @link(TGLSLProgram.Uniform) every time, which may have non-zero cost.

      Actually, the uniform location (TGLSLUniform.Location) is constant for a given program,
      and you can even predict the location in some cases without calling @link(TGLSLProgram.Uniform)
      method (see GLSL reference). So it can be even faster, as you can prepare
      correct TGLSLUniform instance in your own code, without calling
      @link(TGLSLProgram.Uniform).

      @groupBegin }
    procedure SetValue(const Value: boolean        ); overload;
    procedure SetValue(const Value: TGLint         ); overload;
    procedure SetValue(const Value: TVector2Integer); overload;
    procedure SetValue(const Value: TVector3Integer); overload;
    procedure SetValue(const Value: TVector4Integer); overload;
    procedure SetValue(const Value: TGLfloat       ); overload;
    procedure SetValue(const Value: TVector2       ); overload;
    procedure SetValue(const Value: TVector3       ); overload;
    procedure SetValue(const Value: TVector4       ); overload;
    procedure SetValue(const Value: TMatrix2       ); overload;
    procedure SetValue(const Value: TMatrix3       ); overload;
    procedure SetValue(const Value: TMatrix4       ); overload;

    procedure SetValue(const Value: TBooleanList); overload;
    procedure SetValue(const Value: TInt32List  ); overload;
    procedure SetValue(const Value: TSingleList ); overload;
    procedure SetValue(const Value: TVector2List); overload;
    procedure SetValue(const Value: TVector3List); overload;
    procedure SetValue(const Value: TVector4List); overload;
    procedure SetValue(const Value: TMatrix3List); overload;
    procedure SetValue(const Value: TMatrix4List); overload;
    { @groupEnd }
  end;

  { GLSL attribute provides per-vertex information to the shader. }
  TGLSLAttribute = record
  public
    type
      TLocationOffset = 0..3;
    var
    Owner: TGLSLProgram;
    Name: string;
    Location: TGLAttribLocation;
    LocationOffsetsToDisable: array [TLocationOffset] of boolean;

    { Enable an array of arbitary OpenGL type.
      See the OpenGL documentation of glVertexAttribPointer for meaning of the
      parameters.

      Note that Ptr is usually an offset relative to the currently bound VBO
      (Vertex Buffer Object), not a regular pointer to a memory content.
      That is why is has a type PtrUInt (not Pointer), since usually you calculate
      it as an integer.

      Note that the array size is not specified anywhere. The way you access
      the array (what indexes you use) determines the minimum count of the array
      you should have (and uploaded to VBO).

      You have to provide VertexArrayObject where the array is bound. }
    procedure EnableArray(const Vao: TVertexArrayObject;
      const LocationOffset: TLocationOffset;
      const Size: TGLint; const AType: TGLenum; const Normalized: TGLboolean; const Stride: TGLsizei;
      const Ptr: PtrUInt);
    { Shortcut to enable an array of floats (Single in Pascal). }
    procedure EnableArraySingle(const Vao: TVertexArrayObject;
      const Stride: TGLsizei; const Ptr: PtrUInt);
    { Shortcut to enable an array of TVector2. }
    procedure EnableArrayVector2(const Vao: TVertexArrayObject;
      const Stride: TGLsizei; const Ptr: PtrUInt);
    { Shortcut to enable an array of TVector3. }
    procedure EnableArrayVector3(const Vao: TVertexArrayObject;
      const Stride: TGLsizei; const Ptr: PtrUInt);
    { Shortcut to enable an array of TVector4. }
    procedure EnableArrayVector4(const Vao: TVertexArrayObject;
      const Stride: TGLsizei; const Ptr: PtrUInt);
    { Shortcut to enable an array of TMatrix3. }
    procedure EnableArrayMatrix3(const Vao: TVertexArrayObject;
      const Stride: TGLsizei; const Ptr: PtrUInt);
    { Shortcut to enable an array of TMatrix4. }
    procedure EnableArrayMatrix4(const Vao: TVertexArrayObject;
      const Stride: TGLsizei; const Ptr: PtrUInt);

    procedure DisableArray;

    { Set attribute variable value.
      The used type must match the type of this variable in GLSL program.

      OpenGL forces some constraints on using this, see SetUniform.
      In short: use this only after linking the program.
      The program is automatically enabled (set as @link(TRenderContext.CurrentProgram RenderContext.CurrentProgram)) by this.
      And note that attributes declared but not actually used in shader code
      may be eliminated, use DebugInfo to see which attributes are actually
      used (@italic(active) in OpenGL terminology).

      These should not be often useful. Usually, you should rather load
      attribute arrays, by EnableArray.

      @groupBegin }
    procedure SetValue(const Value: TGLfloat); overload;
    procedure SetValue(const Value: TVector2); overload;
    procedure SetValue(const Value: TVector3); overload;
    procedure SetValue(const Value: TVector4); overload;
    procedure SetValue(const Value: TMatrix3); overload;
    procedure SetValue(const Value: TMatrix4); overload;
    {$ifndef OpenGLES}
    procedure SetValue(const Value: TVector4Integer); overload;
    procedure SetValue(const Value: TVector4Byte); overload;
    procedure SetValue(const Value: TGLdouble); overload;
    // Makes FPC errors: Error: Asm: Duplicate label, see https://bugs.freepascal.org/view.php?id=32188
    // procedure SetValue(const Value: TVector2Double);
    // procedure SetValue(const Value: TVector3Double);
    // procedure SetValue(const Value: TVector4Double);
    {$endif}
    { @groupEnd }
  end;

  TGLSLAttributeList = {$ifdef FPC}specialize{$endif} TList<TGLSLAttribute>;

  TUniformLocationCache = {$ifdef FPC}specialize{$endif} TDictionary<String, TGLUniformLocation>;
  TAttribLocationCache = {$ifdef FPC}specialize{$endif} TDictionary<String, TGLAttribLocation>;
  TRHIUniformValueMap = {$ifdef FPC}specialize{$endif} TDictionary<String, TVector4>;

  TRHI2DUniformMapping = record
    Name: String;
    ComponentOffset: Integer;
    ComponentCount: Integer;
  end;

  TGLShaderList = {$ifdef FPC}specialize{$endif} TList<TGLShader>;

  { Manage (build, use) a program in GLSL (OpenGL Shading Language). }
  TGLSLProgram = class
  private
    ProgramId: TGLprogram;
    ShaderIds: TGLShaderList;

    FUniformMissing: TUniformMissing;
    FUniformLocations: TUniformLocationCache;
    FAttributeLocations: TAttribLocationCache;
    FUniformReportedMissing: TStringList;

    // Given stage was attached and compiled by @link(AttachShader).
    HasStage: array [TShaderType] of Boolean;

    FSource: array [TShaderType] of TStringList;
    FRHIUniformValues: TRHIUniformValueMap;
    FRHIUniformOrder: TStringList;
    FRHI2DUniformMappings: array of TRHI2DUniformMapping;
    FRHI2DFragmentShader: TRHIShaderModule;
    FRHI2DAdaptedFragmentSource: String;

    class function GetCurrent: TGLSLProgram; static;
    class procedure SetCurrent(const Value: TGLSLProgram); static;

    { Make sure all shaders are detached and deleted, to free all resources.
      This may be called repeatedly, calling it the 2nd time will not free
      any more resources.

      Note that we don't support calling "Link" repeatedly,
      this class assumes that TGLSLProgram is only linked once and then used,
      for now.
      So calling GLContextClose multiple times has really simple semantics:
      2nd call just does nothing. The shader is not useful after the 1st
      GLContextClose call. }
    procedure GLContextClose;
    procedure OnGlContextClose(Sender: TObject);
  public
    { Shader name is used in log messages. Any String is OK. }
    Name: String;

    {$ifdef CASTLE_CANNOT_CATCH_EXCEPTIONS}
    { Web target doesn't support try..except, so we use this field
      to report that AttachShader or Link caused an error.
      WritelnWarning with details was already generated.
      Non-web targets should not rely on this, exceptions signal errors. }
    ErrorOnCompileLink: Boolean;
    {$endif}

    { Is fragment shader required to link successfully.

      By default this is @false (for now) and you can attempt to link
      a shader with only vertex shader, without fragment shader.
      This makes sense on OpenGL
      ( https://www.khronos.org/opengl/wiki/Fragment_Shader#Optional ).
      Only OpenGLES, or OpenGL on macOS with Core profile,
      strictly requires fragment shader to be present.

      Setting this to @true is useful when you know shader really requires
      fragment shader and you want to get more clear error message.
      CGE then knows to abort linking early, with clear error message,
      when fragment shader is missing. }
    FragmentShaderRequired: Boolean;

    constructor Create;
    destructor Destroy; override;

    { Create shader from given string, compile it and attach to current program.

      For desktop OpenGL, you can attach more than one shader of given
      type, just make sure that only one main() function is among each type
      (otherwise link error will be raised later).
      For OpenGLES, this is not allowed, so don't use this if you want
      to work with OpenGLES too.

      If you want to explicitly get rid of old shaders, use DetachAllShaders.

      @raises(EGLSLShaderCompileError If the shader source code cannot be compiled,
        exception message contains precise description from OpenGL where
        the error is.)

      @groupBegin }
    procedure AttachVertexShader(const S: string);
    procedure AttachGeometryShader(const S: string);
    procedure AttachFragmentShader(const S: string);
    procedure AttachShader(const ShaderType: TShaderType; S: string); overload;
    { @groupEnd }

    { Attach multiple shader parts for given type.

      For normal OpenGL, we can use GLSL separate compilation units.
      So this is equivalent to just calling
      @code(AttachShader(ShaderType, Parts[I])) for each part.

      For OpenGL ES, this is unfortunately not possible,
      you can only attach a single shader of a given type (see
      http://www.khronos.org/opengles/sdk/docs/man/xhtml/glAttachShader.xml ).
      So we simply concatenate the shaders into one. }
    procedure AttachShader(const ShaderType: TShaderType;
      const Parts: TStrings); overload;

    procedure DetachAllShaders;

    { Specify values to record in transform feedback buffers.
      This must be called before @link(Link) method. }
    procedure SetTransformFeedbackVaryings(const Varyings: array of String;
      const IsSingleBufferMode: Boolean = True);

    { Link the program, this should be done after attaching all shaders
      and before actually using the program.

      @raises(EGLSLProgramLinkError If the program cannot be linked,
        exception message contains precise description from OpenGL where
        the error is.) }
    procedure Link; overload; virtual;
    procedure Link(Ignored: boolean); overload; deprecated 'use parameterless Link method';

    { Enable (use) this program. Shortcut for @code(RenderContext.CurrentProgram := Self). }
    procedure Enable;

    { Disable this program (use the fixed function pipeline).
      Shortcut for @code(RenderContext.CurrentProgram := nil). }
    class procedure Disable;

    { Override this to set uniform values, in particular to
      bind the textures used by this shader, right after each @link(Enable)
      call.

      This is automatically called

      - by our shape rendering
        (when we renders shapes, from TShaderCoordinateRenderer.RenderCoordinateBegin)

      - by our screen effects rendering code
        (from TCastleScreenEffects.RenderOverChildren).

      ... right after the given shader is enabled by
      @code(RenderContext.CurrentProgram := ThisShader),
      which is equivalent to @code(ThisShader.Enable).

      If other cases, it's your responsibility to call this method
      explicitly, if you want shaders using it to work. Do this right after
      @code(RenderContext.CurrentProgram := ThisShader),
      or equivalent @code(ThisShader.Enable).

      You can set any uniform values, and generally do
      anything you want to be done each time this shader is enabled.
      In particular, you can bind textures and set corresponding uniform
      variables of them. Increase BoundTextureUnits appropriately.

      Returns @false is some texture couldn't be bound. }
    function SetupUniforms(var BoundTextureUnits: Cardinal): boolean; virtual;

    { Returns multiline debug info about current program.
      Reports whether shaders are supported,
      names of active uniform and attribute variables etc.

      @raises Exception If any OpenGL error will be detected. }
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

    { What to do when GLSL uniform variable is accessed (by @link(SetUniform)
      or @link(Uniform)) but doesn't exist in the shader.
      Note that OpenGL aggresively removes unused code and variables
      from the shader when compiling/linking, so this also happens for
      "declared but unused" variables.

      This is only the default value, each @link(Uniform) method call
      may explicitly use other TUniformMissing.

      @seealso TUniformMissing }
    property UniformMissing: TUniformMissing
      read FUniformMissing write FUniformMissing
      default umWarning;

    { Get the uniform instance. It can be used to make repeated
      @link(TGLSLUniform.SetValue) calls. You must link the program first.

      If the uniform doesn't exist (or is unused), the action
      we take depends on @link(UniformMissing) property
      (by default -- umWarning).
      The overloaded version with extra @code(AUniformMissing)
      parameter follows this parameter value.

      @groupBegin }
    function Uniform(const AName: string): TGLSLUniform; overload;
    function Uniform(const AName: string; const AUniformMissing: TUniformMissing): TGLSLUniform; overload;
    { @groupEnd }

    { Set appropriate uniform variable value.
      The used type must match the type of this variable in GLSL program.

      OpenGL forces some constraints on using this:

      @unorderedList(
        @item(This should be used only after linking, and re-linking clears
          (sets to zero) all uniform variables values.)

        @item(This GLSL program must be currently enabled for this to work.
          So calling this automatically calls @link(Enable)
          and sets @link(TRenderContext.CurrentProgram RenderContext.CurrentProgram).

          This is required by OpenGL glUniform*
          commands. glGetUniformLocation take program id as parameter, so they
          can operate on any program. But glUniform* operate only on
          active program.)

        @item(Only @italic(active) uniforms variables may be set.
          @italic(Active) means, quoting OpenGL manpages,
          a variable that is determined during the link operation
          that it may be accessed during program execution.
          In other words, when linking GLSL program, unused variables
          may be eliminated, and you cannot set them by SetUniform.

          Call DebugInfo to see what uniform variables are considered
          active for your shader.

          You can change UniformMissing not to silently ignore setting
          of inactive (or not existing) uniform values.)
      )

      This will make warning if the variable is not found within
      the program and UniformMissing = umWarning.
      The overloaded version with explicit AUniformMissing parameter uses that
      to decide whether to show warning.

      @groupBegin }
    procedure SetUniform(const AName: string; const Value: boolean        ; const AUniformMissing: TUniformMissing); overload;
    procedure SetUniform(const AName: string; const Value: TGLint         ; const AUniformMissing: TUniformMissing); overload;
    procedure SetUniform(const AName: string; const Value: TVector2Integer; const AUniformMissing: TUniformMissing); overload;
    procedure SetUniform(const AName: string; const Value: TVector3Integer; const AUniformMissing: TUniformMissing); overload;
    procedure SetUniform(const AName: string; const Value: TVector4Integer; const AUniformMissing: TUniformMissing); overload;
    procedure SetUniform(const AName: string; const Value: TGLfloat       ; const AUniformMissing: TUniformMissing); overload;
    procedure SetUniform(const AName: string; const Value: TVector2 ; const AUniformMissing: TUniformMissing); overload;
    procedure SetUniform(const AName: string; const Value: TVector3 ; const AUniformMissing: TUniformMissing); overload;
    procedure SetUniform(const AName: string; const Value: TVector4 ; const AUniformMissing: TUniformMissing); overload;
    procedure SetUniform(const AName: string; const Value: TMatrix2 ; const AUniformMissing: TUniformMissing); overload;
    procedure SetUniform(const AName: string; const Value: TMatrix3 ; const AUniformMissing: TUniformMissing); overload;
    procedure SetUniform(const AName: string; const Value: TMatrix4 ; const AUniformMissing: TUniformMissing); overload;
    procedure SetUniform(const AName: string; const Value: TBooleanList; const AUniformMissing: TUniformMissing); overload;
    procedure SetUniform(const AName: string; const Value: TInt32List  ; const AUniformMissing: TUniformMissing); overload;
    procedure SetUniform(const AName: string; const Value: TSingleList ; const AUniformMissing: TUniformMissing); overload;
    procedure SetUniform(const AName: string; const Value: TVector2List; const AUniformMissing: TUniformMissing); overload;
    procedure SetUniform(const AName: string; const Value: TVector3List; const AUniformMissing: TUniformMissing); overload;
    procedure SetUniform(const AName: string; const Value: TVector4List; const AUniformMissing: TUniformMissing); overload;
    procedure SetUniform(const AName: string; const Value: TMatrix3List; const AUniformMissing: TUniformMissing); overload;
    procedure SetUniform(const AName: string; const Value: TMatrix4List; const AUniformMissing: TUniformMissing); overload;

    procedure SetUniform(const AName: string; const Value: boolean        ); overload;
    procedure SetUniform(const AName: string; const Value: TGLint         ); overload;
    procedure SetUniform(const AName: string; const Value: TVector2Integer); overload;
    procedure SetUniform(const AName: string; const Value: TVector3Integer); overload;
    procedure SetUniform(const AName: string; const Value: TVector4Integer); overload;
    procedure SetUniform(const AName: string; const Value: TGLfloat       ); overload;
    procedure SetUniform(const AName: string; const Value: TVector2 ); overload;
    procedure SetUniform(const AName: string; const Value: TVector3 ); overload;
    procedure SetUniform(const AName: string; const Value: TVector4 ); overload;
    procedure SetUniform(const AName: string; const Value: TMatrix2 ); overload;
    procedure SetUniform(const AName: string; const Value: TMatrix3 ); overload;
    procedure SetUniform(const AName: string; const Value: TMatrix4 ); overload;
    procedure SetUniform(const AName: string; const Value: TBooleanList); overload;
    procedure SetUniform(const AName: string; const Value: TInt32List  ); overload;
    procedure SetUniform(const AName: string; const Value: TSingleList ); overload;
    procedure SetUniform(const AName: string; const Value: TVector2List); overload;
    procedure SetUniform(const AName: string; const Value: TVector3List); overload;
    procedure SetUniform(const AName: string; const Value: TVector4List); overload;
    procedure SetUniform(const AName: string; const Value: TMatrix3List); overload;
    procedure SetUniform(const AName: string; const Value: TMatrix4List); overload;
    { @groupEnd }

    { Get the attribute instance. It can be used to make repeated
      @link(TGLSLAttribute.SetValue) and @link(TGLSLAttribute.EnableArray).

      @raises(EGLSLAttributeNotFound If the variable is not found within
        the program.

        Note that this is only one of the many things that can
        go wrong. And on most cases we don't raise any error,
        instead OpenGL sets it's error state and you probably want to
        call CheckGLErrors from time to time to catch them.) }
    function Attribute(const AName: string): TGLSLAttribute;

    { Get the attribute instance. It can be used to make repeated
      @link(TGLSLAttribute.SetValue) and @link(TGLSLAttribute.EnableArray).

      Returned attribute has @link(TGLSLAttribute.Location) equal to GLAttribLocationNone.
      All the calls to @link(TGLSLAttribute.SetValue)
      or @link(TGLSLAttribute.EnableArray) or @link(TGLSLAttribute.DisableArray)
      methods are silently ignored. }
    function AttributeOptional(const AName: string): TGLSLAttribute;

    { Set attribute variable value.
      The used type must match the type of this variable in GLSL program.

      OpenGL forces some constraints on using this, see SetUniform.
      In short: use this only after linking the program.
      The program is automatically enabled (set as @link(TRenderContext.CurrentProgram RenderContext.CurrentProgram)) by this.
      And note that attributes declared but not actually used in shader code
      may be eliminated, use DebugInfo to see which attributes are actually
      used (@italic(active) in OpenGL terminology).

      These should not be often used. Usually, you should rather load
      attribute arrays, by VertexAttribPointer. You should also get attribute
      by the @link(Attribute) method, as TGLSLAttribute, and then call
      @link(TGLSLAttribute.SetValue) or @link(TGLSLAttribute.EnableArray).

      @raises(EGLSLAttributeNotFound If the variable is not found within
        the program.

        Note that this is only one of the many things that can
        go wrong. And on most cases we don't raise any error,
        instead OpenGL sets it's error state and you probably want to
        call CheckGLErrors from time to time to catch them.)

      @groupBegin }
    procedure SetAttribute(const AName: string; const Value: TGLfloat); overload; deprecated 'use TGLSLAttribute.SetValue';
    procedure SetAttribute(const AName: string; const Value: TVector2); overload; deprecated 'use TGLSLAttribute.SetValue';
    procedure SetAttribute(const AName: string; const Value: TVector3); overload; deprecated 'use TGLSLAttribute.SetValue';
    procedure SetAttribute(const AName: string; const Value: TVector4); overload; deprecated 'use TGLSLAttribute.SetValue';
    procedure SetAttribute(const AName: string; const Value: TMatrix3); overload; deprecated 'use TGLSLAttribute.SetValue';
    procedure SetAttribute(const AName: string; const Value: TMatrix4); overload; deprecated 'use TGLSLAttribute.SetValue';
    {$ifndef OpenGLES}
    procedure SetAttribute(const AName: string; const Value: TVector4Integer); overload; deprecated 'use TGLSLAttribute.SetValue';
    procedure SetAttribute(const AName: string; const Value: TVector4Byte); overload; deprecated 'use TGLSLAttribute.SetValue';
    procedure SetAttribute(const AName: string; const Value: TGLdouble); overload; deprecated 'use TGLSLAttribute.SetValue';
    // Makes FPC errors: Error: Asm: Duplicate label, see https://bugs.freepascal.org/view.php?id=32188
    // procedure SetAttribute(const AName: string; const Value: TVector2Double); deprecated 'use TGLSLAttribute.SetValue';
    // procedure SetAttribute(const AName: string; const Value: TVector3Double); deprecated 'use TGLSLAttribute.SetValue';
    // procedure SetAttribute(const AName: string; const Value: TVector4Double); deprecated 'use TGLSLAttribute.SetValue';
    {$endif}
    { @groupEnd }

    {$ifdef FPC}
    { Currently enabled GLSL program.
      @nil if fixed-function pipeline should be used.
      Setting this property encapsulates the OpenGL glUseProgram
      (or equivalent ARB extension), additionally preventing redundant glUseProgram
      calls. }
    class property Current: TGLSLProgram read GetCurrent write SetCurrent; deprecated 'use RenderContext.CurrentProgram';
    {$endif}

    { Collected shader source code for the given stage. }
    function Source(const ShaderType: TShaderType): String;

    { Store and query uniform values for RHI custom shader execution. }
    procedure SetRHIUniformValue(const AName: String; const AValue: TVector4);
    function GetRHIUniformValue(const AName: String; out AValue: TVector4): Boolean;

    { Lazily adapts attached 2D fragment shader source to GLSL 450 and compiles
      a TRHIShaderModule for TCastleBatch2DRenderer custom 2D shader rendering. }
    function GetOrCreateRHI2DFragmentShader(const ADevice: TRHIDevice): TRHIShaderModule;

    { Packs custom 2D shader uniform values into a vec4 payload for
      CastleDraw.u_ModelViewMatrix[3] (TCastle2DPushConstants.CustomParams). }
    function BuildRHI2DCustomParams: TVector4;

    property RHI2DFragmentShader: TRHIShaderModule read FRHI2DFragmentShader;
    property RHI2DAdaptedFragmentSource: String read FRHI2DAdaptedFragmentSource;
  end;

  TGLSLProgramList = {$ifdef FPC}specialize{$endif} TObjectList<TGLSLProgram>;

var
  LogShaders: boolean;

{$ifdef FPC}
function GetCurrentProgram: TGLSLProgram;
  deprecated 'use RenderContext.CurrentProgram';
procedure SetCurrentProgram(const Value: TGLSLProgram);
  deprecated 'use RenderContext.CurrentProgram';

{ Currently enabled GLSL program.
  @nil if fixed-function pipeline should be used.
  Setting this property encapsulates the OpenGL glUseProgram
  (or equivalent ARB extension), additionally preventing redundant glUseProgram
  calls.

  @deprecated Use RenderContext.CurrentProgram }
property CurrentProgram: TGLSLProgram
  read GetCurrentProgram write SetCurrentProgram;
{$endif FPC}

{ Used by RenderContext.SetCurrentProgram. @exclude }
procedure InternalSetCurrentProgram(const Value: TGLSLProgram);

var
  { By default, TGLSLProgram automatically adds a #version and a few macros
    to make GLSL shader code use a modern version, with only modern (not deprecated)
    GLSL features.

    Set this to @false to disable it -- useful to test
    that shaders also work fine on older OpenGL(ES) versions than what you have.
    Note that resulting shaders may not work in OpenGL "core" profile (only compatibility)
    and some features (like shadow samplers on OpenGLES) may be missing.

    @exclude }
  InternalUpgradeGlslVersion: Boolean = true;

implementation

uses CastleStringUtils, CastleLog, CastleGLVersion, CastleRenderContext,
  CastleInternalGLUtils, CastleApplicationProperties, CastleInternalShaderRHI;

{ Wrapper around glGetShaderInfoLog. }
function GetShaderInfoLog(const ShaderId: TGLShader): String;
{$ifdef CASTLE_WEBGL}
begin
  Result := glGetShaderInfoLog(ShaderId);
{$else}
{ OpenGL version based on Dean Ellis BasicShader.dpr
  (with some changes, e.g. fixed ? <> 0 not > 1, adjusted to both FPC / Delphi
  and our CastleGL header). }
var
  Len, Len2: TGLint;
  { We assume the driver returns UTF-8 (though it is usually ASCII in practice). }
  Result8: Utf8String;
begin
  glGetShaderiv(ShaderId, GL_INFO_LOG_LENGTH, @Len);

  if Len <> 0 then
  begin
    SetLength(Result8, Len);
    glGetShaderInfoLog(ShaderId, Len, @Len2, PAnsiChar(Result8));
    SetLength(Result8, Len2); // trim to Len2, as GL_INFO_LOG_LENGTH included size with null terminator
    Result := Result8;
  end else
    Result := '';
{$endif}
end;

{ Wrapper around glGetProgramInfoLog. }
function GetProgramInfoLog(const ProgramId: TGLProgram): String;
{$ifdef CASTLE_WEBGL}
begin
  Result := glGetProgramInfoLog(ProgramId);
{$else}
var
  Len, Len2: TGLint;
  { We assume the driver returns UTF-8 (though it is usually ASCII in practice). }
  Result8: Utf8String;
begin
  glGetProgramiv(ProgramId, GL_INFO_LOG_LENGTH, @Len);

  if Len <> 0 then
  begin
    SetLength(Result8, Len);
    glGetProgramInfoLog(ProgramId, Len, @Len2, PAnsiChar(Result8));
    SetLength(Result8, Len2); // trim to Len2, as GL_INFO_LOG_LENGTH included size with null terminator
    Result := Result8;
  end else
    Result := '';
{$endif}
end;

function GetCurrentProgram: TGLSLProgram;
begin
  if RenderContext <> nil then
    Result := RenderContext.CurrentProgram
  else
    Result := nil;
end;

procedure SetCurrentProgram(const Value: TGLSLProgram);
begin
  if RenderContext <> nil then
    RenderContext.CurrentProgram := Value;
end;

procedure InternalSetCurrentProgram(const Value: TGLSLProgram);
begin
  if (GLFeatures = nil) or (not GLFeatures.Shaders) then
    Exit;
  if Value <> nil then
  begin
    if Value.ProgramId = GLObjectNone then
    begin
      WritelnWarning('Trying to use a GLSL shader after the OpenGL context for which it was prepared closed; you have to create new TGLSLProgram instance');
      glUseProgram(GLObjectNone);
      Exit;
    end;
    if GLFeatures.Shaders then
      glUseProgram(Value.ProgramId);
  end else
  begin
    if GLFeatures.Shaders then
      glUseProgram(GLObjectNone);
  end;
end;

{ TGLSLUniform --------------------------------------------------------------- }

class function TGLSLUniform.NotExisting: TGLSLUniform; {$ifdef FPC} static;{$endif}
const
  R: TGLSLUniform = (Owner: nil; Name: ''; Location: GLUniformLocationNone);
begin
  Result := R;
end;

procedure TGLSLUniform.SetValue(const Value: boolean);
begin
  if (Owner <> nil) and (Name <> '') then
    Owner.SetRHIUniformValue(Name, Vector4(Ord(Value), 0, 0, 0));
  if Location = GLUniformLocationNone then Exit; // ignore non-existing uniform here
  if (GLFeatures = nil) or (not GLFeatures.Shaders) then Exit;

  { GLSL "bool" types are set using the "i" version. From manpage:

    "Either the i or the f variants
     may be used to provide values for uniform variables of type
     bool, bvec2, bvec3, bvec4, or arrays of these. The uniform
     variable will be set to false if the input value is 0 or 0.0f,
     and it will be set to true otherwise.
    "

    Which means that I can simply call glUniform1i, with Ord(Value). }

  Owner.Enable;
  if GLFeatures.Shaders then
    glUniform1i(Location, Ord(Value));
end;

procedure TGLSLUniform.SetValue(const Value: TGLint);
begin
  if (Owner <> nil) and (Name <> '') then
    Owner.SetRHIUniformValue(Name, Vector4(Value, 0, 0, 0));
  if Location = GLUniformLocationNone then Exit; // ignore non-existing uniform here
  if (GLFeatures = nil) or (not GLFeatures.Shaders) then Exit;
  Owner.Enable;
  if GLFeatures.Shaders then
    glUniform1i(Location, Value);
end;

procedure TGLSLUniform.SetValue(const Value: TVector2Integer);
begin
  if (Owner <> nil) and (Name <> '') then
    Owner.SetRHIUniformValue(Name, Vector4(Value[0], Value[1], 0, 0));
  if Location = GLUniformLocationNone then Exit; // ignore non-existing uniform here
  if (GLFeatures = nil) or (not GLFeatures.Shaders) then Exit;
  Owner.Enable;
  if GLFeatures.Shaders then
    {$ifdef CASTLE_WEBGL}
    glUniform2i(Location, Value[0], Value[1]);
    {$else}
    glUniform2iv(Location, 1, @Value);
    {$endif}
end;

procedure TGLSLUniform.SetValue(const Value: TVector3Integer);
begin
  if (Owner <> nil) and (Name <> '') then
    Owner.SetRHIUniformValue(Name, Vector4(Value[0], Value[1], Value[2], 0));
  if Location = GLUniformLocationNone then Exit; // ignore non-existing uniform here
  if (GLFeatures = nil) or (not GLFeatures.Shaders) then Exit;
  Owner.Enable;
  if GLFeatures.Shaders then
    {$ifdef CASTLE_WEBGL}
    glUniform3i(Location, Value[0], Value[1], Value[2]);
    {$else}
    glUniform3iv(Location, 1, @Value);
    {$endif}
end;

procedure TGLSLUniform.SetValue(const Value: TVector4Integer);
begin
  if (Owner <> nil) and (Name <> '') then
    Owner.SetRHIUniformValue(Name, Vector4(Value[0], Value[1], Value[2], Value[3]));
  if Location = GLUniformLocationNone then Exit; // ignore non-existing uniform here
  if (GLFeatures = nil) or (not GLFeatures.Shaders) then Exit;
  Owner.Enable;
  if GLFeatures.Shaders then
    {$ifdef CASTLE_WEBGL}
    glUniform4i(Location, Value[0], Value[1], Value[2], Value[3]);
    {$else}
    glUniform4iv(Location, 1, @Value);
    {$endif}
end;

procedure TGLSLUniform.SetValue(const Value: TGLfloat);
begin
  if (Owner <> nil) and (Name <> '') then
    Owner.SetRHIUniformValue(Name, Vector4(Value, 0, 0, 0));
  if Location = GLUniformLocationNone then Exit; // ignore non-existing uniform here
  if (GLFeatures = nil) or (not GLFeatures.Shaders) then Exit;
  Owner.Enable;
  if GLFeatures.Shaders then
    glUniform1f(Location, Value);
end;

procedure TGLSLUniform.SetValue(const Value: TVector2);
begin
  if (Owner <> nil) and (Name <> '') then
    Owner.SetRHIUniformValue(Name, Vector4(Value[0], Value[1], 0, 0));
  if Location = GLUniformLocationNone then Exit; // ignore non-existing uniform here
  if (GLFeatures = nil) or (not GLFeatures.Shaders) then Exit;
  Owner.Enable;
  if GLFeatures.Shaders then
    {$ifdef CASTLE_WEBGL}
    glUniform2f(Location, Value[0], Value[1]);
    {$else}
    glUniform2fv(Location, 1, @Value);
    {$endif}
end;

procedure TGLSLUniform.SetValue(const Value: TVector3);
begin
  if (Owner <> nil) and (Name <> '') then
    Owner.SetRHIUniformValue(Name, Vector4(Value[0], Value[1], Value[2], 0));
  if Location = GLUniformLocationNone then Exit; // ignore non-existing uniform here
  if (GLFeatures = nil) or (not GLFeatures.Shaders) then Exit;
  Owner.Enable;
  if GLFeatures.Shaders then
    {$ifdef CASTLE_WEBGL}
    glUniform3f(Location, Value[0], Value[1], Value[2]);
    {$else}
    glUniform3fv(Location, 1, @Value);
    {$endif}
end;

procedure TGLSLUniform.SetValue(const Value: TVector4);
begin
  if (Owner <> nil) and (Name <> '') then
    Owner.SetRHIUniformValue(Name, Value);
  if Location = GLUniformLocationNone then Exit; // ignore non-existing uniform here
  if (GLFeatures = nil) or (not GLFeatures.Shaders) then Exit;
  Owner.Enable;
  if GLFeatures.Shaders then
    {$ifdef CASTLE_WEBGL}
    glUniform4f(Location, Value[0], Value[1], Value[2], Value[3]);
    {$else}
    glUniform4fv(Location, 1, @Value);
    {$endif}
end;

procedure TGLSLUniform.SetValue(const Value: TMatrix2);
begin
  if (Owner <> nil) and (Name <> '') then
  begin
    Owner.SetRHIUniformValue(Name, Vector4(Value.Data[0][0], Value.Data[0][1], Value.Data[1][0], Value.Data[1][1]));
    Owner.SetRHIUniformValue(Name + '[0]', Vector4(Value.Data[0][0], Value.Data[0][1], 0, 0));
    Owner.SetRHIUniformValue(Name + '[1]', Vector4(Value.Data[1][0], Value.Data[1][1], 0, 0));
  end;
  if Location = GLUniformLocationNone then Exit; // ignore non-existing uniform here
  if (GLFeatures = nil) or (not GLFeatures.Shaders) then Exit;
  Owner.Enable;
  if GLFeatures.Shaders then
    {$ifdef CASTLE_WEBGL}
    glUniformMatrix2fv(Location, GL_FALSE, MatrixToWebGL(Value), 0, 0);
    {$else}
    glUniformMatrix2fv(Location, 1, GL_FALSE, @Value);
    {$endif}
end;

procedure TGLSLUniform.SetValue(const Value: TMatrix3);
var
  J: Integer;
begin
  if (Owner <> nil) and (Name <> '') then
  begin
    Owner.SetRHIUniformValue(Name, Vector4(Value.Data[0][0], Value.Data[0][1], Value.Data[0][2], 0));
    for J := 0 to 2 do
      Owner.SetRHIUniformValue(Name + '[' + IntToStr(J) + ']',
        Vector4(Value.Data[J][0], Value.Data[J][1], Value.Data[J][2], 0));
  end;
  if Location = GLUniformLocationNone then Exit; // ignore non-existing uniform here
  if (GLFeatures = nil) or (not GLFeatures.Shaders) then Exit;
  Owner.Enable;
  if GLFeatures.Shaders then
    {$ifdef CASTLE_WEBGL}
    glUniformMatrix3fv(Location, GL_FALSE, MatrixToWebGL(Value), 0, 0);
    {$else}
    glUniformMatrix3fv(Location, 1, GL_FALSE, @Value);
    {$endif}
end;

procedure TGLSLUniform.SetValue(const Value: TMatrix4);
var
  J: Integer;
begin
  if (Owner <> nil) and (Name <> '') then
  begin
    Owner.SetRHIUniformValue(Name, Value.Columns[0]);
    for J := 0 to 3 do
      Owner.SetRHIUniformValue(Name + '[' + IntToStr(J) + ']', Value.Columns[J]);
  end;
  if Location = GLUniformLocationNone then Exit; // ignore non-existing uniform here
  if (GLFeatures = nil) or (not GLFeatures.Shaders) then Exit;
  Owner.Enable;
  if GLFeatures.Shaders then
    {$ifdef CASTLE_WEBGL}
    glUniformMatrix4fv(Location, GL_FALSE, MatrixToWebGL(Value), 0, 0);
    {$else}
    glUniformMatrix4fv(Location, 1, GL_FALSE, @Value);
    {$endif}
end;

procedure TGLSLUniform.SetValue(const Value: TBooleanList);
var
  Ints: TInt32List;
begin
  { We cannot pass Value.List, as Pascal booleans do not have 4 bytes
    (well, actually I could change this by compiler directive or
    by using LongBool for TBooleanList --- but for TBooleanList
    this would enlarge it 4 times, not nice).

    Unfortunately, there's no glUniform*ub (unsigned byte) or such function.

    So convert to 32 ints. }
  Ints := Value.ToInt32;
  try
    SetValue(Ints);
  finally FreeAndNil(Ints) end;
end;

procedure TGLSLUniform.SetValue(const Value: TInt32List);
var
  I: Integer;
  PackedVec: TVector4;
begin
  if (Owner <> nil) and (Name <> '') and (Value <> nil) then
  begin
    PackedVec := TVector4.Zero;
    for I := 0 to Value.Count - 1 do
    begin
      if I < 4 then
        PackedVec.Data[I] := Value[I];
      Owner.SetRHIUniformValue(Name + '[' + IntToStr(I) + ']', Vector4(Value[I], 0, 0, 0));
    end;
    Owner.SetRHIUniformValue(Name, PackedVec);
  end;
  if Location = GLUniformLocationNone then Exit; // ignore non-existing uniform here
  if (GLFeatures = nil) or (not GLFeatures.Shaders) then Exit;
  Assert(SizeOf(Int32) = SizeOf(TGLint));
  Owner.Enable;
  if GLFeatures.Shaders then
    {$ifdef CASTLE_WEBGL}
    glUniform1iv(Location, ListToWebGL(Value), 0, 0);
    {$else}
    glUniform1iv(Location, Value.Count, PGLInt(Value.L));
    {$endif}
end;

procedure TGLSLUniform.SetValue(const Value: TSingleList);
var
  I: Integer;
  PackedVec: TVector4;
begin
  if (Owner <> nil) and (Name <> '') and (Value <> nil) then
  begin
    PackedVec := TVector4.Zero;
    for I := 0 to Value.Count - 1 do
    begin
      if I < 4 then
        PackedVec.Data[I] := Value[I];
      Owner.SetRHIUniformValue(Name + '[' + IntToStr(I) + ']', Vector4(Value[I], 0, 0, 0));
    end;
    Owner.SetRHIUniformValue(Name, PackedVec);
  end;
  if Location = GLUniformLocationNone then Exit; // ignore non-existing uniform here
  if (GLFeatures = nil) or (not GLFeatures.Shaders) then Exit;
  Owner.Enable;
  if GLFeatures.Shaders then
    {$ifdef CASTLE_WEBGL}
    glUniform1fv(Location, ListToWebGL(Value), 0, 0);
    {$else}
    glUniform1fv(Location, Value.Count, PGLfloat(Value.L));
    {$endif}
end;

procedure TGLSLUniform.SetValue(const Value: TVector2List);
var
  I: Integer;
begin
  if (Owner <> nil) and (Name <> '') and (Value <> nil) and (Value.Count > 0) then
  begin
    Owner.SetRHIUniformValue(Name, Vector4(Value[0].X, Value[0].Y, 0, 0));
    for I := 0 to Value.Count - 1 do
      Owner.SetRHIUniformValue(Name + '[' + IntToStr(I) + ']', Vector4(Value[I].X, Value[I].Y, 0, 0));
  end;
  if Location = GLUniformLocationNone then Exit; // ignore non-existing uniform here
  if (GLFeatures = nil) or (not GLFeatures.Shaders) then Exit;
  Owner.Enable;
  if GLFeatures.Shaders then
    {$ifdef CASTLE_WEBGL}
    glUniform2fv(Location, ListToWebGL(Value), 0, 0);
    {$else}
    glUniform2fv(Location, Value.Count, PGLfloat(Value.L));
    {$endif}
end;

procedure TGLSLUniform.SetValue(const Value: TVector3List);
var
  I: Integer;
begin
  if (Owner <> nil) and (Name <> '') and (Value <> nil) and (Value.Count > 0) then
  begin
    Owner.SetRHIUniformValue(Name, Vector4(Value[0].X, Value[0].Y, Value[0].Z, 0));
    for I := 0 to Value.Count - 1 do
      Owner.SetRHIUniformValue(Name + '[' + IntToStr(I) + ']', Vector4(Value[I].X, Value[I].Y, Value[I].Z, 0));
  end;
  if Location = GLUniformLocationNone then Exit; // ignore non-existing uniform here
  if (GLFeatures = nil) or (not GLFeatures.Shaders) then Exit;
  Owner.Enable;
  if GLFeatures.Shaders then
    {$ifdef CASTLE_WEBGL}
    glUniform3fv(Location, ListToWebGL(Value), 0, 0);
    {$else}
    glUniform3fv(Location, Value.Count, PGLfloat(Value.L));
    {$endif}
end;

procedure TGLSLUniform.SetValue(const Value: TVector4List);
var
  I: Integer;
begin
  if (Owner <> nil) and (Name <> '') and (Value <> nil) and (Value.Count > 0) then
  begin
    Owner.SetRHIUniformValue(Name, Value[0]);
    for I := 0 to Value.Count - 1 do
      Owner.SetRHIUniformValue(Name + '[' + IntToStr(I) + ']', Value[I]);
  end;
  if Location = GLUniformLocationNone then Exit; // ignore non-existing uniform here
  if (GLFeatures = nil) or (not GLFeatures.Shaders) then Exit;
  Owner.Enable;
  if GLFeatures.Shaders then
    {$ifdef CASTLE_WEBGL}
    glUniform4fv(Location, ListToWebGL(Value), 0, 0);
    {$else}
    glUniform4fv(Location, Value.Count, PGLfloat(Value.L));
    {$endif}
end;

procedure TGLSLUniform.SetValue(const Value: TMatrix3List);
begin
  if Location = GLUniformLocationNone then Exit; // ignore non-existing uniform here
  if (GLFeatures = nil) or (not GLFeatures.Shaders) then Exit;
  Owner.Enable;
  if GLFeatures.Shaders then
    {$ifdef CASTLE_WEBGL}
    glUniformMatrix3fv(Location, GL_FALSE, ListToWebGL(Value), 0, 0);
    {$else}
    glUniformMatrix3fv(Location, Value.Count, GL_FALSE, PGLfloat(Value.L));
    {$endif}
end;

procedure TGLSLUniform.SetValue(const Value: TMatrix4List);
begin
  if Location = GLUniformLocationNone then Exit; // ignore non-existing uniform here
  if (GLFeatures = nil) or (not GLFeatures.Shaders) then Exit;
  Owner.Enable;
  if GLFeatures.Shaders then
    {$ifdef CASTLE_WEBGL}
    glUniformMatrix4fv(Location, GL_FALSE, ListToWebGL(Value), 0, 0);
    {$else}
    glUniformMatrix4fv(Location, Value.Count, GL_FALSE, PGLfloat(Value.L));
    {$endif}
end;

{ TGLSLAttribute ------------------------------------------------------------- }

procedure TGLSLAttribute.SetValue(const Value: TGLfloat);
begin
  if Location = GLAttribLocationNone then Exit; // ignore non-existing attribute here
  if (GLFeatures = nil) or (not GLFeatures.Shaders) then Exit;
  Owner.Enable;
  if GLFeatures.Shaders then
    glVertexAttrib1f(Location, Value);
end;

procedure TGLSLAttribute.SetValue(const Value: TVector2);
begin
  if Location = GLAttribLocationNone then Exit; // ignore non-existing attribute here
  if (GLFeatures = nil) or (not GLFeatures.Shaders) then Exit;
  Owner.Enable;
  if GLFeatures.Shaders then
    {$ifdef CASTLE_WEBGL}
    glVertexAttrib2f(Location, Value[0], Value[1]);
    {$else}
    glVertexAttrib2fv(Location, @Value);
    {$endif}
end;

procedure TGLSLAttribute.SetValue(const Value: TVector3);
begin
  if Location = GLAttribLocationNone then Exit; // ignore non-existing attribute here
  if (GLFeatures = nil) or (not GLFeatures.Shaders) then Exit;
  Owner.Enable;
  if GLFeatures.Shaders then
    {$ifdef CASTLE_WEBGL}
    glVertexAttrib3f(Location, Value[0], Value[1], Value[2]);
    {$else}
    glVertexAttrib3fv(Location, @Value);
    {$endif}
end;

procedure TGLSLAttribute.SetValue(const Value: TVector4);
begin
  if Location = GLAttribLocationNone then Exit; // ignore non-existing attribute here
  if (GLFeatures = nil) or (not GLFeatures.Shaders) then Exit;
  Owner.Enable;
  if GLFeatures.Shaders then
    {$ifdef CASTLE_WEBGL}
    glVertexAttrib4f(Location, Value[0], Value[1], Value[2], Value[3]);
    {$else}
    glVertexAttrib4fv(Location, @Value);
    {$endif}
end;

procedure TGLSLAttribute.SetValue(const Value: TMatrix3);

  procedure SetColumn(const Column: Integer; const Value: TMatrix3.TIndexArray);
  begin
    {$ifdef CASTLE_WEBGL}
    glVertexAttrib3f(Location + Column, Value[0], Value[1], Value[2]);
    {$else}
    glVertexAttrib3fv(Location + Column, @Value);
    {$endif}
  end;

begin
  if Location = GLAttribLocationNone then Exit; // ignore non-existing attribute here
  if (GLFeatures = nil) or (not GLFeatures.Shaders) then Exit;
  Owner.Enable;
  if GLFeatures.Shaders then
  begin
    SetColumn(0, Value.Data[0]);
    SetColumn(1, Value.Data[1]);
    SetColumn(2, Value.Data[2]);
  end;
end;

procedure TGLSLAttribute.SetValue(const Value: TMatrix4);

  procedure SetColumn(const Column: Integer; const Value: TMatrix4.TIndexArray);
  begin
    {$ifdef CASTLE_WEBGL}
    glVertexAttrib4f(Location + Column, Value[0], Value[1], Value[2], Value[3]);
    {$else}
    glVertexAttrib4fv(Location, @Value);
    {$endif}
  end;

begin
  if Location = GLAttribLocationNone then Exit; // ignore non-existing attribute here
  if (GLFeatures = nil) or (not GLFeatures.Shaders) then Exit;
  Owner.Enable;
  if GLFeatures.Shaders then
  begin
    SetColumn(0, Value.Data[0]);
    SetColumn(1, Value.Data[1]);
    SetColumn(2, Value.Data[2]);
    SetColumn(3, Value.Data[3]);
  end;
end;

{$ifndef OpenGLES}
procedure TGLSLAttribute.SetValue(const Value: TVector4Integer);
begin
  if Location = GLAttribLocationNone then Exit; // ignore non-existing attribute here
  if (GLFeatures = nil) or (not GLFeatures.Shaders) then Exit;
  Owner.Enable;
  if GLFeatures.Shaders then
    glVertexAttrib4iv(Location, @Value);
end;

procedure TGLSLAttribute.SetValue(const Value: TVector4Byte);
begin
  if Location = GLAttribLocationNone then Exit; // ignore non-existing attribute here
  if (GLFeatures = nil) or (not GLFeatures.Shaders) then Exit;
  Owner.Enable;
  if GLFeatures.Shaders then
    glVertexAttrib4ubv(Location, @Value);
end;

procedure TGLSLAttribute.SetValue(const Value: TGLdouble);
begin
  if Location = GLAttribLocationNone then Exit; // ignore non-existing attribute here
  if (GLFeatures = nil) or (not GLFeatures.Shaders) then Exit;
  Owner.Enable;
  if GLFeatures.Shaders then
    glVertexAttrib1d(Location, Value);
end;

// Makes FPC errors: Error: Asm: Duplicate label, see https://bugs.freepascal.org/view.php?id=32188
{
procedure TGLSLAttribute.SetValue(const Value: TVector2Double);
begin
  if Location = GLAttribLocationNone then Exit; // ignore non-existing attribute here
  Owner.Enable;
  if GLFeatures.Shaders then
    glVertexAttrib2dv(Location, @Value);
end;

procedure TGLSLAttribute.SetValue(const Value: TVector3Double);
begin
  if Location = GLAttribLocationNone then Exit; // ignore non-existing attribute here
  Owner.Enable;
  if GLFeatures.Shaders then
    glVertexAttrib3dv(Location, @Value);
end;

procedure TGLSLAttribute.SetValue(const Value: TVector4Double);
begin
  if Location = GLAttribLocationNone then Exit; // ignore non-existing attribute here
  Owner.Enable;
  if GLFeatures.Shaders then
    glVertexAttrib4dv(Location, @Value);
end;
}
{$endif}

procedure TGLSLAttribute.EnableArray(const Vao: TVertexArrayObject;
  const LocationOffset: TLocationOffset;
  const Size: TGLint; const AType: TGLenum; const Normalized: TGLboolean; const Stride: TGLsizei;
  const Ptr: PtrUInt);
begin
  if Location = GLAttribLocationNone then Exit; // ignore non-existing attribute here
  if (GLFeatures <> nil) and GLFeatures.Shaders then
  begin
    RenderContext.CurrentVao := Vao;
    RenderContext.CurrentProgram := Owner;

    LocationOffsetsToDisable[LocationOffset] := true;
    glEnableVertexAttribArray(Location + LocationOffset);
    glVertexAttribPointer    (Location + LocationOffset, Size, AType,
      Normalized, Stride, {$ifndef CASTLE_WEBGL} Pointer {$endif} (Ptr));
  end;
end;

procedure TGLSLAttribute.EnableArraySingle(const Vao: TVertexArrayObject;
  const Stride: TGLsizei; const Ptr: PtrUInt);
begin
  EnableArray(Vao, 0, 1, GL_FLOAT, GL_FALSE, Stride, Ptr);
end;

procedure TGLSLAttribute.EnableArrayVector2(const Vao: TVertexArrayObject;
  const Stride: TGLsizei; const Ptr: PtrUInt);
begin
  EnableArray(Vao, 0, 2, GL_FLOAT, GL_FALSE, Stride, Ptr);
end;

procedure TGLSLAttribute.EnableArrayVector3(const Vao: TVertexArrayObject;
  const Stride: TGLsizei; const Ptr: PtrUInt);
begin
  EnableArray(Vao, 0, 3, GL_FLOAT, GL_FALSE, Stride, Ptr);
end;

procedure TGLSLAttribute.EnableArrayVector4(const Vao: TVertexArrayObject;
  const Stride: TGLsizei; const Ptr: PtrUInt);
begin
  EnableArray(Vao, 0, 4, GL_FLOAT, GL_FALSE, Stride, Ptr);
end;

procedure TGLSLAttribute.EnableArrayMatrix3(const Vao: TVertexArrayObject;
  const Stride: TGLsizei; const Ptr: PtrUInt);
begin
  EnableArray(Vao, 0, 3, GL_FLOAT, GL_FALSE, Stride, Ptr);
  EnableArray(Vao, 1, 3, GL_FLOAT, GL_FALSE, Stride, Ptr + SizeOf(TVector3));
  EnableArray(Vao, 2, 3, GL_FLOAT, GL_FALSE, Stride, Ptr + SizeOf(TVector3) * 2);
end;

procedure TGLSLAttribute.EnableArrayMatrix4(const Vao: TVertexArrayObject;
  const Stride: TGLsizei; const Ptr: PtrUInt);
begin
  EnableArray(Vao, 0, 4, GL_FLOAT, GL_FALSE, Stride, Ptr);
  EnableArray(Vao, 1, 4, GL_FLOAT, GL_FALSE, Stride, Ptr + SizeOf(TVector4));
  EnableArray(Vao, 2, 4, GL_FLOAT, GL_FALSE, Stride, Ptr + SizeOf(TVector4) * 2);
  EnableArray(Vao, 3, 4, GL_FLOAT, GL_FALSE, Stride, Ptr + SizeOf(TVector4) * 3);
end;

procedure TGLSLAttribute.DisableArray;
var
  Offset: TLocationOffset;
begin
  if Location = GLAttribLocationNone then Exit; // ignore non-existing attribute here

  if (GLFeatures <> nil) and GLFeatures.Shaders then
    for Offset := Low(TLocationOffset) to High(TLocationOffset) do
      if LocationOffsetsToDisable[Offset] then
        glDisableVertexAttribArray(Location + Offset);
end;

{ TGLSLProgram --------------------------------------------------------------- }

constructor TGLSLProgram.Create;
var
  ShaderType: TShaderType;
begin
  inherited;

  if (GLFeatures <> nil) and GLFeatures.Shaders then
  begin
    ProgramId := glCreateProgram();

    { Program id = 0 means that an error occurred. glCreateProgram docs say

        "This function returns 0 if an error occurs creating the program object."

      Looking at glGetError, I don't see any error code there.
      So I just have to raise enigmatic error that creating GLSL program failed.
    }

    if ProgramId = GLObjectNone then
      raise EGLSLError.Create('Cannot create GLSL shader program');
  end;

  if ApplicationProperties <> nil then
    ApplicationProperties.OnGLContextCloseObject.Add({$ifdef FPC}@{$endif}OnGlContextClose);

  ShaderIds := TGLShaderList.Create;

  FUniformMissing := umWarning;

  FUniformLocations := TUniformLocationCache.Create;
  FAttributeLocations := TAttribLocationCache.Create;
  FRHIUniformValues := TRHIUniformValueMap.Create;
  FRHIUniformOrder := TStringList.Create;
  FRHIUniformOrder.CaseSensitive := True;

  for ShaderType := Low(TShaderType) to High(TShaderType) do
    FSource[ShaderType] := TStringList.Create;
end;

procedure TGLSLProgram.GLContextClose;
begin
  { This is called from destructor,
    which may be called if exception is raised from constructor,
    so be careful to check here things -- e.g. check that
    ShaderIds was created.

    Note that, if resources have already been freed, we try to not assume
    that GLFeatures is <> nil. So you can free TGLSLProgram even after
    context is closed.
  }

  if ShaderIds <> nil then
    DetachAllShaders;

  FreeAndNil(FRHI2DFragmentShader);

  { Deleting by glDeleteProgram the shader that is currently used
    (by glUseProgram) is allowed in OpenGL, and it implicitly sets
    the used program to 0. Which is absolutely reasonable.

    We need to make sure that RenderContext knowledge corresponds to the
    OpenGL state. So RenderContext.CurrentProgram must become nil.

    Moreover, RenderContext.CurrentProgram cannot point to non-existing
    object. Again, this means that RenderContext.CurrentProgram must become nil.

    If we would ignore 2 reasons above, we could have error:

    - RenderContext.CurrentProgram containing a dangling pointer,
    - and this pointer could be reused for new TGLSLProgram instance,
    - and then "RenderContext.CurrentProgram := ..."
      would do nothing (because FCurrenProgram = Value in the setter)
    - and the new shader would not become active.
    - Causing OpenGL errors when not setting uniforms, because shader program
      would be unset before callling glSetUniform*.
    - See https://github.com/castle-engine/castle-engine/issues/664
      for 3 testcases that show this problem, compounded with overly
      eager recreation of shaders.

    Note: checking RenderContext <> nil to make sure we do nothing when
    we're doing TGLSLProgram.Destroy after FreeAndNil(RenderContext). }
  if (RenderContext <> nil) and
     (RenderContext.CurrentProgram = Self) then
    RenderContext.CurrentProgram := nil;

  if ProgramId <> GLObjectNone then
  begin
    { ProgramId can be non-zero only if GLFeatures.Shaders,
      and right now GLFeatures must be <> nil because we must free while
      GL context is available. }
    Assert((GLFeatures <> nil) and GLFeatures.Shaders);
    glDeleteProgram(ProgramId);
    ProgramId := GLObjectNone;
  end;

  if FUniformLocations <> nil then
    FUniformLocations.Clear;
  if FAttributeLocations <> nil then
    FAttributeLocations.Clear;

  {$ifdef CASTLE_CANNOT_CATCH_EXCEPTIONS}
  ErrorOnCompileLink := false;
  {$endif}
end;

procedure TGLSLProgram.OnGlContextClose(Sender: TObject);
begin
  GLContextClose;
end;

destructor TGLSLProgram.Destroy;
var
  ShaderType: TShaderType;
begin
  if ApplicationProperties <> nil then
    ApplicationProperties.OnGLContextCloseObject.Remove({$ifdef FPC}@{$endif}OnGlContextClose);
  GLContextClose;

  for ShaderType := Low(TShaderType) to High(TShaderType) do
    FreeAndNil(FSource[ShaderType]);

  FreeAndNil(FRHI2DFragmentShader);
  FreeAndNil(FRHIUniformOrder);
  FreeAndNil(FRHIUniformValues);
  FreeAndNil(ShaderIds);
  FreeAndNil(FUniformLocations);
  FreeAndNil(FAttributeLocations);
  FreeAndNil(FUniformReportedMissing);

  inherited;
end;

function TGLSLProgram.ProgramInfoLog: string;
begin
  if (GLFeatures <> nil) and GLFeatures.Shaders then
    Result := GetProgramInfoLog(ProgramId)
  else
    Result := '';
end;

function TGLSLProgram.DebugInfo: string;
{$ifdef CASTLE_WEBGL}
begin
  Result := 'GLSL program support: ' + BoolToStr((GLFeatures <> nil) and GLFeatures.Shaders, true);

  // TODO: web: Restore full DebugInfo functionality with WebGL
{$else}

  function GLShaderVariableTypeName(AType: TGLenum): string;
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
      {$ifndef OpenGLES}
      GL_FLOAT_MAT2x3: Result := 'FLOAT_MAT2x3';
      GL_FLOAT_MAT2x4: Result := 'FLOAT_MAT2x4';
      GL_FLOAT_MAT3x2: Result := 'FLOAT_MAT3x2';
      GL_FLOAT_MAT3x4: Result := 'FLOAT_MAT3x4';
      GL_FLOAT_MAT4x2: Result := 'FLOAT_MAT4x2';
      GL_FLOAT_MAT4x3: Result := 'FLOAT_MAT4x3';
      GL_SAMPLER_1D: Result := 'SAMPLER_1D';
      {$endif}
      GL_SAMPLER_2D: Result := 'SAMPLER_2D';
      {$ifndef OpenGLES} GL_SAMPLER_3D: Result := 'SAMPLER_3D'; {$endif}
      GL_SAMPLER_CUBE: Result := 'SAMPLER_CUBE';
      {$ifndef OpenGLES} GL_SAMPLER_1D_SHADOW: Result := 'SAMPLER_1D_SHADOW'; {$endif}
      GL_SAMPLER_2D_SHADOW: Result := 'SAMPLER_2D_SHADOW';
      {$ifndef OpenGLES}
      GL_SAMPLER_2D_RECT: Result := 'SAMPLER_2D_RECT';
      GL_SAMPLER_2D_RECT_SHADOW: Result := 'SAMPLER_2D_RECT_SHADOW';
      GL_INT_SAMPLER_2D_RECT: Result := 'INT_SAMPLER_2D_RECT';
      GL_UNSIGNED_INT_SAMPLER_2D_RECT: Result := 'UNSIGNED_INT_SAMPLER_2D_RECT';
      GL_SAMPLER_2D_MULTISAMPLE: Result := 'SAMPLER_2D_MULTISAMPLE';
      {$endif}
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
    Name: Utf8String;
    ErrorCode: TGLenum;
  begin
    if (GLFeatures <> nil) and GLFeatures.Shaders then
    begin
      {$ifdef CASTLE_WEBGL}
      UniformsCount := glGetProgramParameter(ProgramId, GL_ACTIVE_UNIFORMS);
      {$else}
      glGetProgramiv(ProgramId, GL_ACTIVE_UNIFORMS, @UniformsCount);
      {$endif}

      { fglrx (Radeon closed-source OpenGL drivers) are buggy (that's not
        news, I know...) and they report GL_INVALID_ENUM on some queries
        here. We detect this, and still produce nice debug message, without
        raising any exception (after all, we have to workaround fglrx bugs,
        and try to run our program anyway...).
        For this we have to check first whether there are any OpenGL errors
        pending, and raise exceptions on them. }
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
        glGetActiveUniform(ProgramId, I, UniformMaxLength,
          {$ifndef USE_DGL}@{$endif} ReturnedLength,
          {$ifndef USE_DGL}@{$endif} Size,
          {$ifndef USE_DGL}@{$endif} AType,
          PAnsiCharOrNil(Name));
        SetLength(Name, ReturnedLength);
        UniformNames.Append(Format('  %d: Name: %s, type: %s, size: %d',
          [I, Name, GLShaderVariableTypeName(AType), Size]));
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
    Name: Utf8String;
    ErrorCode: TGLenum;
  begin
    if (GLFeatures <> nil) and GLFeatures.Shaders then
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
        glGetActiveAttrib(ProgramId, I, AttribMaxLength,
          {$ifndef USE_DGL}@{$endif} ReturnedLength,
          {$ifndef USE_DGL}@{$endif} Size,
          {$ifndef USE_DGL}@{$endif} AType,
          PAnsiCharOrNil(Name));
        SetLength(Name, ReturnedLength);
        AttribNames.Append(Format('  %d: Name: %s, type: %s, size: %d',
          [I, Name, GLShaderVariableTypeName(AType), Size]));
      end;
    end;
  end;

  function ShaderInfoLog(ShaderId: TGLShader): string;
  begin
    if (GLFeatures <> nil) and GLFeatures.Shaders then
      Result := GetShaderInfoLog(ShaderId)
    else
      Result := '';
  end;

var
  S: TStringList;
  I: Integer;
begin
  Result := 'GLSL program support: ' + BoolToStr((GLFeatures <> nil) and GLFeatures.Shaders, true);

  if (GLFeatures <> nil) and GLFeatures.Shaders then
    CheckGLErrors('Check at the beginning of TGLSLProgram.DebugInfo');

  S := TStringList.Create;
  try
    GetActiveUniforms(S);
    Result := Result + NL + 'Active uniforms:' + NL + S.Text;

    S.Clear;
    GetActiveAttribs(S);
    Result := Result +  NL + 'Active attribs:' + NL + S.Text ;
  finally FreeAndNil(S) end;

  for I := 0 to ShaderIds.Count - 1 do
  begin
    Result := Result +  NL + Format('Shader number %d (OpenGL id %d) log:',
      [I, ShaderIds[I]]) + NL +
      ShaderInfoLog(ShaderIds[I]) ;
  end;

  Result := Result +  NL + 'Program info log:' + NL + ProgramInfoLog;

  Result := Result + NL + 'Program detected as running in hardware: ' +
    BoolToStr(RunningInHardware, true);
{$endif CASTLE_WEBGL}
end;

procedure TGLSLProgram.AttachShader(const ShaderType: TShaderType; S: string);
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
  procedure ReportCompileAccessViolation;
  var
    Message: String;
  begin
    Message := Format('%s shader not compiled, segmentation fault in glCompileShader call. Buggy OpenGL GLSL compiler.',
      [ShaderTypeName[ShaderType]]);

    {$ifdef CASTLE_CANNOT_CATCH_EXCEPTIONS}
    WritelnWarning('GLSL', Message);
    ErrorOnCompileLink := true;
    {$else}
    raise EGLSLShaderCompileError.Create(Message);
    {$endif}
  end;

  procedure ReportCompileError(const CompileErrorMessage: String);
  var
    Message: String;
  begin
    Message := Format('%s shader not compiled:' + NL +
      '%s',
      [ShaderTypeName[ShaderType], CompileErrorMessage]);

    Message := Message + NL +
      NL +
      'The shader source code is below:' + NL +
      '-------------------------------------------------------' + NL +
      S + NL +
      '-------------------------------------------------------';

    {$ifdef CASTLE_CANNOT_CATCH_EXCEPTIONS}
    WritelnWarning('GLSL', Message);
    ErrorOnCompileLink := true;
    {$else}
    raise EGLSLShaderCompileError.Create(Message);
    {$endif}
  end;

  { Create a shader, adding sources and compiling it.

    Based on OpenGL code by Dean Ellis BasicShader.dpr,
    many adjustments later - to work with OpenGL / OpenGL ES / WebGL,
    using our CastleGL unit, FPC / Delphi compatibility. }
  function CreateShader(const S: string): TGLshader;
  var
    {$ifndef CASTLE_WEBGL}
    SrcPtr: PGLChar;
    SrcLength: Cardinal;
    Source8: Utf8String;
    CompiledInt: TGLint;
    {$endif not CASTLE_WEBGL}
    Compiled: Boolean;
  begin
    Result := glCreateShader(AType);

    { Call glShaderSource to pass source code S to shader.
      This is trivial in WebGL, but requires some work for OpenGL / OpenGLES
      to express string data as needed.
      To work with both String=AnsiString and String=UnicodeString,
      we use Utf8String to communicate with the OpenGL API. }
    {$ifdef CASTLE_WEBGL}
    glShaderSource(Result, S);
    {$else}
    Source8 := S;
    SrcPtr := PGLChar(Source8);
    SrcLength := Length(Source8);
    glShaderSource(Result, 1, @SrcPtr, @SrcLength);
    {$endif}

    try
      glCompileShader(Result);
    except
      on E: EAccessViolation do ReportCompileAccessViolation;
    end;

    {$ifdef CASTLE_WEBGL}
    Compiled := glGetShaderParameter(Result, GL_COMPILE_STATUS);
    {$else}
    glGetShaderiv(Result, GL_COMPILE_STATUS, @CompiledInt);
    Compiled := CompiledInt = Ord(GL_TRUE);
    {$endif}

    { Although we generally avoid creating multiline exception messages,
      ShaderGetInfoLog naturally comes out multiline (it contains a
      couple of error messages) and it's best presented with line breaks.
      So a line break right before ShaderGetInfoLog contents looks good. }
    if not Compiled then
      ReportCompileError(GetShaderInfoLog(Result));
  end;

const
  {$ifdef OpenGLES}
  GLES30CoreHeader: array [TShaderType] of String = (
    // vertex
    '#version 300 es' + NL +
    '#define CASTLE_GLSL_VERSION_UPGRADE' + NL +
    '#define attribute in' + NL +
    '#define varying out' + NL +
    '#define texture2D texture' + NL +
    '#define texture3D texture' + NL +
    '#define textureCube texture' + NL +
    '#define texture2DProj textureProj' + NL +
    '#define texture3DProj textureProj' + NL +
    { Otherwise each sampler2DShadow would have to contain precision specifier.
      EXT_shadow_samplers says that lowp is default, so presumably it is OK:
      https://registry.khronos.org/OpenGL/extensions/EXT/EXT_shadow_samplers.txt }
    'precision lowp sampler2DShadow;' + NL +
    { Otherwise each sampler3D would have to contain precision specifier.
      3D textures are part of OpenGLES 3.0 core, so it's OK to use sampler3D here.
      Testcase that this is needed: castle-model-viewer, compiled with OpenGLES,
      viewing demo-models/water/caustics/Barna29.x3dv . }
    'precision lowp sampler3D;' + NL,

    // geometry - not supported by OpenGLES
    '#define CASTLE_GLSL_VERSION_UPGRADE' + NL,

    // fragment
    '#version 300 es' + NL +
    '#define CASTLE_GLSL_VERSION_UPGRADE' + NL +
    '#define varying in' + NL +
    '#define texture2D texture' + NL +
    '#define texture3D texture' + NL +
    '#define textureCube texture' + NL +
    '#define texture2DProj textureProj' + NL +
    '#define texture3DProj textureProj' + NL +
    '#define gl_FragColor castle_FragColor' + NL +
    'out mediump vec4 castle_FragColor;' + NL +
    { We need to use highp for shadow maps processing on mobile,
      to make the shadows look good,
      see https://github.com/castle-engine/castle-engine/pull/674 }
    '#define shadowsPrecision highp' + NL +
    'precision lowp sampler2DShadow;' + NL +
    'precision lowp sampler3D;' + NL
  );
  {$else}
  GL31CoreHeader: array [TShaderType] of String = (
    // vertex
    '#version 140' + NL +
    '#define CASTLE_GLSL_VERSION_UPGRADE' + NL +
    '#define attribute in' + NL +
    '#define varying out' + NL +
    '#define texture2D texture' + NL +
    '#define texture3D texture' + NL +
    '#define textureCube texture' + NL +
    '#define texture2DProj textureProj' + NL +
    '#define texture3DProj textureProj' + NL,

    // geometry
    '#version 150' + NL +
    '#define CASTLE_GLSL_VERSION_UPGRADE' + NL,

    // fragment
    '#version 140' + NL +
    '#define CASTLE_GLSL_VERSION_UPGRADE' + NL +
    '#define varying in' + NL +
    '#define texture2D texture' + NL +
    '#define texture3D texture' + NL +
    '#define textureCube texture' + NL +
    '#define texture2DProj textureProj' + NL +
    '#define texture3DProj textureProj' + NL +
    '#define gl_FragColor castle_FragColor' + NL +
    'out mediump vec4 castle_FragColor;' + NL +
    '#define shadowsPrecision' + NL
  );
  {$endif}

  { Does shader code Text has a line starting with LinePrefix. }
  function HasLine(const LinePrefix: String; const Text: String): Boolean;
  begin
    Result :=
      (Pos(#13 + LinePrefix, S) <> 0) or
      (Pos(#10 + LinePrefix, S) <> 0) or
      IsPrefix(LinePrefix, S, false);
  end;

  { Workaround bug of old ANGLE version in Firefox,
    that requires main() to be after all "varying" declarations.
    See
    https://bugzilla.mozilla.org/show_bug.cgi?id=2039887
    https://github.com/michaliskambi/firefox-webgl-angle-varying-bug

    Shaders where this is necessary (only vertex shaders
    are affected by this bug, and only if we'll generate
    there code using our "plugs" system) should have markers
    "/* CASTLE-MAIN-BEGIN */" and "/* CASTLE-MAIN-END */.
    Example: src/scene/glsl/source/main_shading_phong.vs .

    Not doing this would break many features of our engine shaders
    using "varying" variables, like
    - clip planes (to implement X3D ClipPlane node) (testcase: pope call in Unholy)
    - bump mapping
    - fog
    - water in examples/terrain/
    - time to shader (in castle-model-viewer-mobile demo)
  }
  procedure WebGLMoveMainBlockToEnd;
  {$ifdef CASTLE_WEBGL}
  const
    SMarkerBegin = '/* CASTLE-MAIN-BEGIN */';
    SMarkerEnd = '/* CASTLE-MAIN-END */';
  var
    MarkerBegin, MarkerEnd: Integer;
    Main: String;
  begin
    MarkerBegin := Pos(SMarkerBegin, S);
    if MarkerBegin = 0 then Exit;
    MarkerEnd := Pos(SMarkerEnd, S);
    if MarkerEnd = 0 then Exit;
    Main := CopyPos(S, MarkerBegin, MarkerEnd + Length(SMarkerEnd) - 1);
    Delete(S, MarkerBegin, MarkerEnd + Length(SMarkerEnd) - MarkerBegin);
    S := S + NL + Main;
  {$else}
  begin
  {$endif}
  end;

const
  { FPC < 2.4.4 doesn't have it defined }
  GL_GEOMETRY_SHADER = $8DD9;
var
  ShaderId: TGLShader;
begin
  if (GLFeatures = nil) or (not GLFeatures.Shaders) then
  begin
    if FSource[ShaderType] <> nil then
      FSource[ShaderType].Add(S);
    HasStage[ShaderType] := true;
    if ShaderType = stFragment then
    begin
      FreeAndNil(FRHI2DFragmentShader);
      FRHI2DAdaptedFragmentSource := '';
      SetLength(FRHI2DUniformMappings, 0);
    end;
    Exit;
  end;

  { calculate AType }
  case ShaderType of
    stVertex:
      AType := GL_VERTEX_SHADER;
    stGeometry:
      begin
        AType := GL_GEOMETRY_SHADER;
        if not GLVersion.AtLeast(3, 2) then
          { raise an error if Shaders supported but not geometry shaders. }
          raise EGLSLShaderCompileError.Create('Geometry shaders not supported by your OpenGL version');
      end;
    stFragment:
      begin
        AType := GL_FRAGMENT_SHADER;
        {$ifdef OpenGLES}
        if (not HasLine('precision mediump float;', S)) and
           (not HasLine('precision lowp float;', S)) and
           (not HasLine('precision highp float;', S)) then
          S := 'precision mediump float;' + NL + S;
        {$endif}
      end;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TGLSLProgram.AttachShader ShaderType?');
    {$endif}
  end;

  { Add #version.
    Done after adding "precision ..." for OpenGLES fragment shader,
    as #version must be earlier. }
  if InternalUpgradeGlslVersion and
     GLFeatures.
       {$if defined(CASTLE_WEBGL)} VersionWebGL_2
       {$elseif defined(OpenGLES)} VersionES_3_0
       {$else} Version_3_1
       {$endif} and
     (not HasLine('#version ', S)) then
    S := {$ifdef OpenGLES} GLES30CoreHeader {$else} GL31CoreHeader {$endif}
      [ShaderType] + S;

  WebGLMoveMainBlockToEnd;

  ShaderId := CreateShader(S);

  {$ifdef CASTLE_CANNOT_CATCH_EXCEPTIONS}
  if ErrorOnCompileLink then Exit;
  {$endif}

  glAttachShader(ProgramId, ShaderId);
  ShaderIds.Add(ShaderId);

  if FSource[ShaderType] <> nil then
    FSource[ShaderType].Add(S);
  HasStage[ShaderType] := true;
end;

procedure TGLSLProgram.AttachShader(const ShaderType: TShaderType;
  const Parts: TStrings);
var
  I: Integer;
  {$ifdef OpenGLES}
  Concatenated: string;
  {$endif}
begin
  {$ifdef OpenGLES}
  Concatenated := '';
  for I := 0 to Parts.Count - 1 do
    Concatenated := Concatenated +  Parts[I] + NL ;
  if Trim(Concatenated) <> '' then
    AttachShader(ShaderType, Concatenated);
  {$else}
  for I := 0 to Parts.Count - 1 do
    AttachShader(ShaderType, Parts[I]);
  {$endif}
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

procedure TGLSLProgram.SetTransformFeedbackVaryings(
  const Varyings: array of String; const IsSingleBufferMode: Boolean);
var
  TransformFeedbackBufferMode, ErrorCode: TGLuint;
  VaryingLength: Cardinal;
  {$ifndef CASTLE_WEBGL}
  I: Integer;
  Varyings8: array of Utf8String;
  Varyings8Ptr: array of PAnsiChar;
  {$endif}
begin;
  VaryingLength := Length(Varyings);
  if VaryingLength > 0 then
  begin
    if (GLFeatures = nil) or (not GLFeatures.TransformFeedbackVaryings) then
      raise EGLSLTransformFeedbackError.Create('Transform Feedback not supported by this context (too low OpenGL / OpenGLES / WebGL version)');
    if IsSingleBufferMode then
      TransformFeedbackBufferMode := GL_INTERLEAVED_ATTRIBS
    else
      TransformFeedbackBufferMode := GL_SEPARATE_ATTRIBS;

    // call transformFeedbackVaryings
    {$ifdef CASTLE_WEBGL}
    GL2.transformFeedbackVaryings(ProgramId, ListToWebGL(Varyings), TransformFeedbackBufferMode);
    {$else}
    SetLength(Varyings8, VaryingLength);
    SetLength(Varyings8Ptr, VaryingLength);
    for I := 0 to VaryingLength - 1 do
    begin
      Varyings8[I] := Varyings[I];
      Varyings8Ptr[I] := PAnsiChar(Varyings8[I]);
    end;
    glTransformFeedbackVaryings(ProgramId, VaryingLength, @Varyings8Ptr[0], TransformFeedbackBufferMode);
    {$endif}

    ErrorCode := glGetError();
    if ErrorCode = GL_INVALID_VALUE then
    begin
      raise EGLSLTransformFeedbackError.Create('Error occured after setting transform feedback varyings');
    end;
  end;
end;

procedure TGLSLProgram.DetachAllShaders;
var
  I: Integer;
  ShaderType: TShaderType;
begin
  for I := 0 to ShaderIds.Count - 1 do
  begin
    { ShaderIds can be non-empty only if GLFeatures.Shaders,
      and right now GLFeatures must be <> nil because we must free while
      GL context is available. }
    Assert((GLFeatures <> nil) and GLFeatures.Shaders);
    glDetachShader(ProgramId, ShaderIds[I]);
    glDeleteShader(ShaderIds[I]);
  end;

  ShaderIds.Count := 0;
  FreeAndNil(FRHI2DFragmentShader);
  FRHI2DAdaptedFragmentSource := '';
  SetLength(FRHI2DUniformMappings, 0);

  for ShaderType := Low(TShaderType) to High(TShaderType) do
  begin
    if FSource[ShaderType] <> nil then
      FSource[ShaderType].Clear;
    HasStage[ShaderType] := false;
  end;
end;

procedure TGLSLProgram.Link;

  { Outputs shader source code, if collected.
    Returns empty string if not collected.
    If collected, this is multiline string that *starts* with newline,
    and does not end with newline. }
  function LogShaderCode: String;
  {$ifdef CASTLE_COLLECT_SHADER_SOURCE}
  var
    ShaderType: TShaderType;
    I: Integer;
  begin
    Result := NL + Format('Shader "%s" source code:', [Name]) + NL;
    for ShaderType := Low(TShaderType) to High(TShaderType) do
      for I := 0 to FSource[ShaderType].Count - 1 do
        Result := Result + Format(
          '%s [%d] -------------------------------------------------------' + NL +
          '%s' + NL,
          [ ShaderTypeName[ShaderType], I, FSource[ShaderType][I] ] );
    Result := Result +
      'End of shader source code -------------------------------------------------------';
  {$else}
  begin
    Result := '';
  {$endif CASTLE_COLLECT_SHADER_SOURCE}
  end;

  procedure ReportLinkError(const LinkErrorMessage: String);
  var
    Message: String;
  begin
    Message := Format('Shader "%s" not linked:', [Name]) + NL +
      LinkErrorMessage +
      LogShaderCode;

    {$ifdef CASTLE_CANNOT_CATCH_EXCEPTIONS}
    WritelnWarning('GLSL', Message);
    ErrorOnCompileLink := true;
    {$else}
    raise EGLSLProgramLinkError.Create(Message);
    {$endif}
  end;

var
  {$ifndef CASTLE_WEBGL}
  LinkedInt: TGLuint;
  {$endif}
  Linked: Boolean;
begin
  if (GLFeatures <> nil) and GLFeatures.Shaders then
  begin
    { Produce clear error messages, without waiting for OpenGL errors,
      if required shaders are not attached. }
    if not HasStage[stVertex] then
      raise EGLSLProgramLinkError.Create('Vertex shader not attached');
    if FragmentShaderRequired and (not HasStage[stFragment]) then
      raise EGLSLProgramLinkError.Create('Fragment shader not attached');

    glLinkProgram(ProgramId);

    {$ifdef CASTLE_WEBGL}
    Linked := glGetProgramParameter(ProgramId, GL_LINK_STATUS);
    {$else}
    glGetProgramiv(ProgramId, GL_LINK_STATUS, @LinkedInt);
    Linked := LinkedInt = Ord(GL_TRUE);
    {$endif}

    if not Linked then
      // raises exception
      ReportLinkError(GetProgramInfoLog(ProgramId));

    {$ifdef CASTLE_CANNOT_CATCH_EXCEPTIONS}
    if ErrorOnCompileLink then Exit;
    {$endif}

    if LogShaders then
      WritelnLogMultiline('GLSL',
        Format('Shader "%s" successfully linked:', [Name]) + NL +
        'Information:' + NL +
        DebugInfo +
        LogShaderCode);
  end;
end;

procedure TGLSLProgram.Link(Ignored: boolean);
begin
  Link;
end;

function TGLSLProgram.RunningInHardware: boolean;
begin
  { This is good at least for capturing shaders running in software on
      Radeon X300/X550/X1050 Series (crypto on ii.324), Linux fglrx. }

  Result := Pos('shader will run in software due to the', ProgramInfoLog) = 0;
end;

procedure TGLSLProgram.Enable;
begin
  if RenderContext <> nil then
    RenderContext.CurrentProgram := Self;
end;

class procedure TGLSLProgram.Disable;
begin
  if RenderContext <> nil then
    RenderContext.CurrentProgram := nil;
end;

function TGLSLProgram.SetupUniforms(var BoundTextureUnits: Cardinal): boolean;
begin
  Result := true;
end;

function TGLSLProgram.Uniform(const AName: string): TGLSLUniform;
begin
  Result := Uniform(AName, UniformMissing);
end;

function TGLSLProgram.Uniform(const AName: string; const AUniformMissing: TUniformMissing): TGLSLUniform;

  procedure ReportUniformMissing;

    function ErrMessage: string;
    begin
      Result := Format('Uniform variable "%s" not found (or not used) in the shader source code', [AName]);
    end;

  begin
    if FUniformReportedMissing = nil then
    begin
      FUniformReportedMissing := TStringList.Create;
      FUniformReportedMissing.CaseSensitive := true; // GLSL uniform names are case-sensitive
    end;
    // don't spam the log with warning "Uniform variable "%s" not found (or not used)"... every frame
    if FUniformReportedMissing.IndexOf(AName) <> -1 then
      Exit;
    FUniformReportedMissing.Add(AName);

    case AUniformMissing of
      umWarning  : WritelnWarning('GLSL', ErrMessage);
      // This was implemented in the past, but was not useful - we converted it to warnings anyway
      //uaException: raise EGLSLUniformMissing.Create(ErrMessage);
      umIgnore   : ;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('AUniformMissing? in TGLSLProgram.UniformMissing');
      {$endif}
    end;
  end;

begin
  Result.Owner := Self;
  Result.Name := AName;

  if not FUniformLocations.TryGetValue(AName, Result.Location) then
  begin
    if (GLFeatures <> nil) and GLFeatures.Shaders then
      Result.Location := glGetUniformLocation(ProgramId, PAnsiCharOrNil(AName))
    else
    if RHIDevice <> nil then
    begin
      if ((FSource[stFragment] <> nil) and (Pos(AName, FSource[stFragment].Text) > 0)) or
         ((FSource[stVertex] <> nil) and (Pos(AName, FSource[stVertex].Text) > 0)) or
         (AName = 'viewport_size') or (AName = 'color') or
         (AName = 'clip_line') or (AName = 'image_texture') or
         ((FSource[stFragment] <> nil) and (FSource[stFragment].Count = 0) and
          (FSource[stVertex] <> nil) and (FSource[stVertex].Count = 0)) then
        Result.Location := 0
      else
        Result.Location := GLUniformLocationNone;
    end else
      Result.Location := GLUniformLocationNone;

    {$ifdef CASTLE_LOG_GET_LOCATIONS}
    WritelnLog('Doing (potentially expensive) glGetUniformLocation: ' + AName);
    {$endif}
    FUniformLocations.Add(AName, Result.Location);
  end;

  if Result.Location = GLUniformLocationNone then
    ReportUniformMissing;
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: boolean; const AUniformMissing: TUniformMissing);
begin
  Uniform(AName, AUniformMissing).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TGLint; const AUniformMissing: TUniformMissing);
begin
  Uniform(AName, AUniformMissing).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TVector2Integer; const AUniformMissing: TUniformMissing);
begin
  Uniform(AName, AUniformMissing).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TVector3Integer; const AUniformMissing: TUniformMissing);
begin
  Uniform(AName, AUniformMissing).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TVector4Integer; const AUniformMissing: TUniformMissing);
begin
  Uniform(AName, AUniformMissing).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TGLfloat; const AUniformMissing: TUniformMissing);
begin
  Uniform(AName, AUniformMissing).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TVector2; const AUniformMissing: TUniformMissing);
begin
  Uniform(AName, AUniformMissing).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TVector3; const AUniformMissing: TUniformMissing);
begin
  Uniform(AName, AUniformMissing).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TVector4; const AUniformMissing: TUniformMissing);
begin
  Uniform(AName, AUniformMissing).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TMatrix2; const AUniformMissing: TUniformMissing);
begin
  Uniform(AName, AUniformMissing).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TMatrix3; const AUniformMissing: TUniformMissing);
begin
  Uniform(AName, AUniformMissing).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TMatrix4; const AUniformMissing: TUniformMissing);
begin
  Uniform(AName, AUniformMissing).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TBooleanList; const AUniformMissing: TUniformMissing);
begin
  Uniform(AName, AUniformMissing).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TInt32List; const AUniformMissing: TUniformMissing);
begin
  Uniform(AName, AUniformMissing).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TSingleList; const AUniformMissing: TUniformMissing);
begin
  Uniform(AName, AUniformMissing).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TVector2List; const AUniformMissing: TUniformMissing);
begin
  Uniform(AName, AUniformMissing).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TVector3List; const AUniformMissing: TUniformMissing);
begin
  Uniform(AName, AUniformMissing).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TVector4List; const AUniformMissing: TUniformMissing);
begin
  Uniform(AName, AUniformMissing).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TMatrix3List; const AUniformMissing: TUniformMissing);
begin
  Uniform(AName, AUniformMissing).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TMatrix4List; const AUniformMissing: TUniformMissing);
begin
  Uniform(AName, AUniformMissing).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: boolean);
begin
  SetUniform(AName, Value, UniformMissing);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TGLint);
begin
  SetUniform(AName, Value, UniformMissing);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TVector2Integer);
begin
  SetUniform(AName, Value, UniformMissing);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TVector3Integer);
begin
  SetUniform(AName, Value, UniformMissing);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TVector4Integer);
begin
  SetUniform(AName, Value, UniformMissing);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TGLfloat);
begin
  SetUniform(AName, Value, UniformMissing);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TVector2);
begin
  SetUniform(AName, Value, UniformMissing);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TVector3);
begin
  SetUniform(AName, Value, UniformMissing);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TVector4);
begin
  SetUniform(AName, Value, UniformMissing);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TMatrix2);
begin
  SetUniform(AName, Value, UniformMissing);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TMatrix3);
begin
  SetUniform(AName, Value, UniformMissing);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TMatrix4);
begin
  SetUniform(AName, Value, UniformMissing);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TBooleanList);
begin
  SetUniform(AName, Value, UniformMissing);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TInt32List);
begin
  SetUniform(AName, Value, UniformMissing);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TSingleList);
begin
  SetUniform(AName, Value, UniformMissing);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TVector2List);
begin
  SetUniform(AName, Value, UniformMissing);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TVector3List);
begin
  SetUniform(AName, Value, UniformMissing);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TVector4List);
begin
  SetUniform(AName, Value, UniformMissing);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TMatrix3List);
begin
  SetUniform(AName, Value, UniformMissing);
end;

procedure TGLSLProgram.SetUniform(const AName: string; const Value: TMatrix4List);
begin
  SetUniform(AName, Value, UniformMissing);
end;

function TGLSLProgram.AttributeOptional(const AName: string): TGLSLAttribute;
begin
  Result.Owner := Self;
  Result.Name := AName;

  if not FAttributeLocations.TryGetValue(AName, Result.Location) then
  begin
    if (GLFeatures <> nil) and GLFeatures.Shaders then
      Result.Location := glGetAttribLocation(ProgramId, PAnsiCharOrNil(AName))
    else
    if RHIDevice <> nil then
      Result.Location := 0
    else
      Result.Location := GLAttribLocationNone;

    {$ifdef CASTLE_LOG_GET_LOCATIONS}
    WritelnLog('Doing (potentially expensive) glGetAttribLocation: ' + AName);
    {$endif}
    FAttributeLocations.Add(AName, Result.Location);
  end;
end;

function TGLSLProgram.Attribute(const AName: string): TGLSLAttribute;
begin
  Result := AttributeOptional(AName);
  if Result.Location = GLAttribLocationNone then
    raise EGLSLAttributeNotFound.CreateFmt('Attribute variable "%s" not found', [AName]);
end;

procedure TGLSLProgram.SetAttribute(const AName: string; const Value: TGLfloat);
begin
  Attribute(AName).SetValue(Value);
end;

procedure TGLSLProgram.SetAttribute(const AName: string; const Value: TVector2);
begin
  Attribute(AName).SetValue(Value);
end;

procedure TGLSLProgram.SetAttribute(const AName: string; const Value: TVector3);
begin
  Attribute(AName).SetValue(Value);
end;

procedure TGLSLProgram.SetAttribute(const AName: string; const Value: TVector4);
begin
  Attribute(AName).SetValue(Value);
end;

procedure TGLSLProgram.SetAttribute(const AName: string; const Value: TMatrix3);
begin
  Attribute(AName).SetValue(Value);
end;

procedure TGLSLProgram.SetAttribute(const AName: string; const Value: TMatrix4);
begin
  Attribute(AName).SetValue(Value);
end;

{$ifndef OpenGLES}
procedure TGLSLProgram.SetAttribute(const AName: string; const Value: TVector4Integer);
begin
  Attribute(AName).SetValue(Value);
end;

procedure TGLSLProgram.SetAttribute(const AName: string; const Value: TVector4Byte);
begin
  Attribute(AName).SetValue(Value);
end;

procedure TGLSLProgram.SetAttribute(const AName: string; const Value: TGLdouble);
begin
  Attribute(AName).SetValue(Value);
end;

// Makes FPC errors: Error: Asm: Duplicate label, see https://bugs.freepascal.org/view.php?id=32188
{
procedure TGLSLProgram.SetAttribute(const AName: string; const Value: TVector2Double);
begin
  Attribute(AName).SetValue(Value);
end;

procedure TGLSLProgram.SetAttribute(const AName: string; const Value: TVector3Double);
begin
  Attribute(AName).SetValue(Value);
end;

procedure TGLSLProgram.SetAttribute(const AName: string; const Value: TVector4Double);
begin
  Attribute(AName).SetValue(Value);
end;
}
{$endif}

class function TGLSLProgram.GetCurrent: TGLSLProgram;
begin
  if RenderContext <> nil then
    Result := RenderContext.CurrentProgram
  else
    Result := nil;
end;

class procedure TGLSLProgram.SetCurrent(const Value: TGLSLProgram);
begin
  if RenderContext <> nil then
    RenderContext.CurrentProgram := Value;
end;

function TGLSLProgram.Source(const ShaderType: TShaderType): String;
begin
  if FSource[ShaderType] <> nil then
    Result := FSource[ShaderType].Text
  else
    Result := '';
end;

procedure TGLSLProgram.SetRHIUniformValue(const AName: String; const AValue: TVector4);
begin
  if (AName = '') or (FRHIUniformValues = nil) then
    Exit;
  FRHIUniformValues.AddOrSetValue(AName, AValue);
  if (FRHIUniformOrder <> nil) and
     (AName <> 'viewport_size') and (AName <> 'color') and
     (AName <> 'clip_line') and (AName <> 'image_texture') and
     (FRHIUniformOrder.IndexOf(AName) = -1) then
    FRHIUniformOrder.Add(AName);
end;

function TGLSLProgram.GetRHIUniformValue(const AName: String; out AValue: TVector4): Boolean;
begin
  if (FRHIUniformValues <> nil) and FRHIUniformValues.TryGetValue(AName, AValue) then
    Result := True
  else
  begin
    AValue := TVector4.Zero;
    Result := False;
  end;
end;

function TGLSLProgram.GetOrCreateRHI2DFragmentShader(const ADevice: TRHIDevice): TRHIShaderModule;
const
  SwizzleComp: array [0..3] of Char = ('x', 'y', 'z', 'w');
var
  Gen: TCastleShaderGeneratorRHI;
  RawLines: TStringList;
  I, SemiPos, NextCompOffset, CompCount, MapIdx: Integer;
  LineStr, Trimmed, Rest, Tok1, Tok2, VarType, VarName, SwizStr, AdaptedBody, DebugLabel: String;
  SeekPos: Integer;
begin
  if ADevice = nil then
    Exit(nil);
  if (FRHI2DFragmentShader <> nil) and (FRHI2DFragmentShader.Device = ADevice) then
    Exit(FRHI2DFragmentShader);

  FreeAndNil(FRHI2DFragmentShader);
  FRHI2DAdaptedFragmentSource := '';
  SetLength(FRHI2DUniformMappings, 0);

  if (FSource[stFragment] = nil) or (Trim(FSource[stFragment].Text) = '') then
    Exit(nil);

  Gen := TCastleShaderGeneratorRHI.Create(ADevice.Capabilities.Backend);
  RawLines := TStringList.Create;
  try
    RawLines.Text := FSource[stFragment].Text;
    AdaptedBody := '';
    NextCompOffset := 0;

    for I := 0 to RawLines.Count - 1 do
    begin
      LineStr := RawLines[I];
      Trimmed := Trim(LineStr);

      if IsPrefix('#version ', Trimmed, False) or
         IsPrefix('precision ', Trimmed, False) then
        Continue;

      if IsPrefix('varying ', Trimmed, False) and (Pos(';', Trimmed) > 0) then
      begin
        SemiPos := Pos(';', Trimmed);
        Rest := Trim(Copy(Trimmed, Length('varying ') + 1, SemiPos - Length('varying ') - 1));
        SeekPos := 1;
        VarName := '';
        while SeekPos <= Length(Rest) do
        begin
          Tok1 := NextToken(Rest, SeekPos, WhiteSpaces);
          if Tok1 <> '' then
            VarName := Tok1
          else
            Break;
        end;
        if VarName = 'frag_coord' then
          AdaptedBody := AdaptedBody + '#define frag_coord gl_FragCoord.xy' + NL
        else
        if VarName <> '' then
          AdaptedBody := AdaptedBody + '#define ' + VarName + ' castle_tex_coord0' + NL;
        Continue;
      end;

      if IsPrefix('uniform ', Trimmed, False) and (Pos(';', Trimmed) > 0) then
      begin
        SemiPos := Pos(';', Trimmed);
        Rest := Trim(Copy(Trimmed, Length('uniform ') + 1, SemiPos - Length('uniform ') - 1));
        SeekPos := 1;
        Tok1 := NextToken(Rest, SeekPos, WhiteSpaces);
        if (Tok1 = 'lowp') or (Tok1 = 'mediump') or (Tok1 = 'highp') then
          VarType := NextToken(Rest, SeekPos, WhiteSpaces)
        else
          VarType := Tok1;
        Tok2 := NextToken(Rest, SeekPos, WhiteSpaces);
        VarName := Tok2;

        if VarType = 'sampler2D' then
        begin
          if VarName <> '' then
            AdaptedBody := AdaptedBody + 'CASTLE_SAMPLER(0, 0) uniform sampler2D ' + VarName + ';' + NL;
          Continue;
        end else
        if VarName = 'color' then
        begin
          AdaptedBody := AdaptedBody + '#define color castle_color_per_vertex' + NL;
          Continue;
        end else
        if VarName = 'clip_line' then
        begin
          AdaptedBody := AdaptedBody + '#define clip_line (CastleDraw.u_ModelViewMatrix[1].xyz)' + NL;
          Continue;
        end else
        if VarName = 'viewport_size' then
        begin
          AdaptedBody := AdaptedBody +
            '#define viewport_size (vec2(2.0 / max(CastleDraw.u_ModelViewMatrix[0].x, 1e-6), 2.0 / max(CastleDraw.u_ModelViewMatrix[0].y, 1e-6)))' + NL;
          Continue;
        end else
        if (VarName <> '') and
           ((VarType = 'float') or (VarType = 'int') or (VarType = 'bool') or
            (VarType = 'vec2') or (VarType = 'vec3') or (VarType = 'vec4')) then
        begin
          if VarType = 'vec4' then
            CompCount := 4
          else
          if VarType = 'vec3' then
            CompCount := 3
          else
          if VarType = 'vec2' then
            CompCount := 2
          else
            CompCount := 1;

          if NextCompOffset + CompCount > 4 then
            NextCompOffset := 4 - CompCount;

          MapIdx := Length(FRHI2DUniformMappings);
          SetLength(FRHI2DUniformMappings, MapIdx + 1);
          FRHI2DUniformMappings[MapIdx].Name := VarName;
          FRHI2DUniformMappings[MapIdx].ComponentOffset := NextCompOffset;
          FRHI2DUniformMappings[MapIdx].ComponentCount := CompCount;

          case CompCount of
            1: SwizStr := SwizzleComp[NextCompOffset];
            2: SwizStr := SwizzleComp[NextCompOffset] + SwizzleComp[NextCompOffset + 1];
            3: SwizStr := SwizzleComp[NextCompOffset] + SwizzleComp[NextCompOffset + 1] + SwizzleComp[NextCompOffset + 2];
            else SwizStr := 'xyzw';
          end;

          if VarType = 'int' then
            AdaptedBody := AdaptedBody + '#define ' + VarName + ' int(CastleDraw.u_ModelViewMatrix[3].' + SwizStr + ')' + NL
          else
          if VarType = 'bool' then
            AdaptedBody := AdaptedBody + '#define ' + VarName + ' (CastleDraw.u_ModelViewMatrix[3].' + SwizStr + ' != 0.0)' + NL
          else
          if CompCount = 4 then
            AdaptedBody := AdaptedBody + '#define ' + VarName + ' (CastleDraw.u_ModelViewMatrix[3])' + NL
          else
            AdaptedBody := AdaptedBody + '#define ' + VarName + ' (CastleDraw.u_ModelViewMatrix[3].' + SwizStr + ')' + NL;

          if NextCompOffset + CompCount < 4 then
            Inc(NextCompOffset, CompCount);
          Continue;
        end;
      end;

      AdaptedBody := AdaptedBody + LineStr + NL;
    end;

    FRHI2DAdaptedFragmentSource :=
      Gen.GenerateBatch2DFragmentPreamble +
      '#define texture2D texture' + NL +
      '#define gl_FragColor castle_FragColor' + NL +
      AdaptedBody;

    if Name <> '' then
      DebugLabel := Name + '.RHI2D.frag'
    else
      DebugLabel := 'CustomShader2D.RHI2D.frag';
    FRHI2DFragmentShader := ADevice.CreateShaderModuleFromGLSL(
      ssFragment, FRHI2DAdaptedFragmentSource, DebugLabel);
    Result := FRHI2DFragmentShader;
  finally
    FreeAndNil(RawLines);
    FreeAndNil(Gen);
  end;
end;

function TGLSLProgram.BuildRHI2DCustomParams: TVector4;
var
  I, C: Integer;
  Val: TVector4;
begin
  Result := TVector4.Zero;
  if Length(FRHI2DUniformMappings) > 0 then
  begin
    for I := 0 to High(FRHI2DUniformMappings) do
      if GetRHIUniformValue(FRHI2DUniformMappings[I].Name, Val) then
        for C := 0 to FRHI2DUniformMappings[I].ComponentCount - 1 do
          if (FRHI2DUniformMappings[I].ComponentOffset + C >= 0) and
             (FRHI2DUniformMappings[I].ComponentOffset + C <= 3) then
            Result.Data[FRHI2DUniformMappings[I].ComponentOffset + C] := Val.Data[C];
  end else
  if (FRHIUniformOrder <> nil) and (FRHIUniformOrder.Count > 0) then
  begin
    if GetRHIUniformValue(FRHIUniformOrder[0], Val) then
      Result := Val;
  end;
end;

end.
