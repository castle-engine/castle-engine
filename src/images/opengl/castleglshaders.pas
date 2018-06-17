{
  Copyright 2007-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ OpenGL shaders in GLSL language.

  @unorderedList(
    @item(
      Creating/destroying the TGLSLProgram instance immediately creates/destroys
      appropriate program on GPU. So be sure to create/destroy it only
      when you have OpenGL context available (for example, create in TCastleWindowCustom.OnOpen
      and destroy in TCastleWindowCustom.OnClose).)

    @item(
      Upon creation, we check current OpenGL context abilities.
      Currently three support levels are possible:
      no support at all (old OpenGL), support through ARB extensions,
      or support built-in (newer OpenGL versions, >= 2.0).

      All three cases are automatically handled inside, so usually you
      do not have to care about these details. Note that "none" support
      (on older OpenGL version with no appropriate ARB extensions)
      means that shaders are not really initialized at all.))
}
unit CastleGLShaders;

{$I castleconf.inc}

interface

uses SysUtils, Classes, Generics.Collections,
  {$ifdef CASTLE_OBJFPC} CastleGL, {$else} GL, GLExt, {$endif}
  CastleGLUtils, CastleUtils, CastleVectors, CastleRendererBaseTypes;

type
  { Common class for exceptions related to GLSL programs. }
  EGLSLError = class(Exception);

  EGLSLShaderCompileError = class(EGLSLError);
  EGLSLProgramLinkError = class(EGLSLError);
  EGLSLAttributeNotFound = class(EGLSLError);

  EGLSLUniformInvalid = class(EGLSLError);
  EGLSLUniformNotFound = class(EGLSLUniformInvalid);
  EGLSLUniformTypeMismatch = class(EGLSLUniformInvalid);

  TGLuintList = TCardinalList;

  { What to do when GLSL uniform variable is set (TGLSLProgram.SetUniform)
    but doesn't exist in the shader. }
  TUniformNotFoundAction = (
    { Report that uniform variable not found to WritelnWarning. }
    uaWarning,
    { Report that uniform variable not found by raising EGLSLUniformNotFound. }
    uaException,
    { Ignore the fact that uniform variable doesn't exist in the GLSL shader.
      Do not warn anywhere. }
    uaIgnore);

  { What to do when GLSL uniform variable is set (by TGLSLProgram.SetUniform)
    to the type that doesn't match type declared in GLSL shader.

    @deprecated
    Do not use this.
    It has no effect anymore. Checking the GLSL uniform types requires
    doing glGetError after every uniform set, which is prohibitively slow.
    One day we may implement a better way to do this (using the info
    we get about uniforms in TGLSLProgram.DebugInfo) and then this will
    be useful again.
  }
  TUniformTypeMismatchAction = (
    { Do not catch uniform type mismatch, leaving it to OpenGL.
      This will cause OpenGL error "invalid operation" (possibly resulting
      in an exception in some later code that checks OpenGL errors).

      This is unsafe (you may get OpenGL errors later), but is also fastest.
      Other options have to detect invalid types, which means
      checking the OpenGL error state each time you set uniform value. }
    utGLError,
    { Report type mismatch to WritelnWarning. }
    utWarning,
    { Report type mismatch by raising EGLSLUniformTypeMismatch. }
    utException);

  TGLSLProgram = class;

  TGLSLUniform = record
  public
    Owner: TGLSLProgram;
    Name: string;
    Location: TGLint;

    const
      { Calling @link(TGLSLUniform.SetValue) of this is ignored. }
      NotExisting: TGLSLUniform = (Owner: nil; Name: ''; Location: -1);

    { Set uniform variable value.
      You should get the uniform information first using the
      @link(TGLSLProgram.Uniform) call. All the documentation for
      @link(TGLSLProgram.SetUniform) applies also here, so the program must be
      linked, and enabled (it will be enabled automatically by calling this).

      In this case of this method, you cannot get EGLSLUniformNotFound exception
      (it will be raised earlier, at @link(TGLSLProgram.Uniform)).

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
    procedure SetValue(const Value: boolean        ; const ForceException: boolean = false); overload;
    procedure SetValue(const Value: TGLint         ; const ForceException: boolean = false); overload;
    procedure SetValue(const Value: TVector2Integer; const ForceException: boolean = false); overload;
    procedure SetValue(const Value: TVector3Integer; const ForceException: boolean = false); overload;
    procedure SetValue(const Value: TVector4Integer; const ForceException: boolean = false); overload;
    procedure SetValue(const Value: TGLfloat       ; const ForceException: boolean = false); overload;
    procedure SetValue(const Value: TVector2       ; const ForceException: boolean = false); overload;
    procedure SetValue(const Value: TVector3       ; const ForceException: boolean = false); overload;
    procedure SetValue(const Value: TVector4       ; const ForceException: boolean = false); overload;
    procedure SetValue(const Value: TMatrix2       ; const ForceException: boolean = false); overload;
    procedure SetValue(const Value: TMatrix3       ; const ForceException: boolean = false); overload;
    procedure SetValue(const Value: TMatrix4       ; const ForceException: boolean = false); overload;

    procedure SetValue(const Value: TBooleanList; const ForceException: boolean = false); overload;
    procedure SetValue(const Value: TLongIntList; const ForceException: boolean = false); overload;
    procedure SetValue(const Value: TSingleList ; const ForceException: boolean = false); overload;
    procedure SetValue(const Value: TVector2List; const ForceException: boolean = false); overload;
    procedure SetValue(const Value: TVector3List; const ForceException: boolean = false); overload;
    procedure SetValue(const Value: TVector4List; const ForceException: boolean = false); overload;
    procedure SetValue(const Value: TMatrix3List; const ForceException: boolean = false); overload;
    procedure SetValue(const Value: TMatrix4List; const ForceException: boolean = false); overload;
    { @groupEnd }
  end;

  TGLSLAttribute = record
  public
    type
      TLocationOffset = 0..3;
    var
    Owner: TGLSLProgram;
    Name: string;
    Location: TGLint;
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
      you should have (and uploaded to VBO). }
    procedure EnableArray(LocationOffset: TLocationOffset;
      Size: TGLint; AType: TGLenum; Normalized: TGLboolean; Stride: TGLsizei;
      Ptr: PtrUInt);
    { Shortcut to enable an array of floats (Single in Pascal). }
    procedure EnableArraySingle(Stride: TGLsizei; Ptr: PtrUInt);
    { Shortcut to enable an array of TVector2. }
    procedure EnableArrayVector2(Stride: TGLsizei; Ptr: PtrUInt);
    { Shortcut to enable an array of TVector3. }
    procedure EnableArrayVector3(Stride: TGLsizei; Ptr: PtrUInt);
    { Shortcut to enable an array of TVector4. }
    procedure EnableArrayVector4(Stride: TGLsizei; Ptr: PtrUInt);
    { Shortcut to enable an array of TMatrix3. }
    procedure EnableArrayMatrix3(Stride: TGLsizei; Ptr: PtrUInt);
    { Shortcut to enable an array of TMatrix4. }
    procedure EnableArrayMatrix4(Stride: TGLsizei; Ptr: PtrUInt);

    procedure DisableArray;

    { Set attribute variable value.
      The used type must match the type of this variable in GLSL program.

      OpenGL forces some constraints on using this, see SetUniform.
      In short: use this only after linking the program.
      The program is automatically enabled (set as @link(TGLSLProgram.Current)) by this.
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

  TGLSLAttributeList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TList<TGLSLAttribute>;

  TLocationCache = {$ifdef CASTLE_OBJFPC}specialize{$endif} TDictionary<String, TGLint>;

  { Easily handle program in GLSL (OpenGL Shading Language). }
  TGLSLProgram = class
  private
    FSupport: TGLSupport;

    { Note that for GLSL using ARB extension, the right type is GLhandleARB.
      But is has equal size as TGLuint (except on macOS 64-bit, see comments
      at ForceStandardGLSLApi).
      It's simplest to leave ProgramId and ShaderIds as ints, and eventually
      typecast them where necessary. }
    ProgramId: TGLuint;
    ShaderIds: TGLuintList;

    FUniformNotFoundAction: TUniformNotFoundAction;
    FUniformTypeMismatchAction: TUniformTypeMismatchAction;

    FUniformLocations, FAttributeLocations: TLocationCache;

    class var
      FCurrent: TGLSLProgram;

    class function GetCurrent: TGLSLProgram; static;
    class procedure SetCurrent(const Value: TGLSLProgram); static;
  public
    constructor Create;
    destructor Destroy; override;

    property Support: TGLSupport read FSupport;

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
    procedure AttachShader(const ShaderType: TShaderType; const S: string); overload;
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

    { Link the program, this should be done after attaching all shaders
      and before actually using the program.

      @raises(EGLSLProgramLinkError If the program cannot be linked,
        exception message contains precise description from OpenGL where
        the error is.) }
    procedure Link; overload; virtual;
    procedure Link(Ignored: boolean); overload; deprecated 'use parameterless Link method';

    { Enable (use) this program. Shortcut for @code(TGLSLProgram.Current := Self). }
    procedure Enable;

    { Disable this program (use the fixed function pipeline).
      Shortcut for @code(TGLSLProgram.Current := nil). }
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
    class function ClassSupport: TGLSupport; deprecated 'use GLFeatures.Shaders';

    { What to do when GLSL uniform variable is accessed (by @link(SetUniform)
      or @link(Uniform)) but doesn't exist in the shader.
      Note that OpenGL aggresively removes unused code and variables
      from the shader when compiling/linking, so this also happens for
      "declared but unused" variables.

      This is only the default value, each @link(Uniform) method call
      may explicitly use other TUniformNotFoundAction.

      @seealso TUniformNotFoundAction }
    property UniformNotFoundAction: TUniformNotFoundAction
      read FUniformNotFoundAction write FUniformNotFoundAction
      default uaException;

    { What to do when GLSL uniform variable is set
      (by @link(TGLSLUniform.SetValue) or @link(SetUniform))
      but is declared with an incompatible type in the shader source.

      Note that the default (utGLError) is fastest. Other options will need
      to check OpenGL error state often, which may cause performance drop.

      @seealso TUniformTypeMismatchAction }
    property UniformTypeMismatchAction: TUniformTypeMismatchAction
      read FUniformTypeMismatchAction write FUniformTypeMismatchAction
      default utGLError;
      deprecated 'do not use this, it is ignored (old implementation was prohibitively slow)';

    { Get the uniform instance. It can be used to make repeated
      @link(TGLSLUniform.SetValue) calls. You must link the program first.

      If the uniform doesn't exist (or is unused), the action
      we take depends on @link(UniformNotFoundAction) property
      (by default -- uaException).
      The overloaded version with extra @code(AUniformNotFoundAction)
      parameter follows this parameter value.

      @raises(EGLSLUniformNotFound If the variable is not found within
        the program and we the behaviour (determined
        by AUniformTypeMismatchAction parameter or UniformTypeMismatchAction
        property) is uaException.)

      @groupBegin }
    function Uniform(const Name: string): TGLSLUniform; overload;
    function Uniform(const Name: string; const AUniformNotFoundAction: TUniformNotFoundAction): TGLSLUniform; overload;
    { @groupEnd }

    { Set appropriate uniform variable value.
      The used type must match the type of this variable in GLSL program.

      OpenGL forces some constraints on using this:

      @unorderedList(
        @item(This should be used only after linking, and re-linking clears
          (sets to zero) all uniform variables values.)

        @item(This GLSL program must be currently enabled for this to work.
          So calling this automatically calls @link(Enable)
          and sets @link(TGLSLProgram.Current).

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

          You can change UniformNotFoundAction not to silently ignore setting
          of inactive (or not existing) uniform values.)
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
    procedure SetUniform(const Name: string; const Value: boolean        ; const ForceException: boolean = false); overload;
    procedure SetUniform(const Name: string; const Value: TGLint         ; const ForceException: boolean = false); overload;
    procedure SetUniform(const Name: string; const Value: TVector2Integer; const ForceException: boolean = false); overload;
    procedure SetUniform(const Name: string; const Value: TVector3Integer; const ForceException: boolean = false); overload;
    procedure SetUniform(const Name: string; const Value: TVector4Integer; const ForceException: boolean = false); overload;
    procedure SetUniform(const Name: string; const Value: TGLfloat       ; const ForceException: boolean = false); overload;
    procedure SetUniform(const Name: string; const Value: TVector2 ; const ForceException: boolean = false); overload;
    procedure SetUniform(const Name: string; const Value: TVector3 ; const ForceException: boolean = false); overload;
    procedure SetUniform(const Name: string; const Value: TVector4 ; const ForceException: boolean = false); overload;
    procedure SetUniform(const Name: string; const Value: TMatrix2 ; const ForceException: boolean = false); overload;
    procedure SetUniform(const Name: string; const Value: TMatrix3 ; const ForceException: boolean = false); overload;
    procedure SetUniform(const Name: string; const Value: TMatrix4 ; const ForceException: boolean = false); overload;

    procedure SetUniform(const Name: string; const Value: TBooleanList      ; const ForceException: boolean = false); overload;
    procedure SetUniform(const Name: string; const Value: TLongIntList      ; const ForceException: boolean = false); overload;
    procedure SetUniform(const Name: string; const Value: TSingleList       ; const ForceException: boolean = false); overload;
    procedure SetUniform(const Name: string; const Value: TVector2List; const ForceException: boolean = false); overload;
    procedure SetUniform(const Name: string; const Value: TVector3List; const ForceException: boolean = false); overload;
    procedure SetUniform(const Name: string; const Value: TVector4List; const ForceException: boolean = false); overload;
    procedure SetUniform(const Name: string; const Value: TMatrix3List; const ForceException: boolean = false); overload;
    procedure SetUniform(const Name: string; const Value: TMatrix4List; const ForceException: boolean = false); overload;
    { @groupEnd }

    { Get the attribute instance. It can be used to make repeated
      @link(TGLSLAttribute.SetValue) and @link(TGLSLAttribute.EnableArray).

      @raises(EGLSLAttributeNotFound If the variable is not found within
        the program.

        Note that this is only one of the many things that can
        go wrong. And on most cases we don't raise any error,
        instead OpenGL sets it's error state and you probably want to
        call CheckGLErrors from time to time to catch them.) }
    function Attribute(const Name: string): TGLSLAttribute;

    { Get the attribute instance. It can be used to make repeated
      @link(TGLSLAttribute.SetValue) and @link(TGLSLAttribute.EnableArray).

      Returned attribute has @link(TGLSLAttribute.Location) equal to -1.
      All the calls to @link(TGLSLAttribute.SetValue)
      or @link(TGLSLAttribute.EnableArray) or @link(TGLSLAttribute.DisableArray)
      methods are silently ignored. }
    function AttributeOptional(const Name: string): TGLSLAttribute;

    { Load and enable vertex attribute data.
      This calls glVertexAttribPointer and enables it by
      glEnableVertexAttribArray (or ARB extension equivalents),
      see OpenGL reference for details.

      The attribute name is automatically resolved to a "location".
      We add LocationOffset (useful if you want to load matrix attributes,
      when you have to load matrix columns separately, with
      LocationOffset = column index).

      For repeated usage, it's better to use @link(Attribute) method,
      and repeatedly call @link(TGLSLAttribute.EnableArray) on the same
      @link(TGLSLAttribute) instance.

      @raises(EGLSLAttributeNotFound If the variable is not found within
        the program.)

      @returns(Attribute location (with LocationOffset already applied).
        You can use it with DisableVertexAttribArray.) }
    function VertexAttribPointer(const Name: string; LocationOffset: TGLSLAttribute.TLocationOffset;
      Size: TGLint; AType: TGLenum; Normalized: TGLboolean; Stride: TGLsizei;
      Ptr: Pointer): TGLint; deprecated 'use TGLSLAttribute.EnableArray';

    class procedure DisableVertexAttribArray(Location: TGLint); deprecated 'use TGLSLAttribute.DisableArray';

    { Set attribute variable value.
      The used type must match the type of this variable in GLSL program.

      OpenGL forces some constraints on using this, see SetUniform.
      In short: use this only after linking the program.
      The program is automatically enabled (set as @link(TGLSLProgram.Current)) by this.
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
    procedure SetAttribute(const Name: string; const Value: TGLfloat); overload; deprecated 'use TGLSLAttribute.SetValue';
    procedure SetAttribute(const Name: string; const Value: TVector2); overload; deprecated 'use TGLSLAttribute.SetValue';
    procedure SetAttribute(const Name: string; const Value: TVector3); overload; deprecated 'use TGLSLAttribute.SetValue';
    procedure SetAttribute(const Name: string; const Value: TVector4); overload; deprecated 'use TGLSLAttribute.SetValue';
    procedure SetAttribute(const Name: string; const Value: TMatrix3); overload; deprecated 'use TGLSLAttribute.SetValue';
    procedure SetAttribute(const Name: string; const Value: TMatrix4); overload; deprecated 'use TGLSLAttribute.SetValue';
    {$ifndef OpenGLES}
    procedure SetAttribute(const Name: string; const Value: TVector4Integer); overload; deprecated 'use TGLSLAttribute.SetValue';
    procedure SetAttribute(const Name: string; const Value: TVector4Byte); overload; deprecated 'use TGLSLAttribute.SetValue';
    procedure SetAttribute(const Name: string; const Value: TGLdouble); overload; deprecated 'use TGLSLAttribute.SetValue';
    // Makes FPC errors: Error: Asm: Duplicate label, see https://bugs.freepascal.org/view.php?id=32188
    // procedure SetAttribute(const Name: string; const Value: TVector2Double); deprecated 'use TGLSLAttribute.SetValue';
    // procedure SetAttribute(const Name: string; const Value: TVector3Double); deprecated 'use TGLSLAttribute.SetValue';
    // procedure SetAttribute(const Name: string; const Value: TVector4Double); deprecated 'use TGLSLAttribute.SetValue';
    {$endif}
    { @groupEnd }

    { Currently enabled GLSL program.
      @nil if fixed-function pipeline should be used.
      Setting this property encapsulates the OpenGL glUseProgram
      (or equivalent ARB extension), additionally preventing redundant glUseProgram
      calls. }
    class property Current: TGLSLProgram read GetCurrent write SetCurrent;
  end;

  TGLSLProgramList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TGLSLProgram>;

var
  LogShaders: boolean;

{$ifdef CASTLE_OBJFPC}
function GetCurrentProgram: TGLSLProgram;
  deprecated 'use TGLSLProgram.Current';
procedure SetCurrentProgram(const Value: TGLSLProgram);
  deprecated 'use TGLSLProgram.Current';

{ Currently enabled GLSL program.
  @nil if fixed-function pipeline should be used.
  Setting this property encapsulates the OpenGL glUseProgram
  (or equivalent ARB extension), additionally preventing redundant glUseProgram
  calls. }
property CurrentProgram: TGLSLProgram
  read GetCurrentProgram write SetCurrentProgram;
{$endif CASTLE_OBJFPC}

implementation

uses CastleStringUtils, CastleLog, CastleGLVersion;

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
    StringReplaceAllVar(Result, #0, NL);
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
    StringReplaceAllVar(Result, #0, NL);
  end else
    Result := '';
end;

{$ifndef ForceStandardGLSLApi}
{ Wrapper around glGetInfoLogARB (this is for both shaders and programs). }
function GetInfoLogARB(ObjectId: TGLuint): String;
var
  Len, Len2: TGLint;
begin
  glGetObjectParameterivARB(GLhandleARB(ObjectId), GL_OBJECT_INFO_LOG_LENGTH_ARB, @Len);

  if Len <> 0 then
  begin
    SetLength(Result, Len);
    glGetInfoLogARB(GLhandleARB(ObjectId), Len, @Len2, PChar(Result));
    StringReplaceAllVar(Result, #0, NL);
  end else
    Result := '';
end;
{$endif}

function GetCurrentProgram: TGLSLProgram;
begin
  Result := TGLSLProgram.Current;
end;

procedure SetCurrentProgram(const Value: TGLSLProgram);
begin
  TGLSLProgram.Current := Value;
end;

{ TGLSLUniform --------------------------------------------------------------- }

procedure TGLSLUniform.SetValue(const Value: boolean; const ForceException: boolean);
begin
  if Location = -1 then Exit; // ignore non-existing uniform here

  { GLSL "bool" types are set using the "i" version. From manpage:

    "Either the i or the f variants
     may be used to provide values for uniform variables of type
     bool, bvec2, bvec3, bvec4, or arrays of these. The uniform
     variable will be set to false if the input value is 0 or 0.0f,
     and it will be set to true otherwise.
    "

    Which means that I can simply call glUniform1i, with Ord(Value). }

  Owner.Enable;
  case Owner.Support of
    {$ifndef ForceStandardGLSLApi}
    gsExtension: glUniform1iARB(Location, Ord(Value));
    {$endif}
    gsStandard : glUniform1i   (Location, Ord(Value));
  end;
end;

procedure TGLSLUniform.SetValue(const Value: TGLint; const ForceException: boolean);
begin
  if Location = -1 then Exit; // ignore non-existing uniform here
  Owner.Enable;
  case Owner.Support of
    {$ifndef ForceStandardGLSLApi}
    gsExtension: glUniform1iARB(Location, Value);
    {$endif}
    gsStandard : glUniform1i   (Location, Value);
  end;
end;

procedure TGLSLUniform.SetValue(const Value: TVector2Integer; const ForceException: boolean);
begin
  if Location = -1 then Exit; // ignore non-existing uniform here
  Owner.Enable;
  case Owner.Support of
    {$ifndef ForceStandardGLSLApi}
    gsExtension: glUniform2ivARB(Location, 1, @Value);
    {$endif}
    gsStandard : glUniform2iv   (Location, 1, @Value);
  end;
end;

procedure TGLSLUniform.SetValue(const Value: TVector3Integer; const ForceException: boolean);
begin
  if Location = -1 then Exit; // ignore non-existing uniform here
  Owner.Enable;
  case Owner.Support of
    {$ifndef ForceStandardGLSLApi}
    gsExtension: glUniform3ivARB(Location, 1, @Value);
    {$endif}
    gsStandard : glUniform3iv   (Location, 1, @Value);
  end;
end;

procedure TGLSLUniform.SetValue(const Value: TVector4Integer; const ForceException: boolean);
begin
  if Location = -1 then Exit; // ignore non-existing uniform here
  Owner.Enable;
  case Owner.Support of
    {$ifndef ForceStandardGLSLApi}
    gsExtension: glUniform4ivARB(Location, 1, @Value);
    {$endif}
    gsStandard : glUniform4iv   (Location, 1, @Value);
  end;
end;

procedure TGLSLUniform.SetValue(const Value: TGLfloat; const ForceException: boolean);
begin
  if Location = -1 then Exit; // ignore non-existing uniform here
  Owner.Enable;
  case Owner.Support of
    {$ifndef ForceStandardGLSLApi}
    gsExtension: glUniform1fARB(Location, Value);
    {$endif}
    gsStandard : glUniform1f   (Location, Value);
  end;
end;

procedure TGLSLUniform.SetValue(const Value: TVector2; const ForceException: boolean);
begin
  if Location = -1 then Exit; // ignore non-existing uniform here
  Owner.Enable;
  case Owner.Support of
    {$ifndef ForceStandardGLSLApi}
    gsExtension: glUniform2fvARB(Location, 1, @Value);
    {$endif}
    gsStandard : glUniform2fv   (Location, 1, @Value);
  end;
end;

procedure TGLSLUniform.SetValue(const Value: TVector3; const ForceException: boolean);
begin
  if Location = -1 then Exit; // ignore non-existing uniform here
  Owner.Enable;
  case Owner.Support of
    {$ifndef ForceStandardGLSLApi}
    gsExtension: glUniform3fvARB(Location, 1, @Value);
    {$endif}
    gsStandard : glUniform3fv   (Location, 1, @Value);
  end;
end;

procedure TGLSLUniform.SetValue(const Value: TVector4; const ForceException: boolean);
begin
  if Location = -1 then Exit; // ignore non-existing uniform here
  Owner.Enable;
  case Owner.Support of
    {$ifndef ForceStandardGLSLApi}
    gsExtension: glUniform4fvARB(Location, 1, @Value);
    {$endif}
    gsStandard : glUniform4fv   (Location, 1, @Value);
  end;
end;

procedure TGLSLUniform.SetValue(const Value: TMatrix2; const ForceException: boolean);
begin
  if Location = -1 then Exit; // ignore non-existing uniform here
  Owner.Enable;
  case Owner.Support of
    {$ifndef ForceStandardGLSLApi}
    gsExtension: glUniformMatrix2fvARB(Location, 1, GL_FALSE, @Value);
    {$endif}
    gsStandard : glUniformMatrix2fv   (Location, 1, GL_FALSE, @Value);
  end;
end;

procedure TGLSLUniform.SetValue(const Value: TMatrix3; const ForceException: boolean);
begin
  if Location = -1 then Exit; // ignore non-existing uniform here
  Owner.Enable;
  case Owner.Support of
    {$ifndef ForceStandardGLSLApi}
    gsExtension: glUniformMatrix3fvARB(Location, 1, GL_FALSE, @Value);
    {$endif}
    gsStandard : glUniformMatrix3fv   (Location, 1, GL_FALSE, @Value);
  end;
end;

procedure TGLSLUniform.SetValue(const Value: TMatrix4; const ForceException: boolean);
begin
  if Location = -1 then Exit; // ignore non-existing uniform here
  Owner.Enable;
  case Owner.Support of
    {$ifndef ForceStandardGLSLApi}
    gsExtension: glUniformMatrix4fvARB(Location, 1, GL_FALSE, @Value);
    {$endif}
    gsStandard : glUniformMatrix4fv   (Location, 1, GL_FALSE, @Value);
  end;
end;

procedure TGLSLUniform.SetValue(const Value: TBooleanList; const ForceException: boolean);
var
  Ints: TLongIntList;
begin
  if Location = -1 then Exit; // ignore non-existing uniform here

  { We cannot pass Value.List, as Pascal booleans do not have 4 bytes
    (well, actually I could change this by compiler directive or
    by using LongBool for TBooleanList --- but for TBooleanList
    this would enlarge it 4 times, not nice).

    Unfortunately, there's no glUniform*ub (unsigned byte) or such function.

    So convert to longints. }
  Ints := Value.ToLongInt;
  try
    Owner.Enable;
    case Owner.Support of
      {$ifndef ForceStandardGLSLApi}
      gsExtension: glUniform1ivARB(Location, Value.Count, Ints.L);
      {$endif}
      gsStandard : glUniform1iv   (Location, Value.Count, Ints.L);
    end;
    finally FreeAndNil(Ints) end;
end;

procedure TGLSLUniform.SetValue(const Value: TLongIntList; const ForceException: boolean);
begin
  if Location = -1 then Exit; // ignore non-existing uniform here
  Assert(SizeOf(LongInt) = SizeOf(TGLint));
  Owner.Enable;
  case Owner.Support of
    {$ifndef ForceStandardGLSLApi}
    gsExtension: glUniform1ivARB(Location, Value.Count, Value.L);
    {$endif}
    gsStandard : glUniform1iv   (Location, Value.Count, Value.L);
  end;
end;

procedure TGLSLUniform.SetValue(const Value: TSingleList; const ForceException: boolean);
begin
  if Location = -1 then Exit; // ignore non-existing uniform here
  Owner.Enable;
  case Owner.Support of
    {$ifndef ForceStandardGLSLApi}
    gsExtension: glUniform1fvARB(Location, Value.Count, Value.L);
    {$endif}
    gsStandard : glUniform1fv   (Location, Value.Count, Value.L);
  end;
end;

procedure TGLSLUniform.SetValue(const Value: TVector2List; const ForceException: boolean);
begin
  if Location = -1 then Exit; // ignore non-existing uniform here
  Owner.Enable;
  case Owner.Support of
    {$ifndef ForceStandardGLSLApi}
    gsExtension: glUniform2fvARB(Location, Value.Count, PGLfloat(Value.L));
    {$endif}
    gsStandard : glUniform2fv   (Location, Value.Count, PGLfloat(Value.L));
  end;
end;

procedure TGLSLUniform.SetValue(const Value: TVector3List; const ForceException: boolean);
begin
  if Location = -1 then Exit; // ignore non-existing uniform here
  Owner.Enable;
  case Owner.Support of
    {$ifndef ForceStandardGLSLApi}
    gsExtension: glUniform3fvARB(Location, Value.Count, PGLfloat(Value.L));
    {$endif}
    gsStandard : glUniform3fv   (Location, Value.Count, PGLfloat(Value.L));
  end;
end;

procedure TGLSLUniform.SetValue(const Value: TVector4List; const ForceException: boolean);
begin
  if Location = -1 then Exit; // ignore non-existing uniform here
  Owner.Enable;
  case Owner.Support of
    {$ifndef ForceStandardGLSLApi}
    gsExtension: glUniform4fvARB(Location, Value.Count, PGLfloat(Value.L));
    {$endif}
    gsStandard : glUniform4fv   (Location, Value.Count, PGLfloat(Value.L));
  end;
end;

procedure TGLSLUniform.SetValue(const Value: TMatrix3List; const ForceException: boolean);
begin
  if Location = -1 then Exit; // ignore non-existing uniform here
  Owner.Enable;
  case Owner.Support of
    {$ifndef ForceStandardGLSLApi}
    gsExtension: glUniformMatrix3fvARB(Location, Value.Count, GL_FALSE, PGLfloat(Value.L));
    {$endif}
    gsStandard : glUniformMatrix3fv   (Location, Value.Count, GL_FALSE, PGLfloat(Value.L));
  end;
end;

procedure TGLSLUniform.SetValue(const Value: TMatrix4List; const ForceException: boolean);
begin
  if Location = -1 then Exit; // ignore non-existing uniform here
  Owner.Enable;
  case Owner.Support of
    {$ifndef ForceStandardGLSLApi}
    gsExtension: glUniformMatrix4fvARB(Location, Value.Count, GL_FALSE, PGLfloat(Value.L));
    {$endif}
    gsStandard : glUniformMatrix4fv   (Location, Value.Count, GL_FALSE, PGLfloat(Value.L));
  end;
end;

{ TGLSLAttribute ------------------------------------------------------------- }

procedure TGLSLAttribute.SetValue(const Value: TGLfloat);
begin
  if Location = -1 then Exit; // ignore non-existing attribute here
  Owner.Enable;
  case Owner.Support of
    {$ifndef ForceStandardGLSLApi}
    gsExtension: glVertexAttrib1fARB(Location, Value);
    {$endif}
    gsStandard : glVertexAttrib1f   (Location, Value);
  end;
end;

procedure TGLSLAttribute.SetValue(const Value: TVector2);
begin
  if Location = -1 then Exit; // ignore non-existing attribute here
  Owner.Enable;
  case Owner.Support of
    {$ifndef ForceStandardGLSLApi}
    gsExtension: glVertexAttrib2fvARB(Location, @Value);
    {$endif}
    gsStandard : glVertexAttrib2fv   (Location, @Value);
  end;
end;

procedure TGLSLAttribute.SetValue(const Value: TVector3);
begin
  if Location = -1 then Exit; // ignore non-existing attribute here
  Owner.Enable;
  case Owner.Support of
    {$ifndef ForceStandardGLSLApi}
    gsExtension: glVertexAttrib3fvARB(Location, @Value);
    {$endif}
    gsStandard : glVertexAttrib3fv   (Location, @Value);
  end;
end;

procedure TGLSLAttribute.SetValue(const Value: TVector4);
begin
  if Location = -1 then Exit; // ignore non-existing attribute here
  Owner.Enable;
  case Owner.Support of
    {$ifndef ForceStandardGLSLApi}
    gsExtension: glVertexAttrib4fvARB(Location, @Value);
    {$endif}
    gsStandard : glVertexAttrib4fv   (Location, @Value);
  end;
end;

procedure TGLSLAttribute.SetValue(const Value: TMatrix3);
begin
  if Location = -1 then Exit; // ignore non-existing attribute here
  Owner.Enable;
  case Owner.Support of
    {$ifndef ForceStandardGLSLApi}
    gsExtension:
      begin
        glVertexAttrib3fvARB(Location    , @Value.Data[0]);
        glVertexAttrib3fvARB(Location + 1, @Value.Data[1]);
        glVertexAttrib3fvARB(Location + 2, @Value.Data[2]);
      end;
    {$endif}
    gsStandard    :
      begin
        glVertexAttrib3fv   (Location    , @Value.Data[0]);
        glVertexAttrib3fv   (Location + 1, @Value.Data[1]);
        glVertexAttrib3fv   (Location + 2, @Value.Data[2]);
      end;
  end;
end;

procedure TGLSLAttribute.SetValue(const Value: TMatrix4);
begin
  if Location = -1 then Exit; // ignore non-existing attribute here
  Owner.Enable;
  case Owner.Support of
    {$ifndef ForceStandardGLSLApi}
    gsExtension:
      begin
        glVertexAttrib4fvARB(Location    , @Value.Data[0]);
        glVertexAttrib4fvARB(Location + 1, @Value.Data[1]);
        glVertexAttrib4fvARB(Location + 2, @Value.Data[2]);
        glVertexAttrib4fvARB(Location + 3, @Value.Data[3]);
      end;
    {$endif}
    gsStandard    :
      begin
        glVertexAttrib4fv   (Location    , @Value.Data[0]);
        glVertexAttrib4fv   (Location + 1, @Value.Data[1]);
        glVertexAttrib4fv   (Location + 2, @Value.Data[2]);
        glVertexAttrib4fv   (Location + 3, @Value.Data[3]);
      end;
  end;
end;

{$ifndef OpenGLES}
procedure TGLSLAttribute.SetValue(const Value: TVector4Integer);
begin
  if Location = -1 then Exit; // ignore non-existing attribute here
  Owner.Enable;
  case Owner.Support of
    gsExtension: glVertexAttrib4ivARB(Location, @Value);
    gsStandard : glVertexAttrib4iv   (Location, @Value);
  end;
end;

procedure TGLSLAttribute.SetValue(const Value: TVector4Byte);
begin
  if Location = -1 then Exit; // ignore non-existing attribute here
  Owner.Enable;
  case Owner.Support of
    gsExtension: glVertexAttrib4ubvARB(Location, @Value);
    gsStandard : glVertexAttrib4ubv   (Location, @Value);
  end;
end;

procedure TGLSLAttribute.SetValue(const Value: TGLdouble);
begin
  if Location = -1 then Exit; // ignore non-existing attribute here
  Owner.Enable;
  case Owner.Support of
    gsExtension: glVertexAttrib1dARB(Location, Value);
    gsStandard : glVertexAttrib1d   (Location, Value);
  end;
end;

// Makes FPC errors: Error: Asm: Duplicate label, see https://bugs.freepascal.org/view.php?id=32188
{
procedure TGLSLAttribute.SetValue(const Value: TVector2Double);
begin
  if Location = -1 then Exit; // ignore non-existing attribute here
  Owner.Enable;
  case Owner.Support of
    gsExtension: glVertexAttrib2dvARB(Location, @Value);
    gsStandard : glVertexAttrib2dv   (Location, @Value);
  end;
end;

procedure TGLSLAttribute.SetValue(const Value: TVector3Double);
begin
  if Location = -1 then Exit; // ignore non-existing attribute here
  Owner.Enable;
  case Owner.Support of
    gsExtension: glVertexAttrib3dvARB(Location, @Value);
    gsStandard : glVertexAttrib3dv   (Location, @Value);
  end;
end;

procedure TGLSLAttribute.SetValue(const Value: TVector4Double);
begin
  if Location = -1 then Exit; // ignore non-existing attribute here
  Owner.Enable;
  case Owner.Support of
    gsExtension: glVertexAttrib4dvARB(Location, @Value);
    gsStandard : glVertexAttrib4dv   (Location, @Value);
  end;
end;
}
{$endif}

procedure TGLSLAttribute.EnableArray(LocationOffset: TLocationOffset;
  Size: TGLint; AType: TGLenum; Normalized: TGLboolean; Stride: TGLsizei;
  Ptr: PtrUInt);
begin
  if Location = -1 then Exit; // ignore non-existing attribute here
  Owner.Enable;
  LocationOffsetsToDisable[LocationOffset] := true;
  case Owner.Support of
    {$ifndef ForceStandardGLSLApi}
    gsExtension:
      begin
        glEnableVertexAttribArrayARB(Location + LocationOffset);
        glVertexAttribPointerARB(Location + LocationOffset, Size, AType, Normalized, Stride, Pointer(Ptr));
      end;
    {$endif}
    gsStandard    :
      begin
        glEnableVertexAttribArray   (Location + LocationOffset);
        glVertexAttribPointer   (Location + LocationOffset, Size, AType, Normalized, Stride, Pointer(Ptr));
      end;
  end;
end;

procedure TGLSLAttribute.EnableArraySingle(Stride: TGLsizei; Ptr: PtrUInt);
begin
  EnableArray(0, 1, GL_FLOAT, GL_FALSE, Stride, Ptr);
end;

procedure TGLSLAttribute.EnableArrayVector2(Stride: TGLsizei; Ptr: PtrUInt);
begin
  EnableArray(0, 2, GL_FLOAT, GL_FALSE, Stride, Ptr);
end;

procedure TGLSLAttribute.EnableArrayVector3(Stride: TGLsizei; Ptr: PtrUInt);
begin
  EnableArray(0, 3, GL_FLOAT, GL_FALSE, Stride, Ptr);
end;

procedure TGLSLAttribute.EnableArrayVector4(Stride: TGLsizei; Ptr: PtrUInt);
begin
  EnableArray(0, 4, GL_FLOAT, GL_FALSE, Stride, Ptr);
end;

procedure TGLSLAttribute.EnableArrayMatrix3(Stride: TGLsizei; Ptr: PtrUInt);
begin
  EnableArray(0, 3, GL_FLOAT, GL_FALSE, Stride, Ptr);
  EnableArray(1, 3, GL_FLOAT, GL_FALSE, Stride, Ptr + SizeOf(TVector3));
  EnableArray(2, 3, GL_FLOAT, GL_FALSE, Stride, Ptr + SizeOf(TVector3) * 2);
end;

procedure TGLSLAttribute.EnableArrayMatrix4(Stride: TGLsizei; Ptr: PtrUInt);
begin
  EnableArray(0, 4, GL_FLOAT, GL_FALSE, Stride, Ptr);
  EnableArray(1, 4, GL_FLOAT, GL_FALSE, Stride, Ptr + SizeOf(TVector4));
  EnableArray(2, 4, GL_FLOAT, GL_FALSE, Stride, Ptr + SizeOf(TVector4) * 2);
  EnableArray(3, 4, GL_FLOAT, GL_FALSE, Stride, Ptr + SizeOf(TVector4) * 3);
end;

procedure TGLSLAttribute.DisableArray;
var
  Offset: TLocationOffset;
begin
  if Location = -1 then Exit; // ignore non-existing attribute here

  for Offset := Low(TLocationOffset) to High(TLocationOffset) do
    if LocationOffsetsToDisable[Offset] then
      case Owner.Support of
        {$ifndef ForceStandardGLSLApi}
        gsExtension: glDisableVertexAttribArrayARB(Location + Offset);
        {$endif}
        gsStandard : glDisableVertexAttribArray   (Location + Offset);
      end;
end;

{ TGLSLProgram --------------------------------------------------------------- }

constructor TGLSLProgram.Create;
begin
  inherited;

  FSupport := GLFeatures.Shaders;

  case Support of
    {$ifndef ForceStandardGLSLApi}
    gsExtension: GLhandleARB(ProgramId) := glCreateProgramObjectARB();
    {$endif}
    gsStandard :             ProgramId  := glCreateProgram         ();
  end;

  { Program id = 0 means that an error occurred. Citing GL documentation:

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

  FUniformLocations := TLocationCache.Create;
  FAttributeLocations := TLocationCache.Create;
end;

destructor TGLSLProgram.Destroy;
begin
  { make sure all shaders are detached and deleted, to free all resources }

  { Destructor may be called if exception raised from constructor,
    so better check that ShaderIds was created. }
  if ShaderIds <> nil then
    DetachAllShaders;

  case Support of
    {$ifndef ForceStandardGLSLApi}
    gsExtension: glDeleteObjectARB(GLhandleARB(ProgramId));
    {$endif}
    gsStandard : glDeleteProgram  (ProgramId);
  end;

  FreeAndNil(ShaderIds);
  FreeAndNil(FUniformLocations);
  FreeAndNil(FAttributeLocations);

  inherited;
end;

class function TGLSLProgram.ClassSupport: TGLSupport;
begin
  Result := GLFeatures.Shaders;
end;

function TGLSLProgram.ProgramInfoLog: string;
begin
  case Support of
    {$ifndef ForceStandardGLSLApi}
    gsExtension: Result := GetInfoLogARB(ProgramId);
    {$endif}
    gsStandard : Result := GetProgramInfoLog(ProgramId);
    else Result := '';
  end;
end;

function TGLSLProgram.DebugInfo: string;

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
      {$ifndef OpenGLES}
      GL_SAMPLER_1D_SHADOW: Result := 'SAMPLER_1D_SHADOW';
      GL_SAMPLER_2D_SHADOW: Result := 'SAMPLER_2D_SHADOW';
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
    Name: string;
    ErrorCode: TGLenum;
  begin
    case Support of
      {$ifndef ForceStandardGLSLApi}
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
            glGetActiveUniformARB(GLhandleARB(ProgramId), I, UniformMaxLength, @ReturnedLength,
              @Size, @AType, PCharOrNil(Name));

            SetLength(Name, ReturnedLength);
            UniformNames.Append(Format('  Name: %s, type: %s, size: %d',
              [Name, GLShaderVariableTypeName(AType), Size]));
          end;
        end;
      {$endif}

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
      {$ifndef ForceStandardGLSLApi}
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
            glGetActiveAttribARB(GLhandleARB(ProgramId), I, AttribMaxLength, @ReturnedLength,
              @Size, @AType, PCharOrNil(Name));
            SetLength(Name, ReturnedLength);
            AttribNames.Append(Format('  Name: %s, type: %s, size: %d',
              [Name, GLShaderVariableTypeName(AType), Size]));
          end;
        end;
      {$endif}

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
      {$ifndef ForceStandardGLSLApi}
      gsExtension: Result := GetInfoLogARB(ShaderId);
      {$endif}
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

  Result := Result +  NL + 'Program info log:' + NL + ProgramInfoLog ;

  Result := Result + NL + 'Program detected as running in hardware: ' +
    BoolToStr(RunningInHardware, true) ;
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

  {$ifndef ForceStandardGLSLApi}
  function CreateShaderARB(const S: string): TGLuint;
  var
    SrcPtr: PChar;
    SrcLength: Cardinal;
    Compiled: TGLint;
  begin
    GLhandleARB(Result) := glCreateShaderObjectARB(AType);
    SrcPtr := PChar(S);
    SrcLength := Length(S);
    glShaderSourceARB(GLhandleARB(Result), 1, @SrcPtr, @SrcLength);
    try
      glCompileShaderARB(GLhandleARB(Result));
    except
      on E: EAccessViolation do ReportBuggyCompileShader;
    end;
    glGetObjectParameterivARB(GLhandleARB(Result), GL_OBJECT_COMPILE_STATUS_ARB, @Compiled);
    if Compiled <> 1 then
      raise EGLSLShaderCompileError.CreateFmt('%s shader not compiled:' + NL + '%s',
        [ShaderTypeName[ShaderType], GetInfoLogARB(Result)]);
  end;
  {$endif}

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
        {$ifndef ForceStandardGLSLApi}
        gsExtension: AType := GL_VERTEX_SHADER_ARB;
        {$endif}
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
        {$ifndef ForceStandardGLSLApi}
        gsExtension: AType := GL_FRAGMENT_SHADER_ARB;
        {$endif}
        gsStandard : AType := GL_FRAGMENT_SHADER    ;
        else Exit;
      end;
    else raise EInternalError.Create('TGLSLProgram.AttachShader ShaderType?');
  end;

  case Support of
    {$ifndef ForceStandardGLSLApi}
    gsExtension:
      begin
        ShaderId := CreateShaderARB(S);
        glAttachObjectARB(GLhandleARB(ProgramId), GLhandleARB(ShaderId));
        ShaderIds.Add(ShaderId);
      end;
    {$endif}
    gsStandard:
      begin
        ShaderId := CreateShader(S);
        glAttachShader(ProgramId, ShaderId);
        ShaderIds.Add(ShaderId);
      end;
  end;
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

procedure TGLSLProgram.DetachAllShaders;
var
  I: Integer;
begin
  case Support of
    {$ifndef ForceStandardGLSLApi}
    gsExtension:
      for I := 0 to ShaderIds.Count - 1 do
      begin
        glDetachObjectARB(GLhandleARB(ProgramId), GLhandleARB(ShaderIds[I]));
        glDeleteObjectARB(GLhandleARB(ShaderIds[I]));
      end;
    {$endif}
    gsStandard    :
      for I := 0 to ShaderIds.Count - 1 do
      begin
        glDetachShader   (ProgramId, ShaderIds[I]);
        glDeleteShader   (ShaderIds[I]);
      end;
  end;
  ShaderIds.Count := 0;
end;

procedure TGLSLProgram.Link;
var
  Linked: TGLuint;
begin
  case Support of
    {$ifndef ForceStandardGLSLApi}
    gsExtension:
      begin
        glLinkProgramARB(GLhandleARB(ProgramId));
        glGetObjectParameterivARB(GLhandleARB(ProgramId), GL_OBJECT_LINK_STATUS_ARB, @Linked);
        if Linked <> 1 then
          raise EGLSLProgramLinkError.Create('GLSL program not linked' + NL +
            GetInfoLogARB(ProgramId));
      end;
    {$endif}
    gsStandard:
      begin
        glLinkProgram(ProgramId);
        glGetProgramiv(ProgramId, GL_LINK_STATUS, @Linked);
        if Linked <> GL_TRUE then
          raise EGLSLProgramLinkError.Create('GLSL program not linked' + NL +
            GetProgramInfoLog(ProgramId));
      end;
  end;

  if Log and LogShaders then
    WritelnLogMultiline('GLSL', 'GLSL program successfully linked. Information:' + NL + DebugInfo);
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
  Current := Self;
end;

class procedure TGLSLProgram.Disable;
begin
  Current := nil;
end;

function TGLSLProgram.SetupUniforms(var BoundTextureUnits: Cardinal): boolean;
begin
  Result := true;
end;

function TGLSLProgram.Uniform(const Name: string): TGLSLUniform;
begin
  Result := Uniform(Name, UniformNotFoundAction);
end;

function TGLSLProgram.Uniform(const Name: string; const AUniformNotFoundAction: TUniformNotFoundAction): TGLSLUniform;

  procedure ReportUniformNotFound;

    function ErrMessage: string;
    begin
      Result := Format('Uniform variable "%s" not found (or not used) in the shader source code', [Name]);
    end;

  begin
    case AUniformNotFoundAction of
      uaWarning  : WritelnWarning('GLSL', ErrMessage);
      uaException: raise EGLSLUniformNotFound.Create(ErrMessage);
      uaIgnore   : ;
      else raise EInternalError.Create('AUniformNotFoundAction? in TGLSLProgram.UniformNotFound');
    end;
  end;

begin
  Result.Owner := Self;
  Result.Name := Name;

  if not FUniformLocations.TryGetValue(Name, Result.Location) then
  begin
    case Support of
      {$ifndef ForceStandardGLSLApi}
      gsExtension: Result.Location := glGetUniformLocationARB(GLhandleARB(ProgramId), PCharOrNil(Name));
      {$endif}
      gsStandard : Result.Location := glGetUniformLocation   (ProgramId, PCharOrNil(Name));
      else Result.Location := -1;
    end;
    {$ifdef CASTLE_LOG_GET_LOCATIONS}
    WritelnLog('Doing (potentially expensive) glGetUniformLocation: ' + Name);
    {$endif}
    FUniformLocations.Add(Name, Result.Location);
  end;

  if Result.Location = -1 then
    ReportUniformNotFound;
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: boolean; const ForceException: boolean);
begin
  if ForceException then
    Uniform(Name, uaException).SetValue(Value, ForceException) else
    Uniform(Name).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TGLint; const ForceException: boolean);
begin
  if ForceException then
    Uniform(Name, uaException).SetValue(Value, ForceException) else
    Uniform(Name).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TVector2Integer; const ForceException: boolean);
begin
  if ForceException then
    Uniform(Name, uaException).SetValue(Value, ForceException) else
    Uniform(Name).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TVector3Integer; const ForceException: boolean);
begin
  if ForceException then
    Uniform(Name, uaException).SetValue(Value, ForceException) else
    Uniform(Name).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TVector4Integer; const ForceException: boolean);
begin
  if ForceException then
    Uniform(Name, uaException).SetValue(Value, ForceException) else
    Uniform(Name).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TGLfloat; const ForceException: boolean);
begin
  if ForceException then
    Uniform(Name, uaException).SetValue(Value, ForceException) else
    Uniform(Name).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TVector2; const ForceException: boolean);
begin
  if ForceException then
    Uniform(Name, uaException).SetValue(Value, ForceException) else
    Uniform(Name).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TVector3; const ForceException: boolean);
begin
  if ForceException then
    Uniform(Name, uaException).SetValue(Value, ForceException) else
    Uniform(Name).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TVector4; const ForceException: boolean);
begin
  if ForceException then
    Uniform(Name, uaException).SetValue(Value, ForceException) else
    Uniform(Name).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TMatrix2; const ForceException: boolean);
begin
  if ForceException then
    Uniform(Name, uaException).SetValue(Value, ForceException) else
    Uniform(Name).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TMatrix3; const ForceException: boolean);
begin
  if ForceException then
    Uniform(Name, uaException).SetValue(Value, ForceException) else
    Uniform(Name).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TMatrix4; const ForceException: boolean);
begin
  if ForceException then
    Uniform(Name, uaException).SetValue(Value, ForceException) else
    Uniform(Name).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TBooleanList; const ForceException: boolean);
begin
  if ForceException then
    Uniform(Name, uaException).SetValue(Value, ForceException) else
    Uniform(Name).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TLongIntList; const ForceException: boolean);
begin
  if ForceException then
    Uniform(Name, uaException).SetValue(Value, ForceException) else
    Uniform(Name).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TSingleList; const ForceException: boolean);
begin
  if ForceException then
    Uniform(Name, uaException).SetValue(Value, ForceException) else
    Uniform(Name).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TVector2List; const ForceException: boolean);
begin
  if ForceException then
    Uniform(Name, uaException).SetValue(Value, ForceException) else
    Uniform(Name).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TVector3List; const ForceException: boolean);
begin
  if ForceException then
    Uniform(Name, uaException).SetValue(Value, ForceException) else
    Uniform(Name).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TVector4List; const ForceException: boolean);
begin
  if ForceException then
    Uniform(Name, uaException).SetValue(Value, ForceException) else
    Uniform(Name).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TMatrix3List; const ForceException: boolean);
begin
  if ForceException then
    Uniform(Name, uaException).SetValue(Value, ForceException) else
    Uniform(Name).SetValue(Value);
end;

procedure TGLSLProgram.SetUniform(const Name: string; const Value: TMatrix4List; const ForceException: boolean);
begin
  if ForceException then
    Uniform(Name, uaException).SetValue(Value, ForceException) else
    Uniform(Name).SetValue(Value);
end;

function TGLSLProgram.AttributeOptional(const Name: string): TGLSLAttribute;
begin
  Result.Owner := Self;
  Result.Name := Name;

  if not FAttributeLocations.TryGetValue(Name, Result.Location) then
  begin
    case Support of
      {$ifndef ForceStandardGLSLApi}
      gsExtension: Result.Location := glGetAttribLocationARB(GLhandleARB(ProgramId), PCharOrNil(Name));
      {$endif}
      gsStandard: Result.Location := glGetAttribLocation(ProgramId, PCharOrNil(Name));
      else Result.Location := -1;
    end;
    {$ifdef CASTLE_LOG_GET_LOCATIONS}
    WritelnLog('Doing (potentially expensive) glGetAttribLocation: ' + Name);
    {$endif}
    FAttributeLocations.Add(Name, Result.Location);
  end;
end;

function TGLSLProgram.Attribute(const Name: string): TGLSLAttribute;
begin
  Result := AttributeOptional(Name);
  if Result.Location = -1 then
    raise EGLSLAttributeNotFound.CreateFmt('Attribute variable "%s" not found', [Name]);
end;

procedure TGLSLProgram.SetAttribute(const Name: string; const Value: TGLfloat);
begin
  Attribute(Name).SetValue(Value);
end;

procedure TGLSLProgram.SetAttribute(const Name: string; const Value: TVector2);
begin
  Attribute(Name).SetValue(Value);
end;

procedure TGLSLProgram.SetAttribute(const Name: string; const Value: TVector3);
begin
  Attribute(Name).SetValue(Value);
end;

procedure TGLSLProgram.SetAttribute(const Name: string; const Value: TVector4);
begin
  Attribute(Name).SetValue(Value);
end;

procedure TGLSLProgram.SetAttribute(const Name: string; const Value: TMatrix3);
begin
  Attribute(Name).SetValue(Value);
end;

procedure TGLSLProgram.SetAttribute(const Name: string; const Value: TMatrix4);
begin
  Attribute(Name).SetValue(Value);
end;

{$ifndef OpenGLES}
procedure TGLSLProgram.SetAttribute(const Name: string; const Value: TVector4Integer);
begin
  Attribute(Name).SetValue(Value);
end;

procedure TGLSLProgram.SetAttribute(const Name: string; const Value: TVector4Byte);
begin
  Attribute(Name).SetValue(Value);
end;

procedure TGLSLProgram.SetAttribute(const Name: string; const Value: TGLdouble);
begin
  Attribute(Name).SetValue(Value);
end;

// Makes FPC errors: Error: Asm: Duplicate label, see https://bugs.freepascal.org/view.php?id=32188
{
procedure TGLSLProgram.SetAttribute(const Name: string; const Value: TVector2Double);
begin
  Attribute(Name).SetValue(Value);
end;

procedure TGLSLProgram.SetAttribute(const Name: string; const Value: TVector3Double);
begin
  Attribute(Name).SetValue(Value);
end;

procedure TGLSLProgram.SetAttribute(const Name: string; const Value: TVector4Double);
begin
  Attribute(Name).SetValue(Value);
end;
}
{$endif}

function TGLSLProgram.VertexAttribPointer(const Name: string;
  LocationOffset: TGLSLAttribute.TLocationOffset;
  Size: TGLint; AType: TGLenum; Normalized: TGLboolean; Stride: TGLsizei;
  Ptr: Pointer): TGLint;
var
  A: TGLSLAttribute;
begin
  A := Attribute(Name);
  A.EnableArray(LocationOffset, Size, AType, Normalized, Stride, PtrUInt(Ptr));
  Result := A.Location;
end;

class procedure TGLSLProgram.DisableVertexAttribArray(Location: TGLint);
begin
  if Location <> -1 then
    case GLFeatures.Shaders of
      {$ifndef OpenGLES}
      gsExtension: glDisableVertexAttribArrayARB(Location);
      {$endif}
      gsStandard : glDisableVertexAttribArray   (Location);
    end;
end;

class function TGLSLProgram.GetCurrent: TGLSLProgram;
begin
  Result := FCurrent;
end;

class procedure TGLSLProgram.SetCurrent(const Value: TGLSLProgram);
begin
  if FCurrent <> Value then
  begin
    FCurrent := Value;

    if Value <> nil then
    begin
      case GLFeatures.Shaders of
        {$ifndef ForceStandardGLSLApi}
        gsExtension: glUseProgramObjectARB(GLhandleARB(Value.ProgramId));
        {$endif}
        gsStandard : glUseProgram         (Value.ProgramId);
      end;
    end else
    begin
      case GLFeatures.Shaders of
        {$ifndef ForceStandardGLSLApi}
        gsExtension:
          begin
            glUseProgramObjectARB(GLhandleARB(0));
            { Workaround for fglrx bug (Radeon X1600 (chantal)).
              Reproduce: open demo_models/x3d/anchor_test.x3dv,
              and switch in view3dscene "Shaders -> Enable For Everything".
              Text should be still rendered without shaders in this case
              (we cannot currently render text through shaders).
              Without the hack below, the shader from sphere would remain
              active and text would look black. }
            if GLVersion.Fglrx then glUseProgramObjectARB(GLhandleARB(0));
          end;
        {$endif}
        gsStandard    : glUseProgram         (0);
      end;
    end;
  end;
end;

end.
