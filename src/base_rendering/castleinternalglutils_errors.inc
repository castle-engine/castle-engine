{%MainUnit castleinternalglutils.pas}
{
  Copyright 2001-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Part of CastleGLUtils unit: dealing with OpenGL errors. }

{$ifdef read_interface}

type
  { OpenGL error.
    Usually OpenGL error indicates a bug in our code.
    Invalid uniform types for GLSL uniforms (including in user provided shaders)
    can also result in this error.

    Note: @link(EOpenGLOutOfMemoryError) may happen regardless
    of what we do, and depends on the device.

    You can call it with overloaded ancestor Create / CreateFmt that take just string,
    not ErrorCode: TGLenum, this is a feature. }
  EOpenGLError = class(Exception)
  public
    constructor Create(const AErrorCode: TGLenum; const AdditionalComment: string = ''); overload;
  end;

  { GPU memory is not sufficient.

    When programming for platforms with limited GPU memory (Android, iOS...)
    you should prepare to handle EOpenGLOutOfMemoryError (corresponding to
    GL_OUT_OF_MEMORY). This can always happen for large GPU data,
    and you should be prepared to capture it (around loading your data)
    and display some nice information for user.
    Alternatively, you can leave GLOutOfMemoryError = @false,
    and then EOpenGLOutOfMemoryError will not happen, but you risk all kinds
    of rendering artifacts. }
  EOpenGLOutOfMemoryError = class(EOpenGLError)
  end;

var
  { When GPU runs out of memory, raise exception (EOpenGLOutOfMemoryError)
    or merely make a warning. Merely making a warning is very risky (you risk all kinds
    of rendering artifacts), but sometimes the rendering is actually smooth
    even though GPU complains. }
  GLOutOfMemoryError: boolean = false;

procedure GLOutOfMemory(const AdditionalComment: string = '');

function GLErrorString(const ErrorCode: TGLenum; const AdditionalComment: string = ''): string;

{ Check are any OpenGL errors recorded (in glGetError).
  If there are errors, our behavior depends on whether we were compiled
  with -dRELEASE. With -dRELEASE, we make WritelnWarning. This way eventual
  errors in release builds don't completely abort your program.

  Note that the behavior on GL_OUT_OF_MEMORY is different.
  -dRELEASE does not matter here. Only GLOutOfMemoryError boolean dictates
  if we should raise an exception or merely make warning, regardless of -dRELEASE
  state.

  Without -dRELEASE, we raise EOpenGLError. So a developer is strongly
  suggested to fix the code to not produce OpenGL errors, never ever.

  @raises EOpenGLError }
procedure CheckGLErrors(const AdditionalComment: string = '');

{$endif read_interface}

{$ifdef read_implementation}

function GLErrorString(const ErrorCode: TGLenum; const AdditionalComment: string): string;
var
  S: string;
begin
  { Do not use gluErrorString, not available in OpenGL ES.
    Error descriptions below from
    http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGetError.xml }
  case ErrorCode of
    GL_NO_ERROR: S := 'No error has been recorded.';
    GL_INVALID_ENUM: S := 'An unacceptable value is specified for an enumerated argument.';
    GL_INVALID_VALUE: S := 'A numeric argument is out of range.';
    GL_INVALID_OPERATION: S := 'The specified operation is not allowed in the current state.';
    {$ifndef OpenGLES}
    GL_STACK_OVERFLOW: S := 'Stack overflow';
    GL_STACK_UNDERFLOW: S := 'Stack underflow';
    {$endif}
    GL_INVALID_FRAMEBUFFER_OPERATION: S := 'The command is trying to render to or read from the framebuffer while the currently bound framebuffer is not framebuffer complete (i.e. the return value from glCheckFramebufferStatus is not GL_FRAMEBUFFER_COMPLETE).';
    GL_OUT_OF_MEMORY: S := 'There is not enough memory left to execute the command.';
    else S := 'Unknown error.';
  end;

  if AdditionalComment <> '' then
    Result := AdditionalComment + nl else
    Result := '';
  Result := Result + Format('OpenGL error (%d): %s', [ErrorCode, S]);
end;

constructor EOpenGLError.Create(const AErrorCode: TGLenum; const AdditionalComment: string);
begin
  inherited Create(GLErrorString(AErrorCode, AdditionalComment));
end;

procedure GLOutOfMemory(const AdditionalComment: string);
const
  ErrorCode = GL_OUT_OF_MEMORY;
begin
  if GLOutOfMemoryError then
    raise EOpenGLOutOfMemoryError.Create(ErrorCode, AdditionalComment) else
    WritelnWarning('OpenGL', GLErrorString(ErrorCode, AdditionalComment));
end;

procedure CheckGLErrors(const AdditionalComment: string);
var
  ErrorCode: TGLenum;
begin
  ErrorCode := glGetError();
  if ErrorCode <> GL_NO_ERROR then
  begin
    if ErrorCode = GL_OUT_OF_MEMORY then
      GLOutOfMemory(AdditionalComment) else
      {$ifdef RELEASE}
      WritelnWarning('OpenGL', GLErrorString(ErrorCode, AdditionalComment));
      {$else}
      raise EOpenGLError.Create(ErrorCode, AdditionalComment);
      {$endif}
  end;
end;

{$endif read_implementation}
