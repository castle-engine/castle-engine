{%MainUnit castleglutils.pas}
{
  Copyright 2016-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Part of CastleGLUtils unit: Using OpenGL(ES) debug output.
  See https://www.khronos.org/opengl/wiki/Debug_Output . }

{$ifndef CASTLE_WEBGL}
  {$define CASTLE_GL_SUPPORTS_DEBUG}
{$endif}

{$ifdef CASTLE_GL_SUPPORTS_DEBUG}

procedure CallbackGLDebug(Source: TGLenum; AType: TGLenum; Id: TGLuint; Severity: TGLenum;
  Length: TGLsizei; Message: PAnsiChar; UserParam: Pointer);
  {$if     defined(OPENGL_CALLBACK_STDCALL)} stdcall;
  {$elseif defined(OPENGL_CALLBACK_CDECL)} cdecl;
  {$elseif defined(OPENGL_CALLBACK_DEFAULT)}
  {$else} {$message fatal 'Include openglmac.inc that should define OPENGL_CALLBACK_xxx'}
  {$endif}
var
  SourceStr, TypeStr, SeverityStr, MessageStr, MessageWhole: String;
begin
  case Source of
    GL_DEBUG_SOURCE_API            : SourceStr := 'API';
    GL_DEBUG_SOURCE_WINDOW_SYSTEM  : SourceStr := 'Window System';
    GL_DEBUG_SOURCE_SHADER_COMPILER: SourceStr := 'Shader Compiler';
    GL_DEBUG_SOURCE_THIRD_PARTY    : SourceStr := '3rd Party';
    GL_DEBUG_SOURCE_APPLICATION    : SourceStr := 'Application';
    GL_DEBUG_SOURCE_OTHER          : SourceStr := 'Other';
    else                             SourceStr := 'Unknown';
  end;

  case AType of
    GL_DEBUG_TYPE_ERROR              : TypeStr := 'GL error';
    GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR: TypeStr := 'Attempted use of deprecated features';
    GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR : TypeStr := 'Triggering of undefined behavior';
    GL_DEBUG_TYPE_PORTABILITY        : TypeStr := 'Portability issue';
    GL_DEBUG_TYPE_PERFORMANCE        : TypeStr := 'Performance notification';
    GL_DEBUG_TYPE_MARKER             : TypeStr := 'Marker';
    GL_DEBUG_TYPE_PUSH_GROUP         : TypeStr := 'Group push';
    GL_DEBUG_TYPE_POP_GROUP          : TypeStr := 'Group pop';
    GL_DEBUG_TYPE_OTHER              : TypeStr := 'Other';
    else                               TypeStr := 'Unknown';
  end;

  case Severity of
    GL_DEBUG_SEVERITY_LOW         : SeverityStr := 'Low';
    GL_DEBUG_SEVERITY_MEDIUM      : SeverityStr := 'Medium';
    GL_DEBUG_SEVERITY_HIGH        : SeverityStr := 'High';
    GL_DEBUG_SEVERITY_NOTIFICATION: SeverityStr := 'Notification';
    else                            SeverityStr := 'Unknown';
  end;

  MessageStr := Message;

  MessageWhole := Format('OpenGL debug: %s, %s, %d, %s: %s', [
    SourceStr,
    TypeStr,
    Id,
    SeverityStr,
    MessageStr
  ]);

  if AType = GL_DEBUG_TYPE_ERROR then
    raise EOpenGLError.Create(MessageWhole)
  else
  // Don't output notifcations, too much log every frame
  if Severity <> GL_DEBUG_SEVERITY_NOTIFICATION then
    WritelnLog(MessageWhole);
end;

{$ifdef USE_DGL}
type
  GLDEBUGPROC = TGLDEBUGPROC;
{$endif}

procedure TGLFeatures.InitializeDebug;
begin
  if HasExtension('GL_KHR_debug') then
  begin
    if not Assigned(glDebugMessageCallback) then raise Exception.Create('glDebugMessageCallback missing');
    if not Assigned(glDebugMessageControl) then raise Exception.Create('glDebugMessageControl missing');

    glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS);
    glDebugMessageCallback(
      {$ifdef USE_DGL} GLDEBUGPROC(@CallbackGLDebug)
      {$else} @CallbackGLDebug
      {$endif}, nil);
    WritelnLog('OpenGL(ES) debugging enabled, expect extra log messages');
  end;
end;

{$else CASTLE_GL_SUPPORTS_DEBUG}

procedure TGLFeatures.InitializeDebug;
begin
  WritelnWarning('Debugging rendering not available (WebGL does not have GL_KHR_debug)');
end;

{$endif CASTLE_GL_SUPPORTS_DEBUG}

