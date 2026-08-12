{%MainUnit castleinternalfmxutils.pas}
{
  Copyright 2023-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Implementation of CastleInternalFmxUtils (initialize OpenGL context in FMX,
  code shared between FMX TOpenGLControl and FMX TCastleControl)
  for other OSes (with no OS-specific implementation). }

{$ifdef read_TFmxOpenGLUtility_interface}
strict private
  HandleInitialized: Boolean;
{$endif read_TFmxOpenGLUtility_interface}

{$ifdef read_implementation}

{ TFmxOpenGLUtility --------------------------------------------------------- }

procedure TFmxOpenGLUtility.ContextAdjustEarly(const PlatformContext: TGLContext);
begin
end;

procedure TFmxOpenGLUtility.HandleNeeded;
begin
  { We track HandleInitialized merely to call OnHandleAfterCreateEvent.
    There's no need to actually *do* any handle allocation here,
    this is used together with CastleInternalContextExisting.
    But we need to call OnHandleAfterCreateEvent,
    as Fmx.CastleInternalGLControl depends on it,
    to call FPlatformContext.Initialize to mark context as initialized. }
  if not HandleInitialized then
  begin
    HandleInitialized := true;
    if Assigned(OnHandleAfterCreateEvent) then
      OnHandleAfterCreateEvent();
  end;
end;

procedure TFmxOpenGLUtility.HandleRelease;
begin
  if HandleInitialized then
  begin
    HandleInitialized := false;
    if Assigned(OnHandleBeforeDestroyEvent) then
      OnHandleBeforeDestroyEvent();
  end;
end;

procedure TFmxOpenGLUtility.Update;
begin
end;

function TFmxOpenGLUtility.Scale: Single;
begin
  Result := 1;
end;

{$endif read_implementation}
