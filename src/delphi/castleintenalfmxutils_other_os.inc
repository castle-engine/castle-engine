{%MainUnit castleinternalfmxutils.pas}
{
  Copyright 2023-2023 Michalis Kamburelis.

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

{ TFmxOpenGLUtility --------------------------------------------------------- }

procedure TFmxOpenGLUtility.ContextAdjustEarly(const PlatformContext: TGLContext);
begin
end;

procedure TFmxOpenGLUtility.HandleNeeded;
begin
end;

procedure TFmxOpenGLUtility.HandleRelease;
begin
end;

procedure TFmxOpenGLUtility.Update;
begin
end;
