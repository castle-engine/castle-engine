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
  for Windows. }

uses
  Windows, SysUtils,
  CastleInternalContextWgl,
  CastleLog, CastleUtils;

{ TFmxOpenGLUtility --------------------------------------------------------- }

procedure TFmxOpenGLUtility.ContextAdjustEarly(const PlatformContext: TGLContext);
var
  WinContext: TGLContextWgl;
begin
  WinContext := PlatformContext as TGLContextWgl;
  WinContext.WndPtr :=
    (Control.Presentation as TWinPresentation).Handle;
  if WinContext.WndPtr = 0 then
    raise Exception.Create('Native handle not ready when calling ContextAdjustEarly');
  WinContext.h_Dc := GetWindowDC(WinContext.WndPtr);
end;

procedure TFmxOpenGLUtility.HandleNeeded;
var
  H: HWND;
begin
  if Control.Presentation = nil then
    raise EInternalError.CreateFmt('%s: Cannot use ControlHandleNeeded as Presentation not created yet', [Control.ClassName]);
  H := (Control.Presentation as TWinPresentation).Handle;
  if H = 0 { NullHWnd } then
    raise Exception.CreateFmt('%s: ControlHandleNeeded failed to create a handle', [Control.ClassName]);
end;

procedure TFmxOpenGLUtility.HandleRelease;
begin
end;

procedure TFmxOpenGLUtility.Update;
begin
end;

function TFmxOpenGLUtility.Scale: Single;
begin
  // this may be called at units finalization, when Handle is no longer available
  if Control.Presentation <> nil then
    Result := (Control.Presentation as TWinPresentation).Scale
  else
    Result := 1;
end;
