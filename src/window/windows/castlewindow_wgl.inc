{%MainUnit ../castlewindow.pas}
{
  Copyright 2013-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Use this from castlewindow_winapi.inc to make a CastleWindow backend
  using WinAPI and wgl. Initializes window on Windows with OpenGL context.

  At this point this is a thin wrapper that uses CastleIntenalContextWgl
  to do the actual job. }

{$ifdef read_interface_uses}
CastleInternalContextBase,
CastleInternalContextWgl,
{$endif}

{$ifdef read_window_interface}
private
  Context: TGLContextWGL;

  { Methods compatible with castlewindow_egl.inc.
    @groupBegin }
  procedure ContextCreateEnd(WndPtr: HWND);
  procedure ContextDestroy;
  { @groupEnd }
{$endif}

{$ifdef read_implementation}

procedure TCastleWindow.ContextCreateEnd(WndPtr: HWND);
var
  Requirements: TGLContextRequirements;
begin
  Requirements := TGLContextRequirements.Create(Self);
  Requirements.DoubleBuffer := DoubleBuffer;
  Requirements.ColorBits := ColorBits;
  Requirements.RedBits := RedBits;
  Requirements.GreenBits := GreenBits;
  Requirements.BlueBits := BlueBits;
  Requirements.DepthBits := DepthBits;
  Requirements.AlphaBits := AlphaBits;
  Requirements.StencilBits := StencilBits;
  Requirements.MultiSampling := MultiSampling;

  Context := TGLContextWGL.Create;
  Context.WndPtr := WndPtr;
  Context.h_Dc := h_Dc;
  { All OpenGL contexts created by CGE should be shared }
  if Application.OpenWindowsCount <> 0 then
    Context.SharedContext := Application.OpenWindows[0].Context;
  Context.ContextCreate(Requirements);
end;

procedure TCastleWindow.ContextDestroy;
begin
  if Context <> nil then
  begin
    Context.ContextDestroy;
    FreeAndNil(Context);
  end;
end;

procedure TCastleWindow.BackendMakeCurrent;
begin
  Assert(not Closed);
  Assert(Context <> nil);
  Context.MakeCurrent;
end;

procedure TCastleWindow.SwapBuffers;
begin
  Assert(not Closed);
  Assert(Context <> nil);
  Context.SwapBuffers;
end;

{$endif read_implementation}
