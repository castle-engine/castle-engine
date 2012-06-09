{
  Copyright 2006-2012 Michalis Kamburelis.

  This file is part of "castle".

  "castle" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "castle" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "castle"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ Global @link(Window) variable and other features useful for typical 3D game. }
unit CastleGameWindow;

interface

uses CastleWindow, GLRenderer, OpenGLTTFonts;

var
  { @noAutoLinkHere }
  Window: TCastleWindowCustom;

  GLContextCache: TGLRendererContextCache;

  { Just a generally usable OpenGL outline (3D) font. }
  Font3d: TGLOutlineFont;

implementation

uses SysUtils, X3DNodes, GLAntiAliasing, UIControls;

{ initialization / finalization ---------------------------------------------- }

const
  Font3dFamily = ffSans;
  Font3dBold = false;
  Font3dItalic = false;

procedure WindowOpen(const Container: IUIContainer);
begin
  Font3d := GLContextCache.Fonts_IncReference(
    Font3dFamily, Font3dBold, Font3dItalic,
    TFontStyleNode.ClassTTF_Font(Font3dFamily, Font3dBold, Font3dItalic));

  AntiAliasingGLOpen;
  AntiAliasingEnable;
end;

procedure WindowClose(const Container: IUIContainer);
begin
  if (GLContextCache <> nil) and (Font3d <> nil) then
  begin
    GLContextCache.Fonts_DecReference(Font3dFamily, Font3dBold, Font3dItalic);
    Font3d := nil;
  end;
end;

initialization
  Window := TCastleWindowCustom.Create(nil);
  Window.OnDrawStyle := ds3D;

  GLContextCache := TGLRendererContextCache.Create;

  OnGLContextOpen.Add(@WindowOpen);
  OnGLContextClose.Add(@WindowClose);
finalization
  { Fonts_DecReference must be called before freeing GLContextCache.
    It's called from Window.Close. But Window.Close may be called when
    FreeAndNil(Window) below, so to make sure we call Fonts_DecReference
    (by our WindowClose) right now. }
  WindowClose(Window);

  FreeAndNil(GLContextCache);
  FreeAndNil(Window);
end.
