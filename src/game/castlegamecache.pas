{
  Copyright 2006-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Global things useful for typical 3D game. }
unit CastleGameCache;

interface

uses GLRenderer, OpenGLTTFonts;

var
  GLContextCache: TGLRendererContextCache;

  { Just a generally usable OpenGL outline (3D) font. }
  Font3d: TGLOutlineFont;

implementation

uses SysUtils, X3DNodes, UIControls;

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
  GLContextCache := TGLRendererContextCache.Create;

  OnGLContextOpen.Add(@WindowOpen);
  OnGLContextClose.Add(@WindowClose);
finalization
  { Fonts_DecReference must be called before freeing GLContextCache.
    It's called by OnGLContextClose. But OnGLContextClose may be invoked later,
    so make sure we call Fonts_DecReference (by our WindowClose) right now. }
  WindowClose(nil);

  FreeAndNil(GLContextCache);
end.
