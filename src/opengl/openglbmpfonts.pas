{
  Copyright 2001-2011 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ OpenGL bitmap fonts (TGLBitmapFont). }

unit OpenGLBmpFonts;

interface

uses BmpFontsTypes, OpenGLFonts, GL, GLU, KambiGLUtils;

type
  { OpenGL bitmap font. Uses a font description from BmpFontsTypes unit
    (PBmpFont type), and creates OpenGL resources to render text
    using this font.

    This way we can use fonts embedded in our source code
    (as PBmpFont type), independent from the fonts available in external
    files, operating system and such. Or we can load PBmpFont from file.

    See also TGLOutlineFont for a similar class for outline fonts. }
  TGLBitmapFont = class(TGLBitmapFont_Abstract)
  private
    base: TGLuint;
    bmpfont: PBmpFont;
  public
    { Create OpenGL resources to render given bitmap font.

      We remember the pointer BitmapFont, without copying the contents.
      So do not free the BitmapFont contents, do not change it at all
      actually, during the lifetime of this object. }
    constructor Create(BitmapFont: PBmpFont);
    destructor Destroy; override;

    procedure PrintAndMove(const s: string); override;
    function TextWidth(const s: string): integer; override;
    function TextHeight(const s: string): integer; override;
    function TextHeightBase(const s: string): integer; override;
  end;

implementation

uses KambiUtils;

const BmpTableCount = Ord(High(char)) - Ord(Low(char)) +1;

constructor TGLBitmapFont.Create(BitmapFont: PBmpFont);
var i: Cardinal;
    Znak: PBFNTChar;
    Saved_Unpack_Alignment: TGLint;
begin
 inherited Create;
 base := glGenListsCheck(BmpTableCount, 'TGLBitmapFont.Create');
 Self.BmpFont := BitmapFont;
 Saved_Unpack_Alignment := glGetInteger(GL_UNPACK_ALIGNMENT);

 for i := 0 to 255 do
 begin
  Znak := BmpFont^[Chr(i)];
  glPixelStorei(GL_UNPACK_ALIGNMENT, Znak^.Info.Alignment);
  glNewList(i+base, GL_COMPILE);
  glBitmap(Znak^.Info.Width, Znak^.Info.Height,
           Znak^.Info.XOrig, Znak^.Info.YOrig,
           Znak^.Info.XMove, Znak^.Info.YMove,
           @Znak^.Data);
  glEndList;
 end;
 glPixelStorei(GL_UNPACK_ALIGNMENT, Saved_Unpack_Alignment);

 FRowHeight := TextHeight('Wy') + 2;
 { RowHeight zwiekszylem o +2 zeby byl odstep miedzy liniami.
   TODO: this +2 is actually a bad idea, but can't remove now without careful testing. }
 { For RowHeightBase, I do not use +2. }
 FRowHeightBase := TextHeightBase('W');
end;

destructor TGLBitmapFont.Destroy;
begin
 glDeleteLists(base, BmpTableCount);
 inherited;
end;

procedure TGLBitmapFont.PrintAndMove(const s: string);
begin
 glPushAttrib(GL_LIST_BIT);
   glListIBase(TGLint(base));
   glCallLists(length(s), GL_UNSIGNED_BYTE, PChar(s));
 glPopAttrib;
end;

function TGLBitmapFont.TextWidth(const s: string): integer;
var i: integer;
begin
 Result := 0;
 for i := 1 to length(s) do
  Result := Result + Round(BmpFont^[s[i]]^.Info.XMove);
end;

function TGLBitmapFont.TextHeight(const s: string): integer;
var
  i: integer;
  minY, maxY, YOrig: integer;
begin
  minY := 0;
  maxY := 0;
  for i := 1 to length(s) do
  begin
    YOrig := Round(BmpFont^[s[i]]^.Info.YOrig);
    MinTo1st(minY, -YOrig);
    { Yes, YOrig is *subtracted* here, see glBitmap meaning of yorig. }
    MaxTo1st(maxY, BmpFont^[s[i]]^.Info.Height - YOrig);
  end;
  result := maxY - minY;
end;

function TGLBitmapFont.TextHeightBase(const s: string): integer;
var
  I: integer;
  YOrig: integer;
begin
  Result := 0;
  { This is just like TGLBitmapFont.TextHeight implementation, except we only
    calculate (as Result) the MaxY value (assuming that MinY is zero). }
  for i := 1 to length(s) do
  begin
    YOrig := Round(BmpFont^[s[i]]^.Info.YOrig);
    MaxTo1st(Result, BmpFont^[s[i]]^.Info.Height - YOrig);
  end;
end;

end.
