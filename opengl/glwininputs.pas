{
  Copyright 2003-2005 Michalis Kamburelis.

  This file is part of "Kambi's OpenGL Pascal units".

  "Kambi's OpenGL Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's OpenGL Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's OpenGL Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ @abstract(Idea jest zapisanie tutaj funkcji ktore w jakis sposob beda czekaly az
  user cos przycisnie / poda z klawiatury ale w przeciwienstwie do
  modulu GLWinMessages tutejszy modul nie przygotowuje zadnego interfejsu
  do tego - uzycie procedur z tego modulu bedzie wymagalo
  od ciebie przygotowania najpierw odpowiedniego obrazka (w buforze
  kolorow OpenGLa albo w strukturze TImage).)

  Komentarze do parametrow "ReadBuffer, FlushGLWindow" :
    Na poczatku funkcja robi save screen z ReadBuffer.
    Jesli FlushGLWindow to robi najpierw glwin.FlushRedisplay (generalnie,
    powinienes robic FlushGLWindow wtedy i tylko wtedy gdy ReadBuffer = GL_FRONT).
  Komentarze do parametrow "ImageFileName: string / Image: TImage" :
    Podany ImageRec musi miec Kind in ImageGLFormats.
    Alpha obrazka bedzie ignorowane.
}

unit GLWinInputs;

{
  TODO
  - Input i InputAnyKey powinny byc scalone w jedno,
    razem z ich callbackami, przynajmniej OnDraw.
    Musza byc w stanie dobrze zareagowac na wypadek gdyby user
    zrobil resize na okienku.
  - Input, InputAnyKey, GLWinModes.TGLModeFrozenScreen should be fixed
    to not be vulnerable for "glReadPixels from front buffer is not reliable"
    problem.
}

{$I kambiconf.inc}

interface

uses OpenGLh, GLWindow, GLWinModes, OpenGLFonts, KambiUtils, Images,
  KambiStringUtils;

{ Dziala w petli (glwm.ProcessMessage) i wyswietla
  zlapany obrazek (musisz podac ScreenX0, Y0 = taka pozycja rastera ze jest
  ona lewym dolnym rogiem ekranu) a na nim - wczytywany string
  (na pozycji glRasterPos2i(AnswerX0, AnswerY0)).

  Znaczenie AnswerDefault, MinLength, MaxLength i AnswerAllowedChars
  jest jasne, takie samo jak w GLWinMessages. Podobnie jak tam,
  dzialaja one dobrze pod warunkiem ze poczatkowe Answer nie zawiera znakow
  spoza AnswerAllowedChars. }
function Input(glwin: TGLWindow;
  ReadBuffer: TGLenum; FlushGLWindow: boolean;
  Font: TGLBitmapFont_Abstract;
  ScreenX0, ScreenY0, AnswerX0, AnswerY0: Integer;
  AnswerDefault: string {$ifdef DEFPARS} = ''{$endif};
  MinLength: Integer {$ifdef DEFPARS} = 0{$endif};
  MaxLength: Integer {$ifdef DEFPARS} = 0{$endif};
  const AnswerAllowedChars: TSetOfChars {$ifdef DEFPARS} = AllChars{$endif}
  ): string;

{ Czeka na nacisniecie dowolnego klawisza aby wyjsc,
  obrazek wyswietlany na glRasterPos2i RasterX, Y

  Szzegoly:
  - jezeli obrazek jest mniejszy niz glwin.Width/Height to robi glClear
    (GL_COLOR_BUFFER_BIT) aby wyczyscic kazdorazowo reszte okienka.
    Wiec aktualny kolor clear OpenGLa ma wtedy znaczenie.
  - realizuje wewnatrz petle GLWindow glwm.ProcessMessage. W czasie
    wyswietlania obrazka user nie moze wyjsc z programu, closequery jest
    wylaczone itp. Jedyna co user moze zrobic to nacisnac dowolny klawisz.

  InputAnyKey(..ReadBuffer, FlushGLWindow...) pokazuje obrazek ktory aktualnie
  jest narysowany w danym buforze. W ten sposob ta funkcja dziala jako
  "press any key" - po prostu nic nie rysuje nowego na ekranie,
  czeka tylko az przycisniesz klawisz. Podane tu RasterX, RasterY
  musi w tym przypadku oznaczac ScreenX0, Y0 czyli lewy dolny rog okienka,
  no chyba ze chcesz wyswietlac aktualny obraz jakos przesuniety. }
procedure InputAnyKey(glwin: TGLWindow; const ImgFileName: string;
  ResizeX, ResizeY, RasterX, RasterY: Integer); overload;
procedure InputAnyKey(glwin: TGLWindow; const Img: TImage;
  RasterX, RasterY: Integer); overload;
procedure InputAnyKey(glwin: TGLWindow; ReadBuffer: TGLenum; FlushGLWindow: boolean;
  RasterX, RasterY: Integer); overload;

implementation

uses KambiGLUtils;

{ gl window callbacks for GLWinInput -------------------------------------------- }

type
  TGLWinInputData = record
    { input params }
    dlBGImage: TGLuint;
    MinLength, MaxLength: Integer;
    AnswerAllowedChars: TSetOfChars;
    Font: TGLBitmapFont_Abstract;
    ScreenX0, ScreenY0, AnswerX0, AnswerY0: Integer;

    { input/output params }
    Answer: string;
    Answered: boolean;
  end;
  PGLWinInputData = ^TGLWinInputData;

procedure DrawGL(glwin: TGLWindow);
var D: PGLWinInputData;
begin
 D := PGLWinInputData(glwin.UserData);

 glRasterPos2i(D^.ScreenX0, D^.ScreenY0);
 glCallList(D^.dlBGImage);
 glRasterPos2i(D^.AnswerX0, D^.AnswerY0);
 D^.Font.Print(D^.Answer+'_');
end;

procedure KeyDown(glwin: TGLWindow; key: TKey; c: Char);
var D: PGLWinInputData;
begin
 D := PGLWinInputData(glwin.UserData);

 case c of
  CharBackSpace:
    if Length(D^.Answer) > 0 then
     begin SetLength(D^.Answer, Length(D^.Answer)-1); glwin.PostRedisplay; end;
  CharEnter:
    if Between(Length(D^.Answer), D^.MinLength, D^.MaxLength) then
     D^.Answered := true;
  else
    if (c <> #0) and
       (c in D^.AnswerAllowedChars) and
       (Length(D^.Answer) < D^.MaxLength) then
     begin D^.Answer += c; glwin.PostRedisplay; end;
 end;
end;

{ GLWinInput -------------------------------------------------------------- }

function Input(glwin: TGLWindow;
  ReadBuffer: TGLenum; FlushGLWindow: boolean;
  Font: TGLBitmapFont_Abstract;
  ScreenX0, ScreenY0, AnswerX0, AnswerY0: Integer;
  AnswerDefault: string {$ifdef DEFPARS} = ''{$endif};
  MinLength: Integer {$ifdef DEFPARS} = 0{$endif};
  MaxLength: Integer {$ifdef DEFPARS} = 0{$endif};
  const AnswerAllowedChars: TSetOfChars {$ifdef DEFPARS} = AllChars{$endif}
  ): string;
var SavedMode: TGLMode;
    Data: TGLWinInputData;
begin
 SavedMode := TGLMode.Create(glwin, 0, false);
 try
  if FlushGLWindow then glwin.FlushRedisplay;
  Data.dlBGImage := SaveScreenToDisplayList_noflush(ReadBuffer);
  Data.Answer := AnswerDefault;
  Data.MinLength := MinLength;
  Data.MaxLength := MaxLength;
  Data.AnswerAllowedChars := AnswerAllowedChars;
  Data.Answered := false;
  Data.Font := Font;
  Data.ScreenX0 := ScreenX0;
  Data.ScreenY0 := ScreenY0;
  Data.AnswerX0 := AnswerX0;
  Data.AnswerY0 := AnswerY0;

  SetStdNoCloseGLWindowState(glwin, @DrawGL, nil, @Data, false,
    false, false, K_None, false, false);
  glwin.OnKeyDown := @KeyDown;

  repeat glwm.ProcessMessage(true) until Data.Answered;

  result := Data.Answer;
 finally SavedMode.Free end;
end;

{ gl window callbacks for GLWinInputAnyKey ------------------------------------ }

type
  TInputAnyKeyData = record
    DoClear: boolean;
    dlImage: TGLuint;
    KeyPressed: boolean;
  end;
  PInputAnyKeyData = ^TInputAnyKeyData;

procedure DrawGLAnyKey(glwin: TGLWindow);
var D: PInputAnyKeyData;
begin
 D := PInputAnyKeyData(glwin.UserData);
 if D^.DoClear then glClear(GL_COLOR_BUFFER_BIT);
 glCallList(D^.dlImage);
end;

procedure KeyDownAnyKey(glwin: TGLWindow; key: TKey; c: char);
var D: PInputAnyKeyData;
begin
 D := PInputAnyKeyData(glwin.UserData);
 D^.KeyPressed := true;
end;

{ GLWinInputAnyKey ----------------------------------------------------------- }

procedure InputAnyKey(glwin: TGLWindow; const Img: TImage; RasterX, RasterY: Integer);
var Data: TInputAnyKeyData;
    savedMode: TGLMode;
begin
 SavedMode := TGLMode.Create(glwin, GL_COLOR_BUFFER_BIT, false);
 try
  glDisable(GL_ALPHA_TEST);

  Data.DoClear := (glwin.Width > Img.Width) or (glwin.Height > Img.Height);
  Data.dlImage := ImageDrawToDisplayList(Img);
  Data.KeyPressed := false;

  try
   SetStdNoCloseGLWindowState(glwin, @DrawGLAnyKey, nil, @Data,
     false, false, false, K_None, false, false);
   glwin.OnKeyDown := @KeyDownAnyKey;

   glRasterPos2i(RasterX, RasterY);
   repeat glwm.ProcessMessage(true) until Data.KeyPressed;

  finally glDeleteLists(Data.dlImage, 1) end;
 finally SavedMode.Free end;
end;

procedure InputAnyKey(glwin: TGLWindow; const ImgFileName: string; ResizeX, ResizeY, RasterX, RasterY: Integer);
var Img: TImage;
begin
 Img := LoadImage(ImgFileName, [TRGBImage], [], ResizeX, ResizeY);
 try
  InputAnyKey(glwin, Img, RasterX, RasterY);
 finally Img.Free end;
end;

procedure InputAnyKey(glwin: TGLWindow; ReadBuffer: TGLenum; FlushGLWindow: boolean;
  RasterX, RasterY: Integer);
var Img: TImage;
begin
 if FlushGLWindow then glwin.FlushRedisplay;
 Img := SaveScreen_noflush(ReadBuffer);
 try
  InputAnyKey(glwin, Img, RasterX, RasterY);
 finally Img.Free end;
end;

end.
