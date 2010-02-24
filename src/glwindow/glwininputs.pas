{
  Copyright 2003-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
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

uses GL, GLU, KambiGLUtils, GLWindow, GLWinModes, OpenGLFonts, KambiUtils, Images,
  KambiStringUtils;

{ Dziala w petli (Application.ProcessMessage) i wyswietla
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
  - realizuje wewnatrz petle GLWindow Application.ProcessMessage. W czasie
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

uses SysUtils, GLImages;

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
var
  SavedMode: TGLMode;
  Data: TGLWinInputData;
begin
  if FlushGLWindow then glwin.FlushRedisplay;
  Data.dlBGImage := SaveScreenWhole_ToDisplayList_noflush(ReadBuffer);
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

  SavedMode := TGLMode.CreateReset(glwin, 0, false,
    {$ifdef FPC_OBJFPC} @ {$endif} DrawGL, nil,
    {$ifdef FPC_OBJFPC} @ {$endif} NoClose, false);
  try
    Glwin.UserData := @Data;
    Glwin.OnKeyDown := @KeyDown;

    repeat Application.ProcessMessage(true) until Data.Answered;

    result := Data.Answer;
  finally SavedMode.Free end;
end;

{ gl window callbacks for GLWinInputAnyKey ------------------------------------ }

type
  TInputAnyKeyData = record
    DoClear: boolean;
    dlDrawImage: TGLuint;
    KeyPressed: boolean;
  end;
  PInputAnyKeyData = ^TInputAnyKeyData;

procedure DrawGLAnyKey(glwin: TGLWindow);
var D: PInputAnyKeyData;
begin
 D := PInputAnyKeyData(glwin.UserData);
 if D^.DoClear then glClear(GL_COLOR_BUFFER_BIT);
 glCallList(D^.dlDrawImage);
end;

procedure KeyDownAnyKey(glwin: TGLWindow; key: TKey; c: char);
var D: PInputAnyKeyData;
begin
 D := PInputAnyKeyData(glwin.UserData);
 D^.KeyPressed := true;
end;

{ GLWinInputAnyKey ----------------------------------------------------------- }

procedure InputAnyKeyCore(glwin: TGLWindow; dlDrawImage: TGLuint;
  RasterX, RasterY: Integer; BGImageWidth, BGImageHeight: Cardinal);
var
  Data: TInputAnyKeyData;
  savedMode: TGLMode;
begin
 SavedMode := TGLMode.CreateReset(glwin, GL_COLOR_BUFFER_BIT, false,
   {$ifdef FPC_OBJFPC} @ {$endif} DrawGLAnyKey, nil,
   {$ifdef FPC_OBJFPC} @ {$endif} NoClose, false);
 try
  glDisable(GL_ALPHA_TEST);

  Data.DoClear := (Cardinal(glwin.Width ) > BGImageWidth ) or
                  (Cardinal(glwin.Height) > BGImageHeight);
  Data.dlDrawImage := dlDrawImage;
  Data.KeyPressed := false;

  Glwin.UserData := @Data;
  Glwin.OnKeyDown := @KeyDownAnyKey;

  glRasterPos2i(RasterX, RasterY);
  repeat Application.ProcessMessage(true) until Data.KeyPressed;
 finally SavedMode.Free end;
end;

procedure InputAnyKey(glwin: TGLWindow; const Img: TImage;
  RasterX, RasterY: Integer);
var
  DL: TGLuint;
begin
  DL := ImageDrawToDisplayList(Img);
  try
    InputAnyKeyCore(glwin, DL, RasterX, RasterY, Img.Width, Img.Height);
  finally glFreeDisplayList(DL) end;
end;

procedure InputAnyKey(glwin: TGLWindow; const ImgFileName: string;
  ResizeX, ResizeY, RasterX, RasterY: Integer);
var
  DL: TGLuint;
  Image: TImage;
  BGImageWidth, BGImageHeight: Cardinal;
begin
  Image := LoadImage(ImgFileName, [TRGBImage], [], ResizeX, ResizeY);
  try
    BGImageWidth  := Image.Width ;
    BGImageHeight := Image.Height;
    DL := ImageDrawToDisplayList(Image);
  finally FreeAndNil(Image) end;
  try
    InputAnyKeyCore(glwin, DL, RasterX, RasterY, BGImageWidth, BGImageHeight);
  finally glFreeDisplayList(DL) end;
end;

procedure InputAnyKey(glwin: TGLWindow; ReadBuffer: TGLenum;
  FlushGLWindow: boolean; RasterX, RasterY: Integer);
var
  DL: TGLuint;
  BGImageWidth, BGImageHeight: Cardinal;
begin
  if FlushGLWindow then glwin.FlushRedisplay;
  DL := SaveScreenWhole_ToDisplayList_noflush(ReadBuffer, BGImageWidth, BGImageHeight);
  try
    InputAnyKeyCore(glwin, DL, RasterX, RasterY, BGImageWidth, BGImageHeight);
  finally glFreeDisplayList(DL) end;
end;

end.
