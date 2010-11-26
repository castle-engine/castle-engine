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

{ Waiting for user input, keeping static image displayed on TGLWindow. }
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

{ Wait until user inputs a string (accept by Enter), displaying the static
  image with user string.

  At the beginning, we capture the screen from OpenGL ReadBuffer.
  If FlushGLWindow then we'll make Window.FlushRedisplay before capturing
  (you should set FlushGLWindow = @true when ReadBuffer = GL_FRONT).

  ScreenX0, ScreenY0 is raster position for lower-left screen corner

  AnswerX0, AnswerY0 is raster position for displaying user answer.

  AnswerDefault, MinLength, MaxLength and AnswerAllowedChars
  have the same meaning as in GLWinMessages unit. Initial Answer
  cannot contain characters outside AnswerAllowedChars. }
function Input(glwin: TGLWindow;
  ReadBuffer: TGLenum; FlushGLWindow: boolean;
  Font: TGLBitmapFont_Abstract;
  ScreenX0, ScreenY0, AnswerX0, AnswerY0: Integer;
  AnswerDefault: string = '';
  MinLength: Integer = 0;
  MaxLength: Integer = 0;
  const AnswerAllowedChars: TSetOfChars = AllChars
  ): string;

{ Wait until user presses a key.

  Displays a given image on the screen while waiting.
  You can give image filename, or ready TImage instance
  (must be renderable to OpenGL, i.e. by one of GLImages.PixelsImageClasses
  classes).

  RasterX, RasterY is the image position on the screen. In the background
  OpenGL clear color will be used.

  You can also allow to capture the screen contents.
  See @link(Input) for ReadBuffer, FlushGLWindow spec.
  In this case, RasterX, RasterY should be position of lower-left
  screen corner (unless you actully want to shift the displayed screen).
  @groupBegin }
procedure InputAnyKey(glwin: TGLWindow; const ImgFileName: string;
  ResizeX, ResizeY, RasterX, RasterY: Integer); overload;
procedure InputAnyKey(glwin: TGLWindow; const Img: TImage;
  RasterX, RasterY: Integer); overload;
procedure InputAnyKey(glwin: TGLWindow; ReadBuffer: TGLenum; FlushGLWindow: boolean;
  RasterX, RasterY: Integer); overload;
{ @groupEnd }

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
