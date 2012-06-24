{
  Copyright 2003-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Waiting for user input, keeping static image displayed on TCastleWindowBase. }
unit CastleInputs;

{
  TODO
  - Input i InputAnyKey powinny byc scalone w jedno,
    razem z ich callbackami, przynajmniej OnDraw.
    Musza byc w stanie dobrze zareagowac na wypadek gdyby user
    zrobil resize na okienku.
}

{$I castleconf.inc}

interface

uses GL, GLU, CastleGLUtils, CastleWindow, WindowModes, OpenGLFonts, CastleUtils, Images,
  CastleStringUtils;

{ Wait until user inputs a string (accept by Enter), displaying the static
  image with user string. DLDrawImage must be a display list obtained by
  call like SaveScreen_ToDisplayList_NoFlush that draws the image.

  ScreenX0, ScreenY0 is raster position for lower-left screen corner.

  AnswerX0, AnswerY0 is raster position for displaying user answer.

  AnswerDefault, MinLength, MaxLength and AnswerAllowedChars
  have the same meaning as in CastleMessages unit. Initial Answer
  cannot contain characters outside AnswerAllowedChars. }
function Input(Window: TCastleWindowBase;
  DLDrawImage: TGLuint;
  Font: TGLBitmapFont_Abstract;
  ScreenX0, ScreenY0, AnswerX0, AnswerY0: Integer;
  AnswerDefault: string = '';
  MinLength: Integer = 0;
  MaxLength: Integer = 0;
  const AnswerAllowedChars: TSetOfChars = AllChars
  ): string;

{ Wait until user presses a key.

  Displays a given image on the screen while waiting.
  You can give image filename, or ready TCastleImage instance
  (must be renderable to OpenGL, i.e. by one of GLImages.PixelsImageClasses
  classes), or display list to render any image (in which case you
  have to tell us image size).

  RasterX, RasterY is the image position on the screen. In the background
  OpenGL clear color will be used.

  @groupBegin }
procedure InputAnyKey(Window: TCastleWindowBase; const ImgFileName: string;
  ResizeX, ResizeY, RasterX, RasterY: Integer); overload;
procedure InputAnyKey(Window: TCastleWindowBase; const Img: TCastleImage;
  RasterX, RasterY: Integer); overload;
procedure InputAnyKey(Window: TCastleWindowBase; DLDrawImage: TGLuint;
  RasterX, RasterY: Integer; BGImageWidth, BGImageHeight: Cardinal); overload;
{ @groupEnd }

implementation

uses SysUtils, GLImages;

{ gl window callbacks for GLWinInput -------------------------------------------- }

type
  TGLWinInputData = record
    { input params }
    DLDrawImage: TGLuint;
    MinLength, MaxLength: Integer;
    AnswerAllowedChars: TSetOfChars;
    Font: TGLBitmapFont_Abstract;
    ScreenX0, ScreenY0, AnswerX0, AnswerY0: Integer;

    { input/output params }
    Answer: string;
    Answered: boolean;
  end;
  PGLWinInputData = ^TGLWinInputData;

procedure DrawGL(Window: TCastleWindowBase);
var D: PGLWinInputData;
begin
 D := PGLWinInputData(Window.UserData);

 glRasterPos2i(D^.ScreenX0, D^.ScreenY0);
 glCallList(D^.DLDrawImage);
 glRasterPos2i(D^.AnswerX0, D^.AnswerY0);
 D^.Font.Print(D^.Answer+'_');
end;

procedure KeyDown(Window: TCastleWindowBase; key: TKey; c: Char);
var D: PGLWinInputData;
begin
 D := PGLWinInputData(Window.UserData);

 case c of
  CharBackSpace:
    if Length(D^.Answer) > 0 then
     begin SetLength(D^.Answer, Length(D^.Answer)-1); Window.PostRedisplay; end;
  CharEnter:
    if Between(Length(D^.Answer), D^.MinLength, D^.MaxLength) then
     D^.Answered := true;
  else
    if (c <> #0) and
       (c in D^.AnswerAllowedChars) and
       (Length(D^.Answer) < D^.MaxLength) then
     begin D^.Answer += c; Window.PostRedisplay; end;
 end;
end;

{ GLWinInput -------------------------------------------------------------- }

function Input(Window: TCastleWindowBase;
  DLDrawImage: TGLuint;
  Font: TGLBitmapFont_Abstract;
  ScreenX0, ScreenY0, AnswerX0, AnswerY0: Integer;
  AnswerDefault: string = '';
  MinLength: Integer = 0;
  MaxLength: Integer = 0;
  const AnswerAllowedChars: TSetOfChars = AllChars
  ): string;
var
  SavedMode: TGLMode;
  Data: TGLWinInputData;
begin
  Data.DLDrawImage := DLDrawImage;
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

  SavedMode := TGLMode.CreateReset(Window, 0, false,
    {$ifdef FPC_OBJFPC} @ {$endif} DrawGL, nil,
    {$ifdef FPC_OBJFPC} @ {$endif} NoClose);
  try
    Window.UserData := @Data;
    Window.OnKeyDown := @KeyDown;

    repeat Application.ProcessMessage(true, true) until Data.Answered;

    result := Data.Answer;
  finally SavedMode.Free end;
end;

{ gl window callbacks for GLWinInputAnyKey ------------------------------------ }

type
  TInputAnyKeyData = record
    DoClear: boolean;
    DLDrawImage: TGLuint;
    KeyPressed: boolean;
  end;
  PInputAnyKeyData = ^TInputAnyKeyData;

procedure DrawGLAnyKey(Window: TCastleWindowBase);
var D: PInputAnyKeyData;
begin
 D := PInputAnyKeyData(Window.UserData);
 if D^.DoClear then glClear(GL_COLOR_BUFFER_BIT);
 glCallList(D^.DLDrawImage);
end;

procedure KeyDownAnyKey(Window: TCastleWindowBase; key: TKey; c: char);
var D: PInputAnyKeyData;
begin
 D := PInputAnyKeyData(Window.UserData);
 D^.KeyPressed := true;
end;

{ GLWinInputAnyKey ----------------------------------------------------------- }

procedure InputAnyKey(Window: TCastleWindowBase; DLDrawImage: TGLuint;
  RasterX, RasterY: Integer; BGImageWidth, BGImageHeight: Cardinal);
var
  Data: TInputAnyKeyData;
  savedMode: TGLMode;
begin
 SavedMode := TGLMode.CreateReset(Window, GL_COLOR_BUFFER_BIT, false,
   {$ifdef FPC_OBJFPC} @ {$endif} DrawGLAnyKey, nil,
   {$ifdef FPC_OBJFPC} @ {$endif} NoClose);
 try
  glDisable(GL_ALPHA_TEST);

  Data.DoClear := (Cardinal(Window.Width ) > BGImageWidth ) or
                  (Cardinal(Window.Height) > BGImageHeight);
  Data.DLDrawImage := DLDrawImage;
  Data.KeyPressed := false;

  Window.UserData := @Data;
  Window.OnKeyDown := @KeyDownAnyKey;

  glRasterPos2i(RasterX, RasterY);
  repeat Application.ProcessMessage(true, true) until Data.KeyPressed;
 finally SavedMode.Free end;
end;

procedure InputAnyKey(Window: TCastleWindowBase; const Img: TCastleImage;
  RasterX, RasterY: Integer);
var
  DL: TGLuint;
begin
  DL := ImageDrawToDisplayList(Img);
  try
    InputAnyKey(Window, DL, RasterX, RasterY, Img.Width, Img.Height);
  finally glFreeDisplayList(DL) end;
end;

procedure InputAnyKey(Window: TCastleWindowBase; const ImgFileName: string;
  ResizeX, ResizeY, RasterX, RasterY: Integer);
var
  DL: TGLuint;
  Image: TCastleImage;
  BGImageWidth, BGImageHeight: Cardinal;
begin
  Image := LoadImage(ImgFileName, [TRGBImage], [], ResizeX, ResizeY);
  try
    BGImageWidth  := Image.Width ;
    BGImageHeight := Image.Height;
    DL := ImageDrawToDisplayList(Image);
  finally FreeAndNil(Image) end;
  try
    InputAnyKey(Window, DL, RasterX, RasterY, BGImageWidth, BGImageHeight);
  finally glFreeDisplayList(DL) end;
end;

end.
