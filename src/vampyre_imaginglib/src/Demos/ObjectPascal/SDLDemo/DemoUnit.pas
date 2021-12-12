{
  Vampyre Imaging Library Demo
  SDL Demo (SDL extension)

  Demo that shows how to create SDL surfaces from Imaging's
  images and vice versa. SDL window is opened and background
  and sprite surfaces are loaded and blitted to window. You can change
  sprite's data format by pressing SPACE key (it cycles trough all
  TImageFormat values) and toggle alpha blending (working only
  when sprite's current format has alpha channel) and color keying.
  Sprite can be moved accross the screen using arrow keys.
  Screenshots can also be taken. Status of the sprite
  and list of active keys are shown in the console window.
 }
unit DemoUnit;

{$I ImagingOptions.inc}
{$R ..\Common\MainIcon.res}

interface

procedure RunDemo;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils,
  sdl,
  ImagingTypes,
  Imaging,
  ImagingSdl,
  ImagingUtility,
  DemoUtils;

const
  DisplayWidth = 800;
  DisplayHeight = 600;
  SIconFile = 'Icon.png';
  SBackImageFile = 'Tigers.jpg';
  SSpriteImageFile = 'Vezyr.png';
  SOutScreenFile = 'SDLScreen.png';
  SOutSpriteFile = 'SDLSprite.png';
  SWindowTitle = 'Vampyre Imaging Library (version: %s) SDL Demo';
  SWindowIconTitle = 'SDL Demo';

var
  VideoInfo: PSDL_VideoInfo;
  Flags: LongWord = SDL_HWPALETTE;
  DisplaySurface: PSDL_Surface = nil;
  BackSurface: PSDL_Surface = nil;
  SpriteSurface: PSDL_Surface = nil;
  SpriteImage: TImageData;
  Event : TSDL_Event;
  Running: Boolean = True;
  AlphaBlending: Boolean = True;
  ColorKeying: Boolean = True;
  SpriteFormat: TImageFormat = ifA8R8G8B8;
  SpriteX: LongInt = 100;
  SpriteY: LongInt = 50;
  Keys: PByteArray;
  Info: TImageFormatInfo;
  Frames: LongInt = 0;
  LastTime: LongInt = 0;

procedure MessageOut(const Msg: string; const Args: array of const);
begin
  WriteLn(Format(Msg, Args));
end;

procedure MessageOutAndHalt(const Msg: string; const Args: array of const);
begin
  WriteLn('Error: ');
  MessageOut('  ' + Msg, Args);
  WriteLn('Press RETURN to exit');
  ReadLn;
  Halt(1);
end;

procedure ConvertSprite(Format: TImageFormat);
var
  Surface: PSDL_Surface;
  AlphaMsg: string;
  Key: UInt32;
begin
  Key := 0;
  SDL_FreeSurface(SpriteSurface);
  // Convert sprite image to SDL surface with Format override
  Surface := ImagingSDL.CreateSDLSurfaceFromImage(SpriteImage, SDL_SWSURFACE, Format);
  // Convert to display format for faster blits and use alpha
  // if enabled and present
  if (Surface.format.Aloss <> 8) and AlphaBlending then
  begin
    if ColorKeying then
    begin
      // Set color key if enabled
      Move(Surface.pixels^, Key, Surface.format.BytesPerPixel);
      SDL_SetColorKey(Surface, SDL_SRCCOLORKEY or SDL_RLEACCEL, Key);
    end;
    SpriteSurface := SDL_DisplayFormatAlpha(Surface);
  end
  else
  begin
    SpriteSurface := SDL_DisplayFormat(Surface);
    if ColorKeying then
    begin
      // Set color key if enabled
      Move(SpriteSurface.pixels^, Key, SpriteSurface.format.BytesPerPixel);
      SDL_SetColorKey(SpriteSurface, SDL_SRCCOLORKEY or SDL_RLEACCEL, Key);
    end;
  end;
  if SpriteSurface = nil then
    MessageOutAndHalt('Cannot create sprite surface: %s', [SDL_GetError]);
  SDL_FreeSurface(Surface);

  // Output sprite info
  Imaging.GetImageFormatInfo(Format, Info);
  MessageOut('Sprite converted', []);
  MessageOut('  Current sprite format: %s', [Info.Name]);
  AlphaMsg := Iff((SpriteSurface.format.Aloss <> 8) and (SpriteSurface.format.palette = nil),
    'Enabled', 'Disabled or not supported in this format');
  MessageOut('  Alpha blending: %s', [AlphaMsg]);
  MessageOut('  Color keying: %s', [Iff(ColorKeying, 'Enabled', 'Disabled')]);
end;

procedure Initialize;
var
  Image: TImageData;
  Surface: PSDL_Surface;
{$IFDEF MSWINDOWS}
  Caption, Icon: PAnsiChar;
  WindowHandle: THandle;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  SDL_WM_GetCaption(Caption, Icon);
  WindowHandle := FindWindowA('SDL_app', Caption);
  if WindowHandle <> 0 then
  // Place window to the center of the screen
    SetWindowPos(WindowHandle, 0, (GetSystemMetrics(SM_CXSCREEN) - DisplayWidth) div 2,
      (GetSystemMetrics(SM_CYSCREEN) - DisplayHeight - 20) div 2, 0, 0, SWP_NOSIZE or SWP_NOZORDER);
{$ENDIF}

  // Load background image from file, resize it to fit the window,
  // convert it to SDL surface and convert this surface to
  // display format for faster blitting
  if not Imaging.LoadImageFromFile(GetDataDir + PathDelim + SBackImageFile, Image) then
    MessageOutAndHalt('Cannot load background image: %s', [SBackImageFile]);
  Imaging.ResizeImage(Image, DisplayWidth, DisplayHeight, rfBilinear);
  Surface := ImagingSDL.CreateSDLSurfaceFromImage(Image, SDL_SWSURFACE);
  BackSurface := SDL_DisplayFormat(Surface);
  Imaging.FreeImage(Image);
  SDL_FreeSurface(Surface);
  if BackSurface = nil then
    MessageOutAndHalt('Cannot create background surface.', []);

  // Load sprite image
  if not Imaging.LoadImageFromFile(GetDataDir + PathDelim + SSpriteImageFile, SpriteImage) then
    MessageOutAndHalt('Cannot load sprite image: %s', [SSpriteImageFile]);
  ConvertSprite(SpriteFormat);
end;

procedure Present;
var
  Dest: TSDL_Rect;
begin
  Dest.x := SpriteX;
  Dest.y := SpriteY;
  Dest.w := SpriteSurface.w;
  Dest.h := SpriteSurface.h;

  SDL_BlitSurface(BackSurface, nil, DisplaySurface, nil);
  SDL_BlitSurface(SpriteSurface, nil, DisplaySurface, @Dest);
  SDL_UpdateRect(DisplaySurface, 0, 0, DisplayWidth, DisplayHeight);
end;

procedure Finalize;
begin
  // Free all surfaces and images
  SDL_FreeSurface(BackSurface);
  SDL_FreeSurface(SpriteSurface);
  Imaging.FreeImage(SpriteImage);
end;

procedure RunDemo;
begin
  MessageOut('Vampyre Imaging Library Demo - SDL (version %s)' + sLineBreak +
    'written by Marek Mauder' + sLineBreak, [Imaging.GetVersionStr]);
  MessageOut('Keys (when SDL window has focus): ' + sLineBreak +
    '  SPACE - cycle image data formats' + sLineBreak +
    '  A     - toggle alpha blending (only if alpha channel is present)' + sLinebreak +
    '  C     - toggle color keying' + sLineBreak +
    '  S     - take screenshot and save (%s)' + sLineBreak +
    '  D     - save sprite surface (%s)' + sLineBreak +
    '  LEFT/RIGHT/UP/DOWN - move sprite' + sLineBreak +
    '  ESC/ALT+F4 - quit' + sLineBreak, [SOutScreenFile, SOutSpriteFile]);

  // Initialize SDL
  if (SDL_Init(SDL_INIT_VIDEO) < 0) then
    MessageOutAndHalt('SDL initialization failed: %s', [SDL_GetError]);

  // Get video info and set flags
  VideoInfo := SDL_GetVideoInfo;
  if VideoInfo = nil then
    MessageOutAndHalt('SDL GetVideoInfo failed: %s', [SDL_GetError]);
  if VideoInfo.hw_available <> 0  then
    Flags := Flags or SDL_HWSURFACE
  else
    Flags := Flags or SDL_SWSURFACE;

  SDL_WM_SetCaption(PAnsiChar(AnsiString(Format(SWindowTitle, [Imaging.GetVersionStr]))), SWindowIconTitle);
  SDL_WM_SetIcon(LoadSDLSurfaceFromFile(GetDataDir + PathDelim + SIconFile), 0);

  // Initialize video mode
  DisplaySurface := SDL_SetVideoMode(DisplayWidth, DisplayHeight, 32, Flags);
  if DisplaySurface = nil then
    MessageOutAndHalt('SDL SetVideoMode failed: %s', [SDL_GetError]);

  // Initialize surfaces and enter main loop
  Initialize;
  LastTime := SDL_GetTicks;
  while Running do
  begin
    while SDL_PollEvent(@Event) = 1 do
    begin
      case Event.type_ of
        SDL_QUITEV:
          begin
            Running := False;
          end;
        SDL_KEYDOWN:
          begin
            with Event.key.keysym do
              if ((sym = SDLK_F4) and ((modifier and KMOD_ALT) <> 0)) or
                (Event.key.keysym.sym = SDLK_ESCAPE) then
                Running := False;

            if Event.key.keysym.sym in [SDLK_A, SDLK_C, SDLK_SPACE] then
            begin
              // You can toggle alpha blending with A key,
              // color keying with C key, SPACE key
              // can be used to cycle sprite image formats
              case Event.key.keysym.sym of
                SDLK_A: AlphaBlending := not AlphaBlending;
                SDLK_C: ColorKeying := not ColorKeying;
                SDLK_SPACE: SpriteFormat := NextFormat(SpriteFormat);
              end;
              ConvertSprite(SpriteFormat);
            end;

            // Using S and D keys you can take screen shots and sprite
            // shots easily
            case Event.key.keysym.sym of
              SDLK_S: ImagingSDL.SaveSDLSurfaceToFile(SOutScreenFile, DisplaySurface);
              SDLK_D: ImagingSDL.SaveSDLSurfaceToFile(SOutSpriteFile, SpriteSurface);
            end;
          end;
      end;
    end;

    // Sprite can be moved around screen using arrow keys
    Keys := PByteArray(SDL_GetKeyState(nil));
    if Keys[SDLK_LEFT] > 0 then
      SpriteX := Max(0, SpriteX - 1);
    if Keys[SDLK_RIGHT] > 0 then
      SpriteX := Min(DisplayWidth - SpriteSurface.w, SpriteX + 1);
    if Keys[SDLK_UP] > 0 then
      SpriteY := Max(0, SpriteY - 1);
    if Keys[SDLK_DOWN] > 0 then
      SpriteY := Min(DisplayHeight - SpriteSurface.h, SpriteY + 1);

    // Calculate FPS
    if LongInt(SDL_GetTicks) - LastTime > 1000 then
    begin
      SDL_WM_SetCaption(PAnsiChar(AnsiString(Format(SWindowTitle + ' FPS: %d',
        [Imaging.GetVersionStr, Frames]))), SWindowIconTitle);
      Frames := 0;
      LastTime := SDL_GetTicks;
    end;
    Inc(Frames);

    // Blits background and sprite to display surface
    Present;
  end;
  // Frees all surfaces and images
  Finalize;
  SDL_Quit;
end;

{
  File Notes:

  -- 0.77.1 ---------------------------------------------------
    - Refactored the demo (moved stuff to unit from dpr) and
      added Lazarus project files.

  -- 0.26.1 Changes/Bug Fixes ---------------------------------
    - Changed resolution to 800x600.
    - Delphi 2009 compatibility pchar/string changes.
}


end.
