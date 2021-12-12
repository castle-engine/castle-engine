{
  Vampyre Imaging Library Demo
  OpenGL Demo (OpenGL extension)

  Demo that shows how to create OpenGL textures from files
  and Imaging's images and vice versa. This sample uses SDL to create
  window and process messages. Background and sprite textures are loaded from
  files and rendered. Sprite is mapped on the spinning cube in the
  center of the window. You can change sprite's texture format
  by pressing SPACE key (it cycles trough all TImageFormat values).
  Background texture can be saved to file by pressing S key and sprite texture
  can be saved by pressing D key.
}

unit DemoUnit;

{ Define this symbol if you want to use dglOpenGL header.}
{$DEFINE USE_DGL_HEADERS}
{ $DEFINE USE_GLSCENE_HEADERS}

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
  ImagingTypes,
  Imaging,
  ImagingUtility,
  sdl,
{$IF Defined(USE_DGL_HEADERS)}
  dglOpenGL,
{$ELSEIF Defined(USE_GLSCENE_HEADERS)}
  OpenGL1x,
{$ELSE}
  gl, glext,
{$IFEND}
  ImagingOpenGL,
  ImagingSdl,
  DemoUtils;

const
  SWindowTitle = 'Vampyre Imaging Library (%s) - OpenGL Demo (format: %s)';
  SWindowIconTitle = 'OpenGL Demo';
  SBackImageFile = 'Tigers.jng';
  SSpriteImageFile = 'Vezyr.png';
  SOutScreenFile = 'GLScreen.png';
  SOutSpriteFile = 'GLSprite.dds';
  SIconFile = 'Icon.png';
  DisplayWidth = 800;
  DisplayHeight = 600;
  CubeSize = 200.0;

var
  BackTex: GLuint = 0;
  SpriteTex: GLuint = 0;
  DisplaySurface: PSDL_Surface = nil;
  SpriteImage: TImageData;
  SpriteFormat: TImageFormat = ifA8R8G8B8;
  Event : TSDL_Event;
  Running: Boolean = True;
  Frames: LongInt = 0;
  FPS, Elapsed: Single;
  CurrTime, FrameTime, LastTime: Cardinal;
  Angle: Single = 0.0;
  TextureCaps: TGLTextureCaps;
{$IFDEF MSWINDOWS}
  WindowHandle: THandle;
{$ENDIF}

procedure MessageOut(const Msg: string; const Args: array of const);
begin
{$IFDEF MSWINDOWS}
  MessageBox(GetActiveWindow, PChar(Format(Msg, Args)), 'Message',
    MB_ICONINFORMATION or MB_OK);
{$ENDIF}
{$IFDEF UNIX}
  WriteLn(Format(Msg, Args));
{$ENDIF}
end;

procedure MessageOutAndHalt(const Msg: string; const Args: array of const);
begin
{$IFDEF MSWINDOWS}
  MessageBox(GetActiveWindow, PChar(Format(Msg, Args)), 'Error',
    MB_ICONERROR or MB_OK);
{$ENDIF}
{$IFDEF UNIX}
  WriteLn('Error: ');
  MessageOut('  ' + Msg, Args);
  WriteLn('Press RETURN to exit');
  ReadLn;
{$ENDIF}
  SDL_Quit;
  Halt(1);
end;

procedure UpdateCaption;
begin
  SDL_WM_SetCaption(PAnsiChar(AnsiString(Format(SWindowTitle + ' FPS: %.1f',
    [Imaging.GetVersionStr, GetFormatName(SpriteFormat), FPS]))), SWindowIconTitle);
end;

procedure CreateSpriteTexture(Format: TImageFormat);
var
  Info: TImageFormatInfo;
begin
  // Delete old texture and create new one in the different format
  glDeleteTextures(1, @SpriteTex);
  SpriteTex := ImagingOpenGL.CreateGLTextureFromImage(SpriteImage,
    256, 256, True, SpriteFormat);
  if SpriteTex = 0 then
    MessageOut('Sprite texture creation failed.', []);
  // Set tex parameters
  glBindTexture(GL_TEXTURE_2D, SpriteTex);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  if TextureCaps.MaxAnisotropy > 0 then
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, TextureCaps.MaxAnisotropy);

  if Imaging.GetImageFormatInfo(SpriteFormat, Info) then
  begin
    if Info.IsFloatingPoint and (Info.BytesPerPixel in [4, 16]) then
    begin
      // Floating point textures (not half float though) should use nearest
      // filter on current hardware. I get 900 fps with nearest filter
      // and only 2 fps with linear filter
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST_MIPMAP_NEAREST);
    end
    else
    begin
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    end;
  end;
end;

procedure Initialize;
{$IFDEF MSWINDOWS}
var
  Caption, Icon: PAnsiChar;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  SDL_WM_GetCaption(Caption, Icon);
  WindowHandle := FindWindowA('SDL_app', Caption);
  if WindowHandle <> 0 then
  begin
    // Place window to the center of the screen
    SetWindowPos(WindowHandle, 0, (GetSystemMetrics(SM_CXSCREEN) - DisplayWidth) div 2,
      (GetSystemMetrics(SM_CYSCREEN) - DisplayHeight - 20) div 2, 0, 0, SWP_NOSIZE or SWP_NOZORDER);
  end;
{$ENDIF}

{$IFDEF USE_DGL_HEADERS}
  dglOpenGL.InitOpenGL;
  dglOpenGL.ReadExtensions;
  dglOpenGL.ReadImplementationProperties;
{$ENDIF}
  ImagingOpenGL.GetGLTextureCaps(TextureCaps);
  // Disable some GL states
  glDisable(GL_LIGHTING);
  // Enable some GL states
  glEnable(GL_BLEND);
  glEnable(GL_TEXTURE_2D);
  // Prepare for alpha blending
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  // Set projections and model view transformations
  glViewport(0, 0, DisplayWidth, DisplayHeight);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(0, DisplayWidth, DisplayHeight, 0, -1000.0, 1000.0);

  // Load background texture from file
  BackTex := ImagingOpenGL.LoadGLTextureFromFile(GetDataDir + PathDelim + SBackImageFile);
  // Set tex parameters
  glBindTexture(GL_TEXTURE_2D, BackTex);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  Imaging.InitImage(SpriteImage);
  // Load sprite image from file
  Imaging.LoadImageFromFile(GetDataDir + PathDelim + SSpriteImageFile, SpriteImage);
  // Create sprite texture from image
  CreateSpriteTexture(SpriteFormat);
end;

procedure Present;
begin
  // Clear depth and color buffers
  glClearColor(0.0, 0.8, 1.0, 1.0);
  glClearDepth(1.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  // First draw background
  glBindTexture(GL_TEXTURE_2D, BackTex);
  glDisable(GL_DEPTH_TEST);
  glBegin(GL_QUADS);
    glTexCoord2f(0.0, 0.0); glVertex2f(0.0, 0.0);
    glTexCoord2f(1.0, 0.0); glVertex2f(DisplayWidth, 0.0);
    glTexCoord2f(1.0, 1.0); glVertex2f(DisplayWidth, DisplayHeight);
    glTexCoord2f(0.0, 1.0); glVertex2f(0.0, DisplayHeight);
  glEnd;
  // Then draw the spinning cube
  glEnable(GL_DEPTH_TEST);
  glTranslatef(DisplayWidth / 2.0, DisplayHeight / 2.0, 0.0);
  glRotatef(-30.0, 1.0, 0.0, 0.0);
  glRotatef(Angle, 0.0, 1.0, 0.0);
  glTranslatef(-CubeSize / 2.0, -CubeSize / 2.0, -CubeSize / 2.0);
  glBindTexture(GL_TEXTURE_2D, SpriteTex);
  glBegin(GL_QUADS);
    glTexCoord2f(1.0, 0.0); glVertex3f(0.0, 0.0, 0.0);
    glTexCoord2f(0.0, 0.0); glVertex3f(CubeSize, 0.0, 0.0);
    glTexCoord2f(0.0, 1.0); glVertex3f(CubeSize, CubeSize, 0.0);
    glTexCoord2f(1.0, 1.0); glVertex3f(0.0, CubeSize, 0.0);

    glTexCoord2f(0.0, 0.0); glVertex3f(0.0, 0.0, CubeSize);
    glTexCoord2f(1.0, 0.0); glVertex3f(CubeSize, 0.0, CubeSize);
    glTexCoord2f(1.0, 1.0); glVertex3f(CubeSize, CubeSize, CubeSize);
    glTexCoord2f(0.0, 1.0); glVertex3f(0.0, CubeSize, CubeSize);

    glTexCoord2f(0.0, 0.0); glVertex3f(0.0, 0.0, 0.0);
    glTexCoord2f(0.0, 1.0); glVertex3f(0.0, CubeSize, 0.0);
    glTexCoord2f(1.0, 1.0); glVertex3f(0.0, CubeSize, CubeSize);
    glTexCoord2f(1.0, 0.0); glVertex3f(0.0, 0.0, CubeSize);

    glTexCoord2f(1.0, 0.0); glVertex3f(CubeSize, 0.0, 0.0);
    glTexCoord2f(1.0, 1.0); glVertex3f(CubeSize, CubeSize, 0.0);
    glTexCoord2f(0.0, 1.0); glVertex3f(CubeSize, CubeSize, CubeSize);
    glTexCoord2f(0.0, 0.0); glVertex3f(CubeSize, 0.0, CubeSize);
  glEnd;

  Angle := Angle + 50 * Elapsed;
  SDL_GL_SwapBuffers;
end;

procedure Finalize;
begin
  // Free textures and images
  glDeleteTextures(1, @BackTex);
  glDeleteTextures(1, @SpriteTex);
  Imaging.FreeImage(SpriteImage);
end;

procedure TakeScreenShot;
var
  RenderTarget: Gluint;
begin
  // Setup render target texture
  glGenTextures(1, @RenderTarget);
  glBindTexture(GL_TEXTURE_2D, RenderTarget);
  glTexImage2D(GL_TEXTURE_2D, 0, 3, DisplayWidth, DisplayHeight, 0, GL_RGB, GL_UNSIGNED_BYTE, nil);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  // Render all
  Present;
  // Copy framebuffer to texture
  glBindTexture(GL_TEXTURE_2D, RenderTarget);
  glCopyTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, 0, 0, DisplayWidth, DisplayHeight, 0);
  // Save texture & delete it
  ImagingOpenGL.SaveGLTextureToFile(SOutScreenFile, RenderTarget);
  glDeleteTextures(1, @RenderTarget);
end;

procedure RunDemo;
begin
  // Initialize SDL
  if (SDL_Init(SDL_INIT_VIDEO) < 0) then
    MessageOutAndHalt('SDL initialization failed: %s', [SDL_GetError]);

  SDL_WM_SetCaption(PAnsiChar(AnsiString(Format(SWindowTitle, [Imaging.GetVersionStr,
    GetFormatName(SpriteFormat)]))), SWindowIconTitle);
  SDL_WM_SetIcon(LoadSDLSurfaceFromFile(GetDataDir + PathDelim + SIconFile), 0);

  // Set GL attributes using SDL
  SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 24);
  SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, 0);
  SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);

  // Initialize video mode
  DisplaySurface := SDL_SetVideoMode(DisplayWidth, DisplayHeight, 32, SDL_OPENGL);
  if DisplaySurface = nil then
    MessageOutAndHalt('SDL SetVideoMode failed: %s', [SDL_GetError]);

  // Initialize surfaces and enter main loop
  Initialize;
  LastTime := SDL_GetTicks;
  FrameTime := LastTime;

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

            // Using S and D keys you can take screen shots and texture
            // shots easily
            // SPACE key can be used to cycle sprite image formats
            case Event.key.keysym.sym of
              SDLK_S: TakeScreenShot;
              SDLK_D: ImagingOpenGL.SaveGLTextureToFile(SOutSpriteFile, SpriteTex);
              SDLK_SPACE:
                begin
                  SpriteFormat := NextFormat(SpriteFormat);
                  CreateSpriteTexture(SpriteFormat);
                  UpdateCaption;
                end;
            end;
          end;
      end;
    end;

    CurrTime := SDL_GetTicks;
    Elapsed := (CurrTime - LastTime) / 1000;
    LastTime := CurrTime;
    Inc(Frames);
    // Calculate FPS
    if CurrTime - FrameTime > 1000 then
    begin
      FPS := Frames / (CurrTime - FrameTime) * 1000;
      UpdateCaption;
      Frames := 0;
      FrameTime := CurrTime;
    end;

    // Renders background and sprites to the window
    Present;
  end;
  // Frees everything
  Finalize;
  SDL_Quit;
end;

{
  File Notes:

  -- 0.77.1 ---------------------------------------------------
    - Refactored the demo (moved stuff to unit from dpr) and
      added Lazarus project files.

  -- 0.26.1 Changes/Bug Fixes ---------------------------------
    - Added support for GLScene's OpenGL header.
    - Delphi 2009 compatibility pchar/string changes.

  -- 0.25.0 Changes/Bug Fixes ---------------------------------
    - Changes in timing.
    - Can use dglOpenGL headers now.

  -- 0.19 Changes/Bug Fixes -----------------------------------
    - screenshots now work in all OSs and ImagingComponents is no longer needed

  -- 0.17 Changes/Bug Fixes -----------------------------------
    - S key now saves screenshot to file
    - anisotropic filtering enabled if supported by hardware
}

end.
