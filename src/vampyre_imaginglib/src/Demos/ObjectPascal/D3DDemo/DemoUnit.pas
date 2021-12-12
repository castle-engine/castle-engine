{
  Vampyre Imaging Library Demo
  D3D9 Demo (D3D9 extension)

  Demo that shows how to create Direct3D 9 textures from files
  and Imaging's images and vice versa. This sample uses SDL to create
  window and process messages. Background and sprite textures are loaded from
  files and rendered. Sprite is rendered in each corner of the window
  using various texture stage and blending settings.
  You can change sprite's texture format by pressing SPACE key
  (it cycles trough all TImageFormat values). Background texture
  can be saved to file by pressing S key and sprite texture
  can be saved by pressing D key.

}
unit DemoUnit;

{$I ImagingOptions.inc}
{$R ..\Common\MainIcon.res}

interface

procedure RunDemo;

implementation

uses
  Windows,
  SysUtils,
  ImagingTypes,
  Imaging,
  ImagingUtility,
  sdl,
  Direct3D9,
  ImagingDirect3D9,
  ImagingSdl,
  DemoUtils;

type
  TVector2 = record
    X, Y: Single;
  end;

  TVector4 = record
    X, Y, Z, W: Single;
  end;

  TVertex = record
    Position: TVector4;
    TexCoord1: TVector2;
    TexCoord2: TVector2;
  end;

  TRect = array[0..3] of TVertex;

const
  SWindowTitle = 'Vampyre Imaging Library (%s) - Direct3D9 Demo (format: %s)';
  SWindowIconTitle = 'Direct3D9 Demo';
  SBackImageFile = 'Tigers.jpg';
  SSpriteImageFile = 'Vezyr.png';
  SOutScreenFile = 'D3DScreen.png';
  SOutSpriteFile = 'D3DSprite.dds';
  SIconFile = 'Icon.png';
  DisplayWidth = 800;
  DisplayHeight = 600;
  SpriteWidth = 256.0;
  SpriteHeight = 192.0;
  FVF_VERTEX = D3DFVF_XYZRHW or D3DFVF_TEX2;

var
  WindowHandle: THandle;
  Direct3D: IDirect3D9 = nil;
  Device: IDirect3DDevice9 = nil;
  BackTex: IDirect3DTexture9 = nil;
  SpriteTex: IDirect3DTexture9 = nil;
  PresentParams: TD3DPresentParameters;
  DisplaySurface: PSDL_Surface = nil;
  SpriteImage: TImageData;
  SpriteFormat: TImageFormat = ifA8R8G8B8;
  BackRect: TRect;
  Rects: array[0..3] of TRect;
  Event : TSDL_Event;
  Running: Boolean = True;
  Frames: LongInt = 0;
  FPS: LongInt = 0;
  LastTime: LongInt = 0;
  TextureCaps: TD3DTextureCaps;

function Vector2(X, Y: Single): TVector2;
begin
  Result.X := X;
  Result.Y := Y;
end;

function Vector4(X, Y, Z, W: Single): TVector4;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
  Result.W := W;
end;

procedure PlaceRect(Index: LongInt; X, Y: Single);
begin
  Rects[Index, 0].Position.X := X;
  Rects[Index, 0].Position.Y := Y;
  Rects[Index, 1].Position.X := X + SpriteWidth;
  Rects[Index, 1].Position.Y := Y;
  Rects[Index, 2].Position.X := X;
  Rects[Index, 2].Position.Y := Y + SpriteHeight;
  Rects[Index, 3].Position.X := X + SpriteWidth;
  Rects[Index, 3].Position.Y := Y + SpriteHeight;
end;

procedure MessageOut(Window: THandle; const Msg: string; const Args: array of const);
begin
  MessageBox(Window, PChar(Format(Msg, Args)), 'Message',
    MB_ICONINFORMATION or MB_OK);
end;

procedure MessageOutAndHalt(Window: THandle; const Msg: string; const Args: array of const);
begin
  MessageBox(Window, PChar(Format(Msg, Args)), 'Error',
    MB_ICONERROR or MB_OK);
  SDL_Quit;
  Halt(1);
end;

procedure UpdateCaption;
begin
  SDL_WM_SetCaption(PAnsiChar(AnsiString(Format(SWindowTitle + ' FPS: %d',
    [Imaging.GetVersionStr, GetFormatName(SpriteFormat), FPS]))),
    SWindowIconTitle);
end;

procedure CreateSpriteTexture(const Device: IDirect3DDevice9; Format: TImageFormat);
var
  D3DFormat: TD3DFormat;
  ConvTo: TImageFormat;
  ConvImage: TImageData;
begin
  // Find D3D format that matches given TImageFormat
  D3DFormat := ImagingDirect3D9.ImageFormatToD3DFormat(Format, ConvTo);
  if D3DFormat <> D3DFMT_UNKNOWN then
  begin
    // Free old texture and create new one in the different format
    SpriteTex := nil;
    Imaging.InitImage(ConvImage);
    Imaging.CloneImage(SpriteImage, ConvImage);
    // Create texture from image
    ImagingDirect3D9.CreateD3DTextureFromImage(ConvImage, Device, SpriteTex,
      SpriteImage.Width, SpriteImage.Height, 0, 0, D3DFormat, D3DPOOL_MANAGED);
    Imaging.FreeImage(ConvImage);
  end;
end;

procedure Initialize;
var
  Caption, Icon: PAnsiChar;
  Mode: TD3DDisplayMode;
  I: LongInt;
begin
  // Get SDL app window
  SDL_WM_GetCaption(Caption, Icon);
  WindowHandle := FindWindowA('SDL_app', Caption);
  if WindowHandle = 0 then
    MessageOutAndHalt(GetActiveWindow, 'Cannot get SDL window handle', []);

  // Place window to the center of the screen
  SetWindowPos(WindowHandle, 0, (GetSystemMetrics(SM_CXSCREEN) - DisplayWidth) div 2,
    (GetSystemMetrics(SM_CYSCREEN) - DisplayHeight - 20) div 2, 0, 0, SWP_NOSIZE or SWP_NOZORDER);

  // Create IDirect3D interface
  Direct3D := Direct3DCreate9(D3D_SDK_VERSION);
  if Direct3D = nil then
    MessageOutAndHalt(WindowHandle, 'Cannot create Direct3D interface', []);

  // Get the current display mode and fill presentation parameters
  Direct3D.GetAdapterDisplayMode(D3DADAPTER_DEFAULT, Mode);
  FillChar(PresentParams, SizeOf(PresentParams), 0);
  PresentParams.hDeviceWindow := WindowHandle;
  PresentParams.Windowed := True;
  PresentParams.BackBufferCount := 1;
  PresentParams.BackBufferFormat := Mode.Format;
  PresentParams.SwapEffect := D3DSWAPEFFECT_DISCARD;
  PresentParams.PresentationInterval := D3DPRESENT_INTERVAL_IMMEDIATE;
  // Create Direct3D device
  if Failed(Direct3D.CreateDevice(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, PresentParams.hDeviceWindow,
    D3DCREATE_SOFTWARE_VERTEXPROCESSING, @PresentParams, Device)) then
    MessageOutAndHalt(WindowHandle, 'Cannot create Direct3D device', []);

  // Get texture caps
  ImagingDirect3D9.GetDeviceTextureCaps(Device, TextureCaps);

  // Load background texture from file
  ImagingDirect3D9.LoadD3DTextureFromFile(GetDataDir + PathDelim + SBackImageFile, Device, BackTex);

  Imaging.InitImage(SpriteImage);
  // Load sprite image from file
  Imaging.LoadImageFromFile(GetDataDir + PathDelim + SSpriteImageFile, SpriteImage);
  // Create sprite texture from image
  CreateSpriteTexture(Device, SpriteFormat);

  // Set render states
  Device.SetRenderState(D3DRS_LIGHTING, 0);
  Device.SetRenderState(D3DRS_ALPHABLENDENABLE, 1);
  Device.SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_SRCALPHA);
  Device.SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA);
  // Set texture stage states
  Device.SetTextureStageState(0, D3DTSS_COLOROP,   D3DTOP_MODULATE);
  Device.SetTextureStageState(0, D3DTSS_COLORARG1, D3DTA_TEXTURE);
  Device.SetTextureStageState(0, D3DTSS_COLORARG2, D3DTA_DIFFUSE);
  Device.SetTextureStageState(0, D3DTSS_ALPHAOP,   D3DTOP_MODULATE);
  Device.SetTextureStageState(0, D3DTSS_ALPHAARG1, D3DTA_TEXTURE);
  Device.SetTextureStageState(0, D3DTSS_ALPHAARG2, D3DTA_DIFFUSE);
  Device.SetTextureStageState(1, D3DTSS_COLORARG1, D3DTA_TEXTURE);
  Device.SetTextureStageState(1, D3DTSS_COLORARG2, D3DTA_CURRENT);
  Device.SetTextureStageState(1, D3DTSS_ALPHAARG1, D3DTA_TEXTURE);
  Device.SetTextureStageState(1, D3DTSS_ALPHAARG2, D3DTA_CURRENT);

  Device.SetSamplerState(0, D3DSAMP_MINFILTER, D3DTEXF_LINEAR);
  Device.SetSamplerState(0, D3DSAMP_MAGFILTER, D3DTEXF_LINEAR);
  Device.SetSamplerState(0, D3DSAMP_MIPFILTER, D3DTEXF_LINEAR);

  // Set size of the background
  BackRect[0].Position := Vector4(0.0, 0.0, 0.0, 1.0);
  BackRect[1].Position := Vector4(DisplayWidth, 0.0, 0.0, 1.0);
  BackRect[2].Position := Vector4(0.0, DisplayHeight, 0.0, 1.0);
  BackRect[3].Position := Vector4(DisplayWidth, DisplayHeight, 0.0, 1.0);
  // Set background's tex coords
  BackRect[0].TexCoord1 := Vector2(0.0, 0.0);
  BackRect[1].TexCoord1 := Vector2(1.0, 0.0);
  BackRect[2].TexCoord1 := Vector2(0.0, 1.0);
  BackRect[3].TexCoord1 := Vector2(1.0, 1.0);

  // Set sprites' tex coords and defalt position
  for I := 0 to 3 do
  begin
    Rects[I, 0].Position := Vector4(0.0, 0.0, 0.0, 1.0);
    Rects[I, 1].Position := Vector4(0.0, 0.0, 0.0, 1.0);
    Rects[I, 2].Position := Vector4(0.0, 0.0, 0.0, 1.0);
    Rects[I, 3].Position := Vector4(0.0, 0.0, 0.0, 1.0);

    Rects[I, 0].TexCoord1 := Vector2(0.0, 0.0);
    Rects[I, 1].TexCoord1 := Vector2(1.0, 0.0);
    Rects[I, 2].TexCoord1 := Vector2(0.0, 1.0);
    Rects[I, 3].TexCoord1 := Vector2(1.0, 1.0);

    Rects[I, 0].TexCoord2 := Vector2(0.0, 1.0);
    Rects[I, 1].TexCoord2 := Vector2(1.0, 1.0);
    Rects[I, 2].TexCoord2 := Vector2(0.0, 0.0);
    Rects[I, 3].TexCoord2 := Vector2(1.0, 0.0);
  end;
  // Place sprites
  PlaceRect(0, 0, 0);
  PlaceRect(1, DisplayWidth - SpriteWidth, 0);
  PlaceRect(2, 0, DisplayHeight - SpriteHeight);
  PlaceRect(3, DisplayWidth - SpriteWidth, DisplayHeight - SpriteHeight);
end;

procedure Present;
begin
  Device.Clear(0, nil, D3DCLEAR_TARGET, $FFCCFFFF, 1.0, 0);
  if Succeeded(Device.BeginScene) then
  begin
    Device.SetFVF(FVF_VERTEX);
    // First render background
    Device.SetTextureStageState(0, D3DTSS_COLOROP,   D3DTOP_MODULATE);
    Device.SetTextureStageState(0, D3DTSS_ALPHAOP,   D3DTOP_MODULATE);
    Device.SetTextureStageState(1, D3DTSS_COLOROP,   D3DTOP_DISABLE);
    Device.SetTexture(0, BackTex);
    Device.DrawPrimitiveUP(D3DPT_TRIANGLESTRIP, 2, BackRect, SizeOf(TVertex));

    Device.SetTexture(0, SpriteTex);
    Device.SetTexture(1, SpriteTex);
    // Render first sprite
    Device.DrawPrimitiveUP(D3DPT_TRIANGLESTRIP, 2, Rects[0], SizeOf(TVertex));
    // Render second sprite
    Device.SetTextureStageState(0, D3DTSS_COLOROP,   D3DTOP_ADDSIGNED);
    Device.SetTextureStageState(1, D3DTSS_COLOROP,   D3DTOP_ADDSIGNED);
    Device.SetTextureStageState(1, D3DTSS_ALPHAOP,   D3DTOP_MODULATE);
    Device.DrawPrimitiveUP(D3DPT_TRIANGLESTRIP, 2, Rects[1], SizeOf(TVertex));
    // Render third sprite
    Device.SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_ONE);
    Device.SetRenderState(D3DRS_DESTBLEND, D3DBLEND_ONE);
    Device.SetTextureStageState(0, D3DTSS_COLOROP,   D3DTOP_MODULATE);
    Device.SetTextureStageState(0, D3DTSS_ALPHAOP,   D3DTOP_DISABLE);
    Device.SetTextureStageState(1, D3DTSS_COLOROP,   D3DTOP_SUBTRACT);
    Device.SetTextureStageState(1, D3DTSS_ALPHAOP,   D3DTOP_DISABLE);
    Device.DrawPrimitiveUP(D3DPT_TRIANGLESTRIP, 2, Rects[2], SizeOf(TVertex));
    Device.SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_SRCALPHA);
    Device.SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA);
    // Render last sprite
    Device.SetTextureStageState(0, D3DTSS_COLOROP,   D3DTOP_DOTPRODUCT3);
    Device.SetTextureStageState(1, D3DTSS_COLOROP,   D3DTOP_DOTPRODUCT3);
    Device.DrawPrimitiveUP(D3DPT_TRIANGLESTRIP, 2, Rects[3], SizeOf(TVertex));

    Device.SetTexture(0, nil);
    Device.SetTexture(1, nil);

    Device.EndScene;
  end;
  // Copy backbuffer to window
  Device.Present(nil, nil, 0, nil);
end;

procedure Finalize;
begin
  BackTex := nil;
  SpriteTex := nil;
  Device := nil;
  Direct3D := nil;
  Imaging.FreeImage(SpriteImage);
end;

procedure TakeScreenShot;
var
  RenderTarget, OldRenderTarget: IDirect3DSurface9;
  ScreenImg: TImageData;
begin
  // Create new render target and activate it
  Device.CreateRenderTarget(DisplayWidth, DisplayHeight, D3DFMT_A8R8G8B8,
    D3DMULTISAMPLE_NONE, 0, True, RenderTarget, nil);
  Device.GetRenderTarget(0, OldRenderTarget);
  Device.SetRenderTarget(0, RenderTarget);
  // Render to new target
  Present;
  // Activate old target
  Device.SetRenderTarget(0, OldRenderTarget);
  // Convert reder target surface to Imaging image and save it to file
  ImagingDirect3D9.CreateImageFromD3DSurface(RenderTarget, ScreenImg);
  Imaging.SaveImageToFile(SOutScreenFile, ScreenImg);
  // Free all
  Imaging.FreeImage(ScreenImg);
  RenderTarget := nil;
  OldRenderTarget := nil;
end;

procedure RunDemo;
begin
  // Initialize SDL
  if (SDL_Init(SDL_INIT_VIDEO) < 0) then
    MessageOutAndHalt(GetActiveWindow, 'SDL initialization failed: %s', [SDL_GetError]);

  SDL_WM_SetCaption(PAnsiChar(AnsiString(Format(SWindowTitle, [Imaging.GetVersionStr,
    GetFormatName(SpriteFormat)]))), SWindowIconTitle);
  SDL_WM_SetIcon(LoadSDLSurfaceFromFile(GetDataDir + PathDelim + SIconFile), 0);

  // Initialize video mode
  DisplaySurface := SDL_SetVideoMode(DisplayWidth, DisplayHeight, 32, 0);
  if DisplaySurface = nil then
    MessageOutAndHalt(GetActiveWindow, 'SDL SetVideoMode failed: %s', [SDL_GetError]);

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

            // Using S and D keys you can take screen shots and texture
            // shots easily
            // SPACE key can be used to cycle sprite image formats
            case Event.key.keysym.sym of
              SDLK_S: TakeScreenShot;
              SDLK_D: ImagingDirect3D9.SaveD3DTextureToFile(SOutSpriteFile, SpriteTex);
              SDLK_SPACE:
                begin
                  SpriteFormat := NextFormat(SpriteFormat);
                  CreateSpriteTexture(Device, SpriteFormat);
                  UpdateCaption;
                end;
            end;
          end;
      end;
    end;

    // Calculate FPS
    if LongInt(SDL_GetTicks) - LastTime > 1000 then
    begin
      FPS := Frames;
      UpdateCaption;
      Frames := 0;
      LastTime := SDL_GetTicks;
    end;
    Inc(Frames);

    // Renders background and sprites to the window
    Present;
  end;
  // Frees all textures, images, and D3D objects
  Finalize;
  SDL_Quit;
end;

{
  File Notes:

  -- 0.77.1 ---------------------------------------------------
    - Refactored the demo (moved stuff to unit from dpr) and
      added Lazarus project files.

  -- 0.26.1 Changes/Bug Fixes ---------------------------------
    - Delphi 2009 compatibility pchar/string changes.

  -- 0.17 Changes/Bug Fixes -----------------------------------
    - S key now saves screenshot to file
}


end.
