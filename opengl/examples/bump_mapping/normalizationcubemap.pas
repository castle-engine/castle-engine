unit NormalizationCubeMap;

interface

uses OpenGLh;

{ Generate new OpenGL texture number and set it's 6
  GL_TEXTURE_CUBE_MAP_*_ARB targets to cube map used to normalize vectors.
  After this call, returned texture is currently bound. }
function MakeNormalizationCubeMap: TGLuint;

implementation

uses SysUtils, KambiUtils, Images, VectorMath, KambiGLUtils;

function MakeNormalizationCubeMap: TGLuint;

  procedure FillNormalized(out NormalAsColor: TVector3Byte; V: TVector3Single);
  begin
    NormalizeTo1st(V);
    { clamping just for safety, these are floating point vals after all }
    NormalAsColor[0] := Clamped( Trunc((V[0] + 1) / 2 * High(Byte)), Low(Byte), High(Byte));
    NormalAsColor[1] := Clamped( Trunc((V[1] + 1) / 2 * High(Byte)), Low(Byte), High(Byte));
    NormalAsColor[2] := Clamped( Trunc((V[2] + 1) / 2 * High(Byte)), Low(Byte), High(Byte));
  end;

var
  Image: TRGBImage;
  S, T, Size: Cardinal;
begin
  glGenTextures(1, @Result);
  glBindTexture(GL_TEXTURE_CUBE_MAP_ARB, Result);

  glTexParameteri(GL_TEXTURE_CUBE_MAP_ARB, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_CUBE_MAP_ARB, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

  { We will access this using 3D texture coordinates, so wrapping for all
    3 axes is important }
  glTexParameteri(GL_TEXTURE_CUBE_MAP_ARB, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_CUBE_MAP_ARB, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_CUBE_MAP_ARB, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);

  { Make sure we don't use too large size.
    ARB_texture_cube_map requires that it's >= 16.
    On kocury GeForce FX 5200 it's 4096. }
  Size := Min(32, glGetInteger(GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB));

  Image := TRGBImage.Create(Size, Size);
  try
    for S := 0 to Image.Width - 1 do
      for T := 0 to Image.Width - 1 do
        FillNormalized( Image.PixelPtr(S, T)^,
          Vector3Single(
            1,
            - ((T / Image.Width) * 2 - 1),
            - ((S / Image.Width) * 2 - 1)));

    glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB, 0, 3,
      Image.Width, Image.Height, 0, GL_RGB, GL_UNSIGNED_BYTE, Image.RawPixels);

    for S := 0 to Image.Width - 1 do
      for T := 0 to Image.Width - 1 do
        FillNormalized( Image.PixelPtr(S, T)^,
          Vector3Single(
            - 1,
            - ((T / Image.Width) * 2 - 1),
               (S / Image.Width) * 2 - 1));

    glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB, 0, 3,
      Image.Width, Image.Height, 0, GL_RGB, GL_UNSIGNED_BYTE, Image.RawPixels);

    for S := 0 to Image.Width - 1 do
      for T := 0 to Image.Width - 1 do
        FillNormalized( Image.PixelPtr(S, T)^,
          Vector3Single(
            (S / Image.Width) * 2 - 1,
            1,
            (T / Image.Width) * 2 - 1));

    glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB, 0, 3,
      Image.Width, Image.Height, 0, GL_RGB, GL_UNSIGNED_BYTE, Image.RawPixels);

    for S := 0 to Image.Width - 1 do
      for T := 0 to Image.Width - 1 do
        FillNormalized( Image.PixelPtr(S, T)^,
          Vector3Single(
              (S / Image.Width) * 2 - 1,
            - 1,
            - (T / Image.Width) * 2 - 1));

    glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB, 0, 3,
      Image.Width, Image.Height, 0, GL_RGB, GL_UNSIGNED_BYTE, Image.RawPixels);

    for S := 0 to Image.Width - 1 do
      for T := 0 to Image.Width - 1 do
        FillNormalized( Image.PixelPtr(S, T)^,
          Vector3Single(
               (S / Image.Width) * 2 - 1,
            - ((T / Image.Width) * 2 - 1),
            1));

    glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB, 0, 3,
      Image.Width, Image.Height, 0, GL_RGB, GL_UNSIGNED_BYTE, Image.RawPixels);

    for S := 0 to Image.Width - 1 do
      for T := 0 to Image.Width - 1 do
        FillNormalized( Image.PixelPtr(S, T)^,
          Vector3Single(
            - ((S / Image.Width) * 2 - 1),
            - ((T / Image.Width) * 2 - 1),
            - 1));

    glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB, 0, 3,
      Image.Width, Image.Height, 0, GL_RGB, GL_UNSIGNED_BYTE, Image.RawPixels);

    SaveImage(Image, '/tmp/a.png');
  finally FreeAndNil(Image) end;
end;

end.
