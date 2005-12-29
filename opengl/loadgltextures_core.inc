{ Ponizej znajduje sie tresc procedury ktora laduje tekstury dla
  OpenGL'a. Powinny byc wczesniej zadeklarowane :

  - zmienne iloscTekstur:integer;
            textury:PGLuint;
            TextureProc:TProcedureRGBImage;
  - oraz trzy funkcje :
      getFilename(integer):string
      getMinFilter(integer):GLenum
      getMagFilter(integer):GLenum
    ktore dla argumentu 0 .. iloscTekstur-1 zwroca, odpowiednio,
     z jakiego pliku zaladowac i-ta teksture,
     jaki ustawic min filter i-tej teksturze i
     jaki ustawic mag filter i-tej teksturze.
}

type TGLuint_Array = array[0..High(word)]of TGLuint;

var ptextureImage: TRGBImage;
    i:integer;
    texturyArray:^TGLuint_Array absolute textury;
begin
 glGenTextures(iloscTekstur,textury);
 pTextureImage := nil;
 try
  for i:=0 to iloscTekstur-1 do
  begin
   if getFilename(i)<>'' then
   begin
    FreeAndNil(pTextureImage);
    ptextureImage := LoadRGBImage(getFilename(i), 0, 0, TextureProc);
   end else
   if i=0 then raise Exception.Create(
     'LoadGLTextures : pierwsza tekstura musi miec podana nazwe pliku !');
   LoadGLGeneratedTexture(texturyArray[i], pTextureImage, 
     GetMinFilter(i), GetMagFilter(i) LOAD_GL_GENERATED_TEXTURE_POST_PARAMETERS);
  end;

  if Enable2DTextures then glEnable(GL_TEXTURE_2D);
 finally pTextureImage.Free end;
end;
