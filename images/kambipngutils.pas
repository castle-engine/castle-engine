{
  Copyright 2002-2006 Michalis Kamburelis.

  This file is part of "Kambi's images Pascal units".

  "Kambi's images Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's images Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's images Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ @abstract(Some utilities to deal with PNGs and libpng.)

  I don't put these things inside KambiPng unit, because KambiPng unit
  should contain only the things present in libpng,
  not some additional related utilities.
}

unit KambiPngUtils;

{$I pngconf.inc}

interface

uses SysUtils, KambiPng;

type
  ELibPngNotAvailable = class(Exception);

{ Functions SO_Xxx return the version of used libpng library
  (taken by querying png_access_version_number).
  They raise ELibPngNotAvailable exception if libpng library
  was not available (not KambiPngInited) or if version of libpng
  library is incompatible with interface defined in KamiPng unit.

  So these functions actually not only return version of libpng,
  they also check is libpng available (and has the proper version).

  The three Integer functions should always match the first
  3 components of SO_PNG_LIBPNG_VER_STRING.

  @groupBegin }
function SO_PNG_LIBPNG_VER_STRING: PChar;
function SO_PNG_LIBPNG_VER_MAJOR: Integer;
function SO_PNG_LIBPNG_VER_MINOR: Integer;
function SO_PNG_LIBPNG_VER_RELEASE: Integer;
{ @groupEnd }

{ Use all functions below ONLY when KambiPngInited = true
  (often comfortable way to check KambiPngInited is to call
  one of SO_xxx functions above). }

{ Apply transformations in such a way that EVERY png format wil be converted to
  RGB 8bit depth, no fill, no alpha.
  So, paletted and grayscales must be converted, bytes got to have appriopriate
  order, alpha channel must be applied and then thrown out.}
procedure png_transform_to_rgb3byte(png_ptr: png_structp; info_ptr: png_infop);

{ Apply transformation to convert EVERY png to RGBA 8 byte depth.
  So, paletted and grayscales must be converted, bytes got to have appriopriate
  order and alpha channel must be added (=1.0) if it is not already present
  in file. }
procedure png_transform_to_rgba4byte(png_ptr: png_structp; info_ptr: png_infop);

{ png file has alpha info if it is has alpha channel (grayscale or rgb)
  or if it has tRNS chunk (for paletted image this stores alpha values
  for each palette color, for grayscale/rgb it determines one particular
  color to mean "transparent"). Function below checks it. }
function png_has_alpha_info(png_ptr: png_structp; info_ptr: png_infop): boolean;

function PngColorTypeToStr(PngColorType: longint): string;
function PngInterlaceTypeToStr(PngInterlaceType: longint): string;
function PngTextCompressionToStr(PngTextCompression: longint): string;

implementation

uses KambiUtils;

var
  { variables below are initialized in initialization }
  fSO_VER_STRING: AnsiString;
  fSO_VER_MAJOR, fSO_VER_MINOR, fSO_VER_RELEASE: integer;

procedure Check_SO_VER;
{ Wywolujemy ta funkcje tylko gdy zwracamy SO_PNG_LIBPNG_VER_LIBPNG_xxx,
  bo jezeli nie uzyles tej funkcji to znaczy ze uzywasz PNG_LIBPNG_VER_xxx
  a wiec polegasz na libpng ktore samo przetestuje czy wersje biblioteki
  i tego unitu sa kompatybilne.

  Oczywiscie - zrobilem wlasne funkcje SO_PNG_LIBPNG_VER_xx bo
  libpng wedlug mnie testowalo za ostro wersje biblioteki. Wiec tutaj
  chcemy robic sprawdzanie wersji mniej restrykcyjne.}
begin
 if not KambiPngInited then
  raise ELibPngNotAvailable.Create('LibPng is not available');

 if fSO_VER_MAJOR <> 1 then
  raise ELibPngNotAvailable.CreateFmt('LibPng major version is %d, ' +
    'but 1 is required -- cannot use available LibPng library', [fSO_VER_MAJOR]);
end;

function SO_PNG_LIBPNG_VER_STRING: PChar;
begin Check_SO_VER; result := PChar(fSO_VER_STRING) end;

function SO_PNG_LIBPNG_VER_MAJOR: integer;
begin Check_SO_VER; result := fSO_VER_MAJOR end;

function SO_PNG_LIBPNG_VER_MINOR: integer;
begin Check_SO_VER; result := fSO_VER_MINOR end;

function SO_PNG_LIBPNG_VER_RELEASE: integer;
begin Check_SO_VER; result := fSO_VER_RELEASE end;

{ ------------------------------------------------------------------------------------ }

{ Kilka notek do pisania transformacji libpng :

  pamietaj ze transformacje uaktualniaja stan strukturki png_ptr
  (typu png_structp), natomiast nie uaktualniaja info_ptr
  (typu png_infop). Jak po zaaplikowaniu transformacji w
  uniwersalny sposob dowiedziec sie jaki mamy teraz ColorType
  i BitDepth ? Wydaje sie ze sensowny sposob to png_read_update_info
  a potem png_get_IHDR (samo png_get_IHDR nie wystarczy, musi byc
  png_read_update_info aby uaktualnic strukturke info_ptr;
  w ogole nie wiem po co png_get_IHDR pobiera jako parametr takze
  png_ptr - na png_get_IHDR trzeba patrzec jako na rozkodowanie
  wnetrza strukturki info_ptr) ale nie - read_update_info powoduje
  niezrozumialy warning a potem sprawia ze poprawny odczyt jest
  niemozliwy.

  po zaaplikowaniu kazdej transformacji musimy odswiezyc wartosc ColorType
  i bitDepth. Niestety nie mozemy do tego uzywac png_read_update_info +
  png_get_IHDR. Naprawde nie wiem czemu, ale

  uzywanie png_read_update_info wiecej niz raz powoduje warningi
  libpng "Ignoring extra png_read_update_info() call; row buffer not reallocated"
  ktorych sens pozostaje dla mnie nieodgadniony (dlaczego je ignoruje ?
  przeciez uaktualnia info_ptr a chyba taki jest sens tej funkcji ?).
  Potem przy odczytywaniu obrazka jest blad "Decompression error"
  wiec wniosek - nie uzywac png_read_update_info wiecej niz raz.
  Czyli wolno tego uzywac tylko w taki sposob w jaki napisali w manualu -
  jednorazowo, juz po wykonaniu wszystkich transformacji. }

procedure png_transform_to_rgb3byte(png_ptr: png_structp; info_ptr: png_infop);
var bKGD_col_ptr: png_color_16p;
    {my_background_col: png_color_16;}
    ColorType, BitDepth: LongWord;
begin
 BitDepth := png_get_bit_depth(png_ptr, info_ptr);
 ColorType := png_get_color_type(png_ptr, info_ptr);

 {now we're applying transformations. This is the moment we can easily
  make a mistake and forget about something. Our idea is to apply transformations
  in such a way that EVERY png format wil be converted to RGB 8bit depth,
  no fill, no alpha.
  So, paletted and grayscales must be converted, bytes got to have appriopriate
  order, alpha channel must be applied and then thrown out. }

 {palette -> rgb, maybe with alpha}
 if (ColorType and PNG_COLOR_MASK_PALETTE) <> 0 then
 begin
  png_set_palette_to_rgb(png_ptr);
  { we converted palette to rgb; actually it may be RGB or RGBA;
    paletted images can contain alpha channel only using tRNS chunk
    so here we can check whether we got RGB or RGBA by checking
    whether there exists tRNS chunk;
    Ufff; this was a bug corrected after a long day : 21.12.2002}
  if png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS) <> 0 then
   ColorType := PNG_COLOR_TYPE_RGB_ALPHA else
   ColorType := PNG_COLOR_TYPE_RGB;
  BitDepth := 8; { when expanding palette we always get 8 bit depth because pallete entries
                 are always in 8bit RGB }
 end;
 {grayscale -> 8bit rgb}
 if (ColorType and PNG_COLOR_MASK_COLOR) = 0 then
 begin
  if BitDepth < 8 then
  begin
   png_set_gray_1_2_4_to_8(png_ptr);
   BitDepth := 8;
  end;
  png_set_gray_to_rgb(png_ptr);
  {gray color means ColorType = PNG_COLOR_TYPE_GRAY or GRAY_ALPHA
                              = 0 or MASK_ALPHA
   and that's why here we can simply combine it bitwise with MASK_COOR }
  ColorType := ColorType or PNG_COLOR_MASK_COLOR;
 end;
 {now he have rgb 8/16 bitDepth + maybe alpha}
 {rgb 16 bitdepth -> rgb 8 bitdepth}
 if BitDepth = 16 then
 begin
  png_set_strip_16(png_ptr);
  BitDepth := 8;
 end;
 {handle alpha and tRNS by combining image with color in bKGD header part
  (we do it just to get rid of alpha channel; in case of PngGraphic bedziemy
  aplikowac pozniej filler jako czwarty bajt i bedziemy ten czwarty bajt
  ignorowac ale generalnie tak czy siak nalezy skombinowac alpha channel
  z obrazkiem zeby obrazek wygladal tak jakby sie tego mogl spodziewac
  autor obrazka.)}
 if (ColorType and PNG_COLOR_MASK_ALPHA) <> 0 then
 begin
  { combinig with background requires a few parameters.
    We set need_expand (4th parameter) to 1 when we take color from file because
    in file it's written in file's original format and so it must be expanded
    to the currently set format (8 bit rgb). However, we are supplying
    my_background_col in 8bit rgb format already so there we set need_expand to 0.

    We set background_gamma (5th parameter) always as 1.0 - bacause that's
    considered the "default" and we want do just the default thing
    (we want to do what author of the image expected). }
  if png_get_bKGD(png_ptr, info_ptr, @bKGD_col_ptr) <> 0 then
  begin  { combine it with supplied bKGD color }
   png_set_background(png_ptr, bKGD_col_ptr, PNG_BACKGROUND_GAMMA_FILE, 1, 1.0);
  end else
  begin
   { What should we do now ?
     We can apply image on an arbitrary background color
     (but which one ?) or we can just strip alpha channel.

     Some interesting case from testing:

     - png/bufferfs.png: requires stripping, only then looks good.
       Possibly it's just a bad image ? Opening in GIMP, it shows
       only as a text (butterflies not visible), everything else
       is completely transparent. That's why
       - doing png_set_background with my_background_col = white
         makes this image just a text on clear white background and
       - doing png_set_background with my_background_col = black
         makes this image just a text on clear black background
       If you want to see butterflies there, you cannot use
       png_set_background, you must just strip alpha channel.

     - png/moose/customize-m.png. Well this is interesting image...
       It seems that the alpha channel contains negative of how
       the image should look. Somehow, when you combine it with
       white background, it looks perfect. If you combine it with
       black background, it looks black. It you strip alpha,
       looks unsensible (partially clear white, partially black
       silhouette...). I don't know why, but it seems that
       combinging with white is the only sensible option.

     What should I do ? I don't know. }

   { Version with combining with background }
   (*
    with my_background_col do
    begin
      { my_background_col := white color }
      red := $FF; green := $FF;  blue := $FF;
    end;
    png_set_background(png_ptr, @my_background_col, PNG_BACKGROUND_GAMMA_SCREEN, 0, 1.0);
    *)

   { Version with stripping }
   png_set_strip_alpha(png_ptr);
  end;
  ColorType := ColorType and LongWord(not PNG_COLOR_MASK_ALPHA);
 end;

 { Po zakonczeniu tej procedury musimy miec ColorType = PNG_COLOR_TYPE_RGB i
   BitDepth = 8. }
 Assert((ColorType = PNG_COLOR_TYPE_RGB) and (BitDepth = 8),
   'png_transform_to_rgb3byte failed to apply good png transformations');
end;

procedure png_transform_to_rgba4byte(png_ptr: png_structp; info_ptr: png_infop);
var
  {my_background_col: png_color_16;}
  ColorType, BitDepth: LongWord;
  TRNSHandled: boolean;
begin
 BitDepth := png_get_bit_depth(png_ptr, info_ptr);
 ColorType := png_get_color_type(png_ptr, info_ptr);

 { At the beginning: tRNS chunk is already handled if it doesn't
   exist, right ? }
 TRNSHandled := png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS) = 0;

 { palette -> 8bit rgb }
 if (ColorType and PNG_COLOR_MASK_PALETTE) <> 0 then
 begin
   png_set_palette_to_rgb(png_ptr);

   { paletted images can contain alpha channel only using tRNS chunk
     so here we can check whether we got RGB or RGBA by checking
     whether there exists tRNS chunk;
     Ufff; this was a bug corrected after a long day : 21.12.2002 }
   if not TRNSHandled then
   begin
     ColorType := PNG_COLOR_TYPE_RGB_ALPHA;
     TRNSHandled := true;
   end else
     ColorType := PNG_COLOR_TYPE_RGB;

   { When expanding palette we always get 8 bit depth because
     pallete entries are always in 8bit RGB }
   BitDepth := 8;
 end;

 { grayscale -> 8bit rgb }
 if (ColorType and PNG_COLOR_MASK_COLOR) = 0 then
 begin
   if BitDepth < 8 then
   begin
     png_set_gray_1_2_4_to_8(png_ptr);
     BitDepth := 8;
   end;
   png_set_gray_to_rgb(png_ptr);
   {gray color means ColorType = PNG_COLOR_TYPE_GRAY or GRAY_ALPHA
                               = 0 or MASK_ALPHA
    and that's why here we can simply combine it bitwise with MASK_COOR }
   ColorType := ColorType or PNG_COLOR_MASK_COLOR;
 end;

 {now he have rgb 8/16 bitDepth + maybe alpha, maybe unapplied tRNS}
 {rgb 16 bitdepth -> rgb 8 bitdepth}
 if BitDepth = 16 then
 begin
   png_set_strip_16(png_ptr);
   BitDepth := 8;
 end;

 if (not TRNSHandled) and (ColorType = PNG_COLOR_TYPE_RGB) then
 begin
   png_set_tRNS_to_alpha(png_ptr);
   TRNSHandled := true;
 end;

 { In case of some invalid image (Like an image with alpha channel
   and also tRNS chunk ? Does libPNG allow such things ?)
   probably (not confirmed) I may be left here with TRNSHandled = false.
   Ignore this. }

 if (ColorType = PNG_COLOR_TYPE_RGB) and (BitDepth = 8) then
   png_set_filler(png_ptr, High(byte), PNG_FILLER_AFTER) else
 Assert((ColorType = PNG_COLOR_TYPE_RGB_ALPHA) and (BitDepth = 8),
   'png_transform_to_rgba4byte failed to apply good png transformations');
end;

function png_has_alpha_info(png_ptr: png_structp; info_ptr: png_infop): boolean;
var ColorType: LongWord;
begin
 ColorType := png_get_color_type(png_ptr, info_ptr);
 result:= ((ColorType and PNG_COLOR_MASK_ALPHA) <> 0) or
          (png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS) <> 0);
end;

{ *ToStr -------------------------------------------------------------- }

function PngColorTypeToStr(PngColorType: longint): string;
begin
 case PngColorType of
  PNG_COLOR_TYPE_GRAY        :result := 'Gray';
  PNG_COLOR_TYPE_GRAY_ALPHA  :result := 'Gray with alpha';
  PNG_COLOR_TYPE_PALETTE     :result := 'Paletted';
  PNG_COLOR_TYPE_RGB         :result := 'RGB';
  PNG_COLOR_TYPE_RGB_ALPHA   :result := 'RGB with alpha';
  else result := 'unknown';
 end;
end;

function PngInterlaceTypeToStr(PngInterlaceType: longint): string;
begin
 case PngInterlaceType of
  PNG_INTERLACE_NONE  :result := 'None';
  PNG_INTERLACE_ADAM7 :result := 'Adam7';
  else result := 'unknown';
 end;
end;

function PngTextCompressionToStr(PngTextCompression: longint): string;
begin
 case PngTextCompression of
  PNG_TEXT_COMPRESSION_NONE: result := 'None';
  PNG_TEXT_COMPRESSION_zTXT: result := 'zTXT';
  else result := 'unknown';
 end;
end;

{ init / fini ----------------------------------------------------------- }

procedure InitSO_VER;
var ver: png_uint_32;
begin
 if KambiPngInited then
 begin
  ver := png_access_version_number();
  fSO_VER_RELEASE := ver mod 100;
  fSO_VER_MINOR := (ver mod 10000) div 100;
  fSO_VER_MAJOR := ver div 10000;
  fSO_VER_STRING := Format('%d.%d.%d',
    [fSO_VER_MAJOR, fSO_VER_MINOR, fSO_VER_RELEASE]);
 end;
end;

initialization
 InitSO_VER;
end.