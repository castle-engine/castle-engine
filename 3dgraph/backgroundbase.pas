{
  Copyright 2003-2006 Michalis Kamburelis.

  This file is part of "Kambi's 3dgraph Pascal units".

  "Kambi's 3dgraph Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's 3dgraph Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's 3dgraph Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ Base definitions for 3D backgrounds (cubes with different texture on each
  side). These are used in OpenGL programs (see unit BackgroundGL)
  and for VRML TNodeBackground. }

unit BackgroundBase;

interface

uses Images, ImagesCache;

type
  { }
  TBackgroundSide = (bsBack, bsBottom, bsFront, bsLeft, bsRight, bsTop);
  TBackgroundSides = set of TBackgroundSide;
  TBackgroundImages = array[TBackgroundSide]of TImage;

const
  BGAllSides: TBackgroundSides = [Low(TBackgroundSide) .. High(TBackgroundSide)];
  BGHorizonSides: TBackgroundSides = [bsBack, bsFront, bsLeft, bsRight];

  BackgroundImagesNone: TBackgroundImages =
  ( nil, nil, nil, nil, nil, nil );

function BackgroundImages(const BackImg, BottomImg, FrontImg, LeftImg,
  RightImg, TopImg: TImage): TBackgroundImages;

{ Laduje niebo z plikow nazywanych w konwencji panoramaToSzescian,
  starych wersji szklanych lasow i starych wersji malfunction :

  SkyNamePattern to nazwa podstawowa plikow z niebem.
  Beda do nich doklejane literki '_' i
  'u'/ = (up) top
  'd'/ = (down) bottom
  'l'/ = right (!)
  'r'/ = left  (!)
  'f'/ = front
  'b'  = back
  (w zaleznosci od tego ktora scianke nieba bedziemy chcieli odczytac)
  albo '_any' jezeli z powyzsza literka nie znajdzie (wiec mozesz
  np. _u, _d, _r wrzucic do jednego pliku _any jesli sa takie same).

  literka "u" (up) oznacza top,
  literka "d" (down) oznacza bottom - to zamieszanie dlatego ze bottom
    i back zaczynaja sie na ta sama litere.
  ! przy left i right wynika z tego ze tak sobie to zdefiniowalem w
    moim panoramaToSzescian i akurat jest to na odwrot niz definicja
    w VRMLowym nodzie Background ktory renderuje klasa TBackground.
    (W moim panoramaToSzescian jeden ciag (wlasnie "panorame") tworza
    obrazki front, left, back, right. W VRMLu97 jeden ciag to obrazki
    front, right, back, left; kwestia definicji - a tak sie nieszczesliwie
    stalo ze moja definicja z panoramaToSzescian jest inna niz VRMLa 97;
    no, ale to nic takiego, uzywanie tej funkcji pozwala mi wlasnie o tym
    zapomniec)

  Rozszerzenie pliku bedzie zgadywane przy pomocy FindExistingImageExt.
  Potem obazki beda ladowane z LoadAnyImage(FileName, ImageAllowedClasses,
  [], 0, 0) a wiec obrazek zostanie zaladowany tylko do typow ImageAllowedFormats
  (poniewaz tego wymagaja obiekty zdefiniowane w tym module - jesli kiedys
  pojawi sie sens uzywania w TBackgroundImages obrazkow spoza ImageAllowedFormats
  to dorobie jakis parametr do tej funkcji).
  W ten sposob odpowiednio nazwane szesc (lub mniej jesli wiecej
  niz jeden obrazek bedzie uzyty z _any) obrazkow zdefiniuje niebo. }
function BackgroundImagesLoadFromOldNamePattern(const SkyNamePattern: string;
  const ImageAllowedClasses: array of TImageClass): TBackgroundImages;

{ This releases and sets to @nil all images in BgImages.
  If Cache is @nil, it releases images by simple FreeAndNil,
  otherwise they are released from the cache by LoadImage_DecReference. }
procedure BackgroundImagesFreeAll(var BGImages: TBackgroundImages;
  Cache: TImagesCache);

implementation

uses SysUtils, KambiUtils;

{ TBackgroundImages -------------------------------------------------------- }

function BackgroundImages(const BackImg, BottomImg, FrontImg, LeftImg,
  RightImg, TopImg: TImage): TBackgroundImages;
begin
 result[bsBack  ] := BackImg;
 result[bsBottom] := BottomImg;
 result[bsFront ] := FrontImg;
 result[bsLeft  ] := LeftImg;
 result[bsRight ] := RightImg;
 result[bsTop   ] := TopImg;
end;

function BackgroundImagesLoadFromOldNamePattern(const SkyNamePattern: string;
  const ImageAllowedClasses: array of TImageClass): TBackgroundImages;
const
  names_suffix: array[TBackgroundSide]of string = ('b', 'd', 'f', 'r', 'l', 'u');
var
  ImgFileName: string;
  bs: TBackgroundSide;
begin
 for bs := Low(bs) to High(bs) do
 begin
  ImgFileName :=
    TryFindExistingImageExt(SkyNamePattern +'_' +names_suffix[bs], true);
  if ImgFileName = '' then
   ImgFileName := FindExistingImageExt(SkyNamePattern +'_any', true);
  result[bs] := LoadImage(ImgFileName, ImageAllowedClasses, [], 0, 0);
 end;
end;

procedure BackgroundImagesFreeAll(var BGImages: TBackgroundImages;
  Cache: TImagesCache);
var
  bs: TBackgroundSide;
begin
  if Cache <> nil then
  begin
    for bs := Low(bs) to High(bs) do
      { Cache.LoadImage_DecReference is not prepared for nil parameters,
        and some of our BGImages[bs] may be nil. }
      if BGImages[bs] <> nil then
        Cache.LoadImage_DecReference(BGImages[bs]);
  end else
  begin
    for bs := Low(bs) to High(bs) do FreeAndNil(BGImages[bs]);
  end;
end;

end.
