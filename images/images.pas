{
  Copyright 2001-2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ @abstract(This unit allows operating on 2D images.
  Keeping image in memory, loading and saving from/to a files in various
  formats, processing
  (e.g. resizing, converting to grayscale, and various other operations)
  --- it's all here.)

  The most important class here is @link(TImage),
  with 3 descendants @link(TRGBImage), @link(TAlphaImage),
  @link(TRGBEImage). These three descendants describe three possible
  ways to encode color format in memory. But you are free to create
  more descendants of TImage in your own units if you want to encode
  in pixel something different. That's one of advantages of using classes
  -- you can freely derive new classes.

  We also handle various types of image files.
  @unorderedList(
    @item(png (reading/writing using libpng, this is the only format
      supported natively (wthout running any external program) for now
      that allows us to read alpha channel))
    @item(jpeg (using pasjpeg package (it's libjpeg rewritten in Pascal)))
    @item(bmp)
    @item(rgbe (format of images created by Radiance, this is basically just
      an RLE compressed storage format for Greg Ward's ikRGBE image memory
      format. Also it allows to write colors in XYZ (CIE) instead of RGB.
      Don't confuse file format ifRGBE with image format in memory ikRGBE !.))
    @item(ppm)
    @item(Other image formats are handled by converting them
      "under the hood" using external programs. This means that we may
      call external program to handle them.
      This is available only if this unit is compiled with FPC
      (i.e. not with Delphi) on platforms where ExecuteProcess is
      implemented, and of course appropriate external tool must
      be available.
      See glViewImage docs
      [http://vrmlengine.sourceforge.net/glviewimage.php]
      for details what external programs are used.
      )
  )
  All image file formats are enumerated by @link(TImageFormat) type.
  Of course the set of such formats also may be extended with time.

  Historia:
    Kiedys ten modul zawieral zaledwie definicje strukurki TRGBImage
    i kilka podstawowych operacji na niej, na potrzeby ladowania tekstur
    dla OpenGL'a. Nie moglem do tego uzywac klas z modulu Graphics Delphi
    bo chcialem tez dzialac pod FPC ktore nie ma jeszcze gotowego GUI
    a modul Graphics musi byc przeciez z takim GUI zintegrowany,
    poprzez klase TCanvas. Z czasem zobaczylem ze zasadniczy modul Graphics
    Borlanda i tak nie oferowal mi zbyt wiele, w zasadzie zaczynajac i konczac
    swoje mozliwosci na formacie bmp, ktory zreszta byl zapisywany i odczytywany
    poprzez funkcje WinAPI (pod VCLem) lub Qt (po CLXem) wiec w zasadzie
    i tak implementacja formatow plikow graficznych byla gdzie indziej.
    Tymczasem ja nie potrzebowalem zadnego GUI - na poczatek, na potrzeby
    OpenGL'a, wystarczala mi prosta procedurka ktora zaladowalaby obrazek
    z pliku w dowolnym formacie (no, przynajmniej BMP, PNG i JPG, a wiec
    cos prostego, cos skompresowanego i cos skompresowanego stratnie)
    do tablicy 2 wymiarowej ktorej kazdy element bylby 3-ka bajtow RGB.
    Funkcja ktora to robi nazywa sie teraz LoadRGBImage.

    Znalazlem libpng i libjpeg ktore pozwolily mi latwo opanowac formaty
    png i jpg. Implementacja formatu bmp samemu okazala sie dziecinnie prosta.

    W ktoryms momencie zorientowalem sie ze ten modul powinien byc
    uniezalezniony od OpenGLa. Duzo pozniej, uzaleznilem ten modul od VectorMath
    jako ze trojka RGB lub czworka RGBA czy RGBE to w koncu tez wektor
    i moze korzystac z rozlicznych operacji zdefiniowanych juz w VectorMath.

    Dodalem formaty plikow PPM (przy okazji PGK na uniwerku), PCX
    (ze starego DOSowego kodu w TP), IPL (aby zaladowac obrazki z cornell boxa
    na RGK na uniwerku) i w chwili gdy to pisze dodaje format RGBE
    (aby moc zapisywac precyzyjne rysunki ktore dostaje od raytracera).

    Formaty w pamieci tez sie rozszerzyly : do TRGBImage doszedl TAlphaImage
    (abym mogl miec kanal alpha dla obrazkow) i TImage (jako pojemnik
    ktory moze zawierac w srodku TRGBImage lub TAlphaImage,
    w tym momencie dodaje tez format RGBE (ktory prawdopodobnie nie bedzie mial
    odpowiednika w postaci TRGBEImage, bedzie zawsze opakowany w TImage)).

  OpenGL independency:
    While this unit is obviously useful when you're using OpenGL
    (to load OpenGL textures etc.), it is not dependent on OpenGL in any way.
    This has many obvious advantages, you can use this unit without using
    OpenGL and without having any OpenGL context prepared.

  O przydatnosci formatu RGBE i generalnie formatow ktore umozliwiaja
  reprezentowanie koloru RGB jako 3xfloat zamiast 3xbajt, a wiec z duzo wieksza
  precyzja:
    Np. zalozmy ze mamy bardzo bardzo ciemny obrazek :
    dajmy na to, kazdy pixel = 3 wartosci, rgb, i maksymalna skladowa
    kazdego koloru jest < 1/512. Taki obrazek wyswietlony na ekranie
    bedzie wygladal jak czarny, jasne. Ale jezeli taki obrazek w
    rzeczywistosci zawiera jakies niezerowe kolory to rozjasnienie
    tego obrazka przez przeskalowanie wszystkich skladowych powinno
    ujawnic zawartosc obrazka. Lecz niestety - jezeli obrazek
    zostal zapisany w postaci w rodzaju TRGBImage z modulu Images
    to skladowe < 1/512 zostaly z pewnoscia zapisane jako 0 (zera!),
    a wiec rozjasnianie przez mnozenie skladowych nie ma sensu -
    - taki obrazek jest juz do niczego, stracilismy cala informacje przy
    zaokraglaniu floatow z zakresu 0..1 do zakresu 0..255.

    Mozna zastanawiac sie kiedy taka precyzja jest potrzebna. Odpowiedz brzmi :
    przy generowaniu realistycznych obrazkow, np. w VRMLRayTracer, obrazki moga
    czesto wyjsc bardzo ciemne a mimo to zawierajace istotne informacje
    w kontrastach miedzy ciemnymi kolorami.
    Ponadto chcielibysmy w raytracerach swobodnie reprezentowac tez liczby
    powyzej 1 bo w obrazkach jakie wychodza z raytracera wazne sa nie tyle
    ale proporcje miedzy nimi. Scinanie wartosci do zakresu 0..1 nie jest
    najlepszym pomyslem, potencjalnie tracimy wtedy informacje jakie zawarte
    byly w bardzo jasnych kolorach (trzeba bylo dopiero sciemnic obrazek
    zeby je zobaczyc).

    Rozwiazaniem jest wiec aby przechowywac w pamieci i zapisywac obrazek
    nie robiac tak brutalnego zaokraglania wartosci kolorow. Swietnym
    rozwiazaniem, ktore nie tworzy zbyt duzych obrazkow jest format
    rgbe (to samo co picture Radiance'a) Grega Warda, i to wlasnie
    jest format ikRGBE obrazka w pamieci i ifRGBE obrazka w pliku
    (ifRGBE to jest zasadniczo zrzut danych ikRGBE z naglowkiem i
    spakowany prosta kompresja RLE). Przy okazji do Radiance'a jest dolaczonych
    wiele programow ktore potrafia wykorzystac ta precyzje o ktorej mowilem
    wyzej i wykonywac jakies przetwarzanie obrazkow, m.in. pfilt i ximage.

  Some notes about png and zlib:
    Since 2004-01-16 situation is clear and simple:

    Every time you try to load/save from/to a PNG file,
    SavePNG or LoadPNG or other functions may raise exception
    ELibPngNotAvailable if libpng is not installed
    (i.e. PngLibraryName not found).

    This means that you don't have to install libpng+zlib on every system
    that must run programs that use this unit. You will only need libpng+zlib
    present at runtime if you want to load/save from/to PNG file at runtime.

    If you write a program that will never ever load/save a PNG file,
    just don't worry: even if your program uses this unit, Images,
    (that uses KambiPng and KambiZlib units), your program will
    *not* require libpng or zlib to be installed.
}

unit Images;

{
  TODO
  - SaveImage and LoadImage are not implemented in
    an elegant way: they just know special things about "rgbe"
    (that it always has float precision). For "png", SaveImage also
    knows special things about PNG (that it can have alpha channel).

    It should be done in a more
    general way - like "LoadRGB/SaveRGB/Load" functions in ImageformatInfos[],
    ImageformatInfos[] should also allow for other functions to be specified
    that can deal with a particular file format, like Save
    (for SaveAnyPNG etc.)

    Possibly LoadImage (with ForbiddenConvertions
    etc.) should only be exposed in the interface ?
    And Load/SaveXXX functions that load/save
    only to RGB, and Load/SaveAnyXXX functions
    (that load/save only rgb or alpha, but not rgbe)
    should only be internal ?

    In general, some manager of loading and saving functions
    should be implemented, which knows with what format given
    function works and what memory formats (i.e. what descendants
    of TImage) it can produce (in case of loading function) or
    save (in case of saving function).

  - implement more impressive resizing filters, at least simple
    linear like gluScaleImage
}

{$include kambiconf.inc}
{$include pngconf.inc}

interface

uses SysUtils, Classes, Math, KambiUtils, VectorMath,
  KambiPng, KambiPngUtils, KambiPasJpeg, FileFilters;

type
  { See TImage.AlphaChannelType. }
  TAlphaChannelType = (atNone, atSimpleYesNo, atFullRange);

{ Colors ------------------------------------------------------------ }

{ Returns if two RGB colors are be considered equal,
  i.e. each component in Color1 is different than corresponding
  component in Color2 by Tolerance or less. }
function EqualRGB(const Color1, Color2: TVector3Byte; Tolerance: Byte): boolean;

{ TImage ------------------------------------------------------------- }

type
  { Raised by @link(TImage.MakeExtracted) when coordinates on image
    are wrong.
    Possibly I will use it in more routines in the future. }
  EImagePosOutOfRange = class(Exception);

  EImageLerpError = class(Exception);
  EImageLerpInvalidClasses = class(EImageLerpError);
  EImageLerpDifferentSizes = class(EImageLerpError);

  { TImage is an abstract class representing image as a simple array of pixels.
    RawPixels is a pointer to Width * Height of values of some
    type, let's call it TPixel. TPixel specifies the color of the pixel
    (but potentially can also represent some other values
    in some descendants of TImage, not only color).
    But TPixel type is not defined anywhere, because TImage class
    does not specify what exactly TPixel is, as there are many possible
    representations for a color. What exactly is TPixel is specified
    by TImage descendant class. E.g.TRGBImage class encodes colors
    as RGB encoded in 3 bytes using TVector3Byte type.
    TAlphaImage encodes colors as 4 bytes, RGB+Alpha.
    RGBE encodes colors as 4 bytes, RGB+Exponent.

    Pixels in RawPixels are ordered in rows, in each row pixels are specified
    from left to right, rows are specified starting from lower row to upper.
    This means that you can think of RawPixels as
      ^(packed array[0..Height - 1, 0..Width - 1] of TPixel)
    Then RawPixels^[y, x] is color of pixel at position x, y.

    Note that specifying rows from lower to upper is an OpenGL standard,
    this makes using this unit with OpenGL straightforward.
    See unit @link(KambiGLUtils) for many routines that use TImage with
    OpenGL.

    Don't ever operate on RawPixels pointer directly -- allocating, reallocating,
    freeing memory pointed to by RawPixels is handled inside this class.
    You must only worry to always free created TImage instances
    (like with any class).

    Note that the only valid states of instances of this class
    are when (Width * Height > 0 and RawPixels <> nil) or
    (Width * Height = 0 and RawPixels = nil). Otherwise the fundamental
    assumption that RawPixels is a pointer to Width * Height pixels would
    be broken (as nil pointer cannot point to anything, and on the other
    side it's rather useless to have a pointer to 0 bytes (since you
    can never dereference it anyway) even if theoretically every PtrInt
    value can be treated as valid pointer to 0 bytes).

    Note about coordinates:
    1. all X, Y coordinates of pixels are 0-based
       (X in range 0..Width-1, and Y in 0..Height-1).
    2. if documentation for some method does not specify otherwise,
       correctness of coordinates is *not* checked in method,
       which can lead to various errors at runtime if you will pass
       incorrect coordinates to given routine. }
  TImage = class
  private
    FWidth, FHeight: Cardinal;
    procedure NotImplemented(const AMethodName: string);
  protected
    { Operate on this by Get/Realloc/FreeMem.
      It's always freed and nil'ed in destructor. }
    FRawPixels: Pointer;

    { Check that both images have the same sizes and Second image class
      descends from First image class. If not, raise appropriate ELerpXxx
      exceptions.

      Some implementation of TRGBImage.LerpWith may require
      other checks (since LerpWith may be sometimes allowed between unequal
      classes), so this doesn't have to be used by all TRGBImage.LerpWith
      implementations (although it's comfortable for simple implementations). }
    procedure LerpSimpleCheckConditions(SecondImage: TImage);
  public
    { Constructor without parameters creates image with Width = Height = 0
      and RawPixels = nil, so IsNull will return false.

      Both constructors must be virtual, this allows to implement things
      like TImage.MakeCopy. }
    constructor Create; overload; virtual;
    constructor Create(AWidth, AHeight: Cardinal); overload; virtual;

    destructor Destroy; override;

    property Width: Cardinal read FWidth;
    property Height: Cardinal read FHeight;
    property RawPixels: Pointer read FRawPixels;

    { True means that RawPixels = nil.
      In this case Width*Height must be 0, so either Width = 0 or Height = 0.
      False means that RawPixels <> nil and Width*Height <> 0,
      so both Width > 0 and Height > 0. }
    function IsNull: boolean;

    { This is equivalent to SetSize(0, 0).
      It sets Width = Height = 0 and RawPixels = nil. }
    procedure Null;

    { This changes Width and Height to given AWidth, AHeight.
      RawPixels is changed to point to new memory.
      Previous image contents are lost. (use one of the other methods,
      like @link(Resize), if you want to change image size preserving
      it's contents) }
    procedure SetSize(AWidth, AHeight: Cardinal);

    { This is size of TPixel in bytes for this TImage descendant. }
    class function PixelSize: Cardinal; virtual; abstract;

    { This is number of color components in TPixel.

      E.g. RGB is 3 components and RGB+Alpha is 4 components,
      RGB+Exponent is 3 components (because it describes only
      Red, Green and Blue values (Exponent value is just used
      to correctly interpret these, it's not a 4th component)). }
    class function ColorComponentsCount: Cardinal; virtual; abstract;

    { Returns pointer to (x, y) pixel of image.

      Note that they don't check X, Y correctness in any way,
      it's your responsibility to always pass 0 <= X < Width and
      0 <= Y < Height.

      Note that this function *should* be reintroduced in descendants
      to return the same value but typecasted to something better then Pointer
      (something like ^TPixel). }
    function PixelPtr(X, Y: Cardinal): Pointer;

    { Same thing as @link(PixelPtr) but always with X = 0.

      Note that this function *should* be reintroduced in descendants
      to return the same value but typecasted to something better then Pointer,
      preferably something like ^(array of TPixel). }
    function RowPtr(Y: Cardinal): Pointer;

    { This inverts RGB colors (i.e. changes each RGB component's value
      to High(Byte)-value). Doesn't touch other components,
      e.g. alpha value in case of TAlphaImage descendant.

      Note that this may be not overriden in every TImage descendant,
      then default implementation of this method in this class
      will raise EInternalError. This also means that you must not
      call inherited in descendants when overriding this method. }
    procedure InvertRGBColors; virtual;

    { This should set color of pixel to Red, Green, Blue to given
      value (as 3 single values).

      In case of descendants that have more then RGB components,
      other color components are not touched (e.g. in case of TAlphaImage
      alpha value of given pixel is not changed).

      In case of descendants that don't have anything like RGB encoded
      inside (e.g. TGrayscaleImage), this should not be overriden and then
      default implementation of this method in this class
      will raise EInternalError. This also means that you must not
      call inherited in descendants when overriding this method.

      As usual, you are responsible for guaranting correctness of given
      X, Y coordinates because their correctness is not checked here. }
    procedure SetColorRGB(const X, Y: Integer; const v: TVector3Single); virtual;

    { This returns the new created object that has exactly the same class
      and the same contents as this object.
      (note: no, this function is *not* constructor, because it's implemented
      in TImage, but it always returns some descendant of TImage). }
    function MakeCopy: TImage;

    { This changes our Width and Height and appropriately stretches
      our contents.

      If ResizeToX or ResizeToY is 0 then it means to take
      Width or Height, respectively.
      So e.g. using ResizeToX = ResizeToY = 0 is the same thing
      as using ResizeToX = Width and ResizeToY = Height and this is NOP.

      Remember that resizing may change RawPixels pointer, so all pointers
      that you aquired using functions like
      RawPixels, RGBPixels, AlphaPixels, RowPtr, PixelPtr
      may be invalid after calling Resize.

      If ProgressTitle <> '' this will call Progress.Init/Step/Fini
      from ProgressUnit to indicate progress of operation. }
    procedure Resize(ResizeToX, ResizeToY: Cardinal;
      const ProgressTitle: string = '');

    { This creates new TImage instance with size ResizeToX, ResizeToY
      and pixels copied from us and appropriately stretched.
      Class of new instance is the same as our class.

      As with @link(Resize), ResizeTo* = 0 means to use current Width/Height.
      So e.g. using MakeResized(0, 0) is the same thing as using MakeCopy.

      As with @link(Resize),
      if ProgressTitle <> '' this will call Progress.Init/Step/Fini
      from ProgressUnit to indicate progress of operation. }
    function MakeResized(ResizeToX, ResizeToY: Cardinal;
      const ProgressTitle: string = ''): TImage;

    { Mirror image horizotally (i.e. right edge is swapped with left edge) }
    procedure FlipHorizontal;

    { Returns new instance with the same class as our and size
      TileX * Width and TileY * Height and contents being our contents
      duplicated (tiled).
      Must be TileX, TileY > 0. }
    function MakeTiled(TileX, TileY: Cardinal): TImage;

    { Extracts rectangular area of this image.
      X0 and Y0 are start position (lower-left corner),
      ExtractWidth, ExtractHeight specify size of area.

      This checks parameters for correctness -- if start position in not
      good or ExtractWidth/Height are too large exception
      @link(EImagePosOutOfRange) is raised. }
    function MakeExtracted(X0, Y0, ExtractWidth, ExtractHeight: Cardinal): TImage;

    { Sets all image pixels to the same value Pixel.
      This is implemented only in descendants that represent a pixel
      as a TVector4Byte (e.g. TAlphaImage, TRGBEImage) or TVector3Byte
      (e.g. TRGBImage, 4th component is ignored in this case).

      In this class this simply raises EInternalError to say 'not implemented'.
      This also means that you must not call inherited in
      descendants when overriding this method. }
    procedure Clear(const Pixel: TVector4Byte); virtual;

    { Checks do all image pixels have the same value Pixel.
      This is implemented only in descendants that represent a pixel
      as TVector4Byte or TVector3Byte (4th component is ignored in this
      case), just like method @link(Clear).

      In this class this simply raises EInternalError to say 'not implemented'.
      This also means that you must not call inherited in
      descendants when overriding this method. }
    function IsClear(const Pixel: TVector4Byte): boolean; virtual;

    { This is a useful routine for many various conversions of image colors.
      Every pixel's RGB color is multiplied by given Matrix,
      i.e. PixelRGBColor := Matrix * PixelRGBColor.

      If some value in some channel will be < 0, it will be set to 0.
      And if it will be > High(Byte), it will be set to High(Byte).

      Examples: when
        Matrix = IdentityMatrix3Single, this is NOOP.
        Matrix = ((2, 0, 0), (0, 1, 0), (0, 0, 1))
          red channel is made lighter.
        Matrix = ((0, 0, 1), (0, 1, 0), (1, 0, 0))
          swaps red and blue channel.
        Matrix = ((0.33, 0.33, 0.33),
                  (0.33, 0.33, 0.33),
                  (0.33, 0.33, 0.33))
          is a simple conversion to grayscale (actually incorrect, even if often
          visually acceptable; actually instead of 0.33 one has to use
          GrayscaleFloat/ByteValues, this is already implemented
          in ImageTransformColorsTo1st function)

      Note: it's often more optimal to hard-code necessary color transformations
      as TColorModulatorFunc and use ModulateRGB.

      This function is only implemented for images that represent Pixel
      as RGB values, for now this means TRGBImage and TAlphaImage.
      In case of TAlphaImage (or any other class that represents
      colors as RGB + something more) alpha channel (i.e. "something more")
      is ignored (i.e. left without any modification).

      In this class this simply raises EInternalError to say 'not implemented'.
      This also means that you must not call inherited in
      descendants when overriding this method. }
    procedure TransformRGB(const Matrix: TMatrix3Single); virtual;

    { If ColorModulator = nil then this procedure does nothing.
      Else, every RGB color value of an image will be transformed using
      ColorModulator.

      Like TransformRGB:
      This function is only implemented for images that represent Pixel
      as RGB values, for now this means TRGBImage and TAlphaImage.
      In case of TAlphaImage (or any other class that represents
      colors as RGB + something more) alpha channel (i.e. "something more")
      is ignored (i.e. left without any modification).

      In this class this simply raises EInternalError to say 'not implemented'.
      This also means that you must not call inherited in
      descendants when overriding this method.  }
    procedure ModulateRGB(const ColorModulator: TColorModulatorByteFunc); virtual;

    { Just like ModulateRGB, but this returns new image, not changing initial
      image. This means that if ColorModulator = nil this is
      equivalent to MakeCopy.

      Implemented if and only if ModulateRGB is implemented. }
     function MakeModulatedRGB(
       const ColorModulator: TColorModulatorByteFunc): TImage;

    { Converts image colors to grayscale.

      Implemented if and only if ModulateRGB is implemented.
      When image has alpha channel, alpha channel value
      (or just anything beyond 3 rgb components) is ignored (not modified).

      This changes color to grayscale, but format of memory storage is the same.
      For example, for TRGBImage, they are still kept in RGB format
      (just Red = Green = Blue). If you want to convert to true Grayscale format,
      you should use TRGBImage.ToGrayscale that will create new
      TGrayscaleImage instance. }
    procedure Grayscale;

    {$ifdef FPC}

    { This converts every image color using Color*Convert function from VectorMath.
      "Channel" parameter determines which Color*Convert function to use
      (Red, Green or Blue), must be 0, 1 or 2.

      Implemented if and only if ModulateRGB is implemented. }
    procedure ConvertToChannelRGB(Channel: Integer);

    { This converts every image color using Color*Strip function from VectorMath.
      "Channel" parameter determines which Color*Strip function to use
      (Red, Green or Blue), must be 0, 1 or 2.

      Implemented if and only if ModulateRGB is implemented. }
    procedure StripToChannelRGB(Channel: Integer);

    {$endif FPC}

    { This returns true only if given Image and this image have the same
      classes, the same sizes (Width, Height) and contain exactly
      the same values in RawPixels. }
    function IsEqual(Image: TImage): boolean;

    { This is like IsEqual, but is compares only given parts of the images.
      Note that it's your responsibility to make sure that given areas
      are really within the sizes of Self or Image.

      Overloaded version without SelfXxx parameters compares whole Self
      to given part of Image. Analogously, version without ImageXxx parameters
      compares whole Image to part of Self.

      @groupBegin }
    function ArePartsEqual(
      const SelfX0, SelfY0, SelfWidth, SelfHeight: Cardinal;
      Image: TImage;
      const ImageX0, ImageY0, ImageWidth, ImageHeight: Cardinal): boolean; overload;

    function ArePartsEqual(
      Image: TImage;
      const ImageX0, ImageY0, ImageWidth, ImageHeight: Cardinal): boolean; overload;

    function ArePartsEqual(
      const SelfX0, SelfY0, SelfWidth, SelfHeight: Cardinal;
      Image: TImage): boolean; overload;
    { @groupEnd }

    { These check that Image and Self have equal classes, and then
      copy Self to Image or Image to Self.
      X and Y is each case are the position on the destinantion image.
      @groupBegin }
    procedure CopyFrom(Image: TImage; const X0, Y0: Cardinal);
    procedure CopyTo(Image: TImage; const X0, Y0: Cardinal);
    { @groupEnd }

    { @abstract(Check does image have an alpha channel,
      and if yes analyze alpha channel: is it a single yes-no (only full
      or none values), or does it have alpha values in between?)

      This is quite useful for automatic detection how alpha textures
      should be displayed: for simple yes/no alpha, OpenGL alpha_test
      is a simple solution. For full range alpha, OpenGL blending should
      be used. Blending is a little problematic, since it requires
      special rendering order, since it doesn't cooperate nicely with
      Z-buffer. That's why we try to detect simple yes/no alpha textures,
      so that we're able to use simpler alpha test for them.

      This method analyzes every pixel. It's alpha is considered "simple"
      if it's <= AlphaTolerance, or >= 255 - AlphaTolerance.
      So for the default AlphaTolerance, "simple" alpha means only exactly
      0 or 255 (maximum Byte values).
      The method returns true if the ratio of non-simple pixels is
      WrongPixelsTolerance. For example, default WrongPixelsTolerance = 0
      means that every pixel must have "simple" alpha channel.
      Greated WrongPixelsTolerance values may allow some tolerance,
      for example WrongPixelsTolerance = 0.01 allows 1 percent of pixels
      to fail the "simple alpha" test and the image can still be considered
      "simple yes/no alpha channel".

      In summary, default Tolerance values are 0, so exactly all pixels
      must have exactly full or exactly none alpha. Increasing
      tolerance values (for example, AlphaTolerance = 5
      and WrongPixelsTolerance = 0.01 may be good start --- still conservative
      enough, and tolerate small deviations) allows you to accept
      more images as simple yes/no alpha. Of course too large tolerance
      values have no sense --- AlphaTolerance >= 128, or WrongPixelsTolerance >= 1.0
      will cause all images to be accepted as "simple yes/no alpha".

      @italic(Descendants implementors notes:) in this class, this simply
      always returns atNone. For descendants that have alpha channel,
      implement it, honouring AlphaTolerance and WrongPixelsTolerance as
      described. }
    function AlphaChannelType(
      const AlphaTolerance: Byte = 0;
      const WrongPixelsTolerance: Single = 0.0): TAlphaChannelType; virtual;

    { Makes linear interpolation of colors from this image and the SecondImage.
      Intuitively, every pixel in new image is set to

@preformatted(
  (1 - Value) * Self[pixel] + Value * SecondImage[pixel]
)

      Both images need to have the exact same size.
      If they are not, EImageLerpDifferentSizes is raised.

      Not all TImage combinations are allowed. Every subclass is required
      to override this to at least handle Lerp between itself.
      That is, TRGBImage.Lerp has to handle Lerp with other TRGBImage,
      TAlphaImage.Lerp has to handle Lerp with other TAlphaImage etc.
      Other combinations may be permitted, if useful and implemented.
      EImageLerpInvalidClasses is raised if given class combinations are
      not allowed.

      In this class, this simply always raises EImageLerpInvalidClasses.

      @raises(EImageLerpDifferentSizes When SecondImage size differs
        from this image.)
      @raises(EImageLerpInvalidClasses When Lerp between this TImage
        descendant class and SecondImage class is not implemented.) }
    procedure LerpWith(const Value: Single; SecondImage: TImage); virtual;
  end;

{ TImageClass and arrays of TImageClasses ----------------------------- }

type
  TImageClass = class of TImage;

  { Note: Don't name it TDynImageClassesArray,
    as such naming convention is reserved for TDynArray_Base descendants. }
  TDynArrayImageClasses = array of TImageClass;

{ True if ImageClass is one of the items ImageClasses array
  or inherits from (i.e. is descendant of) one of them. }
function InImageClasses(ImageClass: TImageClass;
  const ImageClasses: array of TImageClass): boolean; overload;

{ This is a shortcut for InImageClasses(Image.ClassType, ImageClasses) }
function InImageClasses(Image: TImage;
  const ImageClasses: array of TImageClass): boolean; overload;

{ True if both arrays contain exactly the same classes in the same order.

  May be extended in the future to do better checks and return true
  also if both array contain the same classes but in different order,
  and one array may contain the same classes duplicated any times.
  So the intention is that you should treat both arrays as sets
  (i.e. order of elements is ignored).

  The problem is that this function should be lighting fast
  (as the main purpose of it is to use it in constructions like
  setting property values, e.g.
    if ImageClassesArraysEqual(Value, SomeProperty) then
    begin
     SomeProperty := Value;
     ... do some lengthy operations to update new value of SomeProperty ...
    end;
  ), and doing smarter checks may cost us a little time.

  So for now this function returns
  - true if for sure both arrays contain the same classes and
  - false if *possibly* they don't contain the same classes. }
function ImageClassesEqual(const Ar1, Ar2: array of TImageClass): boolean;

procedure ImageClassesAssign(var Variable: TDynArrayImageClasses;
  const NewValue: array of TImageClass);

{ TImage basic descendants ------------------------------------------------- }

type
  TAlphaImage = class;
  TRGBEImage = class;
  TGrayscaleImage = class;

  { Here pixel is represented as TVector3Byte (red, green, blue) }
  TRGBImage = class(TImage)
  private
    function GetRGBPixels: PVector3Byte;
  public
    { This is the same pointer as RawPixels, only typecasted to PVector3Byte }
    property RGBPixels: PVector3Byte read GetRGBPixels;

    class function PixelSize: Cardinal; override;
    class function ColorComponentsCount: Cardinal; override;

    function PixelPtr(X, Y: Cardinal): PVector3Byte;
    function RowPtr(Y: Cardinal): PArray_Vector3Byte;

    procedure InvertRGBColors; override;

    procedure SetColorRGB(const x, y: Integer; const v: TVector3Single); override;

    procedure Clear(const Pixel: TVector4Byte); override;
    function IsClear(const Pixel: TVector4Byte): boolean; override;

    procedure TransformRGB(const Matrix: TMatrix3Single); override;
    procedure ModulateRGB(const ColorModulator: TColorModulatorByteFunc); override;

    { This functions creates new TAlphaImage object with RGB colors
      copied from this object, but alpha of each pixel is set
      to some random value (whatever was at that particular memory
      place at that time). }
    function ToAlphaImage_AlphaDontCare: TAlphaImage;

    { Like @link(ToAlphaImage_AlphaDontCare), but alpha of every
      pixel is set to given Alpha. }
    function ToAlphaImage_AlphaConst(Alpha: byte): TAlphaImage;

    { Like @link(ToAlphaImage_AlphaDontCare), but alpha of every
      pixel is set to either AlphaOnColor (when color of pixel
      is equal to AlphaColor with Tolerance, see @link(EqualRGB))
      or AlphaOnNoColor. }
    function ToAlphaImage_AlphaDecide(
      const AlphaColor: TVector3Byte; Tolerance: Byte;
      AlphaOnColor: Byte; AlphaOnNoColor: Byte): TAlphaImage;

    { Converts image to TRGBEImage format.

      Although RGBE format offers superior precision compared to RGB on 3 bytes,
      there is a slight chance of some unnoticeable loss of information
      in such convertion, since floating-point values are involved
      in calculation.

      But generally this conversion is relatively safe (contrary to
      convertion RGBE -> RGB, which must be lossy).

      But still you should note that doing such convertion has little
      sense since RGBE is useful only when you have colors that can't
      be expressed as simple RGB on 3 bytes. But by using this convertion
      you initially fill RGBE image with data that does not have
      precision beyond standard 0..255 discreet range for each RGB component... }
    function ToRGBEImage: TRGBEImage;

    function ToGrayscale: TGrayscaleImage;

    { Draws horizontal line. Must be y1 <= y2, else it is NOOP. }
    procedure HorizontalLine(const x1, x2, y: Integer;
      const Color: TVector3Byte);

    { Draws vertical line. Must be x1 <= x2, else it is NOOP. }
    procedure VerticalLine(const x, y1, y2: Integer;
      const Color: TVector3Byte);

    { This is a very special constructor.
      It creates image with the same size as MapImage.
      It also resizes ReplaceWhiteImage, ReplaceBlackImage
      to the size of MapImage.

      Then it inits color of each pixel of our image with
      combined colors of two pixels on the same coordinates from
      ReplaceWhiteImage, ReplaceBlackImage, something like
        Pixel[x, y] := ReplaceWhiteImage[x, y] * S +
                       ReplaceBlackImage[x, y] * (S-1)
      where S = average of red, gree, blue of color MapImage[x, y].

      This means that final image will look like ReplaceWhiteImage
      in the areas where MapImage is white, and it will look like
      ReplaceBlackImage in the areas where MapImage is black. }
    constructor CreateCombined(const MapImage: TRGBImage;
      var ReplaceWhiteImage, ReplaceBlackImage: TRGBImage);

    procedure LerpWith(const Value: Single; SecondImage: TImage); override;
  end;

  TAlphaImage = class(TImage)
  private
    function GetAlphaPixels: PVector4Byte;
  public
    { This is the same pointer as RawPixels, only typecasted to PVector4Byte }
    property AlphaPixels: PVector4Byte read GetAlphaPixels;

    class function PixelSize: Cardinal; override;
    class function ColorComponentsCount: Cardinal; override;

    function PixelPtr(X, Y: Cardinal): PVector4Byte;
    function RowPtr(Y: Cardinal): PArray_Vector4Byte;

    procedure InvertRGBColors; override;

    procedure SetColorRGB(const x, y: Integer; const v: TVector3Single); override;

    procedure Clear(const Pixel: TVector4Byte); override;
    function IsClear(const Pixel: TVector4Byte): boolean; override;

    procedure TransformRGB(const Matrix: TMatrix3Single); override;
    procedure ModulateRGB(const ColorModulator: TColorModulatorByteFunc); override;

    { Sets alpha of every pixel to either AlphaOnColor
      (when color of pixel is equal to AlphaColor with Tolerance,
      see @link(EqualRGB)) or AlphaOnNoColor. }
    procedure AlphaDecide(const AlphaColor: TVector3Byte;
      Tolerance: Byte; AlphaOnColor: Byte; AlphaOnNoColor: Byte);

    { This initializes image contents: RGB channels from RGB image,
      alpha channel from Grayscale image. Given RGB and Grayscale
      images must have the same size, and this is the resulting
      size of this image after Compose call. }
    procedure Compose(RGB: TRGBImage; AGrayscale: TGrayscaleImage);

    function AlphaChannelType(
      const AlphaTolerance: Byte = 0;
      const WrongPixelsTolerance: Single = 0.0): TAlphaChannelType; override;

    procedure LerpWith(const Value: Single; SecondImage: TImage); override;

    { Remove alpha channel, creating new TRGBImage. }
    function ToRGBImage: TRGBImage;
  end;

  { Color is encoded as 3 mantisas + 1 exponent,
    this is Greg Ward's format described in"Graphic Gems" gem II.5.
    This gives
    1. High floating-point-like precision for colors
    2. Color expressed in RGBE format consist of 3 non-negative values,
       not necessarily <= 1.0.
    And all this while pixel is only 4 bytes long (instead of typical
    3 x single, 12 bytes). }
  TRGBEImage = class(TImage)
  private
    function GetRGBEPixels: PVector4Byte;
  public
    { This is the same pointer as RawPixels, only typecasted to PVector4Byte }
    property RGBEPixels: PVector4Byte read GetRGBEPixels;

    class function PixelSize: Cardinal; override;
    class function ColorComponentsCount: Cardinal; override;

    function PixelPtr(X, Y: Cardinal): PVector4Byte;
    function RowPtr(Y: Cardinal): PArray_Vector4Byte;

    procedure SetColorRGB(const x, y: Integer; const v: TVector3Single); override;

    procedure Clear(const Pixel: TVector4Byte); override;
    function IsClear(const Pixel: TVector4Byte): boolean; override;

    { Converts TRGBEImage to TRGBImage.
      Colors in pixels are simply rounded using
      Vector3Byte(VectorRGBETo3Single()).
      So such convertion not only kills the floating-point
      precision in RGBE format but also clamps color components
      to 1.0 (because colors in RGBE format consist of 3 non-negative values,
      not necessarily <= 1.0. But converting them to simple RGB on 3 bytes
      clamps values > 1.0 down to 1.0 (they are converted to High(Byte)). }
    function ToRGBImage: TRGBImage;

    {$ifdef FPC}
    { Every component (red, green, blue) of every pixel
      is multiplied by Scale. }
    procedure ScaleColors(const Scale: Single);

    { Every component (red, green, blue) or every pixel
      is changed to Power(Value, Exp).
      So e.g. Exp = 1/2.2 gives commonly used gamma correction. }
    procedure ExpColors(const Exp: Single);
    {$endif}

    procedure LerpWith(const Value: Single; SecondImage: TImage); override;
  end;

  { Color is a simple Byte value.

    TODO: this is just a start, not supported by many routines as it should
    for now. Many loading routines should be able to directly load image
    to this, if it's the most appropriate format. Right now, LoadImage
    routine will load to this only if TRGBImage will not be allowed and
    this will be (and loading will be done by conversion of RGB to this...). }
  TGrayscaleImage = class(TImage)
  private
    function GetGrayscalePixels: PByte;
  public
    { This is the same pointer as RawPixels, only typecasted to PByteArray }
    property GrayscalePixels: PByte read GetGrayscalePixels;

    class function PixelSize: Cardinal; override;
    class function ColorComponentsCount: Cardinal; override;

    function PixelPtr(X, Y: Cardinal): PByte;
    function RowPtr(Y: Cardinal): PByteArray;

    procedure Clear(const Pixel: Byte); reintroduce;
    function IsClear(const Pixel: Byte): boolean; reintroduce;

    { Every pixels value is halved (divided by 2).
      This is done by simple bitshift, so you can be sure that all
      components are < 2^7 after this. }
    procedure HalfColors;

    procedure LerpWith(const Value: Single; SecondImage: TImage); override;
  end;

{ RGBE <-> 3 Single color convertion --------------------------------- }

{ Encode RGB color as Greg Ward's Red + Green + Blue + Exponent format,
  thus allowing you to encode 12 bytes (3 * SizeOf(Single) = 3 * 4 = 12)
  in 4 bytes (4 * SizeOf(Byte) = 4), usually without any significant
  loss.

  Each component of V (red, green, blue) is from range
  [0, +infinity), not merely from [0, 1].
  I.e. V must have only nonnegative values. }
function Vector3ToRGBE(const v: TVector3Single): TVector4Byte;

{ Decode Red + Green + Blue + Exponent back into RGB 3 x Single. }
function VectorRGBETo3Single(const v: TVector4Byte): TVector3Single;

{ --------------------------------------------------------------------
  rzeczy speszial do rgbe (jako format pliku i jako TImageKind) }

{ jesli RoundToByteRGB to zwroci result.Kind = ikRGB (patrz komentarze
  przy LoadRGBEToByteRGB). Wpp. result.Kind = ikRGBE. }
function LoadRGBE(Stream: TStream; RoundToByteRGB: boolean): TImage;
{ Img.Kind musi byc in ikRGB, ikRGBE (checked even in RELEASE).
  W tym pierwszym przypadku - patrz komentarze przy SaveRGBEFromByteRGB. }
procedure SaveRGBE(const Img: TImage; Stream: TStream);

{ rownowazne ImageRecToRGB(LoadRGBE(Stream, true)). Nazwa jest taka
  pokrecona ("ToByteRGB") bo naprawde rzadko powinienes tego uzywac i zawsze
  musisz zdawac sobie sprawe z ograniczen : zaokraglanie do bajtu w zasadzie
  zabija caly sens formatu RGBE. Odczytaj plik RGBE to RGB (3xByte) i potem
  go zapisz (chocby nawet z powrotem w RGBE) a stracisz cala precyzje zawarta
  w formacie RGBE. }
function LoadRGBEToByteRGB(Stream: TStream): TRGBImage;
{ rownowazne SaveRGBE(ImageRecFromRGB(Img), Stream). Podobnie jak przy
  ImageRGBToRGBE, konwersja RGB na RGBE jest bezstratna ale moze byc
  bezsensowna (po co ci precyzja skoro dane juz sa pozbawione precyzji ?)  }
procedure SaveRGBEFromByteRGB(const Img: TRGBImage; Stream: TStream);

{ -------------------------------------------------------------------------- }

{ LoadXxx : load image from Stream.
  An appropriate descendant of EImageLoadError will be raised
  in case of error when reading from Stream or when Stream will not
  contain correct data. }

type
  EImageLoadError = class(Exception);
  EInvalidImageFormat = class(EImageLoadError);
  EInvalidBMP = class(EInvalidImageFormat);
  EInvalidPNG = class(EInvalidImageFormat);
  EInvalidJPEG = class(EInvalidImageFormat);
  EInvalidPCX =  class(EInvalidImageFormat);
  EInvalidPPM = class(EInvalidImageFormat);
  EInvalidIPL = class(EInvalidImageFormat);
  EInvalidRGBE = class(EInvalidImageFormat);

function LoadBMP(Stream: TStream): TRGBImage;
function LoadPNG(Stream: TStream): TRGBImage;
function LoadJPEG(Stream: TStream): TRGBImage;
{ Only 256-color PCX can be handled.
  This will not probably be ever improved (al least by me, Kambi),
  since I don't use PCX images anymore.
  Use PNG if you want lossless compression. }
function LoadPCX(Stream: TStream): TRGBImage;
{ Loads only the first image in .ppm file. }
function LoadPPM(Stream: TStream): TRGBImage;
function LoadIPL(Stream: TStream): TRGBImage;
function LoadGIF(Stream: TStream): TRGBImage;
function LoadTGA(Stream: TStream): TRGBImage;
function LoadSGI(Stream: TStream): TRGBImage;
function LoadTIFF(Stream: TStream): TRGBImage;
function LoadJP2(Stream: TStream): TRGBImage;
function LoadEXR(Stream: TStream): TRGBImage;

{ ------------------------------------------------------------------------------

  SaveXxx. Each file format has specialized SaveXxx that allows
  you to give some parameters special for given format.

  Each format must also have procedure with two parameters
  (const Img: TRGBImage; Stream: TStream), this will be used with
  ImageFormatsInfo[].
  This means that below we must use overloading instead of
  default parameters, since pointers to given procedures must be
  compatible with @link(TRGBImageSaveFunc). }

procedure SaveBMP(const img: TRGBImage; Stream: TStream);
procedure SavePNG(const img: TRGBImage; Stream: TStream; interlaced: boolean); overload;
procedure SavePNG(const img: TRGBImage; Stream: TStream); { interlaced = false } overload;
procedure SaveJPEG(const img: TRGBImage; Stream: TStream; quality: integer); overload;
procedure SaveJPEG(const img: TRGBImage; Stream: TStream); { quality = 90 } overload;
procedure SavePPM(const img: TRGBImage; Stream: TStream; binary: boolean); overload;
procedure SavePPM(const img: TRGBImage; Stream: TStream); { binary = true } overload;

{ Load and save with possible alpha channel ---------------------------------- }

type
  { }
  EUnableToLoadImage = class(EImageLoadError);

type
  TImageFormatRequirements = (frWithoutAlpha, frWithAlpha, frAny);

{ LoadAnyPNG : zaladuj png. Jesli frAny to zwroci ikRGB lub ikAlpha,
  w zaleznosci od tego co jest zapisane w obrazku. Jesli frWithoutAlpha
  to zwroci ikRGB - jesli obrazek mial zapisane alpha to albo je usunie
  (jesli ConvertToRequired = true) lub rzuci wyjatek EUnableToLoadImage
  (jesli ConvertToRequired = false). Podobnie jesli frWithAlpha to zwroci
  obrazek ikAlpha - jesli obrazek nie mial zapisanego alpha to doda
  alpha (jesli ConvertToRequired = true) rowne wszedzie 1.0 (czyli High(Byte))
  lub rzuci wyjatek EUnableToLoadImage (jesli ConvertToRequired = false).

  LoadPNG mozna teraz zrealizowac jako proste
  ImageRecToRGB(LoadAnyPNG(Stream, frWithoutAlpha, true)); }
function LoadAnyPNG(Stream: TStream; FormatRequired: TImageFormatRequirements;
  ConvertToRequired: boolean): TImage;

{ SaveAnyPNG: Img moze miec Kind = ikRGB (wtedy zadziala jak SavePNG)
  lub ikAlpha (wtedy zapamieta alpha obrazka w pliku) }
procedure SaveAnyPNG(const Img: TImage; Stream: TStream; Interlaced: boolean);

function LoadAnyBMP(Stream: TStream; FormatRequired: TImageFormatRequirements;
  ConvertToRequired: boolean): TImage;

function LoadAnyGIF(Stream: TStream; FormatRequired: TImageFormatRequirements;
  ConvertToRequired: boolean): TImage;

function LoadAnyTGA(Stream: TStream; FormatRequired: TImageFormatRequirements;
  ConvertToRequired: boolean): TImage;

function LoadAnySGI(Stream: TStream; FormatRequired: TImageFormatRequirements;
  ConvertToRequired: boolean): TImage;

function LoadAnyTIFF(Stream: TStream; FormatRequired: TImageFormatRequirements;
  ConvertToRequired: boolean): TImage;

function LoadAnyJP2(Stream: TStream; FormatRequired: TImageFormatRequirements;
  ConvertToRequired: boolean): TImage;

function LoadAnyEXR(Stream: TStream; FormatRequired: TImageFormatRequirements;
  ConvertToRequired: boolean): TImage;

{ File formats managing ----------------------------------------------------- }

type
  { }
  TImageFormat = (ifBMP, ifPNG, ifJPEG, ifPCX, ifPPM, ifIPL, ifRGBE,
    ifGIF, ifTGA, ifSGI, ifTIFF, ifJP2, ifEXR);
  TImageFormats = set of TImageFormat;
  TRGBImageLoadFunc = function (Stream: TStream): TRGBImage;
  TRGBImageSaveFunc = procedure (const Img: TRGBImage; Stream: TStream);
  TImageLoadFunc = function (Stream: TStream;
    FormatRequired: TImageFormatRequirements;
    ConvertToRequired: boolean): TImage;
  { some day TImageSaveFunc will be added too }

  { A type to index TImageFormatInfo.Exts array and also for TImageFormatInfo.ExtsCount.
    So TImageFormatInfo.Exts array is indexed from 1,
    and TImageFormatInfo.ExtsCount must be >= 1, so each file format must have at least one
    (treated as "default" in some cases) file extension. }
  TImageFormatInfoExtsCount = 1..3;

  TImageFormatInfo = record
    { Human-readble format name.

      Note that this is supposed to be shown to normal user,
      in save dialog boxes etc. So it should be short and concise. I used to
      have here long format names like @code(JFIF, JPEG File Interchange Format) or
      @code(PNG, Portable Network Graphic), but they are too ugly, and unnecessarily
      resolving format abbrevs. For example, most users probably used JPEG,
      but not many have to know, or understand, that actually this is image format JFIF;
      these are technical and historical details that are not needed for normal usage of image
      operations.

      Saying it directly, I want to keep this FormatName short and concise.
      This is not a place to educate users what some abbrev means.
      This is a place to "name" each file format in the most natural way, which
      usually means to only slightly rephrase typical file format extension.

      In practice, I now copy descriptions from English GIMP open dialog. }
    FormatName: string;

    ExtsCount: TImageFormatInfoExtsCount;
    { File extensions recognized as this image file format.

      These file extensions must be lowercase, without leading dot '.'.
      First extension is the default extension of this file format
      (some procedures make use of it). }
    Exts: array[TImageFormatInfoExtsCount] of string;

    LoadRGB: TRGBImageLoadFunc; {< = nil if we can't load it from RGB }
    SaveRGB: TRGBImageSaveFunc; {< = nil if we can't save it to RGB }
    Load: TImageLoadFunc; {< = nil if can't load it with alpha channel }
  end;

const
  ImageFormatInfos :array[TImageFormat]of TImageFormatInfo =
  ( ( FormatName: 'Windows BMP image';
      ExtsCount: 1; Exts: ('bmp', '', '');
      LoadRGB: {$ifdef FPC_OBJFPC} @ {$endif} LoadBMP;
      SaveRGB: {$ifdef FPC_OBJFPC} @ {$endif} SaveBMP;
      Load: {$ifdef FPC_OBJFPC} @ {$endif} LoadAnyBMP),
    { Portable Network Graphic } { }
    ( FormatName: 'PNG image';
      ExtsCount: 1; Exts: ('png', '', '');
      LoadRGB: {$ifdef FPC_OBJFPC} @ {$endif} LoadPNG;
      SaveRGB: {$ifdef FPC_OBJFPC} @ {$endif} SavePNG;
      Load: {$ifdef FPC_OBJFPC} @ {$endif} LoadAnyPNG),
    { JFIF, JPEG File Interchange Format } { }
    ( FormatName: 'JPEG image';
      ExtsCount: 3; Exts: ('jpg', 'jpeg', 'jpe');
      LoadRGB: {$ifdef FPC_OBJFPC} @ {$endif} LoadJPEG;
      SaveRGB: {$ifdef FPC_OBJFPC} @ {$endif} SaveJPEG;
      Load: nil),
    ( FormatName: 'ZSoft PCX image';
      ExtsCount: 1; Exts: ('pcx', '', '');
      LoadRGB: {$ifdef FPC_OBJFPC} @ {$endif} LoadPCX;
      SaveRGB: nil;
      Load: nil),
    { Portable Pixel Map } { }
    ( FormatName: 'PPM image';
      ExtsCount: 1; Exts: ('ppm', '', '');
      LoadRGB: {$ifdef FPC_OBJFPC} @ {$endif} LoadPPM;
      SaveRGB: {$ifdef FPC_OBJFPC} @ {$endif} SavePPM;
      Load: nil),
    ( FormatName: 'IPLab image';
      ExtsCount: 1; Exts: ('ipl', '', '');
      LoadRGB: {$ifdef FPC_OBJFPC} @ {$endif} LoadIPL;
      SaveRGB: nil;
      Load: nil),
    ( FormatName: 'RGBE (RGB+Exponent) image';
      ExtsCount: 2; Exts: ('rgbe', 'pic', '');
      LoadRGB: {$ifdef FPC_OBJFPC} @ {$endif} LoadRGBEToByteRGB;
      SaveRGB: {$ifdef FPC_OBJFPC} @ {$endif} SaveRGBEFromByteRGB;
      Load: nil),
    { Graphics Interchange Format } { }
    ( FormatName: 'GIF image';
      ExtsCount: 1; Exts: ('gif', '', '');
      LoadRGB: {$ifdef FPC_OBJFPC} @ {$endif} LoadGIF;
      SaveRGB: nil;
      Load: {$ifdef FPC_OBJFPC} @ {$endif} LoadAnyGIF),
    ( FormatName: 'TarGA image';
      ExtsCount: 1; Exts: ('tga', '', '');
      LoadRGB: {$ifdef FPC_OBJFPC} @ {$endif} LoadTGA;
      SaveRGB: nil;
      Load: {$ifdef FPC_OBJFPC} @ {$endif} LoadAnyTGA),
    ( FormatName: 'SGI image';
      ExtsCount: 1; Exts: ('sgi', '', '');
      LoadRGB: {$ifdef FPC_OBJFPC} @ {$endif} LoadSGI;
      SaveRGB: nil;
      Load: {$ifdef FPC_OBJFPC} @ {$endif} LoadAnySGI),
    ( FormatName: 'TIFF image';
      ExtsCount: 1; Exts: ('tif', '', '');
      LoadRGB: {$ifdef FPC_OBJFPC} @ {$endif} LoadTIFF;
      SaveRGB: nil;
      Load: {$ifdef FPC_OBJFPC} @ {$endif} LoadAnyTIFF),
    ( FormatName: 'JP2 image';
      ExtsCount: 1; Exts: ('jp2', '', '');
      LoadRGB: {$ifdef FPC_OBJFPC} @ {$endif} LoadJP2;
      SaveRGB: nil;
      Load: {$ifdef FPC_OBJFPC} @ {$endif} LoadAnyJP2),
    ( FormatName: 'EXR image';
      ExtsCount: 1; Exts: ('exr', '', '');
      LoadRGB: {$ifdef FPC_OBJFPC} @ {$endif} LoadEXR;
      SaveRGB: nil;
      Load: {$ifdef FPC_OBJFPC} @ {$endif} LoadAnyEXR)
  );

  DefaultSaveImageFormat: TImageFormat = ifBMP;

{ znajdz TImageFormat ktore ma podane dane fileext. fileext moze byc z poczatkowa
  kropka (a wiec tak jak zwraca je ExtractFileExt) lub bez. Zwraca false i nie
  zmienia ImgFormat jesli nie ma formatu o danym fileext. }
function FileExtToImageFormat(fileext: string;
  OnlyLoadable, OnlySaveable: boolean; out ImgFormat: TImageFormat): boolean;

{ jak wyzej, ale jesli nie ma formatu o danym fileext to zwraca DefFormat. }
function FileExtToImageFormatDef(const fileext: string;
  OnlyLoadable, OnlySaveable: boolean; DefFormat: TImageFormat): TImageFormat;

{ jak FileExtToImageFormat ale tutaj interesuje nas tylko czy MOZESZ
  taki TImageFormat znalezc, a nie : jaki on jest. }
function IsFileExtToImageFormat(const fileext: string;
  OnlyLoadable, OnlySaveable: boolean): boolean;

{ j.w. z OnlyLoadable = true, OnlySaveable = false. }
function IsFileExtLoadableImage(const fileext: string): boolean;

type
  ENoExistingImageExt = class(Exception);

{ zadana jest nazwa pliku s, bez koncowego rozszerzenia (takze bez koncowej kropki
  przed rozszerzeniem). Probuje doklejac rozszerzenia sposrod ImageFormatInfos[].exts
  az znajdzie takie ze po doklejeniu plik istnieje (NormalFileExists).

  Jesli nie znajdzie - wersja Try zwroci '', wersja bez Try rzuci wyjatek
  ENoExistingImageExt. Jesli znajdzie - zwraca nazwe pliku
  z doklejonym rozszerzeniem. Jesli OnlyLoadable, bedzie szukal tylko wsrod
  formatow loadable. }
function FindExistingImageExt(const fname: string; OnlyLoadable: boolean): string;
function TryFindExistingImageExt(const fname: string; OnlyLoadable: boolean): string;

{ List available image file formats.

  This is basically for debug/info purposes, you can show this to user
  to let him know which formats are supported (and by which extensions
  they are recognized). Although almost always a better way to show
  this to user is just to use SaveImage_FileFilters with a save dialog
  like TGLWindow.FileDialog, this shows file types in the open/save dialog,
  so it's most natural and convenient to user.

  ListImageExtsLong produces a multiline info (separated by NL, last line not terminated
  by NL), shows all extensions and FormatName for each file format.
  Each line starts with LinePrefix.

  ListImageExtsShort writes all recognized extensions separated by comma (', ').

  @groupBegin }
function ListImageExtsLong(OnlyLoadable, OnlySaveable: boolean; const LinePrefix: string): string;
function ListImageExtsShort(OnlyLoadable, OnlySaveable: boolean): string;
{ @groupEnd }

{ Load[RGB]Image -------------------------------------------------------------- }

{ LoadRGBImage : zgadnij format strumienia na podstawie rozszerzenia pliku
  i zaladuj. typeext moze ale nie musi zawierac na poczatku kropke.
  Mozna tez podac zamiast strumienia nazwe pliku (kazdy rozumie,
  ze implementacja utworzy strumien CreateReadFileStream).

  LoadRGBImage wywoluje odpowiednie LoadXxx na podstawie podanego rozszerzenia.
  Jesli nie rozpoznaje rozszerzenia - wyjatek EImageFormatNotSupported.
}

type
  EImageFormatNotSupported = class(Exception);

function LoadRGBImage(Stream: TStream; const typeext: string): TRGBImage; overload;
function LoadRGBImage(const fname: string): TRGBImage; overload;

type
  TProcedureRGBImage = procedure(var Image: TRGBImage);

{ Jesli resizeTo[] <> 0 to dany wymiar bedzie resizowany.

  ImageProc, jesli <> nil, jest wywolywane dla zaladowanego image'a PRZED wykonaniem
  ewentualnego skalowania. Ma to zastosowanie np. gdy chcesz zaladowac stosunkowo
  maly obrazek z pliku, zamienic go np. na czarno-bialy i potem przeskalowac na
  bardzo duzy rozmiar. W takiej sytuacji duzo bardziej ekonomiczne jest wywolanie
  konwersji na black&white jeszcze PRZED skalowaniem, a wiec najlepiej przekaz
  MakeBlackAndWhite jako ImageProc. Acha, jesli chcesz to mozesz w ImageProc
  zmienic rozmiary obrazka. (chociaz dla typowego resizu pewnie wygodniej bedzie
  uzyc parametrow resizeToX, resizeToY)
}
function LoadRGBImage(const fname: string; resizeToX: Cardinal; resizeToY: Cardinal;
  UnscaledImageProc: TProcedureRGBImage {$IFDEF DEFPARS}=nil{$ENDIF}): TRGBImage; overload;

{ TODO: zrobic LoadImageGuess ktore zgaduje format na podstawie
  zawartosci. }

{ LoadImage is the ultimate procedure to load image from file.

  Two simple example use cases:
    Image := LoadImage('filename.png', [], []);
  (when you don't care what TImage descendant you get) or
    ImageRGB := LoadImage('filename.png', [TRGBImage], []) as TRGBImage;
  (when you insist to get TRGBImage, not e.g. TAlphaImage in case png
  image in file has some alpha channel).

  AllowedImageClasses says what image classes are allowed.
  As a special case, AllowedImageClasses = [] is equivalent to
  AllowedImageClasses = [TImage] which says that all TImage descendants
  are allowed. Then this function will do everything it can to load
  any image into the best subclass of TImage losing as little image
  information it can.

  Example: consider you're loading a PNG file. Let's suppose you're
  loading it with AllowedImageClasses = []. Then if PNG file will have
  alpha channel, LoadImage will return TAlphaImage descendant.
  Else LoadImage will return TRGBImage descendant.
  Now let's suppose you specified AllowedImageClasses = [TRGBImage].
  If PNG file will not have alpha channel,
  LoadImage will return TRGBImage descendant, as before.
  But if PNG fill *will* have alpha channel then
  1. if ForbiddenConvs does not contain [ilcAlphaDelete],
     LoadImage will simply ignore alpha channel and return you TRGBImage
  2. if ForbiddenConvs does contain [ilcAlphaDelete],
     LoadImage will exit with exception EUnableToLoadImage.
     This is somewhat safer, since you can't accidentaly ignore alpha
     channel that was present in file.

  All images have somehow specified RGB colors. Some of them may have alpha
  channel, some of them may have float precision (for now, this is only
  for RGBE images). This means that we may have to drop (ignore)
  these two things while loading image. So you can firbid ignoring them
  by adding to ForbiddenConvs ilcAlphaDelete and/or ilcFloatPrecDelete values.

  There can also happen reverse situation: you e.g. insist that
  AllowedImageClasses = [TAlphaImage] but given PNG image does not
  have alpha channel. In this case LoadImage may add "dummy" alpha channel
  (everywhere equal to 1.0 or High(Byte)).
  Similar thing when you e.g. gave AllowedImageClasses = [TRGBEImage]
  but you're loading from PNG image. In this case you want float precision,
  but image file cannot offer it. So LoadImage can simply convert
  discreet values to appropriating floating point values.
  This is usually harmless, but sometimes it may be unwanted, since
  you're getting something in different format than was in file.
  So you can add to ForbiddenConvs ilcAlphaAdd and/or ilcFloatPrecAdd
  to prevent that.

  If at any point LoadImage will find that it's unable to satisfy
  AllowedImageClasses without doing any forbidden convertions
  in ForbiddenConvs, it will raise @link(EUnableToLoadImage).

  Again some example: specify AllowedImageClasses = [TAlphaImage]
  and ForbiddenConvs = [ilcAlphaAdd] to be sure that
  LoadImage will return TAlphaImage with alpha channel loaded from file.
  If file wlil not have alpha channel, @link(EUnableToLoadImage) will
  be raised.

  Pl:
  Jedna nie-1-znaczna dotad sytuacja : jezeli mamy obrazek RGB (tzn. bez alpha,
  bez precyzji float) a musimy dodac alpha _lub_ precyzje
  (to znaczy AllowedImageKinds = [ikAlpha, ikRGBE]) i
  mozemy dodac obie (tzn. nie ma ilcAlphaAdd ani ilcFloatPrecAdd w ForbiddenConvs)
  to dodamy alpha. (tak czy siak, kombinacja [ikAlpha, ikRGBE] jest malo
  sensowna, w kazdym realnym zastosowaniu jesli umiesz obsluzyc i
  ikAlpha i ikRGBE to chyba ikRGB tez umiesz;
  --- jesli kiedys skorzystam z tej zaleznosci to skresle ta linie
      i dodam tutaj komentarz gdzie to sie moze przydac ------) }
type
  TImageLoadConversion = (ilcAlphaDelete, ilcFloatPrecDelete,
    ilcAlphaAdd, ilcFloatPrecAdd, ilcRGBFlattenToGrayscale);
  TImageLoadConversions = set of TImageLoadConversion;
const
  AllImageLoadConversions: TImageLoadConversions =
  [Low(TImageLoadConversion) .. High(TImageLoadConversion)];
function LoadImage(Stream: TStream; const StreamFormat: TImageFormat;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions)
  :TImage; overload;
function LoadImage(Stream: TStream; const typeext: string;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions)
  :TImage; overload;
function LoadImage(const filename: string;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions)
  :TImage; overload;
function LoadImage(const filename: string;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions;
  const ResizeToX, ResizeToY: Cardinal): TImage; overload;

{ ------------------------------------------------------------------------------ }

type
  EUnableToSaveImage = class(Exception);

{ SaveImage ktore pobiera jako parametr TImage. W zaleznosci od klasy Img :

  TRGBImage - wybiera odpowiednie SaveXxx na podstawie Format,
  TypeExt lub FileName (jezeli podales FileName to juz nie podajesz
  Stream, naturalnie). Jesli rozszerzenie nie odpowiada niczemu co umiemy
  zapisywac - sejwuje do formatu DefaultSaveImageFormat.

  TRGBEImage - jezeli image format (zapisany implicite w FileName lub TypeExt)
  = ifRGBE to zapisze obrazek uzywajac SaveRGBE(Img, fname),
  a wiec precyzja zawarta w wewnetrznym formacie ikRGBE bedzie zapisana
  w formacie pliku ifRGBE.

  Jezeli image format <> ifRGBE to skonwertuje obrazek do RGB
  (uzywajac TRGBEImage.ToRGBImage) i zapisze uzywajac SaveImage(TRGBImage,..).
  Wiec precyzja zawarta w wewnetrznym formacie ikRGBE nie bedzie
  zapisana w pliku, bo formaty inne niz ifRGBE nie pozwalaja na to.

  TODO: zrobic jakas ladniejsza forme, uwzgledniajaca fakt ze byc
  moze kiedys zrobie jeszcze jakis format pliku (if) / lub obrazka
  w pamieci (ik) ktore uznawalbym za precyzyjne na poziomie float.

  TAlphaImage: Format must support saving alpha images then
  (for now this means only ifPNG),
  zapisze wtedy obrazek z alpha. Wpp. rzuci wyjatek Exception.
  TODO: do it nicer, z podobnymi parametrami jak przy LoadImage.  }
procedure SaveImage(const img: TImage; const Format: TImageFormat; Stream: TStream); overload;
procedure SaveImage(const img: TImage; const typeext: string; Stream: TStream); overload;
procedure SaveImage(const Img: TImage; const fname: string); overload;

{ inne przetwarzanie obrazkow TImage ------------------------------------- }

{ dodawanie alpha do ImageRec }
{ ImageAlphaConst :
  - najpierw, jezeli obrazek jest pozbawiony alpha to je dodaj.
  - potem ustaw wszedzie alpha = alphaConst }
procedure ImageAlphaConstTo1st(var img: TImage; AlphaConst: byte);

{ Na podstawie filename zgaduje format pliku ktory mozemy zapisac do
  takiego filename (czyli robi FileExtToImageFormatDef(
  ExtractFileExt(ImageFilename), false, true, DefaultSaveImageFormat))
  (wersja z ImgFormat juz ma to w ImgFormat).

  Potem wybiera TImageKind ktory ma najwiekszy sens dla zadanego
  formatu - tzn. taki TImageKind ze przy sejwowaniu obrazka o takim
  Kind do pliku o takiej nazwie (a wiec i takim formacie) nie bedzie
  robiona zadna strata, nie bedzie zadnego odrzucania kanalu alpha
  ani precyzji i zakresu floatow. W tym momencie oznacza to ze dla
    ifRGBE zwroci ikRGBE
    a dla wszystkich pozostalych ikRGB.
  W tej chwili zadne format nie ma dac kind z alpha - bede tu musial
  kiedys dodac jakis arg aby pozwolic tej funkcji na zwracanie kanalu
  alpha np. dla png. }
function ImageClassBestForSavingToFormat(ImgFormat: TImageFormat): TImageClass; overload;
function ImageClassBestForSavingToFormat(const FileName: string): TImageClass; overload;

var
  { File filters if you want to choose a file that can be loaded/saved
    by appropriate functions from Images unit.

    These objects should be treated as read-only outside this unit.
    Initialization / finalization of this unit automatically take care of them.

    @groupBegin }
  LoadRGBImage_FileFilters: TFileFiltersList;
  LoadImage_FileFilters: TFileFiltersList;
  SaveImage_FileFilters: TFileFiltersList;
  { @groupEnd }

implementation

uses ProgressUnit, KambiClassUtils, KambiStringUtils, KambiFilesUtils,
  DataErrors;

{ file format specific functions : }
{$I images_bmp.inc}
{$I images_png.inc}
{$I images_jpeg.inc}
{$I images_pcx.inc}
{$I images_ppm.inc}
{$I images_ipl.inc}
{$I images_rgbe_fileformat.inc}
{$I images_external_tool.inc}

{ Colors ------------------------------------------------------------------ }

function EqualRGB(const Color1, Color2: TVector3Byte; Tolerance: Byte): boolean;
begin
 result:=(Abs(Smallint(Color1[0])-Color2[0]) <= tolerance) and
         (Abs(Smallint(Color1[1])-Color2[1]) <= tolerance) and
         (Abs(Smallint(Color1[2])-Color2[2]) <= tolerance);
end;

{ TImage ------------------------------------------------------------ }

constructor TImage.Create;
begin
 inherited;
 { Everything is already inited to nil and 0. }
end;

constructor TImage.Create(AWidth, AHeight: Cardinal);
begin
 Create;
 SetSize(AWidth, AHeight);
end;

destructor TImage.Destroy;
begin
 FreeMemNiling(FRawPixels);
 inherited;
end;

function TImage.IsNull: boolean;
begin
 Result := RawPixels = nil;
end;

procedure TImage.Null;
begin
 FreeMemNiling(FRawPixels);
 FWidth := 0;
 FHeight := 0;
end;

procedure TImage.SetSize(AWidth, AHeight: Cardinal);
begin
 FreeMemNiling(FRawPixels);
 FWidth := AWidth;
 FHeight := AHeight;
 if (AWidth <> 0) and (AHeight <> 0) then
  FRawPixels := GetMem(PixelSize * AWidth * AHeight);
end;

function TImage.PixelPtr(X, Y: Cardinal): Pointer;
begin
 Result := PointerAdd(RawPixels, PixelSize * (Width * Y + X));
end;

function TImage.RowPtr(Y: Cardinal): Pointer;
begin
 Result := PointerAdd(RawPixels, PixelSize * (Width * Y));
end;

procedure TImage.NotImplemented(const AMethodName: string);
begin
 raise EInternalError.Create(AMethodName +
   ' method not implemented for this TImage descendant');
end;

procedure TImage.InvertRGBColors;
begin
 NotImplemented('InvertRGBColors');
end;

procedure TImage.SetColorRGB(const x, y: Integer; const v: TVector3Single);
begin
 NotImplemented('SetColorRGB');
end;

function TImage.MakeCopy: TImage;
begin
 Result := TImageClass(Self.ClassType).Create(Width, Height);
 Move(RawPixels^, Result.RawPixels^, Width * Height * PixelSize);
end;

{ This does the real resizing work.
  It assumes that SourceData and DestinData pointers are already allocated.
  DestinWidth, DestinHeight must not be 0. }
procedure InternalResize(PixelSize: Cardinal;
  SourceData: Pointer; SourceWidth, SourceHeight: Cardinal;
  DestinData: Pointer; DestinWidth, DestinHeight: Cardinal;
  ProgressTitle: string);

var DestinY: Cardinal;

  procedure MakeLineDestinY;
  { write row DestinY of DestinData }
  var DestinX, SourceX, SourceY: Cardinal;
      SourceRow, DestinRow: PtrUInt;
  begin
   SourceY := DestinY * SourceHeight div DestinHeight;
   SourceRow := PtrUInt(SourceData) + SourceWidth * SourceY * PixelSize;
   DestinRow := PtrUInt(DestinData) + DestinWidth * DestinY * PixelSize;

   for DestinX := 0 to DestinWidth - 1 do
   begin
    SourceX := DestinX * SourceWidth div DestinWidth;
    Move(Pointer(SourceRow + SourceX * PixelSize)^,
         Pointer(DestinRow + DestinX * PixelSize)^,
         PixelSize);
   end;
  end;

begin
 if ProgressTitle = '' then
 begin
  for DestinY := 0 to DestinHeight - 1 do MakeLineDestinY;
 end else
 begin
  Progress.Init(DestinHeight, ProgressTitle);
  try
   for DestinY := 0 to DestinHeight - 1 do
    begin MakeLineDestinY; Progress.Step end;
  finally
   Progress.Fini;
  end;
 end;
end;

procedure TImage.Resize(ResizeToX, ResizeToY: Cardinal;
  const ProgressTitle: string);
var NewPixels: Pointer;
begin
 if ((ResizeToX <> 0) and (ResizeToX <> Width)) or
    ((ResizeToY <> 0) and (ResizeToY <> Height)) then
 begin
  { Make both ResizeTo* non-zero. }
  if ResizeToX = 0 then ResizeToX := Width;
  if ResizeToY = 0 then ResizeToY := Height;

  NewPixels := GetMem(ResizeToX * ResizeToY * PixelSize);
  InternalResize(PixelSize, RawPixels, Width, Height,
    NewPixels, ResizeToX, ResizeToY, ProgressTitle);
  FreeMemNiling(FRawPixels);

  FRawPixels := NewPixels;
  FWidth := ResizeToX;
  FHeight := ResizeToY;
 end;
end;

function TImage.MakeResized(ResizeToX, ResizeToY: Cardinal;
  const ProgressTitle: string): TImage;
begin
 { Make both ResizeTo* non-zero. }
 if ResizeToX = 0 then ResizeToX := Width;
 if ResizeToY = 0 then ResizeToY := Height;

 Result := TImageClass(ClassType).Create(ResizeToX, ResizeToY);
 try
  if not IsNull then
   InternalResize(PixelSize,
            RawPixels,        Width,        Height,
     Result.RawPixels, Result.Width, Result.Height,
     ProgressTitle);
 except Result.Free; raise end;
end;

procedure TImage.FlipHorizontal;
var ImageRow, TmpPixel, Pix1, Pix2: Pointer;
    x, y: Integer;
begin
 TmpPixel := GetMem(PixelSize);
 try
  for y := 0 to Height-1 do
  begin
   ImageRow := RowPtr(y);
   for x := 0 to (Width-1) div 2 do
   begin
    Pix1 := PointerAdd(ImageRow, Cardinal(x) * PixelSize);
    Pix2 := PointerAdd(ImageRow, (Width-1-Cardinal(x)) * PixelSize);
    Move(Pix1^, TmpPixel^, PixelSize);
    Move(Pix2^, Pix1^, PixelSize);
    Move(TmpPixel^, Pix2^, PixelSize);
   end;
  end;
 finally FreeMem(TmpPixel) end;
end;

function TImage.MakeTiled(TileX, TileY: Cardinal): TImage;
var i, j: Cardinal;
begin
 Result := TImageClass(ClassType).Create(TileX * Width, TileY * Height);
 try
  { Correct but naive version:

  for i := 0 to result.Width-1 do
   for j := 0 to result.Height-1 do
    move(Image.PixelPtr(i mod Image.Width, j mod Image.Height)^,
         Result.PixelPtr( i, j)^,
         Result.PixelSize );

  This can be speeded up copying whole rows at once: }

  for i := 0 to TileX - 1 do
   for j := 0 to Result.Height - 1 do
    Move(PixelPtr(0, j mod Height)^,
         Result.PixelPtr(i * Width, j)^,
         PixelSize * Width );
 except Result.Free; raise end;
end;

function TImage.MakeExtracted(X0, Y0, ExtractWidth, ExtractHeight: Cardinal): TImage;
var y: Cardinal;
begin
 if x0 + ExtractWidth > Width then
  raise EImagePosOutOfRange.Create('x0 in MakeExtracted out of range');
 if y0 + ExtractHeight > Height then
  raise EImagePosOutOfRange.Create('y0 in MakeExtracted out of range');

 Result := TImageClass(ClassType).Create(ExtractWidth, ExtractHeight);
 try
  for y := 0 to ExtractHeight - 1 do
   Move(PixelPtr(x0, y + y0)^, Result.RowPtr(y)^, PixelSize * ExtractWidth);
 except Result.Free; raise end;
end;

procedure TImage.Clear(const Pixel: TVector4Byte);
begin
 NotImplemented('Clear');
end;

function TImage.IsClear(const Pixel: TVector4Byte): boolean;
begin
 NotImplemented('IsClear');
 { code will never get here (NotImplemented always raises an exception),
   and code "Result := false;" below is only to avoid compiler warning
   that function result is undefined. }
 Result := false;
end;

procedure TImage.TransformRGB(const Matrix: TMatrix3Single);
begin
 NotImplemented('TransformRGB');
end;

procedure TImage.ModulateRGB(const ColorModulator: TColorModulatorByteFunc);
begin
 NotImplemented('ModulateRGB');
end;


function TImage.MakeModulatedRGB(
  const ColorModulator: TColorModulatorByteFunc): TImage;
begin
 Result := MakeCopy;
 Result.ModulateRGB(ColorModulator);
end;

procedure TImage.Grayscale;
begin
 ModulateRGB({$ifdef FPC_OBJFPC} @ {$endif} ColorGrayscaleByte);
end;

{$ifdef FPC}

procedure TImage.ConvertToChannelRGB(Channel: Integer);
begin
 case Channel of
  0: ModulateRGB({$ifdef FPC_OBJFPC} @ {$endif} ColorRedConvertByte);
  1: ModulateRGB({$ifdef FPC_OBJFPC} @ {$endif} ColorGreenConvertByte);
  2: ModulateRGB({$ifdef FPC_OBJFPC} @ {$endif} ColorBlueConvertByte);
  else raise EInternalError.Create(
    'ConvertToChannelRGB: Channel must be 0, 1 or 2');
 end;
end;

procedure TImage.StripToChannelRGB(Channel: Integer);
begin
 case Channel of
  0: ModulateRGB({$ifdef FPC_OBJFPC} @ {$endif} ColorRedStripByte);
  1: ModulateRGB({$ifdef FPC_OBJFPC} @ {$endif} ColorGreenStripByte);
  2: ModulateRGB({$ifdef FPC_OBJFPC} @ {$endif} ColorBlueStripByte);
  else raise EInternalError.Create(
    'StripToChannelRGB: Channel must be 0, 1 or 2');
 end;
end;

{$endif FPC}

function TImage.IsEqual(Image: TImage): boolean;
begin
  Result :=
    (Image.ClassType = ClassType) and
    (Image.Width = Width) and
    (Image.Height = Height) and
    (CompareMem(Image.RawPixels, RawPixels, Width * Height * PixelSize));
end;

function TImage.ArePartsEqual(
  const SelfX0, SelfY0, SelfWidth, SelfHeight: Cardinal;
  Image: TImage;
  const ImageX0, ImageY0, ImageWidth, ImageHeight: Cardinal): boolean;
var
  Y: Integer;
  SelfPtr: Pointer;
  ImagePtr: Pointer;
  SelfRowByteWidth, ImageRowByteWidth, RowByteWidth: Cardinal;
begin
  Result :=
    (Image.ClassType = ClassType) and
    (SelfWidth = ImageWidth) and
    (SelfHeight = ImageHeight);
  if Result then
  begin
    SelfPtr := PixelPtr(SelfX0, SelfY0);
    ImagePtr := Image.PixelPtr(ImageX0, ImageY0);
    RowByteWidth := ImageWidth * PixelSize;
    SelfRowByteWidth := Self.Width * PixelSize;
    ImageRowByteWidth := Image.Width * Image.PixelSize;
    for Y := 0 to Integer(ImageHeight) - 1 do
    begin
      if not CompareMem(SelfPtr, ImagePtr, RowByteWidth) then
      begin
        Result := false;
        Exit;
      end;
      PtrUInt(SelfPtr) := PtrUInt(SelfPtr) + SelfRowByteWidth;
      PtrUInt(ImagePtr) := PtrUInt(ImagePtr) + ImageRowByteWidth;
    end;
  end;
end;

function TImage.ArePartsEqual(
  Image: TImage;
  const ImageX0, ImageY0, ImageWidth, ImageHeight: Cardinal): boolean;
begin
  Result := ArePartsEqual(
    0, 0, Width, Height,
    Image,
    ImageX0, ImageY0, ImageWidth, ImageHeight);
end;

function TImage.ArePartsEqual(
  const SelfX0, SelfY0, SelfWidth, SelfHeight: Cardinal;
  Image: TImage): boolean;
begin
  Result := ArePartsEqual(
    SelfX0, SelfY0, SelfWidth, SelfHeight,
    Image,
    0, 0, Image.Width, Image.Height);
end;

procedure TImage.CopyFrom(Image: TImage; const X0, Y0: Cardinal);
var
  Y: Integer;
  SelfPtr: Pointer;
  ImagePtr: Pointer;
  SelfRowByteWidth, ImageRowByteWidth: Cardinal;
begin
  if Image.ClassType <> ClassType then
    raise Exception.Create('Cannot copy pixels from one image to another:' +
      ' different image classes');

  SelfPtr := PixelPtr(X0, Y0);
  ImagePtr := Image.RawPixels;
  SelfRowByteWidth := Self.Width * PixelSize;
  ImageRowByteWidth := Image.Width * Image.PixelSize;
  for Y := 0 to Integer(Image.Height) - 1 do
  begin
    Move(ImagePtr^, SelfPtr^, ImageRowByteWidth);
    PtrUInt(SelfPtr) := PtrUInt(SelfPtr) + SelfRowByteWidth;
    PtrUInt(ImagePtr) := PtrUInt(ImagePtr) + ImageRowByteWidth;
  end;
end;

procedure TImage.CopyTo(Image: TImage; const X0, Y0: Cardinal);
begin
  Image.CopyFrom(Self, X0, Y0);
end;

function TImage.AlphaChannelType(
  const AlphaTolerance: Byte;
  const WrongPixelsTolerance: Single): TAlphaChannelType;
begin
  Result := atNone;
end;

procedure TImage.LerpSimpleCheckConditions(SecondImage: TImage);
begin
  if (Width <> SecondImage.Width) or
     (Height <> SecondImage.Height) then
    raise EImageLerpDifferentSizes.CreateFmt('Linear interpolation not possible, images have different sizes: first has %d x %d, second has %d x %d',
      [Width, Height, SecondImage.Width, SecondImage.Height]);

  if not (SecondImage is Self.ClassType) then
    raise EImageLerpInvalidClasses.CreateFmt('Linear interpolation between %s and %s class not possible',
      [ClassName, SecondImage.ClassName]);
end;

procedure TImage.LerpWith(const Value: Single; SecondImage: TImage);
begin
  raise EImageLerpInvalidClasses.Create('Linear interpolation (TImage.LerpWith) not possible with the base TImage class');
end;

{ TImageClass and arrays of TImageClasses ----------------------------- }

function InImageClasses(ImageClass: TImageClass;
  const ImageClasses: array of TImageClass): boolean;
var i: Integer;
begin
 for i := 0 to High(ImageClasses) do
  if ImageClass.InheritsFrom(ImageClasses[i]) then
  begin
   Result := true;
   Exit;
  end;
 Result := false;
end;

function InImageClasses(Image: TImage;
  const ImageClasses: array of TImageClass): boolean;
begin
 Result := InImageClasses(TImageClass(Image.ClassType), ImageClasses);
end;

function ImageClassesEqual(const Ar1, Ar2: array of TImageClass): boolean;
var i: Integer;
begin
 if High(Ar1) <> High(Ar2) then
 begin
  Result := false;
  Exit;
 end;

 for i := 0 to High(Ar1) do
  if Ar1[I] <> Ar2[I] then
  begin
   Result := false;
   Exit;
  end;

 Result := true;
end;

procedure ImageClassesAssign(var Variable: TDynArrayImageClasses;
  const NewValue: array of TImageClass);
var i: Integer;
begin
 SetLength(Variable, High(NewValue) + 1);
 for i := 0 to High(NewValue) do
  Variable[i] := NewValue[i];
end;

{ TRGBImage ------------------------------------------------------------ }

constructor TRGBImage.CreateCombined(const MapImage: TRGBImage;
  var ReplaceWhiteImage, ReplaceBlackImage: TRGBImage);
var Map, White, Black, Res: PVector3Byte;
    s: single;
    i: integer;
begin
 Create(MapImage.Width, MapImage.Height);

 ReplaceWhiteImage.Resize(MapImage.Width, MapImage.Height);
 ReplaceBlackImage.Resize(MapImage.Width, MapImage.Height);

 Map := MapImage.RGBPixels;
 White := ReplaceWhiteImage.RGBPixels;
 Black := ReplaceBlackImage.RGBPixels;
 Res := RGBPixels;

 for i := 1 to Width * Height do
 begin
  s := (Map^[0] + Map^[1] + Map^[2]) / 255 / 3;
  Res^[0] := Round(s * White^[0] + (1-s) * Black^[0]);
  Res^[1] := Round(s * White^[1] + (1-s) * Black^[1]);
  Res^[2] := Round(s * White^[2] + (1-s) * Black^[2]);
  Inc(Map);
  Inc(White);
  Inc(Black);
  Inc(Res);
 end;
end;

function TRGBImage.GetRGBPixels: PVector3Byte;
begin
 Result := PVector3Byte(RawPixels);
end;

class function TRGBImage.PixelSize: Cardinal;
begin
 Result := 3;
end;

class function TRGBImage.ColorComponentsCount: Cardinal;
begin
 Result := 3;
end;

function TRGBImage.PixelPtr(X, Y: Cardinal): PVector3Byte;
begin
 Result := PVector3Byte(inherited PixelPtr(X, Y));
end;

function TRGBImage.RowPtr(Y: Cardinal): PArray_Vector3Byte;
begin
 Result := PArray_Vector3Byte(inherited RowPtr(Y));
end;

procedure TRGBImage.InvertRGBColors;
var i: Cardinal;
    prgb: PVector3byte;
begin
 prgb := RGBPixels;
 for i := 1 to Width * Height do
 begin
  prgb^[0] := High(byte)-prgb^[0];
  prgb^[1] := High(byte)-prgb^[1];
  prgb^[2] := High(byte)-prgb^[2];
  Inc(prgb);
 end;
end;

procedure TRGBImage.SetColorRGB(const x, y: Integer; const v: TVector3Single);
begin
 PVector3Byte(PixelPtr(x, y))^ := Vector3Byte(v);
end;

procedure TRGBImage.Clear(const Pixel: TVector4Byte);
var P: PVector3Byte;
    i: Cardinal;
begin
 P := RGBPixels;
 for i := 1 to Width * Height do
 begin
  Move(Pixel, P^, SizeOf(TVector3Byte));
  Inc(P);
 end;
end;

function TRGBImage.IsClear(const Pixel: TVector4Byte): boolean;
var P: PVector3Byte;
    i: Cardinal;
begin
 P := RGBPixels;
 for i := 1 to Width * Height do
 begin
  if not CompareMem(@Pixel, P, SizeOf(TVector3Byte)) then
  begin
   Result := false;
   Exit;
  end;
  Inc(P);
 end;
 Result := true;
end;

procedure TRGBImage.TransformRGB(const Matrix: TMatrix3Single);
type PPixel = PVector3Byte;
{$I images_transformrgb_implement.inc}

procedure TRGBImage.ModulateRGB(const ColorModulator: TColorModulatorByteFunc);
type PPixel = PVector3Byte;
{$I images_modulatergb_implement.inc}

function TRGBImage.ToAlphaImage_AlphaDontCare: TAlphaImage;
var pi: PVector3Byte;
    pa: PVector4Byte;
    i: Cardinal;
begin
 Result := TAlphaImage.Create(Width, Height);
 pi := RGBPixels;
 pa := Result.AlphaPixels;
 for i := 1 to Width * Height do
 begin
  Move(pi^, pa^, SizeOf(TVector3Byte));
  {pa^[3] := <dont_care_about_this_value>}
  Inc(pi);
  Inc(pa);
 end;
end;

function TRGBImage.ToAlphaImage_AlphaConst(Alpha: byte): TAlphaImage;

{ Note: implementation of this *could* use ToAlphaImage_AlphaDontCare,
  but doesn't, to be faster. }

var pi: PVector3Byte;
    pa: PVector4Byte;
    i: Cardinal;
begin
 Result := TAlphaImage.Create(Width, Height);
 pi := RGBPixels;
 pa := Result.AlphaPixels;
 for i := 1 to Width * Height do
 begin
  Move(pi^, pa^, SizeOf(TVector3Byte));
  pa^[3] := Alpha;
  Inc(pi);
  Inc(pa);
 end;
end;

function TRGBImage.ToAlphaImage_AlphaDecide(
  const AlphaColor: TVector3Byte;
  Tolerance: byte; AlphaOnColor: byte; AlphaOnNoColor: byte): TAlphaImage;
begin
 Result := ToAlphaImage_AlphaDontCare;
 Result.AlphaDecide(AlphaColor, Tolerance, AlphaOnColor, AlphaOnNoColor);
end;

function TRGBImage.ToRGBEImage: TRGBEImage;
var pRGBE: PVector4Byte;
    pRGB: PVector3Byte;
    i: Cardinal;
begin
 result := TRGBEImage.Create(Width, Height);
 try
  pRGB := RGBPixels;
  pRGBE := Result.RGBEPixels;
  for i := 1 to Width * Height do
  begin
   pRGBE^:=Vector3ToRGBE( Vector3Single(pRGB^) );
   Inc(pRGB);
   Inc(pRGBE);
  end;
 except Result.Free; raise end;
end;

function TRGBImage.ToGrayscale: TGrayscaleImage;
var
  pRGB: PVector3Byte;
  pGrayscale: PByte;
  I: Cardinal;
begin
  Result := TGrayscaleImage.Create(Width, Height);
  try
    pRGB := RGBPixels;
    pGrayscale := Result.GrayscalePixels;
    for i := 1 to Width * Height do
    begin
      pGrayscale^ := GrayscaleValue(pRGB^);
      Inc(pRGB);
      Inc(pGrayscale);
    end;
  except Result.Free; raise end;
end;

procedure TRGBImage.HorizontalLine(const x1, x2, y: Integer;
  const Color: TVector3Byte);
var P: PVector3Byte;
    i: Integer;
begin
 P := PixelPtr(x1, y);
 for i := 0 to x2 - x1 do begin P^ := Color; Inc(P) end;
end;

procedure TRGBImage.VerticalLine(const x, y1, y2: Integer;
  const Color: TVector3Byte);
var P: PVector3Byte;
    i: Integer;
begin
 P := PixelPtr(x, y1);
 for i := 0 to y2 - y1 do
 begin
  P^ := Color;
  P := PointerAdd(P, SizeOf(TVector3Byte) * Width);
 end;
end;

procedure TRGBImage.LerpWith(const Value: Single; SecondImage: TImage);
var
  SelfPtr: PVector3Byte;
  SecondPtr: PVector3Byte;
  I: Cardinal;
begin
  LerpSimpleCheckConditions(SecondImage);

  SelfPtr := RGBPixels;
  SecondPtr := TRGBImage(SecondImage).RGBPixels;
  for I := 1 to Width * Height do
  begin
    SelfPtr^ := Lerp(Value, SelfPtr^, SecondPtr^);
    Inc(SelfPtr);
    Inc(SecondPtr);
  end;
end;

{ TAlphaImage ------------------------------------------------------------ }

function TAlphaImage.GetAlphaPixels: PVector4Byte;
begin
 Result := PVector4Byte(RawPixels);
end;

class function TAlphaImage.PixelSize: Cardinal;
begin
 Result := 4;
end;

class function TAlphaImage.ColorComponentsCount: Cardinal;
begin
 Result := 4;
end;

function TAlphaImage.PixelPtr(X, Y: Cardinal): PVector4Byte;
begin
 Result := PVector4Byte(inherited PixelPtr(X, Y));
end;

function TAlphaImage.RowPtr(Y: Cardinal): PArray_Vector4Byte;
begin
 Result := PArray_Vector4Byte(inherited RowPtr(Y));
end;

procedure TAlphaImage.InvertRGBColors;
var i: Cardinal;
    palpha: PVector4byte;
begin
 palpha := AlphaPixels;
 for i := 1 to Width * Height do
 begin
  palpha^[0] := High(byte)-palpha^[0];
  palpha^[1] := High(byte)-palpha^[1];
  palpha^[2] := High(byte)-palpha^[2];
  Inc(palpha);
 end;
end;

procedure TAlphaImage.SetColorRGB(const x, y: Integer; const v: TVector3Single);
begin
 PVector3Byte(PixelPtr(x, y))^ := Vector3Byte(v);
end;

procedure TAlphaImage.Clear(const Pixel: TVector4Byte);
begin
 FillDWord(RawPixels^, Width*Height, LongWord(Pixel));
end;

function TAlphaImage.IsClear(const Pixel: TVector4Byte): boolean;
begin
 Result := IsMemDWordFilled(RawPixels^, Width*Height, LongWord(Pixel));
end;

procedure TAlphaImage.TransformRGB(const Matrix: TMatrix3Single);
type PPixel = PVector4Byte;
{$I images_transformrgb_implement.inc}

procedure TAlphaImage.ModulateRGB(const ColorModulator: TColorModulatorByteFunc);
type PPixel = PVector4Byte;
{$I images_modulatergb_implement.inc}

procedure TAlphaImage.AlphaDecide(const AlphaColor: TVector3Byte;
  Tolerance: Byte; AlphaOnColor: Byte; AlphaOnNoColor: Byte);
var pa: PVector4Byte;
    i: Cardinal;
begin
 pa := AlphaPixels;
 for i := 1 to Width * Height do
 begin
  if EqualRGB(AlphaColor, PVector3Byte(pa)^, Tolerance) then
   pa^[3] := AlphaOnColor else
   pa^[3] := AlphaOnNoColor;
  Inc(pa);
 end;
end;

procedure TAlphaImage.Compose(RGB: TRGBImage; AGrayscale: TGrayscaleImage);
var
  PtrAlpha: PVector4Byte;
  PtrRGB: PVector3Byte;
  PtrGrayscale: PByte;
  I: Cardinal;
begin
  Check( (RGB.Width = AGrayscale.Width) and
         (RGB.Height = AGrayscale.Height),
    'For TAlphaImage.Compose, RGB and alpha images must have the same sizes');

  SetSize(RGB.Width, RGB.Height);

  PtrAlpha := AlphaPixels;
  PtrRGB := RGB.RGBPixels;
  PtrGrayscale := AGrayscale.GrayscalePixels;

  for I := 1 to Width * Height do
  begin
    System.Move(PtrRGB^, PtrAlpha^, SizeOf(TVector3Byte));
    PtrAlpha^[3] := PtrGrayscale^;

    Inc(PtrAlpha);
    Inc(PtrRGB);
    Inc(PtrGrayscale);
  end;
end;

function TAlphaImage.AlphaChannelType(
  const AlphaTolerance: Byte;
  const WrongPixelsTolerance: Single): TAlphaChannelType;
var
  PtrAlpha: PVector4Byte;
  I, WrongPixels, AllPixels: Cardinal;
begin
  WrongPixels := 0;
  AllPixels := Width * Height;

  PtrAlpha := AlphaPixels;

  if WrongPixelsTolerance = 0 then
  begin
    for I := 1 to AllPixels do
    begin
      if (PtrAlpha^[3] > AlphaTolerance) and
         (PtrAlpha^[3] < 255 - AlphaTolerance) then
        { Special case for WrongPixelsTolerance = exactly 0.
          Avoids the cases when float "WrongPixels / AllPixels"
          may be so small that it's equal to 0, which would
          cause some wrong pixels to "slip" even with
          WrongPixelsTolerance = 0. }
        Exit(atFullRange);
      Inc(PtrAlpha);
    end;
  end else
  begin
    for I := 1 to AllPixels do
    begin
      if (PtrAlpha^[3] > AlphaTolerance) and
         (PtrAlpha^[3] < 255 - AlphaTolerance) then
      begin
        Inc(WrongPixels);
        { From the speed point of view, is it sensible to test
          WrongPixelsTolerance at each WrongPixels increment?
          On one hand, we can Exit with false faster.
          On the other hand, we lose time for checking it many times,
          if WrongPixelsTolerance is larger.
          Well, sensible WrongPixelsTolerance are very small --- so I
          think this is Ok to check this every time. }
        if WrongPixels / AllPixels > WrongPixelsTolerance then
          Exit(atFullRange);
      end;
      Inc(PtrAlpha);
    end;
  end;

  Result := atSimpleYesNo;
end;

procedure TAlphaImage.LerpWith(const Value: Single; SecondImage: TImage);
var
  SelfPtr: PVector4Byte;
  SecondPtr: PVector4Byte;
  I: Cardinal;
begin
  LerpSimpleCheckConditions(SecondImage);

  SelfPtr := AlphaPixels;
  SecondPtr := TAlphaImage(SecondImage).AlphaPixels;
  for I := 1 to Width * Height do
  begin
    SelfPtr^ := Lerp(Value, SelfPtr^, SecondPtr^);
    Inc(SelfPtr);
    Inc(SecondPtr);
  end;
end;

function TAlphaImage.ToRGBImage: TRGBImage;
var
  SelfPtr: PVector4Byte;
  ResultPtr: PVector3Byte;
  I: Cardinal;
begin
  Result := TRGBImage.Create(Width, Height);
  SelfPtr := AlphaPixels;
  ResultPtr := Result.RGBPixels;
  for I := 1 to Width * Height do
  begin
    Move(SelfPtr^, ResultPtr^, SizeOf(TVector3Byte));
    Inc(SelfPtr);
    Inc(ResultPtr);
  end;
end;

{ TRGBEImage ------------------------------------------------------------ }

function TRGBEImage.GetRGBEPixels: PVector4Byte;
begin
 Result := PVector4Byte(RawPixels);
end;

class function TRGBEImage.PixelSize: Cardinal;
begin
 Result := 4;
end;

class function TRGBEImage.ColorComponentsCount: Cardinal;
begin
 Result := 3;
end;

function TRGBEImage.PixelPtr(X, Y: Cardinal): PVector4Byte;
begin
 Result := PVector4Byte(inherited PixelPtr(X, Y));
end;

function TRGBEImage.RowPtr(Y: Cardinal): PArray_Vector4Byte;
begin
 Result := PArray_Vector4Byte(inherited RowPtr(Y));
end;

procedure TRGBEImage.SetColorRGB(const x, y: Integer; const v: TVector3Single);
begin
 PVector4Byte(PixelPtr(x, y))^:=Vector3ToRGBE(v);
end;

procedure TRGBEImage.Clear(const Pixel: TVector4Byte);
begin
 FillDWord(RawPixels^, Width*Height, LongWord(Pixel));
end;

function TRGBEImage.IsClear(const Pixel: TVector4Byte): boolean;
begin
 Result := IsMemDWordFilled(RawPixels^, Width*Height, LongWord(Pixel));
end;

function TRGBEImage.ToRGBImage: TRGBImage;
var pRGBE: PVector4Byte;
    pRGB: PVector3Byte;
    i: Cardinal;
begin
 Result := TRGBImage.Create(Width, Height);
 try
  pRGB := Result.RGBPixels;
  pRGBE := RGBEPixels;
  for i := 1 to Width * Height do
  begin
   pRGB^:=Vector3Byte( VectorRGBETo3Single(pRGBE^) );
   Inc(pRGB);
   Inc(pRGBE);
  end;
 except Result.Free; raise end;
end;

{$ifdef FPC}
procedure TRGBEImage.ScaleColors(const Scale: Single);
var pRGBE: PVector4Byte;
    i: Cardinal;
begin
 pRGBE := RGBEPixels;
 for i := 1 to Width * Height do
 begin
  pRGBE^:=Vector3ToRGBE( VectorScale( VectorRGBETo3Single(pRGBE^), Scale) );
  Inc(pRGBE);
 end;
end;

procedure TRGBEImage.ExpColors(const Exp: Single);
var pRGBE: PVector4Byte;
    i: Cardinal;
begin
 pRGBE := RGBEPixels;
 for i := 1 to Width * Height do
 begin
  pRGBE^:=Vector3ToRGBE( VectorExpComponents( VectorRGBETo3Single(pRGBE^), Exp) );
  Inc(pRGBE);
 end;
end;
{$endif}

procedure TRGBEImage.LerpWith(const Value: Single; SecondImage: TImage);
var
  SelfPtr: PVector4Byte;
  SecondPtr: PVector4Byte;
  I: Cardinal;
begin
  LerpSimpleCheckConditions(SecondImage);

  SelfPtr := RGBEPixels;
  SecondPtr := TRGBEImage(SecondImage).RGBEPixels;
  for I := 1 to Width * Height do
  begin
    SelfPtr^ := Vector3ToRGBE(Lerp(Value,
      VectorRGBETo3Single(SelfPtr^),
      VectorRGBETo3Single(SecondPtr^)));
    Inc(SelfPtr);
    Inc(SecondPtr);
  end;
end;

{ TGrayscaleImage ------------------------------------------------------------ }

function TGrayscaleImage.GetGrayscalePixels: PByte;
begin
  Result := PByte(RawPixels);
end;

class function TGrayscaleImage.PixelSize: Cardinal;
begin
  Result := 1;
end;

class function TGrayscaleImage.ColorComponentsCount: Cardinal;
begin
  Result := 1;
end;

function TGrayscaleImage.PixelPtr(X, Y: Cardinal): PByte;
begin
  Result := PByte(inherited PixelPtr(X, Y));
end;

function TGrayscaleImage.RowPtr(Y: Cardinal): PByteArray;
begin
  Result := PByteArray(inherited RowPtr(Y));
end;

procedure TGrayscaleImage.Clear(const Pixel: Byte);
begin
  FillChar(RawPixels^, Width * Height, Pixel);
end;

function TGrayscaleImage.IsClear(const Pixel: Byte): boolean;
begin
  Result := IsMemCharFilled(RawPixels^, Width * Height, Char(Pixel));
end;

procedure TGrayscaleImage.HalfColors;
var
  P: PByte;
  I: Cardinal;
begin
  P := GrayscalePixels;
  for I := 1 to Width * Height do
  begin
    P^ := P^ shr 1;
    Inc(P);
  end;
end;

procedure TGrayscaleImage.LerpWith(const Value: Single; SecondImage: TImage);
var
  SelfPtr: PByte;
  SecondPtr: PByte;
  I: Cardinal;
begin
  LerpSimpleCheckConditions(SecondImage);

  SelfPtr := GrayscalePixels;
  SecondPtr := TGrayscaleImage(SecondImage).GrayscalePixels;
  for I := 1 to Width * Height do
  begin
    SelfPtr^ := Clamped(Round(Lerp(Value, SelfPtr^, SecondPtr^)), 0, High(Byte));
    Inc(SelfPtr);
    Inc(SecondPtr);
  end;
end;

{ RGBE <-> 3 Single color convertion --------------------------------- }

const
  { do signed Exponent dodaj RGBEExponentOffset zeby zapisac exponent jako Byte }
  RGBEExponentOffset = 128;
  { RGBEMin/MaxExponent = min i max wartosci dla exponent ktore moga dac
    (Exponent + RGBEExponentOffset) w zakresie Byte.
    Czyli RGBEMinExponent + RGBEExponentOffset = 0,
          RGBEMaxExponent + RGBEExponentOffset = High(Byte),
    stad  RGBEMinExponent = -RGBEExponentOffset,
          RGBEMaxExponent = High(Byte) - RGBEExponentOffset }
  RGBEMinExponent = -RGBEExponentOffset;
  RGBEMaxExponent = High(Byte) - RGBEExponentOffset;

  { zero musi byc reprezentowane w specjalny sposob w formacie RGBE,
    podobnie jak w kazdym formacie zmiennoprzec. }
  RGBEZero: TVector4Byte=(0, 0, 0, 0);

  RGBELow :TVector4Byte=(0, 0, 0, 0); { = RGBEZero }
  RGBEHigh: TVector4Byte=(High(Byte), High(Byte), High(Byte), High(Byte));

function Vector3ToRGBE(const v: TVector3Single): TVector4Byte;
{ implementacja : jak Graphic Gems II.5 ale z poprawkami -
  - nazwy MaxVal i V sa osobne (dla czytelnosci),
  - checki czy Exponent jest w granicach RGBEMin/MaxExponent }
{ uwagi : moznaby sadzic ze Mnoznik powinien byc liczony jako
    Mantissa * 255 / MaxVal (255 = High(Byte) zamiast 256),
    zeby poprawnie mapowac zakres 0..1 na zakres bajta.
    Ale,
    - po pierwsze, specyfikacja formatu RGBE (czyli Graphic Gems II.5)
      mowi zeby uzywac 256
    - po drugie, uzywanie 256 podaje nam prosty warunek na sprawdzenie
      czy czworka bajtow jest poprawnym RGBE : mianowicie, przynajmniej
      jeden z pierwszych trzech bajtow musi byc >= 128
      (czyli musi miec najstarszy bit = 1). Tym samym ten bajt jest >= 0.5
      a wiec jest poprawna mantysa. Ten prosty test na poprawnosc ma zastosowanie
      przy kodowaniu plikow rgbe przy uzyciu prostego RLE, gdzie wykorzystujemy
      takie niepoprawne czworki RGBE to kodowania specjalnych informacji.
    - po trzecie i chyba najwazniejsze, gdyby uzywac 256 to wartosc
      mantysy = 255 byla bezuzyteczna bo odpowiadalaby wartosci float = 1.0
      a mantysa zawsze musi byc ostro mniejsza od 1, z definicji.
      I to jest chyba koronny argument za mnozeniem tutaj przez 256.
}
var MaxVal, Mnoznik: Single;
    Mantissa: Extended;
    Exponent: Integer;
begin
 MaxVal := KambiUtils.max(v[0], KambiUtils.max(v[1], v[2]));

 { rozpatrujemy tu nie tylko przypadek gdy liczba jest = 0 ale takze
   gdy jest bliska zeru. To jest standardowe zachowanie, ale uwaga -
   - w tym przypadku mogloby sie (blednie) wydawac ze mozemy tutaj zrobic
   wyjatek i sprawdzac ponizej tylko MaxVal = 0.0 (dokladna rownosc)
   a sprawdzanie bliskosci do zera zrzucic na test Exponent < RGBEMinExponent
   ponizej. ALE to nie jest prawda - test Exponent < RGBEMinExponent przejdzie
   dopiero dla niesamowicie mikroskopijnych liczb (< 1 / 2^127) podczas gdy liczby
   pomiedzy tymi "mikroskopijnie malymi" a SINGLE_EQUALITY_EPSILON ciagle
   beda powodowac problemy (bo przy liczeniu Mnoznik dzielimy przez MaxVal
   wiec male MaxVal -> Float overflow). }
 if MaxVal < SingleEqualityEpsilon then begin result := RGBEZero; Exit end;

 Frexp(MaxVal, Mantissa, Exponent);

 if Exponent < RGBEMinExponent then begin result := RGBELow; Exit end;
 if Exponent > RGBEMaxExponent then begin result := RGBEHigh; Exit end;

 Mnoznik := Mantissa * 256 / MaxVal;

 { MaxVal * Mnoznik daje Mantissa * High(byte) a wiec cos w zakresie
   0 .. High(Byte) bo Mantissa <= 1 (de facto, Mantissa >= 0.5 wiec
   mozna podac dokladniejsze ograniczenie na Mantissa * High(byte)).
   Wszystkie pozostale v[] sa mniejsze od MaxVal wiec one tez dadza cos
   w zakresie bajta. }
 result[0] := Clamped(Round(v[0]*Mnoznik), 0, High(Byte));
 result[1] := Clamped(Round(v[1]*Mnoznik), 0, High(Byte));
 result[2] := Clamped(Round(v[2]*Mnoznik), 0, High(Byte));

 { sprawdzajac czy Exponent in RGBEMin/MaxExponent wczesniej juz zapewnilem
   sobie ze ponizsze przypisanie jest Ok, wynik zmiesci sie w zakresie bajta. }
 result[3] := Exponent + RGBEExponentOffset;
end;

function VectorRGBETo3Single(const v: TVector4Byte): TVector3Single;
{ implementacja : jak Graphic Gems II.5.

  Mnoznik wychodzi od 1/256 (a nie 1/255), nalezaloby tu wiec poczynic
  podobne uwagi co przy konwersji w druga strone, Vector3ToRGBE.
  Patrz tamtejszy komentarz. }
var Mnoznik: Single;
begin
 if v[3] = 0 then begin result := ZeroVector3Single; Exit end;

 Mnoznik := Ldexp(1/256, Integer(v[3])-RGBEExponentOffset);
 result[0] := v[0]*Mnoznik;
 result[1] := v[1]*Mnoznik;
 result[2] := v[2]*Mnoznik;
end;

{ file formats managing ---------------------------------------------------------------- }

function FileExtToImageFormat(fileext: string;
  OnlyLoadable, OnlySaveable: boolean; out ImgFormat: TImageFormat): boolean;
var iff: TImageFormat;
    i: integer;
begin
 if SCharIs(fileext, 1, '.') then Delete(fileext, 1, 1);
 fileext := AnsiLowerCase(fileext);
 for iff := Low(iff) to High(iff) do
 begin
  if ((not OnlyLoadable) or Assigned(ImageFormatInfos[iff].LoadRGB)) and
     ((not OnlySaveable) or Assigned(ImageFormatInfos[iff].SaveRGB)) then
  for i := 1 to ImageFormatInfos[iff].extsCount do
   if fileext = ImageFormatInfos[iff].exts[i] then
   begin
    ImgFormat := iff;
    result := true;
    exit;
   end;
 end;
 result := false;
end;

function FileExtToImageFormatDef(const fileext: string;
  OnlyLoadable, OnlySaveable: boolean; DefFormat: TImageFormat): TImageFormat;
begin
 if not FileExtToImageFormat(fileext, OnlyLoadable, OnlySaveable, result) then
  result := DefFormat;
end;

function IsFileExtToImageFormat(const fileext: string; OnlyLoadable, OnlySaveable: boolean): boolean;
var dummy: TImageFormat;
begin
 result := FileExtToImageFormat(fileext, OnlyLoadable, OnlySaveable, dummy);
end;

function IsFileExtLoadableImage(const fileext: string): boolean;
begin
 result := IsFileExtToImageFormat(fileext, true, false);
end;

function TryFindExistingImageExt(const fname: string; OnlyLoadable: boolean): string;
var iff: TImageFormat;
    i: integer;
begin
 for iff := Low(iff) to High(iff) do
  if (not OnlyLoadable) or Assigned(ImageFormatInfos[iff].LoadRGB) then
  begin
   for i := 1 to ImageFormatInfos[iff].extsCount do
   begin
    result := fname +'.' +ImageFormatInfos[iff].exts[i];
    if NormalFileExists(result) then exit;
   end;
  end;
 result := '';
end;

function FindExistingImageExt(const fname: string; OnlyLoadable: boolean): string;
begin
 result := TryFindExistingImageExt(fname, OnlyLoadable);
 if result = '' then
  raise ENoExistingImageExt.Create('No existing image extension found for image name '+fname);
end;

function ListImageExtsLong(OnlyLoadable, OnlySaveable: boolean; const LinePrefix: string): string;
var iff: TImageFormat;
    i: integer;
begin
 result := '';

 for iff := Low(iff) to High(iff) do
  if ((not OnlyLoadable) or Assigned(ImageFormatInfos[iff].LoadRGB)) and
     ((not OnlySaveable) or Assigned(ImageFormatInfos[iff].SaveRGB)) then
  begin
   { zwrocmy uwage ze nie chcemy doklejac nl na koncu (bo zalatwieniu
     sprawy z formatem iff) bo tam nie byloby zbyt wygodnie rozpoznawac
     czy jestesmy ostatnia linia czy nie (na skutek OnlySaveable/OnlyLoadable
     nie mozna tego rozpoznac prostym sprawdzeniem iff < High(iff) }
   if result <> '' then result := result + nl;

   result := result +LinePrefix +ImageFormatInfos[iff].exts[1];
   for i := 2 to ImageFormatInfos[iff].extsCount do
    result := result + ', ' +ImageFormatInfos[iff].exts[i];
   result := result + ' - '+ImageFormatInfos[iff].formatName;
  end;
end;

function ListImageExtsShort(OnlyLoadable, OnlySaveable: boolean): string;
var iff: TImageFormat;
    i: integer;
begin
 result := '';

 for iff := Low(iff) to High(iff) do
  if ((not OnlyLoadable) or Assigned(ImageFormatInfos[iff].LoadRGB)) and
     ((not OnlySaveable) or Assigned(ImageFormatInfos[iff].SaveRGB)) then
  begin
   for i := 1 to ImageFormatInfos[iff].extsCount do
   begin
    if result <> '' then result := result + ', ';
    result := result + ImageFormatInfos[iff].exts[i];
   end;
  end;
end;

{ LoadRGBImage ----------------------------------------------------------------- }

function LoadRGBImage(Stream: TStream; const typeext: string): TRGBImage;
var
  iff: TImageFormat;
begin
  if FileExtToImageFormat(typeext, true, false, iff) then
    result := ImageFormatInfos[iff].LoadRGB(Stream) else
    raise EImageFormatNotSupported.Create(
      'Unrecognized image format : "' + typeext + '"');
end;

function LoadRGBImage(const fname: string): TRGBImage;
var f: TStream;
begin
 {$ifdef DELPHI} Result := nil; { <- only to avoid stupid warning } {$endif}

 f := CreateReadFileStream(fname);
      { tests: TFileStream.Create(fname, fmOpenRead) }
 try
  try
   Result := LoadRGBImage(f, ExtractFileExt(fname));
  except
   on E: EImageLoadError do begin
     E.Message := 'Error when loading image from file "'+fname+'" : '+E.Message;
     raise;
    end;
   on E: EImageFormatNotSupported do begin
     { przechwyc EImageFormatNotSupported i w tresci wyjatku wklej pelne fname }
     E.Message := 'Unrecognized image format : file "'+fname+'"';
     raise;
    end;
  end;
 finally f.Free end;
end;

function LoadRGBImage(const fname: string; ResizeToX, ResizeToY: Cardinal;
  UnscaledImageProc: TProcedureRGBImage): TRGBImage;
begin
 result := Images.LoadRGBImage(fname);
 try
  if Assigned(UnscaledImageProc) then UnscaledImageProc(result);
  Result.Resize(ResizeToX, ResizeToY);
 except Result.Free; raise end;
end;

{ LoadImage --------------------------------------------------------------- }

function LoadImage(Stream: TStream; const StreamFormat: TImageFormat;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions)
  :TImage;

  { On input, Image must be TRGBImage and on output it will be TGrayscaleImage. }
  procedure ImageGrayscaleTo1st(var Image: TImage);
  var
    NewImage: TGrayscaleImage;
  begin
    NewImage := (Image as TRGBImage).ToGrayscale;
    FreeAndNil(Image);
    Image := NewImage;
  end;

  procedure DoingConversion(Conv: TImageLoadConversion);
  { check is Conv Forbidden, if it is -> raise exception }
  const ConvToStr: array[TImageLoadConversion]of string=(
    'delete alpha channel',
    'lose float precision',
    'add dummy alpha channel',
    'add useless float precision',
    'flatten RGB colors to grayscale');
  begin
   if Conv in ForbiddenConvs then
    raise EUnableToLoadImage.Create('LoadImage: to load this image format '+
      'conversion "'+ConvToStr[Conv]+'" must be done, but it is forbidden here');
  end;

  procedure ImageRGBToRGBETo1st(var Image: TImage);
  var NewResult: TImage;
  begin
   NewResult := (Image as TRGBImage).ToRGBEImage;
   Image.Free;
   Image := NewResult;
  end;

  function ClassAllowed(ImageClass: TImageClass): boolean;
  begin
   Result := (High(AllowedImageClasses) = -1) or
     InImageClasses(ImageClass, AllowedImageClasses);
  end;

  procedure LoadAny(Load: TImageLoadFunc);
  begin
    if ClassAllowed(TAlphaImage) then
    begin
      if ClassAllowed(TRGBImage) then
        Result := Load(Stream, frAny, false) else
      if ClassAllowed(TRGBEImage) then
      begin
        { AllowedImageClasses have TRGBEImage and TAlphaImage but not TRGBImage.
          Jezeli dodawanie alpha channela jest dozwolone to nie ma problemu :
          zaladuj obrazek wymagajc alpha, ew. dodajac alpha.
          Wpp. zaladuj obrazek nie konwertujac nic i jesli wyjdzie nam
          ikRGB to zamien go na ikRGBE. }
        if not(ilcAlphaAdd in ForbiddenConvs) then
          Result := Load(Stream, frWithAlpha, true) else
        begin
          Result := Load(Stream, frAny, false);
          if Result is TRGBImage then
          begin
            DoingConversion(ilcFloatPrecAdd);
            ImageRGBToRGBETo1st(result);
          end;
        end;
      end else
        { w AllowedImageKinds jest tylko ikAlpha. Wiec musimy dac frWithAlpha }
        result := Load(Stream, frWithAlpha, not (ilcAlphaAdd in ForbiddenConvs));
    end else
    begin
      { bez wzgledu na wszystko,
        jesli nie ma ClassAllowed(TAlphaImage)
        to chcemy stracic alpha obrazka (jesli go ma), tzn. chcemy zaladowac
        teraz obrazek do RGB. }
      Result := Load(Stream, frWithoutAlpha, not (ilcAlphaDelete in ForbiddenConvs));

      Assert(Result is TRGBImage);

      if not (ClassAllowed(TRGBImage)) then
      begin
        if ClassAllowed(TRGBEImage) then
        begin
          DoingConversion(ilcFloatPrecAdd);
          ImageRGBToRGBETo1st(result);
        end else
        if ClassAllowed(TGrayscaleImage) then
        begin
          DoingConversion(ilcRGBFlattenToGrayscale);
          ImageGrayscaleTo1st(Result);
        end else
          raise EInternalError.Create('LoadImage: unknown target required');
      end;
    end;
  end;

const
  DummyDefaultAlpha = High(Byte);

  { ten kod zadziala gdy format mozna zaladowac tylko
    ladujac go poprzez ImageFormatInfos[StreamFormat].LoadRGB(Stream).
    Potem trzeba ew. dodac kanal alpha i precyzje float. }
  procedure LoadRGB(Load: TRGBImageLoadFunc);
  begin
    result := Load(Stream);

    Assert(Result is TRGBImage);

    if not (ClassAllowed(TRGBImage)) then
    begin
      if (ClassAllowed(TAlphaImage)) and not(ilcAlphaAdd in ForbiddenConvs) then
      begin
        ImageAlphaConstTo1st(result, DummyDefaultAlpha);
      end else
      if ClassAllowed(TGrayscaleImage) then
      begin
        DoingConversion(ilcRGBFlattenToGrayscale);
        ImageGrayscaleTo1st(Result);
      end else
      if ClassAllowed(TRGBEImage) then
      begin
        DoingConversion(ilcFloatPrecAdd);
        ImageRGBToRGBETo1st(result);
      end else
        { The only situation when this can happen (assuming no internal error,
          and at least one image class was allowed) was if
          ClassAllowed(TAlphaImage) and (ilcAlphaAdd in ForbiddenConvs). }
        raise EUnableToLoadImage.Create('LoadImage: unable to load image: '+
          'alpha channel requested but dummy alpha channel creation forbidden '+
          'but image has no alpha channel');
    end;
  end;

begin
  Result := nil;
  try
    if StreamFormat = ifRGBE then
    begin
      if ClassAllowed(TRGBEImage) then
        result := LoadRGBE(Stream, false) else
      begin
        DoingConversion(ilcFloatPrecDelete);
        result := LoadRGBE(Stream, true);

        if not ClassAllowed(TRGBImage) then
        begin
          if ClassAllowed(TAlphaImage) then
          begin
            DoingConversion(ilcAlphaAdd);
            ImageAlphaConstTo1st(result, DummyDefaultAlpha);
          end else
          if ClassAllowed(TGrayscaleImage) then
          begin
            DoingConversion(ilcRGBFlattenToGrayscale);
            ImageGrayscaleTo1st(Result);
          end else
            raise EInternalError.Create('LoadImage: RGBE format and unknown target required');
        end;
      end;
    end else
    if Assigned(ImageFormatInfos[StreamFormat].Load) then
      LoadAny(ImageFormatInfos[StreamFormat].Load) else
    if Assigned(ImageFormatInfos[StreamFormat].LoadRGB) then
      LoadRGB(ImageFormatInfos[StreamFormat].LoadRGB) else
    raise EImageFormatNotSupported.Create('Can''t load image format "'+
      ImageFormatInfos[StreamFormat].FormatName+'"');

  except Result.Free; raise end;
end;

function LoadImage(Stream: TStream; const typeext: string;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions)
  :TImage;
var iff: TImageFormat;
begin
 if FileExtToImageFormat(typeext, true, false, iff) then
  result := LoadImage(Stream, iff, AllowedImageClasses, ForbiddenConvs) else
  raise EImageFormatNotSupported.Create('Unrecognized image format : "'+typeext+'"');
end;

function LoadImage(const filename: string;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions)
  :TImage;
var f: TStream;
begin
 {$ifdef DELPHI} Result := nil; { <- only to avoid stupid warning } {$endif}

 f := CreateReadFileStream(filename);
 try
  try
   result := LoadImage(f, ExtractFileExt(filename), AllowedImageClasses,
     ForbiddenConvs);
  except
   on E: EImageLoadError do begin
     E.Message := 'Error when loading image from file "'+filename+'" : '+E.Message;
     raise;
    end;
   on E: EImageFormatNotSupported do begin
     { przechwyc EImageFormatNotSupported i w tresci wyjatku wklej pelne filename }
     E.Message := 'Unrecognized image format : file "'+filename+'"';
     raise;
    end;
   else raise;
  end;
 finally f.Free end;
end;

function LoadImage(const filename: string;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions;
  const ResizeToX, ResizeToY: Cardinal): TImage;
begin
 result := LoadImage(filename, AllowedImageClasses, ForbiddenConvs);
 {unused for now : if Assigned(UnscaledImageProc) then UnscaledImageProc(result);}
 Result.Resize(ResizeToX, ResizeToY);
end;

{ SaveImage na TImage ---------------------------------------------------- }

procedure SaveImage(const Img: TImage; const Format: TImageFormat; Stream: TStream);

  procedure SaveRGB(const img: TRGBImage; const Format: TImageFormat; Stream: TStream);
  begin
   if Assigned(ImageFormatInfos[Format].SaveRGB) then
    ImageFormatInfos[Format].SaveRGB(Img, Stream) else
    raise EUnableToSaveImage.Create('Can''t save image format "'+
      ImageFormatInfos[Format].FormatName +'"');
  end;

var ImgRGB: TRGBImage;
begin
 if Img is TRGBImage then
  SaveRGB(TRGBImage(Img), Format, Stream) else

 if Img is TRGBEImage then
 begin
  if Format = ifRGBE then
   SaveRGBE(Img, Stream) else
  begin
   ImgRGB := TRGBEImage(Img).ToRGBImage;
   try
    SaveImage(ImgRGB, Format, Stream);
   finally ImgRGB.Free end;
  end;
 end else

 if Img is TAlphaImage then
 begin
  if Format = ifPNG then
   SaveAnyPNG(Img, Stream, false) else
   raise Exception.Create('SaveImage can save images with alpha channel only to PNG.');
 end else

  raise EInternalError.Create('SaveImage: not implemented for this TImage descendant');
end;

procedure SaveImage(const img: TImage; const typeext: string; Stream: TStream);
begin
 SaveImage(Img, FileExtToImageFormatDef(
   typeext, false, true, DefaultSaveImageFormat), Stream);
end;

procedure SaveImage(const Img: TImage; const fname: string);
var f: TFileStream;
begin
 f := TFileStream.Create(fname, fmCreate);
 try
  SaveImage(img, ExtractFileExt(fname), f);
 finally f.Free end;
end;

{ inne przetwarzanie obrazkow TRGBImage ------------------------------------------- }

procedure ImageAlphaConstTo1st(var img: TImage; alphaConst: byte);
var newimg: TAlphaImage;
    pa: PVector4Byte;
    i: Cardinal;
begin
 if Img is TRGBImage then
 begin
  NewImg := TRGBImage(Img).ToAlphaImage_AlphaDontCare;
  FreeAndNil(Img);
  Img := NewImg;
 end else
 if not (Img is TAlphaImage) then
  raise EInternalError.Create(
    'ImageAlphaConstTo1st -- not implemented for this TImage descendant');

 pa := TAlphaImage(Img).AlphaPixels;
 for i := 1 to Img.Width * Img.Height do
 begin
  pa^[3] := AlphaConst;
  Inc(pa);
 end;
end;

function ImageClassBestForSavingToFormat(const FileName: string): TImageClass;
begin
 Result := ImageClassBestForSavingToFormat(
   FileExtToImageFormatDef(ExtractFileExt(Filename), false, true,
     DefaultSaveImageFormat));
end;

function ImageClassBestForSavingToFormat(ImgFormat: TImageFormat): TImageClass;
begin
 if ImgFormat = ifRGBE then Result := TRGBEImage else Result := TRGBImage;
end;

{ unit initialization / finalization ----------------------------------------- }

procedure InitializeImagesFileFilters;

  function CreateImagesFilters: TFileFiltersList;
  begin
    Result := TFileFiltersList.Create;
    Result.AddFilter('All Files', ['*']);
    Result.AddFilter('All Images', []);
    Result.DefaultFilter := 1;
  end;

  procedure AddImageFormat(Filters: TFileFiltersList; Format: TImageFormatInfo);
  var
    F: TFileFilter;
    ExtIndex: Integer;
    Pattern: string;
  begin
    F := TFileFilter.Create;
    Filters.Add(F);
    F.Name := Format.FormatName + ' (';

    for ExtIndex := 1 to Format.ExtsCount do
    begin
      Pattern := '*.' + Format.Exts[ExtIndex];

      { add to "All images" filter }
      Filters[Filters.DefaultFilter].Patterns.Append(Pattern);

      { add to this filter }
      F.Patterns.Append(Pattern);

      { add to this filter visible name }
      if ExtIndex <> 1 then F.Name := F.Name + ', ';
      F.Name := F.Name + Pattern;
    end;

    F.Name := F.Name + ')';
  end;

var
  Format: TImageFormat;
begin
  LoadRGBImage_FileFilters := CreateImagesFilters;
  LoadImage_FileFilters := CreateImagesFilters;
  SaveImage_FileFilters := CreateImagesFilters;

  for Format := Low(Format) to High(Format) do
  begin
    if Assigned(ImageFormatInfos[Format].LoadRGB) then
    begin
      AddImageFormat(LoadRGBImage_FileFilters, ImageFormatInfos[Format]);

      { For LoadImage, the allowed formats list is specified implicitly
        by LoadImage documentation. As it happens, for now every image
        can be loaded to RGBImage, and so every image can be loaded by
        LoadImage.

        So actually LoadImage_FileFilters is (for now) always equal
        to LoadRGBImage_FileFilters. }

      AddImageFormat(LoadImage_FileFilters, ImageFormatInfos[Format]);
    end;

    { For SaveImage, the allowed formats list is specified implicitly
      by SaveImage documentation.

      For now, this means that SaveImage can save anything that can be saved
      to RGB format and additionally directly handles RGBE and PNG image formats.
      As it happens, these last two formats can also be saved directly from
      RGB pixel format, so for now check below is simple. }

    if Assigned(ImageFormatInfos[Format].SaveRGB) then
      AddImageFormat(SaveImage_FileFilters, ImageFormatInfos[Format]);
  end;
end;

initialization
  InitializeImagesFileFilters;
finalization
  FreeWithContentsAndNil(LoadRGBImage_FileFilters);
  FreeWithContentsAndNil(LoadImage_FileFilters);
  FreeWithContentsAndNil(SaveImage_FileFilters);
end.

(* ----------------------------------------------------------------------------------
stare comments do LoadImage :

 { w result.data zwracaja bitmape w formacie GL_RGB na GL_UNSIGNED_BYTE, bez alignowania !
   Tzn. powinno byc PixelStorei(GL_UNPACK_ALIGNMENT, 1) aby dzialaly w kazdej sytuacji.
   LoadImageData zwraca tylko wskaznik result.data.
   Jesli resizeTo[] <> 0 to dany wymiar bedzie resizowany.
   UWAGA ! Przydzielony pointer data ZAWSZE nalezy zwolnic z pamieci przez FreeMem
     (polecam moje FreeMemNiling).
   ImageProc, jesli <> nil, jest wywolywane dla zaladowanego image'a PRZED wykonaniem
     ewentualnego skalowania. Ma to zastosowanie np. gdy chcesz zaladowac stosunkowo
     maly obrazek z pliku, zamienic go np. na czarno-bialy i potem przeskalowac na
     bardzo duzy rozmiar. W takiej sytuacji duzo bardziej ekonomiczne jest wywolanie
     konwersji na black&white jeszcze PRZED skalowaniem, a wiec najlepiej przekaz
     MakeBlackAndWhite jako ImageProc. Acha, jesli chcesz to mozesz w ImageProc
     zmienic rozmiary obrazka. (chociaz dla typowego resizu pewnie wygodniej bedzie
     uzyc parametrow resizeToX, resizeToY)
 }

*)
