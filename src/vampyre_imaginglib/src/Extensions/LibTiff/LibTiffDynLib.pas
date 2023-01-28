unit LibTiffDynLib;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

// We prefer dynamic loading of the library (GetProcAddress/dlsym way)
// so that we don't get a crash with "libtiff not found!" message on startup
// if libtiff is not found in user's system.
{$DEFINE DYNAMIC_DLL_LOADING}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils;

type
  va_list = Pointer;
{$IFNDEF FPC}
{$IF CompilerVersion <= 18.5}
  SizeInt = Integer;
{$ELSE}
  SizeInt = NativeInt;
{$IFEND}
{$ENDIF}

type
  tmsize_t = SizeInt;
  tsize_t = SizeInt;
  // typedef uint64 toff_t;          /* file offset */
  toff_t = Int64;
  poff_t = ^toff_t;
  tsample_t = Word;
  // Beware: THandle is 32bit in size even on 64bit Linux - this may cause
  // problems as pointers to client data are passed in thandle_t vars.
  thandle_t = THandle;
  tdata_t = Pointer;
  ttag_t = UInt32;
  tdir_t = Word;
  tstrip_t = UInt32;

const
  // LibTiff 4.0
{$IF Defined(MSWINDOWS)}
  SLibName = 'libtiff.dll'; // make sure you have DLL with the same bitness as your app!
{$ELSEIF Defined(DARWIN)} // macOS
  SLibName = 'libtiff.5.dylib';
{$ELSE} // Linux, BSD
  SLibName = 'libtiff.so.5'; // yes, SONAME for libtiff v4.0 is actually libtiff5 (and libtiff.so.4 is libtiff v3.9)
{$IFEND}

  TIFF_NOTYPE                           = 0;
  TIFF_BYTE                             = 1;       { 8-bit unsigned integer }
  TIFF_ASCII                            = 2;       { 8-bit bytes w/ last byte null }
  TIFF_SHORT                            = 3;       { 16-bit unsigned integer }
  TIFF_LONG                             = 4;       { 32-bit unsigned integer }
  TIFF_RATIONAL                         = 5;       { 64-bit unsigned fraction }
  TIFF_SBYTE                            = 6;       { !8-bit signed integer }
  TIFF_UNDEFINED                        = 7;       { !8-bit untyped data }
  TIFF_SSHORT                           = 8;       { !16-bit signed integer }
  TIFF_SLONG                            = 9;       { !32-bit signed integer }
  TIFF_SRATIONAL                        = 10;      { !64-bit signed fraction }
  TIFF_FLOAT                            = 11;      { !32-bit IEEE floating point }
  TIFF_DOUBLE                           = 12;      { !64-bit IEEE floating point }
  TIFF_IFD                              = 13;      { %32-bit unsigned integer (offset) }
  TIFF_UNICODE                          = 14;
  TIFF_COMPLEX                          = 15;
  TIFF_LONG8                            = 16;
  TIFF_SLONG8                           = 17;
  TIFF_IFD8                             = 18;

  TIFFTAG_SUBFILETYPE                   = 254;     { subfile data descriptor }
    FILETYPE_REDUCEDIMAGE               = $1;      { reduced resolution version }
    FILETYPE_PAGE                       = $2;      { one page of many }
    FILETYPE_MASK                       = $4;      { transparency mask }
  TIFFTAG_OSUBFILETYPE                  = 255;     { kind of data in subfile }
    OFILETYPE_IMAGE                     = 1;       { full resolution image data }
    OFILETYPE_REDUCEDIMAGE              = 2;       { reduced size image data }
    OFILETYPE_PAGE                      = 3;       { one page of many }
  TIFFTAG_IMAGEWIDTH                    = 256;     { image width in pixels }
  TIFFTAG_IMAGELENGTH                   = 257;     { image height in pixels }
  TIFFTAG_BITSPERSAMPLE                 = 258;     { bits per channel (sample) }
  TIFFTAG_COMPRESSION                   = 259;   { data compression technique }
    COMPRESSION_NONE                    = 1;     { dump mode }
    COMPRESSION_CCITTRLE                = 2;     { CCITT modified Huffman RLE }
    COMPRESSION_CCITTFAX3               = 3;     { CCITT Group 3 fax encoding }
    COMPRESSION_CCITT_T4                = 3;       { CCITT T.4 (TIFF 6 name) }
    COMPRESSION_CCITTFAX4	              = 4;	   { CCITT Group 4 fax encoding }
    COMPRESSION_CCITT_T6                = 4;       { CCITT T.6 (TIFF 6 name) }
    COMPRESSION_LZW		                  = 5;       { Lempel-Ziv  & Welch }
    COMPRESSION_OJPEG		                = 6;	   { !6.0 JPEG }
    COMPRESSION_JPEG		                = 7;	   { %JPEG DCT compression }
    COMPRESSION_NEXT		                = 32766;   { NeXT 2-bit RLE }
    COMPRESSION_CCITTRLEW	              = 32771;   { #1 w/ word alignment }
    COMPRESSION_PACKBITS	              = 32773;   { Macintosh RLE }
    COMPRESSION_THUNDERSCAN	            = 32809;   { ThunderScan RLE }
    { codes 32895-32898 are reserved for ANSI IT8 TIFF/IT <dkelly@apago.com) }
    COMPRESSION_IT8CTPAD	              = 32895;   { IT8 CT w/padding }
    COMPRESSION_IT8LW		                = 32896;   { IT8 Linework RLE }
    COMPRESSION_IT8MP		                = 32897;   { IT8 Monochrome picture }
    COMPRESSION_IT8BL		                = 32898;   { IT8 Binary line art }
    { compression codes 32908-32911 are reserved for Pixar }
    COMPRESSION_PIXARFILM	              = 32908;   { Pixar companded 10bit LZW }
    COMPRESSION_PIXARLOG	              = 32909;   { Pixar companded 11bit ZIP }
    COMPRESSION_DEFLATE		              = 32946;   { Deflate compression }
    COMPRESSION_ADOBE_DEFLATE           = 8;       { Deflate compression, as recognized by Adobe }
    { compression code 32947 is reserved for Oceana Matrix <dev@oceana.com> }
    COMPRESSION_DCS                     = 32947;   { Kodak DCS encoding }
    COMPRESSION_JBIG		                = 34661;   { ISO JBIG }
    COMPRESSION_SGILOG		              = 34676;   { SGI Log Luminance RLE }
    COMPRESSION_SGILOG24	              = 34677;   { SGI Log 24-bit packed }
    COMPRESSION_JP2000                  = 34712;   { Leadtools JPEG2000 }
  TIFFTAG_PHOTOMETRIC                   = 262;     { photometric interpretation }
    PHOTOMETRIC_MINISWHITE              = 0;       { min value is white }
    PHOTOMETRIC_MINISBLACK              = 1;       { min value is black }
    PHOTOMETRIC_RGB                     = 2;       { RGB color model }
    PHOTOMETRIC_PALETTE                 = 3;       { color map indexed }
    PHOTOMETRIC_MASK                    = 4;       { $holdout mask }
    PHOTOMETRIC_SEPARATED               = 5;       { !color separations }
    PHOTOMETRIC_YCBCR                   = 6;       { !CCIR 601 }
    PHOTOMETRIC_CIELAB                  = 8;       { !1976 CIE L*a*b* }
    PHOTOMETRIC_ICCLAB                  = 9;       { ICC L*a*b* [Adobe TIFF Technote 4] }
    PHOTOMETRIC_ITULAB                  = 10;      { ITU L*a*b* }
    PHOTOMETRIC_LOGL                    = 32844;   { CIE Log2(L) }
    PHOTOMETRIC_LOGLUV                  = 32845;   { CIE Log2(L) (u',v') }
  TIFFTAG_THRESHHOLDING                 = 263;     { thresholding used on data }
    THRESHHOLD_BILEVEL                  = 1;       { b&w art scan }
    THRESHHOLD_HALFTONE                 = 2;       { or dithered scan }
    THRESHHOLD_ERRORDIFFUSE             = 3;       { usually floyd-steinberg }
  TIFFTAG_CELLWIDTH                     = 264;     { +dithering matrix width }
  TIFFTAG_CELLLENGTH                    = 265;     { +dithering matrix height }
  TIFFTAG_FILLORDER                     = 266;     { data order within a byte }
    FILLORDER_MSB2LSB                   = 1;       { most significant -> least }
    FILLORDER_LSB2MSB                   = 2;       { least significant -> most }
  TIFFTAG_DOCUMENTNAME                  = 269;     { name of doc. image is from }
  TIFFTAG_IMAGEDESCRIPTION              = 270;     { info about image }
  TIFFTAG_MAKE                          = 271;     { scanner manufacturer name }
  TIFFTAG_MODEL                         = 272;     { scanner model name/number }
  TIFFTAG_STRIPOFFSETS                  = 273;     { offsets to data strips }
  TIFFTAG_ORIENTATION                   = 274;     { +image orientation }
    ORIENTATION_TOPLEFT                 = 1;       { row 0 top, col 0 lhs }
    ORIENTATION_TOPRIGHT                = 2;       { row 0 top, col 0 rhs }
    ORIENTATION_BOTRIGHT                = 3;       { row 0 bottom, col 0 rhs }
    ORIENTATION_BOTLEFT                 = 4;       { row 0 bottom, col 0 lhs }
    ORIENTATION_LEFTTOP                 = 5;       { row 0 lhs, col 0 top }
    ORIENTATION_RIGHTTOP                = 6;       { row 0 rhs, col 0 top }
    ORIENTATION_RIGHTBOT                = 7;       { row 0 rhs, col 0 bottom }
    ORIENTATION_LEFTBOT                 = 8;       { row 0 lhs, col 0 bottom }
  TIFFTAG_SAMPLESPERPIXEL               = 277;     { samples per pixel }
  TIFFTAG_ROWSPERSTRIP                  = 278;     { rows per strip of data }
  TIFFTAG_STRIPBYTECOUNTS               = 279;     { bytes counts for strips }
  TIFFTAG_MINSAMPLEVALUE                = 280;     { +minimum sample value }
  TIFFTAG_MAXSAMPLEVALUE                = 281;     { +maximum sample value }
  TIFFTAG_XRESOLUTION                   = 282;     { pixels/resolution in x }
  TIFFTAG_YRESOLUTION                   = 283;     { pixels/resolution in y }
  TIFFTAG_PLANARCONFIG                  = 284;     { storage organization }
    PLANARCONFIG_CONTIG                 = 1;       { single image plane }
    PLANARCONFIG_SEPARATE               = 2;       { separate planes of data }
  TIFFTAG_PAGENAME                      = 285;     { page name image is from }
  TIFFTAG_XPOSITION                     = 286;     { x page offset of image lhs }
  TIFFTAG_YPOSITION                     = 287;     { y page offset of image lhs }
  TIFFTAG_FREEOFFSETS                   = 288;     { +byte offset to free block }
  TIFFTAG_FREEBYTECOUNTS                = 289;     { +sizes of free blocks }

  {matched with tag reference up to this point}

  TIFFTAG_GRAYRESPONSEUNIT              = 290;     { $gray scale curve accuracy }
    GRAYRESPONSEUNIT_10S                = 1;       { tenths of a unit }
    GRAYRESPONSEUNIT_100S               = 2;       { hundredths of a unit }
    GRAYRESPONSEUNIT_1000S              = 3;       { thousandths of a unit }
    GRAYRESPONSEUNIT_10000S             = 4;       { ten-thousandths of a unit }
    GRAYRESPONSEUNIT_100000S            = 5;       { hundred-thousandths }
  TIFFTAG_GRAYRESPONSECURVE             = 291;     { $gray scale response curve }
  TIFFTAG_GROUP3OPTIONS                 = 292;     { 32 flag bits }
  TIFFTAG_T4OPTIONS                     = 292;     { TIFF 6.0 proper name alias }
    GROUP3OPT_2DENCODING                = $1;      { 2-dimensional coding }
    GROUP3OPT_UNCOMPRESSED              = $2;      { data not compressed }
    GROUP3OPT_FILLBITS                  = $4;      { fill to byte boundary }
  TIFFTAG_GROUP4OPTIONS                 = 293;     { 32 flag bits }
  TIFFTAG_T6OPTIONS                     = 293;     { TIFF 6.0 proper name }
    GROUP4OPT_UNCOMPRESSED              = $2;      { data not compressed }
  TIFFTAG_RESOLUTIONUNIT                = 296;     { units of resolutions }
    RESUNIT_NONE                        = 1;       { no meaningful units }
    RESUNIT_INCH                        = 2;       { english }
    RESUNIT_CENTIMETER                  = 3;       { metric }
  TIFFTAG_PAGENUMBER                    = 297;     { page numbers of multi-page }
  TIFFTAG_COLORRESPONSEUNIT             = 300;     { $color curve accuracy }
    COLORRESPONSEUNIT_10S               = 1;       { tenths of a unit }
    COLORRESPONSEUNIT_100S              = 2;       { hundredths of a unit }
    COLORRESPONSEUNIT_1000S             = 3;       { thousandths of a unit }
    COLORRESPONSEUNIT_10000S            = 4;       { ten-thousandths of a unit }
    COLORRESPONSEUNIT_100000S           = 5;       { hundred-thousandths }
  TIFFTAG_TRANSFERFUNCTION              = 301;     { !colorimetry info }
  TIFFTAG_SOFTWARE                      = 305;     { name & release }
  TIFFTAG_DATETIME                      = 306;     { creation date and time }
  TIFFTAG_ARTIST                        = 315;     { creator of image }
  TIFFTAG_HOSTCOMPUTER                  = 316;     { machine where created }
  TIFFTAG_PREDICTOR                     = 317;     { prediction scheme w/ LZW }
  TIFFTAG_WHITEPOINT                    = 318;     { image white point }
  TIFFTAG_PRIMARYCHROMATICITIES         = 319;     { !primary chromaticities }
  TIFFTAG_COLORMAP                      = 320;     { RGB map for pallette image }
  TIFFTAG_HALFTONEHINTS                 = 321;     { !highlight+shadow info }
  TIFFTAG_TILEWIDTH                     = 322;     { !rows/data tile }
  TIFFTAG_TILELENGTH                    = 323;     { !cols/data tile }
  TIFFTAG_TILEOFFSETS                   = 324;     { !offsets to data tiles }
  TIFFTAG_TILEBYTECOUNTS                = 325;     { !byte counts for tiles }
  TIFFTAG_BADFAXLINES                   = 326;     { lines w/ wrong pixel count }
  TIFFTAG_CLEANFAXDATA                  = 327;     { regenerated line info }
    CLEANFAXDATA_CLEAN                  = 0;       { no errors detected }
    CLEANFAXDATA_REGENERATED            = 1;       { receiver regenerated lines }
    CLEANFAXDATA_UNCLEAN                = 2;       { uncorrected errors exist }
  TIFFTAG_CONSECUTIVEBADFAXLINES        = 328;     { max consecutive bad lines }
  TIFFTAG_SUBIFD                        = 330;     { subimage descriptors }
  TIFFTAG_INKSET                        = 332;     { !inks in separated image }
    INKSET_CMYK                         = 1;       { !cyan-magenta-yellow-black color }
    INKSET_MULTIINK                     = 2;       { !multi-ink or hi-fi color }
  TIFFTAG_INKNAMES                      = 333;     { !ascii names of inks }
  TIFFTAG_NUMBEROFINKS                  = 334;     { !number of inks }
  TIFFTAG_DOTRANGE                      = 336;     { !0% and 100% dot codes }
  TIFFTAG_TARGETPRINTER                 = 337;     { !separation target }
  TIFFTAG_EXTRASAMPLES                  = 338;     { !info about extra samples }
    EXTRASAMPLE_UNSPECIFIED             = 0;       { !unspecified data }
    EXTRASAMPLE_ASSOCALPHA              = 1;       { !associated alpha data }
    EXTRASAMPLE_UNASSALPHA              = 2;       { !unassociated alpha data }
  TIFFTAG_SAMPLEFORMAT                  = 339;     { !data sample format }
    SAMPLEFORMAT_UINT                   = 1;       { !unsigned integer data }
    SAMPLEFORMAT_INT                    = 2;       { !signed integer data }
    SAMPLEFORMAT_IEEEFP                 = 3;       { !IEEE floating point data }
    SAMPLEFORMAT_VOID                   = 4;       { !untyped data }
    SAMPLEFORMAT_COMPLEXINT             = 5;       { !complex signed int }
    SAMPLEFORMAT_COMPLEXIEEEFP          = 6;       { !complex ieee floating }
  TIFFTAG_SMINSAMPLEVALUE               = 340;     { !variable MinSampleValue }
  TIFFTAG_SMAXSAMPLEVALUE               = 341;     { !variable MaxSampleValue }
  TIFFTAG_CLIPPATH                      = 343;     { %ClipPath [Adobe TIFF technote 2] }
  TIFFTAG_XCLIPPATHUNITS                = 344;     { %XClipPathUnits [Adobe TIFF technote 2] }
  TIFFTAG_YCLIPPATHUNITS                = 345;     { %YClipPathUnits [Adobe TIFF technote 2] }
  TIFFTAG_INDEXED                       = 346;     { %Indexed [Adobe TIFF Technote 3] }
  TIFFTAG_JPEGTABLES                    = 347;     { %JPEG table stream }
  TIFFTAG_OPIPROXY                      = 351;     { %OPI Proxy [Adobe TIFF technote] }
  { Tags 512-521 are obsoleted by Technical Note #2
  which specifies a revised JPEG-in-TIFF scheme. }
  TIFFTAG_JPEGPROC                      = 512;     { !JPEG processing algorithm }
    JPEGPROC_BASELINE                   = 1;       { !baseline sequential }
    JPEGPROC_LOSSLESS                   = 14;      { !Huffman coded lossless }
  TIFFTAG_JPEGIFOFFSET                  = 513;     { !pointer to SOI marker }
  TIFFTAG_JPEGIFBYTECOUNT               = 514;     { !JFIF stream length }
  TIFFTAG_JPEGRESTARTINTERVAL           = 515;     { !restart interval length }
  TIFFTAG_JPEGLOSSLESSPREDICTORS        = 517;     { !lossless proc predictor }
  TIFFTAG_JPEGPOINTTRANSFORM            = 518;     { !lossless point transform }
  TIFFTAG_JPEGQTABLES                   = 519;     { !Q matrice offsets }
  TIFFTAG_JPEGDCTABLES                  = 520;     { !DCT table offsets }
  TIFFTAG_JPEGACTABLES                  = 521;     { !AC coefficient offsets }
  TIFFTAG_YCBCRCOEFFICIENTS             = 529;     { !RGB -> YCbCr transform }
  TIFFTAG_YCBCRSUBSAMPLING              = 530;     { !YCbCr subsampling factors }
  TIFFTAG_YCBCRPOSITIONING              = 531;     { !subsample positioning }
    YCBCRPOSITION_CENTERED              = 1;       { !as in PostScript Level 2 }
    YCBCRPOSITION_COSITED               = 2;       { !as in CCIR 601-1 }
  TIFFTAG_REFERENCEBLACKWHITE           = 532;     { !colorimetry info }
  TIFFTAG_XMLPACKET                     = 700;     { %XML packet [Adobe XMP technote 9-14-02] (dkelly@apago.com) }
  TIFFTAG_OPIIMAGEID                    = 32781;   { %OPI ImageID [Adobe TIFF technote] }
  { tags 32952-32956 are private tags registered to Island Graphics }
  TIFFTAG_REFPTS                        = 32953;   { image reference points }
  TIFFTAG_REGIONTACKPOINT               = 32954;   { region-xform tack point }
  TIFFTAG_REGIONWARPCORNERS             = 32955;   { warp quadrilateral }
  TIFFTAG_REGIONAFFINE                  = 32956;   { affine transformation mat }
  { tags 32995-32999 are private tags registered to SGI }
  TIFFTAG_MATTEING                      = 32995;   { $use ExtraSamples }
  TIFFTAG_DATATYPE                      = 32996;   { $use SampleFormat }
  TIFFTAG_IMAGEDEPTH                    = 32997;   { z depth of image }
  TIFFTAG_TILEDEPTH                     = 32998;   { z depth/data tile }
  { tags 33300-33309 are private tags registered to Pixar }
  { TIFFTAG_PIXAR_IMAGEFULLWIDTH and TIFFTAG_PIXAR_IMAGEFULLLENGTH are set when an image has been cropped out of a larger image.
    They reflect the size of the original uncropped image. The TIFFTAG_XPOSITION and TIFFTAG_YPOSITION can be used to determine the
    position of the smaller image in the larger one. }
  TIFFTAG_PIXAR_IMAGEFULLWIDTH          = 33300;   { full image size in x }
  TIFFTAG_PIXAR_IMAGEFULLLENGTH         = 33301;   { full image size in y }
  { Tags 33302-33306 are used to identify special image modes and data used by Pixar's texture formats. }
  TIFFTAG_PIXAR_TEXTUREFORMAT           = 33302;   { texture map format }
  TIFFTAG_PIXAR_WRAPMODES               = 33303;   { s & t wrap modes }
  TIFFTAG_PIXAR_FOVCOT                  = 33304;   { cotan(fov) for env. maps }
  TIFFTAG_PIXAR_MATRIX_WORLDTOSCREEN    = 33305;
  TIFFTAG_PIXAR_MATRIX_WORLDTOCAMERA    = 33306;
  { tag 33405 is a private tag registered to Eastman Kodak }
  TIFFTAG_WRITERSERIALNUMBER            = 33405;   { device serial number }
  { tag 33432 is listed in the 6.0 spec w/ unknown ownership }
  TIFFTAG_COPYRIGHT                     = 33432;   { copyright string }
  { IPTC TAG from RichTIFF specifications }
  TIFFTAG_RICHTIFFIPTC                  = 33723;
  { 34016-34029 are reserved for ANSI IT8 TIFF/IT <dkelly@apago.com) }
  TIFFTAG_IT8SITE                       = 34016;   { site name }
  TIFFTAG_IT8COLORSEQUENCE              = 34017;   { color seq. [RGB,CMYK,etc] }
  TIFFTAG_IT8HEADER                     = 34018;   { DDES Header }
  TIFFTAG_IT8RASTERPADDING              = 34019;   { raster scanline padding }
  TIFFTAG_IT8BITSPERRUNLENGTH           = 34020;   { # of bits in short run }
  TIFFTAG_IT8BITSPEREXTENDEDRUNLENGTH   = 34021;   { # of bits in long run }
  TIFFTAG_IT8COLORTABLE                 = 34022;   { LW colortable }
  TIFFTAG_IT8IMAGECOLORINDICATOR        = 34023;   { BP/BL image color switch }
  TIFFTAG_IT8BKGCOLORINDICATOR          = 34024;   { BP/BL bg color switch }
  TIFFTAG_IT8IMAGECOLORVALUE            = 34025;   { BP/BL image color value }
  TIFFTAG_IT8BKGCOLORVALUE              = 34026;   { BP/BL bg color value }
  TIFFTAG_IT8PIXELINTENSITYRANGE        = 34027;   { MP pixel intensity value }
  TIFFTAG_IT8TRANSPARENCYINDICATOR      = 34028;   { HC transparency switch }
  TIFFTAG_IT8COLORCHARACTERIZATION      = 34029;   { color character. table }
  TIFFTAG_IT8HCUSAGE                    = 34030;   { HC usage indicator }
  TIFFTAG_IT8TRAPINDICATOR              = 34031;   { Trapping indicator (untrapped=0, trapped=1) }
  TIFFTAG_IT8CMYKEQUIVALENT             = 34032;   { CMYK color equivalents }
  { tags 34232-34236 are private tags registered to Texas Instruments }
  TIFFTAG_FRAMECOUNT                    = 34232;   { Sequence Frame Count }
  { tag 34750 is a private tag registered to Adobe? }
  TIFFTAG_ICCPROFILE                    = 34675;   { ICC profile data }
  { tag 34377 is private tag registered to Adobe for PhotoShop }
  TIFFTAG_PHOTOSHOP                     = 34377;
  { tag 34750 is a private tag registered to Pixel Magic }
  TIFFTAG_JBIGOPTIONS                   = 34750;   { JBIG options }
  { tags 34908-34914 are private tags registered to SGI }
  TIFFTAG_FAXRECVPARAMS                 = 34908;   { encoded Class 2 ses. parms }
  TIFFTAG_FAXSUBADDRESS                 = 34909;   { received SubAddr string }
  TIFFTAG_FAXRECVTIME                   = 34910;   { receive time (secs) }
  { tags 37439-37443 are registered to SGI <gregl@sgi.com> }
  TIFFTAG_STONITS                       = 37439;   { Sample value to Nits }
  { tag 34929 is a private tag registered to FedEx }
  TIFFTAG_FEDEX_EDR                     = 34929;   { unknown use }
  { tag 65535 is an undefined tag used by Eastman Kodak }
  TIFFTAG_DCSHUESHIFTVALUES             = 65535;   { hue shift correction data }
  { The following are ``pseudo tags'' that can be used to control codec-specific functionality. These tags are not written to file.
    Note that these values start at 0xffff+1 so that they'll never collide with Aldus-assigned tags. }
  TIFFTAG_FAXMODE                       = 65536;   { Group 3/4 format control }
    FAXMODE_CLASSIC                     = $0;      { default, include RTC }
    FAXMODE_NORTC                       = $1;      { no RTC at end of data }
    FAXMODE_NOEOL                       = $2;      { no EOL code at end of row }
    FAXMODE_BYTEALIGN                   = $4;      { byte align row }
    FAXMODE_WORDALIGN                   = $8;      { word align row }
    FAXMODE_CLASSF                      = FAXMODE_NORTC;        { TIFF Class F }
  TIFFTAG_JPEGQUALITY                   = 65537;   { Compression quality level }
  { Note: quality level is on the IJG 0-100 scale.  Default value is 75 }
  TIFFTAG_JPEGCOLORMODE                 = 65538;   { Auto RGB<=>YCbCr convert? }
    JPEGCOLORMODE_RAW                   = $0;      { no conversion (default) }
    JPEGCOLORMODE_RGB                   = $1;      { do auto conversion }
  TIFFTAG_JPEGTABLESMODE                = 65539;   { What to put in JPEGTables }
    JPEGTABLESMODE_QUANT                = $1;      { include quantization tbls }
    JPEGTABLESMODE_HUFF                 = $2;      { include Huffman tbls }
  { Note: default is JPEGTABLESMODE_QUANT | JPEGTABLESMODE_HUFF }
  TIFFTAG_FAXFILLFUNC                   = 65540;   { G3/G4 fill function }
  TIFFTAG_PIXARLOGDATAFMT               = 65549;   { PixarLogCodec I/O data sz }
    PIXARLOGDATAFMT_8BIT                = 0;       { regular u_char samples }
    PIXARLOGDATAFMT_8BITABGR            = 1;       { ABGR-order u_chars }
    PIXARLOGDATAFMT_11BITLOG            = 2;       { 11-bit log-encoded (raw) }
    PIXARLOGDATAFMT_12BITPICIO          = 3;       { as per PICIO (1.0==2048) }
    PIXARLOGDATAFMT_16BIT               = 4;       { signed short samples }
    PIXARLOGDATAFMT_FLOAT               = 5;       { IEEE float samples }
  { 65550-65556 are allocated to Oceana Matrix <dev@oceana.com> }
  TIFFTAG_DCSIMAGERTYPE                 = 65550;   { imager model & filter }
    DCSIMAGERMODEL_M3                   = 0;       { M3 chip (1280 x 1024) }
    DCSIMAGERMODEL_M5                   = 1;       { M5 chip (1536 x 1024) }
    DCSIMAGERMODEL_M6                   = 2;       { M6 chip (3072 x 2048) }
    DCSIMAGERFILTER_IR                  = 0;       { infrared filter }
    DCSIMAGERFILTER_MONO                = 1;       { monochrome filter }
    DCSIMAGERFILTER_CFA                 = 2;       { color filter array }
    DCSIMAGERFILTER_OTHER               = 3;       { other filter }
  TIFFTAG_DCSINTERPMODE                 = 65551;   { interpolation mode }
    DCSINTERPMODE_NORMAL                = 0;       { whole image, default }
    DCSINTERPMODE_PREVIEW               = 1;       { preview of image (384x256) }
  TIFFTAG_DCSBALANCEARRAY               = 65552;   { color balance values }
  TIFFTAG_DCSCORRECTMATRIX              = 65553;   { color correction values }
  TIFFTAG_DCSGAMMA                      = 65554;   { gamma value }
  TIFFTAG_DCSTOESHOULDERPTS             = 65555;   { toe & shoulder points }
  TIFFTAG_DCSCALIBRATIONFD              = 65556;   { calibration file desc }
  { Note: quality level is on the ZLIB 1-9 scale. Default value is -1 }
  TIFFTAG_ZIPQUALITY                    = 65557;   { compression quality level }
  TIFFTAG_PIXARLOGQUALITY               = 65558;   { PixarLog uses same scale }
  { 65559 is allocated to Oceana Matrix <dev@oceana.com> }
  TIFFTAG_DCSCLIPRECTANGLE              = 65559;   { area of image to acquire }
  TIFFTAG_SGILOGDATAFMT                 = 65560;   { SGILog user data format }
    SGILOGDATAFMT_FLOAT                 = 0;       { IEEE float samples }
    SGILOGDATAFMT_16BIT                 = 1;       { 16-bit samples }
    SGILOGDATAFMT_RAW                   = 2;       { uninterpreted data }
    SGILOGDATAFMT_8BIT                  = 3;       { 8-bit RGB monitor values }
  TIFFTAG_SGILOGENCODE                  = 65561;   { SGILog data encoding control }
    SGILOGENCODE_NODITHER               = 0;       { do not dither encoded values }
    SGILOGENCODE_RANDITHER              = 1;       { randomly dither encd values }


  { Flags to pass to TIFFPrintDirectory to control printing of data structures that are potentially very large. Bit-or these flags to
    enable printing multiple items. }
  TIFFPRINT_NONE                        = $0;      { no extra info }
  TIFFPRINT_STRIPS                      = $1;      { strips/tiles info }
  TIFFPRINT_CURVES                      = $2;      { color/gray response curves }
  TIFFPRINT_COLORMAP                    = $4;      { colormap }
  TIFFPRINT_JPEGQTABLES                 = $100;    { JPEG Q matrices }
  TIFFPRINT_JPEGACTABLES                = $200;    { JPEG AC tables }
  TIFFPRINT_JPEGDCTABLES                = $200;    { JPEG DC tables }


  TIFF_ANY                              = TIFF_NOTYPE;   { for field descriptor searching }
  TIFF_VARIABLE                         = -1;      { marker for variable length tags }
  TIFF_SPP                              = -2;      { marker for SamplesPerPixel tags }
  TIFF_VARIABLE2                        = -3;      { marker for uint32 var-length tags }

  FIELD_CUSTOM                          = 65;

 {added for LibTiff 3.9.4 by Alex (leontyyy@gmail.com) Dec.2011}
  TIFFTAG_EXIFIFD                       = 34665;   { pointer to the Exif IFD }
  EXIFTAG_FOCALLENGTH                   = 37386;   { focal length }
  EXIFTAG_FOCALLENGTHIN35MMFILM         = 41989;   { indicates the equivalent focal length assuming a 35mm film camera, in mm }
  EXIFTAG_EXIFVERSION                   = 36864;   { version of exif format }
  EXIFTAG_DATETIMEDIGITIZED             = 36868;   { date and time when the image was stored as digital data }
  EXIFTAG_DATETIMEORIGINAL              = 36867;   { date and time when the original image data was generated }
  EXIFTAG_EXPOSURETIME                  = 33434;   { exposure time, given in seconds }
  EXIFTAG_FNUMBER                       = 33437;   { F number }
  EXIFTAG_EXPOSUREPROGRAM               = 34850;   { class of the program used by the camera to set exposure }
  EXIFTAG_SPECTRALSENSITIVITY           = 34852;   { spectral sensitivity of each channel of the camera used }
  EXIFTAG_ISOSPEEDRATINGS               = 34855;   { ISO Speed and ISO Latitude }
  EXIFTAG_OECF                          = 34856;   { Opto-Electric Conversion Function }
  EXIFTAG_COMPONENTSCONFIGURATION       = 37121;   { meaning of each component }
  EXIFTAG_COMPRESSEDBITSPERPIXEL        = 37122;   { compression mode }
  EXIFTAG_SHUTTERSPEEDVALUE             = 37377;   { shutter speed }
  EXIFTAG_APERTUREVALUE                 = 37378;   { lens aperture }
  EXIFTAG_BRIGHTNESSVALUE               = 37379;   { brightness }
  EXIFTAG_EXPOSUREBIASVALUE             = 37380;   { exposure bias }
  EXIFTAG_MAXAPERTUREVALUE              = 37381;   { maximum lens aperture }
  EXIFTAG_SUBJECTDISTANCE               = 37382;   { distance to the subject in meters }
  EXIFTAG_METERINGMODE                  = 37383;   { metering mode }
  EXIFTAG_LIGHTSOURCE                   = 37384;   { light source }
  EXIFTAG_FLASH                         = 37385;   { flash }
  EXIFTAG_SUBJECTAREA                   = 37396;   { subject area (in exif ver.2.2) }
  EXIFTAG_MAKERNOTE                     = 37500;   { manufacturer notes }
  EXIFTAG_USERCOMMENT                   = 37510;   { user comments }
  EXIFTAG_SUBSECTIME                    = 37520;   { DateTime subseconds }
  EXIFTAG_SUBSECTIMEORIGINAL            = 37521;   { DateTimeOriginal subseconds }
  EXIFTAG_SUBSECTIMEDIGITIZED           = 37522;   { DateTimeDigitized subseconds }
  EXIFTAG_FLASHPIXVERSION               = 40960;   { FlashPix format version }
  EXIFTAG_COLORSPACE                    = 40961;   { color space information }
  EXIFTAG_PIXELXDIMENSION               = 40962;   { valid image width }
  EXIFTAG_PIXELYDIMENSION               = 40963;   { valid image height }
  EXIFTAG_RELATEDSOUNDFILE              = 40964;   { related audio file }
  EXIFTAG_FLASHENERGY                   = 41483;   { flash energy }
  EXIFTAG_SPATIALFREQUENCYRESPONSE      = 41484;   { spatial frequency response }
  EXIFTAG_FOCALPLANEXRESOLUTION         = 41486;   { focal plane X resolution }
  EXIFTAG_FOCALPLANEYRESOLUTION         = 41487;   { focal plane Y resolution }
  EXIFTAG_FOCALPLANERESOLUTIONUNIT      = 41488;   { focal plane resolution unit }
  EXIFTAG_SUBJECTLOCATION               = 41492;   { subject location }
  EXIFTAG_EXPOSUREINDEX                 = 41493;   { exposure index }
  EXIFTAG_SENSINGMETHOD                 = 41495;   { sensing method }
  EXIFTAG_FILESOURCE                    = 41728;   { file source }
  EXIFTAG_SCENETYPE                     = 41729;   { scene type }
  EXIFTAG_CFAPATTERN                    = 41730;   { CFA pattern }
  EXIFTAG_CUSTOMRENDERED                = 41985;   { custom image processing (in exif ver.2.2) }
  EXIFTAG_EXPOSUREMODE                  = 41986;   { exposure mode (in exif ver.2.2) }
  EXIFTAG_WHITEBALANCE                  = 41987;   { white balance (in exif ver.2.2) }
  EXIFTAG_DIGITALZOOMRATIO              = 41988;   { digital zoom ratio (in exif ver.2.2) }
  EXIFTAG_SCENECAPTURETYPE              = 41990;   { scene capture type (in exif ver.2.2) }
  EXIFTAG_GAINCONTROL                   = 41991;   { gain control (in exif ver.2.2) }
  EXIFTAG_CONTRAST                      = 41992;   { contrast (in exif ver.2.2) }
  EXIFTAG_SATURATION                    = 41993;   { saturation (in exif ver.2.2) }
  EXIFTAG_SHARPNESS                     = 41994;   { sharpness (in exif ver.2.2) }
  EXIFTAG_DEVICESETTINGDESCRIPTION      = 41995;   { device settings description (in exif ver.2.2) }
  EXIFTAG_SUBJECTDISTANCERANGE          = 41996;   { subject distance range (in exif ver.2.2) }
  EXIFTAG_IMAGEUNIQUEID                 = 42016;   { Unique image ID (in exif ver.2.2) }

type
  PTIFF = Pointer;
  PTIFFRGBAImage = Pointer;

  TIFFReadWriteProc = function(fd: thandle_t; buf: tdata_t; size: tsize_t): tsize_t; cdecl;
  TIFFSeekProc = function(fd: thandle_t; off: toff_t; whence: Integer): toff_t; cdecl;
  TIFFCloseProc = function(fd: thandle_t): Integer; cdecl;
  TIFFSizeProc = function(fd: thandle_t): toff_t; cdecl;
  TIFFMapFileProc = function(fd: thandle_t; var pbase: tdata_t; var psize: toff_t): Integer; cdecl;
  TIFFUnmapFileProc = procedure(fd: thandle_t; base: tdata_t; size: toff_t); cdecl;
  TIFFExtendProc = procedure(Handle: PTIFF); cdecl;
  TIFFErrorHandler = procedure(Module: PAnsiChar; const Format: PAnsiChar; Params: va_list); cdecl;
  TIFFInitMethod = function(Handle: PTIFF; Scheme: Integer): Integer; cdecl;

  PTIFFCodec = ^TIFFCodec;
  TIFFCodec = record
    Name: PAnsiChar;
    Scheme: Word;
    Init: TIFFInitMethod;
  end;

  PTIFFFieldInfo = ^TIFFFieldInfo;
  TIFFFieldInfo = record
    FieldTag: Cardinal;              { field's tag }
    FieldReadCount: Smallint;        { read count/TIFF_VARIABLE/TIFF_SPP }
    FieldWriteCount: Smallint;       { write count/TIFF_VARIABLE }
    FieldType: Integer;              { type of associated data }
    FieldBit: Word;                  { bit in fieldsset bit vector }
    FieldOkToChange: Byte;           { if true, can change while writing }
    FieldPassCount: Byte;            { if true, pass dir count on set }
    FieldName: PAnsiChar;                { ASCII name }
  end;

  PTIFFTagValue = ^TIFFTagValue;
  TIFFTagValue = record
    Info: PTIFFFieldInfo;
    Count: Integer;
    Value: Pointer;
  end;

{$IFDEF DYNAMIC_DLL_LOADING}
var
  TIFFGetVersion: function(): PAnsiChar; cdecl;
  TIFFOpen: function (const FileName: PAnsiChar; const Mode: PAnsiChar): PTIFF; cdecl;
  TIFFClientOpen: function(
    const Name: PAnsiChar;
    const Mode: PAnsiChar;
    ClientData: Cardinal;
    ReadProc: TIFFReadWriteProc;
    WriteProc: TIFFReadWriteProc;
    SeekProc: TIFFSeekProc;
    CloseProc: TIFFCloseProc;
    SizeProc: TIFFSizeProc;
    MapProc: TIFFMapFileProc;
    UnmapProc: TIFFUnmapFileProc): PTIFF; cdecl;
  TIFFClose: procedure(Handle: PTIFF); cdecl;
  TIFFSetFileno: function(Handle: PTIFF; Newvalue: Integer): Integer; cdecl;
  TIFFSetField: function(Handle: PTIFF; Tag: Cardinal): Integer; cdecl varargs;
  TIFFGetField: function(Handle: PTIFF; Tag: Cardinal): Integer; cdecl varargs;
  TIFFGetFieldDefaulted: function(Handle: PTIFF; Tag: Cardinal): Integer; cdecl varargs;
  TIFFReadRGBAImageOriented: function(Handle: PTIFF; RWidth,RHeight: Cardinal; Raster: Pointer; Orientation: Integer; Stop: Integer): Integer; cdecl;
  TIFFReadScanline: function(Handle: PTIFF; Buf: Pointer; Row: Cardinal; Sample: tsample_t): Integer; cdecl;
  TIFFWriteScanline: function(Handle: PTIFF; Buf: Pointer; Row: Cardinal; Sample: tsample_t): Integer; cdecl;
  TIFFScanlineSize: function(Handle: PTIFF): tmsize_t; cdecl;
  TIFFDefaultStripSize: function(Handle: PTIFF; Request: Cardinal): Cardinal; cdecl;
  TIFFNumberOfDirectories: function(Handle: PTIFF): Word; cdecl;
  TIFFSetDirectory: function(Handle: PTIFF; Dirn: Word): Integer; cdecl;
  TIFFWriteDirectory: function(Handle: PTIFF): Integer; cdecl;
  TIFFReadEXIFDirectory: function(Handle: PTIFF; Diroff: toff_t): Integer; cdecl;
  TIFFSetErrorHandler: function(Handler: TIFFErrorHandler): TIFFErrorHandler; cdecl;
  TIFFSetWarningHandler: function(Handler: TIFFErrorHandler): TIFFErrorHandler; cdecl;

function LoadTiffLibrary: Boolean;
{$ELSE}
function  TIFFGetVersion: PAnsiChar; cdecl; external SLibName;
function  TIFFFindCODEC(Scheme: Word): PTIFFCodec; cdecl; external SLibName;
function  TIFFRegisterCODEC(Scheme: Word; Name: PAnsiChar; InitMethod: TIFFInitMethod): PTIFFCodec; cdecl; external SLibName;
procedure TIFFUnRegisterCODEC(c: PTIFFCodec); cdecl; external SLibName;
function  TIFFIsCODECConfigured(Scheme: Word): Integer; cdecl; external SLibName;
function  TIFFGetConfiguredCODECs: PTIFFCodec; cdecl; external SLibName;
function  TIFFClientOpen(Name: PAnsiChar; Mode: PAnsiChar; ClientData: THandle;
          ReadProc: TIFFReadWriteProc;
          WriteProc: TIFFReadWriteProc;
          SeekProc: TIFFSeekProc;
          CloseProc: TIFFCloseProc;
          SizeProc: TIFFSizeProc;
          MapProc: TIFFMapFileProc;
          UnmapProc: TIFFUnmapFileProc): PTIFF; cdecl; external SLibName;
procedure TIFFCleanup(Handle: PTIFF); cdecl; external SLibName;
procedure TIFFClose(Handle: PTIFF); cdecl; external SLibName;
function  TIFFFileno(Handle: PTIFF): Integer; cdecl; external SLibName;
function  TIFFSetFileno(Handle: PTIFF; Newvalue: Integer): Integer; cdecl; external SLibName;
function  TIFFClientdata(Handle: PTIFF): THandle; cdecl; external SLibName;
function  TIFFSetClientdata(Handle: PTIFF; Newvalue: THandle): THandle; cdecl; external SLibName;
function  TIFFGetMode(Handle: PTIFF): Integer; cdecl; external SLibName;
function  TIFFSetMode(Handle: PTIFF; Mode: Integer): Integer; cdecl; external SLibName;
function  TIFFFileName(Handle: PTIFF): Pointer; cdecl; external SLibName;
function  TIFFSetFileName(Handle: PTIFF; Name: PAnsiChar): PAnsiChar; cdecl; external SLibName;
function  TIFFGetReadProc(Handle: PTIFF): TIFFReadWriteProc; cdecl; external SLibName;
function  TIFFGetWriteProc(Handle: PTIFF): TIFFReadWriteProc; cdecl; external SLibName;
function  TIFFGetSeekProc(Handle: PTIFF): TIFFSeekProc; cdecl; external SLibName;
function  TIFFGetCloseProc(Handle: PTIFF): TIFFCloseProc; cdecl; external SLibName;
function  TIFFGetSizeProc(Handle: PTIFF): TIFFSizeProc; cdecl; external SLibName;
procedure TIFFError(Module: Pointer; Fmt: Pointer); cdecl; external SLibName; varargs;
function  TIFFSetErrorHandler(Handler: TIFFErrorHandler): TIFFErrorHandler; cdecl; external SLibName;
procedure TIFFWarning(Module: Pointer; Fmt: Pointer); cdecl; external SLibName; varargs;
function  TIFFSetWarningHandler(Handler: TIFFErrorHandler): TIFFErrorHandler; cdecl; external SLibName;
function  TIFFSetTagExtender(Extender: TIFFExtendProc): TIFFExtendProc; cdecl; external SLibName;
function  TIFFFlush(Handle: PTIFF): Integer; cdecl; external SLibName;
function  TIFFFlushData(Handle: PTIFF): Integer; cdecl; external SLibName;
function  TIFFReadEXIFDirectory(Handle: PTIFF; Diroff: toff_t): Integer; cdecl; external SLibName;
function  TIFFReadDirectory(Handle: PTIFF): Integer; cdecl; external SLibName;
function  TIFFCurrentDirectory(Handle: PTIFF): Word; cdecl; external SLibName;
function  TIFFCurrentDirOffset(Handle: PTIFF): toff_t; cdecl; external SLibName;
function  TIFFLastDirectory(Handle: PTIFF): Integer; cdecl; external SLibName;
function  TIFFNumberOfDirectories(Handle: PTIFF): Word; cdecl; external SLibName;
function  TIFFSetDirectory(Handle: PTIFF; Dirn: Word): Integer; cdecl; external SLibName;
function  TIFFSetSubDirectory(Handle: PTIFF; Diroff: toff_t): Integer; cdecl; external SLibName;
function  TIFFCreateDirectory(Handle: PTIFF): Integer; cdecl; external SLibName;
function  TIFFWriteDirectory(Handle: PTIFF): Integer; cdecl; external SLibName;
function  TIFFUnlinkDirectory(handle: PTIFF; Dirn: Word): Integer; cdecl; external SLibName;
procedure TIFFPrintDirectory(Handle: PTIFF; Fd: Pointer; Flags: Integer); cdecl; external SLibName;
function  TIFFGetField(Handle: PTIFF; Tag: Cardinal): Integer; cdecl; external SLibName; varargs;
function  TIFFGetFieldDefaulted(Handle: PTIFF; Tag: Cardinal): Integer; cdecl; external SLibName; varargs;
function  TIFFVGetField(Handle: PTIFF; Tag: Cardinal; Ap: Pointer): Integer; cdecl; external SLibName;
function  TIFFSetField(Handle: PTIFF; Tag: Cardinal): Integer; cdecl; external SLibName; varargs;
function  TIFFVSetField(Handle: PTIFF; Tag: Cardinal; Ap: Pointer): Integer; cdecl; external SLibName;
function  TIFFIsBigEndian(Handle: PTIFF): Integer; cdecl; external SLibName;
function  TIFFIsTiled(Handle: PTIFF): Integer; cdecl; external SLibName;
function  TIFFIsByteSwapped(Handle: PTIFF): Integer; cdecl; external SLibName;
function  TIFFIsUpSampled(Handle: PTIFF): Integer; cdecl; external SLibName;
function  TIFFIsMSB2LSB(Handle: PTIFF): Integer; cdecl; external SLibName;
function  TIFFGetTagListCount(Handle: PTIFF): Integer; cdecl; external SLibName;
function  TIFFGetTagListEntry(Handle: PTIFF; TagIndex: Integer): Cardinal; cdecl; external SLibName;
procedure TIFFMergeFieldInfo(Handle: PTIFF; Info: PTIFFFieldInfo; N: Integer); cdecl; external SLibName;
function  TIFFFindFieldInfo(Handle: PTIFF; Tag: Cardinal; Dt: Integer): PTIFFFieldInfo; cdecl; external SLibName;
function  TIFFFindFieldInfoByName(Handle: PTIFF; FIeldName: PAnsiChar; Dt: Integer): PTIFFFieldInfo; cdecl; external SLibName;
function  TIFFFieldWithTag(Handle: PTIFF; Tag: Cardinal): PTIFFFieldInfo; cdecl; external SLibName;
function  TIFFFieldWithName(Handle: PTIFF; FieldName: PAnsiChar): PTIFFFieldInfo; cdecl; external SLibName;
function  TIFFDataWidth(DataType: Integer): Integer; cdecl; external SLibName;
function  TIFFReadRGBAImage(Handle: PTIFF; RWidth,RHeight: Cardinal; Raster: Pointer; Stop: Integer): Integer; cdecl; external SLibName;
function  TIFFReadRGBAImageOriented(Handle: PTIFF; RWidth,RHeight: Cardinal; Raster: Pointer; Orientation: Integer; Stop: Integer): Integer; cdecl; external SLibName;
function  TIFFReadRGBAStrip(Handle: PTIFF; Row: Cardinal; Raster: Pointer): Integer; cdecl; external SLibName;
function  TIFFReadRGBATile(Handle: PTIFF; Col,Row: Cardinal; Raster: Pointer): Integer; cdecl; external SLibName;
function  TIFFRGBAImageOk(Handle: PTIFF; Emsg: PAnsiChar): Integer; cdecl; external SLibName;
function  TIFFRGBAImageBegin(Img: PTIFFRGBAImage; Handle: PTIFF; Stop: Integer; Emsg: PAnsiChar): Integer; cdecl; external SLibName;
function  TIFFRGBAImageGet(Img: PTIFFRGBAImage; Raster: Pointer; W, H: Cardinal): Integer; cdecl; external SLibName;
procedure TIFFRGBAImageEnd(Img: PTIFFRGBAImage); cdecl; external SLibName;
function  TIFFCurrentRow(Handle: PTIFF): Cardinal; cdecl; external SLibName;
function  TIFFStripSize(Handle: PTIFF): tmsize_t; cdecl; external SLibName;
function  TIFFRawStripSize(Handle: PTIFF; Strip: Cardinal): tmsize_t; cdecl; external SLibName;
function  TIFFVStripSize(Handle: PTIFF; NRows: Cardinal): tmsize_t; cdecl; external SLibName;
function  TIFFDefaultStripSize(Handle: PTIFF; Request: Cardinal): Cardinal; cdecl; external SLibName;
function  TIFFNumberOfStrips(Handle: PTIFF): Cardinal; cdecl; external SLibName;
function  TIFFComputeStrip(Handle: PTIFF; Row: Cardinal; Sample: Word): Cardinal; cdecl; external SLibName;
function  TIFFReadRawStrip(Handle: PTIFF; Strip: Cardinal; Buf: Pointer; Size: tmsize_t): tmsize_t; cdecl; external SLibName;
function  TIFFReadEncodedStrip(Handle: PTIFF; Strip: Cardinal; Buf: Pointer; Size: tmsize_t): tmsize_t; cdecl; external SLibName;
function  TIFFWriteRawStrip(Handle: PTIFF; Strip: Cardinal; Data: Pointer; Cc: tmsize_t): tmsize_t; cdecl; external SLibName;
function  TIFFWriteEncodedStrip(Handle: PTIFF; Strip: Cardinal; Data: Pointer; Cc: tmsize_t): tmsize_t; cdecl; external SLibName;
function  TIFFCurrentStrip(Handle: PTIFF): Cardinal; cdecl; external SLibName;
function  TIFFTileSize(Handle: PTIFF): tmsize_t; cdecl; external SLibName;
function  TIFFTileRowSize(Handle: PTIFF): tmsize_t; cdecl; external SLibName;
function  TIFFVTileSize(Handle: PTIFF; NRows: Cardinal): tmsize_t; cdecl; external SLibName;
procedure TIFFDefaultTileSize(Handle: PTIFF; Tw: PCardinal; Th: PCardinal); cdecl; external SLibName;
function  TIFFNumberOfTiles(Handle: PTIFF): Cardinal; cdecl; external SLibName;
function  TIFFComputeTile(Handle: PTIFF; X,Y,Z: Cardinal; S: Word): Cardinal; cdecl; external SLibName;
function  TIFFReadRawTile(Handle: PTIFF; Tile: Cardinal; Buf: Pointer; Size: tmsize_t): tmsize_t; cdecl; external SLibName;
function  TIFFReadEncodedTile(Handle: PTIFF; Tile: Cardinal; Buf: Pointer; Size: tmsize_t): tmsize_t; cdecl; external SLibName;
function  TIFFWriteRawTile(Handle: PTIFF; Tile: Cardinal; Data: Pointer; Cc: tmsize_t): tmsize_t; cdecl; external SLibName;
function  TIFFWriteEncodedTile(Handle: PTIFF; Tile: Cardinal; Data: Pointer; Cc: tmsize_t): tmsize_t; cdecl; external SLibName;
function  TIFFCurrentTile(Handle: PTIFF): Cardinal; cdecl; external SLibName;
function  TIFFScanlineSize(Handle: PTIFF): tmsize_t; cdecl; external SLibName;
function  TIFFScanlineSize64(Handle: PTIFF): Int64; cdecl; external SLibName;
function  TIFFRasterScanlineSize64(Handle: PTIFF): Int64; cdecl; external SLibName;
function  TIFFRasterScanlineSize(Handle: PTIFF): tmsize_t; cdecl; external SLibName;
function  TIFFReadScanline(Handle: PTIFF; Buf: Pointer; Row: Cardinal; Sample: tsample_t): Integer; cdecl; external SLibName;
function  TIFFWriteScanline(Handle: PTIFF; Buf: Pointer; Row: Cardinal; Sample: tsample_t): Integer; cdecl; external SLibName;
procedure TIFFSetWriteOffset(Handle: PTIFF; Off: toff_t); cdecl; external SLibName;
procedure TIFFSwabShort(Wp: PWord); cdecl; external SLibName;
procedure TIFFSwabLong(Lp: PCardinal); cdecl; external SLibName;
procedure TIFFSwabDouble(Dp: PDouble); cdecl; external SLibName;
procedure TIFFSwabArrayOfShort(Wp: PWord; N: tmsize_t); cdecl; external SLibName;
procedure TIFFSwabArrayOfTriples(tp:PByte; n: tmsize_t); cdecl; external SLibName;
procedure TIFFSwabArrayOfLong(Lp: PCardinal; N: tmsize_t); cdecl; external SLibName;
procedure TIFFSwabArrayOfDouble(Dp: PDouble; N: tmsize_t); cdecl; external SLibName;
procedure TIFFReverseBits(Cp: Pointer; N: tmsize_t); cdecl; external SLibName;
function  TIFFGetBitRevTable(Reversed: Integer): Pointer; cdecl; external SLibName;
{$ENDIF}

type
  TUserTiffErrorHandler = procedure(const Module, Message: AnsiString);

procedure SetUserMessageHandlers(ErrorHandler, WarningHandler: TUserTiffErrorHandler);
function IsVersion4: Boolean;

implementation

{$IFDEF FPC}
uses
  dynlibs;
{$ENDIF}

var
  UserTiffWarningHandler: TUserTiffErrorHandler;
  UserTiffErrorHandler: TUserTiffErrorHandler;

procedure SetUserMessageHandlers(ErrorHandler, WarningHandler: TUserTiffErrorHandler);
begin
  UserTiffErrorHandler := ErrorHandler;
  UserTiffWarningHandler := WarningHandler;
end;

procedure SetInternalMessageHandlers(ErrorHandler, WarningHandler: TIFFErrorHandler);
begin
  TIFFSetWarningHandler(@WarningHandler);
  TIFFSetErrorHandler(@ErrorHandler);
end;

const
{$IFDEF MSWINDOWS}
  SRuntimeLib = 'msvcrt.dll';
{$ELSE}
  SRuntimeLib = 'libc.so';
{$ENDIF}

function snprintf(S: PAnsiChar; N: Integer; const Format: PAnsiChar): Integer; cdecl; varargs; external SRuntimeLib name {$IFDEF MSWINDOWS}'_snprintf'{$ELSE}'snprintf'{$ENDIF};

procedure FormatAndCallHandler(Handler: TUserTiffErrorHandler; Module: PAnsiChar; Format: PAnsiChar; Params: va_list);
var
  Len: Integer;
  Buffer: array[0..511] of AnsiChar;
  Msg: AnsiString;
begin
  Len := snprintf(@Buffer, 512, Format, Params);
  SetString(Msg, Buffer, Len);
  Handler(Module, Msg);
end;

procedure InternalTIFFWarning(Module: PAnsiChar; Format: PAnsiChar; Params: va_list); cdecl;
begin
  if Assigned(UserTiffWarningHandler) then
    FormatAndCallHandler(UserTiffWarningHandler, Module, Format, Params);
end;

procedure InternallTIFFError(Module: PAnsiChar; Format: PAnsiChar; Params: va_list); cdecl;
begin
  if Assigned(UserTiffErrorHandler) then
    FormatAndCallHandler(UserTiffErrorHandler, Module, Format, Params);
end;

function IsVersion4: Boolean;
var
  Version: PAnsiChar;
begin
    Version := TIFFGetVersion;
    Result := Pos(AnsiString('Version 4'), Version) > 0;
end;

procedure CheckVersion;
begin
{$IFDEF UNIX}
  if not IsVersion4 then
    WriteLn('Warning: installed libtiff seems to be version 3.x. TIFF functions will probably fail. Install libtiff5 package to get libtiff 4.x.');
{$ENDIF}
end;

{$IFDEF DYNAMIC_DLL_LOADING}
var
  TiffLibHandle: {$IFDEF FPC}TLibHandle{$ELSE}THandle{$ENDIF} = 0;

function GetProcAddr(const AProcName: PAnsiChar): Pointer;
begin
  Result := GetProcAddress(TiffLibHandle, AProcName);
  if Addr(Result) = nil then begin
    RaiseLastOSError;
  end;
end;

function LoadTiffLibrary: Boolean;
begin
  Result := False;

  if TiffLibHandle = 0 then
  begin
    TiffLibHandle := LoadLibrary(SLibName);
  {$IF Defined(DARWIN)}
    if TiffLibHandle = 0 then
      TiffLibHandle := LoadLibrary('@executable_path/' + SLibName);
  {$IFEND}

    if TiffLibHandle <> 0 then
    begin
      TIFFGetVersion := GetProcAddr('TIFFGetVersion');
      TIFFOpen := GetProcAddr('TIFFOpen');
      TIFFClientOpen := GetProcAddr('TIFFClientOpen');
      TIFFClose := GetProcAddr('TIFFClose');
      TIFFSetFileno := GetProcAddr('TIFFSetFileno');
      TIFFSetField := GetProcAddr('TIFFSetField');
      TIFFGetField := GetProcAddr('TIFFGetField');
      TIFFGetFieldDefaulted := GetProcAddr('TIFFGetFieldDefaulted');
      TIFFReadRGBAImageOriented := GetProcAddr('TIFFReadRGBAImageOriented');
      TIFFReadScanline := GetProcAddr('TIFFReadScanline');
      TIFFWriteScanline := GetProcAddr('TIFFWriteScanline');
      TIFFScanlineSize := GetProcAddr('TIFFScanlineSize');
      TIFFDefaultStripSize := GetProcAddr('TIFFDefaultStripSize');
      TIFFNumberOfDirectories := GetProcAddr('TIFFNumberOfDirectories');
      TIFFSetDirectory := GetProcAddr('TIFFSetDirectory');
      TIFFWriteDirectory := GetProcAddr('TIFFWriteDirectory');
      TIFFReadEXIFDirectory := GetProcAddr('TIFFReadEXIFDirectory');
      TIFFSetErrorHandler := GetProcAddr('TIFFSetErrorHandler');
      TIFFSetWarningHandler := GetProcAddr('TIFFSetWarningHandler');

      SetInternalMessageHandlers(@InternallTIFFError, @InternalTIFFWarning);
      CheckVersion;

      Result := True;
    end;
  end;
end;

procedure FreeTiffLibrary;
begin
  if TiffLibHandle <> 0 then begin
    FreeLibrary(TiffLibHandle);
    TiffLibHandle := 0;
  end;
end;
{$ENDIF}

initialization
{$IFNDEF DYNAMIC_DLL_LOADING}
  SetInternalMessageHandlers(@InternallTIFFError, @InternalTIFFWarning);
  CheckVersion;
{$ENDIF}

finalization
{$IFDEF DYNAMIC_DLL_LOADING}
  FreeTiffLibrary;
{$ENDIF}
end.

