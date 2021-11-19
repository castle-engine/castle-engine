{
  Vampyre Imaging Library
  by Marek Mauder
  https://github.com/galfar/imaginglib
  https://imaginglib.sourceforge.io
  - - - - -
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0.
} 

{ This is basic unit of Elder Imagery extension for Vampyre Imaging Library.
  It adds support for loading and saving of images and textures from older
  Bethesda games (like TES2: Daggerfall, Redguard, Terminator: FS, TES: Arena, ...).
  This unit registers all file formats declared in additional ElderImagery units
  so its the only unit you need to add to uses clause of your project
  for Imaging to be able to load/save these new formats using standard
  loading/saving functions.}
unit ElderImagery;

{$I ImagingOptions.inc}

interface

uses
  ImagingTypes, Imaging;

type
  TElderFileFormat = class;
  TElderFileFormatClass = class of TElderFileFormat;

  { Used to hold information about some special images without headers.}
  TNoHeaderFileInfo = record
    Size: LongInt;
    Width: LongInt;
    Height: LongInt;
  end;

  { Basic class for image formats used mainly in TES2: Daggerfall.}
  TElderFileFormat = class(TImageFileFormat)
  protected
    FPalette: TPalette24Size256;
    FARGBPalette: PPalette32;
    procedure Define; override;
    { Decodes RLE compressed data.}
    procedure DagRLEDecode(InData: Pointer; OutSize: LongInt; var OutData: Pointer);
    function FindNoHeaderInfo(Size: LongInt; Infos: array of TNoHeaderFileInfo): LongInt;
    function TestNoHeaderFormat(Handle: TImagingHandle): TElderFileFormatClass;
    procedure ConvertPalette(const ElderPal: TPalette24Size256; ARGBPal: PPalette32);
    procedure SetPalette(const Value: TPalette24Size256);
    procedure ConvertToSupported(var Image: TImageData;
      const Info: TImageFormatInfo); override;
    function IsSupported(const Image: TImageData): Boolean; override;
  public
    destructor Destroy; override;
    function TestFormat(Handle: TImagingHandle): Boolean; override;
    { Current palette used when loading and saving images. Nearly all images
      in Daggerfall use external palettes. Change this property if you want
      images that don't use default palette to load correctly.}
    property Palette: TPalette24Size256 read FPalette write SetPalette;
  end;

  { Header of IMG and CIF files.}
  TImgHeader = packed record
    XOff: Word;
    YOff: Word;
    Width: Word;
    Height: Word;
    Unk: Word;        // Might indicate compressed data or not
    ImageSize: Word;  // Size of Image data (but not always)
  end;

const
  { This is default Daggerfall's palette (C:\Dagger\Arena2\pal.pal).
    Every TElderFileFormat descendant loads this pal in constructor.}
  DaggerfallPalette: TPalette24Size256 = (
    (B:   0; G:   0; R:   0), (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255),
    (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255),
    (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255),
    (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255),
    (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255),
    (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255),
    (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255),
    (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255),
    (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255),
    (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255),
    (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255), (B: 244; G: 202; R: 167),
    (B: 227; G: 180; R: 144), (B: 207; G: 152; R: 118), (B: 193; G: 133; R: 100),
    (B: 180; G: 113; R:  80), (B: 165; G: 100; R:  70), (B: 152; G:  93; R:  63),
    (B: 140; G:  86; R:  55), (B: 129; G:  79; R:  48), (B: 122; G:  75; R:  43),
    (B: 112; G:  70; R:  40), (B: 103; G:  64; R:  39), (B:  91; G:  67; R:  38),
    (B:  79; G:  63; R:  43), (B:  66; G:  54; R:  41), (B:  54; G:  50; R:  40),
    (B: 232; G: 196; R: 196), (B: 220; G: 177; R: 177), (B: 204; G: 157; R: 157),
    (B: 188; G: 138; R: 138), (B: 175; G: 122; R: 122), (B: 155; G: 105; R: 106),
    (B: 143; G:  94; R:  97), (B: 126; G:  81; R:  89), (B: 109; G:  72; R:  88),
    (B: 101; G:  68; R:  85), (B:  86; G:  61; R:  77), (B:  75; G:  55; R:  71),
    (B:  67; G:  51; R:  63), (B:  63; G:  47; R:  56), (B:  56; G:  45; R:  52),
    (B:  46; G:  44; R:  46), (B: 245; G: 212; R: 172), (B: 229; G: 193; R: 150),
    (B: 213; G: 174; R: 128), (B: 196; G: 154; R: 105), (B: 183; G: 140; R:  88),
    (B: 173; G: 127; R:  78), (B: 160; G: 118; R:  74), (B: 151; G: 110; R:  69),
    (B: 134; G: 103; R:  65), (B: 123; G:  92; R:  60), (B: 109; G:  85; R:  54),
    (B:  96; G:  76; R:  51), (B:  83; G:  71; R:  44), (B:  69; G:  63; R:  42),
    (B:  61; G:  54; R:  38), (B:  50; G:  45; R:  34), (B: 205; G: 205; R: 224),
    (B: 188; G: 188; R: 199), (B: 165; G: 165; R: 174), (B: 145; G: 145; R: 159),
    (B: 135; G: 135; R: 149), (B: 122; G: 122; R: 137), (B: 114; G: 114; R: 127),
    (B: 103; G: 103; R: 116), (B:  94; G:  94; R: 109), (B:  85; G:  85; R:  96),
    (B:  75; G:  75; R:  85), (B:  68; G:  68; R:  80), (B:  61; G:  61; R:  67),
    (B:  53; G:  53; R:  59), (B:  48; G:  48; R:  50), (B:  44; G:  44; R:  45),
    (B: 176; G: 205; R: 255), (B: 147; G: 185; R: 244), (B: 123; G: 164; R: 230),
    (B: 104; G: 152; R: 217), (B:  87; G: 137; R: 205), (B:  68; G: 124; R: 192),
    (B:  68; G: 112; R: 179), (B:  62; G: 105; R: 167), (B:  55; G:  97; R: 154),
    (B:  49; G:  90; R: 142), (B:  45; G:  82; R: 122), (B:  51; G:  77; R: 102),
    (B:  52; G:  69; R:  87), (B:  50; G:  62; R:  73), (B:  47; G:  59; R:  60),
    (B:  44; G:  48; R:  49), (B: 220; G: 220; R: 220), (B: 197; G: 197; R: 197),
    (B: 185; G: 185; R: 185), (B: 174; G: 174; R: 174), (B: 162; G: 162; R: 162),
    (B: 147; G: 147; R: 147), (B: 132; G: 132; R: 132), (B: 119; G: 119; R: 119),
    (B: 110; G: 110; R: 110), (B:  99; G:  99; R:  99), (B:  87; G:  87; R:  87),
    (B:  78; G:  78; R:  78), (B:  67; G:  67; R:  67), (B:  58; G:  58; R:  58),
    (B:  51; G:  51; R:  51), (B:  44; G:  44; R:  44), (B: 182; G: 218; R: 227),
    (B: 158; G: 202; R: 202), (B: 134; G: 187; R: 187), (B: 109; G: 170; R: 170),
    (B:  87; G: 154; R: 154), (B:  77; G: 142; R: 142), (B:  70; G: 135; R: 135),
    (B:  62; G: 124; R: 124), (B:  54; G: 112; R: 112), (B:  46; G: 103; R: 103),
    (B:  39; G:  91; R:  91), (B:  40; G:  83; R:  83), (B:  45; G:  72; R:  72),
    (B:  47; G:  63; R:  63), (B:  50; G:  55; R:  55), (B:  45; G:  48; R:  48),
    (B: 255; G: 246; R: 103), (B: 241; G: 238; R:  45), (B: 226; G: 220; R:   0),
    (B: 212; G: 203; R:   0), (B: 197; G: 185; R:   0), (B: 183; G: 168; R:   0),
    (B: 168; G: 150; R:   0), (B: 154; G: 133; R:   0), (B: 139; G: 115; R:   0),
    (B: 127; G: 106; R:   4), (B: 116; G:  97; R:   7), (B: 104; G:  87; R:  11),
    (B:  93; G:  78; R:  14), (B:  81; G:  69; R:  18), (B:  69; G:  60; R:  21),
    (B:  58; G:  51; R:  25), (B: 202; G: 221; R: 196), (B: 175; G: 200; R: 168),
    (B: 148; G: 176; R: 141), (B: 123; G: 156; R: 118), (B: 107; G: 144; R: 109),
    (B:  93; G: 130; R:  94), (B:  82; G: 116; R:  86), (B:  77; G: 110; R:  78),
    (B:  68; G:  99; R:  67), (B:  61; G:  89; R:  53), (B:  52; G:  77; R:  45),
    (B:  46; G:  68; R:  37), (B:  39; G:  60; R:  39), (B:  30; G:  55; R:  30),
    (B:  34; G:  51; R:  34), (B:  40; G:  47; R:  40), (B: 179; G: 107; R:  83),
    (B: 175; G:  95; R:  75), (B: 175; G:  87; R:  67), (B: 163; G:  79; R:  59),
    (B: 155; G:  75; R:  51), (B: 147; G:  71; R:  47), (B: 155; G:  91; R:  47),
    (B: 139; G:  83; R:  43), (B: 127; G:  75; R:  39), (B: 115; G:  67; R:  35),
    (B:  99; G:  63; R:  31), (B:  87; G:  55; R:  27), (B:  75; G:  47; R:  23),
    (B:  59; G:  39; R:  19), (B:  47; G:  31; R:  15), (B:  35; G:  23; R:  11),
    (B: 216; G: 227; R: 162), (B: 185; G: 205; R: 127), (B: 159; G: 183; R: 101),
    (B: 130; G: 162; R:  77), (B: 109; G: 146; R:  66), (B: 101; G: 137; R:  60),
    (B:  92; G: 127; R:  54), (B:  84; G: 118; R:  48), (B:  76; G: 108; R:  42),
    (B:  65; G:  98; R:  37), (B:  53; G:  87; R:  34), (B:  51; G:  75; R:  35),
    (B:  45; G:  64; R:  37), (B:  43; G:  56; R:  39), (B:  38; G:  51; R:  40),
    (B:  43; G:  46; R:  45), (B: 179; G: 115; R:  79), (B: 175; G: 111; R:  75),
    (B: 171; G: 107; R:  71), (B: 167; G: 103; R:  67), (B: 159; G:  99; R:  63),
    (B: 155; G:  95; R:  59), (B: 151; G:  91; R:  55), (B: 143; G:  87; R:  51),
    (B:  40; G:  40; R:  40), (B:  38; G:  38; R:  38), (B:  35; G:  35; R:  35),
    (B:  31; G:  31; R:  31), (B:  27; G:  27; R:  27), (B:  23; G:  23; R:  23),
    (B:  19; G:  19; R:  19), (B:  15; G:  15; R:  15), (B: 254; G: 255; R: 199),
    (B: 254; G: 245; R: 185), (B: 254; G: 235; R: 170), (B: 254; G: 225; R: 156),
    (B: 255; G: 215; R: 141), (B: 255; G: 205; R: 127), (B: 255; G: 195; R: 112),
    (B: 255; G: 185; R:  98), (B: 255; G: 175; R:  83), (B: 241; G: 167; R:  54),
    (B: 234; G: 155; R:  50), (B: 226; G: 143; R:  46), (B: 219; G: 131; R:  43),
    (B: 212; G: 119; R:  39), (B: 205; G: 107; R:  35), (B: 198; G:  95; R:  31),
    (B: 190; G:  84; R:  27), (B: 183; G:  72; R:  23), (B: 176; G:  60; R:  19),
    (B: 169; G:  48; R:  15), (B: 162; G:  36; R:  12), (B: 154; G:  24; R:   8),
    (B: 147; G:  12; R:   4), (B: 130; G:  22; R:   0), (B: 111; G:  34; R:   0),
    (B: 102; G:  33; R:   1), (B:  92; G:  33; R:   3), (B:  83; G:  32; R:  10),
    (B:  74; G:  39; R:  27), (B:  65; G:  41; R:  33), (B:  57; G:  43; R:  39),
    (B:  48; G:  45; R:  45));

  { This is default Redguard's palette (Redguard\fxart\Redguard.col).
    It is default palette for BSI image file format.}
  RedguardPalette: TPalette24Size256 = (
    (B:   0; G:   0; R:   0), (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255), 
    (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255), 
    (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255), 
    (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255), 
    (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255), 
    (B: 255; G:   0; R: 255), (B: 133; G: 196; R: 183), (B: 100; G: 181; R: 153), 
    (B:  66; G: 165; R: 124), (B:  33; G: 150; R:  94), (B:  31; G: 139; R:  87), 
    (B:  28; G: 127; R:  80), (B:  26; G: 116; R:  73), (B:  24; G: 105; R:  66), 
    (B:  21; G:  93; R:  59), (B:  19; G:  82; R:  52), (B:  17; G:  71; R:  45), 
    (B:  14; G:  59; R:  38), (B:  12; G:  48; R:  31), (B:  10; G:  37; R:  24), 
    (B:   7; G:  25; R:  17), (B:   5; G:  14; R:  10), (B: 230; G: 179; R: 142), 
    (B: 216; G: 155; R: 127), (B: 199; G: 151; R: 136), (B: 205; G: 134; R: 118), 
    (B: 199; G: 131; R: 103), (B: 191; G: 130; R: 108), (B: 202; G: 113; R:  95), 
    (B: 180; G: 112; R:  94), (B: 197; G:  95; R:  78), (B: 183; G: 106; R:  78), 
    (B: 174; G:  96; R:  75), (B: 160; G:  91; R:  63), (B: 166; G:  84; R:  55), 
    (B: 151; G:  91; R:  54), (B: 152; G:  75; R:  49), (B: 142; G:  81; R:  51), 
    (B: 216; G: 227; R: 162), (B: 155; G: 212; R: 109), (B:  95; G: 198; R:  57),
    (B:  34; G: 183; R:   4), (B:  32; G: 169; R:   4), (B:  29; G: 155; R:   4), 
    (B:  27; G: 141; R:   4), (B:  25; G: 127; R:   4), (B:  22; G: 113; R:   4), 
    (B:  20; G: 100; R:   4), (B:  18; G:  86; R:   3), (B:  15; G:  72; R:   3), 
    (B:  13; G:  58; R:   3), (B:  11; G:  44; R:   3), (B:   8; G:  30; R:   3), 
    (B:   6; G:  16; R:   3), (B: 134; G:  72; R:  57), (B: 132; G:  71; R:  47), 
    (B: 122; G:  75; R:  51), (B: 123; G:  61; R:  44), (B: 119; G:  59; R:  37), 
    (B: 103; G:  55; R:  41), (B: 104; G:  47; R:  31), (B:  98; G:  47; R:  27), 
    (B:  91; G:  45; R:  33), (B:  83; G:  42; R:  34), (B:  75; G:  40; R:  24),
    (B:  80; G:  33; R:  22), (B:  63; G:  29; R:  24), (B:  66; G:  24; R:  16), 
    (B:  51; G:  27; R:  24), (B:  40; G:  24; R:  24), (B: 255; G: 246; R: 103), 
    (B: 241; G: 238; R:  45), (B: 235; G: 247; R:   0), (B: 228; G: 228; R:   3), 
    (B: 204; G: 207; R:   1), (B: 189; G: 187; R:   2), (B: 173; G: 166; R:   2), 
    (B: 158; G: 146; R:   3), (B: 142; G: 126; R:   3), (B: 127; G: 106; R:   4), 
    (B: 114; G:  97; R:   9), (B:  96; G:  81; R:   7), (B:  75; G:  63; R:   6), 
    (B:  53; G:  47; R:   6), (B:  35; G:  31; R:   6), (B:  19; G:  18; R:   6), 
    (B: 184; G: 116; R:  83), (B: 175; G:  96; R:  57), (B: 166; G:  75; R:  30), 
    (B: 157; G:  55; R:   4), (B: 145; G:  51; R:   4), (B: 133; G:  47; R:   4), 
    (B: 122; G:  43; R:   4), (B: 110; G:  39; R:   3), (B:  98; G:  35; R:   3), 
    (B:  86; G:  31; R:   3), (B:  74; G:  26; R:   3), (B:  62; G:  22; R:   3), 
    (B:  51; G:  18; R:   3), (B:  39; G:  14; R:   2), (B:  27; G:  10; R:   2), 
    (B:  15; G:   6; R:   2), (B: 255; G: 255; R: 184), (B: 255; G: 241; R: 137), 
    (B: 255; G: 226; R:  90), (B: 255; G: 212; R:  43), (B: 240; G: 189; R:  39), 
    (B: 225; G: 166; R:  35), (B: 211; G: 144; R:  30), (B: 196; G: 121; R:  26), 
    (B: 181; G:  98; R:  22), (B: 163; G:  92; R:  20), (B: 127; G:  73; R:  15), 
    (B: 105; G:  60; R:  13), (B:  83; G:  46; R:  12), (B:  61; G:  33; R:  10), 
    (B:  39; G:  20; R:   8), (B:  26; G:  15; R:   9), (B: 252; G: 203; R: 179), 
    (B: 245; G: 189; R: 158), (B: 222; G: 167; R: 133), (B: 196; G: 147; R: 111), 
    (B: 186; G: 134; R:  91), (B: 174; G: 125; R:  81), (B: 161; G: 118; R:  78), 
    (B: 147; G: 110; R:  72), (B: 136; G: 102; R:  65), (B: 122; G:  93; R:  59), 
    (B: 110; G:  85; R:  55), (B:  98; G:  79; R:  53), (B:  85; G:  69; R:  46), 
    (B:  66; G:  54; R:  37), (B:  46; G:  40; R:  29), (B:  27; G:  25; R:  20), 
    (B: 228; G: 133; R: 133), (B: 225; G:  96; R:  94), (B: 222; G:  58; R:  55), 
    (B: 219; G:  21; R:  16), (B: 202; G:  20; R:  15), (B: 185; G:  18; R:  14), 
    (B: 167; G:  17; R:  13), (B: 150; G:  16; R:  12), (B: 133; G:  14; R:  11), 
    (B: 116; G:  13; R:  11), (B:  98; G:  12; R:  10), (B:  81; G:  10; R:   9),
    (B:  64; G:   9; R:   8), (B:  47; G:   8; R:   7), (B:  29; G:   6; R:   6), 
    (B:  12; G:   5; R:   5), (B: 255; G: 255; R: 255), (B: 240; G: 240; R: 240), 
    (B: 220; G: 220; R: 220), (B: 201; G: 201; R: 201), (B: 181; G: 181; R: 181), 
    (B: 162; G: 162; R: 162), (B: 148; G: 148; R: 148), (B: 135; G: 135; R: 135), 
    (B: 121; G: 121; R: 121), (B: 108; G: 108; R: 108), (B:  90; G:  90; R:  90), 
    (B:  74; G:  74; R:  74), (B:  58; G:  58; R:  58), (B:  42; G:  42; R:  42), 
    (B:  22; G:  22; R:  22), (B:   8; G:   8; R:   8), (B: 104; G: 150; R: 233), 
    (B:  93; G: 125; R: 242), (B:  82; G:  98; R: 249), (B:  72; G:  72; R: 255), 
    (B:  48; G:  48; R: 255), (B:  25; G:  25; R: 254), (B:   7; G:   7; R: 246), 
    (B:   7; G:   7; R: 220), (B:   6; G:   6; R: 194), (B:   6; G:   6; R: 169), 
    (B:   5; G:   5; R: 143), (B:   5; G:   5; R: 117), (B:   4; G:   4; R:  91), 
    (B:   4; G:   4; R:  66), (B:   3; G:   3; R:  40), (B:   3; G:   3; R:  14), 
    (B: 191; G:  88; R: 117), (B: 180; G:  63; R:  97), (B: 169; G:  38; R:  78), 
    (B: 159; G:  14; R:  56), (B: 147; G:  13; R:  52), (B: 135; G:  12; R:  48), 
    (B: 123; G:  12; R:  44), (B: 111; G:  11; R:  40), (B:  99; G:  10; R:  36),
    (B:  87; G:   9; R:  32), (B:  75; G:   8; R:  27), (B:  63; G:   7; R:  23),
    (B:  51; G:   7; R:  19), (B:  39; G:   6; R:  15), (B:  27; G:   5; R:  11),
    (B:  15; G:   4; R:   7), (B: 135; G: 224; R: 255), (B:  91; G: 213; R: 255),
    (B:  46; G: 197; R: 255), (B:   2; G: 184; R: 255), (B:   2; G: 170; R: 235),
    (B:   2; G: 156; R: 215), (B:   2; G: 141; R: 195), (B:   2; G: 127; R: 175),
    (B:   2; G: 113; R: 155), (B:   3; G:  99; R: 136), (B:   3; G:  84; R: 116),
    (B:   3; G:  70; R:  96), (B:   3; G:  56; R:  76), (B:   3; G:  42; R:  56),
    (B:   3; G:  27; R:  36), (B:   3; G:  13; R:  16), (B: 254; G: 255; R: 199),
    (B: 254; G: 235; R: 170), (B: 255; G: 215; R: 141), (B: 255; G: 205; R: 127),
    (B: 255; G: 195; R: 112), (B: 255; G: 175; R:  83), (B: 234; G: 155; R:  50),
    (B: 219; G: 131; R:  43), (B: 205; G: 107; R:  35), (B: 190; G:  84; R:  27),
    (B: 176; G:  60; R:  19), (B: 155; G:  24; R:  10), (B: 130; G:  21; R:   9),
    (B: 105; G:  19; R:   8), (B:  80; G:  16; R:   7), (B:  55; G:  13; R:   6),
    (B: 197; G: 215; R: 255), (B: 181; G: 196; R: 233), (B: 165; G: 177; R: 212),
    (B: 149; G: 158; R: 190), (B: 138; G: 146; R: 176), (B: 126; G: 134; R: 162),
    (B: 115; G: 122; R: 147), (B: 103; G: 110; R: 133), (B:  92; G:  98; R: 119),
    (B:  81; G:  87; R: 105), (B:  69; G:  75; R:  90), (B:  58; G:  63; R:  76),
    (B:  46; G:  51; R:  62), (B:  35; G:  39; R:  48), (B:  23; G:  27; R:  33),
    (B:  12; G:  15; R:  19));

  { This is default Arena's palette (Arena\pal.col).}
  ArenaPalette: TPalette24Size256 = (
    (B:   0; G:   0; R:   0), (B:   0; G:   0; R: 170), (B:   0; G: 170; R:   0),
    (B:   0; G: 170; R: 170), (B: 170; G:   0; R:   0), (B: 170; G:   0; R: 170),
    (B: 170; G:  85; R:   0), (B: 170; G: 170; R: 170), (B:  85; G:  85; R:  85),
    (B:  85; G:  85; R: 255), (B:  85; G: 255; R:  85), (B:  85; G: 255; R: 255),
    (B: 255; G:  85; R:  85), (B: 255; G:  85; R: 255), (B: 255; G: 255; R:  85),
    (B: 255; G: 255; R: 255), (B: 212; G: 232; R: 248), (B: 193; G: 211; R: 227),
    (B: 174; G: 190; R: 205), (B: 155; G: 169; R: 184), (B: 136; G: 148; R: 163),
    (B: 118; G: 128; R: 142), (B:  99; G: 107; R: 120), (B:  80; G:  86; R:  99),
    (B:  61; G:  65; R:  78), (B:  42; G:  44; R:  56), (B:   0; G: 180; R:   0),
    (B:   0; G: 160; R:   0), (B:   0; G: 144; R:   0), (B: 144; G: 184; R:   0),
    (B: 124; G: 160; R:   0), (B: 108; G: 140; R:   0), (B: 175; G: 175; R: 187),
    (B: 160; G: 160; R: 172), (B: 145; G: 145; R: 157), (B: 129; G: 129; R: 141),
    (B: 114; G: 114; R: 126), (B:  99; G:  99; R: 111), (B:  84; G:  84; R:  96),
    (B:  69; G:  69; R:  81), (B:  53; G:  53; R:  65), (B:  38; G:  38; R:  50),
    (B: 139; G: 127; R: 127), (B: 127; G: 117; R: 118), (B: 116; G: 106; R: 109),
    (B: 104; G:  96; R:  99), (B:  93; G:  85; R:  90), (B:  81; G:  75; R:  81),
    (B:  69; G:  65; R:  72), (B:  58; G:  54; R:  63), (B:  46; G:  44; R:  53),
    (B:  35; G:  33; R:  44), (B: 127; G: 127; R: 139), (B: 117; G: 117; R: 129),
    (B: 106; G: 106; R: 118), (B:  96; G:  96; R: 108), (B:  85; G:  85; R:  97),
    (B:  75; G:  75; R:  87), (B:  65; G:  65; R:  77), (B:  54; G:  54; R:  66), 
    (B:  44; G:  44; R:  56), (B:  33; G:  33; R:  45), (B:   0; G:   0; R: 203), 
    (B:   0; G:   0; R: 175), (B:  30; G:  97; R: 134), (B:  29; G:  90; R: 124), 
    (B:  29; G:  82; R: 114), (B:  28; G:  75; R: 104), (B:  27; G:  67; R:  94), 
    (B:  27; G:  60; R:  85), (B:  26; G:  53; R:  75), (B:  25; G:  45; R:  65), 
    (B:  24; G:  38; R:  55), (B:  24; G:  30; R:  45), (B:   0; G: 127; R: 127), 
    (B:   2; G: 117; R: 118), (B:   5; G: 106; R: 109), (B:   7; G:  96; R:  99), 
    (B:   9; G:  85; R:  90), (B:  12; G:  75; R:  81), (B:  14; G:  65; R:  72), 
    (B:  16; G:  54; R:  63), (B:  18; G:  44; R:  53), (B:  21; G:  33; R:  44), 
    (B:  75; G:  92; R:  95), (B:  70; G:  85; R:  89), (B:  65; G:  78; R:  83), 
    (B:  59; G:  71; R:  77), (B:  54; G:  64; R:  71), (B:  49; G:  58; R:  65), 
    (B:  44; G:  51; R:  59), (B:  39; G:  44; R:  53), (B:  33; G:  37; R:  47), 
    (B:  28; G:  30; R:  41), (B: 187; G:  39; R: 239), (B: 195; G:   0; R: 199), 
    (B: 231; G: 215; R:   0), (B: 255; G: 167; R:   0), (B: 223; G: 119; R:   0), 
    (B: 231; G:  83; R:   0), (B: 139; G: 139; R: 150), (B: 111; G: 111; R: 123), 
    (B:  95; G:  95; R: 107), (B:  79; G:  79; R:  91), (B:  63; G:  63; R:  75), 
    (B:  51; G:  51; R:  59), (B:  43; G:  43; R:  51), (B:  39; G:  39; R:  47),
    (B:  31; G:  31; R:  43), (B:  27; G:  27; R:  39), (B:  23; G:  23; R:  35), 
    (B:  19; G:  19; R:  31), (B:  15; G:  15; R:  27), (B: 255; G: 255; R: 255), 
    (B: 255; G: 255; R: 255), (B:  30; G:   9; R:   1), (B: 112; G: 112; R: 112), 
    (B: 103; G: 103; R: 104), (B:  94; G:  94; R:  97), (B:  85; G:  85; R:  89), 
    (B:  76; G:  76; R:  81), (B:  68; G:  68; R:  74), (B:  59; G:  59; R:  66), 
    (B:  50; G:  50; R:  58), (B:  41; G:  41; R:  50), (B:  32; G:  32; R:  43), 
    (B:   2; G: 221; R: 221), (B:   0; G: 175; R: 175), (B: 155; G:  51; R:  51), 
    (B: 142; G:  48; R:  49), (B: 129; G:  45; R:  48), (B: 115; G:  43; R:  46), 
    (B: 102; G:  40; R:  45), (B:  89; G:  37; R:  43), (B:  76; G:  34; R:  41), 
    (B:  63; G:  31; R:  40), (B:  49; G:  29; R:  38), (B:  36; G:  26; R:  37), 
    (B: 127; G:   0; R:   0), (B: 117; G:   2; R:   4), (B: 106; G:   5; R:   7), 
    (B:  96; G:   7; R:  11), (B:  85; G:   9; R:  14), (B:  75; G:  12; R:  18), 
    (B:  65; G:  14; R:  21), (B:  54; G:  16; R:  25), (B:  44; G:  18; R:  28),
    (B:  33; G:  21; R:  32), (B:  78; G:  61; R:  48), (B:  73; G:  57; R:  47), 
    (B:  67; G:  53; R:  45), (B:  62; G:  50; R:  44), (B:  56; G:  46; R:  43), 
    (B:  51; G:  42; R:  42), (B:  45; G:  38; R:  40), (B:  40; G:  34; R:  39), 
    (B:  34; G:  31; R:  38), (B:  29; G:  27; R:  36), (B: 225; G:   2; R:   2), 
    (B: 195; G:   0; R:   0), (B:   0; G: 127; R:   0), (B:   2; G: 117; R:   4), 
    (B:   5; G: 106; R:   7), (B:   7; G:  96; R:  11), (B:   9; G:  85; R:  14), 
    (B:  12; G:  75; R:  18), (B:  14; G:  65; R:  21), (B:  16; G:  54; R:  25), 
    (B:  18; G:  44; R:  28), (B:  21; G:  33; R:  32), (B:  55; G:  63; R:  39), 
    (B:  52; G:  59; R:  39), (B:  49; G:  55; R:  38), (B:  45; G:  51; R:  38), 
    (B:  42; G:  47; R:  37), (B:  39; G:  43; R:  37), (B:  36; G:  39; R:  37), 
    (B:  33; G:  35; R:  36), (B:  29; G:  31; R:  36), (B:  26; G:  27; R:  35), 
    (B: 158; G: 176; R: 195), (B: 145; G: 161; R: 179), (B: 131; G: 145; R: 163), 
    (B: 118; G: 130; R: 147), (B: 104; G: 115; R: 131), (B:  91; G: 100; R: 115), 
    (B:  77; G:  84; R:  99), (B:  64; G:  69; R:  83), (B:  50; G:  54; R:  67), 
    (B:  37; G:  38; R:  51), (B:  56; G:  25; R:  25), (B:  36; G:  20; R:  26), 
    (B: 215; G: 159; R:   7), (B: 196; G: 145; R:  10), (B: 177; G: 132; R:  13), 
    (B: 157; G: 118; R:  15), (B: 138; G: 105; R:  18), (B: 119; G:  91; R:  21), 
    (B: 100; G:  77; R:  24), (B:  81; G:  64; R:  27), (B:  61; G:  50; R:  29), 
    (B:  42; G:  37; R:  32), (B: 139; G: 115; R:   0), (B: 127; G: 106; R:   4), 
    (B: 116; G:  97; R:   7), (B: 104; G:  87; R:  11), (B:  93; G:  78; R:  14), 
    (B:  81; G:  69; R:  18), (B:  69; G:  60; R:  21), (B:  58; G:  51; R:  25), 
    (B:  46; G:  41; R:  28), (B:  35; G:  32; R:  32), (B: 151; G:  99; R:   0), 
    (B: 138; G:  91; R:   4), (B: 125; G:  84; R:   7), (B: 113; G:  76; R:  11), 
    (B: 100; G:  69; R:  14), (B:  87; G:  61; R:  18), (B:  74; G:  53; R:  21), 
    (B:  61; G:  46; R:  25), (B:  49; G:  38; R:  28), (B:  36; G:  31; R:  32), 
    (B: 254; G: 170; R:   0), (B: 255; G: 184; R:   0), (B: 211; G: 203; R: 179), 
    (B: 208; G: 195; R: 167), (B: 205; G: 186; R: 155), (B: 212; G: 178; R: 143), 
    (B: 200; G: 163; R: 131), (B: 187; G: 148; R: 119), (B: 183; G: 133; R: 107), 
    (B: 170; G: 118; R:  95), (B: 156; G: 104; R:  84), (B: 143; G:  89; R:  72), 
    (B: 126; G:  74; R:  60), (B: 103; G:  59; R:  48), (B:  90; G:  45; R:  36), 
    (B:  77; G:  30; R:  24), (B:  64; G:  15; R:  12), (B:  41; G:   0; R:   0), 
    (B: 212; G: 120; R:   8), (B: 209; G: 111; R:   9), (B: 206; G: 102; R:  10), 
    (B: 204; G:  92; R:  10), (B: 201; G:  83; R:  11), (B: 198; G:  74; R:  12), 
    (B: 195; G:  65; R:  13), (B: 192; G:  56; R:  14), (B: 190; G:  46; R:  14), 
    (B: 187; G:  37; R:  15), (B: 184; G:  28; R:  16), (B:   0; G:   0; R:  60),
    (B: 251; G: 239; R:  79), (B: 191; G: 115; R:   0), (B: 197; G: 197; R: 197), 
    (B:  52; G:  52; R:  52));

  { This is default Terminator Future Shock's palette (Shock\Gamedata\Shock.col).}
  FutureShockPalette: TPalette24Size256 = (
    (B:   0; G:   0; R:   0), (B: 255; G: 255; R: 255), (B: 255; G: 255; R: 211), 
    (B: 255; G: 255; R: 177), (B: 255; G: 255; R: 127), (B: 255; G: 255; R:  97), 
    (B: 255; G: 210; R:  67), (B: 255; G: 166; R:  55), (B: 255; G:   0; R:   0), 
    (B: 255; G: 131; R:   0), (B:   0; G: 255; R:   0), (B:  71; G:  71; R: 255), 
    (B: 255; G: 255; R:   0), (B: 254; G: 137; R:  46), (B: 216; G: 111; R:  37), 
    (B: 177; G:  88; R:  29), (B:  51; G:  55; R:  55), (B:  55; G:  51; R:  55), 
    (B:  51; G:  51; R:  53), (B:  51; G:  51; R:  54), (B:  59; G:  51; R:  63), 
    (B:  59; G:  51; R:  54), (B:  51; G:  55; R:  57), (B:  51; G:  51; R:  51), 
    (B: 239; G:  51; R: 239), (B: 239; G:  51; R: 239), (B: 239; G:  51; R: 239), 
    (B: 239; G:  51; R: 239), (B:  64; G:  41; R:  38), (B:  49; G:  33; R:  32), 
    (B:  33; G:  26; R:  27), (B:  18; G:  18; R:  21), (B: 191; G: 239; R: 211), 
    (B: 179; G: 223; R: 195), (B: 163; G: 207; R: 179), (B: 147; G: 191; R: 159), 
    (B: 131; G: 175; R: 143), (B: 115; G: 155; R: 127), (B: 103; G: 139; R: 111), 
    (B:  91; G: 131; R: 103), (B:  83; G: 119; R:  91), (B:  75; G: 107; R:  83), 
    (B:  67; G:  95; R:  71), (B:  63; G:  87; R:  67), (B:  59; G:  79; R:  63), 
    (B:  55; G:  71; R:  59), (B:  55; G:  63; R:  55), (B:  51; G:  55; R:  51), 
    (B: 227; G: 203; R: 203), (B: 211; G: 187; R: 187), (B: 191; G: 175; R: 171), 
    (B: 175; G: 159; R: 159), (B: 159; G: 143; R: 143), (B: 143; G: 127; R: 127), 
    (B: 127; G: 111; R: 111), (B: 115; G:  99; R:  99), (B: 103; G:  91; R:  91), 
    (B:  91; G:  83; R:  83), (B:  83; G:  71; R:  71), (B:  75; G:  67; R:  67), 
    (B:  71; G:  63; R:  63), (B:  67; G:  59; R:  59), (B:  63; G:  55; R:  55), 
    (B:  59; G:  51; R:  51), (B: 179; G: 235; R: 247), (B: 163; G: 219; R: 231), 
    (B: 147; G: 203; R: 219), (B: 131; G: 187; R: 203), (B: 115; G: 171; R: 191), 
    (B:  99; G: 155; R: 175), (B:  83; G: 139; R: 163), (B:  75; G: 127; R: 151), 
    (B:  67; G: 115; R: 139), (B:  59; G: 103; R: 123), (B:  51; G:  91; R: 111), 
    (B:  51; G:  83; R:  99), (B:  51; G:  79; R:  91), (B:  51; G:  71; R:  79), 
    (B:  51; G:  63; R:  67), (B:  51; G:  55; R:  55), (B: 207; G: 207; R: 215), 
    (B: 191; G: 191; R: 199), (B: 175; G: 179; R: 187), (B: 163; G: 163; R: 171),
    (B: 147; G: 151; R: 155), (B: 131; G: 135; R: 143), (B: 119; G: 119; R: 127), 
    (B: 107; G: 111; R: 115), (B:  99; G: 103; R: 107), (B:  87; G:  91; R:  95),
    (B:  79; G:  83; R:  83), (B:  71; G:  75; R:  79), (B:  67; G:  71; R:  71), 
    (B:  63; G:  63; R:  67), (B:  55; G:  59; R:  59), (B:  51; G:  55; R:  55),
    (B: 231; G: 211; R: 171), (B: 215; G: 195; R: 155), (B: 199; G: 179; R: 143), 
    (B: 187; G: 159; R: 127), (B: 171; G: 143; R: 111), (B: 155; G: 127; R:  95), 
    (B: 139; G: 107; R:  83), (B: 131; G:  99; R:  75), (B: 119; G:  87; R:  67), 
    (B: 107; G:  75; R:  59), (B:  95; G:  67; R:  51), (B:  87; G:  63; R:  51), 
    (B:  79; G:  59; R:  51), (B:  71; G:  59; R:  51), (B:  63; G:  55; R:  51), 
    (B:  55; G:  51; R:  51), (B: 140; G:  47; R:  47), (B: 179; G:  54; R:  54), 
    (B: 255; G:  99; R:   0), (B: 255; G: 191; R:   0), (B: 151; G:  78; R:  26), 
    (B: 112; G:  70; R:  41), (B:  94; G:  57; R:  53), (B:  64; G:  41; R:  38), 
    (B:  47; G:  47; R:  52), (B:  43; G:  43; R:  49), (B:  38; G:  38; R:  44), 
    (B:  35; G:  35; R:  40), (B:  31; G:  31; R:  36), (B:  27; G:  27; R:  30), 
    (B:  22; G:  22; R:  27), (B:  18; G:  18; R:  21), (B: 175; G: 219; R: 219), 
    (B: 131; G: 231; R: 231), (B:  95; G: 231; R: 231), (B:  51; G: 239; R: 239), 
    (B:  51; G: 235; R: 235), (B:  51; G: 219; R: 219), (B:  51; G: 199; R: 199), 
    (B:  51; G: 175; R: 179), (B:  51; G: 159; R: 163), (B:  51; G: 139; R: 143), 
    (B:  51; G: 119; R: 123), (B:  51; G:  99; R: 107), (B:  51; G:  87; R:  91),
    (B:  51; G:  71; R:  79), (B:  51; G:  55; R:  63), (B:  51; G:  51; R:  51), 
    (B: 219; G: 219; R: 175), (B: 231; G: 231; R: 131), (B: 231; G: 231; R:  95), 
    (B: 239; G: 239; R:  51), (B: 235; G: 235; R:  51), (B: 219; G: 219; R:  51), 
    (B: 199; G: 199; R:  51), (B: 179; G: 175; R:  51), (B: 163; G: 159; R:  51), 
    (B: 143; G: 139; R:  51), (B: 123; G: 119; R:  51), (B: 107; G:  99; R:  51), 
    (B:  91; G:  87; R:  51), (B:  79; G:  71; R:  51), (B:  63; G:  55; R:  51), 
    (B:  51; G:  51; R:  51), (B: 219; G: 175; R: 219), (B: 231; G: 131; R: 231), 
    (B: 231; G:  95; R: 231), (B: 239; G:  51; R: 239), (B: 235; G:  51; R: 235), 
    (B: 219; G:  51; R: 219), (B: 199; G:  51; R: 199), (B: 179; G:  51; R: 179), 
    (B: 163; G:  51; R: 159), (B: 143; G:  51; R: 139), (B: 123; G:  51; R: 119), 
    (B: 107; G:  51; R:  99), (B:  91; G:  51; R:  87), (B:  79; G:  51; R:  71), 
    (B:  63; G:  51; R:  55), (B:  51; G:  51; R:  51), (B: 175; G: 219; R: 175), 
    (B: 131; G: 231; R: 131), (B:  99; G: 231; R:  99), (B:  55; G: 235; R:  55), 
    (B:  55; G: 231; R:  55), (B:  55; G: 211; R:  55), (B:  63; G: 187; R:  63), 
    (B:  71; G: 159; R:  71), (B:  67; G: 143; R:  67), (B:  67; G: 127; R:  67), 
    (B:  63; G: 111; R:  63), (B:  59; G:  99; R:  59), (B:  59; G:  83; R:  59), 
    (B:  55; G:  71; R:  55), (B:  55; G:  59; R:  55), (B:  51; G:  55; R:  51), 
    (B: 143; G: 143; R: 223), (B: 123; G: 123; R: 235), (B:  95; G:  95; R: 243),
    (B:  59; G:  59; R: 255), (B:  55; G:  55; R: 235), (B:  59; G:  59; R: 211), 
    (B:  63; G:  63; R: 191), (B:  67; G:  67; R: 167), (B:  63; G:  63; R: 151),
    (B:  63; G:  63; R: 131), (B:  59; G:  59; R: 119), (B:  55; G:  55; R: 103), 
    (B:  55; G:  55; R:  91), (B:  55; G:  55; R:  75), (B:  51; G:  51; R:  63), 
    (B:  51; G:  51; R:  55), (B: 219; G: 131; R: 131), (B: 235; G: 111; R: 111), 
    (B: 239; G:  95; R:  95), (B: 243; G:  67; R:  67), (B: 235; G:  51; R:  51), 
    (B: 215; G:  51; R:  51), (B: 199; G:  55; R:  55), (B: 179; G:  51; R:  51), 
    (B: 163; G:  51; R:  51), (B: 143; G:  51; R:  51), (B: 123; G:  51; R:  51), 
    (B: 107; G:  51; R:  51), (B:  91; G:  51; R:  51), (B:  79; G:  51; R:  51), 
    (B:  63; G:  51; R:  51), (B:  51; G:  51; R:  51), (B: 203; G: 187; R: 227), 
    (B: 191; G: 175; R: 211), (B: 175; G: 163; R: 195), (B: 159; G: 147; R: 179), 
    (B: 147; G: 135; R: 167), (B: 131; G: 123; R: 151), (B: 119; G: 107; R: 135), 
    (B: 107; G:  99; R: 123), (B:  99; G:  91; R: 111), (B:  91; G:  83; R: 103), 
    (B:  79; G:  71; R:  91), (B:  75; G:  67; R:  83), (B:  71; G:  63; R:  79), 
    (B:  67; G:  59; R:  71), (B:  63; G:  55; R:  67), (B:  59; G:  51; R:  63), 
    (B: 183; G: 219; R: 227), (B: 167; G: 203; R: 211), (B: 151; G: 187; R: 191), 
    (B: 135; G: 167; R: 175), (B: 119; G: 151; R: 155), (B:  99; G: 135; R: 139), 
    (B:  83; G: 119; R: 119), (B:  75; G: 107; R: 111), (B:  67; G:  95; R:  99), 
    (B:  67; G:  91; R:  91), (B:  63; G:  83; R:  83), (B:  59; G:  75; R:  79), 
    (B:  59; G:  71; R:  71), (B:  55; G:  63; R:  63), (B:  51; G:  59; R:  59), 
    (B:  49; G:  51; R:  51));

implementation

uses
  Types, 
  SysUtils,
  Classes,
  ImagingIO,
  ImagingUtility,
  ElderImageryBsi,
  ElderImageryCif,
  ElderImageryImg,
  ElderImageryTexture,
  ElderImagerySky;

{ TDaggerfallFileFormat class implementation }

procedure TElderFileFormat.Define;
begin
  inherited;
  FFeatures := [ffLoad, ffSave, ffMultiImage];
  FSupportedFormats := [];

  GetMem(FARGBPalette, Length(FPalette) * SizeOf(TColor32Rec));
  SetPalette(DaggerfallPalette);
end;

destructor TElderFileFormat.Destroy;
begin
  FreeMem(FARGBPalette);
  inherited Destroy;
end;

procedure TElderFileFormat.DagRLEDecode(InData: Pointer; OutSize: LongInt;
  var OutData: Pointer);
var
  I, Pos, CByte: LongInt;
  Rle, B: Byte;
begin
  Pos := 0;
  CByte := 0;
  while Pos < OutSize do
  begin
    Rle := PByteArray(InData)[CByte];
    CByte := CByte + 1;
    if Rle < 128 then
    begin
      Rle := Rle + 1;
      Move(PByteArray(InData)[CByte], PByteArray(OutData)[Pos], Rle);
      CByte := CByte + Rle;
      Pos := Pos + Rle;
    end
    else
    begin
      Rle := Rle - 127;
      B := PByteArray(InData)[CByte];;
      CByte := CByte + 1;
      for I := 0 to Rle - 1 do
      begin
        PByteArray(OutData)[Pos] := B;
        Pos := Pos + 1;
      end;
    end;
  end;
end;

function TElderFileFormat.FindNoHeaderInfo(Size: LongInt;
  Infos: array of TNoHeaderFileInfo): LongInt;
var
  I: LongInt;
begin
  for I := Low(Infos) to High(Infos) do
  begin
    if Size = Infos[I].Size then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

function TElderFileFormat.TestNoHeaderFormat(Handle: TImagingHandle): TElderFileFormatClass;
var
  InputSize, I: LongInt;
begin
  Result := nil;
  if Handle <> nil then
  begin
    InputSize := GetInputSize(GetIO, Handle);
    // Check special IMG files
    I := FindNoHeaderInfo(InputSize, NoHeaderIMGInfos);
    if I >= 0 then
    begin
      Result := TIMGFileFormat;
      Exit;
    end;
    // Check special CIF files
    I := FindNoHeaderInfo(InputSize, NoHeaderCIFInfos);
    if I >= 0 then
    begin
      Result := TCIFFileFormat;
      Exit;
    end;
  end;
end;

procedure TElderFileFormat.ConvertPalette(const ElderPal: TPalette24Size256;
  ARGBPal: PPalette32);
var
  I: LongInt;
begin
  for I := Low(ElderPal) to High(ElderPal) do
  begin
    ARGBPal[I].A := $FF;
    ARGBPal[I].R := ElderPal[I].B;
    ARGBPal[I].G := ElderPal[I].G;
    ARGBPal[I].B := ElderPal[I].R;
  end;
  // Palette index 0 represents transparent color
  ARGBPal[0].A := 0;
end;

procedure TElderFileFormat.SetPalette(const Value: TPalette24Size256);
begin
  FPalette := Value;
  ConvertPalette(FPalette, FARGBPalette);
end;

procedure TElderFileFormat.ConvertToSupported(var Image: TImageData;
  const Info: TImageFormatInfo);
var
  R: TRect;
begin
  if CanSave then
  begin
    if Image.Width * Image.Height > 65535 then
    begin
      // Regular CIF and IMG files can only store images no larger than 65535 bytes
      if Image.Width > Image.Height then
        R := Rect(0, 0, 320, 200)
      else
        R := Rect(0, 0, 200, 320);
      R := ScaleRectToRect(Rect(0, 0, Image.Width, Image.Height), R);
      ResizeImage(Image, R.Right - R.Left, R.Bottom - R.Top, rfBilinear);
    end;
    // Map image to current palette
    MapImageToPalette(Image, FARGBPalette, Length(FPalette));
  end;
end;

function TElderFileFormat.IsSupported(const Image: TImageData): Boolean;
begin
  // Image is supported for saving if its indexed and is mapped to current palette
  Result := (Image.Format = ifIndex8) and
    CompareMem(Image.Palette, FARGBPalette, Length(FPalette) * SizeOf(TColor32Rec));
end;

function TElderFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  Hdr: TImgHeader;
  DagClass: TElderFileFormatClass;
  ReadCount: LongInt;
begin
  // TestFormat for both IMG and CIF formats
  Result := False;
  DagClass := TestNoHeaderFormat(Handle);
  if (DagClass = nil) and (Handle <> nil) then
  begin
    // Check ordinary IMG/CIF files with header
    ReadCount := GetIO.Read(Handle, @Hdr, SizeOf(Hdr));
    GetIO.Seek(Handle, -ReadCount, smFromCurrent);
    Result := (ReadCount > 0) and (Hdr.ImageSize <= Hdr.Width * Hdr.Height) and
      (Hdr.Width * Hdr.Height <= High(Word)) and (Hdr.ImageSize <> 0) and
      (Hdr.Width <> 0) and (Hdr.Height <> 0);
    if IsMultiImageFormat then
      Result := Result and (GetInputSize(GetIO, Handle) > Hdr.ImageSize + SizeOf(Hdr))
    else
      Result := Result and (GetInputSize(GetIO, Handle) = Hdr.ImageSize + SizeOf(Hdr));
  end
  else if DagClass = Self.ClassType then
    Result := True;
end;

initialization
  RegisterImageFileFormat(TBSIFileFormat);
  RegisterImageFileFormat(TCIFFileFormat);
  RegisterImageFileFormat(TIMGFileFormat);
  RegisterImageFileFormat(TTextureFileFormat);
  RegisterImageFileFormat(TSKYFileFormat);

{
  File Notes:

  -- TODOS ----------------------------------------------------
    - nothing now

  -- 0.23 Changes/Bug Fixes -----------------------------------
    - Fixed TestFormat which could identify something (eof) as image.

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - Too large images that are to be saved in CIF/IMG formats are
      automatically rescaled in ConvertToSupported method.
    - MakeCompatible method moved to base class, put ConvertToSupported here.
      GetSupportedFormats removed, it is now set in constructor.
    - Added default palettes for more games.
    - Added transparency to Daggerfall palettes.
    - Initial version created based on my older code.
}

end.
