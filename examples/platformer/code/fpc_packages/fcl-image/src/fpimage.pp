{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    FPImage base definitions.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
unit FPImage;

interface

uses sysutils, classes;

type

  TFPCustomImageReader = class;
  TFPCustomImageReaderClass = class of TFPCustomImageReader;
  TFPCustomImageWriter = class;
  TFPCustomImageWriterClass = class of TFPCustomImageWriter;
  TIHData = class;
  TFPCustomImage = class;

  FPImageException = class (exception);

  TFPColor = record
    Red,Green,Blue,Alpha : word;
  end;
  PFPColor = ^TFPColor;

  TColorFormat = (cfMono,cfGray2,cfGray4,cfGray8,cfGray16,cfGray24,
                  cfGrayA8,cfGrayA16,cfGrayA32,
                  cfRGB15,cfRGB16,cfRGB24,cfRGB32,cfRGB48,
                  cfRGBA8,cfRGBA16,cfRGBA32,cfRGBA64,
                  cfBGR15,cfBGR16,cfBGR24,cfBGR32,cfBGR48,
                  cfABGR8,cfABGR16,cfABGR32,cfABGR64);
  TColorData = qword;
  PColorData = ^TColorData;

  TDeviceColor = record
    Fmt : TColorFormat;
    Data : TColorData;
  end;

{$ifdef CPU68K}
  { 1.0 m68k cpu compiler does not allow
    types larger than 32k....
    if we remove range checking all should be fine PM }
  TFPColorArray = array [0..0] of TFPColor;
{$R-}
{$else not CPU68K}
  TFPColorArray = array [0..(maxint-1) div sizeof(TFPColor)-1] of TFPColor;
{$endif CPU68K}
  PFPColorArray = ^TFPColorArray;

  TFPImgProgressStage = (psStarting, psRunning, psEnding);
  TFPImgProgressEvent = procedure (Sender: TObject; Stage: TFPImgProgressStage;
                                   PercentDone: Byte; RedrawNow: Boolean; const R: TRect;
                                   const Msg: AnsiString; var Continue : Boolean) of object;
  // Delphi compatibility
  TProgressStage = TFPImgProgressStage;
  TProgressEvent = TFPImgProgressEvent;

  TFPPalette = class
    protected
      FData : PFPColorArray;
      FCount, FCapacity : integer;
      procedure SetCount (Value:integer); virtual;
      function GetCount : integer;
      procedure SetColor (index:integer; const Value:TFPColor); virtual;
      function GetColor (index:integer) : TFPColor;
      procedure SetCapacity (ind : Integer);
      procedure CheckIndex (index:integer); virtual;
      procedure EnlargeData; virtual;
    public
      constructor Create (ACount : integer);
      destructor Destroy; override;
      procedure Build (Img : TFPCustomImage); virtual;
      procedure Copy (APalette: TFPPalette); virtual;
      procedure Merge (pal : TFPPalette); virtual;
      function IndexOf (const AColor: TFPColor) : integer; virtual;
      function Add (const Value: TFPColor) : integer; virtual;
      procedure Clear; virtual;
      property Color [Index : integer] : TFPColor read GetColor write SetColor; default;
      property Count : integer read GetCount write SetCount;
      property Capacity : integer read FCapacity write SetCapacity;
  end;

  TFPCustomImage = class(TPersistent)
    private
      FOnProgress : TFPImgProgressEvent;
      FExtra : TStringlist;
      FPalette : TFPPalette;
      FHeight, FWidth : integer;
      procedure SetHeight (Value : integer);
      procedure SetWidth (Value : integer);
      procedure SetExtra (const key:String; const AValue:string);
      function GetExtra (const key:String) : string;
      procedure SetExtraValue (index:integer; const AValue:string);
      function GetExtraValue (index:integer) : string;
      procedure SetExtraKey (index:integer; const AValue:string);
      function GetExtraKey (index:integer) : string;
      procedure CheckIndex (x,y:integer);
      procedure CheckPaletteIndex (PalIndex:integer);
      procedure SetColor (x,y:integer; const Value:TFPColor);
      function GetColor (x,y:integer) : TFPColor;
      procedure SetPixel (x,y:integer; Value:integer);
      function GetPixel (x,y:integer) : integer;
      function GetUsePalette : boolean;
    protected
      // Procedures to store the data. Implemented in descendants
      procedure SetInternalColor (x,y:integer; const Value:TFPColor); virtual;
      function GetInternalColor (x,y:integer) : TFPColor; virtual;
      procedure SetInternalPixel (x,y:integer; Value:integer); virtual; abstract;
      function GetInternalPixel (x,y:integer) : integer; virtual; abstract;
      procedure SetUsePalette (Value:boolean);virtual;
      procedure Progress(Sender: TObject; Stage: TProgressStage;
                         PercentDone: Byte;  RedrawNow: Boolean; const R: TRect;
                         const Msg: AnsiString; var Continue: Boolean); Virtual;
    public
      constructor Create (AWidth,AHeight:integer); virtual;
      destructor Destroy; override;
      procedure Assign(Source: TPersistent); override;
      // Image handlers
      class function FindHandlerFromStream(Str:TStream): TIHData;
      class function FindReaderFromStream(Str:TStream): TFPCustomImageReaderClass;
      class function FindHandlerFromExtension(extension:String): TIHData;
      class function FindReaderFromFileName(const filename:String): TFPCustomImageReaderClass;
      class function FindReaderFromExtension(const extension:String): TFPCustomImageReaderClass;
      class function FindWriterFromFileName(const filename:String): TFPCustomImageWriterClass;
      class function FindWriterFromExtension(const extension:String): TFPCustomImageWriterClass;
      // Saving and loading
      procedure LoadFromStream (Str:TStream; Handler:TFPCustomImageReader);
      procedure LoadFromStream (Str:TStream);
      procedure LoadFromFile (const filename:String; Handler:TFPCustomImageReader);
      function LoadFromFile (const filename:String): Boolean;
      procedure SaveToStream (Str:TStream; Handler:TFPCustomImageWriter);
      procedure SaveToFile (const filename:String; Handler:TFPCustomImageWriter);
      function SaveToFile (const filename:String): Boolean;
      // Size and data
      procedure SetSize (AWidth, AHeight : integer); virtual;
      property  Height : integer read FHeight write SetHeight;
      property  Width : integer read FWidth write SetWidth;
      property  Colors [x,y:integer] : TFPColor read GetColor write SetColor; default;
      // Use of palette for colors
      property  UsePalette : boolean read GetUsePalette write SetUsePalette;
      property  Palette : TFPPalette read FPalette;
      property  Pixels [x,y:integer] : integer read GetPixel write SetPixel;
      // Info unrelated with the image representation
      property  Extra [const key:string] : string read GetExtra write SetExtra;
      property  ExtraValue [index:integer] : string read GetExtraValue write SetExtraValue;
      property  ExtraKey [index:integer] : string read GetExtraKey write SetExtraKey;
      procedure RemoveExtra (const key:string);
      function  ExtraCount : integer;
      property OnProgress: TFPImgProgressEvent read FOnProgress write FOnProgress;
  end;
  TFPCustomImageClass = class of TFPCustomImage;

{$ifdef CPU68K}
  { 1.0 m68k cpu compiler does not allow
    types larger than 32k....
    if we remove range checking all should be fine PM }
  TFPIntegerArray = array [0..0] of integer;
{$R-}
{$else not CPU68K}
  TFPIntegerArray = array [0..(maxint-1) div sizeof(integer)-1] of integer;
{$endif CPU68K}
  PFPIntegerArray = ^TFPIntegerArray;

  TFPMemoryImage = class (TFPCustomImage)
    protected
      function GetInternalColor(x,y:integer):TFPColor;override;
      procedure SetInternalColor (x,y:integer; const Value:TFPColor);override;
      procedure SetUsePalette (Value:boolean);override;
    protected
      FData : PFPIntegerArray;
      procedure SetInternalPixel (x,y:integer; Value:integer); override;
      function GetInternalPixel (x,y:integer) : integer; override;
    public
      constructor Create (AWidth,AHeight:integer); override;
      destructor Destroy; override;
      procedure SetSize (AWidth, AHeight : integer); override;
  end;

  TFPCustomImageHandler = class
    private
      FOnProgress : TFPImgProgressEvent;
      FStream : TStream;
      FImage : TFPCustomImage;
    protected
      procedure Progress(Stage: TProgressStage; PercentDone: Byte;  RedrawNow: Boolean; const R: TRect;
                         const Msg: AnsiString; var Continue: Boolean); Virtual;
      property TheStream : TStream read FStream;
      property TheImage : TFPCustomImage read FImage;
    public
      constructor Create; virtual;
      Property OnProgress : TFPImgProgressEvent Read FOnProgress Write FOnProgress;
  end;

  TFPCustomImageReader = class (TFPCustomImageHandler)
    private
      FDefImageClass:TFPCustomImageClass;
    protected
      procedure InternalRead  (Str:TStream; Img:TFPCustomImage); virtual; abstract;
      function  InternalCheck (Str:TStream) : boolean; virtual; abstract;
      class function InternalSize  (Str:TStream): TPoint; virtual;
    public
      constructor Create; override;
      function ImageRead (Str:TStream; Img:TFPCustomImage) : TFPCustomImage;
      // reads image
      function CheckContents (Str:TStream) : boolean;
      // Returns true if the content is readable
      class function ImageSize(Str:TStream): TPoint;
      // returns the size of image in stream without loading it completely. -1,-1 means this is not implemented.
      property DefaultImageClass : TFPCustomImageClass read FDefImageClass write FDefImageClass;
      // Image Class to create when no img is given for reading
  end;

  TFPCustomImageWriter = class (TFPCustomImageHandler)
    protected
      procedure InternalWrite (Str:TStream; Img:TFPCustomImage); virtual; abstract;
    public
      procedure ImageWrite (Str:TStream; Img:TFPCustomImage);
      // writes given image to stream
  end;

  TIHData = class
    private
      FExtension, FTypeName, FDefaultExt : string;
      FReader : TFPCustomImageReaderClass;
      FWriter : TFPCustomImageWriterClass;
    public
      property Extension: string read FExtension;
      property TypeName: string read FTypeName;
      property DefaultExt: string read FDefaultExt;
      property Reader : TFPCustomImageReaderClass read FReader;
      property Writer : TFPCustomImageWriterClass read FWriter;
  end;

  TImageHandlersManager = class
    private
      FData : TList;
      function GetReader (const TypeName:string) : TFPCustomImageReaderClass;
      function GetWriter (const TypeName:string) : TFPCustomImageWriterClass;
      function GetExt (const TypeName:string) : string;
      function GetDefExt (const TypeName:string) : string;
      function GetTypeName (index:integer) : string;
      function GetData (const ATypeName:string) : TIHData;
      function GetData (index : integer) : TIHData;
      function GetCount : integer;
    public
      constructor Create;
      destructor Destroy; override;
      procedure RegisterImageHandlers (const ATypeName,TheExtensions:string;
                   AReader:TFPCustomImageReaderClass; AWriter:TFPCustomImageWriterClass);
      procedure RegisterImageReader (const ATypeName,TheExtensions:string;
                   AReader:TFPCustomImageReaderClass);
      procedure RegisterImageWriter (const ATypeName,TheExtensions:string;
                   AWriter:TFPCustomImageWriterClass);
      procedure UnregisterImageHandlers(const ATypeName: string; ARemoveReader: boolean = True; ARemoveWriter: boolean = True);
      property Count : integer read GetCount;
      property ImageReader [const TypeName:string] : TFPCustomImageReaderClass read GetReader;
      property ImageWriter [const TypeName:string] : TFPCustomImageWriterClass read GetWriter;
      property Extensions [const TypeName:string] : string read GetExt;
      property DefaultExtension [const TypeName:string] : string read GetDefExt;
      property TypeNames [index:integer] : string read GetTypeName;
    end;

{function ShiftAndFill (initial:word; CorrectBits:byte):word;
function FillOtherBits (initial:word;CorrectBits:byte):word;
}
function CalculateGray (const From : TFPColor) : word;
(*
function ConvertColor (const From : TDeviceColor) : TFPColor;
function ConvertColor (const From : TColorData; FromFmt:TColorFormat) : TFPColor;
function ConvertColorToData (const From : TFPColor; Fmt : TColorFormat) : TColorData;
function ConvertColorToData (const From : TDeviceColor; Fmt : TColorFormat) : TColorData;
function ConvertColor (const From : TFPColor; Fmt : TColorFormat) : TDeviceColor;
function ConvertColor (const From : TDeviceColor; Fmt : TColorFormat) : TDeviceColor;
*)

function AlphaBlend(const color1, color2: TFPColor): TFPColor;

function FPColor (r,g,b,a:word) : TFPColor;
function FPColor (r,g,b:word) : TFPColor;
{$ifdef debug}function MakeHex (n:TColordata;nr:byte): string;{$endif}

operator = (const c,d:TFPColor) : boolean;
operator or (const c,d:TFPColor) : TFPColor;
operator and (const c,d:TFPColor) : TFPColor;
operator xor (const c,d:TFPColor) : TFPColor;
function CompareColors(const Color1, Color2: TFPColor): integer;

var ImageHandlers : TImageHandlersManager;

type
  TErrorTextIndices = (
    StrInvalidIndex,
    StrNoImageToWrite,
    StrNoFile,
    StrNoStream,
    StrPalette,
    StrImageX,
    StrImageY,
    StrImageExtra,
    StrTypeAlreadyExist,
    StrTypeReaderAlreadyExist,
    StrTypeWriterAlreadyExist,
    StrCantDetermineType,
    StrNoCorrectReaderFound,
    StrReadWithError,
    StrWriteWithError,
    StrNoPaletteAvailable,
    StrInvalidHTMLColor
    );

const
  // MG: ToDo: move to implementation and add a function to map to resourcestrings
  ErrorText : array[TErrorTextIndices] of string =
    ('Invalid %s index %d',
     'No image to write',
     'File "%s" does not exist',
     'No stream to write to',
     'palette',
     'horizontal pixel',
     'vertical pixel',
     'extra',
     'Image type "%s" already exists',
     'Image type "%s" already has a reader class',
     'Image type "%s" already has a writer class',
     'Error while determining image type of stream: %s',
     'Can''t determine image type of stream',
     'Error while reading stream: %s',
     'Error while writing stream: %s',
     'No palette available',
     'Invalid HTML color : %s'
     );

{$i fpcolors.inc}

type
  TGrayConvMatrix = record
    red, green, blue : single;
  end;

var
  GrayConvMatrix : TGrayConvMatrix;

const
  GCM_NTSC : TGrayConvMatrix = (red:0.299; green:0.587; blue:0.114);
  GCM_JPEG : TGrayConvMatrix = (red:0.299; green:0.587; blue:0.114);
  GCM_Mathematical : TGrayConvMatrix = (red:0.334; green:0.333; blue:0.333);
  GCM_Photoshop : TGrayConvMatrix = (red:0.213; green:0.715; blue:0.072);

function CreateBlackAndWhitePalette : TFPPalette;
function CreateWebSafePalette : TFPPalette;
function CreateGrayScalePalette : TFPPalette;
function CreateVGAPalette : TFPPalette;

Type
  TFPCompactImgDesc = record
    Gray: boolean; // true = red=green=blue, false: a RGB image
    Depth: word; // 8 or 16 bit
    HasAlpha: boolean; // has alpha channel
  end;

  { TFPCompactImgBase }

  TFPCompactImgBase = class(TFPCustomImage)
  private
    FDesc: TFPCompactImgDesc;
  public
    property Desc: TFPCompactImgDesc read FDesc;
  end;
  TFPCompactImgBaseClass = class of TFPCompactImgBase;

  { TFPCompactImgGray16Bit }

  TFPCompactImgGray16Bit = class(TFPCompactImgBase)
  protected
    FData: PWord;
    function GetInternalColor(x, y: integer): TFPColor; override;
    function GetInternalPixel({%H-}x, {%H-}y: integer): integer; override;
    procedure SetInternalColor (x, y: integer; const Value: TFPColor); override;
    procedure SetInternalPixel({%H-}x, {%H-}y: integer; {%H-}Value: integer); override;
  public
    constructor Create(AWidth, AHeight: integer); override;
    destructor Destroy; override;
    procedure SetSize(AWidth, AHeight: integer); override;
  end;

  TFPCompactImgGrayAlpha16BitValue = packed record
    G,A: word;
  end;
  PFPCompactImgGrayAlpha16BitValue = ^TFPCompactImgGrayAlpha16BitValue;

  { TFPCompactImgGrayAlpha16Bit }

  TFPCompactImgGrayAlpha16Bit = class(TFPCompactImgBase)
  protected
    FData: PFPCompactImgGrayAlpha16BitValue;
    function GetInternalColor(x, y: integer): TFPColor; override;
    function GetInternalPixel({%H-}x, {%H-}y: integer): integer; override;
    procedure SetInternalColor (x, y: integer; const Value: TFPColor); override;
    procedure SetInternalPixel({%H-}x, {%H-}y: integer; {%H-}Value: integer); override;
  public
    constructor Create(AWidth, AHeight: integer); override;
    destructor Destroy; override;
    procedure SetSize(AWidth, AHeight: integer); override;
  end;

  { TFPCompactImgGray8Bit }

  TFPCompactImgGray8Bit = class(TFPCompactImgBase)
  protected
    FData: PByte;
    function GetInternalColor(x, y: integer): TFPColor; override;
    function GetInternalPixel({%H-}x, {%H-}y: integer): integer; override;
    procedure SetInternalColor (x, y: integer; const Value: TFPColor); override;
    procedure SetInternalPixel({%H-}x, {%H-}y: integer; {%H-}Value: integer); override;
  public
    constructor Create(AWidth, AHeight: integer); override;
    destructor Destroy; override;
    procedure SetSize(AWidth, AHeight: integer); override;
  end;

  TFPCompactImgGrayAlpha8BitValue = packed record
    G,A: byte;
  end;
  PFPCompactImgGrayAlpha8BitValue = ^TFPCompactImgGrayAlpha8BitValue;

  { TFPCompactImgGrayAlpha8Bit }

  TFPCompactImgGrayAlpha8Bit = class(TFPCompactImgBase)
  protected
    FData: PFPCompactImgGrayAlpha8BitValue;
    function GetInternalColor(x, y: integer): TFPColor; override;
    function GetInternalPixel({%H-}x, {%H-}y: integer): integer; override;
    procedure SetInternalColor (x, y: integer; const Value: TFPColor); override;
    procedure SetInternalPixel({%H-}x, {%H-}y: integer; {%H-}Value: integer); override;
  public
    constructor Create(AWidth, AHeight: integer); override;
    destructor Destroy; override;
    procedure SetSize(AWidth, AHeight: integer); override;
  end;

  TFPCompactImgRGBA8BitValue = packed record
    R,G,B,A: byte;
  end;
  PFPCompactImgRGBA8BitValue = ^TFPCompactImgRGBA8BitValue;

  { TFPCompactImgRGBA8Bit }

  TFPCompactImgRGBA8Bit = class(TFPCompactImgBase)
  protected
    FData: PFPCompactImgRGBA8BitValue;
    function GetInternalColor(x, y: integer): TFPColor; override;
    function GetInternalPixel({%H-}x, {%H-}y: integer): integer; override;
    procedure SetInternalColor (x, y: integer; const Value: TFPColor); override;
    procedure SetInternalPixel({%H-}x, {%H-}y: integer; {%H-}Value: integer); override;
  public
    constructor Create(AWidth, AHeight: integer); override;
    destructor Destroy; override;
    procedure SetSize(AWidth, AHeight: integer); override;
  end;

  TFPCompactImgRGB8BitValue = packed record
    R,G,B: byte;
  end;
  PFPCompactImgRGB8BitValue = ^TFPCompactImgRGB8BitValue;

  { TFPCompactImgRGB8Bit }

  TFPCompactImgRGB8Bit = class(TFPCompactImgBase)
  protected
    FData: PFPCompactImgRGB8BitValue;
    function GetInternalColor(x, y: integer): TFPColor; override;
    function GetInternalPixel({%H-}x, {%H-}y: integer): integer; override;
    procedure SetInternalColor (x, y: integer; const Value: TFPColor); override;
    procedure SetInternalPixel({%H-}x, {%H-}y: integer; {%H-}Value: integer); override;
  public
    constructor Create(AWidth, AHeight: integer); override;
    destructor Destroy; override;
    procedure SetSize(AWidth, AHeight: integer); override;
  end;

  TFPCompactImgRGB16BitValue = packed record
    R,G,B: word;
  end;
  PFPCompactImgRGB16BitValue = ^TFPCompactImgRGB16BitValue;

  { TFPCompactImgRGB16Bit }

  TFPCompactImgRGB16Bit = class(TFPCompactImgBase)
  protected
    FData: PFPCompactImgRGB16BitValue;
    function GetInternalColor(x, y: integer): TFPColor; override;
    function GetInternalPixel({%H-}x, {%H-}y: integer): integer; override;
    procedure SetInternalColor (x, y: integer; const Value: TFPColor); override;
    procedure SetInternalPixel({%H-}x, {%H-}y: integer; {%H-}Value: integer); override;
  public
    constructor Create(AWidth, AHeight: integer); override;
    destructor Destroy; override;
    procedure SetSize(AWidth, AHeight: integer); override;
  end;

  { TFPCompactImgRGBA16Bit }

  TFPCompactImgRGBA16Bit = class(TFPCompactImgBase)
  protected
    FData: PFPColor;
    function GetInternalColor(x, y: integer): TFPColor; override;
    function GetInternalPixel({%H-}x, {%H-}y: integer): integer; override;
    procedure SetInternalColor (x, y: integer; const Value: TFPColor); override;
    procedure SetInternalPixel({%H-}x, {%H-}y: integer; {%H-}Value: integer); override;
  public
    constructor Create(AWidth, AHeight: integer); override;
    destructor Destroy; override;
    procedure SetSize(AWidth, AHeight: integer); override;
  end;

{ Create a descriptor to select a CompactImg class }
function GetFPCompactImgDesc(Gray: boolean; Depth: word; HasAlpha: boolean): TFPCompactImgDesc;

{ Returns a CompactImg class that fits the descriptor }
function GetFPCompactImgClass(const Desc: TFPCompactImgDesc): TFPCompactImgBaseClass;

{ Create a CompactImg with the descriptor }
function CreateFPCompactImg(const Desc: TFPCompactImgDesc; Width, Height: integer): TFPCustomImage;

{ Create a CompactImg with the same features as Img.
If Img is a TFPCompactImgBaseClass it will create that.
Otherwise it returns a CompactImg that fits the Img using GetMinimumPTDesc. }
function CreateCompatibleFPCompactImg(Img: TFPCustomImage; Width, Height: integer
): TFPCustomImage;

{ As CreateCompatibleFPCompactImg, but the image has always an alpha channel. }
function CreateCompatibleFPCompactImgWithAlpha(Img: TFPCustomImage;
Width, Height: integer): TFPCustomImage;

{ Returns the smallest descriptor that allows to store the Img.
It returns HasAlpha=false if all pixel are opaque.
It returns Gray=true if all red=green=blue.
It returns Depth=8 if all lo byte equals the hi byte or all lo bytes are 0.
To ignore rounding errors you can pass a FuzzyDepth. For example a FuzzyDepth
of 3 ignores the lower 3 bits when comparing.  }
function GetMinimumPTDesc(Img: TFPCustomImage; FuzzyDepth: word = 4): TFPCompactImgDesc;

{ Create a smaller CompactImg with the same information as Img.
Pass FreeImg=true to call Img.Free }
function GetMinimumFPCompactImg(Img: TFPCustomImage; FreeImg: boolean;
FuzzyDepth: word = 4): TFPCustomImage;

{ HTML Color support. RRGGBB or color name. Only W3 color names s are supported}

function TryHtmlToFPColor(const S: String; out FPColor: TFPColor): Boolean;
function HtmlToFPColorDef(const S: String; out FPColor: TFpColor; const Def: TFPColor): TFPColor;
function HtmlToFPColor(const S: String): TFPColor;


implementation

procedure FPImgError (Fmt:TErrorTextIndices; data : array of const);
begin
  raise FPImageException.CreateFmt (ErrorText[Fmt],data);
end;

procedure FPImgError (Fmt:TErrorTextIndices);
begin
  raise FPImageException.Create (ErrorText[Fmt]);
end;

{$i FPImage.inc}
{$i FPHandler.inc}
{$i FPPalette.inc}
{$i FPColCnv.inc}
{$i fpcompactimg.inc}

function FPColor (r,g,b:word) : TFPColor;
begin
  with result do
    begin
    red := r;
    green := g;
    blue := b;
    alpha := alphaOpaque;
    end;
end;

function FPColor (r,g,b,a:word) : TFPColor;
begin
  with result do
    begin
    red := r;
    green := g;
    blue := b;
    alpha := a;
    end;
end;

operator = (const c,d:TFPColor) : boolean;
begin
  result := (c.Red = d.Red) and
            (c.Green = d.Green) and
            (c.Blue = d.Blue) and
            (c.Alpha = d.Alpha);
end;

function GetFullColorData (const color:TFPColor) : TColorData;
begin
  result := PColorData(@color)^;
end;

function SetFullColorData (const color:TColorData) : TFPColor;
begin
  result := PFPColor (@color)^;
end;

operator or (const c,d:TFPColor) : TFPColor;
begin
  result := SetFullColorData(GetFullColorData(c) OR GetFullColorData(d));
end;

operator and (const c,d:TFPColor) : TFPColor;
begin
  result := SetFullColorData(GetFullColorData(c) AND GetFullColorData(d));
end;

operator xor (const c,d:TFPColor) : TFPColor;
begin
  result := SetFullColorData(GetFullColorData(c) XOR GetFullColorData(d));
end;

{$ifdef debug}
function MakeHex (n:TColordata;nr:byte): string;
const hexnums : array[0..15] of char =
              ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
var r : integer;
begin
  result := '';
  for r := 0 to nr-1 do
    begin
    result := hexnums[n and $F] + result;
    n := n shr 4;
    if ((r+1) mod 4) = 0 then
      result := ' ' + result;
    end;
end;
{$endif}

type
  THtmlColorName = (
    hcnWhite, hcnSilver, hcnGray, hcnBlack,
    hcnRed, hcnMaroon, hcnYellow, hcnOlive,
    hcnLime, hcnGreen, hcnAqua, hcnTeal, hcnBlue,
    hcnNavy, hcnFuchsia, hcnPurple);

const
  HtmlColorNameToFPColorMap: array[THtmlColorName] of TFPColor = (
    (red: $ff or $ff shl 8; green: $ff or $ff shl 8; blue: $ff or $ff shl 8; alpha: alphaOpaque), //hcnWhite
    (red: $c0 or $c0 shl 8; green: $c0 or $c0 shl 8; blue: $c0 or $c0 shl 8; alpha: alphaOpaque), //hcnSilver
    (red: $80 or $80 shl 8; green: $80 or $80 shl 8; blue: $80 or $80 shl 8; alpha: alphaOpaque), //hcnGray
    (red: $00;              green: $00;              blue: $00;              alpha: alphaOpaque), //hcnBlack
    (red: $ff or $ff shl 8; green: $00;              blue: $00;              alpha: alphaOpaque), //hcnRed
    (red: $80 or $80 shl 8; green: $00;              blue: $00;              alpha: alphaOpaque), //hcnMaroon
    (red: $ff or $ff shl 8; green: $ff or $ff shl 8; blue: $00;              alpha: alphaOpaque), //hcnYellow
    (red: $80 or $80 shl 8; green: $80 or $80 shl 8; blue: $00;              alpha: alphaOpaque), //hcnOlive
    (red: $00;              green: $ff or $ff shl 8; blue: $00;              alpha: alphaOpaque), //hcnLime
    (red: $00;              green: $80 or $80 shl 8; blue: $00;              alpha: alphaOpaque), //hcnGreen
    (red: $00;              green: $ff or $ff shl 8; blue: $ff or $ff shl 8; alpha: alphaOpaque), //hcnAqua
    (red: $00;              green: $80 or $80 shl 8; blue: $80 or $80 shl 8; alpha: alphaOpaque), //hcnTeal
    (red: $00;              green: $00;              blue: $ff or $ff shl 8; alpha: alphaOpaque), //hcnBlue
    (red: $00;              green: $00;              blue: $80 or $80 shl 8; alpha: alphaOpaque), //hcnNavy
    (red: $ff or $ff shl 8; green: $00;              blue: $ff or $ff shl 8; alpha: alphaOpaque), //hcnFuchsia
    (red: $80 or $80 shl 8; green: $00;              blue: $80 or $80 shl 8; alpha: alphaOpaque)  //hcnPurple
  );

function TryStrToHtmlColorName(const S: String; out AName: THtmlColorName): Boolean;
begin
   Result := True;
   case LowerCase(S) of
     'white'  : AName := hcnWhite;
     'silver' : AName := hcnSilver;
     'gray'   : AName := hcnGray;
     'black'  : AName := hcnBlack;
     'red'    : AName := hcnRed;
     'maroon' : AName := hcnMaroon;
     'yellow' : AName := hcnYellow;
     'olive'  : AName := hcnOlive;
     'lime'   : AName := hcnLime;
     'green'  : AName := hcnGreen;
     'aqua'   : AName := hcnAqua;
     'teal'   : AName := hcnTeal;
     'blue'   : AName := hcnBlue;
     'navy'   : AName := hcnNavy;
     'fuchsia': AName := hcnFuchsia;
     'purple' : AName := hcnPurple;
  else
    Result := False;
  end;
end;

{ Try to translate HTML color code into TFPColor
  Supports following formats
    '#rgb'
    '#rrggbb'
    W3C Html color name
}
function TryHtmlToFPColor(const S: String; out FPColor: TFPColor): Boolean;

  function TryHexStrToWord(const Hex: String; out W: Word): Boolean;
  var
    Code: Integer;
  begin
    Val('$'+Hex, W, Code);
    Result := (Code = 0);
    if Result then
      W := W or (W shl 8)
    else
      W := 0;
  end;

var
  AName: THtmlColorName;
begin
  Result := False;
  FPColor.red := 0;
  FPColor.green := 0;
  FPColor.blue := 0;
  FPColor.alpha := alphaOpaque;
  if (Length(S) = 0) then
    Exit;
  if (S[1] = '#') then
  begin
    if Length(S) = 4 then
    begin  // #rgb
      Result := (TryHexstrToWord(S[2]+S[2], FPColor.red) and
                 TryHexstrToWord(S[3]+S[3], FPColor.green) and
                 TryHexstrToWord(S[4]+S[4], FPColor.blue));
    end
    else if Length(S) = 7 then
    begin  // #rrggbb
      Result := (TryHexstrToWord(S[2]+S[3], FPColor.red) and
                 TryHexstrToWord(S[4]+S[5], FPColor.green) and
                 TryHexstrToWord(S[6]+S[7], FPColor.blue));
    end;
  end
  else
  begin
    Result := TryStrToHtmlColorName(S, AName);
    if Result then
      FPColor := HtmlColorNameToFPColorMap[AName];
  end;
end;

function HtmlToFPColorDef(const S: String; out FPColor: TFpColor; const Def: TFPColor): TFPColor;
begin
  if not TryHtmlToFPColor(S, Result) then
    Result := Def;
end;

function HtmlToFpColor(const S: String): TFPColor;
begin
  if not TryHtmlToFpColor(S, Result) then
    raise EConvertError.CreateFmt(ErrorText[StrInvalidHTMLColor], [S]);
end;


initialization
  ImageHandlers := TImageHandlersManager.Create;
  GrayConvMatrix := GCM_JPEG;
  // Following lines are here because the compiler 1.0 can't work with int64 constants
(*  ColorBits [cfRGB48,1] := ColorBits [cfRGB48,1] shl 16;
  ColorBits [cfRGBA64,1] := ColorBits [cfRGBA64,1] shl 32;
  ColorBits [cfRGBA64,2] := ColorBits [cfRGBA64,2] shl 16;
  ColorBits [cfABGR64,0] := ColorBits [cfABGR64,0] shl 32;
  ColorBits [cfABGR64,3] := ColorBits [cfABGR64,3] shl 16;
  ColorBits [cfBGR48,3] := ColorBits [cfBGR48,3] shl 16;
  PrepareBitMasks;*)

finalization
  ImageHandlers.Free;

end.
