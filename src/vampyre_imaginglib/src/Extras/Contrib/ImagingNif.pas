{
  This unit contains image format loader for textures in NIF model files.
  Works for NIF version 3 (StarTrek Bridge Commander, ...).
  Author: Delfi
}

unit ImagingNIF;

{$I ImagingOptions.inc}

interface

uses
  ImagingTypes, Imaging, ImagingFormats, ImagingUtility;

type
  { Class for loading and saving NIF images. It can load 24 bit RGB and 32 bit RGBA images}
  TNIFFileFormat = class(TImageFileFormat)
  protected
    procedure Define; override;
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean): Boolean; override;
  public
    function TestFormat(Handle: TImagingHandle): Boolean; override;
  end;

implementation

const
  SNIFFormatName = 'NetImmerse Image';
  SNIFMasks      = '*.nif';

type
  { NIF file header.}
  TNIFHeader = packed record
    Width: LongWord;
    Height: LongWord;
    PixelFmt: LongWord;
  end;

{ TNIFFileFormat class implementation }

procedure TNIFFileFormat.Define;
begin
  inherited;
  FName := SNIFFormatName;
  FFeatures := [ffLoad];

  AddMasks(SNIFMasks);
end;

function TNIFFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  Hdr: TNIFHeader;
  FmtInfo: TImageFormatInfo;
begin
  SetLength(Images, 1);
  with GetIO, Images[0] do
  begin
    // Read NIF header

    Seek(Handle, 170, smFromBeginning);

    Read(Handle, @Hdr.Width, SizeOf(Hdr.Width));
    Read(Handle, @Hdr.Height, SizeOf(Hdr.Height));
    Read(Handle, @Hdr.PixelFmt, SizeOf(Hdr.PixelFmt));

    Seek(Handle, 182, smFromBeginning);

    // Determine image format
    Format := ifR8G8B8;

    if Hdr.PixelFmt = 2 then
      Format := ifA8R8G8B8;

    NewImage(Hdr.Width, Hdr.Height, Format, Images[0]);
    FmtInfo := GetFormatInfo(Format);

    Read(Handle, Bits, Size);

    SwapChannels(Images[0], ChannelRed, ChannelBlue);

    Result := True;
  end;
end;

function TNIFFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  Hdr: longword;
  ReadCount: LongInt;
begin
  Result := False;
  if Handle <> nil then
  begin
    ReadCount := GetIO.Read(Handle, @Hdr, SizeOf(Hdr));
    if Hdr = 1232364878 then Result := True;
    GetIO.Seek(Handle, -ReadCount, smFromCurrent);
  end;
end;

initialization
  RegisterImageFileFormat(TNIFFileFormat);

end.

