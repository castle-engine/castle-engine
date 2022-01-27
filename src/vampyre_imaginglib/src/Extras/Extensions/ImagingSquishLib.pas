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

{ High quality DXTC compressor using Squish library (dynamically linked).}
unit ImagingSquishLib;

interface

{$I ImagingOptions.inc}

uses
  ImagingTypes, Imaging, ImagingFormats;

type
  TDXTCompressor = (
    dcClusterFit,   // Use a slow but high quality colour compressor (the default).
    dcRangeFit,     // Use a fast but low quality colour compressor.
    dcClusterFitAlphaWeighted // Cluster fit that weights the colour by alpha.
                              // For images that are rendered using alpha blending,
                              // this can significantly increase the perceived quality.
  );

  TColorMetric = (
    cmPerceptual,   // Use a perceptual metric for colour error (the default).
    cmUniform       // Use a uniform metric for colour error.
  );

{ Compresses SrcImage using selected DXTn compression into DestImage.
  DestImage should be cleared before calling.}
procedure DXTCompressImage(const SrcImage: TImageData; var DestImage: TImageData;
  DXTFormat: TImageFormat; Compressor: TDXTCompressor = dcClusterFit;
  Metric: TColorMetric = cmPerceptual);

implementation

const
  FlagDXT1 = 1 shl 0;
  FlagDXT3 = 1 shl 1;
  FlagDXT5 = 1 shl 2;
  FlagColourClusterFit       = 1 shl 3;
  FlagColourRangeFit         = 1 shl 4;
  FlagColourMetricPerceptual = 1 shl 5;
  FlagColourMetricUniform    = 1 shl 6;
  FlagWeightColourByAlpha    = 1 shl 7;

(* @brief Compresses an image in memory.

	@param rgba		The pixels of the source.
	@param width	The width of the source image.
	@param height	The height of the source image.
	@param blocks	Storage for the compressed output.
	@param flags	Compression flags.

	The source pixels should be presented as a contiguous array of width*height
	rgba values, with each component as 1 byte each. In memory this should be:

		{ r1, g1, b1, a1, .... , rn, gn, bn, an } for n = width*height

	The flags parameter should specify either kDxt1, kDxt3 or kDxt5 compression,
	however, DXT1 will be used by default if none is specified. When using DXT1
	compression, 8 bytes of storage are required for each compressed DXT block.
	DXT3 and DXT5 compression require 16 bytes of storage per block.

	The flags parameter can also specify a preferred colour compressor and
	colour error metric to use when fitting the RGB components of the data.
	Possible colour compressors are: kColourClusterFit (the default) or
	kColourRangeFit. Possible colour error metrics are: kColourMetricPerceptual
	(the default) or kColourMetricUniform. If no flags are specified in any
	particular category then the default will be used. Unknown flags are
	ignored.

	When using kColourClusterFit, an additional flag can be specified to
	weight the colour of each pixel by its alpha value. For images that are
	rendered using alpha blending, this can significantly increase the
	perceived quality.

	Internally this function calls squish::Compress for each block. To see how
	much memory is required in the compressed image, use
	squish::GetStorageRequirements.
*)

procedure CompressImage(RGBA: PByte; Width, Height: Integer; Blocks: Pointer;
  Flags: Integer); cdecl; external 'libsquish.dll';


procedure DXTCompressImage(const SrcImage: TImageData; var DestImage: TImageData;
  DXTFormat: TImageFormat; Compressor: TDXTCompressor = dcClusterFit;
  Metric: TColorMetric = cmPerceptual);
var
  Width, Height: Integer;
  Info: TImageFormatInfo;
  TempImage: TImageData;
  Flags: Integer;

  function GetSquishFlags: Integer;
  begin
    Result := 0;

    case DXTFormat of
      ifDXT1: Result := FlagDXT1;
      ifDXT3: Result := FlagDXT3;
      ifDXT5: Result := FlagDXT5;
    end;

    case Compressor of
      dcClusterFit: Result := Result or FlagColourClusterFit;
      dcRangeFit:   Result := Result or FlagColourRangeFit;
      dcClusterFitAlphaWeighted: Result := Result or FlagColourClusterFit or FlagWeightColourByAlpha;
    end;

    case Metric of
      cmPerceptual: Result := Result or FlagColourMetricPerceptual;
      cmUniform: Result := Result or FlagColourMetricUniform;
    end;
  end;

begin
  Assert(DXTFormat in [ifDXT1, ifDXT3, ifDXT5]);

  Width := SrcImage.Width;
  Height := SrcImage.Height;
  Flags := GetSquishFlags;

  // Check if input has correct dimensions and change them if needed
  GetImageFormatInfo(DXTFormat, Info);
  Info.CheckDimensions(DXTFormat, Width, Height);

  try
    // Create temp image as input for squish (must be ABGR order with
    // dimensions being multiples of 4)
    NewImage(Width, Height, ifA8R8G8B8, TempImage);
    CopyRect(SrcImage, 0, 0, SrcImage.Width, SrcImage.Height, TempImage, 0, 0);
    SwapChannels(TempImage, ChannelRed, ChannelBlue);

    // Init and create out image
    InitImage(DestImage);
    NewImage(Width, Height, DXTFormat, DestImage);

    // Finally call Squish
    CompressImage(TempImage.Bits, Width, Height, DestImage.Bits, Flags);
  finally
    FreeImage(TempImage);
  end;
end;

end.
