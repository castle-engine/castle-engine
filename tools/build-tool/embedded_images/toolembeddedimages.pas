{ -*- buffer-read-only: t -*- }

{ Unit automatically generated by image-to-pascal tool,
  to embed images in Pascal source code.
  @exclude (Exclude this unit from PasDoc documentation.) }
unit ToolEmbeddedImages;

interface

uses CastleImages;

function DefaultIcon: TRGBAlphaImage;

function DefaultIconSquare: TRGBImage;

implementation

uses SysUtils, CastleInternalDataCompression;

{ Actual image data is included from another file, with a deliberately
  non-Pascal file extension ".image_data". This way online code analysis
  tools will NOT consider this source code as an uncommented Pascal code
  (which would be unfair --- the image data file is autogenerated
  and never supposed to be processed by a human). }
{$I toolembeddedimages.image_data}

initialization
finalization
  FreeAndNil(FDefaultIcon);
  FreeAndNil(FDefaultIconSquare);
end.