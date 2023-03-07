{
  Copyright 2001-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Convert one image to another.
  See README.md for details. }

program image_convert;

uses SysUtils, CastleUtils, CastleImages, CastleParameters;

var
  { required params }
  InputImageName, OutputImageName: string;

  { optional params }
  ResizeX: Cardinal = 0;
  ResizeY: Cardinal = 0;
  FloatScale: Single = 1.0;
  FloatGamma: Single = 1.0;
  WasParam_GrayScale: boolean = false;
  Param_ConvertToChannel: Integer = -1; { -1 means "don't convert" }
  Param_StripToChannel: Integer = -1; { -1 means "don't convert" }
  WasParam_AlphaBleed: boolean = false;

const
  Options: array [0..6] of TOption =
  (
    (Short:'h'; Long:'help'; Argument: oaNone),
    (Short:'s'; Long:'scale'; Argument: oaRequired),
    (Short:'g'; Long:'gamma'; Argument: oaRequired),
    (Short:#0 ; Long:'grayscale'; Argument: oaNone),
    (Short:#0 ; Long:'convert-to-channel'; Argument: oaRequired),
    (Short:#0 ; Long:'strip-to-channel'; Argument: oaRequired),
    (Short:#0 ; Long:'alpha-bleed'; Argument: oaNone)
  );

  procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
    const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
  begin
    case OptionNum of
      0: begin
           Writeln(
             'image_convert <inputfile> <outputfile> [<resize-x> <resize-y>]' +nl+
             'will convert image in <inputfile> to image in <outputfile>.' +nl+
             'Format of both images will be recognized from file extension.' +nl+
             '<inputfile> and <outputfile> may be the same file, it doesn''t matter.' +nl+
             '' +nl+
             'If <resize-x> <resize-y> parameters are given then output image' +nl+
             'is scaled to that size. Size = 0 means "do not scale in this dimension",' +nl+
             'e.g. <resize-x> <resize-y> = 200 0 means "scale x dimension to 200' +nl+
             'and leave y dimension as is". (effectively, specifying' +nl+
             '<resize-x> <resize-y> = 0 0 has the same effect as omitting these' +nl+
             'parameters).' +nl+
             '' +nl+
             'Additional params with no fixed position:' +nl+
             '-s <float> or' +nl+
             '--scale <float>' +nl+
             '  Valid only if input file has a float format (this is currently' +nl+
             '  the case only for RGBE (Radiance) image format).' +nl+
             '  Effect : before writing output image, scales each pixel color' +nl+
             '  (it''s red, green and blue value) by <float>.' +nl+
             '  Multiple --scale *cummulate* : e.g.' +nl+
             '  "--scale 1.5 --scale 2" would have the same effect as' +nl+
             '  "--scale 3".' +nl+
             '-g <float>' +nl+
             '--gamma <float>' +nl+
             '  Similiar to --scale - valid only when input has float precision,' +nl+
             '  multiple params are cummulated, default is 1.0.' +nl+
             '  Each component is raised to 1/<float>.' +nl+
             '' +nl+
             '--grayscale' +nl+
             '          Convert to grayscale.' +nl+
             '--convert-to-channel 0|1|2' +nl+
             '          Converts colors to red / green / blue channel,' +nl+
             '          it''s like converting to grayscale and then' +nl+
             '          writing output to only one channel.' +nl+
             '--strip-to-channel 0|1|2' +nl+
             '          Strips colors to only one channel, i.e.' +nl+
             '          sets to zero intensities of other two channels.' +nl+
             '');
           Halt;
         end;
      1: FloatScale := FloatScale * StrToFloatDot(Argument);
      2: FloatGamma := FloatGamma * StrToFloatDot(Argument);
      3: WasParam_GrayScale := true;
      4: Param_ConvertToChannel := StrToInt(Argument);
      5: Param_StripToChannel := StrToInt(Argument);
      6: WasParam_AlphaBleed := true;
      else raise EInternalError.Create('option not impl');
    end;
  end;

var
  { helper variables }
  Img: TCastleImage;
begin
  { parse free position params }
  Parameters.Parse(Options, @OptionProc, nil);

  { parse fixed position params }
  if Parameters.High = 4 then
  begin
    ResizeX := StrToInt(Parameters[3]); Parameters.Delete(3);
    ResizeY := StrToInt(Parameters[3]); Parameters.Delete(3);
  end;
  Parameters.CheckHigh(2);
  InputImageName := Parameters[1];
  OutputImageName := Parameters[2];

  { do work }
  Img := LoadImage(InputImageName, [], ResizeX, ResizeY);
  try
    if FloatScale <> 1.0 then
    begin
      Check(Img is TRGBFloatImage, '--scale <> 1.0 but input image is not in a float (like Radiance RGBE) format');
      (Img as TRGBFloatImage).ScaleColors(FloatScale);
    end;
    if FloatGamma <> 1.0 then
    begin
      Check(Img is TRGBFloatImage, '--gamma <> 1.0 but input image is not in a float (like Radiance RGBE) format');
      (Img as TRGBFloatImage).ExpColors(1/FloatGamma);
    end;

    if WasParam_GrayScale then
      Img.Grayscale;
    if Param_ConvertToChannel <> -1 then
      Img.ConvertToChannelRGB(Param_ConvertToChannel);
    if Param_StripToChannel <> -1 then
      Img.StripToChannelRGB(Param_StripToChannel);
    if WasParam_AlphaBleed then
      Img.AlphaBleed('Alpha Bleeding');

    SaveImage(Img, OutputImageName);
  finally Img.Free end;
end.
