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

{ Unit with operations on binary images. Binary images in Imaging are
  ifGray8 images where pixels with value 0 are considered off, an pixels > 0
  are on.
  Note: Native ifBinary data format was later added to Imaging. However,
  these functions still use ifGray8 for representation for less complex
  and faster processing. ifBinary is meant moreless like interchange
  format for IO file formats. }
unit ImagingBinary;

{$I ImagingOptions.inc}

interface

uses
  Types, ImagingTypes, Imaging, ImagingFormats, ImagingUtility;

type
  { Basic morphologic operators.}
  TMorphologyOp = (
    moErode,  // Erosion
    moDilate  // Dilatation
  );

  { Structuring element for morphology operations. Use ones and
    zeroes to define your struct elements.}
  TStructElement = array of array of Byte;

  TCalcSkewAngleStats = record
    PixelCount: Integer;
    TestedPixels: Integer;
    AccumulatorSize: Integer;
    AccumulatedCounts: Integer;
    BestCount: Integer;
  end;
  PCalcSkewAngleStats = ^TCalcSkewAngleStats;

{ Thresholding using Otsu's method (which chooses the threshold
  to minimize the intraclass variance of the black and white pixels!).
  Functions returns calculated threshold level value [0..255].
  If BinarizeImage is True then the Image is automatically converted to binary using
  computed threshold level.}
function OtsuThresholding(var Image: TImageData; BinarizeImage: Boolean = False): Integer;
{ Applies basic morphology operators (Erode/Dilate) on Image using given structuring element
  Strel. You can do composite operations (Open/Close) by calling this function
  twice each time with different operator.}
procedure Morphology(var Image: TImageData; const Strel: TStructElement; Op: TMorphologyOp);
{ Calculates rotation angle for given 8bit grayscale image.
  Useful for finding skew of scanned documents etc.
  Uses Hough transform internally.
  MaxAngle is maximal (abs. value) expected skew angle in degrees (to speed things up)
  and Threshold (0..255) is used to classify pixel as black (text) or white (background).
  Area of interest rectangle can be defined to restrict the detection to
  work only in defined part of image (useful when the document has text only in
  smaller area of page and non-text features outside the area confuse the rotation detector).
  Various calculations stats can be retrieved by passing Stats parameter.}
function CalcRotationAngle(MaxAngle: Integer; Threshold: Integer;
  Width, Height: Integer; Pixels: PByteArray; DetectionArea: PRect = nil;
  Stats: PCalcSkewAngleStats = nil): Double;
{ Deskews given image. Finds rotation angle and rotates image accordingly.
  Works best on low-color document-like images (scans).
  MaxAngle is maximal (abs. value) expected skew angle in degrees (to speed things up)
  and Threshold (0..255) is used to classify pixel as black (text) or white (background).
  If Threshold=-1 then auto threshold calculated by OtsuThresholding is used.}
procedure DeskewImage(var Image: TImageData; MaxAngle: Integer = 10; Threshold: Integer = -1);

implementation

function OtsuThresholding(var Image: TImageData; BinarizeImage: Boolean): Integer;
var
  Histogram: array[Byte] of Single;
  Level, Max, Min, I, J, NumPixels: Integer;
  Pix: PByte;
  Mean, Variance: Single;
  Mu, Omega, LevelMean, LargestMu: Single;
begin
  Assert(Image.Format = ifGray8);

  FillChar(Histogram, SizeOf(Histogram), 0);
  Min := 255;
  Max := 0;
  Level := 0;
  NumPixels := Image.Width * Image.Height;
  Pix := Image.Bits;

  // Compute histogram and determine min and max pixel values
  for I := 0 to NumPixels - 1 do
  begin
    Histogram[Pix^] := Histogram[Pix^] + 1.0;
    if Pix^ < Min then
      Min := Pix^;
    if Pix^ > Max then
      Max := Pix^;
    Inc(Pix);
  end;

  // Normalize histogram
  for I := 0 to 255 do
    Histogram[I] := Histogram[I] / NumPixels;

  // Compute image mean and variance
  Mean := 0.0;
  Variance := 0.0;
  for I := 0 to 255 do
    Mean := Mean + (I + 1) * Histogram[I];
  for I := 0 to 255 do
    Variance := Variance + Sqr(I + 1 - Mean) * Histogram[I];

  // Now finally compute threshold level
  LargestMu := 0;

  for I := 0 to 255 do
  begin
    Omega := 0.0;
    LevelMean := 0.0;

    for J := 0 to I - 1 do
    begin
      Omega := Omega + Histogram[J];
      LevelMean := LevelMean + (J + 1) * Histogram[J];
    end;

    Mu := Sqr(Mean * Omega - LevelMean);
    Omega := Omega * (1.0 - Omega);

    if Omega > 0.0 then
      Mu := Mu / Omega
    else
      Mu := 0;

    if Mu > LargestMu then
    begin
      LargestMu := Mu;
      Level := I;
    end;
  end;

  if BinarizeImage then
  begin
    // Do thresholding using computed level
    Pix := Image.Bits;
    for I := 0 to Image.Width * Image.Height - 1 do
    begin
      if Pix^ >= Level then
        Pix^ := 255
      else
        Pix^ := 0;
      Inc(Pix);
    end;
  end;

  Result := Level;
end;

procedure Morphology(var Image: TImageData; const Strel: TStructElement; Op: TMorphologyOp);
var
  X, Y, I, J: Integer;
  SWidth, SHeight, PixCount, PixVal, NumOnes, PosX, PosY: Integer;
  ImgOut: TImageData;
  OutPix: PByte;
begin
  Assert(Image.Format = ifGray8);
  Assert((Length(Strel) > 0) and (Length(Strel[0]) > 0));

  SWidth := Length(Strel);
  SHeight := Length(Strel[0]);

  NumOnes := 0;
  if Op = moErode then
  begin
    // We need to know number of ones in the strel for erosion
    for I := 0 to SWidth - 1 do
      for J := 0 to SHeight - 1 do
        NumOnes := NumOnes + Strel[I, J];
  end;

  InitImage(ImgOut);
  NewImage(Image.Width, Image.Height, ifGray8, ImgOut);
  OutPix := ImgOut.Bits;

  for J := 0 to Image.Height - 1 do
    for I := 0 to Image.Width - 1 do
    begin
      PixCount := 0;

      for X := 0 to SWidth - 1 do
      begin
        PosX := ClampInt(X + I - SWidth div 2, 0, Image.Width - 1);
        for Y := 0 to SHeight - 1 do
        begin
          PosY := ClampInt(Y + J - SHeight div 2, 0, Image.Height - 1);
          if (PosX >= 0) and (PosX < Image.Width) and
            (PosY >= 0) and (PosY < Image.Height) then
          begin
            PixVal := PByteArray(Image.Bits)[PosY * Image.Width + PosX];
          end
          else
            PixVal := 0;

          if (Strel[X, Y] > 0) and (PixVal > 0) then
            Inc(PixCount);
        end;
      end;

      case Op of
        moErode:  OutPix^ := Iff(PixCount = NumOnes, 255, 0);
        moDilate: OutPix^ := Iff(PixCount > 0, 255, 0);
      end;

      Inc(OutPix);
    end;

  FreeImage(Image);
  Image := ImgOut;
end;

function CalcRotationAngle(MaxAngle: Integer; Threshold: Integer;
  Width, Height: Integer; Pixels: PByteArray; DetectionArea: PRect; Stats: PCalcSkewAngleStats): Double;
const
  // Number of "best" lines we take into account when determining
  // resulting rotation angle (lines with most votes).
  BestLinesCount = 20;
  // Angle step used in alpha parameter quantization
  AlphaStep = 0.1;
type
  TLine = record
    Count: Integer;
    Index: Integer;
    Alpha: Double;
    D: Double;
  end;
  TLineArray = array of TLine;
var
  AlphaStart, MinD, SumAngles: Double;
  AlphaSteps, DCount, AccumulatorSize, I, AccumulatedCounts: Integer;
  BestLines: TLineArray;
  HoughAccumulator: array of Integer;
  PageWidth, PageHeight: Integer;
  ContentRect: TRect;

  // Classifies pixel at [X, Y] as black or white using threshold.
  function IsPixelBlack(X, Y: Integer): Boolean;
  begin
    Result := Pixels[Y * Width + X] < Threshold;
  end;

  // Calculates alpha parameter for given angle step.
  function GetAlpha(Index: Integer): Double;
  begin
    Result := AlphaStart + Index * AlphaStep;
  end;

  function CalcDIndex(D: Double): Integer;
  begin
    Result := Trunc(D - MinD);
  end;

  // Calculates angle and distance parameters for all lines
  // going through point [X, Y].
  procedure CalcLines(X, Y: Integer);
  var
    D, Rads: Double;
    I, DIndex, Index: Integer;
  begin
    for I := 0 to AlphaSteps - 1 do
    begin
      Rads := GetAlpha(I) * Pi / 180; // Angle for current step in radians
      D := Y * Cos(Rads) - X * Sin(Rads); // Parameter D of the line y=tg(alpha)x + d
      DIndex := CalcDIndex(D);
      Index := DIndex * AlphaSteps + I;
      HoughAccumulator[Index] := HoughAccumulator[Index] + 1; // Add one vote for current line
    end;
  end;

  // Uses Hough transform to calculate all lines that intersect
  // interesting points (those classified as being on base line of the text).
  procedure CalcHoughTransform;
  var
    Y, X: Integer;
  begin
    for Y := 0 to PageHeight - 1 do
      for X := 0 to PageWidth - 1 do
      begin
        if IsPixelBlack(ContentRect.Left + X, ContentRect.Top + Y) and
          not IsPixelBlack(ContentRect.Left + X, ContentRect.Top + Y + 1) then
        begin
          CalcLines(X, Y);
        end;
      end;
  end;

  // Chooses "best" lines (with the most votes) from the accumulator
  function GetBestLines(Count: Integer): TLineArray;
  var
    I, J, DIndex, AlphaIndex: Integer;
    Temp: TLine;
  begin
    SetLength(Result, Count);

    for I := 0 to AccumulatorSize - 1 do
    begin
      if HoughAccumulator[I] > Result[Count - 1].Count then
      begin
        // Current line has more votes than the last selected one,
        // let's put it the pot
        Result[Count - 1].Count := HoughAccumulator[I];
        Result[Count - 1].Index := I;
        J := Count - 1;

        // Sort the lines based on number of votes
        while (J > 0) and (Result[J].Count > Result[J - 1].Count) do
        begin
          Temp := Result[J];
          Result[J] := Result[J - 1];
          Result[J - 1] := Temp;
          J := J - 1;
        end;
      end;

      AccumulatedCounts := AccumulatedCounts + HoughAccumulator[I];
    end;

    for I := 0 to Count - 1 do
    begin
      // Calculate line angle and distance according to index in the accumulator
      DIndex := Result[I].Index div AlphaSteps;
      AlphaIndex := Result[I].Index - DIndex * AlphaSteps;
      Result[I].Alpha := GetAlpha(AlphaIndex);
      Result[I].D := DIndex + MinD;
    end;
  end;

begin
  AccumulatedCounts := 0;

  // Use supplied page content rect or just the whole image
  ContentRect := Rect(0, 0, Width, Height);
  if DetectionArea <> nil then
  begin
    Assert((RectWidth(DetectionArea^) <= Width) and (RectHeight(DetectionArea^) <= Height));
    ContentRect := DetectionArea^;
  end;

  PageWidth := ContentRect.Right - ContentRect.Left;
  PageHeight := ContentRect.Bottom - ContentRect.Top;

  AlphaStart := -MaxAngle;
  AlphaSteps := Round(2 * MaxAngle / AlphaStep); // Number of angle steps = samples from interval <-MaxAngle, MaxAngle>
  MinD := -PageWidth;
  DCount := 2 * (PageWidth + PageHeight);

  // Determine the size of line accumulator
  AccumulatorSize := DCount * AlphaSteps;
  SetLength(HoughAccumulator, AccumulatorSize);

  // Calculate Hough transform
  CalcHoughTransform;

  // Get the best lines with most votes
  BestLines := GetBestLines(BestLinesCount);

  // Average angles of the selected lines to get the rotation angle of the image
  SumAngles := 0;
  for I := 0 to BestLinesCount - 1 do
    SumAngles := SumAngles + BestLines[I].Alpha;

  Result := SumAngles / BestLinesCount;

  if Stats <> nil then
  begin
    Stats.BestCount := BestLines[0].Count;
    Stats.PixelCount := PageWidth * PageHeight;
    Stats.AccumulatorSize := AccumulatorSize;
    Stats.AccumulatedCounts := AccumulatedCounts;
    Stats.TestedPixels := AccumulatedCounts div AlphaSteps;
  end;
end;

procedure DeskewImage(var Image: TImageData; MaxAngle: Integer; Threshold: Integer);
var
  Angle: Double;
  OutputImage: TImageData;
  Info: TImageFormatInfo;
begin
  if not TestImage(Image) then
    raise EImagingBadImage.Create;

  // Clone input image and convert it to 8bit grayscale. This will be our
  // working image.
  CloneImage(Image, OutputImage);
  ConvertImage(Image, ifGray8);

  if Threshold < 0 then
  begin
    // Determine the threshold automatically if needed.
    Threshold := OtsuThresholding(Image);
  end;

  // Main step - calculate image rotation angle
  Angle := CalcRotationAngle(MaxAngle, Threshold, Image.Width, Image.Height, Image.Bits);

  // Finally, rotate the image. We rotate the original input image, not the working
  // one so the color space is preserved.
  GetImageFormatInfo(OutputImage.Format, Info);
  if Info.IsIndexed or Info.IsSpecial then
    ConvertImage(OutputImage, ifA8R8G8B8); // Rotation doesn't like indexed and compressed images
  RotateImage(OutputImage, Angle);

  FreeImage(Image);
  Image := OutputImage;
end;


{
  File Notes:

  -- TODOS ----------------------------------------------------
    - nothing now

  -- 0.77 -------------------------------------------------------
    - OtsuThresholding signature changed, now it's a function and
      always returns the computed level.
    - Extended CalcRotationAngle, added margins and stats.
    - Added CalcRotationAngle and DeskewImage functions.

  -- 0.25.0 Changes/Bug Fixes -----------------------------------
    - Unit created with basic stuff (otsu and erode/dilate morphology ops).

}

end.

