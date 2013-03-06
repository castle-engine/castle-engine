unit ImageRectsUtils;

{$mode objfpc}{$H+}

interface

uses CastleVectors;

var
  AvgRect: boolean = true;
  BWColors: boolean = true;
  BWColorDark: TVector3Byte = (20, 20, 20);
  BWColorLight: TVector3Byte = (255, 100, 100);
  BorderColor: TVector3Byte = (0, 0, 0);

procedure DoImageRects(const SrcImageFilename: string;
  const OutImageFilename: string;
  const RectWidth, RectHeight: Integer);

implementation

uses SysUtils, CastleImages, CastleUtils, CastleColors;

procedure DoImageRects(const SrcImageFilename: string;
  const OutImageFilename: string;
  const RectWidth, RectHeight: Integer);
var
  SrcImage: TRGBImage;

  RectAvgColorCache: boolean;
  RectAvgColorCacheX: Integer;
  RectAvgColorCacheY: Integer;
  RectAvgColorCacheResult: TVector3Byte;

  function RectAvgColor(const RectX, RectY: Integer): TVector3Byte;
  var
    X, Y: Integer;
    Avg: TVector3Double; // Double to keep the sum of many bytes; TODO: QWord would be better?
    Count: Cardinal;
  begin
    if RectAvgColorCache and
       (RectAvgColorCacheX = RectX) and
       (RectAvgColorCacheY = RectY) then
      Exit(RectAvgColorCacheResult);

    Count := 0;
    Avg := Vector3Double(0, 0, 0);
    for X := RectX * RectWidth to (RectX + 1) * RectWidth - 1 do
      for Y := RectY * RectHeight to (RectY + 1) * RectHeight - 1 do
      begin
        if Between(X, 0, SrcImage.Width - 1) and
           Between(Y, 0, SrcImage.Height - 1) then
        begin
          Avg[0] += PVector3Byte(SrcImage.PixelPtr(X, Y))^[0];
          Avg[1] += PVector3Byte(SrcImage.PixelPtr(X, Y))^[1];
          Avg[2] += PVector3Byte(SrcImage.PixelPtr(X, Y))^[2];
          Inc(Count);
        end;
      end;
    // Writeln(VectorToNiceStr(Avg));
    // Writeln(Count);
    // Writeln(RectX);
    // Writeln(RectY);
    if Count = 0 then
      { may happen in case rect is outside our range }
      Result := Vector3Byte(0, 0, 0) else
      Result := Vector3Byte(
        Clamped(Round(Avg[0] / Count), Low(Byte), High(Byte)),
        Clamped(Round(Avg[1] / Count), Low(Byte), High(Byte)),
        Clamped(Round(Avg[2] / Count), Low(Byte), High(Byte))
      );

    if BWColors then
      if GrayscaleValue(Result) > High(Byte) div 2 then
        Result := BWColorLight else
        Result := BWColorDark;

    RectAvgColorCache := true;
    RectAvgColorCacheX := RectX;
    RectAvgColorCacheY := RectY;
    RectAvgColorCacheResult := Result;
  end;

var
  X, Y, XOut, YOut, RectX, RectY: Integer;
  OutImage: TCastleImage;
begin
  SrcImage := LoadImage(SrcImageFilename, [TRGBImage]) as TRGBImage;
  try
    OutImage := TRGBImage.Create(
      SrcImage.Width div RectWidth + SrcImage.Width + 1,
      SrcImage.Height div RectHeight + SrcImage.Height + 1);

    YOut := 0;
    RectY := 0;
    for Y := 0 to SrcImage.Height - 1 do
    begin
      XOut := 0;
      RectX := 0;
      for X := 0 to SrcImage.Width - 1 do
      begin
        if AvgRect then
          PVector3Byte(OutImage.PixelPtr(XOut, YOut))^ := RectAvgColor(RectX, RectY) else
          PVector3Byte(OutImage.PixelPtr(XOut, YOut))^ := PVector3Byte(SrcImage.PixelPtr(X, Y))^;
        Inc(XOut);
        if X mod RectWidth = 0 then
        begin
          PVector3Byte(OutImage.PixelPtr(XOut, YOut))^ := BorderColor;
          Inc(XOut);
          Inc(RectX);
        end;
      end;
      Inc(YOut);

      if Y mod RectHeight = 0 then
      begin
        for XOut := 0 to OutImage.Width - 1 do
          PVector3Byte(OutImage.PixelPtr(XOut, YOut))^ := BorderColor;
        Inc(YOut);
        Inc(RectY);
      end;
    end;

    SaveImage(OutImage, OutImageFilename);
  finally
    FreeAndNil(SrcImage);
    FreeAndNil(OutImage);
  end;
end;

end.

