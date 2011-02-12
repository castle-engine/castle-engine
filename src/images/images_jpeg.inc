{
  Copyright 2001-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

function LoadJPEG(Stream: TStream;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions): TImage;
var MemS: TMemoryStream;
begin
 MemS := TMemoryStream.Create;
 try
  KambiPasJpeg.LoadJPEG(Stream, MemS, true, 0, nil);
  MemS.Position := 0;
  result := LoadBMP(MemS, AllowedImageClasses, ForbiddenConvs);
 finally MemS.Free end;
end;

procedure SaveJPEG(Img: TImage; Stream: TStream; quality: integer);
var MemS: TMemoryStream;
begin
 MemS := TMemoryStream.Create;
 try
  SaveBMP(img, MemS);
  MemS.Position := 0;
  KambiPasJpeg.StoreJPEG(MemS, Stream, false, quality, nil);
 finally MemS.Free end;
end;

procedure SaveJPEG(Img: TImage; Stream: TStream); { quality = 90 }
begin SaveJPEG(img, Stream, 90) end;
