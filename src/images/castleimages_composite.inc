{%MainUnit castleimages.pas}
{
  Copyright 2018-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

function LoadComposite(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass;
  const MimeType: string): TEncodedImage;
var
  Composite: TCompositeImage;
begin
  Composite := TCompositeImage.Create;
  try
    Composite.LoadFromStream(Stream, '', MimeType);

    { After successfully loading, Composite should always contain at least one image }
    Assert(Composite.Images.Count >= 1);

    { This way I don't have to make a copy of Composite.Images[0] for Result,
      which would be a waste of time. }
    Composite.OwnsFirstImage := false;

    { TODO: make sure to honor AllowedImageClasses.
      For now, this just returns whatever Composite set, without converting... }
    Result := Composite.Images[0];
  finally FreeAndNil(Composite) end;
end;

function LoadDDS(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
begin
  Result := LoadComposite(Stream, AllowedImageClasses, 'image/x-dds');
end;

function LoadKTX(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
begin
  Result := LoadComposite(Stream, AllowedImageClasses, 'image/ktx');
end;

procedure SaveComposite(Img: TEncodedImage; Stream: TStream; const MimeType: string);
var
  Composite: TCompositeImage;
begin
  Composite := TCompositeImage.Create;
  try
    Composite.Width := Img.Width;
    Composite.Height := Img.Height;
    Composite.CompositeType := ctTexture;
    Composite.Mipmaps := false;
    Composite.MipmapsCount := 1;
    Composite.Images.Count := 1;
    Composite.Images[0] := Img;

    Composite.OwnsFirstImage := false;

    Composite.SaveToStream(Stream, MimeType);
  finally FreeAndNil(Composite) end;
end;

procedure SaveDDS(const Img: TEncodedImage; const Stream: TStream);
begin
  SaveComposite(Img, Stream, 'image/x-dds');
end;
