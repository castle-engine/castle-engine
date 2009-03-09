function LoadDDS(Stream: TStream;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions): TImage;
var
  DDS: TDDSImage;
begin
  DDS := TDDSImage.Create;
  try
    DDS.LoadFromStream(Stream);

    { After successfully loading, DDS should always contain at least one image }
    Assert(DDS.Images.Count >= 1);

    { This way I don't have to make a copy of DDS.Images[0] for Result,
      which would be a waste of time. }
    DDS.OwnsFirstImage := false;

    { TODO: make sure to honor AllowedImageClasses and ForbiddenConvs.
      For now, I just return whatever DDS set... }
    Result := DDS.Images[0];
  finally FreeAndNil(DDS) end;
end;

procedure SaveDDS(Img: TImage; Stream: TStream);
var
  DDS: TDDSImage;
begin
  DDS := TDDSImage.Create;
  try
    DDS.Width := Img.Width;
    DDS.Height := Img.Height;
    DDS.DDSType := dtTexture;
    DDS.Mipmaps := false;
    DDS.Images.Count := 1;
    DDS.Images[0] := Img;

    DDS.OwnsFirstImage := false;

    DDS.SaveToStream(Stream);
  finally FreeAndNil(DDS) end;
end;
