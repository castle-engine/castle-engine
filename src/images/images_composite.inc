function LoadDDS(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
var
  Composite: TCompositeImage;
begin
  Composite := TCompositeImage.Create;
  try
    Composite.LoadFromStream(Stream);

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

procedure SaveDDS(Img: TEncodedImage; Stream: TStream);
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

    Composite.SaveToStream(Stream);
  finally FreeAndNil(Composite) end;
end;
