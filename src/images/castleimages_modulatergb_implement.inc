{%MainUnit castleimages.pas}

var pvec: PPixel;
    i: Cardinal;
begin
 if not Assigned(ColorModulator) then Exit;

 pvec := PPixel(RawPixels);
 for i := 1 to Width * Height * Depth do
 begin
  PVector3Byte(pvec)^ := ColorModulator(PVector3Byte(pvec)^);
  Inc(pvec);
 end;
end;
