{%MainUnit castleimages.pas}

var NewColor: TVector3;
    p: PPixel;
    i: integer;
begin
 p := PPixel(RawPixels);
 for i := 1 to Width * Height do
 begin
  NewColor.X := Matrix.Data[0, 0]*p^.X + Matrix.Data[1, 0]*p^.Y+ Matrix.Data[2, 0]*p^.Z;
  NewColor.Y := Matrix.Data[0, 1]*p^.X + Matrix.Data[1, 1]*p^.Y+ Matrix.Data[2, 1]*p^.Z;
  NewColor.Z := Matrix.Data[0, 2]*p^.X + Matrix.Data[1, 2]*p^.Y+ Matrix.Data[2, 2]*p^.Z;

  p^.X := Clamped(Round(NewColor.X), 0, High(Byte));
  p^.Y := Clamped(Round(NewColor.Y), 0, High(Byte));
  p^.Z := Clamped(Round(NewColor.Z), 0, High(Byte));
  Inc(P);
 end;
end;
