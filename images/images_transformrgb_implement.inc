var NewColor: TVector3Single;
    p: PPixel;
    i: integer;
begin
 p := PPixel(RawPixels);
 for i := 1 to Width * Height do
 begin
  NewColor[0] := Matrix[0, 0]*p^[0] + Matrix[1, 0]*p^[1]+ Matrix[2, 0]*p^[2];
  NewColor[1] := Matrix[0, 1]*p^[0] + Matrix[1, 1]*p^[1]+ Matrix[2, 1]*p^[2];
  NewColor[2] := Matrix[0, 2]*p^[0] + Matrix[1, 2]*p^[1]+ Matrix[2, 2]*p^[2];

  p^[0] := Clamped(Round(NewColor[0]), 0, High(Byte));
  p^[1] := Clamped(Round(NewColor[1]), 0, High(Byte));
  p^[2] := Clamped(Round(NewColor[2]), 0, High(Byte));
  Inc(P);
 end;
end;
