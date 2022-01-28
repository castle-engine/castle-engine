{%MainUnit castleimages.pas}

var NewColor: TVector3;
    p: PPixel;
    i: integer;
begin
 p := PPixel(RawPixels);
 for i := 1 to Width * Height do
 begin
  NewColor.Data[0] := Matrix.Data[0, 0]*p^.Data[0] + Matrix.Data[1, 0]*p^.Data[1]+ Matrix.Data[2, 0]*p^.Data[2];
  NewColor.Data[1] := Matrix.Data[0, 1]*p^.Data[0] + Matrix.Data[1, 1]*p^.Data[1]+ Matrix.Data[2, 1]*p^.Data[2];
  NewColor.Data[2] := Matrix.Data[0, 2]*p^.Data[0] + Matrix.Data[1, 2]*p^.Data[1]+ Matrix.Data[2, 2]*p^.Data[2];

  p^.Data[0] := Clamped(Round(NewColor.Data[0]), 0, High(Byte));
  p^.Data[1] := Clamped(Round(NewColor.Data[1]), 0, High(Byte));
  p^.Data[2] := Clamped(Round(NewColor.Data[2]), 0, High(Byte));
  Inc(P);
 end;
end;
