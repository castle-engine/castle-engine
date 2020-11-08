var NewColor: TVector3;
    P: PPixel;
    I: Integer;
begin
 P := PPixel(RawPixels);
 for I := 1 to Width * Height do
 begin
  NewColor.Data[0] := Matrix.Data[0, 0] * P^.Data[0] + Matrix.Data[1, 0] * P^.Data[1]+ Matrix.Data[2, 0] * P^.Data[2];
  NewColor.Data[1] := Matrix.Data[0, 1] * P^.Data[0] + Matrix.Data[1, 1] * P^.Data[1]+ Matrix.Data[2, 1] * P^.Data[2];
  NewColor.Data[2] := Matrix.Data[0, 2] * P^.Data[0] + Matrix.Data[1, 2] * P^.Data[1]+ Matrix.Data[2, 2] * P^.Data[2];

  P^.Data[0] := Clamped(Round(NewColor.Data[0]), 0, High(Byte));
  P^.Data[1] := Clamped(Round(NewColor.Data[1]), 0, High(Byte));
  P^.Data[2] := Clamped(Round(NewColor.Data[2]), 0, High(Byte));
  Inc(P);
 end;
end;
