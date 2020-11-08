procedure CalculateAnchor(const OX, OY, OW, OH,
    X, Y, W, H: Integer; out AX, AY: single);
var
  OriginAX, OriginAY,
  TransAX, TransAY: Integer;
begin
  { Find anchor position in original frame. }
  OriginAX := OW div 2;
  OriginAY := OH div 2;
  { Translate anchor position from original frame to the one in spritesheet. }
  TransAX := OriginAX - OX + X;
  TransAY := OriginAY - OY + Y;
  { Calculate anchor. }
  AX := 1 / W * (TransAX - X);
  AY := 1 / H * (TransAY - Y);
end;
