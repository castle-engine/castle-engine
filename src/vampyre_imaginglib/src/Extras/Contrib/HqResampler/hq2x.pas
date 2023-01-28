{

  HQ2X

  Original author: Maxim Stepin
  Pascal translation: Jeremy Darling

}


unit hq2x;

{$DEFINE HAS_INLINE}

interface

type
  hq2xnumber = Longword;
  phq2xnumber=^hq2xnumber;

procedure hq2x_32(Input, Output: Pointer; XResolution, YResolution, Bpl: hq2xnumber);

implementation

{$IFNDEF FPC}
  {$G-}
{$ENDIF}

var
  LUT16to32: array[0..65535] of hq2xnumber;
  RGBtoYUV: array[0..65535] of hq2xnumber;

const
  _Ymask = $00FF0000;
  _Umask = $0000FF00;
  _Vmask = $000000FF;
  _trY   = $00300000;
  _trU   = $00000700;
  _trV   = $00000006;

procedure Interp1(pc: Pointer; c1, c2: hq2xnumber); {$IFDEF HAS_INLINE}inline;{$ENDIF}
begin
  // C Source: *((int*)pc) = (c1*3+c2) >> 2;
  Phq2xnumber(pc)^ := (c1 * 3 + c2) shr 2;
end;

procedure Interp2(pc: Pointer; c1, c2, c3: hq2xnumber); {$IFDEF HAS_INLINE}inline;{$ENDIF}
begin
  // C Source: *((int*)pc) = (c1*2+c2+c3) >> 2;
  Phq2xnumber(pc)^ := (c1 * 2 + c2 + c3) shr 2;
end;

procedure Interp5(pc: Pointer; c1, c2: hq2xnumber); {$IFDEF HAS_INLINE}inline;{$ENDIF}
begin
  // C Source: *((int*)pc) = (c1+c2) >> 1;
  Phq2xnumber(pc)^ := (c1 + c2) shr 1;
end;

procedure Interp6(pc: Pointer; c1, c2, c3: hq2xnumber); {$IFDEF HAS_INLINE}inline;{$ENDIF}
begin
  // C Source: *((int*)pc) = ((((c1 & 0x00FF00)*5 + (c2 & 0x00FF00)*2 + (c3 & 0x00FF00) ) & 0x0007F800) +
  //                         (((c1 & 0xFF00FF)*5 + (c2 & 0xFF00FF)*2 + (c3 & 0xFF00FF) ) & 0x07F807F8)) >> 3;
  Phq2xnumber(pc)^ := ((((c1 and $00FF00) * 5 + (c2 and $00FF00) * 2 + (c3 and $00FF00)) and
    $0007F800) + (((c1 and $FF00FF) * 5 + (c2 and $FF00FF) * 2 +
    (c3 and $FF00FF)) and $07F807F8)) shr 3;
end;

procedure Interp7(pc: Pointer; c1, c2, c3: hq2xnumber); {$IFDEF HAS_INLINE}inline;{$ENDIF}
begin
  // C Source: *((int*)pc) = ((((c1 & 0x00FF00)*6 + (c2 & 0x00FF00) + (c3 & 0x00FF00) ) & 0x0007F800) +
  //                         (((c1 & 0xFF00FF)*6 + (c2 & 0xFF00FF) + (c3 & 0xFF00FF) ) & 0x07F807F8)) >> 3;
  Phq2xnumber(pc)^ := ((((c1 and $00FF00) * 6 + (c2 and $00FF00) + (c3 and $00FF00)) and
    $0007F800) + (((c1 and $FF00FF) * 6 + (c2 and $FF00FF) +
    (c3 and $FF00FF)) and $07F807F8)) shr 3;
end;

procedure Interp9(pc: Pointer; c1, c2, c3: hq2xnumber); {$IFDEF HAS_INLINE}inline;{$ENDIF}
begin
  // C Source: *((int*)pc) = ((((c1 & 0x00FF00)*2 + ((c2 & 0x00FF00) + (c3 & 0x00FF00))*3 ) & 0x0007F800) +
  //                         (((c1 & 0xFF00FF)*2 + ((c2 & 0xFF00FF) + (c3 & 0xFF00FF))*3 ) & 0x07F807F8)) >> 3;
  Phq2xnumber(pc)^ := ((((c1 and $00FF00) * 2 + ((c2 and $00FF00) + (c3 and $00FF00)) * 3) and
    $0007F800) + (((c1 and $FF00FF) * 2 +
    ((c2 and $FF00FF) + (c3 and $FF00FF)) * 3) and $07F807F8)) shr 3;
end;

procedure Interp10(pc: Pointer; c1, c2, c3: hq2xnumber); {$IFDEF HAS_INLINE}inline;{$ENDIF}
begin
  // C Source: *((int*)pc) = ((((c1 & 0x00FF00)*14 + (c2 & 0x00FF00) + (c3 & 0x00FF00) ) & 0x000FF000) +
  //                         (((c1 & 0xFF00FF)*14 + (c2 & 0xFF00FF) + (c3 & 0xFF00FF) ) & 0x0FF00FF0)) >> 4;
  Phq2xnumber(pc)^ := ((((c1 and $00FF00) * 14 + (c2 and $00FF00) + (c3 and $00FF00)) and
    $000FF000) + (((c1 and $FF00FF) * 14 + (c2 and $FF00FF) +
    (c3 and $FF00FF)) and $0FF00FF0)) shr 4;
end;

function Diff(var YUV1, YUV2: hq2xnumber; w1, w2: hq2xnumber): Boolean; {$IFDEF HAS_INLINE}inline;{$ENDIF}
begin
  // C Source:
  //  YUV1 = RGBtoYUV[w1];
  //  YUV2 = RGBtoYUV[w2];
  //  return ( ( abs((YUV1 & Ymask) - (YUV2 & Ymask)) > trY ) ||
  //           ( abs((YUV1 & Umask) - (YUV2 & Umask)) > trU ) ||
  //           ( abs((YUV1 & Vmask) - (YUV2 & Vmask)) > trV ) );
  YUV1 := RGBToYUV[w1];
  YUV2 := RGBToYUV[w2];
  Result := ((Abs(Integer((YUV1 and _Ymask)) - Integer((YUV2 and _Ymask))) > _trY) or
    (Abs(Integer((YUV1 and _Umask)) - Integer((YUV2 and _Umask))) > _trU) or
    (Abs(Integer((YUV1 and _Vmask)) - Integer((YUV2 and _Vmask))) > _trV));
end;

procedure hq2x_32(Input, Output: Pointer; XResolution, YResolution, Bpl: hq2xnumber);
var
  W, C: array[0..9] of hq2xnumber;
  I, J, K: hq2xnumber;
  Prevline, Nextline: hq2xnumber;
  Pattern, Flag: hq2xnumber;
  YUV1, YUV2: hq2xnumber;
begin
  if (Input = nil) or (Output = nil) or (XResolution = 0) or
     (YResolution = 0) or (Bpl = 0) then
  begin
    // Possibly raise exception here
    Exit;
  end;

  // These lines are not crucial but may help in debugging
  FillChar(W, SizeOf(W), 0);
  FillChar(C, SizeOf(C), 0);
  
  // See hq2x.cpp for C Source
  for J := 0 to YResolution - 1 do
  begin
    if J > 0 then
    begin
      Prevline := -XResolution * 2;
    end else
    begin
      Prevline := 0;
    end;

    if J < YResolution - 1 then
    begin
      Nextline := XResolution * 2
    end else
    begin
      Nextline := 0;
    end;

    for I := 0 to XResolution - 1 do
    begin
      W[2] := PWord(PChar(Input) + Prevline)^;
      W[5] := PWord(Input)^;
      W[8] := PWord(PChar(Input) + Nextline)^;

      if I > 0 then
      begin
        W[1] := PWord(PChar(Input) + Prevline - 2)^;
        W[4] := PWord(PChar(Input) - 2)^;
        W[7] := PWord(PChar(Input) + Nextline - 2)^;
      end else
      begin
        W[1] := W[2];
        W[4] := W[5];
        W[7] := W[8];
      end;

      if I < XResolution - 1 then
      begin
        W[3] := PWord(PChar(Input) + Prevline + 2)^;
        W[6] := PWord(PChar(Input) + 2)^;
        W[9] := PWord(PChar(Input) + Nextline + 2)^;
      end else
      begin
        W[3] := W[2];
        W[6] := W[5];
        W[9] := W[8];
      end;

      Pattern := 0;
      Flag := 1;
      YUV1 := RGBtoYUV[W[5]];

      for K := 1 to 9 do
      begin
        if K <> 5 then
        begin
          if W[K] <> W[5] then
          begin
            YUV2 := RGBtoYUV[W[K]];
            if ((Abs((YUV1 and _Ymask) - (YUV2 and _Ymask)) > _trY) or
              (Abs((YUV1 and _Umask) - (YUV2 and _Umask)) > _trU) or
              (Abs((YUV1 and _Vmask) - (YUV2 and _Vmask)) > _trV)) then
            begin
              Pattern := Pattern or Flag;
            end;
          end;
          Flag := Flag shl 1;
        end;

        C[K] := LUT16to32[W[K]];
      end;

      // The big case statement starts here
      case (Pattern) of
        0,
        1,
        4,
        32,
        128,
        5,
        132,
        160,
        33,
        129,
        36,
        133,
        164,
        161,
        37,
        165:
        begin
          Interp2(Output, C[5], C[4], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          //break;
        end;
        2,
        34,
        130,
        162:
        begin
          Interp2(Output, C[5], C[1], C[4]);
          Interp2(PChar(Output) + 4, C[5], C[3], C[6]);
          Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          //break;
        end;
        16,
        17,
        48,
        49:
        begin
          Interp2(Output, C[5], C[4], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[3], C[2]);
          Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[8]);
          //break;
        end;
        64,
        65,
        68,
        69:
        begin
          Interp2(Output, C[5], C[4], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[4]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[6]);
          //break;
        end;
        8,
        12,
        136,
        140:
        begin
          Interp2(Output, C[5], C[1], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[8]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          //break;
        end;
        3,
        35,
        131,
        163:
        begin
          Interp1(Output, C[5], C[4]);
          Interp2(PChar(Output) + 4, C[5], C[3], C[6]);
          Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          //break;
        end;
        6,
        38,
        134,
        166:
        begin
          Interp2(Output, C[5], C[1], C[4]);
          Interp1(PChar(Output) + 4, C[5], C[6]);
          Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          //break;
        end;
        20,
        21,
        52,
        53:
        begin
          Interp2(Output, C[5], C[4], C[2]);
          Interp1(PChar(Output) + 4, C[5], C[2]);
          Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[8]);
          //break;
        end;
        144,
        145,
        176,
        177:
        begin
          Interp2(Output, C[5], C[4], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[3], C[2]);
          Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[8]);
          //break;
        end;
        192,
        193,
        196,
        197:
        begin
          Interp2(Output, C[5], C[4], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[4]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[6]);
          //break;
        end;
        96,
        97,
        100,
        101:
        begin
          Interp2(Output, C[5], C[4], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          Interp1(PChar(Output) + Bpl, C[5], C[4]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[6]);
          //break;
        end;
        40,
        44,
        168,
        172:
        begin
          Interp2(Output, C[5], C[1], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          Interp1(PChar(Output) + Bpl, C[5], C[8]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          //break;
        end;
        9,
        13,
        137,
        141:
        begin
          Interp1(Output, C[5], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[8]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          //break;
        end;
        18,
        50:
        begin
          Interp2(Output, C[5], C[1], C[4]);
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Interp1(PChar(Output) + 4, C[5], C[3]);
          end else
          begin
            Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[8]);
          //break;
        end;
        80,
        81:
        begin
          Interp2(Output, C[5], C[4], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[3], C[2]);
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[4]);
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Interp1(PChar(Output) + Bpl + 4, C[5], C[9]);
          end else
          begin
            Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        72,
        76:
        begin
          Interp2(Output, C[5], C[1], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Interp1(PChar(Output) + Bpl, C[5], C[7]);
          end else
          begin
            Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[6]);
          //break;
        end;
        10,
        138:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Interp1(Output, C[5], C[1]);
          end else
          begin
            Interp2(Output, C[5], C[4], C[2]);
          end;
          Interp2(PChar(Output) + 4, C[5], C[3], C[6]);
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[8]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          //break;
        end;
        66:
        begin
          Interp2(Output, C[5], C[1], C[4]);
          Interp2(PChar(Output) + 4, C[5], C[3], C[6]);
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[4]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[6]);
          //break;
        end;
        24:
        begin
          Interp2(Output, C[5], C[1], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[3], C[2]);
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[8]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[8]);
          //break;
        end;
        7,
        39,
        135:
        begin
          Interp1(Output, C[5], C[4]);
          Interp1(PChar(Output) + 4, C[5], C[6]);
          Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          //break;
        end;
        148,
        149,
        180:
        begin
          Interp2(Output, C[5], C[4], C[2]);
          Interp1(PChar(Output) + 4, C[5], C[2]);
          Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[8]);
          //break;
        end;
        224,
        228,
        225:
        begin
          Interp2(Output, C[5], C[4], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          Interp1(PChar(Output) + Bpl, C[5], C[4]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[6]);
          //break;
        end;
        41,
        169,
        45:
        begin
          Interp1(Output, C[5], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          Interp1(PChar(Output) + Bpl, C[5], C[8]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          //break;
        end;
        22,
        54:
        begin
          Interp2(Output, C[5], C[1], C[4]);
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Phq2xnumber(PChar(Output) + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[8]);
          //break;
        end;
        208,
        209:
        begin
          Interp2(Output, C[5], C[4], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[3], C[2]);
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[4]);
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        104,
        108:
        begin
          Interp2(Output, C[5], C[1], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[6]);
          //break;
        end;
        11,
        139:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Phq2xnumber(Output)^ := C[5];
          end else
          begin
            Interp2(Output, C[5], C[4], C[2]);
          end;
          Interp2(PChar(Output) + 4, C[5], C[3], C[6]);
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[8]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          //break;
        end;
        19,
        51:
        begin
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Interp1(Output, C[5], C[4]);
            Interp1(PChar(Output) + 4, C[5], C[3]);
          end else
          begin
            Interp6(Output, C[5], C[2], C[4]);
            Interp9(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[8]);
          //break;
        end;
        146,
        178:
        begin
          Interp2(Output, C[5], C[1], C[4]);
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Interp1(PChar(Output) + 4, C[5], C[3]);
            Interp1(PChar(Output) + Bpl + 4, C[5], C[8]);
          end else
          begin
            Interp9(PChar(Output) + 4, C[5], C[2], C[6]);
            Interp6(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          //break;
        end;
        84,
        85:
        begin
          Interp2(Output, C[5], C[4], C[2]);
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Interp1(PChar(Output) + 4, C[5], C[2]);
            Interp1(PChar(Output) + Bpl + 4, C[5], C[9]);
          end else
          begin
            Interp6(PChar(Output) + 4, C[5], C[6], C[2]);
            Interp9(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[4]);
          //break;
        end;
        112,
        113:
        begin
          Interp2(Output, C[5], C[4], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[3], C[2]);
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Interp1(PChar(Output) + Bpl, C[5], C[4]);
            Interp1(PChar(Output) + Bpl + 4, C[5], C[9]);
          end else
          begin
            Interp6(PChar(Output) + Bpl, C[5], C[8], C[4]);
            Interp9(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        200,
        204:
        begin
          Interp2(Output, C[5], C[1], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Interp1(PChar(Output) + Bpl, C[5], C[7]);
            Interp1(PChar(Output) + Bpl + 4, C[5], C[6]);
          end else
          begin
            Interp9(PChar(Output) + Bpl, C[5], C[8], C[4]);
            Interp6(PChar(Output) + Bpl + 4, C[5], C[8], C[6]);
          end;
          //break;
        end;
        73,
        77:
        begin
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Interp1(Output, C[5], C[2]);
            Interp1(PChar(Output) + Bpl, C[5], C[7]);
          end else
          begin
            Interp6(Output, C[5], C[4], C[2]);
            Interp9(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[6]);
          //break;
        end;
        42,
        170:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Interp1(Output, C[5], C[1]);
            Interp1(PChar(Output) + Bpl, C[5], C[8]);
          end else
          begin
            Interp9(Output, C[5], C[4], C[2]);
            Interp6(PChar(Output) + Bpl, C[5], C[4], C[8]);
          end;
          Interp2(PChar(Output) + 4, C[5], C[3], C[6]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          //break;
        end;
        14,
        142:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Interp1(Output, C[5], C[1]);
            Interp1(PChar(Output) + 4, C[5], C[6]);
          end else
          begin
            Interp9(Output, C[5], C[4], C[2]);
            Interp6(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[8]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          //break;
        end;
        67:
        begin
          Interp1(Output, C[5], C[4]);
          Interp2(PChar(Output) + 4, C[5], C[3], C[6]);
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[4]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[6]);
          //break;
        end;
        70:
        begin
          Interp2(Output, C[5], C[1], C[4]);
          Interp1(PChar(Output) + 4, C[5], C[6]);
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[4]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[6]);
          //break;
        end;
        28:
        begin
          Interp2(Output, C[5], C[1], C[2]);
          Interp1(PChar(Output) + 4, C[5], C[2]);
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[8]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[8]);
          //break;
        end;
        152:
        begin
          Interp2(Output, C[5], C[1], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[3], C[2]);
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[8]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[8]);
          //break;
        end;
        194:
        begin
          Interp2(Output, C[5], C[1], C[4]);
          Interp2(PChar(Output) + 4, C[5], C[3], C[6]);
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[4]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[6]);
          //break;
        end;
        98:
        begin
          Interp2(Output, C[5], C[1], C[4]);
          Interp2(PChar(Output) + 4, C[5], C[3], C[6]);
          Interp1(PChar(Output) + Bpl, C[5], C[4]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[6]);
          //break;
        end;
        56:
        begin
          Interp2(Output, C[5], C[1], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[3], C[2]);
          Interp1(PChar(Output) + Bpl, C[5], C[8]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[8]);
          //break;
        end;
        25:
        begin
          Interp1(Output, C[5], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[3], C[2]);
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[8]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[8]);
          //break;
        end;
        26,
        31:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Phq2xnumber(Output)^ := C[5];
          end else
          begin
            Interp2(Output, C[5], C[4], C[2]);
          end;
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Phq2xnumber(PChar(Output) + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[8]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[8]);
          //break;
        end;
        82,
        214:
        begin
          Interp2(Output, C[5], C[1], C[4]);
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Phq2xnumber(PChar(Output) + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[4]);
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        88,
        248:
        begin
          Interp2(Output, C[5], C[1], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[3], C[2]);
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        74,
        107:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Phq2xnumber(Output)^ := C[5];
          end else
          begin
            Interp2(Output, C[5], C[4], C[2]);
          end;
          Interp2(PChar(Output) + 4, C[5], C[3], C[6]);
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[6]);
          //break;
        end;
        27:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Phq2xnumber(Output)^ := C[5];
          end else
          begin
            Interp2(Output, C[5], C[4], C[2]);
          end;
          Interp1(PChar(Output) + 4, C[5], C[3]);
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[8]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[8]);
          //break;
        end;
        86:
        begin
          Interp2(Output, C[5], C[1], C[4]);
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Phq2xnumber(PChar(Output) + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[4]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[9]);
          //break;
        end;
        216:
        begin
          Interp2(Output, C[5], C[1], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[3], C[2]);
          Interp1(PChar(Output) + Bpl, C[5], C[7]);
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        106:
        begin
          Interp1(Output, C[5], C[1]);
          Interp2(PChar(Output) + 4, C[5], C[3], C[6]);
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[6]);
          //break;
        end;
        30:
        begin
          Interp1(Output, C[5], C[1]);
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Phq2xnumber(PChar(Output) + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[8]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[8]);
          //break;
        end;
        210:
        begin
          Interp2(Output, C[5], C[1], C[4]);
          Interp1(PChar(Output) + 4, C[5], C[3]);
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[4]);
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        120:
        begin
          Interp2(Output, C[5], C[1], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[3], C[2]);
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          Interp1(PChar(Output) + Bpl + 4, C[5], C[9]);
          //break;
        end;
        75:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Phq2xnumber(Output)^ := C[5];
          end else
          begin
            Interp2(Output, C[5], C[4], C[2]);
          end;
          Interp2(PChar(Output) + 4, C[5], C[3], C[6]);
          Interp1(PChar(Output) + Bpl, C[5], C[7]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[6]);
          //break;
        end;
        29:
        begin
          Interp1(Output, C[5], C[2]);
          Interp1(PChar(Output) + 4, C[5], C[2]);
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[8]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[8]);
          //break;
        end;
        198:
        begin
          Interp2(Output, C[5], C[1], C[4]);
          Interp1(PChar(Output) + 4, C[5], C[6]);
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[4]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[6]);
          //break;
        end;
        184:
        begin
          Interp2(Output, C[5], C[1], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[3], C[2]);
          Interp1(PChar(Output) + Bpl, C[5], C[8]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[8]);
          //break;
        end;
        99:
        begin
          Interp1(Output, C[5], C[4]);
          Interp2(PChar(Output) + 4, C[5], C[3], C[6]);
          Interp1(PChar(Output) + Bpl, C[5], C[4]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[6]);
          //break;
        end;
        57:
        begin
          Interp1(Output, C[5], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[3], C[2]);
          Interp1(PChar(Output) + Bpl, C[5], C[8]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[8]);
          //break;
        end;
        71:
        begin
          Interp1(Output, C[5], C[4]);
          Interp1(PChar(Output) + 4, C[5], C[6]);
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[4]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[6]);
          //break;
        end;
        156:
        begin
          Interp2(Output, C[5], C[1], C[2]);
          Interp1(PChar(Output) + 4, C[5], C[2]);
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[8]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[8]);
          //break;
        end;
        226:
        begin
          Interp2(Output, C[5], C[1], C[4]);
          Interp2(PChar(Output) + 4, C[5], C[3], C[6]);
          Interp1(PChar(Output) + Bpl, C[5], C[4]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[6]);
          //break;
        end;
        60:
        begin
          Interp2(Output, C[5], C[1], C[2]);
          Interp1(PChar(Output) + 4, C[5], C[2]);
          Interp1(PChar(Output) + Bpl, C[5], C[8]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[8]);
          //break;
        end;
        195:
        begin
          Interp1(Output, C[5], C[4]);
          Interp2(PChar(Output) + 4, C[5], C[3], C[6]);
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[4]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[6]);
          //break;
        end;
        102:
        begin
          Interp2(Output, C[5], C[1], C[4]);
          Interp1(PChar(Output) + 4, C[5], C[6]);
          Interp1(PChar(Output) + Bpl, C[5], C[4]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[6]);
          //break;
        end;
        153:
        begin
          Interp1(Output, C[5], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[3], C[2]);
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[8]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[8]);
          //break;
        end;
        58:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Interp1(Output, C[5], C[1]);
          end else
          begin
            Interp7(Output, C[5], C[4], C[2]);
          end;
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Interp1(PChar(Output) + 4, C[5], C[3]);
          end else
          begin
            Interp7(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp1(PChar(Output) + Bpl, C[5], C[8]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[8]);
          //break;
        end;
        83:
        begin
          Interp1(Output, C[5], C[4]);
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Interp1(PChar(Output) + 4, C[5], C[3]);
          end else
          begin
            Interp7(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[4]);
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Interp1(PChar(Output) + Bpl + 4, C[5], C[9]);
          end else
          begin
            Interp7(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        92:
        begin
          Interp2(Output, C[5], C[1], C[2]);
          Interp1(PChar(Output) + 4, C[5], C[2]);
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Interp1(PChar(Output) + Bpl, C[5], C[7]);
          end else
          begin
            Interp7(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Interp1(PChar(Output) + Bpl + 4, C[5], C[9]);
          end else
          begin
            Interp7(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        202:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Interp1(Output, C[5], C[1]);
          end else
          begin
            Interp7(Output, C[5], C[4], C[2]);
          end;
          Interp2(PChar(Output) + 4, C[5], C[3], C[6]);
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Interp1(PChar(Output) + Bpl, C[5], C[7]);
          end else
          begin
            Interp7(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          Interp1(PChar(Output) + Bpl + 4, C[5], C[6]);
          //break;
        end;
        78:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Interp1(Output, C[5], C[1]);
          end else
          begin
            Interp7(Output, C[5], C[4], C[2]);
          end;
          Interp1(PChar(Output) + 4, C[5], C[6]);
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Interp1(PChar(Output) + Bpl, C[5], C[7]);
          end else
          begin
            Interp7(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[6]);
          //break;
        end;
        154:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Interp1(Output, C[5], C[1]);
          end else
          begin
            Interp7(Output, C[5], C[4], C[2]);
          end;
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Interp1(PChar(Output) + 4, C[5], C[3]);
          end else
          begin
            Interp7(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[8]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[8]);
          //break;
        end;
        114:
        begin
          Interp2(Output, C[5], C[1], C[4]);
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Interp1(PChar(Output) + 4, C[5], C[3]);
          end else
          begin
            Interp7(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp1(PChar(Output) + Bpl, C[5], C[4]);
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Interp1(PChar(Output) + Bpl + 4, C[5], C[9]);
          end else
          begin
            Interp7(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        89:
        begin
          Interp1(Output, C[5], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[3], C[2]);
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Interp1(PChar(Output) + Bpl, C[5], C[7]);
          end else
          begin
            Interp7(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Interp1(PChar(Output) + Bpl + 4, C[5], C[9]);
          end else
          begin
            Interp7(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        90:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Interp1(Output, C[5], C[1]);
          end else
          begin
            Interp7(Output, C[5], C[4], C[2]);
          end;
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Interp1(PChar(Output) + 4, C[5], C[3]);
          end else
          begin
            Interp7(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Interp1(PChar(Output) + Bpl, C[5], C[7]);
          end else
          begin
            Interp7(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Interp1(PChar(Output) + Bpl + 4, C[5], C[9]);
          end else
          begin
            Interp7(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        55,
        23:
        begin
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Interp1(Output, C[5], C[4]);
            Phq2xnumber(PChar(Output) + 4)^ := C[5];
          end else
          begin
            Interp6(Output, C[5], C[2], C[4]);
            Interp9(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[8]);
          //break;
        end;
        182,
        150:
        begin
          Interp2(Output, C[5], C[1], C[4]);
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Phq2xnumber(PChar(Output) + 4)^ := C[5];
            Interp1(PChar(Output) + Bpl + 4, C[5], C[8]);
          end else
          begin
            Interp9(PChar(Output) + 4, C[5], C[2], C[6]);
            Interp6(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          //break;
        end;
        213,
        212:
        begin
          Interp2(Output, C[5], C[4], C[2]);
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Interp1(PChar(Output) + 4, C[5], C[2]);
            Phq2xnumber(PChar(Output) + Bpl + 4)^ := C[5];
          end else
          begin
            Interp6(PChar(Output) + 4, C[5], C[6], C[2]);
            Interp9(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[4]);
          //break;
        end;
        241,
        240:
        begin
          Interp2(Output, C[5], C[4], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[3], C[2]);
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Interp1(PChar(Output) + Bpl, C[5], C[4]);
            Phq2xnumber(PChar(Output) + Bpl + 4)^ := C[5];
          end else
          begin
            Interp6(PChar(Output) + Bpl, C[5], C[8], C[4]);
            Interp9(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        236,
        232:
        begin
          Interp2(Output, C[5], C[1], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl)^ := C[5];
            Interp1(PChar(Output) + Bpl + 4, C[5], C[6]);
          end else
          begin
            Interp9(PChar(Output) + Bpl, C[5], C[8], C[4]);
            Interp6(PChar(Output) + Bpl + 4, C[5], C[8], C[6]);
          end;
          //break;
        end;
        109,
        105:
        begin
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Interp1(Output, C[5], C[2]);
            Phq2xnumber(PChar(Output) + Bpl)^ := C[5];
          end else
          begin
            Interp6(Output, C[5], C[4], C[2]);
            Interp9(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[6]);
          //break;
        end;
        171,
        43:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Phq2xnumber(Output)^ := C[5];
            Interp1(PChar(Output) + Bpl, C[5], C[8]);
          end else
          begin
            Interp9(Output, C[5], C[4], C[2]);
            Interp6(PChar(Output) + Bpl, C[5], C[4], C[8]);
          end;
          Interp2(PChar(Output) + 4, C[5], C[3], C[6]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          //break;
        end;
        143,
        15:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Phq2xnumber(Output)^ := C[5];
            Interp1(PChar(Output) + 4, C[5], C[6]);
          end else
          begin
            Interp9(Output, C[5], C[4], C[2]);
            Interp6(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[8]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          //break;
        end;
        124:
        begin
          Interp2(Output, C[5], C[1], C[2]);
          Interp1(PChar(Output) + 4, C[5], C[2]);
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          Interp1(PChar(Output) + Bpl + 4, C[5], C[9]);
          //break;
        end;
        203:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Phq2xnumber(Output)^ := C[5];
          end else
          begin
            Interp2(Output, C[5], C[4], C[2]);
          end;
          Interp2(PChar(Output) + 4, C[5], C[3], C[6]);
          Interp1(PChar(Output) + Bpl, C[5], C[7]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[6]);
          //break;
        end;
        62:
        begin
          Interp1(Output, C[5], C[1]);
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Phq2xnumber(PChar(Output) + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp1(PChar(Output) + Bpl, C[5], C[8]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[8]);
          //break;
        end;
        211:
        begin
          Interp1(Output, C[5], C[4]);
          Interp1(PChar(Output) + 4, C[5], C[3]);
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[4]);
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        118:
        begin
          Interp2(Output, C[5], C[1], C[4]);
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Phq2xnumber(PChar(Output) + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp1(PChar(Output) + Bpl, C[5], C[4]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[9]);
          //break;
        end;
        217:
        begin
          Interp1(Output, C[5], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[3], C[2]);
          Interp1(PChar(Output) + Bpl, C[5], C[7]);
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        110:
        begin
          Interp1(Output, C[5], C[1]);
          Interp1(PChar(Output) + 4, C[5], C[6]);
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[6]);
          //break;
        end;
        155:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Phq2xnumber(Output)^ := C[5];
          end else
          begin
            Interp2(Output, C[5], C[4], C[2]);
          end;
          Interp1(PChar(Output) + 4, C[5], C[3]);
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[8]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[8]);
          //break;
        end;
        188:
        begin
          Interp2(Output, C[5], C[1], C[2]);
          Interp1(PChar(Output) + 4, C[5], C[2]);
          Interp1(PChar(Output) + Bpl, C[5], C[8]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[8]);
          //break;
        end;
        185:
        begin
          Interp1(Output, C[5], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[3], C[2]);
          Interp1(PChar(Output) + Bpl, C[5], C[8]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[8]);
          //break;
        end;
        61:
        begin
          Interp1(Output, C[5], C[2]);
          Interp1(PChar(Output) + 4, C[5], C[2]);
          Interp1(PChar(Output) + Bpl, C[5], C[8]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[8]);
          //break;
        end;
        157:
        begin
          Interp1(Output, C[5], C[2]);
          Interp1(PChar(Output) + 4, C[5], C[2]);
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[8]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[8]);
          //break;
        end;
        103:
        begin
          Interp1(Output, C[5], C[4]);
          Interp1(PChar(Output) + 4, C[5], C[6]);
          Interp1(PChar(Output) + Bpl, C[5], C[4]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[6]);
          //break;
        end;
        227:
        begin
          Interp1(Output, C[5], C[4]);
          Interp2(PChar(Output) + 4, C[5], C[3], C[6]);
          Interp1(PChar(Output) + Bpl, C[5], C[4]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[6]);
          //break;
        end;
        230:
        begin
          Interp2(Output, C[5], C[1], C[4]);
          Interp1(PChar(Output) + 4, C[5], C[6]);
          Interp1(PChar(Output) + Bpl, C[5], C[4]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[6]);
          //break;
        end;
        199:
        begin
          Interp1(Output, C[5], C[4]);
          Interp1(PChar(Output) + 4, C[5], C[6]);
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[4]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[6]);
          //break;
        end;
        220:
        begin
          Interp2(Output, C[5], C[1], C[2]);
          Interp1(PChar(Output) + 4, C[5], C[2]);
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Interp1(PChar(Output) + Bpl, C[5], C[7]);
          end else
          begin
            Interp7(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        158:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Interp1(Output, C[5], C[1]);
          end else
          begin
            Interp7(Output, C[5], C[4], C[2]);
          end;
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Phq2xnumber(PChar(Output) + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[8]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[8]);
          //break;
        end;
        234:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Interp1(Output, C[5], C[1]);
          end else
          begin
            Interp7(Output, C[5], C[4], C[2]);
          end;
          Interp2(PChar(Output) + 4, C[5], C[3], C[6]);
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          Interp1(PChar(Output) + Bpl + 4, C[5], C[6]);
          //break;
        end;
        242:
        begin
          Interp2(Output, C[5], C[1], C[4]);
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Interp1(PChar(Output) + 4, C[5], C[3]);
          end else
          begin
            Interp7(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp1(PChar(Output) + Bpl, C[5], C[4]);
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        59:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Phq2xnumber(Output)^ := C[5];
          end else
          begin
            Interp2(Output, C[5], C[4], C[2]);
          end;
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Interp1(PChar(Output) + 4, C[5], C[3]);
          end else
          begin
            Interp7(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp1(PChar(Output) + Bpl, C[5], C[8]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[8]);
          //break;
        end;
        121:
        begin
          Interp1(Output, C[5], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[3], C[2]);
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Interp1(PChar(Output) + Bpl + 4, C[5], C[9]);
          end else
          begin
            Interp7(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        87:
        begin
          Interp1(Output, C[5], C[4]);
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Phq2xnumber(PChar(Output) + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[4]);
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Interp1(PChar(Output) + Bpl + 4, C[5], C[9]);
          end else
          begin
            Interp7(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        79:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Phq2xnumber(Output)^ := C[5];
          end else
          begin
            Interp2(Output, C[5], C[4], C[2]);
          end;
          Interp1(PChar(Output) + 4, C[5], C[6]);
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Interp1(PChar(Output) + Bpl, C[5], C[7]);
          end else
          begin
            Interp7(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[6]);
          //break;
        end;
        122:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Interp1(Output, C[5], C[1]);
          end else
          begin
            Interp7(Output, C[5], C[4], C[2]);
          end;
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Interp1(PChar(Output) + 4, C[5], C[3]);
          end else
          begin
            Interp7(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Interp1(PChar(Output) + Bpl + 4, C[5], C[9]);
          end else
          begin
            Interp7(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        94:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Interp1(Output, C[5], C[1]);
          end else
          begin
            Interp7(Output, C[5], C[4], C[2]);
          end;
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Phq2xnumber(PChar(Output) + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Interp1(PChar(Output) + Bpl, C[5], C[7]);
          end else
          begin
            Interp7(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Interp1(PChar(Output) + Bpl + 4, C[5], C[9]);
          end else
          begin
            Interp7(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        218:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Interp1(Output, C[5], C[1]);
          end else
          begin
            Interp7(Output, C[5], C[4], C[2]);
          end;
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Interp1(PChar(Output) + 4, C[5], C[3]);
          end else
          begin
            Interp7(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Interp1(PChar(Output) + Bpl, C[5], C[7]);
          end else
          begin
            Interp7(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        91:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Phq2xnumber(Output)^ := C[5];
          end else
          begin
            Interp2(Output, C[5], C[4], C[2]);
          end;
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Interp1(PChar(Output) + 4, C[5], C[3]);
          end else
          begin
            Interp7(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Interp1(PChar(Output) + Bpl, C[5], C[7]);
          end else
          begin
            Interp7(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Interp1(PChar(Output) + Bpl + 4, C[5], C[9]);
          end else
          begin
            Interp7(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        229:
        begin
          Interp2(Output, C[5], C[4], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          Interp1(PChar(Output) + Bpl, C[5], C[4]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[6]);
          //break;
        end;
        167:
        begin
          Interp1(Output, C[5], C[4]);
          Interp1(PChar(Output) + 4, C[5], C[6]);
          Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          //break;
        end;
        173:
        begin
          Interp1(Output, C[5], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          Interp1(PChar(Output) + Bpl, C[5], C[8]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          //break;
        end;
        181:
        begin
          Interp2(Output, C[5], C[4], C[2]);
          Interp1(PChar(Output) + 4, C[5], C[2]);
          Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[8]);
          //break;
        end;
        186:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Interp1(Output, C[5], C[1]);
          end else
          begin
            Interp7(Output, C[5], C[4], C[2]);
          end;
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Interp1(PChar(Output) + 4, C[5], C[3]);
          end else
          begin
            Interp7(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp1(PChar(Output) + Bpl, C[5], C[8]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[8]);
          //break;
        end;
        115:
        begin
          Interp1(Output, C[5], C[4]);
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Interp1(PChar(Output) + 4, C[5], C[3]);
          end else
          begin
            Interp7(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp1(PChar(Output) + Bpl, C[5], C[4]);
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Interp1(PChar(Output) + Bpl + 4, C[5], C[9]);
          end else
          begin
            Interp7(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        93:
        begin
          Interp1(Output, C[5], C[2]);
          Interp1(PChar(Output) + 4, C[5], C[2]);
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Interp1(PChar(Output) + Bpl, C[5], C[7]);
          end else
          begin
            Interp7(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Interp1(PChar(Output) + Bpl + 4, C[5], C[9]);
          end else
          begin
            Interp7(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        206:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Interp1(Output, C[5], C[1]);
          end else
          begin
            Interp7(Output, C[5], C[4], C[2]);
          end;
          Interp1(PChar(Output) + 4, C[5], C[6]);
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Interp1(PChar(Output) + Bpl, C[5], C[7]);
          end else
          begin
            Interp7(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          Interp1(PChar(Output) + Bpl + 4, C[5], C[6]);
          //break;
        end;
        205,
        201:
        begin
          Interp1(Output, C[5], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Interp1(PChar(Output) + Bpl, C[5], C[7]);
          end else
          begin
            Interp7(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          Interp1(PChar(Output) + Bpl + 4, C[5], C[6]);
          //break;
        end;
        174,
        46:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Interp1(Output, C[5], C[1]);
          end else
          begin
            Interp7(Output, C[5], C[4], C[2]);
          end;
          Interp1(PChar(Output) + 4, C[5], C[6]);
          Interp1(PChar(Output) + Bpl, C[5], C[8]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          //break;
        end;
        179,
        147:
        begin
          Interp1(Output, C[5], C[4]);
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Interp1(PChar(Output) + 4, C[5], C[3]);
          end else
          begin
            Interp7(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[8]);
          //break;
        end;
        117,
        116:
        begin
          Interp2(Output, C[5], C[4], C[2]);
          Interp1(PChar(Output) + 4, C[5], C[2]);
          Interp1(PChar(Output) + Bpl, C[5], C[4]);
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Interp1(PChar(Output) + Bpl + 4, C[5], C[9]);
          end else
          begin
            Interp7(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        189:
        begin
          Interp1(Output, C[5], C[2]);
          Interp1(PChar(Output) + 4, C[5], C[2]);
          Interp1(PChar(Output) + Bpl, C[5], C[8]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[8]);
          //break;
        end;
        231:
        begin
          Interp1(Output, C[5], C[4]);
          Interp1(PChar(Output) + 4, C[5], C[6]);
          Interp1(PChar(Output) + Bpl, C[5], C[4]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[6]);
          //break;
        end;
        126:
        begin
          Interp1(Output, C[5], C[1]);
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Phq2xnumber(PChar(Output) + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          Interp1(PChar(Output) + Bpl + 4, C[5], C[9]);
          //break;
        end;
        219:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Phq2xnumber(Output)^ := C[5];
          end else
          begin
            Interp2(Output, C[5], C[4], C[2]);
          end;
          Interp1(PChar(Output) + 4, C[5], C[3]);
          Interp1(PChar(Output) + Bpl, C[5], C[7]);
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        125:
        begin
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Interp1(Output, C[5], C[2]);
            Phq2xnumber(PChar(Output) + Bpl)^ := C[5];
          end else
          begin
            Interp6(Output, C[5], C[4], C[2]);
            Interp9(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          Interp1(PChar(Output) + 4, C[5], C[2]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[9]);
          //break;
        end;
        221:
        begin
          Interp1(Output, C[5], C[2]);
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Interp1(PChar(Output) + 4, C[5], C[2]);
            Phq2xnumber(PChar(Output) + Bpl + 4)^ := C[5];
          end else
          begin
            Interp6(PChar(Output) + 4, C[5], C[6], C[2]);
            Interp9(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          Interp1(PChar(Output) + Bpl, C[5], C[7]);
          //break;
        end;
        207:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Phq2xnumber(Output)^ := C[5];
            Interp1(PChar(Output) + 4, C[5], C[6]);
          end else
          begin
            Interp9(Output, C[5], C[4], C[2]);
            Interp6(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp1(PChar(Output) + Bpl, C[5], C[7]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[6]);
          //break;
        end;
        238:
        begin
          Interp1(Output, C[5], C[1]);
          Interp1(PChar(Output) + 4, C[5], C[6]);
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl)^ := C[5];
            Interp1(PChar(Output) + Bpl + 4, C[5], C[6]);
          end else
          begin
            Interp9(PChar(Output) + Bpl, C[5], C[8], C[4]);
            Interp6(PChar(Output) + Bpl + 4, C[5], C[8], C[6]);
          end;
          //break;
        end;
        190:
        begin
          Interp1(Output, C[5], C[1]);
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Phq2xnumber(PChar(Output) + 4)^ := C[5];
            Interp1(PChar(Output) + Bpl + 4, C[5], C[8]);
          end else
          begin
            Interp9(PChar(Output) + 4, C[5], C[2], C[6]);
            Interp6(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          Interp1(PChar(Output) + Bpl, C[5], C[8]);
          //break;
        end;
        187:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Phq2xnumber(Output)^ := C[5];
            Interp1(PChar(Output) + Bpl, C[5], C[8]);
          end else
          begin
            Interp9(Output, C[5], C[4], C[2]);
            Interp6(PChar(Output) + Bpl, C[5], C[4], C[8]);
          end;
          Interp1(PChar(Output) + 4, C[5], C[3]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[8]);
          //break;
        end;
        243:
        begin
          Interp1(Output, C[5], C[4]);
          Interp1(PChar(Output) + 4, C[5], C[3]);
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Interp1(PChar(Output) + Bpl, C[5], C[4]);
            Phq2xnumber(PChar(Output) + Bpl + 4)^ := C[5];
          end else
          begin
            Interp6(PChar(Output) + Bpl, C[5], C[8], C[4]);
            Interp9(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        119:
        begin
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Interp1(Output, C[5], C[4]);
            Phq2xnumber(PChar(Output) + 4)^ := C[5];
          end else
          begin
            Interp6(Output, C[5], C[2], C[4]);
            Interp9(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp1(PChar(Output) + Bpl, C[5], C[4]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[9]);
          //break;
        end;
        237,
        233:
        begin
          Interp1(Output, C[5], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl)^ := C[5];
          end else
          begin
            Interp10(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          Interp1(PChar(Output) + Bpl + 4, C[5], C[6]);
          //break;
        end;
        175,
        47:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Phq2xnumber(Output)^ := C[5];
          end else
          begin
            Interp10(Output, C[5], C[4], C[2]);
          end;
          Interp1(PChar(Output) + 4, C[5], C[6]);
          Interp1(PChar(Output) + Bpl, C[5], C[8]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          //break;
        end;
        183,
        151:
        begin
          Interp1(Output, C[5], C[4]);
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Phq2xnumber(PChar(Output) + 4)^ := C[5];
          end else
          begin
            Interp10(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[8]);
          //break;
        end;
        245,
        244:
        begin
          Interp2(Output, C[5], C[4], C[2]);
          Interp1(PChar(Output) + 4, C[5], C[2]);
          Interp1(PChar(Output) + Bpl, C[5], C[4]);
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl + 4)^ := C[5];
          end else
          begin
            Interp10(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        250:
        begin
          Interp1(Output, C[5], C[1]);
          Interp1(PChar(Output) + 4, C[5], C[3]);
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        123:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Phq2xnumber(Output)^ := C[5];
          end else
          begin
            Interp2(Output, C[5], C[4], C[2]);
          end;
          Interp1(PChar(Output) + 4, C[5], C[3]);
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          Interp1(PChar(Output) + Bpl + 4, C[5], C[9]);
          //break;
        end;
        95:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Phq2xnumber(Output)^ := C[5];
          end else
          begin
            Interp2(Output, C[5], C[4], C[2]);
          end;
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Phq2xnumber(PChar(Output) + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp1(PChar(Output) + Bpl, C[5], C[7]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[9]);
          //break;
        end;
        222:
        begin
          Interp1(Output, C[5], C[1]);
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Phq2xnumber(PChar(Output) + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp1(PChar(Output) + Bpl, C[5], C[7]);
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        252:
        begin
          Interp2(Output, C[5], C[1], C[2]);
          Interp1(PChar(Output) + 4, C[5], C[2]);
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl + 4)^ := C[5];
          end else
          begin
            Interp10(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        249:
        begin
          Interp1(Output, C[5], C[2]);
          Interp2(PChar(Output) + 4, C[5], C[3], C[2]);
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl)^ := C[5];
          end else
          begin
            Interp10(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        235:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Phq2xnumber(Output)^ := C[5];
          end else
          begin
            Interp2(Output, C[5], C[4], C[2]);
          end;
          Interp2(PChar(Output) + 4, C[5], C[3], C[6]);
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl)^ := C[5];
          end else
          begin
            Interp10(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          Interp1(PChar(Output) + Bpl + 4, C[5], C[6]);
          //break;
        end;
        111:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Phq2xnumber(Output)^ := C[5];
          end else
          begin
            Interp10(Output, C[5], C[4], C[2]);
          end;
          Interp1(PChar(Output) + 4, C[5], C[6]);
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[6]);
          //break;
        end;
        63:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Phq2xnumber(Output)^ := C[5];
          end else
          begin
            Interp10(Output, C[5], C[4], C[2]);
          end;
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Phq2xnumber(PChar(Output) + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp1(PChar(Output) + Bpl, C[5], C[8]);
          Interp2(PChar(Output) + Bpl + 4, C[5], C[9], C[8]);
          //break;
        end;
        159:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Phq2xnumber(Output)^ := C[5];
          end else
          begin
            Interp2(Output, C[5], C[4], C[2]);
          end;
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Phq2xnumber(PChar(Output) + 4)^ := C[5];
          end else
          begin
            Interp10(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[8]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[8]);
          //break;
        end;
        215:
        begin
          Interp1(Output, C[5], C[4]);
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Phq2xnumber(PChar(Output) + 4)^ := C[5];
          end else
          begin
            Interp10(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp2(PChar(Output) + Bpl, C[5], C[7], C[4]);
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        246:
        begin
          Interp2(Output, C[5], C[1], C[4]);
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Phq2xnumber(PChar(Output) + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp1(PChar(Output) + Bpl, C[5], C[4]);
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl + 4)^ := C[5];
          end else
          begin
            Interp10(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        254:
        begin
          Interp1(Output, C[5], C[1]);
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Phq2xnumber(PChar(Output) + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl + 4)^ := C[5];
          end else
          begin
            Interp10(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        253:
        begin
          Interp1(Output, C[5], C[2]);
          Interp1(PChar(Output) + 4, C[5], C[2]);
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl)^ := C[5];
          end else
          begin
            Interp10(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl + 4)^ := C[5];
          end else
          begin
            Interp10(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        251:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Phq2xnumber(Output)^ := C[5];
          end else
          begin
            Interp2(Output, C[5], C[4], C[2]);
          end;
          Interp1(PChar(Output) + 4, C[5], C[3]);
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl)^ := C[5];
          end else
          begin
            Interp10(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        239:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Phq2xnumber(Output)^ := C[5];
          end else
          begin
            Interp10(Output, C[5], C[4], C[2]);
          end;
          Interp1(PChar(Output) + 4, C[5], C[6]);
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl)^ := C[5];
          end else
          begin
            Interp10(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          Interp1(PChar(Output) + Bpl + 4, C[5], C[6]);
          //break;
        end;
        127:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Phq2xnumber(Output)^ := C[5];
          end else
          begin
            Interp10(Output, C[5], C[4], C[2]);
          end;
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Phq2xnumber(PChar(Output) + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          Interp1(PChar(Output) + Bpl + 4, C[5], C[9]);
          //break;
        end;
        191:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Phq2xnumber(Output)^ := C[5];
          end else
          begin
            Interp10(Output, C[5], C[4], C[2]);
          end;
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Phq2xnumber(PChar(Output) + 4)^ := C[5];
          end else
          begin
            Interp10(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp1(PChar(Output) + Bpl, C[5], C[8]);
          Interp1(PChar(Output) + Bpl + 4, C[5], C[8]);
          //break;
        end;
        223:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Phq2xnumber(Output)^ := C[5];
          end else
          begin
            Interp2(Output, C[5], C[4], C[2]);
          end;
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Phq2xnumber(PChar(Output) + 4)^ := C[5];
          end else
          begin
            Interp10(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp1(PChar(Output) + Bpl, C[5], C[7]);
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl + 4)^ := C[5];
          end else
          begin
            Interp2(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        247:
        begin
          Interp1(Output, C[5], C[4]);
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Phq2xnumber(PChar(Output) + 4)^ := C[5];
          end else
          begin
            Interp10(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          Interp1(PChar(Output) + Bpl, C[5], C[4]);
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl + 4)^ := C[5];
          end else
          begin
            Interp10(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
        255:
        begin
          if Diff(YUV1, YUV2, W[4], W[2]) then
          begin
            Phq2xnumber(Output)^ := C[5];
          end else
          begin
            Interp10(Output, C[5], C[4], C[2]);
          end;
          if Diff(YUV1, YUV2, W[2], W[6]) then
          begin
            Phq2xnumber(PChar(Output) + 4)^ := C[5];
          end else
          begin
            Interp10(PChar(Output) + 4, C[5], C[2], C[6]);
          end;
          if Diff(YUV1, YUV2, W[8], W[4]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl)^ := C[5];
          end else
          begin
            Interp10(PChar(Output) + Bpl, C[5], C[8], C[4]);
          end;
          if Diff(YUV1, YUV2, W[6], W[8]) then
          begin
            Phq2xnumber(PChar(Output) + Bpl + 4)^ := C[5];
          end else
          begin
            Interp10(PChar(Output) + Bpl + 4, C[5], C[6], C[8]);
          end;
          //break;
        end;
      end;
      // The big case statement ends here

      Inc(PByte(Input), 2);
      Inc(PByte(Output), 8);
    end;
    Inc(PByte(Output), Bpl);
  end;
end;

procedure InitLUTs;
var
  I, J, K, R, G, B, Y, U, V: hq2xnumber;
begin
  // C Source:
  //  int i, j, k, r, g, b, Y, u, v;
  //  for (i=0; i<65536; i++)
  //    LUT16to32[i] = ((i & 0xF800) << 8) + ((i & 0x07E0) << 5) + ((i & 0x001F) << 3);
  //  for (i=0; i<32; i++)
  //  for (j=0; j<64; j++)
  //  for (k=0; k<32; k++)
  //  {
  //    r = i << 3;
  //    g = j << 2;
  //    b = k << 3;
  //    Y = (r + g + b) >> 2;
  //    u = 128 + ((r - b) >> 2);
  //    v = 128 + ((-r + 2*g -b)>>3);
  //    RGBtoYUV[ (i << 11) + (j << 5) + k ] = (Y<<16) + (u<<8) + v;
  //  }
  for I := 0 to High(LUT16to32) do
  begin
    LUT16to32[I] := ((I and $F800) shl 8) + ((I and $7E0) shl 5) + ((I and $1F) shl 3);
  end;

  for I := 0 to 31 do
  begin
    for J := 0 to 63 do
    begin
      for K := 0 to 31 do
      begin
        R := I shl 3;
        G := J shl 2;
        B := K shl 3;
        Y := (R + G + B) shr 2;
        U := 128 + ((R - B) shr 2);
        // original line: v = 128 + ((-r + 2*g -b)>>3);
        // Changed so that FPC wouldn't complain about mixing signed values
        V := 128 + ((2 * G - B - R) shr 3);
        RGBtoYUV[(I shl 11) + (J shl 5) + K] := (Y shl 16) + (U shl 8) + V;
      end;
    end;
  end;
end;

initialization
  InitLUTs;

end.
