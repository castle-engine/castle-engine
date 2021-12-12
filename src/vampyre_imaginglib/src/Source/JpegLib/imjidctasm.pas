unit imjidctasm;

{ This file contains a slow-but-accurate integer implementation of the
  inverse DCT (Discrete Cosine Transform).  In the IJG code, this routine
  must also perform dequantization of the input coefficients.

  A 2-D IDCT can be done by 1-D IDCT on each column followed by 1-D IDCT
  on each row (or vice versa, but it's more convenient to emit a row at
  a time).  Direct algorithms are also available, but they are much more
  complex and seem not to be any faster when reduced to code.

  This implementation is based on an algorithm described in
    C. Loeffler, A. Ligtenberg and G. Moschytz, "Practical Fast 1-D DCT
    Algorithms with 11 Multiplications", Proc. Int'l. Conf. on Acoustics,
    Speech, and Signal Processing 1989 (ICASSP '89), pp. 988-991.
  The primary algorithm described there uses 11 multiplies and 29 adds.
  We use their alternate method with 12 multiplies and 32 adds.
  The advantage of this method is that no data path contains more than one
  multiplication; this allows a very simple and accurate implementation in
  scaled fixed-point arithmetic, with a minimal number of shifts. }

{ Original : jidctint.c ;  Copyright (C) 1991-1996, Thomas G. Lane. }
{ ;-------------------------------------------------------------------------
  ; JIDCTINT.ASM
  ; 80386 protected mode assembly translation of JIDCTINT.C
  ; **** Optimized to all hell by Jason M. Felice (jasonf@apk.net) ****
  ; **** E-mail welcome											 ****
  ;
  ; ** This code does not make O/S calls -- use it for OS/2, Win95, WinNT,
  ; ** DOS prot. mode., Linux, whatever... have fun.
  ;
  ; ** Note, this code is dependant on the structure member order in the .h
  ; ** files for the following structures:
  ;	-- amazingly NOT j_decompress_struct... cool.
  ;	-- jpeg_component_info (dependant on position of dct_table element)
  ;
  ; Originally created with the /Fa option of MSVC 4.0 (why work when you
  ; don't have to?)
  ;
  ; (this code, when compiled is 1K bytes smaller than the optimized MSVC
  ; release build, not to mention 120-130 ms faster in my profile test with 1
  ; small color and and 1 medium black-and-white jpeg: stats using TASM 4.0
  ; and MSVC 4.0 to create a non-console app; jpeg_idct_islow accumulated
  ; 5,760 hits on all trials)
  ;
  ; TASM -t -ml -os jidctint.asm, jidctint.obj
  ;-------------------------------------------------------------------------
   Converted to Delphi 2.0 BASM for PasJPEG
   by Jacques NOMSSI NZALI  <nomssi@physik.tu-chemnitz.de>
   October 13th 1996
    * assumes Delphi "register" calling convention
        first 3 parameter are in EAX,EDX,ECX
    * register allocation revised
}

interface

{$I imjconfig.inc}

uses
  imjmorecfg,
  imjinclude,
  imjpeglib,
  imjdct;         { Private declarations for DCT subsystem }

{ Perform dequantization and inverse DCT on one block of coefficients. }

{GLOBAL}
procedure jpeg_idct_islow (cinfo : j_decompress_ptr;
                          compptr : jpeg_component_info_ptr;
		          coef_block : JCOEFPTR;
		          output_buf : JSAMPARRAY;
                          output_col : JDIMENSION);

implementation

{ This module is specialized to the case DCTSIZE = 8. }

{$ifndef DCTSIZE_IS_8}
  Sorry, this code only copes with 8x8 DCTs. { deliberate syntax err }
{$endif}

{ The poop on this scaling stuff is as follows:

  Each 1-D IDCT step produces outputs which are a factor of sqrt(N)
  larger than the true IDCT outputs.  The final outputs are therefore
  a factor of N larger than desired; since N=8 this can be cured by
  a simple right shift at the end of the algorithm.  The advantage of
  this arrangement is that we save two multiplications per 1-D IDCT,
  because the y0 and y4 inputs need not be divided by sqrt(N).

  We have to do addition and subtraction of the integer inputs, which
  is no problem, and multiplication by fractional constants, which is
  a problem to do in integer arithmetic.  We multiply all the constants
  by CONST_SCALE and convert them to integer constants (thus retaining
  CONST_BITS bits of precision in the constants).  After doing a
  multiplication we have to divide the product by CONST_SCALE, with proper
  rounding, to produce the correct output.  This division can be done
  cheaply as a right shift of CONST_BITS bits.  We postpone shifting
  as long as possible so that partial sums can be added together with
  full fractional precision.

  The outputs of the first pass are scaled up by PASS1_BITS bits so that
  they are represented to better-than-integral precision.  These outputs
  require BITS_IN_JSAMPLE + PASS1_BITS + 3 bits; this fits in a 16-bit word
  with the recommended scaling.  (To scale up 12-bit sample data further, an
  intermediate INT32 array would be needed.)

  To avoid overflow of the 32-bit intermediate results in pass 2, we must
  have BITS_IN_JSAMPLE + CONST_BITS + PASS1_BITS <= 26.  Error analysis
  shows that the values given below are the most effective. }

const
  CONST_BITS = 13;

{$ifdef BITS_IN_JSAMPLE_IS_8}
const
  PASS1_BITS = 2;
{$else}
const
  PASS1_BITS = 1;	{ lose a little precision to avoid overflow }
{$endif}

const
  CONST_SCALE = (INT32(1) shl CONST_BITS);

const
  FIX_0_298631336 = INT32(Round(CONST_SCALE * 0.298631336));  {2446}
  FIX_0_390180644 = INT32(Round(CONST_SCALE * 0.390180644));  {3196}
  FIX_0_541196100 = INT32(Round(CONST_SCALE * 0.541196100));  {4433}
  FIX_0_765366865 = INT32(Round(CONST_SCALE * 0.765366865));  {6270}
  FIX_0_899976223 = INT32(Round(CONST_SCALE * 0.899976223));  {7373}
  FIX_1_175875602 = INT32(Round(CONST_SCALE * 1.175875602));  {9633}
  FIX_1_501321110 = INT32(Round(CONST_SCALE * 1.501321110));  {12299}
  FIX_1_847759065 = INT32(Round(CONST_SCALE * 1.847759065));  {15137}
  FIX_1_961570560 = INT32(Round(CONST_SCALE * 1.961570560));  {16069}
  FIX_2_053119869 = INT32(Round(CONST_SCALE * 2.053119869));  {16819}
  FIX_2_562915447 = INT32(Round(CONST_SCALE * 2.562915447));  {20995}
  FIX_3_072711026 = INT32(Round(CONST_SCALE * 3.072711026));  {25172}


{ for DESCALE }
const
  ROUND_CONST = (INT32(1) shl (CONST_BITS-PASS1_BITS-1));
const
  ROUND_CONST_2 = (INT32(1) shl (CONST_BITS+PASS1_BITS+3-1));

{ Perform dequantization and inverse DCT on one block of coefficients. }

{GLOBAL}
procedure jpeg_idct_islow (cinfo : j_decompress_ptr;
                           compptr : jpeg_component_info_ptr;
		           coef_block : JCOEFPTR;
		           output_buf : JSAMPARRAY;
                           output_col : JDIMENSION);
type
  PWorkspace = ^TWorkspace;
  TWorkspace = coef_bits_field; { buffers data between passes }
const
  coefDCTSIZE = DCTSIZE*SizeOf(JCOEF);
  wrkDCTSIZE = DCTSIZE*SizeOf(int);
var
  tmp0, tmp1, tmp2, tmp3 : INT32;
  tmp10, tmp11, tmp12, tmp13 : INT32;
  z1, z2, z3, z4, z5 : INT32;
var
  inptr : JCOEFPTR;
  quantptr : ISLOW_MULT_TYPE_FIELD_PTR;
  wsptr : PWorkspace;
  outptr : JSAMPROW;
var
  range_limit : JSAMPROW;
  ctr : int;
  workspace : TWorkspace;
var
  dcval : int;
var
  dcval_ : JSAMPLE;
asm
  push  edi
  push  esi
  push  ebx

  cld	{ The only direction we use, might as well set it now, as opposed }
        { to inside 2 loops. }

{ Each IDCT routine is responsible for range-limiting its results and
  converting them to unsigned form (0..MAXJSAMPLE).  The raw outputs could
  be quite far out of range if the input data is corrupt, so a bulletproof
  range-limiting step is required.  We use a mask-and-table-lookup method
  to do the combined operations quickly.  See the comments with
  prepare_range_limit_table (in jdmaster.c) for more info. }

  {range_limit := JSAMPROW(@(cinfo^.sample_range_limit^[CENTERJSAMPLE]));}
  mov	eax, [eax].jpeg_decompress_struct.sample_range_limit {eax=cinfo}
  add	eax, (MAXJSAMPLE+1 + CENTERJSAMPLE)*(Type JSAMPLE)
  mov	range_limit, eax

  { Pass 1: process columns from input, store into work array. }
  { Note results are scaled up by sqrt(8) compared to a true IDCT; }
  { furthermore, we scale the results by 2**PASS1_BITS. }

  {inptr := coef_block;}
  mov	esi, ecx     { ecx=coef_block }
  {quantptr := ISLOW_MULT_TYPE_FIELD_PTR (compptr^.dct_table);}
  mov	edi, [edx].jpeg_component_info.dct_table  { edx=compptr }

  {wsptr := PWorkspace(@workspace);}
  lea	ecx, workspace

  {for ctr := pred(DCTSIZE) downto 0 do
  begin}
  mov	ctr, DCTSIZE
@loop518:
    { Due to quantization, we will usually find that many of the input
      coefficients are zero, especially the AC terms.  We can exploit this
      by short-circuiting the IDCT calculation for any column in which all
      the AC terms are zero.  In that case each output is equal to the
      DC coefficient (with scale factor as needed).
      With typical images and quantization tables, half or more of the
      column DCT calculations can be simplified this way. }

    {if ((inptr^[DCTSIZE*1]) or (inptr^[DCTSIZE*2]) or (inptr^[DCTSIZE*3]) or
	(inptr^[DCTSIZE*4]) or (inptr^[DCTSIZE*5]) or (inptr^[DCTSIZE*6]) or
	(inptr^[DCTSIZE*7]) = 0) then
    begin}
  mov	eax, DWORD PTR [esi+coefDCTSIZE*1]
  or	eax, DWORD PTR [esi+coefDCTSIZE*2]
  or	eax, DWORD PTR [esi+coefDCTSIZE*3]
  mov	edx, DWORD PTR [esi+coefDCTSIZE*4]
  or    eax, edx
  or	eax, DWORD PTR [esi+coefDCTSIZE*5]
  or	eax, DWORD PTR [esi+coefDCTSIZE*6]
  or	eax, DWORD PTR [esi+coefDCTSIZE*7]
  jne	@loop520

      { AC terms all zero }
      {dcval := ISLOW_MULT_TYPE(inptr^[DCTSIZE*0]) *
               (quantptr^[DCTSIZE*0]) shl PASS1_BITS;}
  mov	eax, DWORD PTR [esi+coefDCTSIZE*0]
  imul	eax, DWORD PTR [edi+wrkDCTSIZE*0]
  shl	eax, PASS1_BITS

  {wsptr^[DCTSIZE*0] := dcval;
  wsptr^[DCTSIZE*1] := dcval;
  wsptr^[DCTSIZE*2] := dcval;
  wsptr^[DCTSIZE*3] := dcval;
  wsptr^[DCTSIZE*4] := dcval;
  wsptr^[DCTSIZE*5] := dcval;
  wsptr^[DCTSIZE*6] := dcval;
  wsptr^[DCTSIZE*7] := dcval;}

  mov	DWORD PTR [ecx+ wrkDCTSIZE*0], eax
  mov	DWORD PTR [ecx+ wrkDCTSIZE*1], eax
  mov	DWORD PTR [ecx+ wrkDCTSIZE*2], eax
  mov	DWORD PTR [ecx+ wrkDCTSIZE*3], eax
  mov	DWORD PTR [ecx+ wrkDCTSIZE*4], eax
  mov	DWORD PTR [ecx+ wrkDCTSIZE*5], eax
  mov	DWORD PTR [ecx+ wrkDCTSIZE*6], eax
  mov	DWORD PTR [ecx+ wrkDCTSIZE*7], eax

      {Inc(JCOEF_PTR(inptr));		{ advance pointers to next column }
      {Inc(ISLOW_MULT_TYPE_PTR(quantptr));
      Inc(int_ptr(wsptr));
      continue;}
  dec	ctr
  je	@loop519

  add   esi, Type JCOEF
  add	edi, Type ISLOW_MULT_TYPE
  add	ecx, Type int  { int_ptr }
  jmp	@loop518

@loop520:

    {end;}

    { Even part: reverse the even part of the forward DCT. }
    { The rotator is sqrt(2)*c(-6). }

    {z2 := ISLOW_MULT_TYPE(inptr^[DCTSIZE*2]) * quantptr^[DCTSIZE*2];
    z3 := ISLOW_MULT_TYPE(inptr^[DCTSIZE*6]) * quantptr^[DCTSIZE*6];

    z1 := (z2 + z3) * INT32(FIX_0_541196100);
    tmp2 := z1 + INT32(z3) * INT32(- FIX_1_847759065);
    tmp3 := z1 + INT32(z2) * INT32(FIX_0_765366865);}

  mov	edx, DWORD PTR [esi+coefDCTSIZE*2]
  imul	edx, DWORD PTR [edi+wrkDCTSIZE*2]  {z2}

  mov	eax, DWORD PTR [esi+coefDCTSIZE*6]
  imul	eax, DWORD PTR [edi+wrkDCTSIZE*6]  {z3}

  lea   ebx, [eax+edx]
  imul  ebx, FIX_0_541196100               {z1}

  imul  eax, (-FIX_1_847759065)
  add   eax, ebx
  mov   tmp2, eax

  imul  edx, FIX_0_765366865
  add   edx, ebx
  mov   tmp3, edx

    {z2 := ISLOW_MULT_TYPE(inptr^[DCTSIZE*0]) * quantptr^[DCTSIZE*0];
    z3 := ISLOW_MULT_TYPE(inptr^[DCTSIZE*4]) * quantptr^[DCTSIZE*4];}

  mov	edx, DWORD PTR [esi+coefDCTSIZE*4]
  imul	edx, DWORD PTR [edi+wrkDCTSIZE*4]      { z3 = edx }

  mov	eax, DWORD PTR [esi+coefDCTSIZE*0]
  imul	eax, DWORD PTR [edi+wrkDCTSIZE*0]      { z2 = eax }

    {tmp0 := (z2 + z3) shl CONST_BITS;
    tmp1 := (z2 - z3) shl CONST_BITS;}
  lea ebx,[eax+edx]
  sub eax, edx
  shl ebx, CONST_BITS                          { tmp0 = ebx }
  shl eax, CONST_BITS                          { tmp1 = eax }

    {tmp10 := tmp0 + tmp3;
    tmp13 := tmp0 - tmp3;}
  mov edx, tmp3
  sub ebx, edx
  mov tmp13, ebx
  add edx, edx
  add ebx, edx
  mov tmp10, ebx

    {tmp11 := tmp1 + tmp2;
    tmp12 := tmp1 - tmp2;}
  mov   ebx, tmp2
  sub   eax, ebx
  mov   tmp12, eax
  add   ebx, ebx
  add   eax, ebx
  mov	tmp11, eax

    { Odd part per figure 8; the matrix is unitary and hence its
      transpose is its inverse.  i0..i3 are y7,y5,y3,y1 respectively. }

    {tmp0 := ISLOW_MULT_TYPE(inptr^[DCTSIZE*7]) * quantptr^[DCTSIZE*7];}
  mov	eax, DWORD PTR [esi+coefDCTSIZE*7]
  imul	eax, DWORD PTR [edi+wrkDCTSIZE*7]
  mov   edx, eax                            { edx = tmp0 }
    {tmp0 := (tmp0) * INT32(FIX_0_298631336); { sqrt(2) * (-c1+c3+c5-c7) }
  imul  eax, FIX_0_298631336
  mov	tmp0, eax

    {tmp3 := ISLOW_MULT_TYPE(inptr^[DCTSIZE*1]) * quantptr^[DCTSIZE*1];}
  mov	eax, DWORD PTR [esi+coefDCTSIZE*1]
  imul	eax, DWORD PTR [edi+wrkDCTSIZE*1]
  mov	tmp3, eax

    {z1 := tmp0 + tmp3;}
    {z1 := (z1) * INT32(- FIX_0_899976223); { sqrt(2) * (c7-c3) }
  add	eax, edx
  imul eax, (-FIX_0_899976223)
  mov  z1, eax

    {tmp1 := ISLOW_MULT_TYPE(inptr^[DCTSIZE*5]) * quantptr^[DCTSIZE*5];}
  mov	eax, DWORD PTR [esi+coefDCTSIZE*5]
  imul	eax, DWORD PTR [edi+wrkDCTSIZE*5]
  mov ebx, eax                            { ebx = tmp1 }
    {tmp1 := (tmp1) * INT32(FIX_2_053119869); { sqrt(2) * ( c1+c3-c5+c7) }
  imul  eax, FIX_2_053119869
  mov	tmp1, eax

    {tmp2 := ISLOW_MULT_TYPE(inptr^[DCTSIZE*3]) * quantptr^[DCTSIZE*3];}
  mov	eax, DWORD PTR [esi+coefDCTSIZE*3]
  imul	eax, DWORD PTR [edi+wrkDCTSIZE*3]
  mov	tmp2, eax

    {z3 := tmp0 + tmp2;}
  add	edx, eax                              { edx = z3 }

    {z2 := tmp1 + tmp2;}
    {z2 := (z2) * INT32(- FIX_2_562915447); { sqrt(2) * (-c1-c3) }
  add	eax, ebx
  imul  eax, (-FIX_2_562915447)
  mov	z2, eax

    {z4 := tmp1 + tmp3;}
  add	ebx, tmp3                             { ebx = z4 }

    {z5 := INT32(z3 + z4) * INT32(FIX_1_175875602); { sqrt(2) * c3 }
  lea   eax, [edx+ebx]
  imul eax, FIX_1_175875602                   { eax = z5 }

    {z4 := (z4) * INT32(- FIX_0_390180644); { sqrt(2) * (c5-c3) }
    {Inc(z4, z5);}
  imul   ebx, (-FIX_0_390180644)
  add    ebx, eax
  mov    z4, ebx

    {z3 := (z3) * INT32(- FIX_1_961570560); { sqrt(2) * (-c3-c5) }
    {Inc(z3, z5);}
  imul edx, (-FIX_1_961570560)
  add  eax, edx                        { z3 = eax }

    {Inc(tmp0, z1 + z3);}
  mov   ebx, z1
  add	ebx, eax
  add	tmp0, ebx

    {tmp2 := (tmp2) * INT32(FIX_3_072711026); { sqrt(2) * ( c1+c3+c5-c7) }
    {Inc(tmp2, z2 + z3);}
  mov   ebx, tmp2
  imul  ebx, FIX_3_072711026
  mov	edx, z2                        { z2 = edx }
  add   ebx, edx
  add   eax, ebx
  mov	tmp2, eax

    {Inc(tmp1, z2 + z4);}
  mov   eax, z4                        { z4 = eax }
  add   edx, eax
  add   tmp1, edx

    {tmp3 := (tmp3) * INT32(FIX_1_501321110); { sqrt(2) * ( c1+c3-c5-c7) }
    {Inc(tmp3, z1 + z4);}
  mov	edx, tmp3
  imul  edx, FIX_1_501321110

  add	edx, eax
  add   edx, z1                        { tmp3 = edx }

    { Final output stage: inputs are tmp10..tmp13, tmp0..tmp3 }

    {wsptr^[DCTSIZE*0] := int (DESCALE(tmp10 + tmp3, CONST_BITS-PASS1_BITS));}
    {wsptr^[DCTSIZE*7] := int (DESCALE(tmp10 - tmp3, CONST_BITS-PASS1_BITS));}    
  mov	eax, tmp10
  add   eax, ROUND_CONST
  lea   ebx, [eax+edx]
  sar	ebx, CONST_BITS-PASS1_BITS
  mov	DWORD PTR [ecx+wrkDCTSIZE*0], ebx

  sub	eax, edx
  sar	eax, CONST_BITS-PASS1_BITS
  mov	DWORD PTR [ecx+wrkDCTSIZE*7], eax

    {wsptr^[DCTSIZE*1] := int (DESCALE(tmp11 + tmp2, CONST_BITS-PASS1_BITS));}
    {wsptr^[DCTSIZE*6] := int (DESCALE(tmp11 - tmp2, CONST_BITS-PASS1_BITS));}
  mov	eax, tmp11
  add   eax, ROUND_CONST
  mov   edx, tmp2
  lea	ebx, [eax+edx]
  sar	ebx, CONST_BITS-PASS1_BITS
  mov	DWORD PTR [ecx+wrkDCTSIZE*1], ebx

  sub	eax, edx
  sar	eax, CONST_BITS-PASS1_BITS
  mov	DWORD PTR [ecx+wrkDCTSIZE*6], eax

    {wsptr^[DCTSIZE*2] := int (DESCALE(tmp12 + tmp1, CONST_BITS-PASS1_BITS));}
    {wsptr^[DCTSIZE*5] := int (DESCALE(tmp12 - tmp1, CONST_BITS-PASS1_BITS));}
  mov	eax, tmp12
  add   eax, ROUND_CONST
  mov   edx, tmp1
  lea	ebx, [eax+edx]
  sar	ebx, CONST_BITS-PASS1_BITS
  mov	DWORD PTR [ecx+wrkDCTSIZE*2], ebx

  sub	eax, edx
  sar	eax, CONST_BITS-PASS1_BITS
  mov	DWORD PTR [ecx+wrkDCTSIZE*5], eax

    {wsptr^[DCTSIZE*3] := int (DESCALE(tmp13 + tmp0, CONST_BITS-PASS1_BITS));}
    {wsptr^[DCTSIZE*4] := int (DESCALE(tmp13 - tmp0, CONST_BITS-PASS1_BITS));}    
  mov	eax, tmp13
  add   eax, ROUND_CONST
  mov   edx, tmp0
  lea   ebx, [eax+edx]
  sar	ebx, CONST_BITS-PASS1_BITS
  mov	DWORD PTR [ecx+wrkDCTSIZE*3], ebx

  sub	eax, edx
  sar	eax, CONST_BITS-PASS1_BITS
  mov	DWORD PTR [ecx+wrkDCTSIZE*4], eax

    {Inc(JCOEF_PTR(inptr));		{ advance pointers to next column }
    {Inc(ISLOW_MULT_TYPE_PTR(quantptr));
    Inc(int_ptr(wsptr));}
  dec	ctr
  je	@loop519

  add   esi, Type JCOEF
  add	edi, Type ISLOW_MULT_TYPE
  add	ecx, Type int  { int_ptr }
  {end;}
	jmp	@loop518
@loop519:
  { Save to memory what we've registerized for the preceding loop. }

  { Pass 2: process rows from work array, store into output array. }
  { Note that we must descale the results by a factor of 8 == 2**3, }
  { and also undo the PASS1_BITS scaling. }

  {wsptr := @workspace;}
  lea	esi, workspace

  {for ctr := 0 to pred(DCTSIZE) do
  begin}
  mov	ctr, 0
@loop523:

    {outptr := output_buf^[ctr];}
  mov	eax, ctr
  mov	ebx, output_buf
  mov	edi, DWORD PTR [ebx+eax*4]           { 4 = SizeOf(pointer) }

    {Inc(JSAMPLE_PTR(outptr), output_col);}
  add	edi, uInt(output_col)

    { Rows of zeroes can be exploited in the same way as we did with columns.
      However, the column calculation has created many nonzero AC terms, so
      the simplification applies less often (typically 5% to 10% of the time).
      On machines with very fast multiplication, it's possible that the
      test takes more time than it's worth.  In that case this section
      may be commented out. }

{$ifndef NO_ZERO_ROW_TEST}
    {if ((wsptr^[1]) or (wsptr^[2]) or (wsptr^[3]) or (wsptr^[4]) or
        (wsptr^[5]) or (wsptr^[6]) or (wsptr^[7]) = 0) then
    begin}
	mov	eax, DWORD PTR [esi+4*1]
	or	eax, DWORD PTR [esi+4*2]
	or	eax, DWORD PTR [esi+4*3]
        jne     @loop525            { Nomssi: early exit path may help }
	or	eax, DWORD PTR [esi+4*4]
	or	eax, DWORD PTR [esi+4*5]
	or	eax, DWORD PTR [esi+4*6]
	or	eax, DWORD PTR [esi+4*7]
	jne	@loop525

      { AC terms all zero }
      {JSAMPLE(dcval_) := range_limit^[int(DESCALE(INT32(wsptr^[0]),
                          PASS1_BITS+3)) and RANGE_MASK];}
	mov	eax, DWORD PTR [esi+4*0]
	add	eax, (INT32(1) shl (PASS1_BITS+3-1))
	sar	eax, PASS1_BITS+3
	and	eax, RANGE_MASK
        mov     ebx, range_limit
	mov	al, BYTE PTR [ebx+eax]
        mov     ah, al

      {outptr^[0] := dcval_;
      outptr^[1] := dcval_;
      outptr^[2] := dcval_;
      outptr^[3] := dcval_;
      outptr^[4] := dcval_;
      outptr^[5] := dcval_;
      outptr^[6] := dcval_;
      outptr^[7] := dcval_;}

	stosw
	stosw
	stosw
	stosw

      {Inc(int_ptr(wsptr), DCTSIZE);	{ advance pointer to next row }
      {continue;}
	add esi, wrkDCTSIZE
	inc	ctr
	cmp	ctr, DCTSIZE
	jl	@loop523
	jmp @loop524
    {end;}
@loop525:
{$endif}


    { Even part: reverse the even part of the forward DCT. }
    { The rotator is sqrt(2)*c(-6). }

    {z2 := INT32 (wsptr^[2]);}
  mov	edx, DWORD PTR [esi+4*2]                   { z2 = edx }

    {z3 := INT32 (wsptr^[6]);}
  mov	ecx, DWORD PTR [esi+4*6]                   { z3 = ecx }

    {z1 := (z2 + z3) * INT32(FIX_0_541196100);}
  lea   eax, [edx+ecx]
  imul  eax, FIX_0_541196100
  mov	ebx, eax                                   { z1 = ebx }

    {tmp2 := z1 + (z3) * INT32(- FIX_1_847759065);}
  imul  ecx, (-FIX_1_847759065)
  add	ecx, ebx                                   { tmp2 = ecx }

    {tmp3 := z1 + (z2) * INT32(FIX_0_765366865);}
  imul  edx, FIX_0_765366865
  add	ebx, edx                                   { tmp3 = ebx }

    {tmp0 := (INT32(wsptr^[0]) + INT32(wsptr^[4])) shl CONST_BITS;}
    {tmp1 := (INT32(wsptr^[0]) - INT32(wsptr^[4])) shl CONST_BITS;}
  mov	edx, DWORD PTR [esi+4*4]
  mov   eax, DWORD PTR [esi+4*0]
  sub   eax, edx
  add   edx, edx
  add   edx, eax
  shl	edx, CONST_BITS              { tmp0 = edx }
  shl	eax, CONST_BITS              { tmp1 = eax }

    {tmp10 := tmp0 + tmp3;}
    {tmp13 := tmp0 - tmp3;}
  sub   edx, ebx
  mov	tmp13, edx
  add   ebx, ebx
  add   edx, ebx
  mov	tmp10, edx

    {tmp11 := tmp1 + tmp2;}
    {tmp12 := tmp1 - tmp2;}
  lea   ebx, [ecx+eax]
  mov	tmp11, ebx
  sub	eax, ecx
  mov	tmp12, eax

    { Odd part per figure 8; the matrix is unitary and hence its
      transpose is its inverse.  i0..i3 are y7,y5,y3,y1 respectively. }

{ The following lines no longer produce code, since wsptr has been
  optimized to esi, it is more efficient to access these values
  directly.
    tmp0 := INT32(wsptr^[7]);
    tmp1 := INT32(wsptr^[5]);
    tmp2 := INT32(wsptr^[3]);
    tmp3 := INT32(wsptr^[1]); }

    {z2 := tmp1 + tmp2;}
    {z2 := (z2) * INT32(- FIX_2_562915447); { sqrt(2) * (-c1-c3) }
  mov	ebx, DWORD PTR [esi+4*3]              { tmp2 }
  mov   ecx, DWORD PTR [esi+4*5]              { tmp1 }
  lea   eax, [ebx+ecx]
  imul  eax, (-FIX_2_562915447)
  mov	z2, eax

    {z3 := tmp0 + tmp2;}
  mov	edx, DWORD PTR [esi+4*7]              { tmp0 }
  add   ebx, edx                              { old z3 = ebx }
  mov	eax, ebx
    {z3 := (z3) * INT32(- FIX_1_961570560); { sqrt(2) * (-c3-c5) }
  imul eax, (-FIX_1_961570560)
  mov	z3, eax

    {z1 := tmp0 + tmp3;}
    {z1 := (z1) * INT32(- FIX_0_899976223); { sqrt(2) * (c7-c3) }
  mov	eax, DWORD PTR [esi+4*1]               { tmp3 }
  add	edx, eax
  imul  edx, (-FIX_0_899976223)                { z1 = edx }

    {z4 := tmp1 + tmp3;}
  add	eax, ecx                              { +tmp1 }
  add	ebx, eax                              { z3 + z4 = ebx }
    {z4 := (z4) * INT32(- FIX_0_390180644); { sqrt(2) * (c5-c3) }
  imul eax, (-FIX_0_390180644)                { z4 = eax }

    {z5 := (z3 + z4) * INT32(FIX_1_175875602); { sqrt(2) * c3 }
    {Inc(z3, z5);}
  imul ebx, FIX_1_175875602
  mov  ecx, z3
  add  ecx, ebx                                { ecx = z3 }

    {Inc(z4, z5);}
  add ebx, eax                                 { z4 = ebx }

    {tmp0 := (tmp0) * INT32(FIX_0_298631336); { sqrt(2) * (-c1+c3+c5-c7) }
    {Inc(tmp0, z1 + z3);}
  mov   eax, DWORD PTR [esi+4*7]
  imul  eax, FIX_0_298631336
  add   eax, edx
  add   eax, ecx
  mov	tmp0, eax

    {tmp1 := (tmp1) * INT32(FIX_2_053119869); { sqrt(2) * ( c1+c3-c5+c7) }
    {Inc(tmp1, z2 + z4);}
  mov  eax, DWORD PTR [esi+4*5]
  imul eax, FIX_2_053119869
  add  eax, z2
  add  eax, ebx
  mov  tmp1, eax

    {tmp2 := (tmp2) * INT32(FIX_3_072711026); { sqrt(2) * ( c1+c3+c5-c7) }
    {Inc(tmp2, z2 + z3);}
  mov	eax, DWORD PTR [esi+4*3]
  imul  eax, FIX_3_072711026
  add   eax, z2
  add   ecx, eax                      { ecx = tmp2 }

    {tmp3 := (tmp3) * INT32(FIX_1_501321110); { sqrt(2) * ( c1+c3-c5-c7) }
    {Inc(tmp3, z1 + z4);}
  mov	eax, DWORD PTR [esi+4*1]
  imul  eax, FIX_1_501321110
  add   eax, edx
  add   ebx, eax                   { ebx = tmp3 }

    { Final output stage: inputs are tmp10..tmp13, tmp0..tmp3 }

    {outptr^[0] := range_limit^[ int(DESCALE(tmp10 + tmp3,
                      CONST_BITS+PASS1_BITS+3)) and RANGE_MASK]; }
    {outptr^[7] := range_limit^[ int(DESCALE(tmp10 - tmp3,
                        CONST_BITS+PASS1_BITS+3)) and RANGE_MASK];}

  mov	edx, tmp10
  add   edx, ROUND_CONST_2
  lea	eax, [ebx+edx]
  sub   edx, ebx

  shr	eax, CONST_BITS+PASS1_BITS+3
  and	eax, RANGE_MASK
  mov   ebx, range_limit           { once for all }
  mov	al, BYTE PTR [ebx+eax]
  mov   [edi+0], al

  shr	edx, CONST_BITS+PASS1_BITS+3
  and	edx, RANGE_MASK
  mov	al, BYTE PTR [ebx+edx]
  mov   [edi+7], al

    {outptr^[1] := range_limit^[ int(DESCALE(tmp11 + tmp2,
                        CONST_BITS+PASS1_BITS+3)) and RANGE_MASK];}
  mov	eax, tmp11
  add   eax, ROUND_CONST_2
  lea	edx, [eax+ecx]
  shr	edx, CONST_BITS+PASS1_BITS+3
  and	edx, RANGE_MASK
  mov	dl, BYTE PTR [ebx+edx]
  mov   [edi+1], dl

    {outptr^[6] := range_limit^[ int(DESCALE(tmp11 - tmp2,
			CONST_BITS+PASS1_BITS+3)) and RANGE_MASK];}
  sub	eax, ecx
  shr	eax, CONST_BITS+PASS1_BITS+3
  and	eax, RANGE_MASK
  mov	al, BYTE PTR [ebx+eax]
  mov   [edi+6], al

    {outptr^[2] := range_limit^[ int(DESCALE(tmp12 + tmp1,
			CONST_BITS+PASS1_BITS+3)) and RANGE_MASK];}
  mov	eax, tmp12
  add   eax, ROUND_CONST_2
  mov   ecx, tmp1
  lea	edx, [eax+ecx]
  shr	edx, CONST_BITS+PASS1_BITS+3
  and	edx, RANGE_MASK
  mov	dl, BYTE PTR [ebx+edx]
  mov   [edi+2], dl

    {outptr^[5] := range_limit^[ int(DESCALE(tmp12 - tmp1,
			CONST_BITS+PASS1_BITS+3)) and RANGE_MASK];}
  sub	eax, ecx
  shr	eax, CONST_BITS+PASS1_BITS+3
  and	eax, RANGE_MASK
  mov	al, BYTE PTR [ebx+eax]
  mov   [edi+5], al

    {outptr^[3] := range_limit^[ int(DESCALE(tmp13 + tmp0,
			CONST_BITS+PASS1_BITS+3)) and RANGE_MASK];}
  mov	eax, tmp13
  add   eax, ROUND_CONST_2
  mov   ecx, tmp0
  lea   edx, [eax+ecx]
  shr	edx, CONST_BITS+PASS1_BITS+3
  and	edx, RANGE_MASK
  mov	dl, BYTE PTR [ebx+edx]
  mov   [edi+3], dl

    {outptr^[4] := range_limit^[ int(DESCALE(tmp13 - tmp0,
			CONST_BITS+PASS1_BITS+3)) and RANGE_MASK];}
  sub	eax, ecx
  shr	eax, CONST_BITS+PASS1_BITS+3
  and	eax, RANGE_MASK
  mov	al, BYTE PTR [ebx+eax]
  mov   [edi+4], al

    {Inc(int_ptr(wsptr), DCTSIZE);	{ advance pointer to next row }
  add	esi, wrkDCTSIZE
  add	edi, DCTSIZE

  {end;}
  inc	ctr 
  cmp	ctr, DCTSIZE
  jl	@loop523

@loop524:
@loop496:
  pop   ebx
  pop   esi
  pop   edi
end;

end.
