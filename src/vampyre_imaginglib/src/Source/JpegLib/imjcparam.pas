unit imjcparam;

{ This file contains optional default-setting code for the JPEG compressor.
  Applications do not have to use this file, but those that don't use it
  must know a lot more about the innards of the JPEG code. }

{ Original: jcparam.c ; Copyright (C) 1991-1998, Thomas G. Lane. }

interface

{$I imjconfig.inc}

uses
  imjmorecfg,
  imjinclude,
  imjdeferr,
  imjerror,
  imjcomapi,
  imjpeglib;

{ Quantization table setup routines }

{GLOBAL}
procedure jpeg_add_quant_table (cinfo : j_compress_ptr;
                                which_tbl : int;
		                const basic_table : array of uInt;
		                scale_factor : int;
                                force_baseline : boolean);

{GLOBAL}
procedure jpeg_set_linear_quality (cinfo : j_compress_ptr;
                                   scale_factor : int;
			           force_baseline : boolean);
{ Set or change the 'quality' (quantization) setting, using default tables
  and a straight percentage-scaling quality scale.  In most cases it's better
  to use jpeg_set_quality (below); this entry point is provided for
  applications that insist on a linear percentage scaling. }

{GLOBAL}
function jpeg_quality_scaling (quality : int) : int;
{ Convert a user-specified quality rating to a percentage scaling factor
  for an underlying quantization table, using our recommended scaling curve.
  The input 'quality' factor should be 0 (terrible) to 100 (very good). }

{GLOBAL}
procedure jpeg_set_quality (cinfo : j_compress_ptr;
                            quality : int;
                            force_baseline : boolean);
{ Set or change the 'quality' (quantization) setting, using default tables.
  This is the standard quality-adjusting entry point for typical user
  interfaces; only those who want detailed control over quantization tables
  would use the preceding three routines directly. }

{GLOBAL}
procedure jpeg_set_defaults (cinfo : j_compress_ptr);

{ Create a recommended progressive-JPEG script.
  cinfo^.num_components and cinfo^.jpeg_color_space must be correct. }

{ Set the JPEG colorspace, and choose colorspace-dependent default values. }

{GLOBAL}
procedure jpeg_set_colorspace (cinfo : j_compress_ptr;
                               colorspace : J_COLOR_SPACE);

{ Select an appropriate JPEG colorspace for in_color_space. }

{GLOBAL}
procedure jpeg_default_colorspace (cinfo : j_compress_ptr);

{GLOBAL}
procedure jpeg_simple_progression (cinfo : j_compress_ptr);


implementation

{ Quantization table setup routines }

{GLOBAL}
procedure jpeg_add_quant_table (cinfo : j_compress_ptr;
                      which_tbl : int;
		      const basic_table : array of uInt;
		      scale_factor : int;
                      force_baseline : boolean);
{ Define a quantization table equal to the basic_table times
  a scale factor (given as a percentage).
  If force_baseline is TRUE, the computed quantization table entries
  are limited to 1..255 for JPEG baseline compatibility. }
var
  qtblptr :^JQUANT_TBL_PTR;
  i : int;
  temp : long;
begin
  { Safety check to ensure start_compress not called yet. }
  if (cinfo^.global_state <> CSTATE_START) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);

  if (which_tbl < 0) or (which_tbl >= NUM_QUANT_TBLS) then
    ERREXIT1(j_common_ptr(cinfo), JERR_DQT_INDEX, which_tbl);

  qtblptr := @(cinfo^.quant_tbl_ptrs[which_tbl]);

  if (qtblptr^ = NIL) then
    qtblptr^ := jpeg_alloc_quant_table(j_common_ptr(cinfo));

  for i := 0 to pred(DCTSIZE2) do
  begin
    temp := (long(basic_table[i]) * scale_factor + long(50)) div long(100);
    { limit the values to the valid range }
    if (temp <= long(0)) then
      temp := long(1);
    if (temp > long(32767)) then
      temp := long(32767); { max quantizer needed for 12 bits }
    if (force_baseline) and (temp > long(255)) then
      temp := long(255);		{ limit to baseline range if requested }
    (qtblptr^)^.quantval[i] := UINT16 (temp);
  end;

  { Initialize sent_table FALSE so table will be written to JPEG file. }
  (qtblptr^)^.sent_table := FALSE;
end;


{GLOBAL}
procedure jpeg_set_linear_quality (cinfo : j_compress_ptr;
                                   scale_factor : int;
			           force_baseline : boolean);
{ Set or change the 'quality' (quantization) setting, using default tables
  and a straight percentage-scaling quality scale.  In most cases it's better
  to use jpeg_set_quality (below); this entry point is provided for
  applications that insist on a linear percentage scaling. }

{ These are the sample quantization tables given in JPEG spec section K.1.
  The spec says that the values given produce "good" quality, and
  when divided by 2, "very good" quality. }

const
  std_luminance_quant_tbl : array[0..DCTSIZE2-1] of uInt =
   (16,  11,  10,  16,  24,  40,  51,  61,
    12,  12,  14,  19,  26,  58,  60,  55,
    14,  13,  16,  24,  40,  57,  69,  56,
    14,  17,  22,  29,  51,  87,  80,  62,
    18,  22,  37,  56,  68, 109, 103,  77,
    24,  35,  55,  64,  81, 104, 113,  92,
    49,  64,  78,  87, 103, 121, 120, 101,
    72,  92,  95,  98, 112, 100, 103,  99);

const
  std_chrominance_quant_tbl : array[0..DCTSIZE2-1] of uInt =
   (17,  18,  24,  47,  99,  99,  99,  99,
    18,  21,  26,  66,  99,  99,  99,  99,
    24,  26,  56,  99,  99,  99,  99,  99,
    47,  66,  99,  99,  99,  99,  99,  99,
    99,  99,  99,  99,  99,  99,  99,  99,
    99,  99,  99,  99,  99,  99,  99,  99,
    99,  99,  99,  99,  99,  99,  99,  99,
    99,  99,  99,  99,  99,  99,  99,  99);
begin
  { Set up two quantization tables using the specified scaling }
  jpeg_add_quant_table(cinfo, 0, std_luminance_quant_tbl,
		       scale_factor, force_baseline);
  jpeg_add_quant_table(cinfo, 1, std_chrominance_quant_tbl,
		       scale_factor, force_baseline);
end;


{GLOBAL}
function jpeg_quality_scaling (quality : int) : int;
{ Convert a user-specified quality rating to a percentage scaling factor
  for an underlying quantization table, using our recommended scaling curve.
  The input 'quality' factor should be 0 (terrible) to 100 (very good). }
begin
  { Safety limit on quality factor.  Convert 0 to 1 to avoid zero divide. }
  if (quality <= 0) then
    quality := 1;
  if (quality > 100) then
    quality := 100;

  { The basic table is used as-is (scaling 100) for a quality of 50.
    Qualities 50..100 are converted to scaling percentage 200 - 2*Q;
    note that at Q=100 the scaling is 0, which will cause jpeg_add_quant_table
    to make all the table entries 1 (hence, minimum quantization loss).
    Qualities 1..50 are converted to scaling percentage 5000/Q. }
  if (quality < 50) then
    quality := 5000 div quality
  else
    quality := 200 - quality*2;

  jpeg_quality_scaling := quality;
end;


{GLOBAL}
procedure jpeg_set_quality (cinfo : j_compress_ptr;
                            quality : int;
                            force_baseline : boolean);
{ Set or change the 'quality' (quantization) setting, using default tables.
  This is the standard quality-adjusting entry point for typical user
  interfaces; only those who want detailed control over quantization tables
  would use the preceding three routines directly. }
begin
  { Convert user 0-100 rating to percentage scaling }
  quality := jpeg_quality_scaling(quality);

  { Set up standard quality tables }
  jpeg_set_linear_quality(cinfo, quality, force_baseline);
end;


{ Huffman table setup routines }

{LOCAL}
procedure add_huff_table (cinfo : j_compress_ptr;
                          var htblptr : JHUFF_TBL_PTR;
                          var bits : array of UINT8;
                          var val : array of UINT8);
{ Define a Huffman table }
var
  nsymbols, len : int;
begin
  if (htblptr = NIL) then
    htblptr := jpeg_alloc_huff_table(j_common_ptr(cinfo));

  { Copy the number-of-symbols-of-each-code-length counts }
  MEMCOPY(@htblptr^.bits, @bits, SIZEOF(htblptr^.bits));


  { Validate the counts.  We do this here mainly so we can copy the right
    number of symbols from the val[] array, without risking marching off
    the end of memory.  jchuff.c will do a more thorough test later. }

  nsymbols := 0;
  for len := 1 to 16 do
    Inc(nsymbols, bits[len]);
  if (nsymbols < 1) or (nsymbols > 256) then
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_HUFF_TABLE);

  MEMCOPY(@htblptr^.huffval, @val, nsymbols * SIZEOF(UINT8));

  { Initialize sent_table FALSE so table will be written to JPEG file. }
  (htblptr)^.sent_table := FALSE;
end;


{$J+}
{LOCAL}
procedure std_huff_tables (cinfo : j_compress_ptr);
{ Set up the standard Huffman tables (cf. JPEG standard section K.3) }
{ IMPORTANT: these are only valid for 8-bit data precision! }
  const bits_dc_luminance : array[0..17-1] of UINT8 =
    ({ 0-base } 0, 0, 1, 5, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0);
  const val_dc_luminance : array[0..11] of UINT8 =
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11);

  const bits_dc_chrominance : array[0..17-1] of UINT8 =
    ( { 0-base } 0, 0, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0 );
  const val_dc_chrominance : array[0..11] of UINT8 =
    ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 );

  const bits_ac_luminance : array[0..17-1] of UINT8 =
    ( { 0-base } 0, 0, 2, 1, 3, 3, 2, 4, 3, 5, 5, 4, 4, 0, 0, 1, $7d );
  const val_ac_luminance : array[0..161] of UINT8 =
    ( $01, $02, $03, $00, $04, $11, $05, $12,
      $21, $31, $41, $06, $13, $51, $61, $07,
      $22, $71, $14, $32, $81, $91, $a1, $08,
      $23, $42, $b1, $c1, $15, $52, $d1, $f0,
      $24, $33, $62, $72, $82, $09, $0a, $16,
      $17, $18, $19, $1a, $25, $26, $27, $28,
      $29, $2a, $34, $35, $36, $37, $38, $39,
      $3a, $43, $44, $45, $46, $47, $48, $49,
      $4a, $53, $54, $55, $56, $57, $58, $59,
      $5a, $63, $64, $65, $66, $67, $68, $69,
      $6a, $73, $74, $75, $76, $77, $78, $79,
      $7a, $83, $84, $85, $86, $87, $88, $89,
      $8a, $92, $93, $94, $95, $96, $97, $98,
      $99, $9a, $a2, $a3, $a4, $a5, $a6, $a7,
      $a8, $a9, $aa, $b2, $b3, $b4, $b5, $b6,
      $b7, $b8, $b9, $ba, $c2, $c3, $c4, $c5,
      $c6, $c7, $c8, $c9, $ca, $d2, $d3, $d4,
      $d5, $d6, $d7, $d8, $d9, $da, $e1, $e2,
      $e3, $e4, $e5, $e6, $e7, $e8, $e9, $ea,
      $f1, $f2, $f3, $f4, $f5, $f6, $f7, $f8,
      $f9, $fa );

  const bits_ac_chrominance : array[0..17-1] of UINT8 =
    ( { 0-base } 0, 0, 2, 1, 2, 4, 4, 3, 4, 7, 5, 4, 4, 0, 1, 2, $77 );
  const val_ac_chrominance : array[0..161] of UINT8 =
    ( $00, $01, $02, $03, $11, $04, $05, $21,
      $31, $06, $12, $41, $51, $07, $61, $71,
      $13, $22, $32, $81, $08, $14, $42, $91,
      $a1, $b1, $c1, $09, $23, $33, $52, $f0,
      $15, $62, $72, $d1, $0a, $16, $24, $34,
      $e1, $25, $f1, $17, $18, $19, $1a, $26,
      $27, $28, $29, $2a, $35, $36, $37, $38,
      $39, $3a, $43, $44, $45, $46, $47, $48,
      $49, $4a, $53, $54, $55, $56, $57, $58,
      $59, $5a, $63, $64, $65, $66, $67, $68,
      $69, $6a, $73, $74, $75, $76, $77, $78,
      $79, $7a, $82, $83, $84, $85, $86, $87,
      $88, $89, $8a, $92, $93, $94, $95, $96,
      $97, $98, $99, $9a, $a2, $a3, $a4, $a5,
      $a6, $a7, $a8, $a9, $aa, $b2, $b3, $b4,
      $b5, $b6, $b7, $b8, $b9, $ba, $c2, $c3,
      $c4, $c5, $c6, $c7, $c8, $c9, $ca, $d2,
      $d3, $d4, $d5, $d6, $d7, $d8, $d9, $da,
      $e2, $e3, $e4, $e5, $e6, $e7, $e8, $e9,
      $ea, $f2, $f3, $f4, $f5, $f6, $f7, $f8,
      $f9, $fa );
begin
  add_huff_table(cinfo, cinfo^.dc_huff_tbl_ptrs[0],
  bits_dc_luminance, val_dc_luminance);
  add_huff_table(cinfo, cinfo^.ac_huff_tbl_ptrs[0],
  bits_ac_luminance, val_ac_luminance);
  add_huff_table(cinfo, cinfo^.dc_huff_tbl_ptrs[1],
  bits_dc_chrominance, val_dc_chrominance);
  add_huff_table(cinfo, cinfo^.ac_huff_tbl_ptrs[1],
  bits_ac_chrominance, val_ac_chrominance);
end;


{ Default parameter setup for compression.

  Applications that don't choose to use this routine must do their
  own setup of all these parameters.  Alternately, you can call this
  to establish defaults and then alter parameters selectively.  This
  is the recommended approach since, if we add any new parameters,
  your code will still work (they'll be set to reasonable defaults). }

{GLOBAL}
procedure jpeg_set_defaults (cinfo : j_compress_ptr);
var
  i : int;
begin
  { Safety check to ensure start_compress not called yet. }
  if (cinfo^.global_state <> CSTATE_START) then
    ERREXIT1(J_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);

  { Allocate comp_info array large enough for maximum component count.
    Array is made permanent in case application wants to compress
    multiple images at same param settings. }

  if (cinfo^.comp_info = NIL) then
    cinfo^.comp_info := jpeg_component_info_list_ptr(
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_PERMANENT,
             MAX_COMPONENTS * SIZEOF(jpeg_component_info)) );

  { Initialize everything not dependent on the color space }

  cinfo^.data_precision := BITS_IN_JSAMPLE;
  { Set up two quantization tables using default quality of 75 }
  jpeg_set_quality(cinfo, 75, TRUE);
  { Set up two Huffman tables }
  std_huff_tables(cinfo);

  { Initialize default arithmetic coding conditioning }
  for i := 0 to pred(NUM_ARITH_TBLS) do
  begin
    cinfo^.arith_dc_L[i] := 0;
    cinfo^.arith_dc_U[i] := 1;
    cinfo^.arith_ac_K[i] := 5;
  end;

  { Default is no multiple-scan output }
  cinfo^.scan_info := NIL;
  cinfo^.num_scans := 0;

  { Expect normal source image, not raw downsampled data }
  cinfo^.raw_data_in := FALSE;

  { Use Huffman coding, not arithmetic coding, by default }
  cinfo^.arith_code := FALSE;

  { By default, don't do extra passes to optimize entropy coding }
  cinfo^.optimize_coding := FALSE;
  { The standard Huffman tables are only valid for 8-bit data precision.
    If the precision is higher, force optimization on so that usable
    tables will be computed.  This test can be removed if default tables
    are supplied that are valid for the desired precision. }

  if (cinfo^.data_precision > 8) then
    cinfo^.optimize_coding := TRUE;

  { By default, use the simpler non-cosited sampling alignment }
  cinfo^.CCIR601_sampling := FALSE;

  { No input smoothing }
  cinfo^.smoothing_factor := 0;

  { DCT algorithm preference }
  cinfo^.dct_method := JDCT_DEFAULT;

  { No restart markers }
  cinfo^.restart_interval := 0;
  cinfo^.restart_in_rows := 0;

  { Fill in default JFIF marker parameters.  Note that whether the marker
    will actually be written is determined by jpeg_set_colorspace.

    By default, the library emits JFIF version code 1.01.
    An application that wants to emit JFIF 1.02 extension markers should set
    JFIF_minor_version to 2.  We could probably get away with just defaulting
    to 1.02, but there may still be some decoders in use that will complain
    about that; saying 1.01 should minimize compatibility problems. }

  cinfo^.JFIF_major_version := 1; { Default JFIF version = 1.01 }
  cinfo^.JFIF_minor_version := 1;
  cinfo^.density_unit := 0;	{ Pixel size is unknown by default }
  cinfo^.X_density := 1;		{ Pixel aspect ratio is square by default }
  cinfo^.Y_density := 1;

  { Choose JPEG colorspace based on input space, set defaults accordingly }

  jpeg_default_colorspace(cinfo);
end;


{ Select an appropriate JPEG colorspace for in_color_space. }

{GLOBAL}
procedure jpeg_default_colorspace (cinfo : j_compress_ptr);
begin
  case (cinfo^.in_color_space) of
  JCS_GRAYSCALE:
    jpeg_set_colorspace(cinfo, JCS_GRAYSCALE);
  JCS_RGB:
    jpeg_set_colorspace(cinfo, JCS_YCbCr);
  JCS_YCbCr:
    jpeg_set_colorspace(cinfo, JCS_YCbCr);
  JCS_CMYK:
    jpeg_set_colorspace(cinfo, JCS_CMYK); { By default, no translation }
  JCS_YCCK:
    jpeg_set_colorspace(cinfo, JCS_YCCK);
  JCS_UNKNOWN:
    jpeg_set_colorspace(cinfo, JCS_UNKNOWN);
  else
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_IN_COLORSPACE);
  end;
end;


{ Set the JPEG colorspace, and choose colorspace-dependent default values. }

{GLOBAL}
procedure jpeg_set_colorspace (cinfo : j_compress_ptr;
                               colorspace : J_COLOR_SPACE);
  { macro }
  procedure SET_COMP(index,id,hsamp,vsamp,quant,dctbl,actbl : int);
  begin
    with cinfo^.comp_info^[index] do
    begin
      component_id := (id);
      h_samp_factor := (hsamp);
      v_samp_factor := (vsamp);
      quant_tbl_no := (quant);
      dc_tbl_no := (dctbl);
      ac_tbl_no := (actbl);
    end;
  end;

var
  ci : int;
begin
  { Safety check to ensure start_compress not called yet. }
  if (cinfo^.global_state <> CSTATE_START) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);

  { For all colorspaces, we use Q and Huff tables 0 for luminance components,
    tables 1 for chrominance components. }

  cinfo^.jpeg_color_space := colorspace;

  cinfo^.write_JFIF_header := FALSE; { No marker for non-JFIF colorspaces }
  cinfo^.write_Adobe_marker := FALSE; { write no Adobe marker by default }

  case (colorspace) of
  JCS_GRAYSCALE:
    begin
      cinfo^.write_JFIF_header := TRUE; { Write a JFIF marker }
      cinfo^.num_components := 1;
      { JFIF specifies component ID 1 }
      SET_COMP(0, 1, 1,1, 0, 0,0);
    end;
  JCS_RGB:
    begin
      cinfo^.write_Adobe_marker := TRUE; { write Adobe marker to flag RGB }
      cinfo^.num_components := 3;
      SET_COMP(0, $52 { 'R' }, 1,1, 0, 0,0);
      SET_COMP(1, $47 { 'G' }, 1,1, 0, 0,0);
      SET_COMP(2, $42 { 'B' }, 1,1, 0, 0,0);
    end;
  JCS_YCbCr:
    begin
      cinfo^.write_JFIF_header := TRUE; { Write a JFIF marker }
      cinfo^.num_components := 3;
      { JFIF specifies component IDs 1,2,3 }
      { We default to 2x2 subsamples of chrominance }
      SET_COMP(0, 1, 2,2, 0, 0,0);
      SET_COMP(1, 2, 1,1, 1, 1,1);
      SET_COMP(2, 3, 1,1, 1, 1,1);
    end;
  JCS_CMYK:
    begin
      cinfo^.write_Adobe_marker := TRUE; { write Adobe marker to flag CMYK }
      cinfo^.num_components := 4;
      SET_COMP(0, $43 { 'C' }, 1,1, 0, 0,0);
      SET_COMP(1, $4D { 'M' }, 1,1, 0, 0,0);
      SET_COMP(2, $59 { 'Y' }, 1,1, 0, 0,0);
      SET_COMP(3, $4B { 'K' }, 1,1, 0, 0,0);
    end;
  JCS_YCCK:
    begin
      cinfo^.write_Adobe_marker := TRUE; { write Adobe marker to flag YCCK }
      cinfo^.num_components := 4;
      SET_COMP(0, 1, 2,2, 0, 0,0);
      SET_COMP(1, 2, 1,1, 1, 1,1);
      SET_COMP(2, 3, 1,1, 1, 1,1);
      SET_COMP(3, 4, 2,2, 0, 0,0);
    end;
  JCS_UNKNOWN:
    begin
      cinfo^.num_components := cinfo^.input_components;
      if (cinfo^.num_components < 1)
      or (cinfo^.num_components > MAX_COMPONENTS) then
        ERREXIT2(j_common_ptr(cinfo), JERR_COMPONENT_COUNT,
                 cinfo^.num_components, MAX_COMPONENTS);
      for ci := 0 to pred(cinfo^.num_components) do
      begin
        SET_COMP(ci, ci, 1,1, 0, 0,0);
      end;
    end;
  else
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_J_COLORSPACE);
  end;
end;


{$ifdef C_PROGRESSIVE_SUPPORTED}

{LOCAL}
function fill_a_scan (scanptr : jpeg_scan_info_ptr;
                      ci : int; Ss : int;
                      Se : int; Ah : int;
                      Al : int) : jpeg_scan_info_ptr;
{ Support routine: generate one scan for specified component }
begin
  scanptr^.comps_in_scan := 1;
  scanptr^.component_index[0] := ci;
  scanptr^.Ss := Ss;
  scanptr^.Se := Se;
  scanptr^.Ah := Ah;
  scanptr^.Al := Al;
  Inc(scanptr);
  fill_a_scan := scanptr;
end;

{LOCAL}
function fill_scans (scanptr : jpeg_scan_info_ptr;
                     ncomps : int;
	             Ss : int; Se : int;
                     Ah : int; Al : int) : jpeg_scan_info_ptr;
{ Support routine: generate one scan for each component }
var
  ci : int;
begin

  for ci := 0 to pred(ncomps) do
  begin
    scanptr^.comps_in_scan := 1;
    scanptr^.component_index[0] := ci;
    scanptr^.Ss := Ss;
    scanptr^.Se := Se;
    scanptr^.Ah := Ah;
    scanptr^.Al := Al;
    Inc(scanptr);
  end;
  fill_scans := scanptr;
end;

{LOCAL}
function fill_dc_scans (scanptr : jpeg_scan_info_ptr;
                        ncomps : int;
                        Ah : int; Al : int) : jpeg_scan_info_ptr;
{ Support routine: generate interleaved DC scan if possible, else N scans }
var
  ci : int;
begin

  if (ncomps <= MAX_COMPS_IN_SCAN) then
  begin
    { Single interleaved DC scan }
    scanptr^.comps_in_scan := ncomps;
    for ci := 0 to pred(ncomps) do
      scanptr^.component_index[ci] := ci;
    scanptr^.Ss := 0;
    scanptr^.Se := 0;
    scanptr^.Ah := Ah;
    scanptr^.Al := Al;
    Inc(scanptr);
  end
  else
  begin
    { Noninterleaved DC scan for each component }
    scanptr := fill_scans(scanptr, ncomps, 0, 0, Ah, Al);
  end;
  fill_dc_scans := scanptr;
end;


{ Create a recommended progressive-JPEG script.
  cinfo^.num_components and cinfo^.jpeg_color_space must be correct. }

{GLOBAL}
procedure jpeg_simple_progression (cinfo : j_compress_ptr);
var
  ncomps : int;
  nscans : int;
  scanptr : jpeg_scan_info_ptr;
begin
  ncomps := cinfo^.num_components;

  { Safety check to ensure start_compress not called yet. }
  if (cinfo^.global_state <> CSTATE_START) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);

  { Figure space needed for script.  Calculation must match code below! }
  if (ncomps = 3) and (cinfo^.jpeg_color_space = JCS_YCbCr) then
  begin
    { Custom script for YCbCr color images. }
    nscans := 10;
  end
  else
  begin
    { All-purpose script for other color spaces. }
    if (ncomps > MAX_COMPS_IN_SCAN) then
      nscans := 6 * ncomps	{ 2 DC + 4 AC scans per component }
    else
      nscans := 2 + 4 * ncomps;	{ 2 DC scans; 4 AC scans per component }
  end;

  { Allocate space for script.
    We need to put it in the permanent pool in case the application performs
    multiple compressions without changing the settings.  To avoid a memory
    leak if jpeg_simple_progression is called repeatedly for the same JPEG
    object, we try to re-use previously allocated space, and we allocate
    enough space to handle YCbCr even if initially asked for grayscale. }

  if (cinfo^.script_space = NIL) or (cinfo^.script_space_size < nscans) then
  begin
    if nscans > 10 then
      cinfo^.script_space_size := nscans
    else
      cinfo^.script_space_size := 10;

    cinfo^.script_space := jpeg_scan_info_ptr(
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_PERMANENT,
		cinfo^.script_space_size * SIZEOF(jpeg_scan_info)) );
  end;
  scanptr := cinfo^.script_space;

  cinfo^.scan_info := scanptr;
  cinfo^.num_scans := nscans;

  if (ncomps = 3) and (cinfo^.jpeg_color_space = JCS_YCbCr) then
  begin
    { Custom script for YCbCr color images. }
    { Initial DC scan }
    scanptr := fill_dc_scans(scanptr, ncomps, 0, 1);
    { Initial AC scan: get some luma data out in a hurry }
    scanptr := fill_a_scan(scanptr, 0, 1, 5, 0, 2);
    { Chroma data is too small to be worth expending many scans on }
    scanptr := fill_a_scan(scanptr, 2, 1, 63, 0, 1);
    scanptr := fill_a_scan(scanptr, 1, 1, 63, 0, 1);
    { Complete spectral selection for luma AC }
    scanptr := fill_a_scan(scanptr, 0, 6, 63, 0, 2);
    { Refine next bit of luma AC }
    scanptr := fill_a_scan(scanptr, 0, 1, 63, 2, 1);
    { Finish DC successive approximation }
    scanptr := fill_dc_scans(scanptr, ncomps, 1, 0);
    { Finish AC successive approximation }
    scanptr := fill_a_scan(scanptr, 2, 1, 63, 1, 0);
    scanptr := fill_a_scan(scanptr, 1, 1, 63, 1, 0);
    { Luma bottom bit comes last since it's usually largest scan }
    scanptr := fill_a_scan(scanptr, 0, 1, 63, 1, 0);
  end
  else
  begin
    { All-purpose script for other color spaces. }
    { Successive approximation first pass }
    scanptr := fill_dc_scans(scanptr, ncomps, 0, 1);
    scanptr := fill_scans(scanptr, ncomps, 1, 5, 0, 2);
    scanptr := fill_scans(scanptr, ncomps, 6, 63, 0, 2);
    { Successive approximation second pass }
    scanptr := fill_scans(scanptr, ncomps, 1, 63, 2, 1);
    { Successive approximation final pass }
    scanptr := fill_dc_scans(scanptr, ncomps, 1, 0);
    scanptr := fill_scans(scanptr, ncomps, 1, 63, 1, 0);
  end;
end;

{$endif}
end.
