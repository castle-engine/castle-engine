Unit JcpHuff;

{ This file contains Huffman entropy encoding routines for progressive JPEG.

  We do not support output suspension in this module, since the library
  currently does not allow multiple-scan files to be written with output
  suspension. }

{ Original: jcphuff.c;  Copyright (C) 1995-1997, Thomas G. Lane. }

interface

{$I jconfig.inc}
uses
  jmorecfg,
  jinclude,
  jpeglib,
  jdeferr,
  jerror,
  jutils,
  jcomapi,
  jchuff;               { Declarations shared with jchuff.c }


{ Module initialization routine for progressive Huffman entropy encoding. }

{GLOBAL}
procedure jinit_phuff_encoder (cinfo : j_compress_ptr);

implementation

{ Expanded entropy encoder object for progressive Huffman encoding. }
type
  phuff_entropy_ptr = ^phuff_entropy_encoder;
  phuff_entropy_encoder = record
    pub : jpeg_entropy_encoder; { public fields }

    { Mode flag: TRUE for optimization, FALSE for actual data output }
    gather_statistics : boolean;

    { Bit-level coding status.
      next_output_byte/free_in_buffer are local copies of cinfo^.dest fields.}

    next_output_byte : JOCTETptr; { => next byte to write in buffer }
    free_in_buffer : size_t;    { # of byte spaces remaining in buffer }
    put_buffer : INT32;         { current bit-accumulation buffer }
    put_bits : int;             { # of bits now in it }
    cinfo : j_compress_ptr;     { link to cinfo (needed for dump_buffer) }

    { Coding status for DC components }
    last_dc_val : array[0..MAX_COMPS_IN_SCAN-1] of int;
                                { last DC coef for each component }

    { Coding status for AC components }
    ac_tbl_no : int;            { the table number of the single component }
    EOBRUN : uInt;              { run length of EOBs }
    BE : uInt;                  { # of buffered correction bits before MCU }
    bit_buffer : JBytePtr;      { buffer for correction bits (1 per char) }
    { packing correction bits tightly would save some space but cost time... }

    restarts_to_go : uInt;      { MCUs left in this restart interval }
    next_restart_num : int;     { next restart number to write (0-7) }

    { Pointers to derived tables (these workspaces have image lifespan).
      Since any one scan codes only DC or only AC, we only need one set
      of tables, not one for DC and one for AC. }

    derived_tbls : array[0..NUM_HUFF_TBLS-1] of c_derived_tbl_ptr;

    { Statistics tables for optimization; again, one set is enough }
    count_ptrs : array[0..NUM_HUFF_TBLS-1] of TLongTablePtr;
  end;


{ MAX_CORR_BITS is the number of bits the AC refinement correction-bit
  buffer can hold.  Larger sizes may slightly improve compression, but
  1000 is already well into the realm of overkill.
  The minimum safe size is 64 bits. }

const
  MAX_CORR_BITS = 1000;         { Max # of correction bits I can buffer }


{ Forward declarations }
{METHODDEF}
function encode_mcu_DC_first (cinfo : j_compress_ptr;
                              const MCU_data: array of JBLOCKROW) : boolean;
                              far; forward;
{METHODDEF}
function encode_mcu_AC_first (cinfo : j_compress_ptr;
                              const MCU_data: array of JBLOCKROW) : boolean;
                              far; forward;
{METHODDEF}
function encode_mcu_DC_refine (cinfo : j_compress_ptr;
                              const MCU_data: array of JBLOCKROW) : boolean;
                              far; forward;
{METHODDEF}
function encode_mcu_AC_refine (cinfo : j_compress_ptr;
                              const MCU_data: array of JBLOCKROW) : boolean;
                              far; forward;

{METHODDEF}
procedure finish_pass_phuff (cinfo : j_compress_ptr); far; forward;

{METHODDEF}
procedure finish_pass_gather_phuff (cinfo : j_compress_ptr); far; forward;


{ Initialize for a Huffman-compressed scan using progressive JPEG. }

{METHODDEF}
procedure start_pass_phuff (cinfo : j_compress_ptr;
                            gather_statistics : boolean); far;
var
  entropy : phuff_entropy_ptr;
  is_DC_band : boolean;
  ci, tbl : int;
  compptr : jpeg_component_info_ptr;
begin
  entropy := phuff_entropy_ptr (cinfo^.entropy);

  entropy^.cinfo := cinfo;
  entropy^.gather_statistics := gather_statistics;

  is_DC_band := (cinfo^.Ss = 0);

  { We assume jcmaster.c already validated the scan parameters. }

  { Select execution routines }
  if (cinfo^.Ah = 0) then
  begin
    if (is_DC_band) then
      entropy^.pub.encode_mcu := encode_mcu_DC_first
    else
      entropy^.pub.encode_mcu := encode_mcu_AC_first;
  end
  else
  begin
    if (is_DC_band) then
      entropy^.pub.encode_mcu := encode_mcu_DC_refine
    else
    begin
      entropy^.pub.encode_mcu := encode_mcu_AC_refine;
      { AC refinement needs a correction bit buffer }
      if (entropy^.bit_buffer = NIL) then
        entropy^.bit_buffer := JBytePtr(
          cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                                      MAX_CORR_BITS * SIZEOF(byte)) );
    end;
  end;
  if (gather_statistics) then
    entropy^.pub.finish_pass := finish_pass_gather_phuff
  else
    entropy^.pub.finish_pass := finish_pass_phuff;

  { Only DC coefficients may be interleaved, so cinfo^.comps_in_scan = 1
    for AC coefficients. }

  for ci := 0 to pred(cinfo^.comps_in_scan) do
  begin
    compptr := cinfo^.cur_comp_info[ci];
    { Initialize DC predictions to 0 }
    entropy^.last_dc_val[ci] := 0;
    { Get table index }
    if (is_DC_band) then
    begin
      if (cinfo^.Ah <> 0) then  { DC refinement needs no table }
        continue;
      tbl := compptr^.dc_tbl_no;
    end
    else
    begin
      tbl := compptr^.ac_tbl_no;
      entropy^.ac_tbl_no := tbl;
    end;
    if (gather_statistics) then
    begin
      { Check for invalid table index }
      { (make_c_derived_tbl does this in the other path) }
      if (tbl < 0) or (tbl >= NUM_HUFF_TBLS) then
        ERREXIT1(j_common_ptr(cinfo), JERR_NO_HUFF_TABLE, tbl);
      { Allocate and zero the statistics tables }
      { Note that jpeg_gen_optimal_table expects 257 entries in each table! }
      if (entropy^.count_ptrs[tbl] = NIL) then
        entropy^.count_ptrs[tbl] := TLongTablePtr(
          cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                                      257 * SIZEOF(long)) );
      MEMZERO(entropy^.count_ptrs[tbl], 257 * SIZEOF(long));
    end else
    begin
      { Compute derived values for Huffman table }
      { We may do this more than once for a table, but it's not expensive }
      jpeg_make_c_derived_tbl(cinfo, is_DC_band, tbl,
                              entropy^.derived_tbls[tbl]);
    end;
  end;

  { Initialize AC stuff }
  entropy^.EOBRUN := 0;
  entropy^.BE := 0;

  { Initialize bit buffer to empty }
  entropy^.put_buffer := 0;
  entropy^.put_bits := 0;

  { Initialize restart stuff }
  entropy^.restarts_to_go := cinfo^.restart_interval;
  entropy^.next_restart_num := 0;
end;




{LOCAL}
procedure dump_buffer (entropy : phuff_entropy_ptr);
{ Empty the output buffer; we do not support suspension in this module. }
var
  dest : jpeg_destination_mgr_ptr;
begin
  dest := entropy^.cinfo^.dest;

  if (not dest^.empty_output_buffer (entropy^.cinfo)) then
    ERREXIT(j_common_ptr(entropy^.cinfo), JERR_CANT_SUSPEND);
  { After a successful buffer dump, must reset buffer pointers }
  entropy^.next_output_byte := dest^.next_output_byte;
  entropy^.free_in_buffer := dest^.free_in_buffer;
end;


{ Outputting bits to the file }

{ Only the right 24 bits of put_buffer are used; the valid bits are
  left-justified in this part.  At most 16 bits can be passed to emit_bits
  in one call, and we never retain more than 7 bits in put_buffer
  between calls, so 24 bits are sufficient. }


{LOCAL}
procedure emit_bits (entropy : phuff_entropy_ptr;
                     code : uInt;
                     size : int); {INLINE}
{ Emit some bits, unless we are in gather mode }
var
  {register} put_buffer : INT32;
  {register} put_bits : int;
var
  c : int;
begin
  { This routine is heavily used, so it's worth coding tightly. }
  put_buffer := INT32 (code);
  put_bits := entropy^.put_bits;

  { if size is 0, caller used an invalid Huffman table entry }
  if (size = 0) then
    ERREXIT(j_common_ptr(entropy^.cinfo), JERR_HUFF_MISSING_CODE);

  if (entropy^.gather_statistics) then
    exit;                       { do nothing if we're only getting stats }

  put_buffer := put_buffer and ((INT32(1) shl size) - 1);
                                { mask off any extra bits in code }

  Inc(put_bits, size);          { new number of bits in buffer }

  put_buffer := put_buffer shl (24 - put_bits); { align incoming bits }

  put_buffer := put_buffer or entropy^.put_buffer;
                                { and merge with old buffer contents }

  while (put_bits >= 8) do
  begin
    c := int ((put_buffer shr 16) and $FF);

    {emit_byte(entropy, c);}
    { Outputting bytes to the file.
      NB: these must be called only when actually outputting,
      that is, entropy^.gather_statistics = FALSE. }
    { Emit a byte }
    entropy^.next_output_byte^ := JOCTET(c);
    Inc(entropy^.next_output_byte);
    Dec(entropy^.free_in_buffer);
    if (entropy^.free_in_buffer = 0) then
      dump_buffer(entropy);

    if (c = $FF) then
    begin               { need to stuff a zero byte? }
      {emit_byte(entropy, 0);}
      entropy^.next_output_byte^ := JOCTET(0);
      Inc(entropy^.next_output_byte);
      Dec(entropy^.free_in_buffer);
      if (entropy^.free_in_buffer = 0) then
        dump_buffer(entropy);
    end;
    put_buffer := put_buffer shl 8;
    Dec(put_bits, 8);
  end;

  entropy^.put_buffer := put_buffer; { update variables }
  entropy^.put_bits := put_bits;
end;


{LOCAL}
procedure flush_bits (entropy : phuff_entropy_ptr);
begin
  emit_bits(entropy, $7F, 7); { fill any partial byte with ones }
  entropy^.put_buffer := 0;     { and reset bit-buffer to empty }
  entropy^.put_bits := 0;
end;

{ Emit (or just count) a Huffman symbol. }


{LOCAL}
procedure emit_symbol (entropy : phuff_entropy_ptr;
                       tbl_no : int;
                       symbol : int); {INLINE}
var
  tbl : c_derived_tbl_ptr;
begin
  if (entropy^.gather_statistics) then
    Inc(entropy^.count_ptrs[tbl_no]^[symbol])
  else
  begin
    tbl := entropy^.derived_tbls[tbl_no];
    emit_bits(entropy, tbl^.ehufco[symbol], tbl^.ehufsi[symbol]);
  end;
end;


{ Emit bits from a correction bit buffer. }

{LOCAL}
procedure emit_buffered_bits (entropy : phuff_entropy_ptr;
                              bufstart : JBytePtr;
                              nbits : uInt);
var
  bufptr : byteptr;
begin
  if (entropy^.gather_statistics) then
    exit;                       { no real work }

  bufptr := byteptr(bufstart);
  while (nbits > 0) do
  begin
    emit_bits(entropy, uInt(bufptr^), 1);
    Inc(bufptr);
    Dec(nbits);
  end;
end;


{ Emit any pending EOBRUN symbol. }

{LOCAL}
procedure emit_eobrun (entropy : phuff_entropy_ptr);
var
  {register} temp, nbits : int;
begin
  if (entropy^.EOBRUN > 0) then
  begin                        { if there is any pending EOBRUN }
    temp := entropy^.EOBRUN;
    nbits := 0;
    temp := temp shr 1;
    while (temp <> 0) do
    begin
      Inc(nbits);
      temp := temp shr 1;
    end;

    { safety check: shouldn't happen given limited correction-bit buffer }
    if (nbits > 14) then
      ERREXIT(j_common_ptr(entropy^.cinfo), JERR_HUFF_MISSING_CODE);

    emit_symbol(entropy, entropy^.ac_tbl_no, nbits shl 4);
    if (nbits <> 0) then
      emit_bits(entropy, entropy^.EOBRUN, nbits);

    entropy^.EOBRUN := 0;

    { Emit any buffered correction bits }
    emit_buffered_bits(entropy, entropy^.bit_buffer, entropy^.BE);
    entropy^.BE := 0;
  end;
end;


{ Emit a restart marker & resynchronize predictions. }

{LOCAL}
procedure emit_restart (entropy : phuff_entropy_ptr;
                        restart_num : int);
var
  ci : int;
begin
  emit_eobrun(entropy);

  if (not entropy^.gather_statistics) then
  begin
    flush_bits(entropy);
    {emit_byte(entropy, $FF);}
    { Outputting bytes to the file.
      NB: these must be called only when actually outputting,
      that is, entropy^.gather_statistics = FALSE. }

    entropy^.next_output_byte^ := JOCTET($FF);
    Inc(entropy^.next_output_byte);
    Dec(entropy^.free_in_buffer);
    if (entropy^.free_in_buffer = 0) then
      dump_buffer(entropy);

    {emit_byte(entropy, JPEG_RST0 + restart_num);}
    entropy^.next_output_byte^ := JOCTET(JPEG_RST0 + restart_num);
    Inc(entropy^.next_output_byte);
    Dec(entropy^.free_in_buffer);
    if (entropy^.free_in_buffer = 0) then
      dump_buffer(entropy);
  end;

  if (entropy^.cinfo^.Ss = 0) then
  begin
    { Re-initialize DC predictions to 0 }
    for ci := 0 to pred(entropy^.cinfo^.comps_in_scan) do
      entropy^.last_dc_val[ci] := 0;
  end
  else
  begin
    { Re-initialize all AC-related fields to 0 }
    entropy^.EOBRUN := 0;
    entropy^.BE := 0;
  end;
end;


{ MCU encoding for DC initial scan (either spectral selection,
  or first pass of successive approximation). }

{METHODDEF}
function encode_mcu_DC_first (cinfo : j_compress_ptr;
                              const MCU_data: array of JBLOCKROW) : boolean;
var
  entropy : phuff_entropy_ptr;
  {register} temp, temp2 : int;
  {register} nbits : int;
  blkn, ci : int;
  Al : int;
  block : JBLOCK_PTR;
  compptr : jpeg_component_info_ptr;
  ishift_temp : int;
begin
  entropy := phuff_entropy_ptr (cinfo^.entropy);
  Al := cinfo^.Al;

  entropy^.next_output_byte := cinfo^.dest^.next_output_byte;
  entropy^.free_in_buffer := cinfo^.dest^.free_in_buffer;

  { Emit restart marker if needed }
  if (cinfo^.restart_interval <> 0) then
    if (entropy^.restarts_to_go = 0) then
      emit_restart(entropy, entropy^.next_restart_num);

  { Encode the MCU data blocks }
  for blkn := 0 to pred(cinfo^.blocks_in_MCU) do
  begin
    block := JBLOCK_PTR(MCU_data[blkn]);
    ci := cinfo^.MCU_membership[blkn];
    compptr := cinfo^.cur_comp_info[ci];

    { Compute the DC value after the required point transform by Al.
      This is simply an arithmetic right shift. }

    {temp2 := IRIGHT_SHIFT( int(block^[0]), Al);}
    {IRIGHT_SHIFT_IS_UNSIGNED}
    ishift_temp := int(block^[0]);
    if ishift_temp < 0 then
      temp2 := (ishift_temp shr Al) or ((not 0) shl (16-Al))
    else
      temp2 := ishift_temp shr Al;


    { DC differences are figured on the point-transformed values. }
    temp := temp2 - entropy^.last_dc_val[ci];
    entropy^.last_dc_val[ci] := temp2;

    { Encode the DC coefficient difference per section G.1.2.1 }
    temp2 := temp;
    if (temp < 0) then
    begin
      temp := -temp;            { temp is abs value of input }
      { For a negative input, want temp2 := bitwise complement of abs(input) }
      { This code assumes we are on a two's complement machine }
      Dec(temp2);
    end;

    { Find the number of bits needed for the magnitude of the coefficient }
    nbits := 0;
    while (temp <> 0) do
    begin
      Inc(nbits);
      temp := temp shr 1;
    end;

    { Check for out-of-range coefficient values.
      Since we're encoding a difference, the range limit is twice as much. }

    if (nbits > MAX_COEF_BITS+1) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_DCT_COEF);

    { Count/emit the Huffman-coded symbol for the number of bits }
    emit_symbol(entropy, compptr^.dc_tbl_no, nbits);

    { Emit that number of bits of the value, if positive, }
    { or the complement of its magnitude, if negative. }
    if (nbits <> 0) then       { emit_bits rejects calls with size 0 }
      emit_bits(entropy, uInt(temp2), nbits);
  end;

  cinfo^.dest^.next_output_byte := entropy^.next_output_byte;
  cinfo^.dest^.free_in_buffer := entropy^.free_in_buffer;

  { Update restart-interval state too }
  if (cinfo^.restart_interval <> 0) then
  begin
    if (entropy^.restarts_to_go = 0) then
    begin
      entropy^.restarts_to_go := cinfo^.restart_interval;
      Inc(entropy^.next_restart_num);
      with entropy^ do
        next_restart_num := next_restart_num and 7;
    end;
    Dec(entropy^.restarts_to_go);
  end;

  encode_mcu_DC_first := TRUE;
end;


{ MCU encoding for AC initial scan (either spectral selection,
  or first pass of successive approximation). }

{METHODDEF}
function encode_mcu_AC_first (cinfo : j_compress_ptr;
                              const MCU_data: array of JBLOCKROW) : boolean;
var
  entropy : phuff_entropy_ptr;
  {register} temp, temp2 : int;
  {register} nbits : int;
  {register} r, k : int;
  Se : int;
  Al : int;
  block : JBLOCK_PTR;
begin
  entropy := phuff_entropy_ptr (cinfo^.entropy);
  Se := cinfo^.Se;
  Al := cinfo^.Al;

  entropy^.next_output_byte := cinfo^.dest^.next_output_byte;
  entropy^.free_in_buffer := cinfo^.dest^.free_in_buffer;

  { Emit restart marker if needed }
  if (cinfo^.restart_interval <> 0) then
    if (entropy^.restarts_to_go = 0) then
      emit_restart(entropy, entropy^.next_restart_num);

  { Encode the MCU data block }
  block := JBLOCK_PTR(MCU_data[0]);

  { Encode the AC coefficients per section G.1.2.2, fig. G.3 }

  r := 0;                       { r := run length of zeros }

  for k := cinfo^.Ss to Se do
  begin
    temp := (block^[jpeg_natural_order[k]]);
    if (temp = 0) then
    begin
      Inc(r);
      continue;
    end;
    { We must apply the point transform by Al.  For AC coefficients this
      is an integer division with rounding towards 0.  To do this portably
      in C, we shift after obtaining the absolute value; so the code is
      interwoven with finding the abs value (temp) and output bits (temp2). }

    if (temp < 0) then
    begin
      temp := -temp;            { temp is abs value of input }
      temp := temp shr Al;      { apply the point transform }
      { For a negative coef, want temp2 := bitwise complement of abs(coef) }
      temp2 := not temp;
    end
    else
    begin
      temp := temp shr Al;      { apply the point transform }
      temp2 := temp;
    end;
    { Watch out for case that nonzero coef is zero after point transform }
    if (temp = 0) then
    begin
      Inc(r);
      continue;
    end;

    { Emit any pending EOBRUN }
    if (entropy^.EOBRUN > 0) then
      emit_eobrun(entropy);
    { if run length > 15, must emit special run-length-16 codes ($F0) }
    while (r > 15) do
    begin
      emit_symbol(entropy, entropy^.ac_tbl_no, $F0);
      Dec(r, 16);
    end;

    { Find the number of bits needed for the magnitude of the coefficient }
    nbits := 0;                 { there must be at least one 1 bit }
    repeat
      Inc(nbits);
      temp := temp shr 1;
    until (temp = 0);

    { Check for out-of-range coefficient values }
    if (nbits > MAX_COEF_BITS) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_DCT_COEF);

    { Count/emit Huffman symbol for run length / number of bits }
    emit_symbol(entropy, entropy^.ac_tbl_no, (r shl 4) + nbits);

    { Emit that number of bits of the value, if positive, }
    { or the complement of its magnitude, if negative. }
    emit_bits(entropy, uInt(temp2), nbits);

    r := 0;                     { reset zero run length }
  end;

  if (r > 0) then
  begin                         { If there are trailing zeroes, }
    Inc(entropy^.EOBRUN);       { count an EOB }
    if (entropy^.EOBRUN = $7FFF) then
      emit_eobrun(entropy);     { force it out to avoid overflow }
  end;

  cinfo^.dest^.next_output_byte := entropy^.next_output_byte;
  cinfo^.dest^.free_in_buffer := entropy^.free_in_buffer;

  { Update restart-interval state too }
  if (cinfo^.restart_interval <> 0) then
  begin
    if (entropy^.restarts_to_go = 0) then
    begin
      entropy^.restarts_to_go := cinfo^.restart_interval;
      Inc(entropy^.next_restart_num);
      with entropy^ do
        next_restart_num := next_restart_num and 7;
    end;
    Dec(entropy^.restarts_to_go);
  end;

  encode_mcu_AC_first := TRUE;
end;


{ MCU encoding for DC successive approximation refinement scan.
  Note: we assume such scans can be multi-component, although the spec
  is not very clear on the point. }

{METHODDEF}
function encode_mcu_DC_refine (cinfo : j_compress_ptr;
                              const MCU_data: array of JBLOCKROW) : boolean;
var
  entropy : phuff_entropy_ptr;
  {register} temp : int;
  blkn : int;
  Al : int;
  block : JBLOCK_PTR;
begin
  entropy := phuff_entropy_ptr (cinfo^.entropy);
  Al := cinfo^.Al;

  entropy^.next_output_byte := cinfo^.dest^.next_output_byte;
  entropy^.free_in_buffer := cinfo^.dest^.free_in_buffer;

  { Emit restart marker if needed }
  if (cinfo^.restart_interval <> 0) then
    if (entropy^.restarts_to_go = 0) then
      emit_restart(entropy, entropy^.next_restart_num);

  { Encode the MCU data blocks }
  for blkn := 0 to pred(cinfo^.blocks_in_MCU) do
  begin
    block := JBLOCK_PTR(MCU_data[blkn]);

    { We simply emit the Al'th bit of the DC coefficient value. }
    temp := block^[0];
    emit_bits(entropy, uInt(temp shr Al), 1);
  end;

  cinfo^.dest^.next_output_byte := entropy^.next_output_byte;
  cinfo^.dest^.free_in_buffer := entropy^.free_in_buffer;

  { Update restart-interval state too }
  if (cinfo^.restart_interval <> 0) then
  begin
    if (entropy^.restarts_to_go = 0) then
    begin
      entropy^.restarts_to_go := cinfo^.restart_interval;
      Inc(entropy^.next_restart_num);
      with entropy^ do
        next_restart_num := next_restart_num and 7;
    end;
    Dec(entropy^.restarts_to_go);
  end;

  encode_mcu_DC_refine := TRUE;
end;


{ MCU encoding for AC successive approximation refinement scan. }

{METHODDEF}
function encode_mcu_AC_refine (cinfo : j_compress_ptr;
                               const MCU_data: array of JBLOCKROW) : boolean;

var
  entropy : phuff_entropy_ptr;
  {register} temp : int;
  {register} r, k : int;
  EOB : int;
  BR_buffer : JBytePtr;
  BR : uInt;
  Se : int;
  Al : int;
  block : JBLOCK_PTR;
  absvalues : array[0..DCTSIZE2-1] of int;
begin
  entropy := phuff_entropy_ptr(cinfo^.entropy);
  Se := cinfo^.Se;
  Al := cinfo^.Al;

  entropy^.next_output_byte := cinfo^.dest^.next_output_byte;
  entropy^.free_in_buffer := cinfo^.dest^.free_in_buffer;

  { Emit restart marker if needed }
  if (cinfo^.restart_interval <> 0) then
    if (entropy^.restarts_to_go = 0) then
      emit_restart(entropy, entropy^.next_restart_num);

  { Encode the MCU data block }
  block := JBLOCK_PTR(MCU_data[0]);

  { It is convenient to make a pre-pass to determine the transformed
    coefficients' absolute values and the EOB position. }

  EOB := 0;
  for k := cinfo^.Ss to Se do
  begin
    temp := block^[jpeg_natural_order[k]];
    { We must apply the point transform by Al.  For AC coefficients this
      is an integer division with rounding towards 0.  To do this portably
      in C, we shift after obtaining the absolute value. }

    if (temp < 0) then
      temp := -temp;            { temp is abs value of input }
    temp := temp shr Al;                { apply the point transform }
    absvalues[k] := temp;       { save abs value for main pass }
    if (temp = 1) then
      EOB := k;                 { EOB := index of last newly-nonzero coef }
  end;

  { Encode the AC coefficients per section G.1.2.3, fig. G.7 }

  r := 0;                       { r := run length of zeros }
  BR := 0;                      { BR := count of buffered bits added now }
  BR_buffer := JBytePtr(@(entropy^.bit_buffer^[entropy^.BE]));
                                { Append bits to buffer }

  for k := cinfo^.Ss to Se do
  begin
    temp := absvalues[k];
    if (temp = 0) then
    begin
      Inc(r);
      continue;
    end;

    { Emit any required ZRLs, but not if they can be folded into EOB }
    while (r > 15) and (k <= EOB) do
    begin
      { emit any pending EOBRUN and the BE correction bits }
      emit_eobrun(entropy);
      { Emit ZRL }
      emit_symbol(entropy, entropy^.ac_tbl_no, $F0);
      Dec(r, 16);
      { Emit buffered correction bits that must be associated with ZRL }
      emit_buffered_bits(entropy, BR_buffer, BR);
      BR_buffer := entropy^.bit_buffer; { BE bits are gone now }
      BR := 0;
    end;

    { If the coef was previously nonzero, it only needs a correction bit.
      NOTE: a straight translation of the spec's figure G.7 would suggest
      that we also need to test r > 15.  But if r > 15, we can only get here
      if k > EOB, which implies that this coefficient is not 1. }
    if (temp > 1) then
    begin
      { The correction bit is the next bit of the absolute value. }
      BR_buffer^[BR] := byte (temp and 1);
      Inc(BR);
      continue;
    end;

    { Emit any pending EOBRUN and the BE correction bits }
    emit_eobrun(entropy);

    { Count/emit Huffman symbol for run length / number of bits }
    emit_symbol(entropy, entropy^.ac_tbl_no, (r shl 4) + 1);

    { Emit output bit for newly-nonzero coef }
    if (block^[jpeg_natural_order[k]] < 0) then
      temp := 0
    else
      temp := 1;
    emit_bits(entropy, uInt(temp), 1);

    { Emit buffered correction bits that must be associated with this code }
    emit_buffered_bits(entropy, BR_buffer, BR);
    BR_buffer := entropy^.bit_buffer; { BE bits are gone now }
    BR := 0;
    r := 0;                     { reset zero run length }
  end;

  if (r > 0) or (BR > 0) then
  begin                         { If there are trailing zeroes, }
    Inc(entropy^.EOBRUN);       { count an EOB }
    Inc(entropy^.BE, BR);          { concat my correction bits to older ones }
    { We force out the EOB if we risk either:
      1. overflow of the EOB counter;
      2. overflow of the correction bit buffer during the next MCU. }

    if (entropy^.EOBRUN = $7FFF) or
       (entropy^.BE > (MAX_CORR_BITS-DCTSIZE2+1)) then
      emit_eobrun(entropy);
  end;

  cinfo^.dest^.next_output_byte := entropy^.next_output_byte;
  cinfo^.dest^.free_in_buffer := entropy^.free_in_buffer;

  { Update restart-interval state too }
  if (cinfo^.restart_interval <> 0) then
  begin
    if (entropy^.restarts_to_go = 0) then
    begin
      entropy^.restarts_to_go := cinfo^.restart_interval;
      Inc(entropy^.next_restart_num);
      with entropy^ do
        next_restart_num := next_restart_num and 7;
    end;
    Dec(entropy^.restarts_to_go);
  end;

  encode_mcu_AC_refine := TRUE;
end;


{ Finish up at the end of a Huffman-compressed progressive scan. }

{METHODDEF}
procedure finish_pass_phuff (cinfo : j_compress_ptr);
var
  entropy : phuff_entropy_ptr;
begin
  entropy := phuff_entropy_ptr (cinfo^.entropy);

  entropy^.next_output_byte := cinfo^.dest^.next_output_byte;
  entropy^.free_in_buffer := cinfo^.dest^.free_in_buffer;

  { Flush out any buffered data }
  emit_eobrun(entropy);
  flush_bits(entropy);

  cinfo^.dest^.next_output_byte := entropy^.next_output_byte;
  cinfo^.dest^.free_in_buffer := entropy^.free_in_buffer;
end;


{ Finish up a statistics-gathering pass and create the new Huffman tables. }

{METHODDEF}
procedure finish_pass_gather_phuff (cinfo : j_compress_ptr);
var
  entropy : phuff_entropy_ptr;
  is_DC_band : boolean;
  ci, tbl : int;
  compptr : jpeg_component_info_ptr;
  htblptr : ^JHUFF_TBL_PTR;
  did : array[0..NUM_HUFF_TBLS-1] of boolean;
begin
  entropy := phuff_entropy_ptr (cinfo^.entropy);

  { Flush out buffered data (all we care about is counting the EOB symbol) }
  emit_eobrun(entropy);

  is_DC_band := (cinfo^.Ss = 0);

  { It's important not to apply jpeg_gen_optimal_table more than once
    per table, because it clobbers the input frequency counts! }

  MEMZERO(@did, SIZEOF(did));

  for ci := 0 to pred(cinfo^.comps_in_scan) do
  begin
    compptr := cinfo^.cur_comp_info[ci];
    if (is_DC_band) then
    begin
      if (cinfo^.Ah <> 0) then     { DC refinement needs no table }
        continue;
      tbl := compptr^.dc_tbl_no;
    end
    else
    begin
      tbl := compptr^.ac_tbl_no;
    end;
    if (not did[tbl]) then
    begin
      if (is_DC_band) then
        htblptr := @(cinfo^.dc_huff_tbl_ptrs[tbl])
      else
        htblptr := @(cinfo^.ac_huff_tbl_ptrs[tbl]);
      if (htblptr^ = NIL) then
        htblptr^ := jpeg_alloc_huff_table(j_common_ptr(cinfo));
      jpeg_gen_optimal_table(cinfo, htblptr^, entropy^.count_ptrs[tbl]^);
      did[tbl] := TRUE;
    end;
  end;
end;


{ Module initialization routine for progressive Huffman entropy encoding. }

{GLOBAL}
procedure jinit_phuff_encoder (cinfo : j_compress_ptr);
var
  entropy : phuff_entropy_ptr;
  i : int;
begin
  entropy := phuff_entropy_ptr(
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                                SIZEOF(phuff_entropy_encoder)) );
  cinfo^.entropy := jpeg_entropy_encoder_ptr(entropy);
  entropy^.pub.start_pass := start_pass_phuff;

  { Mark tables unallocated }
  for i := 0 to pred(NUM_HUFF_TBLS) do
  begin
    entropy^.derived_tbls[i] := NIL;
    entropy^.count_ptrs[i] := NIL;
  end;
  entropy^.bit_buffer := NIL;   { needed only in AC refinement scan }
end;

end.
