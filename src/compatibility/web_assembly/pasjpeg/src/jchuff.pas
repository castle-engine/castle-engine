Unit JcHuff;

{ This file contains Huffman entropy encoding routines.

  Much of the complexity here has to do with supporting output suspension.
  If the data destination module demands suspension, we want to be able to
  back up to the start of the current MCU.  To do this, we copy state
  variables into local working storage, and update them back to the
  permanent JPEG objects only upon successful completion of an MCU. }

{ Original: jchuff.c; Copyright (C) 1991-1997, Thomas G. Lane. }

interface

{$I jconfig.inc}

uses
  jmorecfg, { longptr definition missing }
  jpeglib,
  jdeferr,
  jerror,
  jutils,
  jinclude,
  jcomapi;

{ The legal range of a DCT coefficient is
   -1024 .. +1023  for 8-bit data;
  -16384 .. +16383 for 12-bit data.
  Hence the magnitude should always fit in 10 or 14 bits respectively. }


{$ifdef BITS_IN_JSAMPLE_IS_8}
const
  MAX_COEF_BITS = 10;
{$else}
const
  MAX_COEF_BITS = 14;
{$endif}

{ Derived data constructed for each Huffman table }
{ Declarations shared with jcphuff.c }
type
  c_derived_tbl_ptr = ^c_derived_tbl;
  c_derived_tbl = record
    ehufco : array[0..256-1] of uInt;   { code for each symbol }
    ehufsi : array[0..256-1] of byte;   { length of code for each symbol }
    { If no code has been allocated for a symbol S, ehufsi[S] contains 0 }
  end;
{ for JCHUFF und JCPHUFF }
type
  TLongTable = array[0..256] of long;
  TLongTablePtr = ^TLongTable;

{ Compute the derived values for a Huffman table.
  Note this is also used by jcphuff.c. }

{GLOBAL}
procedure jpeg_make_c_derived_tbl (cinfo : j_compress_ptr;
                                   isDC : boolean;
                                   tblno : int;
                                   var pdtbl : c_derived_tbl_ptr);

{ Generate the optimal coding for the given counts, fill htbl.
  Note this is also used by jcphuff.c. }

{GLOBAL}
procedure jpeg_gen_optimal_table (cinfo : j_compress_ptr;
                                  htbl : JHUFF_TBL_PTR;
                                  var freq : TLongTable);  { Nomssi }

{ Module initialization routine for Huffman entropy encoding. }

{GLOBAL}
procedure jinit_huff_encoder (cinfo : j_compress_ptr);

implementation

{ Expanded entropy encoder object for Huffman encoding.

  The savable_state subrecord contains fields that change within an MCU,
  but must not be updated permanently until we complete the MCU. }

type
  savable_state = record
    put_buffer : INT32;         { current bit-accumulation buffer }
    put_bits : int;             { # of bits now in it }
    last_dc_val : array[0..MAX_COMPS_IN_SCAN-1] of int;
                                { last DC coef for each component }
  end;


type
  huff_entropy_ptr = ^huff_entropy_encoder;
  huff_entropy_encoder = record
    pub : jpeg_entropy_encoder; { public fields }

    saved : savable_state;      { Bit buffer & DC state at start of MCU }

    { These fields are NOT loaded into local working state. }
    restarts_to_go : uInt;      { MCUs left in this restart interval }
    next_restart_num : int;     { next restart number to write (0-7) }

    { Pointers to derived tables (these workspaces have image lifespan) }
    dc_derived_tbls : array[0..NUM_HUFF_TBLS-1] of c_derived_tbl_ptr;
    ac_derived_tbls : array[0..NUM_HUFF_TBLS-1] of c_derived_tbl_ptr;

  {$ifdef ENTROPY_OPT_SUPPORTED} { Statistics tables for optimization }
    dc_count_ptrs : array[0..NUM_HUFF_TBLS-1] of TLongTablePtr;
    ac_count_ptrs : array[0..NUM_HUFF_TBLS-1] of TLongTablePtr;
  {$endif}
  end;



{ Working state while writing an MCU.
  This struct contains all the fields that are needed by subroutines. }

type
  working_state = record
    next_output_byte : JOCTETptr; { => next byte to write in buffer }
    free_in_buffer : size_t;      { # of byte spaces remaining in buffer }
    cur : savable_state;          { Current bit buffer & DC state }
    cinfo : j_compress_ptr;       { dump_buffer needs access to this }
  end;


{ Forward declarations }
{METHODDEF}
function encode_mcu_huff (cinfo : j_compress_ptr;
                          const MCU_data : array of JBLOCKROW) : boolean; far;
                          forward;
{METHODDEF}
procedure finish_pass_huff (cinfo : j_compress_ptr); far; forward;
{$ifdef ENTROPY_OPT_SUPPORTED}
{METHODDEF}
function encode_mcu_gather (cinfo : j_compress_ptr;
                            const MCU_data: array of JBLOCKROW) : boolean;
                            far; forward;

{METHODDEF}
procedure finish_pass_gather (cinfo : j_compress_ptr); far; forward;
{$endif}


{ Initialize for a Huffman-compressed scan.
  If gather_statistics is TRUE, we do not output anything during the scan,
  just count the Huffman symbols used and generate Huffman code tables. }

{METHODDEF}
procedure start_pass_huff (cinfo : j_compress_ptr;
                           gather_statistics : boolean); far;
var
  entropy : huff_entropy_ptr;
  ci, dctbl, actbl : int;
  compptr : jpeg_component_info_ptr;
begin
  entropy := huff_entropy_ptr (cinfo^.entropy);

  if (gather_statistics) then
  begin
{$ifdef ENTROPY_OPT_SUPPORTED}
    entropy^.pub.encode_mcu := encode_mcu_gather;
    entropy^.pub.finish_pass := finish_pass_gather;
{$else}
    ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
{$endif}
  end
  else
  begin
    entropy^.pub.encode_mcu := encode_mcu_huff;
    entropy^.pub.finish_pass := finish_pass_huff;
  end;

  for ci := 0 to pred(cinfo^.comps_in_scan) do
  begin
    compptr := cinfo^.cur_comp_info[ci];
    dctbl := compptr^.dc_tbl_no;
    actbl := compptr^.ac_tbl_no;
    if (gather_statistics) then
    begin
{$ifdef ENTROPY_OPT_SUPPORTED}
      { Check for invalid table indexes }
      { (make_c_derived_tbl does this in the other path) }
      if (dctbl < 0) or (dctbl >= NUM_HUFF_TBLS) then
        ERREXIT1(j_common_ptr(cinfo), JERR_NO_HUFF_TABLE, dctbl);
      if (actbl < 0) or (actbl >= NUM_HUFF_TBLS) then
        ERREXIT1(j_common_ptr(cinfo), JERR_NO_HUFF_TABLE, actbl);
      { Allocate and zero the statistics tables }
      { Note that jpeg_gen_optimal_table expects 257 entries in each table! }
      if (entropy^.dc_count_ptrs[dctbl] = NIL) then
        entropy^.dc_count_ptrs[dctbl] := TLongTablePtr(
          cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                                      257 * SIZEOF(long)) );
      MEMZERO(entropy^.dc_count_ptrs[dctbl], 257 * SIZEOF(long));
      if (entropy^.ac_count_ptrs[actbl] = NIL) then
        entropy^.ac_count_ptrs[actbl] := TLongTablePtr(
          cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                                      257 * SIZEOF(long)) );
      MEMZERO(entropy^.ac_count_ptrs[actbl], 257 * SIZEOF(long));
{$endif}
    end
    else
    begin
      { Compute derived values for Huffman tables }
      { We may do this more than once for a table, but it's not expensive }
      jpeg_make_c_derived_tbl(cinfo, TRUE, dctbl,
                              entropy^.dc_derived_tbls[dctbl]);
      jpeg_make_c_derived_tbl(cinfo, FALSE, actbl,
                              entropy^.ac_derived_tbls[actbl]);
    end;
    { Initialize DC predictions to 0 }
    entropy^.saved.last_dc_val[ci] := 0;
  end;

  { Initialize bit buffer to empty }
  entropy^.saved.put_buffer := 0;
  entropy^.saved.put_bits := 0;

  { Initialize restart stuff }
  entropy^.restarts_to_go := cinfo^.restart_interval;
  entropy^.next_restart_num := 0;
end;


{ Compute the derived values for a Huffman table.
  This routine also performs some validation checks on the table.

  Note this is also used by jcphuff.c. }

{GLOBAL}
procedure jpeg_make_c_derived_tbl (cinfo : j_compress_ptr;
                                   isDC : boolean;
                                   tblno : int;
                                   var pdtbl : c_derived_tbl_ptr);
var
  htbl : JHUFF_TBL_PTR;
  dtbl : c_derived_tbl_ptr;
  p, i, l, lastp, si, maxsymbol : int;
  huffsize : array[0..257-1] of byte;
  huffcode : array[0..257-1] of uInt;
  code : uInt;
begin
  { Note that huffsize[] and huffcode[] are filled in code-length order,
    paralleling the order of the symbols themselves in htbl->huffval[]. }

  { Find the input Huffman table }
  if (tblno < 0) or (tblno >= NUM_HUFF_TBLS) then
    ERREXIT1(j_common_ptr(cinfo), JERR_NO_HUFF_TABLE, tblno);
  if isDC then
    htbl := cinfo^.dc_huff_tbl_ptrs[tblno]
  else
    htbl := cinfo^.ac_huff_tbl_ptrs[tblno];
  if (htbl = NIL) then
    ERREXIT1(j_common_ptr(cinfo), JERR_NO_HUFF_TABLE, tblno);

  { Allocate a workspace if we haven't already done so. }
  if (pdtbl = NIL) then
    pdtbl := c_derived_tbl_ptr(
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                                  SIZEOF(c_derived_tbl)) );
  dtbl := pdtbl;

  { Figure C.1: make table of Huffman code length for each symbol }

  p := 0;
  for l := 1 to 16 do
  begin
    i := int(htbl^.bits[l]);
    if (i < 0) and (p + i > 256) then   { protect against table overrun }
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_HUFF_TABLE);
    while (i > 0) do
    begin
      huffsize[p] := byte(l);
      Inc(p);
      Dec(i);
    end;
  end;
  huffsize[p] := 0;
  lastp := p;

  { Figure C.2: generate the codes themselves }
  { We also validate that the counts represent a legal Huffman code tree. }

  code := 0;
  si := huffsize[0];
  p := 0;
  while (huffsize[p] <> 0) do
  begin
    while (( int(huffsize[p]) ) = si) do
    begin
      huffcode[p] := code;
      Inc(p);
      Inc(code);
    end;
    { code is now 1 more than the last code used for codelength si; but
      it must still fit in si bits, since no code is allowed to be all ones. }

    if (INT32(code) >= (INT32(1) shl si)) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_HUFF_TABLE);
    code := code shl 1;
    Inc(si);
  end;

  { Figure C.3: generate encoding tables }
  { These are code and size indexed by symbol value }

  { Set all codeless symbols to have code length 0;
    this lets us detect duplicate VAL entries here, and later
    allows emit_bits to detect any attempt to emit such symbols. }

  MEMZERO(@dtbl^.ehufsi, SIZEOF(dtbl^.ehufsi));

  { This is also a convenient place to check for out-of-range
    and duplicated VAL entries.  We allow 0..255 for AC symbols
    but only 0..15 for DC.  (We could constrain them further
    based on data depth and mode, but this seems enough.) }

  if isDC then
    maxsymbol := 15
  else
    maxsymbol := 255;

  for p := 0 to pred(lastp) do
  begin
    i := htbl^.huffval[p];
    if (i < 0) or (i > maxsymbol) or (dtbl^.ehufsi[i] <> 0) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_HUFF_TABLE);
    dtbl^.ehufco[i] := huffcode[p];
    dtbl^.ehufsi[i] := huffsize[p];
  end;
end;


{ Outputting bytes to the file }


{LOCAL}
function dump_buffer (var state : working_state) : boolean;
{ Empty the output buffer; return TRUE if successful, FALSE if must suspend }
var
  dest : jpeg_destination_mgr_ptr;
begin
  dest := state.cinfo^.dest;

  if (not dest^.empty_output_buffer (state.cinfo)) then
  begin
    dump_buffer := FALSE;
    exit;
  end;
  { After a successful buffer dump, must reset buffer pointers }
  state.next_output_byte := dest^.next_output_byte;
  state.free_in_buffer := dest^.free_in_buffer;
  dump_buffer := TRUE;
end;


{ Outputting bits to the file }

{ Only the right 24 bits of put_buffer are used; the valid bits are
  left-justified in this part.  At most 16 bits can be passed to emit_bits
  in one call, and we never retain more than 7 bits in put_buffer
  between calls, so 24 bits are sufficient. }


{LOCAL}
function emit_bits (var state : working_state;
                    code : uInt;
                    size : int) : boolean;  {INLINE}
{ Emit some bits; return TRUE if successful, FALSE if must suspend }
var
  { This routine is heavily used, so it's worth coding tightly. }
  {register} put_buffer : INT32;
  {register} put_bits : int;
var
  c : int;
begin
  put_buffer := INT32 (code);
  put_bits := state.cur.put_bits;

  { if size is 0, caller used an invalid Huffman table entry }
  if (size = 0) then
    ERREXIT(j_common_ptr(state.cinfo), JERR_HUFF_MISSING_CODE);

  put_buffer := put_buffer and pred(INT32(1) shl size);
                { mask off any extra bits in code }

  Inc(put_bits, size);          { new number of bits in buffer }

  put_buffer := put_buffer shl (24 - put_bits);
                                { align incoming bits }
  put_buffer := put_buffer or state.cur.put_buffer;
                                { and merge with old buffer contents }
  while (put_bits >= 8) do
  begin
    c := int ((put_buffer shr 16) and $FF);

    {emit_byte(state, c, return FALSE);}
    { Emit a byte, return FALSE if must suspend. }
    state.next_output_byte^ := JOCTET (c);
    Inc(state.next_output_byte);
    Dec(state.free_in_buffer);
    if (state.free_in_buffer = 0) then
      if not dump_buffer(state) then
      begin
        emit_bits := FALSE;
        exit;
      end;

    if (c = $FF) then          { need to stuff a zero byte? }
    begin
      {emit_byte(state, 0, return FALSE);}
      state.next_output_byte^ := JOCTET (0);
      Inc(state.next_output_byte);
      Dec(state.free_in_buffer);
      if (state.free_in_buffer = 0) then
        if not dump_buffer(state) then
        begin
          emit_bits := FALSE;
          exit;
        end;

    end;
    put_buffer := put_buffer shl 8;
    Dec(put_bits, 8);
  end;

  state.cur.put_buffer := put_buffer; { update state variables }
  state.cur.put_bits := put_bits;

  emit_bits := TRUE;
end;


{LOCAL}
function flush_bits (var state : working_state) : boolean;
begin
  if (not emit_bits(state, $7F, 7)) then { fill any partial byte with ones }
  begin
    flush_bits := FALSE;
    exit;
  end;
  state.cur.put_buffer := 0;    { and reset bit-buffer to empty }
  state.cur.put_bits := 0;
  flush_bits := TRUE;
end;


{ Encode a single block's worth of coefficients }

{LOCAL}
function encode_one_block (var state : working_state;
                           const block : JBLOCK;
                           last_dc_val : int;
                           dctbl : c_derived_tbl_ptr;
                           actbl : c_derived_tbl_ptr) : boolean;
var
  {register} temp, temp2 : int;
  {register} nbits : int;
  {register} k, r, i : int;
begin
  { Encode the DC coefficient difference per section F.1.2.1 }

  temp2 := block[0] - last_dc_val;
  temp := temp2;

  if (temp < 0) then
  begin
    temp := -temp;              { temp is abs value of input }
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
    ERREXIT(j_common_ptr(state.cinfo), JERR_BAD_DCT_COEF);

  { Emit the Huffman-coded symbol for the number of bits }
  if not emit_bits(state, dctbl^.ehufco[nbits], dctbl^.ehufsi[nbits]) then
  begin
    encode_one_block := FALSE;
    exit;
  end;

  { Emit that number of bits of the value, if positive, }
  { or the complement of its magnitude, if negative. }
  if (nbits <> 0) then              { emit_bits rejects calls with size 0 }
    if not emit_bits(state, uInt(temp2), nbits) then
    begin
      encode_one_block := FALSE;
      exit;
    end;

  { Encode the AC coefficients per section F.1.2.2 }

  r := 0;                       { r := run length of zeros }

  for k := 1 to pred(DCTSIZE2) do
  begin
    temp := block[jpeg_natural_order[k]];
    if (temp = 0) then
    begin
      Inc(r);
    end
    else
    begin
      { if run length > 15, must emit special run-length-16 codes ($F0) }
      while (r > 15) do
      begin
        if not emit_bits(state, actbl^.ehufco[$F0], actbl^.ehufsi[$F0]) then
        begin
          encode_one_block := FALSE;
          exit;
        end;
        Dec(r, 16);
      end;

      temp2 := temp;
      if (temp < 0) then
      begin
        temp := -temp;          { temp is abs value of input }
        { This code assumes we are on a two's complement machine }
        Dec(temp2);
      end;

      { Find the number of bits needed for the magnitude of the coefficient }
      nbits := 0;               { there must be at least one 1 bit }
      repeat
        Inc(nbits);
        temp := temp shr 1;
      until (temp = 0);

      { Check for out-of-range coefficient values }
      if (nbits > MAX_COEF_BITS) then
        ERREXIT(j_common_ptr(state.cinfo), JERR_BAD_DCT_COEF);

      { Emit Huffman symbol for run length / number of bits }
      i := (r shl 4) + nbits;
      if not emit_bits(state, actbl^.ehufco[i], actbl^.ehufsi[i]) then
      begin
        encode_one_block := FALSE;
        exit;
      end;

      { Emit that number of bits of the value, if positive, }
      { or the complement of its magnitude, if negative. }
      if not emit_bits(state, uInt(temp2), nbits) then
      begin
        encode_one_block := FALSE;
        exit;
      end;

      r := 0;
    end;
  end;

  { If the last coef(s) were zero, emit an end-of-block code }
  if (r > 0) then
    if not emit_bits(state, actbl^.ehufco[0], actbl^.ehufsi[0]) then
    begin
      encode_one_block := FALSE;
      exit;
    end;

  encode_one_block := TRUE;
end;


{ Emit a restart marker & resynchronize predictions. }

{LOCAL}
function emit_restart (var state : working_state;
                       restart_num : int) : boolean;
var
  ci : int;
begin
  if (not flush_bits(state)) then
  begin
    emit_restart  := FALSE;
    exit;
  end;

  {emit_byte(state, $FF, return FALSE);}
  { Emit a byte, return FALSE if must suspend. }
  state.next_output_byte^ := JOCTET ($FF);
  Inc(state.next_output_byte);
  Dec(state.free_in_buffer);
  if (state.free_in_buffer = 0) then
    if not dump_buffer(state) then
    begin
      emit_restart := FALSE;
      exit;
    end;

  {emit_byte(state, JPEG_RST0 + restart_num, return FALSE);}
  { Emit a byte, return FALSE if must suspend. }
  state.next_output_byte^ := JOCTET (JPEG_RST0 + restart_num);
  Inc(state.next_output_byte);
  Dec(state.free_in_buffer);
  if (state.free_in_buffer = 0) then
    if not dump_buffer(state) then
    begin
      emit_restart := FALSE;
      exit;
    end;

  { Re-initialize DC predictions to 0 }
  for ci := 0 to pred(state.cinfo^.comps_in_scan) do
    state.cur.last_dc_val[ci] := 0;

  { The restart counter is not updated until we successfully write the MCU. }

  emit_restart := TRUE;
end;


{ Encode and output one MCU's worth of Huffman-compressed coefficients. }

{METHODDEF}
function encode_mcu_huff (cinfo : j_compress_ptr;
                          const MCU_data: array of JBLOCKROW) : boolean;
var
  entropy : huff_entropy_ptr;
  state : working_state;
  blkn, ci : int;
  compptr : jpeg_component_info_ptr;
begin
  entropy := huff_entropy_ptr (cinfo^.entropy);
  { Load up working state }
  state.next_output_byte := cinfo^.dest^.next_output_byte;
  state.free_in_buffer := cinfo^.dest^.free_in_buffer;
  {ASSIGN_STATE(state.cur, entropy^.saved);}
  state.cur := entropy^.saved;
  state.cinfo := cinfo;

  { Emit restart marker if needed }
  if (cinfo^.restart_interval <> 0) then
  begin
    if (entropy^.restarts_to_go = 0) then
      if not emit_restart(state, entropy^.next_restart_num) then
      begin
        encode_mcu_huff := FALSE;
        exit;
      end;
  end;

  { Encode the MCU data blocks }
  for blkn := 0  to pred(cinfo^.blocks_in_MCU) do
  begin
    ci := cinfo^.MCU_membership[blkn];
    compptr := cinfo^.cur_comp_info[ci];
    if not encode_one_block(state,
                            MCU_data[blkn]^[0],
                            state.cur.last_dc_val[ci],
                            entropy^.dc_derived_tbls[compptr^.dc_tbl_no],
                            entropy^.ac_derived_tbls[compptr^.ac_tbl_no]) then
    begin
      encode_mcu_huff := FALSE;
      exit;
    end;
    { Update last_dc_val }
    state.cur.last_dc_val[ci] := MCU_data[blkn]^[0][0];
  end;

  { Completed MCU, so update state }
  cinfo^.dest^.next_output_byte := state.next_output_byte;
  cinfo^.dest^.free_in_buffer := state.free_in_buffer;
  {ASSIGN_STATE(entropy^.saved, state.cur);}
  entropy^.saved := state.cur;

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

  encode_mcu_huff := TRUE;
end;


{ Finish up at the end of a Huffman-compressed scan. }

{METHODDEF}
procedure finish_pass_huff (cinfo : j_compress_ptr);
var
  entropy : huff_entropy_ptr;
  state : working_state;
begin
  entropy := huff_entropy_ptr (cinfo^.entropy);

  { Load up working state ... flush_bits needs it }
  state.next_output_byte := cinfo^.dest^.next_output_byte;
  state.free_in_buffer := cinfo^.dest^.free_in_buffer;
  {ASSIGN_STATE(state.cur, entropy^.saved);}
  state.cur := entropy^.saved;
  state.cinfo := cinfo;

  { Flush out the last data }
  if not flush_bits(state) then
    ERREXIT(j_common_ptr(cinfo), JERR_CANT_SUSPEND);

  { Update state }
  cinfo^.dest^.next_output_byte := state.next_output_byte;
  cinfo^.dest^.free_in_buffer := state.free_in_buffer;
  {ASSIGN_STATE(entropy^.saved, state.cur);}
  entropy^.saved := state.cur;
end;


{ Huffman coding optimization.

  We first scan the supplied data and count the number of uses of each symbol
  that is to be Huffman-coded. (This process MUST agree with the code above.)
  Then we build a Huffman coding tree for the observed counts.
  Symbols which are not needed at all for the particular image are not
  assigned any code, which saves space in the DHT marker as well as in
  the compressed data. }

{$ifdef ENTROPY_OPT_SUPPORTED}


{ Process a single block's worth of coefficients }

{LOCAL}
procedure htest_one_block (cinfo : j_compress_ptr;
                           const block : JBLOCK;
                           last_dc_val : int;
                           dc_counts : TLongTablePtr;
                           ac_counts : TLongTablePtr);

var
  {register} temp : int;
  {register} nbits : int;
  {register} k, r : int;
begin
  { Encode the DC coefficient difference per section F.1.2.1 }
  temp := block[0] - last_dc_val;
  if (temp < 0) then
    temp := -temp;

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

  { Count the Huffman symbol for the number of bits }
  Inc(dc_counts^[nbits]);

  { Encode the AC coefficients per section F.1.2.2 }

  r := 0;                       { r := run length of zeros }

  for k := 1 to pred(DCTSIZE2) do
  begin
    temp := block[jpeg_natural_order[k]];
    if (temp = 0) then
    begin
      Inc(r);
    end
    else
    begin
      { if run length > 15, must emit special run-length-16 codes ($F0) }
      while (r > 15) do
      begin
        Inc(ac_counts^[$F0]);
        Dec(r, 16);
      end;

      { Find the number of bits needed for the magnitude of the coefficient }
      if (temp < 0) then
        temp := -temp;

      { Find the number of bits needed for the magnitude of the coefficient }
      nbits := 0;               { there must be at least one 1 bit }
      repeat
        Inc(nbits);
        temp := temp shr 1;
      until (temp = 0);


      { Count Huffman symbol for run length / number of bits }
      Inc(ac_counts^[(r shl 4) + nbits]);

      r := 0;
    end;
  end;

  { If the last coef(s) were zero, emit an end-of-block code }
  if (r > 0) then
    Inc(ac_counts^[0]);
end;


{ Trial-encode one MCU's worth of Huffman-compressed coefficients.
  No data is actually output, so no suspension return is possible. }

{METHODDEF}
function encode_mcu_gather (cinfo : j_compress_ptr;
                           const MCU_data: array of JBLOCKROW) : boolean;
var
  entropy : huff_entropy_ptr;
  blkn, ci : int;
  compptr : jpeg_component_info_ptr;
begin
  entropy := huff_entropy_ptr (cinfo^.entropy);
  { Take care of restart intervals if needed }
  if (cinfo^.restart_interval <> 0) then
  begin
    if (entropy^.restarts_to_go = 0) then
    begin
      { Re-initialize DC predictions to 0 }
      for ci := 0 to pred(cinfo^.comps_in_scan) do
        entropy^.saved.last_dc_val[ci] := 0;
      { Update restart state }
      entropy^.restarts_to_go := cinfo^.restart_interval;
    end;
    Dec(entropy^.restarts_to_go);
  end;

  for blkn := 0 to pred(cinfo^.blocks_in_MCU) do
  begin
    ci := cinfo^.MCU_membership[blkn];
    compptr := cinfo^.cur_comp_info[ci];
    htest_one_block(cinfo, MCU_data[blkn]^[0],
                    entropy^.saved.last_dc_val[ci],
                    entropy^.dc_count_ptrs[compptr^.dc_tbl_no],
                    entropy^.ac_count_ptrs[compptr^.ac_tbl_no]);
    entropy^.saved.last_dc_val[ci] := MCU_data[blkn]^[0][0];
  end;

  encode_mcu_gather := TRUE;
end;


{ Generate the best Huffman code table for the given counts, fill htbl.
  Note this is also used by jcphuff.c.

  The JPEG standard requires that no symbol be assigned a codeword of all
  one bits (so that padding bits added at the end of a compressed segment
  can't look like a valid code).  Because of the canonical ordering of
  codewords, this just means that there must be an unused slot in the
  longest codeword length category.  Section K.2 of the JPEG spec suggests
  reserving such a slot by pretending that symbol 256 is a valid symbol
  with count 1.  In theory that's not optimal; giving it count zero but
  including it in the symbol set anyway should give a better Huffman code.
  But the theoretically better code actually seems to come out worse in
  practice, because it produces more all-ones bytes (which incur stuffed
  zero bytes in the final file).  In any case the difference is tiny.

  The JPEG standard requires Huffman codes to be no more than 16 bits long.
  If some symbols have a very small but nonzero probability, the Huffman tree
  must be adjusted to meet the code length restriction.  We currently use
  the adjustment method suggested in JPEG section K.2.  This method is *not*
  optimal; it may not choose the best possible limited-length code.  But
  typically only very-low-frequency symbols will be given less-than-optimal
  lengths, so the code is almost optimal.  Experimental comparisons against
  an optimal limited-length-code algorithm indicate that the difference is
  microscopic --- usually less than a hundredth of a percent of total size.
  So the extra complexity of an optimal algorithm doesn't seem worthwhile. }


{GLOBAL}
procedure jpeg_gen_optimal_table (cinfo : j_compress_ptr;
                                  htbl : JHUFF_TBL_PTR;
                                  var freq : TLongTable);
const
  MAX_CLEN = 32;                { assumed maximum initial code length }
var
  bits : array[0..MAX_CLEN+1-1] of UINT8;  { bits[k] := # of symbols with code length k }
  codesize : array[0..257-1] of int;       { codesize[k] := code length of symbol k }
  others : array[0..257-1] of int;         { next symbol in current branch of tree }
  c1, c2 : int;
  p, i, j : int;
  v : long;
begin
  { This algorithm is explained in section K.2 of the JPEG standard }

  MEMZERO(@bits, SIZEOF(bits));
  MEMZERO(@codesize, SIZEOF(codesize));
  for i := 0 to 256 do
    others[i] := -1;            { init links to empty }

  freq[256] := 1;               { make sure 256 has a nonzero count }
  { Including the pseudo-symbol 256 in the Huffman procedure guarantees
    that no real symbol is given code-value of all ones, because 256
    will be placed last in the largest codeword category. }

  { Huffman's basic algorithm to assign optimal code lengths to symbols }

  while TRUE do
  begin
    { Find the smallest nonzero frequency, set c1 := its symbol }
    { In case of ties, take the larger symbol number }
    c1 := -1;
    v := long(1000000000);
    for i := 0 to 256 do
    begin
      if (freq[i] <> 0) and (freq[i] <= v) then
      begin
        v := freq[i];
        c1 := i;
      end;
    end;

    { Find the next smallest nonzero frequency, set c2 := its symbol }
    { In case of ties, take the larger symbol number }
    c2 := -1;
    v := long(1000000000);
    for i := 0 to 256 do
    begin
      if (freq[i] <> 0) and (freq[i] <= v) and (i <> c1) then
      begin
        v := freq[i];
        c2 := i;
      end;
    end;

    { Done if we've merged everything into one frequency }
    if (c2 < 0) then
      break;

    { Else merge the two counts/trees }
    Inc(freq[c1], freq[c2]);
    freq[c2] := 0;

    { Increment the codesize of everything in c1's tree branch }
    Inc(codesize[c1]);
    while (others[c1] >= 0) do
    begin
      c1 := others[c1];
      Inc(codesize[c1]);
    end;

    others[c1] := c2;           { chain c2 onto c1's tree branch }

    { Increment the codesize of everything in c2's tree branch }
    Inc(codesize[c2]);
    while (others[c2] >= 0) do
    begin
      c2 := others[c2];
      Inc(codesize[c2]);
    end;
  end;

  { Now count the number of symbols of each code length }
  for i := 0 to 256 do
  begin
    if (codesize[i]<>0) then
    begin
      { The JPEG standard seems to think that this can't happen, }
      { but I'm paranoid... }
      if (codesize[i] > MAX_CLEN) then
        ERREXIT(j_common_ptr(cinfo), JERR_HUFF_CLEN_OVERFLOW);

      Inc(bits[codesize[i]]);
    end;
  end;

  { JPEG doesn't allow symbols with code lengths over 16 bits, so if the pure
    Huffman procedure assigned any such lengths, we must adjust the coding.
    Here is what the JPEG spec says about how this next bit works:
    Since symbols are paired for the longest Huffman code, the symbols are
    removed from this length category two at a time.  The prefix for the pair
    (which is one bit shorter) is allocated to one of the pair; then,
    skipping the BITS entry for that prefix length, a code word from the next
    shortest nonzero BITS entry is converted into a prefix for two code words
    one bit longer.  }

  for i := MAX_CLEN downto 17 do
  begin
    while (bits[i] > 0) do
    begin
      j := i - 2;               { find length of new prefix to be used }
      while (bits[j] = 0) do
        Dec(j);

      Dec(bits[i], 2);          { remove two symbols }
      Inc(bits[i-1]);           { one goes in this length }
      Inc(bits[j+1], 2);        { two new symbols in this length }
      Dec(bits[j]);             { symbol of this length is now a prefix }
    end;
  end;

  { Delphi 2: FOR-loop variable 'i' may be undefined after loop }
  i := 16;                      { Nomssi: work around }

  { Remove the count for the pseudo-symbol 256 from the largest codelength }
  while (bits[i] = 0) do        { find largest codelength still in use }
    Dec(i);
  Dec(bits[i]);

  { Return final symbol counts (only for lengths 0..16) }
  MEMCOPY(@htbl^.bits, @bits, SIZEOF(htbl^.bits));

  { Return a list of the symbols sorted by code length }
  { It's not real clear to me why we don't need to consider the codelength
    changes made above, but the JPEG spec seems to think this works. }

  p := 0;
  for i := 1 to MAX_CLEN do
  begin
    for j := 0 to 255 do
    begin
      if (codesize[j] = i) then
      begin
        htbl^.huffval[p] := UINT8 (j);
        Inc(p);
      end;
    end;
  end;

  { Set sent_table FALSE so updated table will be written to JPEG file. }
  htbl^.sent_table := FALSE;
end;


{ Finish up a statistics-gathering pass and create the new Huffman tables. }

{METHODDEF}
procedure finish_pass_gather (cinfo : j_compress_ptr);
var
  entropy : huff_entropy_ptr;
  ci, dctbl, actbl : int;
  compptr : jpeg_component_info_ptr;
  htblptr : ^JHUFF_TBL_PTR;
  did_dc : array[0..NUM_HUFF_TBLS-1] of boolean;
  did_ac : array[0..NUM_HUFF_TBLS-1] of boolean;
begin
  entropy := huff_entropy_ptr (cinfo^.entropy);

  { It's important not to apply jpeg_gen_optimal_table more than once
    per table, because it clobbers the input frequency counts! }

  MEMZERO(@did_dc, SIZEOF(did_dc));
  MEMZERO(@did_ac, SIZEOF(did_ac));

  for ci := 0 to pred(cinfo^.comps_in_scan) do
  begin
    compptr := cinfo^.cur_comp_info[ci];
    dctbl := compptr^.dc_tbl_no;
    actbl := compptr^.ac_tbl_no;
    if (not did_dc[dctbl]) then
    begin
      htblptr := @(cinfo^.dc_huff_tbl_ptrs[dctbl]);
      if ( htblptr^ = NIL) then
        htblptr^ := jpeg_alloc_huff_table(j_common_ptr(cinfo));
      jpeg_gen_optimal_table(cinfo, htblptr^, entropy^.dc_count_ptrs[dctbl]^);
      did_dc[dctbl] := TRUE;
    end;
    if (not did_ac[actbl]) then
    begin
      htblptr := @(cinfo^.ac_huff_tbl_ptrs[actbl]);
      if ( htblptr^ = NIL) then
        htblptr^ := jpeg_alloc_huff_table(j_common_ptr(cinfo));
      jpeg_gen_optimal_table(cinfo, htblptr^, entropy^.ac_count_ptrs[actbl]^);
      did_ac[actbl] := TRUE;
    end;
  end;
end;

{$endif} { ENTROPY_OPT_SUPPORTED }


{ Module initialization routine for Huffman entropy encoding. }

{GLOBAL}
procedure jinit_huff_encoder (cinfo : j_compress_ptr);
var
  entropy : huff_entropy_ptr;
  i : int;
begin
  entropy := huff_entropy_ptr(
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                                SIZEOF(huff_entropy_encoder)) );
  cinfo^.entropy := jpeg_entropy_encoder_ptr (entropy);
  entropy^.pub.start_pass := start_pass_huff;

  { Mark tables unallocated }
  for i := 0 to pred(NUM_HUFF_TBLS) do
  begin
    entropy^.ac_derived_tbls[i] := NIL;
    entropy^.dc_derived_tbls[i] := NIL;
{$ifdef ENTROPY_OPT_SUPPORTED}
    entropy^.ac_count_ptrs[i] := NIL;
    entropy^.dc_count_ptrs[i] := NIL;
{$endif}
  end;
end;

end.
