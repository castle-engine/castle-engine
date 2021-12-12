unit imjdphuff;

{ This file contains Huffman entropy decoding routines for progressive JPEG.

  Much of the complexity here has to do with supporting input suspension.
  If the data source module demands suspension, we want to be able to back
  up to the start of the current MCU.  To do this, we copy state variables
  into local working storage, and update them back to the permanent
  storage only upon successful completion of an MCU. }

{ Original: jdphuff.c ; Copyright (C) 1995-1997, Thomas G. Lane. }

interface

{$I imjconfig.inc}

uses
  imjmorecfg,
  imjinclude,
  imjpeglib,
  imjdeferr,
  imjerror,
  imjutils,
  imjdhuff;		{ Declarations shared with jdhuff.c }


{GLOBAL}
procedure jinit_phuff_decoder (cinfo : j_decompress_ptr);

implementation

{ Expanded entropy decoder object for progressive Huffman decoding.

  The savable_state subrecord contains fields that change within an MCU,
  but must not be updated permanently until we complete the MCU. }

type
  savable_state = record
    EOBRUN : uInt;               { remaining EOBs in EOBRUN }
    last_dc_val : array[00..MAX_COMPS_IN_SCAN-1] of int;
                                 { last DC coef for each component }
  end;


type
  phuff_entropy_ptr  = ^phuff_entropy_decoder;
  phuff_entropy_decoder = record
    pub : jpeg_entropy_decoder; { public fields }

    { These fields are loaded into local variables at start of each MCU.
      In case of suspension, we exit WITHOUT updating them. }

    bitstate : bitread_perm_state;	{ Bit buffer at start of MCU }
    saved : savable_state;		{ Other state at start of MCU }

    { These fields are NOT loaded into local working state. }
    restarts_to_go : uInt;              { MCUs left in this restart interval }

    { Pointers to derived tables (these workspaces have image lifespan) }
    derived_tbls : array[0..NUM_HUFF_TBLS-1] of d_derived_tbl_ptr;

    ac_derived_tbl : d_derived_tbl_ptr; { active table during an AC scan }
  end;



{ Forward declarations }
{METHODDEF}
function decode_mcu_DC_first (cinfo : j_decompress_ptr;
                              var MCU_data : array of JBLOCKROW) : boolean;
                              forward;
{METHODDEF}
function decode_mcu_AC_first (cinfo : j_decompress_ptr;
                              var MCU_data : array of JBLOCKROW) : boolean;
                              forward;
{METHODDEF}
function decode_mcu_DC_refine (cinfo : j_decompress_ptr;
                               var MCU_data : array of JBLOCKROW) : boolean;
                               forward;
{METHODDEF}
function decode_mcu_AC_refine (cinfo : j_decompress_ptr;
                               var MCU_data : array of JBLOCKROW) : boolean;
                               forward;

{ Initialize for a Huffman-compressed scan. }

{METHODDEF}
procedure start_pass_phuff_decoder (cinfo : j_decompress_ptr);  
var
  entropy : phuff_entropy_ptr;
  is_DC_band, bad : boolean;
  ci, coefi, tbl : int;
  coef_bit_ptr : coef_bits_ptr;
  compptr : jpeg_component_info_ptr;
var
  cindex : int;
  expected : int;
begin
  entropy := phuff_entropy_ptr (cinfo^.entropy);

  is_DC_band := (cinfo^.Ss = 0);

  { Validate scan parameters }
  bad := FALSE;
  if (is_DC_band) then
  begin
    if (cinfo^.Se <> 0) then
      bad := TRUE;
  end
  else
  begin
    { need not check Ss/Se < 0 since they came from unsigned bytes }
    if (cinfo^.Ss > cinfo^.Se) or (cinfo^.Se >= DCTSIZE2) then
      bad := TRUE;
    { AC scans may have only one component }
    if (cinfo^.comps_in_scan <> 1) then
      bad := TRUE;
  end;
  if (cinfo^.Ah <> 0) then
  begin
    { Successive approximation refinement scan: must have Al = Ah-1. }
    if (cinfo^.Al <> cinfo^.Ah-1) then
      bad := TRUE;
  end;
  if (cinfo^.Al > 13) then      { need not check for < 0 }
    bad := TRUE;
  { Arguably the maximum Al value should be less than 13 for 8-bit precision,
    but the spec doesn't say so, and we try to be liberal about what we
    accept.  Note: large Al values could result in out-of-range DC
    coefficients during early scans, leading to bizarre displays due to
    overflows in the IDCT math.  But we won't crash. }

  if (bad) then
    ERREXIT4(j_common_ptr(cinfo), JERR_BAD_PROGRESSION,
	     cinfo^.Ss, cinfo^.Se, cinfo^.Ah, cinfo^.Al);
  { Update progression status, and verify that scan order is legal.
    Note that inter-scan inconsistencies are treated as warnings
    not fatal errors ... not clear if this is right way to behave. }

  for ci := 0 to pred(cinfo^.comps_in_scan) do
  begin
    cindex := cinfo^.cur_comp_info[ci]^.component_index;
    coef_bit_ptr := coef_bits_ptr(@(cinfo^.coef_bits^[cindex])); {^[0] ???
                                                                   Nomssi    }
    if (not is_DC_band) and (coef_bit_ptr^[0] < 0) then
      { AC without prior DC scan }
      WARNMS2(j_common_ptr(cinfo), JWRN_BOGUS_PROGRESSION, cindex, 0);
    for coefi := cinfo^.Ss to cinfo^.Se do
    begin
      if (coef_bit_ptr^[coefi] < 0) then
        expected :=  0
      else
        expected := coef_bit_ptr^[coefi];
      if (cinfo^.Ah <> expected) then
	WARNMS2(j_common_ptr(cinfo), JWRN_BOGUS_PROGRESSION, cindex, coefi);
      coef_bit_ptr^[coefi] := cinfo^.Al;
    end;
  end;

  { Select MCU decoding routine }
  if (cinfo^.Ah = 0) then
  begin
    if (is_DC_band) then
      entropy^.pub.decode_mcu := decode_mcu_DC_first
    else
      entropy^.pub.decode_mcu := decode_mcu_AC_first;
  end
  else
  begin
    if (is_DC_band) then
      entropy^.pub.decode_mcu := decode_mcu_DC_refine
    else
      entropy^.pub.decode_mcu := decode_mcu_AC_refine;
  end;

  for ci := 0 to pred(cinfo^.comps_in_scan) do
  begin
    compptr := cinfo^.cur_comp_info[ci];
    { Make sure requested tables are present, and compute derived tables.
      We may build same derived table more than once, but it's not expensive. }

    if (is_DC_band) then
    begin
      if (cinfo^.Ah = 0) then
      begin	{ DC refinement needs no table }
	tbl := compptr^.dc_tbl_no;
	jpeg_make_d_derived_tbl(cinfo, TRUE, tbl,
				 entropy^.derived_tbls[tbl]);
      end;
    end
    else
    begin
      tbl := compptr^.ac_tbl_no;
      jpeg_make_d_derived_tbl(cinfo, FALSE, tbl,
			       entropy^.derived_tbls[tbl]);
      { remember the single active table }
      entropy^.ac_derived_tbl := entropy^.derived_tbls[tbl];
    end;
    { Initialize DC predictions to 0 }
    entropy^.saved.last_dc_val[ci] := 0;
  end;

  { Initialize bitread state variables }
  entropy^.bitstate.bits_left := 0;
  entropy^.bitstate.get_buffer := 0; { unnecessary, but keeps Purify quiet }
  entropy^.pub.insufficient_data := FALSE;

  { Initialize private state variables }
  entropy^.saved.EOBRUN := 0;

  { Initialize restart counter }
  entropy^.restarts_to_go := cinfo^.restart_interval;
end;


{ Figure F.12: extend sign bit.
  On some machines, a shift and add will be faster than a table lookup. }

{$ifdef AVOID_TABLES}

#define HUFF_EXTEND(x,s)
  ((x) < (1shl((s)-1)) ? (x) + (((-1)shl(s)) + 1) : (x))

{$else}

{ #define HUFF_EXTEND(x,s)
  if (x) < extend_test[s] then
    (x) + extend_offset[s]
  else
    (x)}

const
 extend_test : Array[0..16-1] of int =   { entry n is 2**(n-1) }
   ($0000, $0001, $0002, $0004, $0008, $0010, $0020, $0040,
    $0080, $0100, $0200, $0400, $0800, $1000, $2000, $4000);

const
  extend_offset : array[0..16-1] of int = { entry n is (-1 shl n) + 1 }
  ( 0, ((-1) shl 1) + 1, ((-1) shl 2) + 1, ((-1) shl 3) + 1, ((-1) shl 4) + 1,
    ((-1) shl 5) + 1, ((-1) shl 6) + 1, ((-1) shl 7) + 1, ((-1) shl 8) + 1,
    ((-1) shl 9) + 1, ((-1) shl 10) + 1, ((-1) shl 11) + 1, ((-1) shl 12) + 1,
    ((-1) shl 13) + 1, ((-1) shl 14) + 1, ((-1) shl 15) + 1 );

{$endif} { AVOID_TABLES }


{ Check for a restart marker & resynchronize decoder.
  return:=s FALSE if must suspend. }

{LOCAL}
function process_restart (cinfo : j_decompress_ptr) : boolean;
var
  entropy : phuff_entropy_ptr; 
  ci : int;
begin
  entropy := phuff_entropy_ptr (cinfo^.entropy);

  { Throw away any unused bits remaining in bit buffer; }
  { include any full bytes in next_marker's count of discarded bytes }
  Inc(cinfo^.marker^.discarded_bytes, entropy^.bitstate.bits_left div 8);
  entropy^.bitstate.bits_left := 0;

  { Advance past the RSTn marker }
  if (not cinfo^.marker^.read_restart_marker (cinfo)) then
  begin
    process_restart := FALSE;
    exit;
  end;

  { Re-initialize DC predictions to 0 }
  for ci := 0 to pred(cinfo^.comps_in_scan) do
    entropy^.saved.last_dc_val[ci] := 0;
  { Re-init EOB run count, too }
  entropy^.saved.EOBRUN := 0;

  { Reset restart counter }
  entropy^.restarts_to_go := cinfo^.restart_interval;

  { Reset out-of-data flag, unless read_restart_marker left us smack up
    against a marker.  In that case we will end up treating the next data
    segment as empty, and we can avoid producing bogus output pixels by
    leaving the flag set. }
  if (cinfo^.unread_marker = 0) then
    entropy^.pub.insufficient_data := FALSE;

  process_restart := TRUE;
end;


{ Huffman MCU decoding.
  Each of these routines decodes and returns one MCU's worth of
  Huffman-compressed coefficients.
  The coefficients are reordered from zigzag order into natural array order,
  but are not dequantized.

  The i'th block of the MCU is stored into the block pointed to by
  MCU_data[i].  WE ASSUME THIS AREA IS INITIALLY ZEROED BY THE CALLER.

  We return FALSE if data source requested suspension.  In that case no
  changes have been made to permanent state.  (Exception: some output
  coefficients may already have been assigned.  This is harmless for
  spectral selection, since we'll just re-assign them on the next call.
  Successive approximation AC refinement has to be more careful, however.) }


{ MCU decoding for DC initial scan (either spectral selection,
  or first pass of successive approximation). }

{METHODDEF}
function decode_mcu_DC_first (cinfo : j_decompress_ptr;
                              var MCU_data : array of JBLOCKROW) : boolean;
label
  label1;
var
  entropy : phuff_entropy_ptr;
  Al : int;
  {register} s, r : int;
  blkn, ci : int;
  block : JBLOCK_PTR;
  {BITREAD_STATE_VARS;}
  get_buffer : bit_buf_type ; {register}
  bits_left : int; {register}
  br_state : bitread_working_state;

  state : savable_state;
  tbl : d_derived_tbl_ptr;
  compptr : jpeg_component_info_ptr;
var
  nb, look : int; {register}
begin
  entropy := phuff_entropy_ptr (cinfo^.entropy);
  Al := cinfo^.Al;

  { Process restart marker if needed; may have to suspend }
  if (cinfo^.restart_interval <> 0) then
  begin
    if (entropy^.restarts_to_go = 0) then
      if (not process_restart(cinfo)) then
      begin
	decode_mcu_DC_first := FALSE;
        exit;
      end;
  end;

  { If we've run out of data, just leave the MCU set to zeroes.
    This way, we return uniform gray for the remainder of the segment. }

  if not entropy^.pub.insufficient_data then
  begin

    { Load up working state }
    {BITREAD_LOAD_STATE(cinfo,entropy^.bitstate);}
    br_state.cinfo := cinfo;
    br_state.next_input_byte := cinfo^.src^.next_input_byte;
    br_state.bytes_in_buffer := cinfo^.src^.bytes_in_buffer;
    get_buffer := entropy^.bitstate.get_buffer;
    bits_left := entropy^.bitstate.bits_left;

    {ASSIGN_STATE(state, entropy^.saved);}
    state := entropy^.saved;

    { Outer loop handles each block in the MCU }

    for blkn := 0 to pred(cinfo^.blocks_in_MCU) do
    begin
      block := JBLOCK_PTR(MCU_data[blkn]);
      ci := cinfo^.MCU_membership[blkn];
      compptr := cinfo^.cur_comp_info[ci];
      tbl := entropy^.derived_tbls[compptr^.dc_tbl_no];

      { Decode a single block's worth of coefficients }

      { Section F.2.2.1: decode the DC coefficient difference }
      {HUFF_DECODE(s, br_state, tbl, return FALSE, label1);}
      if (bits_left < HUFF_LOOKAHEAD) then
      begin
        if (not jpeg_fill_bit_buffer(br_state,get_buffer,bits_left, 0)) then
        begin
          decode_mcu_DC_first := FALSE;
          exit;
        end;
        get_buffer := br_state.get_buffer;
        bits_left := br_state.bits_left;
        if (bits_left < HUFF_LOOKAHEAD) then
        begin
          nb := 1;
          goto label1;
        end;
      end;
      {look := PEEK_BITS(HUFF_LOOKAHEAD);}
      look := int(get_buffer shr (bits_left -  HUFF_LOOKAHEAD)) and
                     pred(1 shl HUFF_LOOKAHEAD);

      nb := tbl^.look_nbits[look];
      if (nb <> 0) then
      begin
        {DROP_BITS(nb);}
        Dec(bits_left, nb);

        s := tbl^.look_sym[look];
      end
      else
      begin
        nb := HUFF_LOOKAHEAD+1;
    label1:
        s := jpeg_huff_decode(br_state,get_buffer,bits_left,tbl,nb);
        if (s < 0) then
        begin
          decode_mcu_DC_first := FALSE;
          exit;
        end;
        get_buffer := br_state.get_buffer;
        bits_left := br_state.bits_left;
      end;

      if (s <> 0) then
      begin
        {CHECK_BIT_BUFFER(br_state, s, return FALSE);}
        if (bits_left < s) then
        begin
          if (not jpeg_fill_bit_buffer(br_state,get_buffer,bits_left,s)) then
          begin
            decode_mcu_DC_first := FALSE;
            exit;
          end;
          get_buffer := br_state.get_buffer;
          bits_left := br_state.bits_left;
        end;

        {r := GET_BITS(s);}
        Dec(bits_left, s);
        r := (int(get_buffer shr bits_left)) and ( pred(1 shl s) );

        {s := HUFF_EXTEND(r, s);}
        if (r < extend_test[s]) then
          s := r + extend_offset[s]
        else
          s := r;
      end;

      { Convert DC difference to actual value, update last_dc_val }
      Inc(s, state.last_dc_val[ci]);
      state.last_dc_val[ci] := s;
      { Scale and output the DC coefficient (assumes jpeg_natural_order[0]=0) }
      block^[0] := JCOEF (s shl Al);
    end;

    { Completed MCU, so update state }
    {BITREAD_SAVE_STATE(cinfo,entropy^.bitstate);}
    cinfo^.src^.next_input_byte := br_state.next_input_byte;
    cinfo^.src^.bytes_in_buffer := br_state.bytes_in_buffer;
    entropy^.bitstate.get_buffer := get_buffer;
    entropy^.bitstate.bits_left := bits_left;

    {ASSIGN_STATE(entropy^.saved, state);}
    entropy^.saved := state;
  end;

  { Account for restart interval (no-op if not using restarts) }
  Dec(entropy^.restarts_to_go);

  decode_mcu_DC_first := TRUE;
end;


{ MCU decoding for AC initial scan (either spectral selection,
  or first pass of successive approximation). }

{METHODDEF}
function decode_mcu_AC_first (cinfo : j_decompress_ptr;
                              var MCU_data : array of JBLOCKROW) : boolean;
label
  label2;
var
  entropy : phuff_entropy_ptr;
  Se : int;
  Al : int;
  {register} s, k, r : int;
  EOBRUN : uInt;
  block : JBLOCK_PTR;
  {BITREAD_STATE_VARS;}
  get_buffer : bit_buf_type ; {register}
  bits_left : int; {register}
  br_state : bitread_working_state;

  tbl : d_derived_tbl_ptr;
var
  nb, look : int; {register}
begin
  entropy := phuff_entropy_ptr (cinfo^.entropy);
  Se := cinfo^.Se;
  Al := cinfo^.Al;

  { Process restart marker if needed; may have to suspend }
  if (cinfo^.restart_interval <> 0) then
  begin
    if (entropy^.restarts_to_go = 0) then
      if (not process_restart(cinfo)) then
      begin
	decode_mcu_AC_first := FALSE;
        exit;
      end;
  end;

  { If we've run out of data, just leave the MCU set to zeroes.
    This way, we return uniform gray for the remainder of the segment. }
  if not entropy^.pub.insufficient_data then
  begin

    { Load up working state.
      We can avoid loading/saving bitread state if in an EOB run. }

    EOBRUN := entropy^.saved.EOBRUN; { only part of saved state we care about }

    { There is always only one block per MCU }

    if (EOBRUN > 0) then       { if it's a band of zeroes... }
      Dec(EOBRUN)              { ...process it now (we do nothing) }
    else
    begin
      {BITREAD_LOAD_STATE(cinfo,entropy^.bitstate);}
      br_state.cinfo := cinfo;
      br_state.next_input_byte := cinfo^.src^.next_input_byte;
      br_state.bytes_in_buffer := cinfo^.src^.bytes_in_buffer;
      get_buffer := entropy^.bitstate.get_buffer;
      bits_left := entropy^.bitstate.bits_left;

      block := JBLOCK_PTR(MCU_data[0]);
      tbl := entropy^.ac_derived_tbl;

      k := cinfo^.Ss;
      while (k <= Se) do
      begin
        {HUFF_DECODE(s, br_state, tbl, return FALSE, label2);}
        if (bits_left < HUFF_LOOKAHEAD) then
        begin
          if (not jpeg_fill_bit_buffer(br_state,get_buffer,bits_left, 0)) then
          begin
            decode_mcu_AC_first := FALSE;
            exit;
          end;
          get_buffer := br_state.get_buffer;
          bits_left := br_state.bits_left;
          if (bits_left < HUFF_LOOKAHEAD) then
          begin
            nb := 1;
            goto label2;
          end;
        end;
        {look := PEEK_BITS(HUFF_LOOKAHEAD);}
        look := int(get_buffer shr (bits_left -  HUFF_LOOKAHEAD)) and
                       pred(1 shl HUFF_LOOKAHEAD);

        nb := tbl^.look_nbits[look];
        if (nb <> 0) then
        begin
          {DROP_BITS(nb);}
          Dec(bits_left, nb);

          s := tbl^.look_sym[look];
        end
        else
        begin
          nb := HUFF_LOOKAHEAD+1;
      label2:
          s := jpeg_huff_decode(br_state,get_buffer,bits_left,tbl,nb);
          if (s < 0) then
          begin
            decode_mcu_AC_first := FALSE;
            exit;
          end;
          get_buffer := br_state.get_buffer;
          bits_left := br_state.bits_left;
        end;

        r := s shr 4;
        s := s and 15;
        if (s <> 0) then
        begin
          Inc(k, r);
          {CHECK_BIT_BUFFER(br_state, s, return FALSE);}
          if (bits_left < s) then
          begin
            if (not jpeg_fill_bit_buffer(br_state,get_buffer,bits_left,s)) then
            begin
              decode_mcu_AC_first := FALSE;
              exit;
            end;
            get_buffer := br_state.get_buffer;
            bits_left := br_state.bits_left;
          end;

          {r := GET_BITS(s);}
          Dec(bits_left, s);
          r := (int(get_buffer shr bits_left)) and ( pred(1 shl s) );

          {s := HUFF_EXTEND(r, s);}
          if (r < extend_test[s]) then
            s := r + extend_offset[s]
          else
            s := r;

	  { Scale and output coefficient in natural (dezigzagged) order }
          block^[jpeg_natural_order[k]] := JCOEF (s shl Al);
        end
        else
        begin
          if (r = 15) then
          begin		{ ZRL }
            Inc(k, 15);	{ skip 15 zeroes in band }
          end
          else
          begin		{ EOBr, run length is 2^r + appended bits }
            EOBRUN := 1 shl r;
            if (r <> 0) then
            begin		{ EOBr, r > 0 }
	      {CHECK_BIT_BUFFER(br_state, r, return FALSE);}
              if (bits_left < r) then
              begin
                if (not jpeg_fill_bit_buffer(br_state,get_buffer,bits_left,r)) then
                begin
                  decode_mcu_AC_first := FALSE;
                  exit;
                end;
                get_buffer := br_state.get_buffer;
                bits_left := br_state.bits_left;
              end;

              {r := GET_BITS(r);}
              Dec(bits_left, r);
              r := (int(get_buffer shr bits_left)) and ( pred(1 shl r) );

              Inc(EOBRUN, r);
            end;
	    Dec(EOBRUN);          { this band is processed at this moment }
	    break;                { force end-of-band }
	  end;
        end;
        Inc(k);
      end;

      {BITREAD_SAVE_STATE(cinfo,entropy^.bitstate);}
      cinfo^.src^.next_input_byte := br_state.next_input_byte;
      cinfo^.src^.bytes_in_buffer := br_state.bytes_in_buffer;
      entropy^.bitstate.get_buffer := get_buffer;
      entropy^.bitstate.bits_left := bits_left;
    end;

    { Completed MCU, so update state }
    entropy^.saved.EOBRUN := EOBRUN; { only part of saved state we care about }
  end;

  { Account for restart interval (no-op if not using restarts) }
  Dec(entropy^.restarts_to_go);

  decode_mcu_AC_first := TRUE;
end;


{ MCU decoding for DC successive approximation refinement scan.
  Note: we assume such scans can be multi-component, although the spec
  is not very clear on the point. }

{METHODDEF}
function decode_mcu_DC_refine (cinfo : j_decompress_ptr;
                               var MCU_data : array of JBLOCKROW) : boolean;

var
  entropy : phuff_entropy_ptr;
  p1 : int;          { 1 in the bit position being coded }
  blkn : int;
  block : JBLOCK_PTR;
  {BITREAD_STATE_VARS;}
  get_buffer : bit_buf_type ; {register}
  bits_left : int; {register}
  br_state : bitread_working_state;
begin
  entropy := phuff_entropy_ptr (cinfo^.entropy);
  p1 := 1 shl cinfo^.Al;

  { Process restart marker if needed; may have to suspend }
  if (cinfo^.restart_interval <> 0) then
  begin
    if (entropy^.restarts_to_go = 0) then
      if (not process_restart(cinfo)) then
      begin
	decode_mcu_DC_refine := FALSE;
        exit;
      end;
  end;

  { Not worth the cycles to check insufficient_data here,
    since we will not change the data anyway if we read zeroes. }

  { Load up working state }
  {BITREAD_LOAD_STATE(cinfo,entropy^.bitstate);}
  br_state.cinfo := cinfo;
  br_state.next_input_byte := cinfo^.src^.next_input_byte;
  br_state.bytes_in_buffer := cinfo^.src^.bytes_in_buffer;
  get_buffer := entropy^.bitstate.get_buffer;
  bits_left := entropy^.bitstate.bits_left;

  { Outer loop handles each block in the MCU }

  for blkn := 0 to pred(cinfo^.blocks_in_MCU) do
  begin
    block := JBLOCK_PTR(MCU_data[blkn]);

    { Encoded data is simply the next bit of the two's-complement DC value }
    {CHECK_BIT_BUFFER(br_state, 1, return FALSE);}
    if (bits_left < 1) then
    begin
      if (not jpeg_fill_bit_buffer(br_state,get_buffer,bits_left,1)) then
      begin
        decode_mcu_DC_refine := FALSE;
        exit;
      end;
      get_buffer := br_state.get_buffer;
      bits_left := br_state.bits_left;
    end;

    {if (GET_BITS(1)) then}
    Dec(bits_left);
    if (int(get_buffer shr bits_left)) and ( pred(1 shl 1) ) <> 0 then
      block^[0] := block^[0] or p1;
    { Note: since we use OR, repeating the assignment later is safe }
  end;

  { Completed MCU, so update state }
  {BITREAD_SAVE_STATE(cinfo,entropy^.bitstate);}
  cinfo^.src^.next_input_byte := br_state.next_input_byte;
  cinfo^.src^.bytes_in_buffer := br_state.bytes_in_buffer;
  entropy^.bitstate.get_buffer := get_buffer;
  entropy^.bitstate.bits_left := bits_left;

  { Account for restart interval (no-op if not using restarts) }
  Dec(entropy^.restarts_to_go);

  decode_mcu_DC_refine := TRUE;
end;


{ MCU decoding for AC successive approximation refinement scan. }

{METHODDEF}
function decode_mcu_AC_refine (cinfo : j_decompress_ptr;
                               var MCU_data : array of JBLOCKROW) : boolean;
label
  undoit, label3;
var
  entropy : phuff_entropy_ptr;
  Se : int;
  p1 : int;     { 1 in the bit position being coded }
  m1 : int;     { -1 in the bit position being coded }
  {register} s, k, r : int;
  EOBRUN : uInt;
  block : JBLOCK_PTR;
  thiscoef : JCOEF_PTR;
  {BITREAD_STATE_VARS;}
  get_buffer : bit_buf_type ; {register}
  bits_left : int; {register}
  br_state : bitread_working_state;

  tbl : d_derived_tbl_ptr;
  num_newnz : int;
  newnz_pos : array[0..DCTSIZE2-1] of int;
var
  pos : int;
var
  nb, look : int; {register}
begin
  num_newnz := 0;
  block := nil;

  entropy := phuff_entropy_ptr (cinfo^.entropy);
  Se := cinfo^.Se;
  p1 := 1 shl cinfo^.Al;	{ 1 in the bit position being coded }
  m1 := (-1) shl cinfo^.Al;	{ -1 in the bit position being coded }

  { Process restart marker if needed; may have to suspend }
  if (cinfo^.restart_interval <> 0) then
  begin
    if (entropy^.restarts_to_go = 0) then
      if (not process_restart(cinfo)) then
      begin
	decode_mcu_AC_refine := FALSE;
        exit;
      end;
  end;

  { If we've run out of data, don't modify the MCU. }
  if not entropy^.pub.insufficient_data then
  begin

    { Load up working state }
    {BITREAD_LOAD_STATE(cinfo,entropy^.bitstate);}
    br_state.cinfo := cinfo;
    br_state.next_input_byte := cinfo^.src^.next_input_byte;
    br_state.bytes_in_buffer := cinfo^.src^.bytes_in_buffer;
    get_buffer := entropy^.bitstate.get_buffer;
    bits_left := entropy^.bitstate.bits_left;

    EOBRUN := entropy^.saved.EOBRUN; { only part of saved state we care about }

    { There is always only one block per MCU }
    block := JBLOCK_PTR(MCU_data[0]);
    tbl := entropy^.ac_derived_tbl;

    { If we are forced to suspend, we must undo the assignments to any newly
      nonzero coefficients in the block, because otherwise we'd get confused
      next time about which coefficients were already nonzero.
      But we need not undo addition of bits to already-nonzero coefficients;
      instead, we can test the current bit position to see if we already did it.}

    num_newnz := 0;

    { initialize coefficient loop counter to start of band }
    k := cinfo^.Ss;

    if (EOBRUN = 0) then
    begin
      while (k <= Se) do
      begin
        {HUFF_DECODE(s, br_state, tbl, goto undoit, label3);}
        if (bits_left < HUFF_LOOKAHEAD) then
        begin
          if (not jpeg_fill_bit_buffer(br_state,get_buffer,bits_left, 0)) then
            goto undoit;
          get_buffer := br_state.get_buffer;
          bits_left := br_state.bits_left;
          if (bits_left < HUFF_LOOKAHEAD) then
          begin
            nb := 1;
            goto label3;
          end;
        end;
        {look := PEEK_BITS(HUFF_LOOKAHEAD);}
        look := int(get_buffer shr (bits_left -  HUFF_LOOKAHEAD)) and
                       pred(1 shl HUFF_LOOKAHEAD);

        nb := tbl^.look_nbits[look];
        if (nb <> 0) then
        begin
          {DROP_BITS(nb);}
          Dec(bits_left, nb);

          s := tbl^.look_sym[look];
        end
        else
        begin
          nb := HUFF_LOOKAHEAD+1;
      label3:
          s := jpeg_huff_decode(br_state,get_buffer,bits_left,tbl,nb);
          if (s < 0) then
            goto undoit;
          get_buffer := br_state.get_buffer;
          bits_left := br_state.bits_left;
        end;

        r := s shr 4;
        s := s and 15;
        if (s <> 0) then
        begin
	  if (s <> 1) then	{ size of new coef should always be 1 }
	    WARNMS(j_common_ptr(cinfo), JWRN_HUFF_BAD_CODE);
          {CHECK_BIT_BUFFER(br_state, 1, goto undoit);}
          if (bits_left < 1) then
          begin
            if (not jpeg_fill_bit_buffer(br_state,get_buffer,bits_left,1)) then
              goto undoit;
            get_buffer := br_state.get_buffer;
            bits_left := br_state.bits_left;
          end;

          {if (GET_BITS(1)) then}
          Dec(bits_left);
          if (int(get_buffer shr bits_left)) and ( pred(1 shl 1) )<>0 then
	    s := p1		{ newly nonzero coef is positive }
	  else
	    s := m1;		{ newly nonzero coef is negative }
        end
        else
        begin
	  if (r <> 15) then
          begin
	    EOBRUN := 1 shl r;	{ EOBr, run length is 2^r + appended bits }
	    if (r <> 0) then
            begin
	      {CHECK_BIT_BUFFER(br_state, r, goto undoit);}
              if (bits_left < r) then
              begin
                if (not jpeg_fill_bit_buffer(br_state,get_buffer,bits_left,r)) then
                  goto undoit;
                get_buffer := br_state.get_buffer;
                bits_left := br_state.bits_left;
              end;

	      {r := GET_BITS(r);}
              Dec(bits_left, r);
              r := (int(get_buffer shr bits_left)) and ( pred(1 shl r) );

	      Inc(EOBRUN, r);
	    end;
	    break;		{ rest of block is handled by EOB logic }
	  end;
	  { note s := 0 for processing ZRL }
        end;
        { Advance over already-nonzero coefs and r still-zero coefs,
          appending correction bits to the nonzeroes.  A correction bit is 1
          if the absolute value of the coefficient must be increased. }

        repeat
	  thiscoef :=@(block^[jpeg_natural_order[k]]);
	  if (thiscoef^ <> 0) then
          begin
	    {CHECK_BIT_BUFFER(br_state, 1, goto undoit);}
            if (bits_left < 1) then
            begin
              if (not jpeg_fill_bit_buffer(br_state,get_buffer,bits_left,1)) then
                goto undoit;
              get_buffer := br_state.get_buffer;
              bits_left := br_state.bits_left;
            end;

	    {if (GET_BITS(1)) then}
            Dec(bits_left);
            if (int(get_buffer shr bits_left)) and ( pred(1 shl 1) )<>0 then
            begin
	      if ((thiscoef^ and p1) = 0) then
              begin { do nothing if already set it }
	        if (thiscoef^ >= 0) then
		  Inc(thiscoef^, p1)
	        else
		  Inc(thiscoef^, m1);
	      end;
	    end;
	  end
          else
          begin
            Dec(r);
	    if (r < 0) then
	      break;		{ reached target zero coefficient }
	  end;
	  Inc(k);
        until (k > Se);
        if (s <> 0) then
        begin
	  pos := jpeg_natural_order[k];
	  { Output newly nonzero coefficient }
	  block^[pos] := JCOEF (s);
	  { Remember its position in case we have to suspend }
	  newnz_pos[num_newnz] := pos;
          Inc(num_newnz);
        end;
        Inc(k);
      end;
    end;

    if (EOBRUN > 0) then
    begin
      { Scan any remaining coefficient positions after the end-of-band
        (the last newly nonzero coefficient, if any).  Append a correction
        bit to each already-nonzero coefficient.  A correction bit is 1
        if the absolute value of the coefficient must be increased. }

      while (k <= Se) do
      begin
        thiscoef := @(block^[jpeg_natural_order[k]]);
        if (thiscoef^ <> 0) then
        begin
	  {CHECK_BIT_BUFFER(br_state, 1, goto undoit);}
          if (bits_left < 1) then
          begin
            if (not jpeg_fill_bit_buffer(br_state,get_buffer,bits_left,1)) then
              goto undoit;
            get_buffer := br_state.get_buffer;
            bits_left := br_state.bits_left;
          end;

	  {if (GET_BITS(1)) then}
          Dec(bits_left);
          if (int(get_buffer shr bits_left)) and ( pred(1 shl 1) )<>0 then
          begin
	    if ((thiscoef^ and p1) = 0) then
            begin { do nothing if already changed it }
	      if (thiscoef^ >= 0) then
	        Inc(thiscoef^, p1)
	      else
	        Inc(thiscoef^, m1);
	    end;
	  end;
        end;
        Inc(k);
      end;
      { Count one block completed in EOB run }
      Dec(EOBRUN);
    end;

    { Completed MCU, so update state }
    {BITREAD_SAVE_STATE(cinfo,entropy^.bitstate);}
    cinfo^.src^.next_input_byte := br_state.next_input_byte;
    cinfo^.src^.bytes_in_buffer := br_state.bytes_in_buffer;
    entropy^.bitstate.get_buffer := get_buffer;
    entropy^.bitstate.bits_left := bits_left;

    entropy^.saved.EOBRUN := EOBRUN; { only part of saved state we care about }
  end;

  { Account for restart interval (no-op if not using restarts) }
  Dec(entropy^.restarts_to_go);

  decode_mcu_AC_refine := TRUE;
  exit;

undoit:
  { Re-zero any output coefficients that we made newly nonzero }
  while (num_newnz > 0) do
  begin
    Dec(num_newnz);
    block^[newnz_pos[num_newnz]] := 0;
  end;

  decode_mcu_AC_refine := FALSE;
end;


{ Module initialization routine for progressive Huffman entropy decoding. }

{GLOBAL}
procedure jinit_phuff_decoder (cinfo : j_decompress_ptr);
var
  entropy : phuff_entropy_ptr;
  coef_bit_ptr : int_ptr;
  ci, i : int;
begin
  entropy := phuff_entropy_ptr(
    cinfo^.mem^.alloc_small (j_common_ptr (cinfo), JPOOL_IMAGE,
				SIZEOF(phuff_entropy_decoder)) );
  cinfo^.entropy := jpeg_entropy_decoder_ptr (entropy);
  entropy^.pub.start_pass := start_pass_phuff_decoder;

  { Mark derived tables unallocated }
  for i := 0 to pred(NUM_HUFF_TBLS) do
  begin
    entropy^.derived_tbls[i] := NIL;
  end;

  { Create progression status table }
  cinfo^.coef_bits := coef_bits_ptrrow (
     cinfo^.mem^.alloc_small ( j_common_ptr (cinfo), JPOOL_IMAGE,
				cinfo^.num_components*DCTSIZE2*SIZEOF(int)) );
  coef_bit_ptr := @cinfo^.coef_bits^[0][0];
  for ci := 0 to pred(cinfo^.num_components) do
    for i := 0 to pred(DCTSIZE2) do
    begin
      coef_bit_ptr^ := -1;
      Inc(coef_bit_ptr);
    end;
end;

end.
