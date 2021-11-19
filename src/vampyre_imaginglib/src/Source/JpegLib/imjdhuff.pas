unit imjdhuff;

{ This file contains declarations for Huffman entropy decoding routines
  that are shared between the sequential decoder (jdhuff.c) and the
  progressive decoder (jdphuff.c).  No other modules need to see these. }

{ This file contains Huffman entropy decoding routines.

  Much of the complexity here has to do with supporting input suspension.
  If the data source module demands suspension, we want to be able to back
  up to the start of the current MCU.  To do this, we copy state variables
  into local working storage, and update them back to the permanent
  storage only upon successful completion of an MCU. }

{ Original: jdhuff.h+jdhuff.c;  Copyright (C) 1991-1997, Thomas G. Lane. }



interface

{$I imjconfig.inc}

uses
  imjmorecfg,
  imjinclude,
  imjdeferr,
  imjerror,
  imjutils,
  imjpeglib;


{ Declarations shared with jdphuff.c }



{ Derived data constructed for each Huffman table }

const
  HUFF_LOOKAHEAD  = 8;          { # of bits of lookahead }

type
  d_derived_tbl_ptr = ^d_derived_tbl;
  d_derived_tbl = record
    { Basic tables: (element [0] of each array is unused) }
    maxcode : array[0..18-1] of INT32;       { largest code of length k (-1 if none) }
    { (maxcode[17] is a sentinel to ensure jpeg_huff_decode terminates) }
    valoffset : array[0..17-1] of INT32;     { huffval[] offset for codes of length k }
    { valoffset[k] = huffval[] index of 1st symbol of code length k, less
      the smallest code of length k; so given a code of length k, the
      corresponding symbol is huffval[code + valoffset[k]] }

    { Link to public Huffman table (needed only in jpeg_huff_decode) }
    pub : JHUFF_TBL_PTR;

    { Lookahead tables: indexed by the next HUFF_LOOKAHEAD bits of
      the input data stream.  If the next Huffman code is no more
      than HUFF_LOOKAHEAD bits long, we can obtain its length and
      the corresponding symbol directly from these tables. }

    look_nbits : array[0..(1 shl HUFF_LOOKAHEAD)-1] of int;
                                { # bits, or 0 if too long }
    look_sym : array[0..(1 shl HUFF_LOOKAHEAD)-1] of UINT8;
                                { symbol, or unused }
  end;

{ Fetching the next N bits from the input stream is a time-critical operation
  for the Huffman decoders.  We implement it with a combination of inline
  macros and out-of-line subroutines.  Note that N (the number of bits
  demanded at one time) never exceeds 15 for JPEG use.

  We read source bytes into get_buffer and dole out bits as needed.
  If get_buffer already contains enough bits, they are fetched in-line
  by the macros CHECK_BIT_BUFFER and GET_BITS.  When there aren't enough
  bits, jpeg_fill_bit_buffer is called; it will attempt to fill get_buffer
  as full as possible (not just to the number of bits needed; this
  prefetching reduces the overhead cost of calling jpeg_fill_bit_buffer).
  Note that jpeg_fill_bit_buffer may return FALSE to indicate suspension.
  On TRUE return, jpeg_fill_bit_buffer guarantees that get_buffer contains
  at least the requested number of bits --- dummy zeroes are inserted if
  necessary. }


type
  bit_buf_type = INT32 ;        { type of bit-extraction buffer }
const
  BIT_BUF_SIZE = 32;            { size of buffer in bits }

{ If long is > 32 bits on your machine, and shifting/masking longs is
  reasonably fast, making bit_buf_type be long and setting BIT_BUF_SIZE
  appropriately should be a win.  Unfortunately we can't define the size
  with something like  #define BIT_BUF_SIZE (sizeof(bit_buf_type)*8)
  because not all machines measure sizeof in 8-bit bytes. }

type
  bitread_perm_state = record   { Bitreading state saved across MCUs }
    get_buffer : bit_buf_type;  { current bit-extraction buffer }
    bits_left : int;            { # of unused bits in it }
  end;

type
  bitread_working_state = record
    { Bitreading working state within an MCU }
    { current data source location }
    { We need a copy, rather than munging the original, in case of suspension }
    next_input_byte : JOCTETptr;  { => next byte to read from source }
    bytes_in_buffer : size_t;     { # of bytes remaining in source buffer }
    { Bit input buffer --- note these values are kept in register variables,
      not in this struct, inside the inner loops. }

    get_buffer : bit_buf_type;  { current bit-extraction buffer }
    bits_left : int;            { # of unused bits in it }
    { Pointer needed by jpeg_fill_bit_buffer }
    cinfo : j_decompress_ptr;   { back link to decompress master record }
  end;

{ Module initialization routine for Huffman entropy decoding. }

{GLOBAL}
procedure jinit_huff_decoder (cinfo : j_decompress_ptr);

{GLOBAL}
function jpeg_huff_decode(var state : bitread_working_state;
                          get_buffer : bit_buf_type; {register}
                          bits_left : int; {register}
                          htbl : d_derived_tbl_ptr;
                          min_bits : int) : int;

{ Compute the derived values for a Huffman table.
  Note this is also used by jdphuff.c. }

{GLOBAL}
procedure jpeg_make_d_derived_tbl (cinfo : j_decompress_ptr;
                                   isDC : boolean;
                                   tblno : int;
			           var pdtbl : d_derived_tbl_ptr);

{ Load up the bit buffer to a depth of at least nbits }

function jpeg_fill_bit_buffer	(var state : bitread_working_state;
                                 get_buffer : bit_buf_type;  {register}
	                         bits_left : int; {register}
                                 nbits : int) : boolean;

implementation

{$IFDEF MACRO}

{ Macros to declare and load/save bitread local variables. }
{$define BITREAD_STATE_VARS}
	get_buffer : bit_buf_type ; {register}
	bits_left : int; {register}
	br_state : bitread_working_state;

{$define BITREAD_LOAD_STATE(cinfop,permstate)}
	br_state.cinfo := cinfop;
	br_state.next_input_byte := cinfop^.src^.next_input_byte;
	br_state.bytes_in_buffer := cinfop^.src^.bytes_in_buffer;
	get_buffer := permstate.get_buffer;
	bits_left := permstate.bits_left;

{$define BITREAD_SAVE_STATE(cinfop,permstate) }
	cinfop^.src^.next_input_byte := br_state.next_input_byte;
	cinfop^.src^.bytes_in_buffer := br_state.bytes_in_buffer;
	permstate.get_buffer := get_buffer;
	permstate.bits_left := bits_left;


{ These macros provide the in-line portion of bit fetching.
  Use CHECK_BIT_BUFFER to ensure there are N bits in get_buffer
  before using GET_BITS, PEEK_BITS, or DROP_BITS.
  The variables get_buffer and bits_left are assumed to be locals,
  but the state struct might not be (jpeg_huff_decode needs this).
 	CHECK_BIT_BUFFER(state,n,action);
 		Ensure there are N bits in get_buffer; if suspend, take action.
       val = GET_BITS(n);
 		Fetch next N bits.
       val = PEEK_BITS(n);
 		Fetch next N bits without removing them from the buffer.
 	DROP_BITS(n);
 		Discard next N bits.
  The value N should be a simple variable, not an expression, because it
  is evaluated multiple times. }


{$define CHECK_BIT_BUFFER(state,nbits,action)}
  if (bits_left < (nbits)) then
  begin
    if (not jpeg_fill_bit_buffer(&(state),get_buffer,bits_left,nbits)) then
    begin
      action;
      exit;
    end;
    get_buffer := state.get_buffer;
    bits_left := state.bits_left;
  end;


{$define GET_BITS(nbits)}
	Dec(bits_left, (nbits));
	( (int(get_buffer shr bits_left)) and ( pred(1 shl (nbits)) ) )

{$define PEEK_BITS(nbits)}
	int(get_buffer shr (bits_left -  (nbits))) and pred(1 shl (nbits))

{$define DROP_BITS(nbits)}
	Dec(bits_left, nbits);




{ Code for extracting next Huffman-coded symbol from input bit stream.
  Again, this is time-critical and we make the main paths be macros.

  We use a lookahead table to process codes of up to HUFF_LOOKAHEAD bits
  without looping.  Usually, more than 95% of the Huffman codes will be 8
  or fewer bits long.  The few overlength codes are handled with a loop,
  which need not be inline code.

  Notes about the HUFF_DECODE macro:
  1. Near the end of the data segment, we may fail to get enough bits
     for a lookahead.  In that case, we do it the hard way.
  2. If the lookahead table contains no entry, the next code must be
     more than HUFF_LOOKAHEAD bits long.
  3. jpeg_huff_decode returns -1 if forced to suspend. }




macro HUFF_DECODE(s,br_state,htbl,return FALSE,slowlabel);
label showlabel;
var
 nb, look : int; {register}
begin
  if (bits_left < HUFF_LOOKAHEAD) then
  begin
    if (not jpeg_fill_bit_buffer(br_state,get_buffer,bits_left, 0)) then
    begin
      decode_mcu := FALSE;
      exit;
    end;
    get_buffer := br_state.get_buffer;
    bits_left := br_state.bits_left;
    if (bits_left < HUFF_LOOKAHEAD) then
    begin
      nb := 1;
      goto slowlabel;
    end;
  end;
  {look := PEEK_BITS(HUFF_LOOKAHEAD);}
  look := int(get_buffer shr (bits_left -  HUFF_LOOKAHEAD)) and
                 pred(1 shl HUFF_LOOKAHEAD);

  nb := htbl^.look_nbits[look];
  if (nb <> 0) then
  begin
    {DROP_BITS(nb);}
    Dec(bits_left, nb);

    s := htbl^.look_sym[look];
  end
  else
  begin
    nb := HUFF_LOOKAHEAD+1;
slowlabel:
    s := jpeg_huff_decode(br_state,get_buffer,bits_left,htbl,nb));
    if (s < 0) then
    begin
      result := FALSE;
      exit;
    end;
    get_buffer := br_state.get_buffer;
    bits_left := br_state.bits_left;
  end;
end;


{$ENDIF} {MACRO}

{ Expanded entropy decoder object for Huffman decoding.

  The savable_state subrecord contains fields that change within an MCU,
  but must not be updated permanently until we complete the MCU. }

type
  savable_state = record
    last_dc_val : array[0..MAX_COMPS_IN_SCAN-1] of int; { last DC coef for each component }
  end;


type
  huff_entropy_ptr = ^huff_entropy_decoder;
  huff_entropy_decoder = record
    pub : jpeg_entropy_decoder; { public fields }

    { These fields are loaded into local variables at start of each MCU.
      In case of suspension, we exit WITHOUT updating them. }

    bitstate : bitread_perm_state;	{ Bit buffer at start of MCU }
    saved : savable_state;		{ Other state at start of MCU }

    { These fields are NOT loaded into local working state. }
    restarts_to_go : uInt;              { MCUs left in this restart interval }

    { Pointers to derived tables (these workspaces have image lifespan) }
    dc_derived_tbls : array[0..NUM_HUFF_TBLS] of d_derived_tbl_ptr;
    ac_derived_tbls : array[0..NUM_HUFF_TBLS] of d_derived_tbl_ptr;

    { Precalculated info set up by start_pass for use in decode_mcu: }

    { Pointers to derived tables to be used for each block within an MCU }
    dc_cur_tbls : array[0..D_MAX_BLOCKS_IN_MCU-1] of d_derived_tbl_ptr;
    ac_cur_tbls : array[0..D_MAX_BLOCKS_IN_MCU-1] of d_derived_tbl_ptr;
    { Whether we care about the DC and AC coefficient values for each block }
    dc_needed : array[0..D_MAX_BLOCKS_IN_MCU-1] of boolean;
    ac_needed : array[0..D_MAX_BLOCKS_IN_MCU-1] of boolean;
  end;



{ Initialize for a Huffman-compressed scan. }

{METHODDEF}
procedure start_pass_huff_decoder (cinfo : j_decompress_ptr);  
var
  entropy : huff_entropy_ptr;
  ci, blkn, dctbl, actbl : int;
  compptr : jpeg_component_info_ptr;
begin
  entropy := huff_entropy_ptr (cinfo^.entropy);

  { Check that the scan parameters Ss, Se, Ah/Al are OK for sequential JPEG.
    This ought to be an error condition, but we make it a warning because
    there are some baseline files out there with all zeroes in these bytes. }

  if (cinfo^.Ss <> 0) or (cinfo^.Se <> DCTSIZE2-1) or
     (cinfo^.Ah <> 0) or (cinfo^.Al <> 0) then
    WARNMS(j_common_ptr(cinfo), JWRN_NOT_SEQUENTIAL);

  for ci := 0 to pred(cinfo^.comps_in_scan) do
  begin
    compptr := cinfo^.cur_comp_info[ci];
    dctbl := compptr^.dc_tbl_no;
    actbl := compptr^.ac_tbl_no;
    { Compute derived values for Huffman tables }
    { We may do this more than once for a table, but it's not expensive }
    jpeg_make_d_derived_tbl(cinfo, TRUE, dctbl,
			    entropy^.dc_derived_tbls[dctbl]);
    jpeg_make_d_derived_tbl(cinfo, FALSE, actbl,
			    entropy^.ac_derived_tbls[actbl]);
    { Initialize DC predictions to 0 }
    entropy^.saved.last_dc_val[ci] := 0;
  end;

  { Precalculate decoding info for each block in an MCU of this scan }
  for blkn := 0 to pred(cinfo^.blocks_in_MCU) do
  begin
    ci := cinfo^.MCU_membership[blkn];
    compptr := cinfo^.cur_comp_info[ci];
    { Precalculate which table to use for each block }
    entropy^.dc_cur_tbls[blkn] := entropy^.dc_derived_tbls[compptr^.dc_tbl_no];
    entropy^.ac_cur_tbls[blkn] := entropy^.ac_derived_tbls[compptr^.ac_tbl_no];
    { Decide whether we really care about the coefficient values }
    if (compptr^.component_needed) then
    begin
      entropy^.dc_needed[blkn] := TRUE;
      { we don't need the ACs if producing a 1/8th-size image }
      entropy^.ac_needed[blkn] := (compptr^.DCT_scaled_size > 1);
    end
    else
    begin
      entropy^.ac_needed[blkn] := FALSE;
      entropy^.dc_needed[blkn] := FALSE;
    end;
  end;

  { Initialize bitread state variables }
  entropy^.bitstate.bits_left := 0;
  entropy^.bitstate.get_buffer := 0; { unnecessary, but keeps Purify quiet }
  entropy^.pub.insufficient_data := FALSE;

  { Initialize restart counter }
  entropy^.restarts_to_go := cinfo^.restart_interval;
end;


{ Compute the derived values for a Huffman table.
  This routine also performs some validation checks on the table.

  Note this is also used by jdphuff.c. }

{GLOBAL}
procedure jpeg_make_d_derived_tbl (cinfo : j_decompress_ptr;
                                   isDC : boolean;
                                   tblno : int;
	                           var pdtbl : d_derived_tbl_ptr);
var
  htbl : JHUFF_TBL_PTR;
  dtbl : d_derived_tbl_ptr;
  p, i, l, si, numsymbols : int;
  lookbits, ctr : int;
  huffsize : array[0..257-1] of byte;
  huffcode : array[0..257-1] of uInt;
  code : uInt;
var
  sym : int;
begin
  { Note that huffsize[] and huffcode[] are filled in code-length order,
    paralleling the order of the symbols themselves in htbl^.huffval[]. }

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
    pdtbl := d_derived_tbl_ptr(
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				  SIZEOF(d_derived_tbl)) );
  dtbl := pdtbl;
  dtbl^.pub := htbl;		{ fill in back link }

  { Figure C.1: make table of Huffman code length for each symbol }

  p := 0;
  for l := 1 to 16 do
  begin
    i := int(htbl^.bits[l]);
    if (i < 0) or (p + i > 256) then  { protect against table overrun }
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_HUFF_TABLE);
    while (i > 0) do
    begin
      huffsize[p] := byte(l);
      Inc(p);
      Dec(i);
    end;
  end;
  huffsize[p] := 0;
  numsymbols := p;

  { Figure C.2: generate the codes themselves }
  { We also validate that the counts represent a legal Huffman code tree. }

  code := 0;
  si := huffsize[0];
  p := 0;
  while (huffsize[p] <> 0) do
  begin
    while (( int (huffsize[p]) ) = si) do
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

  { Figure F.15: generate decoding tables for bit-sequential decoding }

  p := 0;
  for l := 1 to 16 do
  begin
    if (htbl^.bits[l] <> 0) then
    begin
      { valoffset[l] = huffval[] index of 1st symbol of code length l,
        minus the minimum code of length l }

      dtbl^.valoffset[l] := INT32(p) - INT32(huffcode[p]);
      Inc(p, htbl^.bits[l]);
      dtbl^.maxcode[l] := huffcode[p-1]; { maximum code of length l }
    end
    else
    begin
      dtbl^.maxcode[l] := -1;	{ -1 if no codes of this length }
    end;
  end;
  dtbl^.maxcode[17] := long($FFFFF); { ensures jpeg_huff_decode terminates }

  { Compute lookahead tables to speed up decoding.
    First we set all the table entries to 0, indicating "too long";
    then we iterate through the Huffman codes that are short enough and
    fill in all the entries that correspond to bit sequences starting
    with that code. }

  MEMZERO(@dtbl^.look_nbits, SIZEOF(dtbl^.look_nbits));

  p := 0;
  for l := 1 to HUFF_LOOKAHEAD do
  begin
    for i := 1 to int (htbl^.bits[l]) do
    begin
      { l := current code's length, p := its index in huffcode[] & huffval[]. }
      { Generate left-justified code followed by all possible bit sequences }
      lookbits := huffcode[p] shl (HUFF_LOOKAHEAD-l);
      for ctr := pred(1 shl (HUFF_LOOKAHEAD-l)) downto 0 do
      begin
	dtbl^.look_nbits[lookbits] := l;
	dtbl^.look_sym[lookbits] := htbl^.huffval[p];
	Inc(lookbits);
      end;
      Inc(p);
    end;
  end;

  { Validate symbols as being reasonable.
    For AC tables, we make no check, but accept all byte values 0..255.
    For DC tables, we require the symbols to be in range 0..15.
    (Tighter bounds could be applied depending on the data depth and mode,
    but this is sufficient to ensure safe decoding.) }

  if (isDC) then
  begin
    for i := 0 to pred(numsymbols) do
    begin
      sym := htbl^.huffval[i];
      if (sym < 0) or (sym > 15) then
	ERREXIT(j_common_ptr(cinfo), JERR_BAD_HUFF_TABLE);
    end;
  end;
end;


{ Out-of-line code for bit fetching (shared with jdphuff.c).
  See jdhuff.h for info about usage.
  Note: current values of get_buffer and bits_left are passed as parameters,
  but are returned in the corresponding fields of the state struct.

  On most machines MIN_GET_BITS should be 25 to allow the full 32-bit width
  of get_buffer to be used.  (On machines with wider words, an even larger
  buffer could be used.)  However, on some machines 32-bit shifts are
  quite slow and take time proportional to the number of places shifted.
  (This is true with most PC compilers, for instance.)  In this case it may
  be a win to set MIN_GET_BITS to the minimum value of 15.  This reduces the
  average shift distance at the cost of more calls to jpeg_fill_bit_buffer. }

{$ifdef SLOW_SHIFT_32}
const
  MIN_GET_BITS = 15;	{ minimum allowable value }
{$else}
const
  MIN_GET_BITS = (BIT_BUF_SIZE-7);
{$endif}


{GLOBAL}
function jpeg_fill_bit_buffer (var state : bitread_working_state;
		              {register} get_buffer : bit_buf_type;
                              {register} bits_left : int;
		              nbits  : int) : boolean;
label
  no_more_bytes;
{ Load up the bit buffer to a depth of at least nbits }
var
  { Copy heavily used state fields into locals (hopefully registers) }
  {register} next_input_byte : {const} JOCTETptr;
  {register} bytes_in_buffer : size_t;
var
  {register} c : int;
var
  cinfo : j_decompress_ptr;
begin
  next_input_byte := state.next_input_byte;
  bytes_in_buffer := state.bytes_in_buffer;
  cinfo := state.cinfo;

  { Attempt to load at least MIN_GET_BITS bits into get_buffer. }
  { (It is assumed that no request will be for more than that many bits.) }
  { We fail to do so only if we hit a marker or are forced to suspend. }

  if (cinfo^.unread_marker = 0) then	{ cannot advance past a marker }
  begin
    while (bits_left < MIN_GET_BITS) do
    begin
      { Attempt to read a byte }
      if (bytes_in_buffer = 0) then
      begin
	if not cinfo^.src^.fill_input_buffer(cinfo) then
        begin
	  jpeg_fill_bit_buffer := FALSE;
          exit;
        end;
	next_input_byte := cinfo^.src^.next_input_byte;
	bytes_in_buffer := cinfo^.src^.bytes_in_buffer;
      end;
      Dec(bytes_in_buffer);
      c := GETJOCTET(next_input_byte^);
      Inc(next_input_byte);


      { If it's $FF, check and discard stuffed zero byte }
      if (c = $FF) then
      begin
        { Loop here to discard any padding FF's on terminating marker,
	  so that we can save a valid unread_marker value.  NOTE: we will
	  accept multiple FF's followed by a 0 as meaning a single FF data
	  byte.  This data pattern is not valid according to the standard. }

        repeat
	  if (bytes_in_buffer = 0) then
          begin
	    if (not state.cinfo^.src^.fill_input_buffer (state.cinfo)) then
            begin
	      jpeg_fill_bit_buffer := FALSE;
              exit;
            end;
	    next_input_byte := state.cinfo^.src^.next_input_byte;
	    bytes_in_buffer := state.cinfo^.src^.bytes_in_buffer;
	  end;
	  Dec(bytes_in_buffer);
	  c := GETJOCTET(next_input_byte^);
          Inc(next_input_byte);
        Until (c <> $FF);

        if (c = 0) then
        begin
	  { Found FF/00, which represents an FF data byte }
	  c := $FF;
        end
        else
        begin
	  { Oops, it's actually a marker indicating end of compressed data.
            Save the marker code for later use.
	    Fine point: it might appear that we should save the marker into
	    bitread working state, not straight into permanent state.  But
	    once we have hit a marker, we cannot need to suspend within the
	    current MCU, because we will read no more bytes from the data
	    source.  So it is OK to update permanent state right away. }

	  cinfo^.unread_marker := c;
          { See if we need to insert some fake zero bits. }
	  goto no_more_bytes;
	end;
      end;

      { OK, load c into get_buffer }
      get_buffer := (get_buffer shl 8) or c;
      Inc(bits_left, 8);
    end { end while }
  end
  else
  begin
  no_more_bytes:
    { We get here if we've read the marker that terminates the compressed
      data segment.  There should be enough bits in the buffer register
      to satisfy the request; if so, no problem. }

    if (nbits > bits_left) then
    begin
      { Uh-oh.  Report corrupted data to user and stuff zeroes into
        the data stream, so that we can produce some kind of image.
        We use a nonvolatile flag to ensure that only one warning message
        appears per data segment. }

      if not cinfo^.entropy^.insufficient_data then
      begin
	WARNMS(j_common_ptr(cinfo), JWRN_HIT_MARKER);
	cinfo^.entropy^.insufficient_data := TRUE;
      end;
      { Fill the buffer with zero bits }
      get_buffer := get_buffer shl (MIN_GET_BITS - bits_left);
      bits_left := MIN_GET_BITS;
    end;
  end;

  { Unload the local registers }
  state.next_input_byte := next_input_byte;
  state.bytes_in_buffer := bytes_in_buffer;
  state.get_buffer := get_buffer;
  state.bits_left := bits_left;

  jpeg_fill_bit_buffer := TRUE;
end;


{ Out-of-line code for Huffman code decoding.
  See jdhuff.h for info about usage. }

{GLOBAL}
function jpeg_huff_decode (var state : bitread_working_state;
		          {register} get_buffer : bit_buf_type;
                          {register} bits_left : int;
		          htbl : d_derived_tbl_ptr;
                          min_bits : int) : int;
var
  {register} l : int;
  {register} code : INT32;
begin
  l := min_bits;

  { HUFF_DECODE has determined that the code is at least min_bits }
  { bits long, so fetch that many bits in one swoop. }

  {CHECK_BIT_BUFFER(state, l, return -1);}
  if (bits_left < l) then
  begin
    if (not jpeg_fill_bit_buffer(state, get_buffer, bits_left, l)) then
    begin
      jpeg_huff_decode := -1;
      exit;
    end;
    get_buffer := state.get_buffer;
    bits_left := state.bits_left;
  end;

  {code := GET_BITS(l);}
  Dec(bits_left, l);
  code := (int(get_buffer shr bits_left)) and ( pred(1 shl l) );

  { Collect the rest of the Huffman code one bit at a time. }
  { This is per Figure F.16 in the JPEG spec. }

  while (code > htbl^.maxcode[l]) do
  begin
    code := code shl 1;
    {CHECK_BIT_BUFFER(state, 1, return -1);}
    if (bits_left < 1) then
    begin
      if (not jpeg_fill_bit_buffer(state, get_buffer, bits_left, 1)) then
      begin
        jpeg_huff_decode := -1;
        exit;
      end;
      get_buffer := state.get_buffer;
      bits_left := state.bits_left;
    end;

    {code := code or GET_BITS(1);}
    Dec(bits_left);
    code := code or ( (int(get_buffer shr bits_left)) and pred(1 shl 1) );

    Inc(l);
  end;

  { Unload the local registers }
  state.get_buffer := get_buffer;
  state.bits_left := bits_left;

  { With garbage input we may reach the sentinel value l := 17. }

  if (l > 16) then
  begin
    WARNMS(j_common_ptr(state.cinfo), JWRN_HUFF_BAD_CODE);
    jpeg_huff_decode := 0;	{ fake a zero as the safest result }
    exit;
  end;

  jpeg_huff_decode := htbl^.pub^.huffval[ int (code + htbl^.valoffset[l]) ];
end;


{ Figure F.12: extend sign bit.
  On some machines, a shift and add will be faster than a table lookup. }

{$ifdef AVOID_TABLES}

#define HUFF_EXTEND(x,s)  ((x) < (1<<((s)-1)) ? (x) + (((-1)<<(s)) + 1) : (x))

{$else}

{$define HUFF_EXTEND(x,s)
  if (x < extend_test[s]) then
    := x + extend_offset[s]
  else
   x;}

const
  extend_test : array[0..16-1] of int =   { entry n is 2**(n-1) }
  ($0000, $0001, $0002, $0004, $0008, $0010, $0020, $0040,
   $0080, $0100, $0200, $0400, $0800, $1000, $2000, $4000);

const
  extend_offset : array[0..16-1] of int = { entry n is (-1 << n) + 1 }
(0, ((-1) shl 1) + 1, ((-1) shl 2) + 1, ((-1) shl 3) + 1, ((-1) shl 4) + 1,
    ((-1) shl 5) + 1, ((-1) shl 6) + 1, ((-1) shl 7) + 1, ((-1) shl 8) + 1,
    ((-1) shl 9) + 1, ((-1) shl 10) + 1, ((-1) shl 11) + 1,((-1) shl 12) + 1,
   ((-1) shl 13) + 1, ((-1) shl 14) + 1, ((-1) shl 15) + 1);

{$endif} { AVOID_TABLES }


{ Check for a restart marker & resynchronize decoder.
  Returns FALSE if must suspend. }

{LOCAL}
function process_restart (cinfo : j_decompress_ptr) : boolean;
var
  entropy : huff_entropy_ptr;
  ci : int;
begin
  entropy := huff_entropy_ptr (cinfo^.entropy);

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


{ Decode and return one MCU's worth of Huffman-compressed coefficients.
  The coefficients are reordered from zigzag order into natural array order,
  but are not dequantized.

  The i'th block of the MCU is stored into the block pointed to by
  MCU_data[i].  WE ASSUME THIS AREA HAS BEEN ZEROED BY THE CALLER.
  (Wholesale zeroing is usually a little faster than retail...)

  Returns FALSE if data source requested suspension.  In that case no
  changes have been made to permanent state.  (Exception: some output
  coefficients may already have been assigned.  This is harmless for
  this module, since we'll just re-assign them on the next call.) }

{METHODDEF}
function decode_mcu (cinfo : j_decompress_ptr;
                     var MCU_data : array of JBLOCKROW) : boolean;  
label
  label1, label2, label3;
var
  entropy : huff_entropy_ptr;
  {register} s, k, r : int;
  blkn, ci : int;
  block : JBLOCK_PTR;
  {BITREAD_STATE_VARS}
  get_buffer : bit_buf_type ; {register}
  bits_left : int; {register}
  br_state : bitread_working_state;

  state : savable_state;
  dctbl : d_derived_tbl_ptr;
  actbl : d_derived_tbl_ptr;
var
  nb, look : int; {register}
begin
  entropy := huff_entropy_ptr (cinfo^.entropy);

  { Process restart marker if needed; may have to suspend }
  if (cinfo^.restart_interval <> 0) then
  begin
    if (entropy^.restarts_to_go = 0) then
      if (not process_restart(cinfo)) then
      begin
        decode_mcu := FALSE;
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
      dctbl := entropy^.dc_cur_tbls[blkn];
      actbl := entropy^.ac_cur_tbls[blkn];

      { Decode a single block's worth of coefficients }

      { Section F.2.2.1: decode the DC coefficient difference }
      {HUFF_DECODE(s, br_state, dctbl, return FALSE, label1);}
      if (bits_left < HUFF_LOOKAHEAD) then
      begin
        if (not jpeg_fill_bit_buffer(br_state,get_buffer,bits_left, 0)) then
        begin
          decode_mcu := False;
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

      nb := dctbl^.look_nbits[look];
      if (nb <> 0) then
      begin
        {DROP_BITS(nb);}
        Dec(bits_left, nb);

        s := dctbl^.look_sym[look];
      end
      else
      begin
        nb := HUFF_LOOKAHEAD+1;
    label1:
        s := jpeg_huff_decode(br_state,get_buffer,bits_left,dctbl,nb);
        if (s < 0) then
        begin
          decode_mcu := FALSE;
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
            decode_mcu := FALSE;
            exit;
          end;
          get_buffer := br_state.get_buffer;
          bits_left := br_state.bits_left;
        end;

        {r := GET_BITS(s);}
        Dec(bits_left, s);
        r := ( int(get_buffer shr bits_left)) and ( pred(1 shl s) );

        {s := HUFF_EXTEND(r, s);}
        if (r < extend_test[s]) then
          s := r + extend_offset[s]
        else
          s := r;
      end;

      if (entropy^.dc_needed[blkn]) then
      begin
	{ Convert DC difference to actual value, update last_dc_val }
        ci := cinfo^.MCU_membership[blkn];
	Inc(s, state.last_dc_val[ci]);
        state.last_dc_val[ci] := s;
        { Output the DC coefficient (assumes jpeg_natural_order[0] := 0) }
        block^[0] := JCOEF (s);
      end;

      if (entropy^.ac_needed[blkn]) then
      begin

	{ Section F.2.2.2: decode the AC coefficients }
	{ Since zeroes are skipped, output area must be cleared beforehand }
	k := 1;
        while (k < DCTSIZE2) do         { Nomssi: k is incr. in the loop }
        begin
          {HUFF_DECODE(s, br_state, actbl, return FALSE, label2);}
          if (bits_left < HUFF_LOOKAHEAD) then
          begin
            if (not jpeg_fill_bit_buffer(br_state,get_buffer,bits_left, 0)) then
            begin
              decode_mcu := False;
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

          nb := actbl^.look_nbits[look];
          if (nb <> 0) then
          begin
            {DROP_BITS(nb);}
            Dec(bits_left, nb);

            s := actbl^.look_sym[look];
          end
          else
          begin
            nb := HUFF_LOOKAHEAD+1;
        label2:
            s := jpeg_huff_decode(br_state,get_buffer,bits_left,actbl,nb);
            if (s < 0) then
            begin
              decode_mcu := FALSE;
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
                decode_mcu := FALSE;
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
            { Output coefficient in natural (dezigzagged) order.
              Note: the extra entries in jpeg_natural_order[] will save us
              if k >= DCTSIZE2, which could happen if the data is corrupted. }

            block^[jpeg_natural_order[k]] := JCOEF (s);
          end
          else
          begin
            if (r <> 15) then
              break;
            Inc(k, 15);
          end;
          Inc(k);
        end;
      end
      else
      begin

        { Section F.2.2.2: decode the AC coefficients }
        { In this path we just discard the values }
        k := 1;
        while (k < DCTSIZE2) do
        begin
	  {HUFF_DECODE(s, br_state, actbl, return FALSE, label3);}
          if (bits_left < HUFF_LOOKAHEAD) then
          begin
            if (not jpeg_fill_bit_buffer(br_state,get_buffer,bits_left, 0)) then
            begin
              decode_mcu := False;
              exit;
            end;
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

          nb := actbl^.look_nbits[look];
          if (nb <> 0) then
          begin
            {DROP_BITS(nb);}
	    Dec(bits_left, nb);

            s := actbl^.look_sym[look];
          end
          else
          begin
            nb := HUFF_LOOKAHEAD+1;
        label3:
            s := jpeg_huff_decode(br_state,get_buffer,bits_left,actbl,nb);
            if (s < 0) then
            begin
              decode_mcu := FALSE;
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
                decode_mcu := FALSE;
                exit;
              end;
              get_buffer := br_state.get_buffer;
              bits_left := br_state.bits_left;
            end;

	    {DROP_BITS(s);}
            Dec(bits_left, s);
	  end
          else
          begin
	    if (r <> 15) then
	      break;
	    Inc(k, 15);
	  end;
          Inc(k);
        end;

      end;
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
  if entropy^.restarts_to_go > 0 then
    Dec(entropy^.restarts_to_go);

  decode_mcu := TRUE;
end;


{ Module initialization routine for Huffman entropy decoding. }

{GLOBAL}
procedure jinit_huff_decoder (cinfo : j_decompress_ptr);
var
  entropy : huff_entropy_ptr;
  i : int;
begin
  entropy := huff_entropy_ptr(
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				SIZEOF(huff_entropy_decoder)) );
  cinfo^.entropy := jpeg_entropy_decoder_ptr (entropy);
  entropy^.pub.start_pass := start_pass_huff_decoder;
  entropy^.pub.decode_mcu := decode_mcu;

  { Mark tables unallocated }
  for i := 0 to pred(NUM_HUFF_TBLS) do
  begin
    entropy^.dc_derived_tbls[i] := NIL;
    entropy^.ac_derived_tbls[i] := NIL;
  end;
end;

end.
