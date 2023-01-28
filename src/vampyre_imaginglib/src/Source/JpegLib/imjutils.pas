unit imjutils;

{ This file contains tables and miscellaneous utility routines needed
  for both compression and decompression.
  Note we prefix all global names with "j" to minimize conflicts with
  a surrounding application. }

{ Source: jutils.c; Copyright (C) 1991-1996, Thomas G. Lane. }

interface

{$I imjconfig.inc}

uses
  imjmorecfg,
  imjinclude,
  imjpeglib;


{ jpeg_zigzag_order[i] is the zigzag-order position of the i'th element
  of a DCT block read in natural order (left to right, top to bottom). }


{$ifdef FALSE}      { This table is not actually needed in v6a }

const
  jpeg_zigzag_order : array[0..DCTSIZE2] of int =
  (0,  1,  5,  6, 14, 15, 27, 28,
   2,  4,  7, 13, 16, 26, 29, 42,
   3,  8, 12, 17, 25, 30, 41, 43,
   9, 11, 18, 24, 31, 40, 44, 53,
  10, 19, 23, 32, 39, 45, 52, 54,
  20, 22, 33, 38, 46, 51, 55, 60,
  21, 34, 37, 47, 50, 56, 59, 61,
  35, 36, 48, 49, 57, 58, 62, 63);

{$endif}


{ jpeg_natural_order[i] is the natural-order position of the i'th element
  of zigzag order.

  When reading corrupted data, the Huffman decoders could attempt
  to reference an entry beyond the end of this array (if the decoded
  zero run length reaches past the end of the block).  To prevent
  wild stores without adding an inner-loop test, we put some extra
  "63"s after the real entries.  This will cause the extra coefficient
  to be stored in location 63 of the block, not somewhere random.
  The worst case would be a run-length of 15, which means we need 16
  fake entries. }


const
  jpeg_natural_order : array[0..DCTSIZE2+16-1] of int =
 (0,  1,  8, 16,  9,  2,  3, 10,
 17, 24, 32, 25, 18, 11,  4,  5,
 12, 19, 26, 33, 40, 48, 41, 34,
 27, 20, 13,  6,  7, 14, 21, 28,
 35, 42, 49, 56, 57, 50, 43, 36,
 29, 22, 15, 23, 30, 37, 44, 51,
 58, 59, 52, 45, 38, 31, 39, 46,
 53, 60, 61, 54, 47, 55, 62, 63,
 63, 63, 63, 63, 63, 63, 63, 63, { extra entries for safety in decoder }
 63, 63, 63, 63, 63, 63, 63, 63);



{ Arithmetic utilities }

{GLOBAL}
function jdiv_round_up (a : long; b : long) : long;

{GLOBAL}
function jround_up (a : long; b : long) : long;

{GLOBAL}
procedure jcopy_sample_rows (input_array : JSAMPARRAY;
                             source_row : int;
                             output_array : JSAMPARRAY; dest_row : int;
		             num_rows : int; num_cols : JDIMENSION);

{GLOBAL}
procedure jcopy_block_row (input_row : JBLOCKROW;
                           output_row : JBLOCKROW;
                           num_blocks : JDIMENSION);

{GLOBAL}
procedure jzero_far (target : pointer;{far} bytestozero : size_t);

procedure FMEMZERO(target : pointer; size : size_t);

procedure FMEMCOPY(dest,src : pointer; size : size_t);

implementation

{GLOBAL}
function jdiv_round_up (a : long; b : long) : long;
{ Compute a/b rounded up to next integer, ie, ceil(a/b) }
{ Assumes a >= 0, b > 0 }
begin
  jdiv_round_up := (a + b - long(1)) div b;
end;


{GLOBAL}
function jround_up (a : long; b : long) : long;
{ Compute a rounded up to next multiple of b, ie, ceil(a/b)*b }
{ Assumes a >= 0, b > 0 }
begin
  Inc(a, b - long(1));
  jround_up := a - (a mod b);
end;

{ On normal machines we can apply MEMCOPY() and MEMZERO() to sample arrays
  and coefficient-block arrays.  This won't work on 80x86 because the arrays
  are FAR and we're assuming a small-pointer memory model.  However, some
  DOS compilers provide far-pointer versions of memcpy() and memset() even
  in the small-model libraries.  These will be used if USE_FMEM is defined.
  Otherwise, the routines below do it the hard way.  (The performance cost
  is not all that great, because these routines aren't very heavily used.) }


{$ifndef NEED_FAR_POINTERS}      { normal case, same as regular macros }
procedure FMEMZERO(target : pointer; size : size_t);
begin
  FillChar(target^, size, 0);
end;

procedure FMEMCOPY(dest,src : pointer; size : size_t);
begin
  Move(src^, dest^, size);
end;


{$else}                       { 80x86 case, define if we can }
  {$ifdef USE_FMEM}
   FMEMCOPY(dest,src,size)    _fmemcpy((void FAR *)(dest), (const void FAR *)(src), (size_t)(size))
   FMEMZERO(target,size)      _fmemset((void FAR *)(target), 0, (size_t)(size))
  {$endif}
{$endif}


{GLOBAL}
procedure jcopy_sample_rows (input_array : JSAMPARRAY; source_row : int;
                             output_array : JSAMPARRAY; dest_row : int;
		             num_rows : int; num_cols : JDIMENSION);
{ Copy some rows of samples from one place to another.
  num_rows rows are copied from input_array[source_row++]
  to output_array[dest_row++]; these areas may overlap for duplication.
  The source and destination arrays must be at least as wide as num_cols. }
var
  inptr, outptr : JSAMPLE_PTR; {register}
{$ifdef FMEMCOPY}
  count : size_t; {register}
{$else}
  count : JDIMENSION; {register}
{$endif}
  row : int; {register}
begin
{$ifdef FMEMCOPY}
  count := size_t(num_cols * SIZEOF(JSAMPLE));
{$endif}
  Inc(JSAMPROW_PTR(input_array), source_row);
  Inc(JSAMPROW_PTR(output_array), dest_row);

  for row := pred(num_rows) downto 0 do
  begin
    inptr := JSAMPLE_PTR(input_array^[0]);
    Inc(JSAMPROW_PTR(input_array));
    outptr := JSAMPLE_PTR(output_array^[0]);
    Inc(JSAMPROW_PTR(output_array));
{$ifdef FMEMCOPY}
    FMEMCOPY(outptr, inptr, count);
{$else}
    for count := pred(num_cols) downto 0 do
    begin
      outptr^ := inptr^;        { needn't bother with GETJSAMPLE() here }
      Inc(inptr);
      Inc(outptr);
    end;
{$endif}
  end;
end;


{GLOBAL}
procedure jcopy_block_row (input_row : JBLOCKROW;
                           output_row : JBLOCKROW;
                           num_blocks : JDIMENSION);
{ Copy a row of coefficient blocks from one place to another. }
{$ifdef FMEMCOPY}
begin
  FMEMCOPY(output_row, input_row, num_blocks * (DCTSIZE2 * SIZEOF(JCOEF)));
{$else}
var
  inptr, outptr : JCOEFPTR; {register}
  count : long; {register}
begin
  inptr := JCOEFPTR (input_row);
  outptr := JCOEFPTR (output_row);
  for count := long(num_blocks) * DCTSIZE2 -1 downto 0 do
  begin
    outptr^ := inptr^;
    Inc(outptr);
    Inc(inptr);
  end;
{$endif}
end;


{GLOBAL}
procedure jzero_far (target : pointer;{far} bytestozero : size_t);
{ Zero out a chunk of FAR memory. }
{ This might be sample-array data, block-array data, or alloc_large data. }
{$ifdef FMEMZERO}
begin
  FMEMZERO(target, bytestozero);
{$else}
var
  ptr : byteptr;
  count : size_t; {register}
begin
  ptr := target;
  for count := bytestozero-1 downto 0 do
  begin
    ptr^ := 0;
    Inc(ptr);
  end;
{$endif}
end;

end.
