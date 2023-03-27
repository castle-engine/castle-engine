Unit Jdct;

{ Original: jdct.h; Copyright (C) 1994-1996, Thomas G. Lane. }

{ This include file contains common declarations for the forward and
  inverse DCT modules.  These declarations are private to the DCT managers
  (jcdctmgr.c, jddctmgr.c) and the individual DCT algorithms.
  The individual DCT algorithms are kept in separate files to ease
  machine-dependent tuning (e.g., assembly coding). }

interface

{$I jconfig.inc}

uses
  jmorecfg;


{ A forward DCT routine is given a pointer to a work area of type DCTELEM[];
  the DCT is to be performed in-place in that buffer.  Type DCTELEM is int
  for 8-bit samples, INT32 for 12-bit samples.  (NOTE: Floating-point DCT
  implementations use an array of type FAST_FLOAT, instead.)
  The DCT inputs are expected to be signed (range +-CENTERJSAMPLE).
  The DCT outputs are returned scaled up by a factor of 8; they therefore
  have a range of +-8K for 8-bit data, +-128K for 12-bit data.  This
  convention improves accuracy in integer implementations and saves some
  work in floating-point ones.
  Quantization of the output coefficients is done by jcdctmgr.c. }


{$ifdef BITS_IN_JSAMPLE_IS_8}
type
  DCTELEM = int;                { 16 or 32 bits is fine }
{$else}
type                            { must have 32 bits }
  DCTELEM = INT32;
{$endif}
type
  jTDctElem = 0..(MaxInt div SizeOf(DCTELEM))-1;
  DCTELEM_FIELD = array[jTDctElem] of DCTELEM;
  DCTELEM_FIELD_PTR = ^DCTELEM_FIELD;
  DCTELEMPTR = ^DCTELEM;

type
  forward_DCT_method_ptr = procedure(var data : array of DCTELEM);
  float_DCT_method_ptr = procedure(var data : array of FAST_FLOAT);


{ An inverse DCT routine is given a pointer to the input JBLOCK and a pointer
  to an output sample array.  The routine must dequantize the input data as
  well as perform the IDCT; for dequantization, it uses the multiplier table
  pointed to by compptr->dct_table.  The output data is to be placed into the
  sample array starting at a specified column.  (Any row offset needed will
  be applied to the array pointer before it is passed to the IDCT code.)
  Note that the number of samples emitted by the IDCT routine is
  DCT_scaled_size * DCT_scaled_size. }


{ typedef inverse_DCT_method_ptr is declared in jpegint.h }


{ Each IDCT routine has its own ideas about the best dct_table element type. }


type
  ISLOW_MULT_TYPE = MULTIPLIER;  { short or int, whichever is faster }

{$ifdef BITS_IN_JSAMPLE_IS_8}
type
  IFAST_MULT_TYPE = MULTIPLIER;  { 16 bits is OK, use short if faster }
const
  IFAST_SCALE_BITS = 2;         { fractional bits in scale factors }
{$else}
type
  IFAST_MULT_TYPE = INT32;      {  need 32 bits for scaled quantizers }
const
  IFAST_SCALE_BITS = 13;        { fractional bits in scale factors }
{$endif}
type
  FLOAT_MULT_TYPE = FAST_FLOAT; { preferred floating type }

const
  RANGE_MASK = (MAXJSAMPLE * 4 + 3); { 2 bits wider than legal samples }

type
  jTMultType = 0..(MaxInt div SizeOf(ISLOW_MULT_TYPE))-1;
  ISLOW_MULT_TYPE_FIELD = array[jTMultType] of ISLOW_MULT_TYPE;
  ISLOW_MULT_TYPE_FIELD_PTR = ^ISLOW_MULT_TYPE_FIELD;
  ISLOW_MULT_TYPE_PTR = ^ISLOW_MULT_TYPE;

  jTFloatType = 0..(MaxInt div SizeOf(FLOAT_MULT_TYPE))-1;
  FLOAT_MULT_TYPE_FIELD = array[jTFloatType] of FLOAT_MULT_TYPE;
  FLOAT_MULT_TYPE_FIELD_PTR = ^FLOAT_MULT_TYPE_FIELD;
  FLOAT_MULT_TYPE_PTR = ^FLOAT_MULT_TYPE;

  jTFastType = 0..(MaxInt div SizeOf(IFAST_MULT_TYPE))-1;
  IFAST_MULT_TYPE_FIELD = array[jTFastType] of IFAST_MULT_TYPE;
  IFAST_MULT_TYPE_FIELD_PTR = ^IFAST_MULT_TYPE_FIELD;
  IFAST_MULT_TYPE_PTR = ^IFAST_MULT_TYPE;

type
  jTFastFloat = 0..(MaxInt div SizeOf(FAST_FLOAT))-1;
  FAST_FLOAT_FIELD = array[jTFastFloat] of FAST_FLOAT;
  FAST_FLOAT_FIELD_PTR = ^FAST_FLOAT_FIELD;
  FAST_FLOAT_PTR = ^FAST_FLOAT;

implementation

end.
