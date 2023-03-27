{ ----------------------- JPEG_INTERNAL_OPTIONS ---------------------- }


{ These defines indicate whether to include various optional functions.
  Undefining some of these symbols will produce a smaller but less capable
  library.  Note that you can leave certain source files out of the
  compilation/linking process if you've #undef'd the corresponding symbols.
  (You may HAVE to do that if your compiler doesn't like null source files.)}


{ Arithmetic coding is unsupported for legal reasons.  Complaints to IBM. }

{ Capability options common to encoder and decoder: }

{$define DCT_ISLOW_SUPPORTED}     { slow but accurate integer algorithm }
{$define DCT_IFAST_SUPPORTED}     { faster, less accurate integer method }
{$define DCT_FLOAT_SUPPORTED}     { floating-point: accurate, fast on fast HW }

{ Encoder capability options: }

{$undef C_ARITH_CODING_SUPPORTED}    { Arithmetic coding back end? }
{$define C_MULTISCAN_FILES_SUPPORTED} { Multiple-scan JPEG files? }
{$define C_PROGRESSIVE_SUPPORTED}     { Progressive JPEG? (Requires MULTISCAN)}
{$define ENTROPY_OPT_SUPPORTED}       { Optimization of entropy coding parms? }
{ Note: if you selected 12-bit data precision, it is dangerous to turn off
  ENTROPY_OPT_SUPPORTED.  The standard Huffman tables are only good for 8-bit
  precision, so jchuff.c normally uses entropy optimization to compute
  usable tables for higher precision.  If you don't want to do optimization,
  you'll have to supply different default Huffman tables.
  The exact same statements apply for progressive JPEG: the default tables
  don't work for progressive mode.  (This may get fixed, however.) }

{$define INPUT_SMOOTHING_SUPPORTED}   { Input image smoothing option? }

{ Decoder capability options: }

{$undef  D_ARITH_CODING_SUPPORTED}    { Arithmetic coding back end? }
{$define D_MULTISCAN_FILES_SUPPORTED} { Multiple-scan JPEG files? }
{$define D_PROGRESSIVE_SUPPORTED}     { Progressive JPEG? (Requires MULTISCAN)}
{$define SAVE_MARKERS_SUPPORTED}      { jpeg_save_markers() needed? }
{$define BLOCK_SMOOTHING_SUPPORTED}   { Block smoothing? (Progressive only) }
{$define IDCT_SCALING_SUPPORTED}      { Output rescaling via IDCT? }
{$undef  UPSAMPLE_SCALING_SUPPORTED}  { Output rescaling at upsample stage? }
{$define UPSAMPLE_MERGING_SUPPORTED}  { Fast path for sloppy upsampling? }
{$define QUANT_1PASS_SUPPORTED}       { 1-pass color quantization? }
{$define QUANT_2PASS_SUPPORTED}       { 2-pass color quantization? }

{ If you happen not to want the image transform support, disable it here }
{$define TRANSFORMS_SUPPORTED}

{ more capability options later, no doubt }

{$ifopt I+} {$define IOcheck} {$endif}

{ ------------------------------------------------------------------------ }

{$define USE_FMEM}              { Borland has _fmemcpy() and _fmemset() }

{$define FMEMCOPY}
{$define FMEMZERO}

{$define DCTSIZE_IS_8}          { e.g. unroll the inner loop }
{$define RIGHT_SHIFT_IS_UNSIGNED}
{$undef AVOID_TABLES}
{$undef FAST_DIVIDE}

{$define BITS_IN_JSAMPLE_IS_8}

{----------------------------------------------------------------}
{ for test of 12 bit JPEG code only. !! }
{-- $undef  BITS_IN_JSAMPLE_IS_8}
{----------------------------------------------------------------}

{$define RGB_PIXELSIZE_IS_3}
{$define SLOW_SHIFT_32}
{$define RGB_RED_IS_0}          { RGB byte order in JQUANT2 }

{$undef NO_ZERO_ROW_TEST}

{$define USE_MSDOS_MEMMGR}      { Define this if you use jmemdos.c }
{$define XMS_SUPPORTED}
{$define EMS_SUPPORTED}

{$undef MEM_STATS}              { Write out memory usage }
{$define AM_MEMORY_MANAGER}     { we define jvirt_Xarray_control structs }

{$undef FULL_MAIN_BUFFER_SUPPORTED}

{$define PROGRESS_REPORT}
{$define TWO_FILE_COMMANDLINE}
{$define BMP_SUPPORTED}
{$define PPM_SUPPORTED}
{$undef GIF_SUPPORTED}
{$undef RLE_SUPPORTED}
{$define TARGA_SUPPORTED}
{$define EXT_SWITCH}

{$ifndef BITS_IN_JSAMPLE_IS_8}  { for 12 bit samples }
{$undef BMP_SUPPORTED}
{$undef RLE_SUPPORTED}
{$undef TARGA_SUPPORTED}
{$endif}

{$undef BASM16}                  { for TP7 - use BASM for fast multiply }
{$ifdef Win32}
  {$ifndef FPC}
    {$define BASM}                 { jidctint with BASM for Delphi 2/3 }
    {$undef RGB_RED_IS_0}          { BGR byte order in JQUANT2 }
  {$endif}
{$endif}

{$IFDEF FPC}
{$MODE DELPHI}
{$GOTO ON}
{$DEFINE DELPHI_STREAM}
{$ENDIF}
