Unit JPEGLib;

{ This file defines the application interface for the JPEG library.
  Most applications using the library need only include this file,
  and perhaps jerror.h if they want to know the exact error codes. }

{ Source:jpeglib.h+jpegint.h; Copyright (C) 1991-1998, Thomas G. Lane. }


interface

{$I jconfig.inc}

{ First we include the configuration files that record how this
  installation of the JPEG library is set up.  jconfig.h can be
  generated automatically for many systems.  jmorecfg.h contains
  manual configuration options that most people need not worry about. }

uses
  jdeferr,
  jmorecfg;                     { seldom changed options }

{ Version ID for the JPEG library.
  Might be useful for tests like "#if JPEG_LIB_VERSION >= 60". }


Const
  JPEG_LIB_VERSION = 62;        { Version 6b }


{ These marker codes are exported since applications and data source modules
  are likely to want to use them. }

const
  JPEG_RST0     = $D0;  { RST0 marker code }
  JPEG_EOI      = $D9;  { EOI marker code }
  JPEG_APP0     = $E0;  { APP0 marker code }
  JPEG_COM      = $FE;  { COM marker code }


{ Various constants determining the sizes of things.
  All of these are specified by the JPEG standard, so don't change them
  if you want to be compatible. }

const
  DCTSIZE             = 8;      { The basic DCT block is 8x8 samples }
  DCTSIZE2            = 64;     { DCTSIZE squared; # of elements in a block }
  NUM_QUANT_TBLS      = 4;      { Quantization tables are numbered 0..3 }
  NUM_HUFF_TBLS       = 4;      { Huffman tables are numbered 0..3 }
  NUM_ARITH_TBLS      = 16;     { Arith-coding tables are numbered 0..15 }
  MAX_COMPS_IN_SCAN   = 4;      { JPEG limit on # of components in one scan }
  MAX_SAMP_FACTOR     = 4;      { JPEG limit on sampling factors }
{ Unfortunately, some bozo at Adobe saw no reason to be bound by the standard;
  the PostScript DCT filter can emit files with many more than 10 blocks/MCU.
  If you happen to run across such a file, you can up D_MAX_BLOCKS_IN_MCU
  to handle it.  We even let you do this from the jconfig.h file.  However,
  we strongly discourage changing C_MAX_BLOCKS_IN_MCU; just because Adobe
  sometimes emits noncompliant files doesn't mean you should too. }
  C_MAX_BLOCKS_IN_MCU = 10;     { compressor's limit on blocks per MCU }
  D_MAX_BLOCKS_IN_MCU = 10;     { decompressor's limit on blocks per MCU }


{ Data structures for images (arrays of samples and of DCT coefficients).
  On 80x86 machines, the image arrays are too big for near pointers,
  but the pointer arrays can fit in near memory. }

type
{ for typecasting }
  JSAMPLE_PTR = ^JSAMPLE;
  JSAMPROW_PTR = ^JSAMPROW;
  JBLOCKROW_PTR = ^JBLOCKROW;

  jTSample = 0..(MaxInt div SIZEOF(JSAMPLE))-1;
  JSAMPLE_ARRAY = Array[jTSample] of JSAMPLE;  {far}
  JSAMPROW = ^JSAMPLE_ARRAY;  { ptr to one image row of pixel samples. }

  jTRow = 0..(MaxInt div SIZEOF(JSAMPROW))-1;
  JSAMPROW_ARRAY = Array[jTRow] of JSAMPROW;
  JSAMPARRAY = ^JSAMPROW_ARRAY;  { ptr to some rows (a 2-D sample array) }

  jTArray = 0..(MaxInt div SIZEOF(JSAMPARRAY))-1;
  JSAMP_ARRAY = Array[jTArray] of JSAMPARRAY;
  JSAMPIMAGE = ^JSAMP_ARRAY;  { a 3-D sample array: top index is color }

  JBLOCK = Array[0..DCTSIZE2-1] of JCOEF;   { one block of coefficients }
  JBLOCK_PTR = ^JBLOCK;

  jTBlockRow = 0..(MaxInt div SIZEOF(JBLOCK))-1;
  JBLOCK_ROWS = Array[jTBlockRow] of JBLOCK;
  JBLOCKROW = ^JBLOCK_ROWS; {far} { pointer to one row of coefficient blocks }


  jTBlockArray = 0..(MaxInt div SIZEOF(JBLOCKROW))-1;
  JBLOCK_ARRAY = Array[jTBlockArray] of JBLOCKROW;
  JBLOCKARRAY = ^JBLOCK_ARRAY;    { a 2-D array of coefficient blocks }

  jTBlockImage = 0..(MaxInt div SIZEOF(JBLOCKARRAY))-1;
  JBLOCK_IMAGE = Array[jTBlockImage] of JBLOCKARRAY;
  JBLOCKIMAGE = ^JBLOCK_IMAGE;   { a 3-D array of coefficient blocks }

  jTCoef = 0..(MaxInt div SIZEOF(JCOEF))-1;
  JCOEF_ROW = Array[jTCoef] of JCOEF;
  JCOEFPTR = ^JCOEF_ROW; {far}   { useful in a couple of places }


type
  jTByte = 0..(MaxInt div SIZEOF(byte))-1;
  JByteArray = Array[jTByte] of byte;
  JBytePtr = ^JByteArray;
type
  byteptr = ^byte;

{ Types for JPEG compression parameters and working tables. }


{ DCT coefficient quantization tables. }

type
  JQUANT_TBL_PTR = ^JQUANT_TBL;
  JQUANT_TBL = record
  { This array gives the coefficient quantizers in natural array order
    (not the zigzag order in which they are stored in a JPEG DQT marker).
    CAUTION: IJG versions prior to v6a kept this array in zigzag order. }
    quantval : Array[0..DCTSIZE2-1] of UINT16;
                               { quantization step for each coefficient }
  { This field is used only during compression.  It's initialized FALSE when
    the table is created, and set TRUE when it's been output to the file.
    You could suppress output of a table by setting this to TRUE.
    (See jpeg_suppress_tables for an example.) }
    sent_table : boolean;      { TRUE when table has been output }
  end;
  JQUANT_TBL_FIELD = Array[0..(MaxInt div SizeOf(JQUANT_TBL))-1] of JQUANT_TBL;

{ Huffman coding tables. }

type
  JHUFF_TBL_PTR = ^JHUFF_TBL;
  JHUFF_TBL = record
  { These two fields directly represent the contents of a JPEG DHT marker }
    bits : Array[0..17-1] of UINT8; { bits[k] = # of symbols with codes of }
                                    { length k bits; bits[0] is unused }
    huffval : Array[0..256-1] of UINT8;
                                    { The symbols, in order of incr code length }
  { This field is used only during compression.  It's initialized FALSE when
    the table is created, and set TRUE when it's been output to the file.
    You could suppress output of a table by setting this to TRUE.
    (See jpeg_suppress_tables for an example.) }
    sent_table : boolean;           { TRUE when table has been output }
  end;
  JHUFF_TBL_FIELD = Array[0..(MaxInt div SizeOf(JHUFF_TBL))-1] of JHUFF_TBL;

{ Declarations for both compression & decompression }

type
  J_BUF_MODE = (                { Operating modes for buffer controllers }
        JBUF_PASS_THRU,         { Plain stripwise operation }
        { Remaining modes require a full-image buffer to have been created }
        JBUF_SAVE_SOURCE,       { Run source subobject only, save output }
        JBUF_CRANK_DEST,        { Run dest subobject only, using saved data }
        JBUF_SAVE_AND_PASS      { Run both subobjects, save output }
               );

{ Values of global_state field (jdapi.c has some dependencies on ordering!) }
const
  CSTATE_START        = 100;    { after create_compress }
  CSTATE_SCANNING     = 101;    { start_compress done, write_scanlines OK }
  CSTATE_RAW_OK       = 102;    { start_compress done, write_raw_data OK }
  CSTATE_WRCOEFS      = 103;    { jpeg_write_coefficients done }
  DSTATE_START        = 200;    { after create_decompress }
  DSTATE_INHEADER     = 201;    { reading header markers, no SOS yet }
  DSTATE_READY        = 202;    { found SOS, ready for start_decompress }
  DSTATE_PRELOAD      = 203;    { reading multiscan file in start_decompress}
  DSTATE_PRESCAN      = 204;    { performing dummy pass for 2-pass quant }
  DSTATE_SCANNING     = 205;    { start_decompress done, read_scanlines OK }
  DSTATE_RAW_OK       = 206;    { start_decompress done, read_raw_data OK }
  DSTATE_BUFIMAGE     = 207;    { expecting jpeg_start_output }
  DSTATE_BUFPOST      = 208;    { looking for SOS/EOI in jpeg_finish_output }
  DSTATE_RDCOEFS      = 209;    { reading file in jpeg_read_coefficients }
  DSTATE_STOPPING     = 210;    { looking for EOI in jpeg_finish_decompress }



{ Basic info about one component (color channel). }

type
  jpeg_component_info_ptr = ^jpeg_component_info;
  jpeg_component_info = record
    { These values are fixed over the whole image. }
    { For compression, they must be supplied by parameter setup; }
    { for decompression, they are read from the SOF marker. }
    component_id : int;           { identifier for this component (0..255) }
    component_index : int;        { its index in SOF or cinfo^.comp_info[] }
    h_samp_factor : int;          { horizontal sampling factor (1..4) }
    v_samp_factor : int;          { vertical sampling factor (1..4) }
    quant_tbl_no : int;           { quantization table selector (0..3) }
    { These values may vary between scans. }
    { For compression, they must be supplied by parameter setup; }
    { for decompression, they are read from the SOS marker. }
    { The decompressor output side may not use these variables. }
    dc_tbl_no : int;              { DC entropy table selector (0..3) }
    ac_tbl_no : int;              { AC entropy table selector (0..3) }

    { Remaining fields should be treated as private by applications. }

    { These values are computed during compression or decompression startup: }
    { Component's size in DCT blocks.
      Any dummy blocks added to complete an MCU are not counted; therefore
      these values do not depend on whether a scan is interleaved or not. }
    width_in_blocks : JDIMENSION;
    height_in_blocks : JDIMENSION;
    { Size of a DCT block in samples.  Always DCTSIZE for compression.
      For decompression this is the size of the output from one DCT block,
      reflecting any scaling we choose to apply during the IDCT step.
      Values of 1,2,4,8 are likely to be supported.  Note that different
      components may receive different IDCT scalings. }

    DCT_scaled_size : int;
    { The downsampled dimensions are the component's actual, unpadded number
      of samples at the main buffer (preprocessing/compression interface), thus
      downsampled_width = ceil(image_width * Hi/Hmax)
      and similarly for height.  For decompression, IDCT scaling is included, so
      downsampled_width = ceil(image_width * Hi/Hmax * DCT_scaled_size/DCTSIZE)}

    downsampled_width : JDIMENSION;        { actual width in samples }
    downsampled_height : JDIMENSION;       { actual height in samples }
    { This flag is used only for decompression.  In cases where some of the
      components will be ignored (eg grayscale output from YCbCr image),
      we can skip most computations for the unused components. }

    component_needed : boolean;     { do we need the value of this component? }

    { These values are computed before starting a scan of the component. }
    { The decompressor output side may not use these variables. }
    MCU_width : int;      { number of blocks per MCU, horizontally }
    MCU_height : int;     { number of blocks per MCU, vertically }
    MCU_blocks : int;     { MCU_width * MCU_height }
    MCU_sample_width : int;       { MCU width in samples, MCU_width*DCT_scaled_size }
    last_col_width : int;         { # of non-dummy blocks across in last MCU }
    last_row_height : int;        { # of non-dummy blocks down in last MCU }

    { Saved quantization table for component; NIL if none yet saved.
      See jdinput.c comments about the need for this information.
      This field is currently used only for decompression. }

    quant_table : JQUANT_TBL_PTR;

    { Private per-component storage for DCT or IDCT subsystem. }
    dct_table : pointer;
  end; { record jpeg_component_info }

  jTCinfo = 0..(MaxInt div SizeOf(jpeg_component_info))-1;
  jpeg_component_info_array = array[jTCinfo] of jpeg_component_info;
  jpeg_component_info_list_ptr = ^jpeg_component_info_array;


{ The script for encoding a multiple-scan file is an array of these: }

type
  jpeg_scan_info_ptr = ^jpeg_scan_info;
  jpeg_scan_info = record
    comps_in_scan : int;                { number of components encoded in this scan }
    component_index : Array[0..MAX_COMPS_IN_SCAN-1] of int;
                                        { their SOF/comp_info[] indexes }
    Ss, Se : int;                       { progressive JPEG spectral selection parms }
    Ah, Al : int;                       { progressive JPEG successive approx. parms }
  end;

{ The decompressor can save APPn and COM markers in a list of these: }

type
  jpeg_saved_marker_ptr = ^jpeg_marker_struct;
  jpeg_marker_struct = record
    next : jpeg_saved_marker_ptr;    { next in list, or NULL }
    marker : UINT8;                  { marker code: JPEG_COM, or JPEG_APP0+n }
    original_length : uint;          { # bytes of data in the file }
    data_length : uint;              { # bytes of data saved at data[] }
    data : JOCTET_FIELD_PTR;         { the data contained in the marker }
   { the marker length word is not counted in data_length or original_length }
  end;

{ Known color spaces. }

type
  J_COLOR_SPACE = (
        JCS_UNKNOWN,            { error/unspecified }
        JCS_GRAYSCALE,          { monochrome }
        JCS_RGB,                { red/green/blue }
        JCS_YCbCr,              { Y/Cb/Cr (also known as YUV) }
        JCS_CMYK,               { C/M/Y/K }
        JCS_YCCK                { Y/Cb/Cr/K }
                  );

{ DCT/IDCT algorithm options. }

type
  J_DCT_METHOD = (
        JDCT_ISLOW,             { slow but accurate integer algorithm }
        JDCT_IFAST,             { faster, less accurate integer method }
        JDCT_FLOAT              { floating-point: accurate, fast on fast HW }
                 );

const
  JDCT_DEFAULT = JDCT_ISLOW;
  JDCT_FASTEST = JDCT_IFAST;

{ Dithering options for decompression. }

type
  J_DITHER_MODE = (
    JDITHER_NONE,               { no dithering }
    JDITHER_ORDERED,            { simple ordered dither }
    JDITHER_FS                  { Floyd-Steinberg error diffusion dither }
                  );


const
  JPOOL_PERMANENT  = 0; { lasts until master record is destroyed }
  JPOOL_IMAGE      = 1; { lasts until done with image/datastream }
  JPOOL_NUMPOOLS   = 2;


{ "Object" declarations for JPEG modules that may be supplied or called
  directly by the surrounding application.
  As with all objects in the JPEG library, these structs only define the
  publicly visible methods and state variables of a module.  Additional
  private fields may exist after the public ones. }


{ Error handler object }

const
  JMSG_LENGTH_MAX  = 200;  { recommended size of format_message buffer }
  JMSG_STR_PARM_MAX = 80;

const
  TEMP_NAME_LENGTH = 64;   { max length of a temporary file's name }
type
  TEMP_STRING = string[TEMP_NAME_LENGTH];

{$ifdef USE_MSDOS_MEMMGR}  { DOS-specific junk }
type
  XMSH = ushort;           { type of extended-memory handles }
  EMSH = ushort;           { type of expanded-memory handles }

  handle_union = record
    case byte of
    0:(file_handle : short);    { DOS file handle if it's a temp file }
    1:(xms_handle : XMSH);      { handle if it's a chunk of XMS }
    2:(ems_handle : EMSH);      { handle if it's a chunk of EMS }
  end;
{$endif} { USE_MSDOS_MEMMGR }

type
  jpeg_error_mgr_ptr = ^jpeg_error_mgr;
  jpeg_memory_mgr_ptr = ^jpeg_memory_mgr;
  jpeg_progress_mgr_ptr = ^jpeg_progress_mgr;


{$ifdef common}
{ Common fields between JPEG compression and decompression master structs. }
    err : jpeg_error_mgr_ptr;            { Error handler module }
    mem : jpeg_memory_mgr_ptr;           { Memory manager module }
    progress : jpeg_progress_mgr_ptr;    { Progress monitor, or NIL if none }
    client_data : voidp;                 { Available for use by application }
    is_decompressor : boolean;     { so common code can tell which is which }
    global_state : int;            { for checking call sequence validity }
{$endif}

  j_common_ptr = ^jpeg_common_struct;
  j_compress_ptr = ^jpeg_compress_struct;
  j_decompress_ptr = ^jpeg_decompress_struct;

  {$ifdef AM_MEMORY_MANAGER}    { only jmemmgr.c defines these }

{ This structure holds whatever state is needed to access a single
  backing-store object.  The read/write/close method pointers are called
  by jmemmgr.c to manipulate the backing-store object; all other fields
  are private to the system-dependent backing store routines. }


  backing_store_ptr = ^backing_store_info;
  backing_store_info = record
  { Methods for reading/writing/closing this backing-store object }
    read_backing_store : procedure (cinfo : j_common_ptr;
                                    info : backing_store_ptr;
                                    buffer_address : pointer; {far}
                                    file_offset : long;
                                    byte_count : long);
    write_backing_store : procedure (cinfo : j_common_ptr;
                                     info : backing_store_ptr;
                                     buffer_address : pointer;  {far}
                                     file_offset : long;
                                     byte_count : long);

    close_backing_store : procedure (cinfo : j_common_ptr;
                                     info : backing_store_ptr);

  { Private fields for system-dependent backing-store management }
  {$ifdef USE_MSDOS_MEMMGR}
    { For the MS-DOS manager (jmemdos.c), we need: }
    handle : handle_union;            { reference to backing-store storage object }
    temp_name : TEMP_STRING;  { name if it's a file }
  {$else}
    { For a typical implementation with temp files, we need: }
    temp_file : file;                 { stdio reference to temp file }
    temp_name : TEMP_STRING;  { name of temp file }
  {$endif}
 end;


{ The control blocks for virtual arrays.
  Note that these blocks are allocated in the "small" pool area.
  System-dependent info for the associated backing store (if any) is hidden
  inside the backing_store_info struct. }

  jvirt_sarray_ptr = ^jvirt_sarray_control;
  jvirt_sarray_control = record
    mem_buffer : JSAMPARRAY;    { => the in-memory buffer }
    rows_in_array : JDIMENSION; { total virtual array height }
    samplesperrow : JDIMENSION; { width of array (and of memory buffer) }
    maxaccess : JDIMENSION;     { max rows accessed by access_virt_sarray }
    rows_in_mem : JDIMENSION;   { height of memory buffer }
    rowsperchunk : JDIMENSION;  { allocation chunk size in mem_buffer }
    cur_start_row : JDIMENSION; { first logical row # in the buffer }
    first_undef_row : JDIMENSION; { row # of first uninitialized row }
    pre_zero : boolean;         { pre-zero mode requested? }
    dirty : boolean;            { do current buffer contents need written? }
    b_s_open : boolean;         { is backing-store data valid? }
    next : jvirt_sarray_ptr;    { link to next virtual sarray control block }
    b_s_info : backing_store_info; { System-dependent control info }
  end;

  jvirt_barray_ptr = ^jvirt_barray_control;
  jvirt_barray_control = record
    mem_buffer : JBLOCKARRAY;   { => the in-memory buffer }
    rows_in_array : JDIMENSION; { total virtual array height }
    blocksperrow : JDIMENSION;  { width of array (and of memory buffer) }
    maxaccess : JDIMENSION;     { max rows accessed by access_virt_barray }
    rows_in_mem : JDIMENSION;   { height of memory buffer }
    rowsperchunk : JDIMENSION;  { allocation chunk size in mem_buffer }
    cur_start_row : JDIMENSION; { first logical row # in the buffer }
    first_undef_row : JDIMENSION; { row # of first uninitialized row }
    pre_zero : boolean;         { pre-zero mode requested? }
    dirty : boolean;            { do current buffer contents need written? }
    b_s_open : boolean;         { is backing-store data valid? }
    next : jvirt_barray_ptr;    { link to next virtual barray control block }
    b_s_info : backing_store_info;  { System-dependent control info }
  end;

  {$endif} { AM_MEMORY_MANAGER }

{ Declarations for compression modules }

{ Master control module }
  jpeg_comp_master_ptr = ^jpeg_comp_master;
  jpeg_comp_master = record
    prepare_for_pass : procedure(cinfo : j_compress_ptr);
    pass_startup : procedure(cinfo : j_compress_ptr);
    finish_pass : procedure(cinfo : j_compress_ptr);

    { State variables made visible to other modules }
    call_pass_startup : Boolean;   { True if pass_startup must be called }
    is_last_pass : Boolean;        { True during last pass }
  end;

{ Main buffer control (downsampled-data buffer) }
  jpeg_c_main_controller_ptr = ^jpeg_c_main_controller;
  jpeg_c_main_controller = record
    start_pass : procedure(cinfo : j_compress_ptr; pass_mode : J_BUF_MODE);
    process_data : procedure(cinfo : j_compress_ptr;
                             input_buf : JSAMPARRAY;
                             var in_row_ctr : JDIMENSION;
                             in_rows_avail : JDIMENSION);
  end;

{ Compression preprocessing (downsampling input buffer control) }
  jpeg_c_prep_controller_ptr = ^jpeg_c_prep_controller;
  jpeg_c_prep_controller = record
    start_pass : procedure(cinfo : j_compress_ptr; pass_mode : J_BUF_MODE);
    pre_process_data : procedure(cinfo : j_compress_ptr;
                                 input_buf : JSAMPARRAY;
                                 var in_row_ctr : JDIMENSION;
                                 in_rows_avail : JDIMENSION;
                                 output_buf : JSAMPIMAGE;
                                 var out_row_group_ctr : JDIMENSION;
                                 out_row_groups_avail : JDIMENSION);
  end;

{ Coefficient buffer control }
  jpeg_c_coef_controller_ptr = ^jpeg_c_coef_controller;
  jpeg_c_coef_controller = record
    start_pass : procedure(cinfo : j_compress_ptr; pass_mode : J_BUF_MODE);
    compress_data : function(cinfo : j_compress_ptr;
                             input_buf : JSAMPIMAGE) : boolean;
  end;

{ Colorspace conversion }
  jpeg_color_converter_ptr = ^jpeg_color_converter;
  jpeg_color_converter = record
    start_pass : procedure(cinfo : j_compress_ptr);
    color_convert : procedure(cinfo : j_compress_ptr;
                              input_buf : JSAMPARRAY;
                              output_buf : JSAMPIMAGE;
                              output_row : JDIMENSION;
                              num_rows : int);
  end;

{ Downsampling }
  jpeg_downsampler_ptr = ^jpeg_downsampler;
  jpeg_downsampler = record
    start_pass : procedure(cinfo : j_compress_ptr);
    downsample : procedure(cinfo : j_compress_ptr;
                           input_buf : JSAMPIMAGE;
                           in_row_index :  JDIMENSION;
                           output_buf : JSAMPIMAGE;
                           out_row_group_index: JDIMENSION);
    need_context_rows : Boolean;  { TRUE if need rows above & below }
  end;

{ Forward DCT (also controls coefficient quantization) }
  jpeg_forward_dct_ptr = ^jpeg_forward_dct;
  jpeg_forward_dct = record
    start_pass : procedure(cinfo : j_compress_ptr);
    { perhaps this should be an array??? }
    forward_DCT : procedure(cinfo : j_compress_ptr;
                            compptr : jpeg_component_info_ptr;
                            sample_data : JSAMPARRAY;
                            coef_blocks : JBLOCKROW;
                            start_row : JDIMENSION;
                            start_col : JDIMENSION;
                            num_blocks : JDIMENSION);
  end;

{ Entropy encoding }

  jpeg_entropy_encoder_ptr = ^jpeg_entropy_encoder;
  jpeg_entropy_encoder = record
    start_pass : procedure(cinfo : j_compress_ptr; gather_statistics : boolean);
    encode_mcu : function(cinfo : j_compress_ptr;
                          const MCU_data: array of JBLOCKROW) : boolean;
    finish_pass : procedure(cinfo : j_compress_ptr);
  end;

{ Marker writing }
  jpeg_marker_writer_ptr = ^jpeg_marker_writer;
  jpeg_marker_writer = record
    write_file_header : procedure(cinfo : j_compress_ptr);
    write_frame_header : procedure(cinfo : j_compress_ptr);
    write_scan_header : procedure(cinfo : j_compress_ptr);
    write_file_trailer : procedure(cinfo : j_compress_ptr);
    write_tables_only : procedure(cinfo : j_compress_ptr);
   { These routines are exported to allow insertion of extra markers }
   { Probably only COM and APPn markers should be written this way }
    write_marker_header : procedure (cinfo : j_compress_ptr;
                                     marker : int;
                                     datalen : uint);
    write_marker_byte : procedure (cinfo : j_compress_ptr; val : int);
  end;

{ Declarations for decompression modules }

{ Master control module }
  jpeg_decomp_master_ptr = ^jpeg_decomp_master;
  jpeg_decomp_master = record
    prepare_for_output_pass : procedure( cinfo : j_decompress_ptr);
    finish_output_pass : procedure(cinfo : j_decompress_ptr);

    { State variables made visible to other modules }
    is_dummy_pass : Boolean;    { True during 1st pass for 2-pass quant }
  end;

{ Input control module }
  jpeg_input_controller_ptr = ^jpeg_input_controller;
  jpeg_input_controller = record
    consume_input : function (cinfo : j_decompress_ptr) : int;
    reset_input_controller : procedure(cinfo : j_decompress_ptr);
    start_input_pass : procedure(cinfo : j_decompress_ptr);
    finish_input_pass : procedure(cinfo : j_decompress_ptr);

    { State variables made visible to other modules }
    has_multiple_scans : Boolean;  { True if file has multiple scans }
    eoi_reached : Boolean;         { True when EOI has been consumed }
  end;

{ Main buffer control (downsampled-data buffer) }

  jpeg_d_main_controller_ptr = ^jpeg_d_main_controller;
  jpeg_d_main_controller = record
    start_pass : procedure(cinfo : j_decompress_ptr; pass_mode : J_BUF_MODE);
    process_data : procedure(cinfo : j_decompress_ptr;
                             output_buf : JSAMPARRAY;
                             var out_row_ctr : JDIMENSION;
                             out_rows_avail : JDIMENSION);
  end;

{ Coefficient buffer control }
  jvirt_barray_tbl = array[0..MAX_COMPONENTS-1] of jvirt_barray_ptr;
  jvirt_barray_tbl_ptr = ^jvirt_barray_tbl;
  jpeg_d_coef_controller_ptr = ^jpeg_d_coef_controller;
  jpeg_d_coef_controller = record
    start_input_pass : procedure(cinfo : j_decompress_ptr);
    consume_data : function (cinfo : j_decompress_ptr) : int;
    start_output_pass : procedure(cinfo : j_decompress_ptr);
    decompress_data : function (cinfo : j_decompress_ptr;
                                output_buf : JSAMPIMAGE) : int;
  { Pointer to array of coefficient virtual arrays, or NIL if none }
    coef_arrays : jvirt_barray_tbl_ptr;
  end;

{ Decompression postprocessing (color quantization buffer control) }
  jpeg_d_post_controller_ptr = ^jpeg_d_post_controller;
  jpeg_d_post_controller = record
    start_pass : procedure(cinfo : j_decompress_ptr;
                           pass_mode : J_BUF_MODE);
    post_process_data : procedure(cinfo : j_decompress_ptr;
                                  input_buf : JSAMPIMAGE;
                                  var in_row_group_ctr : JDIMENSION;
                                  in_row_groups_avail : JDIMENSION;
                                  output_buf : JSAMPARRAY;
                                  var out_row_ctr : JDIMENSION;
                                  out_rows_avail : JDIMENSION);
  end;


{ Routine signature for application-supplied marker processing methods.
  Need not pass marker code since it is stored in cinfo^.unread_marker. }

  jpeg_marker_parser_method = function(cinfo : j_decompress_ptr) : boolean;

{ Marker reading & parsing }
  jpeg_marker_reader_ptr = ^jpeg_marker_reader;
  jpeg_marker_reader = record
    reset_marker_reader : procedure(cinfo : j_decompress_ptr);
    { Read markers until SOS or EOI.
      Returns same codes as are defined for jpeg_consume_input:
      JPEG_SUSPENDED, JPEG_REACHED_SOS, or JPEG_REACHED_EOI. }

    read_markers : function (cinfo : j_decompress_ptr) : int;
    { Read a restart marker --- exported for use by entropy decoder only }
    read_restart_marker : jpeg_marker_parser_method;

    { State of marker reader --- nominally internal, but applications
      supplying COM or APPn handlers might like to know the state. }

    saw_SOI : boolean;            { found SOI? }
    saw_SOF : boolean;            { found SOF? }
    next_restart_num : int;       { next restart number expected (0-7) }
    discarded_bytes : uint;       { # of bytes skipped looking for a marker }
  end;

{ Entropy decoding }
  jpeg_entropy_decoder_ptr = ^jpeg_entropy_decoder;
  jpeg_entropy_decoder = record
    start_pass : procedure(cinfo : j_decompress_ptr);
    decode_mcu : function(cinfo : j_decompress_ptr;
                          var MCU_data : array of JBLOCKROW) : boolean;
  { This is here to share code between baseline and progressive decoders; }
  { other modules probably should not use it }
    insufficient_data : BOOLEAN;  { set TRUE after emitting warning }
  end;

{ Inverse DCT (also performs dequantization) }
  inverse_DCT_method_ptr = procedure(cinfo : j_decompress_ptr;
                 compptr : jpeg_component_info_ptr;
                 coef_block : JCOEFPTR;
                 output_buf : JSAMPARRAY; output_col : JDIMENSION);

  jpeg_inverse_dct_ptr = ^jpeg_inverse_dct;
  jpeg_inverse_dct = record
    start_pass : procedure(cinfo : j_decompress_ptr);
    { It is useful to allow each component to have a separate IDCT method. }
    inverse_DCT : Array[0..MAX_COMPONENTS-1] of inverse_DCT_method_ptr;
  end;

{ Upsampling (note that upsampler must also call color converter) }
  jpeg_upsampler_ptr = ^jpeg_upsampler;
  jpeg_upsampler = record
    start_pass : procedure(cinfo : j_decompress_ptr);
    upsample : procedure(cinfo : j_decompress_ptr;
                   input_buf : JSAMPIMAGE;
                   var in_row_group_ctr : JDIMENSION;  { array of }
                   in_row_groups_avail : JDIMENSION;
                   output_buf : JSAMPARRAY;
                   var out_row_ctr : JDIMENSION;
                   out_rows_avail : JDIMENSION);

    need_context_rows : boolean;  { TRUE if need rows above & below }
  end;

{ Colorspace conversion }
  jpeg_color_deconverter_ptr = ^jpeg_color_deconverter;
  jpeg_color_deconverter = record
    start_pass : procedure(cinfo: j_decompress_ptr);
    color_convert : procedure(cinfo : j_decompress_ptr;
                              input_buf : JSAMPIMAGE;
                              input_row : JDIMENSION;
                              output_buf : JSAMPARRAY;
                              num_rows : int);
  end;

{ Color quantization or color precision reduction }
  jpeg_color_quantizer_ptr = ^jpeg_color_quantizer;
  jpeg_color_quantizer = record
    start_pass : procedure(cinfo : j_decompress_ptr; is_pre_scan : boolean);
    color_quantize : procedure(cinfo : j_decompress_ptr;
                               input_buf : JSAMPARRAY;
                               output_buf : JSAMPARRAY;
                               num_rows : int);

    finish_pass : procedure(cinfo : j_decompress_ptr);
    new_color_map : procedure(cinfo : j_decompress_ptr);
  end;

  {int8array = Array[0..8-1] of int;}
  int8array = Array[0..8-1] of longint; { for TP FormatStr }

  jpeg_error_mgr = record
    { Error exit handler: does not return to caller }
    error_exit : procedure  (cinfo : j_common_ptr);
    { Conditionally emit a trace or warning message }
    emit_message : procedure (cinfo : j_common_ptr; msg_level : int);
    { Routine that actually outputs a trace or error message }
    output_message : procedure (cinfo : j_common_ptr);
    { Format a message string for the most recent JPEG error or message }
    format_message : procedure  (cinfo : j_common_ptr; var buffer : string);

    { Reset error state variables at start of a new image }
    reset_error_mgr : procedure (cinfo : j_common_ptr);

    { The message ID code and any parameters are saved here.
      A message can have one string parameter or up to 8 int parameters. }

    msg_code : int;

    msg_parm : record
      case byte of
      0:(i : int8array);
      1:(s : string[JMSG_STR_PARM_MAX]);
    end;

    { Standard state variables for error facility }

    trace_level : int;         { max msg_level that will be displayed }

    { For recoverable corrupt-data errors, we emit a warning message,
      but keep going unless emit_message chooses to abort.  emit_message
      should count warnings in num_warnings.  The surrounding application
      can check for bad data by seeing if num_warnings is nonzero at the
      end of processing. }

    num_warnings : long;       { number of corrupt-data warnings }

    { These fields point to the table(s) of error message strings.
      An application can change the table pointer to switch to a different
      message list (typically, to change the language in which errors are
      reported).  Some applications may wish to add additional error codes
      that will be handled by the JPEG library error mechanism; the second
      table pointer is used for this purpose.

      First table includes all errors generated by JPEG library itself.
      Error code 0 is reserved for a "no such error string" message. }

    {const char * const * jpeg_message_table; }
    jpeg_message_table : ^msg_table; { Library errors }

    last_jpeg_message : J_MESSAGE_CODE;
      { Table contains strings 0..last_jpeg_message }
    { Second table can be added by application (see cjpeg/djpeg for example).
      It contains strings numbered first_addon_message..last_addon_message. }

    {const char * const * addon_message_table; }
    addon_message_table : ^msg_table; { Non-library errors }

    first_addon_message : J_MESSAGE_CODE;  { code for first string in addon table }
    last_addon_message : J_MESSAGE_CODE;   { code for last string in addon table }
  end;


{ Progress monitor object }

  jpeg_progress_mgr = record
    progress_monitor : procedure(cinfo : j_common_ptr);

    pass_counter : long;        { work units completed in this pass }
    pass_limit : long;          { total number of work units in this pass }
    completed_passes : int;     { passes completed so far }
    total_passes : int;         { total number of passes expected }
  end;


{ Data destination object for compression }
  jpeg_destination_mgr_ptr = ^jpeg_destination_mgr;
  jpeg_destination_mgr = record
    next_output_byte : JOCTETptr;  { => next byte to write in buffer }
    free_in_buffer : size_t;    { # of byte spaces remaining in buffer }

    init_destination : procedure (cinfo : j_compress_ptr);
    empty_output_buffer : function (cinfo : j_compress_ptr) : boolean;
    term_destination : procedure (cinfo : j_compress_ptr);
  end;


{ Data source object for decompression }

  jpeg_source_mgr_ptr = ^jpeg_source_mgr;
  jpeg_source_mgr = record
    {const JOCTET * next_input_byte;}
    next_input_byte : JOCTETptr;      { => next byte to read from buffer }
    bytes_in_buffer : size_t;       { # of bytes remaining in buffer }

    init_source : procedure  (cinfo : j_decompress_ptr);
    fill_input_buffer : function (cinfo : j_decompress_ptr) : boolean;
    skip_input_data : procedure (cinfo : j_decompress_ptr; num_bytes : long);
    resync_to_restart : function (cinfo : j_decompress_ptr;
                                  desired : int) : boolean;
    term_source : procedure (cinfo : j_decompress_ptr);
  end;


{ Memory manager object.
  Allocates "small" objects (a few K total), "large" objects (tens of K),
  and "really big" objects (virtual arrays with backing store if needed).
  The memory manager does not allow individual objects to be freed; rather,
  each created object is assigned to a pool, and whole pools can be freed
  at once.  This is faster and more convenient than remembering exactly what
  to free, especially where malloc()/free() are not too speedy.
  NB: alloc routines never return NIL.  They exit to error_exit if not
  successful. }


  jpeg_memory_mgr = record
    { Method pointers }
    alloc_small : function (cinfo : j_common_ptr; pool_id : int;
                                  sizeofobject : size_t) : pointer;
    alloc_large : function (cinfo : j_common_ptr; pool_id : int;
                                  sizeofobject : size_t) : pointer; {far}
    alloc_sarray : function (cinfo : j_common_ptr; pool_id : int;
                             samplesperrow : JDIMENSION;
                             numrows : JDIMENSION) : JSAMPARRAY;

    alloc_barray : function (cinfo : j_common_ptr; pool_id : int;
                             blocksperrow : JDIMENSION;
                             numrows : JDIMENSION) : JBLOCKARRAY;

    request_virt_sarray : function(cinfo : j_common_ptr;
                                   pool_id : int;
                                   pre_zero : boolean;
                                   samplesperrow : JDIMENSION;
                                   numrows : JDIMENSION;
                                   maxaccess : JDIMENSION) : jvirt_sarray_ptr;

    request_virt_barray : function(cinfo : j_common_ptr;
                                   pool_id : int;
                                   pre_zero : boolean;
                                   blocksperrow : JDIMENSION;
                                   numrows : JDIMENSION;
                                   maxaccess : JDIMENSION) : jvirt_barray_ptr;

    realize_virt_arrays : procedure (cinfo : j_common_ptr);

    access_virt_sarray : function (cinfo : j_common_ptr;
                                   ptr : jvirt_sarray_ptr;
                                   start_row : JDIMENSION;
                                   num_rows : JDIMENSION;
                                   writable : boolean) : JSAMPARRAY;

    access_virt_barray : function (cinfo : j_common_ptr;
                                   ptr : jvirt_barray_ptr;
                                   start_row : JDIMENSION;
                                   num_rows : JDIMENSION;
                                   writable : boolean) : JBLOCKARRAY;

    free_pool : procedure  (cinfo : j_common_ptr; pool_id : int);
    self_destruct : procedure (cinfo : j_common_ptr);

    { Limit on memory allocation for this JPEG object.  (Note that this is
      merely advisory, not a guaranteed maximum; it only affects the space
      used for virtual-array buffers.)  May be changed by outer application
      after creating the JPEG object. }
    max_memory_to_use : long;

    { Maximum allocation request accepted by alloc_large. }
    max_alloc_chunk : long;
  end;

{ Routines that are to be used by both halves of the library are declared
  to receive a pointer to this structure.  There are no actual instances of
  jpeg_common_struct, only of jpeg_compress_struct and jpeg_decompress_struct.}
  jpeg_common_struct = record
  { Fields common to both master struct types }
    err : jpeg_error_mgr_ptr;           { Error handler module }
    mem : jpeg_memory_mgr_ptr;          { Memory manager module }
    progress : jpeg_progress_mgr_ptr;   { Progress monitor, or NIL if none }
    client_data : voidp;                { Available for use by application }
    is_decompressor : boolean;     { so common code can tell which is which }
    global_state : int;            { for checking call sequence validity }

  { Additional fields follow in an actual jpeg_compress_struct or
    jpeg_decompress_struct.  All three structs must agree on these
    initial fields!  (This would be a lot cleaner in C++.) }
  end;


{ Master record for a compression instance }

  jpeg_compress_struct = record
    { Fields shared with jpeg_decompress_struct }
    err : jpeg_error_mgr_ptr;          { Error handler module }
    mem : jpeg_memory_mgr_ptr;         { Memory manager module }
    progress : jpeg_progress_mgr_ptr;  { Progress monitor, or NIL if none }
    client_data : voidp;               { Available for use by application }
    is_decompressor : boolean;      { so common code can tell which is which }
    global_state : int;             { for checking call sequence validity }

  { Destination for compressed data }
    dest : jpeg_destination_mgr_ptr;

  { Description of source image --- these fields must be filled in by
    outer application before starting compression.  in_color_space must
    be correct before you can even call jpeg_set_defaults(). }


    image_width : JDIMENSION;         { input image width }
    image_height : JDIMENSION;        { input image height }
    input_components : int;           { # of color components in input image }
    in_color_space : J_COLOR_SPACE;   { colorspace of input image }

    input_gamma : double;             { image gamma of input image }

    { Compression parameters --- these fields must be set before calling
      jpeg_start_compress().  We recommend calling jpeg_set_defaults() to
      initialize everything to reasonable defaults, then changing anything
      the application specifically wants to change.  That way you won't get
      burnt when new parameters are added.  Also note that there are several
      helper routines to simplify changing parameters. }

    data_precision : int;             { bits of precision in image data }

    num_components : int;             { # of color components in JPEG image }
    jpeg_color_space : J_COLOR_SPACE; { colorspace of JPEG image }

    comp_info : jpeg_component_info_list_ptr;
    { comp_info^[i] describes component that appears i'th in SOF }

    quant_tbl_ptrs: Array[0..NUM_QUANT_TBLS-1] of JQUANT_TBL_PTR;
    { ptrs to coefficient quantization tables, or NIL if not defined }

    dc_huff_tbl_ptrs : Array[0..NUM_HUFF_TBLS-1] of JHUFF_TBL_PTR;
    ac_huff_tbl_ptrs : Array[0..NUM_HUFF_TBLS-1] of JHUFF_TBL_PTR;
    { ptrs to Huffman coding tables, or NIL if not defined }

    arith_dc_L : Array[0..NUM_ARITH_TBLS-1] of UINT8; { L values for DC arith-coding tables }
    arith_dc_U : Array[0..NUM_ARITH_TBLS-1] of UINT8; { U values for DC arith-coding tables }
    arith_ac_K : Array[0..NUM_ARITH_TBLS-1] of UINT8; { Kx values for AC arith-coding tables }

    num_scans : int;             { # of entries in scan_info array }
    scan_info : jpeg_scan_info_ptr; { script for multi-scan file, or NIL }
    { The default value of scan_info is NIL, which causes a single-scan
      sequential JPEG file to be emitted.  To create a multi-scan file,
      set num_scans and scan_info to point to an array of scan definitions. }

    raw_data_in : boolean;        { TRUE=caller supplies downsampled data }
    arith_code : boolean;         { TRUE=arithmetic coding, FALSE=Huffman }
    optimize_coding : boolean;    { TRUE=optimize entropy encoding parms }
    CCIR601_sampling : boolean;   { TRUE=first samples are cosited }
    smoothing_factor : int;       { 1..100, or 0 for no input smoothing }
    dct_method : J_DCT_METHOD;    { DCT algorithm selector }

    { The restart interval can be specified in absolute MCUs by setting
      restart_interval, or in MCU rows by setting restart_in_rows
      (in which case the correct restart_interval will be figured
      for each scan). }

    restart_interval : uint;      { MCUs per restart, or 0 for no restart }
    restart_in_rows : int;        { if > 0, MCU rows per restart interval }

    { Parameters controlling emission of special markers. }

    write_JFIF_header : boolean; { should a JFIF marker be written? }
    JFIF_major_version : UINT8;  { What to write for the JFIF version number }
    JFIF_minor_version : UINT8;
    { These three values are not used by the JPEG code, merely copied }
    { into the JFIF APP0 marker.  density_unit can be 0 for unknown, }
    { 1 for dots/inch, or 2 for dots/cm.  Note that the pixel aspect }
    { ratio is defined by X_density/Y_density even when density_unit=0. }
    density_unit : UINT8;         { JFIF code for pixel size units }
    X_density : UINT16;           { Horizontal pixel density }
    Y_density : UINT16;           { Vertical pixel density }
    write_Adobe_marker : boolean; { should an Adobe marker be written? }

    { State variable: index of next scanline to be written to
      jpeg_write_scanlines().  Application may use this to control its
      processing loop, e.g., "while (next_scanline < image_height)". }

    next_scanline : JDIMENSION;   { 0 .. image_height-1  }

    { Remaining fields are known throughout compressor, but generally
      should not be touched by a surrounding application. }

    { These fields are computed during compression startup }
    progressive_mode : boolean;   { TRUE if scan script uses progressive mode }
    max_h_samp_factor : int;      { largest h_samp_factor }
    max_v_samp_factor : int;      { largest v_samp_factor }

    total_iMCU_rows : JDIMENSION; { # of iMCU rows to be input to coef ctlr }
    { The coefficient controller receives data in units of MCU rows as defined
      for fully interleaved scans (whether the JPEG file is interleaved or not).
      There are v_samp_factor * DCTSIZE sample rows of each component in an
      "iMCU" (interleaved MCU) row. }

    { These fields are valid during any one scan.
      They describe the components and MCUs actually appearing in the scan. }

    comps_in_scan : int;          { # of JPEG components in this scan }
    cur_comp_info : Array[0..MAX_COMPS_IN_SCAN-1] of jpeg_component_info_ptr;
    { cur_comp_info[i]^ describes component that appears i'th in SOS }

    MCUs_per_row : JDIMENSION;    { # of MCUs across the image }
    MCU_rows_in_scan : JDIMENSION;{ # of MCU rows in the image }

    blocks_in_MCU : int;          { # of DCT blocks per MCU }
    MCU_membership : Array[0..C_MAX_BLOCKS_IN_MCU-1] of int;
    { MCU_membership[i] is index in cur_comp_info of component owning }
    { i'th block in an MCU }

    Ss, Se, Ah, Al : int;         { progressive JPEG parameters for scan }

    { Links to compression subobjects (methods and private variables of modules) }
    master : jpeg_comp_master_ptr;
    main : jpeg_c_main_controller_ptr;
    prep : jpeg_c_prep_controller_ptr;
    coef : jpeg_c_coef_controller_ptr;
    marker : jpeg_marker_writer_ptr;
    cconvert : jpeg_color_converter_ptr;
    downsample : jpeg_downsampler_ptr;
    fdct : jpeg_forward_dct_ptr;
    entropy : jpeg_entropy_encoder_ptr;
    script_space : jpeg_scan_info_ptr; { workspace for jpeg_simple_progression }
    script_space_size : int;
  end;


{ Master record for a decompression instance }

  coef_bits_field = Array[0..DCTSIZE2-1] of int;
  coef_bits_ptr = ^coef_bits_field;
  coef_bits_ptrfield =  Array[0..MAX_COMPS_IN_SCAN-1] of coef_bits_field;
  coef_bits_ptrrow = ^coef_bits_ptrfield;

  range_limit_table = array[-(MAXJSAMPLE+1)..4*(MAXJSAMPLE+1)
                            + CENTERJSAMPLE -1] of JSAMPLE;
  range_limit_table_ptr = ^range_limit_table;

  jpeg_decompress_struct = record
  { Fields shared with jpeg_compress_struct }
    err : jpeg_error_mgr_ptr;      { Error handler module }
    mem : jpeg_memory_mgr_ptr;        { Memory manager module }
    progress : jpeg_progress_mgr_ptr; { Progress monitor, or NIL if none }
    client_data : voidp;              { Available for use by application }
    is_decompressor : boolean;     { so common code can tell which is which }
    global_state : int;            { for checking call sequence validity }

    { Source of compressed data }
    src : jpeg_source_mgr_ptr;

    { Basic description of image --- filled in by jpeg_read_header(). }
    { Application may inspect these values to decide how to process image. }

    image_width : JDIMENSION;      { nominal image width (from SOF marker) }
    image_height : JDIMENSION;     { nominal image height }
    num_components : int;          { # of color components in JPEG image }
    jpeg_color_space : J_COLOR_SPACE; { colorspace of JPEG image }

    { Decompression processing parameters --- these fields must be set before
      calling jpeg_start_decompress().  Note that jpeg_read_header()
      initializes them to default values. }

    out_color_space : J_COLOR_SPACE; { colorspace for output }

    scale_num, scale_denom : uint ;  { fraction by which to scale image }

    output_gamma : double;           { image gamma wanted in output }

    buffered_image : boolean;        { TRUE=multiple output passes }
    raw_data_out : boolean;          { TRUE=downsampled data wanted }

    dct_method : J_DCT_METHOD;       { IDCT algorithm selector }
    do_fancy_upsampling : boolean;   { TRUE=apply fancy upsampling }
    do_block_smoothing : boolean;    { TRUE=apply interblock smoothing }

    quantize_colors : boolean;       { TRUE=colormapped output wanted }
    { the following are ignored if not quantize_colors: }
    dither_mode : J_DITHER_MODE;     { type of color dithering to use }
    two_pass_quantize : boolean;     { TRUE=use two-pass color quantization }
    desired_number_of_colors : int;  { max # colors to use in created colormap }
    { these are significant only in buffered-image mode: }
    enable_1pass_quant : boolean;    { enable future use of 1-pass quantizer }
    enable_external_quant : boolean; { enable future use of external colormap }
    enable_2pass_quant : boolean;    { enable future use of 2-pass quantizer }

    { Description of actual output image that will be returned to application.
      These fields are computed by jpeg_start_decompress().
      You can also use jpeg_calc_output_dimensions() to determine these values
      in advance of calling jpeg_start_decompress(). }

    output_width : JDIMENSION;       { scaled image width }
    output_height: JDIMENSION;       { scaled image height }
    out_color_components : int;  { # of color components in out_color_space }
    output_components : int;     { # of color components returned }
    { output_components is 1 (a colormap index) when quantizing colors;
      otherwise it equals out_color_components. }

    rec_outbuf_height : int;     { min recommended height of scanline buffer }
    { If the buffer passed to jpeg_read_scanlines() is less than this many
      rows high, space and time will be wasted due to unnecessary data
      copying. Usually rec_outbuf_height will be 1 or 2, at most 4. }

    { When quantizing colors, the output colormap is described by these
      fields. The application can supply a colormap by setting colormap
      non-NIL before calling jpeg_start_decompress; otherwise a colormap
      is created during jpeg_start_decompress or jpeg_start_output. The map
      has out_color_components rows and actual_number_of_colors columns. }

    actual_number_of_colors : int;      { number of entries in use }
    colormap : JSAMPARRAY;              { The color map as a 2-D pixel array }

    { State variables: these variables indicate the progress of decompression.
      The application may examine these but must not modify them. }

    { Row index of next scanline to be read from jpeg_read_scanlines().
      Application may use this to control its processing loop, e.g.,
      "while (output_scanline < output_height)". }

    output_scanline : JDIMENSION; { 0 .. output_height-1  }

    { Current input scan number and number of iMCU rows completed in scan.
      These indicate the progress of the decompressor input side. }

    input_scan_number : int;      { Number of SOS markers seen so far }
    input_iMCU_row : JDIMENSION;  { Number of iMCU rows completed }

    { The "output scan number" is the notional scan being displayed by the
      output side.  The decompressor will not allow output scan/row number
      to get ahead of input scan/row, but it can fall arbitrarily far behind.}

    output_scan_number : int;     { Nominal scan number being displayed }
    output_iMCU_row : int;        { Number of iMCU rows read }

    { Current progression status.  coef_bits[c][i] indicates the precision
      with which component c's DCT coefficient i (in zigzag order) is known.
      It is -1 when no data has yet been received, otherwise it is the point
      transform (shift) value for the most recent scan of the coefficient
      (thus, 0 at completion of the progression).
      This pointer is NIL when reading a non-progressive file. }

    coef_bits : coef_bits_ptrrow;
                 { -1 or current Al value for each coef }

    { Internal JPEG parameters --- the application usually need not look at
      these fields.  Note that the decompressor output side may not use
      any parameters that can change between scans. }

    { Quantization and Huffman tables are carried forward across input
      datastreams when processing abbreviated JPEG datastreams. }

    quant_tbl_ptrs : Array[0..NUM_QUANT_TBLS-1] of JQUANT_TBL_PTR;
    { ptrs to coefficient quantization tables, or NIL if not defined }

    dc_huff_tbl_ptrs : Array[0..NUM_HUFF_TBLS-1] of JHUFF_TBL_PTR;
    ac_huff_tbl_ptrs : Array[0..NUM_HUFF_TBLS-1] of JHUFF_TBL_PTR;
    { ptrs to Huffman coding tables, or NIL if not defined }

    { These parameters are never carried across datastreams, since they
      are given in SOF/SOS markers or defined to be reset by SOI. }

    data_precision : int;          { bits of precision in image data }

    comp_info : jpeg_component_info_list_ptr;
    { comp_info^[i] describes component that appears i'th in SOF }

    progressive_mode : boolean;    { TRUE if SOFn specifies progressive mode }
    arith_code : boolean;          { TRUE=arithmetic coding, FALSE=Huffman }

    arith_dc_L : Array[0..NUM_ARITH_TBLS-1] of UINT8; { L values for DC arith-coding tables }
    arith_dc_U : Array[0..NUM_ARITH_TBLS-1] of UINT8; { U values for DC arith-coding tables }
    arith_ac_K : Array[0..NUM_ARITH_TBLS-1] of UINT8; { Kx values for AC arith-coding tables }

    restart_interval : uint; { MCUs per restart interval, or 0 for no restart }

    { These fields record data obtained from optional markers recognized by
      the JPEG library. }

    saw_JFIF_marker : boolean;  { TRUE iff a JFIF APP0 marker was found }
    { Data copied from JFIF marker; only valid if saw_JFIF_marker is TRUE: }
    JFIF_major_version : UINT8; { JFIF version number }
    JFIF_minor_version : UINT8;
    density_unit : UINT8;       { JFIF code for pixel size units }
    X_density : UINT16;         { Horizontal pixel density }
    Y_density : UINT16;         { Vertical pixel density }
    saw_Adobe_marker : boolean; { TRUE iff an Adobe APP14 marker was found }
    Adobe_transform : UINT8;    { Color transform code from Adobe marker }

    saw_EXIF_marker : boolean;  { TRUE if an Exif APP1 marker was found }
    orientation : UINT16;       { Exif orientation value }

    CCIR601_sampling : boolean; { TRUE=first samples are cosited }

    { Aside from the specific data retained from APPn markers known to the
      library, the uninterpreted contents of any or all APPn and COM markers
      can be saved in a list for examination by the application. }

    marker_list : jpeg_saved_marker_ptr; { Head of list of saved markers }

    { Remaining fields are known throughout decompressor, but generally
      should not be touched by a surrounding application. }


    { These fields are computed during decompression startup }

    max_h_samp_factor : int;    { largest h_samp_factor }
    max_v_samp_factor : int;    { largest v_samp_factor }

    min_DCT_scaled_size : int;  { smallest DCT_scaled_size of any component }

    total_iMCU_rows : JDIMENSION; { # of iMCU rows in image }
    { The coefficient controller's input and output progress is measured in
      units of "iMCU" (interleaved MCU) rows.  These are the same as MCU rows
      in fully interleaved JPEG scans, but are used whether the scan is
      interleaved or not.  We define an iMCU row as v_samp_factor DCT block
      rows of each component.  Therefore, the IDCT output contains
      v_samp_factor*DCT_scaled_size sample rows of a component per iMCU row.}

    sample_range_limit : range_limit_table_ptr; { table for fast range-limiting }


    { These fields are valid during any one scan.
      They describe the components and MCUs actually appearing in the scan.
      Note that the decompressor output side must not use these fields. }

    comps_in_scan : int;           { # of JPEG components in this scan }
    cur_comp_info : Array[0..MAX_COMPS_IN_SCAN-1] of jpeg_component_info_ptr;
    { cur_comp_info[i]^ describes component that appears i'th in SOS }

    MCUs_per_row : JDIMENSION;     { # of MCUs across the image }
    MCU_rows_in_scan : JDIMENSION; { # of MCU rows in the image }

    blocks_in_MCU : JDIMENSION;    { # of DCT blocks per MCU }
    MCU_membership : Array[0..D_MAX_BLOCKS_IN_MCU-1] of int;
    { MCU_membership[i] is index in cur_comp_info of component owning }
    { i'th block in an MCU }

    Ss, Se, Ah, Al : int;          { progressive JPEG parameters for scan }

    { This field is shared between entropy decoder and marker parser.
      It is either zero or the code of a JPEG marker that has been
      read from the data source, but has not yet been processed. }

    unread_marker : int;

    { Links to decompression subobjects
      (methods, private variables of modules) }

    master : jpeg_decomp_master_ptr;
    main : jpeg_d_main_controller_ptr;
    coef : jpeg_d_coef_controller_ptr;
    post : jpeg_d_post_controller_ptr;
    inputctl : jpeg_input_controller_ptr;
    marker : jpeg_marker_reader_ptr;
    entropy : jpeg_entropy_decoder_ptr;
    idct : jpeg_inverse_dct_ptr;
    upsample : jpeg_upsampler_ptr;
    cconvert : jpeg_color_deconverter_ptr;
    cquantize : jpeg_color_quantizer_ptr;
  end;

{ Decompression startup: read start of JPEG datastream to see what's there
   function jpeg_read_header (cinfo : j_decompress_ptr;
                              require_image : boolean) : int;
  Return value is one of: }
const
  JPEG_SUSPENDED              = 0; { Suspended due to lack of input data }
  JPEG_HEADER_OK              = 1; { Found valid image datastream }
  JPEG_HEADER_TABLES_ONLY     = 2; { Found valid table-specs-only datastream }
{ If you pass require_image = TRUE (normal case), you need not check for
  a TABLES_ONLY return code; an abbreviated file will cause an error exit.
  JPEG_SUSPENDED is only possible if you use a data source module that can
  give a suspension return (the stdio source module doesn't). }


{ function jpeg_consume_input (cinfo : j_decompress_ptr) : int;
  Return value is one of: }

  JPEG_REACHED_SOS            = 1; { Reached start of new scan }
  JPEG_REACHED_EOI            = 2; { Reached end of image }
  JPEG_ROW_COMPLETED          = 3; { Completed one iMCU row }
  JPEG_SCAN_COMPLETED         = 4; { Completed last iMCU row of a scan }




implementation

end.
