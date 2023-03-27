Unit jcmarker;

{ This file contains routines to write JPEG datastream markers. }

{ Original: jcmarker.c; Copyright (C) 1991-1998, Thomas G. Lane. }

interface

{$I jconfig.inc}

uses
  jinclude, jmorecfg, jerror,
  jdeferr, jpeglib, jutils;


const
                { JPEG marker codes }
  M_SOF0  = $c0;
  M_SOF1  = $c1;
  M_SOF2  = $c2;
  M_SOF3  = $c3;

  M_SOF5  = $c5;
  M_SOF6  = $c6;
  M_SOF7  = $c7;

  M_JPG   = $c8;
  M_SOF9  = $c9;
  M_SOF10 = $ca;
  M_SOF11 = $cb;

  M_SOF13 = $cd;
  M_SOF14 = $ce;
  M_SOF15 = $cf;

  M_DHT   = $c4;

  M_DAC   = $cc;

  M_RST0  = $d0;
  M_RST1  = $d1;
  M_RST2  = $d2;
  M_RST3  = $d3;
  M_RST4  = $d4;
  M_RST5  = $d5;
  M_RST6  = $d6;
  M_RST7  = $d7;

  M_SOI   = $d8;
  M_EOI   = $d9;
  M_SOS   = $da;
  M_DQT   = $db;
  M_DNL   = $dc;
  M_DRI   = $dd;
  M_DHP   = $de;
  M_EXP   = $df;

  M_APP0  = $e0;
  M_APP1  = $e1;
  M_APP2  = $e2;
  M_APP3  = $e3;
  M_APP4  = $e4;
  M_APP5  = $e5;
  M_APP6  = $e6;
  M_APP7  = $e7;
  M_APP8  = $e8;
  M_APP9  = $e9;
  M_APP10 = $ea;
  M_APP11 = $eb;
  M_APP12 = $ec;
  M_APP13 = $ed;
  M_APP14 = $ee;
  M_APP15 = $ef;

  M_JPG0  = $f0;
  M_JPG13 = $fd;
  M_COM   = $fe;

  M_TEM   = $01;

  M_ERROR = $100;

type
  JPEG_MARKER = Word;

{ Private state }

type
  my_marker_ptr = ^my_marker_writer;
  my_marker_writer = record
    pub : jpeg_marker_writer; { public fields }

    last_restart_interval : uint; { last DRI value emitted; 0 after SOI }
  end;




{GLOBAL}
procedure jinit_marker_writer (cinfo : j_compress_ptr);

implementation

{ Basic output routines.

  Note that we do not support suspension while writing a marker.
  Therefore, an application using suspension must ensure that there is
  enough buffer space for the initial markers (typ. 600-700 bytes) before
  calling jpeg_start_compress, and enough space to write the trailing EOI
  (a few bytes) before calling jpeg_finish_compress.  Multipass compression
  modes are not supported at all with suspension, so those two are the only
  points where markers will be written. }


{LOCAL}
procedure emit_byte (cinfo : j_compress_ptr; val : int);
{ Emit a byte }
var
  dest : jpeg_destination_mgr_ptr;
begin
  dest := cinfo^.dest;

  dest^.next_output_byte^ := JOCTET(val);
  Inc(dest^.next_output_byte);

  Dec(dest^.free_in_buffer);
  if (dest^.free_in_buffer = 0) then
  begin
    if not dest^.empty_output_buffer(cinfo) then
      ERREXIT(j_common_ptr(cinfo), JERR_CANT_SUSPEND);
  end;
end;


{LOCAL}
procedure emit_marker(cinfo : j_compress_ptr; mark : JPEG_MARKER);
{ Emit a marker code }
begin
  emit_byte(cinfo, $FF);
  emit_byte(cinfo, int(mark));
end;


{LOCAL}
procedure emit_2bytes (cinfo : j_compress_ptr; value : int);
{ Emit a 2-byte integer; these are always MSB first in JPEG files }
begin
  emit_byte(cinfo, (value shr 8) and $FF);
  emit_byte(cinfo, value and $FF);
end;


{ Routines to write specific marker types. }

{LOCAL}
function emit_dqt (cinfo : j_compress_ptr; index : int) : int;
{ Emit a DQT marker }
{ Returns the precision used (0 = 8bits, 1 = 16bits) for baseline checking }
var
  qtbl : JQUANT_TBL_PTR;
  prec : int;
  i : int;
var
  qval : uint;
begin
  qtbl := cinfo^.quant_tbl_ptrs[index];
  if (qtbl = NIL) then
    ERREXIT1(j_common_ptr(cinfo), JERR_NO_QUANT_TABLE, index);

  prec := 0;
  for i := 0 to Pred(DCTSIZE2) do
  begin
    if (qtbl^.quantval[i] > 255) then
      prec := 1;
  end;

  if not qtbl^.sent_table then
  begin
    emit_marker(cinfo, M_DQT);

    if (prec <> 0) then
      emit_2bytes(cinfo, DCTSIZE2*2 + 1 + 2)
    else
      emit_2bytes(cinfo, DCTSIZE2 + 1 + 2);

    emit_byte(cinfo, index + (prec shl 4));

    for i := 0 to Pred(DCTSIZE2) do
    begin
      { The table entries must be emitted in zigzag order. }
      qval := qtbl^.quantval[jpeg_natural_order[i]];
      if (prec <> 0) then
        emit_byte(cinfo, int(qval shr 8));
      emit_byte(cinfo, int(qval and $FF));
    end;

    qtbl^.sent_table := TRUE;
  end;

  emit_dqt := prec;
end;


{LOCAL}
procedure emit_dht (cinfo : j_compress_ptr; index : int; is_ac : boolean);
{ Emit a DHT marker }
var
  htbl : JHUFF_TBL_PTR;
  length, i : int;
begin
  if (is_ac) then
  begin
    htbl := cinfo^.ac_huff_tbl_ptrs[index];
    index := index + $10;                { output index has AC bit set }
  end
  else
  begin
    htbl := cinfo^.dc_huff_tbl_ptrs[index];
  end;

  if (htbl = NIL) then
    ERREXIT1(j_common_ptr(cinfo), JERR_NO_HUFF_TABLE, index);

  if not htbl^.sent_table then
  begin
    emit_marker(cinfo, M_DHT);

    length := 0;
    for i := 1 to 16 do
      length := length + htbl^.bits[i];

    emit_2bytes(cinfo, length + 2 + 1 + 16);
    emit_byte(cinfo, index);

    for i := 1 to 16 do
      emit_byte(cinfo, htbl^.bits[i]);

    for i := 0 to Pred(length) do
      emit_byte(cinfo, htbl^.huffval[i]);

    htbl^.sent_table := TRUE;
  end;
end;


{LOCAL}
procedure emit_dac (cinfo : j_compress_ptr);
{ Emit a DAC marker }
{ Since the useful info is so small, we want to emit all the tables in }
{ one DAC marker.  Therefore this routine does its own scan of the table. }
{$ifdef C_ARITH_CODING_SUPPORTED}
var
  dc_in_use : array[0..NUM_ARITH_TBLS] of byte;
  ac_in_use : array[0..NUM_ARITH_TBLS] of byte;
  length, i : int;
  compptr : jpeg_component_info_ptr;
begin
  for i := 0 to pred(NUM_ARITH_TBLS) do
  begin
    dc_in_use[i] := 0;
    ac_in_use[i] := 0;
  end;

  for i := 0 to pred(cinfo^.comps_in_scan) do
  begin
    compptr := cinfo^.cur_comp_info[i];
    dc_in_use[compptr^.dc_tbl_no] := 1;
    ac_in_use[compptr^.ac_tbl_no] := 1;
  end;

  length := 0;
  for i := 0 to pred(NUM_ARITH_TBLS) do
    Inc(length, dc_in_use[i] + ac_in_use[i]);

  emit_marker(cinfo, M_DAC);

  emit_2bytes(cinfo, length*2 + 2);

  for i := 0 to pred(NUM_ARITH_TBLS) do
  begin
    if (dc_in_use[i] <> 0) then
    begin
      emit_byte(cinfo, i);
      emit_byte(cinfo, cinfo^.arith_dc_L[i] + (cinfo^.arith_dc_U[i] shl 4));
    end;
    if (ac_in_use[i] <> 0) then
    begin
      emit_byte(cinfo, i + $10);
      emit_byte(cinfo, cinfo^.arith_ac_K[i]);
    end;
  end;
end;
{$else}
begin
end;
{$endif}  {C_ARITH_CODING_SUPPORTED}


{LOCAL}
procedure emit_dri (cinfo : j_compress_ptr);
{ Emit a DRI marker }
begin
  emit_marker(cinfo, M_DRI);

  emit_2bytes(cinfo, 4);        { fixed length }

  emit_2bytes(cinfo, int(cinfo^.restart_interval));
end;


{LOCAL}
procedure emit_sof (cinfo : j_compress_ptr; code : JPEG_MARKER);
{ Emit a SOF marker }
var
  ci : int;
  compptr : jpeg_component_info_ptr;
begin
  emit_marker(cinfo, code);

  emit_2bytes(cinfo, 3 * cinfo^.num_components + 2 + 5 + 1); { length }

  { Make sure image isn't bigger than SOF field can handle }
  if (long(cinfo^.image_height) > long(65535)) or
     (long(cinfo^.image_width) > long(65535)) then
    ERREXIT1(j_common_ptr(cinfo), JERR_IMAGE_TOO_BIG, uInt(65535));

  emit_byte(cinfo, cinfo^.data_precision);
  emit_2bytes(cinfo, int(cinfo^.image_height));
  emit_2bytes(cinfo, int(cinfo^.image_width));

  emit_byte(cinfo, cinfo^.num_components);

  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0  to Pred(cinfo^.num_components) do
  begin
    emit_byte(cinfo, compptr^.component_id);
    emit_byte(cinfo, (compptr^.h_samp_factor shl 4) + compptr^.v_samp_factor);
    emit_byte(cinfo, compptr^.quant_tbl_no);
    Inc(compptr);
  end;
end;


{LOCAL}
procedure emit_sos (cinfo : j_compress_ptr);
{ Emit a SOS marker }
var
  i, td, ta : int;
  compptr : jpeg_component_info_ptr;
begin
  emit_marker(cinfo, M_SOS);

  emit_2bytes(cinfo, 2 * cinfo^.comps_in_scan + 2 + 1 + 3); { length }

  emit_byte(cinfo, cinfo^.comps_in_scan);

  for i := 0 to Pred(cinfo^.comps_in_scan) do
  begin
    compptr := cinfo^.cur_comp_info[i];
    emit_byte(cinfo, compptr^.component_id);
    td := compptr^.dc_tbl_no;
    ta := compptr^.ac_tbl_no;
    if (cinfo^.progressive_mode) then
    begin
      { Progressive mode: only DC or only AC tables are used in one scan;
        furthermore, Huffman coding of DC refinement uses no table at all.
        We emit 0 for unused field(s); this is recommended by the P&M text
        but does not seem to be specified in the standard. }

      if (cinfo^.Ss = 0) then
      begin
        ta := 0;                { DC scan }
        if (cinfo^.Ah <> 0) and not cinfo^.arith_code then
          td := 0;              { no DC table either }
      end
      else
      begin
        td := 0;                        { AC scan }
      end;
    end;
    emit_byte(cinfo, (td shl 4) + ta);
  end;

  emit_byte(cinfo, cinfo^.Ss);
  emit_byte(cinfo, cinfo^.Se);
  emit_byte(cinfo, (cinfo^.Ah shl 4) + cinfo^.Al);
end;


{LOCAL}
procedure emit_jfif_app0 (cinfo : j_compress_ptr);
{ Emit a JFIF-compliant APP0 marker }
{
 Length of APP0 block   (2 bytes)
 Block ID                       (4 bytes - ASCII "JFIF")
 Zero byte                      (1 byte to terminate the ID string)
 Version Major, Minor   (2 bytes - major first)
 Units                  (1 byte - $00 = none, $01 = inch, $02 = cm)
 Xdpu                   (2 bytes - dots per unit horizontal)
 Ydpu                   (2 bytes - dots per unit vertical)
 Thumbnail X size               (1 byte)
 Thumbnail Y size               (1 byte)
}
begin
  emit_marker(cinfo, M_APP0);

  emit_2bytes(cinfo, 2 + 4 + 1 + 2 + 1 + 2 + 2 + 1 + 1); { length }

  emit_byte(cinfo, $4A);        { Identifier: ASCII "JFIF" }
  emit_byte(cinfo, $46);
  emit_byte(cinfo, $49);
  emit_byte(cinfo, $46);
  emit_byte(cinfo, 0);
  emit_byte(cinfo, cinfo^.JFIF_major_version); { Version fields }
  emit_byte(cinfo, cinfo^.JFIF_minor_version);
  emit_byte(cinfo, cinfo^.density_unit); { Pixel size information }
  emit_2bytes(cinfo, int(cinfo^.X_density));
  emit_2bytes(cinfo, int(cinfo^.Y_density));
  emit_byte(cinfo, 0);          { No thumbnail image }
  emit_byte(cinfo, 0);
end;


{LOCAL}
procedure emit_adobe_app14 (cinfo : j_compress_ptr);
{ Emit an Adobe APP14 marker }
{
  Length of APP14 block (2 bytes)
  Block ID                      (5 bytes - ASCII "Adobe")
  Version Number                (2 bytes - currently 100)
  Flags0                        (2 bytes - currently 0)
  Flags1                        (2 bytes - currently 0)
  Color transform               (1 byte)

  Although Adobe TN 5116 mentions Version = 101, all the Adobe files
  now in circulation seem to use Version = 100, so that's what we write.

  We write the color transform byte as 1 if the JPEG color space is
  YCbCr, 2 if it's YCCK, 0 otherwise.  Adobe's definition has to do with
  whether the encoder performed a transformation, which is pretty useless.
}
begin
  emit_marker(cinfo, M_APP14);

  emit_2bytes(cinfo, 2 + 5 + 2 + 2 + 2 + 1); { length }

  emit_byte(cinfo, $41);        { Identifier: ASCII "Adobe" }
  emit_byte(cinfo, $64);
  emit_byte(cinfo, $6F);
  emit_byte(cinfo, $62);
  emit_byte(cinfo, $65);
  emit_2bytes(cinfo, 100);      { Version }
  emit_2bytes(cinfo, 0);        { Flags0 }
  emit_2bytes(cinfo, 0);        { Flags1 }
  case (cinfo^.jpeg_color_space) of
  JCS_YCbCr:
    emit_byte(cinfo, 1);        { Color transform = 1 }
  JCS_YCCK:
    emit_byte(cinfo, 2);        { Color transform = 2 }
  else
    emit_byte(cinfo, 0);        { Color transform = 0 }
  end;
end;


{ These routines allow writing an arbitrary marker with parameters.
  The only intended use is to emit COM or APPn markers after calling
  write_file_header and before calling write_frame_header.
  Other uses are not guaranteed to produce desirable results.
  Counting the parameter bytes properly is the caller's responsibility. }

{METHODDEF}
procedure write_marker_header (cinfo : j_compress_ptr;
                               marker : int;
                               datalen : uint); far;
{ Emit an arbitrary marker header }
begin
  if (datalen > uint(65533)) then  { safety check }
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_LENGTH);

  emit_marker(cinfo, JPEG_MARKER(marker));

  emit_2bytes(cinfo, int(datalen + 2)); { total length }
end;

{METHODDEF}
procedure write_marker_byte (cinfo : j_compress_ptr; val : int); far;
{ Emit one byte of marker parameters following write_marker_header }
begin
  emit_byte(cinfo, val);
end;

{ Write datastream header.
  This consists of an SOI and optional APPn markers.
  We recommend use of the JFIF marker, but not the Adobe marker,
  when using YCbCr or grayscale data.  The JFIF marker should NOT
  be used for any other JPEG colorspace.  The Adobe marker is helpful
  to distinguish RGB, CMYK, and YCCK colorspaces.
  Note that an application can write additional header markers after
  jpeg_start_compress returns. }


{METHODDEF}
procedure write_file_header (cinfo : j_compress_ptr); far;
var
  marker : my_marker_ptr;
begin
  marker := my_marker_ptr(cinfo^.marker);

  emit_marker(cinfo, M_SOI);     { first the SOI }

  { SOI is defined to reset restart interval to 0 }
  marker^.last_restart_interval := 0;

  if (cinfo^.write_JFIF_header) then { next an optional JFIF APP0 }
    emit_jfif_app0(cinfo);
  if (cinfo^.write_Adobe_marker) then { next an optional Adobe APP14 }
    emit_adobe_app14(cinfo);
end;


{ Write frame header.
  This consists of DQT and SOFn markers.
  Note that we do not emit the SOF until we have emitted the DQT(s).
  This avoids compatibility problems with incorrect implementations that
  try to error-check the quant table numbers as soon as they see the SOF. }


{METHODDEF}
procedure write_frame_header (cinfo : j_compress_ptr); far;
var
  ci, prec : int;
  is_baseline : boolean;
  compptr : jpeg_component_info_ptr;
begin
  { Emit DQT for each quantization table.
    Note that emit_dqt() suppresses any duplicate tables. }

  prec := 0;
  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to Pred(cinfo^.num_components) do
  begin
    prec := prec + emit_dqt(cinfo, compptr^.quant_tbl_no);
    Inc(compptr);
  end;
  { now prec is nonzero iff there are any 16-bit quant tables. }

  { Check for a non-baseline specification.
    Note we assume that Huffman table numbers won't be changed later. }

  if (cinfo^.arith_code) or (cinfo^.progressive_mode)
   or (cinfo^.data_precision <> 8) then
  begin
    is_baseline := FALSE;
  end
  else
  begin
    is_baseline := TRUE;
    compptr := jpeg_component_info_ptr(cinfo^.comp_info);
    for ci := 0 to Pred(cinfo^.num_components) do
    begin
      if (compptr^.dc_tbl_no > 1) or (compptr^.ac_tbl_no > 1) then
        is_baseline := FALSE;
      Inc(compptr);
    end;
    if (prec <> 0) and (is_baseline) then
    begin
      is_baseline := FALSE;
      { If it's baseline except for quantizer size, warn the user }
      {$IFDEF DEBUG}
      TRACEMS(j_common_ptr(cinfo), 0, JTRC_16BIT_TABLES);
      {$ENDIF}
    end;
  end;

  { Emit the proper SOF marker }
  if (cinfo^.arith_code) then
  begin
    emit_sof(cinfo, M_SOF9);    { SOF code for arithmetic coding }
  end
  else
  begin
    if (cinfo^.progressive_mode) then
      emit_sof(cinfo, M_SOF2)   { SOF code for progressive Huffman }
    else if (is_baseline) then
      emit_sof(cinfo, M_SOF0)   { SOF code for baseline implementation }
    else
      emit_sof(cinfo, M_SOF1);  { SOF code for non-baseline Huffman file }
  end;
end;


{ Write scan header.
  This consists of DHT or DAC markers, optional DRI, and SOS.
  Compressed data will be written following the SOS. }

{METHODDEF}
procedure write_scan_header (cinfo : j_compress_ptr); far;
var
  marker : my_marker_ptr;
  i : int;
  compptr : jpeg_component_info_ptr;
begin
  marker := my_marker_ptr(cinfo^.marker);
  if (cinfo^.arith_code) then
  begin
    { Emit arith conditioning info.  We may have some duplication
      if the file has multiple scans, but it's so small it's hardly
      worth worrying about. }
    emit_dac(cinfo);
  end
  else
  begin
    { Emit Huffman tables.
      Note that emit_dht() suppresses any duplicate tables. }
    for i := 0 to Pred(cinfo^.comps_in_scan) do
    begin
      compptr := cinfo^.cur_comp_info[i];
      if (cinfo^.progressive_mode) then
      begin
        { Progressive mode: only DC or only AC tables are used in one scan }
        if (cinfo^.Ss = 0) then
        begin
          if (cinfo^.Ah = 0) then  { DC needs no table for refinement scan }
            emit_dht(cinfo, compptr^.dc_tbl_no, FALSE);
        end
        else
        begin
          emit_dht(cinfo, compptr^.ac_tbl_no, TRUE);
        end;
      end
      else
      begin
        { Sequential mode: need both DC and AC tables }
        emit_dht(cinfo, compptr^.dc_tbl_no, FALSE);
        emit_dht(cinfo, compptr^.ac_tbl_no, TRUE);
      end;
    end;
  end;

  { Emit DRI if required --- note that DRI value could change for each scan.
    We avoid wasting space with unnecessary DRIs, however. }

  if (cinfo^.restart_interval <> marker^.last_restart_interval) then
  begin
    emit_dri(cinfo);
    marker^.last_restart_interval := cinfo^.restart_interval;
  end;

  emit_sos(cinfo);
end;



{ Write datastream trailer. }


{METHODDEF}
procedure write_file_trailer (cinfo : j_compress_ptr); far;
begin
  emit_marker(cinfo, M_EOI);
end;


{ Write an abbreviated table-specification datastream.
  This consists of SOI, DQT and DHT tables, and EOI.
  Any table that is defined and not marked sent_table = TRUE will be
  emitted.  Note that all tables will be marked sent_table = TRUE at exit. }


{METHODDEF}
procedure write_tables_only (cinfo : j_compress_ptr); far;
var
  i : int;
begin
  emit_marker(cinfo, M_SOI);

  for i := 0 to Pred(NUM_QUANT_TBLS) do
  begin
    if (cinfo^.quant_tbl_ptrs[i] <> NIL) then
      emit_dqt(cinfo, i);  { dummy := ... }
  end;

  if (not cinfo^.arith_code) then
  begin
    for i := 0 to Pred(NUM_HUFF_TBLS) do
    begin
      if (cinfo^.dc_huff_tbl_ptrs[i] <> NIL) then
        emit_dht(cinfo, i, FALSE);
      if (cinfo^.ac_huff_tbl_ptrs[i] <> NIL) then
        emit_dht(cinfo, i, TRUE);
    end;
  end;

  emit_marker(cinfo, M_EOI);
end;


{ Initialize the marker writer module. }

{GLOBAL}
procedure jinit_marker_writer (cinfo : j_compress_ptr);
var
  marker : my_marker_ptr;
begin
  { Create the subobject }
  marker := my_marker_ptr(
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                                SIZEOF(my_marker_writer)) );
  cinfo^.marker := jpeg_marker_writer_ptr(marker);
  { Initialize method pointers }
  marker^.pub.write_file_header := write_file_header;
  marker^.pub.write_frame_header := write_frame_header;
  marker^.pub.write_scan_header := write_scan_header;
  marker^.pub.write_file_trailer := write_file_trailer;
  marker^.pub.write_tables_only := write_tables_only;
  marker^.pub.write_marker_header := write_marker_header;
  marker^.pub.write_marker_byte := write_marker_byte;
  { Initialize private state }
  marker^.last_restart_interval := 0;
end;


end.
