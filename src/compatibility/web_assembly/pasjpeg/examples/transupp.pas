Unit transupp;

{* transupp.c
 * transupp.h

 Copyright (C) 1997, Thomas G. Lane.
 This file is part of the Independent JPEG Group's software.
 For conditions of distribution and use, see the accompanying README file.

 This file contains image transformation routines and other utility code
 used by the jpegtran sample application.  These are NOT part of the core
 JPEG library.  But we keep these routines separate from jpegtran.c to
 ease the task of maintaining jpegtran-like programs that have other user
 interfaces.

 NOTE: all the routines declared here have very specific requirements
 about when they are to be executed during the reading and writing of the
 source and destination files.  See the comments in transupp.c, or see
 jpegtran.c for an example of correct usage. }

interface


{$I jconfig.inc}

uses
  jmorecfg,
  jinclude,
  jpeglib;


{ Short forms of external names for systems with brain-damaged linkers. }

{$ifdef NEED_SHORT_EXTERNAL_NAMES}
  jtransform_request_workspace          jTrRequest
  jtransform_adjust_parameters          jTrAdjust
  jtransform_execute_transformation     jTrExec
  jcopy_markers_setup                   jCMrkSetup
  jcopy_markers_execute                 jCMrkExec
{$endif} { NEED_SHORT_EXTERNAL_NAMES }


{ Codes for supported types of image transformations. }

type
  JXFORM_CODE = (
        JXFORM_NONE,            { no transformation }
{$ifdef CROP_SUPPORTED}
        JXFORM_CUT,             { cut out part of the image }
{$endif}
        JXFORM_FLIP_H,          { horizontal flip }
        JXFORM_FLIP_V,          { vertical flip }
        JXFORM_TRANSPOSE,       { transpose across UL-to-LR axis }
        JXFORM_TRANSVERSE,      { transpose across UR-to-LL axis }
        JXFORM_ROT_90,          { 90-degree clockwise rotation }
        JXFORM_ROT_180,         { 180-degree rotation }
        JXFORM_ROT_270          { 270-degree clockwise (or 90 ccw) }
                );

{
  Although rotating and flipping data expressed as DCT coefficients is not
  hard, there is an asymmetry in the JPEG format specification for images
  whose dimensions aren't multiples of the iMCU size.  The right and bottom
  image edges are padded out to the next iMCU boundary with junk data; but
  no padding is possible at the top and left edges.  If we were to flip
  the whole image including the pad data, then pad garbage would become
  visible at the top and/or left, and real pixels would disappear into the
  pad margins --- perhaps permanently, since encoders & decoders may not
  bother to preserve DCT blocks that appear to be completely outside the
  nominal image area.  So, we have to exclude any partial iMCUs from the
  basic transformation.

  Transpose is the only transformation that can handle partial iMCUs at the
  right and bottom edges completely cleanly.  flip_h can flip partial iMCUs
  at the bottom, but leaves any partial iMCUs at the right edge untouched.
  Similarly flip_v leaves any partial iMCUs at the bottom edge untouched.
  The other transforms are defined as combinations of these basic transforms
  and process edge blocks in a way that preserves the equivalence.

  The "trim" option causes untransformable partial iMCUs to be dropped;
  this is not strictly lossless, but it usually gives the best-looking
  result for odd-size images.  Note that when this option is active,
  the expected mathematical equivalences between the transforms may not hold.
  (For example, -rot 270 -trim trims only the bottom edge, but -rot 90 -trim
  followed by -rot 180 -trim trims both edges.)

  We also offer a "force to grayscale" option, which simply discards the
  chrominance channels of a YCbCr image.  This is lossless in the sense that
  the luminance channel is preserved exactly.  It's not the same kind of
  thing as the rotate/flip transformations, but it's convenient to handle it
  as part of this package, mainly because the transformation routines have to
  be aware of the option to know how many components to work on.
 }

type
 jpeg_transform_info = record
   { Options: set by caller }
   transform : JXFORM_CODE;     { image transform operator }
   trim : boolean;              { if TRUE, trim partial MCUs as needed }
   force_grayscale : boolean;   { if TRUE, convert color image to grayscale }
{$ifdef CROP_SUPPORTED}
   xoffs, yoffs, newwidth, newheight : JDIMENSION;
{$endif}
   { Internal workspace: caller should not touch these }
   num_components : int;        { # of components in workspace }
   workspace_coef_arrays : jvirt_barray_tbl_ptr; { workspace for transformations }
 end;


{$ifdef TRANSFORMS_SUPPORTED}

{ Request any required workspace }
procedure jtransform_request_workspace(srcinfo : j_decompress_ptr;
                                       var info : jpeg_transform_info);
{ Adjust output image parameters }
function jtransform_adjust_parameters(
             srcinfo : j_decompress_ptr;
             dstinfo : j_compress_ptr;
             src_coef_arrays : jvirt_barray_tbl_ptr;
             var info : jpeg_transform_info) : jvirt_barray_tbl_ptr;

{ Execute the actual transformation, if any }
procedure jtransform_execute_transformation(
             srcinfo : j_decompress_ptr;
             dstinfo : j_compress_ptr;
             src_coef_arrays : jvirt_barray_tbl_ptr;
             var info : jpeg_transform_info);

{$endif} { TRANSFORMS_SUPPORTED }

{ Support for copying optional markers from source to destination file. }

type
 JCOPY_OPTION = (
        JCOPYOPT_NONE,          { copy no optional markers }
        JCOPYOPT_COMMENTS,      { copy only comment (COM) markers }
        JCOPYOPT_ALL            { copy all optional markers }
                );

const
  JCOPYOPT_DEFAULT = JCOPYOPT_COMMENTS; { recommended default }

{ Setup decompression object to save desired markers in memory }
procedure jcopy_markers_setup(srcinfo : j_decompress_ptr;
                              option : JCOPY_OPTION);
{ Copy markers saved in the given source object to the destination object }
procedure jcopy_markers_execute(srcinfo : j_decompress_ptr;
                                dstinfo : j_compress_ptr;
                                option : JCOPY_OPTION);

implementation

{ Although this file really shouldn't have access to the library internals,
  it's helpful to let it call jround_up() and jcopy_block_row(). }
uses
  jutils,
  jdeferr,
  jerror,
  {$ifdef SAVE_MARKERS_SUPPORTED}
  jdmarker,
  {$endif}
  jcapimin,
  jcparam;  { set color space }

{$ifdef TRANSFORMS_SUPPORTED}

{ Lossless image transformation routines.  These routines work on DCT
  coefficient arrays and thus do not require any lossy decompression
  or recompression of the image.
  Thanks to Guido Vollbeding for the initial design and code of this feature.

  Horizontal flipping is done in-place, using a single top-to-bottom
  pass through the virtual source array.  It will thus be much the
  fastest option for images larger than main memory.

  The other routines require a set of destination virtual arrays, so they
  need twice as much memory as jpegtran normally does.  The destination
  arrays are always written in normal scan order (top to bottom) because
  the virtual array manager expects this.  The source arrays will be scanned
  in the corresponding order, which means multiple passes through the source
  arrays for most of the transforms.  That could result in much thrashing
  if the image is larger than main memory.

  Some notes about the operating environment of the individual transform
  routines:
  1. Both the source and destination virtual arrays are allocated from the
     source JPEG object, and therefore should be manipulated by calling the
     source's memory manager.
  2. The destination's component count should be used.  It may be smaller
     than the source's when forcing to grayscale.
  3. Likewise the destination's sampling factors should be used.  When
     forcing to grayscale the destination's sampling factors will be all 1,
     and we may as well take that as the effective iMCU size.
  4. When "trim" is in effect, the destination's dimensions will be the
     trimmed values but the source's will be untrimmed.
  5. All the routines assume that the source and destination buffers are
     padded out to a full iMCU boundary.  This is true, although for the
     source buffer it is an undocumented property of jdcoefct.c.
  Notes 2,3,4 boil down to this: generally we should use the destination's
  dimensions and ignore the source's. }

{LOCAL}
procedure do_flip_h (srcinfo : j_decompress_ptr;
                     dstinfo : j_compress_ptr;
                     src_coef_arrays : jvirt_barray_tbl_ptr);
{ Horizontal flip; done in-place, so no separate dest array is required }
var
  MCU_cols, comp_width, blk_x, blk_y : JDIMENSION;
  ci, k, offset_y : int;
  buffer : JBLOCKARRAY;
  ptr1, ptr2 : JCOEF_PTR;
  temp1, temp2 : JCOEF;
  compptr : jpeg_component_info_ptr;
begin
  { Horizontal mirroring of DCT blocks is accomplished by swapping
    pairs of blocks in-place.  Within a DCT block, we perform horizontal
    mirroring by changing the signs of odd-numbered columns.
    Partial iMCUs at the right edge are left untouched. }

  MCU_cols := dstinfo^.image_width div (dstinfo^.max_h_samp_factor * DCTSIZE);

  for ci := 0 to dstinfo^.num_components-1 do
  begin
    compptr := jpeg_component_info_ptr(dstinfo^.comp_info);
    Inc(compptr, ci);
    comp_width := MCU_cols * compptr^.h_samp_factor;
    blk_y := 0;
    while (blk_y < compptr^.height_in_blocks) do
    begin
      buffer := srcinfo^.mem^.access_virt_barray
        (j_common_ptr(srcinfo), src_coef_arrays^[ci], blk_y,
         JDIMENSION (compptr^.v_samp_factor), TRUE);
      for offset_y := 0 to compptr^.v_samp_factor-1 do
      begin
        blk_x := 0;
        while (blk_x * 2 < comp_width) do
        begin
          ptr1 := JCOEF_PTR(@(buffer^[offset_y]^[blk_x]));
          ptr2 := JCOEF_PTR(@(buffer^[offset_y]^[comp_width - blk_x - 1]));
          { this unrolled loop doesn't need to know which row it's on... }
          k := 0;
          while (k < DCTSIZE2) do
          begin
            temp1 := ptr1^;     { swap even column }
            temp2 := ptr2^;
            ptr1^ := temp2;
            Inc(ptr1);
            ptr2^ := temp1;
            Inc(ptr2);
            temp1 := ptr1^;     { swap odd column with sign change }
            temp2 := ptr2^;
            ptr1^ := -temp2;
            Inc(ptr1);
            ptr2^ := -temp1;
            Inc(ptr2);
            Inc(k, 2);
          end;
          Inc(blk_x);
        end;
      end;
      Inc(blk_y, compptr^.v_samp_factor);
    end; { while }
  end; { for ci }
end; { do_flip_h }


{LOCAL}
procedure do_flip_v (srcinfo : j_decompress_ptr;
                     dstinfo : j_compress_ptr;
                     src_coef_arrays : jvirt_barray_tbl_ptr;
                     dst_coef_arrays : jvirt_barray_tbl_ptr);
{ Vertical flip }
var
  MCU_rows, comp_height, dst_blk_x, dst_blk_y : JDIMENSION;
  ci, i, j, offset_y : int;
  src_buffer, dst_buffer : JBLOCKARRAY;
  src_row_ptr, dst_row_ptr : JBLOCKROW;
  src_ptr, dst_ptr : JCOEF_PTR;
  compptr : jpeg_component_info_ptr;
begin
  { We output into a separate array because we can't touch different
    rows of the source virtual array simultaneously.  Otherwise, this
    is a pretty straightforward analog of horizontal flip.
    Within a DCT block, vertical mirroring is done by changing the signs
    of odd-numbered rows.
    Partial iMCUs at the bottom edge are copied verbatim. }

  MCU_rows := dstinfo^.image_height div (dstinfo^.max_v_samp_factor * DCTSIZE);

  for ci := 0 to dstinfo^.num_components-1 do
  begin
    compptr := jpeg_component_info_ptr(dstinfo^.comp_info);
    Inc(compptr, ci);
    comp_height := MCU_rows * compptr^.v_samp_factor;
    dst_blk_y := 0;
    while (dst_blk_y < compptr^.height_in_blocks) do
    begin
      dst_buffer := srcinfo^.mem^.access_virt_barray
           (j_common_ptr(srcinfo), dst_coef_arrays^[ci], dst_blk_y,
            JDIMENSION(compptr^.v_samp_factor), TRUE);
      if (dst_blk_y < comp_height) then
      begin
        { Row is within the mirrorable area. }
        src_buffer := srcinfo^.mem^.access_virt_barray
           (j_common_ptr(srcinfo), src_coef_arrays^[ci],
           comp_height - dst_blk_y - JDIMENSION(compptr^.v_samp_factor),
           JDIMENSION (compptr^.v_samp_factor), FALSE);
      end
      else
      begin
        { Bottom-edge blocks will be copied verbatim. }
        src_buffer := srcinfo^.mem^.access_virt_barray
           (j_common_ptr(srcinfo), src_coef_arrays^[ci], dst_blk_y,
            JDIMENSION (compptr^.v_samp_factor), FALSE);
      end;
      for offset_y := 0 to compptr^.v_samp_factor-1 do
      begin
        if (dst_blk_y < comp_height) then
        begin
          { Row is within the mirrorable area. }
          dst_row_ptr := dst_buffer^[offset_y];
          src_row_ptr := src_buffer^[compptr^.v_samp_factor - offset_y - 1];
          for dst_blk_x := 0 to compptr^.width_in_blocks-1 do
          begin
            dst_ptr := JCOEF_PTR(@(dst_row_ptr^[dst_blk_x]));
            src_ptr := JCOEF_PTR(@(src_row_ptr^[dst_blk_x]));
            i := 0;
            while (i < DCTSIZE) do
            begin
              { copy even row }
              for j := 0 to DCTSIZE-1 do
              begin
                dst_ptr^ := src_ptr^;
                Inc(dst_ptr);
                Inc(src_ptr);
              end;
              { copy odd row with sign change }
              for j := 0 to DCTSIZE-1 do
              begin
                dst_ptr^ := - (src_ptr^);
                Inc(dst_ptr);
                Inc(src_ptr);
              end;
              Inc(i, 2);
            end;
          end;
        end
        else
        begin
          { Just copy row verbatim. }
          jcopy_block_row(src_buffer^[offset_y], dst_buffer^[offset_y],
                          compptr^.width_in_blocks);
        end;
      end;
      Inc(dst_blk_y, compptr^.v_samp_factor);
    end; { while }
  end; { for ci }
end; { do_flip_v }

{$ifdef CROP_SUPPORTED}
{LOCAL}
procedure do_transform (srcinfo : j_decompress_ptr;
                        dstinfo : j_compress_ptr;
                        src_coef_arrays : jvirt_barray_tbl_ptr;
                        dst_coef_arrays : jvirt_barray_tbl_ptr;
                        xoffs : JDIMENSION;
                        yoffs : JDIMENSION);
{ transform src_coef_arrays so that the xoffs,yoffs (rounded to an even
  dct block) are the new origin of the image.  copy rather than move because
  I'd never finish if I tried to understand the byzantine memory management.
}
var
  ci : int;
  compptr : jpeg_component_info_ptr;
  src_buffer, dst_buffer : JBLOCKARRAY;
  dst_blk_x, dst_blk_y : JDIMENSION;
begin
  xoffs := xoffs div dstinfo^.max_h_samp_factor * DCTSIZE;
  yoffs := yoffs div dstinfo^.max_v_samp_factor * DCTSIZE;

  for ci := 0 to dstinfo^.num_components-1 do
  begin
    compptr := jpeg_component_info_ptr(dstinfo^.comp_info);
    Inc(compptr, ci);
    dst_blk_y := 0;
    while (dst_blk_y < compptr^.height_in_blocks) do
    begin
      dst_buffer := srcinfo^.mem^.access_virt_barray
        (j_common_ptr(srcinfo), dst_coef_arrays^[ci], dst_blk_y, 1, TRUE);
      src_buffer := srcinfo^.mem^.access_virt_barray
        (j_common_ptr(srcinfo), src_coef_arrays^[ci],
            dst_blk_y + yoffs * JDIMENSION(compptr^.v_samp_factor), 1, FALSE);
      jcopy_block_row(JBLOCKROW(@src_buffer^[0]^[xoffs * compptr^.h_samp_factor]),
             dst_buffer^[0], compptr^.width_in_blocks);
      Inc(dst_blk_y);
    end;
  end;
end; { do_transform }
{$endif}

{LOCAL}
procedure do_transpose (srcinfo : j_decompress_ptr;
                        dstinfo : j_compress_ptr;
                        src_coef_arrays : jvirt_barray_tbl_ptr;
                        dst_coef_arrays : jvirt_barray_tbl_ptr);
{ Transpose source into destination }
var
  dst_blk_x, dst_blk_y : JDIMENSION;
  ci, i, j, offset_x, offset_y : int;
  src_buffer, dst_buffer : JBLOCKARRAY;
  src_ptr, dst_ptr : JCOEFPTR;
  compptr : jpeg_component_info_ptr;
begin

  { Transposing pixels within a block just requires transposing the
    DCT coefficients.
    Partial iMCUs at the edges require no special treatment; we simply
    process all the available DCT blocks for every component. }

  for ci := 0 to dstinfo^.num_components-1 do
  begin
    compptr := jpeg_component_info_ptr(dstinfo^.comp_info);
    Inc(compptr, ci);
    dst_blk_y := 0;
    while (dst_blk_y < compptr^.height_in_blocks) do
    begin
      dst_buffer := srcinfo^.mem^.access_virt_barray
        (j_common_ptr(srcinfo), dst_coef_arrays^[ci], dst_blk_y,
         JDIMENSION (compptr^.v_samp_factor), TRUE);
      for offset_y := 0 to compptr^.v_samp_factor-1 do
      begin
        dst_blk_x := 0;
        while (dst_blk_x < compptr^.width_in_blocks) do
        begin
          src_buffer := srcinfo^.mem^.access_virt_barray
            (j_common_ptr(srcinfo), src_coef_arrays^[ci], dst_blk_x,
             JDIMENSION (compptr^.h_samp_factor), FALSE);
          for offset_x := 0 to compptr^.h_samp_factor-1 do
          begin
            src_ptr := JCOEFPTR(@(src_buffer^[offset_x]^
                              [dst_blk_y + offset_y]));
            dst_ptr := JCOEFPTR(@(dst_buffer^[offset_y]^
                              [dst_blk_x + offset_x]));
            for i := 0 to DCTSIZE-1 do
              for j := 0 to DCTSIZE-1 do
                dst_ptr^[j*DCTSIZE+i] := src_ptr^[i*DCTSIZE+j];
          end;
          Inc(dst_blk_x, compptr^.h_samp_factor);
        end;
      end;
      Inc(dst_blk_y, compptr^.v_samp_factor);
    end; { while }
  end;  { for ci }
end; { do_transpose }


{LOCAL}
procedure do_rot_90 (srcinfo : j_decompress_ptr;
                     dstinfo : j_compress_ptr;
                     src_coef_arrays : jvirt_barray_tbl_ptr;
                     dst_coef_arrays : jvirt_barray_tbl_ptr);
{ 90 degree rotation is equivalent to
    1. Transposing the image;
    2. Horizontal mirroring.
  These two steps are merged into a single processing routine. }
var
  MCU_cols, comp_width, dst_blk_x, dst_blk_y : JDIMENSION;
  ci, i, j, offset_x, offset_y : int;
  src_buffer, dst_buffer : JBLOCKARRAY;
  src_ptr, dst_ptr : JCOEFPTR;
  compptr : jpeg_component_info_ptr;
begin
  { Because of the horizontal mirror step, we can't process partial iMCUs
    at the (output) right edge properly.  They just get transposed and
    not mirrored. }

  MCU_cols := dstinfo^.image_width div (dstinfo^.max_h_samp_factor * DCTSIZE);

  for ci := 0 to dstinfo^.num_components-1 do
  begin
    compptr := jpeg_component_info_ptr(dstinfo^.comp_info);
    Inc(compptr, ci);
    comp_width := MCU_cols * compptr^.h_samp_factor;
    dst_blk_y := 0;
    while ( dst_blk_y < compptr^.height_in_blocks) do
    begin
      dst_buffer := srcinfo^.mem^.access_virt_barray
        (j_common_ptr(srcinfo), dst_coef_arrays^[ci], dst_blk_y,
         JDIMENSION (compptr^.v_samp_factor), TRUE);
      for offset_y := 0 to compptr^.v_samp_factor-1 do
      begin
        dst_blk_x := 0;
        while (dst_blk_x < compptr^.width_in_blocks) do
        begin
          src_buffer := srcinfo^.mem^.access_virt_barray
            (j_common_ptr(srcinfo), src_coef_arrays^[ci], dst_blk_x,
             JDIMENSION (compptr^.h_samp_factor), FALSE);
          for offset_x := 0 to compptr^.h_samp_factor-1 do
          begin
            src_ptr := JCOEFPTR(@(src_buffer^[offset_x]^
                                  [dst_blk_y + offset_y]));
            if (dst_blk_x < comp_width) then
            begin
              { Block is within the mirrorable area. }
              dst_ptr := JCOEFPTR(@(dst_buffer^[offset_y]^
                          [comp_width - dst_blk_x - offset_x - 1]));
              i := 0;
              while (i < DCTSIZE) do
              begin
                for j := 0 to DCTSIZE-1 do
                  dst_ptr^[j*DCTSIZE+i] := src_ptr^[i*DCTSIZE+j];
                Inc(i);
                for j := 0 to DCTSIZE-1 do
                  dst_ptr^[j*DCTSIZE+i] := -src_ptr^[i*DCTSIZE+j];
                Inc(i);
              end;
            end
            else
            begin
              { Edge blocks are transposed but not mirrored. }
              dst_ptr := JCOEFPTR(@(dst_buffer^[offset_y]^
                          [dst_blk_x + offset_x]));
              for i := 0 to DCTSIZE-1 do
                for j := 0 to DCTSIZE-1 do
                  dst_ptr^[j*DCTSIZE+i] := src_ptr^[i*DCTSIZE+j];
            end;
          end;
          Inc(dst_blk_x, compptr^.h_samp_factor);
        end;
      end;
      Inc(dst_blk_y, compptr^.v_samp_factor);
    end;  { while }
  end; { for ci }
end; { do_rot_90 }


{LOCAL}
procedure do_rot_270 (srcinfo : j_decompress_ptr;
                      dstinfo : j_compress_ptr;
                      src_coef_arrays : jvirt_barray_tbl_ptr;
                      dst_coef_arrays : jvirt_barray_tbl_ptr);
{ 270 degree rotation is equivalent to
    1. Horizontal mirroring;
    2. Transposing the image.
  These two steps are merged into a single processing routine. }
var
  MCU_rows, comp_height, dst_blk_x, dst_blk_y : JDIMENSION;
  ci, i, j, offset_x, offset_y : int;
  src_buffer, dst_buffer : JBLOCKARRAY;
  src_ptr, dst_ptr : JCOEFPTR;
  compptr : jpeg_component_info_ptr;
begin
  { Because of the horizontal mirror step, we can't process partial iMCUs
    at the (output) bottom edge properly.  They just get transposed and
    not mirrored. }

  MCU_rows := dstinfo^.image_height div (dstinfo^.max_v_samp_factor * DCTSIZE);

  for ci := 0 to dstinfo^.num_components-1 do
  begin
    compptr := jpeg_component_info_ptr(dstinfo^.comp_info);
    Inc(compptr, ci);
    comp_height := MCU_rows * compptr^.v_samp_factor;
    dst_blk_y := 0;
    while (dst_blk_y < compptr^.height_in_blocks) do
    begin
      dst_buffer := srcinfo^.mem^.access_virt_barray
        (j_common_ptr(srcinfo), dst_coef_arrays^[ci], dst_blk_y,
         JDIMENSION (compptr^.v_samp_factor), TRUE);
      for offset_y := 0 to compptr^.v_samp_factor-1 do
      begin
        dst_blk_x := 0;
        while (dst_blk_x < compptr^.width_in_blocks) do
        begin
          src_buffer := srcinfo^.mem^.access_virt_barray
            (j_common_ptr(srcinfo), src_coef_arrays^[ci], dst_blk_x,
             JDIMENSION (compptr^.h_samp_factor), FALSE);
          for offset_x := 0 to compptr^.h_samp_factor-1 do
          begin
            dst_ptr := JCOEFPTR(@(dst_buffer^[offset_y]^
                                 [dst_blk_x + offset_x]));
            if (dst_blk_y < comp_height) then
            begin
              { Block is within the mirrorable area. }
              src_ptr := JCOEFPTR(@(src_buffer^[offset_x]^
                           [comp_height - dst_blk_y - offset_y - 1]));
              for i := 0 to DCTSIZE-1 do
              begin
                j := 0;
                while (j < DCTSIZE) do
                begin
                  dst_ptr^[j*DCTSIZE+i] := src_ptr^[i*DCTSIZE+j];
                  Inc(j);
                  dst_ptr^[j*DCTSIZE+i] := -src_ptr^[i*DCTSIZE+j];
                  Inc(j);
                end;
              end;
            end
            else
            begin
              { Edge blocks are transposed but not mirrored. }
              src_ptr := JCOEFPTR(@(src_buffer^[offset_x]^
                            [dst_blk_y + offset_y]));
              for i := 0 to DCTSIZE-1 do
                for j := 0 to DCTSIZE-1 do
                  dst_ptr^[j*DCTSIZE+i] := src_ptr^[i*DCTSIZE+j];
            end;
          end;
          Inc(dst_blk_x, compptr^.h_samp_factor);
        end;
      end;
      Inc(dst_blk_y, compptr^.v_samp_factor);
    end; { while }
  end; { for ci }
end; { do_rot_270 }


{LOCAL}
procedure do_rot_180 (srcinfo : j_decompress_ptr;
                      dstinfo : j_compress_ptr;
                      src_coef_arrays : jvirt_barray_tbl_ptr;
                      dst_coef_arrays : jvirt_barray_tbl_ptr);
{ 180 degree rotation is equivalent to
    1. Vertical mirroring;
    2. Horizontal mirroring.
  These two steps are merged into a single processing routine. }
var
  MCU_cols, MCU_rows, comp_width, comp_height, dst_blk_x, dst_blk_y : JDIMENSION;
  ci, i, j, offset_y : int;
  src_buffer, dst_buffer : JBLOCKARRAY;
  src_row_ptr, dst_row_ptr : JBLOCKROW;
  src_ptr, dst_ptr : JCOEF_PTR;
  compptr : jpeg_component_info_ptr;
begin
  MCU_cols := dstinfo^.image_width div (dstinfo^.max_h_samp_factor * DCTSIZE);
  MCU_rows := dstinfo^.image_height div (dstinfo^.max_v_samp_factor * DCTSIZE);

  for ci := 0 to dstinfo^.num_components-1 do
  begin
    compptr := jpeg_component_info_ptr(dstinfo^.comp_info);
    Inc(compptr, ci);
    comp_width := MCU_cols * compptr^.h_samp_factor;
    comp_height := MCU_rows * compptr^.v_samp_factor;
    dst_blk_y := 0;
    while (dst_blk_y < compptr^.height_in_blocks) do
    begin
      dst_buffer := srcinfo^.mem^.access_virt_barray
        (j_common_ptr(srcinfo), dst_coef_arrays^[ci], dst_blk_y,
         JDIMENSION (compptr^.v_samp_factor), TRUE);
      if (dst_blk_y < comp_height) then
      begin
        { Row is within the vertically mirrorable area. }
        src_buffer := srcinfo^.mem^.access_virt_barray
          (j_common_ptr(srcinfo), src_coef_arrays^[ci],
           comp_height - dst_blk_y - JDIMENSION (compptr^.v_samp_factor),
           JDIMENSION (compptr^.v_samp_factor), FALSE);
      end
      else
      begin
        { Bottom-edge rows are only mirrored horizontally. }
        src_buffer := srcinfo^.mem^.access_virt_barray
          (j_common_ptr(srcinfo), src_coef_arrays^[ci], dst_blk_y,
           JDIMENSION (compptr^.v_samp_factor), FALSE);
      end;
      for offset_y := 0 to compptr^.v_samp_factor-1 do
      begin
        if (dst_blk_y < comp_height) then
        begin
          { Row is within the mirrorable area. }
          dst_row_ptr := dst_buffer^[offset_y];
          src_row_ptr := src_buffer^[compptr^.v_samp_factor - offset_y - 1];
          { Process the blocks that can be mirrored both ways. }
          for dst_blk_x := 0 to comp_width-1 do
          begin
            dst_ptr := JCOEF_PTR(@(dst_row_ptr^[dst_blk_x]));
            src_ptr := JCOEF_PTR(@(src_row_ptr^[comp_width - dst_blk_x - 1]));
            i := 0;
            while (i < DCTSIZE) do
            begin
              { For even row, negate every odd column. }
              j := 0;
              while (j < DCTSIZE) do
              begin
                dst_ptr^ := src_ptr^;
                Inc(dst_ptr);
                Inc(src_ptr);
                dst_ptr^ := - src_ptr^;
                Inc(dst_ptr);
                Inc(src_ptr);
                Inc(j, 2);
              end;
              { For odd row, negate every even column. }
              j := 0;
              while (j < DCTSIZE) do
              begin
                dst_ptr^ := - src_ptr^;
                Inc(dst_ptr);
                Inc(src_ptr);
                dst_ptr^ := src_ptr^;
                Inc(dst_ptr);
                Inc(src_ptr);
                Inc(j, 2);
              end;
              Inc(i, 2);
            end; { while i }
          end;
          { Any remaining right-edge blocks are only mirrored vertically. }
          for dst_blk_x := comp_width to compptr^.width_in_blocks-1 do
          begin
            dst_ptr := JCOEF_PTR(@(dst_row_ptr^[dst_blk_x]));
            src_ptr := JCOEF_PTR(@(src_row_ptr^[dst_blk_x]));
            i := 0;
            while (i < DCTSIZE) do
            begin
              for j := 0 to DCTSIZE-1 do
              begin
                dst_ptr^ := src_ptr^;
                Inc(dst_ptr);
                Inc(src_ptr);
              end;
              for j := 0 to DCTSIZE-1 do
              begin
                dst_ptr^ := - src_ptr^;
                Inc(dst_ptr);
                Inc(src_ptr);
              end;
              Inc(i, 2);
            end
          end
        end
        else
        begin
          { Remaining rows are just mirrored horizontally. }
          dst_row_ptr := dst_buffer^[offset_y];
          src_row_ptr := src_buffer^[offset_y];
          { Process the blocks that can be mirrored. }
          for dst_blk_x := 0 to comp_width-1 do
          begin
            dst_ptr := JCOEF_PTR(@(dst_row_ptr^[dst_blk_x]));
            src_ptr := JCOEF_PTR(@(src_row_ptr^[comp_width - dst_blk_x - 1]));
            i := 0;
            while (i < DCTSIZE2) do
            begin
              dst_ptr^ := src_ptr^;
              Inc(dst_ptr);
              Inc(src_ptr);
              dst_ptr^ := - src_ptr^;
              Inc(dst_ptr);
              Inc(src_ptr);
              Inc(i, 2);
            end;
          end;
          { Any remaining right-edge blocks are only copied. }
          for dst_blk_x := comp_width to compptr^.width_in_blocks-1 do
          begin
            dst_ptr := JCOEF_PTR(@(dst_row_ptr^[dst_blk_x]));
            src_ptr := JCOEF_PTR(@(src_row_ptr^[dst_blk_x]));
            for i := 0 to DCTSIZE2-1 do
            begin
              dst_ptr^ := src_ptr^;
              Inc(dst_ptr);
              Inc(src_ptr);
            end;
          end;
        end;
      end;
      Inc(dst_blk_y, compptr^.v_samp_factor) ;
    end; { while }
  end; { for ci }
end;  { do_rot_180 }


{LOCAL}
procedure do_transverse (srcinfo : j_decompress_ptr;
                         dstinfo : j_compress_ptr;
                         src_coef_arrays : jvirt_barray_tbl_ptr;
                         dst_coef_arrays : jvirt_barray_tbl_ptr);
{ Transverse transpose is equivalent to
    1. 180 degree rotation;
    2. Transposition;
  or
    1. Horizontal mirroring;
    2. Transposition;
    3. Horizontal mirroring.
  These steps are merged into a single processing routine. }
var
  MCU_cols, MCU_rows, comp_width, comp_height, dst_blk_x, dst_blk_y : JDIMENSION;
  ci, i, j, offset_x, offset_y : int;
  src_buffer, dst_buffer : JBLOCKARRAY;
  src_ptr, dst_ptr : JCOEFPTR;
  compptr : jpeg_component_info_ptr;
begin
  MCU_cols := dstinfo^.image_width div (dstinfo^.max_h_samp_factor * DCTSIZE);
  MCU_rows := dstinfo^.image_height div (dstinfo^.max_v_samp_factor * DCTSIZE);

  for ci := 0 to dstinfo^.num_components-1 do
  begin
    compptr := jpeg_component_info_ptr(dstinfo^.comp_info);
    Inc(compptr, ci);
    comp_width := MCU_cols * compptr^.h_samp_factor;
    comp_height := MCU_rows * compptr^.v_samp_factor;
    dst_blk_y := 0;
    while (dst_blk_y < compptr^.height_in_blocks) do
    begin
      dst_buffer := srcinfo^.mem^.access_virt_barray
        (j_common_ptr(srcinfo), dst_coef_arrays^[ci], dst_blk_y,
         JDIMENSION (compptr^.v_samp_factor), TRUE);
      for offset_y := 0 to compptr^.v_samp_factor-1 do
      begin
        dst_blk_x := 0;
        while ( dst_blk_x < compptr^.width_in_blocks) do
        begin
          src_buffer := srcinfo^.mem^.access_virt_barray
            (j_common_ptr(srcinfo), src_coef_arrays^[ci], dst_blk_x,
             JDIMENSION (compptr^.h_samp_factor), FALSE);
          for offset_x := 0 to compptr^.h_samp_factor-1 do
          begin
            if (dst_blk_y < comp_height) then
            begin
              src_ptr := JCOEFPTR(@(src_buffer^[offset_x]^
                [comp_height - dst_blk_y - offset_y - 1]));
              if (dst_blk_x < comp_width) then
              begin
                { Block is within the mirrorable area. }
                dst_ptr := JCOEFPTR(@(dst_buffer^[offset_y]^
                  [comp_width - dst_blk_x - offset_x - 1]));
                i := 0;
                while (i < DCTSIZE) do
                begin
                  j := 0;
                  while (j < DCTSIZE) do
                  begin
                    dst_ptr^[j*DCTSIZE+i] := src_ptr^[i*DCTSIZE+j];
                    Inc(j);
                    dst_ptr^[j*DCTSIZE+i] := -src_ptr^[i*DCTSIZE+j];
                    Inc(j);
                  end;
                  Inc(i);
                  j := 0;
                  while (j < DCTSIZE) do
                  begin
                    dst_ptr^[j*DCTSIZE+i] := -src_ptr^[i*DCTSIZE+j];
                    Inc(j);
                    dst_ptr^[j*DCTSIZE+i] := src_ptr^[i*DCTSIZE+j];
                    Inc(j);
                  end;
                  Inc(i);
                end
              end
              else
              begin
                { Right-edge blocks are mirrored in y only }
                dst_ptr := JCOEFPTR(@(dst_buffer^[offset_y]^
                              [dst_blk_x + offset_x]));
                for i := 0 to DCTSIZE-1 do
                begin
                  j := 0;
                  while (j < DCTSIZE) do
                  begin
                    dst_ptr^[j*DCTSIZE+i] := src_ptr^[i*DCTSIZE+j];
                    Inc(j);
                    dst_ptr^[j*DCTSIZE+i] := -src_ptr^[i*DCTSIZE+j];
                    Inc(j);
                  end;
                end;
              end;
            end
            else
            begin
              src_ptr := JCOEFPTR(@(src_buffer^[offset_x]^
                            [dst_blk_y + offset_y]));
              if (dst_blk_x < comp_width) then
              begin
                { Bottom-edge blocks are mirrored in x only }
                dst_ptr := JCOEFPTR(@(dst_buffer^[offset_y]^
                             [comp_width - dst_blk_x - offset_x - 1]));
                i := 0;
                while (i < DCTSIZE) do
                begin
                  for j := 0 to DCTSIZE-1 do
                    dst_ptr^[j*DCTSIZE+i] := src_ptr^[i*DCTSIZE+j];
                  Inc(i);
                  for j := 0 to DCTSIZE-1 do
                    dst_ptr^[j*DCTSIZE+i] := -src_ptr^[i*DCTSIZE+j];
                  Inc(i);
                end;
              end
              else
              begin
                { At lower right corner, just transpose, no mirroring }
                dst_ptr := JCOEFPTR(@(dst_buffer^[offset_y]^
                                 [dst_blk_x + offset_x]));
                for i := 0 to DCTSIZE-1 do
                  for j := 0 to DCTSIZE-1 do
                    dst_ptr^[j*DCTSIZE+i] := src_ptr^[i*DCTSIZE+j];
              end;
            end;
          end;
          Inc(dst_blk_x, compptr^.h_samp_factor);
        end;
      end;
      Inc(dst_blk_y, compptr^.v_samp_factor);
    end;  { while }
  end;  { for ci }
end; { do_transverse }


{ Request any required workspace.

  We allocate the workspace virtual arrays from the source decompression
  object, so that all the arrays (both the original data and the workspace)
  will be taken into account while making memory management decisions.
  Hence, this routine must be called after jpeg_read_header (which reads
  the image dimensions) and before jpeg_read_coefficients (which realizes
  the source's virtual arrays). }

{GLOBAL}
procedure jtransform_request_workspace (
                srcinfo : j_decompress_ptr;
                var info : jpeg_transform_info);
var
  coef_arrays : jvirt_barray_tbl_ptr;
  compptr : jpeg_component_info_ptr;
  ci : int;
begin
  coef_arrays := NIL;
  if (info.force_grayscale) and (srcinfo^.jpeg_color_space = JCS_YCbCr)
    and (srcinfo^.num_components = 3) then
  begin
    { We'll only process the first component }
    info.num_components := 1;
  end
  else
  begin
    { Process all the components }
    info.num_components := srcinfo^.num_components;
  end;

  case (info.transform) of
  JXFORM_NONE,
  JXFORM_FLIP_H:;
    { Don't need a workspace array }
{$ifdef CROP_SUPPORTED}
  JXFORM_CUT,
    { really cut needs smaller arrays if you want to figure it out }
{$endif}
  JXFORM_FLIP_V,
  JXFORM_ROT_180:
    begin
    { Need workspace arrays having same dimensions as source image.
      Note that we allocate arrays padded out to the next iMCU boundary,
      so that transform routines need not worry about missing edge blocks. }

      coef_arrays := jvirt_barray_tbl_ptr (
          srcinfo^.mem^.alloc_small (j_common_ptr(srcinfo), JPOOL_IMAGE,
          SIZEOF(jvirt_barray_ptr) * info.num_components) );
      for ci := 0 to info.num_components-1 do
      begin
        compptr := jpeg_component_info_ptr(srcinfo^.comp_info);
        Inc(compptr, ci);
        coef_arrays^[ci] := srcinfo^.mem^.request_virt_barray
          (j_common_ptr(srcinfo), JPOOL_IMAGE, FALSE,
           JDIMENSION (jround_up( long (compptr^.width_in_blocks),
                                  long (compptr^.h_samp_factor)) ),
           JDIMENSION (jround_up( long (compptr^.height_in_blocks),
                                  long (compptr^.v_samp_factor)) ),
           JDIMENSION (compptr^.v_samp_factor));
      end;
    end;
  JXFORM_TRANSPOSE,
  JXFORM_TRANSVERSE,
  JXFORM_ROT_90,
  JXFORM_ROT_270:
    begin
    { Need workspace arrays having transposed dimensions.
      Note that we allocate arrays padded out to the next iMCU boundary,
      so that transform routines need not worry about missing edge blocks. }

      coef_arrays := jvirt_barray_tbl_ptr(
          srcinfo^.mem^.alloc_small (j_common_ptr(srcinfo), JPOOL_IMAGE,
          SIZEOF(jvirt_barray_ptr) * info.num_components) );
      for ci := 0 to info.num_components-1 do
      begin
        compptr := jpeg_component_info_ptr(srcinfo^.comp_info);
        Inc(compptr, ci);
        coef_arrays^[ci] := srcinfo^.mem^.request_virt_barray
          (j_common_ptr(srcinfo), JPOOL_IMAGE, FALSE,
           JDIMENSION ( jround_up( long(compptr^.height_in_blocks),
                                   long(compptr^.v_samp_factor) ) ),
           JDIMENSION ( jround_up( long(compptr^.width_in_blocks),
                                   long(compptr^.h_samp_factor) ) ),
           JDIMENSION ( compptr^.h_samp_factor ) );
      end;
    end;
  end;
  info.workspace_coef_arrays := coef_arrays;
end;


{ Transpose destination image parameters }

{LOCAL}
procedure transpose_critical_parameters (dstinfo : j_compress_ptr);
var
  tblno, i, j, ci, itemp : int;
  compptr : jpeg_component_info_ptr;
  qtblptr : JQUANT_TBL_PTR;
  dtemp : JDIMENSION;
  qtemp : UINT16;
begin
  { Transpose basic image dimensions }
  dtemp := dstinfo^.image_width;
  dstinfo^.image_width := dstinfo^.image_height;
  dstinfo^.image_height := dtemp;

  { Transpose sampling factors }
  for ci := 0 to dstinfo^.num_components-1 do
  begin
    compptr := jpeg_component_info_ptr(dstinfo^.comp_info);
    Inc(compptr, ci);
    itemp := compptr^.h_samp_factor;
    compptr^.h_samp_factor := compptr^.v_samp_factor;
    compptr^.v_samp_factor := itemp;
  end;

  { Transpose quantization tables }
  for tblno := 0 to NUM_QUANT_TBLS-1 do
  begin
    qtblptr := dstinfo^.quant_tbl_ptrs[tblno];
    if (qtblptr <> NIL) then
    begin
      for i := 0 to DCTSIZE-1 do
      begin
        for j := 0 to i-1 do
        begin
          qtemp := qtblptr^.quantval[i*DCTSIZE+j];
          qtblptr^.quantval[i*DCTSIZE+j] := qtblptr^.quantval[j*DCTSIZE+i];
          qtblptr^.quantval[j*DCTSIZE+i] := qtemp;
        end;
      end;
    end;
  end;
end;


{ Trim off any partial iMCUs on the indicated destination edge }

{LOCAL}
procedure trim_right_edge (dstinfo : j_compress_ptr);
var
  ci, max_h_samp_factor : int;
  MCU_cols : JDIMENSION;
var
  h_samp_factor : int;
begin
  { We have to compute max_h_samp_factor ourselves,
    because it hasn't been set yet in the destination
    (and we don't want to use the source's value). }

  max_h_samp_factor := 1;
  for ci := 0 to dstinfo^.num_components-1 do
  begin
    h_samp_factor := dstinfo^.comp_info^[ci].h_samp_factor;

    {max_h_samp_factor := MAX(max_h_samp_factor, h_samp_factor);}
    if h_samp_factor > max_h_samp_factor then
      max_h_samp_factor := h_samp_factor;
  end;
  MCU_cols := dstinfo^.image_width div (max_h_samp_factor * DCTSIZE);
  if (MCU_cols > 0) then           { can't trim to 0 pixels }
    dstinfo^.image_width := MCU_cols * (max_h_samp_factor * DCTSIZE);
end;

{LOCAL}
procedure trim_bottom_edge (dstinfo : j_compress_ptr);
var
  ci, max_v_samp_factor : int;
  MCU_rows : JDIMENSION;
var
  v_samp_factor : int;
begin
  { We have to compute max_v_samp_factor ourselves,
    because it hasn't been set yet in the destination
    (and we don't want to use the source's value). }

  max_v_samp_factor := 1;
  for ci := 0 to dstinfo^.num_components-1 do
  begin
    v_samp_factor := dstinfo^.comp_info^[ci].v_samp_factor;

    {max_v_samp_factor := MAX(max_v_samp_factor, v_samp_factor);}
    if v_samp_factor > max_v_samp_factor then
      max_v_samp_factor := v_samp_factor;
  end;
  MCU_rows := dstinfo^.image_height div (max_v_samp_factor * DCTSIZE);
  if (MCU_rows > 0) then            { can't trim to 0 pixels }
    dstinfo^.image_height := MCU_rows * (max_v_samp_factor * DCTSIZE);
end;

{$ifdef CROP_SUPPORTED}
{ For cropping, realize and constrain the target area, and reshape the
  dstinfo to hold the resulting image.

  Input was supplied as WxH[+-]X[+-]Y offsets.  Negative offsets are
  relative to the lower righthand corner of the image.  The region is
  expanded so that all boundaries fall on even MCU blocks by rounding
  the offsets *down* (at the do_transform() step) and the size *up*. }

{LOCAL}
procedure set_dest_size(dstinfo : j_compress_ptr;
                        var info : jpeg_transform_info);
var
  ci, max_samp_factor : int;
  MCU_size, newsize, offset, factor : JDIMENSION;
var
  samp_factor : int;
begin
  { Initially the dstinfo is the same size as the srcinfo.
    Use it to constrain the offsets: }
  if (info.xoffs < 0) then
    Inc(info.xoffs, dstinfo^.image_width);
  if (info.yoffs < 0) then
    Inc(info.yoffs, dstinfo^.image_height);
  if (info.xoffs < 0) or (info.xoffs >= dstinfo^.image_width) or
     (info.yoffs < 0) or (info.yoffs >= dstinfo^.image_height) then
  begin
    {jpegtran_error('-cut offsets fall outside source image');}
    ERREXIT(j_common_ptr(dstinfo), JERR_CONVERSION_NOTIMPL);
  end;

  { use it to constrain the size: }
  if (info.newwidth + info.xoffs > dstinfo^.image_width) then
    info.newwidth := dstinfo^.image_width - info.xoffs;
  if (info.newheight + info.yoffs > dstinfo^.image_height) then
    info.newheight := dstinfo^.image_height - info.yoffs;

  { We have to compute max_v/h_samp_factors ourselves,
    because it hasn't been set yet in the destination
    (and we don't want to use the source's value). }
  max_samp_factor := 1;
  for ci := 0 to dstinfo^.num_components-1 do
  begin
    samp_factor := dstinfo^.comp_info^[ci].v_samp_factor;
    {max_samp_factor := MAX(max_samp_factor, samp_factor);}
    if (max_samp_factor < samp_factor) then
      max_samp_factor := samp_factor;
  end;
  { Find original (rounded down) and new (rounded up) heights in full
    dct blocks, choose the smaller of the two. }

  factor := max_samp_factor * DCTSIZE;
  MCU_size := dstinfo^.image_height div factor;
  newsize := (info.newheight + (info.yoffs mod factor) + factor - 1) div factor;
  {MCU_size := MIN(MCU_size, newsize);}
  if (MCU_size > newsize) then
    MCU_size := newsize;
  if (MCU_size > 0) then             { can't trim to 0 pixels }
    dstinfo^.image_height := MCU_size * factor
  else
  begin
    {jpegtran_error('degenerate -cut height');}
    ERREXIT(j_common_ptr(dstinfo), JERR_CONVERSION_NOTIMPL);
  end;

  max_samp_factor := 1;
  for ci := 0 to dstinfo^.num_components-1 do
  begin
    samp_factor := dstinfo^.comp_info^[ci].h_samp_factor;
    {max_samp_factor := MAX(max_samp_factor, samp_factor);}
    if (max_samp_factor < samp_factor) then
      max_samp_factor := samp_factor;
  end;
  { Find original (rounded down) and new (rounded up) heights in full
    dct blocks, choose the smaller of the two. }

  factor := max_samp_factor * DCTSIZE;
  MCU_size := dstinfo^.image_width div factor;
  newsize := (info.newwidth + (info.xoffs mod factor) + factor - 1) div factor;
  {MCU_size := MIN(MCU_size, newsize);}
  if (MCU_size > newsize) then
    MCU_size := newsize;
  if (MCU_size > 0) then             { can't trim to 0 pixels }
    dstinfo^.image_width := MCU_size * factor
  else
  begin
    {jpegtran_error('degenerate -cut width');}
    ERREXIT(j_common_ptr(dstinfo), JERR_CONVERSION_NOTIMPL);
  end;
end;
{$endif}

{ Adjust output image parameters as needed.

  This must be called after jpeg_copy_critical_parameters()
  and before jpeg_write_coefficients().

  The return value is the set of virtual coefficient arrays to be written
  (either the ones allocated by jtransform_request_workspace, or the
  original source data arrays).  The caller will need to pass this value
  to jpeg_write_coefficients(). }

{GLOBAL}
function jtransform_adjust_parameters
            (srcinfo : j_decompress_ptr;
             dstinfo : j_compress_ptr;
             src_coef_arrays : jvirt_barray_tbl_ptr;
             var info : jpeg_transform_info) : jvirt_barray_tbl_ptr;
var
  sv_quant_tbl_no : int;
begin
  { If force-to-grayscale is requested, adjust destination parameters }
  if (info.force_grayscale) then
  begin
    { We use jpeg_set_colorspace to make sure subsidiary settings get fixed
      properly.  Among other things, the target h_samp_factor & v_samp_factor
      will get set to 1, which typically won't match the source.
      In fact we do this even if the source is already grayscale; that
      provides an easy way of coercing a grayscale JPEG with funny sampling
      factors to the customary 1,1.  (Some decoders fail on other factors.) }

    if ((dstinfo^.jpeg_color_space = JCS_YCbCr) and
        (dstinfo^.num_components = 3)) or
       ((dstinfo^.jpeg_color_space = JCS_GRAYSCALE) and
        (dstinfo^.num_components = 1)) then
    begin
      { We have to preserve the source's quantization table number. }
      sv_quant_tbl_no := dstinfo^.comp_info^[0].quant_tbl_no;
      jpeg_set_colorspace(dstinfo, JCS_GRAYSCALE);
      dstinfo^.comp_info^[0].quant_tbl_no := sv_quant_tbl_no;
    end
    else
    begin
      { Sorry, can't do it }
      ERREXIT(j_common_ptr(dstinfo), JERR_CONVERSION_NOTIMPL);
    end;
  end;

  { Correct the destination's image dimensions etc if necessary }
  case (info.transform) of
  JXFORM_NONE:;
    { Nothing to do }
{$ifdef CROP_SUPPORTED}
  JXFORM_CUT:
    set_dest_size(dstinfo, info);
{$endif}
  JXFORM_FLIP_H:
    if (info.trim) then
      trim_right_edge(dstinfo);
  JXFORM_FLIP_V:
    if (info.trim) then
      trim_bottom_edge(dstinfo);
  JXFORM_TRANSPOSE:
    transpose_critical_parameters(dstinfo);
    { transpose does NOT have to trim anything }
  JXFORM_TRANSVERSE:
    begin
      transpose_critical_parameters(dstinfo);
      if (info.trim) then
      begin
        trim_right_edge(dstinfo);
        trim_bottom_edge(dstinfo);
      end;
    end;
  JXFORM_ROT_90:
    begin
      transpose_critical_parameters(dstinfo);
      if (info.trim) then
        trim_right_edge(dstinfo);
    end;
  JXFORM_ROT_180:
    if (info.trim) then
    begin
      trim_right_edge(dstinfo);
      trim_bottom_edge(dstinfo);
    end;
  JXFORM_ROT_270:
    begin
      transpose_critical_parameters(dstinfo);
      if (info.trim) then
        trim_bottom_edge(dstinfo);
    end;
  end;

  { Return the appropriate output data set }
  if (info.workspace_coef_arrays <> NIL) then
    jtransform_adjust_parameters := info.workspace_coef_arrays
  else
    jtransform_adjust_parameters := src_coef_arrays;
end;


{ Execute the actual transformation, if any.

  This must be called *after* jpeg_write_coefficients, because it depends
  on jpeg_write_coefficients to have computed subsidiary values such as
  the per-component width and height fields in the destination object.

  Note that some transformations will modify the source data arrays! }


{GLOBAL}
procedure jtransform_execute_transformation (
                srcinfo : j_decompress_ptr;
                dstinfo : j_compress_ptr;
                src_coef_arrays : jvirt_barray_tbl_ptr;
                var info : jpeg_transform_info);
var
  dst_coef_arrays : jvirt_barray_tbl_ptr;
begin
  dst_coef_arrays := info.workspace_coef_arrays;

  case (info.transform) of
  JXFORM_NONE:;
{$ifdef CROP_SUPPORTED}
  JXFORM_CUT:
    do_transform(srcinfo, dstinfo, src_coef_arrays, dst_coef_arrays,
                 info.xoffs, info.yoffs);
{$endif}
  JXFORM_FLIP_H:
    do_flip_h(srcinfo, dstinfo, src_coef_arrays);
  JXFORM_FLIP_V:
    do_flip_v(srcinfo, dstinfo, src_coef_arrays, dst_coef_arrays);
  JXFORM_TRANSPOSE:
    do_transpose(srcinfo, dstinfo, src_coef_arrays, dst_coef_arrays);
  JXFORM_TRANSVERSE:
    do_transverse(srcinfo, dstinfo, src_coef_arrays, dst_coef_arrays);
  JXFORM_ROT_90:
    do_rot_90(srcinfo, dstinfo, src_coef_arrays, dst_coef_arrays);
  JXFORM_ROT_180:
    do_rot_180(srcinfo, dstinfo, src_coef_arrays, dst_coef_arrays);
  JXFORM_ROT_270:
    do_rot_270(srcinfo, dstinfo, src_coef_arrays, dst_coef_arrays);
  end;
end;

{$endif} { TRANSFORMS_SUPPORTED }


{ Setup decompression object to save desired markers in memory.
  This must be called before jpeg_read_header() to have the desired effect. }

{GLOBAL}
procedure jcopy_markers_setup (srcinfo : j_decompress_ptr;
                               option : JCOPY_OPTION);
var
  m : int;
begin
{$ifdef SAVE_MARKERS_SUPPORTED}
  { Save comments except under NONE option }
  if (option <> JCOPYOPT_NONE) then
  begin
    jpeg_save_markers(srcinfo, JPEG_COM, $FFFF);
  end;
  { Save all types of APPn markers iff ALL option }
  if (option = JCOPYOPT_ALL) then
  begin
    for m := 0 to 16-1 do
      jpeg_save_markers(srcinfo, JPEG_APP0 + m, $FFFF);
  end;
{$endif} { SAVE_MARKERS_SUPPORTED }
end;

{ Copy markers saved in the given source object to the destination object.
  This should be called just after jpeg_start_compress() or
  jpeg_write_coefficients().
  Note that those routines will have written the SOI, and also the
  JFIF APP0 or Adobe APP14 markers if selected. }

{GLOBAL}
procedure jcopy_markers_execute (srcinfo : j_decompress_ptr;
                                 dstinfo : j_compress_ptr;
                                 option : JCOPY_OPTION);
var
  marker : jpeg_saved_marker_ptr;
{$ifdef NEED_FAR_POINTERS}
var
  i : uint;
{$endif}
begin
  { In the current implementation, we don't actually need to examine the
    option flag here; we just copy everything that got saved.
    But to avoid confusion, we do not output JFIF and Adobe APP14 markers
    if the encoder library already wrote one. }

  marker := srcinfo^.marker_list;
  while (marker <> NIL) do
  begin
    if (dstinfo^.write_JFIF_header) and
       (marker^.marker = JPEG_APP0) and
       (marker^.data_length >= 5) and
       ( GETJOCTET(marker^.data^[0]) = $4A ) and
       ( GETJOCTET(marker^.data^[1]) = $46 ) and
       ( GETJOCTET(marker^.data^[2]) = $49 ) and
       ( GETJOCTET(marker^.data^[3]) = $46 ) and
       ( GETJOCTET(marker^.data^[4]) = 0 ) then
    begin
      marker := marker^.next;
      continue;                 { reject duplicate JFIF }
    end;
    if (dstinfo^.write_Adobe_marker ) and
       ( marker^.marker = JPEG_APP0+14 ) and
       ( marker^.data_length >= 5 ) and
       ( GETJOCTET(marker^.data^[0]) = $41 ) and
       ( GETJOCTET(marker^.data^[1]) = $64 ) and
       ( GETJOCTET(marker^.data^[2]) = $6F ) and
       ( GETJOCTET(marker^.data^[3]) = $62 ) and
       ( GETJOCTET(marker^.data^[4]) = $65 ) then
    begin
      marker := marker^.next;
      continue;                 { reject duplicate Adobe }
    end;
{$ifdef NEED_FAR_POINTERS}
    { We could use jpeg_write_marker if the data weren't FAR... }
    begin
      jpeg_write_m_header(dstinfo, marker^.marker, marker^.data_length);
      for i := 0 to marker^.data_length-1 do
        jpeg_write_m_byte(dstinfo, marker^.data^[i]);
    end;
{$else}
    jpeg_write_marker(dstinfo, marker^.marker,
                      JOCTETPTR(marker^.data), marker^.data_length);
{$endif}
    marker := marker^.next;
  end;
end;

end.
