Unit JcColor;

{  This file contains input colorspace conversion routines. }

{ Original : jccolor.c ;  Copyright (C) 1991-1996, Thomas G. Lane. }

interface

{$I jconfig.inc}

uses
  jmorecfg,
  jinclude,
  jdeferr,
  jerror,
  jpeglib;

{ Module initialization routine for input colorspace conversion. }

{GLOBAL}
procedure jinit_color_converter (cinfo : j_compress_ptr);

implementation

{ Private subobject }
type
  jTInt32 = 0..Pred(MaxInt div SizeOf(INT32));
  INT32_FIELD = array[jTInt32] of INT32;
  INT32_FIELD_PTR = ^INT32_FIELD;

type
  my_cconvert_ptr = ^my_color_converter;
  my_color_converter = record
    pub : jpeg_color_converter; { public fields }

    { Private state for RGB -> YCC conversion }
    rgb_ycc_tab : INT32_FIELD_PTR;      { => table for RGB to YCbCr conversion }
  end; {my_color_converter;}


{*************** RGB -> YCbCr conversion: most common case *************}

{
  YCbCr is defined per CCIR 601-1, except that Cb and Cr are
  normalized to the range 0..MAXJSAMPLE rather than -0.5 .. 0.5.
  The conversion equations to be implemented are therefore
        Y  =  0.29900 * R + 0.58700 * G + 0.11400 * B
        Cb = -0.16874 * R - 0.33126 * G + 0.50000 * B  + CENTERJSAMPLE
        Cr =  0.50000 * R - 0.41869 * G - 0.08131 * B  + CENTERJSAMPLE
  (These numbers are derived from TIFF 6.0 section 21, dated 3-June-92.)
  Note: older versions of the IJG code used a zero offset of MAXJSAMPLE/2,
  rather than CENTERJSAMPLE, for Cb and Cr.  This gave equal positive and
  negative swings for Cb/Cr, but meant that grayscale values (Cb=Cr=0)
  were not represented exactly.  Now we sacrifice exact representation of
  maximum red and maximum blue in order to get exact grayscales.

  To avoid floating-point arithmetic, we represent the fractional constants
  as integers scaled up by 2^16 (about 4 digits precision); we have to divide
  the products by 2^16, with appropriate rounding, to get the correct answer.

  For even more speed, we avoid doing any multiplications in the inner loop
  by precalculating the constants times R,G,B for all possible values.
  For 8-bit JSAMPLEs this is very reasonable (only 256 entries per table);
  for 12-bit samples it is still acceptable.  It's not very reasonable for
  16-bit samples, but if you want lossless storage you shouldn't be changing
  colorspace anyway.
  The CENTERJSAMPLE offsets and the rounding fudge-factor of 0.5 are included
  in the tables to save adding them separately in the inner loop. }
const
  SCALEBITS   =  16;    { speediest right-shift on some machines }
  CBCR_OFFSET = INT32(CENTERJSAMPLE shl SCALEBITS);
  ONE_HALF    = INT32(1) shl (SCALEBITS-1);


{ We allocate one big table and divide it up into eight parts, instead of
  doing eight alloc_small requests.  This lets us use a single table base
  address, which can be held in a register in the inner loops on many
  machines (more than can hold all eight addresses, anyway). }

  R_Y_OFF     = 0;                              { offset to R => Y section }
  G_Y_OFF     = 1*(MAXJSAMPLE+1);               { offset to G => Y section }
  B_Y_OFF     = 2*(MAXJSAMPLE+1);               { etc. }
  R_CB_OFF    = 3*(MAXJSAMPLE+1);
  G_CB_OFF    = 4*(MAXJSAMPLE+1);
  B_CB_OFF    = 5*(MAXJSAMPLE+1);
  R_CR_OFF    = B_CB_OFF;                       { B=>Cb, R=>Cr are the same }
  G_CR_OFF    = 6*(MAXJSAMPLE+1);
  B_CR_OFF    = 7*(MAXJSAMPLE+1);
  TABLE_SIZE  = 8*(MAXJSAMPLE+1);


{ Initialize for RGB->YCC colorspace conversion. }

{METHODDEF}
procedure rgb_ycc_start (cinfo : j_compress_ptr); far;
const
  FIX_0_29900 = INT32(Round (0.29900 * (1 shl SCALEBITS)) );
  FIX_0_58700 = INT32(Round (0.58700 * (1 shl SCALEBITS)) );
  FIX_0_11400 = INT32(Round (0.11400 * (1 shl SCALEBITS)) );
  FIX_0_16874 = INT32(Round (0.16874 * (1 shl SCALEBITS)) );
  FIX_0_33126 = INT32(Round (0.33126 * (1 shl SCALEBITS)) );
  FIX_0_50000 = INT32(Round (0.50000 * (1 shl SCALEBITS)) );
  FIX_0_41869 = INT32(Round (0.41869 * (1 shl SCALEBITS)) );
  FIX_0_08131 = INT32(Round (0.08131 * (1 shl SCALEBITS)) );
var
  cconvert : my_cconvert_ptr;
  rgb_ycc_tab : INT32_FIELD_PTR;
  i : INT32;
begin
  cconvert := my_cconvert_ptr (cinfo^.cconvert);

  { Allocate and fill in the conversion tables. }
  rgb_ycc_tab := INT32_FIELD_PTR(
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                                (TABLE_SIZE * SIZEOF(INT32))) );
  cconvert^.rgb_ycc_tab := rgb_ycc_tab;

  for i := 0 to MAXJSAMPLE do
  begin
    rgb_ycc_tab^[i+R_Y_OFF] := FIX_0_29900 * i;
    rgb_ycc_tab^[i+G_Y_OFF] := FIX_0_58700 * i;
    rgb_ycc_tab^[i+B_Y_OFF] := FIX_0_11400 * i     + ONE_HALF;
    rgb_ycc_tab^[i+R_CB_OFF] := (-FIX_0_16874) * i;
    rgb_ycc_tab^[i+G_CB_OFF] := (-FIX_0_33126) * i;
    { We use a rounding fudge-factor of 0.5-epsilon for Cb and Cr.
      This ensures that the maximum output will round to MAXJSAMPLE
      not MAXJSAMPLE+1, and thus that we don't have to range-limit. }

    rgb_ycc_tab^[i+B_CB_OFF] := FIX_0_50000 * i    + CBCR_OFFSET + ONE_HALF-1;
{  B=>Cb and R=>Cr tables are the same
    rgb_ycc_tab^[i+R_CR_OFF] := FIX_0_50000 * i    + CBCR_OFFSET + ONE_HALF-1;
}
    rgb_ycc_tab^[i+G_CR_OFF] := (-FIX_0_41869) * i;
    rgb_ycc_tab^[i+B_CR_OFF] := (-FIX_0_08131) * i;
  end;
end;


{ Convert some rows of samples to the JPEG colorspace.

  Note that we change from the application's interleaved-pixel format
  to our internal noninterleaved, one-plane-per-component format.
  The input buffer is therefore three times as wide as the output buffer.

  A starting row offset is provided only for the output buffer.  The caller
  can easily adjust the passed input_buf value to accommodate any row
  offset required on that side. }

{METHODDEF}
procedure rgb_ycc_convert (cinfo : j_compress_ptr;
                           input_buf : JSAMPARRAY;
                           output_buf :  JSAMPIMAGE;
                           output_row : JDIMENSION;
                           num_rows : int); far;
var
  cconvert : my_cconvert_ptr;
  {register} r, g, b : int;
  {register} ctab : INT32_FIELD_PTR;
  {register} inptr : JSAMPROW;
  {register} outptr0, outptr1, outptr2 : JSAMPROW;
  {register} col : JDIMENSION;
  num_cols : JDIMENSION;
begin
  cconvert := my_cconvert_ptr (cinfo^.cconvert);
  ctab := cconvert^.rgb_ycc_tab;
  num_cols := cinfo^.image_width;

  while (num_rows > 0) do
  begin
    Dec(num_rows);
    inptr := input_buf^[0];
    Inc(JSAMPROW_PTR(input_buf));
    outptr0 := output_buf^[0]^[output_row];
    outptr1 := output_buf^[1]^[output_row];
    outptr2 := output_buf^[2]^[output_row];
    Inc(output_row);
    for col := 0 to pred(num_cols) do
    begin
      r := GETJSAMPLE(inptr^[RGB_RED]);
      g := GETJSAMPLE(inptr^[RGB_GREEN]);
      b := GETJSAMPLE(inptr^[RGB_BLUE]);
      Inc(JSAMPLE_PTR(inptr), RGB_PIXELSIZE);
      { If the inputs are 0..MAXJSAMPLE, the outputs of these equations
        must be too; we do not need an explicit range-limiting operation.
        Hence the value being shifted is never negative, and we don't
        need the general RIGHT_SHIFT macro. }

      { Y }
      outptr0^[col] := JSAMPLE(
                ((ctab^[r+R_Y_OFF] + ctab^[g+G_Y_OFF] + ctab^[b+B_Y_OFF])
                 shr SCALEBITS) );
      { Cb }
      outptr1^[col] := JSAMPLE(
                ((ctab^[r+R_CB_OFF] + ctab^[g+G_CB_OFF] + ctab^[b+B_CB_OFF])
                 shr SCALEBITS) );
      { Cr }
      outptr2^[col] := JSAMPLE(
                ((ctab^[r+R_CR_OFF] + ctab^[g+G_CR_OFF] + ctab^[b+B_CR_OFF])
                 shr SCALEBITS) );
    end;
  end;
end;


{*************** Cases other than RGB -> YCbCr *************}


{ Convert some rows of samples to the JPEG colorspace.
  This version handles RGB -> grayscale conversion, which is the same
  as the RGB -> Y portion of RGB -> YCbCr.
  We assume rgb_ycc_start has been called (we only use the Y tables). }

{METHODDEF}
procedure rgb_gray_convert (cinfo : j_compress_ptr;
                            input_buf : JSAMPARRAY;
                            output_buf : JSAMPIMAGE;
                            output_row : JDIMENSION;
                            num_rows : int); far;
var
  cconvert : my_cconvert_ptr;
  {register} r, g, b : int;
  {register} ctab :INT32_FIELD_PTR;
  {register} inptr : JSAMPROW;
  {register} outptr : JSAMPROW;
  {register} col : JDIMENSION;
  num_cols : JDIMENSION;
begin
  cconvert := my_cconvert_ptr (cinfo^.cconvert);
  ctab := cconvert^.rgb_ycc_tab;
  num_cols := cinfo^.image_width;

  while (num_rows > 0) do
  begin
    Dec(num_rows);
    inptr := input_buf^[0];
    Inc(JSAMPROW_PTR(input_buf));
    outptr := output_buf^[0]^[output_row];
    Inc(output_row);
    for col := 0 to pred(num_cols) do
    begin
      r := GETJSAMPLE(inptr^[RGB_RED]);
      g := GETJSAMPLE(inptr^[RGB_GREEN]);
      b := GETJSAMPLE(inptr^[RGB_BLUE]);
      Inc(JSAMPLE_PTR(inptr), RGB_PIXELSIZE);
      { Y }
      outptr^[col] := JSAMPLE (
                ((ctab^[r+R_Y_OFF] + ctab^[g+G_Y_OFF] + ctab^[b+B_Y_OFF])
                 shr SCALEBITS) );
    end;
  end;
end;


{ Convert some rows of samples to the JPEG colorspace.
  This version handles Adobe-style CMYK -> YCCK conversion,
  where we convert R=1-C, G=1-M, and B=1-Y to YCbCr using the same
  conversion as above, while passing K (black) unchanged.
  We assume rgb_ycc_start has been called. }

{METHODDEF}
procedure cmyk_ycck_convert (cinfo : j_compress_ptr;
                             input_buf : JSAMPARRAY;
                             output_buf : JSAMPIMAGE;
                             output_row : JDIMENSION;
                             num_rows : int); far;
var
  cconvert : my_cconvert_ptr;
  {register} r, g, b : int;
  {register} ctab : INT32_FIELD_PTR;
  {register} inptr : JSAMPROW;
  {register} outptr0, outptr1, outptr2, outptr3 : JSAMPROW;
  {register} col : JDIMENSION;
  num_cols : JDIMENSION;
begin
  cconvert := my_cconvert_ptr (cinfo^.cconvert);
  ctab := cconvert^.rgb_ycc_tab;
  num_cols := cinfo^.image_width;

  while (num_rows > 0) do
  begin
    Dec(num_rows);
    inptr := input_buf^[0];
    Inc(JSAMPROW_PTR(input_buf));
    outptr0 := output_buf^[0]^[output_row];
    outptr1 := output_buf^[1]^[output_row];
    outptr2 := output_buf^[2]^[output_row];
    outptr3 := output_buf^[3]^[output_row];
    Inc(output_row);
    for col := 0 to pred(num_cols) do
    begin
      r := MAXJSAMPLE - GETJSAMPLE(inptr^[0]);
      g := MAXJSAMPLE - GETJSAMPLE(inptr^[1]);
      b := MAXJSAMPLE - GETJSAMPLE(inptr^[2]);
      { K passes through as-is }
      outptr3^[col] := inptr^[3];       { don't need GETJSAMPLE here }
      Inc(JSAMPLE_PTR(inptr), 4);
      { If the inputs are 0..MAXJSAMPLE, the outputs of these equations
        must be too; we do not need an explicit range-limiting operation.
        Hence the value being shifted is never negative, and we don't
        need the general RIGHT_SHIFT macro. }

      { Y }
      outptr0^[col] := JSAMPLE (
                ((ctab^[r+R_Y_OFF] + ctab^[g+G_Y_OFF] + ctab^[b+B_Y_OFF])
                 shr SCALEBITS) );
      { Cb }
      outptr1^[col] := JSAMPLE(
                ((ctab^[r+R_CB_OFF] + ctab^[g+G_CB_OFF] + ctab^[b+B_CB_OFF])
                 shr SCALEBITS) );
      { Cr }
      outptr2^[col] := JSAMPLE (
                ((ctab^[r+R_CR_OFF] + ctab^[g+G_CR_OFF] + ctab^[b+B_CR_OFF])
                 shr SCALEBITS) );
    end;
  end;
end;


{ Convert some rows of samples to the JPEG colorspace.
  This version handles grayscale output with no conversion.
  The source can be either plain grayscale or YCbCr (since Y = gray). }

{METHODDEF}
procedure grayscale_convert (cinfo : j_compress_ptr;
                            input_buf : JSAMPARRAY;
                            output_buf : JSAMPIMAGE;
                            output_row : JDIMENSION;
                            num_rows: int); far;
var
  {register} inptr : JSAMPROW;
  {register} outptr : JSAMPROW;
  {register} col : JDIMENSION;
  num_cols :JDIMENSION;
  instride : int;
begin
  num_cols := cinfo^.image_width;
  instride := cinfo^.input_components;

  while (num_rows > 0) do
  begin
    Dec(num_rows);
    inptr := input_buf^[0];
    Inc(JSAMPROW_PTR(input_buf));
    outptr := output_buf^[0]^[output_row];
    Inc(output_row);
    for col := 0 to pred(num_cols) do
    begin
      outptr^[col] := inptr^[0];        { don't need GETJSAMPLE() here }
      Inc(JSAMPLE_PTR(inptr), instride);
    end;
  end;
end;


{ Convert some rows of samples to the JPEG colorspace.
  This version handles multi-component colorspaces without conversion.
  We assume input_components = num_components. }

{METHODDEF}
procedure null_convert (cinfo : j_compress_ptr;
                        input_buf : JSAMPARRAY;
                        output_buf : JSAMPIMAGE;
                        output_row : JDIMENSION;
                        num_rows : int); far;
var
  {register} inptr : JSAMPROW;
  {register} outptr : JSAMPROW;
  {register} col : JDIMENSION;
  {register} ci : int;
  nc : int;
  num_cols : JDIMENSION;
begin
  nc := cinfo^.num_components;
  num_cols := cinfo^.image_width;

  while (num_rows > 0) do
  begin
    Dec(num_rows);
    { It seems fastest to make a separate pass for each component. }
    for ci := 0 to pred(nc) do
    begin
      inptr := input_buf^[0];
      outptr := output_buf^[ci]^[output_row];
      for col := 0 to pred(num_cols) do
      begin
        outptr^[col] := inptr^[ci]; { don't need GETJSAMPLE() here }
        Inc(JSAMPLE_PTR(inptr), nc);
      end;
    end;
    Inc(JSAMPROW_PTR(input_buf));
    Inc(output_row);
  end;
end;


{ Empty method for start_pass. }

{METHODDEF}
procedure null_method (cinfo : j_compress_ptr); far;
begin
  { no work needed }
end;


{ Module initialization routine for input colorspace conversion. }

{GLOBAL}
procedure jinit_color_converter (cinfo : j_compress_ptr);
var
  cconvert : my_cconvert_ptr;
begin
  cconvert := my_cconvert_ptr(
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                                SIZEOF(my_color_converter)) );
  cinfo^.cconvert := jpeg_color_converter_ptr(cconvert);
  { set start_pass to null method until we find out differently }
  cconvert^.pub.start_pass := null_method;

  { Make sure input_components agrees with in_color_space }
  case (cinfo^.in_color_space) of
  JCS_GRAYSCALE:
    if (cinfo^.input_components <> 1) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_IN_COLORSPACE);

{$ifdef RGB_PIXELSIZE <> 3}
  JCS_RGB:
    if (cinfo^.input_components <> RGB_PIXELSIZE) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_IN_COLORSPACE);
{$else} { share code with YCbCr }
  JCS_RGB,
{$endif}
  JCS_YCbCr:
    if (cinfo^.input_components <> 3) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_IN_COLORSPACE);

  JCS_CMYK,
  JCS_YCCK:
    if (cinfo^.input_components <> 4) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_IN_COLORSPACE);

  else                  { JCS_UNKNOWN can be anything }
    if (cinfo^.input_components < 1) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_IN_COLORSPACE);
  end;

  { Check num_components, set conversion method based on requested space }
  case (cinfo^.jpeg_color_space) of
  JCS_GRAYSCALE:
    begin
      if (cinfo^.num_components <> 1) then
        ERREXIT(j_common_ptr(cinfo), JERR_BAD_J_COLORSPACE);
      if (cinfo^.in_color_space = JCS_GRAYSCALE) then
        cconvert^.pub.color_convert := grayscale_convert
      else
        if (cinfo^.in_color_space = JCS_RGB) then
        begin
          cconvert^.pub.start_pass := rgb_ycc_start;
          cconvert^.pub.color_convert := rgb_gray_convert;
        end
        else
          if (cinfo^.in_color_space = JCS_YCbCr) then
            cconvert^.pub.color_convert := grayscale_convert
          else
            ERREXIT(j_common_ptr(cinfo), JERR_CONVERSION_NOTIMPL);
    end;

  JCS_RGB:
    begin
      if (cinfo^.num_components <> 3) then
        ERREXIT(j_common_ptr(cinfo), JERR_BAD_J_COLORSPACE);
        if (cinfo^.in_color_space = JCS_RGB) and (RGB_PIXELSIZE = 3) then
          cconvert^.pub.color_convert := null_convert
        else
          ERREXIT(j_common_ptr(cinfo), JERR_CONVERSION_NOTIMPL);
    end;

  JCS_YCbCr:
    begin
      if (cinfo^.num_components <> 3) then
        ERREXIT(j_common_ptr(cinfo), JERR_BAD_J_COLORSPACE);
      if (cinfo^.in_color_space = JCS_RGB) then
      begin
        cconvert^.pub.start_pass := rgb_ycc_start;
        cconvert^.pub.color_convert := rgb_ycc_convert;
      end
      else
        if (cinfo^.in_color_space = JCS_YCbCr) then
          cconvert^.pub.color_convert := null_convert
        else
          ERREXIT(j_common_ptr(cinfo), JERR_CONVERSION_NOTIMPL);
    end;

  JCS_CMYK:
    begin
      if (cinfo^.num_components <> 4) then
        ERREXIT(j_common_ptr(cinfo), JERR_BAD_J_COLORSPACE);
      if (cinfo^.in_color_space = JCS_CMYK) then
        cconvert^.pub.color_convert := null_convert
      else
        ERREXIT(j_common_ptr(cinfo), JERR_CONVERSION_NOTIMPL);
    end;

  JCS_YCCK:
    begin
      if (cinfo^.num_components <> 4) then
        ERREXIT(j_common_ptr(cinfo), JERR_BAD_J_COLORSPACE);
      if (cinfo^.in_color_space = JCS_CMYK) then
      begin
        cconvert^.pub.start_pass := rgb_ycc_start;
        cconvert^.pub.color_convert := cmyk_ycck_convert;
      end
      else
        if (cinfo^.in_color_space = JCS_YCCK) then
          cconvert^.pub.color_convert := null_convert
        else
          ERREXIT(j_common_ptr(cinfo), JERR_CONVERSION_NOTIMPL);
    end;

  else          { allow null conversion of JCS_UNKNOWN }
    begin
      if (cinfo^.jpeg_color_space <> cinfo^.in_color_space) or
         (cinfo^.num_components <> cinfo^.input_components) then
        ERREXIT(j_common_ptr(cinfo), JERR_CONVERSION_NOTIMPL);
      cconvert^.pub.color_convert := null_convert;
    end;
  end;
end;

end.
