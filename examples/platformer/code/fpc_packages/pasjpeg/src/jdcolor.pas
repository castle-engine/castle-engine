Unit JdColor;

{ This file contains output colorspace conversion routines. }

{ Original: jdcolor.c ; Copyright (C) 1991-1997, Thomas G. Lane. }

interface

{$I jconfig.inc}

uses
  jmorecfg,
  jinclude,
  jutils,
  jdeferr,
  jerror,
  jpeglib;

{ Module initialization routine for output colorspace conversion. }

{GLOBAL}
procedure jinit_color_deconverter (cinfo : j_decompress_ptr);

implementation

{ Private subobject }
type
  int_Color_Table = array[0..MAXJSAMPLE+1-1] of int;
  int_table_ptr = ^int_Color_Table;
  INT32_Color_Table = array[0..MAXJSAMPLE+1-1] of INT32;
  INT32_table_ptr = ^INT32_Color_Table;
type
  my_cconvert_ptr = ^my_color_deconverter;
  my_color_deconverter = record
    pub : jpeg_color_deconverter; { public fields }

    { Private state for YCC^.RGB conversion }
    Cr_r_tab : int_table_ptr;   { => table for Cr to R conversion }
    Cb_b_tab : int_table_ptr;   { => table for Cb to B conversion }
    Cr_g_tab : INT32_table_ptr; { => table for Cr to G conversion }
    Cb_g_tab : INT32_table_ptr; { => table for Cb to G conversion }
  end;




{*************** YCbCr ^. RGB conversion: most common case *************}

{ YCbCr is defined per CCIR 601-1, except that Cb and Cr are
  normalized to the range 0..MAXJSAMPLE rather than -0.5 .. 0.5.
  The conversion equations to be implemented are therefore
        R = Y                + 1.40200 * Cr
        G = Y - 0.34414 * Cb - 0.71414 * Cr
        B = Y + 1.77200 * Cb
  where Cb and Cr represent the incoming values less CENTERJSAMPLE.
  (These numbers are derived from TIFF 6.0 section 21, dated 3-June-92.)

  To avoid floating-point arithmetic, we represent the fractional constants
  as integers scaled up by 2^16 (about 4 digits precision); we have to divide
  the products by 2^16, with appropriate rounding, to get the correct answer.
  Notice that Y, being an integral input, does not contribute any fraction
  so it need not participate in the rounding.

  For even more speed, we avoid doing any multiplications in the inner loop
  by precalculating the constants times Cb and Cr for all possible values.
  For 8-bit JSAMPLEs this is very reasonable (only 256 entries per table);
  for 12-bit samples it is still acceptable.  It's not very reasonable for
  16-bit samples, but if you want lossless storage you shouldn't be changing
  colorspace anyway.
  The Cr=>R and Cb=>B values can be rounded to integers in advance; the
  values for the G calculation are left scaled up, since we must add them
  together before rounding. }

const
  SCALEBITS = 16;      { speediest right-shift on some machines }
  ONE_HALF  = (INT32(1) shl (SCALEBITS-1));


{ Initialize tables for YCC->RGB colorspace conversion. }

{LOCAL}
procedure build_ycc_rgb_table (cinfo : j_decompress_ptr);
const
  FIX_1_40200 = INT32(Round( 1.40200  * (1 shl SCALEBITS)));
  FIX_1_77200 = INT32(Round( 1.77200  * (1 shl SCALEBITS)));
  FIX_0_71414 = INT32(Round( 0.71414  * (1 shl SCALEBITS)));
  FIX_0_34414 = INT32(Round( 0.34414  * (1 shl SCALEBITS)));

var
  cconvert : my_cconvert_ptr;
  i : int;
  x : INT32;
var
  shift_temp : INT32;
begin
  cconvert := my_cconvert_ptr (cinfo^.cconvert);


  cconvert^.Cr_r_tab := int_table_ptr(
    cinfo^.mem^.alloc_small ( j_common_ptr(cinfo), JPOOL_IMAGE,
                                (MAXJSAMPLE+1) * SIZEOF(int)) );
  cconvert^.Cb_b_tab := int_table_ptr (
    cinfo^.mem^.alloc_small ( j_common_ptr(cinfo), JPOOL_IMAGE,
                                (MAXJSAMPLE+1) * SIZEOF(int)) );
  cconvert^.Cr_g_tab := INT32_table_ptr (
    cinfo^.mem^.alloc_small ( j_common_ptr(cinfo), JPOOL_IMAGE,
                                (MAXJSAMPLE+1) * SIZEOF(INT32)) );
  cconvert^.Cb_g_tab := INT32_table_ptr (
    cinfo^.mem^.alloc_small ( j_common_ptr(cinfo), JPOOL_IMAGE,
                                (MAXJSAMPLE+1) * SIZEOF(INT32)) );


  x := -CENTERJSAMPLE;
  for i := 0 to MAXJSAMPLE do
  begin
    { i is the actual input pixel value, in the range 0..MAXJSAMPLE }
    { The Cb or Cr value we are thinking of is x := i - CENTERJSAMPLE }
    { Cr=>R value is nearest int to 1.40200 * x }

    shift_temp := FIX_1_40200  * x + ONE_HALF;
    if shift_temp < 0 then  { SHIFT arithmetic RIGHT }
      cconvert^.Cr_r_tab^[i] := int((shift_temp shr SCALEBITS)
                             or ( (not INT32(0)) shl (32-SCALEBITS)))
    else
      cconvert^.Cr_r_tab^[i] := int(shift_temp shr SCALEBITS);

    { Cb=>B value is nearest int to 1.77200 * x }
    shift_temp := FIX_1_77200  * x + ONE_HALF;
    if shift_temp < 0 then   { SHIFT arithmetic RIGHT }
      cconvert^.Cb_b_tab^[i] := int((shift_temp shr SCALEBITS)
                                or ( (not INT32(0)) shl (32-SCALEBITS)))
    else
      cconvert^.Cb_b_tab^[i] := int(shift_temp shr SCALEBITS);

    { Cr=>G value is scaled-up -0.71414 * x }
    cconvert^.Cr_g_tab^[i] := (- FIX_0_71414 ) * x;
    { Cb=>G value is scaled-up -0.34414 * x }
    { We also add in ONE_HALF so that need not do it in inner loop }
    cconvert^.Cb_g_tab^[i] := (- FIX_0_34414 ) * x + ONE_HALF;
    Inc(x);
  end;
end;


{ Convert some rows of samples to the output colorspace.

  Note that we change from noninterleaved, one-plane-per-component format
  to interleaved-pixel format.  The output buffer is therefore three times
  as wide as the input buffer.
  A starting row offset is provided only for the input buffer.  The caller
  can easily adjust the passed output_buf value to accommodate any row
  offset required on that side. }

{METHODDEF}
procedure ycc_rgb_convert (cinfo : j_decompress_ptr;
                           input_buf : JSAMPIMAGE;
                           input_row : JDIMENSION;
                           output_buf : JSAMPARRAY;
                           num_rows : int); far;
var
  cconvert : my_cconvert_ptr;
  {register} y, cb, cr : int;
  {register} outptr : JSAMPROW;
  {register} inptr0, inptr1, inptr2 : JSAMPROW;
  {register} col : JDIMENSION;
  num_cols : JDIMENSION;
  { copy these pointers into registers if possible }
  {register} range_limit : range_limit_table_ptr;
  {register} Crrtab : int_table_ptr;
  {register} Cbbtab : int_table_ptr;
  {register} Crgtab : INT32_table_ptr;
  {register} Cbgtab : INT32_table_ptr;
var
  shift_temp : INT32;
begin
  cconvert := my_cconvert_ptr (cinfo^.cconvert);
  num_cols := cinfo^.output_width;
  range_limit := cinfo^.sample_range_limit;
  Crrtab := cconvert^.Cr_r_tab;
  Cbbtab := cconvert^.Cb_b_tab;
  Crgtab := cconvert^.Cr_g_tab;
  Cbgtab := cconvert^.Cb_g_tab;

  while (num_rows > 0) do
  begin
    Dec(num_rows);
    inptr0 := input_buf^[0]^[input_row];
    inptr1 := input_buf^[1]^[input_row];
    inptr2 := input_buf^[2]^[input_row];
    Inc(input_row);
    outptr := output_buf^[0];
    Inc(JSAMPROW_PTR(output_buf));
    for col := 0 to pred(num_cols) do
    begin
      y  := GETJSAMPLE(inptr0^[col]);
      cb := GETJSAMPLE(inptr1^[col]);
      cr := GETJSAMPLE(inptr2^[col]);
      { Range-limiting is essential due to noise introduced by DCT losses. }
      outptr^[RGB_RED] :=   range_limit^[y + Crrtab^[cr]];
      shift_temp := Cbgtab^[cb] + Crgtab^[cr];
      if shift_temp < 0 then   { SHIFT arithmetic RIGHT }
        outptr^[RGB_GREEN] := range_limit^[y + int((shift_temp shr SCALEBITS)
                              or ( (not INT32(0)) shl (32-SCALEBITS)))]
      else
        outptr^[RGB_GREEN] := range_limit^[y + int(shift_temp shr SCALEBITS)];

      outptr^[RGB_BLUE] :=  range_limit^[y + Cbbtab^[cb]];
      Inc(JSAMPLE_PTR(outptr), RGB_PIXELSIZE);
    end;
  end;
end;


{*************** Cases other than YCbCr -> RGB *************}


{ Color conversion for no colorspace change: just copy the data,
  converting from separate-planes to interleaved representation. }

{METHODDEF}
procedure null_convert (cinfo : j_decompress_ptr;
                        input_buf : JSAMPIMAGE;
                        input_row : JDIMENSION;
                        output_buf : JSAMPARRAY;
                        num_rows : int); far;
var
  {register} inptr,
             outptr : JSAMPLE_PTR;
  {register} count : JDIMENSION;
  {register} num_components : int;
  num_cols : JDIMENSION;
  ci : int;
begin
  num_components := cinfo^.num_components;
  num_cols := cinfo^.output_width;

  while (num_rows > 0) do
  begin
    Dec(num_rows);
    for ci := 0 to pred(num_components) do
    begin
      inptr := JSAMPLE_PTR(input_buf^[ci]^[input_row]);
      outptr := JSAMPLE_PTR(@(output_buf^[0]^[ci]));

      for count := pred(num_cols) downto 0 do
      begin
        outptr^ := inptr^;      { needn't bother with GETJSAMPLE() here }
        Inc(inptr);
        Inc(outptr, num_components);
      end;
    end;
    Inc(input_row);
    Inc(JSAMPROW_PTR(output_buf));
  end;
end;


{ Color conversion for grayscale: just copy the data.
  This also works for YCbCr -> grayscale conversion, in which
  we just copy the Y (luminance) component and ignore chrominance. }

{METHODDEF}
procedure grayscale_convert (cinfo : j_decompress_ptr;
                             input_buf : JSAMPIMAGE;
                             input_row : JDIMENSION;
                             output_buf : JSAMPARRAY;
                             num_rows : int); far;
begin
  jcopy_sample_rows(input_buf^[0], int(input_row), output_buf, 0,
                    num_rows, cinfo^.output_width);
end;

{ Convert grayscale to RGB: just duplicate the graylevel three times.
  This is provided to support applications that don't want to cope
  with grayscale as a separate case. }

{METHODDEF}
procedure gray_rgb_convert (cinfo : j_decompress_ptr;
                            input_buf : JSAMPIMAGE;
                            input_row : JDIMENSION;
                            output_buf : JSAMPARRAY;
                            num_rows : int); far;
var
  {register} inptr, outptr : JSAMPLE_PTR;
  {register} col : JDIMENSION;
  num_cols : JDIMENSION;
begin
  num_cols := cinfo^.output_width;
  while (num_rows > 0) do
  begin
    inptr := JSAMPLE_PTR(input_buf^[0]^[input_row]);
    Inc(input_row);
    outptr := JSAMPLE_PTR(@output_buf^[0]);
    Inc(JSAMPROW_PTR(output_buf));
    for col := 0 to pred(num_cols) do
    begin
      { We can dispense with GETJSAMPLE() here }
      JSAMPROW(outptr)^[RGB_RED] := inptr^;
      JSAMPROW(outptr)^[RGB_GREEN] := inptr^;
      JSAMPROW(outptr)^[RGB_BLUE] := inptr^;
      Inc(inptr);
      Inc(outptr, RGB_PIXELSIZE);
    end;
    Dec(num_rows);
  end;
end;


{ Adobe-style YCCK -> CMYK conversion.
  We convert YCbCr to R=1-C, G=1-M, and B=1-Y using the same
  conversion as above, while passing K (black) unchanged.
  We assume build_ycc_rgb_table has been called. }

{METHODDEF}
procedure ycck_cmyk_convert (cinfo : j_decompress_ptr;
                             input_buf : JSAMPIMAGE;
                             input_row : JDIMENSION;
                             output_buf : JSAMPARRAY;
                             num_rows : int); far;
var
  cconvert : my_cconvert_ptr;
  {register} y, cb, cr : int;
  {register} outptr : JSAMPROW;
  {register} inptr0, inptr1, inptr2, inptr3 : JSAMPROW;
  {register} col : JDIMENSION;
  num_cols : JDIMENSION;
  { copy these pointers into registers if possible }
  {register} range_limit : range_limit_table_ptr;
  {register} Crrtab : int_table_ptr;
  {register} Cbbtab : int_table_ptr;
  {register} Crgtab : INT32_table_ptr;
  {register} Cbgtab : INT32_table_ptr;
var
  shift_temp : INT32;
begin
  cconvert := my_cconvert_ptr (cinfo^.cconvert);
  num_cols := cinfo^.output_width;
  { copy these pointers into registers if possible }
  range_limit := cinfo^.sample_range_limit;
  Crrtab := cconvert^.Cr_r_tab;
  Cbbtab := cconvert^.Cb_b_tab;
  Crgtab := cconvert^.Cr_g_tab;
  Cbgtab := cconvert^.Cb_g_tab;

  while (num_rows > 0) do
  begin
    Dec(num_rows);
    inptr0 := input_buf^[0]^[input_row];
    inptr1 := input_buf^[1]^[input_row];
    inptr2 := input_buf^[2]^[input_row];
    inptr3 := input_buf^[3]^[input_row];
    Inc(input_row);
    outptr := output_buf^[0];
    Inc(JSAMPROW_PTR(output_buf));
    for col := 0 to pred(num_cols) do
    begin
      y  := GETJSAMPLE(inptr0^[col]);
      cb := GETJSAMPLE(inptr1^[col]);
      cr := GETJSAMPLE(inptr2^[col]);
      { Range-limiting is essential due to noise introduced by DCT losses. }
      outptr^[0] := range_limit^[MAXJSAMPLE - (y + Crrtab^[cr])];       { red }
      shift_temp := Cbgtab^[cb] + Crgtab^[cr];
      if shift_temp < 0 then
        outptr^[1] := range_limit^[MAXJSAMPLE - (y + int(
          (shift_temp shr SCALEBITS) or ((not INT32(0)) shl (32-SCALEBITS))
                                                        ) )]
      else
        outptr^[1] := range_limit^[MAXJSAMPLE -             { green }
                    (y + int(shift_temp shr SCALEBITS) )];
      outptr^[2] := range_limit^[MAXJSAMPLE - (y + Cbbtab^[cb])];       { blue }
      { K passes through unchanged }
      outptr^[3] := inptr3^[col];       { don't need GETJSAMPLE here }
      Inc(JSAMPLE_PTR(outptr), 4);
    end;
  end;
end;


{ Empty method for start_pass. }

{METHODDEF}
procedure start_pass_dcolor (cinfo : j_decompress_ptr); far;
begin
  { no work needed }
end;


{ Module initialization routine for output colorspace conversion. }

{GLOBAL}
procedure jinit_color_deconverter (cinfo : j_decompress_ptr);
var
  cconvert : my_cconvert_ptr;
  ci : int;
begin
  cconvert := my_cconvert_ptr (
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                                SIZEOF(my_color_deconverter)) );
  cinfo^.cconvert := jpeg_color_deconverter_ptr (cconvert);
  cconvert^.pub.start_pass := start_pass_dcolor;

  { Make sure num_components agrees with jpeg_color_space }
  case (cinfo^.jpeg_color_space) of
  JCS_GRAYSCALE:
    if (cinfo^.num_components <> 1) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_J_COLORSPACE);

  JCS_RGB,
  JCS_YCbCr:
    if (cinfo^.num_components <> 3) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_J_COLORSPACE);

  JCS_CMYK,
  JCS_YCCK:
    if (cinfo^.num_components <> 4) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_J_COLORSPACE);

  else                     { JCS_UNKNOWN can be anything }
    if (cinfo^.num_components < 1) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_J_COLORSPACE);
  end;

  { Set out_color_components and conversion method based on requested space.
    Also clear the component_needed flags for any unused components,
    so that earlier pipeline stages can avoid useless computation. }

  case (cinfo^.out_color_space) of
  JCS_GRAYSCALE:
    begin
      cinfo^.out_color_components := 1;
      if (cinfo^.jpeg_color_space = JCS_GRAYSCALE)
        or (cinfo^.jpeg_color_space = JCS_YCbCr) then
      begin
        cconvert^.pub.color_convert := grayscale_convert;
        { For color -> grayscale conversion, only the
          Y (0) component is needed }
        for ci := 1 to pred(cinfo^.num_components) do
          cinfo^.comp_info^[ci].component_needed := FALSE;
      end
      else
        ERREXIT(j_common_ptr(cinfo), JERR_CONVERSION_NOTIMPL);
    end;

  JCS_RGB:
    begin
      cinfo^.out_color_components := RGB_PIXELSIZE;
      if (cinfo^.jpeg_color_space = JCS_YCbCr) then
      begin
        cconvert^.pub.color_convert := ycc_rgb_convert;
        build_ycc_rgb_table(cinfo);
      end
      else
        if (cinfo^.jpeg_color_space = JCS_GRAYSCALE) then
        begin
          cconvert^.pub.color_convert := gray_rgb_convert;
        end
        else
          if (cinfo^.jpeg_color_space = JCS_RGB) and (RGB_PIXELSIZE = 3) then
          begin
            cconvert^.pub.color_convert := null_convert;
          end
          else
            ERREXIT(j_common_ptr(cinfo), JERR_CONVERSION_NOTIMPL);
    end;

  JCS_CMYK:
    begin
      cinfo^.out_color_components := 4;
      if (cinfo^.jpeg_color_space = JCS_YCCK) then
      begin
        cconvert^.pub.color_convert := ycck_cmyk_convert;
        build_ycc_rgb_table(cinfo);
      end
      else
        if (cinfo^.jpeg_color_space = JCS_CMYK) then
        begin
          cconvert^.pub.color_convert := null_convert;
        end
        else
          ERREXIT(j_common_ptr(cinfo), JERR_CONVERSION_NOTIMPL);
    end;

  else
    begin { Permit null conversion to same output space }
      if (cinfo^.out_color_space = cinfo^.jpeg_color_space) then
      begin
        cinfo^.out_color_components := cinfo^.num_components;
        cconvert^.pub.color_convert := null_convert;
      end
      else                      { unsupported non-null conversion }
        ERREXIT(j_common_ptr(cinfo), JERR_CONVERSION_NOTIMPL);
    end;
  end;

  if (cinfo^.quantize_colors) then
    cinfo^.output_components := 1 { single colormapped output component }
  else
    cinfo^.output_components := cinfo^.out_color_components;
end;

end.
