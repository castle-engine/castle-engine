Unit RdColMap;

{ rdcolmap.c ; Copyright (C) 1994-1996, Thomas G. Lane. }

{ This file implements djpeg's "-map file" switch.  It reads a source image
  and constructs a colormap to be supplied to the JPEG decompressor.

  Currently, these file formats are supported for the map file:
    GIF: the contents of the GIF's global colormap are used.
    PPM (either text or raw flavor): the entire file is read and
       each unique pixel value is entered in the map.
  Note that reading a large PPM file will be horrendously slow.
  Typically, a PPM-format map file should contain just one pixel
  of each desired color.  Such a file can be extracted from an
  ordinary image PPM file with ppmtomap(1).

  Rescaling a PPM that has a maxval unequal to MAXJSAMPLE is not
  currently implemented. }

interface

{$I jconfig.inc}

uses
  jmorecfg,
  cdjpeg,               { Common decls for cjpeg/djpeg applications }
  jdeferr,
  jerror,
  jpeglib;

{ Main entry point from djpeg.c.
   Input: opened input file (from file name argument on command line).
   Output: colormap and actual_number_of_colors fields are set in cinfo. }

{GLOBAL}
{$ifdef QUANT_2PASS_SUPPORTED}
procedure read_color_map (cinfo : j_decompress_ptr; var infile : FILE);
{$endif} { QUANT_2PASS_SUPPORTED }

implementation

{$ifdef QUANT_2PASS_SUPPORTED}
{ otherwise can't quantize to supplied map }

{ Portions of this code are based on the PBMPLUS library, which is:
*
* Copyright (C) 1988 by Jef Poskanzer.
*
* Permission to use, copy, modify, and distribute this software and its
* documentation for any purpose and without fee is hereby granted, provided
* that the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation.  This software is provided "as is" without express or
* implied warranty.
}


{ Add a (potentially) new color to the color map. }

{LOCAL}
procedure add_map_entry (cinfo : j_decompress_ptr;
                         R : int; G : int; B : int);
var
  colormap0 : JSAMPROW;
  colormap1 : JSAMPROW;
  colormap2 : JSAMPROW;
  ncolors : int;
  index : int;
begin
  colormap0 := cinfo^.colormap^[0];
  colormap1 := cinfo^.colormap^[1];
  colormap2 := cinfo^.colormap^[2];
  ncolors := cinfo^.actual_number_of_colors;

  { Check for duplicate color. }
  for index := 0 to pred(ncolors) do
  begin
    if (GETJSAMPLE(colormap0^[index]) = R) and
       (GETJSAMPLE(colormap1^[index]) = G) and
       (GETJSAMPLE(colormap2^[index]) = B) then
      exit;                     { color is already in map }
  end;

  { Check for map overflow. }
  if (ncolors >= (MAXJSAMPLE+1)) then
    ERREXIT1(j_common_ptr(cinfo), JERR_QUANT_MANY_COLORS, (MAXJSAMPLE+1));

  { OK, add color to map. }
  colormap0^[ncolors] := JSAMPLE (R);
  colormap1^[ncolors] := JSAMPLE (G);
  colormap2^[ncolors] := JSAMPLE (B);
  Inc(cinfo^.actual_number_of_colors);
end;


{ Extract color map from a GIF file. }

{LOCAL}
procedure read_gif_map (cinfo : j_decompress_ptr; var infile : file);
var
  header : packed array[1..13-1] of byte;
  i, colormaplen : int;
var
  color : RGBtype;
var
  count : int;
begin
  { Initial 'G' has already been read by read_color_map }
  { Read the rest of the GIF header and logical screen descriptor }
  blockread(infile, header, 13-1, count);
  if (count <> 13-1) then
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_CMAP_FILE);

  { Verify GIF Header }
  if (header[1] <> byte('I')) or (header[2] <> byte('F')) then
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_CMAP_FILE);

  { There must be a global color map. }
  if ((header[10] and $80) = 0) then
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_CMAP_FILE);

  { OK, fetch it. }
  colormaplen := 2 shl (header[10] and $07);

  for i := 0 to pred(colormaplen) do
  begin
    blockread(infile, color, 3, count);
    if (count <> 3) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_CMAP_FILE);
    add_map_entry(cinfo,
                  color.R shl (BITS_IN_JSAMPLE-8),
                  color.G shl (BITS_IN_JSAMPLE-8),
                  color.B shl (BITS_IN_JSAMPLE-8));
  end;
end;


{$IFDEF PPM}
{ Support routines for reading PPM }


{LOCAL}
function pbm_getc (var infile : FILE) : int;
{ Read next char, skipping over any comments }
{ A comment/newline sequence is returned as a newline }
var
  {register} ch : int;
begin
  ch := getc(infile);
  if (ch = '#') then
  begin
    repeat
      ch := getc(infile);
    until not (ch <> '\n') and not EOF(infile);
  end;
  pbm_get := ch;
end;


{LOCAL}
function read_pbm_integer (cinfo : j_decompress_ptr;
                           var infile : FILE) : uInt;
{ Read an unsigned decimal integer from the PPM file }
{ Swallows one trailing character after the integer }
{ Note that on a 16-bit-int machine, only values up to 64k can be read. }
{ This should not be a problem in practice. }
var
  {register} ch : int;
  {register} val : uInt;
begin
  { Skip any leading whitespace }
  repeat
    ch := pbm_getc(infile);
    if EOF(infile) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_CMAP_FILE);
  until (ch <> ' ') and (ch <> '\t') and (ch <> '\n') and (ch <> '\r');

  if (ch < '0') or (ch > '9') then
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_CMAP_FILE);

  val := ch - '0';
  ch := pbm_getc(infile);
  while (ch >= '0') and (ch <= '9') do
  begin
    val := val * 10;
    Inc(val, ch - '0');
    ch := pbm_getc(infile);
  end;
  read_pbm_integer := val;
end;


{ Extract color map from a PPM file. }

{LOCAL}
procedure read_ppm_map (cinfo : j_decompress_ptr; var infile : FILE);
var
  c : int;
  w, h, maxval, row, col : uInt;
  R, G, B : int;
begin
  { Initial 'P' has already been read by read_color_map }
  c := getc(infile);            { save format discriminator for a sec }

  { while we fetch the remaining header info }
  w := read_pbm_integer(cinfo, infile);
  h := read_pbm_integer(cinfo, infile);
  maxval := read_pbm_integer(cinfo, infile);

  if (w <= 0) or (h <= 0) or (maxval <= 0) then { error check }
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_CMAP_FILE);

  { For now, we don't support rescaling from an unusual maxval. }
  if (maxval <> (unsigned int) MAXJSAMPLE) then
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_CMAP_FILE);

  case (c) of
  '3':                  { it's a text-format PPM file }
    for row := 0 to pred(h) do
    begin
      for col := 0 to pred(w) do
      begin
        R := read_pbm_integer(cinfo, infile);
        G := read_pbm_integer(cinfo, infile);
        B := read_pbm_integer(cinfo, infile);
        add_map_entry(cinfo, R, G, B);
      end;
    end;

  '6':                  { it's a raw-format PPM file }
    for row := 0 to pred(h) do
    begin
      for col := 0 to pred(w) do
      begin
        R := pbm_getc(infile);
        G := pbm_getc(infile);
        B := pbm_getc(infile);
        if (R = EOF) or (G = EOF) or (B = EOF) then
          ERREXIT(j_common_ptr(cinfo), JERR_BAD_CMAP_FILE);
        add_map_entry(cinfo, R, G, B);
      end;
    end;

  else
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_CMAP_FILE);
  end;
end;
{$ENDIF}

{ Main entry point from djpeg.c.
   Input: opened input file (from file name argument on command line).
   Output: colormap and actual_number_of_colors fields are set in cinfo. }

{GLOBAL}
procedure read_color_map (cinfo : j_decompress_ptr;
                          var infile : FILE);
var
  ch : char;
begin
  { Allocate space for a color map of maximum supported size. }
  cinfo^.colormap := cinfo^.mem^.alloc_sarray
    (j_common_ptr (cinfo), JPOOL_IMAGE,
     JDIMENSION (MAXJSAMPLE+1), JDIMENSION (3));
  cinfo^.actual_number_of_colors := 0; { initialize map to empty }

  { Read first byte to determine file format }
  BlockRead(infile, ch, 1);
  case ch of
  'G': read_gif_map(cinfo, infile);
  {$IFDEF PPM}
  'P': read_ppm_map(cinfo, infile);
  {$ENDIF}
  else
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_CMAP_FILE);
  end;
end;

{$endif} { QUANT_2PASS_SUPPORTED }
end.
