Program DJpeg;


{ djpeg.c ; Copyright (C) 1991-1997, Thomas G. Lane.  }

{ This file contains a command-line user interface for the JPEG decompressor.
  It should work on any system with Unix- or MS-DOS-style command lines.

  Two different command line styles are permitted, depending on the
  compile-time switch TWO_FILE_COMMANDLINE:
        djpeg [options]  inputfile outputfile
        djpeg [options]  [inputfile]
  In the second style, output is always to standard output, which you'd
  normally redirect to a file or pipe to some other program.  Input is
  either from a named file or from standard input (typically redirected).
  The second style is convenient on Unix but is unhelpful on systems that
  don't support pipes.  Also, you MUST use the first style if your system
  doesn't do binary I/O to stdin/stdout.
  To simplify script writing, the "-outfile" switch is provided.  The syntax
        djpeg [options]  -outfile outputfile  inputfile
  works regardless of which command line style is used. }

{$I jconfig.inc}


uses
  jmorecfg,
  jpeglib,
  jerror,
  RdColMap,
  jdeferr,
  jdapimin, jdapistd, jdatasrc,
{$ifdef BMP_SUPPORTED}  wrbmp, {$endif}
{$ifdef PPM_SUPPORTED}  wrppm, {$endif}
{$ifdef TARGA_SUPPORTED}  wrtarga, {$endif}
  jdmarker,
  cdjpeg;               { Common decls for cjpeg/djpeg applications }
                        { for version message }



{ This list defines the known output image formats
  (not all of which need be supported by a given version).
  You can change the default output format by defining DEFAULT_FMT;
  indeed, you had better do so if you undefine PPM_SUPPORTED. }

type
  IMAGE_FORMATS = (
        FMT_BMP,                { BMP format (Windows flavor) }
        FMT_GIF,                { GIF format }
        FMT_OS2,                { BMP format (OS/2 flavor) }
        FMT_PPM,                { PPM/PGM (PBMPLUS formats) }
        FMT_RLE,                { RLE format }
        FMT_TARGA,              { Targa format }
        FMT_TIFF);              { TIFF format }

const
  DEFAULT_FMT = FMT_PPM;

var
  requested_fmt : IMAGE_FORMATS;


{ Argument-parsing code.
  The switch parser is designed to be useful with DOS-style command line
  syntax, ie, intermixed switches and file names, where only the switches
  to the left of a given file name affect processing of that file.
  The main program in this file doesn't actually use this capability... }


var
  progname,                     { program name for error messages }
  outfilename : string[127];    { for -outfile switch }


{LOCAL}
procedure usage;
const
  default_txt : array[boolean] of string[30] = ('', ' (default)');
{ complain about bad command line }
begin
  Write  (output, 'usage: ', progname, ' [switches] ');
{$ifdef TWO_FILE_COMMANDLINE}
  WriteLn(output, 'inputfile outputfile');
{$else}
  WriteLn(output, '[inputfile]');
{$endif}

  WriteLn(output, 'Switches (names may be abbreviated):');
  WriteLn(output, '  -colors N      Reduce image to no more than N colors');
  WriteLn(output, '  -fast          Fast, low-quality processing');
  WriteLn(output, '  -grayscale     Force grayscale output');
{$ifdef IDCT_SCALING_SUPPORTED}
  WriteLn(output, '  -scale M/N     Scale output image by fraction M/N, eg, 1/8');
{$endif}
{$ifdef BMP_SUPPORTED}
  WriteLn(output, '  -bmp           Select BMP output format (Windows style)',
          default_txt[DEFAULT_FMT = FMT_BMP]);
{$endif}
{$ifdef GIF_SUPPORTED}
  WriteLn(output, '  -gif           Select GIF output format',
          default_txt[DEFAULT_FMT = FMT_GIF]);
{$endif}
{$ifdef BMP_SUPPORTED}
  WriteLn(output, '  -os2           Select BMP output format (OS/2 style)',
          default_txt[DEFAULT_FMT = FMT_OS2]);
{$endif}
{$ifdef PPM_SUPPORTED}
  WriteLn(output, '  -pnm           Select PBMPLUS (PPM/PGM) output format',
          default_txt[DEFAULT_FMT = FMT_PPM]);
{$endif}
{$ifdef RLE_SUPPORTED}
  WriteLn(output, '  -rle           Select Utah RLE output format',
          default_txt[DEFAULT_FMT = FMT_RLE]);
{$endif}
{$ifdef TARGA_SUPPORTED}
  WriteLn(output, '  -targa         Select Targa output format',
          default_txt[DEFAULT_FMT = FMT_TARGA]);
{$endif}
  WriteLn(output, 'Switches for advanced users:');
{$ifdef DCT_ISLOW_SUPPORTED}
  WriteLn(output, '  -dct int       Use integer DCT method',
          default_txt[JDCT_DEFAULT = JDCT_ISLOW]);
{$endif}
{$ifdef DCT_IFAST_SUPPORTED}
  WriteLn(output, '  -dct fast      Use fast integer DCT (less accurate)',
          default_txt[JDCT_DEFAULT = JDCT_IFAST]);
{$endif}
{$ifdef DCT_FLOAT_SUPPORTED}
  WriteLn(output, '  -dct float     Use floating-point DCT method',
          default_txt[JDCT_DEFAULT = JDCT_FLOAT]);
{$endif}
  WriteLn(output, '  -dither fs     Use F-S dithering (default)');
  WriteLn(output, '  -dither none   Don''t use dithering in quantization');
  WriteLn(output, '  -dither ordered  Use ordered dither (medium speed, quality)');
{$ifdef QUANT_2PASS_SUPPORTED}
  WriteLn(output, '  -map FILE      Map to colors used in named image file');
{$endif}
  WriteLn(output, '  -nosmooth      Don''t use high-quality upsampling');
{$ifdef QUANT_1PASS_SUPPORTED}
  WriteLn(output, '  -onepass       Use 1-pass quantization (fast, low quality)');
{$endif}
  WriteLn(output, '  -maxmemory N   Maximum memory to use (in kbytes)');
  WriteLn(output, '  -outfile name  Specify name for output file');
  WriteLn(output, '  -verbose  or  -debug   Emit debug output');
  Halt(EXIT_FAILURE);
end;


{LOCAL}
function parse_switches (cinfo : j_decompress_ptr;
                         last_file_arg_seen : int;
                         for_real : boolean) : int;
{ Parse optional switches.
  Returns argv[] index of first file-name argument (== argc if none).
  Any file names with indexes <= last_file_arg_seen are ignored;
  they have presumably been processed in a previous iteration.
  (Pass 0 for last_file_arg_seen on the first or only iteration.)
  for_real is FALSE on the first (dummy) pass; we may skip any expensive
  processing. }
var
  argn,
  argc : int;
  arg : string;
var
  value : int;
  code : integer;
{$ifdef QUANT_2PASS_SUPPORTED}  { otherwise can't quantize to supplied map }
var
  mapfile : file;
{$endif}
const
  printed_version : boolean = FALSE;
var
  lval : long;
  ch : char;
begin
  { Set up default JPEG parameters. }
  requested_fmt := DEFAULT_FMT; { set default output file format }
  outfilename := '';
  cinfo^.err^.trace_level := 0;

  { Scan command line options, adjust parameters }

  argn := 0;
  argc := ParamCount;

  while argn < argc do
  begin
    Inc(argn);
    arg := ParamStr(argn);
    if (arg[1] <> '-') then
    begin
      { Not a switch, must be a file name argument }
      if (argn <= last_file_arg_seen) then
      begin
        outfilename := '';      { -outfile applies to just one input file }
        continue;               { ignore this name if previously processed }
      end;
      break;                    { else done parsing switches }
    end;
    {Inc(arg);                  - advance past switch marker character }

    if (keymatch(arg, '-bmp', 2)) then
    begin
      { BMP output format. }
      requested_fmt := FMT_BMP;

    end
    else
      if (keymatch(arg, '-colors', 2)) or (keymatch(arg, '-colours', 2)) or
         (keymatch(arg, '-quantize', 2)) or (keymatch(arg, '-quantise', 2)) then
      begin  { Do color quantization. }

        Inc(argn);
        if (argn >= argc) then  { advance to next argument }
          usage;
        Val(ParamStr(argn), value, code);
        if code <> 0 then
          usage;
        cinfo^.desired_number_of_colors := value;
        cinfo^.quantize_colors := TRUE;
      end
      else
        if (keymatch(arg, '-dct', 3)) then
        begin  { Select IDCT algorithm. }
          Inc(argn);
          if (argn >= argc) then        { advance to next argument }
            usage;
          if (keymatch(ParamStr(argn), 'int', 1)) then
            cinfo^.dct_method := JDCT_ISLOW
          else
            if (keymatch(ParamStr(argn), 'fast', 2)) then
              cinfo^.dct_method := JDCT_IFAST
            else
              if (keymatch(ParamStr(argn), 'float', 2)) then
                cinfo^.dct_method := JDCT_FLOAT
              else
                usage;
        end
        else
          if (keymatch(arg, '-dither', 3)) then
          begin { Select dithering algorithm. }
            Inc(argn);
            if (argn >= argc) then      { advance to next argument }
              usage;
            if (keymatch(ParamStr(argn), 'fs', 2)) then
              cinfo^.dither_mode := JDITHER_FS
            else
              if (keymatch(ParamStr(argn), 'none', 2)) then
                cinfo^.dither_mode := JDITHER_NONE
              else
                if (keymatch(ParamStr(argn), 'ordered', 2)) then
                  cinfo^.dither_mode := JDITHER_ORDERED
                else
                  usage;
          end
          else
            if (keymatch(arg, '-debug', 2)) or (keymatch(arg, '-verbose', 2)) then
            begin  { Enable debug printouts. }
              { On first -d, print version identification }

              if (not printed_version) then
              begin
                WriteLn(output, 'PASJPEG Group''s DJPEG, version ',
                      JVERSION);
                WriteLn(output, JCOPYRIGHT);
                WriteLn(output, JNOTICE);
                printed_version := TRUE;
              end;
              Inc(cinfo^.err^.trace_level);
            end
            else
              if (keymatch(arg, '-fast', 2)) then
              begin
                { Select recommended processing options for quick-and-dirty output. }
                cinfo^.two_pass_quantize := FALSE;
                cinfo^.dither_mode := JDITHER_ORDERED;
                if (not cinfo^.quantize_colors) then { don't override an earlier -colors }
                  cinfo^.desired_number_of_colors := 216;
                cinfo^.dct_method := JDCT_FASTEST;
                cinfo^.do_fancy_upsampling := FALSE;
              end
              else
                if (keymatch(arg, '-gif', 2)) then
                begin  { GIF output format. }
                  requested_fmt := FMT_GIF;
                end
                else
                  if (keymatch(arg, '-grayscale', 3)) or
                     (keymatch(arg, '-greyscale',3)) then
                  { Force monochrome output. }
                    cinfo^.out_color_space := JCS_GRAYSCALE
                  else
                    if (keymatch(arg, '-map', 4)) then
                    begin
                      { Quantize to a color map taken from an input file. }
                      Inc(argn);
                      if (argn >= argc) then    { advance to next argument }
                        usage;
                      if (for_real) then
                      begin     { too expensive to do twice! }
{$ifdef QUANT_2PASS_SUPPORTED}  { otherwise can't quantize to supplied map }
                        assign(mapfile, ParamStr(argn));
{$push}{$I-}
                        reset(mapfile, 1);
{$pop}
                        if (IOresult <> 0) then
                        begin
                          WriteLn(output, progname, ': can''t open ', ParamStr(argn));
                          Halt(EXIT_FAILURE);
                        end;
                        read_color_map(cinfo, mapfile);
                        system.close(mapfile);
                        cinfo^.quantize_colors := TRUE;
{$else}
                        ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
{$endif}
                      end;
                    end
                    else
                      if (keymatch(arg, '-maxmemory', 4)) then
                      begin
                        { Maximum memory in Kb (or Mb with 'm'). }

                        ch := 'x';

                        Inc(argn);
                        if (argn >= argc) then  { advance to next argument }
                          usage;
                        arg := ParamStr(argn);
                        if (length(arg) > 1) and (arg[length(arg)] in ['m','M']) then
                        begin
                          ch := arg[length(arg)];
                          arg := Copy(arg, 1, Length(arg)-1);
                        end;

                        Val(arg, lval, code);

                        if (code <> 0) then
                          usage;
                        if (ch = 'm') or (ch = 'M') then
                          lval := lval * long(1000);
                        cinfo^.mem^.max_memory_to_use := lval * long(1000);

                      end
                      else
                        if (keymatch(arg, '-nosmooth', 4)) then
                        begin
                          { Suppress fancy upsampling }
                          cinfo^.do_fancy_upsampling := FALSE;

                        end
                        else
                          if (keymatch(arg, '-onepass', 4)) then
                          begin
                            { Use fast one-pass quantization. }
                            cinfo^.two_pass_quantize := FALSE;
                          end
                          else
                            if (keymatch(arg, '-os2', 4)) then
                            begin
                              { BMP output format (OS/2 flavor). }
                              requested_fmt := FMT_OS2;
                            end
                            else
                              if (keymatch(arg, '-outfile', 5)) then
                              begin
                                { Set output file name. }
                                Inc(argn);
                                if (argn >= argc) then  { advance to next argument }
                                  usage;
                                outfilename := ParamStr(argn);  { save it away for later use }
                              end
                              else
                                if (keymatch(arg, '-pnm', 2)) or
                                   (keymatch(arg, '-ppm', 2)) then
                                begin
                                  { PPM/PGM output format. }
                                  requested_fmt := FMT_PPM;
                                end
                                else
                                  if (keymatch(arg, '-rle', 2)) then
                                  begin
                                    { RLE output format. }
                                    requested_fmt := FMT_RLE;
                                  end
                                  else
                                    if (keymatch(arg, '-scale', 2)) then
                                    begin
                                      { Scale the output image by a fraction M/N. }
                                      Inc(argn);
                                      if (argn >= argc) then    { advance to next argument }
                                        usage;
                                      arg := ParamStr(argn);
                                      Val(copy(arg, 1, Pos('/', arg)-1),
                                          cinfo^.scale_num, code);
                                      if code = 0 then
                                        Val(copy(arg, Pos('/', arg)+1,
                                             length(arg)-Pos('/', arg)),
                                            cinfo^.scale_denom, code);
                                      if code <> 0 then
                                        usage;
                                    end
                                    else
                                      if (keymatch(arg, '-targa', 2)) then
                                      begin
                                        { Targa output format. }
                                        requested_fmt := FMT_TARGA;
                                      end
                                      else
                                        usage;          { bogus switch }
  end;

  parse_switches := argn;       { return index of next arg (file name) }
end;


{ Marker processor for COM and interesting APPn markers.
  This replaces the library's built-in processor, which just skips the marker.
  We want to print out the marker as text, to the extent possible.
  Note this code relies on a non-suspending data source. }

{LOCAL}
function jpeg_getc (cinfo : j_decompress_ptr) : char;
{ Read next byte }
var
  datasrc : jpeg_source_mgr_ptr;
begin
  datasrc := cinfo^.src;

  if (datasrc^.bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer (cinfo)) then
      ERREXIT(j_common_ptr(cinfo), JERR_CANT_SUSPEND);
  end;
  Dec(datasrc^.bytes_in_buffer);
  jpeg_getc := char(GETJOCTET(datasrc^.next_input_byte^));
  Inc(datasrc^.next_input_byte);
end;


{METHODDEF}
function print_text_marker (cinfo : j_decompress_ptr) : boolean; far;
const
  LF = #10;
  CR = #13;
var
  traceit : boolean;
  length : INT32;
  ch : char;
  lastch : char;
begin
  traceit := (cinfo^.err^.trace_level >= 1);
  lastch := #0;
  length := byte(jpeg_getc(cinfo)) shl 8;
  Inc(length, byte(jpeg_getc(cinfo)));
  Dec(length, 2);                       { discount the length word itself }

  if (traceit) then
  begin
    if (cinfo^.unread_marker = JPEG_COM) then
      WriteLn('Comment, length ', long(length), ';')
    else                        { assume it is an APPn otherwise }
      WriteLn('APP', cinfo^.unread_marker - JPEG_APP0, ' length ',
              long(length),':');
  end;

  while (length > 0) do
  begin
    Dec(length);
    ch := jpeg_getc(cinfo);
    if (traceit) then
    begin
      { Emit the character in a readable form.
        Nonprintables are converted to \nnn form,
        while \ is converted to \\.
        Newlines in CR, CR/LF, or LF form will be printed as one newline. }
      if (ch = LF) then
        WriteLn(output)
      else
        if (ch = CR) then
        begin
          if (lastch <> LF) then
            WriteLn(output);
        end
        else
          if (ch >= ' ') and (ch <= #127) then
            Write(output, ch)
          else
            WriteLn(output, '\', byte(ch));
      lastch := ch;
    end;
  end;

  if (traceit) then
    WriteLn(output);

  print_text_marker := TRUE;
end;


{ The main program. }

var
  cinfo : jpeg_decompress_struct;
  jerr : jpeg_error_mgr;
{$ifdef PROGRESS_REPORT}
  progress : cdjpeg_progress_mgr;
{$endif}
  file_index : int;
  dest_mgr : djpeg_dest_ptr;
  input_file : FILE;
  output_file : FILE;
  num_scanlines : JDIMENSION;
var
  argc : int;
begin
  dest_mgr := NIL;
  argc := ParamCount;

  progname := ParamStr(0);

  { Initialize the JPEG decompression object with default error handling. }
  cinfo.err := jpeg_std_error(jerr);
  jpeg_create_decompress(@cinfo);
  { Add some application-specific error messages (from cderror.h) }
  {jerr.addon_message_table := cdjpeg_message_table;}
  jerr.first_addon_message := JMSG_FIRSTADDONCODE;
  jerr.last_addon_message := JMSG_LASTADDONCODE;

  { Insert custom marker processor for COM and APP12.
    APP12 is used by some digital camera makers for textual info,
    so we provide the ability to display it as text.
    If you like, additional APPn marker types can be selected for display,
    but don't try to override APP0 or APP14 this way (see libjpeg.doc). }

  jpeg_set_marker_processor(@cinfo, JPEG_COM, print_text_marker);
  jpeg_set_marker_processor(@cinfo, JPEG_APP0+12, print_text_marker);

  { Now safe to enable signal catcher. }
{$ifdef NEED_SIGNAL_CATCHER}
  enable_signal_catcher(j_common_ptr (@cinfo));
{$endif}

  { Scan command line to find file names. }
  { It is convenient to use just one switch-parsing routine, but the switch
    values read here are ignored; we will rescan the switches after opening
    the input file.
    (Exception: tracing level set here controls verbosity for COM markers
    found during jpeg_read_header...) }

  file_index := parse_switches(@cinfo, 0, FALSE);

{$ifdef TWO_FILE_COMMANDLINE}
  { Must have either -outfile switch or explicit output file name }
  if (outfilename = '') then
  begin
    if (file_index <> argc-1) then
    begin
      WriteLn(output, progname, ': must name one input and one output file');
      usage;
    end;
    outfilename := ParamStr(file_index+1);
  end
  else
  begin
    if (file_index <> argc) then
    begin
      WriteLn(output, progname, ': must name one input and one output file');
      usage;
    end;
  end;
{$else}
  { Unix style: expect zero or one file name }
  if (file_index < argc-1) then
  begin
    WriteLn(output, progname, ': only one input file');
    usage;
  end;
{$endif} { TWO_FILE_COMMANDLINE }

  { Open the input file. }
  if (file_index < argc) then
  begin
    assign(input_file, ParamStr(file_index));
    {$push}{$I-}
    Reset(input_file, 1);
    {$pop}
    if (IOresult <> 0) then
    begin
      WriteLn(output, progname, ': can''t open ', ParamStr(file_index));
      Halt(EXIT_FAILURE);
    end;
  end
  else
  begin
    { default input file is stdin }
    Assign(input_file, '');
    Reset(input_file, 1);
  end;

  { Open the output file. }
  if (outfilename <> '') then
  begin
    assign(output_file, outfilename);
    {$push}{$I-}
    rewrite(output_file, 1);
    {$pop}
    if (IOresult <> 0) then
    begin
      WriteLn(output, progname, ': can''t open ', outfilename);
      Halt(EXIT_FAILURE);
    end;
  end
  else
  begin
    { default output file is stdout }
    assign(output_file, '');
    rewrite(output_file, 1);
  end;

{$ifdef PROGRESS_REPORT}
  start_progress_monitor(j_common_ptr (@cinfo), @progress);
{$endif}

  { Specify data source for decompression }
  jpeg_stdio_src(@cinfo, @input_file);

  { Read file header, set default decompression parameters }
  {void} jpeg_read_header(@cinfo, TRUE);

  { Adjust default decompression parameters by re-parsing the options }
  file_index := parse_switches(@cinfo, 0, TRUE);

  { Initialize the output module now to let it override any crucial
    option settings (for instance, GIF wants to force color quantization). }

  case (requested_fmt) of
{$ifdef BMP_SUPPORTED}
  FMT_BMP:
    dest_mgr := jinit_write_bmp(@cinfo, FALSE);
  FMT_OS2:
    dest_mgr := jinit_write_bmp(@cinfo, TRUE);
{$endif}
{$ifdef GIF_SUPPORTED}
  FMT_GIF:
    dest_mgr := jinit_write_gif(@cinfo);
{$endif}
{$ifdef PPM_SUPPORTED}
  FMT_PPM:
    dest_mgr := jinit_write_ppm(@cinfo);
{$endif}
{$ifdef RLE_SUPPORTED}
  FMT_RLE:
    dest_mgr := jinit_write_rle(@cinfo);
{$endif}
{$ifdef TARGA_SUPPORTED}
  FMT_TARGA:
    dest_mgr := jinit_write_targa(@cinfo);
{$endif}
  else
    ERREXIT(j_common_ptr(@cinfo), JERR_UNSUPPORTED_FORMAT);
  end;
  dest_mgr^.output_file := @output_file;

  { Start decompressor }
  {void} jpeg_start_decompress(@cinfo);

  { Write output file header }
  dest_mgr^.start_output (@cinfo, dest_mgr);

  { Process data }
  while (cinfo.output_scanline < cinfo.output_height) do
  begin
    num_scanlines := jpeg_read_scanlines(@cinfo, dest_mgr^.buffer,
                                        dest_mgr^.buffer_height);
    dest_mgr^.put_pixel_rows (@cinfo, dest_mgr, num_scanlines);
  end;

{$ifdef PROGRESS_REPORT}
  { Hack: count final pass as done in case finish_output does an extra pass.
    The library won't have updated completed_passes. }

  progress.pub.completed_passes := progress.pub.total_passes;
{$endif}

  { Finish decompression and release memory.
    I must do it in this order because output module has allocated memory
    of lifespan JPOOL_IMAGE; it needs to finish before releasing memory. }

  dest_mgr^.finish_output (@cinfo, dest_mgr);
  {void} jpeg_finish_decompress(@cinfo);
  jpeg_destroy_decompress(@cinfo);

  { Close files, if we opened them }
  system.close(input_file);
  system.close(output_file);

{$ifdef PROGRESS_REPORT}
  end_progress_monitor(j_common_ptr (@cinfo));
{$endif}

  { All done. }
  if jerr.num_warnings <> 0 then
    Halt(EXIT_WARNING)
  else
    Halt(EXIT_SUCCESS);
end.
