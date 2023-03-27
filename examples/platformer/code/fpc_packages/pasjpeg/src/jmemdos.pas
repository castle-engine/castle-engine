Unit JmemDos;


{ This file provides an MS-DOS-compatible implementation of the system-
  dependent portion of the JPEG memory manager.  Temporary data can be
  stored in extended or expanded memory as well as in regular DOS files.

  If you use this file, you must be sure that NEED_FAR_POINTERS is defined
  if you compile in a small-data memory model; it should NOT be defined if
  you use a large-data memory model.  This file is not recommended if you
  are using a flat-memory-space 386 environment such as DJGCC or Watcom C.
  Also, this code will NOT work if struct fields are aligned on greater than
  2-byte boundaries.

  Based on code contributed by Ge' Weijers. }

{ Original: jmemdos.c;  Copyright (C) 1992-1996, Thomas G. Lane. }


interface

{$I jconfig.inc}

uses
  jmorecfg,
  jpeglib;

{ If you have both extended and expanded memory, you may want to change the
  order in which they are tried in jopen_backing_store.  On a 286 machine
  expanded memory is usually faster, since extended memory access involves
  an expensive protected-mode-and-back switch.  On 386 and better, extended
  memory is usually faster.  As distributed, the code tries extended memory
  first (what? not everyone has a 386? :-).

  You can disable use of extended/expanded memory entirely by altering these
  definitions or overriding them from the Makefile (eg, -DEMS_SUPPORTED=0).}

{GLOBAL}
procedure jpeg_open_backing_store (cinfo : j_common_ptr;
                                   info : backing_store_ptr;
                                   total_bytes_needed : long);

{ These routines take care of any system-dependent initialization and
  cleanup required. }

{GLOBAL}
function jpeg_mem_init (cinfo : j_common_ptr) : long;

{GLOBAL}
procedure jpeg_mem_term (cinfo : j_common_ptr);

{ These two functions are used to allocate and release small chunks of
  memory.  (Typically the total amount requested through jpeg_get_small is
  no more than 20K or so; this will be requested in chunks of a few K each.)
  Behavior should be the same as for the standard library functions malloc
  and free; in particular, jpeg_get_small must return NIL on failure.
  On most systems, these ARE malloc and free.  jpeg_free_small is passed the
  size of the object being freed, just in case it's needed.
  On an 80x86 machine using small-data memory model, these manage near heap. }


{ Near-memory allocation and freeing are controlled by the regular library
  routines malloc() and free(). }

{GLOBAL}
function jpeg_get_small (cinfo : j_common_ptr;
                         sizeofobject : size_t) : pointer;

{GLOBAL}
{object is a reserved word in Borland Pascal }
procedure jpeg_free_small (cinfo : j_common_ptr;
                           an_object : pointer;
                           sizeofobject : size_t);

{ These two functions are used to allocate and release large chunks of
  memory (up to the total free space designated by jpeg_mem_available).
  The interface is the same as above, except that on an 80x86 machine,
  far pointers are used.  On most other machines these are identical to
  the jpeg_get/free_small routines; but we keep them separate anyway,
  in case a different allocation strategy is desirable for large chunks. }


{ "Large" objects are allocated in far memory, if possible }


{GLOBAL}
function jpeg_get_large (cinfo : j_common_ptr;
                         sizeofobject : size_t) : voidp; {far}

{GLOBAL}
procedure jpeg_free_large (cinfo : j_common_ptr;
                          {var?} an_object : voidp; {FAR}
                          sizeofobject : size_t);

{ This routine computes the total memory space available for allocation.
  It's impossible to do this in a portable way; our current solution is
  to make the user tell us (with a default value set at compile time).
  If you can actually get the available space, it's a good idea to subtract
  a slop factor of 5% or so. }

{GLOBAL}
function jpeg_mem_available (cinfo : j_common_ptr;
                             min_bytes_needed : long;
                             max_bytes_needed : long;
                             already_allocated : long) : long;


{ The macro MAX_ALLOC_CHUNK designates the maximum number of bytes that may
  be requested in a single call to jpeg_get_large (and jpeg_get_small for that
  matter, but that case should never come into play).  This macro is needed
  to model the 64Kb-segment-size limit of far addressing on 80x86 machines.
  On those machines, we expect that jconfig.h will provide a proper value.
  On machines with 32-bit flat address spaces, any large constant may be used.

  NB: jmemmgr.c expects that MAX_ALLOC_CHUNK will be representable as type
  size_t and will be a multiple of sizeof(align_type). }


{$ifdef USE_MSDOS_MEMMGR}            { Define this if you use jmemdos.c }
const
  MAX_ALLOC_CHUNK = long(32752); {65520}  { Maximum request to malloc() }
                             { MAX_ALLOC_CHUNK should be less than 64K. }
{$else}
const
  MAX_ALLOC_CHUNK = long(1000000000);
{$endif}

implementation

uses
  dos,
  jmemdosa,
  jdeferr,
  jerror;



{ Selection of a file name for a temporary file.
  This is highly system-dependent, and you may want to customize it. }

var
  next_file_num : int;          { to distinguish among several temp files }

{LOCAL}
procedure select_file_name (var fname : TEMP_STRING);
var
  env : string;
  suffix,
  prefix : TEMP_STRING;
  tfile : FILE;
  l : byte;
begin
  { Keep generating file names till we find one that's not in use }
  while TRUE do
  begin
     { Get temp directory name from environment TMP or TEMP variable;
      if none, use "." }
    env := getenv('TMP');
    if (env = '') then
    begin
      env := getenv('TEMP');
      if (env = '') then         { null string means "." }
        env := '.';
    end;
    prefix := env;                { copy name to fname }
    { length(fname) > 0 !! }
    if (prefix[length(prefix)] <> '\')
    and (prefix[length(prefix)] <> '/') then
      prefix := prefix + '\';     { append backslash if not in env variable }
    { Append a suitable file name }
    Inc(next_file_num);         { advance counter }

    Str(next_file_num, suffix);
    for l := Length(suffix)+1 to 3 do
      suffix := '0' + suffix;
    fname := prefix + 'JPG' + suffix + '.TMP';
    { Probe to see if file name is already in use }
    system.assign(tfile, fname);
{$push} {$I-}
    system.reset(tfile, 1);
{$pop}
    if (IOresult <> 0) then
    begin
      fname := fname + #0;
      break;
    end;
    system.close(tfile);        { oops, it's there; close tfile & try again }
  end;
end;

{ These two functions are used to allocate and release small chunks of
  memory.  (Typically the total amount requested through jpeg_get_small is
  no more than 20K or so; this will be requested in chunks of a few K each.)
  Behavior should be the same as for the standard library functions malloc
  and free; in particular, jpeg_get_small must return NIL on failure.
  On most systems, these ARE malloc and free.  jpeg_free_small is passed the
  size of the object being freed, just in case it's needed.
  On an 80x86 machine using small-data memory model, these manage near heap. }


{ Near-memory allocation and freeing are controlled by the regular library
  routines malloc() and free(). }

{GLOBAL}
function jpeg_get_small (cinfo : j_common_ptr;
                         sizeofobject : size_t) : pointer;
var
  p : pointer;
begin
  getmem(p, sizeofobject);
  jpeg_get_small := p;
end;

{GLOBAL}
{object is a reserved word in Borland Pascal }
procedure jpeg_free_small (cinfo : j_common_ptr;
                           an_object : pointer;
                           sizeofobject : size_t);
begin
  freemem(an_object, sizeofobject);
end;

{ These two functions are used to allocate and release large chunks of
  memory (up to the total free space designated by jpeg_mem_available).
  The interface is the same as above, except that on an 80x86 machine,
  far pointers are used.  On most other machines these are identical to
  the jpeg_get/free_small routines; but we keep them separate anyway,
  in case a different allocation strategy is desirable for large chunks. }


{GLOBAL}
function jpeg_get_large (cinfo : j_common_ptr;
                         sizeofobject : size_t) : voidp; {far}
var
  p : voidp; {FAR}
begin
  {far_malloc;}
  getmem(p, sizeofobject);
  jpeg_get_large := p;
end;

{GLOBAL}
procedure jpeg_free_large (cinfo : j_common_ptr;
                          {var?} an_object : voidp; {FAR}
                          sizeofobject : size_t);
begin
  {far_free(an_object);}
  FreeMem(an_object, sizeofobject);
end;

{ This routine computes the total space still available for allocation by
  jpeg_get_large.  If more space than this is needed, backing store will be
  used.  NOTE: any memory already allocated must not be counted.

  There is a minimum space requirement, corresponding to the minimum
  feasible buffer sizes; jmemmgr.c will request that much space even if
  jpeg_mem_available returns zero.  The maximum space needed, enough to hold
  all working storage in memory, is also passed in case it is useful.
  Finally, the total space already allocated is passed.  If no better
  method is available, cinfo->mem->max_memory_to_use - already_allocated
  is often a suitable calculation.

  It is OK for jpeg_mem_available to underestimate the space available
  (that'll just lead to more backing-store access than is really necessary).
  However, an overestimate will lead to failure.  Hence it's wise to subtract
  a slop factor from the true available space.  5% should be enough.

  On machines with lots of virtual memory, any large constant may be returned.
  Conversely, zero may be returned to always use the minimum amount of memory.}



{ This routine computes the total memory space available for allocation.
  It's impossible to do this in a portable way; our current solution is
  to make the user tell us (with a default value set at compile time).
  If you can actually get the available space, it's a good idea to subtract
  a slop factor of 5% or so. }

const
  DEFAULT_MAX_MEM = long(300000);   { for total usage about 450K }

{GLOBAL}
function jpeg_mem_available (cinfo : j_common_ptr;
                             min_bytes_needed : long;
                             max_bytes_needed : long;
                             already_allocated : long) : long;
begin
  {jpeg_mem_available := cinfo^.mem^.max_memory_to_use - already_allocated;}
  jpeg_mem_available := MaxAvail*95 div 100;  { 95% }

  { Nomssi: limit the available memory for test purpose }
  {jpeg_mem_available := 30000;}
end;


{ Backing store (temporary file) management.
  Backing store objects are only used when the value returned by
  jpeg_mem_available is less than the total space needed.  You can dispense
  with these routines if you have plenty of virtual memory; see jmemnobs.c. }


{ For MS-DOS we support three types of backing storage:
    1. Conventional DOS files.  We access these by direct DOS calls rather
       than via the stdio package.  This provides a bit better performance,
       but the real reason is that the buffers to be read or written are FAR.
       The stdio library for small-data memory models can't cope with that.
    2. Extended memory, accessed per the XMS V2.0 specification.
    3. Expanded memory, accessed per the LIM/EMS 4.0 specification.
  You'll need copies of those specs to make sense of the related code.
  The specs are available by Internet FTP from the SIMTEL archives
  (oak.oakland.edu and its various mirror sites).  See files
  pub/msdos/microsoft/xms20.arc and pub/msdos/info/limems41.zip. }



{ Access methods for a DOS file. }


{METHODDEF}
procedure read_file_store (cinfo : j_common_ptr;
                           info : backing_store_ptr;
                           buffer_address : pointer; {FAR}
                           file_offset : long;
                           byte_count : long); far;
begin
  if jdos_seek(info^.handle.file_handle, file_offset) <> 0 then
    ERREXIT(cinfo, JERR_TFILE_SEEK);
  { Since MAX_ALLOC_CHUNK is less than 64K, byte_count will be too. }
  if (byte_count > long(65535)) then { safety check }
    ERREXIT(cinfo, JERR_BAD_ALLOC_CHUNK);
  if jdos_read(info^.handle.file_handle, buffer_address,
                ushort(byte_count)) <> 0 then
    ERREXIT(cinfo, JERR_TFILE_READ);
end;


{METHODDEF}
procedure write_file_store (cinfo : j_common_ptr;
                            info : backing_store_ptr;
                            buffer_address : pointer; {FAR}
                            file_offset : long;
                            byte_count : long); far;
begin
  if (jdos_seek(info^.handle.file_handle, file_offset)) <> 0 then
    ERREXIT(cinfo, JERR_TFILE_SEEK);
  { Since MAX_ALLOC_CHUNK is less than 64K, byte_count will be too. }
  if (byte_count > long(65535)) then  { safety check }
    ERREXIT(cinfo, JERR_BAD_ALLOC_CHUNK);
  if jdos_write(info^.handle.file_handle, buffer_address,
                 ushort(byte_count)) <> 0 then
    ERREXIT(cinfo, JERR_TFILE_WRITE);
end;


{METHODDEF}
procedure close_file_store (cinfo : j_common_ptr;
                            info : backing_store_ptr); far;
var
  f : FILE;
begin
  jdos_close(info^.handle.file_handle); { close the file }

  system.assign(f, info^.temp_name);
  system.erase(f);                      { delete the file }
{ If your system doesn't have remove(), try unlink() instead.
  remove() is the ANSI-standard name for this function, but
  unlink() was more common in pre-ANSI systems. }

  TRACEMSS(cinfo, 1, JTRC_TFILE_CLOSE, info^.temp_name);
end;


{LOCAL}
function open_file_store (cinfo : j_common_ptr;
                          info : backing_store_ptr;
                          total_bytes_needed : long): boolean;  far;
var
  handle : short;
begin
  select_file_name(info^.temp_name);
  if jdos_open(handle, info^.temp_name[1]) <> 0 then
  begin
    { might as well exit since jpeg_open_backing_store will fail anyway }
    ERREXITS(cinfo, JERR_TFILE_CREATE, info^.temp_name);
    open_file_store := FALSE;
    exit;
  end;
  info^.handle.file_handle := handle;
  info^.read_backing_store := read_file_store;
  info^.write_backing_store := write_file_store;
  info^.close_backing_store := close_file_store;
  TRACEMSS(cinfo, 1, JTRC_TFILE_OPEN, info^.temp_name);
  open_file_store := TRUE;                             { succeeded }
end;


{ Access methods for extended memory. }

{$ifdef XMS_SUPPORTED}

var
  xms_driver : XMSDRIVER;       { saved address of XMS driver }

type
  XMSPTR = record               { either long offset or real-mode pointer }
    case byte of
    0:(offset : long);
    1:(ptr : pointer {FAR});
  end;

type
  XMSspec = record              { XMS move specification structure }
    length : long;
    src_handle : XMSH;
    src : XMSPTR;
    dst_handle : XMSH;
    dst : XMSPTR;
  end;
type
  TByteArray = Array[0..MAX_ALLOC_CHUNK-1] of byte;

{METHODDEF}
procedure read_xms_store (cinfo : j_common_ptr;
                          info : backing_store_ptr;
                          buffer_address : pointer; {FAR}
                          file_offset : long;
                          byte_count : long); far;
var
  ctx : XMScontext;
  spec : XMSspec;
  endbuffer : packed array[0..1] of byte;
begin
  { The XMS driver can't cope with an odd length, so handle the last byte
    specially if byte_count is odd.  We don't expect this to be common. }


  spec.length := byte_count and (not long(1));
  spec.src_handle := info^.handle.xms_handle;
  spec.src.offset := file_offset;
  spec.dst_handle := 0;
  spec.dst.ptr := buffer_address;

  ctx.ds_si := addr(spec);
  ctx.ax := $0b00;              { EMB move }
  jxms_calldriver(xms_driver, ctx);
  if (ctx.ax <> 1) then
    ERREXIT(cinfo, JERR_XMS_READ);

  if odd(byte_count) then
  begin
    read_xms_store(cinfo, info, pointer(@endbuffer) {FAR},
                   file_offset + byte_count - long(1), long(2));
    TByteArray(buffer_address^)[byte_count - long(1)] := endbuffer[0];
  end;
end;


{METHODDEF}
procedure write_xms_store (cinfo : j_common_ptr;
                           info : backing_store_ptr;
                           buffer_address : pointer; {FAR}
                           file_offset : long;
                           byte_count : long); far;
var
  ctx : XMScontext;
  spec : XMSspec;
  endbuffer : packed array[0..1] of byte;
begin
  { The XMS driver can't cope with an odd length, so handle the last byte
    specially if byte_count is odd.  We don't expect this to be common. }

  spec.length := byte_count and (not long(1));
  spec.src_handle := 0;
  spec.src.ptr := buffer_address;
  spec.dst_handle := info^.handle.xms_handle;
  spec.dst.offset := file_offset;

  ctx.ds_si := addr(spec);
  ctx.ax := $0b00;              { EMB move }
  jxms_calldriver(xms_driver, ctx);
  if (ctx.ax <> 1) then
    ERREXIT(cinfo, JERR_XMS_WRITE);

  if odd(byte_count) then
  begin
    read_xms_store(cinfo, info, pointer(@endbuffer) {FAR},
                   file_offset + byte_count - long(1), long(2));
    endbuffer[0] := TByteArray(buffer_address^)[byte_count - long(1)];
    write_xms_store(cinfo, info, pointer(@endbuffer) {FAR},
                    file_offset + byte_count - long(1), long(2));
  end;
end;


{METHODDEF}
procedure close_xms_store (cinfo : j_common_ptr;
                           info : backing_store_ptr); far;
var
  ctx : XMScontext;
begin
  ctx.dx := info^.handle.xms_handle;
  ctx.ax := $0a00;
  jxms_calldriver(xms_driver, ctx);
  TRACEMS1(cinfo, 1, JTRC_XMS_CLOSE, info^.handle.xms_handle);
  { we ignore any error return from the driver }
end;


{LOCAL}
function open_xms_store (cinfo : j_common_ptr;
                         info : backing_store_ptr;
                         total_bytes_needed : long) : boolean;
var
  ctx : XMScontext;
begin
  { Get address of XMS driver }
  jxms_getdriver(xms_driver);
  if (xms_driver = NIL) then
  begin
    open_xms_store := FALSE;    { no driver to be had }
    exit;
  end;

  { Get version number, must be >= 2.00 }
  ctx.ax := $0000;
  jxms_calldriver(xms_driver, ctx);
  if (ctx.ax < ushort($0200)) then
  begin
    open_xms_store := FALSE;
    exit;
  end;

  { Try to get space (expressed in kilobytes) }
  ctx.dx := ushort ((total_bytes_needed + long(1023)) shr 10);
  ctx.ax := $0900;
  jxms_calldriver(xms_driver, ctx);
  if (ctx.ax <> 1) then
  begin
    open_xms_store := FALSE;
    exit;
  end;

  { Succeeded, save the handle and away we go }
  info^.handle.xms_handle := ctx.dx;
  info^.read_backing_store := read_xms_store;
  info^.write_backing_store := write_xms_store;
  info^.close_backing_store := close_xms_store;
  TRACEMS1(cinfo, 1, JTRC_XMS_OPEN, ctx.dx);
  open_xms_store := TRUE;           { succeeded }
end;

{$endif} { XMS_SUPPORTED }


{ Access methods for expanded memory. }

{$ifdef EMS_SUPPORTED}

{ The EMS move specification structure requires word and long fields aligned
  at odd byte boundaries.  Some compilers will align struct fields at even
  byte boundaries.  While it's usually possible to force byte alignment,
  that causes an overall performance penalty and may pose problems in merging
  JPEG into a larger application.  Instead we accept some rather dirty code
  here.  Note this code would fail if the hardware did not allow odd-byte
  word & long accesses, but all 80x86 CPUs do. }


type
  EMSPTR = pointer; {FAR}


{ types for accessing misaligned fields }
type
  EMSAddrStruct = packed record                       {Size }
    MemType : byte; { emsConventional, emsExpanded }  {  1  }
    Handle : word;  { TEMSHandle; }                   {  2  }
    case integer of                                   {union}
      0 : (Offs : word;                               {  2  }
           Page : word);                              {  2  }
      1 : (Ptr : pointer);                            {or 4 }
  end;
  { EMS move specification structure }
  EMSspec = packed record
    length : longint;                                 { 4 }
    src : EMSAddrStruct;                              { 7 }
    dst : EMSAddrStruct;                              { 7 }
  end;


const
  EMSPAGESIZE = long(16384);    { gospel, see the EMS specs }


{METHODDEF}
procedure read_ems_store (cinfo : j_common_ptr;
                          info : backing_store_ptr;
                          buffer_address : pointer; {FAR}
                          file_offset : long;
                          byte_count : long); far;
var
  ctx : EMScontext;
  spec : EMSspec;
begin
  spec.length := byte_count;
  spec.src.memtype := 1;
  spec.src.handle  := info^.handle.ems_handle;
  spec.src.page  := ushort (file_offset div EMSPAGESIZE);
  spec.src.offs := ushort (file_offset mod EMSPAGESIZE);
  spec.dst.memtype  := 0;
  spec.dst.handle := 0;
  spec.dst.ptr  := buffer_address;

  ctx.ds_si := addr(spec);
  ctx.ax := $5700;              { move memory region }
  jems_calldriver(ctx);
  if (hi(ctx.ax) <> 0) then
    ERREXIT(cinfo, JERR_EMS_READ);
end;


{METHODDEF}
procedure write_ems_store (cinfo : j_common_ptr;
                           info : backing_store_ptr;
                           buffer_address : pointer; {FAR}
                           file_offset : long;
                           byte_count : long); far;
var
  ctx : EMScontext;
  spec : EMSspec;
begin
  spec.length := byte_count;
  spec.src.memtype := 0;
  spec.src.handle := 0;
  spec.src.ptr := buffer_address;
  spec.dst.memtype := 1;
  spec.dst.handle := info^.handle.ems_handle;
  spec.dst.page := ushort (file_offset div EMSPAGESIZE);
  spec.dst.offs := ushort (file_offset mod EMSPAGESIZE);

  ctx.ds_si := addr(spec);
  ctx.ax := $5700;              { move memory region }
  jems_calldriver(ctx);
  if (hi(ctx.ax) <> 0) then
    ERREXIT(cinfo, JERR_EMS_WRITE);
end;


{METHODDEF}
procedure close_ems_store (cinfo : j_common_ptr;
                           info : backing_store_ptr); far;
var
  ctx : EMScontext;
begin
  ctx.ax := $4500;
  ctx.dx := info^.handle.ems_handle;
  jems_calldriver(ctx);
  TRACEMS1(cinfo, 1, JTRC_EMS_CLOSE, info^.handle.ems_handle);
  { we ignore any error return from the driver }
end;


{LOCAL}
function open_ems_store (cinfo : j_common_ptr;
                         info : backing_store_ptr;
                         total_bytes_needed : long) : boolean;
var
  ctx : EMScontext;
begin
  { Is EMS driver there? }
  if (jems_available = 0) then
  begin
    open_ems_store := FALSE;
    exit;
  end;

  { Get status, make sure EMS is OK }
  ctx.ax := $4000;
  jems_calldriver(ctx);
  if (hi(ctx.ax) <> 0) then
  begin
    open_ems_store := FALSE;
    exit;
  end;

  { Get version, must be >= 4.0 }
  ctx.ax := $4600;
  jems_calldriver(ctx);
  if (hi(ctx.ax) <> 0) or (lo(ctx.ax) < $40) then
  begin
    open_ems_store := FALSE;
    exit;
  end;

  { Try to allocate requested space }
  ctx.ax := $4300;
  ctx.bx := ushort ((total_bytes_needed +
                     EMSPAGESIZE-long(1)) div EMSPAGESIZE);
  jems_calldriver(ctx);
  if (hi(ctx.ax) <> 0) then
  begin
    open_ems_store := FALSE;
    exit;
  end;

  { Succeeded, save the handle and away we go }
  info^.handle.ems_handle := ctx.dx;
  info^.read_backing_store := read_ems_store;
  info^.write_backing_store := write_ems_store;
  info^.close_backing_store := close_ems_store;
  TRACEMS1(cinfo, 1, JTRC_EMS_OPEN, ctx.dx);
  open_ems_store := TRUE;        { succeeded }
end;

{$endif} { EMS_SUPPORTED }

{ Initial opening of a backing-store object.  This must fill in the
  read/write/close pointers in the object.  The read/write routines
  may take an error exit if the specified maximum file size is exceeded.
  (If jpeg_mem_available always returns a large value, this routine can
  just take an error exit.) }




{ Initial opening of a backing-store object. }

{GLOBAL}
procedure jpeg_open_backing_store (cinfo : j_common_ptr;
                                   info : backing_store_ptr;
                                   total_bytes_needed : long);
begin
  { Try extended memory, then expanded memory, then regular file. }
{$ifdef XMS_SUPPORTED}
  if (open_xms_store(cinfo, info, total_bytes_needed)) then
    exit;
{$endif}
{$ifdef EMS_SUPPORTED}
  if (open_ems_store(cinfo, info, total_bytes_needed)) then
    exit;
{$endif}
  if (open_file_store(cinfo, info, total_bytes_needed)) then
    exit;
  ERREXITS(cinfo, JERR_TFILE_CREATE, '');
end;

{ These routines take care of any system-dependent initialization and
  cleanup required.  jpeg_mem_init will be called before anything is
  allocated (and, therefore, nothing in cinfo is of use except the error
  manager pointer).  It should return a suitable default value for
  max_memory_to_use; this may subsequently be overridden by the surrounding
  application.  (Note that max_memory_to_use is only important if
  jpeg_mem_available chooses to consult it ... no one else will.)
  jpeg_mem_term may assume that all requested memory has been freed and that
  all opened backing-store objects have been closed. }


{ These routines take care of any system-dependent initialization and
  cleanup required. }


{GLOBAL}
function jpeg_mem_init (cinfo : j_common_ptr) : long;
begin
  next_file_num := 0;           { initialize temp file name generator }
  jpeg_mem_init := DEFAULT_MAX_MEM;   { default for max_memory_to_use }
end;

{GLOBAL}
procedure jpeg_mem_term (cinfo : j_common_ptr);
begin
  { Microsoft C, at least in v6.00A, will not successfully reclaim freed
    blocks of size > 32Kbytes unless we give it a kick in the rear,
    like so: }

{$ifdef NEED_FHEAPMIN}
  _fheapmin();
{$endif}
end;

end.
