Unit jmemnobs;
{ Delphi3 -- > jmemnobs from jmemwin }
{ This file provides an Win32-compatible implementation of the system-
  dependent portion of the JPEG memory manager. }

{ Check jmemnobs.c }
{ Copyright (C) 1996, Jacques Nomssi Nzali }


interface

{$I jconfig.inc}

uses
  jmorecfg,
  jdeferr,
  jerror,
  jpeglib;

{ The macro MAX_ALLOC_CHUNK designates the maximum number of bytes that may
  be requested in a single call to jpeg_get_large (and jpeg_get_small for that
  matter, but that case should never come into play).  This macro is needed
  to model the 64Kb-segment-size limit of far addressing on 80x86 machines.
  On those machines, we expect that jconfig.h will provide a proper value.
  On machines with 32-bit flat address spaces, any large constant may be used.

  NB: jmemmgr.c expects that MAX_ALLOC_CHUNK will be representable as type
  size_t and will be a multiple of sizeof(align_type). }

{$IFDEF CPU16}
const
  MAX_ALLOC_CHUNK = long(32752);
{$ELSE}
const
  MAX_ALLOC_CHUNK = long(1000000000);
{$ENDIF}

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


implementation

{ This structure holds whatever state is needed to access a single
  backing-store object.  The read/write/close method pointers are called
  by jmemmgr.c to manipulate the backing-store object; all other fields
  are private to the system-dependent backing store routines. }



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
  GetMem(p, sizeofobject);
  jpeg_get_small := p;
end;

{GLOBAL}
{object is a reserved word in Object Pascal }
procedure jpeg_free_small (cinfo : j_common_ptr;
                           an_object : pointer;
                           sizeofobject : size_t);
begin
  FreeMem(an_object, sizeofobject);
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
  p : pointer;
begin
  GetMem(p, sizeofobject);
  jpeg_get_large := p;
end;

{GLOBAL}
procedure jpeg_free_large (cinfo : j_common_ptr;
                          {var?} an_object : voidp; {FAR}
                          sizeofobject : size_t);
begin
  Freemem(an_object, sizeofobject);
end;

{ This routine computes the total space still available for allocation by
  jpeg_get_large.  If more space than this is needed, backing store will be
  used.  NOTE: any memory already allocated must not be counted.

  There is a minimum space requirement, corresponding to the minimum
  feasible buffer sizes; jmemmgr.c will request that much space even if
  jpeg_mem_available returns zero.  The maximum space needed, enough to hold
  all working storage in memory, is also passed in case it is useful.
  Finally, the total space already allocated is passed.  If no better
  method is available, cinfo^.mem^.max_memory_to_use - already_allocated
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
  jpeg_mem_available := max_bytes_needed;
end;


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
  ERREXIT(cinfo, JERR_NO_BACKING_STORE);
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
  jpeg_mem_init := DEFAULT_MAX_MEM;   { default for max_memory_to_use }
end;

{GLOBAL}
procedure jpeg_mem_term (cinfo : j_common_ptr);
begin

end;


end.
