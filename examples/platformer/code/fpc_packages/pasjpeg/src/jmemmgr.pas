Unit JMemMgr;

{ This file contains the JPEG system-independent memory management
  routines.  This code is usable across a wide variety of machines; most
  of the system dependencies have been isolated in a separate file.
  The major functions provided here are:
    * pool-based allocation and freeing of memory;
    * policy decisions about how to divide available memory among the
      virtual arrays;
    * control logic for swapping virtual arrays between main memory and
      backing storage.
  The separate system-dependent file provides the actual backing-storage
  access code, and it contains the policy decision about how much total
  main memory to use.
  This file is system-dependent in the sense that some of its functions
  are unnecessary in some systems.  For example, if there is enough virtual
  memory so that backing storage will never be used, much of the virtual
  array control logic could be removed.  (Of course, if you have that much
  memory then you shouldn't care about a little bit of unused code...) }

{ Original : jmemmgr.c ; Copyright (C) 1991-1997, Thomas G. Lane. }

interface

{$I jconfig.inc}

uses
   jmorecfg,
   jinclude,
   jdeferr,
   jerror,
   jpeglib,
   jutils,
{$IFDEF VER70}
{$ifndef NO_GETENV}
   Dos,                         { DOS unit should declare getenv() }
                                { function GetEnv(name : string) : string; }
{$endif}
   jmemdos;                     { import the system-dependent declarations }
{$ELSE}
   jmemnobs;
  {$DEFINE NO_GETENV}
{$ENDIF}

{ Memory manager initialization.
  When this is called, only the error manager pointer is valid in cinfo! }

{GLOBAL}
procedure jinit_memory_mgr (cinfo : j_common_ptr);

implementation


{ Some important notes:
    The allocation routines provided here must never return NIL.
    They should exit to error_exit if unsuccessful.

    It's not a good idea to try to merge the sarray and barray routines,
    even though they are textually almost the same, because samples are
    usually stored as bytes while coefficients are shorts or ints.  Thus,
    in machines where byte pointers have a different representation from
    word pointers, the resulting machine code could not be the same.  }


{ Many machines require storage alignment: longs must start on 4-byte
  boundaries, doubles on 8-byte boundaries, etc.  On such machines, malloc()
  always returns pointers that are multiples of the worst-case alignment
  requirement, and we had better do so too.
  There isn't any really portable way to determine the worst-case alignment
  requirement.  This module assumes that the alignment requirement is
  multiples of sizeof(ALIGN_TYPE).
  By default, we define ALIGN_TYPE as double.  This is necessary on some
  workstations (where doubles really do need 8-byte alignment) and will work
  fine on nearly everything.  If your machine has lesser alignment needs,
  you can save a few bytes by making ALIGN_TYPE smaller.
  The only place I know of where this will NOT work is certain Macintosh
  680x0 compilers that define double as a 10-byte IEEE extended float.
  Doing 10-byte alignment is counterproductive because longwords won't be
  aligned well.  Put "#define ALIGN_TYPE long" in jconfig.h if you have
  such a compiler. }

{$ifndef ALIGN_TYPE} { so can override from jconfig.h }
type
  ALIGN_TYPE = double;
{$endif}


{ We allocate objects from "pools", where each pool is gotten with a single
  request to jpeg_get_small() or jpeg_get_large().  There is no per-object
  overhead within a pool, except for alignment padding.  Each pool has a
  header with a link to the next pool of the same class.
  Small and large pool headers are identical except that the latter's
  link pointer must be FAR on 80x86 machines.
  Notice that the "real" header fields are union'ed with a dummy ALIGN_TYPE
  field.  This forces the compiler to make SIZEOF(small_pool_hdr) a multiple
  of the alignment requirement of ALIGN_TYPE. }

type
  small_pool_ptr = ^small_pool_hdr;
  small_pool_hdr = record
  case byte of
    0:(hdr : record
               next : small_pool_ptr;   { next in list of pools }
               bytes_used : size_t;     { how many bytes already used within pool }
               bytes_left : size_t;     { bytes still available in this pool }
             end);
    1:(dummy : ALIGN_TYPE);             { included in union to ensure alignment }
  end; {small_pool_hdr;}

type
  large_pool_ptr = ^large_pool_hdr; {FAR}
  large_pool_hdr = record
  case byte of
    0:(hdr : record
               next : large_pool_ptr;   { next in list of pools }
               bytes_used : size_t;     { how many bytes already used within pool }
               bytes_left : size_t;     { bytes still available in this pool }
             end);
    1:(dummy : ALIGN_TYPE);             { included in union to ensure alignment }
  end; {large_pool_hdr;}


{ Here is the full definition of a memory manager object. }

type
  my_mem_ptr = ^my_memory_mgr;
  my_memory_mgr = record
    pub : jpeg_memory_mgr;      { public fields }

    { Each pool identifier (lifetime class) names a linked list of pools. }
    small_list : array[0..JPOOL_NUMPOOLS-1] of small_pool_ptr ;
    large_list : array[0..JPOOL_NUMPOOLS-1] of large_pool_ptr ;

    { Since we only have one lifetime class of virtual arrays, only one
      linked list is necessary (for each datatype).  Note that the virtual
      array control blocks being linked together are actually stored somewhere
      in the small-pool list. }

    virt_sarray_list : jvirt_sarray_ptr;
    virt_barray_list : jvirt_barray_ptr;

    { This counts total space obtained from jpeg_get_small/large }
    total_space_allocated : long;

    { alloc_sarray and alloc_barray set this value for use by virtual
      array routines. }

    last_rowsperchunk : JDIMENSION;     { from most recent alloc_sarray/barray }
  end; {my_memory_mgr;}

  {$ifndef AM_MEMORY_MANAGER}   { only jmemmgr.c defines these }

{ The control blocks for virtual arrays.
  Note that these blocks are allocated in the "small" pool area.
  System-dependent info for the associated backing store (if any) is hidden
  inside the backing_store_info struct. }
type
  jvirt_sarray_control = record
    mem_buffer : JSAMPARRAY;    { => the in-memory buffer }
    rows_in_array : JDIMENSION; { total virtual array height }
    samplesperrow : JDIMENSION; { width of array (and of memory buffer) }
    maxaccess : JDIMENSION;     { max rows accessed by access_virt_sarray }
    rows_in_mem : JDIMENSION;   { height of memory buffer }
    rowsperchunk : JDIMENSION;  { allocation chunk size in mem_buffer }
    cur_start_row : JDIMENSION; { first logical row # in the buffer }
    first_undef_row : JDIMENSION;       { row # of first uninitialized row }
    pre_zero : boolean;         { pre-zero mode requested? }
    dirty : boolean;            { do current buffer contents need written? }
    b_s_open : boolean;         { is backing-store data valid? }
    next : jvirt_sarray_ptr;    { link to next virtual sarray control block }
    b_s_info : backing_store_info;      { System-dependent control info }
  end;

  jvirt_barray_control = record
    mem_buffer : JBLOCKARRAY;   { => the in-memory buffer }
    rows_in_array : JDIMENSION; { total virtual array height }
    blocksperrow : JDIMENSION;  { width of array (and of memory buffer) }
    maxaccess : JDIMENSION;     { max rows accessed by access_virt_barray }
    rows_in_mem : JDIMENSION;   { height of memory buffer }
    rowsperchunk : JDIMENSION;  { allocation chunk size in mem_buffer }
    cur_start_row : JDIMENSION; { first logical row # in the buffer }
    first_undef_row : JDIMENSION;       { row # of first uninitialized row }
    pre_zero : boolean;         { pre-zero mode requested? }
    dirty : boolean;            { do current buffer contents need written? }
    b_s_open : boolean;         { is backing-store data valid? }
    next : jvirt_barray_ptr;    { link to next virtual barray control block }
    b_s_info : backing_store_info;      { System-dependent control info }
  end;
  {$endif}  { AM_MEMORY_MANAGER}

{$ifdef MEM_STATS}              { optional extra stuff for statistics }

{LOCAL}
procedure print_mem_stats (cinfo : j_common_ptr; pool_id : int);
var
  mem : my_mem_ptr;
  shdr_ptr : small_pool_ptr;
  lhdr_ptr : large_pool_ptr;
begin
  mem := my_mem_ptr (cinfo^.mem);

  { Since this is only a debugging stub, we can cheat a little by using
    fprintf directly rather than going through the trace message code.
    This is helpful because message parm array can't handle longs. }

  WriteLn(output, 'Freeing pool ', pool_id,', total space := ',
           mem^.total_space_allocated);

  lhdr_ptr := mem^.large_list[pool_id];
  while (lhdr_ptr <> NIL) do
  begin
    WriteLn(output, '  Large chunk used ',
            long (lhdr_ptr^.hdr.bytes_used));
    lhdr_ptr := lhdr_ptr^.hdr.next;
  end;

  shdr_ptr := mem^.small_list[pool_id];

  while (shdr_ptr <> NIL) do
  begin
    WriteLn(output, '  Small chunk used ',
                    long (shdr_ptr^.hdr.bytes_used), ' free ',
                    long (shdr_ptr^.hdr.bytes_left) );
    shdr_ptr := shdr_ptr^.hdr.next;
  end;
end;

{$endif} { MEM_STATS }


{LOCAL}
procedure out_of_memory (cinfo : j_common_ptr; which : int);
{ Report an out-of-memory error and stop execution }
{ If we compiled MEM_STATS support, report alloc requests before dying }
begin
{$ifdef MEM_STATS}
  cinfo^.err^.trace_level := 2; { force self_destruct to report stats }
{$endif}
  ERREXIT1(cinfo, JERR_OUT_OF_MEMORY, which);
end;


{ Allocation of "small" objects.

  For these, we use pooled storage.  When a new pool must be created,
  we try to get enough space for the current request plus a "slop" factor,
  where the slop will be the amount of leftover space in the new pool.
  The speed vs. space tradeoff is largely determined by the slop values.
  A different slop value is provided for each pool class (lifetime),
  and we also distinguish the first pool of a class from later ones.
  NOTE: the values given work fairly well on both 16- and 32-bit-int
  machines, but may be too small if longs are 64 bits or more. }

const
  first_pool_slop : array[0..JPOOL_NUMPOOLS-1] of size_t  =
        (1600,                  { first PERMANENT pool }
        16000);                 { first IMAGE pool }

const
  extra_pool_slop : array[0..JPOOL_NUMPOOLS-1] of size_t =
        (0,                     { additional PERMANENT pools }
        5000);                  { additional IMAGE pools }

const
  MIN_SLOP = 50;                { greater than 0 to avoid futile looping }


{METHODDEF}
function alloc_small (cinfo : j_common_ptr;
                      pool_id : int;
                      sizeofobject : size_t) : pointer; far;
type
  byteptr = ^byte;
{ Allocate a "small" object }
var
  mem : my_mem_ptr;
  hdr_ptr, prev_hdr_ptr : small_pool_ptr;
  data_ptr : byteptr;
  odd_bytes, min_request, slop : size_t;
begin
  mem := my_mem_ptr (cinfo^.mem);

  { Check for unsatisfiable request (do now to ensure no overflow below) }
  if (sizeofobject > size_t(MAX_ALLOC_CHUNK-SIZEOF(small_pool_hdr))) then
    out_of_memory(cinfo, 1);    { request exceeds malloc's ability }

  { Round up the requested size to a multiple of SIZEOF(ALIGN_TYPE) }
  odd_bytes := sizeofobject mod SIZEOF(ALIGN_TYPE);
  if (odd_bytes > 0) then
    Inc(sizeofobject, SIZEOF(ALIGN_TYPE) - odd_bytes);

  { See if space is available in any existing pool }
  if (pool_id < 0) or (pool_id >= JPOOL_NUMPOOLS) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_POOL_ID, pool_id);   { safety check }
  prev_hdr_ptr := NIL;
  hdr_ptr := mem^.small_list[pool_id];
  while (hdr_ptr <> NIL) do
  begin
    if (hdr_ptr^.hdr.bytes_left >= sizeofobject) then
      break;                    { found pool with enough space }
    prev_hdr_ptr := hdr_ptr;
    hdr_ptr := hdr_ptr^.hdr.next;
  end;

  { Time to make a new pool? }
  if (hdr_ptr = NIL) then
  begin
    { min_request is what we need now, slop is what will be leftover }
    min_request := sizeofobject + SIZEOF(small_pool_hdr);
    if (prev_hdr_ptr = NIL) then        { first pool in class? }
      slop := first_pool_slop[pool_id]
    else
      slop := extra_pool_slop[pool_id];
    { Don't ask for more than MAX_ALLOC_CHUNK }
    if (slop > size_t (MAX_ALLOC_CHUNK-min_request)) then
      slop := size_t (MAX_ALLOC_CHUNK-min_request);
    { Try to get space, if fail reduce slop and try again }
    while TRUE do
    begin
      hdr_ptr := small_pool_ptr(jpeg_get_small(cinfo, min_request + slop));
      if (hdr_ptr <> NIL) then
        break;
      slop := slop div 2;
      if (slop < MIN_SLOP) then   { give up when it gets real small }
        out_of_memory(cinfo, 2);  { jpeg_get_small failed }
    end;
    Inc(mem^.total_space_allocated, min_request + slop);
    { Success, initialize the new pool header and add to end of list }
    hdr_ptr^.hdr.next := NIL;
    hdr_ptr^.hdr.bytes_used := 0;
    hdr_ptr^.hdr.bytes_left := sizeofobject + slop;
    if (prev_hdr_ptr = NIL) then       { first pool in class? }
      mem^.small_list[pool_id] := hdr_ptr
    else
      prev_hdr_ptr^.hdr.next := hdr_ptr;
  end;

  { OK, allocate the object from the current pool }
  data_ptr := byteptr (hdr_ptr);
  Inc(small_pool_ptr(data_ptr));  { point to first data byte in pool }
  Inc(data_ptr, hdr_ptr^.hdr.bytes_used); { point to place for object }
  Inc(hdr_ptr^.hdr.bytes_used, sizeofobject);
  Dec(hdr_ptr^.hdr.bytes_left, sizeofobject);

  alloc_small := pointer(data_ptr);
end;


{ Allocation of "large" objects.

  The external semantics of these are the same as "small" objects,
  except that FAR pointers are used on 80x86.  However the pool
  management heuristics are quite different.  We assume that each
  request is large enough that it may as well be passed directly to
  jpeg_get_large; the pool management just links everything together
  so that we can free it all on demand.
  Note: the major use of "large" objects is in JSAMPARRAY and JBLOCKARRAY
  structures.  The routines that create these structures (see below)
  deliberately bunch rows together to ensure a large request size. }

{METHODDEF}
function alloc_large (cinfo : j_common_ptr;
                      pool_id : int;
                      sizeofobject : size_t) : pointer; FAR;
{ Allocate a "large" object }
var
  mem : my_mem_ptr;
  hdr_ptr : large_pool_ptr;
  odd_bytes : size_t;
var
  dest_ptr : large_pool_ptr;
begin
  mem := my_mem_ptr (cinfo^.mem);

  { Check for unsatisfiable request (do now to ensure no overflow below) }
  if (sizeofobject > size_t (MAX_ALLOC_CHUNK-SIZEOF(large_pool_hdr))) then
    out_of_memory(cinfo, 3);    { request exceeds malloc's ability }

  { Round up the requested size to a multiple of SIZEOF(ALIGN_TYPE) }
  odd_bytes := sizeofobject mod SIZEOF(ALIGN_TYPE);
  if (odd_bytes > 0) then
    Inc(sizeofobject, SIZEOF(ALIGN_TYPE) - odd_bytes);

  { Always make a new pool }
  if (pool_id < 0) or (pool_id >= JPOOL_NUMPOOLS) then
    ERREXIT1(cinfo, JERR_BAD_POOL_ID, pool_id); { safety check }

  hdr_ptr := large_pool_ptr (jpeg_get_large(cinfo, sizeofobject +
                                            SIZEOF(large_pool_hdr)));
  if (hdr_ptr = NIL) then
    out_of_memory(cinfo, 4);    { jpeg_get_large failed }
  Inc(mem^.total_space_allocated, sizeofobject + SIZEOF(large_pool_hdr));

  { Success, initialize the new pool header and add to list }
  hdr_ptr^.hdr.next := mem^.large_list[pool_id];
  { We maintain space counts in each pool header for statistical purposes,
    even though they are not needed for allocation. }

  hdr_ptr^.hdr.bytes_used := sizeofobject;
  hdr_ptr^.hdr.bytes_left := 0;
  mem^.large_list[pool_id] := hdr_ptr;

  {alloc_large := pointerFAR (hdr_ptr + 1); - point to first data byte in pool }
  dest_ptr := hdr_ptr;
  Inc(large_pool_ptr(dest_ptr));
  alloc_large := dest_ptr;
end;


{ Creation of 2-D sample arrays.
  The pointers are in near heap, the samples themselves in FAR heap.

  To minimize allocation overhead and to allow I/O of large contiguous
  blocks, we allocate the sample rows in groups of as many rows as possible
  without exceeding MAX_ALLOC_CHUNK total bytes per allocation request.
  NB: the virtual array control routines, later in this file, know about
  this chunking of rows.  The rowsperchunk value is left in the mem manager
  object so that it can be saved away if this sarray is the workspace for
  a virtual array. }

{METHODDEF}
function alloc_sarray (cinfo : j_common_ptr;
                       pool_id : int;
                       samplesperrow : JDIMENSION;
                       numrows : JDIMENSION) : JSAMPARRAY; far;
{ Allocate a 2-D sample array }
var
  mem : my_mem_ptr;
  the_result : JSAMPARRAY;
  workspace : JSAMPROW;
  rowsperchunk, currow, i : JDIMENSION;
  ltemp : long;
begin
  mem := my_mem_ptr(cinfo^.mem);

  { Calculate max # of rows allowed in one allocation chunk }
  ltemp := (MAX_ALLOC_CHUNK-SIZEOF(large_pool_hdr)) div
          (long(samplesperrow) * SIZEOF(JSAMPLE));
  if (ltemp <= 0) then
    ERREXIT(cinfo, JERR_WIDTH_OVERFLOW);
  if (ltemp < long(numrows)) then
    rowsperchunk := JDIMENSION (ltemp)
  else
    rowsperchunk := numrows;
  mem^.last_rowsperchunk := rowsperchunk;

  { Get space for row pointers (small object) }
  the_result := JSAMPARRAY (alloc_small(cinfo, pool_id,
                                    size_t (numrows * SIZEOF(JSAMPROW))));

  { Get the rows themselves (large objects) }
  currow := 0;
  while (currow < numrows) do
  begin
    {rowsperchunk := MIN(rowsperchunk, numrows - currow);}
    if rowsperchunk > numrows - currow then
      rowsperchunk := numrows - currow;

    workspace := JSAMPROW (alloc_large(cinfo, pool_id,
        size_t (size_t(rowsperchunk) * size_t(samplesperrow)
                  * SIZEOF(JSAMPLE))) );
    for i := pred(rowsperchunk) downto 0 do
    begin
      the_result^[currow] := workspace;
      Inc(currow);
      Inc(JSAMPLE_PTR(workspace), samplesperrow);
    end;
  end;

  alloc_sarray := the_result;
end;


{ Creation of 2-D coefficient-block arrays.
  This is essentially the same as the code for sample arrays, above. }

{METHODDEF}
function alloc_barray (cinfo : j_common_ptr;
                       pool_id : int;
                       blocksperrow : JDIMENSION;
                       numrows : JDIMENSION) : JBLOCKARRAY; far;
{ Allocate a 2-D coefficient-block array }
var
  mem : my_mem_ptr;
  the_result : JBLOCKARRAY;
  workspace : JBLOCKROW;
  rowsperchunk, currow, i : JDIMENSION;
  ltemp : long;
begin
  mem := my_mem_ptr(cinfo^.mem);

  { Calculate max # of rows allowed in one allocation chunk }
  ltemp := (MAX_ALLOC_CHUNK-SIZEOF(large_pool_hdr)) div
          (long(blocksperrow) * SIZEOF(JBLOCK));
  if (ltemp <= 0) then
    ERREXIT(cinfo, JERR_WIDTH_OVERFLOW);
  if (ltemp < long(numrows)) then
    rowsperchunk := JDIMENSION (ltemp)
  else
    rowsperchunk := numrows;
  mem^.last_rowsperchunk := rowsperchunk;

  { Get space for row pointers (small object) }
  the_result := JBLOCKARRAY (alloc_small(cinfo, pool_id,
                                     size_t (numrows * SIZEOF(JBLOCKROW))) );

  { Get the rows themselves (large objects) }
  currow := 0;
  while (currow < numrows) do
  begin
    {rowsperchunk := MIN(rowsperchunk, numrows - currow);}
    if rowsperchunk > numrows - currow then
      rowsperchunk := numrows - currow;

    workspace := JBLOCKROW (alloc_large(cinfo, pool_id,
        size_t (size_t(rowsperchunk) * size_t(blocksperrow)
                  * SIZEOF(JBLOCK))) );
    for i := rowsperchunk downto 1 do
    begin
      the_result^[currow] := workspace;
      Inc(currow);
      Inc(JBLOCK_PTR(workspace), blocksperrow);
    end;
  end;

  alloc_barray := the_result;
end;


{ About virtual array management:

  The above "normal" array routines are only used to allocate strip buffers
  (as wide as the image, but just a few rows high).  Full-image-sized buffers
  are handled as "virtual" arrays.  The array is still accessed a strip at a
  time, but the memory manager must save the whole array for repeated
  accesses.  The intended implementation is that there is a strip buffer in
  memory (as high as is possible given the desired memory limit), plus a
  backing file that holds the rest of the array.

  The request_virt_array routines are told the total size of the image and
  the maximum number of rows that will be accessed at once.  The in-memory
  buffer must be at least as large as the maxaccess value.

  The request routines create control blocks but not the in-memory buffers.
  That is postponed until realize_virt_arrays is called.  At that time the
  total amount of space needed is known (approximately, anyway), so free
  memory can be divided up fairly.

  The access_virt_array routines are responsible for making a specific strip
  area accessible (after reading or writing the backing file, if necessary).
  Note that the access routines are told whether the caller intends to modify
  the accessed strip; during a read-only pass this saves having to rewrite
  data to disk.  The access routines are also responsible for pre-zeroing
  any newly accessed rows, if pre-zeroing was requested.

  In current usage, the access requests are usually for nonoverlapping
  strips; that is, successive access start_row numbers differ by exactly
  num_rows := maxaccess.  This means we can get good performance with simple
  buffer dump/reload logic, by making the in-memory buffer be a multiple
  of the access height; then there will never be accesses across bufferload
  boundaries.  The code will still work with overlapping access requests,
  but it doesn't handle bufferload overlaps very efficiently. }


{METHODDEF}
function request_virt_sarray (cinfo : j_common_ptr;
                              pool_id : int;
                              pre_zero : boolean;
                              samplesperrow : JDIMENSION;
                              numrows : JDIMENSION;
                              maxaccess : JDIMENSION) : jvirt_sarray_ptr; far;
{ Request a virtual 2-D sample array }
var
  mem : my_mem_ptr;
  the_result : jvirt_sarray_ptr;
begin
  mem := my_mem_ptr (cinfo^.mem);

  { Only IMAGE-lifetime virtual arrays are currently supported }
  if (pool_id <> JPOOL_IMAGE) then
    ERREXIT1(cinfo, JERR_BAD_POOL_ID, pool_id); { safety check }

  { get control block }
  the_result := jvirt_sarray_ptr (alloc_small(cinfo, pool_id,
                                  SIZEOF(jvirt_sarray_control)) );

  the_result^.mem_buffer := NIL;        { marks array not yet realized }
  the_result^.rows_in_array := numrows;
  the_result^.samplesperrow := samplesperrow;
  the_result^.maxaccess := maxaccess;
  the_result^.pre_zero := pre_zero;
  the_result^.b_s_open := FALSE;        { no associated backing-store object }
  the_result^.next := mem^.virt_sarray_list; { add to list of virtual arrays }
  mem^.virt_sarray_list := the_result;

  request_virt_sarray := the_result;
end;


{METHODDEF}
function request_virt_barray (cinfo : j_common_ptr;
                              pool_id : int;
                              pre_zero : boolean;
                              blocksperrow : JDIMENSION;
                              numrows : JDIMENSION;
                              maxaccess : JDIMENSION) : jvirt_barray_ptr; far;
{ Request a virtual 2-D coefficient-block array }
var
  mem : my_mem_ptr;
  the_result : jvirt_barray_ptr;
begin
  mem := my_mem_ptr(cinfo^.mem);

  { Only IMAGE-lifetime virtual arrays are currently supported }
  if (pool_id <> JPOOL_IMAGE) then
    ERREXIT1(cinfo, JERR_BAD_POOL_ID, pool_id); { safety check }

  { get control block }
  the_result := jvirt_barray_ptr(alloc_small(cinfo, pool_id,
                                  SIZEOF(jvirt_barray_control)) );

  the_result^.mem_buffer := NIL;        { marks array not yet realized }
  the_result^.rows_in_array := numrows;
  the_result^.blocksperrow := blocksperrow;
  the_result^.maxaccess := maxaccess;
  the_result^.pre_zero := pre_zero;
  the_result^.b_s_open := FALSE;        { no associated backing-store object }
  the_result^.next := mem^.virt_barray_list; { add to list of virtual arrays }
  mem^.virt_barray_list := the_result;

  request_virt_barray := the_result;
end;


{METHODDEF}
procedure realize_virt_arrays (cinfo : j_common_ptr); far;
{ Allocate the in-memory buffers for any unrealized virtual arrays }
var
  mem : my_mem_ptr;
  space_per_minheight, maximum_space, avail_mem : long;
  minheights, max_minheights : long;
  sptr : jvirt_sarray_ptr;
  bptr : jvirt_barray_ptr;
begin
  mem := my_mem_ptr (cinfo^.mem);
  { Compute the minimum space needed (maxaccess rows in each buffer)
    and the maximum space needed (full image height in each buffer).
    These may be of use to the system-dependent jpeg_mem_available routine. }

  space_per_minheight := 0;
  maximum_space := 0;
  sptr := mem^.virt_sarray_list;
  while (sptr <> NIL) do
  begin
    if (sptr^.mem_buffer = NIL) then
    begin { if not realized yet }
      Inc(space_per_minheight, long(sptr^.maxaccess) *
                             long(sptr^.samplesperrow) * SIZEOF(JSAMPLE));
      Inc(maximum_space, long(sptr^.rows_in_array) *
                       long(sptr^.samplesperrow) * SIZEOF(JSAMPLE));
    end;
    sptr := sptr^.next;
  end;
  bptr := mem^.virt_barray_list;
  while (bptr <> NIL) do
  begin
    if (bptr^.mem_buffer = NIL) then
    begin { if not realized yet }
      Inc(space_per_minheight, long(bptr^.maxaccess) *
                             long(bptr^.blocksperrow) * SIZEOF(JBLOCK));
      Inc(maximum_space, long(bptr^.rows_in_array) *
                       long(bptr^.blocksperrow) * SIZEOF(JBLOCK));
    end;
    bptr := bptr^.next;
  end;

  if (space_per_minheight <= 0) then
    exit;                       { no unrealized arrays, no work }

  { Determine amount of memory to actually use; this is system-dependent. }
  avail_mem := jpeg_mem_available(cinfo, space_per_minheight, maximum_space,
                                 mem^.total_space_allocated);

  { If the maximum space needed is available, make all the buffers full
    height; otherwise parcel it out with the same number of minheights
    in each buffer. }

  if (avail_mem >= maximum_space) then
    max_minheights := long(1000000000)
  else
  begin
    max_minheights := avail_mem div space_per_minheight;
    { If there doesn't seem to be enough space, try to get the minimum
      anyway.  This allows a "stub" implementation of jpeg_mem_available(). }
    if (max_minheights <= 0) then
      max_minheights := 1;
  end;

  { Allocate the in-memory buffers and initialize backing store as needed. }

  sptr := mem^.virt_sarray_list;
  while (sptr <> NIL) do
  begin
    if (sptr^.mem_buffer = NIL) then
    begin { if not realized yet }
      minheights := (long(sptr^.rows_in_array) - long(1)) div sptr^.maxaccess + long(1);
      if (minheights <= max_minheights) then
      begin
        { This buffer fits in memory }
        sptr^.rows_in_mem := sptr^.rows_in_array;
      end
      else
      begin
        { It doesn't fit in memory, create backing store. }
        sptr^.rows_in_mem := JDIMENSION (max_minheights * sptr^.maxaccess);
        jpeg_open_backing_store(cinfo,
                                @sptr^.b_s_info,
                                long(sptr^.rows_in_array) *
                                long(sptr^.samplesperrow) *
                                long(SIZEOF(JSAMPLE)));
        sptr^.b_s_open := TRUE;
      end;
      sptr^.mem_buffer := alloc_sarray(cinfo, JPOOL_IMAGE,
                                      sptr^.samplesperrow, sptr^.rows_in_mem);
      sptr^.rowsperchunk := mem^.last_rowsperchunk;
      sptr^.cur_start_row := 0;
      sptr^.first_undef_row := 0;
      sptr^.dirty := FALSE;
    end;
    sptr := sptr^.next;
  end;

  bptr := mem^.virt_barray_list;
  while (bptr <> NIL) do
  begin
    if (bptr^.mem_buffer = NIL) then
    begin { if not realized yet }
      minheights := (long(bptr^.rows_in_array) - long(1)) div bptr^.maxaccess + long(1);
      if (minheights <= max_minheights) then
      begin
        { This buffer fits in memory }
        bptr^.rows_in_mem := bptr^.rows_in_array;
      end
      else
      begin
        { It doesn't fit in memory, create backing store. }
        bptr^.rows_in_mem := JDIMENSION (max_minheights * bptr^.maxaccess);
        jpeg_open_backing_store(cinfo,
                                @bptr^.b_s_info,
                                long(bptr^.rows_in_array) *
                                long(bptr^.blocksperrow) *
                                long(SIZEOF(JBLOCK)));
        bptr^.b_s_open := TRUE;
      end;
      bptr^.mem_buffer := alloc_barray(cinfo, JPOOL_IMAGE,
                                      bptr^.blocksperrow, bptr^.rows_in_mem);
      bptr^.rowsperchunk := mem^.last_rowsperchunk;
      bptr^.cur_start_row := 0;
      bptr^.first_undef_row := 0;
      bptr^.dirty := FALSE;
    end;
    bptr := bptr^.next;
  end;
end;


{LOCAL}
procedure do_sarray_io (cinfo : j_common_ptr;
                        ptr : jvirt_sarray_ptr;
                        writing : boolean);
{ Do backing store read or write of a virtual sample array }
var
  bytesperrow, file_offset, byte_count, rows, thisrow, i : long;
begin

  bytesperrow := long(ptr^.samplesperrow * SIZEOF(JSAMPLE));
  file_offset := ptr^.cur_start_row * bytesperrow;
  { Loop to read or write each allocation chunk in mem_buffer }
  i := 0;
  while i < long(ptr^.rows_in_mem) do
  begin

    { One chunk, but check for short chunk at end of buffer }
    {rows := MIN(long(ptr^.rowsperchunk), long(ptr^.rows_in_mem - i));}
    rows := long(ptr^.rowsperchunk);
    if rows > long(ptr^.rows_in_mem - i) then
      rows := long(ptr^.rows_in_mem - i);
    { Transfer no more than is currently defined }
    thisrow := long (ptr^.cur_start_row) + i;
    {rows := MIN(rows, long(ptr^.first_undef_row) - thisrow);}
    if (rows > long(ptr^.first_undef_row) - thisrow) then
      rows := long(ptr^.first_undef_row) - thisrow;
    { Transfer no more than fits in file }
    {rows := MIN(rows, long(ptr^.rows_in_array) - thisrow);}
    if (rows > long(ptr^.rows_in_array) - thisrow) then
      rows := long(ptr^.rows_in_array) - thisrow;

    if (rows <= 0) then        { this chunk might be past end of file! }
      break;
    byte_count := rows * bytesperrow;
    if (writing) then
      ptr^.b_s_info.write_backing_store (cinfo,
                                        @ptr^.b_s_info,
                                        pointer {FAR} (ptr^.mem_buffer^[i]),
                                        file_offset, byte_count)
    else
      ptr^.b_s_info.read_backing_store (cinfo,
                                        @ptr^.b_s_info,
                                        pointer {FAR} (ptr^.mem_buffer^[i]),
                                        file_offset, byte_count);
    Inc(file_offset, byte_count);
    Inc(i, ptr^.rowsperchunk);
  end;
end;


{LOCAL}
procedure do_barray_io (cinfo : j_common_ptr;
                       ptr : jvirt_barray_ptr;
                       writing : boolean);
{ Do backing store read or write of a virtual coefficient-block array }
var
  bytesperrow, file_offset, byte_count, rows, thisrow, i : long;
begin
  bytesperrow := long (ptr^.blocksperrow) * SIZEOF(JBLOCK);
  file_offset := ptr^.cur_start_row * bytesperrow;
  { Loop to read or write each allocation chunk in mem_buffer }
  i := 0;
  while (i < long(ptr^.rows_in_mem)) do
  begin
    { One chunk, but check for short chunk at end of buffer }
    {rows := MIN(long(ptr^.rowsperchunk), long(ptr^.rows_in_mem - i));}
    rows := long(ptr^.rowsperchunk);
    if rows >long(ptr^.rows_in_mem - i) then
      rows := long(ptr^.rows_in_mem - i);
    { Transfer no more than is currently defined }
    thisrow := long (ptr^.cur_start_row) + i;
    {rows := MIN(rows, long(ptr^.first_undef_row - thisrow));}
    if rows > long(ptr^.first_undef_row - thisrow) then
      rows := long(ptr^.first_undef_row - thisrow);
    { Transfer no more than fits in file }
    {rows := MIN(rows, long (ptr^.rows_in_array - thisrow));}
    if (rows > long (ptr^.rows_in_array - thisrow)) then
      rows := long (ptr^.rows_in_array - thisrow);

    if (rows <= 0) then         { this chunk might be past end of file! }
      break;
    byte_count := rows * bytesperrow;
    if (writing) then
      ptr^.b_s_info.write_backing_store (cinfo,
                                         @ptr^.b_s_info,
                                         {FAR} pointer(ptr^.mem_buffer^[i]),
                                          file_offset, byte_count)
    else
      ptr^.b_s_info.read_backing_store (cinfo,
                                        @ptr^.b_s_info,
                                        {FAR} pointer(ptr^.mem_buffer^[i]),
                                        file_offset, byte_count);
    Inc(file_offset, byte_count);
    Inc(i, ptr^.rowsperchunk);
  end;
end;


{METHODDEF}
function access_virt_sarray (cinfo : j_common_ptr;
                             ptr : jvirt_sarray_ptr;
                             start_row : JDIMENSION;
                             num_rows : JDIMENSION;
                             writable : boolean ) : JSAMPARRAY; far;
{ Access the part of a virtual sample array starting at start_row }
{ and extending for num_rows rows.  writable is true if  }
{ caller intends to modify the accessed area. }
var
  end_row : JDIMENSION;
  undef_row : JDIMENSION;
var
  bytesperrow : size_t;
var
  ltemp : long;
begin
  end_row := start_row + num_rows;
  { debugging check }
  if (end_row > ptr^.rows_in_array) or (num_rows > ptr^.maxaccess) or
     (ptr^.mem_buffer = NIL) then
    ERREXIT(cinfo, JERR_BAD_VIRTUAL_ACCESS);

  { Make the desired part of the virtual array accessible }
  if (start_row < ptr^.cur_start_row) or
     (end_row > ptr^.cur_start_row+ptr^.rows_in_mem) then
  begin
    if (not ptr^.b_s_open) then
      ERREXIT(cinfo, JERR_VIRTUAL_BUG);
    { Flush old buffer contents if necessary }
    if (ptr^.dirty) then
    begin
      do_sarray_io(cinfo, ptr, TRUE);
      ptr^.dirty := FALSE;
    end;
    { Decide what part of virtual array to access.
      Algorithm: if target address > current window, assume forward scan,
      load starting at target address.  If target address < current window,
      assume backward scan, load so that target area is top of window.
      Note that when switching from forward write to forward read, will have
      start_row := 0, so the limiting case applies and we load from 0 anyway. }
    if (start_row > ptr^.cur_start_row) then
    begin
      ptr^.cur_start_row := start_row;
    end
    else
    begin
      { use long arithmetic here to avoid overflow & unsigned problems }


      ltemp := long(end_row) - long(ptr^.rows_in_mem);
      if (ltemp < 0) then
        ltemp := 0;             { don't fall off front end of file }
      ptr^.cur_start_row := JDIMENSION(ltemp);
    end;
    { Read in the selected part of the array.
      During the initial write pass, we will do no actual read
      because the selected part is all undefined. }

    do_sarray_io(cinfo, ptr, FALSE);
  end;
  { Ensure the accessed part of the array is defined; prezero if needed.
    To improve locality of access, we only prezero the part of the array
    that the caller is about to access, not the entire in-memory array. }
  if (ptr^.first_undef_row < end_row) then
  begin
    if (ptr^.first_undef_row < start_row) then
    begin
      if (writable) then        { writer skipped over a section of array }
        ERREXIT(cinfo, JERR_BAD_VIRTUAL_ACCESS);
      undef_row := start_row;   { but reader is allowed to read ahead }
    end
    else
    begin
      undef_row := ptr^.first_undef_row;
    end;
    if (writable) then
      ptr^.first_undef_row := end_row;
    if (ptr^.pre_zero) then
    begin
      bytesperrow := size_t(ptr^.samplesperrow) * SIZEOF(JSAMPLE);
      Dec(undef_row, ptr^.cur_start_row); { make indexes relative to buffer }
      Dec(end_row, ptr^.cur_start_row);
      while (undef_row < end_row) do
      begin
        jzero_far({FAR} pointer(ptr^.mem_buffer^[undef_row]), bytesperrow);
        Inc(undef_row);
      end;
    end
    else
    begin
      if (not writable) then    { reader looking at undefined data }
        ERREXIT(cinfo, JERR_BAD_VIRTUAL_ACCESS);
    end;
  end;
  { Flag the buffer dirty if caller will write in it }
  if (writable) then
    ptr^.dirty := TRUE;
  { Return address of proper part of the buffer }
  access_virt_sarray := JSAMPARRAY(@ ptr^.mem_buffer^[start_row - ptr^.cur_start_row]);
end;


{METHODDEF}
function access_virt_barray (cinfo : j_common_ptr;
                             ptr : jvirt_barray_ptr;
                             start_row : JDIMENSION;
                             num_rows : JDIMENSION;
                             writable : boolean) : JBLOCKARRAY; far;
{ Access the part of a virtual block array starting at start_row }
{ and extending for num_rows rows.  writable is true if  }
{ caller intends to modify the accessed area. }
var
  end_row : JDIMENSION;
  undef_row : JDIMENSION;
  ltemp : long;
var
  bytesperrow : size_t;
begin
  end_row := start_row + num_rows;

  { debugging check }
  if (end_row > ptr^.rows_in_array) or (num_rows > ptr^.maxaccess) or
     (ptr^.mem_buffer = NIL) then
    ERREXIT(cinfo, JERR_BAD_VIRTUAL_ACCESS);

  { Make the desired part of the virtual array accessible }
  if (start_row < ptr^.cur_start_row) or
     (end_row > ptr^.cur_start_row+ptr^.rows_in_mem) then
  begin
    if (not ptr^.b_s_open) then
      ERREXIT(cinfo, JERR_VIRTUAL_BUG);
    { Flush old buffer contents if necessary }
    if (ptr^.dirty) then
    begin
      do_barray_io(cinfo, ptr, TRUE);
      ptr^.dirty := FALSE;
    end;
    { Decide what part of virtual array to access.
      Algorithm: if target address > current window, assume forward scan,
      load starting at target address.  If target address < current window,
      assume backward scan, load so that target area is top of window.
      Note that when switching from forward write to forward read, will have
      start_row := 0, so the limiting case applies and we load from 0 anyway. }

    if (start_row > ptr^.cur_start_row) then
    begin
      ptr^.cur_start_row := start_row;
    end
    else
    begin
      { use long arithmetic here to avoid overflow & unsigned problems }

      ltemp := long(end_row) - long(ptr^.rows_in_mem);
      if (ltemp < 0) then
        ltemp := 0;             { don't fall off front end of file }
      ptr^.cur_start_row := JDIMENSION (ltemp);
    end;
    { Read in the selected part of the array.
      During the initial write pass, we will do no actual read
      because the selected part is all undefined. }

    do_barray_io(cinfo, ptr, FALSE);
  end;
  { Ensure the accessed part of the array is defined; prezero if needed.
    To improve locality of access, we only prezero the part of the array
    that the caller is about to access, not the entire in-memory array. }

  if (ptr^.first_undef_row < end_row) then
  begin
    if (ptr^.first_undef_row < start_row) then
    begin
      if (writable) then        { writer skipped over a section of array }
        ERREXIT(cinfo, JERR_BAD_VIRTUAL_ACCESS);
      undef_row := start_row;   { but reader is allowed to read ahead }
    end
    else
    begin
      undef_row := ptr^.first_undef_row;
    end;
    if (writable) then
      ptr^.first_undef_row := end_row;
    if (ptr^.pre_zero) then
    begin
      bytesperrow := size_t (ptr^.blocksperrow) * SIZEOF(JBLOCK);
      Dec(undef_row, ptr^.cur_start_row); { make indexes relative to buffer }
      Dec(end_row, ptr^.cur_start_row);
      while (undef_row < end_row) do
      begin
        jzero_far({FAR}pointer(ptr^.mem_buffer^[undef_row]), bytesperrow);
        Inc(undef_row);
      end;
    end
    else
    begin
      if (not writable) then    { reader looking at undefined data }
        ERREXIT(cinfo, JERR_BAD_VIRTUAL_ACCESS);
    end;
  end;
  { Flag the buffer dirty if caller will write in it }
  if (writable) then
    ptr^.dirty := TRUE;
  { Return address of proper part of the buffer }
  access_virt_barray := JBLOCKARRAY(@ ptr^.mem_buffer^[start_row - ptr^.cur_start_row]);
end;


{ Release all objects belonging to a specified pool. }

{METHODDEF}
procedure free_pool (cinfo : j_common_ptr; pool_id : int); far;
var
  mem : my_mem_ptr;
  shdr_ptr : small_pool_ptr;
  lhdr_ptr : large_pool_ptr;
  space_freed : size_t;
var
  sptr : jvirt_sarray_ptr;
  bptr : jvirt_barray_ptr;
var
  next_lhdr_ptr : large_pool_ptr;
  next_shdr_ptr : small_pool_ptr;
begin
  mem := my_mem_ptr(cinfo^.mem);

  if (pool_id < 0) or (pool_id >= JPOOL_NUMPOOLS) then
    ERREXIT1(cinfo, JERR_BAD_POOL_ID, pool_id); { safety check }

{$ifdef MEM_STATS}
  if (cinfo^.err^.trace_level > 1) then
    print_mem_stats(cinfo, pool_id); { print pool's memory usage statistics }
{$endif}

  { If freeing IMAGE pool, close any virtual arrays first }
  if (pool_id = JPOOL_IMAGE) then
  begin
    sptr := mem^.virt_sarray_list;
    while (sptr <> NIL) do
    begin
      if (sptr^.b_s_open) then
      begin     { there may be no backing store }
        sptr^.b_s_open := FALSE;        { prevent recursive close if error }
        sptr^.b_s_info.close_backing_store (cinfo, @sptr^.b_s_info);
      end;
      sptr := sptr^.next;
    end;
    mem^.virt_sarray_list := NIL;
    bptr := mem^.virt_barray_list;
    while (bptr <> NIL) do
    begin
      if (bptr^.b_s_open) then
      begin     { there may be no backing store }
        bptr^.b_s_open := FALSE;        { prevent recursive close if error }
        bptr^.b_s_info.close_backing_store (cinfo, @bptr^.b_s_info);
      end;
      bptr := bptr^.next;
    end;
    mem^.virt_barray_list := NIL;
  end;

  { Release large objects }
  lhdr_ptr := mem^.large_list[pool_id];
  mem^.large_list[pool_id] := NIL;

  while (lhdr_ptr <> NIL) do
  begin
    next_lhdr_ptr := lhdr_ptr^.hdr.next;
    space_freed := lhdr_ptr^.hdr.bytes_used +
                  lhdr_ptr^.hdr.bytes_left +
                  SIZEOF(large_pool_hdr);
    jpeg_free_large(cinfo, {FAR} pointer(lhdr_ptr), space_freed);
    Dec(mem^.total_space_allocated, space_freed);
    lhdr_ptr := next_lhdr_ptr;
  end;

  { Release small objects }
  shdr_ptr := mem^.small_list[pool_id];
  mem^.small_list[pool_id] := NIL;

  while (shdr_ptr <> NIL) do
  begin
    next_shdr_ptr := shdr_ptr^.hdr.next;
    space_freed := shdr_ptr^.hdr.bytes_used +
                  shdr_ptr^.hdr.bytes_left +
                  SIZEOF(small_pool_hdr);
    jpeg_free_small(cinfo, pointer(shdr_ptr), space_freed);
    Dec(mem^.total_space_allocated, space_freed);
    shdr_ptr := next_shdr_ptr;
  end;
end;


{ Close up shop entirely.
  Note that this cannot be called unless cinfo^.mem is non-NIL. }

{METHODDEF}
procedure self_destruct (cinfo : j_common_ptr); far;
var
  pool : int;
begin
  { Close all backing store, release all memory.
    Releasing pools in reverse order might help avoid fragmentation
    with some (brain-damaged) malloc libraries. }

  for pool := JPOOL_NUMPOOLS-1 downto JPOOL_PERMANENT do
  begin
    free_pool(cinfo, pool);
  end;

  { Release the memory manager control block too. }
  jpeg_free_small(cinfo, pointer(cinfo^.mem), SIZEOF(my_memory_mgr));
  cinfo^.mem := NIL;            { ensures I will be called only once }

  jpeg_mem_term(cinfo);         { system-dependent cleanup }
end;


{ Memory manager initialization.
  When this is called, only the error manager pointer is valid in cinfo! }

{GLOBAL}
procedure jinit_memory_mgr (cinfo : j_common_ptr);
var
  mem : my_mem_ptr;
  max_to_use : long;
  pool : int;
  test_mac : size_t;
{$ifndef NO_GETENV}
var
  memenv : string;
  code : integer;
{$endif}
begin
  cinfo^.mem := NIL;            { for safety if init fails }

  { Check for configuration errors.
    SIZEOF(ALIGN_TYPE) should be a power of 2; otherwise, it probably
    doesn't reflect any real hardware alignment requirement.
    The test is a little tricky: for X>0, X and X-1 have no one-bits
    in common if and only if X is a power of 2, ie has only one one-bit.
    Some compilers may give an "unreachable code" warning here; ignore it. }
  if ((SIZEOF(ALIGN_TYPE) and (SIZEOF(ALIGN_TYPE)-1)) <> 0) then
    ERREXIT(cinfo, JERR_BAD_ALIGN_TYPE);
  { MAX_ALLOC_CHUNK must be representable as type size_t, and must be
    a multiple of SIZEOF(ALIGN_TYPE).
    Again, an "unreachable code" warning may be ignored here.
    But a "constant too large" warning means you need to fix MAX_ALLOC_CHUNK. }

  test_mac := size_t (MAX_ALLOC_CHUNK);
  if (long (test_mac) <> MAX_ALLOC_CHUNK) or
      ((MAX_ALLOC_CHUNK mod SIZEOF(ALIGN_TYPE)) <> 0) then
    ERREXIT(cinfo, JERR_BAD_ALLOC_CHUNK);

  max_to_use := jpeg_mem_init(cinfo); { system-dependent initialization }

  { Attempt to allocate memory manager's control block }
  mem := my_mem_ptr (jpeg_get_small(cinfo, SIZEOF(my_memory_mgr)));

  if (mem = NIL) then
  begin
    jpeg_mem_term(cinfo);       { system-dependent cleanup }
    ERREXIT1(cinfo, JERR_OUT_OF_MEMORY, 0);
  end;

  { OK, fill in the method pointers }
  mem^.pub.alloc_small := alloc_small;
  mem^.pub.alloc_large := alloc_large;
  mem^.pub.alloc_sarray := alloc_sarray;
  mem^.pub.alloc_barray := alloc_barray;
  mem^.pub.request_virt_sarray := request_virt_sarray;
  mem^.pub.request_virt_barray := request_virt_barray;
  mem^.pub.realize_virt_arrays := realize_virt_arrays;
  mem^.pub.access_virt_sarray := access_virt_sarray;
  mem^.pub.access_virt_barray := access_virt_barray;
  mem^.pub.free_pool := free_pool;
  mem^.pub.self_destruct := self_destruct;

  { Make MAX_ALLOC_CHUNK accessible to other modules }
  mem^.pub.max_alloc_chunk := MAX_ALLOC_CHUNK;

  { Initialize working state }
  mem^.pub.max_memory_to_use := max_to_use;

  for pool := JPOOL_NUMPOOLS-1 downto JPOOL_PERMANENT do
  begin
    mem^.small_list[pool] := NIL;
    mem^.large_list[pool] := NIL;
  end;
  mem^.virt_sarray_list := NIL;
  mem^.virt_barray_list := NIL;

  mem^.total_space_allocated := SIZEOF(my_memory_mgr);

  { Declare ourselves open for business }
  cinfo^.mem := @mem^.pub;

  { Check for an environment variable JPEGMEM; if found, override the
    default max_memory setting from jpeg_mem_init.  Note that the
    surrounding application may again override this value.
    If your system doesn't support getenv(), define NO_GETENV to disable
    this feature. }

{$ifndef NO_GETENV}
  memenv := getenv('JPEGMEM');
  if (memenv <> '') then
  begin
    Val(memenv, max_to_use, code);
    if (Code = 0) then
    begin
      max_to_use := max_to_use * long(1000);
      mem^.pub.max_memory_to_use := max_to_use * long(1000);
    end;
  end;
{$endif}

end;

end.
