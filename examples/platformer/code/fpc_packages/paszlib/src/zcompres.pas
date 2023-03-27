Unit ZCompres;

{ compress.c -- compress a memory buffer
  Copyright (C) 1995-1998 Jean-loup Gailly.

  Pascal tranlastion
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}

interface

{$I zconf.inc}

uses
  zbase, zdeflate;

                        { utility functions }

{EXPORT}
function compress (dest : Pbyte;
                   var destLen : cardinal;
                   const source : array of Byte;
                   sourceLen : cardinal) : integer;

 { Compresses the source buffer into the destination buffer.  sourceLen is
   the byte length of the source buffer. Upon entry, destLen is the total
   size of the destination buffer, which must be at least 0.1% larger than
   sourceLen plus 12 bytes. Upon exit, destLen is the actual size of the
   compressed buffer.
     This function can be used to compress a whole file at once if the
   input file is mmap'ed.
     compress returns Z_OK if success, Z_MEM_ERROR if there was not
   enough memory, Z_BUF_ERROR if there was not enough room in the output
   buffer. }

{EXPORT}
function compress2 (dest : Pbyte;
                    var destLen : cardinal;
                    const source : array of byte;
                    sourceLen : cardinal;
                    level : integer) : integer;
{  Compresses the source buffer into the destination buffer. The level
   parameter has the same meaning as in deflateInit.  sourceLen is the byte
   length of the source buffer. Upon entry, destLen is the total size of the
   destination buffer, which must be at least 0.1% larger than sourceLen plus
   12 bytes. Upon exit, destLen is the actual size of the compressed buffer.

   compress2 returns Z_OK if success, Z_MEM_ERROR if there was not enough
   memory, Z_BUF_ERROR if there was not enough room in the output buffer,
   Z_STREAM_ERROR if the level parameter is invalid. }

implementation

{ ===========================================================================
}
function compress2 (dest : Pbyte;
                    var destLen : cardinal;
                    const source : array of byte;
                    sourceLen : cardinal;
                    level : integer) : integer;
var
  stream : z_stream;
  err : integer;
begin
  stream.next_in := Pbyte(@source);
  stream.avail_in := cardinal(sourceLen);
{$ifdef MAXSEG_64K}
  { Check for source > 64K on 16-bit machine: }
  if (cardinal(stream.avail_in) <> sourceLen) then
  begin
    compress2 := Z_BUF_ERROR;
    exit;
  end;
{$endif}
  stream.next_out := dest;
  stream.avail_out := cardinal(destLen);
  if (cardinal(stream.avail_out) <> destLen) then
  begin
    compress2 := Z_BUF_ERROR;
    exit;
  end;

  err := deflateInit(stream, level);
  if (err <> Z_OK) then
  begin
    compress2 := err;
    exit;
  end;

  err := deflate(stream, Z_FINISH);
  if (err <> Z_STREAM_END) then
  begin
    deflateEnd(stream);
    if err = Z_OK then
      compress2 := Z_BUF_ERROR
    else
      compress2 := err;
    exit;
  end;
  destLen := stream.total_out;

  err := deflateEnd(stream);
  compress2 := err;
end;

{ ===========================================================================
 }
function compress (dest : Pbyte;
                   var destLen : cardinal;
                   const source : array of Byte;
                   sourceLen : cardinal) : integer;
begin
  compress := compress2(dest, destLen, source, sourceLen, Z_DEFAULT_COMPRESSION);
end;


end.