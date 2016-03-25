{ Minimal translation of C /usr/include/ogg/ogg.h header.

  Renamed to CastleOgg, to avoid name clash with FPC's Ogg unit.
  In the future, this may be removed in favor of simply using
  FPC's Ogg unit. Although, from the 1st glance, it will require
  fixing FPC's Ogg unit --- it will fail when CastleOgg lib is not available,
  while my CastleOgg and CastleVorbisFile units are made such that they can continue
  (function pointers are loaded in initialization and failure only
  casues setting some bool variable to @false).
  Actually, CastleOgg unit doesn't link at all to ogg library (it's not needed,
  only structs are needed).

  @exclude (Unit not really ready for PasDoc, with many comments from
  original C headers.) }
unit CastleOgg;

{$I castleconf.inc}
{$packrecords C}

interface

uses CTypes;

type
  Toggpack_buffer = record
    endbyte: Clong;
    endbit: Cint;
    buffer: PChar;
    ptr: PChar;
    storage: Clong;
  end;

(*
typedef struct {
  long endbyte;
  int  endbit;

  unsigned char *buffer;
  unsigned char *ptr;
  long storage;
} oggpack_buffer;

/* ogg_page is used to encapsulate the data in one Ogg bitstream page *****/

typedef struct {
  unsigned char *header;
  long header_len;
  unsigned char *body;
  long body_len;
} ogg_page;
*)

{* ogg_stream_state contains the current encode/decode state of a logical
   Ogg bitstream **********************************************************}

  Togg_stream_state = record
    body_data: PChar;          (* bytes from packet bodies *)
    body_storage: Clong;        (* storage elements allocated *)
    body_fill: Clong;           (* elements stored; fill mark *)
    body_returned: Clong;       (* elements of fill returned *)

    lacing_vals: Pointer;                (* The values that will go to the segment table *)
    granule_vals: PInt64;  (* granulepos values for headers. Not compact
                                                this way, but it is simple coupled to the
                                                lacing fifo *)
    lacing_storage: Clong;
    lacing_fill: Clong;
    lacing_packet: Clong;
    lacing_returned: Clong;

    header: array[0..282] of Char;   (* working space for header encode *)
    header_fill: Cint;
    e_o_s: Cint;                      (* set when we have buffered the last packet in the
                                        logical bitstream *)
    b_o_s: Cint;                      (* set after we've written the initial page
                                        of a logical bitstream *)
    serialno: Clong;
    pageno: Clong;
    packetno: Int64;           (* sequence number for decode; the framing
                                        knows where there's a hole in the data,
                                        but we need coupling so that the codec
                                        (which is in a seperate abstraction
                                        layer) also knows about the gap *)
    granulepos: Int64;
  end;

(*
/* ogg_packet is used to encapsulate the data and metadata belonging
   to a single raw Ogg/Vorbis packet *************************************/

typedef struct {
  unsigned char *packet;
  long  bytes;
  long  b_o_s;
  long  e_o_s;

  ogg_int64_t  granulepos;

  ogg_int64_t  packetno;     /* sequence number for decode; the framing
				knows where there's a hole in the data,
				but we need coupling so that the codec
				(which is in a seperate abstraction
				layer) also knows about the gap */
} ogg_packet;
*)

  Togg_sync_state = record
    data: PChar;
    storage: CInt;
    fill: CInt;
    returned: CInt;

    unsynced: CInt;
    headerbytes: CInt;
    bodybytes: CInt;
  end;

(*
/* Ogg BITSTREAM PRIMITIVES: bitstream ************************/

extern void  oggpack_writeinit(oggpack_buffer *b);
extern void  oggpack_writetrunc(oggpack_buffer *b,long bits);
extern void  oggpack_writealign(oggpack_buffer *b);
extern void  oggpack_writecopy(oggpack_buffer *b,void *source,long bits);
extern void  oggpack_reset(oggpack_buffer *b);
extern void  oggpack_writeclear(oggpack_buffer *b);
extern void  oggpack_readinit(oggpack_buffer *b,unsigned char *buf,int bytes);
extern void  oggpack_write(oggpack_buffer *b,unsigned long value,int bits);
extern long  oggpack_look(oggpack_buffer *b,int bits);
extern long  oggpack_look1(oggpack_buffer *b);
extern void  oggpack_adv(oggpack_buffer *b,int bits);
extern void  oggpack_adv1(oggpack_buffer *b);
extern long  oggpack_read(oggpack_buffer *b,int bits);
extern long  oggpack_read1(oggpack_buffer *b);
extern long  oggpack_bytes(oggpack_buffer *b);
extern long  oggpack_bits(oggpack_buffer *b);
extern unsigned char *oggpack_get_buffer(oggpack_buffer *b);

extern void  oggpackB_writeinit(oggpack_buffer *b);
extern void  oggpackB_writetrunc(oggpack_buffer *b,long bits);
extern void  oggpackB_writealign(oggpack_buffer *b);
extern void  oggpackB_writecopy(oggpack_buffer *b,void *source,long bits);
extern void  oggpackB_reset(oggpack_buffer *b);
extern void  oggpackB_writeclear(oggpack_buffer *b);
extern void  oggpackB_readinit(oggpack_buffer *b,unsigned char *buf,int bytes);
extern void  oggpackB_write(oggpack_buffer *b,unsigned long value,int bits);
extern long  oggpackB_look(oggpack_buffer *b,int bits);
extern long  oggpackB_look1(oggpack_buffer *b);
extern void  oggpackB_adv(oggpack_buffer *b,int bits);
extern void  oggpackB_adv1(oggpack_buffer *b);
extern long  oggpackB_read(oggpack_buffer *b,int bits);
extern long  oggpackB_read1(oggpack_buffer *b);
extern long  oggpackB_bytes(oggpack_buffer *b);
extern long  oggpackB_bits(oggpack_buffer *b);
extern unsigned char *oggpackB_get_buffer(oggpack_buffer *b);

/* Ogg BITSTREAM PRIMITIVES: encoding **************************/

extern int      ogg_stream_packetin(ogg_stream_state *os, ogg_packet *op);
extern int      ogg_stream_pageout(ogg_stream_state *os, ogg_page *og);
extern int      ogg_stream_flush(ogg_stream_state *os, ogg_page *og);

/* Ogg BITSTREAM PRIMITIVES: decoding **************************/

extern int      ogg_sync_init(ogg_sync_state *oy);
extern int      ogg_sync_clear(ogg_sync_state *oy);
extern int      ogg_sync_reset(ogg_sync_state *oy);
extern int	ogg_sync_destroy(ogg_sync_state *oy);

extern char    *ogg_sync_buffer(ogg_sync_state *oy, long size);
extern int      ogg_sync_wrote(ogg_sync_state *oy, long bytes);
extern long     ogg_sync_pageseek(ogg_sync_state *oy,ogg_page *og);
extern int      ogg_sync_pageout(ogg_sync_state *oy, ogg_page *og);
extern int      ogg_stream_pagein(ogg_stream_state *os, ogg_page *og);
extern int      ogg_stream_packetout(ogg_stream_state *os,ogg_packet *op);
extern int      ogg_stream_packetpeek(ogg_stream_state *os,ogg_packet *op);

/* Ogg BITSTREAM PRIMITIVES: general ***************************/

extern int      ogg_stream_init(ogg_stream_state *os,int serialno);
extern int      ogg_stream_clear(ogg_stream_state *os);
extern int      ogg_stream_reset(ogg_stream_state *os);
extern int      ogg_stream_reset_serialno(ogg_stream_state *os,int serialno);
extern int      ogg_stream_destroy(ogg_stream_state *os);
extern int      ogg_stream_eos(ogg_stream_state *os);

extern void     ogg_page_checksum_set(ogg_page *og);

extern int      ogg_page_version(ogg_page *og);
extern int      ogg_page_continued(ogg_page *og);
extern int      ogg_page_bos(ogg_page *og);
extern int      ogg_page_eos(ogg_page *og);
extern ogg_int64_t  ogg_page_granulepos(ogg_page *og);
extern int      ogg_page_serialno(ogg_page *og);
extern long     ogg_page_pageno(ogg_page *og);
extern int      ogg_page_packets(ogg_page *og);

extern void     ogg_packet_clear(ogg_packet *op);
*)

implementation

end.
