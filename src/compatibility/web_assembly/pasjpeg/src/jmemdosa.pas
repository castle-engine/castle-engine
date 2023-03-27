Unit jmemdosa;

{$G+} {enable 286/287 instructions }

{ Original: jmemdosa.asm ; Copyright (C) 1992, Thomas G. Lane.
            Based on code contributed by Ge' Weijers.  }

{ This file contains low-level interface routines to support the MS-DOS
  backing store manager (jmemdos.c).  Routines are provided to access disk
  files through direct DOS calls, and to access XMS and EMS drivers. }

interface

uses
  jmorecfg;

type
  XMSDRIVER = pointer; {far}    { actually a pointer to code }
type
  XMScontext = packed record    { registers for calling XMS driver }
    ax, dx, bx : ushort;
    ds_si : pointer; {far}
  end;
type
  EMScontext = packed record    { registers for calling EMS driver }
    ax, dx, bx : ushort;
    ds_si : pointer; {far}
  end;
{ offset is a reserved word in BASM }

function jdos_open (var handle : short {far}; const filename {: PChar}) : short;

function jdos_close (handle : short) : short;

function jdos_seek (handle : short; offs : long) : short;

function jdos_read (handle : short; buffer : pointer; {FAR}
                                count : ushort) : short;
function jdos_write (handle : short; buffer : pointer; {FAR}
                     count : ushort) : short;

procedure jxms_getdriver (var driver : XMSDRIVER);

procedure jxms_calldriver (driver : XMSDRIVER;
                           var ctx : XMScontext);
function jems_available : short;

procedure jems_calldriver (var ctx : EMScontext);


implementation


function jdos_open (var handle : short {far};
                    const filename {: PChar}) : short; assembler;
{ Create and open a temporary file }
label
  open_err;
asm
        push    si                      { save all registers for safety }
        push    di
        push    bx
        push    cx
        push    dx
        push    es
        push    ds
        mov     cx,0                    { normal file attributes }
        lds     dx, filename            { get filename pointer }
        mov     ah,3ch                  { create file }
        int     21h
        jc      open_err                { if failed, return error code }
        lds     bx, handle              { get handle pointer }
        mov     word ptr [bx],ax        { save the handle }
        xor     ax,ax                   { return zero for OK }
open_err:
        pop     ds                      { restore registers and exit }
        pop     es
        pop     dx
        pop     cx
        pop     bx
        pop     di
        pop     si
end; { jdos_open }


function jdos_close (handle : short) : short; assembler;
{ Close the file handle }
label
  close_err;
asm
        push    si                      { save all registers for safety }
        push    di
        push    bx
        push    cx
        push    dx
        push    es
        push    ds
        mov     bx, handle              { file handle }
        mov     ah,3eh                  { close file }
        int     21h
        jc      close_err               { if failed, return error code }
        xor     ax,ax                   { return zero for OK }
close_err:
        pop     ds                      { restore registers and exit }
        pop     es
        pop     dx
        pop     cx
        pop     bx
        pop     di
        pop     si
end; { jdos_close }



function jdos_seek (handle : short; offs : long) : short; assembler;
{ Set file position }
label
  seek_err;
asm
        push    si                      { save all registers for safety }
        push    di
        push    bx
        push    cx
        push    dx
        push    es
        push    ds
        mov     bx, handle              { file handle }
        mov     dx, offs.word           { LS offset }
        mov     cx, offs.word[2]        { MS offset }
        mov     ax,4200h                { absolute seek }
        int     21h
        jc      seek_err                { if failed, return error code }
        xor     ax,ax                   { return zero for OK }
seek_err:
        pop     ds                      { restore registers and exit }
        pop     es
        pop     dx
        pop     cx
        pop     bx
        pop     di
        pop     si
end; { jdos_seek }


function jdos_read (handle : short; buffer : pointer; {FAR}
                                    count : ushort) : short; assembler;
{ Read from file }
label
  read_ok, read_err;
asm
        push    si                      { save all registers for safety }
        push    di
        push    bx
        push    cx
        push    dx
        push    es
        push    ds
        mov     bx, handle              { file handle }
        lds     dx, buffer              { buffer address }
        mov     cx, count               { number of bytes }
        mov     ah,3fh                  { read file }
        int     21h
        jc      read_err                { if failed, return error code }
        cmp     ax, count               { make sure all bytes were read }
        je      read_ok
        mov     ax,1                    { else return 1 for not OK }
        jmp     read_err
read_ok:
        xor     ax,ax                   { return zero for OK }
read_err:
        pop     ds                      { restore registers and exit }
        pop     es
        pop     dx
        pop     cx
        pop     bx
        pop     di
        pop     si
end; { jdos_read }



function jdos_write (handle : short; buffer : pointer; {FAR}
                                     count : ushort) : short;  assembler;
{ Write to file }
label
  write_ok, write_err;
asm
        push    si                      { save all registers for safety }
        push    di
        push    bx
        push    cx
        push    dx
        push    es
        push    ds
        mov     bx, handle              { file handle }
        lds     dx, buffer              { buffer address }
        mov     cx, count               { number of bytes }
        mov     ah,40h                  { write file }
        int     21h
        jc      write_err               { if failed, return error code }
        cmp     ax, count               { make sure all bytes written }
        je      write_ok
        mov     ax,1                    { else return 1 for not OK }
        jmp     write_err
write_ok:
        xor     ax,ax                   { return zero for OK }
write_err:
        pop     ds                      { restore registers and exit }
        pop     es
        pop     dx
        pop     cx
        pop     bx
        pop     di
        pop     si
end; { jdos_write }



procedure jxms_getdriver (var driver : XMSDRIVER); assembler;
{ Get the address of the XMS driver, or NIL if not available }
label
  xmsavail, xmsavail_done;
asm
        push    si                      { save all registers for safety }
        push    di
        push    bx
        push    cx
        push    dx
        push    es
        push    ds
        mov     ax,4300h                { call multiplex interrupt with }
        int     2fh                     { a magic cookie, hex 4300 }
        cmp     al,80h                  { AL should contain hex 80 }
        je      xmsavail
        xor     dx,dx                   { no XMS driver available }
        xor     ax,ax                   { return a nil pointer }
        jmp     xmsavail_done
xmsavail:
        mov     ax,4310h                { fetch driver address with }
        int     2fh                     { another magic cookie }
        mov     dx,es                   { copy address to dx:ax }
        mov     ax,bx
xmsavail_done:
        les     bx,dword ptr [bp+6]     { get pointer to return value }
        mov     word ptr es:[bx],ax
        mov     word ptr es:[bx+2],dx
        pop     ds                      { restore registers and exit }
        pop     es
        pop     dx
        pop     cx
        pop     bx
        pop     di
        pop     si
end; { jxms_getdriver }

procedure jxms_calldriver (driver : XMSDRIVER;
                           var ctx : XMScontext); assembler;
{ The XMScontext structure contains values for the AX,DX,BX,SI,DS registers.}
{ These are loaded, the XMS call is performed, and the new values of the }
{ AX,DX,BX registers are written back to the context structure. }
asm
        push    si                      { save all registers for safety }
        push    di
        push    bx
        push    cx
        push    dx
        push    es
        push    ds
        les     bx, ctx                 { get XMScontext pointer }
        mov     ax,word ptr es:[bx]     { load registers }
        mov     dx,word ptr es:[bx+2]
        mov     si,word ptr es:[bx+6]
        mov     ds,word ptr es:[bx+8]
        mov     bx,word ptr es:[bx+4]
        call    dword ptr driver        { call the driver }
        mov     cx,bx                   { save returned BX for a sec }
        les     bx, ctx                 { get XMScontext pointer }
        mov     word ptr es:[bx],ax     { put back ax,dx,bx }
        mov     word ptr es:[bx+2],dx
        mov     word ptr es:[bx+4],cx
        pop     ds                      { restore registers and exit }
        pop     es
        pop     dx
        pop     cx
        pop     bx
        pop     di
        pop     si
end; { jxms_calldriver }



function jems_available : short; assembler;
{ Have we got an EMS driver? (this comes straight from the EMS 4.0 specs)}
label
  no_ems, avail_done;
const
  ASCII_device_name : packed array[0..7] of char  = 'EMMXXXX0';
asm
        push    si                      { save all registers for safety }
        push    di
        push    bx
        push    cx
        push    dx
        push    es
        push    ds
        mov     ax,3567h                { get interrupt vector 67h }
        int     21h
        push    cs
        pop     ds
        mov     di,000ah                { check offs 10 in returned seg }
        lea     si, ASCII_device_name   { against literal string }
        mov     cx,8
        cld
        repe cmpsb
        jne     no_ems
        mov     ax,1                    { match, it's there }
        jmp     avail_done
no_ems: xor     ax,ax                   { it's not there }
avail_done:
        pop     ds                      { restore registers and exit }
        pop     es
        pop     dx
        pop     cx
        pop     bx
        pop     di
        pop     si
end; { jems_available }


procedure jems_calldriver (var ctx : EMScontext); assembler;
{ The EMScontext structure contains values for the AX,DX,BX,SI,DS registers. }
{ These are loaded, the EMS trap is performed, and the new values of the }
{ AX,DX,BX registers are written back to the context structure. }
asm
        push    si                      { save all registers for safety }
        push    di
        push    bx
        push    cx
        push    dx
        push    es
        push    ds

        les     bx, ctx                 { get EMScontext pointer }
        mov     ax, es:[bx].EMScontext.&ax      { load registers }
        mov     dx, es:[bx].EMScontext.&dx
        mov     si, es:[bx].EMScontext.&ds_si.word
        mov     ds, es:[bx].EMScontext.&ds_si.word[2]
        mov     bx, es:[bx].EMScontext.&bx
        int     67h                     { call the EMS driver }
        mov     cx,bx                   { save returned BX for a sec }
        les     bx, ctx                 { get EMScontext pointer }
        mov     es:[bx].EMScontext.&ax, ax     { put back ax,dx,bx }
        mov     es:[bx].EMScontext.&dx, dx
        mov     es:[bx].EMScontext.&bx, cx

        pop     ds                      { restore registers and exit }
        pop     es
        pop     dx
        pop     cx
        pop     bx
        pop     di
        pop     si
end; { jems_calldriver }

end.
