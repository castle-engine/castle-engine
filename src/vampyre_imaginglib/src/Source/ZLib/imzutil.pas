Unit imzutil;

{
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}

interface

{$I imzconf.inc}

{ Type declarations }

type
  {Byte   = usigned char;  8 bits}
  Bytef  = byte;
  charf  = byte;

  int    = longint;
  intf   = int;
  uInt   = cardinal;     { 16 bits or more }
  uIntf  = uInt;

  Long   = longint;
  uLong  = Cardinal;
  uLongf = uLong;

  voidp  = pointer;
  voidpf = voidp;
  pBytef = ^Bytef;
  pIntf  = ^intf;
  puIntf = ^uIntf;
  puLong = ^uLongf;

{$IF Defined(FPC)}
  ptr2int = PtrUInt;
{$ELSEIF CompilerVersion >= 20}
  ptr2int = NativeUInt;
{$ELSE}
  ptr2int = Cardinal;
{$IFEND}
{ a pointer to integer casting is used to do pointer arithmetic. }

type
  zByteArray = array[0..(MaxInt div SizeOf(Bytef))-1] of Bytef;
  pzByteArray = ^zByteArray;
type
  zIntfArray = array[0..(MaxInt div SizeOf(Intf))-1] of Intf;
  pzIntfArray = ^zIntfArray;
type
  zuIntArray = array[0..(MaxInt div SizeOf(uInt))-1] of uInt;
  PuIntArray = ^zuIntArray;

{ Type declarations - only for deflate }

type
  uch  = Byte;
  uchf = uch; { FAR }
  ush  = Word;
  ushf = ush;
  ulg  = LongInt;

  unsigned = uInt;

  pcharf = ^charf;
  puchf = ^uchf;
  pushf = ^ushf;

type
  zuchfArray = zByteArray;
  puchfArray = ^zuchfArray;
type
  zushfArray = array[0..(MaxInt div SizeOf(ushf))-1] of ushf;
  pushfArray = ^zushfArray;

procedure zmemcpy(destp : pBytef; sourcep : pBytef; len : uInt);
function zmemcmp(s1p, s2p : pBytef; len : uInt) : int;
procedure zmemzero(destp : pBytef; len : uInt);
procedure zcfree(opaque : voidpf; ptr : voidpf);
function zcalloc (opaque : voidpf; items : uInt; size : uInt) : voidpf;

implementation

procedure zmemcpy(destp : pBytef; sourcep : pBytef; len : uInt);
begin
  Move(sourcep^, destp^, len);
end;

function zmemcmp(s1p, s2p : pBytef; len : uInt) : int;
var
  j : uInt;
  source,
  dest : pBytef;
begin
  source := s1p;
  dest := s2p;
  for j := 0 to pred(len) do
  begin
    if (source^ <> dest^) then
    begin
      zmemcmp := 2*Ord(source^ > dest^)-1;
      exit;
    end;
    Inc(source);
    Inc(dest);
  end;
  zmemcmp := 0;
end;

procedure zmemzero(destp : pBytef; len : uInt);
begin
  FillChar(destp^, len, 0);
end;

procedure zcfree(opaque : voidpf; ptr : voidpf);
{$ifdef Delphi16}
var
  Handle : THandle;
{$endif}
{$IFDEF FPC}
var
  memsize : uint;
{$ENDIF}
begin
  (*
  {$IFDEF DPMI}
  {h :=} GlobalFreePtr(ptr);
  {$ELSE}
    {$IFDEF CALL_DOS}
    dosFree(ptr);
    {$ELSE}
      {$ifdef HugeMem}
      FreeMemHuge(ptr);
      {$else}
        {$ifdef Delphi16}
        Handle := GlobalHandle(LH(ptr).H); { HiWord(LongInt(ptr)) }
        GlobalUnLock(Handle);
        GlobalFree(Handle);
        {$else}
          {$IFDEF FPC}
          Dec(puIntf(ptr));
          memsize := puIntf(ptr)^;
          FreeMem(ptr, memsize+SizeOf(uInt));
          {$ELSE}
          FreeMem(ptr);  { Delphi 2,3,4 }
          {$ENDIF}
        {$endif}
      {$endif}
    {$ENDIF}
  {$ENDIF}
  *)
  FreeMem(ptr);
end;

function zcalloc (opaque : voidpf; items : uInt; size : uInt) : voidpf;
var
  p : voidpf;
  memsize : uLong;
{$ifdef Delphi16}
  handle : THandle;
{$endif}
begin
  memsize := uLong(items) * size;
  (*
  { $IFDEF DPMI}
  p := GlobalAllocPtr(gmem_moveable, memsize);
  { $ELSE}
    { $IFDEF CALLDOS}
    p := dosAlloc(memsize);
    { $ELSE}
      {$ifdef HugeMem}
      GetMemHuge(p, memsize);
      { $else}
        { $ifdef Delphi16}
        Handle := GlobalAlloc(HeapAllocFlags, memsize);
        p := GlobalLock(Handle);
        { $else}
          { $IFDEF FPC}
          GetMem(p, memsize+SizeOf(uInt));
          puIntf(p)^:= memsize;
          Inc(puIntf(p));
          { $ELSE}
          GetMem(p, memsize);  { Delphi: p := AllocMem(memsize); }
          { $ENDIF}
        { $endif}
      { $endif}
    { $ENDIF}
  { $ENDIF}
  *)
  GetMem(p, memsize);
  zcalloc := p;
end;

end.

