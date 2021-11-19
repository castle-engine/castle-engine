unit LibDelphi;

{$ifdef FPC}
  {$MODE OBJFPC}
{$endif}

interface

uses
  SysUtils;

type
  va_list = Pointer;

{$IFNDEF FPC}
{$IF CompilerVersion <= 18.5}
  SizeInt = Integer;
  PtrUInt = Cardinal;
{$ELSE}
  SizeInt = NativeInt;
  PtrUInt = NativeUInt;
{$IFEND}
{$ENDIF}

const
{$IFDEF MSWINDOWS}
  SRuntimeLib = 'msvcrt.dll';
{$ELSE}
  SRuntimeLib = 'libc.so';
{$ENDIF}

function  fprintf(stream: Pointer; format: Pointer; arguments: va_list): Integer; cdecl; {$ifdef FPC}[public];{$endif}
function  sprintf(buffer: Pointer; format: Pointer; arguments: va_list): Integer; cdecl; {$ifdef FPC}[public];{$endif}
function  snprintf(buffer: Pointer; n: Integer; format: Pointer; arguments: va_list): Integer; cdecl; {$ifdef FPC}[public];{$endif}
function  fputs(s: Pointer; stream: Pointer): Integer; cdecl; external SRuntimeLib;
function  fputc(c: Integer; stream: Pointer): Integer; cdecl; external SRuntimeLib;
function  isprint(c: Integer): Integer; cdecl; external SRuntimeLib;
procedure memset(a: Pointer; b: Integer; c: SizeInt); cdecl; {$ifdef FPC}[public];{$endif}
function  memcpy(dest: Pointer; const src: Pointer; count: SizeInt): Pointer; cdecl; {$ifdef FPC}[public];{$endif}
function  memcmp(a, b: Pointer; c:SizeInt):Integer; cdecl; {$ifdef FPC}[public];{$endif}
function  malloc(s: Longint): Pointer; cdecl; {$ifdef FPC}[public];{$endif}
procedure free(p: Pointer); cdecl; {$ifdef FPC}[public];{$endif}
{$ifndef FPC}
function  _ftol: Integer; cdecl; external SRuntimeLib;
function  _ltolower(ch: Integer): Integer; cdecl; external SRuntimeLib;
function  _ltoupper(ch: Integer): Integer; cdecl; external SRuntimeLib;
function  _ltowlower(ch: Integer): Integer; cdecl; external SRuntimeLib;
function  _ltowupper(ch: Integer): Integer; cdecl; external SRuntimeLib;
function fwrite(ptr:pointer; size, count:SizeInt; stream:pointer ):SizeInt; cdecl; external SRuntimeLib;
{$endif}
function strcpy(dest: Pointer; src: Pointer): Pointer; cdecl; {$ifdef FPC}[public];{$endif}

{$ifdef FPC}
function fwrite(ptr:pointer; size, count:SizeInt; stream:pointer ):SizeInt; cdecl; {$ifdef FPC}[public];{$endif}
function __udivdi3(a,b:int64):int64; cdecl; [public];
function {$ifdef CPUX86}_imp__isprint{$else}__imp_isprint{$endif}(c: char): integer; cdecl; [public];
{$endif}

{$ifndef FPC}
var
  __turboFloat: LongBool = False;
  _streams: Integer;

{$else}
type
  // from mingw - stdio.h
  cIoBuf = record
    _ptr:Pointer;
    _cnt:LongInt;
    _base:Pointer;
    _flag:LongInt;
    _file:LongInt;
    _charbuf:LongInt;
    _bufsiz:LongInt;
    _tmpfname:Pointer;
  end;
  pIoBuf = ^cIoBuf;

var
  _imp___iob:array[0..2] of cIoBuf; cvar; // stdin,stdout,stderr
  iob:pIoBuf; cvar;
{$endif}

implementation

{$ifndef FPC}
uses
  Windows;
{$endif}

{$ifdef FPC}
function __udivdi3(a, b: int64): int64; cdecl;
begin
  Result:=a div b;
end;
function {$ifdef CPUX86}_imp__isprint{$else}__imp_isprint{$endif}(c: char): integer; cdecl;
begin
  if (c>=#32)and(c<=#127) then
    Result:=1
    else
      Result:=0;
end;
{$endif}

procedure free(p: Pointer); cdecl;
begin
  FreeMem(p);
end;

function malloc(s: Longint): Pointer; cdecl;
begin
  Result := AllocMem(s);
end;

function memcpy(dest: Pointer; const src: Pointer; count: SizeInt): Pointer; cdecl;
begin
  system.Move(src^,dest^,count);
  Result:=dest;
end;

procedure memset(a: Pointer; b: Integer; c: SizeInt); cdecl;
begin
  system.FillChar(a^,c,b);
end;

function memcmp(a, b: Pointer; c: SizeInt): Integer; cdecl; {$ifdef FPC}[public];{$endif}
{$ifndef FPC}
var
  ma,mb: PByte;
  n: Integer;
begin
  ma:=a;
  mb:=b;
  n:=0;
  while Cardinal(n)<c do
  begin
    if ma^<>mb^ then
    begin
      if ma^<mb^ then
        Result:=-1
      else
        Result:=1;
      exit;
    end;
    Inc(ma);
    Inc(mb);
    Inc(n);
  end;
  Result:=0;
{$else}
begin
  Result:=CompareMemRange(a,b,c);
{$endif}
end;

function __sprintf(buffer: Pointer; format: Pointer; arguments: Pointer): Integer; cdecl; external SRuntimeLib name 'sprintf';

function sprintf(buffer: Pointer; format: Pointer; arguments: Pointer): Integer; cdecl;
begin
  Result := __sprintf(buffer, format, arguments);
end;

function __snprintf(buffer: Pointer; n: Integer; format: Pointer; arguments: va_list): Integer; cdecl; external SRuntimeLib name '_snprintf';

function snprintf(buffer: Pointer; n: Integer; format: Pointer; arguments: va_list): Integer; cdecl;
begin
  Result := __snprintf(buffer, n, format, arguments);
end;

function fprintf(stream: Pointer; format: Pointer; arguments: va_list): Integer; cdecl;
var
  m: Integer;
  n: Pointer;
{$ifndef FPC}
  o: Cardinal;
{$endif}
begin
  m:=sprintf(nil,format,@arguments);
  n:=AllocMem(m);
  sprintf(n,format,@arguments);
  {$ifndef FPC}
  WriteFile(Cardinal(stream),n^,Cardinal(m),o,nil);
  {$else}
  FileWrite(pIoBuf(stream)^._file,n^,Cardinal(m));
  {$endif}
  FreeMem(n);
  Result := m;
end;

function strcpy(dest: Pointer; src: Pointer): Pointer; cdecl;
begin
  Result:=SysUtils.strcopy(PAnsiChar(dest),PAnsiChar(src));
end;

{$ifdef FPC}
function fwrite(ptr: pointer; size, count: SizeInt; stream: pointer): SizeInt; cdecl;
begin
  Result:=FileWrite(pIoBuf(stream)^._file,ptr^,size * count);
end;

procedure init_iob;
begin
  FillChar(_imp___iob[0],sizeof(cIoBuf)*3,0);
  _imp___iob[0]._file:=StdInputHandle;
  _imp___iob[1]._file:=StdOutputHandle;
  _imp___iob[2]._file:=StdErrorHandle;
  iob:=@_imp___iob[0];
end;

initialization
  init_iob;
{$endif}

end.
