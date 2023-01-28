{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 by Marco van de Voort, member of the
    Free Pascal development team

    Implements C types for in header conversions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


 **********************************************************************}

unit ctypes;

{$ifdef FPC}
  {$inline on}
  {$define dummy}
{$endif}

interface

{$ifdef unix}
uses unixtype;
{$i aliasctp.inc}
{$else}

type
{$ifndef FPC}
    qword = int64;  // Keep h2pas "uses ctypes" headers working with delphi.
    ptruint = cardinal;
    pptruint = ^ptruint;
{$endif}

  { the following type definitions are compiler dependant }
  { and system dependant                                  }

  cint8                  = shortint;           pcint8                 = ^cint8;
  cuint8                 = byte;               pcuint8                = ^cuint8;
  cchar                  = cint8;              pcchar                 = ^cchar;
  cschar                 = cint8;              pcschar                = ^cschar;
  cuchar                 = cuint8;             pcuchar                = ^cuchar;

  cint16                 = smallint;           pcint16                = ^cint16;
  cuint16                = word;               pcuint16               = ^cuint16;
  cshort                 = cint16;             pcshort                = ^cshort;
  csshort                = cint16;             pcsshort               = ^csshort;
  cushort                = cuint16;            pcushort               = ^cushort;

  cint32                 = longint;            pcint32                = ^cint32;
  cuint32                = longword;           pcuint32               = ^cuint32;

  cint64                 = int64;              pcint64                = ^cint64;
  cuint64                = qword;              pcuint64               = ^cuint64;
  clonglong              = cint64;             pclonglong             = ^clonglong;
  cslonglong             = cint64;             pcslonglong            = ^cslonglong;
  culonglong             = cuint64;            pculonglong            = ^culonglong;

  cbool                  = longbool;           pcbool                 = ^cbool;

{$if defined(cpu64) and not(defined(win64) and defined(cpux86_64))}
  cint                   = cint32;             pcint                  = ^cint;              { minimum range is : 32-bit    }
  csint                  = cint32;             pcsint                 = ^csint;             { minimum range is : 32-bit    }
  cuint                  = cuint32;            pcuint                 = ^cuint;             { minimum range is : 32-bit    }
  clong                  = int64;              pclong                 = ^clong;
  cslong                 = int64;              pcslong                = ^cslong;
  culong                 = qword;              pculong                = ^culong;
{$elseif defined(cpu16)}
  { 16-bit int sizes checked against Borland C++ 3.1 and Open Watcom 1.9 }
  cint                   = cint16;             pcint                  = ^cint;
  csint                  = cint16;             pcsint                 = ^csint;
  cuint                  = cuint16;            pcuint                 = ^cuint;
  clong                  = longint;            pclong                 = ^clong;
  cslong                 = longint;            pcslong                = ^cslong;
  culong                 = cardinal;           pculong                = ^culong;
{$else}
  cint                   = cint32;             pcint                  = ^cint;              { minimum range is : 32-bit    }
  csint                  = cint32;             pcsint                 = ^csint;             { minimum range is : 32-bit    }
  cuint                  = cuint32;            pcuint                 = ^cuint;             { minimum range is : 32-bit    }
  clong                  = longint;            pclong                 = ^clong;
  cslong                 = longint;            pcslong                = ^cslong;
  culong                 = cardinal;           pculong                = ^culong;
{$ifend}

  csigned                = cint;               pcsigned               = ^csigned;
  cunsigned              = cuint;              pcunsigned             = ^cunsigned;

  csize_t                = ptruint;            pcsize_t               = pptruint;

// Kylix compat types
  u_long  = culong;
  u_short = cushort;
  coff_t = clong;

{$ifndef FPUNONE}
  cfloat                 = single;             pcfloat                = ^cfloat;
  cdouble                = double;             pcdouble               = ^cdouble;
{$endif}
{$endif}

{$if defined(win64) or defined(wince) or defined(android)}
  {$define longdouble_is_double}
{$endif}

{$if defined(linux) and (defined(cpupowerpc) or defined(cpuarm))}
  {$define longdouble_is_double}
{$ifend}

{$if defined(darwin) and defined(cpuaarch64)}
  {$define longdouble_is_double}
{$ifend}

{$ifndef FPUNONE}
{$if defined(longdouble_is_double) or not defined(FPC_HAS_CEXTENDED)}
  clongdouble=double;
{$else}
  {$if defined(cpui8086) or defined(cpui386) or defined(cpux86_64) or defined(cpuavr)}
  clongdouble = cextended;
  {$else}
  {$define longdouble_assignment_overload_real128}
  clongdouble = packed array [0..15] of byte;
  {$ifend}
{$ifend}
  Pclongdouble=^clongdouble;

{$ifdef longdouble_assignment_overload_real128}
{Non-x86 typically doesn't have extended. To be fixed once this changes.}
operator := (const v:clongdouble) r:double;inline;
operator := (const v:double) r:clongdouble;inline;
{$ifdef dummy}
operator +(const e:Double;const c:clongdouble) r:Double;inline;
operator +(const c:clongdouble;const e:Double) r:Double;inline;
operator -(const e:Double;const c:clongdouble) r:Double;inline;
operator -(const c:clongdouble;const e:Double) r:Double;inline;
operator *(const e:Double;const c:clongdouble) r:Double;inline;
operator *(const c:clongdouble;const e:Double) r:Double;inline;
operator /(const e:Double;const c:clongdouble) r:Double;inline;
operator /(const c:clongdouble;const e:Double) r:Double;inline;
operator =(const e:Double;const c:clongdouble) r:boolean;inline;
operator =(const c:clongdouble;const e:Double) r:boolean;inline;
operator <(const e:Double;const c:clongdouble) r:boolean;inline;
operator <(const c:clongdouble;const e:Double) r:boolean;inline;
operator >(const e:Double;const c:clongdouble) r:boolean;inline;
operator >(const c:clongdouble;const e:Double) r:boolean;inline;
operator >=(const e:Double;const c:clongdouble) r:boolean;inline;
operator >=(const c:clongdouble;const e:Double) r:boolean;inline;
operator <=(const e:Double;const c:clongdouble) r:boolean;inline;
operator <=(const c:clongdouble;const e:Double) r:boolean;inline;
{$endif dummy}
{$endif}
{$endif FPUNONE}

implementation

{$ifndef FPUNONE}

{$ifdef longdouble_assignment_overload_real128}

{$ifdef ENDIAN_LITTLE}
const r128_mantissa_ofs=0;
      r128_exponent_ofs=14;
{$else}
const r128_mantissa_ofs=2;
      r128_exponent_ofs=0;
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
  {$define USE_UNALIGNED}
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
{$endif}

operator := (const v:clongdouble) r:double;
var
  exp : word;
  mant : qword;
  is_neg : boolean;
begin
  is_neg:=(pword(@v[r128_exponent_ofs])^and $8000)<>0;
  exp:=((Pword(@v[r128_exponent_ofs])^and $7fff)-$4000)+$400;
  if is_neg then
    exp:=exp+$800;
{$ifdef USE_UNALIGNED}
  mant:=unaligned(Pqword(@v[r128_mantissa_ofs])^);
{$else not USE_UNALIGNED}
  mant:=Pqword(@v[r128_mantissa_ofs])^;
{$endif not USE_UNALIGNED}
  qword(r):=(qword(exp) shl 52) or
            (mant shr 12);
end;

operator := (const v:double) r:clongdouble;
var
  is_neg : boolean;
   exp : word;
begin
  is_neg:=(qword(v) shr 63) <> 0;
  exp:=$4000 + ((qword(v) shr 52) and $7ff) -$400;
  if is_neg then
    exp:=exp+$8000;
  Pword(@r[r128_exponent_ofs])^:=exp;
{$ifdef USE_UNALIGNED}
  unaligned(Pqword(@r[r128_mantissa_ofs])^):=qword(v) shl 12;
  Pword(@r[r128_mantissa_ofs+8])^:=0;
  Pword(@r[r128_mantissa_ofs+10])^:=0;
{$else not USE_UNALIGNED}
  Pqword(@r[r128_mantissa_ofs])^:=qword(v) shl 12;
  Pcardinal(@r[r128_mantissa_ofs+8])^:=0;
{$endif not USE_UNALIGNED}
  Pword(@r[r128_mantissa_ofs+12])^:=0;
end;

{$ifdef dummy}

// There is no record with a value field in this case

operator +(const e:Double;const c:clongdouble) r:Double;inline;
begin
  r:=e+double(c);
end;

operator +(const c:clongdouble;const e:Double) r:Double;inline;
begin
  r:=double(c)+e;
end;

operator -(const e:Double;const c:clongdouble) r:Double;inline;
begin
  r:=e-double(c);
end;

operator -(const c:clongdouble;const e:Double) r:Double;inline;
begin
  r:=double(c)-e;
end;

operator *(const e:Double;const c:clongdouble) r:Double;inline;
begin
  r:=e*double(c);
end;

operator *(const c:clongdouble;const e:Double) r:Double;inline;
begin
  r:=double(c)*e;
end;

operator /(const e:Double;const c:clongdouble) r:Double;inline;
begin
  r:=e/double(c);
end;

operator /(const c:clongdouble;const e:Double) r:Double;inline;
begin
  r:=double(c)/e;
end;

operator =(const e:Double;const c:clongdouble) r:boolean;inline;
begin
  r:=e=double(c);
end;

operator =(const c:clongdouble;const e:Double) r:boolean;inline;
begin
  r:=double(c)=e;
end;

operator <(const e:Double;const c:clongdouble) r:boolean;inline;
begin
  r:=e<double(c);
end;

operator <(const c:clongdouble;const e:Double) r:boolean;inline;
begin
  r:=double(c)<e;
end;

operator >(const e:Double;const c:clongdouble) r:boolean;inline;
begin
  r:=e>double(c);
end;

operator >(const c:clongdouble;const e:Double) r:boolean;inline;
begin
  r:=double(c)>e;
end;

operator >=(const e:Double;const c:clongdouble) r:boolean;inline;
begin
  r:=e>=double(c);
end;

operator >=(const c:clongdouble;const e:Double) r:boolean;inline;
begin
  r:=double(c)>=e;
end;

operator <=(const e:Double;const c:clongdouble) r:boolean;inline;
begin
  r:=e<=double(c);
end;

operator <=(const c:clongdouble;const e:Double) r:boolean;inline;
begin
  r:=double(c)<=e;
end;
{$endif}
{$endif}
{$endif FPUNONE}

end.
