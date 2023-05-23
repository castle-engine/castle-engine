(******************************************************************************
 *                                 PasGLTF                                    *
 ******************************************************************************
 *                          Version 2020-02-12-00-29                          *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2018-2020, Benjamin Rosseaux (benjamin@rosseaux.de)          *
 *                                                                            *
 * This software is provided 'as-is', without any express or implied          *
 * warranty. In no event will the authors be held liable for any damages      *
 * arising from the use of this software.                                     *
 *                                                                            *
 * Permission is granted to anyone to use this software for any purpose,      *
 * including commercial applications, and to alter it and redistribute it     *
 * freely, subject to the following restrictions:                             *
 *                                                                            *
 * 1. The origin of this software must not be misrepresented; you must not    *
 *    claim that you wrote the original software. If you use this software    *
 *    in a product, an acknowledgement in the product documentation would be  *
 *    appreciated but is not required.                                        *
 * 2. Altered source versions must be plainly marked as such, and must not be *
 *    misrepresented as being the original software.                          *
 * 3. This notice may not be removed or altered from any source distribution. *
 *                                                                            *
 ******************************************************************************
 *                  General guidelines for code contributors                  *
 *============================================================================*
 *                                                                            *
 * 1. Make sure you are legally allowed to make a contribution under the zlib *
 *    license.                                                                *
 * 2. The zlib license header goes at the top of each source file, with       *
 *    appropriate copyright notice.                                           *
 * 3. After a pull request, check the status of your pull request on          *
 *    http://github.com/BeRo1985/pasGLTF                                      *
 * 4. Write code which's compatible with newer modern Delphi versions and     *
 *    FreePascal >= 3.0.0                                                     *
 * 5. Don't use Delphi-only, FreePascal-only or Lazarus-only libraries/units, *
 *    but if needed, make it out-ifdef-able.                                  *
 * 6. No use of third-party libraries/units as possible, but if needed, make  *
 *    it out-ifdef-able.                                                      *
 * 7. Try to use const when possible.                                         *
 * 8. Make sure to comment out writeln, used while debugging.                 *
 * 9. Make sure the code compiles on 32-bit and 64-bit platforms (x86-32,     *
 *    x86-64, ARM, ARM64, etc.).                                              *
 *                                                                            *
 ******************************************************************************)
unit PasGLTF;
{$ifdef fpc}
 {$mode delphi}
 {$ifdef cpui386}
  {$define cpu386}
 {$endif}
 {$ifdef cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef cpuamd64}
  {$asmmode intel}
 {$endif}
 {$ifdef FPC_LITTLE_ENDIAN}
  {$define LITTLE_ENDIAN}
 {$else}
  {$ifdef FPC_BIG_ENDIAN}
   {$define BIG_ENDIAN}
  {$endif}
 {$endif}
 {-$pic off}
 {$ifdef fpc_has_internal_sar}
  {$define HasSAR}
 {$endif}
 {$ifdef FPC_HAS_TYPE_EXTENDED}
  {$define HAS_TYPE_EXTENDED}
 {$else}
  {$undef HAS_TYPE_EXTENDED}
 {$endif}
 {$ifdef FPC_HAS_TYPE_DOUBLE}
  {$define HAS_TYPE_DOUBLE}
 {$else}
  {$undef HAS_TYPE_DOUBLE}
 {$endif}
 {$ifdef FPC_HAS_TYPE_SINGLE}
  {$define HAS_TYPE_SINGLE}
 {$else}
  {$undef HAS_TYPE_SINGLE}
 {$endif}
 {$define CAN_INLINE}
 {$define HAS_ADVANCED_RECORDS}

 { CGE: disable FPC warnings at range check errors in constants here.
   Note that we need to include more than affected lines in warnings off/on
   section, to effectively disable warnings -- the affected lines are in generic
   class and it seems they are reported at specialization. }
 {$warnings off}
{$else}
 {$warn COMBINING_SIGNED_UNSIGNED off} // CGE added
 {$if CompilerVersion >= 32} {$warn COMBINING_SIGNED_UNSIGNED64 off} {$endif} // only since Delphi 10.2 // CGE added
 {$warn IMPLICIT_STRING_CAST off} // CGE added
 {$realcompatibility off}
 {$localsymbols on}
 {$define LITTLE_ENDIAN}
 {$ifndef cpu64}
  {$define cpu32}
 {$endif}
 {$define HAS_TYPE_EXTENDED}
 {$define HAS_TYPE_DOUBLE}
 {$define HAS_TYPE_SINGLE}
 {$undef CAN_INLINE}
 {$undef HAS_ADVANCED_RECORDS}
 {$ifndef BCB}
  {$ifdef ver120}
   {$define Delphi4or5}
  {$endif}
  {$ifdef ver130}
   {$define Delphi4or5}
  {$endif}
  {$ifdef ver140}
   {$define Delphi6}
  {$endif}
  {$ifdef ver150}
   {$define Delphi7}
  {$endif}
  {$ifdef ver170}
   {$define Delphi2005}
  {$endif}
 {$else}
  {$ifdef ver120}
   {$define Delphi4or5}
   {$define BCB4}
  {$endif}
  {$ifdef ver130}
   {$define Delphi4or5}
  {$endif}
 {$endif}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
  {$if CompilerVersion>=14.0}
   {$if CompilerVersion=14.0}
    {$define Delphi6}
   {$ifend}
   {$define Delphi6AndUp}
  {$ifend}
  {$if CompilerVersion>=15.0}
   {$if CompilerVersion=15.0}
    {$define Delphi7}
   {$ifend}
   {$define Delphi7AndUp}
  {$ifend}
  {$if CompilerVersion>=17.0}
   {$if CompilerVersion=17.0}
    {$define Delphi2005}
   {$ifend}
   {$define Delphi2005AndUp}
  {$ifend}
  {$if CompilerVersion>=18.0}
   {$if CompilerVersion=18.0}
    {$define BDS2006}
    {$define Delphi2006}
   {$ifend}
   {$define Delphi2006AndUp}
   {$define CAN_INLINE}
   {$define HAS_ADVANCED_RECORDS}
  {$ifend}
  {$if CompilerVersion>=18.5}
   {$if CompilerVersion=18.5}
    {$define Delphi2007}
   {$ifend}
   {$define Delphi2007AndUp}
  {$ifend}
  {$if CompilerVersion=19.0}
   {$define Delphi2007Net}
  {$ifend}
  {$if CompilerVersion>=20.0}
   {$if CompilerVersion=20.0}
    {$define Delphi2009}
   {$ifend}
   {$define Delphi2009AndUp}
  {$ifend}
  {$if CompilerVersion>=21.0}
   {$if CompilerVersion=21.0}
    {$define Delphi2010}
   {$ifend}
   {$define Delphi2010AndUp}
  {$ifend}
  {$if CompilerVersion>=22.0}
   {$if CompilerVersion=22.0}
    {$define DelphiXE}
   {$ifend}
   {$define DelphiXEAndUp}
  {$ifend}
  {$if CompilerVersion>=23.0}
   {$if CompilerVersion=23.0}
    {$define DelphiXE2}
   {$ifend}
   {$define DelphiXE2AndUp}
  {$ifend}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
   {$if CompilerVersion=24.0}
    {$define DelphiXE3}
   {$ifend}
   {$define DelphiXE3AndUp}
  {$ifend}
  {$if CompilerVersion>=25.0}
   {$if CompilerVersion=25.0}
    {$define DelphiXE4}
   {$ifend}
   {$define DelphiXE4AndUp}
  {$ifend}
  {$if CompilerVersion>=26.0}
   {$if CompilerVersion=26.0}
    {$define DelphiXE5}
   {$ifend}
   {$define DelphiXE5AndUp}
  {$ifend}
  {$if CompilerVersion>=27.0}
   {$if CompilerVersion=27.0}
    {$define DelphiXE6}
   {$ifend}
   {$define DelphiXE6AndUp}
  {$ifend}
  {$if CompilerVersion>=28.0}
   {$if CompilerVersion=28.0}
    {$define DelphiXE7}
   {$ifend}
   {$define DelphiXE7AndUp}
  {$ifend}
  {$if CompilerVersion>=29.0}
   {$if CompilerVersion=29.0}
    {$define DelphiXE8}
   {$ifend}
   {$define DelphiXE8AndUp}
  {$ifend}
  {$if CompilerVersion>=30.0}
   {$if CompilerVersion=30.0}
    {$define Delphi10Seattle}
   {$ifend}
   {$define Delphi10SeattleAndUp}
  {$ifend}
  {$if CompilerVersion>=31.0}
   {$if CompilerVersion=31.0}
    {$define Delphi10Berlin}
   {$ifend}
   {$define Delphi10BerlinAndUp}
  {$ifend}
  {$if CompilerVersion>=32.0}
   {$if CompilerVersion=32.0}
    {$define Delphi10Tokyo}
   {$ifend}
   {$define Delphi10TokyoAndUp}
  {$ifend}
  {$if CompilerVersion>=33.0}
   {$if CompilerVersion=33.0}
    {$define Delphi10Rio}
   {$ifend}
   {$define Delphi10RioAndUp}
  {$ifend}
 {$endif}
 {$ifndef Delphi4or5}
  {$ifndef BCB}
   {$define Delphi6AndUp}
  {$endif}
   {$ifndef Delphi6}
    {$define BCB6OrDelphi7AndUp}
    {$ifndef BCB}
     {$define Delphi7AndUp}
    {$endif}
    {$ifndef BCB}
     {$ifndef Delphi7}
      {$ifndef Delphi2005}
       {$define BDS2006AndUp}
      {$endif}
     {$endif}
    {$endif}
   {$endif}
 {$endif}
 {$ifdef Delphi6AndUp}
  {$warn symbol_platform off}
  {$warn symbol_deprecated off}
 {$endif}
{$endif}
{$if defined(Win32) or defined(Win64)}
 {$define Windows}
{$ifend}
{$rangechecks off}
{$extendedsyntax on}
{$writeableconst on}
{$hints off}
{$booleval off}
{$typedaddress off}
{$stackframes off}
{$varstringchecks on}
{$typeinfo on}
{$overflowchecks off}
{$longstrings on}
{$openstrings on}
{$ifndef HAS_TYPE_SINGLE}
 {$error No single floating point precision}
{$endif}
{$ifndef HAS_TYPE_DOUBLE}
 {$error No double floating point precision}
{$endif}
{$scopedenums on}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

interface

uses SysUtils,
     Classes,
     Math,
     PasJSON;

type PPPasGLTFInt8=^PPasGLTFInt8;
     PPasGLTFInt8=^TPasGLTFInt8;
     TPasGLTFInt8={$ifdef fpc}Int8{$else}shortint{$endif};

     PPPasGLTFUInt8=^PPasGLTFUInt8;
     PPasGLTFUInt8=^TPasGLTFUInt8;
     TPasGLTFUInt8={$ifdef fpc}UInt8{$else}byte{$endif};

     PPPasGLTFUInt8Array=^PPasGLTFUInt8Array;
     PPasGLTFUInt8Array=^TPasGLTFUInt8Array;
     TPasGLTFUInt8Array=array[0..65535] of TPasGLTFUInt8;

     TPasGLTFUInt8DynamicArray=array of TPasGLTFUInt8;

     PPPasGLTFInt16=^PPasGLTFInt16;
     PPasGLTFInt16=^TPasGLTFInt16;
     TPasGLTFInt16={$ifdef fpc}Int16{$else}smallint{$endif};

     PPPasGLTFUInt16=^PPasGLTFUInt16;
     PPasGLTFUInt16=^TPasGLTFUInt16;
     TPasGLTFUInt16={$ifdef fpc}UInt16{$else}word{$endif};

     PPPasGLTFInt32=^PPasGLTFInt32;
     PPasGLTFInt32=^TPasGLTFInt32;
     TPasGLTFInt32={$ifdef fpc}Int32{$else}longint{$endif};

     TPasGLTFInt32DynamicArray=array of TPasGLTFInt32;

     PPPasGLTFUInt32=^PPasGLTFUInt32;
     PPasGLTFUInt32=^TPasGLTFUInt32;
     TPasGLTFUInt32={$ifdef fpc}UInt32{$else}longword{$endif};

     PPPasGLTFUInt32Array=^PPasGLTFUInt32Array;
     PPasGLTFUInt32Array=^TPasGLTFUInt32Array;
     TPasGLTFUInt32Array=array[0..65535] of TPasGLTFUInt32;

     TPasGLTFUInt32DynamicArray=array of TPasGLTFUInt32;

     PPPasGLTFInt64=^PPasGLTFInt64;
     PPasGLTFInt64=^TPasGLTFInt64;
     TPasGLTFInt64=Int64;

     TPasGLTFInt64DynamicArray=array of TPasGLTFInt64;

     PPPasGLTFUInt64=^PPasGLTFUInt64;
     PPasGLTFUInt64=^TPasGLTFUInt64;
     TPasGLTFUInt64=UInt64;

     TPasGLTFUInt64DynamicArray=array of TPasGLTFUInt64;

     PPPasGLTFChar=^PAnsiChar;
     PPasGLTFChar=PAnsiChar;
     TPasGLTFChar=AnsiChar;

     PPPasGLTFRawByteChar=^PAnsiChar;
     PPasGLTFRawByteChar=PAnsiChar;
     TPasGLTFRawByteChar=AnsiChar;

     PPPasGLTFUTF16Char=^PWideChar;
     PPasGLTFUTF16Char=PWideChar;
     TPasGLTFUTF16Char=WideChar;

     PPPasGLTFPointer=^PPasGLTFPointer;
     PPasGLTFPointer=^TPasGLTFPointer;
     TPasGLTFPointer=Pointer;

     PPPasGLTFPointers=^PPasGLTFPointers;
     PPasGLTFPointers=^TPasGLTFPointers;
     TPasGLTFPointers=array[0..65535] of TPasGLTFPointer;

     PPPasGLTFVoid=^PPasGLTFVoid;
     PPasGLTFVoid=TPasGLTFPointer;

     PPPasGLTFFloat=^PPasGLTFFloat;
     PPasGLTFFloat=^TPasGLTFFloat;
     TPasGLTFFloat=Single;

     TPasGLTFFloatDynamicArray=array of TPasGLTFFloat;

     PPPasGLTFDouble=^PPasGLTFDouble;
     PPasGLTFDouble=^TPasGLTFDouble;
     TPasGLTFDouble=Double;

     TPasGLTFDoubleDynamicArray=array of TPasGLTFDouble;

     PPPasGLTFPtrUInt=^PPasGLTFPtrUInt;
     PPPasGLTFPtrInt=^PPasGLTFPtrInt;
     PPasGLTFPtrUInt=^TPasGLTFPtrUInt;
     PPasGLTFPtrInt=^TPasGLTFPtrInt;
{$ifdef fpc}
     TPasGLTFPtrUInt=PtrUInt;
     TPasGLTFPtrInt=PtrInt;
 {$undef OldDelphi}
{$else}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=23.0}
   {$undef OldDelphi}
     TPasGLTFPtrUInt=NativeUInt;
     TPasGLTFPtrInt=NativeInt;
  {$else}
   {$define OldDelphi}
  {$ifend}
 {$else}
  {$define OldDelphi}
 {$endif}
{$endif}
{$ifdef OldDelphi}
{$ifdef cpu64}
     TPasGLTFPtrUInt=uint64;
     TPasGLTFPtrInt=int64;
{$else}
     TPasGLTFPtrUInt=longword;
     TPasGLTFPtrInt=longint;
{$endif}
{$endif}

     PPPasGLTFSizeUInt=^PPasGLTFSizeUInt;
     PPasGLTFSizeUInt=^TPasGLTFSizeUInt;
     TPasGLTFSizeUInt=TPasGLTFPtrUInt;

     TPasGLTFSizeUIntDynamicArray=array of TPasGLTFSizeUInt;

     PPPasGLTFSizeInt=^PPasGLTFSizeInt;
     PPasGLTFSizeInt=^TPasGLTFSizeInt;
     TPasGLTFSizeInt=TPasGLTFPtrInt;

     TPasGLTFSizeIntDynamicArray=array of TPasGLTFSizeInt;

     PPPasGLTFNativeUInt=^PPasGLTFNativeUInt;
     PPasGLTFNativeUInt=^TPasGLTFNativeUInt;
     TPasGLTFNativeUInt=TPasGLTFPtrUInt;

     PPPasGLTFNativeInt=^PPasGLTFNativeInt;
     PPasGLTFNativeInt=^TPasGLTFNativeInt;
     TPasGLTFNativeInt=TPasGLTFPtrInt;

     PPPasGLTFSize=^PPasGLTFSizeUInt;
     PPasGLTFSize=^TPasGLTFSizeUInt;
     TPasGLTFSize=TPasGLTFPtrUInt;

     PPPasGLTFPtrDiff=^PPasGLTFPtrDiff;
     PPasGLTFPtrDiff=^TPasGLTFPtrDiff;
     TPasGLTFPtrDiff=TPasGLTFPtrInt;

     PPPasGLTFRawByteString=^PPasGLTFRawByteString;
     PPasGLTFRawByteString=^TPasGLTFRawByteString;
     TPasGLTFRawByteString={$if declared(RawByteString)}RawByteString{$else}AnsiString{$ifend};

     PPPasGLTFUTF8String=^PPasGLTFUTF8String;
     PPasGLTFUTF8String=^TPasGLTFUTF8String;
     TPasGLTFUTF8String={$if declared(UTF8String)}UTF8String{$else}AnsiString{$ifend};

     PPPasGLTFUTF16String=^PPasGLTFUTF16String;
     PPasGLTFUTF16String=^TPasGLTFUTF16String;
     TPasGLTFUTF16String={$if declared(UnicodeString)}UnicodeString{$else}WideString{$ifend};

     EPasGLTF=class(Exception);

     EPasGLTFInvalidDocument=class(EPasGLTF);

     EPasGLTFInvalidBase64=class(EPasGLTF);

     TPasGLTFTypedSort<T>=class
      private
       type TStackItem=record
            Left:TPasGLTFSizeInt;
            Right:TPasGLTFSizeInt;
            Depth:TPasGLTFInt32;
           end;
           PStackItem=^TStackItem;
      public
       type TPasGLTFTypedSortCompareFunction=function(const a,b:T):TPasGLTFInt32;
{$ifndef fpc}
      private
       class function BSRDWord(aValue:TPasGLTFUInt32):TPasGLTFInt32; static;
{$endif}
      public
       class procedure IntroSort(const pItems:TPasGLTFPointer;const pLeft,pRight:TPasGLTFSizeInt;const pCompareFunc:TPasGLTFTypedSortCompareFunction); static;
     end;

     TPasGLTFDynamicArray<T>=class
      private
       type TValueEnumerator=record
             private
              fDynamicArray:TPasGLTFDynamicArray<T>;
              fIndex:TPasGLTFSizeInt;
              function GetCurrent:T; inline;
             public
              constructor Create(const aDynamicArray:TPasGLTFDynamicArray<T>);
              function MoveNext:boolean; inline;
              property Current:T read GetCurrent;
            end;
      private
       fItems:array of T;
       fCount:TPasGLTFSizeInt;
       fAllocated:TPasGLTFSizeInt;
       procedure SetCount(const pNewCount:TPasGLTFSizeInt);
       function GetItem(const pIndex:TPasGLTFSizeInt):T; inline;
       procedure SetItem(const pIndex:TPasGLTFSizeInt;const pItem:T); inline;
      protected
      public
       constructor Create;
       destructor Destroy; override;
       procedure Clear;
       function Add(const pItem:T):TPasGLTFSizeInt; overload;
       function Add(const pItems:array of T):TPasGLTFSizeInt; overload;
       procedure Insert(const pIndex:TPasGLTFSizeInt;const pItem:T);
       procedure Delete(const pIndex:TPasGLTFSizeInt);
       procedure Exchange(const pIndex,pWithIndex:TPasGLTFSizeInt); inline;
       function GetEnumerator:TValueEnumerator;
       function Memory:TPasGLTFPointer; inline;
       property Count:TPasGLTFSizeInt read fCount write SetCount;
       property Allocated:TPasGLTFSizeInt read fAllocated;
       property Items[const pIndex:TPasGLTFSizeInt]:T read GetItem write SetItem; default;
     end;

     TPasGLTFObjectList<T:class>=class
      private
       type TValueEnumerator=record
             private
              fObjectList:TPasGLTFObjectList<T>;
              fIndex:TPasGLTFSizeInt;
              function GetCurrent:T; inline;
             public
              constructor Create(const aObjectList:TPasGLTFObjectList<T>);
              function MoveNext:boolean; inline;
              property Current:T read GetCurrent;
            end;
      private
       fItems:array of T;
       fCount:TPasGLTFSizeInt;
       fAllocated:TPasGLTFSizeInt;
       fOwnsObjects:boolean;
       function RoundUpToPowerOfTwoSizeUInt(x:TPasGLTFSizeUInt):TPasGLTFSizeUInt;
       procedure SetCount(const pNewCount:TPasGLTFSizeInt);
       function GetItem(const pIndex:TPasGLTFSizeInt):T;
       procedure SetItem(const pIndex:TPasGLTFSizeInt;const pItem:T);
      public
       constructor Create;
       destructor Destroy; override;
       procedure Clear;
       function IndexOf(const pItem:T):TPasGLTFSizeInt;
       function Add(const pItem:T):TPasGLTFSizeInt;
       procedure Insert(const pIndex:TPasGLTFSizeInt;const pItem:T);
       procedure Delete(const pIndex:TPasGLTFSizeInt);
       procedure Remove(const pItem:T);
       procedure Exchange(const pIndex,pWithIndex:TPasGLTFSizeInt);
       function GetEnumerator:TValueEnumerator;
       property Count:TPasGLTFSizeInt read fCount write SetCount;
       property Allocated:TPasGLTFSizeInt read fAllocated;
       property Items[const pIndex:TPasGLTFSizeInt]:T read GetItem write SetItem; default;
       property OwnsObjects:boolean read fOwnsObjects write fOwnsObjects;
     end;

     TPasGLTFHashMapEntityIndices=array of TPasGLTFInt32;

     TPasGLTFHashMapUInt128=array[0..1] of TPasGLTFUInt64;

     TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>=class
      public
       const CELL_EMPTY=-1;
             CELL_DELETED=-2;
             ENT_EMPTY=-1;
             ENT_DELETED=-2;
       type TPasGLTFHashMapKey=TPasGLTFUTF8String;
            PPasGLTFHashMapEntity=^TPasGLTFHashMapEntity;
            TPasGLTFHashMapEntity=record
             Key:TPasGLTFHashMapKey;
             Value:TPasGLTFHashMapValue;
            end;
            TPasGLTFHashMapEntities=array of TPasGLTFHashMapEntity;
      private
       type TPasGLTFHashMapEntityEnumerator=record
             private
              fHashMap:TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>;
              fIndex:TPasGLTFSizeInt;
              function GetCurrent:TPasGLTFHashMapEntity; inline;
             public
              constructor Create(const aHashMap:TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>);
              function MoveNext:boolean; inline;
              property Current:TPasGLTFHashMapEntity read GetCurrent;
            end;
            TPasGLTFHashMapKeyEnumerator=record
             private
              fHashMap:TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>;
              fIndex:TPasGLTFSizeInt;
              function GetCurrent:TPasGLTFHashMapKey; inline;
             public
              constructor Create(const aHashMap:TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>);
              function MoveNext:boolean; inline;
              property Current:TPasGLTFHashMapKey read GetCurrent;
            end;
            TPasGLTFHashMapValueEnumerator=record
             private
              fHashMap:TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>;
              fIndex:TPasGLTFSizeInt;
              function GetCurrent:TPasGLTFHashMapValue; inline;
             public
              constructor Create(const aHashMap:TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>);
              function MoveNext:boolean; inline;
              property Current:TPasGLTFHashMapValue read GetCurrent;
            end;
            TPasGLTFHashMapEntitiesObject=class
             private
              fOwner:TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>;
             public
              constructor Create(const aOwner:TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>);
              function GetEnumerator:TPasGLTFHashMapEntityEnumerator;
            end;
            TPasGLTFHashMapKeysObject=class
             private
              fOwner:TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>;
             public
              constructor Create(const aOwner:TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>);
              function GetEnumerator:TPasGLTFHashMapKeyEnumerator;
            end;
            TPasGLTFHashMapValuesObject=class
             private
              fOwner:TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>;
              function GetValue(const Key:TPasGLTFHashMapKey):TPasGLTFHashMapValue; inline;
              procedure SetValue(const Key:TPasGLTFHashMapKey;const aValue:TPasGLTFHashMapValue); inline;
             public
              constructor Create(const aOwner:TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>);
              function GetEnumerator:TPasGLTFHashMapValueEnumerator;
              property Values[const Key:TPasGLTFHashMapKey]:TPasGLTFHashMapValue read GetValue write SetValue; default;
            end;
      private
       fRealSize:TPasGLTFInt32;
       fLogSize:TPasGLTFInt32;
       fSize:TPasGLTFInt32;
       fEntities:TPasGLTFHashMapEntities;
       fEntityToCellIndex:TPasGLTFHashMapEntityIndices;
       fCellToEntityIndex:TPasGLTFHashMapEntityIndices;
       fDefaultValue:TPasGLTFHashMapValue;
       fCanShrink:boolean;
       fEntitiesObject:TPasGLTFHashMapEntitiesObject;
       fKeysObject:TPasGLTFHashMapKeysObject;
       fValuesObject:TPasGLTFHashMapValuesObject;
       function HashData(const Data:TPasGLTFPointer;const DataLength:TPasGLTFSizeUInt):TPasGLTFUInt32;
       function HashKey(const Key:TPasGLTFHashMapKey):TPasGLTFUInt32;
       function CompareKey(const KeyA,KeyB:TPasGLTFHashMapKey):boolean;
       function FindCell(const Key:TPasGLTFHashMapKey):TPasGLTFUInt32;
       procedure Resize;
      protected
       function GetValue(const Key:TPasGLTFHashMapKey):TPasGLTFHashMapValue;
       procedure SetValue(const Key:TPasGLTFHashMapKey;const Value:TPasGLTFHashMapValue);
      public
       constructor Create(const DefaultValue:TPasGLTFHashMapValue);
       destructor Destroy; override;
       procedure Clear;
       function Add(const Key:TPasGLTFHashMapKey;const Value:TPasGLTFHashMapValue):PPasGLTFHashMapEntity;
       function Get(const Key:TPasGLTFHashMapKey;const CreateIfNotExist:boolean=false):PPasGLTFHashMapEntity;
       function TryGet(const Key:TPasGLTFHashMapKey;out Value:TPasGLTFHashMapValue):boolean;
       function ExistKey(const Key:TPasGLTFHashMapKey):boolean;
       function Delete(const Key:TPasGLTFHashMapKey):boolean;
       property EntityValues[const Key:TPasGLTFHashMapKey]:TPasGLTFHashMapValue read GetValue write SetValue; default;
       property Entities:TPasGLTFHashMapEntitiesObject read fEntitiesObject;
       property Keys:TPasGLTFHashMapKeysObject read fKeysObject;
       property Values:TPasGLTFHashMapValuesObject read fValuesObject;
       property CanShrink:boolean read fCanShrink write fCanShrink;
     end;

     TPasGLTF=class
      public
       type TBase64=class
             public
              const EncodingLookUpTable:array[0..63] of TPasGLTFRawByteChar=
                     (
                      'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P',
                      'Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f',
                      'g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v',
                      'w','x','y','z','0','1','2','3','4','5','6','7','8','9','+','/'
                     );
                    DecodingLookUpTable:array[TPasGLTFRawByteChar] of TPasGLTFInt8=
                     (
                      -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                      -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                      -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,62,-1,-1,-1,63,
                      52,53,54,55,56,57,58,59,60,61,-1,-1,-1,-1,-1,-1,
                      -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,
                      15,16,17,18,19,20,21,22,23,24,25,-1,-1,-1,-1,-1,
                      -1,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                      41,42,43,44,45,46,47,48,49,50,51,-1,-1,-1,-1,-1,
                      -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                      -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                      -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                      -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                      -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                      -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                      -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                      -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
                     );
             public
              class function Encode(const aData;const aDataLength:TPasGLTFSizeInt):TPasGLTFRawByteString; overload; static;
              class function Encode(const aData:array of TPasGLTFUInt8):TPasGLTFRawByteString; overload; static;
              class function Encode(const aData:TPasGLTFRawByteString):TPasGLTFRawByteString; overload; static;
              class function Encode(const aData:TStream):TPasGLTFRawByteString; overload; static;
              class function Decode(const aInput:TPasGLTFRawByteString;const aOutput:TStream):boolean; overload; static;
            end;
            TChunkHeader=packed record
             ChunkLength:TPasGLTFUInt32;
             ChunkType:TPasGLTFUInt32;
            end;
            PChunkHeader=^TChunkHeader;
            TGLBHeader=packed record
             Magic:TPasGLTFUInt32;
             Version:TPasGLTFUInt32;
             Length:TPasGLTFUInt32;
             JSONChunkHeader:TChunkHeader;
            end;
            TVector2=array[0..1] of TPasGLTFFloat;
            PVector2=^TVector2;
            TVector2DynamicArray=array of TVector2;
            TVector3=array[0..2] of TPasGLTFFloat;
            PVector3=^TVector3;
            TVector3DynamicArray=array of TVector3;
            TVector4=array[0..3] of TPasGLTFFloat;
            PVector4=^TVector4;
            TVector4DynamicArray=array of TVector4;
            TInt32Vector4=array[0..3] of TPasGLTFInt32;
            PInt32Vector4=^TInt32Vector4;
            TInt32Vector4DynamicArray=array of TInt32Vector4;
            TUInt32Vector4=array[0..3] of TPasGLTFUInt32;
            PUInt32Vector4=^TUInt32Vector4;
            TUInt32Vector4DynamicArray=array of TUInt32Vector4;
            TMatrix2x2=array[0..3] of TPasGLTFFloat;
            PMatrix2x2=^TMatrix2x2;
            TMatrix2x2DynamicArray=array of TMatrix2x2;
            TMatrix3x3=array[0..9] of TPasGLTFFloat;
            PMatrix3x3=^TMatrix3x3;
            TMatrix3x3DynamicArray=array of TMatrix3x3;
            TMatrix4x4=array[0..15] of TPasGLTFFloat;
            PMatrix4x4=^TMatrix4x4;
            TMatrix4x4DynamicArray=array of TMatrix4x4;
       const ChunkHeaderSize=SizeOf(TChunkHeader);
             GLBHeaderSize=SizeOf(TGLBHeader);
             GLBHeaderMagicNativeEndianness=TPasGLTFUInt32($46546c67);
             GLBHeaderMagicOtherEndianness=TPasGLTFUInt32($676c5446);
             GLBChunkJSONNativeEndianness=TPasGLTFUInt32($4e4f534a);
             GLBChunkJSONOtherEndianness=TPasGLTFUInt32($4a534f4e);
             GLBChunkBinaryNativeEndianness=TPasGLTFUInt32($004e4942);
             GLBChunkBinaryOtherEndianness=TPasGLTFUInt32($42494e00);
             MimeTypeApplicationOctet='application/octet-stream';
             MimeTypeImagePNG='image/png';
             MimeTypeImageJPG='image/jpg';
       type TDefaults=class
             public
              const AccessorNormalized=false;
                    MaterialAlphaCutoff=0.5;
                    MaterialDoubleSided=false;
                    IdentityScalar=1.0;
                    FloatSentinel=1e+27;
                    NullVector3:TVector3=(0.0,0.0,0.0);
                    IdentityVector3:TVector3=(1.0,1.0,1.0);
                    IdentityVector4:TVector4=(1.0,1.0,1.0,1.0);
                    IdentityQuaternion:TVector4=(0.0,0.0,0.0,1.0);
                    NullMatrix4x4:TMatrix4x4=(0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0);
                    IdentityMatrix4x4:TMatrix4x4=(1.0,0.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,0.0,1.0);
            end;
            TDocument=class;
            TBaseObject=class
             private
              fDocument:TDocument;
             public
              constructor Create(const aDocument:TDocument); reintroduce; virtual;
              destructor Destroy; override;
            end;
            TBaseExtensionsExtrasObject=class(TBaseObject)
             private
              fExtensions:TPasJSONItemObject;
              fExtras:TPasJSONItemObject;
             public
              constructor Create(const aDocument:TDocument); override;
              destructor Destroy; override;
             published
              property Extensions:TPasJSONItemObject read fExtensions;
              property Extras:TPasJSONItemObject read fExtras;
            end;
            TAttributes=TPasGLTFUTF8StringHashMap<TPasGLTFSizeInt>;
            TAttributesList=TPasGLTFObjectList<TAttributes>;
            TAccessor=class(TBaseExtensionsExtrasObject)
             public
              type TComponentType=
                    (
                     None=0,
                     SignedByte=5120,
                     UnsignedByte=5121,
                     SignedShort=5122,
                     UnsignedShort=5123,
                     UnsignedInt=5125,
                     Float=5126
                    );
                   PComponentType=^TComponentType;
                   TComponentTypeHelper=record helper for TComponentType
                    function GetSize:TPasGLTFSizeInt; inline;
                   end;
                   TRawComponentType=TPasGLTFUInt16;
                   PRawComponentType=^TRawComponentType;
                   TType=
                    (
                     None=0,
                     Scalar=1,
                     Vec2=2,
                     Vec3=3,
                     Vec4=4,
                     Mat2=5,
                     Mat3=6,
                     Mat4=7
                    );
                   PType=^TType;
                   TTypeHelper=record helper for TType
                    function GetComponentCount:TPasGLTFSizeInt; inline;
                   end;
                   TRawType=TPasGLTFUInt8;
                   PRawType=^TRawType;
                   TMinMaxDynamicArray=TPasGLTFDynamicArray<TPasGLTFFloat>;
                   TSparse=class(TBaseExtensionsExtrasObject)
                    public
                     type TIndices=class(TBaseExtensionsExtrasObject)
                           private
                            fComponentType:TComponentType;
                            fBufferView:TPasGLTFSizeInt;
                            fByteOffset:TPasGLTFSizeUInt;
                            fEmpty:boolean;
                           public
                            constructor Create(const aDocument:TDocument); override;
                            destructor Destroy; override;
                           published
                            property ComponentType:TComponentType read fComponentType write fComponentType default TComponentType.None;
                            property BufferView:TPasGLTFSizeInt read fBufferView write fBufferView default 0;
                            property ByteOffset:TPasGLTFSizeUInt read fByteOffset write fByteOffset default 0;
                            property Empty:boolean read fEmpty;
                          end;
                          TValues=class(TBaseExtensionsExtrasObject)
                           private
                            fBufferView:TPasGLTFSizeInt;
                            fByteOffset:TPasGLTFSizeUInt;
                            fEmpty:boolean;
                           public
                            constructor Create(const aDocument:TDocument); override;
                            destructor Destroy; override;
                           published
                            property BufferView:TPasGLTFSizeInt read fBufferView write fBufferView default 0;
                            property ByteOffset:TPasGLTFSizeUInt read fByteOffset write fByteOffset default 0;
                            property Empty:boolean read fEmpty;
                          end;
                    private
                     fCount:TPasGLTFSizeInt;
                     fIndices:TIndices;
                     fValues:TValues;
                     function GetEmpty:boolean;
                    public
                     constructor Create(const aDocument:TDocument); override;
                     destructor Destroy; override;
                    published
                     property Count:TPasGLTFSizeInt read fCount write fCount default 0;
                     property Indices:TIndices read fIndices;
                     property Values:TValues read fValues;
                     property Empty:boolean read GetEmpty;
                   end;
              const TypeComponentCountTable:array[TType] of TPasGLTFSizeInt=(0,1,2,3,4,4,9,16);
             private
              fName:TPasGLTFUTF8String;
              fComponentType:TComponentType;
              fType:TType;
              fBufferView:TPasGLTFSizeInt;
              fByteOffset:TPasGLTFSizeUInt;
              fCount:TPasGLTFSizeUInt;
              fNormalized:boolean;
              fMinArray:TMinMaxDynamicArray;
              fMaxArray:TMinMaxDynamicArray;
              fSparse:TSparse;
             public
              constructor Create(const aDocument:TDocument); override;
              destructor Destroy; override;
              function DecodeAsDoubleArray(const aForVertex:boolean=true):TPasGLTFDoubleDynamicArray;
              function DecodeAsInt32Array(const aForVertex:boolean=true):TPasGLTFInt32DynamicArray;
              function DecodeAsUInt32Array(const aForVertex:boolean=true):TPasGLTFUInt32DynamicArray;
              function DecodeAsInt64Array(const aForVertex:boolean=true):TPasGLTFInt64DynamicArray;
              function DecodeAsUInt64Array(const aForVertex:boolean=true):TPasGLTFUInt64DynamicArray;
              function DecodeAsFloatArray(const aForVertex:boolean=true):TPasGLTFFloatDynamicArray;
              function DecodeAsVector2Array(const aForVertex:boolean=true):TVector2DynamicArray;
              function DecodeAsVector3Array(const aForVertex:boolean=true):TVector3DynamicArray;
              function DecodeAsVector4Array(const aForVertex:boolean=true):TVector4DynamicArray;
              function DecodeAsInt32Vector4Array(const aForVertex:boolean=true):TInt32Vector4DynamicArray;
              function DecodeAsUInt32Vector4Array(const aForVertex:boolean=true):TUInt32Vector4DynamicArray;
              function DecodeAsColorArray(const aForVertex:boolean=true):TVector4DynamicArray;
              function DecodeAsMatrix2x2Array(const aForVertex:boolean=true):TMatrix2x2DynamicArray;
              function DecodeAsMatrix3x3Array(const aForVertex:boolean=true):TMatrix3x3DynamicArray;
              function DecodeAsMatrix4x4Array(const aForVertex:boolean=true):TMatrix4x4DynamicArray;
             published
              property ComponentType:TComponentType read fComponentType write fComponentType default TComponentType.None;
              property Type_:TType read fType write fType default TType.None;
              property BufferView:TPasGLTFSizeInt read fBufferView write fBufferView default -1;
              property ByteOffset:TPasGLTFSizeUInt read fByteOffset write fByteOffset default 0;
              property Count:TPasGLTFSizeUInt read fCount write fCount default 0;
              property MinArray:TMinMaxDynamicArray read fMinArray;
              property MaxArray:TMinMaxDynamicArray read fMaxArray;
              property Normalized:boolean read fNormalized write fNormalized default false;
              property Sparse:TSparse read fSparse;
            end;
            TAccessors=TPasGLTFObjectList<TAccessor>;
            TAnimation=class(TBaseExtensionsExtrasObject)
             public
              type TChannel=class(TBaseExtensionsExtrasObject)
                    public
                     type TTarget=class(TBaseExtensionsExtrasObject)
                           private
                            fNode:TPasGLTFSizeInt;
                            fPath:TPasGLTFUTF8String;
                            fEmpty:boolean;
                           public
                            constructor Create(const aDocument:TDocument); override;
                            destructor Destroy; override;
                           published
                            property Node:TPasGLTFSizeInt read fNode write fNode default -1;
                            property Path:TPasGLTFUTF8String read fPath write fPath;
                            property Empty:boolean read fEmpty;
                          end;
                    private
                     fSampler:TPasGLTFSizeInt;
                     fTarget:TChannel.TTarget;
                    public
                     constructor Create(const aDocument:TDocument); override;
                     destructor Destroy; override;
                    published
                     property Sampler:TPasGLTFSizeInt read fSampler write fSampler default -1;
                     property Target:TChannel.TTarget read fTarget;
                   end;
                   TChannels=TPasGLTFObjectList<TChannel>;
                   TSampler=class(TBaseExtensionsExtrasObject)
                    public
                     // CGE: Workaround FPC problems (when generating debug info):
                     // PasGLTF.pas(5785,1) Error: Asm: Duplicate label DBG_$PASGLTF_$$_TTYPE
                     // PasGLTF.pas(5785,1) Error: Asm: Duplicate label DBGREF_$PASGLTF_$$_TTYPE
                     // You cannot define TType multiple times, so we rename to TCameraType, TSamplerType
                     //
                     // TODO: BeRo says fixed in FPC trunk, test.
                     // We need a workaround for FPC 3.0.4 anyway.
                     type TSamplerType=
                           (
                            Linear=0,
                            Step=1,
                            CubicSpline=2
                           );
                           PSamplerType=^TSamplerType;
                    private
                     fInput:TPasGLTFSizeInt;
                     fOutput:TPasGLTFSizeInt;
                     fInterpolation:TSamplerType;
                    public
                     constructor Create(const aDocument:TDocument); override;
                     destructor Destroy; override;
                    published
                     property Input:TPasGLTFSizeInt read fInput write fInput default -1;
                     property Output:TPasGLTFSizeInt read fOutput write fOutput default -1;
                     property Interpolation:TSamplerType read fInterpolation write fInterpolation default TSamplerType.Linear;
                   end;
                   TSamplers=TPasGLTFObjectList<TSampler>;
             private
              fName:TPasGLTFUTF8String;
              fChannels:TChannels;
              fSamplers:TSamplers;
             public
              constructor Create(const aDocument:TDocument); override;
              destructor Destroy; override;
             published
              property Name:TPasGLTFUTF8String read fName write fName;
              property Channels:TChannels read fChannels;
              property Samplers:TSamplers read fSamplers;
            end;
            TAnimations=TPasGLTFObjectList<TAnimation>;
            TAsset=class(TBaseExtensionsExtrasObject)
             private
              fCopyright:TPasGLTFUTF8String;
              fGenerator:TPasGLTFUTF8String;
              fMinVersion:TPasGLTFUTF8String;
              fVersion:TPasGLTFUTF8String;
              fEmpty:boolean;
             public
              constructor Create(const aDocument:TDocument); override;
              destructor Destroy; override;
             published
              property Copyright:TPasGLTFUTF8String read fCopyright write fCopyright;
              property Generator:TPasGLTFUTF8String read fGenerator write fGenerator;
              property MinVersion:TPasGLTFUTF8String read fMinVersion write fMinVersion;
              property Version:TPasGLTFUTF8String read fVersion write fVersion;
              property Empty:boolean read fEmpty;
            end;
            TBuffer=class(TBaseExtensionsExtrasObject)
             private
              fByteLength:TPasGLTFSizeUInt;
              fName:TPasGLTFUTF8String;
              fURI:TPasGLTFUTF8String;
              fData:TMemoryStream;
             public
              constructor Create(const aDocument:TDocument); override;
              destructor Destroy; override;
              procedure SetEmbeddedResourceData;
             published
              property ByteLength:TPasGLTFSizeUInt read fByteLength write fByteLength;
              property Name:TPasGLTFUTF8String read fName write fName;
              property URI:TPasGLTFUTF8String read fURI write fURI;
              property Data:TMemoryStream read fData write fData;
            end;
            TBuffers=TPasGLTFObjectList<TBuffer>;
            TBufferView=class(TBaseExtensionsExtrasObject)
             public
              type TTargetType=
                    (
                     None=0,
                     ArrayBuffer=34962,
                     ElementArrayBuffer=34963
                    );
                   PTargetType=^TTargetType;
             private
              fName:TPasGLTFUTF8String;
              fBuffer:TPasGLTFSizeInt;
              fByteOffset:TPasGLTFSizeUInt;
              fByteLength:TPasGLTFSizeUInt;
              fByteStride:TPasGLTFSizeUInt;
              fTarget:TTargetType;
             public
              constructor Create(const aDocument:TDocument); override;
              destructor Destroy; override;
              function Decode(const aSkipEvery:TPasGLTFSizeUInt;
                              const aSkipBytes:TPasGLTFSizeUInt;
                              const aElementSize:TPasGLTFSizeUInt;
                              const aCount:TPasGLTFSizeUInt;
                              const aType:TPasGLTF.TAccessor.TType;
                              const aComponentCount:TPasGLTFSizeUInt;
                              const aComponentType:TPasGLTF.TAccessor.TComponentType;
                              const aComponentSize:TPasGLTFSizeUInt;
                              const aByteOffset:TPasGLTFSizeUInt;
                              const aNormalized:boolean;
                              const aForVertex:boolean):TPasGLTFDoubleDynamicArray;
             published
              property Name:TPasGLTFUTF8String read fName write fName;
              property Buffer:TPasGLTFSizeInt read fBuffer write fBuffer;
              property ByteOffset:TPasGLTFSizeUInt read fByteOffset write fByteOffset;
              property ByteLength:TPasGLTFSizeUInt read fByteLength write fByteLength;
              property ByteStride:TPasGLTFSizeUInt read fByteStride write fByteStride;
              property Target:TTargetType read fTarget write fTarget default TTargetType.None;
            end;
            TBufferViews=TPasGLTFObjectList<TBufferView>;
            TCamera=class(TBaseExtensionsExtrasObject)
             public
              // CGE: Workaround FPC problems (when generating debug info):
              // PasGLTF.pas(5785,1) Error: Asm: Duplicate label DBG_$PASGLTF_$$_TTYPE
              // PasGLTF.pas(5785,1) Error: Asm: Duplicate label DBGREF_$PASGLTF_$$_TTYPE
              // You cannot define TType multiple times, so we rename to TCameraType, TSamplerType
              //
              // TODO: BeRo says fixed in FPC trunk, test.
              // We need a workaround for FPC 3.0.4 anyway.
              type TCameraType=
                    (
                     None=0,
                     Orthographic=1,
                     Perspective=2
                    );
                   TOrthographic=class(TBaseExtensionsExtrasObject)
                    private
                     fXMag:TPasGLTFFloat;
                     fYMag:TPasGLTFFloat;
                     fZNear:TPasGLTFFloat;
                     fZFar:TPasGLTFFloat;
                     fEmpty:boolean;
                    public
                     constructor Create(const aDocument:TDocument); override;
                     destructor Destroy; override;
                    published
                     property XMag:TPasGLTFFloat read fXMag write fXMag;
                     property YMag:TPasGLTFFloat read fYMag write fYMag;
                     property ZNear:TPasGLTFFloat read fZNear write fZNear;
                     property ZFar:TPasGLTFFloat read fZFar write fZFar;
                     property Empty:boolean read fEmpty;
                   end;
                   TPerspective=class(TBaseExtensionsExtrasObject)
                    private
                     fAspectRatio:TPasGLTFFloat;
                     fYFov:TPasGLTFFloat;
                     fZNear:TPasGLTFFloat;
                     fZFar:TPasGLTFFloat;
                     fEmpty:boolean;
                    public
                     constructor Create(const aDocument:TDocument); override;
                     destructor Destroy; override;
                    published
                     property AspectRatio:TPasGLTFFloat read fAspectRatio write fAspectRatio;
                     property YFov:TPasGLTFFloat read fYFov write fYFov;
                     property ZNear:TPasGLTFFloat read fZNear write fZNear;
                     property ZFar:TPasGLTFFloat read fZFar write fZFar;
                     property Empty:boolean read fEmpty;
                   end;
             private
              fType:TCameraType;
              fOrthographic:TOrthographic;
              fPerspective:TPerspective;
              fName:TPasGLTFUTF8String;
             public
              constructor Create(const aDocument:TDocument); override;
              destructor Destroy; override;
             published
              property Type_:TCameraType read fType write fType;
              property Orthographic:TOrthographic read fOrthographic;
              property Perspective:TPerspective read fPerspective;
              property Name:TPasGLTFUTF8String read fName write fName;
            end;
            TCameras=TPasGLTFObjectList<TCamera>;
            TImage=class(TBaseExtensionsExtrasObject)
             private
              fBufferView:TPasGLTFSizeInt;
              fName:TPasGLTFUTF8String;
              fURI:TPasGLTFUTF8String;
              fMimeType:TPasGLTFUTF8String;
             public
              constructor Create(const aDocument:TDocument); override;
              destructor Destroy; override;
              procedure SetEmbeddedResourceData(const aStream:TStream);
              procedure GetResourceData(const aStream:TStream);
              function IsExternalResource:boolean;
             published
              property BufferView:TPasGLTFSizeInt read fBufferView write fBufferView;
              property Name:TPasGLTFUTF8String read fName write fName;
              property URI:TPasGLTFUTF8String read fURI write fURI;
              property MimeType:TPasGLTFUTF8String read fMimeType write fMimeType;
            end;
            TImages=TPasGLTFObjectList<TImage>;
            TMaterial=class(TBaseExtensionsExtrasObject)
             public
              type TAlphaMode=
                    (
                     Opaque=0,
                     Mask=1,
                     Blend=2
                    );
                   PAlphaMode=^TAlphaMode;
                   TAlphaModes=set of TAlphaMode;
                   TTexture=class(TBaseExtensionsExtrasObject)
                    private
                     fIndex:TPasGLTFSizeInt;
                     fTexCoord:TPasGLTFSizeInt;
                     function GetEmpty:boolean;
                    public
                     constructor Create(const aDocument:TDocument); override;
                     destructor Destroy; override;
                    published
                     property Index:TPasGLTFSizeInt read fIndex write fIndex;
                     property TexCoord:TPasGLTFSizeInt read fTexCoord write fTexCoord;
                     property Empty:boolean read GetEmpty;
                   end;
                   TNormalTexture=class(TTexture)
                    private
                     fScale:TPasGLTFFloat;
                    public
                     constructor Create(const aDocument:TDocument); override;
                    published
                     property Scale:TPasGLTFFloat read fScale write fScale;
                   end;
                   TOcclusionTexture=class(TTexture)
                    private
                     fStrength:TPasGLTFFloat;
                    public
                     constructor Create(const aDocument:TDocument); override;
                    published
                     property Strength:TPasGLTFFloat read fStrength write fStrength;
                   end;
                   TPBRMetallicRoughness=class(TBaseExtensionsExtrasObject)
                    private
                     fBaseColorFactor:TVector4;
                     fBaseColorTexture:TTexture;
                     fRoughnessFactor:TPasGLTFFloat;
                     fMetallicFactor:TPasGLTFFloat;
                     fMetallicRoughnessTexture:TTexture;
                     function GetEmpty:boolean;
                    public
                     constructor Create(const aDocument:TDocument); override;
                     destructor Destroy; override;
                    public
                     property BaseColorFactor:TVector4 read fBaseColorFactor write fBaseColorFactor;
                    published
                     property BaseColorTexture:TTexture read fBaseColorTexture;
                     property RoughnessFactor:TPasGLTFFloat read fRoughnessFactor write fRoughnessFactor;
                     property MetallicFactor:TPasGLTFFloat read fMetallicFactor write fMetallicFactor;
                     property MetallicRoughnessTexture:TTexture read fMetallicRoughnessTexture;
                     property Empty:boolean read GetEmpty;
                   end;
             private
              fName:TPasGLTFUTF8String;
              fAlphaCutOff:TPasGLTFFloat;
              fAlphaMode:TAlphaMode;
              fDoubleSided:boolean;
              fNormalTexture:TNormalTexture;
              fOcclusionTexture:TOcclusionTexture;
              fPBRMetallicRoughness:TPBRMetallicRoughness;
              fEmissiveTexture:TTexture;
              fEmissiveFactor:TVector3;
             public
              constructor Create(const aDocument:TDocument); override;
              destructor Destroy; override;
             public
              property EmissiveFactor:TVector3 read fEmissiveFactor write fEmissiveFactor;
             published
              property Name:TPasGLTFUTF8String read fName write fName;
              property AlphaCutOff:TPasGLTFFloat read fAlphaCutOff write fAlphaCutOff;
              property AlphaMode:TAlphaMode read fAlphaMode write fAlphaMode;
              property DoubleSided:boolean read fDoubleSided write fDoubleSided;
              property NormalTexture:TNormalTexture read fNormalTexture;
              property OcclusionTexture:TOcclusionTexture read fOcclusionTexture;
              property PBRMetallicRoughness:TPBRMetallicRoughness read fPBRMetallicRoughness;
              property EmissiveTexture:TTexture read fEmissiveTexture;
            end;
            TMaterials=TPasGLTFObjectList<TMaterial>;
            TMesh=class(TBaseExtensionsExtrasObject)
             public
              type TPrimitive=class(TBaseExtensionsExtrasObject)
                    public
                     type TMode=
                           (
                            Points=0,
                            Lines=1,
                            LineLoop=2,
                            LineStrip=3,
                            Triangles=4,
                            TriangleStrip=5,
                            TriangleFan=6
                           );
                          PMode=^TMode;
                    private
                     fMode:TMode;
                     fIndices:TPasGLTFSizeInt;
                     fMaterial:TPasGLTFSizeInt;
                     fAttributes:TAttributes;
                     fTargets:TAttributesList;
                    public
                    constructor Create(const aDocument:TDocument); override;
                     destructor Destroy; override;
                    published
                     property Mode:TMode read fMode write fMode;
                     property Indices:TPasGLTFSizeInt read fIndices write fIndices;
                     property Material:TPasGLTFSizeInt read fMaterial write fMaterial;
                     property Attributes:TAttributes read fAttributes;
                     property Targets:TAttributesList read fTargets;
                   end;
                   TPrimitives=TPasGLTFObjectList<TPrimitive>;
                   TWeights=TPasGLTFDynamicArray<TPasGLTFFloat>;
             private
              fName:TPasGLTFUTF8String;
              fWeights:TWeights;
              fPrimitives:TPrimitives;
             public
              constructor Create(const aDocument:TDocument); override;
              destructor Destroy; override;
             public
             published
              property Name:TPasGLTFUTF8String read fName write fName;
              property Weights:TWeights read fWeights;
              property Primitives:TPrimitives read fPrimitives;
            end;
            TMeshes=TPasGLTFObjectList<TMesh>;
            TNode=class(TBaseExtensionsExtrasObject)
             public
              type TChildren=TPasGLTFDynamicArray<TPasGLTFSizeInt>;
                   TWeights=TPasGLTFDynamicArray<TPasGLTFFloat>;
             private
              fName:TPasGLTFUTF8String;
              fCamera:TPasGLTFSizeInt;
              fMesh:TPasGLTFSizeInt;
              fSkin:TPasGLTFSizeInt;
              fMatrix:TMatrix4x4;
              fRotation:TVector4;
              fScale:TVector3;
              fTranslation:TVector3;
              fChildren:TChildren;
              fWeights:TWeights;
             public
              constructor Create(const aDocument:TDocument); override;
              destructor Destroy; override;
             public
              property Matrix:TMatrix4x4 read fMatrix write fMatrix;
              property Rotation:TVector4 read fRotation write fRotation;
              property Scale:TVector3 read fScale write fScale;
              property Translation:TVector3 read fTranslation write fTranslation;
             published
              property Name:TPasGLTFUTF8String read fName write fName;
              property Camera:TPasGLTFSizeInt read fCamera write fCamera;
              property Mesh:TPasGLTFSizeInt read fMesh write fMesh;
              property Skin:TPasGLTFSizeInt read fSkin write fSkin;
              property Children:TChildren read fChildren;
              property Weights:TWeights read fWeights;
            end;
            TNodes=TPasGLTFObjectList<TNode>;
            TSampler=class(TBaseExtensionsExtrasObject)
             public
              type TMagFilter=
                    (
                     None=0,
                     Nearest=9728,
                     Linear=9729
                    );
                   PMagFilter=^TMagFilter;
                   TMinFilter=
                    (
                     None=0,
                     Nearest=9728,
                     Linear=9729,
                     NearestMipMapNearest=9984,
                     LinearMipMapNearest=9985,
                     NearestMipMapLinear=9986,
                     LinearMipMapLinear=9987
                    );
                   PMinFilter=^TMinFilter;
                   TWrappingMode=
                    (
                     Repeat_=10497,
                     ClampToEdge=33071,
                     MirroredRepeat=33648
                    );
                    PWrappingMode=^TWrappingMode;
             private
              fName:TPasGLTFUTF8String;
              fMagFilter:TMagFilter;
              fMinFilter:TMinFilter;
              fWrapS:TWrappingMode;
              fWrapT:TWrappingMode;
              function GetEmpty:boolean;
             public
              constructor Create(const aDocument:TDocument); override;
              destructor Destroy; override;
             published
              property Name:TPasGLTFUTF8String read fName write fName;
              property MagFilter:TMagFilter read fMagFilter write fMagFilter;
              property MinFilter:TMinFilter read fMinFilter write fMinFilter;
              property WrapS:TWrappingMode read fWrapS write fWrapS;
              property WrapT:TWrappingMode read fWrapT write fWrapT;
              property Empty:boolean read GetEmpty;
            end;
            TSamplers=TPasGLTFObjectList<TSampler>;
            TScene=class(TBaseExtensionsExtrasObject)
             public
              type TNodes=TPasGLTFDynamicArray<TPasGLTFSizeUInt>;
             private
              fName:TPasGLTFUTF8String;
              fNodes:TScene.TNodes;
             public
              constructor Create(const aDocument:TDocument); override;
              destructor Destroy; override;
             published
              property Name:TPasGLTFUTF8String read fName write fName;
              property Nodes:TScene.TNodes read fNodes;
            end;
            TScenes=TPasGLTFObjectList<TScene>;
            TSkin=class(TBaseExtensionsExtrasObject)
             public
              type TJoints=TPasGLTFDynamicArray<TPasGLTFSizeUInt>;
             private
              fName:TPasGLTFUTF8String;
              fInverseBindMatrices:TPasGLTFSizeInt;
              fSkeleton:TPasGLTFSizeInt;
              fJoints:TSkin.TJoints;
             public
              constructor Create(const aDocument:TDocument); override;
              destructor Destroy; override;
             published
              property Name:TPasGLTFUTF8String read fName write fName;
              property InverseBindMatrices:TPasGLTFSizeInt read fInverseBindMatrices write fInverseBindMatrices;
              property Skeleton:TPasGLTFSizeInt read fSkeleton write fSkeleton;
              property Joints:TSkin.TJoints read fJoints;
            end;
            TSkins=TPasGLTFObjectList<TSkin>;
            TTexture=class(TBaseExtensionsExtrasObject)
             private
              fName:TPasGLTFUTF8String;
              fSampler:TPasGLTFSizeInt;
              fSource:TPasGLTFSizeInt;
             public
              constructor Create(const aDocument:TDocument); override;
              destructor Destroy; override;
             published
              property Name:TPasGLTFUTF8String read fName write fName;
              property Sampler:TPasGLTFSizeInt read fSampler write fSampler;
              property Source:TPasGLTFSizeInt read fSource write fSource;
            end;
            TTextures=TPasGLTFObjectList<TTexture>;
            TDocument=class(TBaseExtensionsExtrasObject)
             public
              type TGetURI=function(const aURI:TPasGLTFUTF8String):TStream of object;
             private
              fAsset:TAsset;
              fAccessors:TAccessors;
              fAnimations:TAnimations;
              fBuffers:TBuffers;
              fBufferViews:TBufferViews;
              fCameras:TCameras;
              fImages:TImages;
              fMaterials:TMaterials;
              fMeshes:TMeshes;
              fNodes:TNodes;
              fSamplers:TSamplers;
              fScene:TPasGLTFSizeInt;
              fScenes:TScenes;
              fSkins:TSkins;
              fTextures:TTextures;
              fExtensionsUsed:TStringList;
              fExtensionsRequired:TStringList;
              fRootPath:TPasGLTFUTF8String;
              fGetURI:TGetURI;
              function DefaultGetURI(const aURI:TPasGLTFUTF8String):TStream;
              procedure LoadURISource(const aURI:TPasGLTFUTF8String;const aStream:TStream);
              procedure LoadURISources;
             public
              constructor Create(const aDocument:TDocument=nil); override;
              destructor Destroy; override;
              procedure LoadFromJSON(const aJSONRootItem:TPasJSONItem);
              procedure LoadFromBinary(const aStream:TStream);
              procedure LoadFromStream(const aStream:TStream);
              function SaveToJSON(const aFormatted:boolean=false):TPasJSONRawByteString;
              procedure SaveToBinary(const aStream:TStream);
              procedure SaveToStream(const aStream:TStream;const aBinary:boolean=false;const aFormatted:boolean=false);
             published
              property Asset:TAsset read fAsset;
              property Accessors:TAccessors read fAccessors;
              property Animations:TAnimations read fAnimations;
              property Buffers:TBuffers read fBuffers;
              property BufferViews:TBufferViews read fBufferViews;
              property Cameras:TCameras read fCameras;
              property Images:TImages read fImages;
              property Materials:TMaterials read fMaterials;
              property Meshes:TMeshes read fMeshes;
              property Nodes:TNodes read fNodes;
              property Samplers:TSamplers read fSamplers;
              property Scene:TPasGLTFSizeInt read fScene write fScene;
              property Scenes:TScenes read fScenes;
              property Skins:TSkins read fSkins;
              property Textures:TTextures read fTextures;
              property ExtensionsUsed:TStringList read fExtensionsUsed;
              property ExtensionsRequired:TStringList read fExtensionsRequired;
              property RootPath:TPasGLTFUTF8String read fRootPath write fRootPath;
              property GetURI:TGetURI read fGetURI write fGetURI;
            end;
      public
       class function ResolveURIToPath(const aURI:TPasGLTFUTF8String):TPasGLTFUTF8String; static;
     end;

implementation

{$ifndef fpc}
class function TPasGLTFTypedSort<T>.BSRDWord(aValue:TPasGLTFUInt32):TPasGLTFInt32;
const BSRDebruijn32Multiplicator=TPasGLTFUInt32($07c4acdd);
      BSRDebruijn32Shift=27;
      BSRDebruijn32Mask=31;
      BSRDebruijn32Table:array[0..31] of TPasGLTFInt32=(0,9,1,10,13,21,2,29,11,14,16,18,22,25,3,30,8,12,20,28,15,17,24,7,19,27,23,6,26,5,4,31);
var Value:TPasGLTFUInt32;
begin
 if aValue=0 then begin
  result:=255;
 end else begin
  Value:=aValue or (aValue shr 1);
  Value:=Value or (Value shr 2);
  Value:=Value or (Value shr 4);
  Value:=Value or (Value shr 8);
  Value:=Value or (Value shr 16);
  result:=BSRDebruijn32Table[((Value*BSRDebruijn32Multiplicator) shr BSRDebruijn32Shift) and BSRDebruijn32Mask];
 end;
end;
{$endif}

class procedure TPasGLTFTypedSort<T>.IntroSort(const pItems:TPasGLTFPointer;const pLeft,pRight:TPasGLTFSizeInt;const pCompareFunc:TPasGLTFTypedSortCompareFunction);
type TItem=T;
     PItem=^TItem;
     TItemArray=array[0..65535] of TItem;
     PItemArray=^TItemArray;
var Left,Right,i,j,Middle,Size,Parent,Child,Pivot,iA,iB,iC:TPasGLTFSizeInt;
    Depth:TPasGLTFInt32;
    StackItem:PStackItem;
    Stack:array[0..31] of TStackItem;
    Temp:T;
begin
 if pLeft<pRight then begin
  StackItem:=@Stack[0];
  StackItem^.Left:=pLeft;
  StackItem^.Right:=pRight;
  if (TPasGLTFInt64(pRight)-TPasGLTFInt64(pLeft))<=TPasGLTFInt64(High(TPasGLTFUInt32)) then begin
   StackItem^.Depth:=BSRDWord((pRight-pLeft)+1) shl 1;
   if StackItem^.Depth>31 then begin
    StackItem^.Depth:=31;
   end;
  end else begin
   StackItem^.Depth:=31;
  end;
  inc(StackItem);
  while {%H-}TPasGLTFPtrUInt(TPasGLTFPointer(StackItem))>TPasGLTFPtrUInt(TPasGLTFPointer(@Stack[0])) do begin
   dec(StackItem);
   Left:=StackItem^.Left;
   Right:=StackItem^.Right;
   Depth:=StackItem^.Depth;
   Size:=(Right-Left)+1;
   if Size<16 then begin
    // Insertion sort
    iA:=Left;
    iB:=iA+1;
    while iB<=Right do begin
     iC:=iB;
     while (iA>=Left) and
           (iC>=Left) and
           (pCompareFunc(PItemArray(pItems)^[iA],PItemArray(pItems)^[iC])>0) do begin
      Temp:=PItemArray(pItems)^[iA];
      PItemArray(pItems)^[iA]:=PItemArray(pItems)^[iC];
      PItemArray(pItems)^[iC]:=Temp;
      dec(iA);
      dec(iC);
     end;
     iA:=iB;
     inc(iB);
    end;
   end else begin
    if (Depth=0) or ({%H-}TPasGLTFPtrUInt(TPasGLTFPointer(StackItem))>=TPasGLTFPtrUInt(TPasGLTFPointer(@Stack[high(Stack)-1]))) then begin
     // Heap sort
     i:=Size div 2;
     repeat
      if i>0 then begin
       dec(i);
      end else begin
       dec(Size);
       if Size>0 then begin
        Temp:=PItemArray(pItems)^[Left+Size];
        PItemArray(pItems)^[Left+Size]:=PItemArray(pItems)^[Left];
        PItemArray(pItems)^[Left]:=Temp;
       end else begin
        break;
       end;
      end;
      Parent:=i;
      repeat
       Child:=(Parent*2)+1;
       if Child<Size then begin
        if (Child<(Size-1)) and (pCompareFunc(PItemArray(pItems)^[Left+Child],PItemArray(pItems)^[Left+Child+1])<0) then begin
         inc(Child);
        end;
        if pCompareFunc(PItemArray(pItems)^[Left+Parent],PItemArray(pItems)^[Left+Child])<0 then begin
         Temp:=PItemArray(pItems)^[Left+Parent];
         PItemArray(pItems)^[Left+Parent]:=PItemArray(pItems)^[Left+Child];
         PItemArray(pItems)^[Left+Child]:=Temp;
         Parent:=Child;
         continue;
        end;
       end;
       break;
      until false;
     until false;
    end else begin
     // Quick sort width median-of-three optimization
     Middle:=Left+((Right-Left) shr 1);
     if (Right-Left)>3 then begin
      if pCompareFunc(PItemArray(pItems)^[Left],PItemArray(pItems)^[Middle])>0 then begin
       Temp:=PItemArray(pItems)^[Left];
       PItemArray(pItems)^[Left]:=PItemArray(pItems)^[Middle];
       PItemArray(pItems)^[Middle]:=Temp;
      end;
      if pCompareFunc(PItemArray(pItems)^[Left],PItemArray(pItems)^[Right])>0 then begin
       Temp:=PItemArray(pItems)^[Left];
       PItemArray(pItems)^[Left]:=PItemArray(pItems)^[Right];
       PItemArray(pItems)^[Right]:=Temp;
      end;
      if pCompareFunc(PItemArray(pItems)^[Middle],PItemArray(pItems)^[Right])>0 then begin
       Temp:=PItemArray(pItems)^[Middle];
       PItemArray(pItems)^[Middle]:=PItemArray(pItems)^[Right];
       PItemArray(pItems)^[Right]:=Temp;
      end;
     end;
     Pivot:=Middle;
     i:=Left;
     j:=Right;
     repeat
      while (i<Right) and (pCompareFunc(PItemArray(pItems)^[i],PItemArray(pItems)^[Pivot])<0) do begin
       inc(i);
      end;
      while (j>=i) and (pCompareFunc(PItemArray(pItems)^[j],PItemArray(pItems)^[Pivot])>0) do begin
       dec(j);
      end;
      if i>j then begin
       break;
      end else begin
       if i<>j then begin
        Temp:=PItemArray(pItems)^[i];
        PItemArray(pItems)^[i]:=PItemArray(pItems)^[j];
        PItemArray(pItems)^[j]:=Temp;
        if Pivot=i then begin
         Pivot:=j;
        end else if Pivot=j then begin
         Pivot:=i;
        end;
       end;
       inc(i);
       dec(j);
      end;
     until false;
     if i<Right then begin
      StackItem^.Left:=i;
      StackItem^.Right:=Right;
      StackItem^.Depth:=Depth-1;
      inc(StackItem);
     end;
     if Left<j then begin
      StackItem^.Left:=Left;
      StackItem^.Right:=j;
      StackItem^.Depth:=Depth-1;
      inc(StackItem);
     end;
    end;
   end;
  end;
 end;
end;

constructor TPasGLTFDynamicArray<T>.TValueEnumerator.Create(const aDynamicArray:TPasGLTFDynamicArray<T>);
begin
 fDynamicArray:=aDynamicArray;
 fIndex:=-1;
end;

function TPasGLTFDynamicArray<T>.TValueEnumerator.MoveNext:boolean;
begin
 inc(fIndex);
 result:=fIndex<fDynamicArray.fCount;
end;

function TPasGLTFDynamicArray<T>.TValueEnumerator.GetCurrent:T;
begin
 result:=fDynamicArray.fItems[fIndex];
end;

constructor TPasGLTFDynamicArray<T>.Create;
begin
 fItems:=nil;
 fCount:=0;
 fAllocated:=0;
 inherited Create;
end;

destructor TPasGLTFDynamicArray<T>.Destroy;
begin
 SetLength(fItems,0);
 fCount:=0;
 fAllocated:=0;
 inherited Destroy;
end;

procedure TPasGLTFDynamicArray<T>.Clear;
begin
 SetLength(fItems,0);
 fCount:=0;
 fAllocated:=0;
end;

procedure TPasGLTFDynamicArray<T>.SetCount(const pNewCount:TPasGLTFSizeInt);
begin
 if pNewCount<=0 then begin
  SetLength(fItems,0);
  fCount:=0;
  fAllocated:=0;
 end else begin
  if pNewCount<fCount then begin
   fCount:=pNewCount;
   if (fCount+fCount)<fAllocated then begin
    fAllocated:=fCount+fCount;
    SetLength(fItems,fAllocated);
   end;
  end else begin
   fCount:=pNewCount;
   if fAllocated<fCount then begin
    fAllocated:=fCount+fCount;
    SetLength(fItems,fAllocated);
   end;
  end;
 end;
end;

function TPasGLTFDynamicArray<T>.GetItem(const pIndex:TPasGLTFSizeInt):T;
begin
 result:=fItems[pIndex];
end;

procedure TPasGLTFDynamicArray<T>.SetItem(const pIndex:TPasGLTFSizeInt;const pItem:T);
begin
 fItems[pIndex]:=pItem;
end;

function TPasGLTFDynamicArray<T>.Add(const pItem:T):TPasGLTFSizeInt;
begin
 result:=fCount;
 inc(fCount);
 if fAllocated<fCount then begin
  fAllocated:=fCount+fCount;
  SetLength(fItems,fAllocated);
 end;
 fItems[result]:=pItem;
end;

function TPasGLTFDynamicArray<T>.Add(const pItems:array of T):TPasGLTFSizeInt;
var Index:TPasGLTFSizeInt;
begin
 result:=fCount;
 if length(pItems)>0 then begin
  inc(fCount,length(pItems));
  if fAllocated<fCount then begin
   fAllocated:=fCount+fCount;
   SetLength(fItems,fAllocated);
  end;
  for Index:=0 to length(pItems)-1 do begin
   fItems[result+Index]:=pItems[Index];
  end;
 end;
end;

procedure TPasGLTFDynamicArray<T>.Insert(const pIndex:TPasGLTFSizeInt;const pItem:T);
begin
 if pIndex>=0 then begin
  if pIndex<fCount then begin
   inc(fCount);
   if fCount<fAllocated then begin
    fAllocated:=fCount shl 1;
    SetLength(fItems,fAllocated);
   end;
   Move(fItems[pIndex],fItems[pIndex+1],(fCount-pIndex)*SizeOf(T));
   FillChar(fItems[pIndex],SizeOf(T),#0);
  end else begin
   fCount:=pIndex+1;
   if fCount<fAllocated then begin
    fAllocated:=fCount shl 1;
    SetLength(fItems,fAllocated);
   end;
  end;
  fItems[pIndex]:=pItem;
 end;
end;

procedure TPasGLTFDynamicArray<T>.Delete(const pIndex:TPasGLTFSizeInt);
begin
 Finalize(fItems[pIndex]);
 Move(fItems[pIndex+1],fItems[pIndex],(fCount-pIndex)*SizeOf(T));
 dec(fCount);
 FillChar(fItems[fCount],SizeOf(T),#0);
 if fCount<(fAllocated shr 1) then begin
  fAllocated:=fAllocated shr 1;
  SetLength(fItems,fAllocated);
 end;
end;

procedure TPasGLTFDynamicArray<T>.Exchange(const pIndex,pWithIndex:TPasGLTFSizeInt);
var Temporary:T;
begin
 Temporary:=fItems[pIndex];
 fItems[pIndex]:=fItems[pWithIndex];
 fItems[pWithIndex]:=Temporary;
end;

function TPasGLTFDynamicArray<T>.Memory:TPasGLTFPointer;
begin
 result:=@fItems[0];
end;

function TPasGLTFDynamicArray<T>.GetEnumerator:TPasGLTFDynamicArray<T>.TValueEnumerator;
begin
 result:=TValueEnumerator.Create(self);
end;

constructor TPasGLTFObjectList<T>.TValueEnumerator.Create(const aObjectList:TPasGLTFObjectList<T>);
begin
 fObjectList:=aObjectList;
 fIndex:=-1;
end;

function TPasGLTFObjectList<T>.TValueEnumerator.MoveNext:boolean;
begin
 inc(fIndex);
 result:=fIndex<fObjectList.fCount;
end;

function TPasGLTFObjectList<T>.TValueEnumerator.GetCurrent:T;
begin
 result:=fObjectList.fItems[fIndex];
end;

constructor TPasGLTFObjectList<T>.Create;
begin
 inherited Create;
 fItems:=nil;
 fCount:=0;
 fAllocated:=0;
 fOwnsObjects:=true;
end;

destructor TPasGLTFObjectList<T>.Destroy;
begin
 Clear;
 inherited Destroy;
end;

function TPasGLTFObjectList<T>.RoundUpToPowerOfTwoSizeUInt(x:TPasGLTFSizeUInt):TPasGLTFSizeUInt;
begin
 dec(x);
 x:=x or (x shr 1);
 x:=x or (x shr 2);
 x:=x or (x shr 4);
 x:=x or (x shr 8);
 x:=x or (x shr 16);
{$ifdef CPU64}
 x:=x or (x shr 32);
{$endif}
 result:=x+1;
end;

procedure TPasGLTFObjectList<T>.Clear;
var Index:TPasGLTFSizeInt;
begin
 if fOwnsObjects then begin
  for Index:=fCount-1 downto 0 do begin
   FreeAndNil(fItems[Index]);
  end;
 end;
 fItems:=nil;
 fCount:=0;
 fAllocated:=0;
end;

procedure TPasGLTFObjectList<T>.SetCount(const pNewCount:TPasGLTFSizeInt);
var Index,NewAllocated:TPasGLTFSizeInt;
begin
 if fCount<pNewCount then begin
  NewAllocated:=RoundUpToPowerOfTwoSizeUInt(pNewCount);
  if fAllocated<NewAllocated then begin
   SetLength(fItems,NewAllocated);
   FillChar(fItems[fAllocated],(NewAllocated-fAllocated)*SizeOf(T),#0);
   fAllocated:=NewAllocated;
  end;
  FillChar(fItems[fCount],(pNewCount-fCount)*SizeOf(T),#0);
  fCount:=pNewCount;
 end else if fCount>pNewCount then begin
  if fOwnsObjects then begin
   for Index:=fCount-1 downto pNewCount do begin
    FreeAndNil(fItems[Index]);
   end;
  end;
  fCount:=pNewCount;
  if pNewCount<(fAllocated shr 2) then begin
   if pNewCount=0 then begin
    fItems:=nil;
    fAllocated:=0;
   end else begin
    NewAllocated:=fAllocated shr 1;
    SetLength(fItems,NewAllocated);
    fAllocated:=NewAllocated;
   end;
  end;
 end;
end;

function TPasGLTFObjectList<T>.GetItem(const pIndex:TPasGLTFSizeInt):T;
begin
 if (pIndex<0) or (pIndex>=fCount) then begin
  raise ERangeError.Create('Out of index range');
 end;
 result:=fItems[pIndex];
end;

procedure TPasGLTFObjectList<T>.SetItem(const pIndex:TPasGLTFSizeInt;const pItem:T);
begin
 if (pIndex<0) or (pIndex>=fCount) then begin
  raise ERangeError.Create('Out of index range');
 end;
 fItems[pIndex]:=pItem;
end;

function TPasGLTFObjectList<T>.IndexOf(const pItem:T):TPasGLTFSizeInt;
var Index:TPasGLTFSizeInt;
begin
 for Index:=0 to fCount-1 do begin
  if fItems[Index]=pItem then begin
   result:=Index;
   exit;
  end;
 end;
 result:=-1;
end;

function TPasGLTFObjectList<T>.Add(const pItem:T):TPasGLTFSizeInt;
begin
 result:=fCount;
 inc(fCount);
 if fAllocated<fCount then begin
  fAllocated:=fCount+fCount;
  SetLength(fItems,fAllocated);
 end;
 fItems[result]:=pItem;
end;

procedure TPasGLTFObjectList<T>.Insert(const pIndex:TPasGLTFSizeInt;const pItem:T);
var
  OldCount:TPasGLTFSizeInt;
  MoveSrc, MoveDst: Pointer;
begin
 if pIndex>=0 then begin
  OldCount:=fCount;
  if fCount<pIndex then begin
   fCount:=pIndex+1;
  end else begin
   inc(fCount);
  end;
  if fAllocated<fCount then begin
   fAllocated:=fCount shl 1;
   SetLength(fItems,fAllocated);
  end;
  if OldCount<fCount then begin
   FillChar(fItems[OldCount],(fCount-OldCount)*SizeOf(T),#0);
  end;
  if pIndex<OldCount then begin
   // TODO: should be fixed in FPC ( https://bugs.freepascal.org/view.php?id=34893 ), retest.
   //
   // Use MoveSrc/Dst to workaround FPC 3.3.1 error:
   // Error: Incompatible type for arg no. 1: Got "$gendef124", expected "<Formal type>"
   // (FPC 3.3.1-r40366 [2018/11/24], on Linux/x86_64).
   MoveSrc := @(fItems[pIndex]);
   MoveDst := @(fItems[pIndex+1]);
   System.Move(MoveSrc^, MoveDst^, (OldCount-pIndex)*SizeOf(T));
   FillChar(fItems[pIndex],SizeOf(T),#0);
  end;
  fItems[pIndex]:=pItem;
 end;
end;

procedure TPasGLTFObjectList<T>.Delete(const pIndex:TPasGLTFSizeInt);
var
  Old:T;
  MoveSrc, MoveDst: Pointer;
begin
 if (pIndex<0) or (pIndex>=fCount) then begin
  raise ERangeError.Create('Out of index range');
 end;
 Old:=fItems[pIndex];
 dec(fCount);
 FillChar(fItems[pIndex],SizeOf(T),#0);
 if pIndex<>fCount then begin
  // TODO: should be fixed in FPC ( https://bugs.freepascal.org/view.php?id=34893 ), retest.
  //
  // Use MoveSrc/Dst to workaround FPC 3.3.1 error:
  // Error: Incompatible type for arg no. 1: Got "$gendef124", expected "<Formal type>"
  // (FPC 3.3.1-r40366 [2018/11/24], on Linux/x86_64).
  MoveSrc := @(fItems[pIndex+1]);
  MoveDst := @(fItems[pIndex]);
  System.Move(MoveSrc, MoveDst, (fCount-pIndex)*SizeOf(T));
  FillChar(fItems[fCount],SizeOf(T),#0);
 end;
 if fCount<(fAllocated shr 1) then begin
  fAllocated:=fAllocated shr 1;
  SetLength(fItems,fAllocated);
 end;
 if fOwnsObjects then begin
  FreeAndNil(Old);
 end;
end;

procedure TPasGLTFObjectList<T>.Remove(const pItem:T);
var Index:TPasGLTFSizeInt;
begin
 Index:=IndexOf(pItem);
 if Index>=0 then begin
  Delete(Index);
 end;
end;

procedure TPasGLTFObjectList<T>.Exchange(const pIndex,pWithIndex:TPasGLTFSizeInt);
var Temporary:T;
begin
 if ((pIndex<0) or (pIndex>=fCount)) or ((pWithIndex<0) or (pWithIndex>=fCount)) then begin
  raise ERangeError.Create('Out of index range');
 end;
 Temporary:=fItems[pIndex];
 fItems[pIndex]:=fItems[pWithIndex];
 fItems[pWithIndex]:=Temporary;
end;

function TPasGLTFObjectList<T>.GetEnumerator:TPasGLTFObjectList<T>.TValueEnumerator;
begin
 result:=TValueEnumerator.Create(self);
end;

constructor TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>.TPasGLTFHashMapEntityEnumerator.Create(const aHashMap:TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>);
begin
 fHashMap:=aHashMap;
 fIndex:=-1;
end;

function TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>.TPasGLTFHashMapEntityEnumerator.GetCurrent:TPasGLTFHashMapEntity;
begin
 result:=fHashMap.fEntities[fIndex];
end;

function TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>.TPasGLTFHashMapEntityEnumerator.MoveNext:boolean;
begin
 repeat
  inc(fIndex);
  if fIndex<fHashMap.fSize then begin
   if fHashMap.fEntityToCellIndex[fIndex]>=0 then begin
    result:=true;
    exit;
   end;
  end else begin
   break;
  end;
 until false;
 result:=false;
end;

constructor TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>.TPasGLTFHashMapKeyEnumerator.Create(const aHashMap:TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>);
begin
 fHashMap:=aHashMap;
 fIndex:=-1;
end;

function TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>.TPasGLTFHashMapKeyEnumerator.GetCurrent:TPasGLTFHashMapKey;
begin
 result:=fHashMap.fEntities[fIndex].Key;
end;

function TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>.TPasGLTFHashMapKeyEnumerator.MoveNext:boolean;
begin
 repeat
  inc(fIndex);
  if fIndex<fHashMap.fSize then begin
   if fHashMap.fEntityToCellIndex[fIndex]>=0 then begin
    result:=true;
    exit;
   end;
  end else begin
   break;
  end;
 until false;
 result:=false;
end;

constructor TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>.TPasGLTFHashMapValueEnumerator.Create(const aHashMap:TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>);
begin
 fHashMap:=aHashMap;
 fIndex:=-1;
end;

function TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>.TPasGLTFHashMapValueEnumerator.GetCurrent:TPasGLTFHashMapValue;
begin
 result:=fHashMap.fEntities[fIndex].Value;
end;

function TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>.TPasGLTFHashMapValueEnumerator.MoveNext:boolean;
begin
 repeat
  inc(fIndex);
  if fIndex<fHashMap.fSize then begin
   if fHashMap.fEntityToCellIndex[fIndex]>=0 then begin
    result:=true;
    exit;
   end;
  end else begin
   break;
  end;
 until false;
 result:=false;
end;

constructor TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>.TPasGLTFHashMapEntitiesObject.Create(const aOwner:TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>);
begin
 inherited Create;
 fOwner:=aOwner;
end;

function TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>.TPasGLTFHashMapEntitiesObject.GetEnumerator:TPasGLTFHashMapEntityEnumerator;
begin
 result:=TPasGLTFHashMapEntityEnumerator.Create(fOwner);
end;

constructor TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>.TPasGLTFHashMapKeysObject.Create(const aOwner:TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>);
begin
 inherited Create;
 fOwner:=aOwner;
end;

function TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>.TPasGLTFHashMapKeysObject.GetEnumerator:TPasGLTFHashMapKeyEnumerator;
begin
 result:=TPasGLTFHashMapKeyEnumerator.Create(fOwner);
end;

constructor TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>.TPasGLTFHashMapValuesObject.Create(const aOwner:TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>);
begin
 inherited Create;
 fOwner:=aOwner;
end;

function TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>.TPasGLTFHashMapValuesObject.GetEnumerator:TPasGLTFHashMapValueEnumerator;
begin
 result:=TPasGLTFHashMapValueEnumerator.Create(fOwner);
end;

function TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>.TPasGLTFHashMapValuesObject.GetValue(const Key:TPasGLTFHashMapKey):TPasGLTFHashMapValue;
begin
 result:=fOwner.GetValue(Key);
end;

procedure TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>.TPasGLTFHashMapValuesObject.SetValue(const Key:TPasGLTFHashMapKey;const aValue:TPasGLTFHashMapValue);
begin
 fOwner.SetValue(Key,aValue);
end;

constructor TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>.Create(const DefaultValue:TPasGLTFHashMapValue);
begin
 inherited Create;
 fRealSize:=0;
 fLogSize:=0;
 fSize:=0;
 fEntities:=nil;
 fEntityToCellIndex:=nil;
 fCellToEntityIndex:=nil;
 fDefaultValue:=DefaultValue;
 fCanShrink:=true;
 fEntitiesObject:=TPasGLTFHashMapEntitiesObject.Create(self);
 fKeysObject:=TPasGLTFHashMapKeysObject.Create(self);
 fValuesObject:=TPasGLTFHashMapValuesObject.Create(self);
 Resize;
end;

destructor TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>.Destroy;
var Counter:TPasGLTFSizeInt;
begin
 Clear;
 for Counter:=0 to length(fEntities)-1 do begin
  Finalize(fEntities[Counter].Key);
  Finalize(fEntities[Counter].Value);
 end;
 SetLength(fEntities,0);
 SetLength(fEntityToCellIndex,0);
 SetLength(fCellToEntityIndex,0);
 FreeAndNil(fEntitiesObject);
 FreeAndNil(fKeysObject);
 FreeAndNil(fValuesObject);
 inherited Destroy;
end;

procedure TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>.Clear;
var Counter:TPasGLTFSizeInt;
begin
 for Counter:=0 to length(fEntities)-1 do begin
  Finalize(fEntities[Counter].Key);
  Finalize(fEntities[Counter].Value);
 end;
 if fCanShrink then begin
  fRealSize:=0;
  fLogSize:=0;
  fSize:=0;
  SetLength(fEntities,0);
  SetLength(fEntityToCellIndex,0);
  SetLength(fCellToEntityIndex,0);
  Resize;
 end else begin
  for Counter:=0 to length(fCellToEntityIndex)-1 do begin
   fCellToEntityIndex[Counter]:=ENT_EMPTY;
  end;
  for Counter:=0 to length(fEntityToCellIndex)-1 do begin
   fEntityToCellIndex[Counter]:=CELL_EMPTY;
  end;
 end;
end;

function TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>.HashData(const Data:TPasGLTFPointer;const DataLength:TPasGLTFSizeUInt):TPasGLTFUInt32;
// xxHash32
const PRIME32_1=TPasGLTFUInt32(2654435761);
      PRIME32_2=TPasGLTFUInt32(2246822519);
      PRIME32_3=TPasGLTFUInt32(3266489917);
      PRIME32_4=TPasGLTFUInt32(668265263);
      PRIME32_5=TPasGLTFUInt32(374761393);
      Seed=TPasGLTFUInt32($1337c0d3);
      v1Initialization=TPasGLTFUInt32(TPasGLTFUInt64(TPasGLTFUInt64(Seed)+TPasGLTFUInt64(PRIME32_1)+TPasGLTFUInt64(PRIME32_2)));
      v2Initialization=TPasGLTFUInt32(TPasGLTFUInt64(TPasGLTFUInt64(Seed)+TPasGLTFUInt64(PRIME32_2)));
      v3Initialization=TPasGLTFUInt32(TPasGLTFUInt64(TPasGLTFUInt64(Seed)+TPasGLTFUInt64(0)));
      v4Initialization=TPasGLTFUInt32(TPasGLTFUInt64(TPasGLTFInt64(TPasGLTFInt64(Seed)-TPasGLTFInt64(PRIME32_1))));
      HashInitialization=TPasGLTFUInt32(TPasGLTFUInt64(TPasGLTFUInt64(Seed)+TPasGLTFUInt64(PRIME32_5)));
var v1,v2,v3,v4:TPasGLTFUInt32;
    p,e:PPasGLTFUInt8;
begin
 p:=Data;
 if DataLength>=16 then begin
  v1:=v1Initialization;
  v2:=v2Initialization;
  v3:=v3Initialization;
  v4:=v4Initialization;
  e:=@PPasGLTFUInt8Array(Data)^[DataLength-16];
  repeat
{$if defined(fpc) or declared(ROLDWord)}
   v1:=ROLDWord(v1+(TPasGLTFUInt32(TPasGLTFPointer(p)^)*TPasGLTFUInt32(PRIME32_2)),13)*TPasGLTFUInt32(PRIME32_1);
{$else}
   inc(v1,TPasGLTFUInt32(TPasGLTFPointer(p)^)*TPasGLTFUInt32(PRIME32_2));
   v1:=((v1 shl 13) or (v1 shr 19))*TPasGLTFUInt32(PRIME32_1);
{$ifend}
   inc(p,SizeOf(TPasGLTFUInt32));
{$if defined(fpc) or declared(ROLDWord)}
   v2:=ROLDWord(v2+(TPasGLTFUInt32(TPasGLTFPointer(p)^)*TPasGLTFUInt32(PRIME32_2)),13)*TPasGLTFUInt32(PRIME32_1);
{$else}
   inc(v2,TPasGLTFUInt32(TPasGLTFPointer(p)^)*TPasGLTFUInt32(PRIME32_2));
   v2:=((v2 shl 13) or (v2 shr 19))*TPasGLTFUInt32(PRIME32_1);
{$ifend}
   inc(p,SizeOf(TPasGLTFUInt32));
{$if defined(fpc) or declared(ROLDWord)}
   v3:=ROLDWord(v3+(TPasGLTFUInt32(TPasGLTFPointer(p)^)*TPasGLTFUInt32(PRIME32_2)),13)*TPasGLTFUInt32(PRIME32_1);
{$else}
   inc(v3,TPasGLTFUInt32(TPasGLTFPointer(p)^)*TPasGLTFUInt32(PRIME32_2));
   v3:=((v3 shl 13) or (v3 shr 19))*TPasGLTFUInt32(PRIME32_1);
{$ifend}
   inc(p,SizeOf(TPasGLTFUInt32));
{$if defined(fpc) or declared(ROLDWord)}
   v4:=ROLDWord(v4+(TPasGLTFUInt32(TPasGLTFPointer(p)^)*TPasGLTFUInt32(PRIME32_2)),13)*TPasGLTFUInt32(PRIME32_1);
{$else}
   inc(v4,TPasGLTFUInt32(TPasGLTFPointer(p)^)*TPasGLTFUInt32(PRIME32_2));
   v4:=((v4 shl 13) or (v4 shr 19))*TPasGLTFUInt32(PRIME32_1);
{$ifend}
   inc(p,SizeOf(TPasGLTFUInt32));
  until {%H-}TPasGLTFPtrUInt(p)>{%H-}TPasGLTFPtrUInt(e);
{$if defined(fpc) or declared(ROLDWord)}
  result:=ROLDWord(v1,1)+ROLDWord(v2,7)+ROLDWord(v3,12)+ROLDWord(v4,18);
{$else}
  result:=((v1 shl 1) or (v1 shr 31))+
          ((v2 shl 7) or (v2 shr 25))+
          ((v3 shl 12) or (v3 shr 20))+
          ((v4 shl 18) or (v4 shr 14));
{$ifend}
 end else begin
  result:=HashInitialization;
 end;
 inc(result,DataLength);
 e:=@PPasGLTFUInt8Array(Data)^[DataLength];
 while ({%H-}TPasGLTFPtrUInt(p)+SizeOf(TPasGLTFUInt32))<={%H-}TPasGLTFPtrUInt(e) do begin
{$if defined(fpc) or declared(ROLDWord)}
  result:=ROLDWord(result+(TPasGLTFUInt32(TPasGLTFPointer(p)^)*TPasGLTFUInt32(PRIME32_3)),17)*TPasGLTFUInt32(PRIME32_4);
{$else}
  inc(result,TPasGLTFUInt32(TPasGLTFPointer(p)^)*TPasGLTFUInt32(PRIME32_3));
  result:=((result shl 17) or (result shr 15))*TPasGLTFUInt32(PRIME32_4);
{$ifend}
  inc(p,SizeOf(TPasGLTFUInt32));
 end;
 while {%H-}TPasGLTFPtrUInt(p)<{%H-}TPasGLTFPtrUInt(e) do begin
{$if defined(fpc) or declared(ROLDWord)}
  result:=ROLDWord(result+(TPasGLTFUInt8(TPasGLTFPointer(p)^)*TPasGLTFUInt32(PRIME32_5)),11)*TPasGLTFUInt32(PRIME32_1);
{$else}
  inc(result,TPasGLTFUInt8(TPasGLTFPointer(p)^)*TPasGLTFUInt32(PRIME32_5));
  result:=((result shl 11) or (result shr 21))*TPasGLTFUInt32(PRIME32_1);
{$ifend}
  inc(p,SizeOf(TPasGLTFUInt8));
 end;
 result:=(result xor (result shr 15))*TPasGLTFUInt32(PRIME32_2);
 result:=(result xor (result shr 13))*TPasGLTFUInt32(PRIME32_3);
 result:=result xor (result shr 16);
end;

function TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>.HashKey(const Key:TPasGLTFHashMapKey):TPasGLTFUInt32;
begin
 result:=HashData(PPasGLTFUInt8(@Key[1]),length(Key)*SizeOf(TPasGLTFRawByteChar));
{$if defined(CPU386) or defined(CPUAMD64)}
 // Special case: The hash value may be never zero
 result:=result or (-TPasGLTFUInt32(ord(result=0) and 1));
{$else}
 if result=0 then begin
  // Special case: The hash value may be never zero
  result:=$ffffffff;
 end;
{$ifend}
end;

function TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>.CompareKey(const KeyA,KeyB:TPasGLTFHashMapKey):boolean;
begin
 result:=KeyA=KeyB;
end;

function TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>.FindCell(const Key:TPasGLTFHashMapKey):TPasGLTFUInt32;
var HashCode,Mask,Step:TPasGLTFUInt32;
    Entity:TPasGLTFInt32;
begin
 HashCode:=HashKey(Key);
 Mask:=(2 shl fLogSize)-1;
 Step:=((HashCode shl 1)+1) and Mask;
 if fLogSize<>0 then begin
  result:=HashCode shr (32-fLogSize);
 end else begin
  result:=0;
 end;
 repeat
  Entity:=fCellToEntityIndex[result];
  if (Entity=ENT_EMPTY) or ((Entity<>ENT_DELETED) and CompareKey(fEntities[Entity].Key,Key)) then begin
   exit;
  end;
  result:=(result+Step) and Mask;
 until false;
end;

procedure TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>.Resize;
var NewLogSize,NewSize,Cell,Entity:TPasGLTFInt32;
    Counter:TPasGLTFSizeInt;
    OldEntities:TPasGLTFHashMapEntities;
    OldCellToEntityIndex:TPasGLTFHashMapEntityIndices;
    OldEntityToCellIndex:TPasGLTFHashMapEntityIndices;
begin
 NewLogSize:=0;
 NewSize:=fRealSize;
 while NewSize<>0 do begin
  NewSize:=NewSize shr 1;
  inc(NewLogSize);
 end;
 if NewLogSize<1 then begin
  NewLogSize:=1;
 end;
 fSize:=0;
 fRealSize:=0;
 fLogSize:=NewLogSize;
 OldEntities:=fEntities;
 OldCellToEntityIndex:=fCellToEntityIndex;
 OldEntityToCellIndex:=fEntityToCellIndex;
 fEntities:=nil;
 fCellToEntityIndex:=nil;
 fEntityToCellIndex:=nil;
 SetLength(fEntities,2 shl fLogSize);
 SetLength(fCellToEntityIndex,2 shl fLogSize);
 SetLength(fEntityToCellIndex,2 shl fLogSize);
 for Counter:=0 to length(fCellToEntityIndex)-1 do begin
  fCellToEntityIndex[Counter]:=ENT_EMPTY;
 end;
 for Counter:=0 to length(fEntityToCellIndex)-1 do begin
  fEntityToCellIndex[Counter]:=CELL_EMPTY;
 end;
 for Counter:=0 to length(OldEntityToCellIndex)-1 do begin
  Cell:=OldEntityToCellIndex[Counter];
  if Cell>=0 then begin
   Entity:=OldCellToEntityIndex[Cell];
   if Entity>=0 then begin
    Add(OldEntities[Counter].Key,OldEntities[Counter].Value);
   end;
  end;
 end;
 for Counter:=0 to length(OldEntities)-1 do begin
  Finalize(OldEntities[Counter].Key);
  Finalize(OldEntities[Counter].Value);
 end;
 SetLength(OldEntities,0);
 SetLength(OldCellToEntityIndex,0);
 SetLength(OldEntityToCellIndex,0);
end;

function TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>.Add(const Key:TPasGLTFHashMapKey;const Value:TPasGLTFHashMapValue):PPasGLTFHashMapEntity;
var Entity:TPasGLTFInt32;
    Cell:TPasGLTFUInt32;
begin
 result:=nil;
 while fRealSize>=(1 shl fLogSize) do begin
  Resize;
 end;
 Cell:=FindCell(Key);
 Entity:=fCellToEntityIndex[Cell];
 if Entity>=0 then begin
  result:=@fEntities[Entity];
  result^.Key:=Key;
  result^.Value:=Value;
  exit;
 end;
 Entity:=fSize;
 inc(fSize);
 if Entity<(2 shl fLogSize) then begin
  fCellToEntityIndex[Cell]:=Entity;
  fEntityToCellIndex[Entity]:=Cell;
  inc(fRealSize);
  result:=@fEntities[Entity];
  result^.Key:=Key;
  result^.Value:=Value;
 end;
end;

function TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>.Get(const Key:TPasGLTFHashMapKey;const CreateIfNotExist:boolean=false):PPasGLTFHashMapEntity;
var Entity:TPasGLTFInt32;
    Cell:TPasGLTFUInt32;
    Value:TPasGLTFHashMapValue;
begin
 result:=nil;
 Cell:=FindCell(Key);
 Entity:=fCellToEntityIndex[Cell];
 if Entity>=0 then begin
  result:=@fEntities[Entity];
 end else if CreateIfNotExist then begin
  Initialize(Value);
  result:=Add(Key,Value);
 end;
end;

function TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>.TryGet(const Key:TPasGLTFHashMapKey;out Value:TPasGLTFHashMapValue):boolean;
var Entity:TPasGLTFInt32;
begin
 Entity:=fCellToEntityIndex[FindCell(Key)];
 result:=Entity>=0;
 if result then begin
  Value:=fEntities[Entity].Value;
 end else begin
  Initialize(Value);
 end;
end;

function TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>.ExistKey(const Key:TPasGLTFHashMapKey):boolean;
begin
 result:=fCellToEntityIndex[FindCell(Key)]>=0;
end;

function TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>.Delete(const Key:TPasGLTFHashMapKey):boolean;
var Entity:TPasGLTFInt32;
    Cell:TPasGLTFUInt32;
begin
 result:=false;
 Cell:=FindCell(Key);
 Entity:=fCellToEntityIndex[Cell];
 if Entity>=0 then begin
  Finalize(fEntities[Entity].Key);
  Finalize(fEntities[Entity].Value);
  fEntityToCellIndex[Entity]:=CELL_DELETED;
  fCellToEntityIndex[Cell]:=ENT_DELETED;
  result:=true;
 end;
end;

function TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>.GetValue(const Key:TPasGLTFHashMapKey):TPasGLTFHashMapValue;
var Entity:TPasGLTFInt32;
    Cell:TPasGLTFUInt32;
begin
 Cell:=FindCell(Key);
 Entity:=fCellToEntityIndex[Cell];
 if Entity>=0 then begin
  result:=fEntities[Entity].Value;
 end else begin
  result:=fDefaultValue;
 end;
end;

procedure TPasGLTFUTF8StringHashMap<TPasGLTFHashMapValue>.SetValue(const Key:TPasGLTFHashMapKey;const Value:TPasGLTFHashMapValue);
begin
 Add(Key,Value);
end;

{ TPasGLTF }

class function TPasGLTF.ResolveURIToPath(const aURI:TPasGLTFUTF8String):TPasGLTFUTF8String;
begin
 result:=TPasGLTFUTF8String(StringReplace(String(aURI),{$ifdef Windows}'/','\'{$else}'\','/'{$endif},[rfReplaceAll]));
end;

{ TPasGLTF.TBase64 }

class function TPasGLTF.TBase64.Encode(const aData;const aDataLength:TPasGLTFSizeInt):TPasGLTFRawByteString;
var Index,BitCount,OutputIndex:TPasGLTFSizeInt;
    Value:TPasGLTFUInt32;
begin
 result:='';
 if aDataLength>0 then begin
  SetLength(result,(((aDataLength*4) div 3)+3) and not 3);
  OutputIndex:=0;
  Value:=0;
  BitCount:=-6;
  for Index:=0 to aDataLength-1 do begin
   Value:=(Value shl 8) or PPasGLTFUInt8Array(@aData)^[Index];
   inc(BitCount,8);
   while BitCount>=0 do begin
    result[Low(result)+OutputIndex]:=EncodingLookUpTable[(Value shr BitCount) and 63];
    inc(OutputIndex);
    dec(BitCount,6);
   end;
  end;
  if BitCount>-6 then begin
   result[Low(result)+OutputIndex]:=EncodingLookUpTable[((Value shl 8) shr (BitCount+8)) and 63];
   inc(OutputIndex);
  end;
  while (OutputIndex and 3)<>0 do begin
   result[Low(result)+OutputIndex]:='=';
   inc(OutputIndex);
  end;
  SetLength(result,OutputIndex);
 end;
end;

class function TPasGLTF.TBase64.Encode(const aData:array of TPasGLTFUInt8):TPasGLTFRawByteString;
begin
 result:=Encode(aData[0],length(aData));
end;

class function TPasGLTF.TBase64.Encode(const aData:TPasGLTFRawByteString):TPasGLTFRawByteString;
begin
 result:=Encode(aData[Low(aData)],length(aData));
end;

class function TPasGLTF.TBase64.Encode(const aData:TStream):TPasGLTFRawByteString;
var Bytes:TPasGLTFUInt8DynamicArray;
begin
 Bytes:=nil;
 try
  SetLength(Bytes,aData.Size);
  aData.Seek(0,soBeginning);
  aData.ReadBuffer(Bytes[0],aData.Size);
  result:=Encode(Bytes[0],length(Bytes));
 finally
  Bytes:=nil;
 end;
end;

class function TPasGLTF.TBase64.Decode(const aInput:TPasGLTFRawByteString;const aOutput:TStream):boolean;
var Index,Size,BitCount,OutputIndex,
    LookUpTableValue,Remaining:TPasGLTFSizeInt;
    Value:TPasGLTFUInt32;
    Buffer:TPasGLTFUInt8DynamicArray;
begin
 result:=false;
 Buffer:=nil;
 try
  Size:=length(aInput);
  if Size>0 then begin
   if (Size and 3)=0 then begin
    result:=true;
    SetLength(Buffer,(Size*3) shr 2);
    Value:=0;
    BitCount:=-8;
    OutputIndex:=0;
    try
     for Index:=1 to Size do begin
      LookUpTableValue:=DecodingLookUpTable[aInput[Index]];
      if LookUpTableValue>=0 then begin
       Value:=(Value shl 6) or LookUpTableValue;
       inc(BitCount,6);
       while BitCount>=0 do begin
        Buffer[OutputIndex]:=(Value shr BitCount) and $ff;
        inc(OutputIndex);
        dec(BitCount,8);
       end;
      end else begin
       case aInput[Index] of
        '=':begin
         Remaining:=Size-Index;
         if (Remaining>1) or ((Remaining=1) and (aInput[Index+1]<>'=')) then begin
          result:=false;
         end;
        end;
        else begin
         result:=false;
        end;
       end;
       break;
      end;
     end;
    finally
     SetLength(Buffer,OutputIndex);
    end;
    if result then begin
     aOutput.WriteBuffer(Buffer[0],OutputIndex);
    end;
   end;
  end else begin
   result:=true;
  end;
 finally
  Buffer:=nil;
 end;
end;

{ TPasGLTF.TBaseObject }

constructor TPasGLTF.TBaseObject.Create(const aDocument:TDocument);
begin
 inherited Create;
 fDocument:=aDocument;
end;

destructor TPasGLTF.TBaseObject.Destroy;
begin
 inherited Destroy;
end;

{ TPasGLTF.TBaseExtensionsExtrasObject }

constructor TPasGLTF.TBaseExtensionsExtrasObject.Create(const aDocument:TDocument);
begin
 inherited Create(aDocument);
 fExtensions:=TPasJSONItemObject.Create;
 fExtras:=TPasJSONItemObject.Create;
end;

destructor TPasGLTF.TBaseExtensionsExtrasObject.Destroy;
begin
 FreeAndNil(fExtensions);
 FreeAndNil(fExtras);
 inherited Destroy;
end;

{ TPasGLTF.TAccessor.TComponentTypeHelper }

function TPasGLTF.TAccessor.TComponentTypeHelper.GetSize:TPasGLTFSizeInt;
begin
 case self of
  TPasGLTF.TAccessor.TComponentType.SignedByte:begin
   result:=SizeOf(TPasGLTFInt8);
  end;
  TPasGLTF.TAccessor.TComponentType.UnsignedByte:begin
   result:=SizeOf(TPasGLTFUInt8);
  end;
  TPasGLTF.TAccessor.TComponentType.SignedShort:begin
   result:=SizeOf(TPasGLTFInt16);
  end;
  TPasGLTF.TAccessor.TComponentType.UnsignedShort:begin
   result:=SizeOf(TPasGLTFUInt16);
  end;
  TPasGLTF.TAccessor.TComponentType.UnsignedInt:begin
   result:=SizeOf(TPasGLTFUInt32);
  end;
  TPasGLTF.TAccessor.TComponentType.Float:begin
   result:=SizeOf(TPasGLTFFloat);
  end;
  else {TPasGLTF.TAccessor.TComponentType.None:}begin
   result:=0;
   Assert(false);
  end;
 end;
end;

{ TPasGLTF.TAccessor.TTypeHelper }

function TPasGLTF.TAccessor.TTypeHelper.GetComponentCount:TPasGLTFSizeInt;
begin
 result:=TPasGLTF.TAccessor.TypeComponentCountTable[self];
end;

{ TPasGLTF.TAccessor.TSparse.TIndices }

constructor TPasGLTF.TAccessor.TSparse.TIndices.Create(const aDocument:TDocument);
begin
 inherited Create(aDocument);
 fComponentType:=TComponentType.None;
 fBufferView:=0;
 fByteOffset:=0;
 fEmpty:=false;
end;

destructor TPasGLTF.TAccessor.TSparse.TIndices.Destroy;
begin
 inherited Destroy;
end;

{ TPasGLTF.TAccessor.TSparse.TValues }

constructor TPasGLTF.TAccessor.TSparse.TValues.Create(const aDocument:TDocument);
begin
 inherited Create(aDocument);
 fBufferView:=0;
 fByteOffset:=0;
 fEmpty:=false;
end;

destructor TPasGLTF.TAccessor.TSparse.TValues.Destroy;
begin
 inherited Destroy;
end;

{ TPasGLTF.TAccessor.TSparse }

constructor TPasGLTF.TAccessor.TSparse.Create(const aDocument:TDocument);
begin
 inherited Create(aDocument);
 fCount:=0;
 fIndices:=TIndices.Create(fDocument);
 fValues:=TValues.Create(fDocument);
end;

destructor TPasGLTF.TAccessor.TSparse.Destroy;
begin
 FreeAndNil(fIndices);
 FreeAndNil(fValues);
 inherited Destroy;
end;

function TPasGLTF.TAccessor.TSparse.GetEmpty:boolean;
begin
 result:=fCount=0;
end;

{ TPasGLTF.TAccessor }

constructor TPasGLTF.TAccessor.Create(const aDocument:TDocument);
begin
 inherited Create(aDocument);
 fComponentType:=TComponentType.None;
 fType:=TType.None;
 fBufferView:=-1;
 fByteOffset:=0;
 fCount:=0;
 fNormalized:=TDefaults.AccessorNormalized;
 fMinArray:=TMinMaxDynamicArray.Create;
 fMaxArray:=TMinMaxDynamicArray.Create;
 fSparse:=TSparse.Create(fDocument);
end;

destructor TPasGLTF.TAccessor.Destroy;
begin
 FreeAndNil(fMinArray);
 FreeAndNil(fMaxArray);
 FreeAndNil(fSparse);
 inherited Destroy;
end;

function TPasGLTF.TAccessor.DecodeAsDoubleArray(const aForVertex:boolean=true):TPasGLTFDoubleDynamicArray;
var Index,
    ComponentIndex,
    ComponentCount,
    ComponentSize,
    ElementSize,
    SkipEvery,
    SkipBytes,
    IndicesComponentSize,
    Base:TPasGLTFSizeInt;
    Indices,
    Values:TPasGLTFDoubleDynamicArray;
begin
 ComponentCount:=fType.GetComponentCount;
 ComponentSize:=fComponentType.GetSize;
 ElementSize:=ComponentSize*ComponentCount;
 SkipEvery:=0;
 SkipBytes:=0;
 case fComponentType of
  TPasGLTF.TAccessor.TComponentType.SignedByte,
  TPasGLTF.TAccessor.TComponentType.UnsignedByte:begin
   case fType of
    TPasGLTF.TAccessor.TType.Mat2:begin
     SkipEvery:=2;
     SkipBytes:=2;
     ElementSize:=8;
    end;
    TPasGLTF.TAccessor.TType.Mat3:begin
     SkipEvery:=3;
     SkipBytes:=1;
     ElementSize:=12;
    end;
   end;
  end;
  TPasGLTF.TAccessor.TComponentType.SignedShort,
  TPasGLTF.TAccessor.TComponentType.UnsignedShort:begin
   case fType of
    TPasGLTF.TAccessor.TType.Mat3:begin
     SkipEvery:=6;
     SkipBytes:=4;
     ElementSize:=16;
    end;
   end;
  end;
 end;
 result:=nil;
 if fBufferView>=0 then begin
  if fBufferView<fDocument.fBufferViews.Count then begin
   result:=fDocument.fBufferViews[fBufferView].Decode(SkipEvery,
                                                      SkipBytes,
                                                      ElementSize,
                                                      fCount,
                                                      fType,
                                                      ComponentCount,
                                                      fComponentType,
                                                      ComponentSize,
                                                      fByteOffset,
                                                      fNormalized,
                                                      aForVertex);
  end else begin
   raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
  end;
 end else begin
  SetLength(result,ComponentCount*fCount);
  for Index:=0 to length(result)-1 do begin
   result[Index]:=0;
  end;
 end;
 if fSparse.fCount>0 then begin
  if (fSparse.fIndices.fBufferView>=0) and (fSparse.fIndices.fBufferView<fDocument.fBufferViews.Count) then begin
   IndicesComponentSize:=fSparse.fIndices.fComponentType.GetSize;
   Indices:=fDocument.fBufferViews[fSparse.fIndices.fBufferView].Decode(0,
                                                                        0,
                                                                        IndicesComponentSize,
                                                                        fSparse.fCount,
                                                                        TType.Scalar,
                                                                        1,
                                                                        fSparse.fIndices.fComponentType,
                                                                        IndicesComponentSize,
                                                                        fSparse.fIndices.fByteOffset,
                                                                        false,
                                                                        false);
   if (fSparse.fValues.fBufferView>=0) and (fSparse.fValues.fBufferView<fDocument.fBufferViews.Count) then begin
    Values:=fDocument.fBufferViews[fSparse.fValues.fBufferView].Decode(SkipEvery,
                                                                       SkipBytes,
                                                                       ElementSize,
                                                                       fSparse.fCount,
                                                                       fType,
                                                                       ComponentCount,
                                                                       fComponentType,
                                                                       ComponentSize,
                                                                       fSparse.fValues.fByteOffset,
                                                                       fNormalized,
                                                                       aForVertex);
    for Index:=0 to length(Indices)-1 do begin
     Base:=trunc(Indices[Index])*ComponentCount;
     for ComponentIndex:=0 to ComponentCount-1 do begin
      result[Base+ComponentIndex]:=Values[(Index*ComponentCount)+ComponentIndex];
     end;
    end;
   end else begin
    raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
   end;
  end else begin
   raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
  end;
 end;
end;

function TPasGLTF.TAccessor.DecodeAsInt32Array(const aForVertex:boolean):TPasGLTFInt32DynamicArray;
var Index:TPasGLTFSizeInt;
    DoubleArray:TPasGLTFDoubleDynamicArray;
begin
 result:=nil;
 DoubleArray:=DecodeAsDoubleArray(aForVertex);
 SetLength(result,length(DoubleArray));
 for Index:=0 to length(result)-1 do begin
  result[Index]:=trunc(DoubleArray[Index]);
 end;
end;

function TPasGLTF.TAccessor.DecodeAsUInt32Array(const aForVertex:boolean):TPasGLTFUInt32DynamicArray;
var Index:TPasGLTFSizeInt;
    DoubleArray:TPasGLTFDoubleDynamicArray;
begin
 result:=nil;
 DoubleArray:=DecodeAsDoubleArray(aForVertex);
 SetLength(result,length(DoubleArray));
 for Index:=0 to length(result)-1 do begin
  result[Index]:=trunc(DoubleArray[Index]);
 end;
end;

function TPasGLTF.TAccessor.DecodeAsInt64Array(const aForVertex:boolean):TPasGLTFInt64DynamicArray;
var Index:TPasGLTFSizeInt;
    DoubleArray:TPasGLTFDoubleDynamicArray;
begin
 result:=nil;
 DoubleArray:=DecodeAsDoubleArray(aForVertex);
 SetLength(result,length(DoubleArray));
 for Index:=0 to length(result)-1 do begin
  result[Index]:=trunc(DoubleArray[Index]);
 end;
end;

function TPasGLTF.TAccessor.DecodeAsUInt64Array(const aForVertex:boolean):TPasGLTFUInt64DynamicArray;
var Index:TPasGLTFSizeInt;
    DoubleArray:TPasGLTFDoubleDynamicArray;
begin
 result:=nil;
 DoubleArray:=DecodeAsDoubleArray(aForVertex);
 SetLength(result,length(DoubleArray));
 for Index:=0 to length(result)-1 do begin
  result[Index]:=trunc(DoubleArray[Index]);
 end;
end;

function TPasGLTF.TAccessor.DecodeAsFloatArray(const aForVertex:boolean):TPasGLTFFloatDynamicArray;
var Index:TPasGLTFSizeInt;
    DoubleArray:TPasGLTFDoubleDynamicArray;
begin
 result:=nil;
 DoubleArray:=DecodeAsDoubleArray(aForVertex);
 SetLength(result,length(DoubleArray));
 for Index:=0 to length(result)-1 do begin
  result[Index]:=DoubleArray[Index];
 end;
end;

function TPasGLTF.TAccessor.DecodeAsVector2Array(const aForVertex:boolean):TVector2DynamicArray;
var Index:TPasGLTFSizeInt;
    DoubleArray:TPasGLTFDoubleDynamicArray;
begin
 result:=nil;
 DoubleArray:=DecodeAsDoubleArray(aForVertex);
 Assert((length(DoubleArray) and 1)=0);
 SetLength(result,length(DoubleArray) shr 1);
 for Index:=0 to length(result)-1 do begin
  result[Index,0]:=DoubleArray[(Index shl 1) or 0];
  result[Index,1]:=DoubleArray[(Index shl 1) or 1];
 end;
end;

function TPasGLTF.TAccessor.DecodeAsVector3Array(const aForVertex:boolean):TVector3DynamicArray;
var Index:TPasGLTFSizeInt;
    DoubleArray:TPasGLTFDoubleDynamicArray;
begin
 result:=nil;
 DoubleArray:=DecodeAsDoubleArray(aForVertex);
 Assert((length(DoubleArray) mod 3)=0);
 SetLength(result,length(DoubleArray) div 3);
 for Index:=0 to length(result)-1 do begin
  result[Index,0]:=DoubleArray[(Index*3)+0];
  result[Index,1]:=DoubleArray[(Index*3)+1];
  result[Index,2]:=DoubleArray[(Index*3)+2];
 end;
end;

function TPasGLTF.TAccessor.DecodeAsVector4Array(const aForVertex:boolean):TVector4DynamicArray;
var Index:TPasGLTFSizeInt;
    DoubleArray:TPasGLTFDoubleDynamicArray;
begin
 result:=nil;
 DoubleArray:=DecodeAsDoubleArray(aForVertex);
 Assert((length(DoubleArray) and 3)=0);
 SetLength(result,length(DoubleArray) shr 2);
 for Index:=0 to length(result)-1 do begin
  result[Index,0]:=DoubleArray[(Index shl 2) or 0];
  result[Index,1]:=DoubleArray[(Index shl 2) or 1];
  result[Index,2]:=DoubleArray[(Index shl 2) or 2];
  result[Index,3]:=DoubleArray[(Index shl 2) or 3];
 end;
end;

function TPasGLTF.TAccessor.DecodeAsInt32Vector4Array(const aForVertex:boolean):TInt32Vector4DynamicArray;
var Index:TPasGLTFSizeInt;
    DoubleArray:TPasGLTFDoubleDynamicArray;
begin
 result:=nil;
 DoubleArray:=DecodeAsDoubleArray(aForVertex);
 Assert((length(DoubleArray) and 3)=0);
 SetLength(result,length(DoubleArray) shr 2);
 for Index:=0 to length(result)-1 do begin
  result[Index,0]:=trunc(DoubleArray[(Index shl 2) or 0]);
  result[Index,1]:=trunc(DoubleArray[(Index shl 2) or 1]);
  result[Index,2]:=trunc(DoubleArray[(Index shl 2) or 2]);
  result[Index,3]:=trunc(DoubleArray[(Index shl 2) or 3]);
 end;
end;

function TPasGLTF.TAccessor.DecodeAsUInt32Vector4Array(const aForVertex:boolean):TUInt32Vector4DynamicArray;
var Index:TPasGLTFSizeInt;
    DoubleArray:TPasGLTFDoubleDynamicArray;
begin
 result:=nil;
 DoubleArray:=DecodeAsDoubleArray(aForVertex);
 Assert((length(DoubleArray) and 3)=0);
 SetLength(result,length(DoubleArray) shr 2);
 for Index:=0 to length(result)-1 do begin
  result[Index,0]:=trunc(DoubleArray[(Index shl 2) or 0]);
  result[Index,1]:=trunc(DoubleArray[(Index shl 2) or 1]);
  result[Index,2]:=trunc(DoubleArray[(Index shl 2) or 2]);
  result[Index,3]:=trunc(DoubleArray[(Index shl 2) or 3]);
 end;
end;

function TPasGLTF.TAccessor.DecodeAsColorArray(const aForVertex:boolean=true):TVector4DynamicArray;
var Index:TPasGLTFSizeInt;
    DoubleArray:TPasGLTFDoubleDynamicArray;
begin
 result:=nil;
 DoubleArray:=DecodeAsDoubleArray(aForVertex);
 if fType=TType.Vec3 then begin
  Assert((length(DoubleArray) mod 3)=0);
  SetLength(result,length(DoubleArray) div 3);
  for Index:=0 to length(result)-1 do begin
   result[Index,0]:=DoubleArray[(Index*3)+0];
   result[Index,1]:=DoubleArray[(Index*3)+1];
   result[Index,2]:=DoubleArray[(Index*3)+2];
   result[Index,3]:=1.0;
  end;
 end else begin
  Assert((length(DoubleArray) and 3)=0);
  SetLength(result,length(DoubleArray) shr 2);
  for Index:=0 to length(result)-1 do begin
   result[Index,0]:=DoubleArray[(Index shl 2) or 0];
   result[Index,1]:=DoubleArray[(Index shl 2) or 1];
   result[Index,2]:=DoubleArray[(Index shl 2) or 2];
   result[Index,3]:=DoubleArray[(Index shl 2) or 3];
  end;
 end;
end;

function TPasGLTF.TAccessor.DecodeAsMatrix2x2Array(const aForVertex:boolean=true):TMatrix2x2DynamicArray;
var Index:TPasGLTFSizeInt;
    DoubleArray:TPasGLTFDoubleDynamicArray;
begin
 result:=nil;
 DoubleArray:=DecodeAsDoubleArray(aForVertex);
 Assert((length(DoubleArray) and 3)=0);
 SetLength(result,length(DoubleArray) shr 2);
 for Index:=0 to length(result)-1 do begin
  result[Index,0]:=DoubleArray[(Index shl 2) or 0];
  result[Index,1]:=DoubleArray[(Index shl 2) or 1];
  result[Index,2]:=DoubleArray[(Index shl 2) or 2];
  result[Index,3]:=DoubleArray[(Index shl 2) or 3];
 end;
end;

function TPasGLTF.TAccessor.DecodeAsMatrix3x3Array(const aForVertex:boolean=true):TMatrix3x3DynamicArray;
var Index:TPasGLTFSizeInt;
    DoubleArray:TPasGLTFDoubleDynamicArray;
begin
 result:=nil;
 DoubleArray:=DecodeAsDoubleArray(aForVertex);
 Assert((length(DoubleArray) mod 9)=0);
 SetLength(result,length(DoubleArray) div 9);
 for Index:=0 to length(result)-1 do begin
  result[Index,0]:=DoubleArray[(Index*9)+0];
  result[Index,1]:=DoubleArray[(Index*9)+1];
  result[Index,2]:=DoubleArray[(Index*9)+2];
  result[Index,3]:=DoubleArray[(Index*9)+3];
  result[Index,4]:=DoubleArray[(Index*9)+4];
  result[Index,5]:=DoubleArray[(Index*9)+5];
  result[Index,6]:=DoubleArray[(Index*9)+6];
  result[Index,7]:=DoubleArray[(Index*9)+7];
  result[Index,8]:=DoubleArray[(Index*9)+8];
 end;
end;

function TPasGLTF.TAccessor.DecodeAsMatrix4x4Array(const aForVertex:boolean=true):TMatrix4x4DynamicArray;
var Index:TPasGLTFSizeInt;
    DoubleArray:TPasGLTFDoubleDynamicArray;
begin
 result:=nil;
 DoubleArray:=DecodeAsDoubleArray(aForVertex);
 Assert((length(DoubleArray) and 15)=0);
 SetLength(result,length(DoubleArray) shr 4);
 for Index:=0 to length(result)-1 do begin
  result[Index,0]:=DoubleArray[(Index shl 4) or 0];
  result[Index,1]:=DoubleArray[(Index shl 4) or 1];
  result[Index,2]:=DoubleArray[(Index shl 4) or 2];
  result[Index,3]:=DoubleArray[(Index shl 4) or 3];
  result[Index,4]:=DoubleArray[(Index shl 4) or 4];
  result[Index,5]:=DoubleArray[(Index shl 4) or 5];
  result[Index,6]:=DoubleArray[(Index shl 4) or 6];
  result[Index,7]:=DoubleArray[(Index shl 4) or 7];
  result[Index,8]:=DoubleArray[(Index shl 4) or 8];
  result[Index,9]:=DoubleArray[(Index shl 4) or 9];
  result[Index,10]:=DoubleArray[(Index shl 4) or 10];
  result[Index,11]:=DoubleArray[(Index shl 4) or 11];
  result[Index,12]:=DoubleArray[(Index shl 4) or 12];
  result[Index,13]:=DoubleArray[(Index shl 4) or 13];
  result[Index,14]:=DoubleArray[(Index shl 4) or 14];
  result[Index,15]:=DoubleArray[(Index shl 4) or 15];
 end;
end;

{ TPasGLTF.TAnimation.TChannel.TTarget }

constructor TPasGLTF.TAnimation.TChannel.TTarget.Create(const aDocument:TDocument);
begin
 inherited Create(aDocument);
 fNode:=-1;
 fPath:='';
 fEmpty:=false;
end;

destructor TPasGLTF.TAnimation.TChannel.TTarget.Destroy;
begin
 inherited Destroy;
end;

{ TPasGLTF.TAnimation.TChannel }

constructor TPasGLTF.TAnimation.TChannel.Create(const aDocument:TDocument);
begin
 inherited Create(aDocument);
 fSampler:=-1;
 fTarget:=TTarget.Create(aDocument);
end;

destructor TPasGLTF.TAnimation.TChannel.Destroy;
begin
 FreeAndNil(fTarget);
 inherited Destroy;
end;

{ TPasGLTF.TAnimation.TSampler }

constructor TPasGLTF.TAnimation.TSampler.Create(const aDocument:TDocument);
begin
 inherited Create(aDocument);
 fInput:=-1;
 fOutput:=-1;
 fInterpolation:=TSamplerType.Linear;
end;

destructor TPasGLTF.TAnimation.TSampler.Destroy;
begin
 inherited Destroy;
end;

{ TPasGLTF.TAnimation }

constructor TPasGLTF.TAnimation.Create(const aDocument:TDocument);
begin
 inherited Create(aDocument);
 fName:='';
 fChannels:=TChannels.Create;
 fSamplers:=TSamplers.Create;
end;

destructor TPasGLTF.TAnimation.Destroy;
begin
 FreeAndNil(fChannels);
 FreeAndNil(fSamplers);
 inherited Destroy;
end;

{ TPasGLTF.TAsset }

constructor TPasGLTF.TAsset.Create(const aDocument:TDocument);
begin
 inherited Create(aDocument);
 fCopyright:='';
 fGenerator:='';
 fMinVersion:='';
 fVersion:='2.0';
 fEmpty:=false;
end;

destructor TPasGLTF.TAsset.Destroy;
begin
 inherited Destroy;
end;

{ TPasGLTF.TBuffer }

constructor TPasGLTF.TBuffer.Create(const aDocument:TDocument);
begin
 inherited Create(aDocument);
 fByteLength:=0;
 fName:='';
 fURI:='';
 fData:=TMemoryStream.Create;
end;

destructor TPasGLTF.TBuffer.Destroy;
begin
 FreeAndNil(fData);
 inherited Destroy;
end;

procedure TPasGLTF.TBuffer.SetEmbeddedResourceData;
begin
 fURI:='data:'+MimeTypeApplicationOctet+';base64,'+TBase64.Encode(fData);
end;

{ TPasGLTF.TBufferView }

constructor TPasGLTF.TBufferView.Create(const aDocument:TDocument);
begin
 inherited Create(aDocument);
 fName:='';
 fBuffer:=-1;
 fByteOffset:=0;
 fByteLength:=0;
 fByteStride:=0;
 fTarget:=TTargetType.None;
end;

destructor TPasGLTF.TBufferView.Destroy;
begin
 inherited Destroy;
end;

function TPasGLTF.TBufferView.Decode(const aSkipEvery:TPasGLTFSizeUInt;
                                     const aSkipBytes:TPasGLTFSizeUInt;
                                     const aElementSize:TPasGLTFSizeUInt;
                                     const aCount:TPasGLTFSizeUInt;
                                     const aType:TPasGLTF.TAccessor.TType;
                                     const aComponentCount:TPasGLTFSizeUInt;
                                     const aComponentType:TPasGLTF.TAccessor.TComponentType;
                                     const aComponentSize:TPasGLTFSizeUInt;
                                     const aByteOffset:TPasGLTFSizeUInt;
                                     const aNormalized:boolean;
                                     const aForVertex:boolean):TPasGLTFDoubleDynamicArray;
var Stride,Offset,Index,
    ComponentIndex,
    OutputIndex:TPasGLTFSizeUInt;
    Buffer:TPasGLTF.TBuffer;
    BufferData,Source:PPasGLTFUInt8Array;
    Value:TPasGLTFDouble;
begin

 result:=nil;

 Buffer:=fDocument.fBuffers[fBuffer];

 if fByteStride<>0 then begin
  Stride:=fByteStride;
 end else begin
  Stride:=aElementSize;
 end;

 if aForVertex and ((Stride and 3)<>0) then begin
  inc(Stride,4-(Stride and 3));
 end;

 SetLength(result,aCount*aComponentCount);

 Offset:=fByteOffset+aByteOffset;

 BufferData:=Buffer.fData.Memory;

 if (((Stride*(aCount-1))+aElementSize)>fByteLength) or
    ((Offset+((Stride*(aCount-1))+aElementSize))>TPasGLTFSizeUInt(Buffer.fData.Size)) then begin
  raise EPasGLTFInvalidDocument.Create('Invalid document');
 end;

 OutputIndex:=0;

 for Index:=1 to aCount do begin

  Source:=@BufferData^[Offset+((Index-1)*Stride)];

  for ComponentIndex:=1 to aComponentCount do begin

   if (aSkipEvery>0) and (ComponentIndex>1) and (((ComponentIndex-1) mod aSkipEvery)=0)  then begin
    Source:=@Source^[aSkipBytes];
   end;

   Value:=0.0;

   case aComponentType of
    TPasGLTF.TAccessor.TComponentType.SignedByte:begin
     if aNormalized then begin
      Value:=TPasGLTFInt8(TPasGLTFPointer(@Source^[0])^)/128.0;
     end else begin
      Value:=TPasGLTFInt8(TPasGLTFPointer(@Source^[0])^);
     end;
    end;
    TPasGLTF.TAccessor.TComponentType.UnsignedByte:begin
     if aNormalized then begin
      Value:=TPasGLTFUInt8(TPasGLTFPointer(@Source^[0])^)/255.0;
     end else begin
      Value:=TPasGLTFUInt8(TPasGLTFPointer(@Source^[0])^);
     end;
    end;
    TPasGLTF.TAccessor.TComponentType.SignedShort:begin
     if aNormalized then begin
      Value:=TPasGLTFInt16(TPasGLTFPointer(@Source^[0])^)/32768.0;
     end else begin
      Value:=TPasGLTFInt16(TPasGLTFPointer(@Source^[0])^);
     end;
    end;
    TPasGLTF.TAccessor.TComponentType.UnsignedShort:begin
     if aNormalized then begin
      Value:=TPasGLTFUInt16(TPasGLTFPointer(@Source^[0])^)/65535.0;
     end else begin
      Value:=TPasGLTFUInt16(TPasGLTFPointer(@Source^[0])^);
     end;
    end;
    TPasGLTF.TAccessor.TComponentType.UnsignedInt:begin
     if aNormalized then begin
      Value:=TPasGLTFUInt32(TPasGLTFPointer(@Source^[0])^)/4294967295.0;
     end else begin
      Value:=TPasGLTFUInt32(TPasGLTFPointer(@Source^[0])^);
     end;
    end;
    TPasGLTF.TAccessor.TComponentType.Float:begin
     Value:=TPasGLTFFloat(TPasGLTFPointer(@Source^[0])^);
    end;
    else {TPasGLTF.TAccessor.TComponentType.None:}begin
     raise EPasGLTFInvalidDocument.Create('Invalid document');
    end;
   end;

   result[OutputIndex]:=Value;
   inc(OutputIndex);

   Source:=@Source^[aComponentSize];

  end;

 end;

end;

{ TPasGLTF.TCamera.TOrthographic }

constructor TPasGLTF.TCamera.TOrthographic.Create(const aDocument:TDocument);
begin
 inherited Create(aDocument);
 fXMag:=TDefaults.FloatSentinel;
 fYMag:=TDefaults.FloatSentinel;
 fZNear:=-TDefaults.FloatSentinel;
 fZFar:=-TDefaults.FloatSentinel;
 fEmpty:=false;
end;

destructor TPasGLTF.TCamera.TOrthographic.Destroy;
begin
 inherited Destroy;
end;

{ TPasGLTF.TCamera.TPerspective }

constructor TPasGLTF.TCamera.TPerspective.Create(const aDocument:TDocument);
begin
 inherited Create(aDocument);
 fAspectRatio:=1.778;
 fYFov:=0.252;
 fZNear:=0.1;
 fZFar:=1000.0;
 fEmpty:=false;
end;

destructor TPasGLTF.TCamera.TPerspective.Destroy;
begin
 inherited Destroy;
end;

{ TPasGLTF.TCamera }

constructor TPasGLTF.TCamera.Create(const aDocument:TDocument);
begin
 inherited Create(aDocument);
 fType:=TCameraType.None;
 fOrthographic:=TOrthographic.Create(fDocument);
 fPerspective:=TPerspective.Create(fDocument);
end;

destructor TPasGLTF.TCamera.Destroy;
begin
 FreeAndNil(fOrthographic);
 FreeAndNil(fPerspective);
 inherited Destroy;
end;

{ TPasGLTF.TImage }

constructor TPasGLTF.TImage.Create(const aDocument:TDocument);
begin
 inherited Create(aDocument);
 fBufferView:=-1;
 fName:='';
 fURI:='';
 fMimeType:='';
end;

destructor TPasGLTF.TImage.Destroy;
begin
 inherited Destroy;
end;

procedure TPasGLTF.TImage.SetEmbeddedResourceData(const aStream:TStream);
begin
 fURI:='data:'+fMimeType+';base64,'+TBase64.Encode(aStream);
end;

procedure TPasGLTF.TImage.GetResourceData(const aStream:TStream);
var BufferView:TBufferView;
    Buffer:TBuffer;
begin
 if fBufferView>=0 then begin
  if fBufferView<fDocument.fBufferViews.Count then begin
   BufferView:=fDocument.fBufferViews[fBufferView];
   if (BufferView.fBuffer>=0) and (BufferView.fBuffer<fDocument.fBuffers.Count) then begin
    Buffer:=fDocument.fBuffers[BufferView.fBuffer];
    if (BufferView.fByteOffset+BufferView.fByteLength)<=Buffer.fData.Size then begin
     aStream.WriteBuffer(PPasGLTFUInt8Array(Buffer.fData.Memory)^[BufferView.fByteOffset],BufferView.fByteLength);
     aStream.Seek(-BufferView.fByteLength,soCurrent);
    end else begin
     raise EInOutError.Create('I/O error');
    end;
   end else begin
    raise EInOutError.Create('I/O error');
   end;
  end else begin
   raise EInOutError.Create('I/O error');
  end;
 end else begin
  fDocument.LoadURISource(fURI,aStream);
 end;
end;

function TPasGLTF.TImage.IsExternalResource:boolean;
begin
 result:=not ((fBufferView>=0) or (pos('data:',fURI)=1));
end;

{ TPasGLTF.TMaterial.TTexture }

constructor TPasGLTF.TMaterial.TTexture.Create(const aDocument:TDocument);
begin
 inherited Create(aDocument);
 fIndex:=-1;
 fTexCoord:=0;
end;

destructor TPasGLTF.TMaterial.TTexture.Destroy;
begin
 inherited Destroy;
end;

function TPasGLTF.TMaterial.TTexture.GetEmpty:boolean;
begin
 result:=fIndex<0;
end;

{ TPasGLTF.TMaterial.TNormalTexture }

constructor TPasGLTF.TMaterial.TNormalTexture.Create(const aDocument:TDocument);
begin
 inherited Create(aDocument);
 fScale:=TDefaults.IdentityScalar;
end;

{ TPasGLTF.TMaterial.TOcclusionTexture }

constructor TPasGLTF.TMaterial.TOcclusionTexture.Create(const aDocument:TDocument);
begin
 inherited Create(aDocument);
 fStrength:=TDefaults.IdentityScalar;
end;

{ TPasGLTF.TMaterial.TPBRMetallicRoughness }

constructor TPasGLTF.TMaterial.TPBRMetallicRoughness.Create(const aDocument:TDocument);
begin
 inherited Create(aDocument);
 fBaseColorFactor:=TDefaults.IdentityVector4;
 fBaseColorTexture:=TTexture.Create(fDocument);
 fRoughnessFactor:=TDefaults.IdentityScalar;
 fMetallicFactor:=TDefaults.IdentityScalar;
 fMetallicRoughnessTexture:=TTexture.Create(fDocument);
end;

destructor TPasGLTF.TMaterial.TPBRMetallicRoughness.Destroy;
begin
 FreeAndNil(fBaseColorTexture);
 FreeAndNil(fMetallicRoughnessTexture);
 inherited Destroy;
end;

function TPasGLTF.TMaterial.TPBRMetallicRoughness.GetEmpty:boolean;
begin
 result:=fBaseColorTexture.Empty and
         fMetallicRoughnessTexture.Empty and
         SameValue(fRoughnessFactor,TDefaults.IdentityScalar) and
         SameValue(fMetallicFactor,TDefaults.IdentityScalar);
end;

{ TPasGLTF.TMaterial }

constructor TPasGLTF.TMaterial.Create(const aDocument:TDocument);
begin
 inherited Create(aDocument);
 fName:='';
 fAlphaCutOff:=TDefaults.MaterialAlphaCutoff;
 fAlphaMode:=TAlphaMode.Opaque;
 fDoubleSided:=TDefaults.MaterialDoubleSided;
 fNormalTexture:=TNormalTexture.Create(fDocument);
 fOcclusionTexture:=TOcclusionTexture.Create(fDocument);
 fPBRMetallicRoughness:=TPBRMetallicRoughness.Create(fDocument);
 fEmissiveTexture:=TTexture.Create(fDocument);
 fEmissiveFactor:=TDefaults.NullVector3;
end;

destructor TPasGLTF.TMaterial.Destroy;
begin
 FreeAndNil(fNormalTexture);
 FreeAndNil(fOcclusionTexture);
 FreeAndNil(fPBRMetallicRoughness);
 FreeAndNil(fEmissiveTexture);
 inherited Destroy;
end;

{ TPasGLTF.TMesh.TPrimitive }

constructor TPasGLTF.TMesh.TPrimitive.Create(const aDocument:TDocument);
begin
 inherited Create(aDocument);
 fMode:=TMode.Triangles;
 fIndices:=-1;
 fMaterial:=-1;
 fAttributes:=TAttributes.Create(-1);
 fTargets:=TAttributesList.Create;
end;

destructor TPasGLTF.TMesh.TPrimitive.Destroy;
begin
 FreeAndNil(fAttributes);
 FreeAndNil(fTargets);
 inherited Destroy;
end;

{ TPasGLTF.TMesh }

constructor TPasGLTF.TMesh.Create(const aDocument:TDocument);
begin
 inherited Create(aDocument);
 fName:='';
 fWeights:=TWeights.Create;
 fPrimitives:=TPrimitives.Create;
end;

destructor TPasGLTF.TMesh.Destroy;
begin
 FreeAndNil(fWeights);
 FreeAndNil(fPrimitives);
 inherited Destroy;
end;

{ TPasGLTF.TNode }

constructor TPasGLTF.TNode.Create(const aDocument:TDocument);
begin
 inherited Create(aDocument);
 fName:='';
 fCamera:=-1;
 fMesh:=-1;
 fSkin:=-1;
 fMatrix:=TDefaults.IdentityMatrix4x4;
 fRotation:=TDefaults.IdentityQuaternion;
 fScale:=TDefaults.IdentityVector3;
 fTranslation:=TDefaults.NullVector3;
 fChildren:=TChildren.Create;
 fWeights:=TWeights.Create;
end;

destructor TPasGLTF.TNode.Destroy;
begin
 FreeAndNil(fChildren);
 FreeAndNil(fWeights);
 inherited Destroy;
end;

{ TPasGLTF.TSampler }

constructor TPasGLTF.TSampler.Create(const aDocument:TDocument);
begin
 inherited Create(aDocument);
 fName:='';
 fMagFilter:=TMagFilter.None;
 fMinFilter:=TMinFilter.None;
 fWrapS:=TWrappingMode.Repeat_;
 fWrapT:=TWrappingMode.Repeat_;
end;

destructor TPasGLTF.TSampler.Destroy;
begin
 inherited Destroy;
end;

function TPasGLTF.TSampler.GetEmpty:boolean;
begin
 result:=(length(fName)=0) and
         (fMagFilter=TMagFilter.None) and
         (fMinFilter=TMinFilter.None) and
         (fWrapS=TWrappingMode.Repeat_) and
         (fWrapT=TWrappingMode.Repeat_);
end;

{ TPasGLTF.TScene }

constructor TPasGLTF.TScene.Create(const aDocument:TDocument);
begin
 inherited Create(aDocument);
 fName:='';
 fNodes:=TPasGLTF.TScene.TNodes.Create;
end;

destructor TPasGLTF.TScene.Destroy;
begin
 FreeAndNil(fNodes);
 inherited Destroy;
end;

{ TPasGLTF.TSkin }

constructor TPasGLTF.TSkin.Create(const aDocument:TDocument);
begin
 inherited Create(aDocument);
 fName:='';
 fInverseBindMatrices:=-1;
 fSkeleton:=-1;
 fJoints:=TPasGLTF.TSkin.TJoints.Create;
end;

destructor TPasGLTF.TSkin.Destroy;
begin
 FreeAndNil(fJoints);
 inherited Destroy;
end;

{ TPasGLTF.TTexture }

constructor TPasGLTF.TTexture.Create(const aDocument:TDocument);
begin
 inherited Create(aDocument);
 fName:='';
 fSampler:=-1;
 fSource:=-1;
end;

destructor TPasGLTF.TTexture.Destroy;
begin
 inherited Destroy;
end;

{ TPasGLTF.TDocument }

constructor TPasGLTF.TDocument.Create(const aDocument:TDocument=nil);
begin
 inherited Create(aDocument);
 fAsset:=TAsset.Create(fDocument);
 fAccessors:=TAccessors.Create;
 fAnimations:=TAnimations.Create;
 fBuffers:=TBuffers.Create;
 fBufferViews:=TBufferViews.Create;
 fCameras:=TCameras.Create;
 fImages:=TImages.Create;
 fMaterials:=TMaterials.Create;
 fMeshes:=TMeshes.Create;
 fNodes:=TNodes.Create;
 fSamplers:=TSamplers.Create;
 fScene:=-1;
 fScenes:=TScenes.Create;
 fSkins:=TSkins.Create;
 fTextures:=TTextures.Create;
 fExtensionsUsed:=TStringList.Create;
 fExtensionsRequired:=TStringList.Create;
 fRootPath:='';
 fGetURI:=DefaultGetURI;
end;

destructor TPasGLTF.TDocument.Destroy;
begin
 FreeAndNil(fAsset);
 FreeAndNil(fAccessors);
 FreeAndNil(fAnimations);
 FreeAndNil(fBuffers);
 FreeAndNil(fBufferViews);
 FreeAndNil(fCameras);
 FreeAndNil(fImages);
 FreeAndNil(fMaterials);
 FreeAndNil(fMeshes);
 FreeAndNil(fNodes);
 FreeAndNil(fSamplers);
 FreeAndNil(fScenes);
 FreeAndNil(fSkins);
 FreeAndNil(fTextures);
 FreeAndNil(fExtensionsUsed);
 FreeAndNil(fExtensionsRequired);
 inherited Destroy;
end;

function TPasGLTF.TDocument.DefaultGetURI(const aURI:TPasGLTFUTF8String):TStream;
var FileName:String;
begin
 FileName:=ExpandFileName(IncludeTrailingPathDelimiter(fRootPath)+String(TPasGLTF.ResolveURIToPath(aURI)));
 result:=TFileStream.Create(FileName,fmOpenRead or fmShareDenyWrite);
end;

procedure TPasGLTF.TDocument.LoadURISource(const aURI:TPasGLTFUTF8String;const aStream:TStream);
const Base64Signature=';base64,';
var Stream:TStream;
    Base64Position:TPasGLTFSizeInt;
begin
 if length(trim(aURI))>0 then begin
  if (length(aURI)>5) and
     (aURI[1]='d') and
     (aURI[2]='a') and
     (aURI[3]='t') and
     (aURI[4]='a') and
     (aURI[5]=':') then begin
   Base64Position:=pos(Base64Signature,aURI);
   if Base64Position>0 then begin
    TBase64.Decode(copy(aURI,Base64Position+length(Base64Signature),(length(aURI)-(Base64Position+length(Base64Signature)))+1),aStream);
   end;
  end else if assigned(fGetURI) then begin
   Stream:=fGetURI(aURI);
   if assigned(Stream) then begin
    try
     Stream.Seek(0,soBeginning);
     if aStream.CopyFrom(Stream,Stream.Size)<>Stream.Size then begin
      raise EInOutError.Create('I/O error');
     end;
    finally
     FreeAndNil(Stream);
    end;
   end;
  end;
  aStream.Seek(0,soBeginning);
 end;
end;

procedure TPasGLTF.TDocument.LoadURISources;
var Buffer:TBuffer;
begin
 for Buffer in fBuffers do begin
  if length(trim(Buffer.fURI))>0 then begin
   LoadURISource(Buffer.fURI,Buffer.fData);
  end;
 end;
end;

procedure TPasGLTF.TDocument.LoadFromJSON(const aJSONRootItem:TPasJSONItem);
 function Required(const aJSONItem:TPasJSONItem;const aName:TPasGLTFUTF8String=''):TPasJSONItem;
 begin
  result:=aJSONItem;
  if not assigned(result) then begin
   if length(aName)>0 then begin
    raise EPasGLTFInvalidDocument.Create('Invalid GLTF document, missing "'+String(aName)+'" field');
   end else begin
    raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
   end;
  end;
 end;
 procedure ProcessExtensionsAndExtras(const aJSONItem:TPasJSONItem;const aBaseExtensionsExtrasObject:TBaseExtensionsExtrasObject);
 var JSONObject:TPasJSONItemObject;
     JSONObjectItem:TPasJSONItem;
 begin
  if not (assigned(aJSONItem) and (aJSONItem is TPasJSONItemObject)) then begin
   raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
  end;
  JSONObject:=TPasJSONItemObject(aJSONItem);
  begin
   JSONObjectItem:=JSONObject.Properties['extensions'];
   if assigned(JSONObjectItem) and (JSONObjectItem is TPasJSONItemObject) then begin
    aBaseExtensionsExtrasObject.fExtensions.Merge(JSONObjectItem);
   end;
  end;
  begin
   JSONObjectItem:=JSONObject.Properties['extras'];
   if assigned(JSONObjectItem) and (JSONObjectItem is TPasJSONItemObject) then begin
    aBaseExtensionsExtrasObject.fExtras.Merge(JSONObjectItem);
   end;
  end;
 end;
 procedure ProcessAccessors(const aJSONItem:TPasJSONItem);
  function ProcessAccessor(const aJSONItem:TPasJSONItem):TAccessor;
   procedure ProcessSparse(const aJSONItem:TPasJSONItem;const aSparse:TAccessor.TSparse);
   var JSONObject:TPasJSONItemObject;
       JSONItem:TPasJSONItem;
   begin
    if not (assigned(aJSONItem) and (aJSONItem is TPasJSONItemObject)) then begin
     raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
    end;
    JSONObject:=TPasJSONItemObject(aJSONItem);
    ProcessExtensionsAndExtras(JSONObject,aSparse);
    aSparse.fCount:=TPasJSON.GetInt64(Required(JSONObject.Properties['count'],'count'),aSparse.fCount);
    begin
     JSONItem:=JSONObject.Properties['indices'];
     if not (assigned(JSONItem) and (JSONItem is TPasJSONItemObject)) then begin
      raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
     end;
     ProcessExtensionsAndExtras(TPasJSONItemObject(JSONItem),aSparse.fIndices);
     aSparse.fIndices.fBufferView:=TPasJSON.GetInt64(Required(TPasJSONItemObject(JSONItem).Properties['bufferView'],'bufferView'),aSparse.fIndices.fBufferView);
     aSparse.fIndices.fComponentType:=TAccessor.TComponentType(TPasJSON.GetInt64(Required(TPasJSONItemObject(JSONItem).Properties['componentType'],'componentType'),TPasGLTFInt64(TAccessor.TComponentType.None)));
     aSparse.fIndices.fByteOffset:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['byteOffset'],aSparse.fIndices.fByteOffset);
    end;
    begin
     JSONItem:=JSONObject.Properties['values'];
     if not (assigned(JSONItem) and (JSONItem is TPasJSONItemObject)) then begin
      raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
     end;
     ProcessExtensionsAndExtras(TPasJSONItemObject(JSONItem),aSparse.fValues);
     aSparse.fValues.fBufferView:=TPasJSON.GetInt64(Required(TPasJSONItemObject(JSONItem).Properties['bufferView'],'bufferView'),aSparse.fValues.fBufferView);
     aSparse.fValues.fByteOffset:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['byteOffset'],aSparse.fValues.fByteOffset);
    end;
   end;
  var JSONObject:TPasJSONItemObject;
      JSONItem,JSONArrayItem:TPasJSONItem;
      Type_:TPasGLTFUTF8String;
  begin
   if not (assigned(aJSONItem) and (aJSONItem is TPasJSONItemObject)) then begin
    raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
   end;
   JSONObject:=TPasJSONItemObject(aJSONItem);
   result:=TAccessor.Create(self);
   try
    ProcessExtensionsAndExtras(JSONObject,result);
    result.fComponentType:=TAccessor.TComponentType(TPasJSON.GetInt64(Required(JSONObject.Properties['componentType'],'componentType'),TPasGLTFInt64(TAccessor.TComponentType.None)));
    result.fCount:=TPasJSON.GetInt64(Required(JSONObject.Properties['count'],'count'),result.fCount);
    begin
     Type_:=TPasJSON.GetString(Required(JSONObject.Properties['type'],'type'),'NONE');
     if Type_='SCALAR' then begin
      result.fType:=TAccessor.TType.Scalar;
     end else if Type_='VEC2' then begin
      result.fType:=TAccessor.TType.Vec2;
     end else if Type_='VEC3' then begin
      result.fType:=TAccessor.TType.Vec3;
     end else if Type_='VEC4' then begin
      result.fType:=TAccessor.TType.Vec4;
     end else if Type_='MAT2' then begin
      result.fType:=TAccessor.TType.Mat2;
     end else if Type_='MAT3' then begin
      result.fType:=TAccessor.TType.Mat3;
     end else if Type_='MAT4' then begin
      result.fType:=TAccessor.TType.Mat4;
     end else begin
      raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
     end;
    end;
    result.fBufferView:=TPasJSON.GetInt64(JSONObject.Properties['bufferView'],result.fBufferView);
    result.fByteOffset:=TPasJSON.GetInt64(JSONObject.Properties['byteOffset'],result.fByteOffset);
    begin
     JSONItem:=JSONObject.Properties['min'];
     if assigned(JSONItem) then begin
      if not (JSONItem is TPasJSONItemArray) then begin
       raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
      end;
      for JSONArrayItem in TPasJSONItemArray(JSONItem) do begin
       if not (assigned(JSONArrayItem) and (JSONArrayItem is TPasJSONItemNumber)) then begin
        raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
       end;
       result.fMinArray.Add(TPasJSON.GetNumber(JSONArrayItem,0.0));
      end;
     end;
    end;
    begin
     JSONItem:=JSONObject.Properties['max'];
     if assigned(JSONItem) then begin
      if not (JSONItem is TPasJSONItemArray) then begin
       raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
      end;
      for JSONArrayItem in TPasJSONItemArray(JSONItem) do begin
       if not (assigned(JSONArrayItem) and (JSONArrayItem is TPasJSONItemNumber)) then begin
        raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
       end;
       result.fMaxArray.Add(TPasJSON.GetNumber(JSONArrayItem,0.0));
      end;
     end;
    end;
    result.fName:=TPasJSON.GetString(JSONObject.Properties['name'],result.fName);
    result.fNormalized:=TPasJSON.GetBoolean(JSONObject.Properties['normalized'],result.fNormalized);
    begin
     JSONItem:=JSONObject.Properties['sparse'];
     if assigned(JSONItem) then begin
      ProcessSparse(JSONItem,result.fSparse);
     end;
    end;
   except
    FreeAndNil(result);
    raise;
   end;
  end;
 var JSONArray:TPasJSONItemArray;
     JSONItem:TPasJSONItem;
 begin
  if not (assigned(aJSONItem) and (aJSONItem is TPasJSONItemArray)) then begin
   raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
  end;
  JSONArray:=TPasJSONItemArray(aJSONItem);
  for JSONItem in JSONArray do begin
   fAccessors.Add(ProcessAccessor(JSONItem));
  end;
 end;
 procedure ProcessAnimations(const aJSONItem:TPasJSONItem);
  function ProcessAnimation(const aJSONItem:TPasJSONItem):TAnimation;
  var JSONObject:TPasJSONItemObject;
      JSONItem,JSONArrayItem,TargetItem,InterpolationItem:TPasJSONItem;
      Interpolation:TPasGLTFUTF8String;
      Channel:TAnimation.TChannel;
      Sampler:TAnimation.TSampler;
  begin
   if not (assigned(aJSONItem) and (aJSONItem is TPasJSONItemObject)) then begin
    raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
   end;
   JSONObject:=TPasJSONItemObject(aJSONItem);
   result:=TAnimation.Create(self);
   try
    ProcessExtensionsAndExtras(JSONObject,result);
    result.fName:=TPasJSON.GetString(JSONObject.Properties['name'],result.fName);
    begin
     JSONItem:=JSONObject.Properties['channels'];
     if not (assigned(JSONItem) and (JSONItem is TPasJSONItemArray)) then begin
      raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
     end;
     for JSONArrayItem in TPasJSONItemArray(JSONItem) do begin
      if not (assigned(JSONArrayItem) and (JSONArrayItem is TPasJSONItemObject)) then begin
       raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
      end;
      Channel:=TAnimation.TChannel.Create(self);
      try
       ProcessExtensionsAndExtras(TPasJSONItemObject(JSONArrayItem),Channel);
       Channel.fSampler:=TPasJSON.GetInt64(Required(TPasJSONItemObject(JSONArrayItem).Properties['sampler'],'sampler'),Channel.fSampler);
       begin
        TargetItem:=Required(TPasJSONItemObject(JSONArrayItem).Properties['target'],'target');
        if not (assigned(TargetItem) and (TargetItem is TPasJSONItemObject)) then begin
         raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
        end;
        ProcessExtensionsAndExtras(TPasJSONItemObject(TargetItem),Channel.fTarget);
        Channel.fTarget.fPath:=TPasJSON.GetString(Required(TPasJSONItemObject(TargetItem).Properties['path'],'path'),Channel.fTarget.fPath);
        Channel.fTarget.fNode:=TPasJSON.GetInt64(TPasJSONItemObject(TargetItem).Properties['node'],Channel.fTarget.fNode);
       end;
      finally
       result.fChannels.Add(Channel);
      end;
     end;
    end;
    begin
     JSONItem:=JSONObject.Properties['samplers'];
     if not (assigned(JSONItem) and (JSONItem is TPasJSONItemArray)) then begin
      raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
     end;
     for JSONArrayItem in TPasJSONItemArray(JSONItem) do begin
      if not (assigned(JSONArrayItem) and (JSONArrayItem is TPasJSONItemObject)) then begin
       raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
      end;
      Sampler:=TAnimation.TSampler.Create(self);
      try
       ProcessExtensionsAndExtras(TPasJSONItemObject(JSONArrayItem),Sampler);
       Sampler.fInput:=TPasJSON.GetInt64(Required(TPasJSONItemObject(JSONArrayItem).Properties['input'],'input'),Sampler.fInput);
       Sampler.fOutput:=TPasJSON.GetInt64(Required(TPasJSONItemObject(JSONArrayItem).Properties['output'],'output'),Sampler.fOutput);
       begin
        InterpolationItem:=TPasJSONItemObject(JSONArrayItem).Properties['interpolation'];
        if assigned(InterpolationItem) then begin
         if not (InterpolationItem is TPasJSONItemString) then begin
          raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
         end;
         Interpolation:=TPasJSON.GetString(InterpolationItem,'NONE');
         if Interpolation='LINEAR' then begin
          Sampler.fInterpolation:=TAnimation.TSampler.TSamplerType.Linear;
         end else if Interpolation='STEP' then begin
          Sampler.fInterpolation:=TAnimation.TSampler.TSamplerType.Step;
         end else if Interpolation='CUBICSPLINE' then begin
          Sampler.fInterpolation:=TAnimation.TSampler.TSamplerType.CubicSpline;
         end else begin
          raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
         end;
        end;
       end;
      finally
       result.fSamplers.Add(Sampler);
      end;
     end;
    end;
   except
    FreeAndNil(result);
    raise;
   end;
  end;
 var JSONArray:TPasJSONItemArray;
     JSONItem:TPasJSONItem;
 begin
  if not (assigned(aJSONItem) and (aJSONItem is TPasJSONItemArray)) then begin
   raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
  end;
  JSONArray:=TPasJSONItemArray(aJSONItem);
  for JSONItem in JSONArray do begin
   fAnimations.Add(ProcessAnimation(JSONItem));
  end;
 end;
 procedure ProcessAsset(const aJSONItem:TPasJSONItem);
 var JSONObject:TPasJSONItemObject;
 begin
  if not (assigned(aJSONItem) and (aJSONItem is TPasJSONItemObject)) then begin
   raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
  end;
  JSONObject:=TPasJSONItemObject(aJSONItem);
  ProcessExtensionsAndExtras(JSONObject,fAsset);
  fAsset.fCopyright:=TPasJSON.GetString(JSONObject.Properties['copyright'],fAsset.fCopyright);
  fAsset.fGenerator:=TPasJSON.GetString(JSONObject.Properties['generator'],fAsset.fGenerator);
  fAsset.fMinVersion:=TPasJSON.GetString(JSONObject.Properties['minVersion'],fAsset.fMinVersion);
  fAsset.fVersion:=TPasJSON.GetString(Required(JSONObject.Properties['version'],'version'),fAsset.fVersion);
 end;
 procedure ProcessBuffers(const aJSONItem:TPasJSONItem);
  function ProcessBuffer(const aJSONItem:TPasJSONItem):TBuffer;
  var JSONObject:TPasJSONItemObject;
  begin
   if not (assigned(aJSONItem) and (aJSONItem is TPasJSONItemObject)) then begin
    raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
   end;
   JSONObject:=TPasJSONItemObject(aJSONItem);
   result:=TBuffer.Create(self);
   try
    ProcessExtensionsAndExtras(JSONObject,result);
    result.fName:=TPasJSON.GetString(JSONObject.Properties['name'],result.fName);
    result.fURI:=TPasJSON.GetString(JSONObject.Properties['uri'],result.fURI);
    result.fByteLength:=TPasJSON.GetInt64(Required(JSONObject.Properties['byteLength'],'byteLength'),result.fByteLength);
   except
    FreeAndNil(result);
    raise;
   end;
  end;
 var JSONArray:TPasJSONItemArray;
     JSONItem:TPasJSONItem;
 begin
  if not (assigned(aJSONItem) and (aJSONItem is TPasJSONItemArray)) then begin
   raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
  end;
  JSONArray:=TPasJSONItemArray(aJSONItem);
  for JSONItem in JSONArray do begin
   fBuffers.Add(ProcessBuffer(JSONItem));
  end;
 end;
 procedure ProcessBufferViews(const aJSONItem:TPasJSONItem);
  function ProcessBufferView(const aJSONItem:TPasJSONItem):TBufferView;
  var JSONObject:TPasJSONItemObject;
  begin
   if not (assigned(aJSONItem) and (aJSONItem is TPasJSONItemObject)) then begin
    raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
   end;
   JSONObject:=TPasJSONItemObject(aJSONItem);
   result:=TBufferView.Create(self);
   try
    ProcessExtensionsAndExtras(JSONObject,result);
    result.fBuffer:=TPasJSON.GetInt64(Required(JSONObject.Properties['buffer'],'buffer'),result.fBuffer);
    result.fByteLength:=TPasJSON.GetInt64(Required(JSONObject.Properties['byteLength'],'byteLength'),result.fByteLength);
    result.fByteOffset:=TPasJSON.GetInt64(JSONObject.Properties['byteOffset'],result.fByteOffset);
    result.fByteStride:=TPasJSON.GetInt64(JSONObject.Properties['byteStride'],result.fByteStride);
    result.fName:=TPasJSON.GetString(JSONObject.Properties['name'],result.fName);
    result.fTarget:=TBufferView.TTargetType(TPasJSON.GetInt64(JSONObject.Properties['target'],TPasGLTFInt64(result.fTarget)));
   except
    FreeAndNil(result);
    raise;
   end;
  end;
 var JSONArray:TPasJSONItemArray;
     JSONItem:TPasJSONItem;
 begin
  if not (assigned(aJSONItem) and (aJSONItem is TPasJSONItemArray)) then begin
   raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
  end;
  JSONArray:=TPasJSONItemArray(aJSONItem);
  for JSONItem in JSONArray do begin
   fBufferViews.Add(ProcessBufferView(JSONItem));
  end;
 end;
 procedure ProcessCameras(const aJSONItem:TPasJSONItem);
  function ProcessCamera(const aJSONItem:TPasJSONItem):TCamera;
  var JSONObject:TPasJSONItemObject;
      JSONItem:TPasJSONItem;
      Type_:TPasGLTFUTF8String;
  begin
   if not (assigned(aJSONItem) and (aJSONItem is TPasJSONItemObject)) then begin
    raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
   end;
   JSONObject:=TPasJSONItemObject(aJSONItem);
   result:=TCamera.Create(self);
   try
    ProcessExtensionsAndExtras(JSONObject,result);
    result.fName:=TPasJSON.GetString(JSONObject.Properties['name'],result.fName);
    begin
     Type_:=TPasJSON.GetString(Required(JSONObject.Properties['type'],'type'),'none');
     if Type_='orthographic' then begin
      result.fType:=TCamera.TCameraType.Orthographic;
     end else if Type_='perspective' then begin
      result.fType:=TCamera.TCameraType.Perspective;
     end else begin
      raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
     end;
    end;
    case result.fType of
     TCamera.TCameraType.Orthographic:begin
      JSONItem:=JSONObject.Properties['orthographic'];
      if not (assigned(JSONItem) and (JSONItem is TPasJSONItemObject)) then begin
       raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
      end;
      ProcessExtensionsAndExtras(TPasJSONItemObject(JSONItem),result.fOrthographic);
      result.fOrthographic.fXMag:=TPasJSON.GetNumber(Required(TPasJSONItemObject(JSONItem).Properties['xmag'],'xmag'),result.fOrthographic.fXMag);
      result.fOrthographic.fYMag:=TPasJSON.GetNumber(Required(TPasJSONItemObject(JSONItem).Properties['ymag'],'ymag'),result.fOrthographic.fYMag);
      result.fOrthographic.fZNear:=TPasJSON.GetNumber(Required(TPasJSONItemObject(JSONItem).Properties['znear'],'znear'),result.fOrthographic.fZNear);
      result.fOrthographic.fZFar:=TPasJSON.GetNumber(Required(TPasJSONItemObject(JSONItem).Properties['zfar'],'zfar'),result.fOrthographic.fZFar);
     end;
     TCamera.TCameraType.Perspective:begin
      JSONItem:=JSONObject.Properties['perspective'];
      if not (assigned(JSONItem) and (JSONItem is TPasJSONItemObject)) then begin
       raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
      end;
      ProcessExtensionsAndExtras(TPasJSONItemObject(JSONItem),result.fPerspective);
      result.fPerspective.fAspectRatio:=TPasJSON.GetNumber(TPasJSONItemObject(JSONItem).Properties['aspectRatio'],result.fPerspective.fAspectRatio);
      result.fPerspective.fYFov:=TPasJSON.GetNumber(Required(TPasJSONItemObject(JSONItem).Properties['yfov'],'yfov'),result.fPerspective.fYFov);
      result.fPerspective.fZNear:=TPasJSON.GetNumber(Required(TPasJSONItemObject(JSONItem).Properties['znear'],'znear'),result.fPerspective.fZNear);
      result.fPerspective.fZFar:=TPasJSON.GetNumber(Required(TPasJSONItemObject(JSONItem).Properties['zfar'],'zfar'),result.fPerspective.fZFar);
     end;
     else begin
      raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
     end;
    end;
   except
    FreeAndNil(result);
    raise;
   end;
  end;
 var JSONArray:TPasJSONItemArray;
     JSONItem:TPasJSONItem;
 begin
  if not (assigned(aJSONItem) and (aJSONItem is TPasJSONItemArray)) then begin
   raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
  end;
  JSONArray:=TPasJSONItemArray(aJSONItem);
  for JSONItem in JSONArray do begin
   fCameras.Add(ProcessCamera(JSONItem));
  end;
 end;
 procedure ProcessImages(const aJSONItem:TPasJSONItem);
  function ProcessImage(const aJSONItem:TPasJSONItem):TImage;
  var JSONObject:TPasJSONItemObject;
  begin
   if not (assigned(aJSONItem) and (aJSONItem is TPasJSONItemObject)) then begin
    raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
   end;
   JSONObject:=TPasJSONItemObject(aJSONItem);
   result:=TImage.Create(self);
   try
    ProcessExtensionsAndExtras(JSONObject,result);
    result.fBufferView:=TPasJSON.GetInt64(JSONObject.Properties['bufferView'],result.fBufferView);
    result.fMimeType:=TPasJSON.GetString(JSONObject.Properties['mimeType'],result.fMimeType);
    result.fName:=TPasJSON.GetString(JSONObject.Properties['name'],result.fName);
    result.fURI:=TPasJSON.GetString(JSONObject.Properties['uri'],result.fURI);
   except
    FreeAndNil(result);
    raise;
   end;
  end;
 var JSONArray:TPasJSONItemArray;
     JSONItem:TPasJSONItem;
 begin
  if not (assigned(aJSONItem) and (aJSONItem is TPasJSONItemArray)) then begin
   raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
  end;
  JSONArray:=TPasJSONItemArray(aJSONItem);
  for JSONItem in JSONArray do begin
   fImages.Add(ProcessImage(JSONItem));
  end;
 end;
 procedure ProcessMaterials(const aJSONItem:TPasJSONItem);
  function ProcessMaterial(const aJSONItem:TPasJSONItem):TMaterial;
  var JSONObject:TPasJSONItemObject;
      JSONItem,JSONSubItem:TPasJSONItem;
      Mode:TPasGLTFUTF8String;
      Index:TPasGLTFSizeInt;
  begin
   if not (assigned(aJSONItem) and (aJSONItem is TPasJSONItemObject)) then begin
    raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
   end;
   JSONObject:=TPasJSONItemObject(aJSONItem);
   result:=TMaterial.Create(self);
   try
    ProcessExtensionsAndExtras(JSONObject,result);
    result.fAlphaCutOff:=TPasJSON.GetNumber(JSONObject.Properties['alphaCutoff'],result.fAlphaCutOff);
    begin
     JSONItem:=JSONObject.Properties['alphaMode'];
     if assigned(JSONItem) then begin
      Mode:=TPasJSON.GetString(JSONItem,'NONE');
      if Mode='OPAQUE' then begin
       result.fAlphaMode:=TMaterial.TAlphaMode.Opaque;
      end else if Mode='MASK' then begin
       result.fAlphaMode:=TMaterial.TAlphaMode.Mask;
      end else if Mode='BLEND' then begin
       result.fAlphaMode:=TMaterial.TAlphaMode.Blend;
      end else begin
       raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
      end;
     end;
    end;
    result.fDoubleSided:=TPasJSON.GetBoolean(JSONObject.Properties['doubleSided'],result.fDoubleSided);
    begin
     JSONItem:=JSONObject.Properties['emissiveFactor'];
     if assigned(JSONItem) then begin
      if not ((JSONItem is TPasJSONItemArray) and (TPasJSONItemArray(JSONItem).Count=3)) then begin
       raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
      end;
      for Index:=0 to 2 do begin
       result.fEmissiveFactor[Index]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[Index],result.fEmissiveFactor[Index]);
      end;
     end;
    end;
    begin
     JSONItem:=JSONObject.Properties['emissiveTexture'];
     if assigned(JSONItem) then begin
      if not (JSONItem is TPasJSONItemObject) then begin
       raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
      end;
      ProcessExtensionsAndExtras(TPasJSONItemObject(JSONItem),result.fEmissiveTexture);
      result.fEmissiveTexture.fIndex:=TPasJSON.GetInt64(Required(TPasJSONItemObject(JSONItem).Properties['index'],'index'),result.fEmissiveTexture.fIndex);
      result.fEmissiveTexture.fTexCoord:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['texCoord'],result.fEmissiveTexture.fTexCoord);
     end;
    end;
    result.fName:=TPasJSON.GetString(JSONObject.Properties['name'],result.fName);
    begin
     JSONItem:=JSONObject.Properties['normalTexture'];
     if assigned(JSONItem) then begin
      if not (JSONItem is TPasJSONItemObject) then begin
       raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
      end;
      ProcessExtensionsAndExtras(TPasJSONItemObject(JSONItem),result.fNormalTexture);
      result.fNormalTexture.fIndex:=TPasJSON.GetInt64(Required(TPasJSONItemObject(JSONItem).Properties['index'],'index'),result.fNormalTexture.fIndex);
      result.fNormalTexture.fTexCoord:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['texCoord'],result.fNormalTexture.fTexCoord);
      result.fNormalTexture.fScale:=TPasJSON.GetNumber(TPasJSONItemObject(JSONItem).Properties['scale'],result.fNormalTexture.fScale);
     end;
    end;
    begin
     JSONItem:=JSONObject.Properties['occlusionTexture'];
     if assigned(JSONItem) then begin
      if not (JSONItem is TPasJSONItemObject) then begin
       raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
      end;
      ProcessExtensionsAndExtras(TPasJSONItemObject(JSONItem),result.fOcclusionTexture);
      result.fOcclusionTexture.fIndex:=TPasJSON.GetInt64(Required(TPasJSONItemObject(JSONItem).Properties['index'],'index'),result.fOcclusionTexture.fIndex);
      result.fOcclusionTexture.fTexCoord:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['texCoord'],result.fOcclusionTexture.fTexCoord);
      result.fOcclusionTexture.fStrength:=TPasJSON.GetNumber(TPasJSONItemObject(JSONItem).Properties['scale'],result.fOcclusionTexture.fStrength);
     end;
    end;
    begin
     JSONItem:=JSONObject.Properties['pbrMetallicRoughness'];
     if assigned(JSONItem) then begin
      if not (JSONItem is TPasJSONItemObject) then begin
       raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
      end;
      ProcessExtensionsAndExtras(TPasJSONItemObject(JSONItem),result.fPBRMetallicRoughness);
      begin
       JSONSubItem:=TPasJSONItemObject(JSONItem).Properties['baseColorFactor'];
       if assigned(JSONSubItem) then begin
        if not ((JSONSubItem is TPasJSONItemArray) and (TPasJSONItemArray(JSONSubItem).Count=4)) then begin
         raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
        end;
        for Index:=0 to 3 do begin
         result.fPBRMetallicRoughness.fBaseColorFactor[Index]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONSubItem).Items[Index],result.fPBRMetallicRoughness.fBaseColorFactor[Index]);
        end;
       end;
      end;
      begin
       JSONSubItem:=TPasJSONItemObject(JSONItem).Properties['baseColorTexture'];
       if assigned(JSONSubItem) then begin
        if not (JSONSubItem is TPasJSONItemObject) then begin
         raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
        end;
        ProcessExtensionsAndExtras(TPasJSONItemObject(JSONSubItem),result.fPBRMetallicRoughness.fBaseColorTexture);
        result.fPBRMetallicRoughness.fBaseColorTexture.fIndex:=TPasJSON.GetInt64(Required(TPasJSONItemObject(JSONSubItem).Properties['index'],'index'),result.fPBRMetallicRoughness.fBaseColorTexture.fIndex);
        result.fPBRMetallicRoughness.fBaseColorTexture.fTexCoord:=TPasJSON.GetInt64(TPasJSONItemObject(JSONSubItem).Properties['texCoord'],result.fPBRMetallicRoughness.fBaseColorTexture.fTexCoord);
       end;
      end;
      result.fPBRMetallicRoughness.fMetallicFactor:=TPasJSON.GetNumber(TPasJSONItemObject(JSONItem).Properties['metallicFactor'],result.fPBRMetallicRoughness.fMetallicFactor);
      begin
       JSONSubItem:=TPasJSONItemObject(JSONItem).Properties['metallicRoughnessTexture'];
       if assigned(JSONSubItem) then begin
        if not (JSONSubItem is TPasJSONItemObject) then begin
         raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
        end;
        ProcessExtensionsAndExtras(TPasJSONItemObject(JSONSubItem),result.fPBRMetallicRoughness.fMetallicRoughnessTexture);
        result.fPBRMetallicRoughness.fMetallicRoughnessTexture.fIndex:=TPasJSON.GetInt64(Required(TPasJSONItemObject(JSONSubItem).Properties['index'],'index'),result.fPBRMetallicRoughness.fMetallicRoughnessTexture.fIndex);
        result.fPBRMetallicRoughness.fMetallicRoughnessTexture.fTexCoord:=TPasJSON.GetInt64(TPasJSONItemObject(JSONSubItem).Properties['texCoord'],result.fPBRMetallicRoughness.fMetallicRoughnessTexture.fTexCoord);
       end;
      end;
      result.fPBRMetallicRoughness.fRoughnessFactor:=TPasJSON.GetNumber(TPasJSONItemObject(JSONItem).Properties['roughnessFactor'],result.fPBRMetallicRoughness.fRoughnessFactor);
     end;
    end;
   except
    FreeAndNil(result);
    raise;
   end;
  end;
 var JSONArray:TPasJSONItemArray;
     JSONItem:TPasJSONItem;
 begin
  if not (assigned(aJSONItem) and (aJSONItem is TPasJSONItemArray)) then begin
   raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
  end;
  JSONArray:=TPasJSONItemArray(aJSONItem);
  for JSONItem in JSONArray do begin
   fMaterials.Add(ProcessMaterial(JSONItem));
  end;
 end;
 procedure ProcessMeshes(const aJSONItem:TPasJSONItem);
  function ProcessMesh(const aJSONItem:TPasJSONItem):TMesh;
   function ProcessPrimitive(const aJSONItem:TPasJSONItem):TMesh.TPrimitive;
   var JSONObject:TPasJSONItemObject;
       JSONItem,JSONArrayItem:TPasJSONItem;
       JSONObjectProperty:TPasJSONItemObjectProperty;
       Attributes:TAttributes;
   begin
    if not (assigned(aJSONItem) and (aJSONItem is TPasJSONItemObject)) then begin
     raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
    end;
    JSONObject:=TPasJSONItemObject(aJSONItem);
    result:=TMesh.TPrimitive.Create(self);
    try
     ProcessExtensionsAndExtras(JSONObject,result);
     begin
      JSONItem:=Required(JSONObject.Properties['attributes'],'attributes');
      if not (assigned(JSONItem) and (JSONItem is TPasJSONItemObject)) then begin
       raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
      end;
      for JSONObjectProperty in TPasJSONItemObject(JSONItem) do begin
       result.fAttributes.Add(JSONObjectProperty.Key,TPasJSON.GetInt64(JSONObjectProperty.Value,0));
      end;
     end;
     result.fIndices:=TPasJSON.GetInt64(JSONObject.Properties['indices'],result.fIndices);
     result.fMaterial:=TPasJSON.GetInt64(JSONObject.Properties['material'],result.fMaterial);
     result.fMode:=TMesh.TPrimitive.TMode(TPasJSON.GetInt64(JSONObject.Properties['mode'],TPasGLTFInt64(result.fMode)));
     begin
      JSONItem:=JSONObject.Properties['targets'];
      if assigned(JSONItem) then begin
       if not (JSONItem is TPasJSONItemArray) then begin
        raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
       end;
       for JSONArrayItem in TPasJSONItemArray(JSONItem) do begin
        if not (assigned(JSONArrayItem) and (JSONArrayItem is TPasJSONItemObject)) then begin
         raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
        end;
        Attributes:=TAttributes.Create(-1);
        try
         for JSONObjectProperty in TPasJSONItemObject(JSONArrayItem) do begin
          Attributes.Add(JSONObjectProperty.Key,TPasJSON.GetInt64(JSONObjectProperty.Value,0));
         end;
        finally
         result.fTargets.Add(Attributes);
        end;
       end;
      end;
     end;
    except
     FreeAndNil(result);
     raise;
    end;
   end;
  var JSONObject:TPasJSONItemObject;
      JSONItem,JSONArrayItem:TPasJSONItem;
  begin
   if not (assigned(aJSONItem) and (aJSONItem is TPasJSONItemObject)) then begin
    raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
   end;
   JSONObject:=TPasJSONItemObject(aJSONItem);
   result:=TMesh.Create(self);
   try
    ProcessExtensionsAndExtras(JSONObject,result);
    result.fName:=TPasJSON.GetString(JSONObject.Properties['name'],result.fName);
    begin
     JSONItem:=Required(JSONObject.Properties['primitives'],'primitives');
     if not (assigned(JSONItem) and (JSONItem is TPasJSONItemArray)) then begin
      raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
     end;
     for JSONArrayItem in TPasJSONItemArray(JSONItem) do begin
      result.fPrimitives.Add(ProcessPrimitive(JSONArrayItem));
     end;
    end;
    begin
     JSONItem:=JSONObject.Properties['weights'];
     if assigned(JSONItem) then begin
      if not (JSONItem is TPasJSONItemArray) then begin
       raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
      end;
      for JSONArrayItem in TPasJSONItemArray(JSONItem) do begin
       result.fWeights.Add(TPasJSON.GetNumber(JSONArrayItem,0.0));
      end;
     end;
    end;
   except
    FreeAndNil(result);
    raise;
   end;
  end;
 var JSONArray:TPasJSONItemArray;
     JSONItem:TPasJSONItem;
 begin
  if not (assigned(aJSONItem) and (aJSONItem is TPasJSONItemArray)) then begin
   raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
  end;
  JSONArray:=TPasJSONItemArray(aJSONItem);
  for JSONItem in JSONArray do begin
   fMeshes.Add(ProcessMesh(JSONItem));
  end;
 end;
 procedure ProcessNodes(const aJSONItem:TPasJSONItem);
  function ProcessNode(const aJSONItem:TPasJSONItem):TNode;
  var JSONObject:TPasJSONItemObject;
      JSONItem,JSONArrayItem:TPasJSONItem;
      Index:TPasGLTFSizeInt;
  begin
   if not (assigned(aJSONItem) and (aJSONItem is TPasJSONItemObject)) then begin
    raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
   end;
   JSONObject:=TPasJSONItemObject(aJSONItem);
   result:=TNode.Create(self);
   try
    ProcessExtensionsAndExtras(JSONObject,result);
    result.fCamera:=TPasJSON.GetInt64(JSONObject.Properties['camera'],result.fCamera);
    begin
     JSONItem:=JSONObject.Properties['children'];
     if assigned(JSONItem) then begin
      if not (JSONItem is TPasJSONItemArray) then begin
       raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
      end;
      for JSONArrayItem in TPasJSONItemArray(JSONItem) do begin
       result.fChildren.Add(TPasJSON.GetInt64(JSONArrayItem));
      end;
     end;
    end;
    begin
     JSONItem:=JSONObject.Properties['matrix'];
     if assigned(JSONItem) then begin
      if not ((JSONItem is TPasJSONItemArray) and (TPasJSONItemArray(JSONItem).Count=16)) then begin
       raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
      end;
      for Index:=0 to 15 do begin
       result.fMatrix[Index]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[Index],result.fMatrix[Index]);
      end;
     end;
    end;
    result.fMesh:=TPasJSON.GetInt64(JSONObject.Properties['mesh'],result.fMesh);
    result.fName:=TPasJSON.GetString(JSONObject.Properties['name'],result.fName);
    begin
     JSONItem:=JSONObject.Properties['rotation'];
     if assigned(JSONItem) then begin
      if not ((JSONItem is TPasJSONItemArray) and (TPasJSONItemArray(JSONItem).Count=4)) then begin
       raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
      end;
      for Index:=0 to 3 do begin
       result.fRotation[Index]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[Index],result.fRotation[Index]);
      end;
     end;
    end;
    begin
     JSONItem:=JSONObject.Properties['scale'];
     if assigned(JSONItem) then begin
      if not ((JSONItem is TPasJSONItemArray) and (TPasJSONItemArray(JSONItem).Count=3)) then begin
       raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
      end;
      for Index:=0 to 2 do begin
       result.fScale[Index]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[Index],result.fScale[Index]);
      end;
     end;
    end;
    result.fSkin:=TPasJSON.GetInt64(JSONObject.Properties['skin'],result.fSkin);
    begin
     JSONItem:=JSONObject.Properties['translation'];
     if assigned(JSONItem) then begin
      if not ((JSONItem is TPasJSONItemArray) and (TPasJSONItemArray(JSONItem).Count=3)) then begin
       raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
      end;
      for Index:=0 to 2 do begin
       result.fTranslation[Index]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[Index],result.fTranslation[Index]);
      end;
     end;
    end;
   except
    FreeAndNil(result);
    raise;
   end;
  end;
 var JSONArray:TPasJSONItemArray;
     JSONItem:TPasJSONItem;
 begin
  if not (assigned(aJSONItem) and (aJSONItem is TPasJSONItemArray)) then begin
   raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
  end;
  JSONArray:=TPasJSONItemArray(aJSONItem);
  for JSONItem in JSONArray do begin
   fNodes.Add(ProcessNode(JSONItem));
  end;
 end;
 procedure ProcessSamplers(const aJSONItem:TPasJSONItem);
  function ProcessSampler(const aJSONItem:TPasJSONItem):TSampler;
  var JSONObject:TPasJSONItemObject;
  begin
   if not (assigned(aJSONItem) and (aJSONItem is TPasJSONItemObject)) then begin
    raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
   end;
   JSONObject:=TPasJSONItemObject(aJSONItem);
   result:=TSampler.Create(self);
   try
    ProcessExtensionsAndExtras(JSONObject,result);
    result.fMagFilter:=TSampler.TMagFilter(TPasJSON.GetInt64(JSONObject.Properties['magFilter'],TPasGLTFInt64(result.fMagFilter)));
    result.fMinFilter:=TSampler.TMinFilter(TPasJSON.GetInt64(JSONObject.Properties['minFilter'],TPasGLTFInt64(result.fMinFilter)));
    result.fName:=TPasJSON.GetString(JSONObject.Properties['name'],result.fName);
    result.fWrapS:=TSampler.TWrappingMode(TPasJSON.GetInt64(JSONObject.Properties['wrapS'],TPasGLTFInt64(result.fWrapS)));
    result.fWrapT:=TSampler.TWrappingMode(TPasJSON.GetInt64(JSONObject.Properties['wrapT'],TPasGLTFInt64(result.fWrapT)));
   except
    FreeAndNil(result);
    raise;
   end;
  end;
 var JSONArray:TPasJSONItemArray;
     JSONItem:TPasJSONItem;
 begin
  if not (assigned(aJSONItem) and (aJSONItem is TPasJSONItemArray)) then begin
   raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
  end;
  JSONArray:=TPasJSONItemArray(aJSONItem);
  for JSONItem in JSONArray do begin
   fSamplers.Add(ProcessSampler(JSONItem));
  end;
 end;
 procedure ProcessScenes(const aJSONItem:TPasJSONItem);
  function ProcessScene(const aJSONItem:TPasJSONItem):TScene;
  var JSONObject:TPasJSONItemObject;
      JSONItem,JSONArrayItem:TPasJSONItem;
  begin
   if not (assigned(aJSONItem) and (aJSONItem is TPasJSONItemObject)) then begin
    raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
   end;
   JSONObject:=TPasJSONItemObject(aJSONItem);
   result:=TScene.Create(self);
   try
    ProcessExtensionsAndExtras(JSONObject,result);
    result.fName:=TPasJSON.GetString(JSONObject.Properties['name'],result.fName);
    begin
     JSONItem:=JSONObject.Properties['nodes'];
     if assigned(JSONItem) then begin
      if not (JSONItem is TPasJSONItemArray) then begin
       raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
      end;
      for JSONArrayItem in TPasJSONItemArray(JSONItem) do begin
       result.fNodes.Add(TPasJSON.GetInt64(JSONArrayItem));
      end;
     end;
    end;
   except
    FreeAndNil(result);
    raise;
   end;
  end;
 var JSONArray:TPasJSONItemArray;
     JSONItem:TPasJSONItem;
 begin
  if not (assigned(aJSONItem) and (aJSONItem is TPasJSONItemArray)) then begin
   raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
  end;
  JSONArray:=TPasJSONItemArray(aJSONItem);
  for JSONItem in JSONArray do begin
   fScenes.Add(ProcessScene(JSONItem));
  end;
 end;
 procedure ProcessSkins(const aJSONItem:TPasJSONItem);
  function ProcessSkin(const aJSONItem:TPasJSONItem):TSkin;
  var JSONObject:TPasJSONItemObject;
      JSONItem,JSONArrayItem:TPasJSONItem;
  begin
   if not (assigned(aJSONItem) and (aJSONItem is TPasJSONItemObject)) then begin
    raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
   end;
   JSONObject:=TPasJSONItemObject(aJSONItem);
   result:=TSkin.Create(self);
   try
    ProcessExtensionsAndExtras(JSONObject,result);
    result.fName:=TPasJSON.GetString(JSONObject.Properties['name'],result.fName);
    begin
     JSONItem:=Required(JSONObject.Properties['joints']);
     if assigned(JSONItem) then begin
      if not (JSONItem is TPasJSONItemArray) then begin
       raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
      end;
      for JSONArrayItem in TPasJSONItemArray(JSONItem) do begin
       result.fJoints.Add(TPasJSON.GetInt64(JSONArrayItem));
      end;
     end;
    end;
    result.fInverseBindMatrices:=TPasJSON.GetInt64(JSONObject.Properties['inverseBindMatrices'],result.fInverseBindMatrices);
    result.fSkeleton:=TPasJSON.GetInt64(JSONObject.Properties['skeleton'],result.fSkeleton);
   except
    FreeAndNil(result);
    raise;
   end;
  end;
 var JSONArray:TPasJSONItemArray;
     JSONItem:TPasJSONItem;
 begin
  if not (assigned(aJSONItem) and (aJSONItem is TPasJSONItemArray)) then begin
   raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
  end;
  JSONArray:=TPasJSONItemArray(aJSONItem);
  for JSONItem in JSONArray do begin
   fSkins.Add(ProcessSkin(JSONItem));
  end;
 end;
 procedure ProcessTextures(const aJSONItem:TPasJSONItem);
  function ProcessTexture(const aJSONItem:TPasJSONItem):TTexture;
  var JSONObject:TPasJSONItemObject;
  begin
   if not (assigned(aJSONItem) and (aJSONItem is TPasJSONItemObject)) then begin
    raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
   end;
   JSONObject:=TPasJSONItemObject(aJSONItem);
   result:=TTexture.Create(self);
   try
    ProcessExtensionsAndExtras(JSONObject,result);
    result.fName:=TPasJSON.GetString(JSONObject.Properties['name'],result.fName);
    result.fSampler:=TPasJSON.GetInt64(JSONObject.Properties['sampler'],result.fSampler);
    result.fSource:=TPasJSON.GetInt64(JSONObject.Properties['source'],result.fSource);
   except
    FreeAndNil(result);
    raise;
   end;
  end;
 var JSONArray:TPasJSONItemArray;
     JSONItem:TPasJSONItem;
 begin
  if not (assigned(aJSONItem) and (aJSONItem is TPasJSONItemArray)) then begin
   raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
  end;
  JSONArray:=TPasJSONItemArray(aJSONItem);
  for JSONItem in JSONArray do begin
   fTextures.Add(ProcessTexture(JSONItem));
  end;
 end;
 procedure ProcessStringList(const aJSONItem:TPasJSONItem;const aStrings:TStrings);
 var JSONArray:TPasJSONItemArray;
     JSONItem:TPasJSONItem;
 begin
  if not (assigned(aJSONItem) and (aJSONItem is TPasJSONItemArray)) then begin
   raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
  end;
  JSONArray:=TPasJSONItemArray(aJSONItem);
  for JSONItem in JSONArray do begin
   aStrings.Add(TPasJSON.GetString(JSONItem,''));
  end;
 end;
var JSONObject:TPasJSONItemObject;
    JSONObjectProperty:TPasJSONItemObjectProperty;
    HasAsset:boolean;
begin
 if not (assigned(aJSONRootItem) and (aJSONRootItem is TPasJSONItemObject)) then begin
  raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
 end;
 JSONObject:=TPasJSONItemObject(aJSONRootItem);
 ProcessExtensionsAndExtras(JSONObject,self);
 HasAsset:=false;
 for JSONObjectProperty in JSONObject do begin
  if JSONObjectProperty.Key='accessors' then begin
   ProcessAccessors(JSONObjectProperty.Value);
  end else if JSONObjectProperty.Key='animations' then begin
   ProcessAnimations(JSONObjectProperty.Value);
  end else if JSONObjectProperty.Key='asset' then begin
   HasAsset:=true;
   ProcessAsset(JSONObjectProperty.Value);
  end else if JSONObjectProperty.Key='buffers' then begin
   ProcessBuffers(JSONObjectProperty.Value);
  end else if JSONObjectProperty.Key='bufferViews' then begin
   ProcessBufferViews(JSONObjectProperty.Value);
  end else if JSONObjectProperty.Key='cameras' then begin
   ProcessCameras(JSONObjectProperty.Value);
  end else if JSONObjectProperty.Key='images' then begin
   ProcessImages(JSONObjectProperty.Value);
  end else if JSONObjectProperty.Key='materials' then begin
   ProcessMaterials(JSONObjectProperty.Value);
  end else if JSONObjectProperty.Key='meshes' then begin
   ProcessMeshes(JSONObjectProperty.Value);
  end else if JSONObjectProperty.Key='nodes' then begin
   ProcessNodes(JSONObjectProperty.Value);
  end else if JSONObjectProperty.Key='samplers' then begin
   ProcessSamplers(JSONObjectProperty.Value);
  end else if JSONObjectProperty.Key='scene' then begin
   fScene:=TPasJSON.GetInt64(JSONObjectProperty.Value,fScene);
  end else if JSONObjectProperty.Key='scenes' then begin
   ProcessScenes(JSONObjectProperty.Value);
  end else if JSONObjectProperty.Key='skins' then begin
   ProcessSkins(JSONObjectProperty.Value);
  end else if JSONObjectProperty.Key='textures' then begin
   ProcessTextures(JSONObjectProperty.Value);
  end else if JSONObjectProperty.Key='extensionsUsed' then begin
   ProcessStringList(JSONObjectProperty.Value,fExtensionsUsed);
  end else if JSONObjectProperty.Key='extensionsRequired' then begin
   ProcessStringList(JSONObjectProperty.Value,fExtensionsRequired);
  end;
 end;
 if not HasAsset then begin
  raise EPasGLTFInvalidDocument.Create('Invalid GLTF document');
 end;
 LoadURISources;
end;

procedure TPasGLTF.TDocument.LoadFromBinary(const aStream:TStream);
var GLBHeader:TGLBHeader;
    OtherEndianness:boolean;
 function SwapEndianness32(const aValue:TPasGLTFUInt32):TPasGLTFUInt32;
 begin
  if OtherEndianness then begin
   result:=((aValue and $000000ff) shl 24) or
           ((aValue and $0000ff00) shl 8) or
           ((aValue and $00ff0000) shr 8) or
           ((aValue and $ff000000) shr 24);
  end else begin
   result:=aValue;
  end;
 end;
var RawJSONRawByteString:TPasJSONRawByteString;
    ChunkHeader:TChunkHeader;
    Stream:TMemoryStream;
    JSONItem:TPasJSONItem;
begin
 if not (assigned(aStream) and (aStream.Size>=GLBHeaderSize)) then begin
  raise EPasGLTFInvalidDocument.Create('Invalid GLB document');
 end;
 if aStream.Read(GLBHeader,SizeOf(TGLBHeader))<>SizeOf(TGLBHeader) then begin
  raise EPasGLTFInvalidDocument.Create('Invalid GLB document');
 end;
 if (GLBHeader.Magic<>GLBHeaderMagicNativeEndianness) and
    (GLBHeader.Magic<>GLBHeaderMagicOtherEndianness) then begin
  raise EPasGLTFInvalidDocument.Create('Invalid GLB document');
 end;
 OtherEndianness:=GLBHeader.Magic=GLBHeaderMagicOtherEndianness;
 if not ((not OtherEndianness) and (GLBHeader.JSONChunkHeader.ChunkType=GLBChunkJSONNativeEndianness)) or
         (OtherEndianness and (GLBHeader.JSONChunkHeader.ChunkType=GLBChunkJSONOtherEndianness)) then begin
  raise EPasGLTFInvalidDocument.Create('Invalid GLB document');
 end;
 GLBHeader.Magic:=SwapEndianness32(GLBHeader.Magic);
 GLBHeader.Version:=SwapEndianness32(GLBHeader.Version);
 GLBHeader.Length:=SwapEndianness32(GLBHeader.Length);
 GLBHeader.JSONChunkHeader.ChunkLength:=SwapEndianness32(GLBHeader.JSONChunkHeader.ChunkLength);
 GLBHeader.JSONChunkHeader.ChunkType:=SwapEndianness32(GLBHeader.JSONChunkHeader.ChunkType);
 if ((GLBHeader.JSONChunkHeader.ChunkLength+GLBHeaderSize)>GLBHeader.Length) or
    (GLBHeader.JSONChunkHeader.ChunkLength<2) then begin
  raise EPasGLTFInvalidDocument.Create('Invalid GLB document');
 end;
 RawJSONRawByteString:='';
 SetLength(RawJSONRawByteString,GLBHeader.JSONChunkHeader.ChunkLength);
 aStream.ReadBuffer(RawJSONRawByteString[1],length(RawJSONRawByteString));
 JSONItem:=TPasJSON.Parse(RawJSONRawByteString,[],TPasJSONEncoding.UTF8);
 if assigned(JSONItem) then begin
  try
   LoadFromJSON(JSONItem);
  finally
   FreeAndNil(JSONItem);
  end;
 end;
 if aStream.Position<aStream.Size then begin
  if aStream.Read(ChunkHeader,SizeOf(TChunkHeader))<>SizeOf(ChunkHeader) then begin
   raise EPasGLTFInvalidDocument.Create('Invalid GLB document');
  end;
  ChunkHeader.ChunkLength:=SwapEndianness32(ChunkHeader.ChunkLength);
  ChunkHeader.ChunkType:=SwapEndianness32(ChunkHeader.ChunkType);
  if (ChunkHeader.ChunkType<>GLBChunkBinaryNativeEndianness) or
     ((ChunkHeader.ChunkLength+aStream.Position)>GLBHeader.Length) then begin
   raise EPasGLTFInvalidDocument.Create('Invalid GLB document');
  end;
  if fBuffers.Count<1 then begin
   fBuffers.Add(TBuffer.Create(self));
  end;
  Stream:=fBuffers[0].fData;
  Stream.Clear;
  Stream.CopyFrom(aStream,ChunkHeader.ChunkLength);
 end;
end;

procedure TPasGLTF.TDocument.LoadFromStream(const aStream:TStream);
var FirstFourBytes:array[0..3] of TPasGLTFUInt8;
    JSONItem:TPasJSONItem;
begin
 aStream.ReadBuffer(FirstFourBytes,SizeOf(FirstFourBytes));
 aStream.Seek(-SizeOf(FirstFourBytes),soCurrent);
 if (FirstFourBytes[0]=ord('g')) and
    (FirstFourBytes[1]=ord('l')) and
    (FirstFourBytes[2]=ord('T')) and
    (FirstFourBytes[3]=ord('F')) then begin
  LoadFromBinary(aStream);
 end else begin
  JSONItem:=TPasJSON.Parse(aStream,[],TPasJSONEncoding.AutomaticDetection);
  if assigned(JSONItem) then begin
   try
    LoadFromJSON(JSONItem);
   finally
    FreeAndNil(JSONItem);
   end;
  end;
 end;
end;

function TPasGLTF.TDocument.SaveToJSON(const aFormatted:boolean=false):TPasJSONRawByteString;
 procedure ProcessExtensionsAndExtras(const aJSONObject:TPasJSONItemObject;const aBaseExtensionsExtrasObject:TBaseExtensionsExtrasObject);
 var TemporaryObject,TemporarySubObject:TPasJSONItemObject;
 begin
  TemporaryObject:=TPasJSONItemObject.Create;
  try
   if aBaseExtensionsExtrasObject.fExtensions.Count>0 then begin
    TemporarySubObject:=TPasJSONItemObject.Create;
    try
     TemporarySubObject.Merge(aBaseExtensionsExtrasObject.fExtensions);
    finally
     TemporaryObject.Add('extensions',TemporarySubObject);
    end;
   end;
   if aBaseExtensionsExtrasObject.fExtras.Count>0 then begin
    TemporarySubObject:=TPasJSONItemObject.Create;
    try
     TemporarySubObject.Merge(aBaseExtensionsExtrasObject.fExtras);
    finally
     TemporaryObject.Add('extras',TemporarySubObject);
    end;
   end;
   aJSONObject.Merge(TemporaryObject);
  finally
   FreeAndNil(TemporaryObject);
  end;
 end;
 function ProcessAccessors:TPasJSONItemArray;
  function ProcessAccessor(const aObject:TAccessor):TPasJSONItemObject;
  var Index:TPasJSONSizeInt;
      JSONArray:TPasJSONItemArray;
      JSONObject,JSONSubObject:TPasJSONItemObject;
  begin
   result:=TPasJSONItemObject.Create;
   try
    if aObject.fBufferView>=0 then begin
     result.Add('bufferView',TPasJSONItemNumber.Create(aObject.fBufferView));
    end;
    result.Add('byteOffset',TPasJSONItemNumber.Create(aObject.fByteOffset));
    if aObject.fComponentType<>TAccessor.TComponentType.None then begin
     result.Add('componentType',TPasJSONItemNumber.Create(TPasGLTFInt64(aObject.fComponentType)));
    end;
    result.Add('count',TPasJSONItemNumber.Create(aObject.fCount));
    if aObject.fMinArray.Count>0 then begin
     JSONArray:=TPasJSONItemArray.Create;
     try
      for Index:=0 to aObject.fMinArray.Count-1 do begin
       JSONArray.Add(TPasJSONItemNumber.Create(aObject.fMinArray.Items[Index]));
      end;
     finally
      result.Add('min',JSONArray);
     end;
    end;
    if aObject.fMaxArray.Count>0 then begin
     JSONArray:=TPasJSONItemArray.Create;
     try
      for Index:=0 to aObject.fMaxArray.Count-1 do begin
       JSONArray.Add(TPasJSONItemNumber.Create(aObject.fMaxArray.Items[Index]));
      end;
     finally
      result.Add('max',JSONArray);
     end;
    end;
    if length(aObject.fName)>0 then begin
     result.Add('name',TPasJSONItemString.Create(aObject.fName));
    end;
    if aObject.Normalized then begin
     result.Add('normalized',TPasJSONItemBoolean.Create(aObject.Normalized));
    end;
    if not aObject.fSparse.Empty then begin
     JSONObject:=TPasJSONItemObject.Create;
     try
      if aObject.fSparse.fCount>=0 then begin
       JSONObject.Add('count',TPasJSONItemNumber.Create(aObject.fSparse.fCount));
      end;
      if not aObject.fSparse.fIndices.Empty then begin
       JSONSubObject:=TPasJSONItemObject.Create;
       try
        if aObject.fSparse.fIndices.fComponentType<>TAccessor.TComponentType.None then begin
         JSONSubObject.Add('componentType',TPasJSONItemNumber.Create(TPasGLTFInt64(aObject.fSparse.fIndices.fComponentType)));
        end;
        if aObject.fSparse.fIndices.fBufferView>=0 then begin
         JSONSubObject.Add('bufferView',TPasJSONItemNumber.Create(aObject.fSparse.fIndices.fBufferView));
        end;
        JSONSubObject.Add('byteOffset',TPasJSONItemNumber.Create(aObject.fSparse.fIndices.fByteOffset));
       finally
        JSONObject.Add('indices',JSONSubObject);
       end;
      end;
      if not aObject.fSparse.fValues.Empty then begin
       JSONSubObject:=TPasJSONItemObject.Create;
       try
        if aObject.fSparse.fValues.fBufferView>=0 then begin
         JSONSubObject.Add('bufferView',TPasJSONItemNumber.Create(aObject.fSparse.fValues.fBufferView));
        end;
        JSONSubObject.Add('byteOffset',TPasJSONItemNumber.Create(aObject.fSparse.fValues.fByteOffset));
       finally
        JSONObject.Add('values',JSONSubObject);
       end;
      end;
     finally
      result.Add('sparse',JSONObject);
     end;
    end;
    case aObject.fType of
     TPasGLTF.TAccessor.TType.Scalar:begin
      result.Add('type',TPasJSONItemString.Create('SCALAR'));
     end;
     TPasGLTF.TAccessor.TType.Vec2:begin
      result.Add('type',TPasJSONItemString.Create('VEC2'));
     end;
     TPasGLTF.TAccessor.TType.Vec3:begin
      result.Add('type',TPasJSONItemString.Create('VEC3'));
     end;
     TPasGLTF.TAccessor.TType.Vec4:begin
      result.Add('type',TPasJSONItemString.Create('VEC4'));
     end;
     TPasGLTF.TAccessor.TType.Mat2:begin
      result.Add('type',TPasJSONItemString.Create('MAT2'));
     end;
     TPasGLTF.TAccessor.TType.Mat3:begin
      result.Add('type',TPasJSONItemString.Create('MAT3'));
     end;
     TPasGLTF.TAccessor.TType.Mat4:begin
      result.Add('type',TPasJSONItemString.Create('MAT4'));
     end;
    end;
    ProcessExtensionsAndExtras(result,aObject);
   except
    FreeAndNil(result);
    raise;
   end;
  end;
 var Accessor:TAccessor;
 begin
  result:=TPasJSONItemArray.Create;
  try
   for Accessor in fAccessors do begin
    result.Add(ProcessAccessor(Accessor));
   end;
  except
   FreeAndNil(result);
   raise;
  end;
 end;
 function ProcessAnimations:TPasJSONItemArray;
  function ProcessAnimation(const aObject:TAnimation):TPasJSONItemObject;
  var JSONArray:TPasJSONItemArray;
      JSONObject,JSONSubObject:TPasJSONItemObject;
      Channel:TAnimation.TChannel;
      Sampler:TAnimation.TSampler;
  begin
   result:=TPasJSONItemObject.Create;
   try
    if length(aObject.fName)>0 then begin
     result.Add('name',TPasJSONItemString.Create(aObject.fName));
    end;
    if aObject.fChannels.Count>0 then begin
     JSONArray:=TPasJSONItemArray.Create;
     try
      for Channel in aObject.fChannels do begin
       JSONObject:=TPasJSONItemObject.Create;
       try
        if Channel.fSampler>=0 then begin
         JSONObject.Add('sampler',TPasJSONItemNumber.Create(Channel.fSampler));
        end;
        if not Channel.fTarget.Empty then begin
         JSONSubObject:=TPasJSONItemObject.Create;
         try
          if Channel.fTarget.fNode>=0 then begin
           JSONSubObject.Add('node',TPasJSONItemNumber.Create(Channel.fTarget.fNode));
          end;
          if length(Channel.fTarget.fPath)>0 then begin
           JSONSubObject.Add('path',TPasJSONItemString.Create(Channel.fTarget.fPath));
          end;
          ProcessExtensionsAndExtras(JSONSubObject,Channel.fTarget);
         finally
          JSONObject.Add('target',JSONSubObject);
         end;
        end;
        ProcessExtensionsAndExtras(JSONObject,Channel);
       finally
        JSONArray.Add(JSONObject);
       end;
      end;
     finally
      result.Add('channels',JSONArray);
     end;
    end;
    if aObject.fSamplers.Count>0 then begin
     JSONArray:=TPasJSONItemArray.Create;
     try
      for Sampler in aObject.fSamplers do begin
       JSONObject:=TPasJSONItemObject.Create;
       try
        if Sampler.fInput>=0 then begin
         JSONObject.Add('input',TPasJSONItemNumber.Create(Sampler.fInput));
        end;
        if Sampler.fOutput>=0 then begin
         JSONObject.Add('output',TPasJSONItemNumber.Create(Sampler.fOutput));
        end;
        case Sampler.fInterpolation of
         TPasGLTF.TAnimation.TSampler.TSamplerType.Linear:begin
          JSONObject.Add('interpolation',TPasJSONItemString.Create('LINEAR'));
         end;
         TPasGLTF.TAnimation.TSampler.TSamplerType.Step:begin
          JSONObject.Add('interpolation',TPasJSONItemString.Create('STEP'));
         end;
         TPasGLTF.TAnimation.TSampler.TSamplerType.CubicSpline:begin
          JSONObject.Add('interpolation',TPasJSONItemString.Create('CUBICSPLINE'));
         end;
         else begin
          Assert(false);
         end;
        end;
       finally
        JSONArray.Add(JSONObject);
       end;
      end;
     finally
      result.Add('samplers',JSONArray);
     end;
    end;
    ProcessExtensionsAndExtras(result,aObject);
   except
    FreeAndNil(result);
    raise;
   end;
  end;
 var Animation:TAnimation;
 begin
  result:=TPasJSONItemArray.Create;
  try
   for Animation in fAnimations do begin
    result.Add(ProcessAnimation(Animation));
   end;
  except
   FreeAndNil(result);
   raise;
  end;
 end;
 function ProcessAsset:TPasJSONItemObject;
 begin
  result:=TPasJSONItemObject.Create;
  try
   if length(fAsset.fCopyright)>0 then begin
    result.Add('copyright',TPasJSONItemString.Create(fAsset.fCopyright));
   end;
   if length(fAsset.fGenerator)>0 then begin
    result.Add('generator',TPasJSONItemString.Create(fAsset.fGenerator));
   end;
   if length(fAsset.fMinVersion)>0 then begin
    result.Add('minVersion',TPasJSONItemString.Create(fAsset.fMinVersion));
   end;
   result.Add('version',TPasJSONItemString.Create(fAsset.fVersion));
   ProcessExtensionsAndExtras(result,fAsset);
  except
   FreeAndNil(result);
   raise;
  end;
 end;
 function ProcessBuffers:TPasJSONItemArray;
  function ProcessBuffer(const aObject:TBuffer):TPasJSONItemObject;
  begin
   result:=TPasJSONItemObject.Create;
   try
    if length(aObject.fName)>0 then begin
     result.Add('name',TPasJSONItemString.Create(aObject.fName));
    end;
    if length(aObject.fURI)>0 then begin
     result.Add('uri',TPasJSONItemString.Create(aObject.fURI));
    end;
    result.Add('byteLength',TPasJSONItemNumber.Create(aObject.fByteLength));
    ProcessExtensionsAndExtras(result,aObject);
   except
    FreeAndNil(result);
    raise;
   end;
  end;
 var Buffer:TBuffer;
 begin
  result:=TPasJSONItemArray.Create;
  try
   for Buffer in fBuffers do begin
    result.Add(ProcessBuffer(Buffer));
   end;
  except
   FreeAndNil(result);
   raise;
  end;
 end;
 function ProcessBufferViews:TPasJSONItemArray;
  function ProcessBufferView(const aObject:TBufferView):TPasJSONItemObject;
  begin
   result:=TPasJSONItemObject.Create;
   try
    if length(aObject.fName)>0 then begin
     result.Add('name',TPasJSONItemString.Create(aObject.fName));
    end;
    if aObject.fBuffer>=0 then begin
     result.Add('buffer',TPasJSONItemNumber.Create(aObject.fBuffer));
    end;
    result.Add('byteLength',TPasJSONItemNumber.Create(aObject.fByteLength));
    result.Add('byteOffset',TPasJSONItemNumber.Create(aObject.fByteOffset));
    if aObject.fByteStride>0 then begin
     result.Add('byteStride',TPasJSONItemNumber.Create(aObject.fByteStride));
    end;
    if aObject.fTarget<>TBufferView.TTargetType.None then begin
     result.Add('target',TPasJSONItemNumber.Create(TPasGLTFInt64(aObject.fTarget)));
    end;
    ProcessExtensionsAndExtras(result,aObject);
   except
    FreeAndNil(result);
    raise;
   end;
  end;
 var BufferView:TBufferView;
 begin
  result:=TPasJSONItemArray.Create;
  try
   for BufferView in fBufferViews do begin
    result.Add(ProcessBufferView(BufferView));
   end;
  except
   FreeAndNil(result);
   raise;
  end;
 end;
 function ProcessCameras:TPasJSONItemArray;
  function ProcessCamera(const aObject:TCamera):TPasJSONItemObject;
  var JSONObject:TPasJSONItemObject;
  begin
   result:=TPasJSONItemObject.Create;
   try
    if length(aObject.fName)>0 then begin
     result.Add('name',TPasJSONItemString.Create(aObject.fName));
    end;
    case aObject.Type_ of
     TPasGLTF.TCamera.TCameraType.Orthographic:begin
      result.Add('type',TPasJSONItemString.Create('orthographic'));
      if not aObject.Orthographic.Empty then begin
       JSONObject:=TPasJSONItemObject.Create;
       try
        if aObject.Orthographic.fXMag<>TDefaults.FloatSentinel then begin
         JSONObject.Add('xmag',TPasJSONItemNumber.Create(aObject.Orthographic.fXMag));
        end;
        if aObject.Orthographic.fYMag<>TDefaults.FloatSentinel then begin
         JSONObject.Add('ymag',TPasJSONItemNumber.Create(aObject.Orthographic.fYMag));
        end;
        if aObject.Orthographic.fZNear<>-TDefaults.FloatSentinel then begin
         JSONObject.Add('znear',TPasJSONItemNumber.Create(aObject.Orthographic.fZNear));
        end;
        if aObject.Orthographic.fZFar<>-TDefaults.FloatSentinel then begin
         JSONObject.Add('zfar',TPasJSONItemNumber.Create(aObject.Orthographic.fZfar));
        end;
        ProcessExtensionsAndExtras(JSONObject,aObject.Orthographic);
       finally
        result.Add('orthographic',JSONObject);
       end;
      end;
     end;
     TPasGLTF.TCamera.TCameraType.Perspective:begin
      result.Add('type',TPasJSONItemString.Create('perspective'));
      if not aObject.Perspective.Empty then begin
       JSONObject:=TPasJSONItemObject.Create;
       try
        JSONObject.Add('aspectRatio',TPasJSONItemNumber.Create(aObject.Perspective.fAspectRatio));
        JSONObject.Add('yfov',TPasJSONItemNumber.Create(aObject.Perspective.fYFov));
        JSONObject.Add('znear',TPasJSONItemNumber.Create(aObject.Perspective.fZNear));
        JSONObject.Add('zfar',TPasJSONItemNumber.Create(aObject.Perspective.fZfar));
        ProcessExtensionsAndExtras(JSONObject,aObject.Perspective);
       finally
        result.Add('perspective',JSONObject);
       end;
      end;
     end;
     else begin
      Assert(false);
     end;
    end;
    ProcessExtensionsAndExtras(result,aObject);
   except
    FreeAndNil(result);
    raise;
   end;
  end;
 var Camera:TCamera;
 begin
  result:=TPasJSONItemArray.Create;
  try
   for Camera in fCameras do begin
    result.Add(ProcessCamera(Camera));
   end;
  except
   FreeAndNil(result);
   raise;
  end;
 end;
 function ProcessImages:TPasJSONItemArray;
  function ProcessImage(const aObject:TImage):TPasJSONItemObject;
  begin
   result:=TPasJSONItemObject.Create;
   try
    if ((aObject.fBufferView>=0) and (length(aObject.fURI)=0)) or
       ((aObject.fBufferView>0) and (length(aObject.fURI)>0)) then begin
     result.Add('bufferView',TPasJSONItemNumber.Create(aObject.fBufferView));
    end;
    if length(aObject.fName)>0 then begin
     result.Add('name',TPasJSONItemString.Create(aObject.fName));
    end;
    if length(aObject.fMimeType)>0 then begin
     result.Add('mimeType',TPasJSONItemString.Create(aObject.fMimeType));
    end;
    if length(aObject.fURI)>0 then begin
     result.Add('uri',TPasJSONItemString.Create(aObject.fURI));
    end;
    ProcessExtensionsAndExtras(result,aObject);
   except
    FreeAndNil(result);
    raise;
   end;
  end;
 var Image:TImage;
 begin
  result:=TPasJSONItemArray.Create;
  try
   for Image in fImages do begin
    result.Add(ProcessImage(Image));
   end;
  except
   FreeAndNil(result);
   raise;
  end;
 end;
 function ProcessMaterials:TPasJSONItemArray;
  function ProcessMaterial(const aObject:TMaterial):TPasJSONItemObject;
  var JSONArray:TPasJSONItemArray;
      JSONObject,JSONSubObject:TPasJSONItemObject;
  begin
   result:=TPasJSONItemObject.Create;
   try
    if aObject.fAlphaCutOff<>TDefaults.MaterialAlphaCutoff then begin
     result.Add('alphaCutoff',TPasJSONItemNumber.Create(aObject.fAlphaCutOff));
    end;
    case aObject.fAlphaMode of
     TPasGLTF.TMaterial.TAlphaMode.Opaque:begin
      // Default value
      // result.Add('alphaMode',TPasJSONItemString.Create('OPAQUE'));
     end;
     TPasGLTF.TMaterial.TAlphaMode.Mask:begin
      result.Add('alphaMode',TPasJSONItemString.Create('MASK'));
     end;
     TPasGLTF.TMaterial.TAlphaMode.Blend:begin
      result.Add('alphaMode',TPasJSONItemString.Create('BLEND'));
     end;
     else begin
      Assert(false);
     end;
    end;
    if aObject.fDoubleSided<>TDefaults.MaterialDoubleSided then begin
     result.Add('doubleSided',TPasJSONItemBoolean.Create(aObject.fDoubleSided));
    end;
    if not CompareMem(@aObject.EmissiveFactor,@TDefaults.NullVector3,SizeOf(TVector3)) then begin
     JSONArray:=TPasJSONItemArray.Create;
     try
      JSONArray.Add(TPasJSONItemNumber.Create(aObject.EmissiveFactor[0]));
      JSONArray.Add(TPasJSONItemNumber.Create(aObject.EmissiveFactor[1]));
      JSONArray.Add(TPasJSONItemNumber.Create(aObject.EmissiveFactor[2]));
     finally
      result.Add('emissiveFactor',JSONArray);
     end;
    end;
    if not aObject.fEmissiveTexture.Empty then begin
     JSONObject:=TPasJSONItemObject.Create;
     try
      if aObject.fEmissiveTexture.fIndex>=0 then begin
       JSONObject.Add('index',TPasJSONItemNumber.Create(aObject.fEmissiveTexture.fIndex));
      end;
      if aObject.fEmissiveTexture.fTexCoord>0 then begin
       JSONObject.Add('texCoord',TPasJSONItemNumber.Create(aObject.fEmissiveTexture.fTexCoord));
      end;
      ProcessExtensionsAndExtras(JSONObject,aObject.fEmissiveTexture);
     finally
      result.Add('emissiveTexture',JSONObject);
     end;
    end;
    if length(aObject.fName)>0 then begin
     result.Add('name',TPasJSONItemString.Create(aObject.fName));
    end;
    if not aObject.fNormalTexture.Empty then begin
     JSONObject:=TPasJSONItemObject.Create;
     try
      if aObject.fNormalTexture.fIndex>=0 then begin
       JSONObject.Add('index',TPasJSONItemNumber.Create(aObject.fNormalTexture.fIndex));
      end;
      if aObject.fNormalTexture.fTexCoord>0 then begin
       JSONObject.Add('texCoord',TPasJSONItemNumber.Create(aObject.fNormalTexture.fTexCoord));
      end;
      if aObject.fNormalTexture.fScale<>TDefaults.IdentityScalar then begin
       JSONObject.Add('scale',TPasJSONItemNumber.Create(aObject.fNormalTexture.fScale));
      end;
      ProcessExtensionsAndExtras(JSONObject,aObject.fNormalTexture);
     finally
      result.Add('normalTexture',JSONObject);
     end;
    end;
    if not aObject.fOcclusionTexture.Empty then begin
     JSONObject:=TPasJSONItemObject.Create;
     try
      if aObject.fOcclusionTexture.fIndex>=0 then begin
       JSONObject.Add('index',TPasJSONItemNumber.Create(aObject.fOcclusionTexture.fIndex));
      end;
      if aObject.fOcclusionTexture.fTexCoord>0 then begin
       JSONObject.Add('texCoord',TPasJSONItemNumber.Create(aObject.fOcclusionTexture.fTexCoord));
      end;
      if aObject.fOcclusionTexture.fStrength<>TDefaults.IdentityScalar then begin
       JSONObject.Add('strength',TPasJSONItemNumber.Create(aObject.fOcclusionTexture.fStrength));
      end;
      ProcessExtensionsAndExtras(JSONObject,aObject.fOcclusionTexture);
     finally
      result.Add('occlusionTexture',JSONObject);
     end;
    end;
    if not aObject.PBRMetallicRoughness.Empty then begin
     JSONObject:=TPasJSONItemObject.Create;
     try
      if not CompareMem(@aObject.PBRMetallicRoughness.fBaseColorFactor,@TDefaults.IdentityVector4,SizeOf(TVector4)) then begin
       JSONArray:=TPasJSONItemArray.Create;
       try
        JSONArray.Add(TPasJSONItemNumber.Create(aObject.PBRMetallicRoughness.fBaseColorFactor[0]));
        JSONArray.Add(TPasJSONItemNumber.Create(aObject.PBRMetallicRoughness.fBaseColorFactor[1]));
        JSONArray.Add(TPasJSONItemNumber.Create(aObject.PBRMetallicRoughness.fBaseColorFactor[2]));
        JSONArray.Add(TPasJSONItemNumber.Create(aObject.PBRMetallicRoughness.fBaseColorFactor[3]));
       finally
        JSONObject.Add('baseColorFactor',JSONArray);
       end;
      end;
      if not aObject.fPBRMetallicRoughness.fBaseColorTexture.Empty then begin
       JSONSubObject:=TPasJSONItemObject.Create;
       try
        if aObject.fPBRMetallicRoughness.fBaseColorTexture.fIndex>=0 then begin
         JSONSubObject.Add('index',TPasJSONItemNumber.Create(aObject.fPBRMetallicRoughness.fBaseColorTexture.fIndex));
        end;
        if aObject.fPBRMetallicRoughness.fBaseColorTexture.fTexCoord>0 then begin
         JSONSubObject.Add('texCoord',TPasJSONItemNumber.Create(aObject.fPBRMetallicRoughness.fBaseColorTexture.fTexCoord));
        end;
        ProcessExtensionsAndExtras(JSONSubObject,aObject.fPBRMetallicRoughness.fBaseColorTexture);
       finally
        JSONObject.Add('baseColorTexture',JSONSubObject);
       end;
      end;
      if aObject.fPBRMetallicRoughness.fMetallicFactor<>TDefaults.IdentityScalar then begin
       JSONObject.Add('metallicFactor',TPasJSONItemNumber.Create(aObject.fPBRMetallicRoughness.fMetallicFactor));
      end;
      if not aObject.fPBRMetallicRoughness.fMetallicRoughnessTexture.Empty then begin
       JSONSubObject:=TPasJSONItemObject.Create;
       try
        if aObject.fPBRMetallicRoughness.fMetallicRoughnessTexture.fIndex>=0 then begin
         JSONSubObject.Add('index',TPasJSONItemNumber.Create(aObject.fPBRMetallicRoughness.fMetallicRoughnessTexture.fIndex));
        end;
        if aObject.fPBRMetallicRoughness.fMetallicRoughnessTexture.fTexCoord>0 then begin
         JSONSubObject.Add('texCoord',TPasJSONItemNumber.Create(aObject.fPBRMetallicRoughness.fMetallicRoughnessTexture.fTexCoord));
        end;
        ProcessExtensionsAndExtras(JSONSubObject,aObject.fPBRMetallicRoughness.fMetallicRoughnessTexture);
       finally
        JSONObject.Add('metallicRoughnessTexture',JSONSubObject);
       end;
      end;
      if aObject.fPBRMetallicRoughness.fRoughnessFactor<>TDefaults.IdentityScalar then begin
       JSONObject.Add('roughnessFactor',TPasJSONItemNumber.Create(aObject.fPBRMetallicRoughness.fRoughnessFactor));
      end;
      ProcessExtensionsAndExtras(JSONObject,aObject.fPBRMetallicRoughness);
     finally
      result.Add('pbrMetallicRoughness',JSONObject);
     end;
    end;
    ProcessExtensionsAndExtras(result,aObject);
   except
    FreeAndNil(result);
    raise;
   end;
  end;
 var Material:TMaterial;
 begin
  result:=TPasJSONItemArray.Create;
  try
   for Material in fMaterials do begin
    result.Add(ProcessMaterial(Material));
   end;
  except
   FreeAndNil(result);
   raise;
  end;
 end;
 function ProcessMeshes:TPasJSONItemArray;
  function ProcessMesh(const aObject:TMesh):TPasJSONItemObject;
  var Index:TPasJSONSizeInt;
      JSONArray:TPasJSONItemArray;
      JSONObject,JSONSubObject:TPasJSONItemObject;
      Primitive:TMesh.TPrimitive;
      Attributes:TAttributes;
      Used:boolean;
  begin
   result:=TPasJSONItemObject.Create;
   try
    if aObject.fPrimitives.Count>0 then begin
     JSONArray:=TPasJSONItemArray.Create;
     try
      for Primitive in aObject.fPrimitives do begin
       JSONObject:=TPasJSONItemObject.Create;
       try
        begin
         Used:=false;
         for Index:=0 to Primitive.fAttributes.fSize-1 do begin
          if Primitive.fAttributes.fEntityToCellIndex[Index]>=0 then begin
           Used:=true;
           break;
          end;
         end;
         if Used then begin
          JSONSubObject:=TPasJSONItemObject.Create;
          try
           for Index:=0 to Primitive.fAttributes.fSize-1 do begin
            if Primitive.fAttributes.fEntityToCellIndex[Index]>=0 then begin
             JSONSubObject.Add(Primitive.fAttributes.fEntities[Index].Key,TPasJSONItemNumber.Create(Primitive.fAttributes.fEntities[Index].Value));
            end;
           end;
          finally
           JSONObject.Add('attributes',JSONSubObject);
          end;
         end;
        end;
        if Primitive.fIndices>=0 then begin
         JSONObject.Add('indices',TPasJSONItemNumber.Create(Primitive.fIndices));
        end;
        if Primitive.fMaterial>=0 then begin
         JSONObject.Add('material',TPasJSONItemNumber.Create(Primitive.fMaterial));
        end;
        if Primitive.fMode<>TMesh.TPrimitive.TMode.Triangles then begin
         JSONObject.Add('mode',TPasJSONItemNumber.Create(TPasGLTFInt64(Primitive.fMode)));
        end;
        if Primitive.fTargets.Count>0 then begin
         JSONArray:=TPasJSONItemArray.Create;
         try
          for Attributes in Primitive.fTargets do begin
           JSONSubObject:=TPasJSONItemObject.Create;
           try
            for Index:=0 to Attributes.fSize-1 do begin
             if Attributes.fEntityToCellIndex[Index]>=0 then begin
              JSONSubObject.Add(Attributes.fEntities[Index].Key,TPasJSONItemNumber.Create(Attributes.fEntities[Index].Value));
             end;
            end;
           finally
            JSONArray.Add(JSONSubObject);
           end;
          end;
         finally
          JSONObject.Add('targets',JSONArray);
         end;
        end;
        ProcessExtensionsAndExtras(JSONObject,Primitive);
       finally
        JSONArray.Add(JSONObject);
       end;
      end;
     finally
      result.Add('primitives',JSONArray);
     end;
    end;
    if aObject.fWeights.Count>0 then begin
     JSONArray:=TPasJSONItemArray.Create;
     try
      for Index:=0 to aObject.fWeights.Count-1 do begin
       JSONArray.Add(TPasJSONItemNumber.Create(aObject.fWeights.Items[Index]));
      end;
     finally
      result.Add('weights',JSONArray);
     end;
    end;
    ProcessExtensionsAndExtras(result,aObject);
   except
    FreeAndNil(result);
    raise;
   end;
  end;
 var Mesh:TMesh;
 begin
  result:=TPasJSONItemArray.Create;
  try
   for Mesh in fMeshes do begin
    result.Add(ProcessMesh(Mesh));
   end;
  except
   FreeAndNil(result);
   raise;
  end;
 end;
 function ProcessNodes:TPasJSONItemArray;
  function ProcessNode(const aObject:TNode):TPasJSONItemObject;
  var Index:TPasJSONSizeInt;
      JSONArray:TPasJSONItemArray;
  begin
   result:=TPasJSONItemObject.Create;
   try
    if aObject.fCamera>=0 then begin
     result.Add('camera',TPasJSONItemNumber.Create(aObject.fCamera));
    end;
    if aObject.fChildren.Count>0 then begin
     JSONArray:=TPasJSONItemArray.Create;
     try
      for Index:=0 to aObject.fChildren.Count-1 do begin
       JSONArray.Add(TPasJSONItemNumber.Create(aObject.fChildren.Items[Index]));
      end;
     finally
      result.Add('children',JSONArray);
     end;
    end;
    if not CompareMem(@aObject.fMatrix,@TDefaults.IdentityMatrix4x4,SizeOf(TMatrix4x4)) then begin
     JSONArray:=TPasJSONItemArray.Create;
     try
      for Index:=0 to 15 do begin
       JSONArray.Add(TPasJSONItemNumber.Create(aObject.fMatrix[Index]));
      end;
     finally
      result.Add('matrix',JSONArray);
     end;
    end;
    if length(aObject.fName)>0 then begin
     result.Add('name',TPasJSONItemString.Create(aObject.fName));
    end;
    if aObject.fMesh>=0 then begin
     result.Add('mesh',TPasJSONItemNumber.Create(aObject.fMesh));
    end;
    if not CompareMem(@aObject.fRotation,@TDefaults.IdentityQuaternion,SizeOf(TVector4)) then begin
     JSONArray:=TPasJSONItemArray.Create;
     try
      for Index:=0 to 3 do begin
       JSONArray.Add(TPasJSONItemNumber.Create(aObject.fRotation[Index]));
      end;
     finally
      result.Add('rotation',JSONArray);
     end;
    end;
    if not CompareMem(@aObject.fScale,@TDefaults.IdentityVector3,SizeOf(TVector3)) then begin
     JSONArray:=TPasJSONItemArray.Create;
     try
      for Index:=0 to 2 do begin
       JSONArray.Add(TPasJSONItemNumber.Create(aObject.fScale[Index]));
      end;
     finally
      result.Add('scale',JSONArray);
     end;
    end;
    if aObject.fSkin>=0 then begin
     result.Add('skin',TPasJSONItemNumber.Create(aObject.fSkin));
    end;
    if not CompareMem(@aObject.fTranslation,@TDefaults.NullVector3,SizeOf(TVector3)) then begin
     JSONArray:=TPasJSONItemArray.Create;
     try
      for Index:=0 to 2 do begin
       JSONArray.Add(TPasJSONItemNumber.Create(aObject.fTranslation[Index]));
      end;
     finally
      result.Add('translation',JSONArray);
     end;
    end;
    if aObject.fWeights.Count>0 then begin
     JSONArray:=TPasJSONItemArray.Create;
     try
      for Index:=0 to aObject.fWeights.Count-1 do begin
       JSONArray.Add(TPasJSONItemNumber.Create(aObject.fWeights.Items[Index]));
      end;
     finally
      result.Add('weights',JSONArray);
     end;
    end;
    ProcessExtensionsAndExtras(result,aObject);
   except
    FreeAndNil(result);
    raise;
   end;
  end;
 var Node:TNode;
 begin
  result:=TPasJSONItemArray.Create;
  try
   for Node in fNodes do begin
    result.Add(ProcessNode(Node));
   end;
  except
   FreeAndNil(result);
   raise;
  end;
 end;
 function ProcessSamplers:TPasJSONItemArray;
  function ProcessSampler(const aObject:TSampler):TPasJSONItemObject;
  begin
   result:=TPasJSONItemObject.Create;
   try
    if not aObject.Empty then begin
     if aObject.fMinFilter<>TSampler.TMinFilter.None then begin
      result.Add('minFilter',TPasJSONItemNumber.Create(TPasGLTFInt64(aObject.fMinFilter)));
     end;
     if aObject.fMagFilter<>TSampler.TMagFilter.None then begin
      result.Add('magFilter',TPasJSONItemNumber.Create(TPasGLTFInt64(aObject.fMagFilter)));
     end;
     if length(aObject.fName)>0 then begin
      result.Add('name',TPasJSONItemString.Create(aObject.fName));
     end;
     if aObject.fWrapS<>TSampler.TWrappingMode.Repeat_ then begin
      result.Add('wrapS',TPasJSONItemNumber.Create(TPasGLTFInt64(aObject.fWrapS)));
     end;
     if aObject.fWrapT<>TSampler.TWrappingMode.Repeat_ then begin
      result.Add('wrapS',TPasJSONItemNumber.Create(TPasGLTFInt64(aObject.fWrapT)));
     end;
     ProcessExtensionsAndExtras(result,aObject);
    end;
   except
    FreeAndNil(result);
    raise;
   end;
  end;
 var Sampler:TSampler;
 begin
  result:=TPasJSONItemArray.Create;
  try
   for Sampler in fSamplers do begin
    result.Add(ProcessSampler(Sampler));
   end;
  except
   FreeAndNil(result);
   raise;
  end;
 end;
 function ProcessScenes:TPasJSONItemArray;
  function ProcessScene(const aObject:TScene):TPasJSONItemObject;
  var Index:TPasJSONSizeInt;
      JSONArray:TPasJSONItemArray;
  begin
   result:=TPasJSONItemObject.Create;
   try
    if length(aObject.fName)>0 then begin
     result.Add('name',TPasJSONItemString.Create(aObject.fName));
    end;
    if aObject.fNodes.Count>0 then begin
     JSONArray:=TPasJSONItemArray.Create;
     try
      for Index:=0 to aObject.fNodes.Count-1 do begin
       JSONArray.Add(TPasJSONItemNumber.Create(aObject.fNodes.Items[Index]));
      end;
     finally
      result.Add('nodes',JSONArray);
     end;
    end;
    ProcessExtensionsAndExtras(result,aObject);
   except
    FreeAndNil(result);
    raise;
   end;
  end;
 var Scene:TScene;
 begin
  result:=TPasJSONItemArray.Create;
  try
   for Scene in fScenes do begin
    result.Add(ProcessScene(Scene));
   end;
  except
   FreeAndNil(result);
   raise;
  end;
 end;
 function ProcessSkins:TPasJSONItemArray;
  function ProcessSkin(const aObject:TSkin):TPasJSONItemObject;
  var Index:TPasJSONSizeInt;
      JSONArray:TPasJSONItemArray;
  begin
   result:=TPasJSONItemObject.Create;
   try
    if aObject.fInverseBindMatrices>=0 then begin
     result.Add('inverseBindMatrices',TPasJSONItemNumber.Create(aObject.fInverseBindMatrices));
    end;
    if aObject.fJoints.Count>0 then begin
     JSONArray:=TPasJSONItemArray.Create;
     try
      for Index:=0 to aObject.fJoints.Count-1 do begin
       JSONArray.Add(TPasJSONItemNumber.Create(aObject.fJoints.Items[Index]));
      end;
     finally
      result.Add('joints',JSONArray);
     end;
    end;
    if length(aObject.fName)>0 then begin
     result.Add('name',TPasJSONItemString.Create(aObject.fName));
    end;
    if aObject.fSkeleton>=0 then begin
     result.Add('skeleton',TPasJSONItemNumber.Create(aObject.fSkeleton));
    end;
    ProcessExtensionsAndExtras(result,aObject);
   except
    FreeAndNil(result);
    raise;
   end;
  end;
 var Skin:TSkin;
 begin
  result:=TPasJSONItemArray.Create;
  try
   for Skin in fSkins do begin
    result.Add(ProcessSkin(Skin));
   end;
  except
   FreeAndNil(result);
   raise;
  end;
 end;
 function ProcessTextures:TPasJSONItemArray;
  function ProcessTexture(const aObject:TTexture):TPasJSONItemObject;
  begin
   result:=TPasJSONItemObject.Create;
   try
    if length(aObject.fName)>0 then begin
     result.Add('name',TPasJSONItemString.Create(aObject.fName));
    end;
    if aObject.fSampler>=0 then begin
     result.Add('sampler',TPasJSONItemNumber.Create(aObject.fSampler));
    end;
    if aObject.fSource>=0 then begin
     result.Add('source',TPasJSONItemNumber.Create(aObject.fSource));
    end;
    ProcessExtensionsAndExtras(result,aObject);
   except
    FreeAndNil(result);
    raise;
   end;
  end;
 var Texture:TTexture;
 begin
  result:=TPasJSONItemArray.Create;
  try
   for Texture in fTextures do begin
    result.Add(ProcessTexture(Texture));
   end;
  except
   FreeAndNil(result);
   raise;
  end;
 end;
var JSONRootItem:TPasJSONItemObject;
    JSONArray:TPasJSONItemArray;
    Extension:String;
begin
 JSONRootItem:=TPasJSONItemObject.Create;
 try
  if fAccessors.Count>0 then begin
   JSONRootItem.Add('accessors',ProcessAccessors);
  end;
  if fAnimations.Count>0 then begin
   JSONRootItem.Add('animations',ProcessAnimations);
  end;
  JSONRootItem.Add('asset',ProcessAsset);
  if fBuffers.Count>0 then begin
   JSONRootItem.Add('buffers',ProcessBuffers);
  end;
  if fBufferViews.Count>0 then begin
   JSONRootItem.Add('bufferViews',ProcessBufferViews);
  end;
  if fCameras.Count>0 then begin
   JSONRootItem.Add('cameras',ProcessCameras);
  end;
  if fImages.Count>0 then begin
   JSONRootItem.Add('images',ProcessImages);
  end;
  if fMaterials.Count>0 then begin
   JSONRootItem.Add('materials',ProcessMaterials);
  end;
  if fMeshes.Count>0 then begin
   JSONRootItem.Add('meshes',ProcessMeshes);
  end;
  if fNodes.Count>0 then begin
   JSONRootItem.Add('nodes',ProcessNodes);
  end;
  if fSamplers.Count>0 then begin
   JSONRootItem.Add('samplers',ProcessSamplers);
  end;
  if fScene>=0 then begin
   JSONRootItem.Add('scene',TPasJSONItemNumber.Create(fScene));
  end;
  if fScenes.Count>0 then begin
   JSONRootItem.Add('scenes',ProcessScenes);
  end;
  if fSkins.Count>0 then begin
   JSONRootItem.Add('skins',ProcessSkins);
  end;
  if fTextures.Count>0 then begin
   JSONRootItem.Add('textures',ProcessTextures);
  end;
  if fExtensionsUsed.Count>0 then begin
   JSONArray:=TPasJSONItemArray.Create;
   try
    for Extension in fExtensionsUsed do begin
     JSONArray.Add(TPasJSONItemString.Create(TPasJSONUTF8String(Extension)));
    end;
   finally
    JSONRootItem.Add('extensionsUsed',JSONArray);
   end;
  end;
  if fExtensionsRequired.Count>0 then begin
   JSONArray:=TPasJSONItemArray.Create;
   try
    for Extension in fExtensionsRequired do begin
     JSONArray.Add(TPasJSONItemString.Create(TPasJSONUTF8String(Extension)));
    end;
   finally
    JSONRootItem.Add('extensionsRequired',JSONArray);
   end;
  end;
  ProcessExtensionsAndExtras(JSONRootItem,self);
  result:=TPasJSON.Stringify(JSONRootItem,aFormatted,[]);
 finally
  FreeAndNil(JSONRootItem);
 end;
end;

procedure TPasGLTF.TDocument.SaveToBinary(const aStream:TStream);
var JSONRawByteString:TPasJSONRawByteString;
    GLBHeader:TGLBHeader;
    ChunkHeader:TChunkHeader;
begin
 JSONRawByteString:=SaveToJSON(false);
 while (length(JSONRawByteString)=0) or ((length(JSONRawByteString) and 3)<>0) do begin
  JSONRawByteString:=JSONRawByteString+#32;
 end;
 GLBHeader.Magic:=GLBHeaderMagicNativeEndianness;
 GLBHeader.Version:=$00000002;
 GLBHeader.Length:=SizeOf(TGLBHeader)+Length(JSONRawByteString);
 if (fBuffers.Count>0) and (fBuffers[0].fData.Size>0) then begin
  inc(GLBHeader.Length,SizeOf(TChunkHeader)+fBuffers[0].fData.Size);
 end;
 GLBHeader.JSONChunkHeader.ChunkLength:=Length(JSONRawByteString);
 GLBHeader.JSONChunkHeader.ChunkType:=GLBChunkJSONNativeEndianness;
 aStream.WriteBuffer(GLBHeader,SizeOf(TGLBHeader));
 aStream.WriteBuffer(JSONRawByteString[1],Length(JSONRawByteString));
 if (fBuffers.Count>0) and (fBuffers[0].fData.Size>0) then begin
  ChunkHeader.ChunkLength:=fBuffers[0].fData.Size;
  ChunkHeader.ChunkType:=GLBChunkBinaryNativeEndianness;
  aStream.WriteBuffer(ChunkHeader,SizeOf(TChunkHeader));
  aStream.WriteBuffer(fBuffers[0].fData.Memory^,fBuffers[0].fData.Size);
 end;
end;

procedure TPasGLTF.TDocument.SaveToStream(const aStream:TStream;const aBinary:boolean=false;const aFormatted:boolean=false);
var JSONRawByteString:TPasJSONRawByteString;
begin
 if aBinary then begin
  SaveToBinary(aStream);
 end else begin
  JSONRawByteString:=SaveToJSON(aFormatted);
  if length(JSONRawByteString)>0 then begin
   aStream.WriteBuffer(JSONRawByteString[1],length(JSONRawByteString));
  end;
 end;
end;

end.
