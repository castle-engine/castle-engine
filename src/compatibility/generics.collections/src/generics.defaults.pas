{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2014 by Maciej Izak (hnb)
    member of the Free Sparta development team (http://freesparta.com)

    Copyright(c) 2004-2014 DaThoX

    It contains the Free Pascal generics library

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Generics.Defaults;

{$MODE DELPHI}{$H+}
{$POINTERMATH ON}
{$MACRO ON}
{$COPERATORS ON}
{$HINTS OFF}
{$WARNINGS OFF}
{$NOTES OFF}
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

interface

uses
  Classes, SysUtils, Generics.Hashes, TypInfo, Variants, Math, Generics.Strings, Generics.Helpers;

type
  IComparer<T> = interface
    function Compare(constref Left, Right: T): Integer; overload;
  end;

  TOnComparison<T> = function(constref Left, Right: T): Integer of object;
  TComparisonFunc<T> = function(constref Left, Right: T): Integer;

  TComparer<T> = class(TInterfacedObject, IComparer<T>)
  public
    class function Default: IComparer<T>; static;
    function Compare(constref ALeft, ARight: T): Integer; virtual; abstract; overload;

    class function Construct(const AComparison: TOnComparison<T>): IComparer<T>; overload;
    class function Construct(const AComparison: TComparisonFunc<T>): IComparer<T>; overload;
  end;

  TDelegatedComparerEvents<T> = class(TComparer<T>)
  private
    FComparison: TOnComparison<T>;
  public
    function Compare(constref ALeft, ARight: T): Integer; override;
    constructor Create(AComparison: TOnComparison<T>);
  end;

  TDelegatedComparerFunc<T> = class(TComparer<T>)
  private
    FComparison: TComparisonFunc<T>;
  public
    function Compare(constref ALeft, ARight: T): Integer; override;
    constructor Create(AComparison: TComparisonFunc<T>);
  end;

  IEqualityComparer<T> = interface
    function Equals(constref ALeft, ARight: T): Boolean;
    function GetHashCode(constref AValue: T): UInt32;
  end;

  IExtendedEqualityComparer<T> = interface(IEqualityComparer<T>)
    procedure GetHashList(constref AValue: T; AHashList: PUInt32); // for double hashing and more
  end;

  ShortString1 = string[1];
  ShortString2 = string[2];
  ShortString3 = string[3];

  { TAbstractInterface }

  TInterface = class
  public
    function QueryInterface(constref {%H-}IID: TGUID;{%H-} out Obj): HResult; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF}; virtual;
    function _AddRef: LongInt; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF}; virtual; abstract;
    function _Release: LongInt; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};  virtual; abstract;
  end;

  { TRawInterface }

  TRawInterface = class(TInterface)
  public
    function _AddRef: LongInt; override;
    function _Release: LongInt; override;
  end;

  { TComTypeSizeInterface }

  // INTERNAL USE ONLY!
  TComTypeSizeInterface = class(TInterface)
  public
    // warning ! self as PSpoofInterfacedTypeSizeObject
    function _AddRef: LongInt; override;
    // warning ! self as PSpoofInterfacedTypeSizeObject
    function _Release: LongInt; override;
  end;

  { TSingletonImplementation }

  TSingletonImplementation = class(TRawInterface, IInterface)
  public
    function QueryInterface(constref IID: TGUID; out Obj): HResult; override;
  end;

  TCompare = class
  protected
    // warning ! self as PSpoofInterfacedTypeSizeObject
    class function _Binary(constref ALeft, ARight): Integer;
    // warning ! self as PSpoofInterfacedTypeSizeObject
    class function _DynArray(constref ALeft, ARight: Pointer): Integer;
  public
    class function Integer(constref ALeft, ARight: Integer): Integer;
    class function Int8(constref ALeft, ARight: Int8): Integer;
    class function Int16(constref ALeft, ARight: Int16): Integer;
    class function Int32(constref ALeft, ARight: Int32): Integer;
    class function Int64(constref ALeft, ARight: Int64): Integer;
    class function UInt8(constref ALeft, ARight: UInt8): Integer;
    class function UInt16(constref ALeft, ARight: UInt16): Integer;
    class function UInt32(constref ALeft, ARight: UInt32): Integer;
    class function UInt64(constref ALeft, ARight: UInt64): Integer;
    class function Single(constref ALeft, ARight: Single): Integer;
    class function Double(constref ALeft, ARight: Double): Integer;
    class function Extended(constref ALeft, ARight: Extended): Integer;
    class function Currency(constref ALeft, ARight: Currency): Integer;
    class function Comp(constref ALeft, ARight: Comp): Integer;
    class function Binary(constref ALeft, ARight; const ASize: SizeInt): Integer;
    class function DynArray(constref ALeft, ARight: Pointer; const AElementSize: SizeInt): Integer;
    class function ShortString1(constref ALeft, ARight: ShortString1): Integer;
    class function ShortString2(constref ALeft, ARight: ShortString2): Integer;
    class function ShortString3(constref ALeft, ARight: ShortString3): Integer;
    class function &String(constref ALeft, ARight: string): Integer;
    class function ShortString(constref ALeft, ARight: ShortString): Integer;
    class function AnsiString(constref ALeft, ARight: AnsiString): Integer;
    class function WideString(constref ALeft, ARight: WideString): Integer;
    class function UnicodeString(constref ALeft, ARight: UnicodeString): Integer;
    class function Method(constref ALeft, ARight: TMethod): Integer;
    class function Variant(constref ALeft, ARight: PVariant): Integer;
    class function Pointer(constref ALeft, ARight: PtrUInt): Integer;
  end;

  { TEquals }

  TEquals = class
  protected
    // warning ! self as PSpoofInterfacedTypeSizeObject
    class function _Binary(constref ALeft, ARight): Boolean;
    // warning ! self as PSpoofInterfacedTypeSizeObject
    class function _DynArray(constref ALeft, ARight: Pointer): Boolean;
  public
    class function Integer(constref ALeft, ARight: Integer): Boolean;
    class function Int8(constref ALeft, ARight: Int8): Boolean;
    class function Int16(constref ALeft, ARight: Int16): Boolean;
    class function Int32(constref ALeft, ARight: Int32): Boolean;
    class function Int64(constref ALeft, ARight: Int64): Boolean;
    class function UInt8(constref ALeft, ARight: UInt8): Boolean;
    class function UInt16(constref ALeft, ARight: UInt16): Boolean;
    class function UInt32(constref ALeft, ARight: UInt32): Boolean;
    class function UInt64(constref ALeft, ARight: UInt64): Boolean;
    class function Single(constref ALeft, ARight: Single): Boolean;
    class function Double(constref ALeft, ARight: Double): Boolean;
    class function Extended(constref ALeft, ARight: Extended): Boolean;
    class function Currency(constref ALeft, ARight: Currency): Boolean;
    class function Comp(constref ALeft, ARight: Comp): Boolean;
    class function Binary(constref ALeft, ARight; const ASize: SizeInt): Boolean;
    class function DynArray(constref ALeft, ARight: Pointer; const AElementSize: SizeInt): Boolean;
    class function &Class(constref ALeft, ARight: TObject): Boolean;
    class function ShortString1(constref ALeft, ARight: ShortString1): Boolean;
    class function ShortString2(constref ALeft, ARight: ShortString2): Boolean;
    class function ShortString3(constref ALeft, ARight: ShortString3): Boolean;
    class function &String(constref ALeft, ARight: String): Boolean;
    class function ShortString(constref ALeft, ARight: ShortString): Boolean;
    class function AnsiString(constref ALeft, ARight: AnsiString): Boolean;
    class function WideString(constref ALeft, ARight: WideString): Boolean;
    class function UnicodeString(constref ALeft, ARight: UnicodeString): Boolean;
    class function Method(constref ALeft, ARight: TMethod): Boolean;
    class function Variant(constref ALeft, ARight: PVariant): Boolean;
    class function Pointer(constref ALeft, ARight: PtrUInt): Boolean;
  end;

  THashServiceClass = class of THashService;
  TExtendedHashServiceClass = class of TExtendedHashService;
  THashFactoryClass = class of THashFactory;

  TExtendedHashFactoryClass = class of TExtendedHashFactory;

  { TComparerService }

{$DEFINE STD_RAW_INTERFACE_METHODS :=
    QueryInterface: @TRawInterface.QueryInterface;
    _AddRef       : @TRawInterface._AddRef;
    _Release      : @TRawInterface._Release
}

{$DEFINE STD_COM_TYPESIZE_INTERFACE_METHODS :=
    QueryInterface: @TComTypeSizeInterface.QueryInterface;
    _AddRef       : @TComTypeSizeInterface._AddRef;
    _Release      : @TComTypeSizeInterface._Release
}

  TGetHashListOptions = set of (ghloHashListAsInitData);

  THashFactory = class
  private type
    PPEqualityComparerVMT = ^PEqualityComparerVMT;
    PEqualityComparerVMT = ^TEqualityComparerVMT;
    TEqualityComparerVMT = packed record
      QueryInterface: CodePointer;
      _AddRef: CodePointer;
      _Release: CodePointer;
      Equals: CodePointer;
      GetHashCode: CodePointer;
      __Reserved: Pointer; // initially or TExtendedEqualityComparerVMT compatibility
                           // (important when ExtendedEqualityComparer is calling Binary method)
      __ClassRef: THashFactoryClass; // hidden field in VMT. For class ref THashFactoryClass
    end;

  private
(***********************************************************************************************************************
      Hashes
(**********************************************************************************************************************)

    class function Int8         (constref AValue: Int8         ): UInt32; overload;
    class function Int16        (constref AValue: Int16        ): UInt32; overload;
    class function Int32        (constref AValue: Int32        ): UInt32; overload;
    class function Int64        (constref AValue: Int64        ): UInt32; overload;
    class function UInt8        (constref AValue: UInt8        ): UInt32; overload;
    class function UInt16       (constref AValue: UInt16       ): UInt32; overload;
    class function UInt32       (constref AValue: UInt32       ): UInt32; overload;
    class function UInt64       (constref AValue: UInt64       ): UInt32; overload;
    class function Single       (constref AValue: Single       ): UInt32; overload;
    class function Double       (constref AValue: Double       ): UInt32; overload;
    class function Extended     (constref AValue: Extended     ): UInt32; overload;
    class function Currency     (constref AValue: Currency     ): UInt32; overload;
    class function Comp         (constref AValue: Comp         ): UInt32; overload;
    // warning ! self as PSpoofInterfacedTypeSizeObject
    class function Binary       (constref AValue               ): UInt32; overload;
    // warning ! self as PSpoofInterfacedTypeSizeObject
    class function DynArray     (constref AValue: Pointer      ): UInt32; overload;
    class function &Class       (constref AValue: TObject      ): UInt32; overload;
    class function ShortString1 (constref AValue: ShortString1 ): UInt32; overload;
    class function ShortString2 (constref AValue: ShortString2 ): UInt32; overload;
    class function ShortString3 (constref AValue: ShortString3 ): UInt32; overload;
    class function ShortString  (constref AValue: ShortString   ): UInt32; overload;
    class function AnsiString   (constref AValue: AnsiString   ): UInt32; overload;
    class function WideString   (constref AValue: WideString   ): UInt32; overload;
    class function UnicodeString(constref AValue: UnicodeString): UInt32; overload;
    class function Method       (constref AValue: TMethod      ): UInt32; overload;
    class function Variant      (constref AValue: PVariant     ): UInt32; overload;
    class function Pointer      (constref AValue: Pointer      ): UInt32; overload;
  public
    const MAX_HASHLIST_COUNT = 1;
    const HASH_FUNCTIONS_COUNT = 1;
    const HASHLIST_COUNT_PER_FUNCTION: array[1..HASH_FUNCTIONS_COUNT] of Integer = (1);
    const HASH_FUNCTIONS_MASK_SIZE = 1;

    class function GetHashService: THashServiceClass; virtual; abstract;
    class function GetHashCode(AKey: Pointer; ASize: SizeInt; AInitVal: UInt32 = 0): UInt32; virtual; abstract; reintroduce;
  end;

  TExtendedHashFactory = class(THashFactory)
  private type
    PPExtendedEqualityComparerVMT = ^PExtendedEqualityComparerVMT;
    PExtendedEqualityComparerVMT = ^TExtendedEqualityComparerVMT;
    TExtendedEqualityComparerVMT = packed record
      QueryInterface: CodePointer;
      _AddRef: CodePointer;
      _Release: CodePointer;
      Equals: CodePointer;
      GetHashCode: CodePointer;
      GetHashList: CodePointer;
      __ClassRef: TExtendedHashFactoryClass; // hidden field in VMT. For class ref THashFactoryClass
    end;
  private
(***********************************************************************************************************************
      Hashes 2
(**********************************************************************************************************************)

    class procedure Int8         (constref AValue: Int8         ; AHashList: PUInt32); overload;
    class procedure Int16        (constref AValue: Int16        ; AHashList: PUInt32); overload;
    class procedure Int32        (constref AValue: Int32        ; AHashList: PUInt32); overload;
    class procedure Int64        (constref AValue: Int64        ; AHashList: PUInt32); overload;
    class procedure UInt8        (constref AValue: UInt8        ; AHashList: PUInt32); overload;
    class procedure UInt16       (constref AValue: UInt16       ; AHashList: PUInt32); overload;
    class procedure UInt32       (constref AValue: UInt32       ; AHashList: PUInt32); overload;
    class procedure UInt64       (constref AValue: UInt64       ; AHashList: PUInt32); overload;
    class procedure Single       (constref AValue: Single       ; AHashList: PUInt32); overload;
    class procedure Double       (constref AValue: Double       ; AHashList: PUInt32); overload;
    class procedure Extended     (constref AValue: Extended     ; AHashList: PUInt32); overload;
    class procedure Currency     (constref AValue: Currency     ; AHashList: PUInt32); overload;
    class procedure Comp         (constref AValue: Comp         ; AHashList: PUInt32); overload;
    // warning ! self as PSpoofInterfacedTypeSizeObject
    class procedure Binary       (constref AValue               ; AHashList: PUInt32); overload;
    // warning ! self as PSpoofInterfacedTypeSizeObject
    class procedure DynArray     (constref AValue: Pointer      ; AHashList: PUInt32); overload;
    class procedure &Class       (constref AValue: TObject      ; AHashList: PUInt32); overload;
    class procedure ShortString1 (constref AValue: ShortString1 ; AHashList: PUInt32); overload;
    class procedure ShortString2 (constref AValue: ShortString2 ; AHashList: PUInt32); overload;
    class procedure ShortString3 (constref AValue: ShortString3 ; AHashList: PUInt32); overload;
    class procedure ShortString  (constref AValue: ShortString   ; AHashList: PUInt32); overload;
    class procedure AnsiString   (constref AValue: AnsiString   ; AHashList: PUInt32); overload;
    class procedure WideString   (constref AValue: WideString   ; AHashList: PUInt32); overload;
    class procedure UnicodeString(constref AValue: UnicodeString; AHashList: PUInt32); overload;
    class procedure Method       (constref AValue: TMethod      ; AHashList: PUInt32); overload;
    class procedure Variant      (constref AValue: PVariant     ; AHashList: PUInt32); overload;
    class procedure Pointer      (constref AValue: Pointer      ; AHashList: PUInt32); overload;
  public
    class procedure GetHashList(AKey: Pointer; ASize: SizeInt; AHashList: PUInt32; AOptions: TGetHashListOptions = []); virtual; abstract;
  end;

  TComparerService = class abstract
  private type
    TSelectMethod = function(ATypeData: PTypeData; ASize: SizeInt): Pointer of object;
  private
    class function SelectIntegerEqualityComparer(ATypeData: PTypeData; ASize: SizeInt): Pointer; virtual; abstract;
    class function SelectFloatEqualityComparer(ATypeData: PTypeData; ASize: SizeInt): Pointer; virtual; abstract;
    class function SelectShortStringEqualityComparer(ATypeData: PTypeData; ASize: SizeInt): Pointer; virtual; abstract;
    class function SelectBinaryEqualityComparer(ATypeData: PTypeData; ASize: SizeInt): Pointer; virtual; abstract;
    class function SelectDynArrayEqualityComparer(ATypeData: PTypeData; ASize: SizeInt): Pointer; virtual; abstract;
  private type
    PSpoofInterfacedTypeSizeObject = ^TSpoofInterfacedTypeSizeObject;
    TSpoofInterfacedTypeSizeObject = record
      VMT: Pointer;
      RefCount: LongInt;
      Size: SizeInt;
    end;

    PInstance = ^TInstance;
    TInstance = record
      class function Create(ASelector: Boolean; AInstance: Pointer): TComparerService.TInstance; static;
      class function CreateSelector(ASelectorInstance: CodePointer): TComparerService.TInstance; static;

      case Selector: Boolean of
        false: (Instance: Pointer);
        true:  (SelectorInstance: CodePointer);
    end;

    PComparerVMT = ^TComparerVMT;
    TComparerVMT = packed record
      QueryInterface: CodePointer;
      _AddRef: CodePointer;
      _Release: CodePointer;
      Compare: CodePointer;
    end;

    TSelectFunc = function(ATypeData: PTypeData; ASize: SizeInt): Pointer;

  private
    class function CreateInterface(AVMT: Pointer; ASize: SizeInt): PSpoofInterfacedTypeSizeObject; static;

    class function SelectIntegerComparer(ATypeData: PTypeData; ASize: SizeInt): Pointer; static;
    class function SelectInt64Comparer(ATypeData: PTypeData; ASize: SizeInt): Pointer; static;
    class function SelectFloatComparer(ATypeData: PTypeData; ASize: SizeInt): Pointer; static;
    class function SelectShortStringComparer(ATypeData: PTypeData; ASize: SizeInt): Pointer; static;
    class function SelectBinaryComparer(ATypeData: PTypeData; ASize: SizeInt): Pointer; static;
    class function SelectDynArrayComparer(ATypeData: PTypeData; ASize: SizeInt): Pointer; static;
  private const
    // IComparer VMT
    Comparer_Int8_VMT  : TComparerVMT = (STD_RAW_INTERFACE_METHODS; Compare: @TCompare.Int8);
    Comparer_Int16_VMT : TComparerVMT = (STD_RAW_INTERFACE_METHODS; Compare: @TCompare.Int16 );
    Comparer_Int32_VMT : TComparerVMT = (STD_RAW_INTERFACE_METHODS; Compare: @TCompare.Int32 );
    Comparer_Int64_VMT : TComparerVMT = (STD_RAW_INTERFACE_METHODS; Compare: @TCompare.Int64 );
    Comparer_UInt8_VMT : TComparerVMT = (STD_RAW_INTERFACE_METHODS; Compare: @TCompare.UInt8 );
    Comparer_UInt16_VMT: TComparerVMT = (STD_RAW_INTERFACE_METHODS; Compare: @TCompare.UInt16);
    Comparer_UInt32_VMT: TComparerVMT = (STD_RAW_INTERFACE_METHODS; Compare: @TCompare.UInt32);
    Comparer_UInt64_VMT: TComparerVMT = (STD_RAW_INTERFACE_METHODS; Compare: @TCompare.UInt64);

    Comparer_Single_VMT  : TComparerVMT = (STD_RAW_INTERFACE_METHODS; Compare: @TCompare.Single  );
    Comparer_Double_VMT  : TComparerVMT = (STD_RAW_INTERFACE_METHODS; Compare: @TCompare.Double  );
    Comparer_Extended_VMT: TComparerVMT = (STD_RAW_INTERFACE_METHODS; Compare: @TCompare.Extended);

    Comparer_Currency_VMT: TComparerVMT = (STD_RAW_INTERFACE_METHODS; Compare: @TCompare.Currency);
    Comparer_Comp_VMT    : TComparerVMT = (STD_RAW_INTERFACE_METHODS; Compare: @TCompare.Comp    );

    Comparer_Binary_VMT  : TComparerVMT = (STD_COM_TYPESIZE_INTERFACE_METHODS; Compare: @TCompare._Binary  );
    Comparer_DynArray_VMT: TComparerVMT = (STD_COM_TYPESIZE_INTERFACE_METHODS; Compare: @TCompare._DynArray);

    Comparer_ShortString1_VMT : TComparerVMT = (STD_RAW_INTERFACE_METHODS; Compare: @TCompare.ShortString1 );
    Comparer_ShortString2_VMT : TComparerVMT = (STD_RAW_INTERFACE_METHODS; Compare: @TCompare.ShortString2 );
    Comparer_ShortString3_VMT : TComparerVMT = (STD_RAW_INTERFACE_METHODS; Compare: @TCompare.ShortString3 );
    Comparer_ShortString_VMT  : TComparerVMT = (STD_RAW_INTERFACE_METHODS; Compare: @TCompare.ShortString  );
    Comparer_AnsiString_VMT   : TComparerVMT = (STD_RAW_INTERFACE_METHODS; Compare: @TCompare.AnsiString   );
    Comparer_WideString_VMT   : TComparerVMT = (STD_RAW_INTERFACE_METHODS; Compare: @TCompare.WideString   );
    Comparer_UnicodeString_VMT: TComparerVMT = (STD_RAW_INTERFACE_METHODS; Compare: @TCompare.UnicodeString);

    Comparer_Method_VMT : TComparerVMT = (STD_RAW_INTERFACE_METHODS; Compare: @TCompare.Method );
    Comparer_Variant_VMT: TComparerVMT = (STD_RAW_INTERFACE_METHODS; Compare: @TCompare.Variant);
    Comparer_Pointer_VMT: TComparerVMT = (STD_RAW_INTERFACE_METHODS; Compare: @TCompare.Pointer);

    // Instances
    Comparer_Int8_Instance  : Pointer = @Comparer_Int8_VMT  ;
    Comparer_Int16_Instance : Pointer = @Comparer_Int16_VMT ;
    Comparer_Int32_Instance : Pointer = @Comparer_Int32_VMT ;
    Comparer_Int64_Instance : Pointer = @Comparer_Int64_VMT ;
    Comparer_UInt8_Instance : Pointer = @Comparer_UInt8_VMT ;
    Comparer_UInt16_Instance: Pointer = @Comparer_UInt16_VMT;
    Comparer_UInt32_Instance: Pointer = @Comparer_UInt32_VMT;
    Comparer_UInt64_Instance: Pointer = @Comparer_UInt64_VMT;

    Comparer_Single_Instance  : Pointer = @Comparer_Single_VMT  ;
    Comparer_Double_Instance  : Pointer = @Comparer_Double_VMT  ;
    Comparer_Extended_Instance: Pointer = @Comparer_Extended_VMT;

    Comparer_Currency_Instance: Pointer = @Comparer_Currency_VMT;
    Comparer_Comp_Instance    : Pointer = @Comparer_Comp_VMT    ;

    //Comparer_Binary_Instance  : Pointer = @Comparer_Binary_VMT  ;  // dynamic instance
    //Comparer_DynArray_Instance: Pointer = @Comparer_DynArray_VMT;  // dynamic instance

    Comparer_ShortString1_Instance : Pointer = @Comparer_ShortString1_VMT ;
    Comparer_ShortString2_Instance : Pointer = @Comparer_ShortString2_VMT ;
    Comparer_ShortString3_Instance : Pointer = @Comparer_ShortString3_VMT ;
    Comparer_ShortString_Instance  : Pointer = @Comparer_ShortString_VMT  ;
    Comparer_AnsiString_Instance   : Pointer = @Comparer_AnsiString_VMT   ;
    Comparer_WideString_Instance   : Pointer = @Comparer_WideString_VMT   ;
    Comparer_UnicodeString_Instance: Pointer = @Comparer_UnicodeString_VMT;

    Comparer_Method_Instance : Pointer = @Comparer_Method_VMT ;
    Comparer_Variant_Instance: Pointer = @Comparer_Variant_VMT;
    Comparer_Pointer_Instance: Pointer = @Comparer_Pointer_VMT;

    ComparerInstances: array[TTypeKind] of TInstance =
      (
        // tkUnknown
        (Selector: True;  SelectorInstance: @TComparerService.SelectBinaryComparer),
        // tkInteger
        (Selector: True;  SelectorInstance: @TComparerService.SelectIntegerComparer),
        // tkChar
        (Selector: False; Instance: @Comparer_UInt8_Instance),
        // tkEnumeration
        (Selector: True;  SelectorInstance: @TComparerService.SelectIntegerComparer),
        // tkFloat
        (Selector: True;  SelectorInstance: @TComparerService.SelectFloatComparer),
        // tkSet
        (Selector: True;  SelectorInstance: @TComparerService.SelectBinaryComparer),
        // tkMethod
        (Selector: False; Instance: @Comparer_Method_Instance),
        // tkSString
        (Selector: True;  SelectorInstance: @TComparerService.SelectShortStringComparer),
        // tkLString - only internal use / deprecated in compiler
        (Selector: False; Instance: @Comparer_AnsiString_Instance), // <- unsure
        // tkAString
        (Selector: False; Instance: @Comparer_AnsiString_Instance),
        // tkWString
        (Selector: False; Instance: @Comparer_WideString_Instance),
        // tkVariant
        (Selector: False; Instance: @Comparer_Variant_Instance),
        // tkArray
        (Selector: True;  SelectorInstance: @TComparerService.SelectBinaryComparer),
        // tkRecord
        (Selector: True;  SelectorInstance: @TComparerService.SelectBinaryComparer),
        // tkInterface
        (Selector: False; Instance: @Comparer_Pointer_Instance),
        // tkClass
        (Selector: False; Instance: @Comparer_Pointer_Instance),
        // tkObject
        (Selector: True;  SelectorInstance: @TComparerService.SelectBinaryComparer),
        // tkWChar
        (Selector: False; Instance: @Comparer_UInt16_Instance),
        // tkBool
        (Selector: True;  SelectorInstance: @TComparerService.SelectIntegerComparer),
        // tkInt64
        (Selector: False;  Instance: @Comparer_Int64_Instance),
        // tkQWord
        (Selector: False;  Instance: @Comparer_UInt64_Instance),
        // tkDynArray
        (Selector: True;  SelectorInstance: @TComparerService.SelectDynArrayComparer),
        // tkInterfaceRaw
        (Selector: False; Instance: @Comparer_Pointer_Instance),
        // tkProcVar
        (Selector: False; Instance: @Comparer_Pointer_Instance),
        // tkUString
        (Selector: False; Instance: @Comparer_UnicodeString_Instance),
        // tkUChar - WTF? ... http://bugs.freepascal.org/view.php?id=24609
        (Selector: False; Instance: @Comparer_UInt16_Instance), // <- unsure maybe Comparer_UInt32_Instance
        // tkHelper
        (Selector: False; Instance: @Comparer_Pointer_Instance),
        // tkFile
        (Selector: True;  SelectorInstance: @TComparerService.SelectBinaryComparer), // <- unsure what type?
        // tkClassRef
        (Selector: False; Instance: @Comparer_Pointer_Instance),
        // tkPointer
        (Selector: False; Instance: @Comparer_Pointer_Instance)
      );
  public
    class function LookupComparer(ATypeInfo: PTypeInfo; ASize: SizeInt): Pointer; static;
  end;

  THashService = class(TComparerService)
  public
    class function LookupEqualityComparer(ATypeInfo: PTypeInfo; ASize: SizeInt): Pointer; virtual; abstract;
  end;

  TExtendedHashService = class(THashService)
  public
    class function LookupExtendedEqualityComparer(ATypeInfo: PTypeInfo; ASize: SizeInt): Pointer; virtual; abstract;
  end;

{$DEFINE HASH_FACTORY := PPEqualityComparerVMT(Self)^.__ClassRef}
{$DEFINE EXTENDED_HASH_FACTORY := PPExtendedEqualityComparerVMT(Self)^.__ClassRef}

  { THashService }

  THashService<T: THashFactory> = class(THashService)
  private
    class function SelectIntegerEqualityComparer(ATypeData: PTypeData; ASize: SizeInt): Pointer; override;
    class function SelectFloatEqualityComparer(ATypeData: PTypeData; ASize: SizeInt): Pointer; override;
    class function SelectShortStringEqualityComparer(ATypeData: PTypeData; ASize: SizeInt): Pointer; override;
    class function SelectBinaryEqualityComparer(ATypeData: PTypeData; ASize: SizeInt): Pointer; override;
    class function SelectDynArrayEqualityComparer(ATypeData: PTypeData; ASize: SizeInt): Pointer; override;
  private const
    // IEqualityComparer VMT templates
{$WARNINGS OFF}
    EqualityComparer_Int8_VMT  : THashFactory.TEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.Int8  ; GetHashCode: @THashFactory.Int8  );
    EqualityComparer_Int16_VMT : THashFactory.TEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.Int16 ; GetHashCode: @THashFactory.Int16 );
    EqualityComparer_Int32_VMT : THashFactory.TEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.Int32 ; GetHashCode: @THashFactory.Int32 );
    EqualityComparer_Int64_VMT : THashFactory.TEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.Int64 ; GetHashCode: @THashFactory.Int64 );
    EqualityComparer_UInt8_VMT : THashFactory.TEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.UInt8 ; GetHashCode: @THashFactory.UInt8 );
    EqualityComparer_UInt16_VMT: THashFactory.TEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.UInt16; GetHashCode: @THashFactory.UInt16);
    EqualityComparer_UInt32_VMT: THashFactory.TEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.UInt32; GetHashCode: @THashFactory.UInt32);
    EqualityComparer_UInt64_VMT: THashFactory.TEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.UInt64; GetHashCode: @THashFactory.UInt64);

    EqualityComparer_Single_VMT  : THashFactory.TEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.Single  ; GetHashCode: @THashFactory.Single  );
    EqualityComparer_Double_VMT  : THashFactory.TEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.Double  ; GetHashCode: @THashFactory.Double  );
    EqualityComparer_Extended_VMT: THashFactory.TEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.Extended; GetHashCode: @THashFactory.Extended);

    EqualityComparer_Currency_VMT: THashFactory.TEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.Currency; GetHashCode: @THashFactory.Currency);
    EqualityComparer_Comp_VMT    : THashFactory.TEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.Comp    ; GetHashCode: @THashFactory.Comp    );

    EqualityComparer_Binary_VMT  : THashFactory.TEqualityComparerVMT = (STD_COM_TYPESIZE_INTERFACE_METHODS; Equals: @TEquals._Binary  ; GetHashCode: @THashFactory.Binary  );
    EqualityComparer_DynArray_VMT: THashFactory.TEqualityComparerVMT = (STD_COM_TYPESIZE_INTERFACE_METHODS; Equals: @TEquals._DynArray; GetHashCode: @THashFactory.DynArray);

    EqualityComparer_Class_VMT: THashFactory.TEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.&Class; GetHashCode: @THashFactory.&Class);

    EqualityComparer_ShortString1_VMT : THashFactory.TEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.ShortString1 ; GetHashCode: @THashFactory.ShortString1 );
    EqualityComparer_ShortString2_VMT : THashFactory.TEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.ShortString2 ; GetHashCode: @THashFactory.ShortString2 );
    EqualityComparer_ShortString3_VMT : THashFactory.TEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.ShortString3 ; GetHashCode: @THashFactory.ShortString3 );
    EqualityComparer_ShortString_VMT  : THashFactory.TEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.ShortString  ; GetHashCode: @THashFactory.ShortString  );
    EqualityComparer_AnsiString_VMT   : THashFactory.TEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.AnsiString   ; GetHashCode: @THashFactory.AnsiString   );
    EqualityComparer_WideString_VMT   : THashFactory.TEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.WideString   ; GetHashCode: @THashFactory.WideString   );
    EqualityComparer_UnicodeString_VMT: THashFactory.TEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.UnicodeString; GetHashCode: @THashFactory.UnicodeString);

    EqualityComparer_Method_VMT : THashFactory.TEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.Method ; GetHashCode: @THashFactory.Method );
    EqualityComparer_Variant_VMT: THashFactory.TEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.Variant; GetHashCode: @THashFactory.Variant);
    EqualityComparer_Pointer_VMT: THashFactory.TEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.Pointer; GetHashCode: @THashFactory.Pointer);
{$WARNINGS ON}
  private class var
    // IEqualityComparer VMT
    FEqualityComparer_Int8_VMT  : THashFactory.TEqualityComparerVMT;
    FEqualityComparer_Int16_VMT : THashFactory.TEqualityComparerVMT;
    FEqualityComparer_Int32_VMT : THashFactory.TEqualityComparerVMT;
    FEqualityComparer_Int64_VMT : THashFactory.TEqualityComparerVMT;
    FEqualityComparer_UInt8_VMT : THashFactory.TEqualityComparerVMT;
    FEqualityComparer_UInt16_VMT: THashFactory.TEqualityComparerVMT;
    FEqualityComparer_UInt32_VMT: THashFactory.TEqualityComparerVMT;
    FEqualityComparer_UInt64_VMT: THashFactory.TEqualityComparerVMT;

    FEqualityComparer_Single_VMT  : THashFactory.TEqualityComparerVMT;
    FEqualityComparer_Double_VMT  : THashFactory.TEqualityComparerVMT;
    FEqualityComparer_Extended_VMT: THashFactory.TEqualityComparerVMT;

    FEqualityComparer_Currency_VMT: THashFactory.TEqualityComparerVMT;
    FEqualityComparer_Comp_VMT    : THashFactory.TEqualityComparerVMT;

    FEqualityComparer_Binary_VMT  : THashFactory.TEqualityComparerVMT;
    FEqualityComparer_DynArray_VMT: THashFactory.TEqualityComparerVMT;

    FEqualityComparer_Class_VMT: THashFactory.TEqualityComparerVMT;

    FEqualityComparer_ShortString1_VMT : THashFactory.TEqualityComparerVMT;
    FEqualityComparer_ShortString2_VMT : THashFactory.TEqualityComparerVMT;
    FEqualityComparer_ShortString3_VMT : THashFactory.TEqualityComparerVMT;
    FEqualityComparer_ShortString_VMT  : THashFactory.TEqualityComparerVMT;
    FEqualityComparer_AnsiString_VMT   : THashFactory.TEqualityComparerVMT;
    FEqualityComparer_WideString_VMT   : THashFactory.TEqualityComparerVMT;
    FEqualityComparer_UnicodeString_VMT: THashFactory.TEqualityComparerVMT;

    FEqualityComparer_Method_VMT : THashFactory.TEqualityComparerVMT;
    FEqualityComparer_Variant_VMT: THashFactory.TEqualityComparerVMT;
    FEqualityComparer_Pointer_VMT: THashFactory.TEqualityComparerVMT;

    FEqualityComparer_Int8_Instance         : Pointer;
    FEqualityComparer_Int16_Instance        : Pointer;
    FEqualityComparer_Int32_Instance        : Pointer;
    FEqualityComparer_Int64_Instance        : Pointer;
    FEqualityComparer_UInt8_Instance        : Pointer;
    FEqualityComparer_UInt16_Instance       : Pointer;
    FEqualityComparer_UInt32_Instance       : Pointer;
    FEqualityComparer_UInt64_Instance       : Pointer;

    FEqualityComparer_Single_Instance       : Pointer;
    FEqualityComparer_Double_Instance       : Pointer;
    FEqualityComparer_Extended_Instance     : Pointer;

    FEqualityComparer_Currency_Instance     : Pointer;
    FEqualityComparer_Comp_Instance         : Pointer;

    //FEqualityComparer_Binary_Instance     : Pointer;  // dynamic instance
    //FEqualityComparer_DynArray_Instance   : Pointer;  // dynamic instance

    FEqualityComparer_ShortString1_Instance : Pointer;
    FEqualityComparer_ShortString2_Instance : Pointer;
    FEqualityComparer_ShortString3_Instance : Pointer;
    FEqualityComparer_ShortString_Instance  : Pointer;
    FEqualityComparer_AnsiString_Instance   : Pointer;
    FEqualityComparer_WideString_Instance   : Pointer;
    FEqualityComparer_UnicodeString_Instance: Pointer;

    FEqualityComparer_Method_Instance       : Pointer;
    FEqualityComparer_Variant_Instance      : Pointer;
    FEqualityComparer_Pointer_Instance      : Pointer;


    FEqualityComparerInstances: array[TTypeKind] of TInstance;
  private
    class constructor Create;
  public
    class function LookupEqualityComparer(ATypeInfo: PTypeInfo; ASize: SizeInt): Pointer; override;
  end;

  { TExtendedHashService }

  TExtendedHashService<T: TExtendedHashFactory> = class(TExtendedHashService)
  private
    class function SelectIntegerEqualityComparer(ATypeData: PTypeData; ASize: SizeInt): Pointer; override;
    class function SelectFloatEqualityComparer(ATypeData: PTypeData; ASize: SizeInt): Pointer; override;
    class function SelectShortStringEqualityComparer(ATypeData: PTypeData; ASize: SizeInt): Pointer; override;
    class function SelectBinaryEqualityComparer(ATypeData: PTypeData; ASize: SizeInt): Pointer; override;
    class function SelectDynArrayEqualityComparer(ATypeData: PTypeData; ASize: SizeInt): Pointer; override;
  private const
    // IExtendedEqualityComparer VMT templates
{$WARNINGS OFF}
    ExtendedEqualityComparer_Int8_VMT  : TExtendedHashFactory.TExtendedEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.Int8  ; GetHashCode: @THashFactory.Int8  ; GetHashList: @TExtendedHashFactory.Int8  );
    ExtendedEqualityComparer_Int16_VMT : TExtendedHashFactory.TExtendedEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.Int16 ; GetHashCode: @THashFactory.Int16 ; GetHashList: @TExtendedHashFactory.Int16 );
    ExtendedEqualityComparer_Int32_VMT : TExtendedHashFactory.TExtendedEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.Int32 ; GetHashCode: @THashFactory.Int32 ; GetHashList: @TExtendedHashFactory.Int32 );
    ExtendedEqualityComparer_Int64_VMT : TExtendedHashFactory.TExtendedEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.Int64 ; GetHashCode: @THashFactory.Int64 ; GetHashList: @TExtendedHashFactory.Int64 );
    ExtendedEqualityComparer_UInt8_VMT : TExtendedHashFactory.TExtendedEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.UInt8 ; GetHashCode: @THashFactory.UInt8 ; GetHashList: @TExtendedHashFactory.UInt8 );
    ExtendedEqualityComparer_UInt16_VMT: TExtendedHashFactory.TExtendedEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.UInt16; GetHashCode: @THashFactory.UInt16; GetHashList: @TExtendedHashFactory.UInt16);
    ExtendedEqualityComparer_UInt32_VMT: TExtendedHashFactory.TExtendedEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.UInt32; GetHashCode: @THashFactory.UInt32; GetHashList: @TExtendedHashFactory.UInt32);
    ExtendedEqualityComparer_UInt64_VMT: TExtendedHashFactory.TExtendedEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.UInt64; GetHashCode: @THashFactory.UInt64; GetHashList: @TExtendedHashFactory.UInt64);

    ExtendedEqualityComparer_Single_VMT  : TExtendedHashFactory.TExtendedEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.Single  ; GetHashCode: @THashFactory.Single  ; GetHashList: @TExtendedHashFactory.Single  );
    ExtendedEqualityComparer_Double_VMT  : TExtendedHashFactory.TExtendedEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.Double  ; GetHashCode: @THashFactory.Double  ; GetHashList: @TExtendedHashFactory.Double  );
    ExtendedEqualityComparer_Extended_VMT: TExtendedHashFactory.TExtendedEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.Extended; GetHashCode: @THashFactory.Extended; GetHashList: @TExtendedHashFactory.Extended);

    ExtendedEqualityComparer_Currency_VMT: TExtendedHashFactory.TExtendedEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.Currency; GetHashCode: @THashFactory.Currency; GetHashList: @TExtendedHashFactory.Currency);
    ExtendedEqualityComparer_Comp_VMT    : TExtendedHashFactory.TExtendedEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.Comp    ; GetHashCode: @THashFactory.Comp    ; GetHashList: @TExtendedHashFactory.Comp    );

    ExtendedEqualityComparer_Binary_VMT  : TExtendedHashFactory.TExtendedEqualityComparerVMT = (STD_COM_TYPESIZE_INTERFACE_METHODS; Equals: @TEquals._Binary  ; GetHashCode: @THashFactory.Binary  ; GetHashList: @TExtendedHashFactory.Binary   );
    ExtendedEqualityComparer_DynArray_VMT: TExtendedHashFactory.TExtendedEqualityComparerVMT = (STD_COM_TYPESIZE_INTERFACE_METHODS; Equals: @TEquals._DynArray; GetHashCode: @THashFactory.DynArray; GetHashList: @TExtendedHashFactory.DynArray);

    ExtendedEqualityComparer_Class_VMT: TExtendedHashFactory.TExtendedEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.&Class; GetHashCode: @THashFactory.&Class; GetHashList: @TExtendedHashFactory.&Class);

    ExtendedEqualityComparer_ShortString1_VMT : TExtendedHashFactory.TExtendedEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.ShortString1 ; GetHashCode: @THashFactory.ShortString1 ; GetHashList: @TExtendedHashFactory.ShortString1 );
    ExtendedEqualityComparer_ShortString2_VMT : TExtendedHashFactory.TExtendedEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.ShortString2 ; GetHashCode: @THashFactory.ShortString2 ; GetHashList: @TExtendedHashFactory.ShortString2 );
    ExtendedEqualityComparer_ShortString3_VMT : TExtendedHashFactory.TExtendedEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.ShortString3 ; GetHashCode: @THashFactory.ShortString3 ; GetHashList: @TExtendedHashFactory.ShortString3 );
    ExtendedEqualityComparer_ShortString_VMT  : TExtendedHashFactory.TExtendedEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.ShortString  ; GetHashCode: @THashFactory.ShortString  ; GetHashList: @TExtendedHashFactory.ShortString  );
    ExtendedEqualityComparer_AnsiString_VMT   : TExtendedHashFactory.TExtendedEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.AnsiString   ; GetHashCode: @THashFactory.AnsiString   ; GetHashList: @TExtendedHashFactory.AnsiString   );
    ExtendedEqualityComparer_WideString_VMT   : TExtendedHashFactory.TExtendedEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.WideString   ; GetHashCode: @THashFactory.WideString   ; GetHashList: @TExtendedHashFactory.WideString   );
    ExtendedEqualityComparer_UnicodeString_VMT: TExtendedHashFactory.TExtendedEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.UnicodeString; GetHashCode: @THashFactory.UnicodeString; GetHashList: @TExtendedHashFactory.UnicodeString);

    ExtendedEqualityComparer_Method_VMT : TExtendedHashFactory.TExtendedEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.Method ; GetHashCode: @THashFactory.Method ; GetHashList: @TExtendedHashFactory.Method );
    ExtendedEqualityComparer_Variant_VMT: TExtendedHashFactory.TExtendedEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.Variant; GetHashCode: @THashFactory.Variant; GetHashList: @TExtendedHashFactory.Variant);
    ExtendedEqualityComparer_Pointer_VMT: TExtendedHashFactory.TExtendedEqualityComparerVMT = (STD_RAW_INTERFACE_METHODS; Equals: @TEquals.Pointer; GetHashCode: @THashFactory.Pointer; GetHashList: @TExtendedHashFactory.Pointer);
{$WARNINGS ON}
  private class var
    // IExtendedEqualityComparer VMT
    FExtendedEqualityComparer_Int8_VMT  : TExtendedHashFactory.TExtendedEqualityComparerVMT;
    FExtendedEqualityComparer_Int16_VMT : TExtendedHashFactory.TExtendedEqualityComparerVMT;
    FExtendedEqualityComparer_Int32_VMT : TExtendedHashFactory.TExtendedEqualityComparerVMT;
    FExtendedEqualityComparer_Int64_VMT : TExtendedHashFactory.TExtendedEqualityComparerVMT;
    FExtendedEqualityComparer_UInt8_VMT : TExtendedHashFactory.TExtendedEqualityComparerVMT;
    FExtendedEqualityComparer_UInt16_VMT: TExtendedHashFactory.TExtendedEqualityComparerVMT;
    FExtendedEqualityComparer_UInt32_VMT: TExtendedHashFactory.TExtendedEqualityComparerVMT;
    FExtendedEqualityComparer_UInt64_VMT: TExtendedHashFactory.TExtendedEqualityComparerVMT;

    FExtendedEqualityComparer_Single_VMT  : TExtendedHashFactory.TExtendedEqualityComparerVMT;
    FExtendedEqualityComparer_Double_VMT  : TExtendedHashFactory.TExtendedEqualityComparerVMT;
    FExtendedEqualityComparer_Extended_VMT: TExtendedHashFactory.TExtendedEqualityComparerVMT;

    FExtendedEqualityComparer_Currency_VMT: TExtendedHashFactory.TExtendedEqualityComparerVMT;
    FExtendedEqualityComparer_Comp_VMT    : TExtendedHashFactory.TExtendedEqualityComparerVMT;

    FExtendedEqualityComparer_Binary_VMT  : TExtendedHashFactory.TExtendedEqualityComparerVMT;
    FExtendedEqualityComparer_DynArray_VMT: TExtendedHashFactory.TExtendedEqualityComparerVMT;

    FExtendedEqualityComparer_Class_VMT: TExtendedHashFactory.TExtendedEqualityComparerVMT;

    FExtendedEqualityComparer_ShortString1_VMT : TExtendedHashFactory.TExtendedEqualityComparerVMT;
    FExtendedEqualityComparer_ShortString2_VMT : TExtendedHashFactory.TExtendedEqualityComparerVMT;
    FExtendedEqualityComparer_ShortString3_VMT : TExtendedHashFactory.TExtendedEqualityComparerVMT;
    FExtendedEqualityComparer_ShortString_VMT  : TExtendedHashFactory.TExtendedEqualityComparerVMT;
    FExtendedEqualityComparer_AnsiString_VMT   : TExtendedHashFactory.TExtendedEqualityComparerVMT;
    FExtendedEqualityComparer_WideString_VMT   : TExtendedHashFactory.TExtendedEqualityComparerVMT;
    FExtendedEqualityComparer_UnicodeString_VMT: TExtendedHashFactory.TExtendedEqualityComparerVMT;

    FExtendedEqualityComparer_Method_VMT : TExtendedHashFactory.TExtendedEqualityComparerVMT;
    FExtendedEqualityComparer_Variant_VMT: TExtendedHashFactory.TExtendedEqualityComparerVMT;
    FExtendedEqualityComparer_Pointer_VMT: TExtendedHashFactory.TExtendedEqualityComparerVMT;

    FExtendedEqualityComparer_Int8_Instance         : Pointer;
    FExtendedEqualityComparer_Int16_Instance        : Pointer;
    FExtendedEqualityComparer_Int32_Instance        : Pointer;
    FExtendedEqualityComparer_Int64_Instance        : Pointer;
    FExtendedEqualityComparer_UInt8_Instance        : Pointer;
    FExtendedEqualityComparer_UInt16_Instance       : Pointer;
    FExtendedEqualityComparer_UInt32_Instance       : Pointer;
    FExtendedEqualityComparer_UInt64_Instance       : Pointer;

    FExtendedEqualityComparer_Single_Instance       : Pointer;
    FExtendedEqualityComparer_Double_Instance       : Pointer;
    FExtendedEqualityComparer_Extended_Instance     : Pointer;

    FExtendedEqualityComparer_Currency_Instance     : Pointer;
    FExtendedEqualityComparer_Comp_Instance         : Pointer;

    //FExtendedEqualityComparer_Binary_Instance     : Pointer;  // dynamic instance
    //FExtendedEqualityComparer_DynArray_Instance   : Pointer;  // dynamic instance

    FExtendedEqualityComparer_ShortString1_Instance : Pointer;
    FExtendedEqualityComparer_ShortString2_Instance : Pointer;
    FExtendedEqualityComparer_ShortString3_Instance : Pointer;
    FExtendedEqualityComparer_ShortString_Instance  : Pointer;
    FExtendedEqualityComparer_AnsiString_Instance   : Pointer;
    FExtendedEqualityComparer_WideString_Instance   : Pointer;
    FExtendedEqualityComparer_UnicodeString_Instance: Pointer;

    FExtendedEqualityComparer_Method_Instance       : Pointer;
    FExtendedEqualityComparer_Variant_Instance      : Pointer;
    FExtendedEqualityComparer_Pointer_Instance      : Pointer;

    // all instances
    FExtendedEqualityComparerInstances: array[TTypeKind] of TInstance;
  private
    class constructor Create;
  public
    class function LookupExtendedEqualityComparer(ATypeInfo: PTypeInfo; ASize: SizeInt): Pointer; override;
  end;

  TOnEqualityComparison<T> = function(constref ALeft, ARight: T): Boolean of object;
  TEqualityComparisonFunc<T> = function(constref ALeft, ARight: T): Boolean;

  TOnHasher<T> = function(constref AValue: T): UInt32 of object;
  TOnExtendedHasher<T> = procedure(constref AValue: T; AHashList: PUInt32) of object;
  THasherFunc<T> = function(constref AValue: T): UInt32;
  TExtendedHasherFunc<T> = procedure(constref AValue: T; AHashList: PUInt32);

  TEqualityComparer<T> = class(TInterfacedObject, IEqualityComparer<T>)
  public
    class function Default: IEqualityComparer<T>; static; overload;
    class function Default(AHashFactoryClass: THashFactoryClass): IEqualityComparer<T>; static; overload;

    class function Construct(const AEqualityComparison: TOnEqualityComparison<T>;
      const AHasher: TOnHasher<T>): IEqualityComparer<T>; overload;
    class function Construct(const AEqualityComparison: TEqualityComparisonFunc<T>;
      const AHasher: THasherFunc<T>): IEqualityComparer<T>; overload;

    function Equals(constref ALeft, ARight: T): Boolean; virtual; overload; abstract;
    function GetHashCode(constref AValue: T): UInt32;  virtual; overload; abstract;
  end;

  { TDelegatedEqualityComparerEvent }

  TDelegatedEqualityComparerEvents<T> = class(TEqualityComparer<T>)
  private
    FEqualityComparison: TOnEqualityComparison<T>;
    FHasher: TOnHasher<T>;
  public
    function Equals(constref ALeft, ARight: T): Boolean; override;
    function GetHashCode(constref AValue: T): UInt32; override;

    constructor Create(const AEqualityComparison: TOnEqualityComparison<T>;
      const AHasher: TOnHasher<T>);
  end;

  TDelegatedEqualityComparerFunc<T> = class(TEqualityComparer<T>)
  private
    FEqualityComparison: TEqualityComparisonFunc<T>;
    FHasher: THasherFunc<T>;
  public
    function Equals(constref ALeft, ARight: T): Boolean; override;
    function GetHashCode(constref AValue: T): UInt32; override;

    constructor Create(const AEqualityComparison: TEqualityComparisonFunc<T>;
      const AHasher: THasherFunc<T>);
  end;

  { TExtendedEqualityComparer }

  TExtendedEqualityComparer<T> = class(TEqualityComparer<T>, IExtendedEqualityComparer<T>)
  public
    class function Default: IExtendedEqualityComparer<T>; static; overload; reintroduce;
    class function Default(AExtenedHashFactoryClass: TExtendedHashFactoryClass): IExtendedEqualityComparer<T>; static; overload; reintroduce;

    class function Construct(const AEqualityComparison: TOnEqualityComparison<T>;
       const AHasher: TOnHasher<T>; const AExtendedHasher: TOnExtendedHasher<T>): IExtendedEqualityComparer<T>; overload; reintroduce;
    class function Construct(const AEqualityComparison: TEqualityComparisonFunc<T>;
       const AHasher: THasherFunc<T>; const AExtendedHasher: TExtendedHasherFunc<T>): IExtendedEqualityComparer<T>; overload; reintroduce;
    class function Construct(const AEqualityComparison: TOnEqualityComparison<T>;
       const AExtendedHasher: TOnExtendedHasher<T>): IExtendedEqualityComparer<T>; overload; reintroduce;
    class function Construct(const AEqualityComparison: TEqualityComparisonFunc<T>;
       const AExtendedHasher: TExtendedHasherFunc<T>): IExtendedEqualityComparer<T>; overload; reintroduce;

    procedure GetHashList(constref AValue: T; AHashList: PUInt32); virtual; abstract;
  end;

  TDelegatedExtendedEqualityComparerEvents<T> = class(TExtendedEqualityComparer<T>)
  private
    FEqualityComparison: TOnEqualityComparison<T>;
    FHasher: TOnHasher<T>;
    FExtendedHasher: TOnExtendedHasher<T>;

    function GetHashCodeMethod(constref AValue: T): UInt32;
  public
    function Equals(constref ALeft, ARight: T): Boolean; override;
    function GetHashCode(constref AValue: T): UInt32; override;
    procedure GetHashList(constref AValue: T; AHashList: PUInt32); override;

    constructor Create(const AEqualityComparison: TOnEqualityComparison<T>;
      const AHasher: TOnHasher<T>; const AExtendedHasher: TOnExtendedHasher<T>); overload;
    constructor Create(const AEqualityComparison: TOnEqualityComparison<T>;
      const AExtendedHasher: TOnExtendedHasher<T>); overload;
  end;

  TDelegatedExtendedEqualityComparerFunc<T> = class(TExtendedEqualityComparer<T>)
  private
    FEqualityComparison: TEqualityComparisonFunc<T>;
    FHasher: THasherFunc<T>;
    FExtendedHasher: TExtendedHasherFunc<T>;
  public
    function Equals(constref ALeft, ARight: T): Boolean; override;
    function GetHashCode(constref AValue: T): UInt32; override;
    procedure GetHashList(constref AValue: T; AHashList: PUInt32); override;

    constructor Create(const AEqualityComparison: TEqualityComparisonFunc<T>;
      const AHasher: THasherFunc<T>; const AExtendedHasher: TExtendedHasherFunc<T>); overload;
    constructor Create(const AEqualityComparison: TEqualityComparisonFunc<T>;
      const AExtendedHasher: TExtendedHasherFunc<T>); overload;
  end;

  { TDelphiHashFactory }

  TDelphiHashFactory = class(THashFactory)
  public
    class function GetHashService: THashServiceClass; override;
    class function GetHashCode(AKey: Pointer; ASize: SizeInt; AInitVal: UInt32 = 0): UInt32; override;
  end;

  { TAdler32HashFactory }

  TAdler32HashFactory = class(THashFactory)
  public
    class function GetHashService: THashServiceClass; override;
    class function GetHashCode(AKey: Pointer; ASize: SizeInt; AInitVal: UInt32 = 0): UInt32; override;
  end;

  { TSdbmHashFactory }

  TSdbmHashFactory = class(THashFactory)
  public
    class function GetHashService: THashServiceClass; override;
    class function GetHashCode(AKey: Pointer; ASize: SizeInt; AInitVal: UInt32 = 0): UInt32; override;
  end;

  { TSdbmHashFactory }

  TSimpleChecksumFactory = class(THashFactory)
  public
    class function GetHashService: THashServiceClass; override;
    class function GetHashCode(AKey: Pointer; ASize: SizeInt; AInitVal: UInt32 = 0): UInt32; override;
  end;

  { TDelphiDoubleHashFactory }

  TDelphiDoubleHashFactory = class(TExtendedHashFactory)
  public
    const MAX_HASHLIST_COUNT = 2;
    const HASH_FUNCTIONS_COUNT = 1;
    const HASHLIST_COUNT_PER_FUNCTION: array[1..HASH_FUNCTIONS_COUNT] of Integer = (2);
    const HASH_FUNCTIONS_MASK_SIZE = 1;
    const HASH_FUNCTIONS_MASK = 1; // 00000001b

    class function GetHashService: THashServiceClass; override;
    class function GetHashCode(AKey: Pointer; ASize: SizeInt; AInitVal: UInt32 = 0): UInt32; override;
    class procedure GetHashList(AKey: Pointer; ASize: SizeInt; AHashList: PUInt32; AOptions: TGetHashListOptions = []); override;
  end;

  TDelphiQuadrupleHashFactory = class(TExtendedHashFactory)
  public
    const MAX_HASHLIST_COUNT = 4;
    const HASH_FUNCTIONS_COUNT = 2;
    const HASHLIST_COUNT_PER_FUNCTION: array[1..HASH_FUNCTIONS_COUNT] of Integer = (2, 2);
    const HASH_FUNCTIONS_MASK_SIZE = 2;
    const HASH_FUNCTIONS_MASK = 3; // 00000011b

    class function GetHashService: THashServiceClass; override;
    class function GetHashCode(AKey: Pointer; ASize: SizeInt; AInitVal: UInt32 = 0): UInt32; override;
    class procedure GetHashList(AKey: Pointer; ASize: SizeInt; AHashList: PUInt32; AOptions: TGetHashListOptions = []); override;
  end;

  TDelphiSixfoldHashFactory = class(TExtendedHashFactory)
  public
    const MAX_HASHLIST_COUNT = 6;
    const HASH_FUNCTIONS_COUNT = 3;
    const HASHLIST_COUNT_PER_FUNCTION: array[1..HASH_FUNCTIONS_COUNT] of Integer = (2, 2, 2);
    const HASH_FUNCTIONS_MASK_SIZE = 3;
    const HASH_FUNCTIONS_MASK = 7; // 00000111b

    class function GetHashService: THashServiceClass; override;
    class function GetHashCode(AKey: Pointer; ASize: SizeInt; AInitVal: UInt32 = 0): UInt32; override;
    class procedure GetHashList(AKey: Pointer; ASize: SizeInt; AHashList: PUInt32; AOptions: TGetHashListOptions = []); override;
  end;

  TDefaultHashFactory = TDelphiQuadrupleHashFactory;

  TDefaultGenericInterface = (giComparer, giEqualityComparer, giExtendedEqualityComparer);

  TCustomComparer<T> = class(TSingletonImplementation, IComparer<T>, IEqualityComparer<T>, IExtendedEqualityComparer<T>)
  protected
    function Compare(constref Left, Right: T): Integer; virtual; abstract;
    function Equals(constref Left, Right: T): Boolean; reintroduce; overload; virtual; abstract;
    function GetHashCode(constref Value: T): UInt32; reintroduce; overload; virtual; abstract;
    procedure GetHashList(constref Value: T; AHashList: PUInt32); virtual; abstract;
  end;

  TOrdinalComparer<T, THashFactory> = class(TCustomComparer<T>)
  protected class var
    FComparer: IComparer<T>;
    FEqualityComparer: IEqualityComparer<T>;
    FExtendedEqualityComparer: IExtendedEqualityComparer<T>;

    class constructor Create;
  public
    class function Ordinal: TCustomComparer<T>; virtual; abstract;
  end;

  // TGStringComparer will be renamed to TStringComparer -> bug #26030
  // anyway class var can't be used safely -> bug #24848

  TGStringComparer<T, THashFactory> = class(TOrdinalComparer<T, THashFactory>)
  private class var
    FOrdinal: TCustomComparer<T>;
    class destructor Destroy;
  public
    class function Ordinal: TCustomComparer<T>; override;
  end;

  TGStringComparer<T> = class(TGStringComparer<T, TDelphiQuadrupleHashFactory>);
  TStringComparer = class(TGStringComparer<string>);
  TAnsiStringComparer = class(TGStringComparer<AnsiString>);
  TUnicodeStringComparer = class(TGStringComparer<UnicodeString>);

  { TGOrdinalStringComparer }

  // TGOrdinalStringComparer will be renamed to TOrdinalStringComparer -> bug #26030
  // anyway class var can't be used safely -> bug #24848
  TGOrdinalStringComparer<T, THashFactory> = class(TGStringComparer<T, THashFactory>)
  public
    function Compare(constref ALeft, ARight: T): Integer; override;
    function Equals(constref ALeft, ARight: T): Boolean; overload; override;
    function GetHashCode(constref AValue: T): UInt32; overload; override;
    procedure GetHashList(constref AValue: T; AHashList: PUInt32); override;
  end;

  TGOrdinalStringComparer<T> = class(TGOrdinalStringComparer<T, TDelphiQuadrupleHashFactory>);
  TOrdinalStringComparer = class(TGOrdinalStringComparer<string>);

  TGIStringComparer<T, THashFactory> = class(TOrdinalComparer<T, THashFactory>)
  private class var
    FOrdinal: TCustomComparer<T>;
    class destructor Destroy;
  public
    class function Ordinal: TCustomComparer<T>; override;
  end;

  TGIStringComparer<T> = class(TGIStringComparer<T, TDelphiQuadrupleHashFactory>);
  TIStringComparer = class(TGIStringComparer<string>);
  TIAnsiStringComparer = class(TGIStringComparer<AnsiString>);
  TIUnicodeStringComparer = class(TGIStringComparer<UnicodeString>);

  TGOrdinalIStringComparer<T, THashFactory> = class(TGIStringComparer<T, THashFactory>)
  public
    function Compare(constref ALeft, ARight: T): Integer; override;
    function Equals(constref ALeft, ARight: T): Boolean; overload; override;
    function GetHashCode(constref AValue: T): UInt32; overload; override;
    procedure GetHashList(constref AValue: T; AHashList: PUInt32); override;
  end;

  TGOrdinalIStringComparer<T> = class(TGOrdinalIStringComparer<T, TDelphiQuadrupleHashFactory>);
  TOrdinalIStringComparer = class(TGOrdinalIStringComparer<string>);

// Delphi version of Bob Jenkins Hash
function BobJenkinsHash(const AData; ALength, AInitData: Integer): Integer; // same result as HashLittle_Delphi, just different interface
function BinaryCompare(const ALeft, ARight: Pointer; ASize: PtrUInt): Integer; inline;

function _LookupVtableInfo(AGInterface: TDefaultGenericInterface; ATypeInfo: PTypeInfo; ASize: SizeInt): Pointer; inline;
function _LookupVtableInfoEx(AGInterface: TDefaultGenericInterface; ATypeInfo: PTypeInfo; ASize: SizeInt;
  AFactory: THashFactoryClass): Pointer;

implementation

{ TComparer<T> }

class function TComparer<T>.Default: IComparer<T>;
begin
  Result := _LookupVtableInfo(giComparer, TypeInfo(T), SizeOf(T));
end;

class function TComparer<T>.Construct(const AComparison: TOnComparison<T>): IComparer<T>;
begin
  Result := TDelegatedComparerEvents<T>.Create(AComparison);
end;

class function TComparer<T>.Construct(const AComparison: TComparisonFunc<T>): IComparer<T>;
begin
  Result := TDelegatedComparerFunc<T>.Create(AComparison);
end;

function TDelegatedComparerEvents<T>.Compare(constref ALeft, ARight: T): Integer;
begin
  Result := FComparison(ALeft, ARight);
end;

constructor TDelegatedComparerEvents<T>.Create(AComparison: TOnComparison<T>);
begin
  FComparison := AComparison;
end;

function TDelegatedComparerFunc<T>.Compare(constref ALeft, ARight: T): Integer;
begin
  Result := FComparison(ALeft, ARight);
end;

constructor TDelegatedComparerFunc<T>.Create(AComparison: TComparisonFunc<T>);
begin
  FComparison := AComparison;
end;

{ TInterface }

function TInterface.QueryInterface(constref IID: TGUID; out Obj): HResult;
begin
  Result := E_NOINTERFACE;
end;

{ TRawInterface }

function TRawInterface._AddRef: LongInt;
begin
  Result := -1;
end;

function TRawInterface._Release: LongInt;
begin
  Result := -1;
end;

{ TComTypeSizeInterface }

function TComTypeSizeInterface._AddRef: LongInt;
var
  _self: TComparerService.PSpoofInterfacedTypeSizeObject absolute Self;
begin
  Result := InterLockedIncrement(_self.RefCount);
end;

function TComTypeSizeInterface._Release: LongInt;
var
  _self: TComparerService.PSpoofInterfacedTypeSizeObject absolute Self;
begin
  Result := InterLockedDecrement(_self.RefCount);
  if _self.RefCount = 0 then
    Dispose(_self);
end;

{ TSingletonImplementation }

function TSingletonImplementation.QueryInterface(constref IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

{ TCompare }

(***********************************************************************************************************************
  Comparers
(**********************************************************************************************************************)

{-----------------------------------------------------------------------------------------------------------------------
  Comparers Int8 - Int32 and UInt8 - UInt32
{----------------------------------------------------------------------------------------------------------------------}

class function TCompare.Integer(constref ALeft, ARight: Integer): Integer;
begin
  Result := Math.CompareValue(ALeft, ARight);
end;

class function TCompare.Int8(constref ALeft, ARight: Int8): Integer;
begin
  Result := ALeft - ARight;
end;

class function TCompare.Int16(constref ALeft, ARight: Int16): Integer;
begin
  Result := ALeft - ARight;
end;

class function TCompare.Int32(constref ALeft, ARight: Int32): Integer;
begin
  if ALeft > ARight then
    Exit(1)
  else if ALeft < ARight then
    Exit(-1)
  else
    Exit(0);
end;

class function TCompare.Int64(constref ALeft, ARight: Int64): Integer;
begin
  if ALeft > ARight then
    Exit(1)
  else if ALeft < ARight then
    Exit(-1)
  else
    Exit(0);
end;

class function TCompare.UInt8(constref ALeft, ARight: UInt8): Integer;
begin
  Result := System.Integer(ALeft) - System.Integer(ARight);
end;

class function TCompare.UInt16(constref ALeft, ARight: UInt16): Integer;
begin
  Result := System.Integer(ALeft) - System.Integer(ARight);
end;

class function TCompare.UInt32(constref ALeft, ARight: UInt32): Integer;
begin
  if ALeft > ARight then
    Exit(1)
  else if ALeft < ARight then
    Exit(-1)
  else
    Exit(0);
end;

class function TCompare.UInt64(constref ALeft, ARight: UInt64): Integer;
begin
  if ALeft > ARight then
    Exit(1)
  else if ALeft < ARight then
    Exit(-1)
  else
    Exit(0);
end;

{-----------------------------------------------------------------------------------------------------------------------
  Comparers for Float types
{----------------------------------------------------------------------------------------------------------------------}

class function TCompare.Single(constref ALeft, ARight: Single): Integer;
begin
  if ALeft > ARight then
    Exit(1)
  else if ALeft < ARight then
    Exit(-1)
  else
    Exit(0);
end;

class function TCompare.Double(constref ALeft, ARight: Double): Integer;
begin
  if ALeft > ARight then
    Exit(1)
  else if ALeft < ARight then
    Exit(-1)
  else
    Exit(0);
end;

class function TCompare.Extended(constref ALeft, ARight: Extended): Integer;
begin
  if ALeft > ARight then
    Exit(1)
  else if ALeft < ARight then
    Exit(-1)
  else
    Exit(0);
end;

{-----------------------------------------------------------------------------------------------------------------------
  Comparers for other number types
{----------------------------------------------------------------------------------------------------------------------}

class function TCompare.Currency(constref ALeft, ARight: Currency): Integer;
begin
  if ALeft > ARight then
    Exit(1)
  else if ALeft < ARight then
    Exit(-1)
  else
    Exit(0);
end;

class function TCompare.Comp(constref ALeft, ARight: Comp): Integer;
begin
  if ALeft > ARight then
    Exit(1)
  else if ALeft < ARight then
    Exit(-1)
  else
    Exit(0);
end;

{-----------------------------------------------------------------------------------------------------------------------
  Comparers for binary data (records etc) and dynamics arrays
{----------------------------------------------------------------------------------------------------------------------}

class function TCompare._Binary(constref ALeft, ARight): Integer;
var
  _self: TComparerService.PSpoofInterfacedTypeSizeObject absolute Self;
begin
  Result := CompareMemRange(@ALeft, @ARight, _self.Size);
end;

class function TCompare._DynArray(constref ALeft, ARight: Pointer): Integer;
var
  _self: TComparerService.PSpoofInterfacedTypeSizeObject absolute Self;
  LLength, LLeftLength, LRightLength: Integer;
begin
  LLeftLength := DynArraySize(ALeft);
  LRightLength := DynArraySize(ARight);
  if LLeftLength > LRightLength then
    LLength := LRightLength
  else
    LLength := LLeftLength;

  Result := CompareMemRange(ALeft, ARight, LLength * _self.Size);

  if Result = 0 then
    Result := LLeftLength - LRightLength;
end;

class function TCompare.Binary(constref ALeft, ARight; const ASize: SizeInt): Integer;
begin
  Result := CompareMemRange(@ALeft, @ARight, ASize);
end;

class function TCompare.DynArray(constref ALeft, ARight: Pointer; const AElementSize: SizeInt): Integer;
var
  LLength, LLeftLength, LRightLength: Integer;
begin
  LLeftLength := DynArraySize(ALeft);
  LRightLength := DynArraySize(ARight);
  if LLeftLength > LRightLength then
    LLength := LRightLength
  else
    LLength := LLeftLength;

  Result := CompareMemRange(ALeft, ARight, LLength * AElementSize);

  if Result = 0 then
    Result := LLeftLength - LRightLength;
end;

{-----------------------------------------------------------------------------------------------------------------------
  Comparers for string types
{----------------------------------------------------------------------------------------------------------------------}

class function TCompare.ShortString1(constref ALeft, ARight: ShortString1): Integer;
begin
  if ALeft > ARight then
    Exit(1)
  else if ALeft < ARight then
    Exit(-1)
  else
    Exit(0);
end;

class function TCompare.ShortString2(constref ALeft, ARight: ShortString2): Integer;
begin
  if ALeft > ARight then
    Exit(1)
  else if ALeft < ARight then
    Exit(-1)
  else
    Exit(0);
end;

class function TCompare.ShortString3(constref ALeft, ARight: ShortString3): Integer;
begin
  if ALeft > ARight then
    Exit(1)
  else if ALeft < ARight then
    Exit(-1)
  else
    Exit(0);
end;

class function TCompare.ShortString(constref ALeft, ARight: ShortString): Integer;
begin
  if ALeft > ARight then
    Exit(1)
  else if ALeft < ARight then
    Exit(-1)
  else
    Exit(0);
end;

class function TCompare.&String(constref ALeft, ARight: String): Integer;
begin
  Result := CompareStr(ALeft, ARight);
end;

class function TCompare.AnsiString(constref ALeft, ARight: AnsiString): Integer;
begin
  Result := AnsiCompareStr(ALeft, ARight);
end;

class function TCompare.WideString(constref ALeft, ARight: WideString): Integer;
begin
  Result := WideCompareStr(ALeft, ARight);
end;

class function TCompare.UnicodeString(constref ALeft, ARight: UnicodeString): Integer;
begin
  Result := UnicodeCompareStr(ALeft, ARight);
end;

{-----------------------------------------------------------------------------------------------------------------------
  Comparers for Delegates
{----------------------------------------------------------------------------------------------------------------------}

class function TCompare.Method(constref ALeft, ARight: TMethod): Integer;
begin
  Result := CompareMemRange(@ALeft, @ARight, SizeOf(System.TMethod));
end;

{-----------------------------------------------------------------------------------------------------------------------
  Comparers for Variant
{----------------------------------------------------------------------------------------------------------------------}

class function TCompare.Variant(constref ALeft, ARight: PVariant): Integer;
var
  LLeftString, LRightString: string;
begin
  try
    case VarCompareValue(ALeft^, ARight^) of
      vrGreaterThan:
        Exit(1);
      vrLessThan:
        Exit(-1);
      vrEqual:
        Exit(0);
      vrNotEqual:
        if VarIsEmpty(ALeft^) or VarIsNull(ALeft^) then
          Exit(1)
        else
          Exit(-1);
    end;
  except
    try
      LLeftString := ALeft^;
      LRightString := ARight^;
      Result := CompareStr(LLeftString, LRightString);
    except
      Result := CompareMemRange(ALeft, ARight, SizeOf(System.Variant));
    end;
  end;
end;

{-----------------------------------------------------------------------------------------------------------------------
  Comparers for Pointer
{----------------------------------------------------------------------------------------------------------------------}

class function TCompare.Pointer(constref ALeft, ARight: PtrUInt): Integer;
begin
  if ALeft > ARight then
    Exit(1)
  else if ALeft < ARight then
    Exit(-1)
  else
    Exit(0);
end;

{ TEquals }

(***********************************************************************************************************************
  Equality Comparers
(**********************************************************************************************************************)

{-----------------------------------------------------------------------------------------------------------------------
  Equality Comparers Int8 - Int32 and UInt8 - UInt32
{----------------------------------------------------------------------------------------------------------------------}

class function TEquals.Integer(constref ALeft, ARight: Integer): Boolean;
begin
  Result := ALeft = ARight;
end;

class function TEquals.Int8(constref ALeft, ARight: Int8): Boolean;
begin
  Result := ALeft = ARight;
end;

class function TEquals.Int16(constref ALeft, ARight: Int16): Boolean;
begin
  Result := ALeft = ARight;
end;

class function TEquals.Int32(constref ALeft, ARight: Int32): Boolean;
begin
  Result := ALeft = ARight;
end;

class function TEquals.Int64(constref ALeft, ARight: Int64): Boolean;
begin
  Result := ALeft = ARight;
end;

class function TEquals.UInt8(constref ALeft, ARight: UInt8): Boolean;
begin
  Result := ALeft = ARight;
end;

class function TEquals.UInt16(constref ALeft, ARight: UInt16): Boolean;
begin
  Result := ALeft = ARight;
end;

class function TEquals.UInt32(constref ALeft, ARight: UInt32): Boolean;
begin
  Result := ALeft = ARight;
end;

class function TEquals.UInt64(constref ALeft, ARight: UInt64): Boolean;
begin
  Result := ALeft = ARight;
end;

{-----------------------------------------------------------------------------------------------------------------------
  Equality Comparers for Float types
{----------------------------------------------------------------------------------------------------------------------}

class function TEquals.Single(constref ALeft, ARight: Single): Boolean;
begin
  Result := ALeft = ARight;
end;

class function TEquals.Double(constref ALeft, ARight: Double): Boolean;
begin
  Result := ALeft = ARight;
end;

class function TEquals.Extended(constref ALeft, ARight: Extended): Boolean;
begin
  Result := ALeft = ARight;
end;

{-----------------------------------------------------------------------------------------------------------------------
  Equality Comparers for other number types
{----------------------------------------------------------------------------------------------------------------------}

class function TEquals.Currency(constref ALeft, ARight: Currency): Boolean;
begin
  Result := ALeft = ARight;
end;

class function TEquals.Comp(constref ALeft, ARight: Comp): Boolean;
begin
  Result := ALeft = ARight;
end;

{-----------------------------------------------------------------------------------------------------------------------
  Equality Comparers for binary data (records etc) and dynamics arrays
{----------------------------------------------------------------------------------------------------------------------}

class function TEquals._Binary(constref ALeft, ARight): Boolean;
var
  _self: TComparerService.PSpoofInterfacedTypeSizeObject absolute Self;
begin
  Result := CompareMem(@ALeft, @ARight, _self.Size);
end;

class function TEquals._DynArray(constref ALeft, ARight: Pointer): Boolean;
var
  _self: TComparerService.PSpoofInterfacedTypeSizeObject absolute Self;
  LLength: Integer;
begin
  LLength := DynArraySize(ALeft);
  if LLength <> DynArraySize(ARight) then
    Exit(False);

  Result := CompareMem(ALeft, ARight, LLength * _self.Size);
end;

class function TEquals.Binary(constref ALeft, ARight; const ASize: SizeInt): Boolean;
begin
  Result := CompareMem(@ALeft, @ARight, ASize);
end;

class function TEquals.DynArray(constref ALeft, ARight: Pointer; const AElementSize: SizeInt): Boolean;
var
  LLength: Integer;
begin
  LLength := DynArraySize(ALeft);
  if LLength <> DynArraySize(ARight) then
    Exit(False);

  Result := CompareMem(ALeft, ARight, LLength * AElementSize);
end;

{-----------------------------------------------------------------------------------------------------------------------
  Equality Comparers for classes
{----------------------------------------------------------------------------------------------------------------------}

class function TEquals.&class(constref ALeft, ARight: TObject): Boolean;
begin
  if ALeft <> nil then
    Exit(ALeft.Equals(ARight))
  else
    Exit(ARight = nil);
end;

{-----------------------------------------------------------------------------------------------------------------------
  Equality Comparers for string types
{----------------------------------------------------------------------------------------------------------------------}

class function TEquals.ShortString1(constref ALeft, ARight: ShortString1): Boolean;
begin
  Result := ALeft = ARight;
end;

class function TEquals.ShortString2(constref ALeft, ARight: ShortString2): Boolean;
begin
  Result := ALeft = ARight;
end;

class function TEquals.ShortString3(constref ALeft, ARight: ShortString3): Boolean;
begin
  Result := ALeft = ARight;
end;

class function TEquals.&String(constref ALeft, ARight: String): Boolean;
begin
  Result := ALeft = ARight;
end;

class function TEquals.ShortString(constref ALeft, ARight: ShortString): Boolean;
begin
  Result := ALeft = ARight;
end;

class function TEquals.AnsiString(constref ALeft, ARight: AnsiString): Boolean;
begin
  Result := ALeft = ARight;
end;

class function TEquals.WideString(constref ALeft, ARight: WideString): Boolean;
begin
  Result := ALeft = ARight;
end;

class function TEquals.UnicodeString(constref ALeft, ARight: UnicodeString): Boolean;
begin
  Result := ALeft = ARight;
end;

{-----------------------------------------------------------------------------------------------------------------------
  Equality Comparers for Delegates
{----------------------------------------------------------------------------------------------------------------------}

class function TEquals.Method(constref ALeft, ARight: TMethod): Boolean;
begin
  Result := (ALeft.Code = ARight.Code) and (ALeft.Data = ARight.Data);
end;

{-----------------------------------------------------------------------------------------------------------------------
  Equality Comparers for Variant
{----------------------------------------------------------------------------------------------------------------------}

class function TEquals.Variant(constref ALeft, ARight: PVariant): Boolean;
begin
  Result := VarCompareValue(ALeft^, ARight^) = vrEqual;
end;

{-----------------------------------------------------------------------------------------------------------------------
  Equality Comparers for Pointer
{----------------------------------------------------------------------------------------------------------------------}

class function TEquals.Pointer(constref ALeft, ARight: PtrUInt): Boolean;
begin
  Result := ALeft = ARight;
end;

(***********************************************************************************************************************
  Hashes
(**********************************************************************************************************************)

{-----------------------------------------------------------------------------------------------------------------------
  GetHashCode Int8 - Int32 and UInt8 - UInt32
{----------------------------------------------------------------------------------------------------------------------}

class function THashFactory.Int8(constref AValue: Int8): UInt32;
begin
  Result := HASH_FACTORY.GetHashCode(@AValue, SizeOf(System.Int8), 0);
end;

class function THashFactory.Int16(constref AValue: Int16): UInt32;
begin
  Result := HASH_FACTORY.GetHashCode(@AValue, SizeOf(System.Int16), 0);
end;

class function THashFactory.Int32(constref AValue: Int32): UInt32;
begin
  Result := HASH_FACTORY.GetHashCode(@AValue, SizeOf(System.Int32), 0);
end;

class function THashFactory.Int64(constref AValue: Int64): UInt32;
begin
  Result := HASH_FACTORY.GetHashCode(@AValue, SizeOf(System.Int64), 0);
end;

class function THashFactory.UInt8(constref AValue: UInt8): UInt32;
begin
  Result := HASH_FACTORY.GetHashCode(@AValue, SizeOf(System.UInt8), 0);
end;

class function THashFactory.UInt16(constref AValue: UInt16): UInt32;
begin
  Result := HASH_FACTORY.GetHashCode(@AValue, SizeOf(System.UInt16), 0);
end;

class function THashFactory.UInt32(constref AValue: UInt32): UInt32;
begin
  Result := HASH_FACTORY.GetHashCode(@AValue, SizeOf(System.UInt32), 0);
end;

class function THashFactory.UInt64(constref AValue: UInt64): UInt32;
begin
  Result := HASH_FACTORY.GetHashCode(@AValue, SizeOf(System.UInt64), 0);
end;

{-----------------------------------------------------------------------------------------------------------------------
  GetHashCode for Float types
{----------------------------------------------------------------------------------------------------------------------}

class function THashFactory.Single(constref AValue: Single): UInt32;
var
  LMantissa: Float;
  LExponent: Integer;
begin
  Frexp(AValue, LMantissa, LExponent);

  if LMantissa = 0 then
    LMantissa := Abs(LMantissa);

  Result := HASH_FACTORY.GetHashCode(@LMantissa, SizeOf(Math.Float), 0);
  Result := HASH_FACTORY.GetHashCode(@LExponent, SizeOf(System.Integer), Result);
end;

class function THashFactory.Double(constref AValue: Double): UInt32;
var
  LMantissa: Float;
  LExponent: Integer;
begin
  Frexp(AValue, LMantissa, LExponent);

  if LMantissa = 0 then
    LMantissa := Abs(LMantissa);

  Result := HASH_FACTORY.GetHashCode(@LMantissa, SizeOf(Math.Float), 0);
  Result := HASH_FACTORY.GetHashCode(@LExponent, SizeOf(System.Integer), Result);
end;

class function THashFactory.Extended(constref AValue: Extended): UInt32;
var
  LMantissa: Float;
  LExponent: Integer;
begin
  Frexp(AValue, LMantissa, LExponent);

  if LMantissa = 0 then
    LMantissa := Abs(LMantissa);

  Result := HASH_FACTORY.GetHashCode(@LMantissa, SizeOf(Math.Float), 0);
  Result := HASH_FACTORY.GetHashCode(@LExponent, SizeOf(System.Integer), Result);
end;

{-----------------------------------------------------------------------------------------------------------------------
  GetHashCode for other number types
{----------------------------------------------------------------------------------------------------------------------}

class function THashFactory.Currency(constref AValue: Currency): UInt32;
begin
  Result := HASH_FACTORY.GetHashCode(@AValue, SizeOf(System.Int64), 0);
end;

class function THashFactory.Comp(constref AValue: Comp): UInt32;
begin
  Result := HASH_FACTORY.GetHashCode(@AValue, SizeOf(System.Int64), 0);
end;

{-----------------------------------------------------------------------------------------------------------------------
  GetHashCode for binary data (records etc) and dynamics arrays
{----------------------------------------------------------------------------------------------------------------------}

class function THashFactory.Binary(constref AValue): UInt32;
var
  _self: TComparerService.PSpoofInterfacedTypeSizeObject absolute Self;
begin
  Result := HASH_FACTORY.GetHashCode(@AValue, _self.Size, 0);
end;

class function THashFactory.DynArray(constref AValue: Pointer): UInt32;
var
  _self: TComparerService.PSpoofInterfacedTypeSizeObject absolute Self;
begin
  Result := HASH_FACTORY.GetHashCode(AValue, DynArraySize(AValue) * _self.Size, 0);
end;

{-----------------------------------------------------------------------------------------------------------------------
  GetHashCode for classes
{----------------------------------------------------------------------------------------------------------------------}

class function THashFactory.&Class(constref AValue: TObject): UInt32;
begin
  if AValue = nil then
    Exit($2A);

  Result := AValue.GetHashCode;
end;

{-----------------------------------------------------------------------------------------------------------------------
  GetHashCode for string types
{----------------------------------------------------------------------------------------------------------------------}

class function THashFactory.ShortString1(constref AValue: ShortString1): UInt32;
begin
  Result := HASH_FACTORY.GetHashCode(@AValue[1], Length(AValue), 0);
end;

class function THashFactory.ShortString2(constref AValue: ShortString2): UInt32;
begin
  Result := HASH_FACTORY.GetHashCode(@AValue[1], Length(AValue), 0);
end;

class function THashFactory.ShortString3(constref AValue: ShortString3): UInt32;
begin
  Result := HASH_FACTORY.GetHashCode(@AValue[1], Length(AValue), 0);
end;

class function THashFactory.ShortString(constref AValue: ShortString): UInt32;
begin
  Result := HASH_FACTORY.GetHashCode(@AValue[1], Length(AValue), 0);
end;

class function THashFactory.AnsiString(constref AValue: AnsiString): UInt32;
begin
  Result := HASH_FACTORY.GetHashCode(@AValue[1], Length(AValue) * SizeOf(System.AnsiChar), 0);
end;

class function THashFactory.WideString(constref AValue: WideString): UInt32;
begin
  Result := HASH_FACTORY.GetHashCode(@AValue[1], Length(AValue) * SizeOf(System.WideChar), 0);
end;

class function THashFactory.UnicodeString(constref AValue: UnicodeString): UInt32;
begin
  Result := HASH_FACTORY.GetHashCode(@AValue[1], Length(AValue) * SizeOf(System.UnicodeChar), 0);
end;

{-----------------------------------------------------------------------------------------------------------------------
  GetHashCode for Delegates
{----------------------------------------------------------------------------------------------------------------------}

class function THashFactory.Method(constref AValue: TMethod): UInt32;
begin
  Result := HASH_FACTORY.GetHashCode(@AValue, SizeOf(System.TMethod), 0);
end;

{-----------------------------------------------------------------------------------------------------------------------
  GetHashCode for Variant
{----------------------------------------------------------------------------------------------------------------------}

class function THashFactory.Variant(constref AValue: PVariant): UInt32;
begin
  try
    Result := HASH_FACTORY.UnicodeString(AValue^);
  except
    Result := HASH_FACTORY.GetHashCode(AValue, SizeOf(System.Variant), 0);
  end;
end;

{-----------------------------------------------------------------------------------------------------------------------
  GetHashCode for Pointer
{----------------------------------------------------------------------------------------------------------------------}

class function THashFactory.Pointer(constref AValue: Pointer): UInt32;
begin
  Result := HASH_FACTORY.GetHashCode(@AValue, SizeOf(System.Pointer), 0);
end;

{ TExtendedHashFactory }

(***********************************************************************************************************************
  Hashes 2
(**********************************************************************************************************************)

{-----------------------------------------------------------------------------------------------------------------------
  GetHashCode Int8 - Int32 and UInt8 - UInt32
{----------------------------------------------------------------------------------------------------------------------}

class procedure TExtendedHashFactory.Int8(constref AValue: Int8; AHashList: PUInt32);
begin
  EXTENDED_HASH_FACTORY.GetHashList(@AValue, SizeOf(System.Int8), AHashList, []);
end;

class procedure TExtendedHashFactory.Int16(constref AValue: Int16; AHashList: PUInt32);
begin
  EXTENDED_HASH_FACTORY.GetHashList(@AValue, SizeOf(System.Int16), AHashList, []);
end;

class procedure TExtendedHashFactory.Int32(constref AValue: Int32; AHashList: PUInt32);
begin
  EXTENDED_HASH_FACTORY.GetHashList(@AValue, SizeOf(System.Int32), AHashList, []);
end;

class procedure TExtendedHashFactory.Int64(constref AValue: Int64; AHashList: PUInt32);
begin
  EXTENDED_HASH_FACTORY.GetHashList(@AValue, SizeOf(System.Int64), AHashList, []);
end;

class procedure TExtendedHashFactory.UInt8(constref AValue: UInt8; AHashList: PUInt32);
begin
  EXTENDED_HASH_FACTORY.GetHashList(@AValue, SizeOf(System.UInt8), AHashList, []);
end;

class procedure TExtendedHashFactory.UInt16(constref AValue: UInt16; AHashList: PUInt32);
begin
  EXTENDED_HASH_FACTORY.GetHashList(@AValue, SizeOf(System.UInt16), AHashList, []);
end;

class procedure TExtendedHashFactory.UInt32(constref AValue: UInt32; AHashList: PUInt32);
begin
  EXTENDED_HASH_FACTORY.GetHashList(@AValue, SizeOf(System.UInt32), AHashList, []);
end;

class procedure TExtendedHashFactory.UInt64(constref AValue: UInt64; AHashList: PUInt32);
begin
  EXTENDED_HASH_FACTORY.GetHashList(@AValue, SizeOf(System.UInt64), AHashList, []);
end;

{-----------------------------------------------------------------------------------------------------------------------
  GetHashCode for Float types
{----------------------------------------------------------------------------------------------------------------------}

class procedure TExtendedHashFactory.Single(constref AValue: Single; AHashList: PUInt32);
var
  LMantissa: Float;
  LExponent: Integer;
begin
  Frexp(AValue, LMantissa, LExponent);

  if LMantissa = 0 then
    LMantissa := Abs(LMantissa);

  EXTENDED_HASH_FACTORY.GetHashList(@LMantissa, SizeOf(Math.Float), AHashList, []);
  EXTENDED_HASH_FACTORY.GetHashList(@LExponent, SizeOf(System.Integer), AHashList, [ghloHashListAsInitData]);
end;

class procedure TExtendedHashFactory.Double(constref AValue: Double; AHashList: PUInt32);
var
  LMantissa: Float;
  LExponent: Integer;
begin
  Frexp(AValue, LMantissa, LExponent);

  if LMantissa = 0 then
    LMantissa := Abs(LMantissa);

  EXTENDED_HASH_FACTORY.GetHashList(@LMantissa, SizeOf(Math.Float), AHashList, []);
  EXTENDED_HASH_FACTORY.GetHashList(@LExponent, SizeOf(System.Integer), AHashList, [ghloHashListAsInitData]);
end;

class procedure TExtendedHashFactory.Extended(constref AValue: Extended; AHashList: PUInt32);
var
  LMantissa: Float;
  LExponent: Integer;
begin
  Frexp(AValue, LMantissa, LExponent);

  if LMantissa = 0 then
    LMantissa := Abs(LMantissa);

  EXTENDED_HASH_FACTORY.GetHashList(@LMantissa, SizeOf(Math.Float), AHashList, []);
  EXTENDED_HASH_FACTORY.GetHashList(@LExponent, SizeOf(System.Integer), AHashList, [ghloHashListAsInitData]);
end;

{-----------------------------------------------------------------------------------------------------------------------
  GetHashCode for other number types
{----------------------------------------------------------------------------------------------------------------------}

class procedure TExtendedHashFactory.Currency(constref AValue: Currency; AHashList: PUInt32);
begin
  EXTENDED_HASH_FACTORY.GetHashList(@AValue, SizeOf(System.Int64), AHashList, []);
end;

class procedure TExtendedHashFactory.Comp(constref AValue: Comp; AHashList: PUInt32);
begin
  EXTENDED_HASH_FACTORY.GetHashList(@AValue, SizeOf(System.Int64), AHashList, []);
end;

{-----------------------------------------------------------------------------------------------------------------------
  GetHashCode for binary data (records etc) and dynamics arrays
{----------------------------------------------------------------------------------------------------------------------}

class procedure TExtendedHashFactory.Binary(constref AValue; AHashList: PUInt32);
var
  _self: TComparerService.PSpoofInterfacedTypeSizeObject absolute Self;
begin
  EXTENDED_HASH_FACTORY.GetHashList(@AValue, _self.Size, AHashList, []);
end;

class procedure TExtendedHashFactory.DynArray(constref AValue: Pointer; AHashList: PUInt32);
var
  _self: TComparerService.PSpoofInterfacedTypeSizeObject absolute Self;
begin
  EXTENDED_HASH_FACTORY.GetHashList(AValue, DynArraySize(AValue) * _self.Size, AHashList, []);
end;

{-----------------------------------------------------------------------------------------------------------------------
  GetHashCode for classes
{----------------------------------------------------------------------------------------------------------------------}

class procedure TExtendedHashFactory.&Class(constref AValue: TObject; AHashList: PUInt32);
var
  LValue: PtrInt;
begin
  if AValue = nil then
  begin
    LValue := $2A;
    EXTENDED_HASH_FACTORY.GetHashList(@LValue, SizeOf(LValue), AHashList, []);
    Exit;
  end;

  LValue := AValue.GetHashCode;
  EXTENDED_HASH_FACTORY.GetHashList(@LValue, SizeOf(LValue), AHashList, []);
end;

{-----------------------------------------------------------------------------------------------------------------------
  GetHashCode for string types
{----------------------------------------------------------------------------------------------------------------------}

class procedure TExtendedHashFactory.ShortString1(constref AValue: ShortString1; AHashList: PUInt32);
begin
  EXTENDED_HASH_FACTORY.GetHashList(@AValue[1], Length(AValue), AHashList, []);
end;

class procedure TExtendedHashFactory.ShortString2(constref AValue: ShortString2; AHashList: PUInt32);
begin
  EXTENDED_HASH_FACTORY.GetHashList(@AValue[1], Length(AValue), AHashList, []);
end;

class procedure TExtendedHashFactory.ShortString3(constref AValue: ShortString3; AHashList: PUInt32);
begin
  EXTENDED_HASH_FACTORY.GetHashList(@AValue[1], Length(AValue), AHashList, []);
end;

class procedure TExtendedHashFactory.ShortString(constref AValue: ShortString; AHashList: PUInt32);
begin
  EXTENDED_HASH_FACTORY.GetHashList(@AValue[1], Length(AValue), AHashList, []);
end;

class procedure TExtendedHashFactory.AnsiString(constref AValue: AnsiString; AHashList: PUInt32);
begin
  EXTENDED_HASH_FACTORY.GetHashList(@AValue[1], Length(AValue) * SizeOf(System.AnsiChar), AHashList, []);
end;

class procedure TExtendedHashFactory.WideString(constref AValue: WideString; AHashList: PUInt32);
begin
  EXTENDED_HASH_FACTORY.GetHashList(@AValue[1], Length(AValue) * SizeOf(System.WideChar), AHashList, []);
end;

class procedure TExtendedHashFactory.UnicodeString(constref AValue: UnicodeString; AHashList: PUInt32);
begin
  EXTENDED_HASH_FACTORY.GetHashList(@AValue[1], Length(AValue) * SizeOf(System.UnicodeChar), AHashList, []);
end;

{-----------------------------------------------------------------------------------------------------------------------
  GetHashCode for Delegates
{----------------------------------------------------------------------------------------------------------------------}

class procedure TExtendedHashFactory.Method(constref AValue: TMethod; AHashList: PUInt32);
begin
  EXTENDED_HASH_FACTORY.GetHashList(@AValue, SizeOf(System.TMethod), AHashList, []);
end;

{-----------------------------------------------------------------------------------------------------------------------
  GetHashCode for Variant
{----------------------------------------------------------------------------------------------------------------------}

class procedure TExtendedHashFactory.Variant(constref AValue: PVariant; AHashList: PUInt32);
begin
  try
    EXTENDED_HASH_FACTORY.UnicodeString(AValue^, AHashList);
  except
    EXTENDED_HASH_FACTORY.GetHashList(AValue, SizeOf(System.Variant), AHashList, []);
  end;
end;

{-----------------------------------------------------------------------------------------------------------------------
  GetHashCode for Pointer
{----------------------------------------------------------------------------------------------------------------------}

class procedure TExtendedHashFactory.Pointer(constref AValue: Pointer; AHashList: PUInt32);
begin
  EXTENDED_HASH_FACTORY.GetHashList(@AValue, SizeOf(System.Pointer), AHashList, []);
end;

{ TComparerService }

class function TComparerService.CreateInterface(AVMT: Pointer; ASize: SizeInt): PSpoofInterfacedTypeSizeObject;
begin
    Result := New(PSpoofInterfacedTypeSizeObject);
    Result.VMT      := AVMT;
    Result.RefCount := 0;
    Result.Size     := ASize;
end;

class function TComparerService.SelectIntegerComparer(ATypeData: PTypeData; ASize: SizeInt): Pointer;
begin
  case ATypeData.OrdType of
    otSByte:
      Exit(@Comparer_Int8_Instance);
    otUByte:
      Exit(@Comparer_UInt8_Instance);
    otSWord:
      Exit(@Comparer_Int16_Instance);
    otUWord:
      Exit(@Comparer_UInt16_Instance);
    otSLong:
      Exit(@Comparer_Int32_Instance);
    otULong:
      Exit(@Comparer_UInt32_Instance);
  else
    System.Error(reRangeError);
    Exit(nil);
  end;
end;

class function TComparerService.SelectInt64Comparer(ATypeData: PTypeData; ASize: SizeInt): Pointer;
begin
  if ATypeData.MaxInt64Value > ATypeData.MinInt64Value then
    Exit(@Comparer_Int64_Instance)
  else
    Exit(@Comparer_UInt64_Instance);
end;

class function TComparerService.SelectFloatComparer(ATypeData: PTypeData;
  ASize: SizeInt): Pointer;
begin
  case ATypeData.FloatType of
    ftSingle:
      Exit(@Comparer_Single_Instance);
    ftDouble:
      Exit(@Comparer_Double_Instance);
    ftExtended:
      Exit(@Comparer_Extended_Instance);
    ftComp:
      Exit(@Comparer_Comp_Instance);
    ftCurr:
      Exit(@Comparer_Currency_Instance);
  else
    System.Error(reRangeError);
    Exit(nil);
  end;
end;

class function TComparerService.SelectShortStringComparer(ATypeData: PTypeData;
  ASize: SizeInt): Pointer;
begin
  case ASize of
    2: Exit(@Comparer_ShortString1_Instance);
    3: Exit(@Comparer_ShortString2_Instance);
    4: Exit(@Comparer_ShortString3_Instance);
  else
    Exit(@Comparer_ShortString_Instance);
  end;
end;

class function TComparerService.SelectBinaryComparer(ATypeData: PTypeData;
  ASize: SizeInt): Pointer;
begin
  case ASize of
    1: Exit(@Comparer_UInt8_Instance);
    2: Exit(@Comparer_UInt16_Instance);
    4: Exit(@Comparer_UInt32_Instance);
{$IFDEF CPU64}
    8: Exit(@Comparer_UInt64_Instance)
{$ENDIF}
  else
    Result := CreateInterface(@Comparer_Binary_VMT, ASize);
  end;
end;

class function TComparerService.SelectDynArrayComparer(ATypeData: PTypeData; ASize: SizeInt): Pointer;
begin
  Result := CreateInterface(@Comparer_DynArray_VMT, ATypeData.elSize);
end;

class function TComparerService.LookupComparer(ATypeInfo: PTypeInfo; ASize: SizeInt): Pointer;
var
  LInstance: PInstance;
begin
  if ATypeInfo = nil then
    Exit(SelectBinaryComparer(GetTypeData(ATypeInfo), ASize))
  else
  begin
    LInstance := @ComparerInstances[ATypeInfo.Kind];
    Result := LInstance.Instance;
    if LInstance.Selector then
      Result := TSelectFunc(Result)(GetTypeData(ATypeInfo), ASize);
  end;
end;

{ TComparerService.TInstance }

class function TComparerService.TInstance.Create(ASelector: Boolean;
  AInstance: Pointer): TComparerService.TInstance;
begin
  Result.Selector := ASelector;
  Result.Instance := AInstance;
end;

class function TComparerService.TInstance.CreateSelector(ASelectorInstance: CodePointer): TComparerService.TInstance;
begin
  Result.Selector := True;
  Result.SelectorInstance := ASelectorInstance;
end;

{ THashService }

class function THashService<T>.SelectIntegerEqualityComparer(ATypeData: PTypeData; ASize: SizeInt): Pointer;
begin
  case ATypeData.OrdType of
    otSByte:
      Exit(@FEqualityComparer_Int8_Instance);
    otUByte:
      Exit(@FEqualityComparer_UInt8_Instance);
    otSWord:
      Exit(@FEqualityComparer_Int16_Instance);
    otUWord:
      Exit(@FEqualityComparer_UInt16_Instance);
    otSLong:
      Exit(@FEqualityComparer_Int32_Instance);
    otULong:
      Exit(@FEqualityComparer_UInt32_Instance);
  else
    System.Error(reRangeError);
    Exit(nil);
  end;
end;

class function THashService<T>.SelectFloatEqualityComparer(ATypeData: PTypeData;
  ASize: SizeInt): Pointer;
begin
  case ATypeData.FloatType of
    ftSingle:
      Exit(@FEqualityComparer_Single_Instance);
    ftDouble:
      Exit(@FEqualityComparer_Double_Instance);
    ftExtended:
      Exit(@FEqualityComparer_Extended_Instance);
    ftComp:
      Exit(@FEqualityComparer_Comp_Instance);
    ftCurr:
      Exit(@FEqualityComparer_Currency_Instance);
  else
    System.Error(reRangeError);
    Exit(nil);
  end;
end;

class function THashService<T>.SelectShortStringEqualityComparer(
  ATypeData: PTypeData; ASize: SizeInt): Pointer;
begin
  case ASize of
    2: Exit(@FEqualityComparer_ShortString1_Instance);
    3: Exit(@FEqualityComparer_ShortString2_Instance);
    4: Exit(@FEqualityComparer_ShortString3_Instance);
  else
    Exit(@FEqualityComparer_ShortString_Instance);
  end
end;

class function THashService<T>.SelectBinaryEqualityComparer(ATypeData: PTypeData;
  ASize: SizeInt): Pointer;
begin
  case ASize of
    1: Exit(@FEqualityComparer_UInt8_Instance);
    2: Exit(@FEqualityComparer_UInt16_Instance);
    4: Exit(@FEqualityComparer_UInt32_Instance);
{$IFDEF CPU64}
    8: Exit(@FEqualityComparer_UInt64_Instance)
{$ENDIF}
  else
    Result := CreateInterface(@FEqualityComparer_Binary_VMT, ASize);
  end;
end;

class function THashService<T>.SelectDynArrayEqualityComparer(
  ATypeData: PTypeData; ASize: SizeInt): Pointer;
begin
  Result := CreateInterface(@FEqualityComparer_DynArray_VMT, ATypeData.elSize);
end;

class function THashService<T>.LookupEqualityComparer(ATypeInfo: PTypeInfo;
  ASize: SizeInt): Pointer;
var
  LInstance: PInstance;
  LSelectMethod: TSelectMethod;
begin
  if ATypeInfo = nil then
    Exit(SelectBinaryEqualityComparer(GetTypeData(ATypeInfo), ASize))
  else
  begin
    LInstance := @FEqualityComparerInstances[ATypeInfo.Kind];
    Result := LInstance.Instance;
    if LInstance.Selector then
    begin
      TMethod(LSelectMethod).Code := LInstance.SelectorInstance;
      TMethod(LSelectMethod).Data := Self;
      Result := LSelectMethod(GetTypeData(ATypeInfo), ASize);
    end;
  end;
end;

class constructor THashService<T>.Create;
begin
  FEqualityComparer_Int8_VMT          := EqualityComparer_Int8_VMT         ;
  FEqualityComparer_Int16_VMT         := EqualityComparer_Int16_VMT        ;
  FEqualityComparer_Int32_VMT         := EqualityComparer_Int32_VMT        ;
  FEqualityComparer_Int64_VMT         := EqualityComparer_Int64_VMT        ;
  FEqualityComparer_UInt8_VMT         := EqualityComparer_UInt8_VMT        ;
  FEqualityComparer_UInt16_VMT        := EqualityComparer_UInt16_VMT       ;
  FEqualityComparer_UInt32_VMT        := EqualityComparer_UInt32_VMT       ;
  FEqualityComparer_UInt64_VMT        := EqualityComparer_UInt64_VMT       ;
  FEqualityComparer_Single_VMT        := EqualityComparer_Single_VMT       ;
  FEqualityComparer_Double_VMT        := EqualityComparer_Double_VMT       ;
  FEqualityComparer_Extended_VMT      := EqualityComparer_Extended_VMT     ;
  FEqualityComparer_Currency_VMT      := EqualityComparer_Currency_VMT     ;
  FEqualityComparer_Comp_VMT          := EqualityComparer_Comp_VMT         ;
  FEqualityComparer_Binary_VMT        := EqualityComparer_Binary_VMT       ;
  FEqualityComparer_DynArray_VMT      := EqualityComparer_DynArray_VMT     ;
  FEqualityComparer_Class_VMT         := EqualityComparer_Class_VMT        ;
  FEqualityComparer_ShortString1_VMT  := EqualityComparer_ShortString1_VMT ;
  FEqualityComparer_ShortString2_VMT  := EqualityComparer_ShortString2_VMT ;
  FEqualityComparer_ShortString3_VMT  := EqualityComparer_ShortString3_VMT ;
  FEqualityComparer_ShortString_VMT   := EqualityComparer_ShortString_VMT  ;
  FEqualityComparer_AnsiString_VMT    := EqualityComparer_AnsiString_VMT   ;
  FEqualityComparer_WideString_VMT    := EqualityComparer_WideString_VMT   ;
  FEqualityComparer_UnicodeString_VMT := EqualityComparer_UnicodeString_VMT;
  FEqualityComparer_Method_VMT        := EqualityComparer_Method_VMT       ;
  FEqualityComparer_Variant_VMT       := EqualityComparer_Variant_VMT      ;
  FEqualityComparer_Pointer_VMT       := EqualityComparer_Pointer_VMT      ;

  /////
  FEqualityComparer_Int8_VMT.__ClassRef          := THashFactoryClass(T.ClassType);
  FEqualityComparer_Int16_VMT.__ClassRef         := THashFactoryClass(T.ClassType);
  FEqualityComparer_Int32_VMT.__ClassRef         := THashFactoryClass(T.ClassType);
  FEqualityComparer_Int64_VMT.__ClassRef         := THashFactoryClass(T.ClassType);
  FEqualityComparer_UInt8_VMT.__ClassRef         := THashFactoryClass(T.ClassType);
  FEqualityComparer_UInt16_VMT.__ClassRef        := THashFactoryClass(T.ClassType);
  FEqualityComparer_UInt32_VMT.__ClassRef        := THashFactoryClass(T.ClassType);
  FEqualityComparer_UInt64_VMT.__ClassRef        := THashFactoryClass(T.ClassType);
  FEqualityComparer_Single_VMT.__ClassRef        := THashFactoryClass(T.ClassType);
  FEqualityComparer_Double_VMT.__ClassRef        := THashFactoryClass(T.ClassType);
  FEqualityComparer_Extended_VMT.__ClassRef      := THashFactoryClass(T.ClassType);
  FEqualityComparer_Currency_VMT.__ClassRef      := THashFactoryClass(T.ClassType);
  FEqualityComparer_Comp_VMT.__ClassRef          := THashFactoryClass(T.ClassType);
  FEqualityComparer_Binary_VMT.__ClassRef        := THashFactoryClass(T.ClassType);
  FEqualityComparer_DynArray_VMT.__ClassRef      := THashFactoryClass(T.ClassType);
  FEqualityComparer_Class_VMT.__ClassRef         := THashFactoryClass(T.ClassType);
  FEqualityComparer_ShortString1_VMT.__ClassRef  := THashFactoryClass(T.ClassType);
  FEqualityComparer_ShortString2_VMT.__ClassRef  := THashFactoryClass(T.ClassType);
  FEqualityComparer_ShortString3_VMT.__ClassRef  := THashFactoryClass(T.ClassType);
  FEqualityComparer_ShortString_VMT.__ClassRef   := THashFactoryClass(T.ClassType);
  FEqualityComparer_AnsiString_VMT.__ClassRef    := THashFactoryClass(T.ClassType);
  FEqualityComparer_WideString_VMT.__ClassRef    := THashFactoryClass(T.ClassType);
  FEqualityComparer_UnicodeString_VMT.__ClassRef := THashFactoryClass(T.ClassType);
  FEqualityComparer_Method_VMT.__ClassRef        := THashFactoryClass(T.ClassType);
  FEqualityComparer_Variant_VMT.__ClassRef       := THashFactoryClass(T.ClassType);
  FEqualityComparer_Pointer_VMT.__ClassRef       := THashFactoryClass(T.ClassType);

  ///////
  FEqualityComparer_Int8_Instance          := @FEqualityComparer_Int8_VMT         ;
  FEqualityComparer_Int16_Instance         := @FEqualityComparer_Int16_VMT        ;
  FEqualityComparer_Int32_Instance         := @FEqualityComparer_Int32_VMT        ;
  FEqualityComparer_Int64_Instance         := @FEqualityComparer_Int64_VMT        ;
  FEqualityComparer_UInt8_Instance         := @FEqualityComparer_UInt8_VMT        ;
  FEqualityComparer_UInt16_Instance        := @FEqualityComparer_UInt16_VMT       ;
  FEqualityComparer_UInt32_Instance        := @FEqualityComparer_UInt32_VMT       ;
  FEqualityComparer_UInt64_Instance        := @FEqualityComparer_UInt64_VMT       ;
  FEqualityComparer_Single_Instance        := @FEqualityComparer_Single_VMT       ;
  FEqualityComparer_Double_Instance        := @FEqualityComparer_Double_VMT       ;
  FEqualityComparer_Extended_Instance      := @FEqualityComparer_Extended_VMT     ;
  FEqualityComparer_Currency_Instance      := @FEqualityComparer_Currency_VMT     ;
  FEqualityComparer_Comp_Instance          := @FEqualityComparer_Comp_VMT         ;
  //FEqualityComparer_Binary_Instance        := @FEqualityComparer_Binary_VMT       ;  // dynamic instance
  //FEqualityComparer_DynArray_Instance      := @FEqualityComparer_DynArray_VMT     ;  // dynamic instance
  FEqualityComparer_ShortString1_Instance  := @FEqualityComparer_ShortString1_VMT ;
  FEqualityComparer_ShortString2_Instance  := @FEqualityComparer_ShortString2_VMT ;
  FEqualityComparer_ShortString3_Instance  := @FEqualityComparer_ShortString3_VMT ;
  FEqualityComparer_ShortString_Instance   := @FEqualityComparer_ShortString_VMT  ;
  FEqualityComparer_AnsiString_Instance    := @FEqualityComparer_AnsiString_VMT   ;
  FEqualityComparer_WideString_Instance    := @FEqualityComparer_WideString_VMT   ;
  FEqualityComparer_UnicodeString_Instance := @FEqualityComparer_UnicodeString_VMT;
  FEqualityComparer_Method_Instance        := @FEqualityComparer_Method_VMT       ;
  FEqualityComparer_Variant_Instance       := @FEqualityComparer_Variant_VMT      ;
  FEqualityComparer_Pointer_Instance       := @FEqualityComparer_Pointer_VMT      ;

  //////
  FEqualityComparerInstances[tkUnknown]      := TInstance.CreateSelector(TMethod(TSelectMethod(THashService<T>.SelectBinaryEqualityComparer)).Code);
  FEqualityComparerInstances[tkInteger]      := TInstance.CreateSelector(TMethod(TSelectMethod(THashService<T>.SelectIntegerEqualityComparer)).Code);
  FEqualityComparerInstances[tkChar]         := TInstance.Create(False, @FEqualityComparer_UInt8_Instance);
  FEqualityComparerInstances[tkEnumeration]  := TInstance.CreateSelector(TMethod(TSelectMethod(THashService<T>.SelectIntegerEqualityComparer)).Code);
  FEqualityComparerInstances[tkFloat]        := TInstance.CreateSelector(TMethod(TSelectMethod(THashService<T>.SelectFloatEqualityComparer)).Code);
  FEqualityComparerInstances[tkSet]          := TInstance.CreateSelector(TMethod(TSelectMethod(THashService<T>.SelectBinaryEqualityComparer)).Code);
  FEqualityComparerInstances[tkMethod]       := TInstance.Create(False, @FEqualityComparer_Method_Instance);
  FEqualityComparerInstances[tkSString]      := TInstance.CreateSelector(TMethod(TSelectMethod(THashService<T>.SelectShortStringEqualityComparer)).Code);
  FEqualityComparerInstances[tkLString]      := TInstance.Create(False, @FEqualityComparer_AnsiString_Instance);
  FEqualityComparerInstances[tkAString]      := TInstance.Create(False, @FEqualityComparer_AnsiString_Instance);
  FEqualityComparerInstances[tkWString]      := TInstance.Create(False, @FEqualityComparer_WideString_Instance);
  FEqualityComparerInstances[tkVariant]      := TInstance.Create(False, @FEqualityComparer_Variant_Instance);
  FEqualityComparerInstances[tkArray]        := TInstance.CreateSelector(TMethod(TSelectMethod(THashService<T>.SelectBinaryEqualityComparer)).Code);
  FEqualityComparerInstances[tkRecord]       := TInstance.CreateSelector(TMethod(TSelectMethod(THashService<T>.SelectBinaryEqualityComparer)).Code);
  FEqualityComparerInstances[tkInterface]    := TInstance.Create(False, @FEqualityComparer_Pointer_Instance);
  FEqualityComparerInstances[tkClass]        := TInstance.Create(False, @FEqualityComparer_Pointer_Instance);
  FEqualityComparerInstances[tkObject]       := TInstance.CreateSelector(TMethod(TSelectMethod(THashService<T>.SelectBinaryEqualityComparer)).Code);
  FEqualityComparerInstances[tkWChar]        := TInstance.Create(False, @FEqualityComparer_UInt16_Instance);
  FEqualityComparerInstances[tkBool]         := TInstance.CreateSelector(TMethod(TSelectMethod(THashService<T>.SelectIntegerEqualityComparer)).Code);
  FEqualityComparerInstances[tkInt64]        := TInstance.Create(False, @FEqualityComparer_Int64_Instance);
  FEqualityComparerInstances[tkQWord]        := TInstance.Create(False, @FEqualityComparer_UInt64_Instance);
  FEqualityComparerInstances[tkDynArray]     := TInstance.CreateSelector(TMethod(TSelectMethod(THashService<T>.SelectDynArrayEqualityComparer)).Code);
  FEqualityComparerInstances[tkInterfaceRaw] := TInstance.Create(False, @FEqualityComparer_Pointer_Instance);
  FEqualityComparerInstances[tkProcVar]      := TInstance.Create(False, @FEqualityComparer_Pointer_Instance);
  FEqualityComparerInstances[tkUString]      := TInstance.Create(False, @FEqualityComparer_UnicodeString_Instance);
  FEqualityComparerInstances[tkUChar]        := TInstance.Create(False, @FEqualityComparer_UInt16_Instance);
  FEqualityComparerInstances[tkHelper]       := TInstance.Create(False, @FEqualityComparer_Pointer_Instance);
  FEqualityComparerInstances[tkFile]         := TInstance.CreateSelector(TMethod(TSelectMethod(THashService<T>.SelectBinaryEqualityComparer)).Code);
  FEqualityComparerInstances[tkClassRef]     := TInstance.Create(False, @FEqualityComparer_Pointer_Instance);
  FEqualityComparerInstances[tkPointer]      := TInstance.Create(False, @FEqualityComparer_Pointer_Instance)
end;

{ TExtendedHashService }

class function TExtendedHashService<T>.SelectIntegerEqualityComparer(ATypeData: PTypeData; ASize: SizeInt): Pointer;
begin
  case ATypeData.OrdType of
    otSByte:
      Exit(@FExtendedEqualityComparer_Int8_Instance);
    otUByte:
      Exit(@FExtendedEqualityComparer_UInt8_Instance);
    otSWord:
      Exit(@FExtendedEqualityComparer_Int16_Instance);
    otUWord:
      Exit(@FExtendedEqualityComparer_UInt16_Instance);
    otSLong:
      Exit(@FExtendedEqualityComparer_Int32_Instance);
    otULong:
      Exit(@FExtendedEqualityComparer_UInt32_Instance);
  else
    System.Error(reRangeError);
    Exit(nil);
  end;
end;

class function TExtendedHashService<T>.SelectFloatEqualityComparer(ATypeData: PTypeData; ASize: SizeInt): Pointer;
begin
  case ATypeData.FloatType of
    ftSingle:
      Exit(@FExtendedEqualityComparer_Single_Instance);
    ftDouble:
      Exit(@FExtendedEqualityComparer_Double_Instance);
    ftExtended:
      Exit(@FExtendedEqualityComparer_Extended_Instance);
    ftComp:
      Exit(@FExtendedEqualityComparer_Comp_Instance);
    ftCurr:
      Exit(@FExtendedEqualityComparer_Currency_Instance);
  else
    System.Error(reRangeError);
    Exit(nil);
  end;
end;

class function TExtendedHashService<T>.SelectShortStringEqualityComparer(ATypeData: PTypeData;
  ASize: SizeInt): Pointer;
begin
  case ASize of
    2: Exit(@FExtendedEqualityComparer_ShortString1_Instance);
    3: Exit(@FExtendedEqualityComparer_ShortString2_Instance);
    4: Exit(@FExtendedEqualityComparer_ShortString3_Instance);
  else
    Exit(@FExtendedEqualityComparer_ShortString_Instance);
  end
end;

class function TExtendedHashService<T>.SelectBinaryEqualityComparer(ATypeData: PTypeData;
  ASize: SizeInt): Pointer;
begin
  case ASize of
    1: Exit(@FExtendedEqualityComparer_UInt8_Instance);
    2: Exit(@FExtendedEqualityComparer_UInt16_Instance);
    4: Exit(@FExtendedEqualityComparer_UInt32_Instance);
{$IFDEF CPU64}
    8: Exit(@FExtendedEqualityComparer_UInt64_Instance)
{$ENDIF}
  else
    Result := CreateInterface(@FExtendedEqualityComparer_Binary_VMT, ASize);
  end;
end;

class function TExtendedHashService<T>.SelectDynArrayEqualityComparer(
  ATypeData: PTypeData; ASize: SizeInt): Pointer;
begin
  Result := CreateInterface(@FExtendedEqualityComparer_DynArray_VMT, ATypeData.elSize);
end;

class function TExtendedHashService<T>.LookupExtendedEqualityComparer(
  ATypeInfo: PTypeInfo; ASize: SizeInt): Pointer;
var
  LInstance: PInstance;
  LSelectMethod: TSelectMethod;
begin
  if ATypeInfo = nil then
    Exit(SelectBinaryEqualityComparer(GetTypeData(ATypeInfo), ASize))
  else
  begin
    LInstance := @FExtendedEqualityComparerInstances[ATypeInfo.Kind];
    Result := LInstance.Instance;
    if LInstance.Selector then
    begin
      TMethod(LSelectMethod).Code := LInstance.SelectorInstance;
      TMethod(LSelectMethod).Data := Self;
      Result := LSelectMethod(GetTypeData(ATypeInfo), ASize);
    end;
  end;
end;

class constructor TExtendedHashService<T>.Create;
begin
  FExtendedEqualityComparer_Int8_VMT          := ExtendedEqualityComparer_Int8_VMT         ;
  FExtendedEqualityComparer_Int16_VMT         := ExtendedEqualityComparer_Int16_VMT        ;
  FExtendedEqualityComparer_Int32_VMT         := ExtendedEqualityComparer_Int32_VMT        ;
  FExtendedEqualityComparer_Int64_VMT         := ExtendedEqualityComparer_Int64_VMT        ;
  FExtendedEqualityComparer_UInt8_VMT         := ExtendedEqualityComparer_UInt8_VMT        ;
  FExtendedEqualityComparer_UInt16_VMT        := ExtendedEqualityComparer_UInt16_VMT       ;
  FExtendedEqualityComparer_UInt32_VMT        := ExtendedEqualityComparer_UInt32_VMT       ;
  FExtendedEqualityComparer_UInt64_VMT        := ExtendedEqualityComparer_UInt64_VMT       ;
  FExtendedEqualityComparer_Single_VMT        := ExtendedEqualityComparer_Single_VMT       ;
  FExtendedEqualityComparer_Double_VMT        := ExtendedEqualityComparer_Double_VMT       ;
  FExtendedEqualityComparer_Extended_VMT      := ExtendedEqualityComparer_Extended_VMT     ;
  FExtendedEqualityComparer_Currency_VMT      := ExtendedEqualityComparer_Currency_VMT     ;
  FExtendedEqualityComparer_Comp_VMT          := ExtendedEqualityComparer_Comp_VMT         ;
  FExtendedEqualityComparer_Binary_VMT        := ExtendedEqualityComparer_Binary_VMT       ;
  FExtendedEqualityComparer_DynArray_VMT      := ExtendedEqualityComparer_DynArray_VMT     ;
  FExtendedEqualityComparer_Class_VMT         := ExtendedEqualityComparer_Class_VMT        ;
  FExtendedEqualityComparer_ShortString1_VMT  := ExtendedEqualityComparer_ShortString1_VMT ;
  FExtendedEqualityComparer_ShortString2_VMT  := ExtendedEqualityComparer_ShortString2_VMT ;
  FExtendedEqualityComparer_ShortString3_VMT  := ExtendedEqualityComparer_ShortString3_VMT ;
  FExtendedEqualityComparer_ShortString_VMT   := ExtendedEqualityComparer_ShortString_VMT  ;
  FExtendedEqualityComparer_AnsiString_VMT    := ExtendedEqualityComparer_AnsiString_VMT   ;
  FExtendedEqualityComparer_WideString_VMT    := ExtendedEqualityComparer_WideString_VMT   ;
  FExtendedEqualityComparer_UnicodeString_VMT := ExtendedEqualityComparer_UnicodeString_VMT;
  FExtendedEqualityComparer_Method_VMT        := ExtendedEqualityComparer_Method_VMT       ;
  FExtendedEqualityComparer_Variant_VMT       := ExtendedEqualityComparer_Variant_VMT      ;
  FExtendedEqualityComparer_Pointer_VMT       := ExtendedEqualityComparer_Pointer_VMT      ;

  /////
  FExtendedEqualityComparer_Int8_VMT.__ClassRef          := TExtendedHashFactoryClass(T.ClassType);
  FExtendedEqualityComparer_Int16_VMT.__ClassRef         := TExtendedHashFactoryClass(T.ClassType);
  FExtendedEqualityComparer_Int32_VMT.__ClassRef         := TExtendedHashFactoryClass(T.ClassType);
  FExtendedEqualityComparer_Int64_VMT.__ClassRef         := TExtendedHashFactoryClass(T.ClassType);
  FExtendedEqualityComparer_UInt8_VMT.__ClassRef         := TExtendedHashFactoryClass(T.ClassType);
  FExtendedEqualityComparer_UInt16_VMT.__ClassRef        := TExtendedHashFactoryClass(T.ClassType);
  FExtendedEqualityComparer_UInt32_VMT.__ClassRef        := TExtendedHashFactoryClass(T.ClassType);
  FExtendedEqualityComparer_UInt64_VMT.__ClassRef        := TExtendedHashFactoryClass(T.ClassType);
  FExtendedEqualityComparer_Single_VMT.__ClassRef        := TExtendedHashFactoryClass(T.ClassType);
  FExtendedEqualityComparer_Double_VMT.__ClassRef        := TExtendedHashFactoryClass(T.ClassType);
  FExtendedEqualityComparer_Extended_VMT.__ClassRef      := TExtendedHashFactoryClass(T.ClassType);
  FExtendedEqualityComparer_Currency_VMT.__ClassRef      := TExtendedHashFactoryClass(T.ClassType);
  FExtendedEqualityComparer_Comp_VMT.__ClassRef          := TExtendedHashFactoryClass(T.ClassType);
  FExtendedEqualityComparer_Binary_VMT.__ClassRef        := TExtendedHashFactoryClass(T.ClassType);
  FExtendedEqualityComparer_DynArray_VMT.__ClassRef      := TExtendedHashFactoryClass(T.ClassType);
  FExtendedEqualityComparer_Class_VMT.__ClassRef         := TExtendedHashFactoryClass(T.ClassType);
  FExtendedEqualityComparer_ShortString1_VMT.__ClassRef  := TExtendedHashFactoryClass(T.ClassType);
  FExtendedEqualityComparer_ShortString2_VMT.__ClassRef  := TExtendedHashFactoryClass(T.ClassType);
  FExtendedEqualityComparer_ShortString3_VMT.__ClassRef  := TExtendedHashFactoryClass(T.ClassType);
  FExtendedEqualityComparer_ShortString_VMT.__ClassRef   := TExtendedHashFactoryClass(T.ClassType);
  FExtendedEqualityComparer_AnsiString_VMT.__ClassRef    := TExtendedHashFactoryClass(T.ClassType);
  FExtendedEqualityComparer_WideString_VMT.__ClassRef    := TExtendedHashFactoryClass(T.ClassType);
  FExtendedEqualityComparer_UnicodeString_VMT.__ClassRef := TExtendedHashFactoryClass(T.ClassType);
  FExtendedEqualityComparer_Method_VMT.__ClassRef        := TExtendedHashFactoryClass(T.ClassType);
  FExtendedEqualityComparer_Variant_VMT.__ClassRef       := TExtendedHashFactoryClass(T.ClassType);
  FExtendedEqualityComparer_Pointer_VMT.__ClassRef       := TExtendedHashFactoryClass(T.ClassType);

  ///////
  FExtendedEqualityComparer_Int8_Instance          := @FExtendedEqualityComparer_Int8_VMT         ;
  FExtendedEqualityComparer_Int16_Instance         := @FExtendedEqualityComparer_Int16_VMT        ;
  FExtendedEqualityComparer_Int32_Instance         := @FExtendedEqualityComparer_Int32_VMT        ;
  FExtendedEqualityComparer_Int64_Instance         := @FExtendedEqualityComparer_Int64_VMT        ;
  FExtendedEqualityComparer_UInt8_Instance         := @FExtendedEqualityComparer_UInt8_VMT        ;
  FExtendedEqualityComparer_UInt16_Instance        := @FExtendedEqualityComparer_UInt16_VMT       ;
  FExtendedEqualityComparer_UInt32_Instance        := @FExtendedEqualityComparer_UInt32_VMT       ;
  FExtendedEqualityComparer_UInt64_Instance        := @FExtendedEqualityComparer_UInt64_VMT       ;
  FExtendedEqualityComparer_Single_Instance        := @FExtendedEqualityComparer_Single_VMT       ;
  FExtendedEqualityComparer_Double_Instance        := @FExtendedEqualityComparer_Double_VMT       ;
  FExtendedEqualityComparer_Extended_Instance      := @FExtendedEqualityComparer_Extended_VMT     ;
  FExtendedEqualityComparer_Currency_Instance      := @FExtendedEqualityComparer_Currency_VMT     ;
  FExtendedEqualityComparer_Comp_Instance          := @FExtendedEqualityComparer_Comp_VMT         ;
  //FExtendedEqualityComparer_Binary_Instance        := @FExtendedEqualityComparer_Binary_VMT       ;  // dynamic instance
  //FExtendedEqualityComparer_DynArray_Instance      := @FExtendedEqualityComparer_DynArray_VMT     ;  // dynamic instance
  FExtendedEqualityComparer_ShortString1_Instance  := @FExtendedEqualityComparer_ShortString1_VMT ;
  FExtendedEqualityComparer_ShortString2_Instance  := @FExtendedEqualityComparer_ShortString2_VMT ;
  FExtendedEqualityComparer_ShortString3_Instance  := @FExtendedEqualityComparer_ShortString3_VMT ;
  FExtendedEqualityComparer_ShortString_Instance   := @FExtendedEqualityComparer_ShortString_VMT  ;
  FExtendedEqualityComparer_AnsiString_Instance    := @FExtendedEqualityComparer_AnsiString_VMT   ;
  FExtendedEqualityComparer_WideString_Instance    := @FExtendedEqualityComparer_WideString_VMT   ;
  FExtendedEqualityComparer_UnicodeString_Instance := @FExtendedEqualityComparer_UnicodeString_VMT;
  FExtendedEqualityComparer_Method_Instance        := @FExtendedEqualityComparer_Method_VMT       ;
  FExtendedEqualityComparer_Variant_Instance       := @FExtendedEqualityComparer_Variant_VMT      ;
  FExtendedEqualityComparer_Pointer_Instance       := @FExtendedEqualityComparer_Pointer_VMT      ;

  //////
  FExtendedEqualityComparerInstances[tkUnknown]      := TInstance.CreateSelector(TMethod(TSelectMethod(TExtendedHashService<T>.SelectBinaryEqualityComparer)).Code);
  FExtendedEqualityComparerInstances[tkInteger]      := TInstance.CreateSelector(TMethod(TSelectMethod(TExtendedHashService<T>.SelectIntegerEqualityComparer)).Code);
  FExtendedEqualityComparerInstances[tkChar]         := TInstance.Create(False, @FExtendedEqualityComparer_UInt8_Instance);
  FExtendedEqualityComparerInstances[tkEnumeration]  := TInstance.CreateSelector(TMethod(TSelectMethod(TExtendedHashService<T>.SelectIntegerEqualityComparer)).Code);
  FExtendedEqualityComparerInstances[tkFloat]        := TInstance.CreateSelector(TMethod(TSelectMethod(TExtendedHashService<T>.SelectFloatEqualityComparer)).Code);
  FExtendedEqualityComparerInstances[tkSet]          := TInstance.CreateSelector(TMethod(TSelectMethod(TExtendedHashService<T>.SelectBinaryEqualityComparer)).Code);
  FExtendedEqualityComparerInstances[tkMethod]       := TInstance.Create(False, @FExtendedEqualityComparer_Method_Instance);
  FExtendedEqualityComparerInstances[tkSString]      := TInstance.CreateSelector(TMethod(TSelectMethod(TExtendedHashService<T>.SelectShortStringEqualityComparer)).Code);
  FExtendedEqualityComparerInstances[tkLString]      := TInstance.Create(False, @FExtendedEqualityComparer_AnsiString_Instance);
  FExtendedEqualityComparerInstances[tkAString]      := TInstance.Create(False, @FExtendedEqualityComparer_AnsiString_Instance);
  FExtendedEqualityComparerInstances[tkWString]      := TInstance.Create(False, @FExtendedEqualityComparer_WideString_Instance);
  FExtendedEqualityComparerInstances[tkVariant]      := TInstance.Create(False, @FExtendedEqualityComparer_Variant_Instance);
  FExtendedEqualityComparerInstances[tkArray]        := TInstance.CreateSelector(TMethod(TSelectMethod(TExtendedHashService<T>.SelectBinaryEqualityComparer)).Code);
  FExtendedEqualityComparerInstances[tkRecord]       := TInstance.CreateSelector(TMethod(TSelectMethod(TExtendedHashService<T>.SelectBinaryEqualityComparer)).Code);
  FExtendedEqualityComparerInstances[tkInterface]    := TInstance.Create(False, @FExtendedEqualityComparer_Pointer_Instance);
  FExtendedEqualityComparerInstances[tkClass]        := TInstance.Create(False, @FExtendedEqualityComparer_Pointer_Instance);
  FExtendedEqualityComparerInstances[tkObject]       := TInstance.CreateSelector(TMethod(TSelectMethod(TExtendedHashService<T>.SelectBinaryEqualityComparer)).Code);
  FExtendedEqualityComparerInstances[tkWChar]        := TInstance.Create(False, @FExtendedEqualityComparer_UInt16_Instance);
  FExtendedEqualityComparerInstances[tkBool]         := TInstance.CreateSelector(TMethod(TSelectMethod(TExtendedHashService<T>.SelectIntegerEqualityComparer)).Code);
  FExtendedEqualityComparerInstances[tkInt64]        := TInstance.Create(False, @FExtendedEqualityComparer_Int64_Instance);
  FExtendedEqualityComparerInstances[tkQWord]        := TInstance.Create(False, @FExtendedEqualityComparer_UInt64_Instance);
  FExtendedEqualityComparerInstances[tkDynArray]     := TInstance.CreateSelector(TMethod(TSelectMethod(TExtendedHashService<T>.SelectDynArrayEqualityComparer)).Code);
  FExtendedEqualityComparerInstances[tkInterfaceRaw] := TInstance.Create(False, @FExtendedEqualityComparer_Pointer_Instance);
  FExtendedEqualityComparerInstances[tkProcVar]      := TInstance.Create(False, @FExtendedEqualityComparer_Pointer_Instance);
  FExtendedEqualityComparerInstances[tkUString]      := TInstance.Create(False, @FExtendedEqualityComparer_UnicodeString_Instance);
  FExtendedEqualityComparerInstances[tkUChar]        := TInstance.Create(False, @FExtendedEqualityComparer_UInt16_Instance);
  FExtendedEqualityComparerInstances[tkHelper]       := TInstance.Create(False, @FExtendedEqualityComparer_Pointer_Instance);
  FExtendedEqualityComparerInstances[tkFile]         := TInstance.CreateSelector(TMethod(TSelectMethod(TExtendedHashService<T>.SelectBinaryEqualityComparer)).Code);
  FExtendedEqualityComparerInstances[tkClassRef]     := TInstance.Create(False, @FExtendedEqualityComparer_Pointer_Instance);
  FExtendedEqualityComparerInstances[tkPointer]      := TInstance.Create(False, @FExtendedEqualityComparer_Pointer_Instance);
end;

{ TEqualityComparer<T> }

class function TEqualityComparer<T>.Default: IEqualityComparer<T>;
begin
  Result := _LookupVtableInfo(giEqualityComparer, TypeInfo(T), SizeOf(T));
end;

class function TEqualityComparer<T>.Default(AHashFactoryClass: THashFactoryClass): IEqualityComparer<T>;
begin
  if AHashFactoryClass.InheritsFrom(TExtendedHashFactory) then
    Result := _LookupVtableInfoEx(giExtendedEqualityComparer, TypeInfo(T), SizeOf(T), AHashFactoryClass)
  else if AHashFactoryClass.InheritsFrom(THashFactory) then
    Result := _LookupVtableInfoEx(giEqualityComparer, TypeInfo(T), SizeOf(T), AHashFactoryClass);
end;

class function  TEqualityComparer<T>.Construct(const AEqualityComparison: TOnEqualityComparison<T>;
  const AHasher: TOnHasher<T>): IEqualityComparer<T>;
begin
  Result := TDelegatedEqualityComparerEvents<T>.Create(AEqualityComparison, AHasher);
end;

class function  TEqualityComparer<T>.Construct(const AEqualityComparison: TEqualityComparisonFunc<T>;
  const AHasher: THasherFunc<T>): IEqualityComparer<T>;
begin
  Result := TDelegatedEqualityComparerFunc<T>.Create(AEqualityComparison, AHasher);
end;

{ TDelegatedEqualityComparerEvents<T> }

function TDelegatedEqualityComparerEvents<T>.Equals(constref ALeft, ARight: T): Boolean;
begin
  Result := FEqualityComparison(ALeft, ARight);
end;

function TDelegatedEqualityComparerEvents<T>.GetHashCode(constref AValue: T): UInt32;
begin
  Result := FHasher(AValue);
end;

constructor TDelegatedEqualityComparerEvents<T>.Create(const AEqualityComparison: TOnEqualityComparison<T>;
  const AHasher: TOnHasher<T>);
begin
  FEqualityComparison := AEqualityComparison;
  FHasher := AHasher;
end;

{ TDelegatedEqualityComparerFunc<T> }

function TDelegatedEqualityComparerFunc<T>.Equals(constref ALeft, ARight: T): Boolean;
begin
  Result := FEqualityComparison(ALeft, ARight);
end;

function TDelegatedEqualityComparerFunc<T>.GetHashCode(constref AValue: T): UInt32;
begin
  Result := FHasher(AValue);
end;

constructor TDelegatedEqualityComparerFunc<T>.Create(const AEqualityComparison: TEqualityComparisonFunc<T>;
  const AHasher: THasherFunc<T>);
begin
  FEqualityComparison := AEqualityComparison;
  FHasher := AHasher;
end;

{ TDelegatedExtendedEqualityComparerEvents<T> }

function TDelegatedExtendedEqualityComparerEvents<T>.GetHashCodeMethod(constref AValue: T): UInt32;
var
  LHashList: array[0..1] of Int32;
  LHashListParams: array[0..3] of Int16 absolute LHashList;
begin
  LHashListParams[0] := -1;
  FExtendedHasher(AValue, @LHashList[0]);
  Result := LHashList[1];
end;

function TDelegatedExtendedEqualityComparerEvents<T>.Equals(constref ALeft, ARight: T): Boolean;
begin
  Result := FEqualityComparison(ALeft, ARight);
end;

function TDelegatedExtendedEqualityComparerEvents<T>.GetHashCode(constref AValue: T): UInt32;
begin
  Result := FHasher(AValue);
end;

procedure TDelegatedExtendedEqualityComparerEvents<T>.GetHashList(constref AValue: T; AHashList: PUInt32);
begin
  FExtendedHasher(AValue, AHashList);
end;

constructor TDelegatedExtendedEqualityComparerEvents<T>.Create(const AEqualityComparison: TOnEqualityComparison<T>;
  const AHasher: TOnHasher<T>; const AExtendedHasher: TOnExtendedHasher<T>);
begin
  FEqualityComparison := AEqualityComparison;
  FHasher := AHasher;
  FExtendedHasher := AExtendedHasher;
end;

constructor TDelegatedExtendedEqualityComparerEvents<T>.Create(const AEqualityComparison: TOnEqualityComparison<T>;
  const AExtendedHasher: TOnExtendedHasher<T>);
begin
  Create(AEqualityComparison, GetHashCodeMethod, AExtendedHasher);
end;

{ TDelegatedExtendedEqualityComparerFunc<T> }

function TDelegatedExtendedEqualityComparerFunc<T>.Equals(constref ALeft, ARight: T): Boolean;
begin
  Result := FEqualityComparison(ALeft, ARight);
end;

function TDelegatedExtendedEqualityComparerFunc<T>.GetHashCode(constref AValue: T): UInt32;
var
  LHashList: array[0..1] of Int32;
  LHashListParams: array[0..3] of Int16 absolute LHashList;
begin
  if not Assigned(FHasher) then
  begin
    LHashListParams[0] := -1;
    FExtendedHasher(AValue, @LHashList[0]);
    Result := LHashList[1];
  end
  else
    Result := FHasher(AValue);
end;

procedure TDelegatedExtendedEqualityComparerFunc<T>.GetHashList(constref AValue: T; AHashList: PUInt32);
begin
  FExtendedHasher(AValue, AHashList);
end;

constructor TDelegatedExtendedEqualityComparerFunc<T>.Create(const AEqualityComparison: TEqualityComparisonFunc<T>;
  const AHasher: THasherFunc<T>; const AExtendedHasher: TExtendedHasherFunc<T>);
begin
  FEqualityComparison := AEqualityComparison;
  FHasher := AHasher;
  FExtendedHasher := AExtendedHasher;
end;

constructor TDelegatedExtendedEqualityComparerFunc<T>.Create(const AEqualityComparison: TEqualityComparisonFunc<T>;
  const AExtendedHasher: TExtendedHasherFunc<T>);
begin
  Create(AEqualityComparison, nil, AExtendedHasher);
end;

{ TExtendedEqualityComparer<T> }

class function TExtendedEqualityComparer<T>.Default: IExtendedEqualityComparer<T>;
begin
  Result := _LookupVtableInfo(giExtendedEqualityComparer, TypeInfo(T), SizeOf(T));
end;

class function TExtendedEqualityComparer<T>.Default(
  AExtenedHashFactoryClass: TExtendedHashFactoryClass
  ): IExtendedEqualityComparer<T>;
begin
  Result := _LookupVtableInfoEx(giExtendedEqualityComparer, TypeInfo(T), SizeOf(T), AExtenedHashFactoryClass);
end;

class function TExtendedEqualityComparer<T>.Construct(
  const AEqualityComparison: TOnEqualityComparison<T>; const AHasher: TOnHasher<T>;
  const AExtendedHasher: TOnExtendedHasher<T>): IExtendedEqualityComparer<T>;
begin
  Result := TDelegatedExtendedEqualityComparerEvents<T>.Create(AEqualityComparison, AHasher, AExtendedHasher);
end;

class function TExtendedEqualityComparer<T>.Construct(
  const AEqualityComparison: TEqualityComparisonFunc<T>; const AHasher: THasherFunc<T>;
  const AExtendedHasher: TExtendedHasherFunc<T>): IExtendedEqualityComparer<T>;
begin
  Result := TDelegatedExtendedEqualityComparerFunc<T>.Create(AEqualityComparison, AHasher, AExtendedHasher);
end;

class function TExtendedEqualityComparer<T>.Construct(
  const AEqualityComparison: TOnEqualityComparison<T>;
  const AExtendedHasher: TOnExtendedHasher<T>): IExtendedEqualityComparer<T>;
begin
  Result := TDelegatedExtendedEqualityComparerEvents<T>.Create(AEqualityComparison, AExtendedHasher);
end;

class function TExtendedEqualityComparer<T>.Construct(
  const AEqualityComparison: TEqualityComparisonFunc<T>;
  const AExtendedHasher: TExtendedHasherFunc<T>): IExtendedEqualityComparer<T>;
begin
  Result := TDelegatedExtendedEqualityComparerFunc<T>.Create(AEqualityComparison, AExtendedHasher);
end;

{ TDelphiHashFactory }

class function TDelphiHashFactory.GetHashService: THashServiceClass;
begin
  Result := THashService<TDelphiHashFactory>;
end;

class function TDelphiHashFactory.GetHashCode(AKey: Pointer; ASize: SizeInt; AInitVal: UInt32): UInt32;
begin
  Result := DelphiHashLittle(AKey, ASize, AInitVal);
end;

{ TAdler32HashFactory }

class function TAdler32HashFactory.GetHashService: THashServiceClass;
begin
  Result := THashService<TAdler32HashFactory>;
end;

class function TAdler32HashFactory.GetHashCode(AKey: Pointer; ASize: SizeInt;
  AInitVal: UInt32): UInt32;
begin
  Result := Adler32(AKey, ASize);
end;

{ TSdbmHashFactory }

class function TSdbmHashFactory.GetHashService: THashServiceClass;
begin
  Result := THashService<TSdbmHashFactory>;
end;

class function TSdbmHashFactory.GetHashCode(AKey: Pointer; ASize: SizeInt;
  AInitVal: UInt32): UInt32;
begin
  Result := sdbm(AKey, ASize);
end;

{ TSimpleChecksumFactory }

class function TSimpleChecksumFactory.GetHashService: THashServiceClass;
begin
  Result := THashService<TSimpleChecksumFactory>;
end;

class function TSimpleChecksumFactory.GetHashCode(AKey: Pointer; ASize: SizeInt;
  AInitVal: UInt32): UInt32;
begin
  Result := SimpleChecksumHash(AKey, ASize);
end;

{ TDelphiDoubleHashFactory }

class function TDelphiDoubleHashFactory.GetHashService: THashServiceClass;
begin
  Result := TExtendedHashService<TDelphiDoubleHashFactory>;
end;

class function TDelphiDoubleHashFactory.GetHashCode(AKey: Pointer; ASize: SizeInt; AInitVal: UInt32): UInt32;
begin
  Result := DelphiHashLittle(AKey, ASize, AInitVal);
end;

class procedure TDelphiDoubleHashFactory.GetHashList(AKey: Pointer; ASize: SizeInt; AHashList: PUInt32;
        AOptions: TGetHashListOptions);
var
  LHash: UInt32;
  AHashListParams: PUInt16 absolute AHashList;
begin
{$WARNINGS OFF}
  case AHashListParams[0] of
    -2:
      begin
        if not (ghloHashListAsInitData in AOptions) then
          AHashList[1] := 0;
        LHash := 0;
        DelphiHashLittle2(AKey, ASize, LHash, AHashList[1]);
        Exit;
      end;
    -1:
      begin
        if not (ghloHashListAsInitData in AOptions) then
          AHashList[1] := 0;
        LHash := 0;
        DelphiHashLittle2(AKey, ASize, AHashList[1], LHash);
        Exit;
      end;
    0: Exit;
    1:
      begin
        if not (ghloHashListAsInitData in AOptions) then
          AHashList[1] := 0;
        LHash := 0;
        DelphiHashLittle2(AKey, ASize, AHashList[1], LHash);
        Exit;
      end;
    2:
      begin
        if not (ghloHashListAsInitData in AOptions) then
        begin
          AHashList[1] := 0;
          AHashList[2] := 0;
        end;
        DelphiHashLittle2(AKey, ASize, AHashList[1], AHashList[2]);
        Exit;
      end;
  else
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  end;
{$WARNINGS ON}
end;

{ TDelphiQuadrupleHashFactory }

class function TDelphiQuadrupleHashFactory.GetHashService: THashServiceClass;
begin
  Result := TExtendedHashService<TDelphiQuadrupleHashFactory>;
end;

class function TDelphiQuadrupleHashFactory.GetHashCode(AKey: Pointer; ASize: SizeInt; AInitVal: UInt32): UInt32;
begin
  Result := DelphiHashLittle(AKey, ASize, AInitVal);
end;

class procedure TDelphiQuadrupleHashFactory.GetHashList(AKey: Pointer; ASize: SizeInt; AHashList: PUInt32;
        AOptions: TGetHashListOptions);
var
  LHash: UInt32;
  AHashListParams: PInt16 absolute AHashList;
begin
  case AHashListParams[0] of
    -4:
      begin
        if not (ghloHashListAsInitData in AOptions) then
          AHashList[1] := 1988;
        LHash := 2004;
        DelphiHashLittle2(AKey, ASize, LHash, AHashList[1]);
        Exit;
      end;
    -3:
      begin
        if not (ghloHashListAsInitData in AOptions) then
          AHashList[1] := 2004;
        LHash := 1988;
        DelphiHashLittle2(AKey, ASize, AHashList[1], LHash);
        Exit;
      end;
    -2:
      begin
        if not (ghloHashListAsInitData in AOptions) then
          AHashList[1] := 0;
        LHash := 0;
        DelphiHashLittle2(AKey, ASize, LHash, AHashList[1]);
        Exit;
      end;
    -1:
      begin
        if not (ghloHashListAsInitData in AOptions) then
          AHashList[1] := 0;
        LHash := 0;
        DelphiHashLittle2(AKey, ASize, AHashList[1], LHash);
        Exit;
      end;
    0: Exit;
    1:
      begin
        if not (ghloHashListAsInitData in AOptions) then
          AHashList[1] := 0;
        LHash := 0;
        DelphiHashLittle2(AKey, ASize, AHashList[1], LHash);
        Exit;
      end;
    2:
      begin
        case AHashListParams[1] of
          0, 1:
            begin
              if not (ghloHashListAsInitData in AOptions) then
              begin
                AHashList[1] := 0;
                AHashList[2] := 0;
              end;
              DelphiHashLittle2(AKey, ASize, AHashList[1], AHashList[2]);
              Exit;
            end;
          2:
            begin
              if not (ghloHashListAsInitData in AOptions) then
              begin
                AHashList[1] := 2004;
                AHashList[2] := 1988;
              end;
              DelphiHashLittle2(AKey, ASize, AHashList[1], AHashList[2]);
              Exit;
            end;
        else
          raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
        end;
      end;
    4:
      case AHashListParams[1] of
        1:
          begin
            if not (ghloHashListAsInitData in AOptions) then
            begin
              AHashList[1] := 0;
              AHashList[2] := 0;
            end;
            DelphiHashLittle2(AKey, ASize, AHashList[1], AHashList[2]);
            Exit;
          end;
        2:
          begin
            if not (ghloHashListAsInitData in AOptions) then
            begin
              AHashList[3] := 2004;
              AHashList[4] := 1988;
            end;
            DelphiHashLittle2(AKey, ASize, AHashList[3], AHashList[4]);
            Exit;
          end;
      else
        raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
      end;
  else
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  end;
end;

{ TDelphiSixfoldHashFactory }

class function TDelphiSixfoldHashFactory.GetHashService: THashServiceClass;
begin
  Result := TExtendedHashService<TDelphiSixfoldHashFactory>;
end;

class function TDelphiSixfoldHashFactory.GetHashCode(AKey: Pointer; ASize: SizeInt; AInitVal: UInt32): UInt32;
begin
  Result := DelphiHashLittle(AKey, ASize, AInitVal);
end;

class procedure TDelphiSixfoldHashFactory.GetHashList(AKey: Pointer; ASize: SizeInt; AHashList: PUInt32;
        AOptions: TGetHashListOptions);
var
  LHash: UInt32;
  AHashListParams: PInt16 absolute AHashList;
begin
  case AHashListParams[0] of
    -6:
      begin
        if not (ghloHashListAsInitData in AOptions) then
          AHashList[1] := 2;
        LHash := 1;
        DelphiHashLittle2(AKey, ASize, LHash, AHashList[1]);
        Exit;
      end;
    -5:
      begin
        if not (ghloHashListAsInitData in AOptions) then
          AHashList[1] := 1;
        LHash := 2;
        DelphiHashLittle2(AKey, ASize, AHashList[1], LHash);
        Exit;
      end;
    -4:
      begin
        if not (ghloHashListAsInitData in AOptions) then
          AHashList[1] := 1988;
        LHash := 2004;
        DelphiHashLittle2(AKey, ASize, LHash, AHashList[1]);
        Exit;
      end;
    -3:
      begin
        if not (ghloHashListAsInitData in AOptions) then
          AHashList[1] := 2004;
        LHash := 1988;
        DelphiHashLittle2(AKey, ASize, AHashList[1], LHash);
        Exit;
      end;
    -2:
      begin
        if not (ghloHashListAsInitData in AOptions) then
          AHashList[1] := 0;
        LHash := 0;
        DelphiHashLittle2(AKey, ASize, LHash, AHashList[1]);
        Exit;
      end;
    -1:
      begin
        if not (ghloHashListAsInitData in AOptions) then
          AHashList[1] := 0;
        LHash := 0;
        DelphiHashLittle2(AKey, ASize, AHashList[1], LHash);
        Exit;
      end;
    0: Exit;
    1:
      begin
        if not (ghloHashListAsInitData in AOptions) then
          AHashList[1] := 0;
        LHash := 0;
        DelphiHashLittle2(AKey, ASize, AHashList[1], LHash);
        Exit;
      end;
    2:
      begin
        case AHashListParams[1] of
          0, 1:
            begin
              if not (ghloHashListAsInitData in AOptions) then
              begin
                AHashList[1] := 0;
                AHashList[2] := 0;
              end;
              DelphiHashLittle2(AKey, ASize, AHashList[1], AHashList[2]);
              Exit;
            end;
          2:
            begin
              if not (ghloHashListAsInitData in AOptions) then
              begin
                AHashList[1] := 2004;
                AHashList[2] := 1988;
              end;
              DelphiHashLittle2(AKey, ASize, AHashList[1], AHashList[2]);
              Exit;
            end;
        else
          raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
        end;
      end;
    6:
      case AHashListParams[1] of
        1:
          begin
            if not (ghloHashListAsInitData in AOptions) then
            begin
              AHashList[1] := 0;
              AHashList[2] := 0;
            end;
            DelphiHashLittle2(AKey, ASize, AHashList[1], AHashList[2]);
            Exit;
          end;
        2:
          begin
            if not (ghloHashListAsInitData in AOptions) then
            begin
              AHashList[3] := 2004;
              AHashList[4] := 1988;
            end;
            DelphiHashLittle2(AKey, ASize, AHashList[3], AHashList[4]);
            Exit;
          end;
        3:
          begin
            if not (ghloHashListAsInitData in AOptions) then
            begin
              AHashList[5] := 1;
              AHashList[6] := 2;
            end;
            DelphiHashLittle2(AKey, ASize, AHashList[5], AHashList[6]);
            Exit;
          end;
      else
        raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
      end;
  else
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  end;
end;

{ TOrdinalComparer<T, THashFactory> }

class constructor TOrdinalComparer<T, THashFactory>.Create;
begin
  if THashFactory.InheritsFrom(TExtendedHashService) then
  begin
    FExtendedEqualityComparer := TExtendedEqualityComparer<T>.Default(TExtendedHashFactoryClass(THashFactory));
    FEqualityComparer := IEqualityComparer<T>(FExtendedEqualityComparer);
  end
  else
    FEqualityComparer := TEqualityComparer<T>.Default(THashFactory);
  FComparer := TComparer<T>.Default;
end;

{ TGStringComparer<T, THashFactory> }

class destructor TGStringComparer<T, THashFactory>.Destroy;
begin
  if Assigned(FOrdinal) then
    FOrdinal.Free;
end;

class function TGStringComparer<T, THashFactory>.Ordinal: TCustomComparer<T>;
begin
  if not Assigned(FOrdinal) then
    FOrdinal := TGOrdinalStringComparer<T, THashFactory>.Create;
  Result := FOrdinal;
end;

{ TGOrdinalStringComparer<T, THashFactory> }

function TGOrdinalStringComparer<T, THashFactory>.Compare(constref ALeft, ARight: T): Integer;
begin
  Result := FComparer.Compare(ALeft, ARight);
end;

function TGOrdinalStringComparer<T, THashFactory>.Equals(constref ALeft, ARight: T): Boolean;
begin
  Result := FEqualityComparer.Equals(ALeft, ARight);
end;

function TGOrdinalStringComparer<T, THashFactory>.GetHashCode(constref AValue: T): UInt32;
begin
  Result := FEqualityComparer.GetHashCode(AValue);
end;

procedure TGOrdinalStringComparer<T, THashFactory>.GetHashList(constref AValue: T; AHashList: PUInt32);
begin
  FExtendedEqualityComparer.GetHashList(AValue, AHashList);
end;

{ TGIStringComparer<T, THashFactory> }

class destructor TGIStringComparer<T, THashFactory>.Destroy;
begin
  if Assigned(FOrdinal) then
    FOrdinal.Free;
end;

class function TGIStringComparer<T, THashFactory>.Ordinal: TCustomComparer<T>;
begin
  if not Assigned(FOrdinal) then
    FOrdinal := TGOrdinalIStringComparer<T, THashFactory>.Create;
  Result := FOrdinal;
end;

{ TGOrdinalIStringComparer<T, THashFactory> }

function TGOrdinalIStringComparer<T, THashFactory>.Compare(constref ALeft, ARight: T): Integer;
begin
  Result := FComparer.Compare(ALeft.ToLower, ARight.ToLower);
end;

function TGOrdinalIStringComparer<T, THashFactory>.Equals(constref ALeft, ARight: T): Boolean;
begin
  Result := FEqualityComparer.Equals(ALeft.ToLower, ARight.ToLower);
end;

function TGOrdinalIStringComparer<T, THashFactory>.GetHashCode(constref AValue: T): UInt32;
begin
  Result := FEqualityComparer.GetHashCode(AValue.ToLower);
end;

procedure TGOrdinalIStringComparer<T, THashFactory>.GetHashList(constref AValue: T; AHashList: PUInt32);
begin
  FExtendedEqualityComparer.GetHashList(AValue.ToLower, AHashList);
end;

function BobJenkinsHash(const AData; ALength, AInitData: Integer): Integer;
begin
  Result := DelphiHashLittle(@AData, ALength, AInitData);
end;

function BinaryCompare(const ALeft, ARight: Pointer; ASize: PtrUInt): Integer;
begin
  Result := CompareMemRange(ALeft, ARight, ASize);
end;

function _LookupVtableInfo(AGInterface: TDefaultGenericInterface; ATypeInfo: PTypeInfo; ASize: SizeInt): Pointer;
begin
  Result := _LookupVtableInfoEx(AGInterface, ATypeInfo, ASize, nil);
end;

function _LookupVtableInfoEx(AGInterface: TDefaultGenericInterface; ATypeInfo: PTypeInfo; ASize: SizeInt;
  AFactory: THashFactoryClass): Pointer;
begin
  case AGInterface of
    giComparer:
        Exit(
          TComparerService.LookupComparer(ATypeInfo, ASize));
    giEqualityComparer:
      begin
        if AFactory = nil then
          AFactory := TDelphiHashFactory;

        Exit(
          AFactory.GetHashService.LookupEqualityComparer(ATypeInfo, ASize));
      end;
    giExtendedEqualityComparer:
      begin
        if AFactory = nil then
          AFactory := TDelphiDoubleHashFactory;

        Exit(
          TExtendedHashServiceClass(AFactory.GetHashService).LookupExtendedEqualityComparer(ATypeInfo, ASize));
      end;
  else
    System.Error(reRangeError);
    Exit(nil);
  end;
end;

end.

