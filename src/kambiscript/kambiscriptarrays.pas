{
  Copyright 2008-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ KambiScript array types and built-in functions. }
unit KambiScriptArrays;

interface

uses VectorMath, KambiScript, KambiScriptVectors, KambiUtils, KambiStringUtils;

type
  TKamScriptArray = class(TKamScriptValue)
  end;

  TKamScriptLongIntArray = class(TKamScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
  private
    FValue: TLongIntList;
    procedure SetValue(const AValue: TLongIntList);
  public
    constructor Create(const AWriteable: boolean; const AValue: TLongIntList);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TLongIntList read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptSingleArray = class(TKamScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
  private
    FValue: TSingleList;
    procedure SetValue(const AValue: TSingleList);
  public
    constructor Create(const AWriteable: boolean; const AValue: TSingleList);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TSingleList read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptDoubleArray = class(TKamScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
  private
    FValue: TDoubleList;
    procedure SetValue(const AValue: TDoubleList);
  public
    constructor Create(const AWriteable: boolean; const AValue: TDoubleList);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TDoubleList read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptBooleanArray = class(TKamScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
  private
    FValue: TBooleanList;
    procedure SetValue(const AValue: TBooleanList);
  public
    constructor Create(const AWriteable: boolean; const AValue: TBooleanList);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TBooleanList read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptStringArray = class(TKamScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
  private
    FValue: TKamStringList;
    procedure SetValue(const AValue: TKamStringList);
  public
    constructor Create(const AWriteable: boolean; const AValue: TKamStringList);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TKamStringList read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptVec2fArray = class(TKamScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
  private
    FValue: TVector2SingleList;
    procedure SetValue(const AValue: TVector2SingleList);
  public
    constructor Create(const AWriteable: boolean; const AValue: TVector2SingleList);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TVector2SingleList read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptVec3fArray = class(TKamScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
  private
    FValue: TVector3SingleList;
    procedure SetValue(const AValue: TVector3SingleList);
  public
    constructor Create(const AWriteable: boolean; const AValue: TVector3SingleList);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TVector3SingleList read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptVec4fArray = class(TKamScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
  private
    FValue: TVector4SingleList;
    procedure SetValue(const AValue: TVector4SingleList);
  public
    constructor Create(const AWriteable: boolean; const AValue: TVector4SingleList);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TVector4SingleList read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptVec2dArray = class(TKamScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
  private
    FValue: TVector2DoubleList;
    procedure SetValue(const AValue: TVector2DoubleList);
  public
    constructor Create(const AWriteable: boolean; const AValue: TVector2DoubleList);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TVector2DoubleList read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptVec3dArray = class(TKamScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
  private
    FValue: TVector3DoubleList;
    procedure SetValue(const AValue: TVector3DoubleList);
  public
    constructor Create(const AWriteable: boolean; const AValue: TVector3DoubleList);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TVector3DoubleList read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptVec4dArray = class(TKamScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
  private
    FValue: TVector4DoubleList;
    procedure SetValue(const AValue: TVector4DoubleList);
  public
    constructor Create(const AWriteable: boolean; const AValue: TVector4DoubleList);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TVector4DoubleList read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptMatrix3fArray = class(TKamScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
  private
    FValue: TMatrix3SingleList;
    procedure SetValue(const AValue: TMatrix3SingleList);
  public
    constructor Create(const AWriteable: boolean; const AValue: TMatrix3SingleList);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TMatrix3SingleList read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptMatrix4fArray = class(TKamScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
  private
    FValue: TMatrix4SingleList;
    procedure SetValue(const AValue: TMatrix4SingleList);
  public
    constructor Create(const AWriteable: boolean; const AValue: TMatrix4SingleList);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TMatrix4SingleList read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptMatrix3dArray = class(TKamScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
  private
    FValue: TMatrix3DoubleList;
    procedure SetValue(const AValue: TMatrix3DoubleList);
  public
    constructor Create(const AWriteable: boolean; const AValue: TMatrix3DoubleList);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TMatrix3DoubleList read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptMatrix4dArray = class(TKamScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
  private
    FValue: TMatrix4DoubleList;
    procedure SetValue(const AValue: TMatrix4DoubleList);
  public
    constructor Create(const AWriteable: boolean; const AValue: TMatrix4DoubleList);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TMatrix4DoubleList read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptArrayFun = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptArrayD = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptArrayGetCount = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptArraySetCount = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
    class function ArgumentMustBeAssignable(const Index: Integer): boolean; override;
  end;

  TKamScriptArrayGet = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptArraySet = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
    class function ArgumentMustBeAssignable(const Index: Integer): boolean; override;
  end;

implementation

uses SysUtils, KambiScriptCoreFunctions;

{ KambiScript values --------------------------------------------------------- }

{$define TKamScriptXxxArray := TKamScriptLongIntArray}
{$define TXxxList := TLongIntList}
{$define TKamScriptXxxElement := TKamScriptInteger}
{$define RegisterXxxFunctions := RegisterLongIntFunctions}
{$define TKamScriptXxxArrayFun := TKamScriptArrayFun}
{$I kambiscriptarrays_implement.inc}

{$define TKamScriptXxxArray := TKamScriptSingleArray}
{$define TXxxList := TSingleList}
{$define TKamScriptXxxElement := TKamScriptFloat}
{$define RegisterXxxFunctions := RegisterSingleFunctions}
{$define TKamScriptXxxArrayFun := TKamScriptArrayFun}
{$I kambiscriptarrays_implement.inc}

{$define TKamScriptXxxArray := TKamScriptDoubleArray}
{$define TXxxList := TDoubleList}
{$define TKamScriptXxxElement := TKamScriptFloat}
{$define RegisterXxxFunctions := RegisterDoubleFunctions}
{$define TKamScriptXxxArrayFun := TKamScriptArrayD}
{$I kambiscriptarrays_implement.inc}

{$define TKamScriptXxxArray := TKamScriptBooleanArray}
{$define TXxxList := TBooleanList}
{$define TKamScriptXxxElement := TKamScriptBoolean}
{$define RegisterXxxFunctions := RegisterBooleanFunctions}
{$define TKamScriptXxxArrayFun := TKamScriptArrayFun}
{$I kambiscriptarrays_implement.inc}

{$define TKamScriptXxxArray := TKamScriptStringArray}
{$define TXxxList := TKamStringList}
{$define TKamScriptXxxElement := TKamScriptString}
{$define RegisterXxxFunctions := RegisterStringFunctions}
{$define TKamScriptXxxArrayFun := TKamScriptArrayFun}
{$I kambiscriptarrays_implement.inc}

{$define TKamScriptXxxArray := TKamScriptVec2fArray}
{$define TXxxList := TVector2SingleList}
{$define TKamScriptXxxElement := TKamScriptVec2f}
{$define RegisterXxxFunctions := RegisterVec2fFunctions}
{$define TKamScriptXxxArrayFun := TKamScriptArrayFun}
{$I kambiscriptarrays_implement.inc}

{$define TKamScriptXxxArray := TKamScriptVec3fArray}
{$define TXxxList := TVector3SingleList}
{$define TKamScriptXxxElement := TKamScriptVec3f}
{$define RegisterXxxFunctions := RegisterVec3fFunctions}
{$define TKamScriptXxxArrayFun := TKamScriptArrayFun}
{$I kambiscriptarrays_implement.inc}

{$define TKamScriptXxxArray := TKamScriptVec4fArray}
{$define TXxxList := TVector4SingleList}
{$define TKamScriptXxxElement := TKamScriptVec4f}
{$define RegisterXxxFunctions := RegisterVec4fFunctions}
{$define TKamScriptXxxArrayFun := TKamScriptArrayFun}
{$I kambiscriptarrays_implement.inc}

{$define TKamScriptXxxArray := TKamScriptVec2dArray}
{$define TXxxList := TVector2DoubleList}
{$define TKamScriptXxxElement := TKamScriptVec2d}
{$define RegisterXxxFunctions := RegisterVec2dFunctions}
{$define TKamScriptXxxArrayFun := TKamScriptArrayFun}
{$I kambiscriptarrays_implement.inc}

{$define TKamScriptXxxArray := TKamScriptVec3dArray}
{$define TXxxList := TVector3DoubleList}
{$define TKamScriptXxxElement := TKamScriptVec3d}
{$define RegisterXxxFunctions := RegisterVec3dFunctions}
{$define TKamScriptXxxArrayFun := TKamScriptArrayFun}
{$I kambiscriptarrays_implement.inc}

{$define TKamScriptXxxArray := TKamScriptVec4dArray}
{$define TXxxList := TVector4DoubleList}
{$define TKamScriptXxxElement := TKamScriptVec4d}
{$define RegisterXxxFunctions := RegisterVec4dFunctions}
{$define TKamScriptXxxArrayFun := TKamScriptArrayFun}
{$I kambiscriptarrays_implement.inc}

{$define TKamScriptXxxArray := TKamScriptMatrix3fArray}
{$define TXxxList := TMatrix3SingleList}
{$define TKamScriptXxxElement := TKamScriptMatrix3f}
{$define RegisterXxxFunctions := RegisterMatrix3fFunctions}
{$define TKamScriptXxxArrayFun := TKamScriptArrayFun}
{$I kambiscriptarrays_implement.inc}

{$define TKamScriptXxxArray := TKamScriptMatrix4fArray}
{$define TXxxList := TMatrix4SingleList}
{$define TKamScriptXxxElement := TKamScriptMatrix4f}
{$define RegisterXxxFunctions := RegisterMatrix4fFunctions}
{$define TKamScriptXxxArrayFun := TKamScriptArrayFun}
{$I kambiscriptarrays_implement.inc}

{$define TKamScriptXxxArray := TKamScriptMatrix3dArray}
{$define TXxxList := TMatrix3DoubleList}
{$define TKamScriptXxxElement := TKamScriptMatrix3d}
{$define RegisterXxxFunctions := RegisterMatrix3dFunctions}
{$define TKamScriptXxxArrayFun := TKamScriptArrayFun}
{$I kambiscriptarrays_implement.inc}

{$define TKamScriptXxxArray := TKamScriptMatrix4dArray}
{$define TXxxList := TMatrix4DoubleList}
{$define TKamScriptXxxElement := TKamScriptMatrix4d}
{$define RegisterXxxFunctions := RegisterMatrix4dFunctions}
{$define TKamScriptXxxArrayFun := TKamScriptArrayFun}
{$I kambiscriptarrays_implement.inc}

{ KambiScript functions ------------------------------------------------------ }

class function TKamScriptArrayFun.ShortName: string;
begin
  Result := 'array';
end;

class function TKamScriptArrayD.ShortName: string;
begin
  Result := 'array_d';
end;

class function TKamScriptArrayGetCount.ShortName: string;
begin
  Result := 'array_get_count';
end;

class function TKamScriptArraySetCount.ShortName: string;
begin
  Result := 'array_set_count';
end;

class function TKamScriptArraySetCount.ArgumentMustBeAssignable(const Index: Integer): boolean;
begin
  Result := Index = 0;
end;

class function TKamScriptArrayGet.ShortName: string;
begin
  Result := 'array_get';
end;

class function TKamScriptArraySet.ShortName: string;
begin
  Result := 'array_set';
end;

class function TKamScriptArraySet.ArgumentMustBeAssignable(const Index: Integer): boolean;
begin
  Result := Index = 0;
end;

{ Handling strings as arrays of characters ----------------------------------- }

type
  TKamScriptCharacterArray = class
    class procedure HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
  end;

class procedure TKamScriptCharacterArray.HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptInteger);
  TKamScriptInteger(AResult).Value := Length(TKamScriptString(Arguments[0]).Value);
end;

class procedure TKamScriptCharacterArray.HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  NewCount: Int64;
  NewValue: string;
begin
  if ParentOfResult then
    AResult.FreeByParentExpression;
  AResult := nil;
  ParentOfResult := false;

  NewCount := TKamScriptInteger(Arguments[1]).Value;
  if NewCount < 0 then
    raise EKamScriptError.CreateFmt('Invalid count %d for array_set_count (should be non-negative)',
      [NewCount]);

  NewValue := TKamScriptString(Arguments[0]).Value;
  SetLength(NewValue, NewCount);
  TKamScriptString(Arguments[0]).Value := NewValue;

  TKamScriptString(Arguments[0]).ValueAssigned := true;

  AResult := Arguments[0];
end;

class procedure TKamScriptCharacterArray.HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  Index: Integer;
  Arr: string;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptString);

  Arr := TKamScriptString(Arguments[0]).Value;

  Index := TKamScriptInteger(Arguments[1]).Value;
  if not Between(Index, 0, Length(Arr) - 1) then
    raise EKamScriptError.CreateFmt('Invalid index %d for array_get, array count is %d',
      [Index, Length(Arr)]);

  TKamScriptString(AResult).Value := Arr[Index+1];
end;

class procedure TKamScriptCharacterArray.HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  Index: Integer;
  Arr: string;
begin
  if ParentOfResult then
    AResult.FreeByParentExpression;
  AResult := nil;
  ParentOfResult := false;

  Arr := TKamScriptString(Arguments[0]).Value;

  Index := TKamScriptInteger(Arguments[1]).Value;
  if not Between(Index, 0, Length(Arr) - 1) then
    raise EKamScriptError.CreateFmt('Invalid index %d for array_set, array count is %d',
      [Index, Length(Arr)]);

  if Length(TKamScriptString(Arguments[2]).Value) <> 1 then
    raise EKamScriptError.CreateFmt('Invalid value as the last array_set argument: given array is a string, so value is expected to be a character (that is, a string of length exactly 1). But given value is "%s" (length %d)',
      [ TKamScriptString(Arguments[2]).Value,
        Length(TKamScriptString(Arguments[2]).Value) ]);

  Arr[Index+1] := TKamScriptString(Arguments[2]).Value[1];
  TKamScriptString(Arguments[0]).Value := Arr;

  AResult := Arguments[0];
end;

procedure RegisterCharacterFunctions;
begin
  FunctionHandlers.RegisterHandler(@TKamScriptCharacterArray(nil).HandleArrayGetCount, TKamScriptArrayGetCount, [TKamScriptString], false);
  FunctionHandlers.RegisterHandler(@TKamScriptCharacterArray(nil).HandleArraySetCount, TKamScriptArraySetCount, [TKamScriptString, TKamScriptInteger], false);
  FunctionHandlers.RegisterHandler(@TKamScriptCharacterArray(nil).HandleArrayGet, TKamScriptArrayGet, [TKamScriptString, TKamScriptInteger], false);
  FunctionHandlers.RegisterHandler(@TKamScriptCharacterArray(nil).HandleArraySet, TKamScriptArraySet, [TKamScriptString, TKamScriptInteger, TKamScriptString], false);
end;

initialization
  RegisterLongIntFunctions;
  RegisterSingleFunctions;
  RegisterDoubleFunctions;
  RegisterBooleanFunctions;
  RegisterStringFunctions;

  RegisterVec2fFunctions;
  RegisterVec3fFunctions;
  RegisterVec4fFunctions;
  RegisterVec2dFunctions;
  RegisterVec3dFunctions;
  RegisterVec4dFunctions;

  RegisterMatrix3fFunctions;
  RegisterMatrix4fFunctions;
  RegisterMatrix3dFunctions;
  RegisterMatrix4dFunctions;

  RegisterCharacterFunctions;
end.
