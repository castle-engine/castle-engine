{
  Copyright 2008-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ CastleScript array types and built-in functions. }
unit CastleScriptArrays;

{$I castleconf.inc}

interface

uses CastleVectors, CastleScript, CastleScriptVectors, CastleUtils, CastleStringUtils;

type
  TCasScriptArray = class(TCasScriptValue)
  end;

  TCasScriptLongIntArray = class(TCasScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
  private
    FValue: TLongIntList;
    procedure SetValue(const AValue: TLongIntList);
  public
    constructor Create(const AWriteable: boolean; const AValue: TLongIntList);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TLongIntList read FValue write SetValue;

    procedure AssignValue(Source: TCasScriptValue); override;
  end;

  TCasScriptSingleArray = class(TCasScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleCatmullRomSpline(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleHermiteSpline(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleHermiteTenseSpline(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
  private
    FValue: TSingleList;
    procedure SetValue(const AValue: TSingleList);
  public
    constructor Create(const AWriteable: boolean; const AValue: TSingleList);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TSingleList read FValue write SetValue;

    procedure AssignValue(Source: TCasScriptValue); override;
  end;

  TCasScriptDoubleArray = class(TCasScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
  private
    FValue: TDoubleList;
    procedure SetValue(const AValue: TDoubleList);
  public
    constructor Create(const AWriteable: boolean; const AValue: TDoubleList);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TDoubleList read FValue write SetValue;

    procedure AssignValue(Source: TCasScriptValue); override;
  end;

  TCasScriptBooleanArray = class(TCasScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
  private
    FValue: TBooleanList;
    procedure SetValue(const AValue: TBooleanList);
  public
    constructor Create(const AWriteable: boolean; const AValue: TBooleanList);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TBooleanList read FValue write SetValue;

    procedure AssignValue(Source: TCasScriptValue); override;
  end;

  TCasScriptStringArray = class(TCasScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
  private
    FValue: TCastleStringList;
    procedure SetValue(const AValue: TCastleStringList);
  public
    constructor Create(const AWriteable: boolean; const AValue: TCastleStringList);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TCastleStringList read FValue write SetValue;

    procedure AssignValue(Source: TCasScriptValue); override;
  end;

  TCasScriptVec2fArray = class(TCasScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
  private
    FValue: TVector2SingleList;
    procedure SetValue(const AValue: TVector2SingleList);
  public
    constructor Create(const AWriteable: boolean; const AValue: TVector2SingleList);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TVector2SingleList read FValue write SetValue;

    procedure AssignValue(Source: TCasScriptValue); override;
  end;

  TCasScriptVec3fArray = class(TCasScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
  private
    FValue: TVector3SingleList;
    procedure SetValue(const AValue: TVector3SingleList);
  public
    constructor Create(const AWriteable: boolean; const AValue: TVector3SingleList);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TVector3SingleList read FValue write SetValue;

    procedure AssignValue(Source: TCasScriptValue); override;
  end;

  TCasScriptVec4fArray = class(TCasScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
  private
    FValue: TVector4SingleList;
    procedure SetValue(const AValue: TVector4SingleList);
  public
    constructor Create(const AWriteable: boolean; const AValue: TVector4SingleList);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TVector4SingleList read FValue write SetValue;

    procedure AssignValue(Source: TCasScriptValue); override;
  end;

  TCasScriptVec2dArray = class(TCasScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
  private
    FValue: TVector2DoubleList;
    procedure SetValue(const AValue: TVector2DoubleList);
  public
    constructor Create(const AWriteable: boolean; const AValue: TVector2DoubleList);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TVector2DoubleList read FValue write SetValue;

    procedure AssignValue(Source: TCasScriptValue); override;
  end;

  TCasScriptVec3dArray = class(TCasScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
  private
    FValue: TVector3DoubleList;
    procedure SetValue(const AValue: TVector3DoubleList);
  public
    constructor Create(const AWriteable: boolean; const AValue: TVector3DoubleList);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TVector3DoubleList read FValue write SetValue;

    procedure AssignValue(Source: TCasScriptValue); override;
  end;

  TCasScriptVec4dArray = class(TCasScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
  private
    FValue: TVector4DoubleList;
    procedure SetValue(const AValue: TVector4DoubleList);
  public
    constructor Create(const AWriteable: boolean; const AValue: TVector4DoubleList);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TVector4DoubleList read FValue write SetValue;

    procedure AssignValue(Source: TCasScriptValue); override;
  end;

  TCasScriptMatrix3fArray = class(TCasScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
  private
    FValue: TMatrix3SingleList;
    procedure SetValue(const AValue: TMatrix3SingleList);
  public
    constructor Create(const AWriteable: boolean; const AValue: TMatrix3SingleList);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TMatrix3SingleList read FValue write SetValue;

    procedure AssignValue(Source: TCasScriptValue); override;
  end;

  TCasScriptMatrix4fArray = class(TCasScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
  private
    FValue: TMatrix4SingleList;
    procedure SetValue(const AValue: TMatrix4SingleList);
  public
    constructor Create(const AWriteable: boolean; const AValue: TMatrix4SingleList);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TMatrix4SingleList read FValue write SetValue;

    procedure AssignValue(Source: TCasScriptValue); override;
  end;

  TCasScriptMatrix3dArray = class(TCasScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
  private
    FValue: TMatrix3DoubleList;
    procedure SetValue(const AValue: TMatrix3DoubleList);
  public
    constructor Create(const AWriteable: boolean; const AValue: TMatrix3DoubleList);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TMatrix3DoubleList read FValue write SetValue;

    procedure AssignValue(Source: TCasScriptValue); override;
  end;

  TCasScriptMatrix4dArray = class(TCasScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
  private
    FValue: TMatrix4DoubleList;
    procedure SetValue(const AValue: TMatrix4DoubleList);
  public
    constructor Create(const AWriteable: boolean; const AValue: TMatrix4DoubleList);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TMatrix4DoubleList read FValue write SetValue;

    procedure AssignValue(Source: TCasScriptValue); override;
  end;

  TCasScriptArrayFun = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptArrayD = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptArrayGetCount = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptArraySetCount = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
    class function ArgumentMustBeAssignable(const Index: Integer): boolean; override;
  end;

  TCasScriptArrayGet = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptArraySet = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
    class function ArgumentMustBeAssignable(const Index: Integer): boolean; override;
  end;

  TCasScriptCatmullRomSpline = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptHermiteSpline = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptHermiteTenseSpline = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

implementation

uses SysUtils, CastleScriptCoreFunctions, CastleCurves;

{ CastleScript values --------------------------------------------------------- }

{$define TCasScriptXxxArray := TCasScriptLongIntArray}
{$define TXxxList := TLongIntList}
{$define TCasScriptXxxElement := TCasScriptInteger}
{$define RegisterXxxFunctions := RegisterLongIntFunctions}
{$define TCasScriptXxxArrayFun := TCasScriptArrayFun}
{$I castlescriptarrays_implement.inc}

{$define TCasScriptXxxArray := TCasScriptSingleArray}
{$define TXxxList := TSingleList}
{$define TCasScriptXxxElement := TCasScriptFloat}
{$define RegisterXxxFunctions := RegisterSingleFunctions}
{$define TCasScriptXxxArrayFun := TCasScriptArrayFun}
{$I castlescriptarrays_implement.inc}

{$define TCasScriptXxxArray := TCasScriptDoubleArray}
{$define TXxxList := TDoubleList}
{$define TCasScriptXxxElement := TCasScriptFloat}
{$define RegisterXxxFunctions := RegisterDoubleFunctions}
{$define TCasScriptXxxArrayFun := TCasScriptArrayD}
{$I castlescriptarrays_implement.inc}

{$define TCasScriptXxxArray := TCasScriptBooleanArray}
{$define TXxxList := TBooleanList}
{$define TCasScriptXxxElement := TCasScriptBoolean}
{$define RegisterXxxFunctions := RegisterBooleanFunctions}
{$define TCasScriptXxxArrayFun := TCasScriptArrayFun}
{$I castlescriptarrays_implement.inc}

{$define TCasScriptXxxArray := TCasScriptStringArray}
{$define TXxxList := TCastleStringList}
{$define TCasScriptXxxElement := TCasScriptString}
{$define RegisterXxxFunctions := RegisterStringFunctions}
{$define TCasScriptXxxArrayFun := TCasScriptArrayFun}
{$I castlescriptarrays_implement.inc}

{$define TCasScriptXxxArray := TCasScriptVec2fArray}
{$define TXxxList := TVector2SingleList}
{$define TCasScriptXxxElement := TCasScriptVec2f}
{$define RegisterXxxFunctions := RegisterVec2fFunctions}
{$define TCasScriptXxxArrayFun := TCasScriptArrayFun}
{$I castlescriptarrays_implement.inc}

{$define TCasScriptXxxArray := TCasScriptVec3fArray}
{$define TXxxList := TVector3SingleList}
{$define TCasScriptXxxElement := TCasScriptVec3f}
{$define RegisterXxxFunctions := RegisterVec3fFunctions}
{$define TCasScriptXxxArrayFun := TCasScriptArrayFun}
{$I castlescriptarrays_implement.inc}

{$define TCasScriptXxxArray := TCasScriptVec4fArray}
{$define TXxxList := TVector4SingleList}
{$define TCasScriptXxxElement := TCasScriptVec4f}
{$define RegisterXxxFunctions := RegisterVec4fFunctions}
{$define TCasScriptXxxArrayFun := TCasScriptArrayFun}
{$I castlescriptarrays_implement.inc}

{$define TCasScriptXxxArray := TCasScriptVec2dArray}
{$define TXxxList := TVector2DoubleList}
{$define TCasScriptXxxElement := TCasScriptVec2d}
{$define RegisterXxxFunctions := RegisterVec2dFunctions}
{$define TCasScriptXxxArrayFun := TCasScriptArrayFun}
{$I castlescriptarrays_implement.inc}

{$define TCasScriptXxxArray := TCasScriptVec3dArray}
{$define TXxxList := TVector3DoubleList}
{$define TCasScriptXxxElement := TCasScriptVec3d}
{$define RegisterXxxFunctions := RegisterVec3dFunctions}
{$define TCasScriptXxxArrayFun := TCasScriptArrayFun}
{$I castlescriptarrays_implement.inc}

{$define TCasScriptXxxArray := TCasScriptVec4dArray}
{$define TXxxList := TVector4DoubleList}
{$define TCasScriptXxxElement := TCasScriptVec4d}
{$define RegisterXxxFunctions := RegisterVec4dFunctions}
{$define TCasScriptXxxArrayFun := TCasScriptArrayFun}
{$I castlescriptarrays_implement.inc}

{$define TCasScriptXxxArray := TCasScriptMatrix3fArray}
{$define TXxxList := TMatrix3SingleList}
{$define TCasScriptXxxElement := TCasScriptMatrix3f}
{$define RegisterXxxFunctions := RegisterMatrix3fFunctions}
{$define TCasScriptXxxArrayFun := TCasScriptArrayFun}
{$I castlescriptarrays_implement.inc}

{$define TCasScriptXxxArray := TCasScriptMatrix4fArray}
{$define TXxxList := TMatrix4SingleList}
{$define TCasScriptXxxElement := TCasScriptMatrix4f}
{$define RegisterXxxFunctions := RegisterMatrix4fFunctions}
{$define TCasScriptXxxArrayFun := TCasScriptArrayFun}
{$I castlescriptarrays_implement.inc}

{$define TCasScriptXxxArray := TCasScriptMatrix3dArray}
{$define TXxxList := TMatrix3DoubleList}
{$define TCasScriptXxxElement := TCasScriptMatrix3d}
{$define RegisterXxxFunctions := RegisterMatrix3dFunctions}
{$define TCasScriptXxxArrayFun := TCasScriptArrayFun}
{$I castlescriptarrays_implement.inc}

{$define TCasScriptXxxArray := TCasScriptMatrix4dArray}
{$define TXxxList := TMatrix4DoubleList}
{$define TCasScriptXxxElement := TCasScriptMatrix4d}
{$define RegisterXxxFunctions := RegisterMatrix4dFunctions}
{$define TCasScriptXxxArrayFun := TCasScriptArrayFun}
{$I castlescriptarrays_implement.inc}

class procedure TCasScriptSingleArray.HandleCatmullRomSpline(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptFloat);
  TCasScriptFloat(AResult).Value := CatmullRomSpline(
    TCasScriptFloat(Arguments[0]).Value,
    TCasScriptBoolean(Arguments[1]).Value,
    TCasScriptSingleArray(Arguments[2]).Value,
    TCasScriptSingleArray(Arguments[3]).Value);
end;

class procedure TCasScriptSingleArray.HandleHermiteSpline(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptFloat);
  TCasScriptFloat(AResult).Value := HermiteSpline(
    TCasScriptFloat(Arguments[0]).Value,
    TCasScriptBoolean(Arguments[1]).Value,
    TCasScriptSingleArray(Arguments[2]).Value,
    TCasScriptSingleArray(Arguments[3]).Value,
    TCasScriptSingleArray(Arguments[4]).Value);
end;

class procedure TCasScriptSingleArray.HandleHermiteTenseSpline(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptFloat);
  TCasScriptFloat(AResult).Value := HermiteTenseSpline(
    TCasScriptFloat(Arguments[0]).Value,
    TCasScriptBoolean(Arguments[1]).Value,
    TCasScriptSingleArray(Arguments[2]).Value,
    TCasScriptSingleArray(Arguments[3]).Value);
end;

{ CastleScript functions ------------------------------------------------------ }

class function TCasScriptArrayFun.ShortName: string;
begin
  Result := 'array';
end;

class function TCasScriptArrayD.ShortName: string;
begin
  Result := 'array_d';
end;

class function TCasScriptArrayGetCount.ShortName: string;
begin
  Result := 'array_get_count';
end;

class function TCasScriptArraySetCount.ShortName: string;
begin
  Result := 'array_set_count';
end;

class function TCasScriptArraySetCount.ArgumentMustBeAssignable(const Index: Integer): boolean;
begin
  Result := Index = 0;
end;

class function TCasScriptArrayGet.ShortName: string;
begin
  Result := 'array_get';
end;

class function TCasScriptArraySet.ShortName: string;
begin
  Result := 'array_set';
end;

class function TCasScriptArraySet.ArgumentMustBeAssignable(const Index: Integer): boolean;
begin
  Result := Index = 0;
end;

class function TCasScriptCatmullRomSpline.ShortName: string;
begin
  Result := 'catmull_rom_spline';
end;

class function TCasScriptHermiteSpline.ShortName: string;
begin
  Result := 'hermite_spline';
end;

class function TCasScriptHermiteTenseSpline.ShortName: string;
begin
  Result := 'hermite_tense_spline';
end;

{ Handling strings as arrays of characters ----------------------------------- }

type
  TCasScriptCharacterArray = class
    class procedure HandleArrayGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
  end;

class procedure TCasScriptCharacterArray.HandleArrayGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptInteger);
  TCasScriptInteger(AResult).Value := Length(TCasScriptString(Arguments[0]).Value);
end;

class procedure TCasScriptCharacterArray.HandleArraySetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  NewCount: Int64;
  NewValue: string;
begin
  if ParentOfResult then
    AResult.FreeByParentExpression;
  AResult := nil;
  ParentOfResult := false;

  NewCount := TCasScriptInteger(Arguments[1]).Value;
  if NewCount < 0 then
    raise ECasScriptError.CreateFmt('Invalid count %d for array_set_count (should be non-negative)',
      [NewCount]);

  NewValue := TCasScriptString(Arguments[0]).Value;
  SetLength(NewValue, NewCount);
  TCasScriptString(Arguments[0]).Value := NewValue;

  TCasScriptString(Arguments[0]).ValueAssigned := true;

  AResult := Arguments[0];
end;

class procedure TCasScriptCharacterArray.HandleArrayGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  Index: Integer;
  Arr: string;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptString);

  Arr := TCasScriptString(Arguments[0]).Value;

  Index := TCasScriptInteger(Arguments[1]).Value;
  if not Between(Index, 0, Length(Arr) - 1) then
    raise ECasScriptError.CreateFmt('Invalid index %d for array_get, array count is %d',
      [Index, Length(Arr)]);

  TCasScriptString(AResult).Value := Arr[Index+1];
end;

class procedure TCasScriptCharacterArray.HandleArraySet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  Index: Integer;
  Arr: string;
begin
  if ParentOfResult then
    AResult.FreeByParentExpression;
  AResult := nil;
  ParentOfResult := false;

  Arr := TCasScriptString(Arguments[0]).Value;

  Index := TCasScriptInteger(Arguments[1]).Value;
  if not Between(Index, 0, Length(Arr) - 1) then
    raise ECasScriptError.CreateFmt('Invalid index %d for array_set, array count is %d',
      [Index, Length(Arr)]);

  if Length(TCasScriptString(Arguments[2]).Value) <> 1 then
    raise ECasScriptError.CreateFmt('Invalid value as the last array_set argument: given array is a string, so value is expected to be a character (that is, a string of length exactly 1). But given value is "%s" (length %d)',
      [ TCasScriptString(Arguments[2]).Value,
        Length(TCasScriptString(Arguments[2]).Value) ]);

  Arr[Index+1] := TCasScriptString(Arguments[2]).Value[1];
  TCasScriptString(Arguments[0]).Value := Arr;

  AResult := Arguments[0];
end;

procedure RegisterCharacterFunctions;
begin
  FunctionHandlers.RegisterHandler(@TCasScriptCharacterArray(nil).HandleArrayGetCount, TCasScriptArrayGetCount, [TCasScriptString], false);
  FunctionHandlers.RegisterHandler(@TCasScriptCharacterArray(nil).HandleArraySetCount, TCasScriptArraySetCount, [TCasScriptString, TCasScriptInteger], false);
  FunctionHandlers.RegisterHandler(@TCasScriptCharacterArray(nil).HandleArrayGet, TCasScriptArrayGet, [TCasScriptString, TCasScriptInteger], false);
  FunctionHandlers.RegisterHandler(@TCasScriptCharacterArray(nil).HandleArraySet, TCasScriptArraySet, [TCasScriptString, TCasScriptInteger, TCasScriptString], false);
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

  FunctionHandlers.RegisterHandler(@TCasScriptSingleArray(nil).HandleCatmullRomSpline, TCasScriptCatmullRomSpline, [TCasScriptFloat, TCasScriptBoolean, TCasScriptSingleArray, TCasScriptSingleArray], false);
  FunctionHandlers.RegisterHandler(@TCasScriptSingleArray(nil).HandleHermiteSpline, TCasScriptHermiteSpline, [TCasScriptFloat, TCasScriptBoolean, TCasScriptSingleArray, TCasScriptSingleArray, TCasScriptSingleArray], false);
  FunctionHandlers.RegisterHandler(@TCasScriptSingleArray(nil).HandleHermiteTenseSpline, TCasScriptHermiteTenseSpline, [TCasScriptFloat, TCasScriptBoolean, TCasScriptSingleArray, TCasScriptSingleArray], false);

  RegisterMatrix3fFunctions;
  RegisterMatrix4fFunctions;
  RegisterMatrix3dFunctions;
  RegisterMatrix4dFunctions;

  RegisterCharacterFunctions;
end.
