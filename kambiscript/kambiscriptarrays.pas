{
  Copyright 2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ KambiScript array types and built-in functions. }
unit KambiScriptArrays;

interface

uses VectorMath, KambiScript, KambiScriptVectors, KambiUtils;

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

  private
    FValue: TDynLongIntArray;
    procedure SetValue(const AValue: TDynLongIntArray);
  public
    constructor Create(const AWriteable: boolean; const AValue: TDynLongIntArray);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TDynLongIntArray read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptSingleArray = class(TKamScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

  private
    FValue: TDynSingleArray;
    procedure SetValue(const AValue: TDynSingleArray);
  public
    constructor Create(const AWriteable: boolean; const AValue: TDynSingleArray);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TDynSingleArray read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptDoubleArray = class(TKamScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

  private
    FValue: TDynDoubleArray;
    procedure SetValue(const AValue: TDynDoubleArray);
  public
    constructor Create(const AWriteable: boolean; const AValue: TDynDoubleArray);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TDynDoubleArray read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptBooleanArray = class(TKamScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

  private
    FValue: TDynBooleanArray;
    procedure SetValue(const AValue: TDynBooleanArray);
  public
    constructor Create(const AWriteable: boolean; const AValue: TDynBooleanArray);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TDynBooleanArray read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptStringArray = class(TKamScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

  private
    FValue: TDynStringArray;
    procedure SetValue(const AValue: TDynStringArray);
  public
    constructor Create(const AWriteable: boolean; const AValue: TDynStringArray);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TDynStringArray read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptVec2fArray = class(TKamScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

  private
    FValue: TDynVector2SingleArray;
    procedure SetValue(const AValue: TDynVector2SingleArray);
  public
    constructor Create(const AWriteable: boolean; const AValue: TDynVector2SingleArray);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TDynVector2SingleArray read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptVec3fArray = class(TKamScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

  private
    FValue: TDynVector3SingleArray;
    procedure SetValue(const AValue: TDynVector3SingleArray);
  public
    constructor Create(const AWriteable: boolean; const AValue: TDynVector3SingleArray);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TDynVector3SingleArray read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptVec4fArray = class(TKamScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

  private
    FValue: TDynVector4SingleArray;
    procedure SetValue(const AValue: TDynVector4SingleArray);
  public
    constructor Create(const AWriteable: boolean; const AValue: TDynVector4SingleArray);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TDynVector4SingleArray read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptVec2dArray = class(TKamScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

  private
    FValue: TDynVector2DoubleArray;
    procedure SetValue(const AValue: TDynVector2DoubleArray);
  public
    constructor Create(const AWriteable: boolean; const AValue: TDynVector2DoubleArray);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TDynVector2DoubleArray read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptVec3dArray = class(TKamScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

  private
    FValue: TDynVector3DoubleArray;
    procedure SetValue(const AValue: TDynVector3DoubleArray);
  public
    constructor Create(const AWriteable: boolean; const AValue: TDynVector3DoubleArray);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TDynVector3DoubleArray read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptVec4dArray = class(TKamScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

  private
    FValue: TDynVector4DoubleArray;
    procedure SetValue(const AValue: TDynVector4DoubleArray);
  public
    constructor Create(const AWriteable: boolean; const AValue: TDynVector4DoubleArray);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TDynVector4DoubleArray read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptMatrix3fArray = class(TKamScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

  private
    FValue: TDynMatrix3SingleArray;
    procedure SetValue(const AValue: TDynMatrix3SingleArray);
  public
    constructor Create(const AWriteable: boolean; const AValue: TDynMatrix3SingleArray);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TDynMatrix3SingleArray read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptMatrix4fArray = class(TKamScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

  private
    FValue: TDynMatrix4SingleArray;
    procedure SetValue(const AValue: TDynMatrix4SingleArray);
  public
    constructor Create(const AWriteable: boolean; const AValue: TDynMatrix4SingleArray);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TDynMatrix4SingleArray read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptMatrix3dArray = class(TKamScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

  private
    FValue: TDynMatrix3DoubleArray;
    procedure SetValue(const AValue: TDynMatrix3DoubleArray);
  public
    constructor Create(const AWriteable: boolean; const AValue: TDynMatrix3DoubleArray);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TDynMatrix3DoubleArray read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptMatrix4dArray = class(TKamScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

  private
    FValue: TDynMatrix4DoubleArray;
    procedure SetValue(const AValue: TDynMatrix4DoubleArray);
  public
    constructor Create(const AWriteable: boolean; const AValue: TDynMatrix4DoubleArray);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TDynMatrix4DoubleArray read FValue write SetValue;

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

uses SysUtils;

{ KambiScript values --------------------------------------------------------- }

{$define TKamScriptXxxArray := TKamScriptLongIntArray}
{$define TDynXxxArray := TDynLongIntArray}
{$define TKamScriptXxxElement := TKamScriptInteger}
{$define RegisterXxxFunctions := RegisterLongIntFunctions}
{$define TKamScriptXxxArrayFun := TKamScriptArrayFun}
{$I kambiscriptarrays_implement.inc}

{$define TKamScriptXxxArray := TKamScriptSingleArray}
{$define TDynXxxArray := TDynSingleArray}
{$define TKamScriptXxxElement := TKamScriptFloat}
{$define RegisterXxxFunctions := RegisterSingleFunctions}
{$define TKamScriptXxxArrayFun := TKamScriptArrayFun}
{$I kambiscriptarrays_implement.inc}

{$define TKamScriptXxxArray := TKamScriptDoubleArray}
{$define TDynXxxArray := TDynDoubleArray}
{$define TKamScriptXxxElement := TKamScriptFloat}
{$define RegisterXxxFunctions := RegisterDoubleFunctions}
{$define TKamScriptXxxArrayFun := TKamScriptArrayD}
{$I kambiscriptarrays_implement.inc}

{$define TKamScriptXxxArray := TKamScriptBooleanArray}
{$define TDynXxxArray := TDynBooleanArray}
{$define TKamScriptXxxElement := TKamScriptBoolean}
{$define RegisterXxxFunctions := RegisterBooleanFunctions}
{$define TKamScriptXxxArrayFun := TKamScriptArrayFun}
{$I kambiscriptarrays_implement.inc}

{$define TKamScriptXxxArray := TKamScriptStringArray}
{$define TDynXxxArray := TDynStringArray}
{$define TKamScriptXxxElement := TKamScriptString}
{$define RegisterXxxFunctions := RegisterStringFunctions}
{$define TKamScriptXxxArrayFun := TKamScriptArrayFun}
{$I kambiscriptarrays_implement.inc}

{$define TKamScriptXxxArray := TKamScriptVec2fArray}
{$define TDynXxxArray := TDynVector2SingleArray}
{$define TKamScriptXxxElement := TKamScriptVec2f}
{$define RegisterXxxFunctions := RegisterVec2fFunctions}
{$define TKamScriptXxxArrayFun := TKamScriptArrayFun}
{$I kambiscriptarrays_implement.inc}

{$define TKamScriptXxxArray := TKamScriptVec3fArray}
{$define TDynXxxArray := TDynVector3SingleArray}
{$define TKamScriptXxxElement := TKamScriptVec3f}
{$define RegisterXxxFunctions := RegisterVec3fFunctions}
{$define TKamScriptXxxArrayFun := TKamScriptArrayFun}
{$I kambiscriptarrays_implement.inc}

{$define TKamScriptXxxArray := TKamScriptVec4fArray}
{$define TDynXxxArray := TDynVector4SingleArray}
{$define TKamScriptXxxElement := TKamScriptVec4f}
{$define RegisterXxxFunctions := RegisterVec4fFunctions}
{$define TKamScriptXxxArrayFun := TKamScriptArrayFun}
{$I kambiscriptarrays_implement.inc}

{$define TKamScriptXxxArray := TKamScriptVec2dArray}
{$define TDynXxxArray := TDynVector2DoubleArray}
{$define TKamScriptXxxElement := TKamScriptVec2d}
{$define RegisterXxxFunctions := RegisterVec2dFunctions}
{$define TKamScriptXxxArrayFun := TKamScriptArrayFun}
{$I kambiscriptarrays_implement.inc}

{$define TKamScriptXxxArray := TKamScriptVec3dArray}
{$define TDynXxxArray := TDynVector3DoubleArray}
{$define TKamScriptXxxElement := TKamScriptVec3d}
{$define RegisterXxxFunctions := RegisterVec3dFunctions}
{$define TKamScriptXxxArrayFun := TKamScriptArrayFun}
{$I kambiscriptarrays_implement.inc}

{$define TKamScriptXxxArray := TKamScriptVec4dArray}
{$define TDynXxxArray := TDynVector4DoubleArray}
{$define TKamScriptXxxElement := TKamScriptVec4d}
{$define RegisterXxxFunctions := RegisterVec4dFunctions}
{$define TKamScriptXxxArrayFun := TKamScriptArrayFun}
{$I kambiscriptarrays_implement.inc}

{$define TKamScriptXxxArray := TKamScriptMatrix3fArray}
{$define TDynXxxArray := TDynMatrix3SingleArray}
{$define TKamScriptXxxElement := TKamScriptMatrix3f}
{$define RegisterXxxFunctions := RegisterMatrix3fFunctions}
{$define TKamScriptXxxArrayFun := TKamScriptArrayFun}
{$I kambiscriptarrays_implement.inc}

{$define TKamScriptXxxArray := TKamScriptMatrix4fArray}
{$define TDynXxxArray := TDynMatrix4SingleArray}
{$define TKamScriptXxxElement := TKamScriptMatrix4f}
{$define RegisterXxxFunctions := RegisterMatrix4fFunctions}
{$define TKamScriptXxxArrayFun := TKamScriptArrayFun}
{$I kambiscriptarrays_implement.inc}

{$define TKamScriptXxxArray := TKamScriptMatrix3dArray}
{$define TDynXxxArray := TDynMatrix3DoubleArray}
{$define TKamScriptXxxElement := TKamScriptMatrix3d}
{$define RegisterXxxFunctions := RegisterMatrix3dFunctions}
{$define TKamScriptXxxArrayFun := TKamScriptArrayFun}
{$I kambiscriptarrays_implement.inc}

{$define TKamScriptXxxArray := TKamScriptMatrix4dArray}
{$define TDynXxxArray := TDynMatrix4DoubleArray}
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
end.
