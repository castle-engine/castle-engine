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

  TKamScriptIntegerArray = class(TKamScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    FValue: TDynIntegerArray;
    procedure SetValue(const AValue: TDynIntegerArray);
  public
    constructor Create(const AWriteable: boolean; const AValue: TDynIntegerArray);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Array value. Assigning here makes a @italic(copy) of the array. }
    property Value: TDynIntegerArray read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptSingleArray = class(TKamScriptArray)
  private
    class procedure HandleArrayFun(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

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
  end;

  TKamScriptArrayGet = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptArraySet = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

implementation

uses SysUtils;

{ KambiScript values --------------------------------------------------------- }

{$define TKamScriptXxxArray := TKamScriptIntegerArray}
{$define TDynXxxArray := TDynIntegerArray}
{$define TKamScriptXxxElement := TKamScriptInteger}
{$define RegisterXxxFunctions := RegisterIntegerFunctions}
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

class function TKamScriptArrayGet.ShortName: string;
begin
  Result := 'array_get';
end;

class function TKamScriptArraySet.ShortName: string;
begin
  Result := 'array_set';
end;

initialization
  RegisterIntegerFunctions;
  RegisterSingleFunctions;
  RegisterDoubleFunctions;
  RegisterBooleanFunctions;
  RegisterStringFunctions;
end.
