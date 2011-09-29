{
  Copyright 2008-2011 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ CastleScript vector and matrix types and built-in functions. }
unit CastleScriptVectors;

interface

uses VectorMath, CastleScript;

type
  TKamScriptVec2f = class(TKamScriptValue)
  private
    class procedure HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleSubtract(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleNegate(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleNotEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleMultiply(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleDivide(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleMax(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleMin(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleVector(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorSet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorLength(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorSqrLength(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorDot(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

  private
    FValue: TVector2Single;
    procedure SetValue(const AValue: TVector2Single);
  public
    property Value: TVector2Single read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptVec3f = class(TKamScriptValue)
  private
    class procedure HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleSubtract(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleNegate(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleNotEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleMultiply(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleDivide(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleMax(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleMin(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleVector(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorSet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorLength(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorSqrLength(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorDot(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleVectorCross(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleGrayscale(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

  private
    FValue: TVector3Single;
    procedure SetValue(const AValue: TVector3Single);
  public
    property Value: TVector3Single read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptVec4f = class(TKamScriptValue)
  private
    class procedure HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleSubtract(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleNegate(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleNotEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleMultiply(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleDivide(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleMax(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleMin(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleVector(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorSet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorLength(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorSqrLength(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorDot(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleOrientationFromDirectionUp(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleRotateCore(
      const Rotation: TVector4Single; const Point: TVector3Single;
      var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleRotate(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleOrientationToDirection(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleOrientationToUp(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleSlerp(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

  private
    FValue: TVector4Single;
    procedure SetValue(const AValue: TVector4Single);
  public
    property Value: TVector4Single read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptVec2d = class(TKamScriptValue)
  private
    class procedure HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleSubtract(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleNegate(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleNotEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleMultiply(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleDivide(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleMax(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleMin(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleVector(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorSet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorLength(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorSqrLength(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorDot(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

  private
    FValue: TVector2Double;
    procedure SetValue(const AValue: TVector2Double);
  public
    property Value: TVector2Double read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptVec3d = class(TKamScriptValue)
  private
    class procedure HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleSubtract(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleNegate(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleNotEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleMultiply(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleDivide(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleMax(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleMin(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleVector(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorSet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorLength(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorSqrLength(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorDot(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleVectorCross(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

  private
    FValue: TVector3Double;
    procedure SetValue(const AValue: TVector3Double);
  public
    property Value: TVector3Double read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptVec4d = class(TKamScriptValue)
  private
    class procedure HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleSubtract(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleNegate(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleNotEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleMultiply(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleDivide(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleMax(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleMin(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleVector(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorSet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorLength(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorSqrLength(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorDot(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

  private
    FValue: TVector4Double;
    procedure SetValue(const AValue: TVector4Double);
  public
    property Value: TVector4Double read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptMatrix3f = class(TKamScriptValue)
  private
    class procedure HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleSubtract(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleNegate(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleMultiply(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleDivide(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleNotEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleMatrix(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleMatrixGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleMatrixSet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleMatrixGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

  private
    FValue: TMatrix3Single;
    procedure SetValue(const AValue: TMatrix3Single);
  public
    property Value: TMatrix3Single read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptMatrix4f = class(TKamScriptValue)
  private
    class procedure HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleSubtract(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleNegate(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleMultiply(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleDivide(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleNotEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleMatrix(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleMatrixGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleMatrixSet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleMatrixGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

  private
    FValue: TMatrix4Single;
    procedure SetValue(const AValue: TMatrix4Single);
  public
    property Value: TMatrix4Single read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptMatrix3d = class(TKamScriptValue)
  private
    class procedure HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleSubtract(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleNegate(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleMultiply(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleDivide(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleNotEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleMatrix(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleMatrixGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleMatrixSet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleMatrixGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

  private
    FValue: TMatrix3Double;
    procedure SetValue(const AValue: TMatrix3Double);
  public
    property Value: TMatrix3Double read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptMatrix4d = class(TKamScriptValue)
  private
    class procedure HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleSubtract(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleNegate(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleMultiply(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleDivide(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleNotEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleMatrix(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleMatrixGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleMatrixSet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleMatrixGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

  private
    FValue: TMatrix4Double;
    procedure SetValue(const AValue: TMatrix4Double);
  public
    property Value: TMatrix4Double read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptVector = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptVectorD = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptVectorGet = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptVectorSet = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
    class function ArgumentMustBeAssignable(const Index: Integer): boolean; override;
  end;

  TKamScriptVectorGetCount = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptVectorLength = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptVectorSqrLength = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptVectorDot = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptVectorCross = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptGrayscale = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptOrientationFromDirectionUp = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptRotate = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptOrientationToDirection = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptOrientationToUp = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptSlerp = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptMatrixFun = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptMatrixGet = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptMatrixSet = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
    class function ArgumentMustBeAssignable(const Index: Integer): boolean; override;
  end;

  TKamScriptMatrixGetCount = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

implementation

uses CastleScriptCoreFunctions, CastleUtils, CastleWarnings, Cameras, Quaternions;

{ Single-precision vectors --------------------------------------------------- }

{$define TKamScriptVectorFunXxx := TKamScriptVector}

{$define VectorGetCount := 2}
{$define TKamScriptVecXx := TKamScriptVec2f}
{$define TVectorXxx := TVector2Single}
{$define RegisterVecXxFunctions := RegisterVec2fFunctions}
{$I castlescriptvectors_implement_vector.inc}

{$define VectorGetCount := 3}
{$define TKamScriptVecXx := TKamScriptVec3f}
{$define TVectorXxx := TVector3Single}
{$define RegisterVecXxFunctions := RegisterVec3fFunctions}
{$I castlescriptvectors_implement_vector.inc}

{$define VectorGetCount := 4}
{$define TKamScriptVecXx := TKamScriptVec4f}
{$define TVectorXxx := TVector4Single}
{$define RegisterVecXxFunctions := RegisterVec4fFunctions}
{$I castlescriptvectors_implement_vector.inc}

class procedure TKamScriptVec3f.HandleVectorCross(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptVec3f);
  TKamScriptVec3f(AResult).Value :=
    VectorProduct( TKamScriptVec3f(Arguments[0]).Value,
                   TKamScriptVec3f(Arguments[1]).Value );
end;

class procedure TKamScriptVec3f.HandleGrayscale(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value :=
    GrayscaleValue( TKamScriptVec3f(Arguments[0]).Value );
end;

class procedure TKamScriptVec4f.HandleOrientationFromDirectionUp(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  Dir, Up: TVector3Single;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptVec4f);

  Dir := TKamScriptVec3f(Arguments[0]).Value;
  Up := TKamScriptVec3f(Arguments[1]).Value;
  MakeVectorsOrthoOnTheirPlane(Up, Dir);
  { no need to normalize Dir, Up here (CamDirUp2Orient will do it) }

  TKamScriptVec4f(AResult).Value := CamDirUp2Orient(Dir, Up);
end;

class procedure TKamScriptVec4f.HandleRotateCore(
  const Rotation: TVector4Single; const Point: TVector3Single;
  var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  Axis: TVector3Single absolute Rotation;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptVec3f);

  if not ZeroVector(Axis) then
    TKamScriptVec3f(AResult).Value := RotatePointAroundAxisRad(
      Rotation[3], Point, Axis) else
  begin
    { Safeguard against rotation around zero vector, which produces unpredictable
      results (actually, Result would be filled with Nan values).
      VRML spec says that SFRotation should always specify a normalized vector. }
    TKamScriptVec3f(AResult).Value := Point;
    OnWarning(wtMajor, 'CastleScript', 'Rotation around zero vector');
  end;
end;

class procedure TKamScriptVec4f.HandleRotate(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  HandleRotateCore(
    TKamScriptVec4f(Arguments[0]).Value,
    TKamScriptVec3f(Arguments[1]).Value, AResult, ParentOfResult);
end;

class procedure TKamScriptVec4f.HandleOrientationToDirection(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  HandleRotateCore(
    TKamScriptVec4f(Arguments[0]).Value,
    DefaultCameraDirection, AResult, ParentOfResult);
end;

class procedure TKamScriptVec4f.HandleOrientationToUp(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  HandleRotateCore(
    TKamScriptVec4f(Arguments[0]).Value,
    DefaultCameraUp, AResult, ParentOfResult);
end;

class procedure TKamScriptVec4f.HandleSlerp(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptVec4f);
  TKamScriptVec4f(AResult).Value := SLerp(
    TKamScriptFloat(Arguments[0]).Value,
    TKamScriptVec4f(Arguments[1]).Value,
    TKamScriptVec4f(Arguments[2]).Value);
end;

{ Double-precision vectors --------------------------------------------------- }

{$define TKamScriptVectorFunXxx := TKamScriptVectorD}

{$define VectorGetCount := 2}
{$define TKamScriptVecXx := TKamScriptVec2d}
{$define TVectorXxx := TVector2Double}
{$define RegisterVecXxFunctions := RegisterVec2dFunctions}
{$I castlescriptvectors_implement_vector.inc}

{$define VectorGetCount := 3}
{$define TKamScriptVecXx := TKamScriptVec3d}
{$define TVectorXxx := TVector3Double}
{$define RegisterVecXxFunctions := RegisterVec3dFunctions}
{$I castlescriptvectors_implement_vector.inc}

{$define VectorGetCount := 4}
{$define TKamScriptVecXx := TKamScriptVec4d}
{$define TVectorXxx := TVector4Double}
{$define RegisterVecXxFunctions := RegisterVec4dFunctions}
{$I castlescriptvectors_implement_vector.inc}

class procedure TKamScriptVec3d.HandleVectorCross(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptVec3d);
  TKamScriptVec3d(AResult).Value :=
    VectorProduct( TKamScriptVec3d(Arguments[0]).Value,
                   TKamScriptVec3d(Arguments[1]).Value );
end;


{ Matrices ------------------------------------------------------------------- }

{$define MatrixGetCount := 3}
{$define TKamScriptVecXx := TKamScriptVec3f}
{$define TKamScriptMatrixXx := TKamScriptMatrix3f}
{$define TMatrixXxx := TMatrix3Single}
{$define RegisterMatrixXxFunctions := RegisterMatrix3fFunctions}
{$I castlescriptvectors_implement_matrix.inc}

{$define MatrixGetCount := 4}
{$define TKamScriptVecXx := TKamScriptVec4f}
{$define TKamScriptMatrixXx := TKamScriptMatrix4f}
{$define TMatrixXxx := TMatrix4Single}
{$define RegisterMatrixXxFunctions := RegisterMatrix4fFunctions}
{$I castlescriptvectors_implement_matrix.inc}

{$define MatrixGetCount := 3}
{$define TKamScriptVecXx := TKamScriptVec3d}
{$define TKamScriptMatrixXx := TKamScriptMatrix3d}
{$define TMatrixXxx := TMatrix3Double}
{$define RegisterMatrixXxFunctions := RegisterMatrix3dFunctions}
{$I castlescriptvectors_implement_matrix.inc}

{$define MatrixGetCount := 4}
{$define TKamScriptVecXx := TKamScriptVec4d}
{$define TKamScriptMatrixXx := TKamScriptMatrix4d}
{$define TMatrixXxx := TMatrix4Double}
{$define RegisterMatrixXxFunctions := RegisterMatrix4dFunctions}
{$I castlescriptvectors_implement_matrix.inc}

{ TKamScriptFunction descendants --------------------------------------------- }

class function TKamScriptVector.ShortName: string;
begin
  Result := 'vector';
end;

class function TKamScriptVectorD.ShortName: string;
begin
  Result := 'vector_d';
end;

class function TKamScriptVectorGet.ShortName: string;
begin
  Result := 'vector_get';
end;

class function TKamScriptVectorSet.ShortName: string;
begin
  Result := 'vector_set';
end;

class function TKamScriptVectorSet.ArgumentMustBeAssignable(const Index: Integer): boolean;
begin
  Result := Index = 0;
end;

class function TKamScriptVectorGetCount.ShortName: string;
begin
  Result := 'vector_get_count';
end;

class function TKamScriptVectorLength.ShortName: string;
begin
  Result := 'vector_length';
end;

class function TKamScriptVectorSqrLength.ShortName: string;
begin
  Result := 'vector_sqr_length';
end;

class function TKamScriptVectorDot.ShortName: string;
begin
  Result := 'vector_dot';
end;

class function TKamScriptVectorCross.ShortName: string;
begin
  Result := 'vector_cross';
end;

class function TKamScriptGrayscale.ShortName: string;
begin
  Result := 'grayscale';
end;

class function TKamScriptOrientationFromDirectionUp.ShortName: string;
begin
  Result := 'orientation_from_direction_up';
end;

class function TKamScriptRotate.ShortName: string;
begin
  Result := 'rotate';
end;

class function TKamScriptOrientationToDirection.ShortName: string;
begin
  Result := 'orientation_to_direction';
end;

class function TKamScriptOrientationToUp.ShortName: string;
begin
  Result := 'orientation_to_up';
end;

class function TKamScriptSlerp.ShortName: string;
begin
  Result := 'slerp';
end;

{ matrix functions ----------------------------------------------------------- }

class function TKamScriptMatrixFun.ShortName: string;
begin
  Result := 'matrix';
end;

class function TKamScriptMatrixGet.ShortName: string;
begin
  Result := 'matrix_get';
end;

class function TKamScriptMatrixSet.ShortName: string;
begin
  Result := 'matrix_set';
end;

class function TKamScriptMatrixSet.ArgumentMustBeAssignable(const Index: Integer): boolean;
begin
  Result := Index = 0;
end;

class function TKamScriptMatrixGetCount.ShortName: string;
begin
  Result := 'matrix_get_count';
end;

{ unit init/fini ------------------------------------------------------------- }

initialization
  RegisterVec2fFunctions;
  RegisterVec3fFunctions;
  RegisterVec4fFunctions;

  FunctionHandlers.RegisterHandler(@TKamScriptVec3f(nil).HandleVectorCross, TKamScriptVectorCross, [TKamScriptVec3f, TKamScriptVec3f], false);
  FunctionHandlers.RegisterHandler(@TKamScriptVec3f(nil).HandleGrayscale, TKamScriptGrayscale, [TKamScriptVec3f], false);

  FunctionHandlers.RegisterHandler(@TKamScriptVec4f(nil).HandleOrientationFromDirectionUp, TKamScriptOrientationFromDirectionUp, [TKamScriptVec3f, TKamScriptVec3f], false);
  FunctionHandlers.RegisterHandler(@TKamScriptVec4f(nil).HandleRotate, TKamScriptRotate, [TKamScriptVec4f, TKamScriptVec3f], false);
  FunctionHandlers.RegisterHandler(@TKamScriptVec4f(nil).HandleOrientationToDirection, TKamScriptOrientationToDirection, [TKamScriptVec4f], false);
  FunctionHandlers.RegisterHandler(@TKamScriptVec4f(nil).HandleOrientationToUp, TKamScriptOrientationToUp, [TKamScriptVec4f], false);
  FunctionHandlers.RegisterHandler(@TKamScriptVec4f(nil).HandleSlerp, TKamScriptSlerp, [TKamScriptFloat, TKamScriptVec4f, TKamScriptVec4f], false);

  RegisterVec2dFunctions;
  RegisterVec3dFunctions;
  RegisterVec4dFunctions;

  FunctionHandlers.RegisterHandler(@TKamScriptVec3d(nil).HandleVectorCross, TKamScriptVectorCross, [TKamScriptVec3d, TKamScriptVec3d], false);

  RegisterMatrix3fFunctions;
  RegisterMatrix4fFunctions;
  RegisterMatrix3dFunctions;
  RegisterMatrix4dFunctions;
end.
