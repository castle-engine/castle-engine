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

{ CastleScript vector and matrix types and built-in functions. }
unit CastleScriptVectors;

{$I castleconf.inc}

interface

uses CastleVectors, CastleScript;

type
  TCasScriptVec2f = class(TCasScriptValue)
  private
    class procedure HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleSubtract(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleNegate(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleNotEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class procedure HandleMultiply(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleDivide(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleLerp(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class procedure HandleMax(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleMin(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class procedure HandleVector(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorSet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorLength(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorSqrLength(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorDot(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

  private
    FValue: TVector2;
    procedure SetValue(const AValue: TVector2);
  public
    property Value: TVector2 read FValue write SetValue;

    procedure AssignValue(Source: TCasScriptValue); override;
  end;

  TCasScriptVec3f = class(TCasScriptValue)
  private
    class procedure HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleSubtract(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleNegate(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleNotEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class procedure HandleMultiply(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleDivide(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleLerp(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class procedure HandleMax(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleMin(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class procedure HandleVector(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorSet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorLength(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorSqrLength(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorDot(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class procedure HandleVectorCross(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleGrayscale(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

  private
    FValue: TVector3;
    procedure SetValue(const AValue: TVector3);
  public
    property Value: TVector3 read FValue write SetValue;

    procedure AssignValue(Source: TCasScriptValue); override;
  end;

  TCasScriptVec4f = class(TCasScriptValue)
  private
    class procedure HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleSubtract(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleNegate(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleNotEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class procedure HandleMultiply(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleDivide(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleLerp(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class procedure HandleMax(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleMin(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class procedure HandleVector(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorSet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorLength(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorSqrLength(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorDot(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class procedure HandleOrientationFromDirectionUp(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleRotateCore(
      const Rotation: TVector4; const Point: TVector3;
      var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleRotate(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleOrientationToDirection(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleOrientationToUp(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleSlerp(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

  private
    FValue: TVector4;
    procedure SetValue(const AValue: TVector4);
  public
    property Value: TVector4 read FValue write SetValue;

    procedure AssignValue(Source: TCasScriptValue); override;
  end;

  TCasScriptMatrix3f = class(TCasScriptValue)
  private
    class procedure HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleSubtract(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleNegate(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleMultiply(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleDivide(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleLerp(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleNotEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class procedure HandleMatrix(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleMatrixGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleMatrixSet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleMatrixGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

  private
    FValue: TMatrix3;
    procedure SetValue(const AValue: TMatrix3);
  public
    property Value: TMatrix3 read FValue write SetValue;

    procedure AssignValue(Source: TCasScriptValue); override;
  end;

  TCasScriptMatrix4f = class(TCasScriptValue)
  private
    class procedure HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleSubtract(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleNegate(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleMultiply(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleDivide(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleLerp(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleNotEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class procedure HandleMatrix(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleMatrixGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleMatrixSet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleMatrixGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

  private
    FValue: TMatrix4;
    procedure SetValue(const AValue: TMatrix4);
  public
    property Value: TMatrix4 read FValue write SetValue;

    procedure AssignValue(Source: TCasScriptValue); override;
  end;

  {$ifdef CASTLE_HAS_DOUBLE_PRECISION}
  TCasScriptVec2d = class(TCasScriptValue)
  private
    class procedure HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleSubtract(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleNegate(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleNotEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class procedure HandleMultiply(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleDivide(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleLerp(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class procedure HandleMax(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleMin(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class procedure HandleVector(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorSet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorLength(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorSqrLength(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorDot(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

  private
    FValue: TVector2Double;
    procedure SetValue(const AValue: TVector2Double);
  public
    property Value: TVector2Double read FValue write SetValue;

    procedure AssignValue(Source: TCasScriptValue); override;
  end;

  TCasScriptVec3d = class(TCasScriptValue)
  private
    class procedure HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleSubtract(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleNegate(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleNotEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class procedure HandleMultiply(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleDivide(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleLerp(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class procedure HandleMax(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleMin(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class procedure HandleVector(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorSet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorLength(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorSqrLength(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorDot(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class procedure HandleVectorCross(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

  private
    FValue: TVector3Double;
    procedure SetValue(const AValue: TVector3Double);
  public
    property Value: TVector3Double read FValue write SetValue;

    procedure AssignValue(Source: TCasScriptValue); override;
  end;

  TCasScriptVec4d = class(TCasScriptValue)
  private
    class procedure HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleSubtract(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleNegate(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleNotEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class procedure HandleMultiply(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleDivide(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleLerp(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class procedure HandleMax(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleMin(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class procedure HandleVector(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorSet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorLength(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorSqrLength(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorDot(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

  private
    FValue: TVector4Double;
    procedure SetValue(const AValue: TVector4Double);
  public
    property Value: TVector4Double read FValue write SetValue;

    procedure AssignValue(Source: TCasScriptValue); override;
  end;

  TCasScriptMatrix3Double = class(TCasScriptValue)
  private
    class procedure HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleSubtract(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleNegate(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleMultiply(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleDivide(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleLerp(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleNotEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class procedure HandleMatrix(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleMatrixGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleMatrixSet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleMatrixGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

  private
    FValue: TMatrix3Double;
    procedure SetValue(const AValue: TMatrix3Double);
  public
    property Value: TMatrix3Double read FValue write SetValue;

    procedure AssignValue(Source: TCasScriptValue); override;
  end;

  TCasScriptMatrix4Double = class(TCasScriptValue)
  private
    class procedure HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleSubtract(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleNegate(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleMultiply(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleDivide(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleLerp(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleNotEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class procedure HandleMatrix(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleMatrixGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleMatrixSet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleMatrixGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

  private
    FValue: TMatrix4Double;
    procedure SetValue(const AValue: TMatrix4Double);
  public
    property Value: TMatrix4Double read FValue write SetValue;

    procedure AssignValue(Source: TCasScriptValue); override;
  end;
  {$endif CASTLE_HAS_DOUBLE_PRECISION}

  TCasScriptVector = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptVectorD = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptVectorGet = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptVectorSet = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
    class function ArgumentMustBeAssignable(const Index: Integer): boolean; override;
  end;

  TCasScriptVectorGetCount = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptVectorLength = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptVectorSqrLength = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptVectorDot = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptVectorCross = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptGrayscale = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptOrientationFromDirectionUp = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptRotate = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptOrientationToDirection = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptOrientationToUp = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptSlerp = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptMatrixFun = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptMatrixGet = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptMatrixSet = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
    class function ArgumentMustBeAssignable(const Index: Integer): boolean; override;
  end;

  TCasScriptMatrixGetCount = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

implementation

uses CastleScriptCoreFunctions, CastleUtils, CastleLog, CastleCameras, CastleQuaternions,
  CastleColors;

{ Single-precision vectors --------------------------------------------------- }

{$define TCasScriptVectorFunXxx := TCasScriptVector}

{$define VectorGetCount := 2}
{$define TCasScriptVecXx := TCasScriptVec2f}
{$define TVectorXxx := TVector2}
{$define RegisterVecXxFunctions := RegisterVec2fFunctions}
{$I castlescriptvectors_implement_vector.inc}

{$define VectorGetCount := 3}
{$define TCasScriptVecXx := TCasScriptVec3f}
{$define TVectorXxx := TVector3}
{$define RegisterVecXxFunctions := RegisterVec3fFunctions}
{$I castlescriptvectors_implement_vector.inc}

{$define VectorGetCount := 4}
{$define TCasScriptVecXx := TCasScriptVec4f}
{$define TVectorXxx := TVector4}
{$define RegisterVecXxFunctions := RegisterVec4fFunctions}
{$I castlescriptvectors_implement_vector.inc}

class procedure TCasScriptVec3f.HandleVectorCross(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptVec3f);
  TCasScriptVec3f(AResult).Value :=
    VectorProduct( TCasScriptVec3f(Arguments[0]).Value,
                   TCasScriptVec3f(Arguments[1]).Value );
end;

class procedure TCasScriptVec3f.HandleGrayscale(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptFloat);
  TCasScriptFloat(AResult).Value :=
    GrayscaleValue( TCasScriptVec3f(Arguments[0]).Value );
end;

class procedure TCasScriptVec4f.HandleOrientationFromDirectionUp(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  Dir, Up: TVector3;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptVec4f);

  Dir := TCasScriptVec3f(Arguments[0]).Value;
  Up := TCasScriptVec3f(Arguments[1]).Value;
  MakeVectorsOrthoOnTheirPlane(Up, Dir);
  { no need to normalize Dir, Up here (CamDirUp2Orient will do it) }

  TCasScriptVec4f(AResult).Value := CamDirUp2Orient(Dir, Up);
end;

class procedure TCasScriptVec4f.HandleRotateCore(
  const Rotation: TVector4; const Point: TVector3;
  var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  Axis: TVector3 absolute Rotation;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptVec3f);

  if not ZeroVector(Axis) then
    TCasScriptVec3f(AResult).Value := RotatePointAroundAxisRad(
      Rotation[3], Point, Axis) else
  begin
    { Safeguard against rotation around zero vector, which produces unpredictable
      results (actually, Result would be filled with Nan values).
      VRML/X3D specs says that SFRotation should always specify a normalized vector. }
    TCasScriptVec3f(AResult).Value := Point;
    WritelnWarning('CastleScript', 'Rotation around zero vector');
  end;
end;

class procedure TCasScriptVec4f.HandleRotate(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  HandleRotateCore(
    TCasScriptVec4f(Arguments[0]).Value,
    TCasScriptVec3f(Arguments[1]).Value, AResult, ParentOfResult);
end;

class procedure TCasScriptVec4f.HandleOrientationToDirection(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  HandleRotateCore(
    TCasScriptVec4f(Arguments[0]).Value,
    DefaultCameraDirection, AResult, ParentOfResult);
end;

class procedure TCasScriptVec4f.HandleOrientationToUp(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  HandleRotateCore(
    TCasScriptVec4f(Arguments[0]).Value,
    DefaultCameraUp, AResult, ParentOfResult);
end;

class procedure TCasScriptVec4f.HandleSlerp(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptVec4f);
  TCasScriptVec4f(AResult).Value := SLerp(
    TCasScriptFloat(Arguments[0]).Value,
    TCasScriptVec4f(Arguments[1]).Value,
    TCasScriptVec4f(Arguments[2]).Value);
end;

{ Double-precision vectors --------------------------------------------------- }

{$ifdef CASTLE_HAS_DOUBLE_PRECISION}

  {$define TCasScriptVectorFunXxx := TCasScriptVectorD}

  {$define VectorGetCount := 2}
  {$define TCasScriptVecXx := TCasScriptVec2d}
  {$define TVectorXxx := TVector2Double}
  {$define RegisterVecXxFunctions := RegisterVec2dFunctions}
  {$I castlescriptvectors_implement_vector.inc}

  {$define VectorGetCount := 3}
  {$define TCasScriptVecXx := TCasScriptVec3d}
  {$define TVectorXxx := TVector3Double}
  {$define RegisterVecXxFunctions := RegisterVec3dFunctions}
  {$I castlescriptvectors_implement_vector.inc}

  {$define VectorGetCount := 4}
  {$define TCasScriptVecXx := TCasScriptVec4d}
  {$define TVectorXxx := TVector4Double}
  {$define RegisterVecXxFunctions := RegisterVec4dFunctions}
  {$I castlescriptvectors_implement_vector.inc}

  class procedure TCasScriptVec3d.HandleVectorCross(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
  begin
    CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptVec3d);
    TCasScriptVec3d(AResult).Value :=
      TVector3Double.CrossProduct(
        TCasScriptVec3d(Arguments[0]).Value,
        TCasScriptVec3d(Arguments[1]).Value );
  end;

{$endif CASTLE_HAS_DOUBLE_PRECISION}

{ Matrices ------------------------------------------------------------------- }

{$define MatrixGetCount := 3}
{$define TCasScriptVecXx := TCasScriptVec3f}
{$define TCasScriptMatrixXx := TCasScriptMatrix3f}
{$define TMatrixXxx := TMatrix3}
{$define TVectorXxx := TVector3}
{$define RegisterMatrixXxFunctions := RegisterMatrix3fFunctions}
{$I castlescriptvectors_implement_matrix.inc}

{$define MatrixGetCount := 4}
{$define TCasScriptVecXx := TCasScriptVec4f}
{$define TCasScriptMatrixXx := TCasScriptMatrix4f}
{$define TMatrixXxx := TMatrix4}
{$define TVectorXxx := TVector4}
{$define RegisterMatrixXxFunctions := RegisterMatrix4fFunctions}
{$I castlescriptvectors_implement_matrix.inc}

{$ifdef CASTLE_HAS_DOUBLE_PRECISION}

  {$define MatrixGetCount := 3}
  {$define TCasScriptVecXx := TCasScriptVec3d}
  {$define TCasScriptMatrixXx := TCasScriptMatrix3Double}
  {$define TMatrixXxx := TMatrix3Double}
  {$define TVectorXxx := TVector3Double}
  {$define RegisterMatrixXxFunctions := RegisterMatrix3DoubleFunctions}
  {$I castlescriptvectors_implement_matrix.inc}

  {$define MatrixGetCount := 4}
  {$define TCasScriptVecXx := TCasScriptVec4d}
  {$define TCasScriptMatrixXx := TCasScriptMatrix4Double}
  {$define TMatrixXxx := TMatrix4Double}
  {$define TVectorXxx := TVector4Double}
  {$define RegisterMatrixXxFunctions := RegisterMatrix4DoubleFunctions}
  {$I castlescriptvectors_implement_matrix.inc}

{$endif CASTLE_HAS_DOUBLE_PRECISION}

{ TCasScriptFunction descendants --------------------------------------------- }

class function TCasScriptVector.ShortName: string;
begin
  Result := 'vector';
end;

class function TCasScriptVectorD.ShortName: string;
begin
  Result := 'vector_d';
end;

class function TCasScriptVectorGet.ShortName: string;
begin
  Result := 'vector_get';
end;

class function TCasScriptVectorSet.ShortName: string;
begin
  Result := 'vector_set';
end;

class function TCasScriptVectorSet.ArgumentMustBeAssignable(const Index: Integer): boolean;
begin
  Result := Index = 0;
end;

class function TCasScriptVectorGetCount.ShortName: string;
begin
  Result := 'vector_get_count';
end;

class function TCasScriptVectorLength.ShortName: string;
begin
  Result := 'vector_length';
end;

class function TCasScriptVectorSqrLength.ShortName: string;
begin
  Result := 'vector_sqr_length';
end;

class function TCasScriptVectorDot.ShortName: string;
begin
  Result := 'vector_dot';
end;

class function TCasScriptVectorCross.ShortName: string;
begin
  Result := 'vector_cross';
end;

class function TCasScriptGrayscale.ShortName: string;
begin
  Result := 'grayscale';
end;

class function TCasScriptOrientationFromDirectionUp.ShortName: string;
begin
  Result := 'orientation_from_direction_up';
end;

class function TCasScriptRotate.ShortName: string;
begin
  Result := 'rotate';
end;

class function TCasScriptOrientationToDirection.ShortName: string;
begin
  Result := 'orientation_to_direction';
end;

class function TCasScriptOrientationToUp.ShortName: string;
begin
  Result := 'orientation_to_up';
end;

class function TCasScriptSlerp.ShortName: string;
begin
  Result := 'slerp';
end;

{ matrix functions ----------------------------------------------------------- }

class function TCasScriptMatrixFun.ShortName: string;
begin
  Result := 'matrix';
end;

class function TCasScriptMatrixGet.ShortName: string;
begin
  Result := 'matrix_get';
end;

class function TCasScriptMatrixSet.ShortName: string;
begin
  Result := 'matrix_set';
end;

class function TCasScriptMatrixSet.ArgumentMustBeAssignable(const Index: Integer): boolean;
begin
  Result := Index = 0;
end;

class function TCasScriptMatrixGetCount.ShortName: string;
begin
  Result := 'matrix_get_count';
end;

{ unit init/fini ------------------------------------------------------------- }

initialization
  RegisterVec2fFunctions;
  RegisterVec3fFunctions;
  RegisterVec4fFunctions;

  FunctionHandlers.RegisterHandler(@TCasScriptVec3f(nil).HandleVectorCross, TCasScriptVectorCross, [TCasScriptVec3f, TCasScriptVec3f], false);
  FunctionHandlers.RegisterHandler(@TCasScriptVec3f(nil).HandleGrayscale, TCasScriptGrayscale, [TCasScriptVec3f], false);

  FunctionHandlers.RegisterHandler(@TCasScriptVec4f(nil).HandleOrientationFromDirectionUp, TCasScriptOrientationFromDirectionUp, [TCasScriptVec3f, TCasScriptVec3f], false);
  FunctionHandlers.RegisterHandler(@TCasScriptVec4f(nil).HandleRotate, TCasScriptRotate, [TCasScriptVec4f, TCasScriptVec3f], false);
  FunctionHandlers.RegisterHandler(@TCasScriptVec4f(nil).HandleOrientationToDirection, TCasScriptOrientationToDirection, [TCasScriptVec4f], false);
  FunctionHandlers.RegisterHandler(@TCasScriptVec4f(nil).HandleOrientationToUp, TCasScriptOrientationToUp, [TCasScriptVec4f], false);
  FunctionHandlers.RegisterHandler(@TCasScriptVec4f(nil).HandleSlerp, TCasScriptSlerp, [TCasScriptFloat, TCasScriptVec4f, TCasScriptVec4f], false);

  RegisterMatrix3fFunctions;
  RegisterMatrix4fFunctions;

  {$ifdef CASTLE_HAS_DOUBLE_PRECISION}
  RegisterVec2dFunctions;
  RegisterVec3dFunctions;
  RegisterVec4dFunctions;

  FunctionHandlers.RegisterHandler(@TCasScriptVec3d(nil).HandleVectorCross, TCasScriptVectorCross, [TCasScriptVec3d, TCasScriptVec3d], false);

  RegisterMatrix3DoubleFunctions;
  RegisterMatrix4DoubleFunctions;
  {$endif CASTLE_HAS_DOUBLE_PRECISION}
end.
