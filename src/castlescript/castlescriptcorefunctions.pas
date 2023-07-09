{
  Copyright 2001-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ CastleScript built-in simple functions on four "core" types. }
unit CastleScriptCoreFunctions;

{$I castleconf.inc}

interface

uses CastleScript;

type
  TCasScriptAdd = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TCasScriptSubtract = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TCasScriptMultiply = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TCasScriptFloatDivide = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TCasScriptIntDivide = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TCasScriptNegate = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TCasScriptModulo = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TCasScriptSin = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TCasScriptCos = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TCasScriptTan = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TCasScriptCotan = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TCasScriptArcSin = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TCasScriptArcCos = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TCasScriptArcTan = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TCasScriptArcCotan = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TCasScriptSinh = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TCasScriptCosh = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TCasScriptTanh = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TCasScriptCotanh = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TCasScriptLog2 = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TCasScriptLn = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TCasScriptLog = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TCasScriptPower2 = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TCasScriptExp = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TCasScriptPower = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TCasScriptSqr = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TCasScriptSqrt = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TCasScriptMax = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptMin = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptSgn = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TCasScriptAbs = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TCasScriptCeil = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptFloor = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptRound = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptGreater = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TCasScriptLesser = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TCasScriptGreaterEq = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TCasScriptLesserEq = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TCasScriptEqual = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TCasScriptNotEqual = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TCasScriptOr = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TCasScriptAnd = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TCasScriptNot = class(TCasScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TCasScriptInt = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptFloatFun = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptBool = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptStringFun = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptWriteln = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptCharacterFromCode = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptRandom = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptLerp = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  { CastleScript function @code(shortcut),
    see [https://castle-engine.io/castle_script.php#function_shortcut]. }
  TCasScriptShortcut = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
    class procedure Handle(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
  end;

  { CastleScript function @code(deg) that converts degrees to radians. }
  TCasScriptDeg = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
    class procedure Handle(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
  end;

implementation

uses SysUtils, Math,
  CastleInputs, CastleUtils;

class function TCasScriptAdd.Name: string;
begin
  Result := 'add (+)';
end;

class function TCasScriptAdd.ShortName: string;
begin
  Result := '';
end;

class function TCasScriptAdd.InfixOperatorName: string;
begin
  Result := '+';
end;

class function TCasScriptSubtract.Name: string;
begin
  Result := 'subtract (-)';
end;

class function TCasScriptSubtract.ShortName: string;
begin
  Result := '';
end;

class function TCasScriptSubtract.InfixOperatorName: string;
begin
  Result := '-';
end;

class function TCasScriptMultiply.Name: string;
begin
  Result := 'multiply (*)';
end;

class function TCasScriptMultiply.ShortName: string;
begin
  Result := '';
end;

class function TCasScriptMultiply.InfixOperatorName: string;
begin
  Result := '*';
end;

class function TCasScriptFloatDivide.Name: string;
begin
  Result := 'float division (/)';
end;

class function TCasScriptFloatDivide.ShortName: string;
begin
  Result := '';
end;

class function TCasScriptFloatDivide.InfixOperatorName: string;
begin
  Result := '/';
end;

class function TCasScriptIntDivide.Name: string;
begin
  Result := 'int division (div)';
end;

class function TCasScriptIntDivide.ShortName: string;
begin
  Result := '';
end;

class function TCasScriptIntDivide.InfixOperatorName: string;
begin
  Result := 'div';
end;

class function TCasScriptNegate.Name: string;
begin
  Result := 'negate (unary -)';
end;

class function TCasScriptNegate.ShortName: string;
begin
  Result := '';
end;

class function TCasScriptModulo.Name: string;
begin
  Result := 'modulo (%)';
end;

class function TCasScriptModulo.ShortName: string;
begin
  Result := '';
end;

class function TCasScriptModulo.InfixOperatorName: string;
begin
  Result := '%';
end;

class function TCasScriptSin.Name: string;
begin
  Result := 'sinus';
end;

class function TCasScriptSin.ShortName: string;
begin
  Result := 'Sin';
end;

class function TCasScriptCos.Name: string;
begin
  Result := 'cosinus';
end;

class function TCasScriptCos.ShortName: string;
begin
  Result := 'Cos';
end;

class function TCasScriptTan.Name: string;
begin
  Result := 'tangens';
end;

class function TCasScriptTan.ShortName: string;
begin
  Result := 'Tan';
end;

class function TCasScriptCotan.Name: string;
begin
  Result := 'cotangens';
end;

class function TCasScriptCotan.ShortName: string;
begin
  Result := 'Cotan';
end;

class function TCasScriptArcSin.Name: string;
begin
  Result := 'arcSinus';
end;

class function TCasScriptArcSin.ShortName: string;
begin
  Result := 'ArcSin';
end;

class function TCasScriptArcCos.Name: string;
begin
  Result := 'arcCosinus';
end;

class function TCasScriptArcCos.ShortName: string;
begin
  Result := 'ArcCos';
end;

class function TCasScriptArcTan.Name: string;
begin
  Result := 'arcTangens';
end;

class function TCasScriptArcTan.ShortName: string;
begin
  Result := 'ArcTan';
end;

class function TCasScriptArcCotan.Name: string;
begin
  Result := 'arcCotangens';
end;

class function TCasScriptArcCotan.ShortName: string;
begin
  Result := 'ArcCotan';
end;

class function TCasScriptSinh.Name: string;
begin
  Result := 'sinh';
end;

class function TCasScriptSinh.ShortName: string;
begin
  Result := 'Sinh';
end;

class function TCasScriptCosh.Name: string;
begin
  Result := 'cosh';
end;

class function TCasScriptCosh.ShortName: string;
begin
  Result := 'Cosh';
end;

class function TCasScriptTanh.Name: string;
begin
  Result := 'tanh';
end;

class function TCasScriptTanh.ShortName: string;
begin
  Result := 'Tanh';
end;

class function TCasScriptCotanh.Name: string;
begin
  Result := 'cotanh';
end;

class function TCasScriptCotanh.ShortName: string;
begin
  Result := 'Cotanh';
end;

class function TCasScriptLog2.Name: string;
begin
  Result := 'logarithm (base 2)';
end;

class function TCasScriptLog2.ShortName: string;
begin
  Result := 'Log2';
end;

class function TCasScriptLn.Name: string;
begin
  Result := 'logarithm (base e)';
end;

class function TCasScriptLn.ShortName: string;
begin
  Result := 'Ln';
end;

class function TCasScriptLog.Name: string;
begin
  Result := 'logarithm';
end;

class function TCasScriptLog.ShortName: string;
begin
  Result := 'Log';
end;

class function TCasScriptPower2.Name: string;
begin
  Result := 'power (base 2)';
end;

class function TCasScriptPower2.ShortName: string;
begin
  Result := 'Power2';
end;

class function TCasScriptExp.Name: string;
begin
  Result := 'power (base enat)';
end;

class function TCasScriptExp.ShortName: string;
begin
  Result := 'Exp';
end;

class function TCasScriptPower.Name: string;
begin
  Result := 'power';
end;

class function TCasScriptPower.ShortName: string;
begin
  Result := 'Power';
end;

class function TCasScriptPower.InfixOperatorName: string;
begin
  Result := '^';
end;

class function TCasScriptSqr.Name: string;
begin
  Result := 'sqr (square)';
end;

class function TCasScriptSqr.ShortName: string;
begin
  Result := 'Sqr';
end;

class function TCasScriptSqrt.Name: string;
begin
  Result := 'sqrt (square root)';
end;

class function TCasScriptSqrt.ShortName: string;
begin
  Result := 'Sqrt';
end;

class function TCasScriptMax.ShortName: string;
begin
  Result := 'max';
end;

class function TCasScriptMin.ShortName: string;
begin
  Result := 'min';
end;

class function TCasScriptSgn.Name: string;
begin
  Result := 'signum';
end;

class function TCasScriptSgn.ShortName: string;
begin
  Result := 'Sgn';
end;

class function TCasScriptAbs.Name: string;
begin
  Result := 'abs (absolute value)';
end;

class function TCasScriptAbs.ShortName: string;
begin
  Result := 'Abs';
end;

class function TCasScriptCeil.ShortName: string;
begin
  Result := 'Ceil';
end;

class function TCasScriptFloor.ShortName: string;
begin
  Result := 'Floor';
end;

class function TCasScriptRound.ShortName: string;
begin
  Result := 'round';
end;

class function TCasScriptGreater.Name: string;
begin
  Result := 'greater (>)';
end;

class function TCasScriptGreater.ShortName: string;
begin
  Result := '';
end;

class function TCasScriptGreater.InfixOperatorName: string;
begin
  Result := '>';
end;

class function TCasScriptLesser.Name: string;
begin
  Result := 'lesser (<)';
end;

class function TCasScriptLesser.ShortName: string;
begin
  Result := '';
end;

class function TCasScriptLesser.InfixOperatorName: string;
begin
  Result := '<';
end;

class function TCasScriptGreaterEq.Name: string;
begin
  Result := 'greater/equal (>=)';
end;

class function TCasScriptGreaterEq.ShortName: string;
begin
  Result := '';
end;

class function TCasScriptGreaterEq.InfixOperatorName: string;
begin
  Result := '>=';
end;

class function TCasScriptLesserEq.Name: string;
begin
  Result := 'lesser/equal (<=)';
end;

class function TCasScriptLesserEq.ShortName: string;
begin
  Result := '';
end;

class function TCasScriptLesserEq.InfixOperatorName: string;
begin
  Result := '<=';
end;

class function TCasScriptEqual.Name: string;
begin
  Result := 'equal (=)';
end;

class function TCasScriptEqual.ShortName: string;
begin
  Result := '';
end;

class function TCasScriptEqual.InfixOperatorName: string;
begin
  Result := '=';
end;

class function TCasScriptNotEqual.Name: string;
begin
  Result := 'not equal (<>)';
end;

class function TCasScriptNotEqual.ShortName: string;
begin
  Result := '';
end;

class function TCasScriptNotEqual.InfixOperatorName: string;
begin
  Result := '<>';
end;

class function TCasScriptOr.Name: string;
begin
  Result := 'or (alternative)';
end;

class function TCasScriptOr.ShortName: string;
begin
  Result := 'Or';
end;

class function TCasScriptAnd.Name: string;
begin
  Result := 'and (conjunction)';
end;

class function TCasScriptAnd.ShortName: string;
begin
  Result := 'And';
end;

class function TCasScriptNot.Name: string;
begin
  Result := 'not (logical negation)';
end;

class function TCasScriptNot.ShortName: string;
begin
  Result := 'Not';
end;

class function TCasScriptInt.ShortName: string;
begin
  Result := 'int';
end;

class function TCasScriptFloatFun.ShortName: string;
begin
  Result := 'float';
end;

class function TCasScriptBool.ShortName: string;
begin
  Result := 'bool';
end;

class function TCasScriptStringFun.ShortName: string;
begin
  Result := 'string';
end;

class function TCasScriptWriteln.ShortName: string;
begin
  Result := 'writeln';
end;

class function TCasScriptCharacterFromCode.ShortName: string;
begin
  Result := 'character_from_code';
end;

class function TCasScriptRandom.ShortName: string;
begin
  Result := 'random';
end;

class function TCasScriptLerp.ShortName: string;
begin
  Result := 'lerp';
end;

{ TCasScriptShortcut --------------------------------------------------------- }

class function TCasScriptShortcut.ShortName: string;
begin
  Result := 'shortcut';
end;

class procedure TCasScriptShortcut.Handle(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  N: string;
  I: TInputShortcut;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptString);
  N := TCasScriptString(Arguments[0]).Value;
  if InputsAll <> nil then
  begin
    I := InputsAll.FindName(N);
    if I <> nil then
      TCasScriptString(AResult).Value := I.Description else
      TCasScriptString(AResult).Value := Format('(shortcut name "%s" undefined)', [N]);
  end else
    TCasScriptString(AResult).Value := 'input names not available (finalization of CastleInputs unit is already done)';
end;

{ TCasScriptDeg --------------------------------------------------------- }

class function TCasScriptDeg.ShortName: string;
begin
  Result := 'deg';
end;

class procedure TCasScriptDeg.Handle(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  Param: Float;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptFloat);
  Param := TCasScriptFloat(Arguments[0]).Value;
  TCasScriptFloat(AResult).Value := DegToRad(Param);
end;

end.
