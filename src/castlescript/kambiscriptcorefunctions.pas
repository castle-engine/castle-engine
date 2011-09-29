{
  Copyright 2001-2011 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ KambiScript built-in simple functions on four "core" types. }
unit KambiScriptCoreFunctions;

interface

uses KambiScript;

type
  TKamScriptAdd = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TKamScriptSubtract = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TKamScriptMultiply = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TKamScriptDivide = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TKamScriptNegate = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptModulo = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TKamScriptSin = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptCos = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptTan = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptCotan = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptArcSin = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptArcCos = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptArcTan = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptArcCotan = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptSinh = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptCosh = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptTanh = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptCotanh = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptLog2 = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptLn = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptLog = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptPower2 = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptExp = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptPower = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TKamScriptSqr = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptSqrt = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptMax = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptMin = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptSgn = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptAbs = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptCeil = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptFloor = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptRound = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptGreater = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TKamScriptLesser = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TKamScriptGreaterEq = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TKamScriptLesserEq = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TKamScriptEqual = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TKamScriptNotEqual = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TKamScriptOr = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptAnd = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptNot = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptInt = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptFloatFun = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptBool = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptStringFun = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptWriteln = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptCharacterFromCode = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptRandom = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

implementation

class function TKamScriptAdd.Name: string;
begin
  Result := 'add (+)';
end;

class function TKamScriptAdd.ShortName: string;
begin
  Result := '';
end;

class function TKamScriptAdd.InfixOperatorName: string;
begin
  Result := '+';
end;

class function TKamScriptSubtract.Name: string;
begin
  Result := 'subtract (-)';
end;

class function TKamScriptSubtract.ShortName: string;
begin
  Result := '';
end;

class function TKamScriptSubtract.InfixOperatorName: string;
begin
  Result := '-';
end;

class function TKamScriptMultiply.Name: string;
begin
  Result := 'multiply (*)';
end;

class function TKamScriptMultiply.ShortName: string;
begin
  Result := '';
end;

class function TKamScriptMultiply.InfixOperatorName: string;
begin
  Result := '*';
end;

class function TKamScriptDivide.Name: string;
begin
  Result := 'divide (/)';
end;

class function TKamScriptDivide.ShortName: string;
begin
  Result := '';
end;

class function TKamScriptDivide.InfixOperatorName: string;
begin
  Result := '/';
end;

class function TKamScriptNegate.Name: string;
begin
  Result := 'negate (unary -)';
end;

class function TKamScriptNegate.ShortName: string;
begin
  Result := '';
end;

class function TKamScriptModulo.Name: string;
begin
  Result := 'modulo (%)';
end;

class function TKamScriptModulo.ShortName: string;
begin
  Result := '';
end;

class function TKamScriptModulo.InfixOperatorName: string;
begin
  Result := '%';
end;

class function TKamScriptSin.Name: string;
begin
  Result := 'sinus';
end;

class function TKamScriptSin.ShortName: string;
begin
  Result := 'Sin';
end;

class function TKamScriptCos.Name: string;
begin
  Result := 'cosinus';
end;

class function TKamScriptCos.ShortName: string;
begin
  Result := 'Cos';
end;

class function TKamScriptTan.Name: string;
begin
  Result := 'tangens';
end;

class function TKamScriptTan.ShortName: string;
begin
  Result := 'Tan';
end;

class function TKamScriptCotan.Name: string;
begin
  Result := 'cotangens';
end;

class function TKamScriptCotan.ShortName: string;
begin
  Result := 'Cotan';
end;

class function TKamScriptArcSin.Name: string;
begin
  Result := 'arcSinus';
end;

class function TKamScriptArcSin.ShortName: string;
begin
  Result := 'ArcSin';
end;

class function TKamScriptArcCos.Name: string;
begin
  Result := 'arcCosinus';
end;

class function TKamScriptArcCos.ShortName: string;
begin
  Result := 'ArcCos';
end;

class function TKamScriptArcTan.Name: string;
begin
  Result := 'arcTangens';
end;

class function TKamScriptArcTan.ShortName: string;
begin
  Result := 'ArcTan';
end;

class function TKamScriptArcCotan.Name: string;
begin
  Result := 'arcCotangens';
end;

class function TKamScriptArcCotan.ShortName: string;
begin
  Result := 'ArcCotan';
end;

class function TKamScriptSinh.Name: string;
begin
  Result := 'sinh';
end;

class function TKamScriptSinh.ShortName: string;
begin
  Result := 'Sinh';
end;

class function TKamScriptCosh.Name: string;
begin
  Result := 'cosh';
end;

class function TKamScriptCosh.ShortName: string;
begin
  Result := 'Cosh';
end;

class function TKamScriptTanh.Name: string;
begin
  Result := 'tanh';
end;

class function TKamScriptTanh.ShortName: string;
begin
  Result := 'Tanh';
end;

class function TKamScriptCotanh.Name: string;
begin
  Result := 'cotanh';
end;

class function TKamScriptCotanh.ShortName: string;
begin
  Result := 'Cotanh';
end;

class function TKamScriptLog2.Name: string;
begin
  Result := 'logarithm (base 2)';
end;

class function TKamScriptLog2.ShortName: string;
begin
  Result := 'Log2';
end;

class function TKamScriptLn.Name: string;
begin
  Result := 'logarithm (base e)';
end;

class function TKamScriptLn.ShortName: string;
begin
  Result := 'Ln';
end;

class function TKamScriptLog.Name: string;
begin
  Result := 'logarithm';
end;

class function TKamScriptLog.ShortName: string;
begin
  Result := 'Log';
end;

class function TKamScriptPower2.Name: string;
begin
  Result := 'power (base 2)';
end;

class function TKamScriptPower2.ShortName: string;
begin
  Result := 'Power2';
end;

class function TKamScriptExp.Name: string;
begin
  Result := 'power (base enat)';
end;

class function TKamScriptExp.ShortName: string;
begin
  Result := 'Exp';
end;

class function TKamScriptPower.Name: string;
begin
  Result := 'power';
end;

class function TKamScriptPower.ShortName: string;
begin
  Result := 'Power';
end;

class function TKamScriptPower.InfixOperatorName: string;
begin
  Result := '^';
end;

class function TKamScriptSqr.Name: string;
begin
  Result := 'sqr (square)';
end;

class function TKamScriptSqr.ShortName: string;
begin
  Result := 'Sqr';
end;

class function TKamScriptSqrt.Name: string;
begin
  Result := 'sqrt (square root)';
end;

class function TKamScriptSqrt.ShortName: string;
begin
  Result := 'Sqrt';
end;

class function TKamScriptMax.ShortName: string;
begin
  Result := 'max';
end;

class function TKamScriptMin.ShortName: string;
begin
  Result := 'min';
end;

class function TKamScriptSgn.Name: string;
begin
  Result := 'signum';
end;

class function TKamScriptSgn.ShortName: string;
begin
  Result := 'Sgn';
end;

class function TKamScriptAbs.Name: string;
begin
  Result := 'abs (absolute value)';
end;

class function TKamScriptAbs.ShortName: string;
begin
  Result := 'Abs';
end;

class function TKamScriptCeil.ShortName: string;
begin
  Result := 'Ceil';
end;

class function TKamScriptFloor.ShortName: string;
begin
  Result := 'Floor';
end;

class function TKamScriptRound.ShortName: string;
begin
  Result := 'round';
end;

class function TKamScriptGreater.Name: string;
begin
  Result := 'greater (>)';
end;

class function TKamScriptGreater.ShortName: string;
begin
  Result := '';
end;

class function TKamScriptGreater.InfixOperatorName: string;
begin
  Result := '>';
end;

class function TKamScriptLesser.Name: string;
begin
  Result := 'lesser (<)';
end;

class function TKamScriptLesser.ShortName: string;
begin
  Result := '';
end;

class function TKamScriptLesser.InfixOperatorName: string;
begin
  Result := '<';
end;

class function TKamScriptGreaterEq.Name: string;
begin
  Result := 'greater/equal (>=)';
end;

class function TKamScriptGreaterEq.ShortName: string;
begin
  Result := '';
end;

class function TKamScriptGreaterEq.InfixOperatorName: string;
begin
  Result := '>=';
end;

class function TKamScriptLesserEq.Name: string;
begin
  Result := 'lesser/equal (<=)';
end;

class function TKamScriptLesserEq.ShortName: string;
begin
  Result := '';
end;

class function TKamScriptLesserEq.InfixOperatorName: string;
begin
  Result := '<=';
end;

class function TKamScriptEqual.Name: string;
begin
  Result := 'equal (=)';
end;

class function TKamScriptEqual.ShortName: string;
begin
  Result := '';
end;

class function TKamScriptEqual.InfixOperatorName: string;
begin
  Result := '=';
end;

class function TKamScriptNotEqual.Name: string;
begin
  Result := 'not equal (<>)';
end;

class function TKamScriptNotEqual.ShortName: string;
begin
  Result := '';
end;

class function TKamScriptNotEqual.InfixOperatorName: string;
begin
  Result := '<>';
end;

class function TKamScriptOr.Name: string;
begin
  Result := 'or (alternative)';
end;

class function TKamScriptOr.ShortName: string;
begin
  Result := 'Or';
end;

class function TKamScriptAnd.Name: string;
begin
  Result := 'and (conjunction)';
end;

class function TKamScriptAnd.ShortName: string;
begin
  Result := 'And';
end;

class function TKamScriptNot.Name: string;
begin
  Result := 'not (logical negation)';
end;

class function TKamScriptNot.ShortName: string;
begin
  Result := 'Not';
end;

class function TKamScriptInt.ShortName: string;
begin
  Result := 'int';
end;

class function TKamScriptFloatFun.ShortName: string;
begin
  Result := 'float';
end;

class function TKamScriptBool.ShortName: string;
begin
  Result := 'bool';
end;

class function TKamScriptStringFun.ShortName: string;
begin
  Result := 'string';
end;

class function TKamScriptWriteln.ShortName: string;
begin
  Result := 'writeln';
end;

class function TKamScriptCharacterFromCode.ShortName: string;
begin
  Result := 'character_from_code';
end;

class function TKamScriptRandom.ShortName: string;
begin
  Result := 'random';
end;

end.
