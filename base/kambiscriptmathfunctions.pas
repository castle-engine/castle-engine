{
  Copyright 2001-2008 Michalis Kamburelis.

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
}

{ KambiScript built-in simple math functions. }
unit KambiScriptMathFunctions;

interface

uses KambiScript;

type
  TKamScriptAdd = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TKamScriptSubtract = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TKamScriptMultiply = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TKamScriptDivide = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TKamScriptNegate = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptModulo = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TKamScriptSin = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptCos = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptTan = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptCotan = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptArcSin = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptArcCos = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptArcTan = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptArcCotan = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptSinh = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptCosh = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptTanh = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptCotanh = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptLog2 = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptLn = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptLog = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptPower2 = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptExp = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptPower = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TKamScriptSqr = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptSqrt = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptSgn = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptAbs = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptCeil = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptFloor = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptGreater = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TKamScriptLesser = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TKamScriptGreaterEq = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TKamScriptLesserEq = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TKamScriptEqual = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TKamScriptNotEqual = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  TKamScriptOr = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptAnd = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptNot = class(TKamScriptFunction)
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
  end;

implementation

class function TKamScriptAdd.AllowedArgumentsCount: Integer;
begin
  Result := -1;
end;

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

class function TKamScriptSubtract.AllowedArgumentsCount: Integer;
begin
  Result := -1;
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

class function TKamScriptMultiply.AllowedArgumentsCount: Integer;
begin
  Result := -1;
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

class function TKamScriptDivide.AllowedArgumentsCount: Integer;
begin
  Result := -1;
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

class function TKamScriptNegate.AllowedArgumentsCount: Integer;
begin
  Result := 1;
end;

class function TKamScriptNegate.Name: string;
begin
  Result := 'negate (unary -)';
end;

class function TKamScriptNegate.ShortName: string;
begin
  Result := '';
end;

class function TKamScriptModulo.AllowedArgumentsCount: Integer;
begin
  Result := 2;
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

class function TKamScriptSin.AllowedArgumentsCount: Integer;
begin
  Result := 1;
end;

class function TKamScriptSin.Name: string;
begin
  Result := 'sinus';
end;

class function TKamScriptSin.ShortName: string;
begin
  Result := 'Sin';
end;

class function TKamScriptCos.AllowedArgumentsCount: Integer;
begin
  Result := 1;
end;

class function TKamScriptCos.Name: string;
begin
  Result := 'cosinus';
end;

class function TKamScriptCos.ShortName: string;
begin
  Result := 'Cos';
end;

class function TKamScriptTan.AllowedArgumentsCount: Integer;
begin
  Result := 1;
end;

class function TKamScriptTan.Name: string;
begin
  Result := 'tangens';
end;

class function TKamScriptTan.ShortName: string;
begin
  Result := 'Tan';
end;

class function TKamScriptCotan.AllowedArgumentsCount: Integer;
begin
  Result := 1;
end;

class function TKamScriptCotan.Name: string;
begin
  Result := 'cotangens';
end;

class function TKamScriptCotan.ShortName: string;
begin
  Result := 'Cotan';
end;

class function TKamScriptArcSin.AllowedArgumentsCount: Integer;
begin
  Result := 1;
end;

class function TKamScriptArcSin.Name: string;
begin
  Result := 'arcSinus';
end;

class function TKamScriptArcSin.ShortName: string;
begin
  Result := 'ArcSin';
end;

class function TKamScriptArcCos.AllowedArgumentsCount: Integer;
begin
  Result := 1;
end;

class function TKamScriptArcCos.Name: string;
begin
  Result := 'arcCosinus';
end;

class function TKamScriptArcCos.ShortName: string;
begin
  Result := 'ArcCos';
end;

class function TKamScriptArcTan.AllowedArgumentsCount: Integer;
begin
  Result := 1;
end;

class function TKamScriptArcTan.Name: string;
begin
  Result := 'arcTangens';
end;

class function TKamScriptArcTan.ShortName: string;
begin
  Result := 'ArcTan';
end;

class function TKamScriptArcCotan.AllowedArgumentsCount: Integer;
begin
  Result := 1;
end;

class function TKamScriptArcCotan.Name: string;
begin
  Result := 'arcCotangens';
end;

class function TKamScriptArcCotan.ShortName: string;
begin
  Result := 'ArcCotan';
end;

class function TKamScriptSinh.AllowedArgumentsCount: Integer;
begin
  Result := 1;
end;

class function TKamScriptSinh.Name: string;
begin
  Result := 'sinh';
end;

class function TKamScriptSinh.ShortName: string;
begin
  Result := 'Sinh';
end;

class function TKamScriptCosh.AllowedArgumentsCount: Integer;
begin
  Result := 1;
end;

class function TKamScriptCosh.Name: string;
begin
  Result := 'cosh';
end;

class function TKamScriptCosh.ShortName: string;
begin
  Result := 'Cosh';
end;

class function TKamScriptTanh.AllowedArgumentsCount: Integer;
begin
  Result := 1;
end;

class function TKamScriptTanh.Name: string;
begin
  Result := 'tanh';
end;

class function TKamScriptTanh.ShortName: string;
begin
  Result := 'Tanh';
end;

class function TKamScriptCotanh.AllowedArgumentsCount: Integer;
begin
  Result := 1;
end;

class function TKamScriptCotanh.Name: string;
begin
  Result := 'cotanh';
end;

class function TKamScriptCotanh.ShortName: string;
begin
  Result := 'Cotanh';
end;

class function TKamScriptLog2.AllowedArgumentsCount: Integer;
begin
  Result := 1;
end;

class function TKamScriptLog2.Name: string;
begin
  Result := 'logarithm (base 2)';
end;

class function TKamScriptLog2.ShortName: string;
begin
  Result := 'Log2';
end;

class function TKamScriptLn.AllowedArgumentsCount: Integer;
begin
  Result := 1;
end;

class function TKamScriptLn.Name: string;
begin
  Result := 'logarithm (base e)';
end;

class function TKamScriptLn.ShortName: string;
begin
  Result := 'Ln';
end;

class function TKamScriptLog.AllowedArgumentsCount: Integer;
begin
  Result := 2;
end;

class function TKamScriptLog.Name: string;
begin
  Result := 'logarithm';
end;

class function TKamScriptLog.ShortName: string;
begin
  Result := 'Log';
end;

class function TKamScriptPower2.AllowedArgumentsCount: Integer;
begin
  Result := 1;
end;

class function TKamScriptPower2.Name: string;
begin
  Result := 'power (base 2)';
end;

class function TKamScriptPower2.ShortName: string;
begin
  Result := 'Power2';
end;

class function TKamScriptExp.AllowedArgumentsCount: Integer;
begin
  Result := 1;
end;

class function TKamScriptExp.Name: string;
begin
  Result := 'power (base enat)';
end;

class function TKamScriptExp.ShortName: string;
begin
  Result := 'Exp';
end;

class function TKamScriptPower.AllowedArgumentsCount: Integer;
begin
  Result := 2;
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

class function TKamScriptSqr.AllowedArgumentsCount: Integer;
begin
  Result := 1;
end;

class function TKamScriptSqr.Name: string;
begin
  Result := 'sqr (square)';
end;

class function TKamScriptSqr.ShortName: string;
begin
  Result := 'Sqr';
end;

class function TKamScriptSqrt.AllowedArgumentsCount: Integer;
begin
  Result := 1;
end;

class function TKamScriptSqrt.Name: string;
begin
  Result := 'sqrt (square root)';
end;

class function TKamScriptSqrt.ShortName: string;
begin
  Result := 'Sqrt';
end;

class function TKamScriptSgn.AllowedArgumentsCount: Integer;
begin
  Result := 1;
end;

class function TKamScriptSgn.Name: string;
begin
  Result := 'signum';
end;

class function TKamScriptSgn.ShortName: string;
begin
  Result := 'Sgn';
end;

class function TKamScriptAbs.AllowedArgumentsCount: Integer;
begin
  Result := 1;
end;

class function TKamScriptAbs.Name: string;
begin
  Result := 'abs (absolute value)';
end;

class function TKamScriptAbs.ShortName: string;
begin
  Result := 'Abs';
end;

class function TKamScriptCeil.AllowedArgumentsCount: Integer;
begin
  Result := 1;
end;

class function TKamScriptCeil.Name: string;
begin
  Result := 'ceil';
end;

class function TKamScriptCeil.ShortName: string;
begin
  Result := 'Ceil';
end;

class function TKamScriptFloor.AllowedArgumentsCount: Integer;
begin
  Result := 1;
end;

class function TKamScriptFloor.Name: string;
begin
  Result := 'floor';
end;

class function TKamScriptFloor.ShortName: string;
begin
  Result := 'Floor';
end;

class function TKamScriptGreater.AllowedArgumentsCount: Integer;
begin
  Result := 2;
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

class function TKamScriptLesser.AllowedArgumentsCount: Integer;
begin
  Result := 2;
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

class function TKamScriptGreaterEq.AllowedArgumentsCount: Integer;
begin
  Result := 2;
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

class function TKamScriptLesserEq.AllowedArgumentsCount: Integer;
begin
  Result := 2;
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

class function TKamScriptEqual.AllowedArgumentsCount: Integer;
begin
  Result := 2;
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

class function TKamScriptNotEqual.AllowedArgumentsCount: Integer;
begin
  Result := 2;
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

class function TKamScriptOr.AllowedArgumentsCount: Integer;
begin
  Result := 2;
end;

class function TKamScriptOr.Name: string;
begin
  Result := 'or (alternative)';
end;

class function TKamScriptOr.ShortName: string;
begin
  Result := 'Or';
end;

class function TKamScriptAnd.AllowedArgumentsCount: Integer;
begin
  Result := 2;
end;

class function TKamScriptAnd.Name: string;
begin
  Result := 'and (conjunction)';
end;

class function TKamScriptAnd.ShortName: string;
begin
  Result := 'And';
end;

class function TKamScriptNot.AllowedArgumentsCount: Integer;
begin
  Result := 1;
end;

class function TKamScriptNot.Name: string;
begin
  Result := 'not (logical negation)';
end;

class function TKamScriptNot.ShortName: string;
begin
  Result := 'Not';
end;

end.
