{
  Copyright 2001-2018 Michalis Kamburelis.

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
    class function Name: String; override;
    class function ShortName: String; override;
    class function InfixOperatorName: String; override;
  end;

  TCasScriptSubtract = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
    class function InfixOperatorName: String; override;
  end;

  TCasScriptMultiply = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
    class function InfixOperatorName: String; override;
  end;

  TCasScriptDivide = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
    class function InfixOperatorName: String; override;
  end;

  TCasScriptNegate = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
  end;

  TCasScriptModulo = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
    class function InfixOperatorName: String; override;
  end;

  TCasScriptSin = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
  end;

  TCasScriptCos = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
  end;

  TCasScriptTan = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
  end;

  TCasScriptCotan = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
  end;

  TCasScriptArcSin = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
  end;

  TCasScriptArcCos = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
  end;

  TCasScriptArcTan = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
  end;

  TCasScriptArcCotan = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
  end;

  TCasScriptSinh = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
  end;

  TCasScriptCosh = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
  end;

  TCasScriptTanh = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
  end;

  TCasScriptCotanh = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
  end;

  TCasScriptLog2 = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
  end;

  TCasScriptLn = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
  end;

  TCasScriptLog = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
  end;

  TCasScriptPower2 = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
  end;

  TCasScriptExp = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
  end;

  TCasScriptPower = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
    class function InfixOperatorName: String; override;
  end;

  TCasScriptSqr = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
  end;

  TCasScriptSqrt = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
  end;

  TCasScriptMax = class(TCasScriptFunction)
  public
    class function ShortName: String; override;
  end;

  TCasScriptMin = class(TCasScriptFunction)
  public
    class function ShortName: String; override;
  end;

  TCasScriptSgn = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
  end;

  TCasScriptAbs = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
  end;

  TCasScriptCeil = class(TCasScriptFunction)
  public
    class function ShortName: String; override;
  end;

  TCasScriptFloor = class(TCasScriptFunction)
  public
    class function ShortName: String; override;
  end;

  TCasScriptRound = class(TCasScriptFunction)
  public
    class function ShortName: String; override;
  end;

  TCasScriptGreater = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
    class function InfixOperatorName: String; override;
  end;

  TCasScriptLesser = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
    class function InfixOperatorName: String; override;
  end;

  TCasScriptGreaterEq = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
    class function InfixOperatorName: String; override;
  end;

  TCasScriptLesserEq = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
    class function InfixOperatorName: String; override;
  end;

  TCasScriptEqual = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
    class function InfixOperatorName: String; override;
  end;

  TCasScriptNotEqual = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
    class function InfixOperatorName: String; override;
  end;

  TCasScriptOr = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
  end;

  TCasScriptAnd = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
  end;

  TCasScriptNot = class(TCasScriptFunction)
  public
    class function Name: String; override;
    class function ShortName: String; override;
  end;

  TCasScriptInt = class(TCasScriptFunction)
  public
    class function ShortName: String; override;
  end;

  TCasScriptFloatFun = class(TCasScriptFunction)
  public
    class function ShortName: String; override;
  end;

  TCasScriptBool = class(TCasScriptFunction)
  public
    class function ShortName: String; override;
  end;

  TCasScriptStringFun = class(TCasScriptFunction)
  public
    class function ShortName: String; override;
  end;

  TCasScriptWriteln = class(TCasScriptFunction)
  public
    class function ShortName: String; override;
  end;

  TCasScriptCharacterFromCode = class(TCasScriptFunction)
  public
    class function ShortName: String; override;
  end;

  TCasScriptRandom = class(TCasScriptFunction)
  public
    class function ShortName: String; override;
  end;

  TCasScriptLerp = class(TCasScriptFunction)
  public
    class function ShortName: String; override;
  end;

  { CastleScript function @code(shortcut),
    see [https://castle-engine.io/castle_script.php#function_shortcut]. }
  TCasScriptShortcut = class(TCasScriptFunction)
  public
    class function ShortName: String; override;
    class procedure Handle(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: Boolean);
  end;

implementation

uses SysUtils, CastleInputs;

class function TCasScriptAdd.Name: String;
begin
  Result := 'add (+)';
end;

class function TCasScriptAdd.ShortName: String;
begin
  Result := '';
end;

class function TCasScriptAdd.InfixOperatorName: String;
begin
  Result := '+';
end;

class function TCasScriptSubtract.Name: String;
begin
  Result := 'subtract (-)';
end;

class function TCasScriptSubtract.ShortName: String;
begin
  Result := '';
end;

class function TCasScriptSubtract.InfixOperatorName: String;
begin
  Result := '-';
end;

class function TCasScriptMultiply.Name: String;
begin
  Result := 'multiply (*)';
end;

class function TCasScriptMultiply.ShortName: String;
begin
  Result := '';
end;

class function TCasScriptMultiply.InfixOperatorName: String;
begin
  Result := '*';
end;

class function TCasScriptDivide.Name: String;
begin
  Result := 'divide (/)';
end;

class function TCasScriptDivide.ShortName: String;
begin
  Result := '';
end;

class function TCasScriptDivide.InfixOperatorName: String;
begin
  Result := '/';
end;

class function TCasScriptNegate.Name: String;
begin
  Result := 'negate (unary -)';
end;

class function TCasScriptNegate.ShortName: String;
begin
  Result := '';
end;

class function TCasScriptModulo.Name: String;
begin
  Result := 'modulo (%)';
end;

class function TCasScriptModulo.ShortName: String;
begin
  Result := '';
end;

class function TCasScriptModulo.InfixOperatorName: String;
begin
  Result := '%';
end;

class function TCasScriptSin.Name: String;
begin
  Result := 'sinus';
end;

class function TCasScriptSin.ShortName: String;
begin
  Result := 'Sin';
end;

class function TCasScriptCos.Name: String;
begin
  Result := 'cosinus';
end;

class function TCasScriptCos.ShortName: String;
begin
  Result := 'Cos';
end;

class function TCasScriptTan.Name: String;
begin
  Result := 'tangens';
end;

class function TCasScriptTan.ShortName: String;
begin
  Result := 'Tan';
end;

class function TCasScriptCotan.Name: String;
begin
  Result := 'cotangens';
end;

class function TCasScriptCotan.ShortName: String;
begin
  Result := 'Cotan';
end;

class function TCasScriptArcSin.Name: String;
begin
  Result := 'arcSinus';
end;

class function TCasScriptArcSin.ShortName: String;
begin
  Result := 'ArcSin';
end;

class function TCasScriptArcCos.Name: String;
begin
  Result := 'arcCosinus';
end;

class function TCasScriptArcCos.ShortName: String;
begin
  Result := 'ArcCos';
end;

class function TCasScriptArcTan.Name: String;
begin
  Result := 'arcTangens';
end;

class function TCasScriptArcTan.ShortName: String;
begin
  Result := 'ArcTan';
end;

class function TCasScriptArcCotan.Name: String;
begin
  Result := 'arcCotangens';
end;

class function TCasScriptArcCotan.ShortName: String;
begin
  Result := 'ArcCotan';
end;

class function TCasScriptSinh.Name: String;
begin
  Result := 'sinh';
end;

class function TCasScriptSinh.ShortName: String;
begin
  Result := 'Sinh';
end;

class function TCasScriptCosh.Name: String;
begin
  Result := 'cosh';
end;

class function TCasScriptCosh.ShortName: String;
begin
  Result := 'Cosh';
end;

class function TCasScriptTanh.Name: String;
begin
  Result := 'tanh';
end;

class function TCasScriptTanh.ShortName: String;
begin
  Result := 'Tanh';
end;

class function TCasScriptCotanh.Name: String;
begin
  Result := 'cotanh';
end;

class function TCasScriptCotanh.ShortName: String;
begin
  Result := 'Cotanh';
end;

class function TCasScriptLog2.Name: String;
begin
  Result := 'logarithm (base 2)';
end;

class function TCasScriptLog2.ShortName: String;
begin
  Result := 'Log2';
end;

class function TCasScriptLn.Name: String;
begin
  Result := 'logarithm (base e)';
end;

class function TCasScriptLn.ShortName: String;
begin
  Result := 'Ln';
end;

class function TCasScriptLog.Name: String;
begin
  Result := 'logarithm';
end;

class function TCasScriptLog.ShortName: String;
begin
  Result := 'Log';
end;

class function TCasScriptPower2.Name: String;
begin
  Result := 'power (base 2)';
end;

class function TCasScriptPower2.ShortName: String;
begin
  Result := 'Power2';
end;

class function TCasScriptExp.Name: String;
begin
  Result := 'power (base enat)';
end;

class function TCasScriptExp.ShortName: String;
begin
  Result := 'Exp';
end;

class function TCasScriptPower.Name: String;
begin
  Result := 'power';
end;

class function TCasScriptPower.ShortName: String;
begin
  Result := 'Power';
end;

class function TCasScriptPower.InfixOperatorName: String;
begin
  Result := '^';
end;

class function TCasScriptSqr.Name: String;
begin
  Result := 'sqr (square)';
end;

class function TCasScriptSqr.ShortName: String;
begin
  Result := 'Sqr';
end;

class function TCasScriptSqrt.Name: String;
begin
  Result := 'sqrt (square root)';
end;

class function TCasScriptSqrt.ShortName: String;
begin
  Result := 'Sqrt';
end;

class function TCasScriptMax.ShortName: String;
begin
  Result := 'max';
end;

class function TCasScriptMin.ShortName: String;
begin
  Result := 'min';
end;

class function TCasScriptSgn.Name: String;
begin
  Result := 'signum';
end;

class function TCasScriptSgn.ShortName: String;
begin
  Result := 'Sgn';
end;

class function TCasScriptAbs.Name: String;
begin
  Result := 'abs (absolute value)';
end;

class function TCasScriptAbs.ShortName: String;
begin
  Result := 'Abs';
end;

class function TCasScriptCeil.ShortName: String;
begin
  Result := 'Ceil';
end;

class function TCasScriptFloor.ShortName: String;
begin
  Result := 'Floor';
end;

class function TCasScriptRound.ShortName: String;
begin
  Result := 'round';
end;

class function TCasScriptGreater.Name: String;
begin
  Result := 'greater (>)';
end;

class function TCasScriptGreater.ShortName: String;
begin
  Result := '';
end;

class function TCasScriptGreater.InfixOperatorName: String;
begin
  Result := '>';
end;

class function TCasScriptLesser.Name: String;
begin
  Result := 'lesser (<)';
end;

class function TCasScriptLesser.ShortName: String;
begin
  Result := '';
end;

class function TCasScriptLesser.InfixOperatorName: String;
begin
  Result := '<';
end;

class function TCasScriptGreaterEq.Name: String;
begin
  Result := 'greater/equal (>=)';
end;

class function TCasScriptGreaterEq.ShortName: String;
begin
  Result := '';
end;

class function TCasScriptGreaterEq.InfixOperatorName: String;
begin
  Result := '>=';
end;

class function TCasScriptLesserEq.Name: String;
begin
  Result := 'lesser/equal (<=)';
end;

class function TCasScriptLesserEq.ShortName: String;
begin
  Result := '';
end;

class function TCasScriptLesserEq.InfixOperatorName: String;
begin
  Result := '<=';
end;

class function TCasScriptEqual.Name: String;
begin
  Result := 'equal (=)';
end;

class function TCasScriptEqual.ShortName: String;
begin
  Result := '';
end;

class function TCasScriptEqual.InfixOperatorName: String;
begin
  Result := '=';
end;

class function TCasScriptNotEqual.Name: String;
begin
  Result := 'not equal (<>)';
end;

class function TCasScriptNotEqual.ShortName: String;
begin
  Result := '';
end;

class function TCasScriptNotEqual.InfixOperatorName: String;
begin
  Result := '<>';
end;

class function TCasScriptOr.Name: String;
begin
  Result := 'or (alternative)';
end;

class function TCasScriptOr.ShortName: String;
begin
  Result := 'Or';
end;

class function TCasScriptAnd.Name: String;
begin
  Result := 'and (conjunction)';
end;

class function TCasScriptAnd.ShortName: String;
begin
  Result := 'And';
end;

class function TCasScriptNot.Name: String;
begin
  Result := 'not (logical negation)';
end;

class function TCasScriptNot.ShortName: String;
begin
  Result := 'Not';
end;

class function TCasScriptInt.ShortName: String;
begin
  Result := 'int';
end;

class function TCasScriptFloatFun.ShortName: String;
begin
  Result := 'float';
end;

class function TCasScriptBool.ShortName: String;
begin
  Result := 'bool';
end;

class function TCasScriptStringFun.ShortName: String;
begin
  Result := 'string';
end;

class function TCasScriptWriteln.ShortName: String;
begin
  Result := 'writeln';
end;

class function TCasScriptCharacterFromCode.ShortName: String;
begin
  Result := 'character_from_code';
end;

class function TCasScriptRandom.ShortName: String;
begin
  Result := 'random';
end;

class function TCasScriptLerp.ShortName: String;
begin
  Result := 'lerp';
end;

{ TCasScriptShortcut --------------------------------------------------------- }

class function TCasScriptShortcut.ShortName: String;
begin
  Result := 'shortcut';
end;

class procedure TCasScriptShortcut.Handle(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: Boolean);
var
  N: String;
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

end.
