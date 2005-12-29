{ Simple command-line calculator.

  Calculates value of 1st param, e.g. '1 + 2' or 'sin(5)' etc. --
  any expression that can be parsed with MathExprParser 
  to TMathExpr object.

  This is a simplest example of some usable program using MathExpr
  and MathExprParser units. }

{$apptype CONSOLE}

program kambi_calc;

uses KambiUtils, MathExprParser;

begin
 { testing :  Writeln('Expression is ',E.ToString); }
 Writeln(EvalConstMathExpr(ParStr(1)): 10: 10);
end.