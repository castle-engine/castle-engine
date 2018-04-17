{ Simple command-line calculator.

  Calculates value of 1st param, e.g. '1 + 2' or 'sin(5)' etc.
  See [https://castle-engine.io/castle_script.php]
  for expression syntax. }

{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

program castle_calculator;

uses CastleParameters, CastleScriptParser;

begin
  Parameters.CheckHigh(1);
  { testing :  Writeln('Expression is ',E.ToString); }
  Writeln(ParseConstantFloatExpression(Parameters[1]): 10: 10);
end.
