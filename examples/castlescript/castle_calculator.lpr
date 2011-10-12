{ Simple command-line calculator.

  Calculates value of 1st param, e.g. '1 + 2' or 'sin(5)' etc.
  See [http://castle-engine.sourceforge.net/castle_script.php]
  for expression syntax. }

{$apptype CONSOLE}

program castle_calculator;

uses CastleParameters, CastleScriptParser;

begin
  Parameters.CheckHigh(1);
  { testing :  Writeln('Expression is ',E.ToString); }
  Writeln(ParseConstantFloatExpression(Parameters[1]): 10: 10);
end.