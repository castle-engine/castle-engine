{ Simple command-line calculator.

  Calculates value of 1st param, e.g. '1 + 2' or 'sin(5)' etc.
  See [http://vrmlengine.sourceforge.net/kambi_script.php]
  for expression syntax. }

{$apptype CONSOLE}

program kambi_calc;

uses KambiParameters, KambiScriptParser;

begin
  Parameters.CheckHigh(1);
  { testing :  Writeln('Expression is ',E.ToString); }
  Writeln(ParseConstantFloatExpression(Parameters[1]): 10: 10);
end.