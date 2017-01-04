{ Parametry SpeedTest_Name, SpeedTest_FasterName, SpeedTest_SlowerName (string)
            SpeedTest_Cycles (Cardinal)
            SpeedTest_DoFasterCycle (Pascal instruction, without ; at the end)
            SpeedTest_DoSlowerCycle (Pascal instruction, without ; at the end)
  SpeedTest_DoFasterCycle nie musi w rzeczywistosci byc szybsze od
  SpeedTest_DoSlowerCycle, po prostu jezeli zdarzy sie ze jest bardziej wolne
  to bedzie wypisany smieszny (ale w gruncie rzeczy sensowny) komunikat
  w rodzaju
    'Faster is faster than Slower by 0.5'
  (tzn. jest szybsze pol raza, czyli de facto 2 razy bardziej wolne).
}

{$define SpeedTest_Declare:=
  {$ifndef NO_SPEED_TESTS}
  var
    SpeedTest_i: Cardinal;
    SpeedTest_Time0, SpeedTest_Time1, SpeedTest_Time2: Double;
    StartTime: TProcessTimerResult;
  {$endif not NO_SPEED_TESTS}
}

{$define SpeedTest:=
  {$ifndef NO_SPEED_TESTS}
  Writeln('SPEED TEST ',SpeedTest_Name, '-------------------');

  StartTime := ProcessTimer;
  for SpeedTest_i := 1 to SpeedTest_Cycles do ;
  SpeedTest_Time0 := ProcessTimerSeconds(ProcessTimer, StartTime);
  Writeln(Format('Empty loop = %f',[SpeedTest_Time0]));

  StartTime := ProcessTimer;
  for SpeedTest_i := 1 to SpeedTest_Cycles do SpeedTest_DoFasterCycle;
  SpeedTest_Time1 := ProcessTimerSeconds(ProcessTimer, StartTime);
  Writeln(SpeedTest_FasterName, Format(' = %f',[SpeedTest_Time1]));

  StartTime := ProcessTimer;
  for SpeedTest_i := 1 to SpeedTest_Cycles do SpeedTest_DoSlowerCycle;
  SpeedTest_Time2 := ProcessTimerSeconds(ProcessTimer, StartTime);
  Writeln(SpeedTest_SlowerName, Format(' = %f',[SpeedTest_Time2]));

  Writeln(SpeedTest_FasterName, ' is faster than ',
          SpeedTest_SlowerName, ' by ',
	   Format('%f', [(SpeedTest_Time2-SpeedTest_Time0)/
	                 (SpeedTest_Time1-SpeedTest_Time0)]));
  {$endif not NO_SPEED_TESTS}
}
