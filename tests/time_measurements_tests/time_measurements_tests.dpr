{
  Copyright 2024-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test various time-measurement functions from CastleTimeUtils. }

uses SysUtils,
  CastleTimeUtils;
var
  TimerBefore: TTimerResult;
  ProcessTimerBefore: TProcessTimerResult;
  TickBefore: QWord;
  I: Integer;
  X: Single;
begin
  Writeln('Test Sleep(2500) (2.5 seconds should pass)');

  TimerBefore := Timer;
  ProcessTimerBefore := ProcessTimer;
  TickBefore := CastleGetTickCount64;

  Sleep(2500);

  Writeln('  Timer detected: ', TimerBefore.ElapsedTime:1:2, ' seconds');
  Writeln('  ProcessTimer detected: ', ProcessTimerBefore.ElapsedTime:1:2, ' seconds');
  Writeln('  CastleGetTickCount64 detected: ', ((CastleGetTickCount64 - TickBefore) / 1000):1:2, ' seconds');

  Writeln('Test some intensive operation (time may vary widly depending on your CPU)');

  TimerBefore := Timer;
  ProcessTimerBefore := ProcessTimer;
  TickBefore := CastleGetTickCount64;

  X := 0;
  for I := 0 to 100 * 1000 * 1000 do
  begin
    X := X + Sqrt(I);
  end;

  Writeln('  Timer detected: ', TimerBefore.ElapsedTime:1:2, ' seconds');
  Writeln('  ProcessTimer detected: ', ProcessTimerBefore.ElapsedTime:1:2, ' seconds');
  Writeln('  CastleGetTickCount64 detected: ', ((CastleGetTickCount64 - TickBefore) / 1000):1:2, ' seconds');
end.