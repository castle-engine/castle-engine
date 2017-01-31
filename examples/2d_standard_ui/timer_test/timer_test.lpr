{
  Copyright 2016-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Demo of TCastleTimer and TCastleFlashEffect. }
program timer_test;

{$apptype GUI}

uses Math,
  CastleColors, CastleWindow, CastleControls, CastleFlashEffect;

var
  Window: TCastleWindow;
  Flash: array [0..2] of TCastleFlashEffect;
  Timer: array [0..2] of TCastleTimer;

type
  TEventHandler = class
    class procedure Timer(Sender: TObject);
  end;

class procedure TEventHandler.Timer(Sender: TObject);
begin
  Flash[(Sender as TCastleTimer).Tag].Flash(White, false);
end;

var
  I: Integer;
begin
  Window := TCastleWindow.Create(Application);
  Window.Width := 600;
  Window.Height := 120;

  for I := 0 to 2 do
  begin
    Timer[I] := TCastleTimer.Create(Application);
    Timer[I].IntervalSeconds := Power(2, I - 1); //0.5, 1, 2, 4...
    Timer[I].OnTimer := @TEventHandler(nil).Timer;
    Timer[I].Tag := I;
    // without this, timers will slightly desynchronize after a couple of seconds
    Timer[I].CounteractDelays := true;
    Window.Controls.InsertFront(Timer[I]);

    Flash[I] := TCastleFlashEffect.Create(Application);
    Flash[I].FullSize := false;
    Flash[I].Left := I * 200 + 10;
    Flash[I].Bottom := 10;
    Flash[I].Width := 180;
    Flash[I].Height := 100;
    Window.Controls.InsertFront(Flash[I]);
  end;

  Window.OpenAndRun;
end.

