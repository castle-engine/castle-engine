program random_threads_test;

uses {$IFDEF LINUX} cthreads, {$ENDIF} SysUtils, Classes, CastleLog, CastleRandom;

{$R+}{$Q+}

type
  TRandomThread = class(TThread)
    rnd: TCastleRandom;
    id: integer;
    procedure execute; override;
  end;


procedure TRandomThread.execute;
begin
 rnd := TCastleRandom.create;
 writeLnLog('Thread'+inttostr(id),'seed: '+inttostr(rnd.Random32bit));
 freeandnil(rnd);
end;

const n_threads = 150;
var rnd_thread: array [0..n_threads] of TRandomThread;
    i: integer;

begin
  InitializeLog;
  writeLnLog('init','starting...');
  for i:= 0 to n_threads do begin
    rnd_thread[i] := TRandomThread.Create(true);
    rnd_thread[i].id := i;
    rnd_thread[i].FreeOnTerminate := true;
    rnd_thread[i].Priority := tpLower;
    rnd_thread[i].Start;
  end;
  sleep(1000);
end.

