program random_threads_test;

uses {$IFDEF LINUX} cthreads, {$ENDIF} SysUtils, Classes, CastleLog, CastleRandom
  { Needed only for GetTickCount64 on FPC 2.6.4.
    In newer FPC, GetTickCount64 is already in SysUtils. }
  {$ifdef VER2} , CastleTimeUtils {$endif};

{$R+}{$Q+}

type
  TRandomThread = class(TThread)
    rnd: TCastleRandom;
    id: integer;
    procedure execute; override;
  end;


procedure TRandomThread.execute;
var ThreadName, SeedName, TickName: string;
begin
 rnd := TCastleRandom.create;
 TickName := inttostr(GetTickCount64);
 ThreadName := inttostr(id);
 while length(ThreadName)<3 do ThreadName:='0'+ThreadName;
 SeedName := inttostr(rnd.Random32bit);
 while length(SeedName)<10 do SeedName:='0'+SeedName;
 writeLnLog('Thread'+ThreadName,'at tick '+TickName+' seed: '+SeedName);
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
  end;
  for i:= 0 to n_threads do
    rnd_thread[i].Start;
  sleep(1000);
end.
