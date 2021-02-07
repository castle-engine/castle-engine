program random_threads_test;

uses {$IFDEF LINUX} cthreads, {$ENDIF} SysUtils, Classes, CastleLog, CastleRandom;

{$R+}{$Q+}

type
  TRandomThread = class(TThread)
    Rnd: TCastleRandom;
    Id: Integer;
    procedure Execute; override;
  end;


procedure TRandomThread.execute;
var
  ThreadName, SeedName, TickName: String;
begin
 Rnd := TCastleRandom.Create;
 TickName := IntToStr(GetTickCount64);
 ThreadName := IntToStr(Id);
 while Length(ThreadName) < 3 do
   ThreadName := '0' + ThreadName;
 SeedName := IntToStr(Rnd.Random32bit);
 while Length(SeedName) < 10 do
   SeedName := '0' + SeedName;
 WriteLnLog('Thread' + ThreadName,'at tick ' + TickName + ' seed: ' + SeedName);
 FreeAndNil(Rnd);
end;

const
  NThreads = 150;
var
  RndThread: array [0..NThreads] of TRandomThread;
  I: Integer;

begin
  InitializeLog;
  WriteLnLog('Init','Starting...');
  for I:= 0 to NThreads do begin
    RndThread[I] := TRandomThread.Create(true);
    RndThread[I].Id := I;
    RndThread[I].FreeOnTerminate := true;
    RndThread[I].Priority := tpLower;
  end;
  for I := 0 to NThreads do
    RndThread[I].Start;
  Sleep(1000);
end.
