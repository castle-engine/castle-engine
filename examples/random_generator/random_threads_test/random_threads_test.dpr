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
var ThreadName, SeedName, TickName: string;
begin
 Rnd := TCastleRandom.Create;
 TickName := inttostr(GetTickCount64);
 ThreadName := inttostr(Id);
 while length(ThreadName)<3 do ThreadName:='0'+ThreadName;
 SeedName := IntToStr(Rnd.Random32bit);
 while Length(SeedName)<10 do SeedName:='0'+SeedName;
 WriteLnLog('Thread'+ThreadName,'at tick '+TickName+' seed: '+SeedName);
 FreeAndNil(Rnd);
end;

const NThreads = 150;
var RndThread: array [0..NThreads] of TRandomThread;
    i: Integer;

begin
  InitializeLog;
  WriteLnLog('Init','Starting...');
  for i:= 0 to NThreads do begin
    RndThread[i] := TRandomThread.Create(true);
    RndThread[i].Id := i;
    RndThread[i].FreeOnTerminate := true;
    RndThread[i].Priority := tpLower;
  end;
  for i:= 0 to NThreads do
    RndThread[i].Start;
  Sleep(1000);
end.
