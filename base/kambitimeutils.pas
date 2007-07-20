{
  Copyright 2000-2007 Michalis Kamburelis.

  This file is part of "Kambi's base Pascal units".

  "Kambi's base Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's base Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's base Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ Things working with time }
unit KambiTimeUtils;

interface

uses
  {$ifdef WIN32}
    Windows,
  {$endif}
  {$ifdef UNIX}
    {$ifdef USE_LIBC} Libc, {$else} BaseUnix, Unix, Dl, {$endif}
  {$endif}
  SysUtils, KambiUtils, Math;

procedure Delay(MiliSec: Word); {nie robiac Process messages}

type
  TMilisecTime = LongWord;

{ zwraca czy secondTime jest pozniejsze o co najmniej timeDelay od firstTime.
  first i second Time sa time'ami branymi z GetTickCount, tzn. czas jest w zakresie Dword
  i moze sie przewinac. Tzn. kazdy czas jest rozumiany jako Later - natomiast
  sprawdzane jest czy czas jest pozniejszy o timeDelay.
  TimeTickSecond(x, x, 0) = true (czas x jest pozniejszy od x o 0 = true). }
function TimeTickSecondLater(firstTime, secondTime, timeDelay: TMilisecTime): boolean;

{ zwraca roznice czasow secondTime-firstTime. Czasy sa brane z GetTickCount czyli
  sa Dwordami i moga sie przewinac. }
function TimeTickDiff(firstTime, secondTime: TMilisecTime): TMilisecTime;

{ dodawanie i odejmowanie dwoch czasow. Zwroc uwage na brak precyzji :
  czy TMilisecTime to punkt w czasie czy okres czasu ? Nie jest to
  zdefiniowane, bo moze byc dowolnie. Mozesz dodac punkt czasu do okresu czasu
  albo dwa okresy czasu, albo dwa punkty czasu i nawet jesli to ostatnie jest
  bez sensu -  z punktu widzenia ponizszych funkcji nie istnieje rozroznienie
  miedzy tymi operacjami.
  Przy okazji mozna zauwazyc ze skoro wynik odejmowania tez jest TMilisecTime
  to jesli odejmujesz dwa czasy od siebie na pewno otrzymasz wynik dodatni
  (innymi slowy, jezeli chcesz odjac czas mniejszy od wiekszego to funkcja
  ponizej zachowa sie jakby uznala ze czas sie musial "przewinac". }
function MilisecTimesAdd(t1, t2: TMilisecTime): TMilisecTime;
function MilisecTimesSubtract(t1, t2: TMilisecTime): TMilisecTime;

{ czas od uruchomienia systemu w milisekundach (przewija sie co 49 dni) }
function GetTickCount: TMilisecTime;
 {$ifdef WIN32} stdcall; external KernelDLL name 'GetTickCount'; {$endif WIN32}

const
  MinDateTime: TDateTime = MinDouble;

{ zamien DateTime na "at" string czyli string w postaci "data at time".
  Czesto uzywam takiego formatu. }
function DateTimeToAtStr(DateTime: TDateTime): string;

{ ------------------------------------------------------------------------------
  @section(Process Timer)

  ProcessTimerEnd podaje czas jaki uplynal od ostatniego wywolania
  ProcessTimer Begin w sekundach. (zawsze zrob tak zeby przed TimerEnd wystapilo
  gdzies TimerBegin). Stara sie mierzyc czas jaki system spedzil TYLKO na tym
  procesie wiec nie dokonuje faktycznego pomiaru czasu. Tzn. tak brzmi teoria -
  pod windowsem nie ma takiej fajnej procedury clock() z Libc wiec uzywamy
  prostego GetTickCount ktore de facto mierzy uplyw czasu rzeczywistego.
  "Proc" jest skrotem od "Process".

  Z definicji ProcessTimerBegin/End sa NON-REENTRANT. Aby skonstruowac je w obrebie
  pojedynczego watku, a wiec aby pomiar byl reentrant, uzywaj
  ProcessTimerNow : aby pobrac faktyczny stan timera
  ProcessTimerDiff(a, b) : aby wykonac odejmowanie a-b (ProcessTimerDiff wykonuje
     nie tylko a-b, np. pod Windowsem TimerDiff uwzglednia fakt ze GetTickCount
     moze sie przewinac i o ile przewiniecie sie nie bylo zbyt duze zwraca i
     tak dobry wynik. Ale mysl o ProcessTimerDiff(a, b) jako o a-b).
  wynik ProcessTimerDiff jest przedzialem , w jednostkach TTimerResult. aby zamienic
    go na sekundy podziel go przez ProcessTimersPerSec.

  Jakiej rozdzielczosci uzywa timer ? No coz, zalezy od OSa i od implementacji.
  Patrz stala ProcessTimersPerSec ponizej.
}

type
  { }
  TProcessTimerResult =
    {$ifdef UNIX} clock_t {$endif}
    {$ifdef WIN32} DWord {$endif};

const
  ProcessTimersPerSec
    {$ifdef UNIX}
      {$ifdef USE_LIBC}
        : function: clock_t = Libc.CLK_TCK
      {$else}
        = { TODO: what is the frequency of FpTimes ?
            sysconf (_SC_CLK_TCK) ?
            Or does sysconf exist only in Libc ? }
          { Values below were choosen experimentally for Linux and FreeBSD
            (and I know that on most UNIXes it should be 128, that's
            a traditional value) }
          {$ifdef LINUX} 100 {$else} 128 {$endif}
      {$endif}
    {$endif}
    {$ifdef WIN32} = 1000 { Using GetLastError } {$endif};

function ProcessTimerNow: TProcessTimerResult;
function ProcessTimerDiff(a, b: TProcessTimerResult): TProcessTimerResult;
procedure ProcessTimerBegin;
function ProcessTimerEnd: Double;

{ -----------------------------------------------------------------------------

  TODO:
  - performance timer should be changed : function names should be changed
    (no need to emphasize that "performance")
  - PerfTimer should never return false, instead it should fall back
    on GetTickCount
  - integrate PerfTimer with ProcessTimer, to use the same functions.

  performance timer : wywolaj dwa razy, odejmij roznice a otrzymasz
  czas jaki uplynal z b.duza dokladnoscia (miejmy nadzieje).
  Pod WinAPI realizacja przez QueryPerformanceCounter/Frequency,
  pod UNIXem przez gettimeofday. }

{$ifdef WIN32}
type TPerfTimerResult = Int64;
     TPerfTimerFreqResult = Int64;

{ PerfTimer Init : jesli false to znaczy ze PerfTimer nieosiagalny }
function PerfTimerInit: boolean;
{ Performance timer frequency, czyli ile tykniec robi PerfTimer na sekunde.
  Wszyscy mamy nadzieje ze bedzie to dosc duza liczba. }
function PerfTimerFreq: TPerfTimerFreqResult;
{$endif WIN32}

{$ifdef UNIX}
type TPerfTimerResult = Int64;
     TPerfTimerFreqResult = LongWord;
const PerfTimerInit = true;
      PerfTimerFreq = 1000000;
{$endif UNIX}

{ PerfTime: po prostu, czas w/g tego timera. Odejmij dwa wywolania PerfTime,
  podziel przez PerfTimeFreq i otrzymasz wynik : ile sekund (jaka czesc sekundy)
  uplynelo }
function PerfTime: TPerfTimerResult;

implementation

procedure Delay (MiliSec: Word);
begin
 SysUtils.Sleep(MiliSec);
end;

function TimeTickSecondLater(firstTime, secondTime, timeDelay: TMilisecTime): boolean;
{ trzeba uwazac na typy w tej procedurze.
  Dword to 32 bit unsigned int,
  Int64 to 64 bit signed int.
  I tak musi byc.
  Uwaga ! W Delphi 3, o ile dobrze pamietam, bylo Dword = longint ! tak nie moze byc ! }
var bigint: Int64;
begin
 bigint := secondTime-timeDelay;
 if bigint < 0 then
 begin
  bigint := bigint+High(TMilisecTime);
  result:=(firstTime > secondTime) and (firstTime <= bigint);
 end else result := firstTime <= bigint;
end;

function TimeTickDiff(firstTime, secondTime: TMilisecTime): TMilisecTime;
begin
 result := MilisecTimesSubtract(secondTime, firstTime);
{old implementation :

 if firstTime <= secondTime then
  result := secondTime-firstTime else
  result := High(TMilisecTime) -firstTime +secondTime;
}
end;

{$I norqcheckbegin.inc}
function MilisecTimesAdd(t1, t2: TMilisecTime): TMilisecTime;
begin result := t1+t2 end;

function MilisecTimesSubtract(t1, t2: TMilisecTime): TMilisecTime;
begin result := t1-t2 end;
{$I norqcheckend.inc}

{$ifndef WIN32}

{$I norqcheckbegin.inc}
function GetTickCount: TMilisecTime;
var timeval: TTimeVal;
begin
 {$ifdef USE_LIBC} gettimeofday(timeval, nil)
 {$else}           FpGettimeofday(@timeval, nil)
 {$endif};

 { Odrzucamy najbardziej znaczace cyfry z x -- i dobrze, bo w
   timeval.tv_sec najbardziej znaczace cyfry sa najmniej wazne bo najrzadziej
   sie zmieniaja.
   x*1000 jednoczesnie ma puste miejsca na trzech najmniej znaczaych cyfrach
   dziesietnych (mowiac po ludzku, po prostu x*1000 ma trzy zera na koncu).
   Wykorzystujemy te trzy miejsca na tv_usec div 1000 ktore na pewno zawiera
   sie w 0..999 bo tv_usec zawiera sie w 0..milion-1.

   W ten sposob zamenilismy timeval na jedna 32-bitowa liczbe w taki sposob
   ze odrzucilismy tylko najbardziej znaczace cyfry - a wiec lepiej
   sie nie da. W rezultacie otrzymana cyfra mierzy czas w milisekundach a wiec
   przewinie sie, podobnie jak Windowsowe GetTickCount, co jakies 49 dni
   (49 dni = 49* 24* 60* 60 *1000 milisekund = 4 233 600 000 a wiec okolice
   High(LongWord)). Tyle ze ponizsze GetTickCount nie liczy czasu poczawszy
   od startu windowsa wiec moze sie przewinac w kazdej chwili.

   Note: I used to have here some old code that instead of
     LongWord(timeval.tv_sec) * 1000
   was doing
     ( LongWord(timeval.tv_sec) mod (Int64(High(LongWord)) div 1000 + 1) ) * 1000
   but I longer think it's necessary. After all, I'm inside
   norqcheck begin/end so I don't have to care about such things,
   and everything should work OK.
 }

 Result := LongWord(timeval.tv_sec) * 1000 + Longword(timeval.tv_usec) div 1000;
end;
{$I norqcheckend.inc}

{$endif not WIN32}

function DateTimeToAtStr(DateTime: TDateTime): string;
begin
 result := FormatDateTime('yyyy"-"mm"-"dd" at "tt', DateTime);
end;

{ cross-platform process timery ---------------------------------------------- }

{$ifdef UNIX}
function ProcessTimerNow: TProcessTimerResult;
var Dummy: tms;
begin
 { See /c/mojepasy/console.testy/test_times/RESULTS,
   it occurs that (at least on my Linux ? Debian, Linux 2.4.20, libc-2.3.2)
   the only reliable way is to use return value from times
   (from Libc or FpTimes).

   tms.tms_utime, tms.tms_stime, clock() values are nonsense !

   This is not FPC bug as I tested this with C program too. }

 Result := {$ifdef USE_LIBC} times {$else} FpTimes {$endif} (Dummy);
end;

function ProcessTimerDiff(a, b: TProcessTimerResult): TProcessTimerResult;
begin
 Result := a - b;
end;
{$endif UNIX}

{$ifdef WIN32}
function ProcessTimerNow: TProcessTimerResult;
begin
  Result := GetTickCount;
end;

function ProcessTimerDiff(a, b: TProcessTimerResult): TProcessTimerResult;
begin
  Result := TimeTickDiff(b, a);
end;
{$endif WIN32}

var
  LastProcessTimerBegin: TProcessTimerResult;

procedure ProcessTimerBegin;
begin
  LastProcessTimerBegin := ProcessTimerNow
end;

function ProcessTimerEnd: Double;
begin
  Result := ProcessTimerDiff(ProcessTimerNow, LastProcessTimerBegin)
    / ProcessTimersPerSec;
end;

{ performance timer ---------------------------------------------------------- }

{$ifdef WIN32}
var FPerfTimerFreq: TPerfTimerFreqResult;

function PerfTimerInit: boolean;
begin
 result := QueryPerformanceFrequency(FPerfTimerFreq);
end;

function PerfTimerFreq: TPerfTimerFreqResult;
begin result := FPerfTimerFreq end;

function PerfTime: TPerfTimerResult;
begin
 QueryPerformanceCounter(result);
end;
{$endif WIN32}

{$ifdef UNIX}
function PerfTime: TPerfTimerResult;
var tv: TTimeval;
begin
 {$ifdef USE_LIBC} gettimeofday(tv, nil)
 {$else}           FpGettimeofday(@tv, nil)
 {$endif};

 {w Int64 zmiesci sie cale TTimeval bez obcinania.
  Robie tylko odpowiednie casty na zapas zeby na pewno liczyl wszystko
  od poczatku jako Int64}
 result := Int64(tv.tv_sec)*1000000 + Int64(tv.tv_usec);
end;
{$endif UNIX}

end.
