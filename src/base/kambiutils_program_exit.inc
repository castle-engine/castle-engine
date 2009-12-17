{
  Copyright 2002-2004 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{ Things related to exiting with program.
  Some important things are set up here, like our own exception handler.

  Polish:
  Kilka slow o tym co sie dzieje z naszym programem gdy wystapi exception :
  generalnie, wiele procedur w moich programach rzuca wyjatkami i niejednokrotnie
  pisze programy w ten sposob ze pozwalam pewnym wyjatkom pozostac niewylapanymi
  i przerwac program. Np. ParamCount(Equal/Greater/Lesser...) albo
  TDynLib.Load zazwyczaj nie sa otoczone klauzulami try..except ktore
  maja wylapac wyjatki jakie wystapia : zamiast tego, zazwyczaj pozwalam
  tym procedurom na przerwanie programu przez rzucenie wyjatkiem.
  I to jest dobre, i tak chce.

  Ale pojawia sie problemik : poniewaz program moze sie konczyc na skutek
  niewylapanego wyjatku, i jest to dla mnie sytuacja normalna (tzn. nie jest
  to dla mnie zaden blad programu) chcialbym aby program konczacy sie
  niewylapanym wyjatkiem wypisywal ladny komunikat dla usera, czytelny
  i bez zadnych debuggowych wtretow (w rodzaju adresu i linii w
  kodzie zrodlowym gdzie bylo wywolane raise). Pascal umozliwia mi to :
  wystarczy ze ustawie zmienne ExceptProc z SysUtils na wlasna procedure.
  Jest to obslugiwane zarowno przez FPC jak i przez Delphi i dziala swietnie
  i jestem szczesliwy. Wiec niniejszy modul instaluje wlasnie nasze wlasne
  ExceptProc ktore w razie niewylapanego wyjatku wypisuje OutputException()
  i robi Halt(HaltCodeOnException). CHYBA ze zdefiniowany jest symbol DEBUG,
  wtedy nasze ExceptProc wypisuje OutputException() i nie robi Halt -
  zamiast tego przekazuje sterowanie do ExceptProc domyslnej, zdefiniowanej
  w SysUtils. W ten sposob moje prywatne wersje programow beda wypisywac
  mi troche debugging info - np. FPC probuje zrobic backtrace od miejsca
  wywolania i nieraz jest to bardzo uzyteczne.

  Acha, jeszcze jedno : gdy wyjatek ktory spowodowal wyjscie z programu
  (a wiec doszedl do naszego ExceptProc) to BreakProgram to nasz handler
  wyjatku zachowa sie specyficznie:
    - nie wypisze OutputException(E), w ogole nic nie wypisze
    - wyjdzie z programu z Halt(E.HaltCode), a wiec nie uzyje HaltCodeOnException
    - zrobi powyzsze Halt nawet jesli symbol DEBUG byl zdefiniowany podczas
      kompilacji tego modulu - czyli sterowanie nigdy nie przejdzie wtedy do
      domyslnej ExceptProc w SysUtils.
  Dzieki tym "hackom" mozesz uzywac raise BreakProgram.Create(<exit-code>)
  zamiast Halt(<exit-code>) i zyskasz na tym tyle ze program zakonczy
  sie "czysto", po drodze wychodzac wykonujac wszystkie sekcje finally...end.
  Jest gwarantowane (i musisz tego przestrzegac w swoich programach)
  ze NIC nie przechwyci i nie zatrzyma tego TCodeBreakera.
  Patrz @noAutoLink(@code(console.testy/breakProgram/)) po demo tego.

  Wiec jestem niezwykle szczesliwy.
  Pojawia sie jednak pewna skaza na niebsklonie mojego szczescia :
  Delphi+Win32. W razie niewylapanego wyjatku wyswietla ono @bold(paskudne)
  okienko Windowsa - "program popelnil blad, skontaktuj sie z dostawca itp.
  bzdury". Wyjatkowo dezorientujace dla usera ktory nie wie ze to po prostu
  niewylapany wyjatek i program jest OK. Niestety, Delphi wyswietla to
  paskudne okienko jeszcze przed wywolaniem ExceptProc, a wiec nie moge
  sie przed tym uchronic. Dlatego programy pod Delphi+Win32 nigdy nie powinny
  powodowac niewylapanych wyjatkow - i do tego sluzy mi procedura
  HaltOnException(proc). }

{$ifdef read_interface}

var BonusErrorMessg: string ='';

{ wyjatki dziedziczace od EWithHiddenClassName sa normalnymi wyjatkami
  tyle ze beda w inny sposob formatowane funkcja ExceptMessage :
  ExceptMessage NIE bedzie zawieralo nazwy klasy tych wyjatkow.
  To jest czasami estetyczne gdy wiemy ze Message wyjatku bedzie
  wystarczajaco dobra informacja na temat wyjatku i nie ma sensu pokazywac
  userowi ClassName. }
type EWithHiddenClassName = class(Exception);

{ Except message zwraca komunikat jaki program powinien wyswietlic w reakcji
  na jakies ogolne exception. Komunikat zawiera ProgramName, E.ClassName,
  E.Message (o ile E is Exception), ExceptAddr (o ile jest <> nil i jest
  zdefiniowany symbol DEBUG) i BonusErrorMesssg. }
function ExceptMessage(E: TObject; ExceptAddr: Pointer {$ifdef DEFPARS} = nil {$endif}): string; overload;
{ OutputException = ErrorWrite(ExceptMessage(E)); Taki wygodny skrot }
procedure OutputException(E: TObject; ExceptAddr: Pointer {$ifdef DEFPARS} = nil {$endif}); overload;

{ If Value then Halt(0), else Halt(1).

  It is the standard convention of command-line programs to
  exit with code 0 on success and <> 0 on failure.
  Or (for some programs like `test') exit with code 0 to indicate true result
  and <> 0 to indicate false result.
  So you will probably want to pass here some boolean variable
  indicating "Success" or "TestPassed". }
procedure HaltBool(Value: boolean);

var
  HaltCodeOnException: Integer = 1;

{ This calls Proc and catches all exceptions inside Proc,
  and in case of exception in Proc it does OutputException and
  Halt(HaltCode). The result is that HaltOnException doesn't
  raise any exception, never. It always deals with exceptions inside
  Proc itself.

  For the special exception class @link(BreakProgram), it does simply
  Halt(BreakProgram(E).ExitCode)) (no OutputException in this case).

  When symbol DEBUG is defined, then HaltOnException works differently
  --- it just calls Proc (and doesn't catch any exceptions).

  This is particularly useful under Delphi/Win32. There main program
  should @italic(never) exit with exception. Because such exception
  (because of Delphi stupidity ?) shows ugly Windows
  dialog box saying something like "Program exited unexpectedly,
  contain with author etc. bullshit". There is no way for me to avoid
  this dialog box, even by my own ExceptProc.
  See comments at the beginning of this file @code(kambiutils_program_exit.inc). }
procedure HaltOnException(proc: TProcedure); overload;

{ This is just like HaltOnException(HaltCodeOnException). }
procedure HaltOnException(proc: TProcedure; HaltCode: integer); overload;

type
  { This class is specially handled in my exception handler.
    Look at comments at the beginning of this file, KambiUtils_program_exit.inc }
  BreakProgram = class(TCodeBreaker)
  private
    FHaltCode: Integer;
  public
    property HaltCode: Integer read FHaltCode;
    constructor Create(AHaltCode: Integer); overload;
    constructor Create; overload; { HaltCode = 0 }
  end;

{ ProgramBreak = raise BreakProgram.Create(AHaltCode); }
procedure ProgramBreak(AHaltCode: Integer {$ifdef DEFPARS} =0 {$endif}); overload;

{$endif read_interface}

{$ifdef read_implementation}

function ExceptMessage(E: TObject; ExceptAddr: Pointer): string;
var Message, NiceMessage: string;
begin
 if E is Exception then
 begin
  Message := Exception(E).Message;

  if E is EInOutError then
  begin
   { jezeli wyjatek to EInOutError to zamien [...] nazwy bledow
     "IO Error xxx" na troche lepsze. }
   case EInOutError(E).ErrorCode of
    1..99: NiceMessage := SysErrorMessage(EInOutError(E).ErrorCode);
    100: NiceMessage := 'End of File';
    101: NiceMessage := 'Disk Full';
    102: NiceMessage := 'File variable not assigned';
    103: NiceMessage := 'File not open';
    104: NiceMessage := 'File not open for input';
    105: NiceMessage := 'File not open for output';
    106: NiceMessage := 'Error in formatted input';
    107: NiceMessage := 'File already open';
    else NiceMessage := '';
   end;
   if NiceMessage<>'' then Message := Message +' (' +NiceMessage +')';
  end;

 end else
  Message := '';

 if (E is EWithHiddenClassName) and (Message<>'') then
  result := ProgramName +': ' +Message else
 begin
  result := ProgramName +': Exception ' +E.ClassName;
  {$ifdef DEBUG}
  if ExceptAddr<>nil then
   result := result +' (at address ' + PointerToStr(ExceptAddr) + ')';
  {$endif}
  if Message<>'' then result := result +' : ' +nl +Message;
 end;

 if BonusErrorMessg<>'' then result := result +nl +BonusErrorMessg;
end;

procedure OutputException(E: TObject; ExceptAddr: Pointer);
begin
 ErrorWrite(ExceptMessage(E, ExceptAddr));
 { zeby wyswietlic tu message tak jak Delphi/Kylix : ShowException(E, nil) }
end;

procedure HaltBool(Value: boolean);
begin
 if Value then Halt(0) else Halt(1);
end;

procedure HaltOnException(proc: TProcedure; HaltCode: integer);
begin
 {$ifdef DEBUG} proc;
 {$else}
  try Proc;
  except
   on E: TObject do
   begin
    if E is BreakProgram then Halt(BreakProgram(E).HaltCode);
    OutputException(E, nil);
    Halt(HaltCode);
   end;
  end;
 {$endif}
end;

procedure HaltOnException(proc: TProcedure);
begin HaltOnException(proc, HaltCodeOnException); end;

{$ifdef DELPHI}
type TExceptProc = procedure(ExceptObject: TObject; ExceptAddr: Pointer);
var ExceptProc: TExceptProc absolute System.ExceptProc;
{$endif}
var OldExceptProc: TExceptProc;

procedure KambiUtils_ExceptProc(ExceptObject: TObject;
  ExceptAddr: Pointer
  {$ifdef FPC}
    {$ifdef VER1_9_6} ; Frame: Pointer
    {$else} ; FrameCount: Longint; Frame: PPointer
    {$endif}
  {$endif});
begin
 if ExceptObject is BreakProgram then
  Halt(BreakProgram(ExceptObject).HaltCode);

 OutputException(ExceptObject, ExceptAddr);
 {$ifdef DEBUG}
 OldExceptProc(ExceptObject, ExceptAddr
   {$ifdef FPC}
     {$ifdef VER1_9_6} ,Frame
     {$else} ,FrameCount, Frame
     {$endif}
   {$endif});
 {$else}
 Halt(HaltCodeOnException);
 {$endif}
end;

constructor BreakProgram.Create(AHaltCode: Integer);
begin
 inherited Create;
 FHaltCode := AHaltCode;
end;

constructor BreakProgram.Create;
begin
 Create(0);
end;

procedure ProgramBreak(AHaltCode: Integer);
begin
 raise BreakProgram.Create(AHaltCode);
end;

procedure InitializationProgramExit;
begin
 OldExceptProc := ExceptProc;
 ExceptProc := {$ifdef FPC_OBJFPC} @ {$endif} KambiUtils_ExceptProc;
end;

procedure FinalizationProgramExit;
begin
 ExceptProc := OldExceptProc;
end;

{$endif read_implementation}
