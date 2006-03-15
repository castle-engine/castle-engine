{
  Copyright 2000-2004 Michalis Kamburelis.

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

{ Some WinAPI helpers }

{$ifdef read_interface}

const
  KernelDLL = 'kernel32.dll';
  UserDLL = 'user32.dll';
  GdiDLL = 'gdi32.dll';
  ShellDLL = 'shell32.dll';

{ OneInstanceCode wykona MainProgram i zwroci true jezeli w momencie wywolania
  w systemie nie bedzie w systemie operacyjnym juz wykonywany kod o
  identyfikatorze CodeIdent. Wpp. zwroci false.

  Jest to wiec podstawowa funkcja aby robic aplikacje One-Instance jak edytorek,
  ale moze tez sluzyc to robienia jakichs innych sztuczek i program
  w ogole nie musi byc GUI.

  TODO: jak zrealizowac to pod Linuxem ? }
function OneInstanceCode(MainProgram:TProcedure; CodeIdent:string):boolean; overload;
{ Same thing as OneInstanceCode(MainProgram, ProgramName + '_one_instance_mutex') }
function OneInstanceCode(MainProgram:TProcedure):boolean; overload;

const
  IdentityMat2:Mat2= { identity matrix }
    (eM11:(fract:0; value:1); eM12:(fract:0; value:0);    { 1.0  0.0 }
     eM21:(fract:0; value:0); eM22:(fract:0; value:1));   { 0.0  1.0 }

{$ifdef FPC}
type
  TWMCopyData = packed record
    Msg: Cardinal;
    From: HWND;
    CopyDataStruct: PCopyDataStruct;
    Result: Longint;
  end;
{$endif}

{ SendWMCopyData sends WM_COPYDATA message.
  Sender is set to SenderWnd, dwData to dwData, cbData to Length(strdata)
  and lpData to PChar(strData) }
procedure SendWMCopyData(hWnd:THandle; dwData:DWORD; const strData:string; SenderWnd:THandle {$ifdef DEFPARS}= INVALID_HANDLE_VALUE{$endif}); overload;
{ ReceiveWMCopyData_str returns string passed by SendWMCopyData :
  that is, string that has length of cbData and it's contents are COPY
  OF THE CONTENTS lpData. It's important that they are a copy because after
  processing WM_COPYDATA message memory pointed by lpData may not be accessible
  for the application - but string obtained by ReceiveWMCopyData_str will
  be valid. }
function ReceiveWMCopyData_str(const msg:TWMCopyData):string;

{ Zwracaja wyniki funkcji Windowsa GetModuleFilename, GetClassName w
    postaci porzadnego pascalowego stringa (dbaja zeby zawsze zaladowac cala nazwe
    bez wzgledu na to jak jest dluga itd.)
  Notka co do GetWindowLong(,GWL_HINSTANCE) i GetClassLong(GCL_HMODULE)
    ktore z definicji zwracaja te same rzeczy i ktorych pewnie bedziesz czesto uzywac
    do dotarczenia argumentu funkcji GetModuleFilename_str : otoz przynajmniej pod moim
    Win95 sa one bez najmniejszych watpliwosci [...]; zwracaja wartosc rowna naszemu
    HInstance dla okien ktore w oczywisty sposob nie maja zwiazku z naszym programem.
    Nie widze na to wytlumaczenia i naprawde nie za bardzo mialem tu gdzie popelnic
    blad; Patrz console.testy\listwind po demo tego kretynizmu.
  Notka do GetModuleFilename : w helpie do WinAPI nie napisali ze ta funkcja
    zwraca string tylko dla h_Module zwiazanych z naszym procesem
    (tzn. naszego EXE'ca, EXE'ca innej instancji naszego programu w systemie lub
    DLL'i ktorych uzywamy). Jezeli masz h_Module identyfikujace jakis inny proces,
    GetModuleFilename zwroci 0 (blad) a GetModuleFilename_str zwroci ''
    (nie rzuci wyjatku - uznalem ze w tym przypadku wygodniejsze jest zeby zwrocilo
    specjalna wartosc).
}
function GetModuleFilename_str(h_Module:HMODULE):string;
function GetClassName_str(h_Wnd:HWnd):string;

{ znajdz okno klasy SearchWndClassName nalezace do modulu dla ktorego
  GetModuleFilename zwraca SearchModuleName. Zwraca INVALID_HANDLE_VALUE
  jesli nie znajdzie. }
function FindWindowOfModule(const SearchModuleName, SearchWndClassName:string):HWnd;

{ wykonuje na wszystkich handlach ktore sa <> INVALID_HANDLE_VALUE
  KambiOSCheck( CloseHandle ) }
procedure CloseHandles(const handles:array of THandle);

{ this function takes a handle value, creates it duplicate with inheritable
  set as the parameter, destroys the original handle and returns as "handle"
  new handle. This is useful for changing inheritable attribute of
  the handle. But REMEMBER - this function, in fact, creates new handle
  and destroys the old one - so call this function BEFORE initing any
  work with that handle. }
procedure ChangeHandleInheritable(var handle:THandle; inheritable:boolean);

{ rob GetCursorPos(mousePos) aby odswiezac mousePos, albo rob GetMouseX albo
  GetMouseY. Obie funkcje NON-REENTRANT. }
var mousePos:Windows.TPoint;
function GetMouseX:integer;
function GetMouseY:integer;

{ Okienka robione prostymi messageBoxami. Jesli nie podasz 3-go parametru parent
  bedzie 0 (co spowoduje ze okno bedzie modalne dla calego procesu ze wzgledu na flage
  MB_TASKMODAL w wywolaniu MessageBox). }
function PytanieBox    (const tekst:string; const caption:string {$ifdef DEFPARS}='Question'          {$endif}; parent:HWND {$ifdef DEFPARS}=0{$endif}):boolean; overload;
procedure ErrorBox     (const tekst:string; const caption:string {$ifdef DEFPARS}='Error'             {$endif}; parent:HWND {$ifdef DEFPARS}=0{$endif});         overload;
procedure InfoBox      (const tekst:string; const caption:string {$ifdef DEFPARS}='Information'       {$endif}; parent:HWND {$ifdef DEFPARS}=0{$endif});         overload;
procedure WarningBox   (const tekst:string; const caption:string {$ifdef DEFPARS}='Warning'           {$endif}; parent:HWND {$ifdef DEFPARS}=0{$endif});         overload;
function WarnPytanieBox(const tekst:string; const caption:string {$ifdef DEFPARS}='Important question'{$endif}; parent:HWND {$ifdef DEFPARS}=0{$endif}):boolean; overload;

{ WMSizing : sposob aby przypisac minimalne wymiary formularzowi w Delphi < 5 }
type
  TWMSizing=record
    Msg: Cardinal;    {=wm_sizing}
    fwSide: Longint;  {WMSZ_xxx stale}
    lprc: PRect;      {wskaznik na prostokat = obramowanie okna}
    Result: LongBool; {powinienem zwrocic longbool(true)}
  end;
function WMSizingHandler(lParm:LPARAM; wParm:WPARAM; minWidth,minHeight, maxWidth,maxHeight:integer):LRESULT; overload;
procedure WMSizingHandler(var Msg:TWMSizing; minWidth,minHeight, maxWidth,maxHeight:integer); overload;
procedure WMSizingHandler(var Msg:TWMSizing; minWidth,minHeight:integer); overload;

{$ifdef FPC}
type
  TWMDropFiles = packed record
    Msg: Cardinal;
    Drop: THANDLE;
    Unused: Longint;
    Result: Longint;
  end;
{$endif}

type TStringObjProc = procedure (const s:string) of object;
{ WMDropFiles handler wywoluje dla kazdego pliku przekazanego przez dane
  WM_DROPFILES procedure FileProc jako parametr podajac nazwe kolejnego pliku.
  Przekaz true aby wywolac takze DragFinish na koncu. (Ten parametr jest tu po to
  zebys nie zapomnial nigdy wywolac DragFinish) }
procedure WMDropFilesHandler(var msg:TWMDropFiles; FileProc:TStringObjProc; CallDragFinish:boolean);

function MessageDescr(const Mesg:TMsg):string; overload;
function MessageDescr(hWnd: HWND; MessageNum: UINT; wParm: WPARAM; lParm: LPARAM):string; overload;

{ poprawione deklaracje z WinAPI }
function ChangeDisplaySettings(lpDevMode:PDEVMODE; dwFlags:DWORD):longint;
  stdcall; external userDLL name 'ChangeDisplaySettingsA';

{ This is useful for debugging ChangeDisplaySettings result. }
function DispChangeToStr(L: LongInt): string;

{$endif read_interface}

{$ifdef read_implementation}

function OneInstanceCode(MainProgram:TProcedure; CodeIdent:string):boolean;
var HMutex:THandle;
begin
 HMutex := CreateMutex (nil, False, PChar(CodeIdent));
 try
  { We try to to take ownership of the mutex.
    We'll fail if mutex will not be in signalled state (which means some process
    already has the mutex) and then WaitForSingleObject will return wait_TimeOut.
    Otherwise, if result <> wait_TimeOut, it will mean we succeeded and
     1) mutex we're in signalled state, so we're only one copy of the app in system
     2) now we own mutex so now mutex in nonsignalled. }
  result:=WaitForSingleObject (hMutex, 0) <> wait_TimeOut;
  if result then
  begin
   try MainProgram;
   finally
    {Windows should do ReleaseMutex + CloseHandle automatically when our process
     finishes. But we don't trust Windows, besides no one said 'OneInstanceCode'
     function must manage calym programem - funkcja OneInstanceCode moze
     byc uzywana dla dowolnych kawalkow kodu, dlatego musimy byc eleganccy i
     ladnie zamykac i zwalniac uzywane mutexy. }
    ReleaseMutex(HMutex);
   end;
  end
 finally CloseHandle(HMutex) end;
end;

function OneInstanceCode(MainProgram:TProcedure):boolean;
begin
 result:=OneInstanceCode(MainProgram, ProgramName+'_one_instance_mutex');
end;

procedure SendWMCopyData(hWnd:THandle; dwData:DWORD; const strData:string; SenderWnd:THandle);
var data:TCopyDataStruct;
begin
 data.dwData:=dwData;
 data.cbData:=Length(strData);
 data.lpData:=PCharOrNil(strData);
 SendMessage(hWnd, WM_COPYDATA, WPARAM(SenderWnd), LPARAM(@data));
end;

function ReceiveWMCopyData_str(const msg:TWMCopyData):string;
begin
 SetString(result, PChar(msg.CopyDataStruct.lpData), msg.CopyDataStruct.cbData);
end;

function GetModuleFilename_str(h_Module:HMODULE):string;
var copied:DWORD;
begin
 result:='';
 repeat
  SetLength(result, Length(result)+100);
  copied:=GetModuleFilename(h_Module, PChar(result), Length(result)+1);

  { Nie sprawdzaj czy copied<>0. Jezeli copied = 0 to po prostu zroc result = ''.
    Wiec ponizsza linijka zakomentarzowana.
    KambiOSCheck( copied<>0, 'GetModuleFilename');}

  { parametr nSize podaje ile mamy miejsca na string razem z koncowym zerem.
    Wiec podajemy Length(result)+1. copied, czyli wynik GetModuleName, mowi
    ile znakow zostalo skopiowanych nie liczac zera. Wiec maksymalnie copied
    moze byc rowne Length(result) i wtedy wlasnie nie wiadomo czy bylo
    za malo miejsca czy tez bylo go w sam raz. Dopiero gdy copied jest
    ostro mniejsze od Length(result) wiemy ze miejsca byla za duzo, a wiec
    uzyskalismy caly prawidlowy string. }
 until copied < Cardinal(Length(result));
 SetLength(result, copied); { obetnij do prawdziwej dlugosci }
end;

function GetClassName_str(h_Wnd:HWnd):string;
var copied:DWORD;
begin
 result:='';
 repeat
  SetLength(result, Length(result)+100);
  copied:=GetClassName(h_Wnd, PChar(result), Length(result)+1);
  KambiOSCheck( copied<>0, 'GetClassName');
  {strategia - patrz GetModuleFilename_str}
 until copied < Cardinal(Length(result));
 result:=PChar(result); { obetnij do prawdziwej dlugosci }
end;

type
  TFindWindowOfModule_Data = record
    SearchModuleName, SearchWndClassName : string;
    ResultPtr:^Hwnd;
  end;
  PFindWindowOfModule_Data = ^TFindWindowOfModule_Data;

function EnumWndProc (h_wnd: HWnd; param:LPARAM): Bool; stdcall;
var pdata:PFindWindowOfModule_Data;
begin
 pdata:=PFindWindowOfModule_Data(param);
 if (GetClassName_str(h_wnd) = pdata.SearchWndClassName) and
    { compare module names case-insensitive under Windows }
    SameFileName(
      GetModuleFilename_str( HMODULE(GetWindowLong(h_wnd, GWL_HINSTANCE)) ),
      pdata.SearchModuleName) then
 begin
  pdata.ResultPtr^:=h_Wnd;
  Result:=False;
 end else Result:=true;
end;

function FindWindowOfModule(const SearchModuleName, SearchWndClassName:string):HWnd;
var data:TFindWindowOfModule_Data;
begin
 data.SearchModuleName:=SearchModuleName;
 data.SearchWndClassName:=SearchWndClassName;
 data.ResultPtr:=@Result;
 Result:=INVALID_HANDLE_VALUE;
 EnumWindows (@EnumWndProc, LongInt(@data));
end;

procedure CloseHandles(const handles:array of THandle);
var i:cardinal;
begin
 for i:=0 to High(handles) do
  if handles[i]<>INVALID_HANDLE_VALUE then
   KambiOSCheck( CloseHandle(handles[i]), 'Error in CloseHandle by CloseHandles' );
end;

procedure ChangeHandleInheritable(var handle:THandle; inheritable:boolean);
var newhandle:THandle;
begin
 KambiOSCheck( DuplicateHandle(GetCurrentProcess, handle, GetCurrentProcess, @newhandle,
   0, inheritable, DUPLICATE_SAME_ACCESS), 'DuplicateHandle' );
 KambiOSCheck( CloseHandle(handle), 'CloseHandle');
 handle:=newhandle;
end;

function GetMouseX:integer;
begin
 GetCursorPos(mousePos);
 result:=mousePos.x;
end;

function GetMouseY:integer;
begin
 GetCursorPos(mousePos);
 result:=mousePos.y;
end;

{ proste opakowania dla MessageBox ------------------------------------------- }

function PytanieBox(const tekst:string; const caption:string; parent:HWND):boolean;
begin
 result:=MessageBox(parent,PChar(tekst),PCHar(caption),MB_YESNO or MB_ICONQUESTION or MB_TASKMODAL)=IDYES;
end;

procedure ErrorBox(const tekst:string; const caption:string; parent:HWND);
begin
 MessageBox(parent,PChar(tekst),PChar(caption),MB_OK or MB_ICONERROR or MB_TASKMODAL);
end;

procedure InfoBox(const tekst:string; const caption:string;parent:HWND);
begin
 MessageBox(parent,PChar(tekst),PChar(caption),MB_OK or MB_ICONINFORMATION or MB_TASKMODAL);
end;

procedure WarningBox(const tekst:string; const caption:string;parent:HWND);
begin
 MessageBox(parent,PChar(tekst),PChar(caption),MB_OK or MB_ICONWARNING or MB_TASKMODAL);
end;

function WarnPytanieBox(const tekst:string; const caption:string{='Wazne pytanie'}; parent:HWND{=0}):boolean;
begin
 result:=MessageBox(parent,PChar(tekst),PCHar(caption),MB_YESNO or MB_ICONWARNING or MB_TASKMODAL)=IDYES;
end;

{ minX / minY / maxX / maxY : WM_SIZING handler -----------------------------

 Dodaj do formularza proc.
   procedure WMSizing(var Msg:TWMSizing); message wm_Sizing;
 i zapisz ja jako
   procedure TForm1.WMSizing(var Msg:TWMSizing);
   begin
    WMSizingHandler(msg,100,200, 400,400);
   end;
 gdzie liczby 100 i 200 sa przykladowe. Otrzymasz formularz ktory nie moze byc resizowany
 do rozmiartu mniejszego od 100 x 200 ani wiekszego niz 400x400.
 Uwaga 1 : to nie ma wplywu na rozmiar formularza po "maximize" przez usera -
           - user moze tym maximizem przekroczyc maxy lub miny (jesli ustawiles miny
           na wieksze niz rozmiar ekranu...); jesli chcesz miec pewnosc, wylacz userowi
           mozliwosc maximize'a
       2 : to nie ma wplywu na rozmiar formularza jaki sam nadajesz (programowo).
           Ustaw wiec dobre wartosci poczatkowe i nie ustawiaj sam zlych rozmiarow
           formularzowi.
}

function WMSizingHandler(lParm:LPARAM; wParm:WPARAM; minWidth,minHeight, maxWidth,maxHeight:integer):LRESULT;
var msg:TWMSizing;
begin
 msg.Msg:=WM_SIZING;
 msg.fwSide:=wParm;
 msg.lprc:=PRect(lParm);
 WMSizingHandler(msg, minWidth,minHeight, maxWidth,maxHeight);
 result:=LRESULT(msg.Result);
end;

procedure WMSizingHandler(var Msg:TWMSizing; minWidth,minHeight, maxWidth,maxHeight:integer); overload;
var w,h:integer;
begin
 assert(minWidth<=maxWidth);
 assert(minHeight<=maxHeight);
 with msg.lprc^ do
 begin
  w:=right-left;
  if w<minWidth then right:=left+minWidth else
   if w>maxWidth then right:=left+maxWidth;
  h:=bottom-top;
  if h<minHeight then bottom:=top+minHeight else
   if h>maxHeight then bottom:=top+maxHeight;
 end;
 msg.result:=true;
end;

procedure WMSizingHandler(var Msg:TWMSizing; minWidth,minHeight :integer);
begin
 with msg.lprc^ do
 begin
  if right-left<minWidth then right:=left+minWidth;
  if bottom-top<minHeight then bottom:=top+minHeight;
 end;
 msg.result:=true;
end;

{ WM_DROPFILES handler --------------------------------------------------------- }

procedure WMDropFilesHandler(var msg:TWMDropFiles; FileProc:TStringObjProc; CallDragFinish:boolean);
var fcount:UINT;
    fname:string;
    i:integer;
begin
 fcount:=DragQueryFile(msg.Drop, $FFFFFFFF, nil, 0);
 for i:=0 to fcount-1 do
 begin
  SetLength(fname, DragQueryFile(msg.Drop, i, nil, 0));
  DragQueryFile(msg.Drop, i, PChar(fname), Length(fname)+1);
  FileProc(fname);
 end;
 if CallDragFinish then DragFinish(msg.Drop);
end;

{ dobra procedurka do debuggowania przeplywu messagy pod windowsa --------- }

function MessageDescr(const Mesg:TMsg):string;
begin
 with Mesg do result:=MessageDescr(hWnd, Message, wParam, lParam);
end;

function MessageDescr(hWnd: HWND; MessageNum: UINT; wParm: WPARAM; lParm: LPARAM):string;
var MsgName, MsgInfo:string;
begin
 MsgInfo:='';
 case MessageNum of
   WM_NULL : begin MsgName:='WM_NULL'; end;
   WM_CREATE : begin MsgName:='WM_CREATE'; end;
   WM_DESTROY : begin MsgName:='WM_DESTROY'; end;
   WM_MOVE : begin MsgName:='WM_MOVE'; end;
   WM_SIZE : begin MsgName:='WM_SIZE'; end;
   WM_ACTIVATE : begin MsgName:='WM_ACTIVATE'; end;
   WM_SETFOCUS : begin MsgName:='WM_SETFOCUS'; end;
   WM_KILLFOCUS : begin MsgName:='WM_KILLFOCUS'; end;
   WM_ENABLE : begin MsgName:='WM_ENABLE'; end;
   WM_SETREDRAW : begin MsgName:='WM_SETREDRAW'; end;
   WM_SETTEXT : begin MsgName:='WM_SETTEXT'; end;
   WM_GETTEXT : begin MsgName:='WM_GETTEXT'; end;
   WM_GETTEXTLENGTH : begin MsgName:='WM_GETTEXTLENGTH'; end;
   WM_PAINT : begin MsgName:='WM_PAINT'; end;
   WM_CLOSE : begin MsgName:='WM_CLOSE'; end;
   WM_QUERYENDSESSION : begin MsgName:='WM_QUERYENDSESSION'; end;
   WM_QUIT : begin MsgName:='WM_QUIT';
      MsgInfo:='ExitCode : '+IntToStr(Integer(wParm));
     end;
   WM_QUERYOPEN : begin MsgName:='WM_QUERYOPEN'; end;
   WM_ERASEBKGND : begin MsgName:='WM_ERASEBKGND'; end;
   WM_SYSCOLORCHANGE : begin MsgName:='WM_SYSCOLORCHANGE'; end;
   WM_ENDSESSION : begin MsgName:='WM_ENDSESSION'; end;
   WM_SHOWWINDOW : begin MsgName:='WM_SHOWWINDOW'; end;
   WM_CTLCOLOR : begin MsgName:='WM_CTLCOLOR'; end;
   WM_WININICHANGE : begin MsgName:='WM_WININICHANGE = WM_SETTINGCHANGE'; end;
   WM_DEVMODECHANGE : begin MsgName:='WM_DEVMODECHANGE'; end;
   WM_ACTIVATEAPP : begin MsgName:='WM_ACTIVATEAPP'; end;
   WM_FONTCHANGE : begin MsgName:='WM_FONTCHANGE'; end;
   WM_TIMECHANGE : begin MsgName:='WM_TIMECHANGE'; end;
   WM_CANCELMODE : begin MsgName:='WM_CANCELMODE'; end;
   WM_SETCURSOR : begin MsgName:='WM_SETCURSOR'; end;
   WM_MOUSEACTIVATE : begin MsgName:='WM_MOUSEACTIVATE'; end;
   WM_CHILDACTIVATE : begin MsgName:='WM_CHILDACTIVATE'; end;
   WM_QUEUESYNC : begin MsgName:='WM_QUEUESYNC'; end;
   WM_GETMINMAXINFO : begin MsgName:='WM_GETMINMAXINFO'; end;
   WM_PAINTICON : begin MsgName:='WM_PAINTICON'; end;
   WM_ICONERASEBKGND : begin MsgName:='WM_ICONERASEBKGND'; end;
   WM_NEXTDLGCTL : begin MsgName:='WM_NEXTDLGCTL'; end;
   WM_SPOOLERSTATUS : begin MsgName:='WM_SPOOLERSTATUS'; end;
   WM_DRAWITEM : begin MsgName:='WM_DRAWITEM'; end;
   WM_MEASUREITEM : begin MsgName:='WM_MEASUREITEM'; end;
   WM_DELETEITEM : begin MsgName:='WM_DELETEITEM'; end;
   WM_VKEYTOITEM : begin MsgName:='WM_VKEYTOITEM'; end;
   WM_CHARTOITEM : begin MsgName:='WM_CHARTOITEM'; end;
   WM_SETFONT : begin MsgName:='WM_SETFONT'; end;
   WM_GETFONT : begin MsgName:='WM_GETFONT'; end;
   WM_SETHOTKEY : begin MsgName:='WM_SETHOTKEY'; end;
   WM_GETHOTKEY : begin MsgName:='WM_GETHOTKEY'; end;
   WM_QUERYDRAGICON : begin MsgName:='WM_QUERYDRAGICON'; end;
   WM_COMPAREITEM : begin MsgName:='WM_COMPAREITEM'; end;
   WM_COMPACTING : begin MsgName:='WM_COMPACTING'; end;
   WM_WINDOWPOSCHANGING : begin MsgName:='WM_WINDOWPOSCHANGING'; end;
   WM_WINDOWPOSCHANGED : begin MsgName:='WM_WINDOWPOSCHANGED'; end;
   WM_POWER : begin MsgName:='WM_POWER'; end;
   WM_COPYDATA : begin MsgName:='WM_COPYDATA'; end;
   WM_CANCELJOURNAL : begin MsgName:='WM_CANCELJOURNAL'; end;
   WM_NOTIFY : begin MsgName:='WM_NOTIFY'; end;
   WM_INPUTLANGCHANGEREQUEST : begin MsgName:='WM_INPUTLANGCHANGEREQUEST'; end;
   WM_INPUTLANGCHANGE : begin MsgName:='WM_INPUTLANGCHANGE'; end;
   WM_TCARD : begin MsgName:='WM_TCARD'; end;
   WM_HELP : begin MsgName:='WM_HELP'; end;
   WM_USERCHANGED : begin MsgName:='WM_USERCHANGED'; end;
   WM_NOTIFYFORMAT : begin MsgName:='WM_NOTIFYFORMAT'; end;
   WM_CONTEXTMENU : begin MsgName:='WM_CONTEXTMENU'; end;
   WM_STYLECHANGING : begin MsgName:='WM_STYLECHANGING'; end;
   WM_STYLECHANGED : begin MsgName:='WM_STYLECHANGED'; end;
   WM_DISPLAYCHANGE : begin MsgName:='WM_DISPLAYCHANGE'; end;
   WM_GETICON : begin MsgName:='WM_GETICON'; end;
   WM_SETICON : begin MsgName:='WM_SETICON'; end;
   WM_NCCREATE : begin MsgName:='WM_NCCREATE'; end;
   WM_NCDESTROY : begin MsgName:='WM_NCDESTROY'; end;
   WM_NCCALCSIZE : begin MsgName:='WM_NCCALCSIZE'; end;
   WM_NCHITTEST : begin MsgName:='WM_NCHITTEST'; end;
   WM_NCPAINT : begin MsgName:='WM_NCPAINT'; end;
   WM_NCACTIVATE : begin MsgName:='WM_NCACTIVATE'; end;
   WM_GETDLGCODE : begin MsgName:='WM_GETDLGCODE'; end;
   WM_NCMOUSEMOVE : begin MsgName:='WM_NCMOUSEMOVE'; end;
   WM_NCLBUTTONDOWN : begin MsgName:='WM_NCLBUTTONDOWN'; end;
   WM_NCLBUTTONUP : begin MsgName:='WM_NCLBUTTONUP'; end;
   WM_NCLBUTTONDBLCLK : begin MsgName:='WM_NCLBUTTONDBLCLK'; end;
   WM_NCRBUTTONDOWN : begin MsgName:='WM_NCRBUTTONDOWN'; end;
   WM_NCRBUTTONUP : begin MsgName:='WM_NCRBUTTONUP'; end;
   WM_NCRBUTTONDBLCLK : begin MsgName:='WM_NCRBUTTONDBLCLK'; end;
   WM_NCMBUTTONDOWN : begin MsgName:='WM_NCMBUTTONDOWN'; end;
   WM_NCMBUTTONUP : begin MsgName:='WM_NCMBUTTONUP'; end;
   WM_NCMBUTTONDBLCLK : begin MsgName:='WM_NCMBUTTONDBLCLK'; end;
   WM_KEYDOWN : begin MsgName:='WM_KEYDOWN'; end;
   WM_KEYUP : begin MsgName:='WM_KEYUP'; end;
   WM_CHAR : begin MsgName:='WM_CHAR';
      MsgInfo:='Char #'+IntToStr(wParm)+' ('+Char(wParm)+')';
     end;
   WM_DEADCHAR : begin MsgName:='WM_DEADCHAR'; end;
   WM_SYSKEYDOWN : begin MsgName:='WM_SYSKEYDOWN'; end;
   WM_SYSKEYUP : begin MsgName:='WM_SYSKEYUP'; end;
   WM_SYSCHAR : begin MsgName:='WM_SYSCHAR'; end;
   WM_SYSDEADCHAR : begin MsgName:='WM_SYSDEADCHAR'; end;
   WM_KEYLAST : begin MsgName:='WM_KEYLAST'; end;
   WM_INITDIALOG : begin MsgName:='WM_INITDIALOG'; end;
   WM_COMMAND : begin MsgName:='WM_COMMAND'; end;
   WM_SYSCOMMAND : begin MsgName:='WM_SYSCOMMAND'; end;
   WM_TIMER : begin MsgName:='WM_TIMER'; end;
   WM_HSCROLL : begin MsgName:='WM_HSCROLL'; end;
   WM_VSCROLL : begin MsgName:='WM_VSCROLL'; end;
   WM_INITMENU : begin MsgName:='WM_INITMENU'; end;
   WM_INITMENUPOPUP : begin MsgName:='WM_INITMENUPOPUP'; end;
   WM_MENUSELECT : begin MsgName:='WM_MENUSELECT'; end;
   WM_MENUCHAR : begin MsgName:='WM_MENUCHAR'; end;
   WM_ENTERIDLE : begin MsgName:='WM_ENTERIDLE'; end;
   WM_CTLCOLORMSGBOX : begin MsgName:='WM_CTLCOLORMSGBOX'; end;
   WM_CTLCOLOREDIT : begin MsgName:='WM_CTLCOLOREDIT'; end;
   WM_CTLCOLORLISTBOX : begin MsgName:='WM_CTLCOLORLISTBOX'; end;
   WM_CTLCOLORBTN : begin MsgName:='WM_CTLCOLORBTN'; end;
   WM_CTLCOLORDLG : begin MsgName:='WM_CTLCOLORDLG'; end;
   WM_CTLCOLORSCROLLBAR : begin MsgName:='WM_CTLCOLORSCROLLBAR'; end;
   WM_CTLCOLORSTATIC : begin MsgName:='WM_CTLCOLORSTATIC'; end;
   WM_MOUSEMOVE : begin MsgName:='WM_MOUSEMOVE'; end;
   WM_LBUTTONDOWN : begin MsgName:='WM_LBUTTONDOWN'; end;
   WM_LBUTTONUP : begin MsgName:='WM_LBUTTONUP'; end;
   WM_LBUTTONDBLCLK : begin MsgName:='WM_LBUTTONDBLCLK'; end;
   WM_RBUTTONDOWN : begin MsgName:='WM_RBUTTONDOWN'; end;
   WM_RBUTTONUP : begin MsgName:='WM_RBUTTONUP'; end;
   WM_RBUTTONDBLCLK : begin MsgName:='WM_RBUTTONDBLCLK'; end;
   WM_MBUTTONDOWN : begin MsgName:='WM_MBUTTONDOWN'; end;
   WM_MBUTTONUP : begin MsgName:='WM_MBUTTONUP'; end;
   WM_MBUTTONDBLCLK : begin MsgName:='WM_MBUTTONDBLCLK'; end;
   WM_PARENTNOTIFY : begin MsgName:='WM_PARENTNOTIFY'; end;
   WM_ENTERMENULOOP : begin MsgName:='WM_ENTERMENULOOP'; end;
   WM_EXITMENULOOP : begin MsgName:='WM_EXITMENULOOP'; end;
   WM_SIZING : begin MsgName:='WM_SIZING'; end;
   WM_CAPTURECHANGED : begin MsgName:='WM_CAPTURECHANGED'; end;
   WM_MOVING : begin MsgName:='WM_MOVING'; end;
   WM_POWERBROADCAST : begin MsgName:='WM_POWERBROADCAST'; end;
   WM_DEVICECHANGE : begin MsgName:='WM_DEVICECHANGE'; end;
   WM_IME_STARTCOMPOSITION : begin MsgName:='WM_IME_STARTCOMPOSITION'; end;
   WM_IME_ENDCOMPOSITION : begin MsgName:='WM_IME_ENDCOMPOSITION'; end;
   WM_IME_COMPOSITION : begin MsgName:='WM_IME_COMPOSITION'; end;
   WM_IME_SETCONTEXT : begin MsgName:='WM_IME_SETCONTEXT'; end;
   WM_IME_NOTIFY : begin MsgName:='WM_IME_NOTIFY'; end;
   WM_IME_CONTROL : begin MsgName:='WM_IME_CONTROL'; end;
   WM_IME_COMPOSITIONFULL : begin MsgName:='WM_IME_COMPOSITIONFULL'; end;
   WM_IME_SELECT : begin MsgName:='WM_IME_SELECT'; end;
   WM_IME_CHAR : begin MsgName:='WM_IME_CHAR'; end;
   WM_IME_KEYDOWN : begin MsgName:='WM_IME_KEYDOWN'; end;
   WM_IME_KEYUP : begin MsgName:='WM_IME_KEYUP'; end;
   WM_MDICREATE : begin MsgName:='WM_MDICREATE'; end;
   WM_MDIDESTROY : begin MsgName:='WM_MDIDESTROY'; end;
   WM_MDIACTIVATE : begin MsgName:='WM_MDIACTIVATE'; end;
   WM_MDIRESTORE : begin MsgName:='WM_MDIRESTORE'; end;
   WM_MDINEXT : begin MsgName:='WM_MDINEXT'; end;
   WM_MDIMAXIMIZE : begin MsgName:='WM_MDIMAXIMIZE'; end;
   WM_MDITILE : begin MsgName:='WM_MDITILE'; end;
   WM_MDICASCADE : begin MsgName:='WM_MDICASCADE'; end;
   WM_MDIICONARRANGE : begin MsgName:='WM_MDIICONARRANGE'; end;
   WM_MDIGETACTIVE : begin MsgName:='WM_MDIGETACTIVE'; end;
   WM_MDISETMENU : begin MsgName:='WM_MDISETMENU'; end;
   WM_ENTERSIZEMOVE : begin MsgName:='WM_ENTERSIZEMOVE'; end;
   WM_EXITSIZEMOVE : begin MsgName:='WM_EXITSIZEMOVE'; end;
   WM_DROPFILES : begin MsgName:='WM_DROPFILES'; end;
   WM_MDIREFRESHMENU : begin MsgName:='WM_MDIREFRESHMENU'; end;
   WM_CUT : begin MsgName:='WM_CUT'; end;
   WM_COPY : begin MsgName:='WM_COPY'; end;
   WM_PASTE : begin MsgName:='WM_PASTE'; end;
   WM_CLEAR : begin MsgName:='WM_CLEAR'; end;
   WM_UNDO : begin MsgName:='WM_UNDO'; end;
   WM_RENDERFORMAT : begin MsgName:='WM_RENDERFORMAT'; end;
   WM_RENDERALLFORMATS : begin MsgName:='WM_RENDERALLFORMATS'; end;
   WM_DESTROYCLIPBOARD : begin MsgName:='WM_DESTROYCLIPBOARD'; end;
   WM_DRAWCLIPBOARD : begin MsgName:='WM_DRAWCLIPBOARD'; end;
   WM_PAINTCLIPBOARD : begin MsgName:='WM_PAINTCLIPBOARD'; end;
   WM_VSCROLLCLIPBOARD : begin MsgName:='WM_VSCROLLCLIPBOARD'; end;
   WM_SIZECLIPBOARD : begin MsgName:='WM_SIZECLIPBOARD'; end;
   WM_ASKCBFORMATNAME : begin MsgName:='WM_ASKCBFORMATNAME'; end;
   WM_CHANGECBCHAIN : begin MsgName:='WM_CHANGECBCHAIN'; end;
   WM_HSCROLLCLIPBOARD : begin MsgName:='WM_HSCROLLCLIPBOARD'; end;
   WM_QUERYNEWPALETTE : begin MsgName:='WM_QUERYNEWPALETTE'; end;
   WM_PALETTEISCHANGING : begin MsgName:='WM_PALETTEISCHANGING'; end;
   WM_PALETTECHANGED : begin MsgName:='WM_PALETTECHANGED'; end;
   WM_HOTKEY : begin MsgName:='WM_HOTKEY'; end;
   WM_PRINT : begin MsgName:='WM_PRINT'; end;
   WM_PRINTCLIENT : begin MsgName:='WM_PRINTCLIENT'; end;
   WM_PENWINFIRST : begin MsgName:='WM_PENWINFIRST'; end;
   WM_PENWINLAST : begin MsgName:='WM_PENWINLAST'; end;
   WM_USER : begin MsgName:='WM_USER'; end;
 end;

 { TODO: it's probably FPC 1.0.x bug that forces me to include this
   NoRQCheckBegin/End here, test with FPC 1.9.3 }
 {$I NoRQCheckBegin.inc}
 result:=Format('Message %s (number %d) to window %d. WParam %d, LParam %d.',
                [MsgName, MessageNum, hWnd, wParm, lParm] );
 {$I NoRQCheckEnd.inc}

 if MsgInfo<>'' then
  result:=result +nl +MsgInfo;
end;

function DispChangeToStr(L: LongInt): string;
begin
 case L of
  DISP_CHANGE_SUCCESSFUL: Result := 'DISP_CHANGE_SUCCESSFUL';
  DISP_CHANGE_RESTART: Result := 'DISP_CHANGE_RESTART';
  DISP_CHANGE_BADFLAGS: Result := 'DISP_CHANGE_BADFLAGS';
  DISP_CHANGE_FAILED: Result := 'DISP_CHANGE_FAILED';
  DISP_CHANGE_BADMODE: Result := 'DISP_CHANGE_BADMODE';
  DISP_CHANGE_NOTUPDATED: Result := 'DISP_CHANGE_NOTUPDATED';
  else Result := IntToStr(L);
 end;
end;

procedure InitializationOSSpecific;
begin
end;

procedure FinalizationOSSpecific;
begin
end;

{$endif read_implementation}