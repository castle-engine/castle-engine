{
  Copyright 2000-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Some WinAPI helpers }

{$ifdef read_interface}

const
  KernelDLL = 'kernel32.dll';
  UserDLL = 'user32.dll';
  GdiDLL = 'gdi32.dll';
  ShellDLL = 'shell32.dll';

{ Simple standard Windows message boxes.
  If Parent is 0 (default), the box will be a modal box for the whole process
  (all our windows), since we use MB_TASKMODAL.

  @deprecated Deprecated. Instead of these it's much better to use:
  - TCastleWindowCustom.MessageOK, TCastleWindowCustom.MessageYesNo if you use
    CastleWindow.
  - or LCL dialog routines (like ShowMessage and everything in Dialogs unit)
    if you use LCL.

  @groupBegin }
procedure ErrorBox  (const Text:string; const caption:string ='Error'      ; parent:HWND =0); deprecated;
procedure InfoBox   (const Text:string; const caption:string ='Information'; parent:HWND =0); deprecated;
procedure WarningBox(const Text:string; const caption:string ='Warning'    ; parent:HWND =0); deprecated;
{ @groupEnd }

type
  { Handling WMSizing message is a way to force minimum/maximum form
    sizes under WinAPI. }
  TWMSizing=record
    Msg: Cardinal;    {< wm_SIZING }
    fwSide: Longint;  {< const WMSZ_xxx }
    lprc: PRect;      {< rectangle with window sizes }
    Result: LongBool; {< should return longbool(true) }
  end;
function WMSizingHandler(lParm:LPARAM; wParm:WPARAM; minWidth,minHeight, maxWidth,maxHeight:integer):LRESULT; overload;
procedure WMSizingHandler(var Msg:TWMSizing; minWidth,minHeight, maxWidth,maxHeight:integer); overload;
procedure WMSizingHandler(var Msg:TWMSizing; minWidth,minHeight:integer); overload;

{ WinAPI ChangeDisplaySettings declaration. (Was wrong/missing in old
  FPC versions? TODO: check and probably remove this.) }
function ChangeDisplaySettings(lpDevMode:PDEVMODE; dwFlags:DWORD):longint;
  stdcall; external userDLL name 'ChangeDisplaySettingsA';

{ Describe ChangeDisplaySettings result. }
function DispChangeToStr(L: LongInt): string;

{$endif read_interface}

{$ifdef read_implementation}

procedure ErrorBox(const Text:string; const caption:string; parent:HWND);
begin
 MessageBox(parent,PChar(Text),PChar(caption),MB_OK or MB_ICONERROR or MB_TASKMODAL);
end;

procedure InfoBox(const Text:string; const caption:string;parent:HWND);
begin
 MessageBox(parent,PChar(Text),PChar(caption),MB_OK or MB_ICONINFORMATION or MB_TASKMODAL);
end;

procedure WarningBox(const Text:string; const caption:string;parent:HWND);
begin
 MessageBox(parent,PChar(Text),PChar(caption),MB_OK or MB_ICONWARNING or MB_TASKMODAL);
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
