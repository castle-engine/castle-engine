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
