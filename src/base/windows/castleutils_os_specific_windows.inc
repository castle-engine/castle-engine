{
  Copyright 2000-2018 Michalis Kamburelis.

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
  - TCastleWindowBase.MessageOK, TCastleWindowBase.MessageYesNo if you use
    CastleWindow.
  - or LCL dialog routines (like ShowMessage and everything in Dialogs unit)
    if you use LCL.

  @groupBegin }
procedure ErrorBox  (const Text: String; const caption: String ='Error'      ; parent: HWND =0); deprecated;
procedure InfoBox   (const Text: String; const caption: String ='Information'; parent: HWND =0); deprecated;
procedure WarningBox(const Text: String; const caption: String ='Warning'    ; parent: HWND =0); deprecated;
{ @groupEnd }

{ WinAPI ChangeDisplaySettings declaration. (Was wrong/missing in old
  FPC versions? TODO: check and probably remove this.) }
function ChangeDisplaySettings(lpDevMode: PDEVMODE; dwFlags: DWORD): LongInt;
  stdcall; external userDLL name 'ChangeDisplaySettingsA';

{ Describe ChangeDisplaySettings result. }
function DispChangeToStr(L: LongInt): String;

{ Get exe name through WinAPI GetModuleFileNameW.

  @exclude This is internal.

  Using ParamStr(0) on Windows is buggy in case the directory name contains local characters
  that cannot be expressed in current Windows locale.
  E.g. if parent directory name contains Chinese or Russian characters on Polish Windows.
  It seems it's because FPC uses GetModuleFileNameA for this.

  Using GetModuleFileNameW instead works cool.

  TODO: submit to FPC,
  https://trello.com/c/7Xfek9e4/15-submit-to-fpc-testcase-need-to-use-getmodulefilenamew-instead-of-getmodulefilenamea }
function ExeNameFromGetModule: String;

{$endif read_interface}

{$ifdef read_implementation}

procedure ErrorBox(const Text: String; const caption: String; parent: HWND);
begin
 MessageBox(parent,PChar(Text),PChar(caption),MB_OK or MB_ICONERROR or MB_TASKMODAL);
end;

procedure InfoBox(const Text: String; const caption: String;parent: HWND);
begin
 MessageBox(parent,PChar(Text),PChar(caption),MB_OK or MB_ICONINFORMATION or MB_TASKMODAL);
end;

procedure WarningBox(const Text: String; const caption: String;parent: HWND);
begin
 MessageBox(parent,PChar(Text),PChar(caption),MB_OK or MB_ICONWARNING or MB_TASKMODAL);
end;

function DispChangeToStr(L: LongInt): String;
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

function ExeNameFromGetModule: String;
var
  S: UnicodeString;
begin
  SetLength(S, MAX_PATH);
  if GetModuleFileNameW(0, PWideChar(@S[1]), MAX_PATH) = 0 then
  begin
    // WritelnWarning('GetModuleFileNameW failed. We will use old method to determine ExeName, which will fail if parent directory contains local characters');
    Exit(ParamStr(0)); // fallback to old method
  end;
  SetLength(S, StrLen(PWideChar(S))); // It's only null-terminated after WinAPI call, set actual length for Pascal UnicodeString
  Result := UTF8Encode(S);
end;

{$endif read_implementation}
