{
  Copyright 2002-2004 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ WinAPI's GetLastError / Libc.ErrNo / BaseUnix.ErrNo error checking }

{ I don't ever use RaiseLastOSError or OSCheck or EOSError,
  instead I'm using my own EKambiOSError, KambiOSCheck, RaiseLastKambiOSError.
  This is the safest way when trying to be compatible with FPC 1.0.x, 1.9.x 
  and Windows Delphi. And when I want to have possibility to use Libc unit,
  at least under Linux, preferably also under other UNIXes. }

{$ifdef read_interface}

type
  EKambiOSError = class(Exception)
    { code from LastKambiOSError }
    ErrorCode: LongWord;
    constructor Create(AErrorCode: LongWord); overload;
    constructor Create(AErrorCode: LongWord; const AdditionalDescription: string); overload;
  end;

{ error code from 
  - BaseUnix.ErrNo under UNIXes when USE_LIBC not defined
  - Libc.ErrNo under UNIXes when USE_LIBC is defined
  - Windows.GetLastError under Windows }
function LastKambiOSError: LongWord;

{ raise EKambiOSError with LastKambiOSError (even when LastKambiOSError = 
  NO_ERROR or something like that) }
procedure RaiseLastKambiOSError; overload;
procedure RaiseLastKambiOSError(const AdditionalDescription: string); overload;

{ if not TrueValue then RaiseLastKambiOSError }
procedure KambiOSCheck(TrueValue: boolean); overload;
procedure KambiOSCheck(TrueValue: boolean; const AdditionalDescription: string); overload;

{$endif read_interface}

{$ifdef read_implementation}

constructor EKambiOSError.Create(AErrorCode: LongWord);
begin
 Create(AErrorCode, '');
end;

constructor EKambiOSError.Create(AErrorCode: LongWord; const AdditionalDescription: string); 
var s: string;
begin 
 ErrorCode := AErrorCode;
 
 if AdditionalDescription <> '' then 
   s := AdditionalDescription + nl else
   s := ''; 
 s := s + 'System error';
 if ErrorCode<>0 then
   s := s + Format('(%d) :' +nl+ '%s', [ErrorCode, SysErrorMessage(ErrorCode)]);

 inherited Create(s);
end;

function LastKambiOSError: LongWord;
begin
 Result:= 
   {$ifdef UNIX} 
     {$ifdef USE_LIBC} Libc.ErrNo {$else} BaseUnix.ErrNo {$endif} 
   {$endif UNIX}
   {$ifdef MSWINDOWS} Windows.GetLastError {$endif} ;
end;

procedure RaiseLastKambiOSError;
begin
 raise EKambiOSError.Create(LastKambiOSError);
end;

procedure RaiseLastKambiOSError(const AdditionalDescription: string);
begin
 raise EKambiOSError.Create(LastKambiOSError, AdditionalDescription);
end;

procedure KambiOSCheck(TrueValue: boolean);
begin
 if not TrueValue then RaiseLastKambiOSError;
end;

procedure KambiOSCheck(TrueValue: boolean; const AdditionalDescription: string); 
begin
 if not TrueValue then RaiseLastKambiOSError(AdditionalDescription);
end;

{$endif read_implementation}
