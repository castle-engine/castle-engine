{
  Copyright 2002-2006 Michalis Kamburelis.

  This file is part of "Kambi's 3dmodels Pascal units".

  "Kambi's 3dmodels Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's 3dmodels Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's 3dmodels Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ }
unit VRMLErrors;

interface

uses SysUtils;

type
  EVRMLError = class(Exception);

{ gdy podczas robienia czegos na VRMLu wystapi blad ale taki po ktorym mozemy
  kontynuowac dzialanie (np. nie mozna odczytac tekstury ze wskazanego pliku)
  to zostanie wywolane VRMLNonFatalError z odpowiednim stringiem.
  Mozesz zrobic z tym co chcesz - zignorowac, wyrzucic exception,
  wypisac przez WarningWrite - slowem, cokolwiek. Jak widac, domyslnie
  jest exception klasy EVRMLError.
}
procedure VRMLNonFatalError_WarningWrite(const s: string);
procedure VRMLNonFatalError_RaiseEVRMLError(const s: string);
procedure VRMLNonFatalError_Ignore(const s: string);
type
  TVRMLNonFatalErrorProc = procedure(const s: string);
var
  VRMLNonFatalError: TVRMLNonFatalErrorProc =
    {$ifdef FPC_OBJFPC} @ {$endif} VRMLNonFatalError_RaiseEVRMLError;

implementation

uses KambiUtils, KambiFilesUtils;

procedure VRMLNonFatalError_WarningWrite(const s: string);
begin WarningWrite(ProgramName+ ': WARNING: '+ s) end;

procedure VRMLNonFatalError_RaiseEVRMLError(const s: string);
begin raise EVRMLError.Create(s); end;

procedure VRMLNonFatalError_Ignore(const s: string);
begin end;

end.