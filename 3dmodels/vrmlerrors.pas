{
  Copyright 2002-2007 Michalis Kamburelis.

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

{ Handling non-fatal VRML files errors.
  See VRMLNonFatalError description for more. }
unit VRMLErrors;

interface

uses SysUtils, DataErrors;

type
  EVRMLError = class(Exception);

procedure VRMLNonFatalError_WarningWrite(const s: string);
procedure VRMLNonFatalError_RaiseEVRMLError(const s: string);
procedure VRMLNonFatalError_Ignore(const s: string);
type
  TVRMLNonFatalErrorProc = TDataNonFatalErrorProc;
var
  { Handling non-fatal VRML files errors.

    When our VRML engine encounters a problem with VRML file, it tries
    to report it, but also to continue work if possible. This way many incorrect
    VRML files are reported but we still handle them.
    In fact, VRML requires that some resource problems (missing URL resource,
    like a texture or inline or external prototype that can't be accessed)
    are handled gracefully.

    All such cases with problematic VRML content are reported to VRMLNonFatalError.
    You can assign any procedure to VRMLNonFatalError.

    @orderedList(
      @item(You can report the problem and continue.
        This is suggested for general VRML browsers like view3dscene, that must
        handle any VRML content gracefully.
        E.g. use VRMLNonFatalError_WarningWrite.)

      @item(You can raise an exception, that will possibly stop your program.
        This may be appropriate for cases when you have some control over
        VRML content, and you do not want to tolerate problems with VRML data.
        E.g. it may be appropriate for games with VRML data.
        See VRMLNonFatalError_RaiseEVRMLError for this.)

      @item(You can even simply ignore the problem by
        VRMLNonFatalError_Ignore (this procedure simply does nothing).)
    )
  }
  VRMLNonFatalError: TVRMLNonFatalErrorProc =
    {$ifdef FPC_OBJFPC} @ {$endif} VRMLNonFatalError_RaiseEVRMLError;

implementation

uses KambiUtils, KambiFilesUtils;

procedure VRMLNonFatalError_WarningWrite(const s: string);
begin
  WarningWrite(ProgramName+ ': VRML Warning: '+ s)
end;

procedure VRMLNonFatalError_RaiseEVRMLError(const s: string);
begin
  raise EVRMLError.Create(s);
end;

procedure VRMLNonFatalError_Ignore(const s: string);
begin
end;

end.