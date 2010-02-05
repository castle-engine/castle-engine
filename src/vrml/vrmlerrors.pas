{
  Copyright 2002-2009 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{ Handling VRML files errors and warnings. }
unit VRMLErrors;

interface

uses SysUtils;

type
  EVRMLError = class(Exception);

  { Types of VRML warnings. }
  TVRMLWarningType = (
    { These are warnings that should not usually cause anything more
      than reporting them to user.

      Things that make VRML file somewhat faulty,
      but still correct with respect to specification fall here:
      for example, if we're unable to load texture/inline from specified URL. }
    vwIgnorable,

    { These are warnings that indicate that VRML file is invalid.
      Our engine can always continue with them, but the author of VRML
      file really should fix them. }
    vwSerious);

procedure VRMLWarning_Write(
  const WarningType: TVRMLWarningType; const s: string);
procedure VRMLWarning_RaiseErrorIfSerious(
  const WarningType: TVRMLWarningType; const s: string);
procedure VRMLWarning_RaiseError(
  const WarningType: TVRMLWarningType; const s: string);
procedure VRMLWarning_Ignore(
  const WarningType: TVRMLWarningType; const s: string);

type
  TVRMLWarningProc =
    procedure (const WarningType: TVRMLWarningType; const s: string);

var
  { Handling VRML warnings.

    When our VRML engine encounters a problem with VRML file, it tries
    to report it, but also to continue work if possible. This way many incorrect
    VRML files are reported but we still handle them.
    In fact, VRML requires that some resource problems (missing URL resource,
    like a texture or inline or external prototype that can't be accessed)
    are handled gracefully.

    All such cases with problematic VRML content are reported to VRMLWarning.
    You can assign any procedure to VRMLWarning.

    @orderedList(
      @item(You can report the problem and continue.
        This is suggested for general VRML browsers like view3dscene, that must
        handle any VRML content gracefully.

        E.g. use VRMLWarning_Write.)

      @item(You can raise an exception, that will possibly stop your program.
        This may be appropriate for cases when you have some control over
        VRML content, and you do not want to tolerate problems with VRML data.
        E.g. it may be appropriate for games with VRML data.

        See VRMLWarning_RaiseError for this.)

      @item(You can even simply ignore the problem by
        VRMLWarning_Ignore (this procedure simply does nothing).)
    )
  }
  VRMLWarning: TVRMLWarningProc =
    {$ifdef FPC_OBJFPC} @ {$endif} VRMLWarning_RaiseErrorIfSerious;

  WarningTypeToStr: array [TVRMLWarningType] of string = ('ignorable', 'serious');

implementation

uses KambiUtils, KambiFilesUtils, KambiLog;

procedure VRMLWarning_Write(
  const WarningType: TVRMLWarningType; const s: string);
begin
  WarningWrite(ProgramName+ ': VRML Warning: '+ S);
end;

procedure VRMLWarning_RaiseErrorIfSerious(
  const WarningType: TVRMLWarningType; const s: string);
begin
  if WarningType = vwSerious then
    raise EVRMLError.Create(s) else
  { non-serious warnings are printed to log }
  if Log then
    WritelnLog('VRML warning', S);
end;

procedure VRMLWarning_RaiseError(
  const WarningType: TVRMLWarningType; const s: string);
begin
  raise EVRMLError.Create(s);
end;

procedure VRMLWarning_Ignore(
  const WarningType: TVRMLWarningType; const s: string);
begin
end;

end.
