{
  Copyright 2007-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Reporting warnings (OnWarning). }
unit CastleWarnings
  deprecated 'this interface to warnings is overcomplicated; better use WritelnWarning from CastleLog unit instead';

{$I castleconf.inc}

interface

type
  { Types of warnings. }
  TWarningType = (
    { Something failed (like URL not available), but still the data
      is semantically correct. For data formats that have a precise specification
      (like VRML/X3D or Collada), this means that the file satisfies the specification,
      but there is a minor problem (like one of the referred URLs is not available). }
    wtMinor,

    { The data is invalid. For data formats that have a precise specification
      (like VRML/X3D or Collada), this means that the file is incorrect
      with respect to this specification. We can handle it,
      but other (less forgiving but still correct) software may reject it.

      It's strongly suggested to report this to the author of the file,
      as the file should really be corrected. }
    wtMajor);

{ Assign this to OnWarning to report warnings using WarningWrite, and log them too.
  For Windows programs with no console available,
  WarningWrite will make a message box,
  in all other cases the warning just goes to ErrOutput.
  The warning is also logged using CastleLog. }
procedure OnWarningWrite(const AType: TWarningType; const Category, S: string);

{ Assign this to OnWarning to only log warnings using CastleLog. }
procedure OnWarningLog(const AType: TWarningType; const Category, S: string);

type
  TWarningProc = procedure (const AType: TWarningType; const Category, S: string);

var
  { Reporting warnings. Used by other units to report
    warnings about various data (images, sound files, 3D models),
    indicating that data is invalid in some way but we can continue.

    You can assign any procedure here. You can ignore, or report this warning
    in any way. If you want to be really strict about the data correctness,
    you can also raise an exception (or raise it only when type is wtMajor).

    The default behavior is to ignore possible warnings
    (as there is no safe cross-platform default place where they can be reported).
    Actually, you can initialize CastleLog to see warnings in the log.
    You can also assign your own callback to OnWarning to record and show
    warnings in any way. }
  OnWarning: TWarningProc = @OnWarningLog;

implementation

uses CastleUtils, SysUtils, CastleLog;

procedure OnWarningWrite(const AType: TWarningType; const Category, S: string);
begin
  if Log then
    WritelnLog('Warning: ' + Category, S);
  WarningWrite(ApplicationName + ': ' + Category + ' warning: ' + S);
end;

procedure OnWarningLog(const AType: TWarningType; const Category, S: string);
begin
  if Log then
    WritelnLog('Warning: ' + Category, S);
end;

end.
