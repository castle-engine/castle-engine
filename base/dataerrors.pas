{
  Copyright 2007 Michalis Kamburelis.

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

{ Reporting warnings about reading data.

  This unit should be used by other units that read some data,
  when data may contain non-fatal errors (i.e. data is invalid,
  but still our programs can manage to read them).
  For example images, sound files.

  Typically for multimedia data, you want to report the errors,
  but continue reading. The default behavior is to ignore possible warnings
  (as there is no safe default place where errors should be reported). }
unit DataErrors;

interface

{ Write error message using WarningWrite (for Windows programs with no console
  available, this will make a message box, in all other cases the error
  just goes to ErrOutput). }
procedure DataWarning_Write(const S: string);

{ Ignore error (do nothing). }
procedure DataWarning_Ignore(const S: string);

type
  TDataWarningProc = procedure (const S: string);

var
  { Used to report non-fatal errors. Should always be non-nil
    (assign DataWarning_Ignore if you want to ignore warnings). }
  DataWarning: TDataWarningProc = @DataWarning_Ignore;

implementation

uses KambiUtils, KambiFilesUtils;

procedure DataWarning_Write(const S: string);
begin
  WarningWrite(ProgramName+ ': Warning: '+ s);
end;

procedure DataWarning_Ignore(const S: string);
begin
end;

end.
