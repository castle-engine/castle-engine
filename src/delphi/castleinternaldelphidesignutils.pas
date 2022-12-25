{
  Copyright 2022-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Utilities for Delphi (both VCL and FMX), only design-time.
  At registration, this sets OnGetDesignTimeProjectPath so that other code
  (design-time or not only design-time) can use it, if assigned. }
unit CastleInternalDelphiDesignUtils;

interface

procedure Register;

implementation

uses SysUtils, Classes,
  ToolsAPI, // design-time only unit
  CastleInternalDelphiUtils;

function GetProjectPath: String;
begin
  Result := ExtractFilePath(GetActiveProject.FileName);
end;

procedure Register;
begin
  OnGetDesignTimeProjectPath := GetProjectPath;
end;

end.
