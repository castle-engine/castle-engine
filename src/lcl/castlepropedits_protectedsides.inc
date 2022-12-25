{%MainUnit castlepropedits.pas}
{
  Copyright 2018-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ ProtectedSides editor. }

{ TCastleProtectedSidesEditor ------------------------------------------------- }

type
  TCastleProtectedSidesEditor = class(TSubPropertiesEditor)
  public
    //function GetName: ShortString; override;
    function GetValue: String; override;
  end;

{ Works, but not prominently visible.

function TCastleProtectedSidesEditor.GetName: ShortString;
begin
  Result := (inherited GetName) + ' (9-slice scaling)';
end;
}

// by default this only shows "(TBorder)"
function TCastleProtectedSidesEditor.GetValue: String;
begin
  Result := '(TBorder; 9-slice scaling)';
end;
