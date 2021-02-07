{%MainUnit castlepropedits.pas}
{
  Copyright 2010-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TCastleImagePersistentEditor ----------------------------------------------------- }

type
  { Editor for TCastleImagePersistent. }
  TCastleImagePersistentEditor = class(TSubPropertiesEditor)
  public
    function GetName: ShortString; override;
  end;

function TCastleImagePersistentEditor.GetName: ShortString;
begin
  Result := inherited GetName;
  Result := SuffixRemove('persistent', Result, true);
end;

