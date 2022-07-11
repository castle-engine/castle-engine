{%MainUnit castlepropedits.pas}
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

{ TCastle property editor. }

{ TMeshColliderScenePropertyEditor ------------------------------------------ }

type
  TMeshColliderScenePropertyEditor = class(TPersistentPropertyEditor)


  public
    procedure GetValues(Proc: TGetStrProc); override;



  end;


procedure TMeshColliderScenePropertyEditor.GetValues(Proc: TGetStrProc);
begin
  inherited GetValues(Proc);
end;

