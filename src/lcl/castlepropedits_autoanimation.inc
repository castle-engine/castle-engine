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

{ TCastleSceneCore.AutoAnimation property editor. }

{ TSceneAutoAnimationPropertyEditor ------------------------------------------ }

type
  { Property editor to select an animation on TCastleSceneCore. }
  TSceneAutoAnimationPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue: String); override;
  end;

function TSceneAutoAnimationPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList, paRevertable];
end;

procedure TSceneAutoAnimationPropertyEditor.GetValues(Proc: TGetStrProc);
var
  Scene: TCastleSceneCore;
  S: String;
begin
  Proc('');
  Scene := GetComponent(0) as TCastleSceneCore;
  for S in Scene.AnimationsList do
    Proc(S);
end;

procedure TSceneAutoAnimationPropertyEditor.SetValue(const NewValue: String);
var
  Scene: TCastleSceneCore;
begin
  inherited SetValue(NewValue);
  Scene := GetComponent(0) as TCastleSceneCore;
  Scene.AutoAnimation := NewValue;
end;
