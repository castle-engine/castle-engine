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

  strict private
    FProc: TGetStrProc;
    procedure FilterValues(const S: String);
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure TMeshColliderScenePropertyEditor.FilterValues(const S: String);
var
  Persistent: TPersistent;
  CastleTransform: TCastleTransform;
begin
  if Assigned(PropertyHook) and Assigned(FProc) then
  begin
    Persistent := PropertyHook.GetComponent(S);
    CastleTransform := Persistent as TCastleTransform;

    if Assigned(CastleTransform) and CastleTransform.HasColliderMesh then
      FProc(S);
  end;
end;

procedure TMeshColliderScenePropertyEditor.GetValues(Proc: TGetStrProc);
begin
  FProc := Proc; // used by FilterValues
  Proc(oisNone); // add none value
  if Assigned(PropertyHook) then
    PropertyHook.GetComponentNames(GetTypeData(GetPropType), @FilterValues);
end;

