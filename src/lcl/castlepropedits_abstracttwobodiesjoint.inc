{%MainUnit castlepropedits.pas}
{
  Copyright 2022-2023 Michalis Kamburelis, Andrzej Kilijanski

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TCastle property editor. }

{ TConnectedPropertyEditor ------------------------------------------ }

type

  { Removes behavior parent transform from list of transfroms that can be connected }
  TConnectedPropertyEditor = class(TPersistentPropertyEditor)
  strict private
    FProc: TGetStrProc; // used by FilterValues

    procedure FilterValues(const S: String);
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure TConnectedPropertyEditor.FilterValues(const S: String);
var
  Persistent: TPersistent;
  CastleTransform: TCastleTransform;
  ParentCastleTransform: TCastleTransform;
begin
  if Assigned(PropertyHook) and Assigned(FProc) then
  begin
    Persistent := PropertyHook.GetComponent(S);
    CastleTransform := Persistent as TCastleTransform;

    if GetInstProp^.Instance is TCastleBehavior then
    begin
      ParentCastleTransform := (GetInstProp^.Instance as TCastleBehavior).Parent;

      if CastleTransform <> ParentCastleTransform then
        FProc(S);
    end;
  end;
end;

procedure TConnectedPropertyEditor.GetValues(Proc: TGetStrProc);
begin
  FProc := Proc; // used by FilterValues
  Proc(oisNone); // add none value
  if Assigned(PropertyHook) then
    PropertyHook.GetComponentNames(GetTypeData(GetPropType), @FilterValues);
end;


