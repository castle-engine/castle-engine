{%MainUnit castlescene.pas}
{
  Copyright 2021-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Lights that can be easily added and adjusted inside TCastleViewport. }

{$ifdef read_interface}

type
  { Base class to express lights that can be easily added and
    adjusted inside TCastleViewport. }
  TCastleAbstractLight = class(TCastleTransform)
  strict private
    FScene: TCastleScene;
    FRootNode: TX3DRootNode;
    FLightNode: TAbstractLightNode;
    FGizmo: TInternalCastleEditorGizmo;
    procedure SetLightNode(const Value: TAbstractLightNode);
  protected
    { Descendants should set X3D light node here. }
    property LightNode: TAbstractLightNode read FLightNode write SetLightNode;
    property Gizmo: TInternalCastleEditorGizmo read FGizmo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
  end;

{$endif read_interface}

{$ifdef read_implementation}

constructor TCastleAbstractLight.Create(AOwner: TComponent);
begin
  inherited;

  { internal stuff create }
  FScene := TCastleScene.Create(nil);
  FScene.SetTransient;
  FScene.CastGlobalLights := true;
  Add(FScene);

  { build X3D nodes }
  FRootNode := TX3DRootNode.Create;
  FScene.Load(FRootNode, true);

  if CastleDesignMode then
  begin
    FGizmo := TInternalCastleEditorGizmo.Create(Self);
    { The features shown by light gizmo are not affected by scale,
      and we don't want light icon to get huge/small because of scaling. }
    FGizmo.InternalIgnoreParentScale := true;
    Add(FGizmo);
  end;
end;

destructor TCastleAbstractLight.Destroy;
begin
  FreeAndNil(FScene);
  inherited;
end;

procedure TCastleAbstractLight.SetLightNode(const Value: TAbstractLightNode);
begin
  if FLightNode <> Value then
  begin
    if FLightNode <> nil then
      FRootNode.RemoveChildren(FLightNode);
    FLightNode := Value;
    if FLightNode <> nil then
      FRootNode.AddChildren(FLightNode);
  end;
end;

function TCastleAbstractLight.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'CastShadows', 'RenderLayer'
     ]) then
    // Hide some properties inherited from TCastleTransform that actually do nothing for light sources
    Result := []
  else
    Result := inherited PropertySections(PropertyName);
end;

{$endif read_implementation}
