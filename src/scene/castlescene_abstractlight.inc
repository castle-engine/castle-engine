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
    FGizmoImage: TCastleImageTransform;
    FGizmoTextureUrl: String;
    FGizmoBillboard: TCastleBillboard;
    FRootNode: TX3DRootNode;
    FLightNode: TAbstractLightNode;
    { For now just always constant and initialized from CastleDesignMode. }
    FGizmo: Boolean;
    FGizmoScene: TCastleScene;
    FGizmoSelect: TCastleBox;
    procedure SetLightNode(const Value: TAbstractLightNode);
    procedure SetGizmoTextureUrl(const Value: String);
  protected
    { Descendants should set X3D light node here. }
    property LightNode: TAbstractLightNode read FLightNode write SetLightNode;
    property GizmoTextureUrl: String read FGizmoTextureUrl write SetGizmoTextureUrl;
    property GizmoScene: TCastleScene read FGizmoScene;
    property Gizmo: Boolean read FGizmo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
  end;

{$endif read_interface}

{$ifdef read_implementation}

constructor TCastleAbstractLight.Create(AOwner: TComponent);

  procedure InternalTransform(const T: TCastleTransform);
  begin
    T.SetTransient;
    T.InternalExcludeFromParentBoundingVolume := true;
    T.Pickable := false;
    T.Collides := false;
    T.CastShadowVolumes := false;
    T.ExcludeFromStatistics := true;
  end;

const
  GizmoSize = 1.0;
begin
  inherited;

  { internal stuff create }
  FScene := TCastleScene.Create(nil);
  FScene.SetTransient;
  FScene.CastGlobalLights := true;
  Add(FScene);

  FGizmo := CastleDesignMode;

  { build X3D nodes }
  FRootNode := TX3DRootNode.Create;
  FScene.Load(FRootNode, true);

  if Gizmo then
  begin
    // This allows user to select light by clicking in CGE editor
    FGizmoSelect := TCastleBox.Create(nil);
    FGizmoSelect.Size := Vector3(GizmoSize, GizmoSize, GizmoSize);
    InternalTransform(FGizmoSelect);
    FGizmoSelect.Visible := false;
    // to make picking natural, restore to default these values, after InternalTransform modified them
    FGizmoSelect.Pickable := true;
    FGizmoSelect.InternalExcludeFromParentBoundingVolume := false;
    Add(FGizmoSelect);

    FGizmoImage := TCastleImageTransform.Create(nil);
    FGizmoImage.Size := Vector2(GizmoSize, GizmoSize);
    FGizmoImage.Mipmaps := true;
    InternalTransform(FGizmoImage);
    Add(FGizmoImage);

    FGizmoBillboard := TCastleBillboard.Create(nil);
    FGizmoBillboard.AxisOfRotation := TVector3.Zero;
    FGizmoImage.AddBehavior(FGizmoBillboard);

    FGizmoScene := TCastleScene.Create(nil);
    InternalTransform(FGizmoScene);
    Add(FGizmoScene);
  end;
end;

destructor TCastleAbstractLight.Destroy;
begin
  FreeAndNil(FScene);
  FreeAndNil(FGizmoScene);
  FreeAndNil(FGizmoImage);
  FreeAndNil(FGizmoBillboard);
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

procedure TCastleAbstractLight.SetGizmoTextureUrl(const Value: String);
begin
  if FGizmoTextureUrl <> Value then
  begin
    FGizmoTextureUrl := Value;
    if Gizmo then
      FGizmoImage.Url := FGizmoTextureUrl;
  end;
end;

function TCastleAbstractLight.PropertySections(const PropertyName: String): TPropertySections;
begin
  // PropertySections not necessary for now in this class.
  // if (PropertyName = 'xxx') then
  //   Result := [psBasic]
  // else
    Result := inherited PropertySections(PropertyName);
end;

{$endif read_implementation}
