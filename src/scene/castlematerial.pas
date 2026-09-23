{
  Copyright 2026-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Customize materials.

  Note that the base materials are configured using
  @url(https://castle-engine.io/x3d X3D nodes) like @link(TMaterialNode),
  @link(TPhysicalMaterialNode), @link(TUnlitMaterialNode) defined in the
  @link(X3DNodes) unit.

  This unit defines higher-level components (descending from @code(TComponent),
  comfortable to use from Pascal and in our engine @link(https://castle-engine.io/editor
  editor)) to define materials. Internally, these higher-level components
  affect the X3D nodes properties. So you can forget about X3D complexity,
  and just use components like @link(TCastleScene) with the components in this
  unit to easily customize the look of your objects. }
unit CastleMaterial;

interface

uses Classes,
  X3DNodes;

type
  TAppearanceNodeList = {$ifdef FPC}specialize{$endif} TObjectList<TAppearanceNode>;

  { Base class to customize how objects are displayed, throughout
    the engine components like @link(TCastleScene) and @link(TCastleAbstractPrimitive).

    Descendants of this class enable particular types of material behavior,
    like physical-based rendering, unlit materials, water effects, and mirrors.
    Using this class also allows to synchronize the look across multiple
    components, e.g. multiple TCastleScene instances can use the same
    material instance. }
  TCastleAbstractMaterial = class(TComponent)
  strict private
    type
      TInternalApplyNodesUtility = class
        Parent: TCastleAbstractMaterial;
        Appearance: TAppearanceNode;
        procedure Process(ParentNode: TX3DNode; var Node: TX3DNode);
      end;

    var
      FAppearances: TAppearanceNodeList;

    function GetAppearance(const Index: Integer): TAppearanceNode;
  protected
    { Create TAppearanceNode representing this material in X3D.
      This component is responsible for keeping references to all
      appearances created this way.

      @unorderedList(
        @item(
          It will update these nodes, immediately as our properties change,
          e.g. if @link(TCastlePhysicalMaterial.BaseColor) changes than all corresponding
          appearances will be updated accordingly.

        @item(
          It will watch for them being freed by @link(TX3DNode.AddDestructionNotification).
        )
      )

      This maintenance is necessary as different TCastleScene instances
      must use different TAppearanceNode instance. But at the same time, we want
      updating this material to be reflected in all users.
    }
    function CreateAppearance: TAppearanceNode; virtual; abstract;

    { List of all currently maintained TAppearanceNode nodes,
      which equals to all nodes created by @link(CreateAppearance)
      and not yet freed.
      To be used by descendants to update the properties of all appearances
      when necessary.
      @groupBegin }
    property Appearance [const Index: Integer]: TAppearanceNode read GetAppearance;
    function AppearanceCount: SizeInt;
    { @groupEnd }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Replace all suitable appearance nodes in this graph with the an appearance
      representing this particular @link(TCastleAbstractMaterial) descendant.

      TODO: Right now, each run just does the job from scratch.
      If this Root have already have been using customized appearance,
      then it will be freed (since it's ref count will drop to zero)
      and a new one will be created.
      Maybe this will be made more efficient in the future, if needed.

      TODO: We don't support selectively updating appearance nodes.
      It would be easy to do once, but harden to maintain across updates:
      to be able to always restore appearances, we should keep a mapping to be
      able to always revert this operation.
    }
    procedure InternalApplyNodes(const Root: TX3DNode);
  end;

  // TODO: list of effects at texture and materials, including ready-made like hsv,
  // and ability to just add source code for fragment/vertex.

  { Base class to customize material textures, like
    @link(TCastlePhysicalMaterial.BaseTexture),
    @link(TCastlePhysicalMaterial.EmissiveTexture) and more. }
  TCastleAbstractTexture = class(TComponent)
    // TODO
  end;

  { 2D texture loaded from a file.

    The look can be customized using a shader effect if you define
    @link(ShaderEffectVertex), @link(ShaderEffectFragment).
    You want to at least define PLUG_texture_color in the @link(ShaderEffectFragment)
    for texture customization. See https://castle-engine.io/shaders
    and https://castle-engine.io/compositing_shaders_doc/html/section.effects_on_texture.html
    about procedural textures. }
  TCastleImage2DTexture = class(TComponent)
    // TODO
  end;

  { Material exposing modern physically-based properties.
    Matches exactly glTF standard material, and X3D @link(TPhysicaMaterialNode),
    in capabilities. }
  TCastlePhysicalMaterial = class(TCastleAbstractMaterial)
  strict private
    FBaseColor: TCastleColor;
  published
    { Base color of the material togerher with alpha (expressing opacity). }
    property BaseColor: TCastleColor read FBaseColor write FBaseColor;
  end;

implementation

{ TCastleAbstractMaterial ---------------------------------------------------- }

constructor TCastleAbstractMaterial.Create(AOwner: TComponent);
begin
  inherited;
  FAppearances := TAppearanceNodeList.Create;
end;

destructor TCastleAbstractMaterial.Destroy;
begin
  FreeAndNil(FAppearances);
  inherited;
end;

function TCastleAbstractMaterial.GetAppearance(const Index: Integer): TAppearanceNode;
begin
  Result := FAppearances[Index];
end;

function TCastleAbstractMaterial.AppearanceCount: SizeInt;
begin
  Result := FAppearances.Count;
end;


procedure TCastleAbstractMaterial.InternalApplyNodesProcess(
  ParentNode: TX3DNode; var Node: TX3DNode);
begin
  if (Node is TAppearanceNode) and
     { When Appearance is nil (not yet created), this will always be true
       since Node is never nil, so it's OK -> it will pass. }
     (Node <> Appearance) then
  begin
    if Apperance = nil then
    begin
      Apperance := Parent.CreateAppearances;
      FAppearances.Add(Appearance);
    end;
    Node := Appearance;
  end;
end;

procedure TCastleAbstractMaterial.InternalApplyNodes(const Root: TX3DNode);
var
  Utility: TCastleAbstractMaterial.TInternalApplyNodesUtility;
begin
  Utility := TCastleAbstractMaterial.TInternalApplyNodesUtility.Create;
  try
    Utility.Parent := Self;
    Root.EnumerateReplaceChildren({$ifdef FPC}@{$endif} Utility.Process);
  finally FreeAndNil(Utility) end;
end;

end.
