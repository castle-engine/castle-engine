{%MainUnit castlescene.pas}
{
  Copyright 2025-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Helper to implement TPrimitiveMaterial.

  In separate unit, because

  @unorderedList(
    @itemSpacing Compact
    @item(We want to keep it internal.)
    @item(We need to declare it in the interface section of some classes.)
    @item(C++ Builder doesn't allow to reuse between 2 classes (in same unit)
      the type declared in the "private" section of one class.)
  )

  This unit is doomed to be removed anyway when we make new "material components"
  planned in https://castle-engine.io/roadmap#materials . }
unit CastleInternalPrimitiveMaterial;

{$I castleconf.inc}

interface

uses X3DNodes, CastleColors, CastleRenderOptions;

type
  { Information how to configure X3D texture node and the material field that
    describes the texture.

    Note: Deliberately designed to make the default value
    equal to be filled by zeroes. }
  TMaterialTexture = record
    { Node of the texture, never @nil. }
    Node: TImageTextureNode;
    { URL of the texture. Empty means no texture, unless LoadedDirectly. }
    Url: String;
    { If @true, it means that Node has contents loaded using Node.LoadFromImage,
      so it should be shown even if Url is empty. }
    LoadedDirectly: Boolean;
  end;

{ Update X3D nodes from information in TMaterialTexture.
  Utility to implement TPrimitiveMaterial, shared by
  TCastleAbstractPrimitive and TCastleImageTransform.

  Changes:
  @unorderedList(
    @itemSpacing Compact
    @item(AppearanceNode.Material)
    @item(Possibly the texture node inside main material's texture slot)
    @item(Possibly the texture node inside normalmap material's texture slot)
  ) }
procedure CommonUpdateMaterialNode(const Material: TPrimitiveMaterial;
  const Color: TCastleColor; const AppearanceNode: TAppearanceNode;
  const Base, NormalMap: TMaterialTexture);

implementation

procedure CommonUpdateMaterialNode(const Material: TPrimitiveMaterial;
  const Color: TCastleColor; const AppearanceNode: TAppearanceNode;
  const Base, NormalMap: TMaterialTexture);

  { Depending on Texture.* fields, adjust Texture.Node.FdUrl
    and make NodeField refer to Texture.Node or nil. }
  procedure AdjustTexture(const NodeField: TSFNode;
    const Texture: TMaterialTexture);
  begin
    if (Texture.Url <> '') or Texture.LoadedDirectly then
    begin
      // set NodeField.Value
      if NodeField.Value <> Texture.Node then
      begin
        NodeField.Value := Texture.Node;
        NodeField.Changed;
      end;
      // set Texture.Node.FdUrl (loads texture image)
      if (not Texture.LoadedDirectly) and
         ( (Texture.Node.FdUrl.Count <> 1) or
           (Texture.Node.FdUrl.Items[0] <> Texture.Url) ) then
        Texture.Node.SetUrl([Texture.Url]);
    end else
    begin
      // set NodeField.Value
      if NodeField.Value <> nil then
      begin
        NodeField.Value := nil;
        { This causes time-consuming ChangedAll, which is why we checked
          NodeField.Value <> nil above, to not call this from every UpdateMaterialNode,
          e.g. if you changed something else than Texture.Url. }
        NodeField.Changed;
      end;
      { Set Texture.Node.FdUrl (unloads texture image).

        This is not strictly necessary for functionality here, since we have
        disconnected Texture.Node from NodeField anyway.
        But it's nice to
        1. release memory of the image
        2. TCastleImageTransform.UpdateCoordinateNodes depends
           on using texture node's XxxTexture.Node.IsTextureImage. }
      if Texture.Node.FdUrl.Count <> 0 then
        Texture.Node.SetUrl([]);
    end;
  end;

  procedure DoPhysical;
  var
    PM: TPhysicalMaterialNode;
  begin
    if AppearanceNode.Material is TPhysicalMaterialNode then
      PM := TPhysicalMaterialNode(AppearanceNode.Material)
    else
    begin
      PM := TPhysicalMaterialNode.Create;
      AppearanceNode.Material := PM;
    end;
    PM.BaseColor := Color.XYZ;
    PM.Transparency := 1 - Color.W;
    AdjustTexture(PM.FdBaseTexture, Base);
    AdjustTexture(PM.FdNormalTexture, NormalMap);
  end;

  procedure DoPhong;
  var
    M: TMaterialNode;
  begin
    if AppearanceNode.Material is TMaterialNode then
      M := TMaterialNode(AppearanceNode.Material)
    else
    begin
      M := TMaterialNode.Create;
      AppearanceNode.Material := M;
    end;
    M.DiffuseColor := Color.XYZ;
    M.Transparency := 1 - Color.W;
    AdjustTexture(M.FdDiffuseTexture, Base);
    AdjustTexture(M.FdNormalTexture, NormalMap);
  end;

  procedure DoUnlit;
  var
    UM: TUnlitMaterialNode;
  begin
    if AppearanceNode.Material is TUnlitMaterialNode then
      UM := TUnlitMaterialNode(AppearanceNode.Material)
    else
    begin
      UM := TUnlitMaterialNode.Create;
      AppearanceNode.Material := UM;
    end;
    UM.EmissiveColor := Color.XYZ;
    UM.Transparency := 1 - Color.W;
    AdjustTexture(UM.FdEmissiveTexture, Base);
    AdjustTexture(UM.FdNormalTexture, NormalMap);
  end;

begin
  case Material of
    pmPhysical: DoPhysical;
    pmPhong   : DoPhong;
    pmUnlit   : DoUnlit;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('CommonUpdateMaterialNode:Material?');
    {$endif}
  end;
end;

end.
