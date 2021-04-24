{
  Copyright 2021-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

type
  { Box with configurable size, position, material and colliders. }
  TCastleBox = class(TCastleAbstractPrimitive)
  public
    constructor Create(AOwner: TComponent); override;

    { Size of the box in 3D. By default (2, 2, 2). }
    property Size: TVector3 read FSize write SetSize;

    { Get or set the box @link(Size) and @link(Translation) using the TBox3D type. }
    property Box: TBox3D read GetBox write SetBox;

    { Color of the box.

      What exactly it means, depends on @link(Material):
      @underedList(
        @item(When Material is pmPhysical, then MainColor determines TPhysicalMaterialNode.BaseColor and TPhysicalMaterialNode.Transparency.)
        @item(When Material is pmPhong, then MainColor determines TMaterialNode.BaseColor and TMaterialNode.Transparency.)
        @item(When Material is pmUnlit, then MainColor determines TUnlitMaterialNode.BaseColor and TUnlitMaterialNode.Transparency.)
      )
    }
    property MainColor: TCastleColor;
  published
    { Material type (which determined lighting calculation) for this primitive. }
    property Material: TPrimitiveMaterial read FMaterial write SetMaterial default pmPhysical;

    { Set to @true to automatically setup and synchronize proper physics rigidbody and collider for this box. }
    property PhysicsCollider: Boolean read FPhysicsCollider set SetPhysicsCollider default false;

    // TODO make
    // Size: TVector3
    // MainColor: TCastleColor
    // exposed
  end;

{ TODO: same for sphere.
  Set CollidesAsSphere automatically, to also make CGE collisions automatically good.

  TODO: problem with this approach to primitives:
  - Doesn't scale.
    Doesn't expose 100 of material/texture/shaders features available in underlying nodes.
    So we could end up extending this forever.

  It is better to expose editing X3D nodes eventually.
}
