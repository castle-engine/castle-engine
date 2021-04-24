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

{ Primitives that can be easily added and adjusted to TCastleViewport. }

{$ifdef read_interface}

type
  TPrimitiveMaterial = (pmPhysical, pmPhong, pmUnlit);

  { Base class to express primitives that can be easily added and
    adjusted to TCastleViewport.

    Such primitives can be anything that can be expressed using X3D nodes
    (like boxes or spheres or lights or sounds).
    Particular descendants of this class, like TCastleBox or TCastleSphere,
    define a particular primitive.

    Using this class is somewhat similar to using TCastleScene,
    and loading (using @link(TCastleScene.Load)) an X3D nodes graph
    with appropriate primitives. In fact, that is exactly what happens internally.
    But it is often simpler, in particular 1. you can customize such primitives
    using the CGE editor, 2. the X3D nodes are hidden from you. }
  TCastleAbstractPrimitive = class(TCastleTransform)
  end;

{$endif read_interface}

{$ifdef read_implementation}

{$endif read_implementation}
