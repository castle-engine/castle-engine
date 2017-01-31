{
  Copyright 2008-2017 Michalis Kamburelis.

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
  { Collada primitive (item handled by ReadPrimitiveCommon,
    from <poly*> or <tri*> or <lines*> elements within <mesh>),
    which are X3D geometries with a material name. }
  TColladaPrimitive = class
    { Collada material name (from "material" attribute of primitive element).
      For Collada 1.3.x, this is just a name of material in material library.
      For Collada 1.4.x, when instantiating geometry you specify which material
      name (inside geometry) corresponds to which material name on Materials
      list. }
    Material: string;
    X3DGeometry: TAbstractGeometryNode;
    destructor Destroy; override;
  end;

  TColladaPrimitiveList = specialize TFPGObjectList<TColladaPrimitive>;

{$endif read_interface}

{$ifdef read_implementation}

destructor TColladaPrimitive.Destroy;
begin
  FreeIfUnusedAndNil(X3DGeometry);
  inherited;
end;

{$endif read_implementation}
