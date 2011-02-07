{
  Copyright 2010-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Describing face as indexes range (TFaceIndex). }
unit FaceIndex;

interface

uses KambiUtils;

{$define read_interface}

type
  { Describe a range of indexes where the face (polygon and such) is located.

    When a triangle is part of a face defined by the coordIndex field
    (like in IndexedFaceSet) then this describes where
    in this coordIndex this face is located. This is useful for
    editing / removing a face corresponding to a given triangle.

    Otherwise, both IndexBegin and IndexEnd are -1. }
  TFaceIndex = object
    IndexBegin, IndexEnd: Integer;
  end;

  TDynArrayItem_1 = TFaceIndex;
  PDynArrayItem_1 = ^TFaceIndex;
  {$define DYNARRAY_1_IS_STRUCT}
  {$I dynarray_1.inc}
  TFaceIndexesList = TDynArray_1;

const
  UnknownFaceIndex: TFaceIndex = (IndexBegin: -1; IndexEnd: -1);

{$undef read_interface}

implementation

{$define read_implementation}
{$I dynarray_1.inc}

end.
