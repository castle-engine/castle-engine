{
  Copyright 2010-2011 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Describing face as indexes range (TFaceIndex). }
unit FaceIndex;

interface

uses CastleUtils, GenericStructList;

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

  TFaceIndexesList = specialize TGenericStructList<TFaceIndex>;

const
  UnknownFaceIndex: TFaceIndex = (IndexBegin: -1; IndexEnd: -1);

implementation

end.
