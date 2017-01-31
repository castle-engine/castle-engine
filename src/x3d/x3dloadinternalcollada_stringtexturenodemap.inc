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
  TStringTextureNodeMap = class(specialize TFPGMap<string, TAbstractTextureNode>)
    { For given Key return it's data, or @nil if not found.
      In our usage, we never insert @nil node as data, so this is
      not ambiguous. }
    function Find(const Key: string): TAbstractTextureNode;
  end;

{$endif read_interface}

{$ifdef read_implementation}

function TStringTextureNodeMap.Find(const Key: string): TAbstractTextureNode;
var
  Index: Integer;
begin
  { do not use "inherited Find", it expects list is sorted }
  Index := IndexOf(Key);
  if Index <> -1 then
    Result := Data[Index] else
    Result := nil;
end;

{$endif read_implementation}
