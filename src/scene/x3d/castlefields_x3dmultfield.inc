{%MainUnit x3dfields.pas}
{
  Copyright 2002-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { X3D field with a list of values. }
  TX3DMultField = class(TX3DField)
  strict protected
    { Get or set the number of items, see @link(Count).
      @groupBegin }
    function GetCount: TListSize; virtual; abstract;
    procedure SetCount(const Value: TListSize); virtual; abstract;
    { @groupEnd }
  public
    { Number of items in this field.

      Remember that increasing this generally sets new items to undefined
      values (see SetCount documentation of particular descendant for docs).
      So you usually want to initialize them afterwards to something correct. }
    property Count: TListSize read GetCount write SetCount;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{$endif read_implementation}
