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

  TX3DSingleField = class(TX3DField)
///  {$ifndef FPC}
///    function GetValue: TValue; virtual; abstract;
///  {$endif}
  end;
  TX3DSingleFieldClass = class of TX3DSingleField;

  TX3DSingleFieldList = {$ifdef FPC}specialize{$endif} TObjectList<TX3DSingleField>;

{$endif read_interface}

{$ifdef read_implementation}

{$endif read_implementation}
