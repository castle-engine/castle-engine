{%MainUnit x3dnodes.pas}
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

{$endif read_interface}

{$ifdef read_implementation}

resourcestring
  SExpectedInterfaceDeclaration =
    'Expected interface declaration (for VRML 2.0: eventIn, eventOut, field or ' +
    'exposedField keyword; for X3D: inputOnly, outputOnly, initializeOnly or ' +
    'inputOutput keyword) but found %s';
  SExpectedFieldType =
    'Expected field type name (like SFVec2f etc.) but found %s';
  SLoadError = 'Error %s when loading %s from URL "%s": %s';

{$endif read_implementation}
