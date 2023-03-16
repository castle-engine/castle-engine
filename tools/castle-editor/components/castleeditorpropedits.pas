{
  Copyright 2010-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Property and component editors using LCL and internal to CGE editor. }
unit CastleEditorPropEdits;

{$I castleconf.inc}

interface

uses Classes, PropEdits, Forms, CastleColors, CastlePropEdits;

{$define read_interface}
{$I castleeditorpropedits_color.inc}
{$undef read_interface}

procedure Register;

implementation

uses // FPC and LCL units
  SysUtils, TypInfo,
  // Lazarus design-time (IDE) units
  ComponentEditors,
  // CGE units
  FormCastleColorPicker, CastleStringUtils;

{$define read_implementation}
{$I castleeditorpropedits_color.inc}

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TCastleColorPersistent), nil, '',
    TCastleColorPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCastleColorRGBPersistent), nil, '',
    TCastleColorRGBPropertyEditor);
end;

end.
