{
  Copyright 2022-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Customized object inspector control.
  The basis is just LCL TOIPropertyGrid. }
unit DesignObjectInspector;

interface

uses Classes, ObjectInspector, LCLType;

type
  { Customized (for CGE editor) object inspector control. }
  TCastleObjectInspector = class(TOIPropertyGrid)
  protected
    procedure HandleStandardKeys(var Key: Word; Shift: TShiftState); override;
  end;

implementation

procedure TCastleObjectInspector.HandleStandardKeys(var Key: Word; Shift: TShiftState);
var
  InitialKey: Word;
begin
  InitialKey := Key;
  inherited;
  { inherited handles VK_ESCAPE and sets Key to VK_UNKNOWN.
    We restore it, to allow FormProject to process VK_ESCAPE as menu shortcut
    to focus TCastleControl. }
  if InitialKey = VK_ESCAPE then
    Key := InitialKey;
end;

end.
