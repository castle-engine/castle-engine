{ -*- buffer-read-only: t -*-

  Copyright 2018-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Automatically generated wrapper to make vectors/colors persistent.

  Do not edit this file manually!
  Instead
  - edit tools/internal/generate-persistent-vectors/persistent_vectors_input.txt
  - and run generate-persistent-vectors in that directory.
}

{$ifdef read_interface_class}
strict private
  FOriginPersistent: TCastleVector2Persistent;
  function GetOriginForPersistent: TVector2;
  procedure SetOriginForPersistent(const AValue: TVector2);
published
  { @link(Origin) that can be visually edited in
    Castle Game Engine Editor, Lazarus and Delphi.
    Normal user code does not need to deal with this,
    instead read or write @link(Origin) directly.

    @seealso Origin }
  property OriginPersistent: TCastleVector2Persistent read FOriginPersistent ;
{$endif read_interface_class}

{$ifdef read_implementation_methods}
function TCastleOrthographic.GetOriginForPersistent: TVector2;
begin
  Result := Origin;
end;

procedure TCastleOrthographic.SetOriginForPersistent(const AValue: TVector2);
begin
  Origin := AValue;
end;
{$endif read_implementation_methods}

{$ifdef read_implementation_constructor}
  FOriginPersistent := TCastleVector2Persistent.Create(nil);
  { Note that some classes (like TCastleVector3Persistent) call SetSubComponent(true)
    on themselves, for backward compatibility. But we should not depend on it,
    some classes (like TFloatRectanglePersistent) may not call it.
    The responsibility of calling "SetSubComponent(true)" should be on the owner
    in general (this is more functional, and more standard too). }
  FOriginPersistent.SetSubComponent(true);
  FOriginPersistent.InternalGetValue := {$ifdef FPC}@{$endif}GetOriginForPersistent;
  FOriginPersistent.InternalSetValue := {$ifdef FPC}@{$endif}SetOriginForPersistent;
  FOriginPersistent.InternalDefaultValue := Origin; // current value is default
{$endif read_implementation_constructor}

{$ifdef read_implementation_destructor}
  FreeAndNil(FOriginPersistent);
{$endif read_implementation_destructor}

