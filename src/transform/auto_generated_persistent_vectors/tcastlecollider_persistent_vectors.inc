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
  FTranslationPersistent: TCastleVector3Persistent;
  function GetTranslationForPersistent: TVector3;
  procedure SetTranslationForPersistent(const AValue: TVector3);
published
  { @link(Translation) that can be visually edited in
    Castle Game Engine Editor, Lazarus and Delphi.
    Normal user code does not need to deal with this,
    instead read or write @link(Translation) directly.

    @seealso Translation }
  property TranslationPersistent: TCastleVector3Persistent read FTranslationPersistent stored StoredWithAutoSize;
{$endif read_interface_class}

{$ifdef read_implementation_methods}
function TCastleCollider.GetTranslationForPersistent: TVector3;
begin
  Result := Translation;
end;

procedure TCastleCollider.SetTranslationForPersistent(const AValue: TVector3);
begin
  Translation := AValue;
end;
{$endif read_implementation_methods}

{$ifdef read_implementation_constructor}
  FTranslationPersistent := TCastleVector3Persistent.Create(nil);
  { Note that some classes (like TCastleVector3Persistent) call SetSubComponent(true)
    on themselves, for backward compatibility. But we should not depend on it,
    some classes (like TFloatRectanglePersistent) may not call it.
    The responsibility of calling "SetSubComponent(true)" should be on the owner
    in general (this is more functional, and more standard too). }
  FTranslationPersistent.SetSubComponent(true);
  FTranslationPersistent.InternalGetValue := {$ifdef FPC}@{$endif}GetTranslationForPersistent;
  FTranslationPersistent.InternalSetValue := {$ifdef FPC}@{$endif}SetTranslationForPersistent;
  FTranslationPersistent.InternalDefaultValue := Translation; // current value is default
{$endif read_implementation_constructor}

{$ifdef read_implementation_destructor}
  FreeAndNil(FTranslationPersistent);
{$endif read_implementation_destructor}

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
  FRotationPersistent: TCastleVector4RotationPersistent;
  function GetRotationForPersistent: TVector4;
  procedure SetRotationForPersistent(const AValue: TVector4);
published
  { @link(Rotation) that can be visually edited in
    Castle Game Engine Editor, Lazarus and Delphi.
    Normal user code does not need to deal with this,
    instead read or write @link(Rotation) directly.

    @seealso Rotation }
  property RotationPersistent: TCastleVector4RotationPersistent read FRotationPersistent stored StoredWithAutoSize;
{$endif read_interface_class}

{$ifdef read_implementation_methods}
function TCastleCollider.GetRotationForPersistent: TVector4;
begin
  Result := Rotation;
end;

procedure TCastleCollider.SetRotationForPersistent(const AValue: TVector4);
begin
  Rotation := AValue;
end;
{$endif read_implementation_methods}

{$ifdef read_implementation_constructor}
  FRotationPersistent := TCastleVector4RotationPersistent.Create(nil);
  { Note that some classes (like TCastleVector3Persistent) call SetSubComponent(true)
    on themselves, for backward compatibility. But we should not depend on it,
    some classes (like TFloatRectanglePersistent) may not call it.
    The responsibility of calling "SetSubComponent(true)" should be on the owner
    in general (this is more functional, and more standard too). }
  FRotationPersistent.SetSubComponent(true);
  FRotationPersistent.InternalGetValue := {$ifdef FPC}@{$endif}GetRotationForPersistent;
  FRotationPersistent.InternalSetValue := {$ifdef FPC}@{$endif}SetRotationForPersistent;
  FRotationPersistent.InternalDefaultValue := Rotation; // current value is default
{$endif read_implementation_constructor}

{$ifdef read_implementation_destructor}
  FreeAndNil(FRotationPersistent);
{$endif read_implementation_destructor}

