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
  FCenterPersistent: TCastleVector3Persistent;
  function GetCenterForPersistent: TVector3;
  procedure SetCenterForPersistent(const AValue: TVector3);
published
  { @link(Center) that can be visually edited in
    Castle Game Engine Editor, Lazarus and Delphi.
    Normal user code does not need to deal with this,
    instead read or write @link(Center) directly.

    @seealso Center }
  property CenterPersistent: TCastleVector3Persistent read FCenterPersistent ;
{$endif read_interface_class}

{$ifdef read_implementation_methods}
function TCastleTransform.GetCenterForPersistent: TVector3;
begin
  Result := Center;
end;

procedure TCastleTransform.SetCenterForPersistent(const AValue: TVector3);
begin
  Center := AValue;
end;
{$endif read_implementation_methods}

{$ifdef read_implementation_constructor}
  FCenterPersistent := TCastleVector3Persistent.Create(nil);
  { Note that some classes (like TCastleVector3Persistent) call SetSubComponent(true)
    on themselves, for backward compatibility. But we should not depend on it,
    some classes (like TFloatRectanglePersistent) may not call it.
    The responsibility of calling "SetSubComponent(true)" should be on the owner
    in general (this is more functional, and more standard too). }
  FCenterPersistent.SetSubComponent(true);
  FCenterPersistent.InternalGetValue := {$ifdef FPC}@{$endif}GetCenterForPersistent;
  FCenterPersistent.InternalSetValue := {$ifdef FPC}@{$endif}SetCenterForPersistent;
  FCenterPersistent.InternalDefaultValue := Center; // current value is default
{$endif read_implementation_constructor}

{$ifdef read_implementation_destructor}
  FreeAndNil(FCenterPersistent);
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
  property RotationPersistent: TCastleVector4RotationPersistent read FRotationPersistent ;
{$endif read_interface_class}

{$ifdef read_implementation_methods}
function TCastleTransform.GetRotationForPersistent: TVector4;
begin
  Result := Rotation;
end;

procedure TCastleTransform.SetRotationForPersistent(const AValue: TVector4);
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
  FScalePersistent: TCastleVector3Persistent;
  function GetScaleForPersistent: TVector3;
  procedure SetScaleForPersistent(const AValue: TVector3);
published
  { @link(Scale) that can be visually edited in
    Castle Game Engine Editor, Lazarus and Delphi.
    Normal user code does not need to deal with this,
    instead read or write @link(Scale) directly.

    @seealso Scale }
  property ScalePersistent: TCastleVector3Persistent read FScalePersistent ;
{$endif read_interface_class}

{$ifdef read_implementation_methods}
function TCastleTransform.GetScaleForPersistent: TVector3;
begin
  Result := Scale;
end;

procedure TCastleTransform.SetScaleForPersistent(const AValue: TVector3);
begin
  Scale := AValue;
end;
{$endif read_implementation_methods}

{$ifdef read_implementation_constructor}
  FScalePersistent := TCastleVector3Persistent.Create(nil);
  { Note that some classes (like TCastleVector3Persistent) call SetSubComponent(true)
    on themselves, for backward compatibility. But we should not depend on it,
    some classes (like TFloatRectanglePersistent) may not call it.
    The responsibility of calling "SetSubComponent(true)" should be on the owner
    in general (this is more functional, and more standard too). }
  FScalePersistent.SetSubComponent(true);
  FScalePersistent.InternalGetValue := {$ifdef FPC}@{$endif}GetScaleForPersistent;
  FScalePersistent.InternalSetValue := {$ifdef FPC}@{$endif}SetScaleForPersistent;
  FScalePersistent.InternalDefaultValue := Scale; // current value is default
{$endif read_implementation_constructor}

{$ifdef read_implementation_destructor}
  FreeAndNil(FScalePersistent);
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
  FScaleOrientationPersistent: TCastleVector4Persistent;
  function GetScaleOrientationForPersistent: TVector4;
  procedure SetScaleOrientationForPersistent(const AValue: TVector4);
published
  { @link(ScaleOrientation) that can be visually edited in
    Castle Game Engine Editor, Lazarus and Delphi.
    Normal user code does not need to deal with this,
    instead read or write @link(ScaleOrientation) directly.

    @seealso ScaleOrientation }
  property ScaleOrientationPersistent: TCastleVector4Persistent read FScaleOrientationPersistent ;
{$endif read_interface_class}

{$ifdef read_implementation_methods}
function TCastleTransform.GetScaleOrientationForPersistent: TVector4;
begin
  Result := ScaleOrientation;
end;

procedure TCastleTransform.SetScaleOrientationForPersistent(const AValue: TVector4);
begin
  ScaleOrientation := AValue;
end;
{$endif read_implementation_methods}

{$ifdef read_implementation_constructor}
  FScaleOrientationPersistent := TCastleVector4Persistent.Create(nil);
  { Note that some classes (like TCastleVector3Persistent) call SetSubComponent(true)
    on themselves, for backward compatibility. But we should not depend on it,
    some classes (like TFloatRectanglePersistent) may not call it.
    The responsibility of calling "SetSubComponent(true)" should be on the owner
    in general (this is more functional, and more standard too). }
  FScaleOrientationPersistent.SetSubComponent(true);
  FScaleOrientationPersistent.InternalGetValue := {$ifdef FPC}@{$endif}GetScaleOrientationForPersistent;
  FScaleOrientationPersistent.InternalSetValue := {$ifdef FPC}@{$endif}SetScaleOrientationForPersistent;
  FScaleOrientationPersistent.InternalDefaultValue := ScaleOrientation; // current value is default
{$endif read_implementation_constructor}

{$ifdef read_implementation_destructor}
  FreeAndNil(FScaleOrientationPersistent);
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
  FTranslationPersistent: TCastleVector3Persistent;
  function GetTranslationForPersistent: TVector3;
  procedure SetTranslationForPersistent(const AValue: TVector3);
published
  { @link(Translation) that can be visually edited in
    Castle Game Engine Editor, Lazarus and Delphi.
    Normal user code does not need to deal with this,
    instead read or write @link(Translation) directly.

    @seealso Translation }
  property TranslationPersistent: TCastleVector3Persistent read FTranslationPersistent ;
{$endif read_interface_class}

{$ifdef read_implementation_methods}
function TCastleTransform.GetTranslationForPersistent: TVector3;
begin
  Result := Translation;
end;

procedure TCastleTransform.SetTranslationForPersistent(const AValue: TVector3);
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
  FDirectionPersistent: TCastleVector3Persistent;
  function GetDirectionForPersistent: TVector3;
  procedure SetDirectionForPersistent(const AValue: TVector3);
published
  { @link(Direction) that can be visually edited in
    Castle Game Engine Editor, Lazarus and Delphi.
    Normal user code does not need to deal with this,
    instead read or write @link(Direction) directly.

    @seealso Direction }
  property DirectionPersistent: TCastleVector3Persistent read FDirectionPersistent stored false;
{$endif read_interface_class}

{$ifdef read_implementation_methods}
function TCastleTransform.GetDirectionForPersistent: TVector3;
begin
  Result := Direction;
end;

procedure TCastleTransform.SetDirectionForPersistent(const AValue: TVector3);
begin
  Direction := AValue;
end;
{$endif read_implementation_methods}

{$ifdef read_implementation_constructor}
  FDirectionPersistent := TCastleVector3Persistent.Create(nil);
  { Note that some classes (like TCastleVector3Persistent) call SetSubComponent(true)
    on themselves, for backward compatibility. But we should not depend on it,
    some classes (like TFloatRectanglePersistent) may not call it.
    The responsibility of calling "SetSubComponent(true)" should be on the owner
    in general (this is more functional, and more standard too). }
  FDirectionPersistent.SetSubComponent(true);
  FDirectionPersistent.InternalGetValue := {$ifdef FPC}@{$endif}GetDirectionForPersistent;
  FDirectionPersistent.InternalSetValue := {$ifdef FPC}@{$endif}SetDirectionForPersistent;
  FDirectionPersistent.InternalDefaultValue := Direction; // current value is default
{$endif read_implementation_constructor}

{$ifdef read_implementation_destructor}
  FreeAndNil(FDirectionPersistent);
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
  FUpPersistent: TCastleVector3Persistent;
  function GetUpForPersistent: TVector3;
  procedure SetUpForPersistent(const AValue: TVector3);
published
  { @link(Up) that can be visually edited in
    Castle Game Engine Editor, Lazarus and Delphi.
    Normal user code does not need to deal with this,
    instead read or write @link(Up) directly.

    @seealso Up }
  property UpPersistent: TCastleVector3Persistent read FUpPersistent stored false;
{$endif read_interface_class}

{$ifdef read_implementation_methods}
function TCastleTransform.GetUpForPersistent: TVector3;
begin
  Result := Up;
end;

procedure TCastleTransform.SetUpForPersistent(const AValue: TVector3);
begin
  Up := AValue;
end;
{$endif read_implementation_methods}

{$ifdef read_implementation_constructor}
  FUpPersistent := TCastleVector3Persistent.Create(nil);
  { Note that some classes (like TCastleVector3Persistent) call SetSubComponent(true)
    on themselves, for backward compatibility. But we should not depend on it,
    some classes (like TFloatRectanglePersistent) may not call it.
    The responsibility of calling "SetSubComponent(true)" should be on the owner
    in general (this is more functional, and more standard too). }
  FUpPersistent.SetSubComponent(true);
  FUpPersistent.InternalGetValue := {$ifdef FPC}@{$endif}GetUpForPersistent;
  FUpPersistent.InternalSetValue := {$ifdef FPC}@{$endif}SetUpForPersistent;
  FUpPersistent.InternalDefaultValue := Up; // current value is default
{$endif read_implementation_constructor}

{$ifdef read_implementation_destructor}
  FreeAndNil(FUpPersistent);
{$endif read_implementation_destructor}

