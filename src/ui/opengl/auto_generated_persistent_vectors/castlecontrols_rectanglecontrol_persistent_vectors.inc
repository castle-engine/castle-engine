{
  Copyright 2018-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Auto-generated (TODO: in the future) wrapper to make
  TCastleRectangleControl vectors persistent.

  This means they are
  - Saved/loaded using our serialization system in CastleComponentSerialize,
  - saved/loaded using standard Lazarus/Delphi serialization (lfm, dfm)
    (Not yet working, a published TPersistent is not saved automatically in LFM.
    In the future, possibly we will upgrade them to TComponent,
    or we will save a JSON serialization of TCastleControl.Controls in LFM,
    or we will figure out a way to serialize a bare TPersistent in LFM.)
  - editable in Castle Game Engine Editor,
  - editable in Lazarus/Delphi Object Inspector (when used in TCastleControl).
}

{$ifdef read_interface_class}
strict private
  FColorPersistent: TCastleColorPersistent;
  function GetColorForPersistent: TCastleColor;
  procedure SetColorForPersistent(const AValue: TCastleColor);
published
  { @link(Color) that can be visually edited in
    Lazarus, Delphi and Castle Game Engine visual designer.
    Normal user code does not need to deal with this,
    instead read or write @link(Color) directly.

    @seealso Color }
  property ColorPersistent: TCastleColorPersistent read FColorPersistent;
{$endif read_interface_class}

{$ifdef read_implementation_methods}
function TCastleRectangleControl.GetColorForPersistent: TCastleColor;
begin
  Result := Color;
end;

procedure TCastleRectangleControl.SetColorForPersistent(const AValue: TCastleColor);
begin
  Color := AValue;
end;
{$endif read_implementation_methods}

{$ifdef read_implementation_constructor}
  FColorPersistent := TCastleColorPersistent.Create;
  FColorPersistent.InternalGetValue := @GetColorForPersistent;
  FColorPersistent.InternalSetValue := @SetColorForPersistent;
  FColorPersistent.InternalDefaultValue := FColor; // current value is default
{$endif read_implementation_constructor}

{$ifdef read_implementation_destructor}
  FreeAndNil(FColorPersistent);
{$endif read_implementation_destructor}
