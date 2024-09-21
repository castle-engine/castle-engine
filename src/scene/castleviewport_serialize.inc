{%MainUnit castleviewport.pas}
{
  Copyright 2018-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Integration of TCastleTransform / TCastleUserInterface with CastleComponentSerialize. }

{$ifdef read_interface}

type
  { Helper methods extending TCastleComponentFactory.

    Implementation note: Ideally these should be split in CastleUIControls
    and CastleTransform units. But we cannot, as these units should not
    depend on each other, and only last defined helper works.

    TODO: FPC trunk has a solution to this ($modeswitch multiscopehelpers)
    https://wiki.lazarus.freepascal.org/Helper_types .
    Since we require FPC >= 3.2.0, maybe we can us it?
    If FPC >= 3.2.0 and our supported Delphi versions support this? }
  TCastleComponentFactoryHelper = class helper for TCastleComponentFactory
    { Instantiate component.
      Using this is equivalent to using global @link(CastleUIControls.UserInterfaceLoad),
      but this is much faster if you want to instantiate the same file many times. }
    function UserInterfaceLoad(const Owner: TComponent): TCastleUserInterface;

    { Instantiate component.
      Using this is equivalent to using global @link(CastleTransform.TransformLoad),
      but this is much faster if you want to instantiate the same file many times. }
    function TransformLoad(const Owner: TComponent): TCastleTransform;
  end;

{$endif read_interface}

{$ifdef read_implementation}

function TCastleComponentFactoryHelper.UserInterfaceLoad(const Owner: TComponent): TCastleUserInterface;
begin
  Result := ComponentLoad(Owner) as TCastleUserInterface;
end;

function TCastleComponentFactoryHelper.TransformLoad(const Owner: TComponent): TCastleTransform;
begin
  Result := ComponentLoad(Owner) as TCastleTransform;
end;

{$endif read_implementation}
