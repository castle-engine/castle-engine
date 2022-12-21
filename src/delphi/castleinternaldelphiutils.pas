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

{ Utilities for Delphi UI that works the same in VCL and FMX. }
unit CastleInternalDelphiUtils;

{$I castleconf.inc}

interface

uses UITypes,
  CastleKeysMouse;

function MouseButtonToCastle(const MouseButton: TMouseButton;
  out MyMouseButton: TCastleMouseButton): Boolean;

implementation

function MouseButtonToCastle(const MouseButton: TMouseButton;
  out MyMouseButton: TCastleMouseButton): Boolean;
begin
  Result := true;
  case MouseButton of
    TMouseButton.mbLeft  : MyMouseButton := buttonLeft;
    TMouseButton.mbRight : MyMouseButton := buttonRight;
    TMouseButton.mbMiddle: MyMouseButton := buttonMiddle;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else Result := false;
    {$endif}
  end;
end;

end.
