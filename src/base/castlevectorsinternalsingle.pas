{ Define the vectors and matrices using Single float precision.

  This is an internal unit: do not use this in your code,
  you do not want to have the types like TGenericScalar or TGenericVector3
  defined.

  Instead use the types exposed by CastleVectors unit, like @link(TVector3). }
unit CastleVectorsInternalSingle;

{$I castleconf.inc}
{$ifdef FPC} {$modeswitch advancedrecords} {$endif}

interface

uses SysUtils;

type
  { Internal type, always means Single within the CastleVectorsInternalSingle unit. }
  TGenericScalar = Single;

{$I castlevectors_generic_float_record.inc}
