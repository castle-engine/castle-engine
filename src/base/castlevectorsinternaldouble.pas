{ Define the vectors and matrices using Double float precision.

  This is an internal unit: do not use this in your code,
  you do not want to have the types like TGenericScalar or TGenericVector3
  defined.

  Instead use the types exposed by CastleVectors unit, like @link(TVector3Double). }
unit CastleVectorsInternalDouble;

{$I castleconf.inc}
{$ifdef FPC} {$modeswitch advancedrecords} {$endif}

interface

uses SysUtils;

type
  { Internal type, always means Double within the CastleVectorsInternalDouble unit. }
  TGenericScalar = Double;

{$I castlevectors_generic_float_record.inc}
