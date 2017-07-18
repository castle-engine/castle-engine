{ Define the vectors and matrices using Double float precision.

  This is an internal unit: do not use this in your code,
  you do not want to have the types like TGenericScalar or TGenericVector3
  defined.

  Instead use the types exposed by CastleVectors unit, like @link(TVector3Double). }
unit CastleVectorsInternalDouble;

{$ifdef FPC} {$modeswitch advancedrecords} {$endif}

{$ifdef VER3_0_2} // causes internal error in FPC 3.0.2 (not in 3.0.0 or 3.1.1)
  {$define BUGGY_ZERO_CONSTANT}
{$endif}

interface

uses SysUtils;

type
  { Internal type, always means Double within the CastleVectorsInternalDouble unit. }
  TGenericScalar = Double;

{$I castlevectors_generic_float_record.inc}
