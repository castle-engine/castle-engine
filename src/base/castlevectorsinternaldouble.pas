{ Define the vectors and matrices using Double float precision.

  This is an internal unit: do not use this in your code,
  you do not want to have the types like TGenericScalar or TGenericVector3
  defined.

  Instead use the types exposed by CastleVectors unit, like @link(TVector3d). }
unit CastleVectorsInternalDouble;

{$ifdef FPC} {$modeswitch advancedrecords} {$endif}

interface

uses SysUtils;

type
  { Internal type, always means Double within the NewVectorsInternalDouble unit. }
  TGenericScalar = Double;

{$define read_interface}
{$I castlevectors_generic_float_record.inc}
{$undef read_interface}

implementation

{$define read_implementation}
{$I castlevectors_generic_float_record.inc}

end.
