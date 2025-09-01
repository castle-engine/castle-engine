{%MainUnit castleinternalnodesunsupported.pas}

{ Enumeration types (and their string values) used by X3D nodes in VolumeRendering.txt }

{$ifdef read_interface}

  TWeightFunction = (
    wfConstant,
    wfAlpha1,
    wfAlpha2,
    wfTable,
    wfOneMinusAlpha1,
    wfOneMinusAlpha2
  );

  TProjectionVolumeType = (
    ptMax,
    ptMin,
    ptAverage
  );

{$endif read_interface}

{$ifdef read_implementation}

const
  WeightFunctionNames: array [TWeightFunction] of String =
  ( 'CONSTANT', 'ALPHA1', 'ALPHA2', 'TABLE', 'ONE_MINUS_ALPHA1', 'ONE_MINUS_ALPHA2' );

  ProjectionVolumeTypeNames: array [TProjectionVolumeType] of String =
  ( 'MAX', 'MIN', 'AVERAGE' );

{$endif read_implementation}
