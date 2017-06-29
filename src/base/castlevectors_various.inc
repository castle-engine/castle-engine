{
  Copyright 2003-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ CastleVectors interface and implementation of various small helpers. }

{$ifdef read_interface}

type
  EPlanesParallel = class(Exception);
  ELinesParallel = class(Exception);
  ETransformedResultInvalid = class(EVectorInvalidOp);

var
  { Format used by functions named "ToNiceStr", like FloatToNiceStr and VectorToNiceStr. }
  FloatNiceFormat: string = 'f';

{$endif read_interface}

{$ifdef read_implementation}

{ Separated from PointsDistance2DSqr, to not slow it down by implicit
  try/finally section because we use string. }
procedure PointsDistance2DSqr_InvalidIgnoreIndex;
begin
  raise EInternalError.Create('Invalid IgnoreIndex for PointsDistance2DSqr');
end;

{$endif read_implementation}
