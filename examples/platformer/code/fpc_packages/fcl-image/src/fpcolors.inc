{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Color definitions and functions.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
const
  BytesNeeded : array[TColorFormat] of byte =
      (1,1,1,1,2,3,1,2,4,2,2,3,4,6,1,2,4,8,2,2,3,4,6,1,2,4,8);

  alphaTransparent = $0000;
  alphaOpaque      = $FFFF;
  colTransparent: TFPColor = (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaTransparent);
  colBlack      : TFPColor = (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque);
  colBlue       : TFPColor = (Red: $0000; Green: $0000; Blue: $ffff; Alpha: alphaOpaque);
  colGreen      : TFPColor = (Red: $0000; Green: $ffff; Blue: $0000; Alpha: alphaOpaque);
  colCyan       : TFPColor = (Red: $0000; Green: $ffff; Blue: $ffff; Alpha: alphaOpaque);
  colRed        : TFPColor = (Red: $ffff; Green: $0000; Blue: $0000; Alpha: alphaOpaque);
  colMagenta    : TFPColor = (Red: $ffff; Green: $0000; Blue: $ffff; Alpha: alphaOpaque);
  colYellow     : TFPColor = (Red: $ffff; Green: $ffff; Blue: $0000; Alpha: alphaOpaque);
  colWhite      : TFPColor = (Red: $ffff; Green: $ffff; Blue: $ffff; Alpha: alphaOpaque);
  colGray       : TFPColor = (Red: $8000; Green: $8000; Blue: $8000; Alpha: alphaOpaque);
  colLtGray     : TFPColor = (Red: $c000; Green: $c000; Blue: $c000; Alpha: alphaOpaque);
  colDkGray     : TFPColor = (Red: $4000; Green: $4000; Blue: $4000; Alpha: alphaOpaque);
  colDkBlue     : TFPColor = (Red: $0000; Green: $0000; Blue: $8000; Alpha: alphaOpaque);
  colDkGreen    : TFPColor = (Red: $0000; Green: $8000; Blue: $0000; Alpha: alphaOpaque);
  colDkCyan     : TFPColor = (Red: $0000; Green: $8000; Blue: $8000; Alpha: alphaOpaque);
  colDkRed      : TFPColor = (Red: $8000; Green: $0000; Blue: $0000; Alpha: alphaOpaque);
  colDkMagenta  : TFPColor = (Red: $8000; Green: $0000; Blue: $8000; Alpha: alphaOpaque);
  colDkYellow   : TFPColor = (Red: $8000; Green: $8000; Blue: $0000; Alpha: alphaOpaque);
  colMaroon     : TFPColor = (Red: $8000; Green: $0000; Blue: $0000; Alpha: alphaOpaque);
  colLtGreen    : TFPColor = (Red: $0000; Green: $8000; Blue: $0000; Alpha: alphaOpaque);
  colOlive      : TFPColor = (Red: $8000; Green: $8000; Blue: $0000; Alpha: alphaOpaque);
  colNavy       : TFPColor = (Red: $0000; Green: $0000; Blue: $8000; Alpha: alphaOpaque);
  colPurple     : TFPColor = (Red: $8000; Green: $0000; Blue: $8000; Alpha: alphaOpaque);
  colTeal       : TFPColor = (Red: $0000; Green: $8000; Blue: $8000; Alpha: alphaOpaque);
  colSilver     : TFPColor = (Red: $c000; Green: $c000; Blue: $c000; Alpha: alphaOpaque);
  colLime       : TFPColor = (Red: $bfbf; Green: $ffff; Blue: $0000; Alpha: alphaOpaque);
  colFuchsia    : TFPColor = (Red: $ffff; Green: $0000; Blue: $ffff; Alpha: alphaOpaque);
  colAqua       : TFPColor = (Red: $0000; Green: $ffff; Blue: $ffff; Alpha: alphaOpaque);

