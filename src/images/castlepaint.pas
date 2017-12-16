{
  Copyright 2001-2017 Michalis Kamburelis, Yevhen Loza.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

(* Paint extension for CastleImages unit and must be used with it. *)

unit CastlePaint;

{$mode objfpc}{$H+}

interface

uses
  CastleImages;

type
  TCastleImageHelper = class helper for TCastleImage
  public
  end;

type
  TRGBAlphaImageHelper = class helper for TRGBAlphaImage
  public
  end;

type
  TRGBImageHelper = class helper for TRGBImage
  public
  end;

type
  TGrayscaleAlphaImageHelper = class helper for TGrayscaleAlphaImage
  public
  end;

type
  TGrayscaleImageHelper = class helper for TGrayscaleImage
  public
  end;

type
  TRGBFloatImageHelper = class helper for TRGBFloatImage
  public
  end;

implementation

end.

