{
  Copyright 2009-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simple test of DDS reading.

  Reads DDS file $1, outputs some information about it,
  and (if not --no-save) saves a sequence of PNG files xxx-yyy.png
  (where xxx comes
  from the basename of $1, and yyy is the consecutive number from 0,
  and the files are saved in $1 directory). }
program dds_decompose;

uses SysUtils, CastleUtils, CastleImages, CastleCompositeImage, CastleWarnings,
  CastleStringUtils, CastleParameters, CastleURIUtils;

var
  SaveDecomposed: boolean = true;

const
  Options: array[0..0] of TOption =
  ( (Short:'n'; Long:'no-save'; Argument: oaNone) );

  procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
    const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
  begin
    case OptionNum of
      0: SaveDecomposed := false;
      else raise EInternalError.Create('option not impl');
    end;
  end;

var
  Composite: TCompositeImage;
  OutputName, OutputBaseName: string;
  I: Integer;
begin
  OnWarning := @OnWarningWrite;
  Parameters.Parse(Options, @OptionProc, nil);
  Parameters.CheckHigh(1);
  Composite := TCompositeImage.Create;
  try
    Composite.LoadFromFile(Parameters[1]);
    Composite.Flatten3d;
    Writeln('Composite (DDS, KTX...) loaded:', nl,
      '  Width x Height x Depth: ', Composite.Width, ' x ', Composite.Height, ' x ', Composite.Depth, nl,
      '  Type: ', CompositeTypeToString[Composite.CompositeType], nl,
      '  Mipmaps: ', Composite.Mipmaps, nl,
      '  Simple 2D images inside: ', Composite.Images.Count);

    if SaveDecomposed then
    begin
      OutputBaseName := ExtractURIPath(Parameters[1]) +
        DeleteURIExt(ExtractURIName(Parameters[1])) + '_';
      for I := 0 to Composite.Images.Count - 1 do
      begin
        OutputName := OutputBaseName + IntToStrZPad(I, 2) + '.png';
        Writeln('Writing ', OutputName);
        SaveImage(Composite.Images[I], OutputName);
      end;
    end;

  finally FreeAndNil(Composite) end;
end.
