{
  Copyright 2009-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Read KTX or DDS and decompose multiple images into a number of simple images.
  See README.md for details. }
program image_decompose;

uses SysUtils, CastleUtils, CastleImages, CastleInternalCompositeImage, CastleLog,
  CastleStringUtils, CastleParameters, CastleUriUtils, CastleApplicationProperties;

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
  ApplicationProperties.OnWarning.Add(
    {$ifdef FPC}@{$endif} ApplicationProperties.WriteWarningOnConsole);
  Parameters.Parse(Options, @OptionProc, nil);
  Parameters.CheckHigh(1);
  Composite := TCompositeImage.Create;
  try
    Composite.LoadFromFile(Parameters[1]);
    Composite.Flatten3d;
    Writeln('Composite image loaded:', nl,
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
