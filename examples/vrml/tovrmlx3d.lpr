{
  Copyright 2003-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simple converter from 3DS, Collada, and every other 3D format to VRML / X3D.
  Can also process VRML/X3D -> VRML/X3D, in which case it's useful as a converter
  between X3D encodings (classic vs xml) and as a pretty-printer.

  Reads a 3D model from the filename given as command-line parameter
  ('-' means stdin), and outputs model as VRML/X3D to stdout.

  Can be used instead of view3dscene --write parameter.
  This way you don't need e.g. OpenGL libraries (that are required
  to run view3dscene) installed on your system.

  For your own uses, you can easily extend this to process VRML/X3D graph.
  For example, add or remove some nodes. See TVRMLNode methods.
}

program tovrmlx3d;

uses SysUtils, KambiUtils, KambiClassUtils, VRMLNodes, Object3DAsVRML,
  ParseParametersUnit;

var
  Encoding: TX3DEncoding = xeClassic;

const
  Options: array[0..1] of TOption =
  (
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short:  #0; Long: 'encoding'; Argument: oaRequired)
  );

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
begin
  case OptionNum of
    0: begin
         InfoWrite(
           'tovrmlx3d: converter from various 3D model formats into VRML/X3D.' +NL+
           'Give input 3D model filename on the command-line, and output model' +NL+
           'will be written to the standard output.' +NL+
           NL+
           'Available options are:' +NL+
           HelpOptionHelp +NL+
           '  --encoding classic|xml' +NL+
           '                        Choose X3D encoding to use.' +NL+
           '                        Default is "classic". Choosing XML encoding' +NL+
           '                        will convert VRML to X3D if needed, but it works' +NL+
           '                        sensibly only for VRML 2.0 now (not for VRML 1.0).');
         ProgramBreak;
       end;
    1: if SameText(Argument, 'classic') then
         Encoding := xeClassic else
       if SameText(Argument, 'xml') then
         Encoding := xeXML else
         raise EInvalidParams.CreateFmt('Invalid --encoding argument "%s"', [Argument]);
    else raise EInternalError.Create('OptionProc');
  end;
end;

var
  FileName: string;
  Node: TVRMLNode;
begin
  { parse command-line }
  ParseParameters(Options, @OptionProc, nil);
  Parameters.CheckHigh(1);
  FileName := Parameters[1];

  Node := LoadVRML(FileName, true);
  try
    SaveVRML(Node, StdOutStream, 'tovrmlx3d, http://vrmlengine.sourceforge.net/',
      ExtractFileName(FileName), Encoding);
  finally FreeAndNil(Node) end;
end.
