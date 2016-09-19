{
  Copyright 2003-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Parsing command-line options that control the quadric rendering quality. }
unit X3DNodesDetailOptions;

{$I castleconf.inc}

interface

{ Parses @--detail-xxx command-line options, and sets Detail_Xxx variables
  appropriately.

  Parses these options:

  @preformatted(
    --detail-quadric-slices VALUE
    --detail-quadric-stacks VALUE
    --detail-rect-divisions VALUE
  )

  and sets Detail_QuadricSlices, Detail_QuadricStacks and Detail_RectDivisions
  variables.

  @raises(EInvalidParams on violation of allowed Detail values
    (e.g. when Detail_QuadricSlices < 3 etc.))
  @raises(EConvertError when option Argument is not an int) }
procedure X3DNodesDetailOptionsParse;

function X3DNodesDetailOptionsHelp: string;

implementation

uses SysUtils, CastleUtils, CastleParameters, X3DNodes;

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);

  function ArgumentToCardinal(const ParamName: string; MinValue: Cardinal): Cardinal;
  var i: Integer;
  begin
   i := StrToInt(Argument);
   if Int64(i) < Int64(MinValue) then
    raise EInvalidParams.CreateFmt('%s count must be at least %d, but %d is given',
      [ParamName, MinValue, i]);
   { jezeli i >= MinValue to na pewno i jest >= 0 (bo MinValue >= 0 bo MinValue jest
     typu Cardinal) wiec konwersja i z Integer na Cardinal jest bezpieczna }
   result := i;
  end;

begin
 case OptionNum of
  0: Detail_QuadricSlices := ArgumentToCardinal('Quadric slices', MinQuadricSlices);
  1: Detail_QuadricStacks := ArgumentToCardinal('Quadric stacks', MinQuadricStacks);
  2: Detail_RectDivisions := ArgumentToCardinal('Rectangle divisions', MinRectDivisions);
  else raise EInternalError.Create('X3DNodesDetailOptions.OptionProc: OptionNum');
 end;
end;

procedure X3DNodesDetailOptionsParse;
const
  Options: array[0..2] of TOption =
  ( (Short: #0; Long: 'detail-quadric-slices'; Argument: oaRequired),
    (Short: #0; Long: 'detail-quadric-stacks'; Argument: oaRequired),
    (Short: #0; Long: 'detail-rect-divisions'; Argument: oaRequired)
  );
begin
 Parameters.Parse(Options, @OptionProc, nil, true);
end;

function X3DNodesDetailOptionsHelp: string;
begin
 Result:=
   '  --detail-quadric-slices VALUE ,' +nl+
   '  --detail-quadric-stacks VALUE' +nl+
   '                        Adjust triangulation quality of quadrics.' +NL+
   '                        Slices and stacks' +nl+
   '                        adjust the triangulation of Spheres, Cones and' +nl+
   '                        Cylinders (like slices of a pizza and stacks of' +nl+
   '                        a tower). Deprecated, consider' +nl+
   '                        using KambiTriangulation node in your scene instead.' +nl+
   '  --detail-rect-divisions VALUE' +nl+
   '                        Adjust triangulation quality of boxes. Deprecated, consider' +nl+
   '                        using KambiTriangulation node in your scene instead.';
end;

end.
