{
  Copyright 2003-2005 Michalis Kamburelis.

  This file is part of "Kambi's 3dmodels Pascal units".

  "Kambi's 3dmodels Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's 3dmodels Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's 3dmodels Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ @abstract(Parsing command-line options related to VRML Detail_ variables.) }

unit VRMLNodesDetailOptions;

interface

{ Parses options

@preformatted(
  --detail-quadric-slices VALUE
  --detail-quadric-stacks VALUE
  --detail-rect-divisions VALUE
)

  and sets Detail_QuadricSlices, Detail_QuadricStacks and Detail_RectDivisions
  variables appropriately.

  @raises(EInvalidParams on violation of allowed Detail values
    (e.g. when Detail_QuadricSlices < 3 etc.))
  @raises(EConverError when option Argument is not an int) }
procedure VRMLNodesDetailOptionsParse;

function VRMLNodesDetailOptionsHelp: string;

implementation

uses SysUtils, KambiUtils, ParseParametersUnit, VRMLNodes;

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
  else raise EInternalError.Create('VRMLNodesDetailOptions.OptionProc: OptionNum');
 end;
end;

procedure VRMLNodesDetailOptionsParse;
const
  Options: array[0..2] of TOption =
  ( (Short: #0; Long: 'detail-quadric-slices'; Argument: oaRequired),
    (Short: #0; Long: 'detail-quadric-stacks'; Argument: oaRequired),
    (Short: #0; Long: 'detail-rect-divisions'; Argument: oaRequired)
  );
begin
 ParseParameters(Options, @OptionProc, nil, true);
end;

function VRMLNodesDetailOptionsHelp: string;
begin
 Result:=
   '  --detail-quadric-slices VALUE ,' +nl+
   '  --detail-quadric-stacks VALUE' +nl+
   '                        Adjust triangulation quality. Slices and stacks' +nl+
   '                        adjust the triangulation of Spheres, Cones and' +nl+
   '                        Cylinders (like slices of a pizza and stacks of' +nl+
   '                        a tower).' +nl+
   '  --detail-rect-divisions VALUE' +nl+
   '                        Adjust triangulation quality. Rectangle divisions' +nl+
   '                        adjust the triangulation of Cubes.';
end;

end.