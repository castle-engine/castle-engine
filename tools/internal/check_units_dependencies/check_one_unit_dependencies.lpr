// -*- compile-command: "make" -*-
{
  Copyright 2021-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

uses Classes, SysUtils,
  CastleUtils, CastleParameters, CastleStringUtils;

  { Is it OK to depend from unit in CurrentCategory on a unit in DependencyCategory. }
  function DependencyOk(CurrentCategory, DependencyCategory: string): boolean;

    { Enable depending of a unit from ValidCategory on a unit
      in ValidDependencyCategory.
      Works recursively, so any unit in a category that can depend on
      ValidCategory can also depend on ValidDependencyCategory. }
    function IndirectDependencyCheck(const ValidCategory, ValidDependencyCategory: string): boolean;
    begin
      Result :=
        ( (CurrentCategory = ValidCategory) and (DependencyCategory = ValidDependencyCategory) ) or
        ( (CurrentCategory = ValidCategory) and (DependencyOk(ValidDependencyCategory, DependencyCategory)) );
    end;

  begin
    CurrentCategory := LowerCase(CurrentCategory);
    DependencyCategory := LowerCase(DependencyCategory);

    Result :=
      (CurrentCategory = DependencyCategory) or
      IndirectDependencyCheck('base', 'physics') or
      IndirectDependencyCheck('base', 'compatibility') or
      IndirectDependencyCheck('base', 'deprecated_units') or // everything can use deprecated_units, ignore
      IndirectDependencyCheck('files', 'base') or
      IndirectDependencyCheck('audio', 'files') or
      IndirectDependencyCheck('images', 'files') or
      IndirectDependencyCheck('images', 'vampyre_imaginglib') or
      IndirectDependencyCheck('base_rendering', 'images') or
      IndirectDependencyCheck('fonts', 'base_rendering') or
      IndirectDependencyCheck('ui', 'audio') or
      IndirectDependencyCheck('ui', 'fonts') or
      IndirectDependencyCheck('services', 'ui') or
      IndirectDependencyCheck('transform', 'services') or
      IndirectDependencyCheck('castlescript', 'transform') or
      IndirectDependencyCheck('scene', 'castlescript') or
      IndirectDependencyCheck('window', 'castlescript') or
      IndirectDependencyCheck('lcl', 'ui');
  end;

var
  AllCgeUnits: TStringList;
  DependenciesToCheck: TStringList;

  procedure ExtractCategory(UnitPath: string; out Category: string);
  var
    TokenPos: Integer;
    //PartAfterCategory: string;
  begin
    while SCharIs(UnitPath, 1, '.') or
          SCharIs(UnitPath, 1, '/') do
      UnitPath := SEnding(UnitPath, 2); // cut off initial ./ from UnitPath
    TokenPos := 1;
    Category := NextToken(UnitPath, TokenPos, ['/', '\']);
    //PartAfterCategory := NextToken(UnitPath, TokenPos, ['/', '\']);
  end;

  function FindCategory(const DependencyBaseName: string;
    out Category: string): boolean;
  var
    UnitPath: string;
  begin
    for UnitPath in AllCgeUnits do
      if SameText(ChangeFileExt(ExtractFileName(UnitPath), ''), DependencyBaseName) then
      begin
        ExtractCategory(UnitPath, Category);
        Exit(true);
      end;
    Result := false;
  end;

const
  AllowedExceptions: array [0..8] of String = (
    // Because TCurve.GeometryNode uses X3D nodes. Possible to overcome (just move it somewhere else) but weird (it is natural to have this in TCurve).
    'Category castlescript uses scene. Unit ./castlescript/castlecurves.pas uses X3DNodes',

    // Because TCastleXxxNavigation access InternalViewport and some it's TCastleViewport-specific props.
    'Category transform uses scene. Unit ./transform/castlecameras.pas uses CastleViewport',

    // Because TCastleCamera displays it's own visualization at design-time. Possible to overcome, but would complicate code.
    'Category transform uses scene. Unit ./transform/castletransform.pas uses CastleScene',
    'Category transform uses scene. Unit ./transform/castletransform.pas uses X3DNodes',

    // Because TCastleJointTool uses CastleScene
    'Category transform uses scene. Unit ./transform/castleinternalphysicsvisualization.pas uses CastleScene',

    // Because TCastleInspector accesses some key engine classes for some functionality.
    'Category ui uses transform. Unit ./ui/castleinternalinspector.pas uses CastleCameras',
    'Category ui uses scene. Unit ./ui/castleinternalinspector.pas uses CastleScene',
    'Category ui uses transform. Unit ./ui/castleinternalinspector.pas uses CastleTransform',
    'Category ui uses scene. Unit ./ui/castleinternalinspector.pas uses CastleViewport'
  );

var
  CurrentUnit, DependencyToCheck, CurrentCategory, DependencyCategory, DependencyDescribe: string;
begin
  Parameters.CheckHigh(3);
  AllCgeUnits := TStringList.Create;
  AllCgeUnits.LoadFromFile(Parameters[1]);
  CurrentUnit := Parameters[2];
  DependenciesToCheck := TStringList.Create;
  DependenciesToCheck.Text := Parameters[3];

  try
    for DependencyToCheck in DependenciesToCheck do
    begin
      if FindCategory(DependencyToCheck, DependencyCategory) then
      begin
        ExtractCategory(CurrentUnit, CurrentCategory);
        DependencyDescribe := Format('Category %s uses %s. Unit %s uses %s', [
          CurrentCategory,
          DependencyCategory,
          CurrentUnit,
          DependencyToCheck
        ]);
        // Writeln('Checking: ', DependencyDescribe);
        if not DependencyOk(CurrentCategory, DependencyCategory) then
        begin
          if ArrayPosStr(DependencyDescribe, AllowedExceptions) <> -1 then
          begin
            Writeln(ErrOutput, 'WARNING: Not allowed dependency, but it is listed as exception to temporarily allow: ', DependencyDescribe);
          end else
          begin
            Writeln(ErrOutput, 'ERROR: Not allowed dependency: ', DependencyDescribe);
            ExitCode := 1;
          end;
        end;
      end;
    end;
  finally
    FreeAndNil(AllCgeUnits);
    FreeAndNil(DependenciesToCheck);
  end;
end.
