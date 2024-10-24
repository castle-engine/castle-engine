{
  Copyright 2024-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleInternalLoadSaveIfc (TODO: just CastleIfc) unit. }
unit TestCastleIfc;

interface

uses
  Classes, SysUtils,
  CastleTester;

type
  TTestCastleIfc = class(TCastleTestCase)
  published
    procedure TestIfcClasses;
  end;

implementation

uses TypInfo, RttiUtils,
  CastleStringUtils, CastleInternalLoadSaveIfc, CastleInternalRttiUtils;

{ Simple hack to detect does given object is a TObjectList<xxx> specialization.
  They don't share any common ancestor in Generics.Collections in FPC,
  so there's no obvious "is" check to do this.
  This hack seems acceptable in this case -- as this is only internal
  and has to account only for classes inside
  our own castleinternalloadsaveifc_ifc_standard_types.inc,
  so we can rely on our own naming conventions. }
function ClassNameOfList(const PotentialListClassName: String): Boolean;
begin
  Result :=
    IsSuffix('List', PotentialListClassName, false) or
    IsPrefixSuffix('TObjectList<', '>', PotentialListClassName, false);
end;

procedure TTestCastleIfc.TestIfcClasses;

  procedure TestIfcInstance(const Ifc: TIfcPersistent);
  var
    PropNames: TStringList;
    PropInfos: TPropInfoList;
    PropInfo: PPropInfo;
    PropName: String;
    PropClass: TClass;
    List: TObject;
    I: Integer;
  begin
    inherited;

    PropInfos := TPropInfoList.Create(Ifc, tkProperties);
    try
      PropNames := TStringList.Create;
      try
        PropNames.CaseSensitive := false;
        PropNames.Duplicates := dupError;

        for I := 0 to PropInfos.Count - 1 do
        begin
          PropInfo := PropInfos.Items[I];
          PropName := PropInfo^.Name;

          { Make an exception if we have repeated the same property
            name across descendants. This detects early a mistake if we would
            define the same property name in 2 classes, ancestor and descendant,
            by accident. }
          PropNames.Add(PropName);

          { Make an exception if class has a list property, but it is not created
            in constructor. }
          if PropertyType(PropInfo) = ptInstance then
          begin
            PropClass := PropertyGetInstanceClass(Ifc, PropInfo);
            if ClassNameOfList(PropClass.ClassName) then
            begin
              List := PropertyGetInstance(Ifc, PropInfo);
              if List = nil then
                raise EInvalidIfc.CreateFmt('IFC property "%s.%s" is a list, but it has not been created in constructor', [
                  ClassName,
                  PropName
                ]);
            end;
          end;
        end;
      finally FreeAndNil(PropNames) end;
    finally FreeAndNil(PropInfos) end;
  end;

var
  IfcClass: TIfcPersistentClass;
  Ifc: TIfcPersistent;
begin
  InitializeIfcClasses;
  for IfcClass in IfcClasses do
  begin
    Ifc := IfcClass.Create(nil);
    try
      TestIfcInstance(Ifc);
    finally FreeAndNil(Ifc) end;
  end;
end;

initialization
  RegisterTest(TTestCastleIfc);
end.
