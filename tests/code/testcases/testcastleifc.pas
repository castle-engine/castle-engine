{
  Copyright 2024-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleIfc unit. }
unit TestCastleIfc;

interface

uses
  Classes, SysUtils,
  CastleTester, CastleStringUtils, X3DNodes;

type
  TTestCastleIfc = class(TCastleTestCase)
  strict private
    { Used by TestTextLiteral to gather X3D TTextNode contents. }
    FoundTexts: TCastleStringList;
    procedure GatherTextNode(Node: TX3DNode);
  published
    procedure TestIfcClasses;
    procedure TestIfcClassesNoDuplicates;
    procedure TestAxis2Placement2D;
    procedure TestAxis2Placement3D;
    procedure TestTextLiteral;
  end;

implementation

uses TypInfo, FpJson,
  CastleIfc, CastleInternalRttiUtils, CastleVectors;

{ Simple hack to detect does given object is a TObjectList<xxx> specialization
  and is a list of IFC classes.

  Reason: TObjectList<xxx> don't share any common ancestor
  in Generics.Collections in FPC,
  so there's no obvious "is" check to do this.
  This hack seems acceptable in this case -- as this is only internal
  and has to account only for classes inside
  our own castleifc_ifc_standard_types.inc,
  so we can rely on our own naming conventions. }
function ClassNameOfList(const PotentialListClassName: String): Boolean;
begin
  Result :=
    IsPrefixSuffix('TIfc', 'List', PotentialListClassName, false) or
    IsPrefixSuffix('TObjectList<TIfc', '>', PotentialListClassName, false) or
    IsPrefixSuffix('TObjectList<CastleIfc.TIfc', '>', PotentialListClassName, false);
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
            if ClassNameOfList(PropClass.ClassName) or
               PropClass.InheritsFrom(TStrings) then
            begin
              List := PropertyGetInstance(Ifc, PropInfo);
              if List = nil then
                raise EInvalidIfc.CreateFmt('IFC property "%s.%s" is a list, but it has not been created in constructor', [
                  Ifc.ClassName,
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
  for IfcClass in IfcClasses do
  begin
    Ifc := IfcClass.Create(nil);
    try
      TestIfcInstance(Ifc);
    finally FreeAndNil(Ifc) end;
  end;
end;

procedure TTestCastleIfc.TestIfcClassesNoDuplicates;
var
  IfcClass: TIfcPersistentClass;
  I, J: Integer;
begin
  for I := 0 to IfcClasses.Count - 1 do
  begin
    IfcClass := IfcClasses[I];
    for J := I + 1 to IfcClasses.Count - 1 do
      if IfcClass = IfcClasses[J] then
        raise EInvalidIfc.CreateFmt('IFC class %s is duplicated in IfcClasses', [IfcClass.ClassName]);
  end;
end;

procedure TTestCastleIfc.TestAxis2Placement2D;
var
  Axis2Placement2D: TIfcAxis2Placement2D;
  X, Y: TVector2;
begin
  Axis2Placement2D := TIfcAxis2Placement2D.Create(nil);
  try
    AssertTrue(Axis2Placement2D.RefDirection = nil);
    AssertVectorEquals(Vector2(1, 0), Axis2Placement2D.P(0));
    AssertVectorEquals(Vector2(0, 1), Axis2Placement2D.P(1));

    Axis2Placement2D.RefDirection := TIfcDirection.Create(Axis2Placement2D);
    Axis2Placement2D.RefDirection.DirectionRatios.Value := Vector3(1, 1, 0);
    X := Vector2(1, 1).Normalize;
    AssertVectorEquals(X, Axis2Placement2D.P(0), 0.01);
    //Writeln('Axis2Placement2D.P(1) = ' + Axis2Placement2D.P(1).ToString);
    Y := Vector2(-1, 1).Normalize;
    AssertVectorEquals(Y, Axis2Placement2D.P(1), 0.01);
  finally FreeAndNil(Axis2Placement2D) end;
end;

procedure TTestCastleIfc.TestAxis2Placement3D;
var
  Axis2Placement3D: TIfcAxis2Placement3D;
  X, Y, Z: TVector3;
begin
  Axis2Placement3D := TIfcAxis2Placement3D.Create(nil);
  try
    AssertTrue(Axis2Placement3D.RefDirection = nil);
    AssertTrue(Axis2Placement3D.Axis = nil);
    AssertVectorEquals(Vector3(1, 0, 0), Axis2Placement3D.P(0));
    AssertVectorEquals(Vector3(0, 1, 0), Axis2Placement3D.P(1));
    AssertVectorEquals(Vector3(0, 0, 1), Axis2Placement3D.P(2));

    Axis2Placement3D.RefDirection := TIfcDirection.Create(Axis2Placement3D);
    Axis2Placement3D.RefDirection.DirectionRatios.Value := Vector3(1, 1, 0);
    X := Vector3(1, 1, 0).Normalize;
    AssertVectorEquals(X, Axis2Placement3D.P(0), 0.01);
    Y := Vector3(-1, 1, 0).Normalize;
    AssertVectorEquals(Y, Axis2Placement3D.P(1), 0.01);
    Z := Vector3(0, 0, 1);
    AssertVectorEquals(Z, Axis2Placement3D.P(2), 0.01);
  finally FreeAndNil(Axis2Placement3D) end;
end;

procedure TTestCastleIfc.GatherTextNode(Node: TX3DNode);
var
  TextNode: TTextNode;
  S: String;
begin
  TextNode := Node as TTextNode;
  S := GlueStrings(TextNode.FdString.Items, '|');
  FoundTexts.Add(S);
end;

{ Test loading, saving and displaying IfcTextLiteral and IfcTextLiteralWithExtent.
  The testcase file is a copy of demo-models/ifc/text_literal/text_literal.ifcjson . }
procedure TTestCastleIfc.TestTextLiteral;

  { Check the IFC classes were loaded with correct properties. }
  procedure CheckIfcContents(const IfcFile: TIfcFile);
  var
    I: Integer;
    Component: TComponent;
    TextLiteral: TIfcTextLiteral;
    LiteralsCount, WithExtentCount: Cardinal;
    Literals: TCastleStringList;
  begin
    LiteralsCount := 0;
    WithExtentCount := 0;
    Literals := TCastleStringList.Create;
    try
      for I := 0 to IfcFile.ComponentCount - 1 do
      begin
        Component := IfcFile.Components[I];
        if Component is TIfcTextLiteral then
        begin
          TextLiteral := TIfcTextLiteral(Component);
          Inc(LiteralsCount);
          Literals.Add(TextLiteral.Literal);
          { All our texts have a placement, even if it is an identity placement. }
          AssertTrue(TextLiteral.Placement <> nil);

          { Check that the enumerated Path is read (and written) correctly. }
          if TextLiteral.Literal = 'B: object placement' then
            AssertTrue(TextLiteral.Path = TIfcTextPath.Left)
          else
            AssertTrue(TextLiteral.Path = TIfcTextPath.Right);

          if TextLiteral is TIfcTextLiteralWithExtent then
          begin
            Inc(WithExtentCount);
            AssertEquals('bottom-left', TIfcTextLiteralWithExtent(TextLiteral).BoxAlignment);
            AssertTrue(TIfcTextLiteralWithExtent(TextLiteral).Extent <> nil);
            AssertSameValue(6, TIfcTextLiteralWithExtent(TextLiteral).Extent.SizeInX);
            AssertSameValue(1, TIfcTextLiteralWithExtent(TextLiteral).Extent.SizeInY);
          end;
        end;
      end;

      AssertEquals(4, LiteralsCount);
      AssertEquals(1, WithExtentCount);
      AssertTrue(Literals.IndexOf('A: literal placement') <> -1);
      AssertTrue(Literals.IndexOf('B: object placement') <> -1);
      AssertTrue(Literals.IndexOf('C: with extent') <> -1);
      AssertTrue(Literals.IndexOf('D: rotated' + #10 + 'and multi-line') <> -1);
    finally FreeAndNil(Literals) end;
  end;

var
  IfcFile, IfcFileFromSaved: TIfcFile;
  Json: TJsonObject;
  RootNode: TX3DRootNode;
begin
  IfcFile := IfcJsonLoad('castle-data:/ifc/text_literal.ifcjson');
  try
    CheckIfcContents(IfcFile);

    { Test that saving preserves everything: save to JSON, load it back. }
    Json := IfcJsonSave(IfcFile);
    try
      IfcFileFromSaved := IfcJsonLoad(Json);
      try
        CheckIfcContents(IfcFileFromSaved);
      finally FreeAndNil(IfcFileFromSaved) end;
    finally FreeAndNil(Json) end;

    { Test that the conversion to X3D creates TTextNode with proper contents. }
    FoundTexts := TCastleStringList.Create;
    try
      RootNode := IfcToX3D(IfcFile, '');
      try
        RootNode.EnumerateNodes(TTextNode,
          {$ifdef FPC}@{$endif} GatherTextNode, false);
      finally FreeAndNil(RootNode) end;

      AssertEquals(4, FoundTexts.Count);
      AssertTrue(FoundTexts.IndexOf('A: literal placement') <> -1);
      AssertTrue(FoundTexts.IndexOf('B: object placement') <> -1);
      AssertTrue(FoundTexts.IndexOf('C: with extent') <> -1);
      { The multi-line literal must be split into 2 X3D strings. }
      AssertTrue(FoundTexts.IndexOf('D: rotated|and multi-line') <> -1);
    finally FreeAndNil(FoundTexts) end;
  finally FreeAndNil(IfcFile) end;
end;

initialization
  RegisterTest(TTestCastleIfc);
end.
