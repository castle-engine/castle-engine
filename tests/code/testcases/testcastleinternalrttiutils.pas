// -*- compile-command: "./test_single_testcase.sh TTestCastleInternalRttiUtils" -*-
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

{ Test CastleInternalRttiUtils unit. }
unit TestCastleInternalRttiUtils;

interface

uses CastleTester;

type
  TTestCastleInternalRttiUtils = class(TCastleTestCase)
  published
    procedure TestPropertyHasDefaultValue;
    procedure TestPropertyGetValue;
    procedure TestPropertyTypeAndGetSet;
  end;

implementation

uses SysUtils, Math, Classes, TypInfo,
  CastleVectors, CastleTransform, CastleInternalRttiUtils;

procedure TTestCastleInternalRttiUtils.TestPropertyHasDefaultValue;
var
  Cam: TCastleCamera;
begin
  Cam := TCastleCamera.Create(nil);
  try
    Cam.ProjectionNear := 10;
    AssertTrue(PropertyHasDefaultValue(Cam, GetPropInfo(Cam, 'ProjectionType')));
    AssertFalse(PropertyHasDefaultValue(Cam, GetPropInfo(Cam, 'ProjectionNear')));
    AssertTrue(PropertyHasDefaultValue(Cam, GetPropInfo(Cam, 'ProjectionFar')));

    AssertFalse(Cam.TranslationPersistent.ValueIsStreamed);
    Cam.Translation := Vector3(10, 0, 0);
    AssertTrue(Cam.TranslationPersistent.ValueIsStreamed);

    { vector components have no defaults (not even 0 is considered default),
      they are decided using IsStoredProp. }
    AssertFalse(PropertyHasDefaultValue(Cam.TranslationPersistent, GetPropInfo(Cam.TranslationPersistent, 'X')));
    AssertFalse(PropertyHasDefaultValue(Cam.TranslationPersistent, GetPropInfo(Cam.TranslationPersistent, 'Y')));
    AssertFalse(PropertyHasDefaultValue(Cam.TranslationPersistent, GetPropInfo(Cam.TranslationPersistent, 'Z')));

    AssertTrue(IsStoredProp(Cam.TranslationPersistent, GetPropInfo(Cam.TranslationPersistent, 'X')));
    AssertFalse(IsStoredProp(Cam.TranslationPersistent, GetPropInfo(Cam.TranslationPersistent, 'Y')));
    AssertFalse(IsStoredProp(Cam.TranslationPersistent, GetPropInfo(Cam.TranslationPersistent, 'Z')));
  finally FreeAndNil(Cam) end;
end;

procedure TTestCastleInternalRttiUtils.TestPropertyGetValue;
var
  Cam: TCastleCamera;
  PropName, PropValue: String;
begin
  Cam := TCastleCamera.Create(nil);
  try
    Cam.ProjectionNear := 10;

    PropertyGet(Cam, GetPropInfo(Cam, 'ProjectionType'), PropName, PropValue);
    AssertEquals('ProjectionType', PropName);
    AssertEquals('ptPerspective', PropValue);

    PropertyGet(Cam, GetPropInfo(Cam, 'ProjectionNear'), PropName, PropValue);
    AssertEquals('ProjectionNear', PropName);
    AssertEquals('10', PropValue);

    PropertyGet(Cam, GetPropInfo(Cam, 'ProjectionFar'), PropName, PropValue);
    AssertEquals('ProjectionFar', PropName);
    AssertEquals('0', PropValue);

    Cam.Translation := Vector3(10, 0, 0);

    PropertyGet(Cam.TranslationPersistent, GetPropInfo(Cam.TranslationPersistent, 'X'), PropName, PropValue);
    AssertEquals('X', PropName);
    AssertEquals('10', PropValue);

    PropertyGet(Cam.TranslationPersistent, GetPropInfo(Cam.TranslationPersistent, 'Y'), PropName, PropValue);
    AssertEquals('Y', PropName);
    AssertEquals('0', PropValue);

    PropertyGet(Cam.TranslationPersistent, GetPropInfo(Cam.TranslationPersistent, 'Z'), PropName, PropValue);
    AssertEquals('Z', PropName);
    AssertEquals('0', PropValue);
  finally FreeAndNil(Cam) end;
end;

type
  TMyChildObject = class
  end;

  TMyEnum = (meOne, meTwo, meThree);

  TMyObject = class
  private
    FMyInt: Integer;
    FMyInt64: Int64;
    FMyString: String;
    FMyFloat: Single;
    FMyBool: Boolean;
    FMyChildObject: TMyChildObject;
    FMyEnum: TMyEnum;
  published
    property MyInt: Integer read FMyInt write FMyInt;
    property MyInt64: Int64 read FMyInt64 write FMyInt64;
    property MyString: String read FMyString write FMyString;
    property MyFloat: Single read FMyFloat write FMyFloat;
    property MyBool: Boolean read FMyBool write FMyBool;
    property MyChildObject: TMyChildObject read FMyChildObject write FMyChildObject;
    property MyEnum: TMyEnum read FMyEnum write FMyEnum;
  end;

procedure TTestCastleInternalRttiUtils.TestPropertyTypeAndGetSet;
var
  MyObject: TMyObject;
  Child1, Child2: TMyChildObject;
begin
  // Initialize to nil, for easy "finally" clause later
  Child1 := nil;
  Child2 := nil;
  MyObject := nil;
  try
    Child1 := TMyChildObject.Create;
    Child2 := TMyChildObject.Create;
    MyObject := TMyObject.Create;

    MyObject.MyInt := 10;
    MyObject.MyInt64 := Int64(High(UInt32)) + 123; // assign some value that really needs 64-bit
    MyObject.MyString := 'Hello';
    MyObject.MyFloat := 3.14;
    MyObject.MyBool := true;
    MyObject.MyChildObject := Child1;
    MyObject.MyEnum := meTwo;

    AssertTrue(PropertyType(GetPropInfo(MyObject, 'MyInt')) = ptInteger);
    AssertEquals(10, PropertyGetInteger(MyObject, GetPropInfo(MyObject, 'MyInt')));

    PropertySetInteger(MyObject, GetPropInfo(MyObject, 'MyInt'), 20);
    AssertEquals(20, MyObject.MyInt);

    AssertTrue(PropertyType(GetPropInfo(MyObject, 'MyInt64')) = ptInteger);
    AssertEquals(Int64(High(UInt32)) + 123, PropertyGetInteger(MyObject, GetPropInfo(MyObject, 'MyInt64')));

    PropertySetInteger(MyObject, GetPropInfo(MyObject, 'MyInt64'), Int64(High(UInt32)) + 124);
    AssertEquals(Int64(High(UInt32)) + 124, MyObject.MyInt64);

    AssertTrue(PropertyType(GetPropInfo(MyObject, 'MyString')) = ptString);
    AssertEquals('Hello', PropertyGetString(MyObject, GetPropInfo(MyObject, 'MyString')));

    PropertySetString(MyObject, GetPropInfo(MyObject, 'MyString'), 'World');
    AssertEquals('World', MyObject.MyString);

    AssertTrue(PropertyType(GetPropInfo(MyObject, 'MyFloat')) = ptFloat);
    AssertSameValue(3.14, PropertyGetFloat(MyObject, GetPropInfo(MyObject, 'MyFloat')), 0.0001);

    PropertySetFloat(MyObject, GetPropInfo(MyObject, 'MyFloat'), 2.71);
    AssertSameValue(2.71, MyObject.MyFloat, 0.0001);

    AssertTrue(PropertyType(GetPropInfo(MyObject, 'MyBool')) = ptBoolean);
    AssertEquals(true, PropertyGetBoolean(MyObject, GetPropInfo(MyObject, 'MyBool')));

    PropertySetBoolean(MyObject, GetPropInfo(MyObject, 'MyBool'), false);
    AssertEquals(false, MyObject.MyBool);

    AssertTrue(PropertyType(GetPropInfo(MyObject, 'MyChildObject')) = ptInstance);
    AssertTrue(PropertyGetInstance(MyObject, GetPropInfo(MyObject, 'MyChildObject')) = Child1);

    PropertySetInstance(MyObject, GetPropInfo(MyObject, 'MyChildObject'), Child2);
    AssertTrue(MyObject.MyChildObject = Child2);

    PropertySetInstance(MyObject, GetPropInfo(MyObject, 'MyChildObject'), nil);
    AssertTrue(MyObject.MyChildObject = nil);

    AssertTrue(PropertyType(GetPropInfo(MyObject, 'MyEnum')) = ptOther);
  finally
    FreeAndNil(MyObject);
    FreeAndNil(Child1);
    FreeAndNil(Child2);
  end;
end;

initialization
  RegisterTest(TTestCastleInternalRttiUtils);
end.
