// -*- compile-command: "./test_single_testcase.sh TTestCastleComponentSerialize" -*-
{
  Copyright 2017-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleComponentSerialize unit. }
unit TestCastleComponentSerialize;

interface

uses
  Classes, SysUtils, CastleTester;

type
  TTestCastleComponentSerialize = class(TCastleTestCase)
    procedure TestDefaultValues;
    procedure TestEmptyCaption;
    procedure TestSaveLoad1;
    procedure TestSaveLoad2;
    procedure TestDeserializeObjectReferences;
    procedure TestDepth;
    procedure TestVectorDeserializedOnce;
    procedure TestCustomSerialization;
    procedure TestInternalAssignUsingSerialization;
    procedure TestViewportCameraReferencesReading;
    procedure TestToleratingInvalidReferences;
    procedure TestPastePreserveReferences;
    procedure TestPastePreserveReferencesOutsideCopied;
    procedure TestAssociateReferences;
    procedure TestAssociateReferencesReuseDesign;
    procedure TestSaveTransformNoDefaultVectors;
    procedure TestSaveUiNoDefaultVectors;
  end;

implementation

uses CastleFilesUtils, CastleComponentSerialize, CastleVectors,
  CastleUIControls, CastleControls, CastleUtils, CastleSceneManager,
  CastleScene, CastleClassUtils, CastleColors, CastleStringUtils, CastleTransform,
  CastleFonts,
  { needed to deserialize castle-data:/designs/test_object_references.castle-user-interface }
  Castle2DSceneManager;

{ TMyComponent -------------------------------------------------------------- }

type
  TMyComponent = class(TComponent)
  private
    FPosition, FScale: TVector3;
    FPositionPersistent: TCastleVector3Persistent;
    FScalePersistent: TCastleVector3Persistent;
    function GetPosition: TVector3;
    procedure SetPosition(const AValue: TVector3);
    function GetScale: TVector3;
    procedure SetScale(const AValue: TVector3);
  public
    SetScalePersistentCalls, SetPositionPersistentCalls: Cardinal;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Position: TVector3 read FPosition write FPosition;
    property Scale: TVector3 read FScale write FScale;
  published
    { @link(Position) that can be visually edited in
      Lazarus, Delphi and Castle Game Engine visual designer.
      Normal user code does not need to deal with this,
      instead read or write @link(Position) directly.

      @seealso Position }
    property PositionPersistent: TCastleVector3Persistent read FPositionPersistent;

    { @link(Scale) that can be visually edited in
      Lazarus, Delphi and Castle Game Engine visual designer.
      Normal user code does not need to deal with this,
      instead read or write @link(Scale) directly.

      @seealso Scale }
    property ScalePersistent: TCastleVector3Persistent read FScalePersistent;
  end;

constructor TMyComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScale := Vector3(1, 1, 1);

  // FPositionComponent := TCastleVector3Component.Create(nil);
  // FPositionComponent.Name := 'Position';
  // FPositionComponent.SetSubComponent(true);
  // FPositionComponent.InternalGetValue := @GetPosition;
  // FPositionComponent.InternalSetValue := @SetPosition;
  // FPositionComponent.InternalDefaultValue := FPosition; // current value is default

  // FScaleComponent := TCastleVector3Component.Create(nil);
  // FScaleComponent.Name := 'Scale';
  // FScaleComponent.SetSubComponent(true);
  // FScaleComponent.InternalGetValue := @GetScale;
  // FScaleComponent.InternalSetValue := @SetScale;
  // FScaleComponent.InternalDefaultValue := FScale; // current value is default

  FPositionPersistent := TCastleVector3Persistent.Create;
  FPositionPersistent.SetSubComponent(true);
  FPositionPersistent.InternalGetValue := {$ifdef FPC}@{$endif}GetPosition;
  FPositionPersistent.InternalSetValue := {$ifdef FPC}@{$endif}SetPosition;
  FPositionPersistent.InternalDefaultValue := FPosition; // current value is default

  FScalePersistent := TCastleVector3Persistent.Create;
  FScalePersistent.SetSubComponent(true);
  FScalePersistent.InternalGetValue := {$ifdef FPC}@{$endif}GetScale;
  FScalePersistent.InternalSetValue := {$ifdef FPC}@{$endif}SetScale;
  FScalePersistent.InternalDefaultValue := FScale; // current value is default
end;

destructor TMyComponent.Destroy;
begin
  FreeAndNil(FPositionPersistent);
  FreeAndNil(FScalePersistent);
  inherited;
end;

function TMyComponent.GetPosition: TVector3;
begin
  Result := Position;
end;

procedure TMyComponent.SetPosition(const AValue: TVector3);
begin
  Inc(SetPositionPersistentCalls);
  Position := AValue;
end;

function TMyComponent.GetScale: TVector3;
begin
  Result := Scale;
end;

procedure TMyComponent.SetScale(const AValue: TVector3);
begin
  Inc(SetScalePersistentCalls);
  Scale := AValue;
end;

{ TTestCastleComponentSerialize ---------------------------------------------- }

procedure TTestCastleComponentSerialize.TestDefaultValues;
var
  TempFileName: String;
  TestOutputOwner: TComponent;
  TestInput, TestOutput: TMyComponent;
begin
  TempFileName := GetTempFileNameCheck;

  TestInput := TMyComponent.Create(nil);
  try
    AssertVectorEquals(Vector3(0, 0, 0), TestInput.PositionPersistent.Value);
    AssertVectorEquals(Vector3(0, 0, 0), TestInput.Position);
    AssertVectorEquals(Vector3(1, 1, 1), TestInput.ScalePersistent.Value);
    AssertVectorEquals(Vector3(1, 1, 1), TestInput.Scale);

    TestInput.PositionPersistent.Value := Vector3(0, 20, 30);
    TestInput.ScalePersistent.Value := Vector3(0, 1, 2);

    AssertVectorEquals(Vector3(0, 20, 30), TestInput.PositionPersistent.Value);
    AssertVectorEquals(Vector3(0, 20, 30), TestInput.Position);
    AssertVectorEquals(Vector3(0, 1, 2), TestInput.ScalePersistent.Value);
    AssertVectorEquals(Vector3(0, 1, 2), TestInput.Scale);

    ComponentSave(TestInput, TempFileName);
  finally
    FreeAndNil(TestInput);
  end;

  TestOutputOwner := TComponent.Create(nil);
  try
    TestOutput := ComponentLoad(TempFileName, TestOutputOwner) as TMyComponent;
    AssertVectorEquals(Vector3(0, 20, 30), TestOutput.PositionPersistent.Value);
    AssertVectorEquals(Vector3(0, 20, 30), TestOutput.Position);
    AssertVectorEquals(Vector3(0, 1, 2), TestOutput.ScalePersistent.Value);
    AssertVectorEquals(Vector3(0, 1, 2), TestOutput.Scale);
  finally
    FreeAndNil(TestOutputOwner);
  end;

  CheckDeleteFile(TempFileName, true);
end;

procedure TTestCastleComponentSerialize.TestEmptyCaption;
var
  UiOwner: TComponent;
  //C: TCastleUserInterface;
begin
  UiOwner := TComponent.Create(nil);
  try
    UserInterfaceLoad('castle-data:/designs/test_caption_empty.castle-user-interface', UiOwner);
    AssertEquals('Button1', (UiOwner.FindRequiredComponent('Button1') as TCastleButton).Caption);
    AssertEquals('', (UiOwner.FindRequiredComponent('Button2') as TCastleButton).Caption);
    AssertEquals('Button3', (UiOwner.FindRequiredComponent('Button3') as TCastleButton).Caption);
  finally FreeAndNil(UiOwner) end;
end;

procedure TTestCastleComponentSerialize.TestSaveLoad1;
var
  UiOwner: TComponent;
  Ui: TCastleButton;
  Lab: TCastleLabel;
  LoadedUi: TCastleUserInterface;
  TempFileName: String;
begin
  UiOwner := TComponent.Create(nil);
  try
    Ui := TCastleButton.Create(UiOwner);

    Lab := TCastleLabel.Create(UiOwner);
    Lab.Color := Vector4(0, 1, 0, 1);
    Lab.FrameColor := Vector4(0, 1, 0, 1);
    Lab.Caption := 'test caption' + NL + 'another caption';
    Lab.LineSpacing := 4;
    Ui.InsertFront(Lab);

    TempFileName := GetTempFileNameCheck;
    UserInterfaceSave(Ui, TempFileName);
  finally FreeAndNil(UiOwner) end;

  Ui := nil; // make sure state is cleared
  Lab := nil;

  UiOwner := TComponent.Create(nil);
  try
    LoadedUi := UserInterfaceLoad(TempFileName, UiOwner);
    AssertTrue(LoadedUi is TCastleButton);
    Ui := LoadedUi as TCastleButton;

    AssertEquals(Ui.ControlsCount, 1);
    AssertTrue(Ui.Controls[0] is TCastleLabel);
    Lab := Ui.Controls[0] as TCastleLabel;

    AssertVectorEquals(Vector4(0, 1, 0, 1), Lab.Color);
    AssertVectorEquals(Vector4(0, 1, 0, 1), Lab.FrameColor);
    AssertEquals(4, Lab.LineSpacing);
    AssertEquals('test caption' + NL + 'another caption', Lab.Caption);
  finally FreeAndNil(UiOwner) end;
end;

procedure TTestCastleComponentSerialize.TestSaveLoad2;
var
  UiOwner: TComponent;
  Ui: TCastleButton;
  Lab: TCastleLabel;
  LoadedUi: TCastleUserInterface;
  TempFileName: String;
begin
  UiOwner := TComponent.Create(nil);
  try
    Ui := TCastleButton.Create(UiOwner);

    Lab := TCastleLabel.Create(UiOwner);
    // leave defaults
    Ui.InsertFront(Lab);

    TempFileName := GetTempFileNameCheck;
    UserInterfaceSave(Ui, TempFileName);
  finally FreeAndNil(UiOwner) end;

  Ui := nil; // make sure state is cleared
  Lab := nil;

  UiOwner := TComponent.Create(nil);
  try
    LoadedUi := UserInterfaceLoad(TempFileName, UiOwner);
    AssertTrue(LoadedUi is TCastleButton);
    Ui := LoadedUi as TCastleButton;

    AssertEquals(Ui.ControlsCount, 1);
    AssertTrue(Ui.Controls[0] is TCastleLabel);
    Lab := Ui.Controls[0] as TCastleLabel;

    AssertVectorEquals(Vector4(0, 0, 0, 1), Lab.Color);
    AssertVectorEquals(Vector4(1, 1, 1, 1), Lab.FrameColor);
    AssertEquals(TCastleLabel.DefaultLineSpacing, Lab.LineSpacing);
    AssertEquals('', Lab.Caption);
  finally FreeAndNil(UiOwner) end;
end;

procedure TTestCastleComponentSerialize.TestDeserializeObjectReferences;
var
  UiOwner: TComponent;
  //Ui: TCastleUserInterface;
  Sm1, Sm2, Sm3: TCastleSceneManager;
  {Scene1, Scene2, }Scene3, Scene4{, Scene5, Scene6}: TCastleScene;
begin
  UiOwner := TComponent.Create(nil);
  try
    {Ui := }UserInterfaceLoad('castle-data:/designs/test_object_references.castle-user-interface', UiOwner);
    Sm1 := UiOwner.FindRequiredComponent('SceneManager1') as TCastleSceneManager;
    Sm2 := UiOwner.FindRequiredComponent('SceneManager2') as TCastleSceneManager;
    Sm3 := UiOwner.FindRequiredComponent('SceneManager3') as TCastleSceneManager;
    // Scene1 := UiOwner.FindRequiredComponent('Scene1') as TCastleScene;
    // Scene2 := UiOwner.FindRequiredComponent('Scene2') as TCastleScene;
    Scene3 := UiOwner.FindRequiredComponent('Scene3') as TCastleScene;
    Scene4 := UiOwner.FindRequiredComponent('Scene4') as TCastleScene;
    // Scene5 := UiOwner.FindRequiredComponent('Scene5') as TCastleScene;
    // Scene6 := UiOwner.FindRequiredComponent('Scene6') as TCastleScene;
    AssertTrue(Sm1.MainScene = Scene3);
    AssertTrue(Sm2.MainScene = Scene4);
    AssertTrue(Sm3.MainScene = nil);
  finally FreeAndNil(UiOwner) end;
end;

procedure TTestCastleComponentSerialize.TestDepth;
{ Internally, CastleComponentSerialize must increase SerializationProcessPool
  for larger depths. Make sure it works OK. }
var
  I: Integer;
  UiOwner, C: TComponent;
  RootLabel, ParentLabel: TCastleLabel;
  ChildLabel: TCastleLabel;
  TempFileName: String;
begin
  UiOwner := TComponent.Create(nil);
  try
    RootLabel := TCastleLabel.Create(UiOwner);
    RootLabel.Tag := 1000;

    C := TComponent.Create(UiOwner);
    C.Tag := 2000;
    RootLabel.AddNonVisualComponent(C);

    ParentLabel := RootLabel;
    for I := 1 to 100 do
    begin
      ChildLabel := TCastleLabel.Create(UiOwner);
      ChildLabel.Tag := 1000 + I;
      ParentLabel.InsertFront(ChildLabel);

      C := TComponent.Create(UiOwner);
      C.Tag := 2000 + I;
      ChildLabel.AddNonVisualComponent(C);

      ParentLabel := ChildLabel;
    end;

    TempFileName := GetTempFileNameCheck;
    ComponentSave(RootLabel, TempFileName);
    TestLog(
      'TTestCastleComponentSerialize.TestDepth: Saved to ' + TempFileName);

  finally FreeAndNil(UiOwner) end;

  { Now load, and see if we have the same structure }
  UiOwner := TComponent.Create(nil);
  try
    RootLabel := ComponentLoad(TempFileName, UiOwner) as TCastleLabel;

    AssertEquals(1000, RootLabel.Tag);
    AssertEquals(1, RootLabel.NonVisualComponentsCount);
    C := RootLabel.NonVisualComponents[0];
    AssertEquals(2000, C.Tag);

    ParentLabel := RootLabel;
    for I := 1 to 100 do
    begin
      AssertEquals(1, ParentLabel.ControlsCount);
      ChildLabel := ParentLabel.Controls[0] as TCastleLabel;
      AssertEquals(1000 + I, ChildLabel.Tag);

      AssertEquals(1, ChildLabel.NonVisualComponentsCount);
      C := ChildLabel.NonVisualComponents[0];
      AssertEquals(2000 + I, C.Tag);

      ParentLabel := ChildLabel;
    end;
  finally FreeAndNil(UiOwner) end;
end;

procedure TTestCastleComponentSerialize.TestVectorDeserializedOnce;
var
  TestOutputOwner: TComponent;
  TestInput, TestOutput: TMyComponent;
  TempFileName: String;
begin
  TestInput := TMyComponent.Create(nil);
  try
    TestInput.Position := Vector3(1, 2, 3);
    TestInput.Scale := Vector3(4, 5, 6);
    AssertEquals(0, TestInput.SetScalePersistentCalls);
    AssertEquals(0, TestInput.SetPositionPersistentCalls);

    TempFileName := GetTempFileNameCheck;
    ComponentSave(TestInput, TempFileName);
  finally
    FreeAndNil(TestInput);
  end;

  TestOutputOwner := TComponent.Create(nil);
  try
    TestOutput := ComponentLoad(TempFileName, TestOutputOwner) as TMyComponent;
    AssertVectorEquals(Vector3(1, 2, 3), TestOutput.Position);
    AssertVectorEquals(Vector3(4, 5, 6), TestOutput.Scale);
    AssertEquals(1, TestOutput.SetScalePersistentCalls);
    AssertEquals(1, TestOutput.SetPositionPersistentCalls);
  finally
    FreeAndNil(TestOutputOwner);
  end;
end;

type
  TTestComponent2 = class;

  TTestComponent1 = class(TCastleComponent)
  strict private
    FSomeSingle: Single;
  private
    InternalSingle: Single;
    InternalString: String;
    InternalBoolean: Boolean;
    InternalInteger: Integer;
    InternalSubComponent: TTestComponent2;
    InternalColor: TCastleColor;
    InternalColorRGB: TCastleColorRGB;
    InternalVec2: TVector2;
    InternalVec3: TVector3;
    InternalVec4: TVector4;
    InternalVecNotPresent: TVector4;
    InternalVecOnly2ComponentsPresent: TVector4;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CustomSerialization(const SerializationProcess: TSerializationProcess); override;
  published
    property SomeSingle: Single read FSomeSingle write FSomeSingle {$ifdef FPC}default 0{$endif};
  end;

  TTestComponent2 = class(TCastleComponent)
  strict private
    FSomeSingle: Single;
  private
    InternalSingle: Single;
    InternalString: String;
    InternalBoolean: Boolean;
    InternalInteger: Integer;
    InternalColor: TCastleColor;
    InternalColorRGB: TCastleColorRGB;
    InternalVec2: TVector2;
    InternalVec3: TVector3;
    InternalVec4: TVector4;
    InternalVecNotPresent: TVector4;
    InternalVecOnly2ComponentsPresent: TVector4;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CustomSerialization(const SerializationProcess: TSerializationProcess); override;
  published
    property SomeSingle: Single read FSomeSingle write FSomeSingle {$ifdef FPC}default 0{$endif};
  end;

constructor TTestComponent1.Create(AOwner: TComponent);
begin
  inherited;
  InternalSubComponent := TTestComponent2.Create(Self);
  InternalSubComponent.Name := 'SomeSubComponentName';
  InternalSubComponent.SetSubComponent(true); // this only decides if SomeSubComponentName is recorded

  InternalVecNotPresent := Vector4(901, 902, 903, 904);
  InternalVecOnly2ComponentsPresent := Vector4(801, 802, 803, 804);
end;

procedure TTestComponent1.CustomSerialization(const SerializationProcess: TSerializationProcess);
begin
  inherited;
  SerializationProcess.ReadWriteInteger('InternalInteger', InternalInteger, true);
  SerializationProcess.ReadWriteBoolean('InternalBoolean', InternalBoolean, true);
  SerializationProcess.ReadWriteString('InternalString', InternalString, true);
  SerializationProcess.ReadWriteSingle('InternalSingle', InternalSingle, true);

  SerializationProcess.ReadWriteSubComponent('InternalSubComponent', InternalSubComponent, true);

  SerializationProcess.ReadWriteColor('InternalColor', InternalColor, White, true);
  SerializationProcess.ReadWriteColor('InternalColorRGB', InternalColorRGB, WhiteRGB, true);
  SerializationProcess.ReadWriteVector('InternalVec2', InternalVec2, Vector2(-1, -1), true);
  SerializationProcess.ReadWriteVector('InternalVec3', InternalVec3, Vector3(-1, -1, -1), true);
  SerializationProcess.ReadWriteVector('InternalVec4', InternalVec4, Vector4(-1, -1, -1, -1), true);
  SerializationProcess.ReadWriteVector('InternalVecNotPresent', InternalVecNotPresent, Vector4(-1, -1, -1, -1), true);
  SerializationProcess.ReadWriteVector('InternalVecOnly2ComponentsPresent', InternalVecOnly2ComponentsPresent, Vector4(-1, -1, -1, -1), true);
end;

constructor TTestComponent2.Create(AOwner: TComponent);
begin
  inherited;
  InternalVecNotPresent := Vector4(11901, 11902, 11903, 11904);
  InternalVecOnly2ComponentsPresent := Vector4(11801, 11802, 11803, 11804);
end;

procedure TTestComponent2.CustomSerialization(const SerializationProcess: TSerializationProcess);
begin
  inherited;
  SerializationProcess.ReadWriteInteger('InternalInteger', InternalInteger, true);
  SerializationProcess.ReadWriteBoolean('InternalBoolean', InternalBoolean, true);
  SerializationProcess.ReadWriteString('InternalString', InternalString, true);
  SerializationProcess.ReadWriteSingle('InternalSingle', InternalSingle, true);

  SerializationProcess.ReadWriteColor('InternalColor', InternalColor, White, true);
  SerializationProcess.ReadWriteColor('InternalColorRGB', InternalColorRGB, WhiteRGB, true);
  SerializationProcess.ReadWriteVector('InternalVec2', InternalVec2, Vector2(-1, -1), true);
  SerializationProcess.ReadWriteVector('InternalVec3', InternalVec3, Vector3(-1, -1, -1), true);
  SerializationProcess.ReadWriteVector('InternalVec4', InternalVec4, Vector4(-1, -1, -1, -1), true);
  SerializationProcess.ReadWriteVector('InternalVecNotPresent', InternalVecNotPresent, Vector4(-1, -1, -1, -1), true);
  SerializationProcess.ReadWriteVector('InternalVecOnly2ComponentsPresent', InternalVecOnly2ComponentsPresent, Vector4(-1, -1, -1, -1), true);
end;

procedure TTestCastleComponentSerialize.TestCustomSerialization;
const
  ValidOutputNumbersCount = 51;
  ValidOutputNumbers: array [0..ValidOutputNumbersCount - 1] of Float = (
    3333,
    123.456,
    4444,
    789.123,

    4004,
    4003,
    4002,
    4001,

    4007,
    4006,
    4005,

    40010,
    40011,

    40020,
    40021,
    40022,

    40033,
    40030,
    40031,
    40032,

    11904,
    11901,
    11902,
    11903,

    11804,
    11801,
    4060031,
    4060032,

    4,
    3,
    2,

    7,
    6,
    5,

    10,
    11,

    20,
    21,
    22,

    33,
    30,
    31,
    32,

    904,
    901,
    902,
    903,

    804,
    801,
    60031,
    60032
  );
var
  COwner: TComponent;
  T1: TTestComponent1;
  ValidOutput: String;
  I: Integer;
  NumbersReadBack: array [0..ValidOutputNumbersCount - 1] of Float;
  NumbersReadBackPtrs: array [0..ValidOutputNumbersCount - 1] of Pointer;
begin
  COwner := TComponent.Create(nil);
  try
    T1 := ComponentLoad('castle-data:/designs/test_custom_serialization.castle-component', COwner) as TTestComponent1;

    { Test reading with CustomSerialization }

    AssertEquals('TestComponent1', T1.Name);
    AssertSameValue(3333, T1.SomeSingle);
    AssertSameValue(123.456, T1.InternalSingle, 0.0001);
    AssertEquals('something something', T1.InternalString);
    AssertTrue(T1.InternalBoolean);
    AssertEquals(123, T1.InternalInteger);
    AssertVectorEquals(Vector4(1, 2, 3, 4), T1.InternalColor);
    AssertVectorEquals(Vector3(5, 6, 7), T1.InternalColorRGB);
    AssertVectorEquals(Vector2(10, 11), T1.InternalVec2);
    AssertVectorEquals(Vector3(20, 21, 22), T1.InternalVec3);
    AssertVectorEquals(Vector4(30, 31, 32, 33), T1.InternalVec4);
    AssertVectorEquals(Vector4(901, 902, 903, 904), T1.InternalVecNotPresent);
    AssertVectorEquals(Vector4(801, 60031, 60032, 804), T1.InternalVecOnly2ComponentsPresent);

    AssertEquals('TestComponent2', T1.InternalSubComponent.Name);
    AssertSameValue(4444, T1.InternalSubComponent.SomeSingle);
    AssertSameValue(789.123, T1.InternalSubComponent.InternalSingle, 0.0001);
    AssertEquals('something else something', T1.InternalSubComponent.InternalString);
    AssertTrue(T1.InternalSubComponent.InternalBoolean);
    AssertEquals(789, T1.InternalSubComponent.InternalInteger);
    AssertVectorEquals(Vector4(4001, 4002, 4003, 4004), T1.InternalSubComponent.InternalColor);
    AssertVectorEquals(Vector3(4005, 4006, 4007), T1.InternalSubComponent.InternalColorRGB);
    AssertVectorEquals(Vector2(40010, 40011), T1.InternalSubComponent.InternalVec2);
    AssertVectorEquals(Vector3(40020, 40021, 40022), T1.InternalSubComponent.InternalVec3);
    AssertVectorEquals(Vector4(40030, 40031, 40032, 40033), T1.InternalSubComponent.InternalVec4);
    AssertVectorEquals(Vector4(11901, 11902, 11903, 11904), T1.InternalSubComponent.InternalVecNotPresent);
    AssertVectorEquals(Vector4(11801, 4060031, 4060032, 11804), T1.InternalSubComponent.InternalVecOnly2ComponentsPresent);

    { Test writing with CustomSerialization.
      Save the result to string, see whether it matches what we expect.
      Note that we don't enforce how floats are encoded in JSON,
      this may be different between compilers and platforms due to floating-point accuracy,
      and the way default conversion float->string is done. }

    for I := 0 to ValidOutputNumbersCount - 1 do
      NumbersReadBackPtrs[I] := @(NumbersReadBack[I]);

    ValidOutput := FileToString('castle-data:/designs/test_custom_serialization_valid_output.castle-component');
    DeFormat(ComponentToString(T1), ValidOutput, NumbersReadBackPtrs, false);

    for I := 0 to ValidOutputNumbersCount - 1 do
      AssertEquals(ValidOutputNumbers[I], NumbersReadBack[I]);
  finally FreeAndNil(COwner) end;
end;

procedure TTestCastleComponentSerialize.TestInternalAssignUsingSerialization;
var
  C1, C2: TCastleCamera;
  Pos, Dir, Up: TVector3;
begin
  C1 := nil;
  C2 := nil;
  try
    C1 := TCastleCamera.Create(nil);
    C2 := TCastleCamera.Create(nil);
    C1.SetView(
      Vector3(1, 2, 3),
      Vector3(0, 0, 1),
      Vector3(-1, 0, 0)
    );
    InternalAssignUsingSerialization(C2, C1);
    C2.GetView(
      Pos,
      Dir,
      Up);
    AssertVectorEquals(Vector3(1, 2, 3), Pos);
    AssertVectorEquals(Vector3(0, 0, 1), Dir);
    AssertVectorEquals(Vector3(-1, 0, 0), Up);
  finally
    FreeAndNil(C1);
    FreeAndNil(C2);
  end;
end;

procedure TTestCastleComponentSerialize.TestViewportCameraReferencesReading;
var
//  Ui: TCastleUserInterface;
  UiOwner: TComponent;
  Viewport1, Viewport2: TCastleViewport;
begin
  UiOwner := TComponent.Create(nil);
  try
    {Ui := }UserInterfaceLoad('castle-data:/designs/test_viewport_using_another_camera.castle-user-interface', UiOwner);
    Viewport1 := UiOwner.FindRequiredComponent('Viewport1') as TCastleViewport;
    Viewport2 := UiOwner.FindRequiredComponent('Viewport2') as TCastleViewport;
    AssertEquals('Camera2', Viewport1.Camera.Name);
    AssertEquals('Camera2', Viewport2.Camera.Name);
  finally FreeAndNil(UiOwner) end;
end;

procedure TTestCastleComponentSerialize.TestToleratingInvalidReferences;
var
//  Ui: TCastleUserInterface;
  UiOwner: TComponent;
  Viewport1: TCastleViewport;
begin
  UiOwner := TComponent.Create(nil);
  try
    {Ui := }UserInterfaceLoad('castle-data:/designs/test_viewport_invalid_camera_reference.castle-user-interface', UiOwner);
    Viewport1 := UiOwner.FindRequiredComponent('Viewport1') as TCastleViewport;
    AssertTrue(Viewport1.Camera = nil);
  finally FreeAndNil(UiOwner) end;
end;

procedure TTestCastleComponentSerialize.TestPastePreserveReferences;
var
  Viewport1, Viewport2: TCastleViewport;
  Camera1, Camera2: TCastleCamera;
  Background1, Background2: TCastleBackground;
  DesignOwner: TComponent;
begin
  DesignOwner := TComponent.Create(nil);
  try
    Viewport1 := TCastleViewport.Create(DesignOwner);
    Viewport1.Name := 'Viewport1';
    Viewport1.Camera.Free; // remove camera auto-created by TCastleViewport.Create

    Camera1 := TCastleCamera.Create(DesignOwner);
    Camera1.Name := 'Camera1';
    Viewport1.Items.Add(Camera1);

    Background1 := TCastleBackground.Create(DesignOwner);
    Background1.Name := 'Background1';
    Viewport1.AddNonVisualComponent(Background1);

    Viewport1.Camera := Camera1;
    Viewport1.Background := Background1;

    AssertTrue(Viewport1.Items.Count = 1);
    AssertTrue(Viewport1.Items[0] = Camera1);
    AssertTrue(Viewport1.NonVisualComponentsCount = 1);
    AssertTrue(Viewport1.NonVisualComponents[0] = Background1);

    AssertTrue(Viewport1.Camera = Camera1);
    AssertTrue(Viewport1.Background = Background1);

    // copy Viewport1 into *same* owner,
    // we should make new references good.
    Viewport2 := StringToComponent(ComponentToString(Viewport1), DesignOwner)
      as TCastleViewport;

    AssertTrue(Viewport2.Items.Count = 1);
    Camera2 := Viewport2.Items[0] as TCastleCamera;
    AssertTrue(Viewport2.NonVisualComponentsCount = 1);
    Background2 := Viewport2.NonVisualComponents[0] as TCastleBackground;

    // new names should be OK
    AssertEquals('Viewport2', Viewport2.Name);
    AssertEquals('Camera2', Camera2.Name);
    AssertEquals('Background2', Background2.Name);

    // new references should be inter-connected OK
    AssertTrue(Viewport2.Camera = Camera2);
    AssertTrue(Viewport2.Background = Background2);
  finally FreeAndNil(DesignOwner) end;
end;

procedure TTestCastleComponentSerialize.TestPastePreserveReferencesOutsideCopied;

{ Differently than in TTestCastleComponentSerialize.TestPastePreserveReferences,
  we test copying of hierarchy that has a reference to an object outside of
  this hierarchy. In particular, you copy TCastleLabel that has some CustomFont
  assigned, abd this CustomFont is not copied, but it is reachable in the owner.
}

var
  Label1, Label2: TCastleLabel;
  MyFont: TCastleFont;
  DesignOwner: TComponent;
begin
  DesignOwner := TComponent.Create(nil);
  try
    MyFont := TCastleFont.Create(DesignOwner);
    MyFont.Name := 'MyFont';

    Label1 := TCastleLabel.Create(DesignOwner);
    Label1.Name := 'Label1';
    Label1.CustomFont := MyFont;
    Label1.Caption := 'blablabla';

    Label2 := StringToComponent(ComponentToString(Label1), DesignOwner)
      as TCastleLabel;

    AssertEquals('Label2', Label2.Name);
    AssertEquals('blablabla', Label2.Caption);
    AssertTrue(Label2.CustomFont = MyFont);
  finally FreeAndNil(DesignOwner) end;
end;

type
  TRocketDesign = class(TPersistent)
  published
    RocketRigidBody: TCastleRigidBody;
    Sphere1: TCastleTransform;
  end;

procedure TTestCastleComponentSerialize.TestAssociateReferences;
var
  RocketsOwner: TComponent;
  RocketsFactory: TCastleComponentFactory;
  Rocket1: TCastleTransform;
  RocketDesign1: TRocketDesign;
  Rocket2: TCastleTransform;
  RocketDesign2: TRocketDesign;
begin
  RocketsOwner := nil;
  RocketDesign1 := nil;
  RocketDesign2 := nil;
  RocketsFactory := nil;
  try
    RocketsOwner := TComponent.Create(nil);

    RocketsFactory := TCastleComponentFactory.Create(nil);
    RocketsFactory.Url := 'castle-data:/designs/test_associate_references.castle-transform';

    RocketDesign1 := TRocketDesign.Create;
    Rocket1 := RocketsFactory.ComponentLoad(RocketsOwner, RocketDesign1) as TCastleTransform;
    AssertTrue(RocketDesign1.RocketRigidBody <> nil);
    AssertEquals('RocketRigidBody', RocketDesign1.RocketRigidBody.Name);
    AssertTrue(RocketDesign1.Sphere1 <> nil);
    AssertEquals('Sphere1', RocketDesign1.Sphere1.Name);

    RocketDesign2 := TRocketDesign.Create;
    Rocket2 := RocketsFactory.ComponentLoad(RocketsOwner, RocketDesign2) as TCastleTransform;
    AssertTrue(RocketDesign2.RocketRigidBody <> nil);
    AssertEquals('RocketRigidBody1', RocketDesign2.RocketRigidBody.Name);
    AssertTrue(RocketDesign2.Sphere1 <> nil);
    AssertEquals('Sphere2', RocketDesign2.Sphere1.Name);

    { Both RocketDesign1 and RocketDesign2 should have different references,
      despite Rocket1 and Rocket2 having the same owner. }
    AssertTrue(RocketDesign1.RocketRigidBody <> RocketDesign2.RocketRigidBody);
    AssertTrue(RocketDesign1.Sphere1 <> RocketDesign2.Sphere1);
  finally
    // can be freed early, and Rocket1 and Rocket2 remain valid
    FreeAndNil(RocketDesign1);
    FreeAndNil(RocketDesign2);
    AssertTrue(Rocket1.Name <> '');
    AssertTrue(Rocket2.Name <> '');

    // factory can also be freed, and Rocket1 and Rocket2 remain valid
    FreeAndNil(RocketsFactory);
    AssertTrue(Rocket1.Name <> '');
    AssertTrue(Rocket2.Name <> '');

    FreeAndNil(RocketsOwner);
  end;
end;

procedure TTestCastleComponentSerialize.TestAssociateReferencesReuseDesign;
var
  RocketsOwner: TComponent;
  RocketsFactory: TCastleComponentFactory;
  RocketDesign: TRocketDesign;
  Rocket1: TCastleTransform;
  Rocket2: TCastleTransform;
begin
  RocketsOwner := nil;
  RocketDesign := nil;
  RocketsFactory := nil;
  try
    RocketsOwner := TComponent.Create(nil);

    RocketsFactory := TCastleComponentFactory.Create(nil);
    RocketsFactory.Url := 'castle-data:/designs/test_associate_references.castle-transform';

    RocketDesign := TRocketDesign.Create;

    Rocket1 := RocketsFactory.ComponentLoad(RocketsOwner, RocketDesign) as TCastleTransform;
    AssertTrue(RocketDesign.RocketRigidBody <> nil);
    AssertEquals('RocketRigidBody', RocketDesign.RocketRigidBody.Name);
    AssertTrue(RocketDesign.Sphere1 <> nil);
    AssertEquals('Sphere1', RocketDesign.Sphere1.Name);

    Rocket2 := RocketsFactory.ComponentLoad(RocketsOwner, RocketDesign) as TCastleTransform;
    AssertTrue(RocketDesign.RocketRigidBody <> nil);
    AssertEquals('RocketRigidBody1', RocketDesign.RocketRigidBody.Name);
    AssertTrue(RocketDesign.Sphere1 <> nil);
    AssertEquals('Sphere2', RocketDesign.Sphere1.Name);
  finally
    // can be freed early, and Rocket1 and Rocket2 remain valid
    FreeAndNil(RocketDesign);
    AssertTrue(Rocket1.Name <> '');
    AssertTrue(Rocket2.Name <> '');

    // factory can also be freed, and Rocket1 and Rocket2 remain valid
    FreeAndNil(RocketsFactory);
    AssertTrue(Rocket1.Name <> '');
    AssertTrue(Rocket2.Name <> '');

    FreeAndNil(RocketsOwner);
  end;
end;

procedure TTestCastleComponentSerialize.TestSaveTransformNoDefaultVectors;
var
  T: TCastleTransform;
  TAsString: String;
begin
  T := TCastleTransform.Create(nil);
  try
    T.Name := 'MyTransform';
    TAsString := ComponentToString(T);
    // make sure result is sensible
    AssertTrue(Pos('"MyTransform"', TAsString) > 0);
    // make sure result does not contain any vectors,
    // it should not have to -- when everything is default,
    // output should be simple, for easy diffs
    AssertEquals(0, Pos('TCastleVector3Persistent', TAsString));
    AssertEquals(0, Pos('TCastleVector4Persistent', TAsString));
    AssertEquals(0, Pos('TCastleVector4RotationPersistent', TAsString));
    AssertEquals(0, Pos('TCastleColorPersistent', TAsString));
    AssertEquals(0, Pos('TCastleColorRGBPersistent', TAsString));
  finally FreeAndNil(T) end;
end;

procedure TTestCastleComponentSerialize.TestSaveUiNoDefaultVectors;
var
  C: TCastleRectangleControl;
  CAsString: String;
begin
  C := TCastleRectangleControl.Create(nil);
  try
    C.Name := 'MyControl';
    CAsString := ComponentToString(C);
    // make sure result is sensible
    AssertTrue(Pos('"MyControl"', CAsString) > 0);
    // make sure result does not contain any vectors,
    // it should not have to -- when everything is default,
    // output should be simple, for easy diffs
    AssertEquals(0, Pos('TCastleVector3Persistent', CAsString));
    AssertEquals(0, Pos('TCastleVector4Persistent', CAsString));
    AssertEquals(0, Pos('TCastleVector4RotationPersistent', CAsString));
    AssertEquals(0, Pos('TCastleColorPersistent', CAsString));
    AssertEquals(0, Pos('TCastleColorRGBPersistent', CAsString));
  finally FreeAndNil(C) end;
end;

initialization
  RegisterTest(TTestCastleComponentSerialize);

  RegisterSerializableComponent(TMyComponent, 'My Test Component');
  RegisterSerializableComponent(TComponent, 'Component (Basic)');

  RegisterSerializableComponent(TTestComponent1, 'T1');
  RegisterSerializableComponent(TTestComponent2, 'T2');
end.
