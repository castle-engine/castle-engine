// -*- compile-command: "cd ../ && ./compile_console.sh && ./test_castle_game_engine --suite=TTestCastleComponentSerialize" -*-
{
  Copyright 2017-2021 Michalis Kamburelis.

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
  Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry,
  CastleTestCase{$else}CastleTester{$endif};

type
  TTestCastleComponentSerialize = class(TCastleTestCase)
    procedure TestDefaultValues;
    procedure TestEmptyCaption;
    procedure TestSaveLoad1;
    procedure TestSaveLoad2;
    {$ifdef FPC}
    procedure TestDeserializeObjectReferences;
    {$endif}
    procedure TestDepth;
  end;

implementation

uses CastleFilesUtils, CastleComponentSerialize, CastleVectors,
  CastleUIControls, CastleControls, CastleUtils, {$ifdef FPC}CastleSceneManager,{$endif}
  CastleScene{$ifdef FPC},
  { needed to deserialize castle-data:/designs/test_object_references.castle-user-interface }
  Castle2DSceneManager{$endif};

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
  FPositionPersistent.InternalGetValue := {$ifdef FPC}@{$endif}GetPosition;
  FPositionPersistent.InternalSetValue := {$ifdef FPC}@{$endif}SetPosition;
  FPositionPersistent.InternalDefaultValue := FPosition; // current value is default

  FScalePersistent := TCastleVector3Persistent.Create;
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
  Position := AValue;
end;

function TMyComponent.GetScale: TVector3;
begin
  Result := Scale;
end;

procedure TMyComponent.SetScale(const AValue: TVector3);
begin
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

{$ifdef FPC}
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
{$endif FPC}


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
    {$ifdef CASTLE_TESTER}TestLog{$else}Writeln{$endif}(
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

initialization
{$ifndef CASTLE_TESTER}
  RegisterTest(TTestCastleComponentSerialize);
{$endif}
  RegisterSerializableComponent(TMyComponent, 'My Test Component');
  RegisterSerializableComponent(TComponent, 'Component (Basic)');
end.
