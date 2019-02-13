{
  Copyright 2017-2018 Michalis Kamburelis.

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
  Classes, SysUtils, fpcunit, testutils, testregistry, CastleTestCase;

type
  TTestCastleComponentSerialize = class(TCastleTestCase)
    procedure TestDefaultValues;
    procedure TestEmptyCaption;
  end;

implementation

uses CastleFilesUtils, CastleComponentSerialize, CastleVectors,
  CastleUIControls, CastleControls;

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
  FPositionPersistent.InternalGetValue := @GetPosition;
  FPositionPersistent.InternalSetValue := @SetPosition;
  FPositionPersistent.InternalDefaultValue := FPosition; // current value is default

  FScalePersistent := TCastleVector3Persistent.Create;
  FScalePersistent.InternalGetValue := @GetScale;
  FScalePersistent.InternalSetValue := @SetScale;
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

initialization
  RegisterTest(TTestCastleComponentSerialize);
  RegisterSerializableComponent(TMyComponent, 'My Test Component');
end.
