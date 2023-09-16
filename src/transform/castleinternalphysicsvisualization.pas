{
  Copyright 2022-2022 Andrzej Kilijański, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Visualization of physics stuff. }
unit CastleInternalPhysicsVisualization;

interface

uses
  Classes, SysUtils, Math,
  CastleTransform, CastleColors, CastleRenderOptions,
  CastleVectors, CastleScene, CastleClassUtils, CastleUtils;

type
  TOnGetValue = function: TVector3 of object;
  TOnSetValue = procedure (const Value: TVector3) of object;

  { Ancestor for design-time tools created by behaviors to manipulate e.g. joints. }
  TCastleToolTransform = class(TCastleTransform);

  { Ancestor for design-time tools created by physics joints. }
  TCastleJointTool = class(TCastleToolTransform)
  strict private
    FColor: TCastleColor;
    FJoint: TCastleAbstractJoint;
    procedure SetColor(const Value: TCastleColor);
  protected
    FSphere: TCastleSphere;

    // For now it's actually more natural to just *not* adjust sphere size
    //function EstimateSphereRadius: Single;

    function GetValue: TVector3;
    procedure SetValue(const AValue: TVector3);
    procedure ChangedTransform; override;
  public
    OnGetValue: TOnGetValue;
    OnSetValue: TOnSetValue;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    property Value: TVector3 read GetValue write SetValue;
    property Joint: TCastleAbstractJoint read FJoint write FJoint;
    property Color: TCastleColor read FColor write SetColor;
  end;

const
  ConnectedAnchorColor: TCastleColor = (X: 0.0; Y: 1.0; Z: 0.0; W: 1.0); // = Green;

implementation

uses CastleRenderContext;

{ TCastleJointTool --------------------------------------------------- }

constructor TCastleJointTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FColor := Red;
  SetTransient;

  FSphere := TCastleSphere.Create(nil);
  FSphere.RenderLayer := rlFront;
  FSphere.SetTransient;
  FSphere.Color := FColor;
  FSphere.Material := pmUnlit;
  FSphere.Radius := 0.1;
  Add(FSphere);
end;

destructor TCastleJointTool.Destroy;
begin
  FreeAndNil(FSphere);
  inherited Destroy;
end;

procedure TCastleJointTool.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
// var
//   NewRadius: Single;
begin
  inherited Update(SecondsPassed, RemoveMe);

  (*
  if FSphere <> nil then
  begin
    NewRadius := EstimateSphereRadius;
    if not SameValue(NewRadius, FSphere.Radius) then
    begin
      FSphere.Radius := NewRadius;
    end;
  end;
  *)
end;

function TCastleJointTool.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
      'TranslationPersistent'
    ]) then
    Result := [psBasic, psLayout]
  else
    Result := [];
end;

procedure TCastleJointTool.SetColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FColor, Value) then
  begin
    FColor := Value;
    if FSphere <> nil then
      FSphere.Color := FColor;
  end;
end;

(*
function TCastleJointTool.EstimateSphereRadius: Single;
var
  Viewport: TCastleViewport;
  SphereWorldPos1, SphereWorldPos2: TVector3;
  SphereViewportPos1, SphereViewportPos2: TVector2;
  DistanceOnViewport: Single;
begin
  if (Parent <> nil) and (Parent.World <> nil) and (Parent.World.Owner <> nil) and
     (Parent.World.Owner is TCastleViewport) then
  begin
    Viewport := Parent.World.Owner as TCastleViewport;
    SphereWorldPos1 := FSphere.LocalToWorld(TVector3.Zero);
    SphereViewportPos1 := Viewport.PositionFromWorld(SphereWorldPos1);

    SphereWorldPos2 := SphereWorldPos1 + CastleVectors.Vector3(0, 1, 0);
    SphereViewportPos2 := Viewport.PositionFromWorld(SphereWorldPos2);
    DistanceOnViewport := PointsDistance(SphereViewportPos1, SphereViewportPos2);

    { Make size about 10 pixels }
    Result := 10 / DistanceOnViewport;
  end else
    Result := 1;
end;
*)

function TCastleJointTool.GetValue: TVector3;
begin
  Assert(Assigned(OnGetValue));
  Result := OnGetValue();
end;

procedure TCastleJointTool.SetValue(const AValue: TVector3);
begin
  Assert(Assigned(OnSetValue));
  OnSetValue(AValue);
  if not TVector3.PerfectlyEquals(Translation, AValue) then
    Translation := AValue;
end;

procedure TCastleJointTool.ChangedTransform;
begin
  inherited;
  if not TVector3.PerfectlyEquals(Translation, Value) then
    Value := Translation;
end;

end.
