unit CheckWall;

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleViewport, CastleUIControls,
  CastleVectors,  CastleInputs, CastleInputAxis, CastleClassUtils;

type

  { Workaround for projectiles going through the wall  }
  TCheckWall = class(TCastleBehavior)
  strict private
    FMinVelocity: Single;
    FPreviousPosition: TVector3;

    const DefaultMinVelocity = 50.0;
  protected
     procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  public
     constructor Create(AOwner: TComponent); override;

     function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property MinVelocity: Single read FMinVelocity write FMinVelocity {$ifdef FPC}default DefaultMinVelocity{$endif};
  end;

implementation

uses CastleUtils, CastleComponentSerialize, CastleKeysMouse, CastleLog;

{ TCheckWall ----------------------------------------------------------------- }

procedure TCheckWall.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
var
  Velocity: TVector3;
  VelocityLength: Single;
  RayResult: TPhysicsRayCastResult;
  RBody: TCastleRigidBody;
  RCollider: TCastleCollider;
begin
  //Exit;

  if CastleApplicationMode = appDesign then
    Exit;
  RBody := Parent.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
  if RBody = nil then
    Exit;

  RCollider := Parent.FindBehavior(TCastleCollider) as TCastleCollider;
  if RCollider = nil then
    Exit;

  Velocity := RBody.LinearVelocity;
  VelocityLength := Velocity.Length * SecondsPassed;

  if VelocityLength > FMinVelocity * SecondsPassed then
  begin
    //WritelnLog('Check by cast');
    RayResult := RBody.PhysicsRayCast(RCollider.Middle, Velocity.Normalize, (VelocityLength + RCollider.ScaledLocalBoundingBox.MaxSize / 2) * 1.5 );
    if RayResult.Hit then
    begin
      if RayResult.Transform.Name = 'Player' then
        Exit;
      WritelnLog(' ---- ' );
      WritelnLog('Velocity.Normalize :' + Velocity.Normalize.ToString);
      WritelnLog('Hits: ' + RayResult.Transform.Name);
      WritelnLog('Parent translation A ' + Parent.Translation.ToString);
      WritelnLog('Distance ' + FloatToStr(RayResult.Distance));
      WritelnLog('VelocityLength before ' + FloatToStr(VelocityLength));
      WritelnLog('Velocity before ' + Velocity.ToString);
      //Velocity := RayResult.Point - RCollider.Middle;
      {Velocity := Velocity.Normalize * RayResult.Distance;
      RBody.LinearVelocity := Velocity;
      WritelnLog('Slowdown ' + Velocity.ToString);}
      RBody.LinearVelocity := TVector3.Zero;
      Velocity.Y := 0;
      //Parent.Translation := Parent.Translation - Velocity * SecondsPassed;
      Parent.Translation := FPreviousPosition;
      //Parent.Translation := RayResult.Point;
      WritelnLog('Parent translation B ' + Parent.Translation.ToString);
    end;
    FPreviousPosition := Parent.Translation;
  end;

  //WritelnLog(Parent.Translation.ToString);

  inherited Update(SecondsPassed, RemoveMe);
end;

constructor TCheckWall.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMinVelocity := DefaultMinVelocity;
  FPreviousPosition := TVector3.Zero;
end;

function TCheckWall.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
     'MinVelocity'
     ]) then
    Result := [psBasic]
  else
  Result := inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TCheckWall, ['Physics', 'Check wall for fast bullets']);

end.

