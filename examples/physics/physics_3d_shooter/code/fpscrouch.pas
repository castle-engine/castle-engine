unit FpsCrouch;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleViewport, CastleUIControls,
  CastleVectors, CastleInputs;

{ Crouch support by scale collider and change player translation }

type
  TFpsCrouch = class(TCastleBehavior)
  strict private
    FInput_Crouch: TInputShortcut;
    FCrouchSpeed: Single;
    FIsCrouching: Boolean;
  private
    const
      DefaultCrouchSpeed = 3.0;

  protected
     procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  public
     constructor Create(AOwner: TComponent); override;
     property IsCrouching: Boolean read FIsCrouching;
  published
    property Input_Crouch: TInputShortcut read FInput_Crouch;

    property CrouchSpeed: Single
      read FCrouchSpeed write FCrouchSpeed
      {$ifdef FPC}default DefaultCrouchSpeed{$endif};
  end;

implementation

uses CastleUtils, CastleComponentSerialize, CastleKeysMouse;

{ TRotateRigidBodyByKeys }

procedure TFpsCrouch.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
var
  SpeedScale: Single;
  RBody: TCastleRigidBody;
  Collider: TCastleCollider;
begin
  if CastleApplicationMode = appDesign then
    Exit;

  if FocusedContainer = nil then
    Exit;

  RBody := Parent.FindBehavior(TCastleRigidBody) as TCastleRigidBody;

  if Rbody = nil then
    Exit;

  Collider := Parent.FindBehavior(TCastleCollider) as TCastleCollider;

  if (not FIsCrouching) and (Input_Crouch.IsPressed(FocusedContainer)) then
  begin
    { Start crouching }
    Collider.SizeScale := 0.5;
    Parent.Translation := Parent.Translation + Vector3(0, -(Collider.ScaledLocalBoundingBox.SizeY / 2 * 0.99), 0); // place player on ground, 0.99 to ensure player will be above ground
    FIsCrouching := true;
  end else
  if FIsCrouching and (not Input_Crouch.IsPressed(FocusedContainer)) then
  begin
    // TODO: maybe here check we can do it - cant be done in tight places

    Parent.Translation := Parent.Translation + Vector3(0, Collider.ScaledLocalBoundingBox.SizeY / 2 * 1.01, 0); // place player on ground before scale change 1.01 to ensure player will be above ground
    Collider.SizeScale := 1;
    FIsCrouching := false;
  end;

  // TODO: change player speed?

  inherited Update(SecondsPassed, RemoveMe);
end;

constructor TFpsCrouch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCrouchSpeed := DefaultCrouchSpeed;

  FInput_Crouch              := TInputShortcut.Create(Self);

  Input_Crouch              .Assign(keyC);
end;

initialization
  RegisterSerializableComponent(TFpsCrouch, ['Physics', 'FPS Crouch support']);

end.

