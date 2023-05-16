unit HeadBobbing;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleViewport, CastleUIControls,
  CastleVectors;

type
  THeadBobbing = class(TCastleBehavior)
  strict private
    HeadBobbingPosition: Single;
    HeadBobbingTime: Single;
    FHeadBobbing: Single;
    const
      DefaultHeadBobbingTime = 0.5;
      DefaultHeadBobbing = 0.02;

    function UseHeadBobbing: Boolean; // should check we are on ground,
    function HeadBobbingHeight(PlayerBodyHeight: Single): Single;
  protected
     function GetParentCamera: TCastleCamera; virtual;
     procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  public
     constructor Create(AOwner: TComponent); override;
  end;



implementation

uses CastleUtils, CastleComponentSerialize;

{ THeadBobbing }

function THeadBobbing.UseHeadBobbing: Boolean;
begin
  Result := true;
end;

function THeadBobbing.HeadBobbingHeight(PlayerBodyHeight: Single): Single;
var
  BobbingModifier: Single;
begin
  Result := PlayerBodyHeight;

  if UseHeadBobbing then
  begin
    { HeadBobbingPosition = 0 means that head is at lowest position.
      HeadBobbingPosition = 0.5 means that head is at highest position.
      HeadBobbingPosition = 1.0 means that head is at lowest position again.

      Larger HeadBobbingPosition work like Frac(HeadBobbingPosition)
      (i.e. function HeadBobbingPosition -> BobbingModifier
      is periodic with period = 1.0). }

    BobbingModifier := Frac(HeadBobbingPosition);

    if BobbingModifier <= 0.5 then
      BobbingModifier := MapRange(BobbingModifier, 0.0, 0.5, -1, +1) else
      BobbingModifier := MapRange(BobbingModifier, 0.5, 1.0, +1, -1);

    { Most game tutorials and codes advice that head bobbing be done with sinus,
      as below. But actually I found that the visual difference between
      sin-based head bobbing and linear-based (like above) head bobbing
      is not noticeable, so I'm using linear-based right now (as it's
      a little faster --- no trig calculation needed, although this
      could be avoided with sinus lookup table).

      If however you prefer sin-based head bobbing, uncomment line below
      and comment out 3 lines "if BobbingModifier <= 0.5 then ...." above.

    BobbingModifier := Sin(BobbingModifier * 2 * Pi);
    }

    BobbingModifier := BobbingModifier * (Result * FHeadBobbing);
    Result := Result + BobbingModifier;
  end;
end;

function THeadBobbing.GetParentCamera: TCastleCamera;
var
  I: Integer;
begin
  for I := 0 to Parent.Count -1 do
  begin
    if Parent.Items[I] is TCastleCamera then
      Exit(Parent.Items[I] as TCastleCamera);
  end;
end;

procedure THeadBobbing.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
var
  RBody: TCastleRigidBody;
  Collider: TCastleCollider;
  Camera: TCastleCamera;
  CamTransl: TVector3;
  PlayerHeight: Single;
begin
  RBody := Parent.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
  if RBody = nil then
    Exit;

  Collider := Parent.FindBehavior(TCastleCollider) as TCastleCollider;
  if Collider = nil then
    Exit;

  Camera := GetParentCamera;
  if Camera = nil then
    Exit;

  PlayerHeight := Collider.ScaledLocalBoundingBox.SizeY;
  HeadBobbingPosition := HeadBobbingPosition + (SecondsPassed / HeadBobbingTime);

  { Calculate new camera position }
  CamTransl := Camera.Translation;
  CamTransl.Y := HeadBobbingHeight(PlayerHeight) - PlayerHeight;
  Camera.Translation := CamTransl;

  inherited Update(SecondsPassed, RemoveMe);
end;

constructor THeadBobbing.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  HeadBobbingPosition := 0;
  HeadBobbingTime := DefaultHeadBobbingTime;
  FHeadBobbing := DefaultHeadBobbing;
end;

initialization
  RegisterSerializableComponent(THeadBobbing, ['Physics', 'Head Bobbing']);

end.

