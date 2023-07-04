unit AnimationTrigger;

interface

uses
  Classes, SysUtils, ModularMovement, CastleTransform, CastleBehaviors,
  CastleVectors, CastleClassUtils;

type

  { Animation trigger for TFpsModularMovement. Changes animation by using
    TModularMovementState }
  TAnimationTrigger = class(TAbstractMovementModifier)
  strict private
    FIdleAnimation: String;
    FWalkAnimation: String;
    FFlyAnimation: String;
    FJumpAnimation: String;
    FFallAnimation: String;

  public
    constructor Create(AOwner: TComponent); override;

    function PropertySections(const PropertyName: String): TPropertySections; override;

    procedure UpdateMovement(const MovementState: TModularMovementState); override;
  published
    property IdleAnimation: String read FIdleAnimation write FIdleAnimation;
    property WalkAnimation: String read FWalkAnimation write FWalkAnimation;
    property FlyAnimation: String read FFlyAnimation write FFlyAnimation;
    property JumpAnimation: String read FJumpAnimation write FJumpAnimation;
    property FallAnimation: String read FFallAnimation write FFallAnimation;
  end;

implementation

uses Math, CastleUtils, CastleComponentSerialize, CastleKeysMouse, CastleLog;

{ TAnimationTrigger ------------------------------------------------------------ }

procedure TAnimationTrigger.UpdateMovement(const MovementState: TModularMovementState);
begin

end;

constructor TAnimationTrigger.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

function TAnimationTrigger.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
      'IdleAnimation', 'WalkAnimation', 'FlyAnimation', 'JumpAnimation',
      'FallAnimation'
    ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TAnimationTrigger, ['Navigation', 'Modules', 'Animation Trigger']);

end.

