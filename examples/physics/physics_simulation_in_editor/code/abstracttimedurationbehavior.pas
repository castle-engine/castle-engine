unit AbstractTimeDurationBehavior;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CastleTransform, CastleBehaviors, CastleVectors, CastleClassUtils;

type
  { Functions used in many physics simulation behaviors }
  TAbstractTimeDurationBehavior = class (TCastleBehavior)
  private
    FStartTime: Single;
    FExpiredTimeToStart: Single;
    FDurationTime: Single;
    FExpiredDurationTime: Single;
    FOneShot: Boolean;
    FWasShot: Boolean;
  protected
    { Checks Start Time expired }
    function ShouldStart: Boolean;
    { Checks Duration Time expired }
    function ShouldStop: Boolean;
    { Check should we run our code in Update }
    function ShouldUpdate: Boolean;
    { If OneShot = true make a shot }
    procedure Shot;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;

    property WasShot: Boolean read FWasShot;

    function PropertySections(const PropertyName: String): TPropertySections; override;

  published
    property OneShot: Boolean read FOneShot write FOneShot default false;
    property DurationTime: Single read FDurationTime write FDurationTime default 10;
    property StartTime: Single read FStartTime write FStartTime default 0;
  end;

implementation

{ TAbstractTimeDurationBehavior ---------------------------------------------- }

function TAbstractTimeDurationBehavior.ShouldStart: Boolean;
begin
  Result := FExpiredTimeToStart >= FStartTime;
end;

function TAbstractTimeDurationBehavior.ShouldStop: Boolean;
begin
  Result := FExpiredDurationTime >= FDurationTime;
end;

function TAbstractTimeDurationBehavior.ShouldUpdate: Boolean;
begin
  if not World.IsPhysicsRunning then
    Exit(false);

  if OneShot then
  begin
    if WasShot then
      Exit(false)
    else
      Shot;
  end else
  if (not ShouldStart) or (ShouldStop) then
    Exit(false);

  Result := true;
end;

procedure TAbstractTimeDurationBehavior.Shot;
begin
  FWasShot := true;
end;

constructor TAbstractTimeDurationBehavior.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDurationTime := 10;
  FStartTime := 0;
  FExpiredTimeToStart := 0;
  FExpiredDurationTime := 0;
end;

procedure TAbstractTimeDurationBehavior.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
begin
  inherited Update(SecondsPassed, RemoveMe);

  if not World.IsPhysicsRunning then
    Exit;

  if ShouldStart then
    FExpiredDurationTime := FExpiredDurationTime + SecondsPassed
  else
    FExpiredTimeToStart := FExpiredTimeToStart + SecondsPassed;
end;

function TAbstractTimeDurationBehavior.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'OneShot') or
     (PropertyName = 'DurationTime') or
     (PropertyName = 'StartTime') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

end.

