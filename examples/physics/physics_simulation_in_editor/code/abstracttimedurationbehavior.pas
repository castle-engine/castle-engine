unit AbstractTimeDurationBehavior;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleVectors;

type
  TAbstractTimeDurationBehavior = class (TCastleBehavior)
  private
    FStartTime: Single;
    FExpiredTimeToStart: Single;
    FDurationTime: Single;
    FExpiredDurationTime: Single;
    FOneShot: Boolean;
    FShoted: Boolean;
  protected
    function ShouldStart: Boolean;
    function ShouldStop: Boolean;
    procedure Shot;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;

    property Shoted: Boolean read FShoted;

  published
    property OneShot: Boolean read FOneShot write FOneShot default false;
    property DurationTime: Single read FDurationTime write FDurationTime default 10;
    property StartTime: Single read FStartTime write FStartTime default 0;
  end;

implementation

{ TAbstractTimeDurationBehavior }

function TAbstractTimeDurationBehavior.ShouldStart: Boolean;
begin
  Result := FExpiredTimeToStart >= FStartTime;
end;

function TAbstractTimeDurationBehavior.ShouldStop: Boolean;
begin
  Result := FExpiredDurationTime >= FDurationTime;
end;

procedure TAbstractTimeDurationBehavior.Shot;
begin
  FShoted := true;
end;

constructor TAbstractTimeDurationBehavior.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDurationTime := 10;
  FStartTime := 0;
end;

procedure TAbstractTimeDurationBehavior.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
begin
  inherited Update(SecondsPassed, RemoveMe);

  if ShouldStart then
    FExpiredDurationTime := FExpiredDurationTime + SecondsPassed
  else
    FExpiredTimeToStart := FExpiredTimeToStart + SecondsPassed;
end;

end.

