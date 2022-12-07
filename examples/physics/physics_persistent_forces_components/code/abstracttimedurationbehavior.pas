{
  Copyright 2022-2022 Michalis Kamburelis, Andrzej Kilijański.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TAbstractTimeDurationBehavior }
unit AbstractTimeDurationBehavior;

interface

uses
  Classes, SysUtils,
  CastleTransform, CastleBehaviors, CastleVectors, CastleClassUtils;

type
  { Functions used in many physics simulation behaviors }
  TAbstractTimeDurationBehavior = class(TCastleBehavior)
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
    property DurationTime: Single read FDurationTime write FDurationTime {$ifdef FPC}default 10{$endif};
    property StartTime: Single read FStartTime write FStartTime {$ifdef FPC}default 0{$endif};
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

