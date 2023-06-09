{
  Copyright 2023-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TSubAnimations behavior to play subanimations on parent TCastleScene. }
unit GameSubAnimations;

interface

uses Classes, Generics.Collections,
  CastleScene, CastleTimeUtils, CastleTransform;

type
  { Subanimation definition used with TSubAnimations. }
  TSubAnimation = class
    Name: String;
    BaseAnimation: String;
    Start: TFloatTime;
    Duration: TFloatTime;
  end;

  TSubAnimationsDictionary = {$ifdef FPC}specialize{$endif} TObjectDictionary<String, TSubAnimation>;

  { Behavior to play subanimations on parent TCastleScene.

    Define subanimations using @link(Add).
    Play them with @link(Play).
    List them with @link(List). }
  TSubAnimations = class(TCastleBehavior)
  private
    FList: TSubAnimationsDictionary;
    FCurrentSubAnimation: TSubAnimation;
    FCurrentTime: TFloatTime;
    FLoop: Boolean;
    ParentScene: TCastleScene;
    procedure UpdateAnimationPose;
  protected
    procedure ParentAfterAttach; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;

    { Define subanimation for future use.
      The TSubAnimation instance given here becomes owned by this class,
      do not manage it anymore. }
    procedure Add(const SubAnimation: TSubAnimation); overload;

    { Define subanimation for future use. }
    procedure Add(const SubAnimationName, BaseAnimation: String;
      const Start, Duration: TFloatTime); overload;

    { List of the subanimations. Read-only. }
    property List: TSubAnimationsDictionary read FList;

    { Start playing given subanimation, by name. }
    procedure Play(const SubAnimationName: String; const ALoop: Boolean); overload;

    { Start playing given subanimation. }
    procedure Play(const SubAnim: TSubAnimation; const ALoop: Boolean); overload;
  end;

implementation

uses SysUtils,
  CastleURIUtils, CastleDownload, CastleStringUtils, CastleLog, CastleUtils;

constructor TSubAnimations.Create(AOwner: TComponent);
begin
  inherited;
  FList := TSubAnimationsDictionary.Create([doOwnsValues]);
end;

destructor TSubAnimations.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

procedure TSubAnimations.ParentAfterAttach;
begin
  inherited;
  ParentScene := Parent as TCastleScene; // TSubAnimations can only be added as behavior to TCastleScene
end;

procedure TSubAnimations.Add(
  const SubAnimationName, BaseAnimation: String;
  const Start, Duration: TFloatTime);
var
  SubAnim: TSubAnimation;
begin
  SubAnim := TSubAnimation.Create;
  SubAnim.Name := SubAnimationName;
  SubAnim.BaseAnimation := BaseAnimation;
  SubAnim.Start := Start;
  SubAnim.Duration := Duration;
  Add(SubAnim);
end;

procedure TSubAnimations.Add(const SubAnimation: TSubAnimation);
begin
  FList.Add(SubAnimation.Name, SubAnimation);
end;

procedure TSubAnimations.Play(const SubAnimationName: String; const ALoop: Boolean);
var
  SubAnim: TSubAnimation;
begin
  if not FList.TryGetValue(SubAnimationName, SubAnim) then
  begin
    WritelnWarning('Cannot play subanimation named "%s", not found', [SubAnimationName]);
    Exit;
  end;
  Play(SubAnim, ALoop);
end;

procedure TSubAnimations.Play(const SubAnim: TSubAnimation; const ALoop: Boolean);
begin
  FCurrentTime := 0;
  FCurrentSubAnimation := SubAnim;
  FLoop := ALoop;
  UpdateAnimationPose;
end;

procedure TSubAnimations.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
  if FCurrentSubAnimation <> nil then
  begin
    FCurrentTime := FCurrentTime + SecondsPassed;
    UpdateAnimationPose;
  end;
end;

procedure TSubAnimations.UpdateAnimationPose;
var
  T: TFloatTime;
begin
  Assert(FCurrentSubAnimation <> nil);
  if FLoop then
    T := FloatModulo(FCurrentTime, FCurrentSubAnimation.Duration)
  else
    T := Clamped(FCurrentTime, 0, FCurrentSubAnimation.Duration);
  ParentScene.ForceAnimationPose(
    FCurrentSubAnimation.BaseAnimation,
    FCurrentSubAnimation.Start + T, false);
end;

end.
