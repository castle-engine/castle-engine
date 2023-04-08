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

{ TSceneSubAnimations class. }
unit GameSceneSubAnimations;

interface

uses Classes, Generics.Collections,
  CastleScene, CastleTimeUtils, CastleTransform;

type
  TSubAnimation = class
    Name: String;

    { Subanimation information below is encoded following MD3 animation.cfg conventions.
      See e.g. https://github.com/edems96/MD3-Model---Animation-Viewer/blob/master/src/md3anim.h
      for source code reading it. }

    { Time of first frame.

      In the current implementation of TSceneSubAnimations,
      FirstFrame/30 is just a time in seconds. }
    FirstFrame: Cardinal;

    { Duration of subanimation.
      May be negative in animation.cfg (likely to indicate playing animation backwards,
      not supported).

      In the current implementation of TSceneSubAnimations,
      Abs(NumFrames)/30 is just subanimation duration in seconds. }
    NumFrames: Integer;

    { We read these from animation.cfg, but don't support them now. }
    LoopingFrames, Fps: Cardinal;
  end;

  TSubAnimations = {$ifdef FPC}specialize{$endif} TObjectDictionary<String, TSubAnimation>;

  { Descendant of TCastleScene with subanimations,
    that are subranges of a main animation called "animation".

    We read the subanimations from the "animation.cfg" file that has to exist alongside
    the md3 file.
    Play them with PlaySubAnimation,
    list with SubAnimations. }
  TSceneSubAnimations = class(TCastleScene)
  private
    FSubAnimations: TSubAnimations;
    FCurrentSubAnimation: TSubAnimation;
    FCurrentTime: TFloatTime;
    FLoop: Boolean;
    procedure UpdateAnimationPose;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;

    { Load the model from ModelUrl, additionally initialize subanimations from
      "animation.cfg". }
    procedure Load(const ModelUrl: String);

    { List of the subanimation names. Read-only. }
    property SubAnimations: TSubAnimations read FSubAnimations;

    { Start playing given subanimation, by name. }
    procedure PlaySubAnimation(const SubAnimationName: String; const ALoop: Boolean); overload;

    { Start playing given subanimation. }
    procedure PlaySubAnimation(const SubAnim: TSubAnimation; const ALoop: Boolean); overload;
  end;

implementation

uses SysUtils,
  CastleURIUtils, CastleDownload, CastleStringUtils, CastleLog, CastleUtils;

constructor TSceneSubAnimations.Create(AOwner: TComponent);
begin
  inherited;
  FSubAnimations := TSubAnimations.Create([doOwnsValues]);
end;

destructor TSceneSubAnimations.Destroy;
begin
  FreeAndNil(FSubAnimations);
  inherited;
end;

procedure TSceneSubAnimations.Load(const ModelUrl: String);

  procedure ReadAnimationCfg(const AnimationCfgUrl: String);
  var
    Reader: TTextReader;
    Line: String;
    SubAnim: TSubAnimation;
    Tokens: TCastleStringList;
  begin
    FSubAnimations.Clear;

    Reader := TTextReader.Create(AnimationCfgUrl);
    try
      while not Reader.Eof do
      begin
        Line := Reader.ReadLn;
        Tokens := CreateTokens(Line);
        try
          // skip lines that don't interest us
          if Tokens.Count = 0 then
            Continue; // empty line
          if (Tokens[0] = '//') or // comment
             (Tokens[0] = 'sex') or // various ignored information
             (Tokens[0] = 'footsteps') or
             (Tokens[0] = 'nonsegmented') then
            Continue;
          if Tokens.Count <> 5 then
          begin
            WritelnWarning('Unexpected line format "%s", not 5 items', [Line]);
            Continue;
          end;

          // line defines a subanimation
          SubAnim := TSubAnimation.Create;
          SubAnim.FirstFrame := StrToInt(Tokens[0]);
          SubAnim.NumFrames := StrToInt(Tokens[1]);
          SubAnim.LoopingFrames := StrToInt(Tokens[2]);
          SubAnim.Fps := StrToInt(Tokens[3]);
          SubAnim.Name := PrefixRemove('//', Tokens[4], true);
          FSubAnimations.Add(SubAnim.Name, SubAnim);
        finally FreeAndNil(Tokens) end;
      end;
    finally FreeAndNil(Reader) end;

    WritelnLog('Model "%s" has %d subanimations', [
      URIDisplay(ModelUrl),
      FSubAnimations.Count
    ]);
  end;

begin
  inherited Load(ModelUrl);
  ReadAnimationCfg(ExtractURIPath(ModelUrl) + 'animation.cfg');
end;

procedure TSceneSubAnimations.PlaySubAnimation(const SubAnimationName: String; const ALoop: Boolean);
var
  SubAnim: TSubAnimation;
begin
  if not FSubAnimations.TryGetValue(SubAnimationName, SubAnim) then
  begin
    WritelnWarning('Cannot play subanimation named "%s", not found', [SubAnimationName]);
    Exit;
  end;
  PlaySubAnimation(SubAnim, ALoop);
end;

procedure TSceneSubAnimations.PlaySubAnimation(const SubAnim: TSubAnimation; const ALoop: Boolean);
begin
  FCurrentTime := 0;
  FCurrentSubAnimation := SubAnim;
  FLoop := ALoop;
  UpdateAnimationPose;
end;

procedure TSceneSubAnimations.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
  if FCurrentSubAnimation <> nil then
  begin
    FCurrentTime := FCurrentTime + SecondsPassed;
    UpdateAnimationPose;
  end;
end;

procedure TSceneSubAnimations.UpdateAnimationPose;
const
  Md3Fps = 30;
var
  SubAnimStart, SubAnimDuration, T: TFloatTime;
begin
  Assert(FCurrentSubAnimation <> nil);
  { We use -1 below, to not show last frame of this animation blended with 1st frame
    of next animation in MD3 sequence. }
  SubAnimDuration := (Abs(FCurrentSubAnimation.NumFrames) - 1) / Md3Fps;
  SubAnimStart := FCurrentSubAnimation.FirstFrame / Md3Fps;
  if FLoop then
    T := FloatModulo(FCurrentTime, SubAnimDuration)
  else
    T := Clamped(FCurrentTime, 0, SubAnimDuration);
  ForceAnimationPose('animation', SubAnimStart + T, false);
end;

end.
