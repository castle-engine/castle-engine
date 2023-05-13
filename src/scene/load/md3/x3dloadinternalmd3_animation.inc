{%MainUnit x3dloadinternalmd3.pas}
{
  Copyright 2007-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ MD3 animation support (TMd3Animations and TMd3Animation classes),
  with animations info parsed from animation.cfg. }

type
  { MD3 animation information.
    Encoded following MD3 animation.cfg conventions.
    See e.g. https://github.com/edems96/MD3-Model---Animation-Viewer/blob/master/src/md3anim.h
    for sample source code reading MD3 animations. }
  TMd3Animation = class
    Name: String;

    { Number of the first frame. }
    FirstFrame: Cardinal;

    { Duration of animation, in frames.
      Divide by @link(Fps) to get duration in seconds.

      May be negative in animation.cfg (likely to indicate playing animation backwards,
      not supported). We ignore it for now, i.e. we take
      Abs(NumFrames)/Fps as just duration in seconds. }
    NumFrames: Integer;

    { We read this from animation.cfg, but don't support now.
      It seems that it is either 0 (when animation is not supposed
      to be used a looped) or equal to NumFrames (when animation is
      supposed to be used as looped). }
    LoopingFrames: Cardinal;

    { Frames per second, controlling the speed of this animation. }
    Fps: Cardinal;
  end;

  { MD3 animations list. }
  TMd3Animations = class({$ifdef FPC}specialize{$endif} TObjectList<TMd3Animation>)
    { Read animation information from animation.cfg file. }
    procedure ReadAnimationCfg(const AnimationCfgUrl: String);

    { Delete from the list animations that are not suitable for the model with NumFrames.

      This is necessary, as animation.cfg is sometimes placed adjacent to multiple MD3
      files, and it matches some but not all animations there,
      i.e. our current MD3 model may not even have enough frames to perform this animation.
      By removing invalid animations, we let TMd3Converter to process all animations
      without fear that it will cause buffer overflow when reading e.g. Tags array.

      We don't remove all animations, as this would remove too much.
      E.g. in Tremulous human player animations, this approach allows to at least
      play attack, gesture etc. of upper.md3. }
    procedure ValidateFrames(const NumFrames: Cardinal);
  end;

procedure TMd3Animations.ReadAnimationCfg(const AnimationCfgUrl: String);
var
  LegsOffset: Integer;

  { For unknown reason, the first frame numbers for LEGS_xxx animations are shifted
    in at least Tremulous models. If we take them exactly, the legs animations
    will be broken (some LEGS_xxx animations will be nonsense, some will go beyond
    available number of frames).

    The solution is to shift the first frame by the difference in frames
    between LEGS_WALKCR and TORSO_GESTURE.
    This is a terrible hack, and I can only guess it is necessary because of some
    late hack in id Tech MD3 handling -- instead of fixing frame numbers in animation.cfg,
    someone decided to specifically shift LEGS_xxx numbers in code.
    And in effect everyone now has to shift LEGS_xxx in code, otherwise they are broken.

    The solution credit goes to
    https://github.com/MJL85/MenderD3/ , specifically
    https://github.com/MJL85/MenderD3/blob/4b95d8b0d03e46513d49e12c17ecac23f1ba8dd4/src/md3_parse.c#L702 . }
  procedure FixLegsFirstFrame(const Anim: TMd3Animation);

    function FirstFrameOf(const AnimName: String): Integer;
    var
      I: Integer;
    begin
      for I := 0 to Count - 1 do
        if Items[I].Name = AnimName then
          Exit(Items[I].FirstFrame);
      Result := 0;
    end;

  var
    LegsOffset1, LegsOffset2: Integer;
  begin
    if IsPrefix('LEGS_', Anim.Name, false) then
    begin
      { calculate LegsOffset only once.
        Not only for performance, also because LEGS_WALKCR first frame will be changed
        by this offset right after first calculation. }
      if LegsOffset = 0 then
      begin
        LegsOffset1 := FirstFrameOf('LEGS_WALKCR');
        LegsOffset2 := FirstFrameOf('TORSO_GESTURE');
        LegsOffset := LegsOffset1 - LegsOffset2;
        {WritelnLog('Shifting MD3 LEGS animations first frame by LEGS_WALKCR - TORSO_GESTURE difference (%d - %d)', [
          LegsOffset1,
          LegsOffset2
        ]);}
      end;
      Anim.FirstFrame := Anim.FirstFrame - LegsOffset;
    end;
  end;

var
  Reader: TTextReader;
  Line, LineComment, AnimationName: String;
  Anim: TMd3Animation;
  Tokens: TCastleStringList;
  CommentPos: Integer;
begin
  LegsOffset := 0;
  Reader := TTextReader.Create(AnimationCfgUrl);
  try
    while not Reader.Eof do
    begin
      Line := Reader.ReadLn;

      { Calculate LineComment (after //), and trim Line (prefix before //).

        Animation name is the first token after //
        (sometimes separated by whitespace from //, sometimes not).
        So we do not just reject the comment, we keep it for later.
        We have to handle lines like this (examples from Tremulous):

          130   6       0       15              // TORSO_ATTACK         (MUST NOT CHANGE -- hand animation is synced to this)
          90    40      0       20              // TORSO_GESTURE
          41    20    20    30  //NSPA_STAND
      }
      CommentPos := Pos('//', Line);
      if CommentPos <> 0 then
      begin
        LineComment := SEnding(Line, CommentPos + 2);
        Line := Copy(Line, 1, CommentPos - 1);
      end else
        LineComment := '';

      Tokens := CreateTokens(Line);
      try
        // skip lines that don't interest us
        if Tokens.Count = 0 then
          Continue; // empty line
        if (Tokens[0] = 'sex') or // various ignored information
           (Tokens[0] = 'footsteps') or
           (Tokens[0] = 'headoffset') or // example in upper.md3 submitted by https://forum.castle-engine.io/t/md3-improve-animations-support/775/61?u=michalis
           (Tokens[0] = 'nonsegmented') then
          Continue;
        if Tokens.Count <> 4 then
        begin
          WritelnWarning('Unexpected line format in animation.cfg "%s", not 4 items before comment', [Line]);
          Continue;
        end;

        AnimationName := NextTokenOnce(LineComment);
        if AnimationName = '' then
        begin
          WritelnWarning('Empty animation name in animation.cfg');
          Continue;
        end;

        // line defines a animation
        Anim := TMd3Animation.Create;
        Anim.FirstFrame := StrToInt(Tokens[0]);
        Anim.NumFrames := StrToInt(Tokens[1]);
        Anim.LoopingFrames := StrToInt(Tokens[2]);
        Anim.Fps := StrToInt(Tokens[3]);
        Anim.Name := AnimationName;
        Add(Anim);

        FixLegsFirstFrame(Anim);
      finally FreeAndNil(Tokens) end;
    end;
  finally FreeAndNil(Reader) end;
end;

procedure TMd3Animations.ValidateFrames(const NumFrames: Cardinal);
var
  RequiredNumFrames, I: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    RequiredNumFrames := Items[I].FirstFrame + Abs(Items[I].NumFrames);
    if RequiredNumFrames > NumFrames then
    begin
      // This warning would be too spamming, ignore
      {
      WritelnWarning('Animation "%s" defined in "animation.cfg" file would require more frames (%d) than this MD3 model has (%d), ignoring this animation.', [
        Items[I].Name,
        RequiredNumFrames,
        NumFrames
      ]);
      }
      Delete(I);
    end;
  end;
end;
