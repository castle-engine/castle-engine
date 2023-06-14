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

    { Time of the first frame.
      Divide by Md3Fps to get time in seconds. }
    FirstFrame: Cardinal;

    { Duration of animation.
      Divide by Md3Fps to get duration in seconds.

      May be negative in animation.cfg (likely to indicate playing animation backwards,
      not supported). We ignore it for now, i.e. we takes
      Abs(NumFrames)/30 as just duration in seconds. }
    NumFrames: Integer;

    { We read these from animation.cfg, but don't support them now. }
    LoopingFrames, Fps: Cardinal;
  end;

  { MD3 animations list. }
  TMd3Animations = class({$ifdef FPC}specialize{$endif} TObjectList<TMd3Animation>)
    { Read animation information from animation.cfg file. }
    procedure ReadAnimationCfg(const AnimationCfgUrl: String);
  end;

procedure TMd3Animations.ReadAnimationCfg(const AnimationCfgUrl: String);
var
  Reader: TTextReader;
  Line: String;
  Anim: TMd3Animation;
  Tokens: TCastleStringList;
begin
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
          WritelnWarning('Unexpected line format in animation.cfg "%s", not 5 items', [Line]);
          Continue;
        end;

        // line defines a animation
        Anim := TMd3Animation.Create;
        Anim.FirstFrame := StrToInt(Tokens[0]);
        Anim.NumFrames := StrToInt(Tokens[1]);
        Anim.LoopingFrames := StrToInt(Tokens[2]);
        Anim.Fps := StrToInt(Tokens[3]);
        Anim.Name := PrefixRemove('//', Tokens[4], true);
        Add(Anim);
      finally FreeAndNil(Tokens) end;
    end;
  finally FreeAndNil(Reader) end;
end;
