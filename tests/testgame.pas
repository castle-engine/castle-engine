{
  Copyright 2004-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestGame;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestGame = class(TTestCase)
    procedure TestGameData;
  end;

implementation

uses CastleVectors, CastleLevels, CastleResources, CastleSoundEngine, CastlePlayer,
  CastleMaterialProperties, CastleCreatures, CastleShapes, Castle3D;

procedure TTestGame.TestGameData;

  procedure AssertFloat(const A, B: Single);
  begin
    Assert(FloatsEqual(A, B, 0.01));
  end;

  procedure AssertFileName(const A, B: string);
  begin
    { When reading XML files, we make filenames absolute. For comparison,
      strip directory part. }
    Assert(ExtractFileName(A) = B);
  end;

  procedure AssertSound(const A: TSoundType; const B: string);
  begin
    Assert(A = SoundEngine.SoundFromName(B));
  end;

  procedure AssertVector(const A: TVector3Single; const BX, BY, BZ: Single);
  begin
    Assert(VectorsEqual(A, Vector3Single(BX, BY, BZ), 0.01));
  end;

var
  Player: TPlayer;
  RemovePlayer: TRemoveType;
  SoundType: TSoundType;
  C: TWalkAttackCreatureResource;
begin
  SoundEngine.SoundsFileName := 'data/game/sounds.xml';

  Assert(SoundEngine.Sounds[stNone].Name = '');
  Assert(SoundEngine.Sounds[stNone].FileName = '');
  SoundType := SoundEngine.SoundFromName('player_sudden_pain');
  Assert(SoundEngine.Sounds[SoundType].Name = 'player_sudden_pain');
  AssertFileName(SoundEngine.Sounds[SoundType].FileName, 'test_name.wav');
  Assert(SoundEngine.Sounds[SoundType].DefaultImportance = PlayerSoundImportance);
  AssertFloat(SoundEngine.Sounds[SoundType].Gain, 1);
  AssertFloat(SoundEngine.Sounds[SoundType].MinGain, 0.8);
  AssertFloat(SoundEngine.Sounds[SoundType].MaxGain, 1);

  Resources.LoadFromFiles('data/game/');

  Assert(Resources.Count = 1);
  Assert(Resources[0].Name = 'TestCreature');
  Assert(Resources[0].ClassType = TWalkAttackCreatureResource);
  C := Resources[0] as TWalkAttackCreatureResource;
  AssertFloat(C.KnockBackSpeed, 1.2);
  AssertFloat(C.KnockBackDistance, 3.4);
  Assert(C.Flying = true);
  Assert(C.SoundDieTiedToCreature = true);
  AssertFloat(C.DefaultMaxLife, 5.6);
  AssertFloat(C.Radius, 7.8);
  AssertFloat(C.MiddleHeight, 6.7);
  AssertSound(C.SoundSuddenPain, 'test_sound_6');
  AssertSound(C.SoundDie, 'test_sound_7');
  AssertFloat(C.MoveSpeed, 1.2);
  AssertFloat(C.MinLifeLossToHurt, 3.4);
  AssertFloat(C.ChanceToHurt, 0.56);
  AssertFloat(C.MaxHeightAcceptableToFall, 5.6);
  AssertFloat(C.RandomWalkDistance, 7.8);
  Assert(C.RemoveDead = true);
  AssertFloat(C.PreferredDistance, 9.1);
  Assert(C.AlwaysPrepared = true);
  AssertFloat(C.FallSpeed, 1.2);
  AssertFloat(C.GrowSpeed, 3.4);
  Assert(C.ReceiveShadowVolumes = false);
  Assert(C.CastShadowVolumes = false);
  AssertFileName(C.ModelFileName, 'main.x3d');
  Assert(C.IdleAnimation.Defined);
//  AssertFileName(C.IdleAnimation.FileName, 'idle.x3d')); // private
//  Assert(C.IdleAnimation.TimeSensor = 'TimeSensorIdle'); // private
  Assert(C.IdleToWalkAnimation.Defined);
  Assert(C.WalkAnimation.Defined);
  Assert(C.FireMissileAnimation.Defined);
  Assert(C.AttackAnimation.Defined);
  Assert(C.DieAnimation.Defined);
  Assert(C.DieBackAnimation.Defined);
  Assert(C.HurtAnimation.Defined);
  AssertFloat(C.AttackKnockbackDistance, 4.5);
  AssertFloat(C.AttackTime, 7.8);
  AssertFloat(C.AttackMaxDistance, 9.1);
  AssertFloat(C.AttackMaxAngle, 2.3);
  AssertFloat(C.AttackMinDelay, 4.5);
  AssertSound(C.AttackSoundHit, 'test_sound_6');
  AssertSound(C.AttackSoundStart, 'test_sound_7');
  AssertFloat(C.AttackDamageConst, 9.1);
  AssertFloat(C.AttackDamageRandom, 2.3);
  AssertFloat(C.FireMissileTime, 1.2);
  AssertFloat(C.FireMissileMaxDistance, 3.4);
  AssertFloat(C.FireMissileMaxAngle, 5.6);
  AssertFloat(C.FireMissileMinDelay, 7.8);
  AssertSound(C.FireMissileSound, 'test_sound_8');
  Assert(C.FireMissileName = 'TestMissileCreature');
  AssertFloat(C.FireMissileHeight, 0.12);
  AssertFloat(C.FallMinHeightToSound, 7.8);
  AssertSound(C.FallSound, 'test_sound_5');
  AssertFloat(C.FallMinHeightToDamage, 1.2);
  AssertFloat(C.FallDamageScaleMin, 3.4);
  AssertFloat(C.FallDamageScaleMax, 5.6);
  AssertFloat(C.RunAwayLife, 1.2);
  AssertFloat(C.RunAwayDistance, 3.4);
  AssertFloat(C.VisibilityAngle, 5.6);

  Levels.LoadFromFiles('data/game/');

  Assert(Levels[0].Name = 'my_level');
  Assert(Levels[0].LogicClass = TLevelLogic);
  AssertFileName(Levels[0].SceneFileName, 'scene.x3d');
  Assert(Levels[0].Title = 'My Level');
  Assert(Levels[0].Number = 123);
  Assert(Levels[0].Demo = true);
  Assert(Levels[0].TitleHint = 'Title Hint');
  Assert(Levels[0].DefaultPlayed = true);
  Assert(Levels[0].PlaceholderName = PlaceholderNames['blender']);
  Assert(not Levels[0].LoadingImage.IsEmpty);
  Assert(Levels[0].LoadingImage.Width = 16);
  Assert(Levels[0].LoadingImage.Height = 16);
  AssertFloat(Levels[0].LoadingImageBarYPosition, 1.2);
  AssertVector(Levels[0].PlaceholderReferenceDirection, 1, 2, 3);
  AssertSound(Levels[0].MusicSound, 'test_sound_2');

  MaterialProperties.FileName := 'data/game/material_properties.xml';

  Assert(MaterialProperties.Count = 2);

  Assert(MaterialProperties[0].TextureBaseName = 'test_texture');
  AssertSound(MaterialProperties[0].FootstepsSound, 'test_sound_4');
  Assert(MaterialProperties[0].Toxic = true);
  AssertFloat(MaterialProperties[0].ToxicDamageConst, 1.2);
  AssertFloat(MaterialProperties[0].ToxicDamageRandom, 3.4);
  AssertFloat(MaterialProperties[0].ToxicDamageTime, 5.6);
  AssertFileName(MaterialProperties[0].NormalMap, '');

  Assert(MaterialProperties[1].TextureBaseName = 'test_texture_2');
  Assert(MaterialProperties[1].FootstepsSound = stNone);
  Assert(MaterialProperties[1].Toxic = false);
  AssertFloat(MaterialProperties[1].ToxicDamageConst, 0.0);
  AssertFloat(MaterialProperties[1].ToxicDamageRandom, 0.0);
  AssertFloat(MaterialProperties[1].ToxicDamageTime, 0.0);
  AssertFileName(MaterialProperties[1].NormalMap, 'test_normal_map.png');

  Player := TPlayer.Create(nil);
  try
    Player.LoadFromFile('data/game/player.xml');

    AssertFloat(Player.KnockBackSpeed, 1.2);
    AssertFloat(Player.Camera.HeadBobbingTime, 9.1);
    AssertFloat(Player.HeadBobbing, 2.3);
    AssertFloat(Player.SickProjectionSpeed, 4.5);
    AssertFloat(Player.Camera.JumpMaxHeight, 3.4);
    AssertFloat(Player.Camera.JumpHorizontalSpeedMultiply, 5.6);
    AssertFloat(Player.Camera.JumpTime, 7.8);
    AssertFloat(Player.FallMinHeightToSound, 6.7);
    AssertSound(Player.FallSound, 'test_sound_1');
    AssertFloat(Player.FallMinHeightToDamage, 8.9);
    AssertFloat(Player.FallDamageScaleMin, 1.2);
    AssertFloat(Player.FallDamageScaleMax, 3.4);
    AssertFloat(Player.SwimBreath, 5.6);
    AssertFloat(Player.SwimSoundPause, 6.7);
    AssertFloat(Player.DrownPause, 7.8);
    AssertFloat(Player.DrownDamageConst, 9.1);
    AssertFloat(Player.DrownDamageRandom, 2.3);

    RemovePlayer := rtNone;
    Player.Idle(0, RemovePlayer);
    // ignore resulting RemovePlayer

    { some properties are applied to Camera with delay }
    AssertFloat(Player.Camera.HeadBobbing, 2.3);

  finally FreeAndNil(Player) end;
end;

initialization
  RegisterTest(TTestGame);
end.
