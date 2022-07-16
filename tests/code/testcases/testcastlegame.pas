// -*- compile-command: "./test_single_testcase.sh TTestGame" -*-
{
  Copyright 2004-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestCastleGame;

interface

uses
  Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry,
  CastleTestCase{$else}CastleTester{$endif};

type
  TTestGame = class(TCastleTestCase)
    procedure TestGameData;
  end;

implementation

uses CastleVectors, CastleLevels, CastleResources, CastleSoundEngine, CastlePlayer,
  CastleMaterialProperties, CastleCreatures, CastleShapes, CastleTransform,
  CastleURIUtils;

procedure TTestGame.TestGameData;

  procedure AssertFloat(const A, B: Single);
  begin
    AssertSameValue(A, B, 0.01);
  end;

  procedure AssertURL(const A, B: string);
  begin
    { When reading XML files, we make URLs absolute. For comparison,
      strip directory part. }
    AssertEquals(ExtractURIName(A), B);
  end;

  procedure AssertSound(const A: TCastleSound; const B: string);
  begin
    AssertTrue(A = SoundEngine.SoundFromName(B));
  end;

  procedure AssertVector(const A: TVector3; const BX, BY, BZ: Single);
  begin
    AssertVectorEquals(A, Vector3(BX, BY, BZ), 0.01);
  end;

var
  Player: TPlayer;
  RemovePlayer: TRemoveType;
  //SoundType: TCastleSound;
  C: TWalkAttackCreatureResource;
  MatProp: TMaterialProperty;
begin
  SoundEngine.RepositoryURL := 'castle-data:/game/sounds.xml';

  Resources.LoadFromFiles('castle-data:/game/');

  AssertTrue(Resources.Count = 1);
  AssertTrue(Resources[0].Name = 'TestCreature');
  AssertTrue(Resources[0].ClassType = TWalkAttackCreatureResource);
  C := Resources[0] as TWalkAttackCreatureResource;
  AssertFloat(C.KnockBackSpeed, 1.2);
  AssertFloat(C.KnockBackDistance, 3.4);
  AssertTrue(C.Flying = true);
  AssertTrue(C.SoundDieTiedToCreature = true);
  AssertFloat(C.DefaultMaxLife, 5.6);
  AssertFloat(C.RadiusOverride, 7.8);
  AssertFloat(C.MiddleHeight, 6.7);
  AssertSound(C.SoundSuddenPain, 'test_sound_6');
  AssertSound(C.SoundDie, 'test_sound_7');
  AssertFloat(C.MoveSpeed, 1.2);
  AssertFloat(C.MinLifeLossToHurt, 3.4);
  AssertFloat(C.ChanceToHurt, 0.56);
  AssertFloat(C.MaxHeightAcceptableToFall, 5.6);
  AssertFloat(C.RandomWalkDistance, 7.8);
  AssertTrue(C.RemoveDead = true);
  AssertFloat(C.PreferredDistance, 9.1);
  AssertTrue(C.AlwaysPrepared = true);
  { change AlwaysPrepared to false,
    to avoid crashing tests that actually do LoadLevel,
    since 3D models referenced by this resource do not exist. }
  C.ConfigAlwaysPrepared := false;
  AssertFloat(C.FallSpeed, 1.2);
  AssertFloat(C.GrowSpeed, 3.4);
  AssertTrue(C.ReceiveShadowVolumes = false);
  AssertTrue(C.CastShadowVolumes = false);
  AssertURL(C.ModelURL, 'main.x3d');
  AssertTrue(C.IdleAnimation.Defined);
//  AssertURL(C.IdleAnimation.URL, 'idle.x3d')); // private
//  AssertTrue(C.IdleAnimation.TimeSensor = 'TimeSensorIdle'); // private
  AssertTrue(C.IdleToWalkAnimation.Defined);
  AssertTrue(C.WalkAnimation.Defined);
  AssertTrue(C.FireMissileAnimation.Defined);
  AssertTrue(C.AttackAnimation.Defined);
  AssertTrue(C.DieAnimation.Defined);
  AssertTrue(C.DieBackAnimation.Defined);
  AssertTrue(C.HurtAnimation.Defined);
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
  AssertTrue(C.FireMissileName = 'TestMissileCreature');
  AssertFloat(C.FireMissileHeight, 0.12);
  AssertFloat(C.FallMinHeightToSound, 7.8);
  AssertSound(C.FallSound, 'test_sound_5');
  AssertFloat(C.FallMinHeightToDamage, 1.2);
  AssertFloat(C.FallDamageScaleMin, 3.4);
  AssertFloat(C.FallDamageScaleMax, 5.6);
  AssertFloat(C.RunAwayLife, 1.2);
  AssertFloat(C.RunAwayDistance, 3.4);
  AssertFloat(C.VisibilityAngle, 5.6);

  Levels.LoadFromFiles('castle-data:/game/');

  AssertTrue(Levels[0].Name = 'my_level');
  AssertTrue(Levels[0].LogicClass = TLevelLogic);
  AssertURL(Levels[0].SceneURL, 'scene.x3d');
  AssertTrue(Levels[0].Title = 'My Level');
  AssertTrue(Levels[0].Number = 123);
  AssertTrue(Levels[0].Demo = true);
  AssertTrue(Levels[0].TitleHint = 'Title Hint');
  AssertTrue(Levels[0].DefaultPlayed = true);
  AssertTrue({$ifndef FPC}@{$endif}Levels[0].PlaceholderName = {$ifndef FPC}@{$endif}PlaceholderNames['blender']);
  AssertTrue(not Levels[0].LoadingImage.IsEmpty);
  AssertTrue(Levels[0].LoadingImage.Width = 16);
  AssertTrue(Levels[0].LoadingImage.Height = 16);
  AssertFloat(Levels[0].LoadingBarYPosition, 1.2);
  AssertVector(Levels[0].PlaceholderReferenceDirection, 1, 2, 3);
  AssertSound(Levels[0].MusicSound, 'test_sound_2');

  MaterialProperties.URL := 'castle-data:/game/material_properties.xml';

  // not exposed anymore
  //AssertTrue(MaterialProperties.Count = 2);

  MatProp := MaterialProperties.FindTextureBaseName('test_texture');
  AssertSound(MatProp.FootstepsSound, 'test_sound_4');
  AssertTrue(MatProp.Toxic = true);
  AssertFloat(MatProp.ToxicDamageConst, 1.2);
  AssertFloat(MatProp.ToxicDamageRandom, 3.4);
  AssertFloat(MatProp.ToxicDamageTime, 5.6);
  AssertURL(MatProp.NormalMap, '');

  MatProp := MaterialProperties.FindTextureBaseName('test_texture_2');
  AssertTrue(MatProp.TextureBaseName = 'test_texture_2');
  AssertTrue(MatProp.FootstepsSound = nil);
  AssertTrue(MatProp.Toxic = false);
  AssertFloat(MatProp.ToxicDamageConst, 0.0);
  AssertFloat(MatProp.ToxicDamageRandom, 0.0);
  AssertFloat(MatProp.ToxicDamageTime, 0.0);
  AssertURL(MatProp.NormalMap, 'test_normal_map.png');

  Player := TPlayer.Create(nil);
  try
    Player.LoadFromFile('castle-data:/game/player.xml');

    AssertFloat(Player.KnockBackSpeed, 1.2);
    AssertFloat(Player.WalkNavigation.HeadBobbingTime, 9.1);
    AssertFloat(Player.HeadBobbing, 2.3);
    AssertFloat(Player.SickProjectionSpeed, 4.5);
    AssertFloat(Player.WalkNavigation.JumpMaxHeight, 3.4);
    AssertFloat(Player.WalkNavigation.JumpHorizontalSpeedMultiply, 5.6);
    AssertFloat(Player.WalkNavigation.JumpTime, 7.8);
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
    Player.Update(0, RemovePlayer);
    // ignore resulting RemovePlayer

    { some properties are applied to Camera with delay }
    AssertFloat(Player.WalkNavigation.HeadBobbing, 2.3);

  finally FreeAndNil(Player) end;
end;

initialization
  RegisterTest(TTestGame);
end.
