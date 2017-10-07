/*
  Copyright 2017-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in the "Castle Game Engine" distribution,
  for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
*/

/*
  Abstract: Provide an example of how to successfully submit achievements and store them when network connection is not available

  Based on
  https://developer.apple.com/library/content/samplecode/GKAchievements/Introduction/Intro.html
  that is copyright (C) 2011 Apple Inc. All Rights Reserved.
  The license is suitable to insert it into our LGPL engine.
*/

#import <GameKit/GameKit.h>

@interface PlayerModel : NSObject
{
    NSLock *writeLock;
}

@property (readonly, nonatomic) NSString *storedFilename;
@property (readonly, nonatomic) NSMutableDictionary *storedAchievements;

// resubmit any local instances of GKAchievement that was stored on a failed submission.
- (void)resubmitStoredAchievements;

// write all stored achievements for future resubmission
- (void)writeStoredAchievements;

// load stored achievements that haven't been submitted to the server
- (void)loadStoredAchievements;

// store an achievement for future resubmit
- (void)storeAchievement:(GKAchievement *)achievement ;

// submit an achievement
- (void)submitAchievement:(GKAchievement *)achievement ;

// reset achievements
- (void)resetAchievements;


@end
