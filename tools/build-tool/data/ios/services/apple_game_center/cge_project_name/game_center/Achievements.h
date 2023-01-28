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
  Submit achievements and store them when network connection
  is not available.

  Based on
  https://developer.apple.com/library/content/samplecode/GKAchievements/Introduction/Intro.html
  that is copyright (C) 2011 Apple Inc. All Rights Reserved.
  The license is suitable to insert it into our LGPL engine.
*/

#import <GameKit/GameKit.h>

@interface Achievements : NSObject
{
    NSLock *writeLock;
    NSString *storedFilename;
    NSMutableDictionary *storedAchievements;
}

// Load stored (unsubmitted) achievements from disk and attempt to submit them.
- (void)loadStoredAchievements;

// Submit an achievement to the server (or store if submission fails).
- (void)submitAchievement:(NSString*) achievementId;

@end
