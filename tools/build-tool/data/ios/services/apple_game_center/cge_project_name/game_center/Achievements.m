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
  Submit achievements and store them when network connection is not available.

  Based on
  https://developer.apple.com/library/content/samplecode/GKAchievements/Introduction/Intro.html
  that is copyright (C) 2011 Apple Inc. All Rights Reserved.
  The license is suitable to insert it into our LGPL engine.
*/

#import "Achievements.h"
#import "../CgeUtils.h"

@implementation Achievements

- (id)init
{
    self = [super init];
    if (self) {
        writeLock = [[NSLock alloc] init];
        NSString* path = [NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES) objectAtIndex:0];
        storedFilename = [[NSString alloc] initWithFormat:@"%@/%@.storedAchievements.plist",path,[GKLocalPlayer localPlayer].playerID];
    }

    // testing:
    // [self resetAchievements];

    return self;
}

// Resubmit any local instances of GKAchievement that was stored on a failed submission.
// Try to submit all stored achievements to update any achievements that were not successful.
// Writes achievements to disk when changed.
- (void)resubmitStoredAchievements
{
    if (storedAchievements && storedAchievements.count > 0) {
        NSLog(@"INFO: %lu achievements pending to be submitted to server, submitting.",
            (unsigned long)storedAchievements.count);
        for (NSString* key in storedAchievements) {
            GKAchievement* achievement = [storedAchievements objectForKey:key];
            [storedAchievements removeObjectForKey:key];
            [self submitAchievementObject:achievement];
        }
        [self writeStoredAchievements];
    }
}

- (void)loadStoredAchievements
{
    if (!storedAchievements) {
        NSDictionary* unarchivedObj = [NSKeyedUnarchiver unarchiveObjectWithFile:storedFilename];

        if (unarchivedObj) {
            storedAchievements = [[NSMutableDictionary alloc] initWithDictionary:unarchivedObj];
            [self resubmitStoredAchievements];
        } else {
            storedAchievements = [[NSMutableDictionary alloc] init];
        }
    }
}

// Store achievements to disk to submit at a later time.
- (void)writeStoredAchievements
{
    [writeLock lock];
    NSData* archivedAchievements = [NSKeyedArchiver archivedDataWithRootObject:storedAchievements];
    NSError* error;
    [archivedAchievements writeToFile:storedFilename options:NSDataWritingFileProtectionNone error:&error];
    if (error) {
        NSLog(@"Error when saving stored achievements to file \"%@\" because: %@",
            storedFilename, error.localizedDescription);
    }
    [writeLock unlock];
}

- (void)submitAchievement:(NSString*) achievementId
{
    // Old Apple example incorrectly suggests that achievement id
    // should be prefixed by bundle id
    //    NSString* bundleIdentifier = [[NSBundle mainBundle] bundleIdentifier];
    //    NSString* identifier = [[bundleIdentifier stringByAppendingString:@"."]
    //        stringByAppendingString:achievementId];

    NSLog(@"Submitting achievement %@", achievementId);

    GKAchievement* achievement = [[GKAchievement alloc] initWithIdentifier:achievementId];
    achievement.showsCompletionBanner = YES;
    [achievement setPercentComplete: 100];
    [self submitAchievementObject:achievement];
}

// Submit an achievement (as GKAchievement) to the server (or store if submission fails).
- (void)submitAchievementObject:(GKAchievement*) achievement
{
    if (achievement) {
        // Submit the achievement.
        [GKAchievement reportAchievements:@[achievement] withCompletionHandler:^(NSError* error)
        {
            if (error) {
                NSLog(@"Failed to submit achievement to server, will submit it later.");
                // Store achievement to be submitted at a later time.
                [self storeAchievement:achievement];
            } else {
                if ([storedAchievements objectForKey:achievement.identifier]) {
                    // Achievement is reported, remove from store.
                    [storedAchievements removeObjectForKey:achievement.identifier];
                }
                [self resubmitStoredAchievements];
            }
        }];
    }
}

// Store an achievement for future resubmit.
// Use for an achievement that hasn't been submitted to the server.
// This writes achievements to disk when changed.
- (void)storeAchievement:(GKAchievement*) achievement
{
    GKAchievement* currentStorage = [storedAchievements objectForKey:achievement.identifier];
    if (!currentStorage || (currentStorage && currentStorage.percentComplete < achievement.percentComplete)) {
        [storedAchievements setObject:achievement forKey:achievement.identifier];
        [self writeStoredAchievements];
    }
}

// Reset all the achievements for current player.
// Useful for testing.
- (void)resetAchievements
{
    [GKAchievement resetAchievementsWithCompletionHandler: ^(NSError* error)
    {
        if (!error) {
            // this will release previous storedAchievements
            storedAchievements = [[NSMutableDictionary alloc] init];

            // overwrite any previously stored file
            [self writeStoredAchievements];
        } else {
            // Error clearing achievements.
        }
    }];
}

@end
