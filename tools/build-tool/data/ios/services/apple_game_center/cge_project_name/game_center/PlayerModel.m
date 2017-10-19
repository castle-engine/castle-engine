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

#import "PlayerModel.h"

@implementation PlayerModel

@synthesize storedFilename, storedAchievements;

- (id)init
{
    self = [super init];
    if (self) {
        writeLock = [[NSLock alloc] init];
        NSString* path = [NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES) objectAtIndex:0];
        storedFilename = [[NSString alloc] initWithFormat:@"%@/%@.storedAchievements.plist",[GKLocalPlayer localPlayer].playerID,path];
    }
    return self;
}

// Try to submit all stored achievements to update any achievements that were not successful.
- (void)resubmitStoredAchievements
{
    if (storedAchievements) {
        for (NSString *key in storedAchievements){
            GKAchievement * achievement = [storedAchievements objectForKey:key];
            [storedAchievements removeObjectForKey:key];
            [self submitAchievement:achievement];
        }
        [self writeStoredAchievements];
    }
}

// Load stored achievements and attempt to submit them
- (void)loadStoredAchievements
{
    if (!storedAchievements) {
        NSDictionary *  unarchivedObj = [NSKeyedUnarchiver unarchiveObjectWithFile:storedFilename];;

        if (unarchivedObj) {
            storedAchievements = [[NSMutableDictionary alloc] initWithDictionary:unarchivedObj];
            [self resubmitStoredAchievements];
        } else {
            storedAchievements = [[NSMutableDictionary alloc] init];
        }
    }
}

// store achievements to disk to submit at a later time.
- (void)writeStoredAchievements
{
    [writeLock lock];
    NSData * archivedAchievements = [NSKeyedArchiver archivedDataWithRootObject:storedAchievements];
    NSError * error;
    [archivedAchievements writeToFile:storedFilename options:NSDataWritingFileProtectionNone error:&error];
    if (error) {
        //  Error saving file, handle accordingly
    }
    [writeLock unlock];
}

// Submit an achievement to the server and store if submission fails
- (void)submitAchievement:(GKAchievement *)achievement
{
    if (achievement) {
        // Submit the achievement.
        [GKAchievement reportAchievements:@[achievement] withCompletionHandler:^(NSError *error)
        {
            if (error) {
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

// Create an entry for an achievement that hasn't been submitted to the server
- (void)storeAchievement:(GKAchievement *)achievement
{
    GKAchievement * currentStorage = [storedAchievements objectForKey:achievement.identifier];
    if (!currentStorage || (currentStorage && currentStorage.percentComplete < achievement.percentComplete)) {
        [storedAchievements setObject:achievement forKey:achievement.identifier];
        [self writeStoredAchievements];
    }
}

// Reset all the achievements for local player
- (void)resetAchievements
{
     [GKAchievement resetAchievementsWithCompletionHandler: ^(NSError *error)
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
