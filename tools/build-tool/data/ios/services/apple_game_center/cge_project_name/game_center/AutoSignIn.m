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

#import "AutoSignIn.h"
#import "../CgeUtils.h"

@implementation AutoSignIn

- (id)init
{
    self = [super init];

    if (self) {
        writeLock = [[NSLock alloc] init];
        NSString* path = [NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES) objectAtIndex:0];
        saveFilename = [[NSString alloc] initWithFormat:@"%@/gameCenterAutoSignIn.plist", path];

        [self loadFromFile];
    }

    return self;
}

- (bool)isAutoSignIn
{
    return m_isAutoSignIn;
}

- (void)setAutoSignIn:(bool) value
{
    if (m_isAutoSignIn != value) {
        m_isAutoSignIn = value;
        [self saveToFile];
    }
}

- (void) loadFromFile
{
    m_isAutoSignIn = false;

    NSFileManager* fileManager = [NSFileManager defaultManager];
    if ([fileManager fileExistsAtPath:saveFilename]) {
        NSMutableDictionary* saveData;

        NSDictionary* unarchivedObj = [NSKeyedUnarchiver unarchiveObjectWithFile:saveFilename];
        if (unarchivedObj) {
            saveData = [[NSMutableDictionary alloc] initWithDictionary:unarchivedObj];
            NSNumber* autoSignInValue = [saveData objectForKey:@"autoSignIn"];
            m_isAutoSignIn = [autoSignInValue boolValue];
        }
    }

    if (m_isAutoSignIn) {
        NSLog(@"Will auto sign-in to Apple Game Center");
    }
}

- (void) saveToFile
{
    [writeLock lock];

    NSLog(@"Saving auto sign-in to Apple Game Center: %@ to file \"%@\"",
        [NSNumber numberWithBool:m_isAutoSignIn], saveFilename);

    NSMutableDictionary* saveData = [[NSMutableDictionary alloc] init];
    NSNumber* value = [NSNumber numberWithBool:m_isAutoSignIn];
    [saveData setObject:value forKey:@"autoSignIn"];

    NSData* archivedData = [NSKeyedArchiver archivedDataWithRootObject:saveData];
    NSError* error;
    [archivedData writeToFile:saveFilename options:NSDataWritingFileProtectionNone error:&error];
    if (error) {
        NSLog(@"Error when saving \"%@\" because: %@",
            saveFilename, error.localizedDescription);
    }

    [writeLock unlock];
}

@end
