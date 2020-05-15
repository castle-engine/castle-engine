/*
  Copyright 2020-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in the "Castle Game Engine" distribution,
  for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
*/

/* Activity recognition for Castle Game Engine https://castle-engine.io/ on iOS.
*/

#import "ActivityRecognitionService.h"
#import "../CgeUtils.h"

@implementation ActivityRecognitionService

- (void) startListening
{
    if (![CMMotionActivityManager isActivityAvailable]) {
        NSLog(@"Motion activity not available on this device.");
        return;
    }

    if (@available(iOS 11.0, *)) {
        if ([CMMotionActivityManager authorizationStatus] != CMAuthorizationStatusAuthorized) {
            NSLog(@"Not authorized to read motion activity. Trying to read anyway (will ask for permission)...");
        }
    }

    if (motionActivityManager == NULL) {
        motionActivityManager = [[CMMotionActivityManager alloc] init];
    }
    [motionActivityManager startActivityUpdatesToQueue:[NSOperationQueue mainQueue] withHandler:^(CMMotionActivity * _Nullable activity) {
        [self processUpdate:activity];
    }];
}

- (void) processUpdate: (CMMotionActivity*) activity
{
    NSMutableArray* possibleActivitiesNames = [[NSMutableArray alloc] init];
    if (activity.stationary) {
        [possibleActivitiesNames addObject:@"stationary"];
    }
    if (activity.walking) {
        [possibleActivitiesNames addObject:@"walking"];
    }
    if (activity.running) {
        [possibleActivitiesNames addObject:@"running"];
    }
    if (activity.automotive) {
        [possibleActivitiesNames addObject:@"automotive"];
    }
    if (activity.cycling) {
        [possibleActivitiesNames addObject:@"cycling"];
    }
    if (activity.unknown) {
        [possibleActivitiesNames addObject:@"unknown"];
    }

    if ([possibleActivitiesNames count] == 0) {
        /* iOS often updates us when no activity is detected, and it is rather useless,
           it practically means that previously detected activity was OK. */
        return;
    }

    NSString* possibleActivitiesStr = [possibleActivitiesNames componentsJoinedByString:@"\2"];

    NSString* confidenceName;
    switch (activity.confidence) {
        case CMMotionActivityConfidenceLow: confidenceName = @"low"; break;
        case CMMotionActivityConfidenceMedium: confidenceName = @"medium"; break;
        case CMMotionActivityConfidenceHigh: confidenceName = @"high"; break;
        default: NSLog(@"Unrecognized activity confidence"); break;
    }

    [self messageSend:@[
        @"activity-recognition-change",
        possibleActivitiesStr,
        confidenceName
    ]];
}

- (void) stopListening
{
    if (motionActivityManager != NULL) {
        [motionActivityManager stopActivityUpdates];
    }

}

- (bool)messageReceived:(NSArray* )message
{
    if (message.count == 2 &&
        [[message objectAtIndex: 0] isEqualToString:@"activity-recognition"] &&
        [[message objectAtIndex: 1] isEqualToString:@"start"])
    {
        [self startListening];
        return TRUE;
    } else
    if (message.count == 2 &&
        [[message objectAtIndex: 0] isEqualToString:@"activity-recognition"] &&
        [[message objectAtIndex: 1] isEqualToString:@"stop"])
    {
        [self stopListening];
        return TRUE;
    }

    return FALSE;
}

@end
