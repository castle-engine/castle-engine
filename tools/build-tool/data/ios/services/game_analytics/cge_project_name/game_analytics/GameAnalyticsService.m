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

/* Game Analytics https://gameanalytics.com/ integration with Castle Game Engine. */

#import "GameAnalyticsService.h"

@implementation GameAnalyticsService

- (void)initialize:(NSString*) gameKey secretKey:(NSString*) secretKey
{
    // TODO
}

- (void)sendScreenView:(NSString*) screenName
{
    // TODO
}

- (void)sendEvent:(NSString*) category
    action:(NSString*) action
    label:(NSString*) label
    value:(long) value
    dimensionIndex:(int) dimensionIndex
    dimensionValue:(NSString*) dimensionValue
{
    // TODO
}

- (void)sendTiming:(NSString*) category
    variable:(NSString*) variable
    label:(NSString*) label
    timeMiliseconds:(long) timeMiliseconds
{
    // TODO
}

- (void)sendProgress:(int) status
    world:(NSString*) world
    level:(NSString*) level
    phase:(NSString*) phase
    score:(int) score
{
    // TODO
}

/* Handle messages received from Pascal code
   (send from src/services/castleanalytics.pas).
*/
- (bool)messageReceived:(NSArray*) message
{
    if (message.count == 3 &&
        [[message objectAtIndex: 0] isEqualToString:@"game-analytics-initialize"])
    {
        [self initialize: [message objectAtIndex: 1]
              secretKey: [message objectAtIndex: 2]];
        return TRUE;
    } else
    if (message.count == 2 &&
        [[message objectAtIndex: 0] isEqualToString:@"analytics-send-screen-view"])
    {
        [self sendScreenView: [message objectAtIndex: 1]];
        return TRUE;
    } else
    if (message.count == 7 &&
        [[message objectAtIndex: 0] isEqualToString:@"analytics-send-event"])
    {
        [self sendEvent: [message objectAtIndex: 1]
              action: [message objectAtIndex: 2]
              label: [message objectAtIndex: 3]
              value: [[message objectAtIndex: 4] longLongValue]
              dimensionIndex: [[message objectAtIndex: 5] intValue]
              dimensionValue: [message objectAtIndex: 6]];
        return TRUE;
    } else
    if (message.count == 5 &&
        [[message objectAtIndex: 0] isEqualToString:@"analytics-send-timing"])
    {
        [self sendTiming: [message objectAtIndex: 1]
              variable: [message objectAtIndex: 2]
              label: [message objectAtIndex: 3]
              timeMiliseconds: [[message objectAtIndex: 4] longLongValue]];
        return TRUE;
    } else
    if (message.count == 6 &&
        [[message objectAtIndex: 0] isEqualToString:@"analytics-send-progress"])
    {
        [self sendProgress: [[message objectAtIndex: 1] intValue]
              world:  [message objectAtIndex: 2]
              level: [message objectAtIndex: 3]
              phase: [message objectAtIndex: 4]
              score: [[message objectAtIndex: 5] intValue]];
        return TRUE;
    }

    return FALSE;
}

@end
