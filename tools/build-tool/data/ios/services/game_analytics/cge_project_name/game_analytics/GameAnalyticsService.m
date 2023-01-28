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

/* Game Analytics https://gameanalytics.com/ integration with Castle Game Engine.

   See https://castle-engine.io/
   and https://castle-engine.io/ios-Services
   for information about Castle Game Engine and services on iOS.

   See https://gameanalytics.com/docs/ios-sdk
   for Game Analytics docs on iOS.
*/

#import "GameAnalyticsService.h"
#import "../AvailableProduct.h"
#import "../CgeUtils.h"

#import <GameAnalytics/GameAnalytics.h>

@implementation GameAnalyticsService

- (void)initialize:(NSString*) gameKey secretKey:(NSString*) secretKey
{
    NSDictionary *infoDictionary = [[NSBundle mainBundle] infoDictionary];
    NSString *versionNumber = [infoDictionary objectForKey:@"CFBundleShortVersionString"];
    NSString *buildNumber = [infoDictionary objectForKey:@"CFBundleVersion"];
    NSString *version = [NSString stringWithFormat:@"iOS %@ (%@)", versionNumber, buildNumber];

    [GameAnalytics configureBuild: version];

    [GameAnalytics initializeWithGameKey: gameKey gameSecret: secretKey];

    bool debug = FALSE;
    if (debug) {
        [GameAnalytics setEnabledInfoLog: YES];
        [GameAnalytics setEnabledVerboseLog: YES];
    }

    initialized = TRUE;
}

- (void)sendScreenView:(NSString*) screenName
{
    if (!initialized) {
        return;
    }
    NSString* eventId = [NSString stringWithFormat:@"screenView:%@", screenName];
    [GameAnalytics addDesignEventWithEventId: eventId];
}

- (void)sendEvent:(NSString*) category
    action:(NSString*) action
    label:(NSString*) label
    value:(long) value
    dimensionIndex:(int) dimensionIndex
    dimensionValue:(NSString*) dimensionValue
{
    if (!initialized) {
        return;
    }
    NSString* eventId = [NSString stringWithFormat: @"%@:%@:%@", category, action, label];
    if (dimensionIndex > 0 && ![dimensionValue isEqualToString:@""]) {
        eventId = [NSString stringWithFormat: @"%@:dimension%@.%@",
            eventId, [NSNumber numberWithInt: dimensionIndex], dimensionValue];
    }
    [GameAnalytics addDesignEventWithEventId: eventId
        value: [NSNumber numberWithLong: value]];
}

- (void)sendTiming:(NSString*) category
    variable:(NSString*) variable
    label:(NSString*) label
    timeMiliseconds:(long) timeMiliseconds
{
    if (!initialized) {
        return;
    }
    NSString* eventId = [NSString stringWithFormat: @"timing:%@:%@:%@", category, variable, label];
    [GameAnalytics addDesignEventWithEventId: eventId
        value: [NSNumber numberWithLong: timeMiliseconds]];
}

- (void)sendProgress:(int) status
    world:(NSString*) world
    level:(NSString*) level
    phase:(NSString*) phase
    score:(int) score
{
    if (!initialized) {
        return;
    }
    GAProgressionStatus gaStatus;
    switch (status) {
        case 0: gaStatus = GAProgressionStatusStart; break;
        case 1: gaStatus = GAProgressionStatusFail; break;
        case 2: gaStatus = GAProgressionStatusComplete; break;
        default:
            NSLog(@"WARNING: Invalid analytics-send-progress status %@",
                [NSNumber numberWithInt: status]);
            return;
    }
    // level and phase may be nil, not "", other Game Analytics logs:
    // Info/GA/Analytics: Validation fail - progression event - progression02: Cannot be empty or above 64 characters. String:
    if ([level isEqualToString: @""]) {
        level = nil;
    }
    if ([phase isEqualToString: @""]) {
        phase = nil;
    }
    [GameAnalytics addProgressionEventWithProgressionStatus: gaStatus
        progression01: world progression02: level progression03: phase score: score];
}

- (void)onPurchase:(AvailableProduct*) product
  withTransaction:(SKPaymentTransaction*) transaction
{
    if (!initialized) {
        return;
    }

    // for GameAnalytics, currency must be valid
    NSString* currency = product.priceCurrencyCode;
    if (currency == nil || [currency isEqualToString:@""]) {
        currency = @"USD";
    }

    // for GameAnalytics, category must be valid:
    // Cannot be (null), empty or above 64 characters
    NSString* category = product.category;
    if (category == nil || [category isEqualToString:@""]) {
        category = @"defaultProductCategory";
    }
    if ([category length] > 64) {
        category = [category substringToIndex: 64];
    }

    /* Note: using autoFetchReceipt, works only on iOS >= 7,
       on older iOS this purchase will not be logged by GameAnalytics. */
    [GameAnalytics addBusinessEventWithCurrency: currency amount: product.priceAmountCents
        itemType: category itemId: product.id cartType: @"defaultCart" autoFetchReceipt: YES];
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
