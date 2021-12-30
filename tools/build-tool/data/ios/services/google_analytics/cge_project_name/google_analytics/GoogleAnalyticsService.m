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

/* Google Analytics https://analytics.google.com/ integration with Castle Game Engine.

   See https://castle-engine.io/
   and https://castle-engine.io/ios-Services
   for information about Castle Game Engine and services on iOS.

   See https://developers.google.com/analytics/devguides/collection/ios/v3/?ver=objc
   for Google Analytics docs on iOS.
*/

#import "GoogleAnalyticsService.h"
#import "../CgeUtils.h"

// Contrary to docs, this import doesn't work, see comments in Podfile
//#import <Google/Analytics.h>

#import <GoogleAnalytics/GAI.h>
#import <GoogleAnalytics/GAIFields.h>
#import <GoogleAnalytics/GAIDictionaryBuilder.h>
#import <GoogleAnalytics/GAIEcommerceProduct.h>
#import <GoogleAnalytics/GAIEcommerceFields.h>
#import <GoogleAnalytics/GAIEcommerceProductAction.h>

@implementation GoogleAnalyticsService

- (void)initialize:(NSString*) analyticsPropertyId
{
    GAI *gai = [GAI sharedInstance];
    [gai trackerWithTrackingId: analyticsPropertyId];

    // Optional: automatically report uncaught exceptions.
    gai.trackUncaughtExceptions = YES;

    // Optional: set Logger to VERBOSE for debug information.
    // Remove before app release.
    bool debug = FALSE;
    if (debug) {
        gai.logger.logLevel = kGAILogLevelVerbose;
    }

    initialized = TRUE;
}

- (void)sendScreenView:(NSString*) screenName
{
    if (!initialized) {
        return;
    }
    id<GAITracker> tracker = [GAI sharedInstance].defaultTracker;
    [tracker set: kGAIScreenName value: screenName];
    [tracker send: [[GAIDictionaryBuilder createScreenView] build]];
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
    id<GAITracker> tracker = [GAI sharedInstance].defaultTracker;
    GAIDictionaryBuilder* builder = [GAIDictionaryBuilder
        createEventWithCategory: category
        action: action
        label: label
        value: [NSNumber numberWithLong: value]];
    // This method of setting custom dimention (only for this event)
    // is documented in .../Pods/GoogleAnalytics/Sources/GAIDictionaryBuilder.h
    if (dimensionIndex > 0 && ![dimensionValue isEqualToString: @""]) {
        [builder set: dimensionValue forKey: [GAIFields customDimensionForIndex: dimensionIndex]];
    }
    [tracker send:[builder build]];
}

- (void)sendTiming:(NSString*) category
    variable:(NSString*) variable
    label:(NSString*) label
    timeMiliseconds:(long) timeMiliseconds
{
    if (!initialized) {
        return;
    }
    id<GAITracker> tracker = [GAI sharedInstance].defaultTracker;
    [tracker send:[[GAIDictionaryBuilder
        createTimingWithCategory: category
        interval: [NSNumber numberWithLong: timeMiliseconds]
        name: variable
        label: label] build]];
}

- (void)sendProgress:(int) status
    world:(NSString*) world
    level:(NSString*) level
    phase:(NSString*) phase
    score:(int) score
{
    NSString* strStatus;
    switch (status) {
        case 0: strStatus = @"start"; break;
        case 1: strStatus = @"fail"; break;
        case 2: strStatus = @"complete"; break;
        default:
            NSLog(@"WARNING: Invalid analytics-send-progress status %@",
                [NSNumber numberWithInt: status]);
            return;
    }
    [self sendEvent: @"progress"
        action: strStatus
        label: [NSString stringWithFormat: @"%@-%@-%@", world, level, phase]
        value: score
        dimensionIndex: 0
        dimensionValue: @""];
}

- (void)onPurchase:(AvailableProduct*) availableProduct
  withTransaction:(SKPaymentTransaction*) transaction
{
    if (!initialized) {
        return;
    }
    id<GAITracker> tracker = [GAI sharedInstance].defaultTracker;

    /* Note that we don't use the API documented on
       https://developers.google.com/analytics/devguides/collection/ios/v3/ecommerce
       with createTransactionWithId, createItemWithTransactionId.

       Instead we use the API with setProductAction.
       It is described on
       https://developers.google.com/analytics/devguides/collection/ios/v3/enhanced-ecommerce
       The documentation there suggests you need to use some special things,
       but actually the necessary headers are already included in latest
       GoogleAnalytics CocoaPod, and nothing more is needed.

       And this makes our reporting consistent with what Android reporting does.
       Which may be crucial if you use single Google Analytics property for both
       Android and iOS.
    */

    GAIEcommerceProduct *product = [[GAIEcommerceProduct alloc] init];
    [product setId: availableProduct.id];
    [product setCategory: availableProduct.category];
    [product setPrice: [NSNumber numberWithFloat: (availableProduct.priceAmountCents / 100.0)]];

    GAIEcommerceProductAction *productAction = [[GAIEcommerceProductAction alloc] init];
    [productAction setAction: kGAIPAPurchase];

    GAIDictionaryBuilder* builder = [GAIDictionaryBuilder
        createEventWithCategory: @"defaultCart"
        action: @"purchase"
        label: nil
        value: nil];
    [builder setProductAction: productAction];
    [builder addProduct: product];

    [tracker set:kGAICurrencyCode value: availableProduct.priceCurrencyCode];

    // Send the transaction data with the event.
    [tracker send:[builder build]];
}

/* Handle messages received from Pascal code
   (send from src/services/castleanalytics.pas).
*/
- (bool)messageReceived:(NSArray*) message
{
    if (message.count == 2 &&
        [[message objectAtIndex: 0] isEqualToString:@"google-analytics-initialize"])
    {
        [self initialize: [message objectAtIndex: 1]];
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
