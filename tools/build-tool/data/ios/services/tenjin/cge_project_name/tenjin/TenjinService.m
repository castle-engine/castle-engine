/*
  Copyright 2021-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in the "Castle Game Engine" distribution,
  for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
*/

/* Tenjin SDK https://github.com/tenjin/tenjin-ios-sdk
   integration with Castle Game Engine https://castle-engine.io/ .
*/

#import "TenjinService.h"
#import "../AvailableProduct.h"

#import "TenjinSDK.h"

#import <AppTrackingTransparency/AppTrackingTransparency.h>

@implementation TenjinService

- (void)application:(UIApplication *) application
    didFinishLaunchingWithOptions:(NSDictionary *) launchOptions
{
    [TenjinSDK initialize:@"${IOS.TENJIN.API_KEY}"];

    connected = FALSE;

    if (@available(iOS 14, *)) {
        /* initialize Tenjin using iOS >= 14 prompt that allows to cancel tracking,
           https://docs.tenjin.com/en/send-events/ios.html */
        [ATTrackingManager requestTrackingAuthorizationWithCompletionHandler:^(ATTrackingManagerAuthorizationStatus status) {
            [TenjinSDK connect];
            connected = TRUE;
        }];
    } else {
        [TenjinSDK connect];
        connected = TRUE;
    }
}

- (bool)messageReceived:(NSArray* )message
{
    if (message.count == 2 &&
        [[message objectAtIndex: 0] isEqualToString:@"tenjin-send-event"])
    {
        if (connected) {
            [TenjinSDK sendEventWithName: [message objectAtIndex: 1]];
        }
        return TRUE;
    }

    return FALSE;
}

- (void)onPurchase:(AvailableProduct*) product
  withTransaction:(SKPaymentTransaction*) transaction
{
    if (!connected) {
        return;
    }

    // Get the NSData receipt
    NSURL *receiptURL = [[NSBundle mainBundle] appStoreReceiptURL];
    NSData *receiptData = [NSData dataWithContentsOfURL:receiptURL];

    // Pass the transaction and the receiptData to Tenjin
    [TenjinSDK transaction: transaction andReceipt: receiptData];
}

@end
