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

#import "ServiceAbstract.h"
#include "castleiosappglue.h"
#import "CgeUtils.h"

@implementation ServiceAbstract

- (void)applicationDidEnterBackground
{
}

- (void)application:(UIApplication *) application didFinishLaunchingWithOptions:(NSDictionary *) launchOptions
{
}

- (void)applicationDidBecomeActive:(UIApplication *)application
{
}

- (BOOL)application:(UIApplication *)app
    openURL:(NSURL *)url
    options:(NSDictionary<UIApplicationOpenURLOptionsKey, id> *)options
{
    return NO;
}

- (bool)messageReceived:(NSArray* )message
{
    return FALSE;
}

- (bool)stringToBool:(NSString* )value
{
    if ([value isEqualToString:@"true"]) {
        return TRUE;
    } else
    if ([value isEqualToString:@"false"]) {
        return FALSE;
    } else
    {
        NSLog(@"Invalid boolean value in message: %@", value);
        return FALSE;
    }
}

- (NSString*)boolToString:(bool)value
{
    return value ? @"true" : @"false";
}

- (void)messageSend:(NSArray* )message
{
    NSString* messageStr = [message componentsJoinedByString:@"\1"];
    const char* messageCPointer = [messageStr UTF8String];
    CGEApp_SendMessageToPascal(messageCPointer);
}

- (void)onPurchase:(AvailableProduct*) product
  withTransaction:(SKPaymentTransaction*) transaction
{
}

@end

NSString* stringNonNil(NSString* value)
{
    return value != nil ? value : @"";
}
