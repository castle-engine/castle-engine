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

#import "OpenGLController.h"
#import "AvailableProduct.h"

/* Base class for all Castle Game Engine iOS application services. */
@interface ServiceAbstract : NSObject {
}

/* Called when AppDelegate receives applicationDidEnterBackground. */
- (void)applicationDidEnterBackground;

/* Called when AppDelegate receives application:didFinishLaunchingWithOptions: message. */
- (void)application:(UIApplication *) application didFinishLaunchingWithOptions:(NSDictionary *) launchOptions;

/* Called when AppDelegate receives applicationDidBecomeActive: message. */
- (void)applicationDidBecomeActive:(UIApplication *)application;

/* Called when AppDelegate receives application:openURL:options message. */
- (BOOL)application:(UIApplication *)app
    openURL:(NSURL *)url
    options:(NSDictionary<UIApplicationOpenURLOptionsKey, id> *)options;

/* Try handling this message. Returns TRUE if handled. */
- (bool)messageReceived:(NSArray* )message;

/* Send the message to Pascal. */
- (void)messageSend:(NSArray* )message;

/* Convert string to boolean when receiving message in messageReceive. */
- (bool)stringToBool:(NSString* )value;

/* Convert boolean to string when sending the message in messageSend. */
- (NSString*)boolToString:(bool)value;

/* Notification that a purchase just happened. */
- (void)onPurchase:(AvailableProduct*) product
  withTransaction:(SKPaymentTransaction*) transaction;

@property (nonatomic, retain) UIWindow *window;
@property (nonatomic, retain) OpenGLController *mainController;

@end

/* Return non-nil version of the string.
   In case value is nil, returns zero-length non-nil string version, @"".

   This is particularly useful because when NSArray cannot hold nil.
   So this is unsafe, in case myString may be nil:

     @[myString]

   This is always safe:

     @[stringNonNil(myString)]
*/
NSString* stringNonNil(NSString* value);
