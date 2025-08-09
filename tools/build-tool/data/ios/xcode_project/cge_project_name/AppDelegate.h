/*
  Copyright 2013-2025 Jan Adamec, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in the "Castle Game Engine" distribution,
  for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
*/

#import <UIKit/UIKit.h>
#import "AvailableProduct.h"
#include "castleiosappglue.h"

@interface AppDelegate : UIResponder <UIApplicationDelegate>
{
    /* Services (integrations with 3rd party services).
       Array of ServiceAbstract instances. */
    NSMutableArray* services;
    NSDictionary* appLaunchOptions;    // keep a reference to simulate application:didFinishLaunchingWithOptions: on services with UISceneDelegate
}
- (void)messageReceived:(const char *)message;

/* Called by in_app_purchase service to notify all other services
   (e.g. analytics) that a purchase occured.
*/
- (void)onPurchase:(AvailableProduct*) product
  withTransaction:(SKPaymentTransaction*) transaction;

- (void)initializeRootViewControllerInWindow:(UIWindow*)sceneWindow;
- (void)onSceneDidFinishLaunching;
- (void)onSceneDidEnterBackground;
- (void)onSceneDidBecomeActive;
- (BOOL)onOpenURLContexts:(NSSet<UIOpenURLContext *> *)URLContexts  API_AVAILABLE(ios(13.0));

@property (strong, nonatomic) UIWindow *window;  // is valid for apps before iOS 13 (without SceneDelegate)

@end

/* Get singleton AppDelegate instance. */
AppDelegate* getAppDelegate(void);
