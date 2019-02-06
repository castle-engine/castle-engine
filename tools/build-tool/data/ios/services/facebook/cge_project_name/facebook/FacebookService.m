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

/* Facebook SDK https://developers.facebook.com/docs/ios/
   integration with Castle Game Engine https://castle-engine.io/ .
*/

#import "FacebookService.h"

#import <FBSDKCoreKit/FBSDKCoreKit.h>
#import <FBSDKLoginKit/FBSDKLoginKit.h>

@implementation FacebookService

- (void)applicationDidBecomeActive:(UIApplication *) application
{
    [FBSDKAppEvents activateApp];
}

- (void)application:(UIApplication *) application
    didFinishLaunchingWithOptions:(NSDictionary *) launchOptions
{
    [[FBSDKApplicationDelegate sharedInstance]
        application: application
        didFinishLaunchingWithOptions: launchOptions];
}

- (BOOL)application:(UIApplication *)application
    openURL:(NSURL *)url
    options:(NSDictionary<UIApplicationOpenURLOptionsKey,id> *)options
{
    return [[FBSDKApplicationDelegate sharedInstance] application:application
        openURL:url
        sourceApplication:options[UIApplicationOpenURLOptionsSourceApplicationKey]
        annotation:options[UIApplicationOpenURLOptionsAnnotationKey]
    ];
}

- (void)facebookLoginButton
{
    FBSDKLoginButton *loginButton = [[FBSDKLoginButton alloc] init];
    loginButton.center = self.mainController.view.center;
    [self.mainController.view addSubview:loginButton];
}

- (bool)messageReceived:(NSArray* )message
{
    if (message.count == 1 &&
        [[message objectAtIndex: 0] isEqualToString:@"facebook-login-button"])
    {
        [self facebookLoginButton];
        return TRUE;
    }

    return FALSE;
}

@end
