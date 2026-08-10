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

#import "AppDelegate.h"
#import "OpenGLController.h"
#import "ServiceAbstract.h"
#import "MiscellaneousService.h"
#import "CgeUtils.h"

// import services
/* IOS-SERVICES-IMPORT */

AppDelegate* appDelegateSingleton;

void receiveMessageFromPascal(const char* message)
{
    if (appDelegateSingleton == nil) {
        NSLog(@"Objective-C received message from Pascal, but appDelegateSingleton is not assigned, this should not happen.");
        return;
    }
    [appDelegateSingleton messageReceived:message];
}

AppDelegate* getAppDelegate(void)
{
    return appDelegateSingleton;
}

@implementation AppDelegate

- (void)initializeServices:(OpenGLController* )viewController window:(UIWindow*)sceneWindow
{
    services = [[NSMutableArray alloc] init];

    // create MiscellaneousService, added by default
    {
    MiscellaneousService* serviceInstance;
    serviceInstance = [[MiscellaneousService alloc] init];
    serviceInstance.mainController = viewController;
    serviceInstance.window = sceneWindow;
    [services addObject: serviceInstance];
    }

    // create services
    /* IOS-SERVICES-CREATE */

    // initialize messaging with CastleMessaging unit
    appDelegateSingleton = self;
    CGEApp_SetReceiveMessageFromPascalCallback(receiveMessageFromPascal);
}

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions
{
    if (@available(iOS 13.0, *)) {
        // window creation is done in SceneDelegate scene:willConnectToSession:options:
        // We will simulate calling didFinishLaunchingWithOptions on services later in onSceneDidFinishLaunching
        appLaunchOptions = launchOptions;
    }
    else {
        self.window = [[UIWindow alloc] initWithFrame:[[UIScreen mainScreen] bounds]];

        [self initializeRootViewControllerInWindow:self.window];

        // Although the window seems already visible, but makeKeyAndVisible call is still
        // needed to keep visible after application:didFinishLaunchingWithOptions call.
        [self.window makeKeyAndVisible];

        // call application:didFinishLaunchingWithOptions on all services
        for (int i = 0; i < [services count]; i++) {
            ServiceAbstract* service = [services objectAtIndex: i];
            [service application: application didFinishLaunchingWithOptions: launchOptions];
        }
    }
    return YES;
}

- (void)initializeRootViewControllerInWindow:(UIWindow*)sceneWindow
{
    GLKView *view = [[GLKView alloc] initWithFrame:[[UIScreen mainScreen] bounds]];

    OpenGLController * viewController = [[OpenGLController alloc] initWithNibName:nil bundle:nil];
    viewController.view = view;
    viewController.preferredFramesPerSecond = 60;

    sceneWindow.rootViewController = viewController;

    // initialize services once window and viewController are initialized,
    // but before doing [viewController viewDidLoad] which performs OpenGL initialization
    // including calling Application.OnInitialize (that may want to already use services).
    [self initializeServices:viewController window:sceneWindow];

    [viewController viewDidLoad];
}

- (void)onSceneDidFinishLaunching
{
    // call application:didFinishLaunchingWithOptions on all services
    for (int i = 0; i < [services count]; i++) {
        ServiceAbstract* service = [services objectAtIndex: i];
        [service application: [UIApplication sharedApplication] didFinishLaunchingWithOptions: appLaunchOptions];
    }
}

// Note: called until iOS 12, UIKit uses SceneDelegate since iOS 13
- (void)applicationWillResignActive:(UIApplication *)application
{
    // Sent when the application is about to move from active to inactive state. This can occur for certain types of temporary interruptions (such as an incoming phone call or SMS message) or when the user quits the application and it begins the transition to the background state.
    // Use this method to pause ongoing tasks, disable timers, and throttle down OpenGL ES frame rates. Games should use this method to pause the game.
}

// Note: called until iOS 12, UIKit uses SceneDelegate since iOS 13
- (void)applicationDidEnterBackground:(UIApplication *)application
{
    // Use this method to release shared resources, save user data, invalidate timers, and store enough application state information to restore your application to its current state in case it is terminated later.
    // If your application supports background execution, this method is called instead of applicationWillTerminate: when the user quits.
    [self onSceneDidEnterBackground];
}

// Note: called until iOS 12, UIKit uses SceneDelegate since iOS 13
- (void)applicationWillEnterForeground:(UIApplication *)application
{
    // Called as part of the transition from the background to the inactive state; here you can undo many of the changes made on entering the background.
}

// Note: called until iOS 12, UIKit uses SceneDelegate since iOS 13
- (void)applicationDidBecomeActive:(UIApplication *)application
{
    // Restart any tasks that were paused (or not yet started) while the application was inactive. If the application was previously in the background, optionally refresh the user interface.
    [self onSceneDidBecomeActive];
}

- (void)applicationWillTerminate:(UIApplication *)application
{
    // Called when the application is about to terminate. Save data if appropriate. See also applicationDidEnterBackground:.
}

- (void)onSceneDidEnterBackground
{
    // This method is called from SceneDelegate on iOS13, or applicationDidEnterBackground on earlier iOS versions

    // call applicationDidEnterBackground on all services
    for (int i = 0; i < [services count]; i++) {
        ServiceAbstract* service = [services objectAtIndex: i];
        [service applicationDidBecomeActive: [UIApplication sharedApplication]];
    }
}

- (void)onSceneDidBecomeActive
{
    // This method is called from SceneDelegate on iOS13, or applicationDidBecomeActive on earlier iOS versions

    // call applicationDidBecomeActive on all services
    for (int i = 0; i < [services count]; i++) {
        ServiceAbstract* service = [services objectAtIndex: i];
        [service applicationDidBecomeActive: [UIApplication sharedApplication]];
    }
}

// Note: called until iOS 12, UIKit uses SceneDelegate since iOS 13, please check the code in onOpenURLContexts:
- (BOOL)application:(UIApplication *)application
    openURL:(NSURL *)url
    options:(NSDictionary<UIApplicationOpenURLOptionsKey, id> *)options
{
    // call application:openURL:options on all services
    for (int i = 0; i < [services count]; i++) {
        ServiceAbstract* service = [services objectAtIndex: i];
        BOOL result = [service application: application openURL: url options: options];
        if (result) {
            return result;
        }
    }
    if ([url.scheme isEqualToString:@"file"])
    {
        [url startAccessingSecurityScopedResource];     // when opening from Files app (after declaring LSSupportsOpeningDocumentsInPlace in Info.plist to true)
        int ret = CGEApp_HandleOpenUrl(url.fileSystemRepresentation);
        [url stopAccessingSecurityScopedResource];
        return ret;
    }
    return NO;
}

// called in iOS 13 and later, when user opens a file associated with the app.
- (BOOL)onOpenURLContexts:(NSSet<UIOpenURLContext *> *)URLContexts  API_AVAILABLE(ios(13.0))
{
    // call application:openURL:options on all services
    for (int i = 0; i < [services count]; i++) {
        ServiceAbstract* service = [services objectAtIndex: i];
        BOOL result = [service openURLContexts:URLContexts];
        if (result) {
            return result;
        }
    }

    UIOpenURLContext* context = URLContexts.anyObject;
    if (context != nil)
    {
        NSURL *url = context.URL;
        if ([url.scheme isEqualToString:@"file"])
        {
            [url startAccessingSecurityScopedResource];     // when opening from Files app (after declaring LSSupportsOpeningDocumentsInPlace in Info.plist to true)
            int ret = CGEApp_HandleOpenUrl(url.fileSystemRepresentation);
            [url stopAccessingSecurityScopedResource];
            return ret;
        }
    }
    return NO;
}

- (void)messageReceived:(const char *)message
{
    NSString* messageStr = @(message);
    NSArray* messageAsList = [messageStr componentsSeparatedByString:@"\1"];

    // call receiveMessageFromPascal on all services
    bool handled = false;
    for (int i = 0; i < [services count]; i++) {
        ServiceAbstract* service = [services objectAtIndex: i];
        bool serviceHandled = [service messageReceived: messageAsList];
        handled = handled || serviceHandled;
    }

    if (!handled) {
        NSString* messageMultiline = [messageAsList componentsJoinedByString:@"\n"];
        NSLog(@"Message received by Objective-C but not handled by any service: %@", messageMultiline);
    }
}

- (void)onPurchase:(AvailableProduct*) product
  withTransaction:(SKPaymentTransaction*) transaction
{
    for (int i = 0; i < [services count]; i++) {
        ServiceAbstract* service = [services objectAtIndex: i];
        [service onPurchase: product withTransaction: transaction];
    }
}

@end
