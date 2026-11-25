/*
  Copyright 2026-2026 Hamid reza Kabiri.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in the "Castle Game Engine" distribution,
  for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
*/

#import "LocalNotificationsService.h"

@implementation LocalNotificationsService
{
    /* Whether we already asked the user for the permission to show
       notifications. We ask once, at the first scheduled notification,
       not at startup: asking makes sense only when there is something to show. */
    bool permissionAsked;
}

- (id)init
{
    self = [super init];
    if (self) {
        permissionAsked = FALSE;
        /* Become the delegate as early as possible, Apple documents that
           it should be set before the application finishes launching. */
        if (@available(iOS 10.0, *)) {
            [UNUserNotificationCenter currentNotificationCenter].delegate = self;
        }
    }
    return self;
}

- (void)requestPermission API_AVAILABLE(ios(10.0))
{
    if (permissionAsked) {
        return;
    }
    permissionAsked = TRUE;

    UNAuthorizationOptions options =
        UNAuthorizationOptionAlert | UNAuthorizationOptionSound | UNAuthorizationOptionBadge;
    [[UNUserNotificationCenter currentNotificationCenter]
        requestAuthorizationWithOptions: options
        completionHandler: ^(BOOL granted, NSError * _Nullable error) {
            /* Nothing to do with the answer: if the user refuses,
               the notifications are scheduled but iOS does not show them. */
            if (!granted) {
                NSLog(@"LocalNotificationsService: permission to show notifications not granted");
            }
        }];
}

- (void)schedule:(NSString*) notificationId
  delaySeconds:(NSTimeInterval) delaySeconds
  title:(NSString*) title
  text:(NSString*) text
{
    if (@available(iOS 10.0, *)) {
        [self requestPermission];

        UNMutableNotificationContent* content = [[UNMutableNotificationContent alloc] init];
        content.title = title;
        content.body = text;
        content.sound = [UNNotificationSound defaultSound];

        /* UNTimeIntervalNotificationTrigger requires an interval > 0.
           A nil trigger means "deliver right away", which is what delay 0 means. */
        UNTimeIntervalNotificationTrigger* trigger = nil;
        if (delaySeconds > 0) {
            trigger = [UNTimeIntervalNotificationTrigger
                triggerWithTimeInterval: delaySeconds repeats: NO];
        }

        /* Adding a request with an identifier that is already pending
           replaces it, just like scheduling the same id again on Android. */
        UNNotificationRequest* request = [UNNotificationRequest
            requestWithIdentifier: notificationId content: content trigger: trigger];
        [[UNUserNotificationCenter currentNotificationCenter]
            addNotificationRequest: request
            withCompletionHandler: ^(NSError * _Nullable error) {
                if (error != nil) {
                    NSLog(@"LocalNotificationsService: cannot schedule notification %@: %@",
                        notificationId, error);
                }
            }];
    } else {
        NSLog(@"LocalNotificationsService: local notifications require iOS 10 or newer");
    }
}

- (void)cancel:(NSString*) notificationId
{
    if (@available(iOS 10.0, *)) {
        /* Canceling an unknown id is not an error, like on Android. */
        UNUserNotificationCenter* center = [UNUserNotificationCenter currentNotificationCenter];
        [center removePendingNotificationRequestsWithIdentifiers: @[notificationId]];
        [center removeDeliveredNotificationsWithIdentifiers: @[notificationId]];
    }
}

/* By default iOS does not show a notification while the application
   is in the foreground. Show it anyway, to behave like Android. */
- (void)userNotificationCenter:(UNUserNotificationCenter *)center
    willPresentNotification:(UNNotification *)notification
    withCompletionHandler:(void (^)(UNNotificationPresentationOptions options))completionHandler
    API_AVAILABLE(ios(10.0))
{
    if (@available(iOS 14.0, *)) {
        completionHandler(UNNotificationPresentationOptionBanner |
            UNNotificationPresentationOptionList |
            UNNotificationPresentationOptionSound);
    } else {
        completionHandler(UNNotificationPresentationOptionAlert |
            UNNotificationPresentationOptionSound);
    }
}

/* Called when the user taps the notification. iOS has already brought
   the application to the front, nothing more to do. */
- (void)userNotificationCenter:(UNUserNotificationCenter *)center
    didReceiveNotificationResponse:(UNNotificationResponse *)response
    withCompletionHandler:(void (^)(void))completionHandler
    API_AVAILABLE(ios(10.0))
{
    completionHandler();
}

- (bool)messageReceived:(NSArray* )message
{
    if (message.count == 5 &&
        [[message objectAtIndex: 0] isEqualToString:@"local-notification-schedule"])
    {
        [self schedule: [message objectAtIndex: 1]
            delaySeconds: [[message objectAtIndex: 2] doubleValue]
            title: [message objectAtIndex: 3]
            text: [message objectAtIndex: 4]];
        return TRUE;
    } else
    if (message.count == 2 &&
        [[message objectAtIndex: 0] isEqualToString:@"local-notification-cancel"])
    {
        [self cancel: [message objectAtIndex: 1]];
        return TRUE;
    }

    return FALSE;
}

@end
