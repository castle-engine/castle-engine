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

/* Service to handle miscellaneous simple messages, like view URLs. */

#import "MiscellaneousService.h"

@implementation MiscellaneousService

- (void)viewUrl:(NSString*) url
{
    [[UIApplication sharedApplication] openURL: [NSURL URLWithString: url]];
}

- (void)openApplicationStore:(NSString*) applicationId
{
    // see https://stackoverflow.com/questions/3124080/app-store-link-for-rate-review-this-app

    static NSString *const iOS7AppStoreURLFormat = @"itms-apps://itunes.apple.com/app/id%@";
    static NSString *const iOSAppStoreURLFormat = @"itms-apps://itunes.apple.com/WebObjects/MZStore.woa/wa/viewContentsUserReviews?type=Purple+Software&id=%@";

    NSString* url = [NSString stringWithFormat:
        ([[UIDevice currentDevice].systemVersion floatValue] >= 7.0f) ?
        iOS7AppStoreURLFormat : iOSAppStoreURLFormat, applicationId];

    [self viewUrl: url];
}

- (void)shareText:(NSString*) title
    subject:(NSString*) subject
    content:(NSString*) content
{
    // see https://stackoverflow.com/questions/25320520/sharing-text-as-a-string-to-another-app

    UIActivityViewController *activityVC = [[UIActivityViewController alloc]
        initWithActivityItems: @[content] applicationActivities: nil];
    activityVC.excludedActivityTypes = @[
        UIActivityTypePrint,
        UIActivityTypeCopyToPasteboard,
        UIActivityTypeAssignToContact,
        UIActivityTypeSaveToCameraRoll]; // Exclude whichever aren't relevant
    [self.mainController presentViewController: activityVC animated: YES completion: nil];
}

- (bool)messageReceived:(NSArray* )message
{
    if (message.count == 2 &&
        [[message objectAtIndex: 0] isEqualToString:@"view-url"])
    {
        [self viewUrl: [message objectAtIndex: 1]];
        return TRUE;
    } else
    if (message.count == 2 &&
        [[message objectAtIndex: 0] isEqualToString:@"open-application-store"])
    {
        [self openApplicationStore: [message objectAtIndex: 1]];
        return TRUE;
    } else
    if (message.count == 4 &&
        [[message objectAtIndex: 0] isEqualToString:@"share-text"])
    {
        [self shareText: [message objectAtIndex: 1]
            subject: [message objectAtIndex: 2]
            content: [message objectAtIndex: 3]];
        return TRUE;
    }

    return FALSE;
}

@end
