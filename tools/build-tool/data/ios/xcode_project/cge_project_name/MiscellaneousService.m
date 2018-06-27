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

- (void)debugScreenSizeShare
{
    CGFloat scale;
    if ([[UIScreen mainScreen] respondsToSelector:@selector(displayLinkWithTarget:selector:)])
        scale = [UIScreen mainScreen].scale; // check retina
    else
        scale = 1.0;

    int w1 = self.mainController.view.bounds.size.width;
    int h1 = self.mainController.view.bounds.size.height;


    // methods from
    // https://stackoverflow.com/questions/4779221/in-iphone-app-how-to-detect-the-screen-resolution-of-the-device
    CGRect screenBounds = [[UIScreen mainScreen] bounds];
    float w2 = screenBounds.size.width;
    float h2 = screenBounds.size.height;

    // according to https://stackoverflow.com/questions/4779221/in-iphone-app-how-to-detect-the-screen-resolution-of-the-device
    // native bounds should be better?
    // Native Bounds - Detect Screen size in Pixels.
    CGRect nativeBounds = [[UIScreen mainScreen] nativeBounds];
    float w3 = nativeBounds.size.width;
    float h3 = nativeBounds.size.height;

    NSString* screenSizeDetails = [NSString stringWithFormat:
        @"[UIScreen mainScreen].scale = %f\nself.mainController.view.bounds.size.width / height = %d / %d\n[[UIScreen mainScreen] bounds].width / height = %f / %f\n[[UIScreen mainScreen] nativeBounds].width / height = %f / %f",
        scale,
        w1, h1,
        w2, h2,
        w3, h3
    ];
    NSLog(@"Screen size debug details follows");
    NSLog(@"%@", screenSizeDetails);

    [self shareText: @"Screen size debug details"
        subject: @"Screen size debug details"
        content: screenSizeDetails];
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
    } else
    if (message.count == 1 &&
        [[message objectAtIndex: 0] isEqualToString:@"debug-screen-size-share"])
    {
        [self debugScreenSizeShare];
        return TRUE;
    }

    return FALSE;
}

@end
