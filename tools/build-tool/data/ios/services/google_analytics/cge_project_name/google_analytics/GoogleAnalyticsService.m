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

/* Google Analytics https://analytics.google.com/ integration with Castle Game Engine. */

#import "GoogleAnalyticsService.h"

@implementation GoogleAnalyticsService

- (void)initialize:(NSString*) analyticsPropertyId
{
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
    }

    // TODO: handle rest of messages

    return FALSE;
}

@end
