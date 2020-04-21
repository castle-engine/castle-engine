/*
  Copyright 2020-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in the "Castle Game Engine" distribution,
  for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
*/

#import <AudioToolbox/AudioToolbox.h>

#import "VibrateService.h"

@implementation VibrateService

- (void)vibrate:(int) miliseconds
{
    AudioServicesPlaySystemSound(kSystemSoundID_Vibrate);
}

- (bool)messageReceived:(NSArray* )message
{
    if (message.count == 2 &&
        [[message objectAtIndex: 0] isEqualToString:@"vibrate"])
    {
        [self vibrate: [[message objectAtIndex: 1] intValue]];
        return TRUE;
    }

    return FALSE;
}

@end
