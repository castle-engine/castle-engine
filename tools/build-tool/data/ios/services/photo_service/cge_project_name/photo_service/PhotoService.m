/*
  Copyright 2017-2018 Michalis Kamburelis and Jan Adamec.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in the "Castle Game Engine" distribution,
  for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
*/

/*
  PhotoLibrary integration with Castle Game Engine.
*/

#import "PhotoService.h"
#import <AudioToolbox/AudioToolbox.h>

@implementation PhotoService

#pragma mark -

- (bool)messageReceived:(NSArray* )message
{
    if (message.count == 2 &&
        [[message objectAtIndex: 0] isEqualToString:@"photoservice-store-image"])
    {
        NSString* sFileUrl = [message objectAtIndex: 1];
        NSURL *urlFile = [NSURL URLWithString:sFileUrl];
        UIImage *img = [UIImage imageWithContentsOfFile:[urlFile path]];
        if (img!=nil)
        {
            UIImageWriteToSavedPhotosAlbum(img, nil, nil, nil);

            // play sound, see http://iphonedevwiki.net/index.php/AudioServices
            AudioServicesPlaySystemSound(1108);
        }
        return TRUE;
    }
    return FALSE;
}

@end
