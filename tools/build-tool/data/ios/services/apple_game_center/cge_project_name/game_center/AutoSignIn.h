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

/*
  Whether to automatically sign-in to Apple Game Center on initialization.
*/

#import <GameKit/GameKit.h>

@interface AutoSignIn : NSObject
{
    bool m_isAutoSignIn;
    NSLock *writeLock;
    NSString *saveFilename;
}

// Should we automatically sign-in next time Game Center is initialized.
- (bool)isAutoSignIn;

// Make us automatically sign-in next time Game Center is initialized.
- (void)setAutoSignIn:(bool) value;

@end
