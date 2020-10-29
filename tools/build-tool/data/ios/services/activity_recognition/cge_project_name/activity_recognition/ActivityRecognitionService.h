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

/* Activity recognition for Castle Game Engine https://castle-engine.io/ on iOS. */

#import <CoreMotion/CoreMotion.h>
#import "../ServiceAbstract.h"

@interface ActivityRecognitionService : ServiceAbstract
{
    CMMotionActivityManager *motionActivityManager;
}

@end
