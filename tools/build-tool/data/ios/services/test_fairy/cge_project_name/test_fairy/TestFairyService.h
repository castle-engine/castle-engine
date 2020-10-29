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

/* TestFairy SDK https://docs.testfairy.com/iOS_SDK/Integrating_iOS_SDK.html#cocoapods
   integration with Castle Game Engine https://castle-engine.io/ .
*/

#import "../ServiceAbstract.h"

@interface TestFairyService : ServiceAbstract
{
}

@end

void CGE_TestFairyLog(const char *message);
