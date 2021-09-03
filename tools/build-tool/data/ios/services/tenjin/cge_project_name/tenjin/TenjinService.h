/*
  Copyright 2021-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in the "Castle Game Engine" distribution,
  for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
*/

/* Tenjin SDK https://github.com/tenjin/tenjin-ios-sdk
   integration with Castle Game Engine https://castle-engine.io/ .
*/

#import "../ServiceAbstract.h"

@interface TenjinService : ServiceAbstract
{
    bool connected;
}

@end
