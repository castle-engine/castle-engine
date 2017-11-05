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

/* Apple Game Center integration with Castle Game Engine. */

#import <UIKit/UIKit.h>
#import <GameKit/GameKit.h>
#import "../ServiceAbstract.h"
#import "Achievements.h"
#import "AutoSignIn.h"

// Preferred method for testing for Game Center
bool isGameCenterAPIAvailable(void);

@interface GameCenterService : ServiceAbstract <GKGameCenterControllerDelegate> {
    bool m_autoStartSignInFlow;
    bool m_finishedLaunching;
    bool m_duringSignIn;
    bool m_signedIn;

    // value of the playerID last time GameKit authenticated.
    NSString* authenticatedPlayerID;

    // achievements management
    Achievements* achievements;

    // whether to automatically sign-in on initialization
    AutoSignIn* autoSignIn;
}

@end
