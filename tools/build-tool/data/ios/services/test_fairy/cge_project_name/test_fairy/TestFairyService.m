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

/* TestFairy SDK https://docs.testfairy.com/iOS_SDK/Integrating_iOS_SDK.html
   integration with Castle Game Engine https://castle-engine.io/ .
*/

#import "TestFairyService.h"

#import "TestFairy.h"

bool testFairyInitialized = false;

void TestFairyInitialize()
{
    if (!testFairyInitialized) {
        /* Do not specify domain anymore?
           Free TestFairy users do not have a domain,
           and it seems not necessary to specify it anyway. */
        // [TestFairy setServerEndpoint:@"https://${IOS.TEST_FAIRY.DOMAIN}.testfairy.com"];

        [TestFairy begin:@"${IOS.TEST_FAIRY.SDK_APP_TOKEN}"];
        testFairyInitialized = true;
    }
}

void CGE_TestFairyLog_NSString(NSString* message)
{
    // TestFairy cuts off too long messages, to send them in parts
    const int MAX_SEND_LENGTH = 2048;
    int done = 0;
    int length = [message length];
    while (done < length) {
        NSRange range = NSMakeRange(done, MIN(MAX_SEND_LENGTH, length - done));
        NSString* messagePart = [message substringWithRange: range];
        done += range.length;
        [TestFairy log: messagePart];
    }
}

/* We are using special callback for this, not our messaging system,
   to receive messages even before application finished initializing. */
void CGE_TestFairyLog(const char *message)
{
    TestFairyInitialize();
    CGE_TestFairyLog_NSString([NSString stringWithUTF8String: message]);
}

@implementation TestFairyService

- (void)application:(UIApplication *) application
    didFinishLaunchingWithOptions:(NSDictionary *) launchOptions
{
    TestFairyInitialize();
}

/*
- (bool)messageReceived:(NSArray* )message
{
    if (message.count == 2 &&
        [[message objectAtIndex: 0] isEqualToString:@"test-fairy-log"])
    {
        [TestFairy log: [message objectAtIndex: 1]];
        return TRUE;
    }

    return FALSE;
}
*/

@end
