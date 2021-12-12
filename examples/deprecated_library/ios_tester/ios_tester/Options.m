/*
  Copyright 2014 Jan Adamec, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
*/

#import "Options.h"


@implementation Options

+ (id)sharedOptions
{
    static Options *sharedMyOptions = nil;
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        sharedMyOptions = [[self alloc] init];
    });
    return sharedMyOptions;
}

//-----------------------------------------------------------------
- (id)init
{
    Options *pThis = [super init];
    if (pThis!=nil)
    {
        pThis.walkTwoControls = YES;
        pThis.walkHeadBobbing = YES;
        pThis.ssao = NO;
    }
    return pThis;
}

@end
