//
//  Options.m
//  ios_tester
//
//  Created by Jan Adamec on 05.02.14.
//  Copyright (c) 2014 Jan Adamec. All rights reserved.
//

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
