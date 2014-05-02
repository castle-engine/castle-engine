//
//  Options.h
//  ios_tester
//
//  Created by Jan Adamec on 05.02.14.
//  Copyright (c) 2014 Jan Adamec. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface Options : NSObject

@property (nonatomic, assign) BOOL walkTwoControls;
@property (nonatomic, assign) BOOL walkHeadBobbing;
@property (nonatomic, assign) BOOL ssao;

+ (id)sharedOptions;  //singleton

@end
