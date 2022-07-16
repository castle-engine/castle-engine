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

#import <Foundation/Foundation.h>

@interface Options : NSObject

@property (nonatomic, assign) BOOL walkTwoControls;
@property (nonatomic, assign) BOOL walkHeadBobbing;
@property (nonatomic, assign) BOOL ssao;

+ (id)sharedOptions;  //singleton

@end
