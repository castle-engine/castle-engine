//
//  ViewpointsController.h
//  CastleEngineTest
//
//  Created by Jan Adamec on 18.11.13.
//  Copyright (c) 2013 Jan Adamec. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface ViewpointsController : UITableViewController

@property (nonatomic, assign) UIPopoverController *popover;
@property (nonatomic, strong) NSMutableArray* arrayViewpoints;
@property (nonatomic, assign) NSInteger selectedViewpoint;

@end
