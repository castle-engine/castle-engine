//
//  OpenGLController.h
//  CastleEngineTest
//
//  Created by Jan Adamec on 18.11.13.
//  Copyright (c) 2013 Jan Adamec. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <GLKit/GLKit.h>
#import "ViewpointsController.h"

@interface OpenGLController : GLKViewController<UIPopoverControllerDelegate>
{
    ViewpointsController *m_viewpointsController;
    UIPopoverController *m_currentPopover;
}

@property (nonatomic, strong) NSString* fileToOpen;

- (IBAction)OnTouchGesture:(UITapGestureRecognizer *)sender;
- (IBAction)OnPanGesture:(UIPanGestureRecognizer *)sender;
- (IBAction)OnPinchGesture:(UIPinchGestureRecognizer *)sender;


@end
