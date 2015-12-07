/*
  Copyright 2013-2014 Jan Adamec, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
*/

#import "OpenGLController.h"
#include "castleiosappglue.h"

#define MAX_TOUCHES 12

@interface OpenGLController ()
{
    CGFloat m_fScale;
    UITouch* m_arrTouches[MAX_TOUCHES];
}

@property (strong, nonatomic) EAGLContext *context;

@end

//-----------------------------------------------------------------
@implementation OpenGLController

- (void)viewDidLoad
{
    [super viewDidLoad];
    
    self.context = [[EAGLContext alloc] initWithAPI:kEAGLRenderingAPIOpenGLES2];
    
    if (!self.context) {
        NSLog(@"Failed to create ES context");
    }
    
    GLKView *view = (GLKView *)self.view;
    view.context = self.context;
    
    self.view.multipleTouchEnabled = YES;
    for (int i = 0; i < MAX_TOUCHES; i++) m_arrTouches[i] = nil;
    
    [self setupGL];
}

//-----------------------------------------------------------------
- (void)dealloc
{
    [self tearDownGL];
    
    if ([EAGLContext currentContext] == self.context) {
        [EAGLContext setCurrentContext:nil];
    }
}

//-----------------------------------------------------------------
- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    
    if ([self isViewLoaded] && ([[self view] window] == nil)) {
        self.view = nil;
        
        [self tearDownGL];
        
        if ([EAGLContext currentContext] == self.context) {
            [EAGLContext setCurrentContext:nil];
        }
        self.context = nil;
    }
    
    // Dispose of any resources that can be recreated.
}

//-----------------------------------------------------------------
- (void)setupGL
{
    [EAGLContext setCurrentContext:self.context];

    if ([[UIScreen mainScreen] respondsToSelector:@selector(displayLinkWithTarget:selector:)])
        m_fScale = [UIScreen mainScreen].scale; // check retina
    else
        m_fScale = 1.0;
    
    CGEA_Open();
    CGEA_SetUserInterface(false, 115 * m_fScale);   // TODO: we had to send "desktop", otherwise touch controls does not work (as they want to be automatic?)
    
    [self update];
}

//-----------------------------------------------------------------
- (void)tearDownGL
{
    [EAGLContext setCurrentContext:self.context];
    
    CGEA_Close();
}

#pragma mark - GLKView and GLKViewController delegate methods
//-----------------------------------------------------------------
- (void)update
{
    // set and update the geometry
    int nViewSizeX = self.view.bounds.size.width;
    int nViewSizeY = self.view.bounds.size.height;
    
    CGEA_Resize(nViewSizeX*m_fScale, nViewSizeY*m_fScale);
    
    // send accumulated touch positions (sending them right away jams the engine)
    for (NSInteger i = 0; i < MAX_TOUCHES; i++)
    {
        if (m_arrTouches[i] == nil) continue;
        
        CGPoint pt = [m_arrTouches[i] locationInView:self.view];
        [self RecalcTouchPosForCGE:&pt];
        CGEA_Motion(pt.x, pt.y, (int)i);
    }
    
    CGEA_Update();
}

//-----------------------------------------------------------------
- (void)glkView:(GLKView *)view drawInRect:(CGRect)rect
{
    CGEA_Render();
}

#pragma mark - gestures

//-----------------------------------------------------------------
- (void)RecalcTouchPosForCGE:(CGPoint *)pt
{
    pt->x*=m_fScale; pt->y*=m_fScale;
    pt->y = self.view.bounds.size.height*m_fScale - 1 - pt->y;
}

//-----------------------------------------------------------------
- (NSInteger)IndexOfTouch:(UITouch*)touch
{
    for (NSInteger i = 0; i < MAX_TOUCHES; i++)
        if (m_arrTouches[i] == touch) return i;
    return -1;
}

//-----------------------------------------------------------------
- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event
{
    for (UITouch *touch in touches)
    {
        NSInteger nFingerIdx = [self IndexOfTouch:touch];
        if (nFingerIdx != -1) continue; // we already have it (should not happen)
        
        for (NSInteger i = 0; i < MAX_TOUCHES; i++)
        {
            if (m_arrTouches[i]==nil)    // find empty place
            {
                nFingerIdx = i;
                m_arrTouches[i] = touch;
                break;
            }
        }
        if (nFingerIdx==-1) continue;   // array full, should not happen
        
        CGPoint pt = [touch locationInView:self.view];
        [self RecalcTouchPosForCGE:&pt];
        CGEA_TouchDown(pt.x, pt.y, (int)nFingerIdx);
    }
    
    [super touchesBegan:touches withEvent:event];
}

//-----------------------------------------------------------------
- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event
{
    for (UITouch *touch in touches)
    {
        NSInteger nFingerIdx = [self IndexOfTouch:touch];
        if (nFingerIdx == -1) continue;
        
        m_arrTouches[nFingerIdx] = nil;
        
        CGPoint pt = [touch locationInView:self.view];
        [self RecalcTouchPosForCGE:&pt];
        CGEA_TouchUp(pt.x, pt.y, (int)nFingerIdx);
    }

    [super touchesEnded:touches withEvent:event];
}

//-----------------------------------------------------------------
- (void)touchesCancelled:(NSSet *)touches withEvent:(UIEvent *)event
{
    for (UITouch *touch in touches)
    {
        NSInteger nFingerIdx = [self IndexOfTouch:touch];
        if (nFingerIdx == -1) continue;
        
        m_arrTouches[nFingerIdx] = nil;
        
        CGPoint pt = [touch locationInView:self.view];
        [self RecalcTouchPosForCGE:&pt];
        CGEA_TouchUp(pt.x, pt.y, (int)nFingerIdx);
    }
    
    [super touchesCancelled:touches withEvent:event];
}

//-----------------------------------------------------------------
- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event
{
    // note: motions are called in update function. Calling it too frequently jams the engine.
    /*
    for (UITouch *touch in touches)
    {
        NSInteger nFingerIdx = [self IndexOfTouch:touch];
        if (nFingerIdx == -1) continue;
        
        CGPoint pt = [touch locationInView:self.view];
        [self RecalcTouchPosForCGE:&pt];
        CGEA_Motion(pt.x, pt.y, nFingerIdx);
    }*/
    
    [super touchesMoved:touches withEvent:event];
}

#pragma mark - interface

//-----------------------------------------------------------------
- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
    return UIInterfaceOrientationIsLandscape(interfaceOrientation);
}

//---------------------------------------------------------------------------
- (BOOL)prefersStatusBarHidden   // hide status bar (iOS7)
{
    return YES;
}

@end
