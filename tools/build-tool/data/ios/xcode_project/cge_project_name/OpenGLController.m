/*
  Copyright 2013-2017 Jan Adamec, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in the "Castle Game Engine" distribution,
  for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
*/

#import "OpenGLController.h"
#include "castleiosappglue.h"
#import "CgeUtils.h"

#define MAX_TOUCHES 12

typedef struct TouchInfo {
    // nil if nothing registered
    UITouch* uiTouch;
    // We should send motion event for this
    bool pendingMotion;
} TouchInfo;

@interface OpenGLController ()
{
    CGFloat m_fScale;
    TouchInfo m_touches[MAX_TOUCHES];
    int m_currentViewWidth;
    int m_currentViewHeight;
}

@property (strong, nonatomic) EAGLContext *context;

@end

//-----------------------------------------------------------------
@implementation OpenGLController

- (void)viewDidLoad
{
    [super viewDidLoad];

    // Try to initialize OpenGLES 3, fallback on version 2
    // (following https://developer.apple.com/library/archive/documentation/3DDrawing/Conceptual/OpenGLES_ProgrammingGuide/WorkingwithOpenGLESContexts/WorkingwithOpenGLESContexts.html )
    /*
    EAGLContext *context = [[EAGLContext alloc] initWithAPI:kEAGLRenderingAPIOpenGLES3];
    if (context == nil) {
        context = [[EAGLContext alloc] initWithAPI:kEAGLRenderingAPIOpenGLES2];
    }
    */
    /* TODO: OpenGLES 3 commented out for now.
       It breaks Unholy rendering of "evil plane".
       Looks like on iOS, something in OpenGLES3 is subtly broken compared to OpenGLES2,
       maybe related to ScreenFbo which is already weird (non-zero) in OpenGLES2,
       see
       src/images/opengl/castleglimages_rendertotexture.inc
       https://stackoverflow.com/questions/11617013/why-would-glbindframebuffergl-framebuffer-0-result-in-blank-screen-in-cocos2d)

       Ideally we should just fix it, and work flawlessly in both OpenGLES2 and OpenGLES3 on iOS.

       Eventually we can add a flag, like IOS_ENABLE_ES3, to make it optional decision
       per-application.
    */
    EAGLContext *context = [[EAGLContext alloc] initWithAPI:kEAGLRenderingAPIOpenGLES2];

    self.context = context;

    if (!self.context) {
        NSLog(@"Failed to create ES context");
    }

    GLKView *view = (GLKView *)self.view;
    view.context = self.context;

    /* Configure OpenGLES buffer sizes.
       GLKView provides very limited configuration options, see
       https://developer.apple.com/library/archive/documentation/3DDrawing/Conceptual/OpenGLES_ProgrammingGuide/DrawingWithOpenGLES/DrawingWithOpenGLES.html#//apple_ref/doc/uid/TP40008793-CH503-SW1
       https://developer.apple.com/documentation/glkit/glkview
    */
    int redBits, greenBits, blueBits, alphaBits, depthBits, stencilBits, multiSampling;
    CGEApp_ContextProperties(&redBits, &greenBits, &blueBits, &alphaBits, &depthBits, &stencilBits, &multiSampling);
    // view.drawableColorFormat = GLKViewDrawableColorFormatRGBA8888; // default

    if (depthBits == 0) {
        view.drawableDepthFormat = GLKViewDrawableDepthFormatNone;
    } else
    if (depthBits <= 16) {
        view.drawableDepthFormat = GLKViewDrawableDepthFormat16;
    } else {
        view.drawableDepthFormat = GLKViewDrawableDepthFormat24;
    }

    if (stencilBits == 0) {
        view.drawableStencilFormat = GLKViewDrawableStencilFormatNone;
    } else {
        view.drawableStencilFormat = GLKViewDrawableStencilFormat8;
    }

    if (multiSampling < 4) {
        view.drawableMultisample = GLKViewDrawableMultisampleNone;
    } else {
        view.drawableMultisample = GLKViewDrawableMultisample4X;
    }

    // initialize input

    self.view.multipleTouchEnabled = YES;
    for (int i = 0; i < MAX_TOUCHES; i++) {
        m_touches[i].uiTouch = nil;
        m_touches[i].pendingMotion = FALSE;
    }

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
- (int)statusBarHeight
{
    CGSize statusBarSize = [[UIApplication sharedApplication] statusBarFrame].size;
    return MIN(statusBarSize.width, statusBarSize.height) * m_fScale;
}

//-----------------------------------------------------------------
- (void)setupGL
{
    [EAGLContext setCurrentContext:self.context];

    /* We need to look at scale, to apply Retina.

       Moreover, iPhone 8 Plus does more tricks,
       and we need to look at nativeScale.
       See https://www.paintcodeapp.com/news/iphone-6-screens-demystified
       for explanation what is happening under the hood.

       (Using [[UIScreen mainScreen] nativeBounds] also gets correct size,
       but not adjusted to orientation.)
    */
    if ([[UIScreen mainScreen] respondsToSelector:@selector(displayLinkWithTarget:selector:)])
        m_fScale = [UIScreen mainScreen].nativeScale;
    else
        m_fScale = 1.0;

    /*
     Get the DPI/PPI. The real values (132*scale for iPhones, 163*scale for iPads)
     are too high when used in reality. The concept of scaling UI using DPI fails
     on iOS. It is much better to use screen scale only (m_fScale). In that case,
     when you define 25 px large icon, it will have 50 px at Retina resolution,
     which is what we want.

     Also, it is impossible to get the precise iDevice PPI other then enumerating all
     existing devices.
     */
    /*unsigned dpi = 160;
    if (UI_USER_INTERFACE_IDIOM() == UIUserInterfaceIdiomPad)
        dpi = 132;
    else if (UI_USER_INTERFACE_IDIOM() == UIUserInterfaceIdiomPhone)
        dpi = 163;*/
    unsigned dpi = 96; // CastleWindow.DefaultDpi

    m_currentViewWidth  = self.view.bounds.size.width  * m_fScale;
    m_currentViewHeight = self.view.bounds.size.height * m_fScale;

    // Get a directory where we can write files,
    // see http://stackoverflow.com/questions/1567134/how-can-i-get-a-writable-path-on-the-iphone/1567147#1567147
    NSArray *paths = NSSearchPathForDirectoriesInDomains(NSLibraryDirectory, NSUserDomainMask, YES);
    NSString *libraryDirectory = [paths objectAtIndex:0];

    CGEApp_Initialize([libraryDirectory fileSystemRepresentation]);
    CGEApp_Open(m_currentViewWidth, m_currentViewHeight, [self statusBarHeight], (unsigned)(dpi * m_fScale));

    [self update];
}

//-----------------------------------------------------------------
- (void)tearDownGL
{
    [EAGLContext setCurrentContext:self.context];

    CGEApp_Close(true);
    CGEApp_Finalize();
}

#pragma mark - GLKView and GLKViewController delegate methods
//-----------------------------------------------------------------
- (void)update
{
    // update the viewport size, if changed
    int newViewWidth  = self.view.bounds.size.width  * m_fScale;
    int newViewHeight = self.view.bounds.size.height * m_fScale;
    if (m_currentViewWidth  != newViewWidth ||
        m_currentViewHeight != newViewHeight)
    {
        m_currentViewWidth  = newViewWidth;
        m_currentViewHeight = newViewHeight;
        CGEApp_Resize(newViewWidth, newViewHeight, [self statusBarHeight]);
    }

    // send accumulated motion events (sending them right away can jam the engine)
    for (NSInteger i = 0; i < MAX_TOUCHES; i++)
    {
        if (m_touches[i].uiTouch == nil) continue;
        if (!m_touches[i].pendingMotion) continue;

        CGPoint pt = [m_touches[i].uiTouch locationInView:self.view];
        [self RecalcTouchPosForCGE:&pt];
        CGEApp_Motion(pt.x, pt.y, (int)i);
        m_touches[i].pendingMotion = FALSE;
    }

    CGEApp_Update();
}

//-----------------------------------------------------------------
- (void)glkView:(GLKView *)view drawInRect:(CGRect)rect
{
    CGEApp_Render();
}

#pragma mark - gestures

//-----------------------------------------------------------------
- (void)RecalcTouchPosForCGE:(CGPoint *)pt
{
    pt->x *= m_fScale;
    pt->y *= m_fScale;
    pt->y = m_currentViewHeight - 1 - pt->y;
}

//-----------------------------------------------------------------
- (NSInteger)IndexOfTouch:(UITouch*)touch
{
    for (NSInteger i = 0; i < MAX_TOUCHES; i++)
        if (m_touches[i].uiTouch == touch) {
            return i;
        }
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
            if (m_touches[i].uiTouch == nil)    // find empty place
            {
                nFingerIdx = i;
                m_touches[i].uiTouch = touch;
                m_touches[i].pendingMotion = FALSE; // initial value
                break;
            }
        }
        if (nFingerIdx==-1) continue;   // array full, should not happen

        CGPoint pt = [touch locationInView:self.view];
        [self RecalcTouchPosForCGE:&pt];
        CGEApp_MouseDown(pt.x, pt.y, true, (int)nFingerIdx);
    }

    [super touchesBegan:touches withEvent:event];
}

//-----------------------------------------------------------------
- (void)TouchesEndedOrCancelled:(NSSet *)touches
{
    for (UITouch *touch in touches)
    {
        NSInteger nFingerIdx = [self IndexOfTouch:touch];
        if (nFingerIdx == -1) continue;

        m_touches[nFingerIdx].uiTouch = nil;

        CGPoint pt = [touch locationInView:self.view];
        [self RecalcTouchPosForCGE:&pt];
        CGEApp_MouseUp(pt.x, pt.y, true, (int)nFingerIdx, false);
    }
}

//-----------------------------------------------------------------
- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event
{
    [self TouchesEndedOrCancelled:touches];
    [super touchesEnded:touches withEvent:event];
}

//-----------------------------------------------------------------
- (void)touchesCancelled:(NSSet *)touches withEvent:(UIEvent *)event
{
    [self TouchesEndedOrCancelled:touches];
    [super touchesCancelled:touches withEvent:event];
}

//-----------------------------------------------------------------
- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event
{
    // CGEApp_Motion will be called in the Update function. Calling it too frequently can jam the engine.
    for (UITouch *touch in touches)
    {
        NSInteger nFingerIdx = [self IndexOfTouch:touch];
        if (nFingerIdx == -1) continue;

        m_touches[nFingerIdx].pendingMotion = TRUE;
    }

    [super touchesMoved:touches withEvent:event];
}

#pragma mark - interface

//-----------------------------------------------------------------
- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
    // TODO: This probably can be removed?
    // This is deprecated:
    // https://developer.apple.com/documentation/uikit/uiviewcontroller/1621459-shouldautorotatetointerfaceorien
    // And results of this are intersected with orientations we define in project already:
    // https://developer.apple.com/documentation/uikit/uiviewcontroller/1621435-supportedinterfaceorientations?language=objc
    return YES;
}

//---------------------------------------------------------------------------
- (BOOL)prefersStatusBarHidden   // hide status bar (iOS7)
{
    return ${IOS_STATUSBAR_HIDDEN};
}

//---------------------------------------------------------------------------
- (UIStatusBarStyle)preferredStatusBarStyle
{
    return UIStatusBarStyleLightContent;
}

@end
