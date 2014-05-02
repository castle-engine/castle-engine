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
#import "ViewInfoController.h"
#import "Options.h"
#include "castleengine.h"

#define MAX_TOUCHES 12

//#define USE_GESTURE_RECOGNIZERS

@interface OpenGLController ()
{
    bool m_bIsPanning;
    CGPoint m_ptPanningMousePos;
    CGFloat m_fScale;
    
    UITouch* m_arrTouches[MAX_TOUCHES];
    
    UISegmentedControl *m_segmNavigation;
    UIBarButtonItem *m_btnViewpointPrev;
    UIBarButtonItem *m_btnViewpointNext;
    
    int m_nViewpointCount, m_nCurrentViewpoint;
    
}

@property (strong, nonatomic) EAGLContext *context;

@end

//-----------------------------------------------------------------
@implementation OpenGLController

- (void)viewDidLoad
{
    [super viewDidLoad];
    
    m_nViewpointCount = 0;
    m_bIsPanning = false;
    
    self.context = [[EAGLContext alloc] initWithAPI:kEAGLRenderingAPIOpenGLES2];
    
    if (!self.context) {
        NSLog(@"Failed to create ES context");
    }
    
    GLKView *view = (GLKView *)self.view;
    view.context = self.context;
    
    // setup recognizers
#ifdef USE_GESTURE_RECOGNIZERS
    [self.view addGestureRecognizer:[[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(OnTouchGesture:)]];
    [self.view addGestureRecognizer:[[UIPinchGestureRecognizer alloc] initWithTarget:self action:@selector(OnPinchGesture:)]];
    [self.view addGestureRecognizer:[[UIPanGestureRecognizer alloc] initWithTarget:self action:@selector(OnPanGesture:)]];
#else
    self.view.multipleTouchEnabled = YES;
    for (int i = 0; i < MAX_TOUCHES; i++) m_arrTouches[i] = nil;
#endif
    
    // create toolbar controls
    m_segmNavigation = [[UISegmentedControl alloc] initWithItems:@[@"Walk", @"Fly", @"Examine", @"Turntable"]];
    m_segmNavigation.segmentedControlStyle = UISegmentedControlStyleBar;
    m_segmNavigation.selectedSegmentIndex = 0;
    [m_segmNavigation addTarget:self
                         action:@selector(OnNavigationSegmentChanged:)
               forControlEvents:UIControlEventValueChanged];
    
    UIBarButtonItem *itemSegm = [[UIBarButtonItem alloc] initWithCustomView:m_segmNavigation];
    
    m_btnViewpointPrev = [[UIBarButtonItem alloc] initWithTitle:@" < " style:UIBarButtonItemStyleBordered target:self action:@selector(OnBtnViewpointPrev:)];
    m_btnViewpointNext = [[UIBarButtonItem alloc] initWithTitle:@" > " style:UIBarButtonItemStyleBordered target:self action:@selector(OnBtnViewpointNext:)];
    UIBarButtonItem *btnViewpointPopup = [[UIBarButtonItem alloc] initWithTitle:@"Viewpoints" style:UIBarButtonItemStyleBordered target:self action:@selector(OnBtnViewpointPopup:)];

    UIBarButtonItem *btnOptions = [[UIBarButtonItem alloc] initWithTitle:@"Options" style:UIBarButtonItemStyleBordered target:self action:@selector(OnBtnOptions:)];

    UIButton *btnInfo = [UIButton buttonWithType:UIButtonTypeInfoLight];
    [btnInfo addTarget:self action:@selector(OnBtnInfo:) forControlEvents:UIControlEventTouchUpInside];
    UIBarButtonItem *btnBarInfo =[[UIBarButtonItem alloc] initWithCustomView:btnInfo];
    
    UIBarButtonItem *btnOpenFile = [[UIBarButtonItem alloc] initWithTitle:@"Open File" style:UIBarButtonItemStyleBordered target:self action:@selector(OnBtnOpenFile:)];
    
    self.navigationItem.leftBarButtonItem = btnOpenFile;
    self.navigationItem.rightBarButtonItems = @[btnBarInfo, btnOptions, m_btnViewpointNext, btnViewpointPopup, m_btnViewpointPrev, itemSegm];
    
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
    
    if (self.fileToOpen == nil)
    {
        NSString *sBundlePath = [[NSBundle mainBundle] bundlePath];
        self.fileToOpen = [sBundlePath stringByAppendingPathComponent:@"sampledata/castle_with_lights_and_camera.wrl"];
    }

    CGE_Open(ecgeofLog);
    CGE_SetUserInterface(true, 115 * m_fScale);
    
    Options *opt = [Options sharedOptions];
    CGE_SetVariableInt(ecgevarWalkTouchCtl, opt.walkTwoControls ? ecgetciCtlWalkCtlRotate : ecgetciCtlWalkDragRotate);
    
    [self LoadSceneFile];

    [self update];
}

//-----------------------------------------------------------------
- (void)LoadSceneFile
{
    CGE_LoadSceneFromFile([self.fileToOpen fileSystemRepresentation]);
    
    Options *opt = [Options sharedOptions];
    CGE_SetVariableInt(ecgevarWalkHeadBobbing, opt.walkHeadBobbing ? 1 : 0);
    CGE_SetVariableInt(ecgevarEffectSSAO, opt.ssao ? 1 : 0);

    m_nViewpointCount = CGE_GetViewpointsCount();
    m_nCurrentViewpoint = 0;
    [self updateViewpointButtons];
}

//-----------------------------------------------------------------
- (void)tearDownGL
{
    [EAGLContext setCurrentContext:self.context];
    
    CGE_Close();
}

#pragma mark - GLKView and GLKViewController delegate methods
//-----------------------------------------------------------------
- (void)update
{
    // set and update the geometry
    int nViewSizeX = self.view.bounds.size.width;
    int nViewSizeY = self.view.bounds.size.height;
    
    CGE_Resize(nViewSizeX*m_fScale, nViewSizeY*m_fScale);
    
    // send accumulated touch positions (sending them right away jams the engine)
#ifdef USE_GESTURE_RECOGNIZERS
    if (m_bIsPanning)
        CGE_Motion(m_ptPanningMousePos.x, m_ptPanningMousePos.y, 0);
#else
    for (NSInteger i = 0; i < MAX_TOUCHES; i++)
    {
        if (m_arrTouches[i] == nil) continue;
        
        CGPoint pt = [m_arrTouches[i] locationInView:self.view];
        [self RecalcTouchPosForCGE:&pt];
        CGE_Motion(pt.x, pt.y, i);
    }

#endif
    
    CGE_Update();
}

//-----------------------------------------------------------------
- (void)glkView:(GLKView *)view drawInRect:(CGRect)rect
{
    CGE_Render();
}

#pragma mark - gestures

//-----------------------------------------------------------------
- (void)RecalcTouchPosForCGE:(CGPoint *)pt
{
    pt->x*=m_fScale; pt->y*=m_fScale;
    pt->y = self.view.bounds.size.height*m_fScale - 1 - pt->y;
}

//-----------------------------------------------------------------
- (IBAction)OnPanGesture:(UIPanGestureRecognizer *)sender
{
    // simulate sending PC mouse messages to the engine
    
    CGPoint pt = [sender locationInView:sender.view];
    [self RecalcTouchPosForCGE:&pt];
    m_ptPanningMousePos = pt;
    
    if (sender.state == UIGestureRecognizerStateBegan)
    {
        m_bIsPanning = true;
        CGE_MouseDown(pt.x, pt.y, true, 0);
    }
    else if (sender.state == UIGestureRecognizerStateChanged)
    {
        // note: mouse move is called in update function. Calling it too frequently jams the engine.
        //CGE_Motion(pt.x, pt.y, 0);
    }
    else if (sender.state == UIGestureRecognizerStateEnded)
    {
        CGE_MouseUp(pt.x, pt.y, true, 0);
        m_bIsPanning = false;
    }
}

//-----------------------------------------------------------------
- (IBAction)OnTouchGesture:(UITapGestureRecognizer *)sender
{
    if (sender.state != UIGestureRecognizerStateEnded)
        return;
    CGPoint pt = [sender locationInView:sender.view];
    [self RecalcTouchPosForCGE:&pt];

    CGE_MouseDown(pt.x, pt.y, true, 0);
    CGE_MouseUp(pt.x, pt.y, true, 0);
}

//-----------------------------------------------------------------
- (IBAction)OnPinchGesture:(UIPinchGestureRecognizer *)sender
{
    if (sender.state != UIGestureRecognizerStateChanged)
        return;
    
    float fZDelta = 0;
    CGFloat fRecScale = sender.scale;
    if (fRecScale > 1.0f)
        fZDelta = (fRecScale - 1.0f) * 80;
    else if (fRecScale < 1.0f)
        fZDelta = (1.0/fRecScale - 1.0f) * -80;
    
    sender.scale = 1.0;
    
    CGE_MouseWheel(fZDelta*20, true);
}

#ifndef USE_GESTURE_RECOGNIZERS
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
        CGE_MouseDown(pt.x, pt.y, true, nFingerIdx);
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
        CGE_MouseUp(pt.x, pt.y, true, nFingerIdx);
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
        CGE_MouseUp(pt.x, pt.y, true, nFingerIdx);
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
        CGE_Motion(pt.x, pt.y, nFingerIdx);
    }*/
    
    [super touchesMoved:touches withEvent:event];
}
#endif  // not USE_GESTURE_RECOGNIZERS

#pragma mark - interface

//-----------------------------------------------------------------
- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
    return YES;
}

//-----------------------------------------------------------------
- (void)updateViewpointButtons
{
    m_btnViewpointPrev.enabled = (m_nCurrentViewpoint > 0);
    m_btnViewpointNext.enabled = (m_nCurrentViewpoint < m_nViewpointCount-1);
    
    enum ECgeNavigationType eNav = CGE_GetNavigationType();
    int nSegment;
    switch (eNav) {
        case ecgenavWalk: nSegment = 0; break;
        case ecgenavFly:  nSegment = 1; break;
        case ecgenavExamine:  nSegment = 2; break;
        case ecgenavTurntable:  nSegment = 3; break;
        default:          nSegment = 2; break;
    }
    m_segmNavigation.selectedSegmentIndex = nSegment;
}

//-----------------------------------------------------------------
- (void)OnNavigationSegmentChanged:(id)sender
{
    int nSegment = m_segmNavigation.selectedSegmentIndex;
    enum ECgeNavigationType eNav;
    switch (nSegment) {
        case 0: eNav = ecgenavWalk; break;
        case 1: eNav = ecgenavFly; break;
        case 2: eNav = ecgenavExamine; break;
        case 3: eNav = ecgenavTurntable; break;
        default: eNav = ecgenavExamine; break;
    }
    CGE_SetNavigationType(eNav);
}

//-----------------------------------------------------------------
- (void)OnBtnViewpointPrev:(id)sender
{
    if (m_nCurrentViewpoint > 0)
    {
        m_nCurrentViewpoint--;
        CGE_MoveToViewpoint(m_nCurrentViewpoint, true);
        [self updateViewpointButtons];
    }
}

//-----------------------------------------------------------------
- (void)OnBtnViewpointNext:(id)sender
{
    if (m_nCurrentViewpoint < m_nViewpointCount-1)
    {
        m_nCurrentViewpoint++;
        CGE_MoveToViewpoint(m_nCurrentViewpoint, true);
        [self updateViewpointButtons];
    }
}

//-----------------------------------------------------------------
- (void)OnBtnViewpointPopup:(id)sender
{
    if (m_currentPopover!=nil) return;
    
    // show popover
    UIStoryboard *storyboard = [UIStoryboard storyboardWithName:@"Main" bundle:nil];
    m_viewpointsController = [storyboard instantiateViewControllerWithIdentifier:@"ViewpointsPopover"];
    m_currentPopover = [[UIPopoverController alloc] initWithContentViewController:m_viewpointsController];
    
    // fill with viewpoint names
    NSMutableArray *arrViewpoints = [[NSMutableArray alloc] initWithCapacity:m_nViewpointCount];
    for (int i = 0; i < m_nViewpointCount; i++)
    {
        char szName[64];
        szName[0] = 0;
        CGE_GetViewpointName(i, szName, 64);
        NSString *sName = [NSString stringWithUTF8String:szName];
        [arrViewpoints addObject:sName];
    }

    m_viewpointsController.arrayViewpoints = arrViewpoints;
    m_viewpointsController.selectedViewpoint = m_nCurrentViewpoint;
    m_viewpointsController.popover = m_currentPopover;
    
    [m_currentPopover setDelegate:self];
    [m_currentPopover presentPopoverFromBarButtonItem:sender permittedArrowDirections:UIPopoverArrowDirectionAny animated:YES];
}

//-----------------------------------------------------------------
- (void)popoverControllerDidDismissPopover:(UIPopoverController *)popoverController
{
    if (popoverController!=m_currentPopover)
        return;
    m_currentPopover = nil;
    
    if (m_viewpointsController!=nil)    // viewpoint selected
    {
        m_nCurrentViewpoint = m_viewpointsController.selectedViewpoint;
        CGE_MoveToViewpoint(m_nCurrentViewpoint, true);
        [self updateViewpointButtons];
        
        m_viewpointsController = nil;
    }
    
    if (m_fileOpenController!=nil)      // scene file selected
    {
        NSString *sFile = m_fileOpenController.selectedFile;
        m_fileOpenController = nil;
        
        if (sFile!=nil && sFile.length > 0)
        {
            self.fileToOpen = sFile;
            [self LoadSceneFile];
        }
    }
}

//-----------------------------------------------------------------
- (void)OnBtnOpenFile:(id)sender
{
    if (m_currentPopover!=nil) return;
    
    // show popover
    UIStoryboard *storyboard = [UIStoryboard storyboardWithName:@"Main" bundle:nil];
    m_fileOpenController = [storyboard instantiateViewControllerWithIdentifier:@"FileOpenPopover"];
    m_currentPopover = [[UIPopoverController alloc] initWithContentViewController:m_fileOpenController];
    
    m_fileOpenController.popover = m_currentPopover;
    
    [m_currentPopover setDelegate:self];
    [m_currentPopover presentPopoverFromBarButtonItem:sender permittedArrowDirections:UIPopoverArrowDirectionAny animated:YES];
}

//-----------------------------------------------------------------
- (void)OnBtnOptions:(id)sender
{
    if (m_currentPopover!=nil) return;
    
    UIStoryboard *storyboard = [UIStoryboard storyboardWithName:@"Main" bundle:nil];
    UIViewController *viewctl = [storyboard instantiateViewControllerWithIdentifier:@"OptionsPopover"];
    m_currentPopover = [[UIPopoverController alloc] initWithContentViewController:viewctl];
    
    [m_currentPopover setDelegate:self];
    [m_currentPopover presentPopoverFromBarButtonItem:sender permittedArrowDirections:UIPopoverArrowDirectionAny animated:YES];
}

//-----------------------------------------------------------------
- (void)OnBtnInfo:(id)sender
{
    int nInfoTextMax = 4096;
    char *szInfo = malloc(nInfoTextMax);
    szInfo[0] = 0;
    CGE_GetOpenGLInformation(szInfo, nInfoTextMax);
    free(szInfo);
    
    NSString *sInfo = [NSString stringWithUTF8String:szInfo];
    
    UIStoryboard *storyboard = [UIStoryboard storyboardWithName:@"Main" bundle:nil];
    UINavigationController *pNavCtl = [storyboard instantiateViewControllerWithIdentifier:@"OpenGLInfoNav"];
    ViewInfoController *infoCtl = (ViewInfoController*)[pNavCtl topViewController];
    infoCtl.infoText = sInfo;
    pNavCtl.modalPresentationStyle = UIModalPresentationFormSheet;
    
    [self presentViewController:pNavCtl animated:YES completion:nil];
}

@end
