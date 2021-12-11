/*
  Copyright 2013-2017 Jan Adamec, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
*/

#import "OpenGLController.h"
#import "FileOpenController.h"
#import "ViewInfoController.h"
#import "ViewpointsController.h"
#import "Options.h"
#include "castleengine.h"

#define MAX_TOUCHES 12

//#define USE_GESTURE_RECOGNIZERS

@interface OpenGLController () <ViewpointCtlDelegate, FileOpenCtlDelegate>
{
    bool m_bIsPanning;
    CGPoint m_ptPanningMousePos;
    CGFloat m_fScale;

    UITouch* m_arrTouches[MAX_TOUCHES];

    UISegmentedControl *m_segmNavigation;
    UIBarButtonItem *m_btnViewpointPrev;
    UIBarButtonItem *m_btnViewpointNext;

    int m_nViewpointCount, m_nCurrentViewpoint;

    int m_oldViewWidth;
    int m_oldViewHeight;
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
    m_segmNavigation.selectedSegmentIndex = 0;
    [m_segmNavigation addTarget:self
                         action:@selector(OnNavigationSegmentChanged:)
               forControlEvents:UIControlEventValueChanged];

    UIBarButtonItem *itemSegm = [[UIBarButtonItem alloc] initWithCustomView:m_segmNavigation];

    m_btnViewpointPrev = [[UIBarButtonItem alloc] initWithTitle:@" < " style:UIBarButtonItemStylePlain target:self action:@selector(OnBtnViewpointPrev:)];
    m_btnViewpointNext = [[UIBarButtonItem alloc] initWithTitle:@" > " style:UIBarButtonItemStylePlain target:self action:@selector(OnBtnViewpointNext:)];
    UIBarButtonItem *btnViewpointPopup = [[UIBarButtonItem alloc] initWithTitle:@"Viewpoints" style:UIBarButtonItemStylePlain target:self action:@selector(OnBtnViewpointPopup:)];

    UIBarButtonItem *btnOptions = [[UIBarButtonItem alloc] initWithTitle:@"Options" style:UIBarButtonItemStylePlain target:self action:@selector(OnBtnOptions:)];

    UIButton *btnInfo = [UIButton buttonWithType:UIButtonTypeInfoLight];
    [btnInfo addTarget:self action:@selector(OnBtnInfo:) forControlEvents:UIControlEventTouchUpInside];
    UIBarButtonItem *btnBarInfo =[[UIBarButtonItem alloc] initWithCustomView:btnInfo];

    UIBarButtonItem *btnOpenFile = [[UIBarButtonItem alloc] initWithTitle:@"Open" style:UIBarButtonItemStylePlain target:self action:@selector(OnBtnOpenFile:)];

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

    m_oldViewWidth  = self.view.bounds.size.width;
    m_oldViewHeight = self.view.bounds.size.height;

    // Get a directory where we can write files,
    // see http://stackoverflow.com/questions/1567134/how-can-i-get-a-writable-path-on-the-iphone/1567147#1567147
    NSArray *paths = NSSearchPathForDirectoriesInDomains(NSLibraryDirectory, NSUserDomainMask, YES);
    NSString *libraryDirectory = [paths objectAtIndex:0];

    CGE_Initialize([libraryDirectory fileSystemRepresentation]);
    CGE_Open(ecgeofLog, m_oldViewWidth * m_fScale, m_oldViewHeight * m_fScale, (unsigned)(dpi * m_fScale));
    CGE_SetUserInterface(true);

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
    CGE_Finalize();
}

#pragma mark - GLKView and GLKViewController delegate methods
//-----------------------------------------------------------------
- (void)update
{
    // update the viewport size, if changed
    int newViewWidth  = self.view.bounds.size.width;
    int newViewHeight = self.view.bounds.size.height;
    if (m_oldViewWidth  != newViewWidth ||
        m_oldViewHeight != newViewHeight)
    {
	m_oldViewWidth  = newViewWidth;
	m_oldViewHeight = newViewHeight;
	CGE_Resize(newViewWidth * m_fScale, newViewHeight * m_fScale);
    }

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
        CGE_Motion(pt.x, pt.y, (int)i);
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
        CGE_MouseUp(pt.x, pt.y, true, 0, false);
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
    CGE_MouseUp(pt.x, pt.y, true, 0, false);
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
        CGE_MouseDown(pt.x, pt.y, true, (int)nFingerIdx);
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
        CGE_MouseUp(pt.x, pt.y, true, (int)nFingerIdx, false);
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
        CGE_MouseUp(pt.x, pt.y, true, (int)nFingerIdx, false);
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
    NSInteger nSegment = m_segmNavigation.selectedSegmentIndex;
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
    UIStoryboard *storyboard = [UIStoryboard storyboardWithName:@"Main" bundle:nil];
    ViewpointsController* viewpointsController = [storyboard instantiateViewControllerWithIdentifier:@"ViewpointsPopover"];

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

    viewpointsController.arrayViewpoints = arrViewpoints;
    viewpointsController.selectedViewpoint = m_nCurrentViewpoint;
    viewpointsController.delegate = self;

    if (UIDevice.currentDevice.userInterfaceIdiom == UIUserInterfaceIdiomPad)
    {
        viewpointsController.modalPresentationStyle = UIModalPresentationPopover;
        viewpointsController.popoverPresentationController.barButtonItem = sender;
        [self presentViewController:viewpointsController animated:YES completion:nil];
    }
    else
    {
        UINavigationController *navCtl = [[UINavigationController alloc] initWithRootViewController:viewpointsController];
        [self presentViewController:navCtl animated:YES completion:nil];
    }
}

//-----------------------------------------------------------------
- (void)viewpointDidChange:(int)nNewViewpoint
{
    m_nCurrentViewpoint = nNewViewpoint;
    CGE_MoveToViewpoint(m_nCurrentViewpoint, true);
    [self updateViewpointButtons];
}

//-----------------------------------------------------------------
- (void)OnBtnOpenFile:(id)sender
{
    UIStoryboard *storyboard = [UIStoryboard storyboardWithName:@"Main" bundle:nil];
    FileOpenController* fileOpenController = [storyboard instantiateViewControllerWithIdentifier:@"FileOpenPopover"];
    fileOpenController.delegate = self;

    if (UIDevice.currentDevice.userInterfaceIdiom == UIUserInterfaceIdiomPad)
    {
        fileOpenController.modalPresentationStyle = UIModalPresentationPopover;
        fileOpenController.popoverPresentationController.barButtonItem = sender;
        [self presentViewController:fileOpenController animated:YES completion:nil];
    }
    else
    {
        UINavigationController *navCtl = [[UINavigationController alloc] initWithRootViewController:fileOpenController];
        [self presentViewController:navCtl animated:YES completion:nil];
    }
}

//-----------------------------------------------------------------
- (void)fileSelectedToOpen:(NSString *)selectedFile
{
    if (selectedFile != nil && selectedFile.length > 0)
    {
        self.fileToOpen = selectedFile;
        [self LoadSceneFile];
    }
}

//-----------------------------------------------------------------
- (void)OnBtnOptions:(id)sender
{
    UIStoryboard *storyboard = [UIStoryboard storyboardWithName:@"Main" bundle:nil];
    UIViewController* ctl = [storyboard instantiateViewControllerWithIdentifier:@"OptionsPopover"];

    if (UIDevice.currentDevice.userInterfaceIdiom == UIUserInterfaceIdiomPad)
    {
        ctl.modalPresentationStyle = UIModalPresentationPopover;
        ctl.popoverPresentationController.barButtonItem = sender;
        [self presentViewController:ctl animated:YES completion:nil];
    }
    else
    {
        UINavigationController *navCtl = [[UINavigationController alloc] initWithRootViewController:ctl];
        [self presentViewController:navCtl animated:YES completion:nil];
    }
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
