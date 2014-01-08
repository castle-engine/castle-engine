//
//  OpenGLController.m
//  CastleEngineTest
//
//  Created by Jan Adamec on 18.11.13.
//  Copyright (c) 2013 Jan Adamec. All rights reserved.
//

#import "OpenGLController.h"
#import "ViewInfoController.h"
#include "castleengine.h"

@interface OpenGLController ()
{
    bool m_bIsPanning;
    CGPoint m_ptPanningMousePos;
    CGFloat m_fScale;
    
    UISegmentedControl *m_segmNavigation;
    UIBarButtonItem *m_btnViewpointPopup;
    UIBarButtonItem *m_btnViewpointPrev;
    UIBarButtonItem *m_btnViewpointNext;
    
    int m_nViewpointCount, m_nCurrentViewpoint;
    
}

@property (strong, nonatomic) EAGLContext *context;

@end


@implementation OpenGLController

- (void)viewDidLoad
{
    [super viewDidLoad];
    
    m_nViewpointCount = 0;
    
    self.context = [[EAGLContext alloc] initWithAPI:kEAGLRenderingAPIOpenGLES2];
    
    if (!self.context) {
        NSLog(@"Failed to create ES context");
    }
    
    GLKView *view = (GLKView *)self.view;
    view.context = self.context;
    
    [self setupGL];
}

- (void)dealloc
{
    [self tearDownGL];
    
    if ([EAGLContext currentContext] == self.context) {
        [EAGLContext setCurrentContext:nil];
    }
}

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
        self.fileToOpen = [sBundlePath stringByAppendingPathComponent:@"sampledata/Image3D.wrl"];
    }
    
    CGE_Open();
    CGE_SetUserInterface(true, 115 * m_fScale);
    CGE_LoadSceneFromFile([self.fileToOpen fileSystemRepresentation]);
    
    m_nViewpointCount = CGE_GetViewpointsCount();
    m_nCurrentViewpoint = 0;
    [self createViewerControls];
    
    [self update];
}

- (void)tearDownGL
{
    [EAGLContext setCurrentContext:self.context];
    
    CGE_Close();
}

#pragma mark - GLKView and GLKViewController delegate methods

- (void)update
{
    // set and update the geometry
    int nViewSizeX = self.view.bounds.size.width;
    int nViewSizeY = self.view.bounds.size.height;
    
    CGE_Resize(nViewSizeX*m_fScale, nViewSizeY*m_fScale);
    if (m_bIsPanning)
        CGE_MouseMove(m_ptPanningMousePos.x, m_ptPanningMousePos.y);
    CGE_Update();
}

- (void)glkView:(GLKView *)view drawInRect:(CGRect)rect
{
    CGE_Render();
}

#pragma mark - gestures
// simulate sending PC mouse messages to the engine

- (IBAction)OnPanGesture:(UIPanGestureRecognizer *)sender
{
    CGPoint pt = [sender locationInView:sender.view];
    pt.x*=m_fScale; pt.y*=m_fScale;
    m_ptPanningMousePos = pt;
    
    if (sender.state == UIGestureRecognizerStateBegan)
    {
        m_bIsPanning = true;
        CGE_MouseDown(pt.x, pt.y, true);
    }
    else if (sender.state == UIGestureRecognizerStateChanged)
    {
        // note: mouse move is called in update function. Calling it too frequently jams the engine.
        //CGE_MouseMove(pt.x, pt.y);
    }
    else if (sender.state == UIGestureRecognizerStateEnded)
    {
        CGE_MouseUp(pt.x, pt.y, true);
        m_bIsPanning = false;
    }
}

- (IBAction)OnTouchGesture:(UITapGestureRecognizer *)sender
{
    if (sender.state != UIGestureRecognizerStateEnded)
        return;
    CGPoint pt = [sender locationInView:sender.view];
    pt.x*=m_fScale; pt.y*=m_fScale;

    CGE_MouseDown(pt.x, pt.y, true);
    CGE_MouseUp(pt.x, pt.y, true);
}

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

#pragma mark - interface

//-----------------------------------------------------------------
- (void)createViewerControls
{
    m_segmNavigation = [[UISegmentedControl alloc] initWithItems:@[@"Walk", @"Fly", @"Examine"]];
    m_segmNavigation.selectedSegmentIndex = 0;
    [m_segmNavigation addTarget:self
                         action:@selector(OnNavigationSegmentChanged:)
               forControlEvents:UIControlEventValueChanged];
    
    UIBarButtonItem *itemSegm = [[UIBarButtonItem alloc] initWithCustomView:m_segmNavigation];
                             
    m_btnViewpointPrev = [[UIBarButtonItem alloc] initWithTitle:@" < " style:UIBarButtonItemStyleBordered target:self action:@selector(OnBtnViewpointPrev:)];
    m_btnViewpointNext = [[UIBarButtonItem alloc] initWithTitle:@" > " style:UIBarButtonItemStyleBordered target:self action:@selector(OnBtnViewpointNext:)];
    m_btnViewpointPopup = [[UIBarButtonItem alloc] initWithTitle:@"Viewpoints" style:UIBarButtonItemStyleBordered target:self action:@selector(OnBtnViewpointPopup:)];
    
    UIButton *btnInfo = [UIButton buttonWithType:UIButtonTypeInfoLight];
    [btnInfo addTarget:self action:@selector(OnBtnInfo:) forControlEvents:UIControlEventTouchUpInside];
    UIBarButtonItem *btnBarInfo =[[UIBarButtonItem alloc] initWithCustomView:btnInfo];
    
    self.navigationItem.rightBarButtonItems = @[btnBarInfo, m_btnViewpointNext, m_btnViewpointPopup, m_btnViewpointPrev, itemSegm];
    
    [self updateViewpointButtons];
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
        default: eNav = ecgenavTurntable; break;
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
    
    if (m_viewpointsController!=nil)
    {
        m_nCurrentViewpoint = m_viewpointsController.selectedViewpoint;
        CGE_MoveToViewpoint(m_nCurrentViewpoint, true);
        [self updateViewpointButtons];
        
        m_viewpointsController = nil;
    }
}

//-----------------------------------------------------------------
- (void)OnBtnInfo:(id)sender
{
    [self performSegueWithIdentifier:@"SegueOpenGLInfo" sender:self];
}

//-----------------------------------------------------------------
- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender
{
    if ([segue.identifier isEqualToString:@"SegueOpenGLInfo"])
    {
        int nInfoTextMax = 4096;
        char *szInfo = malloc(nInfoTextMax);
        szInfo[0] = 0;
        CGE_GetOpenGLInformation(szInfo, nInfoTextMax);
        free(szInfo);

        NSString *sInfo = [NSString stringWithUTF8String:szInfo];
        
        UINavigationController *destCtl = segue.destinationViewController;
        ViewInfoController *infoCtl = (ViewInfoController*)destCtl.topViewController;
        infoCtl.infoText = sInfo;
    }
}

@end
