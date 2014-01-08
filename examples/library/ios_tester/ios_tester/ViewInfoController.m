//
//  ViewInfoController.m
//  CastleEngineTest
//
//  Created by Jan Adamec on 19.11.13.
//  Copyright (c) 2013 Jan Adamec. All rights reserved.
//

#import "ViewInfoController.h"

@interface ViewInfoController ()
{
    __weak IBOutlet UITextView *m_textView;
    
}

@end

@implementation ViewInfoController

- (void)viewDidLoad
{
    [super viewDidLoad];
    m_textView.text = self.infoText;
}

- (IBAction)OnBtnDoneTouched:(id)sender
{
    [self dismissViewControllerAnimated:YES completion:nil];
}

- (IBAction)OnBtnCopyTouched:(id)sender
{
    NSString *sText = m_textView.text;
    UIPasteboard *pb = [UIPasteboard generalPasteboard];
    [pb setString:sText];
}

@end
