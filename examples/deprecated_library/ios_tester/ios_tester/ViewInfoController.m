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

- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)toInterfaceOrientation
{
    return YES;
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
