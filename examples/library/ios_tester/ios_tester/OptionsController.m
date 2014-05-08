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

#import "OptionsController.h"
#import "Options.h"
#include "castleengine.h"

@implementation OptionsController

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{
    return 1;
}

//-----------------------------------------------------------------
- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    return 3;
}

//-----------------------------------------------------------------
- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:@"OptionsCell"];
    
    Options *opt = [Options sharedOptions];
    
    NSString *sText;
    bool bSwitchOn = false;
    switch (indexPath.row)
    {
        case 0: sText = @"Two Touch Controls";  // also called twin stick
            bSwitchOn = opt.walkTwoControls;
            break;
        case 1: sText = @"Walk Head Bobbing";
            bSwitchOn = opt.walkHeadBobbing;
            break;
        case 2: sText = @"SSAO";
            bSwitchOn = opt.ssao;
            break;
            
        default:
            break;
    }
    cell.textLabel.text = sText;
    
    UISwitch *chkSwitch = [[UISwitch alloc] init];
    chkSwitch.tag = indexPath.row;
    chkSwitch.on = bSwitchOn;
    [chkSwitch addTarget:self action:@selector(OnOptionSwitchChanged:) forControlEvents:UIControlEventValueChanged];
    cell.accessoryView = chkSwitch;
    return cell;
}

//-----------------------------------------------------------------
- (void)OnOptionSwitchChanged:(id)sender
{
    int nRow = [(UISwitch*)sender tag];
    bool bOn = [(UISwitch*)sender isOn];

    Options *opt = [Options sharedOptions];
    switch (nRow)
    {
        case 0:
            opt.walkTwoControls = bOn;
            CGE_SetVariableInt(ecgevarWalkTouchCtl, bOn ? ecgetciCtlWalkCtlRotate : ecgetciCtlWalkDragRotate);
            break;
        case 1: opt.walkHeadBobbing = bOn; CGE_SetVariableInt(ecgevarWalkHeadBobbing, bOn ? 1 : 0); break;
        case 2: opt.ssao = bOn; CGE_SetVariableInt(ecgevarEffectSSAO, bOn ? 1 : 0); break;
            
        default:
            break;
    }
}

@end
