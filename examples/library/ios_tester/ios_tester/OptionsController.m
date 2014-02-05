//
//  OptionsController.m
//  ios_tester
//
//  Created by Jan Adamec on 05.02.14.
//  Copyright (c) 2014 Jan Adamec. All rights reserved.
//

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
    return 2;
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
        case 0: sText = @"Walk Head Bobbing";
            bSwitchOn = opt.walkHeadBobbing;
            break;
        case 1: sText = @"SSAO";
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
        case 0: opt.walkHeadBobbing = bOn; CGE_SetWalkHeadBobbing(bOn); break;
        case 1: opt.ssao = bOn; CGE_SetEffectSsao(bOn); break;
            
        default:
            break;
    }
}

@end
