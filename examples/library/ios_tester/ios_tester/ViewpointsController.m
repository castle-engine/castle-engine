//
//  ViewpointsController.m
//  CastleEngineTest
//
//  Created by Jan Adamec on 18.11.13.
//  Copyright (c) 2013 Jan Adamec. All rights reserved.
//

#import "ViewpointsController.h"

@implementation ViewpointsController

- (void)viewDidLoad
{
    
}

//-----------------------------------------------------------------
- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{
    return 1;
}

//-----------------------------------------------------------------
- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    return [self.arrayViewpoints count];
}

//-----------------------------------------------------------------
- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:@"ViewpointTableCell"];
    cell.textLabel.text = [self.arrayViewpoints objectAtIndex:indexPath.row];
    cell.accessoryType = (indexPath.row == self.selectedViewpoint) ? UITableViewCellAccessoryCheckmark : UITableViewCellAccessoryNone;
    return cell;
}

//-----------------------------------------------------------------
- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath
{
    self.selectedViewpoint = indexPath.row;
    [self.popover dismissPopoverAnimated:YES];
    [self.popover.delegate popoverControllerDidDismissPopover:self.popover]; // call delegate programatically in order to get the data from here
}


@end
