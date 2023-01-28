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

#import "ViewpointsController.h"

@implementation ViewpointsController

- (void)viewDidLoad
{
    [super viewDidLoad];
    
    self.navigationItem.rightBarButtonItem = [[UIBarButtonItem alloc] initWithBarButtonSystemItem:UIBarButtonSystemItemCancel target:self action:@selector(OnBtnCancel:)];
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
    if (self.delegate != nil)
        [self.delegate viewpointDidChange:(int)indexPath.row];
    [self dismissViewControllerAnimated:YES completion:nil];
}

//-----------------------------------------------------------------
- (void)OnBtnCancel:(id)sender
{
    [self dismissViewControllerAnimated:YES completion:nil];
}

@end
