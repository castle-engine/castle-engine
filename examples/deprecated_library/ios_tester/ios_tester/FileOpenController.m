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

#import "FileOpenController.h"

@interface FileOpenController ()
{
    NSMutableArray *m_arrayFiles;
}
@end

//-----------------------------------------------------------------
@implementation FileOpenController

- (void)viewDidLoad
{
    [super viewDidLoad];
    
    self.navigationItem.rightBarButtonItem = [[UIBarButtonItem alloc] initWithBarButtonSystemItem:UIBarButtonSystemItemCancel target:self action:@selector(OnBtnCancel:)];
    
    m_arrayFiles = [[NSMutableArray alloc] init];
    
    // documnets folder
    NSFileManager *fm = [NSFileManager defaultManager];
    NSArray *paths = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES);
    NSString *sFolder = [paths objectAtIndex:0];
    NSArray *dirContents = [fm contentsOfDirectoryAtPath:sFolder error:nil];
    for (NSString *item in dirContents)
    {
        NSString *sFileExt = item.pathExtension;
        if ([sFileExt isEqualToString:@"wrl"]
            || [sFileExt isEqualToString:@"x3dv"]
            || [sFileExt isEqualToString:@"x3d"])
            [m_arrayFiles addObject:[sFolder stringByAppendingPathComponent:item]];
    }
    
    // sampledata folder
    NSString *sBundlePath = [[NSBundle mainBundle] bundlePath];
    sFolder = [sBundlePath stringByAppendingPathComponent:@"sampledata"];
    dirContents = [fm contentsOfDirectoryAtPath:sFolder error:nil];
    for (NSString *item in dirContents)
    {
        NSString *sFileExt = item.pathExtension;
        if ([sFileExt isEqualToString:@"wrl"]
            || [sFileExt isEqualToString:@"x3dv"]
            || [sFileExt isEqualToString:@"x3d"])
            [m_arrayFiles addObject:[sFolder stringByAppendingPathComponent:item]];
    }

    // regiser to set content size as compact as possible
    [self.tableView addObserver:self forKeyPath:@"contentSize" options: NSKeyValueObservingOptionNew context: nil];
}

//-----------------------------------------------------------------
- (void)observeValueForKeyPath:(NSString *)keyPath ofObject:(id)object change:(NSDictionary<NSKeyValueChangeKey,id> *)change context:(void *)context
{
    if ([keyPath isEqualToString:@"contentSize"] && object == self.tableView)
        self.preferredContentSize = self.tableView.contentSize;
}

//-----------------------------------------------------------------
- (void)viewDidDisappear:(BOOL)animated
{
    [self.tableView removeObserver:self forKeyPath:@"contentSize"];
}

//-----------------------------------------------------------------
- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{
    return 1;
}

//-----------------------------------------------------------------
- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    return m_arrayFiles.count;
}

//-----------------------------------------------------------------
- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:@"FileOpenCell"];
    cell.textLabel.text = [[m_arrayFiles objectAtIndex:indexPath.row] lastPathComponent];
    return cell;
}

//-----------------------------------------------------------------
- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath
{
    if (self.delegate != nil)
        [self.delegate fileSelectedToOpen:[m_arrayFiles objectAtIndex:indexPath.row]];
    [self dismissViewControllerAnimated:YES completion:nil];
}

//-----------------------------------------------------------------
- (void)OnBtnCancel:(id)sender
{
    [self dismissViewControllerAnimated:YES completion:nil];
}

@end
