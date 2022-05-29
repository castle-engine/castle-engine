{
  Copyright 2022-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Implementation of CastleWindow.BackendFileDialog.

  Lots of inspiration from LCL Cocoa widgetset code.
}

function TCastleWindow.BackendFileDialog(const Title: string; var FileName: string;
  OpenDialog: boolean; FileFilters: TFileFilterList): boolean;

{ Implementation based on LCL TCocoaWSFileDialog.ShowModal }

var
  openDlg: NSOpenPanel;
  saveDlg: NSSavePanel;
  InitName, InitDir: string;

  // filter accessory view
  // accessoryView: NSView;
  // lFilter: TCocoaFilterComboBox;

(*
  // setup panel and its accessory view
  procedure CreateAccessoryView(AOpenOwner: NSOpenPanel; ASaveOwner: NSSavePanel);
  const
    INT_MIN_ACCESSORYVIEW_WIDTH = 300;
    OFS_HRZ = 10;
  var
    lRect: NSRect;
    lText: NSTextField;
    lTextStr: NSString;
    lDialogView: NSView;
    lAccessoryWidth: Integer = INT_MIN_ACCESSORYVIEW_WIDTH;
    w: Integer;
    nw: Integer;
    fw: Integer;
  begin
    // check if the accessory is necessary
    if FileDialog.Filter = '' then Exit;

    // try to obtain the dialog size
    lDialogView := NSView(ASaveOwner.contentView);
    if lDialogView <> nil then
    begin
      if (NSAppkitVersionNumber >= NSAppKitVersionNumber11_0) then
        // starting with Big Sur, the dialog retains the last openned size
        // causing the width to be increased on every openning of the dialog
        // we'd simply force the lAccessoryWidth to start with the minimum width
        lAccessoryWidth := INT_MIN_ACCESSORYVIEW_WIDTH
      else if lDialogView.frame.size.width > INT_MIN_ACCESSORYVIEW_WIDTH then
        lAccessoryWidth := Round(lDialogView.frame.size.width);
    end;
    lRect := GetNSRect(0, 0, lAccessoryWidth, 30);
    accessoryView := NSView.alloc.initWithFrame(lRect);

    // "Format:" label
    lText := NSTextField.alloc.initWithFrame(NSNullRect);
    {$ifdef BOOLFIX}
    lText.setBezeled_(Ord(False));
    lText.setDrawsBackground_(Ord(False));
    lText.setEditable_(Ord(False));
    lText.setSelectable_(Ord(False));
    {$else}
    lText.setBezeled(False);
    lText.setDrawsBackground(False);
    lText.setEditable(False);
    lText.setSelectable(False);
    {$endif}
    lTextStr := NSStringUTF8(rsMacOSFileFormat);
    lText.setStringValue(lTextStr);
    lText.setFont(NSFont.systemFontOfSize(NSFont.systemFontSizeForControlSize(NSRegularControlSize)));
    lText.sizeToFit;

    // Combobox
    lFilter := TCocoaFilterComboBox.alloc.initWithFrame(NSNullRect);
    lFilter.IsOpenDialog := AOpenOwner <> nil;
    if lFilter.IsOpenDialog then
      lFilter.DialogHandle := AOpenOwner
    else
      lFilter.DialogHandle := ASaveOwner;
    lFilter.Owner := FileDialog;
    lFilter.setTarget(lFilter);
    lFilter.setAction(objcselector('comboboxAction:'));
    lFilter.updateFilterList();
    if FileDialog.FilterIndex <= 0 then
      lFilter.lastSelectedItemIndex := 0
    else
      lFilter.lastSelectedItemIndex := FileDialog.FilterIndex-1;
    lFilter.lastSelectedItemIndex := lFilter.setDialogFilter(lFilter.lastSelectedItemIndex);
    lFilter.sizeToFit;
    lFilter.setAutoresizingMask(NSViewWidthSizable);
    if FileDialog.FilterIndex>0 then
      lFilter.selectItemAtIndex(FileDialog.FilterIndex-1);

    // Trying to put controls into the center of the Acc-view
    //  Label must fit in full. Whatever is left is for filter
    w:=lAccessoryWidth - OFS_HRZ - OFS_HRZ;
    fw:=Round(lFilter.frame.size.width);
    nw:=Round(lText.frame.size.width + fw + OFS_HRZ);
    if nw>w then begin
      dec(fw, nw-w);
      nw:=w;
    end;

    lText.setFrame(  NSMakeRect(
       Round((w-nw) / 2+OFS_HRZ)
       , 0
       , lText.frame.size.width
       , lFilter.frame.size.height
    ));

    lFilter.setFrame( NSMakeRect(
       lText.frame.origin.x+lText.frame.size.width+OFS_HRZ
       ,4
       ,fw
       ,lFilter.frame.size.height
      ));

    accessoryView.addSubview(lText.autorelease);
    accessoryView.addSubview(lFilter.autorelease);

    lFilter.setAutoresizingMask(NSViewWidthSizable);

    lFilter.DialogHandle.setAccessoryView(accessoryView.autorelease);
    lFilter.DialogHandle.setDelegate(lFilter);
  end;
*)

begin
  InitName := ExtractFileName(FileName);
  InitDir := ExtractFileDir(FileName);

  // TODO: FileFilters
  // Adapt LCL code? Or resign -- Cocoa doesn't have such filters natively.
  // lFilter := nil;

  if OpenDialog then
  begin
    openDlg := NSOpenPanel.openPanel;
    openDlg.setAllowsMultipleSelection(false);

    { to choose only directory we would do this:
    openDlg.setCanChooseDirectories(true);
    openDlg.setCanChooseFiles(false);
    }
    openDlg.setCanChooseFiles(true);
    openDlg.setCanChooseDirectories(false);

    // that's correct, NSOpenPanel descends from NSSavePanel and we use it
    saveDlg := openDlg;
  end else
  begin
    saveDlg := NSSavePanel.savePanel;
    saveDlg.setCanCreateDirectories(true);
    saveDlg.setNameFieldStringValue(NSSTR(InitName));
    openDlg := nil;
  end;

  saveDlg.setAccessoryView(nil);
  saveDlg.retain; // LCL comment: this is for OSX 10.6 (and we don't use ARC either)
  saveDlg.setTitle(NSSTR(Title));
  saveDlg.setDirectoryURL(NSURL.fileURLWithPath(NSSTR(InitDir)));

  MenuForcefullyDisabled := true;
  try
    Result := saveDlg.runModal = NSModalResponseOK;
    if Result then
      FileName := saveDlg.URL.path.UTF8String;

    // release everything
    saveDlg.release;
  finally
    MenuForcefullyDisabled := false;
  end;

  ref.makeKeyAndOrderFront(nil);
end;
