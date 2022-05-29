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

{ Implementation of CastleWindow.ColorDialog.

  Lots of inspiration from LCL Cocoa widgetset code.
}

type
  TColorPanelDelegate = objcclass(NSObject, NSWindowDelegateProtocol)
  public
    colorPanel: NSColorPanel;
    didPickColor: Boolean;
    // NSWindowDelegateProtocol
    procedure windowWillClose(notification: NSNotification); message 'windowWillClose:';
    //
    procedure pickColor; message 'pickColor'; // button action
    procedure exit; message 'exit'; // button action
  end;

{ TColorPanelDelegate }

procedure TColorPanelDelegate.windowWillClose(notification: NSNotification);
begin
  NSApp.stopModal();
end;

procedure TColorPanelDelegate.pickColor();
begin
  didPickColor := true;
  exit();
end;

procedure TColorPanelDelegate.exit();
begin
  colorPanel.close();
end;

function TCastleWindow.ColorDialog(var Color: TCastleColor): boolean;

  function GetNSRect(x, y, width, height: Integer): NSRect;
  begin
    Result.origin.x := x;
    Result.origin.y := y;
    Result.size.width := width;
    Result.size.height := height;
  end;

  function NSColorFromCastle(const Color: TCastleColor): NSColor;
  begin
    Result := NSColor.colorWithDeviceRed_green_blue_alpha(
      Color.X,
      Color.Y,
      Color.Z,
      Color.W
    );
  end;

  function NSColorToCastle(Color: NSColor): TCastleColor;
  begin
    // We must convert color to device RGB to be able to read it,
    // otherwise reading e.g. grayscale results in crash.
    if Color.colorSpace <> NSColorSpace.deviceRGBColorSpace then
    begin
      Color := Color.colorUsingColorSpace(NSColorSpace.deviceRGBColorSpace);
      if Color = nil then
      begin
        WritelnWarning('Cannot convert color to RGB');
        Exit(Black);
      end;
    end;

    Result := Vector4(
      Color.redComponent,
      Color.greenComponent,
      Color.blueComponent,
      Color.alphaComponent
    );
  end;

{ Implementation based on LCL TCocoaWSColorDialog.ShowModal }

var
  colorDelegate: TColorPanelDelegate;
  colorPanel: NSColorPanel;
  // accessory view
  accessoryView: NSView;
  lRect: NSRect;
  okButton, cancelButton: NSButton;
begin
  colorPanel := NSColorPanel.sharedColorPanel();
  if (colorPanel.respondsToSelector(ObjCSelector('setRestorable:'))) then
    colorPanel.setRestorable(false);
  colorPanel.setColor(NSColorFromCastle(Color));

  colorDelegate := TColorPanelDelegate.alloc.init();
  colorDelegate.colorPanel := colorPanel;

  // setup panel and its accessory view
  lRect := GetNSRect(0, 0, 220, 30);
  accessoryView := NSView.alloc.initWithFrame(lRect);

  lRect := GetNSRect(110, 4, 110-8, 24);
  okButton := NSButton.alloc.initWithFrame(lRect);
  okButton.setButtonType(NSMomentaryPushInButton);
  okButton.setBezelStyle(NSRoundedBezelStyle);
  okButton.setTitle(NSSTR('Pick'));
  okButton.setAction(objcselector('pickColor'));
  okButton.setTarget(colorDelegate);

  lRect := GetNSRect(8, 4, 110-8, 24);
  cancelButton := NSButton.alloc.initWithFrame(lRect);
  cancelButton.setButtonType(NSMomentaryPushInButton);
  cancelButton.setBezelStyle(NSRoundedBezelStyle);
  cancelButton.setTitle(NSSTR('Cancel'));
  cancelButton.SetAction(objcselector('exit'));
  cancelButton.setTarget(colorDelegate);

  accessoryView.addSubview(okButton.autorelease);
  accessoryView.addSubview(cancelButton.autorelease);

  colorPanel.setDelegate(colorDelegate);
  colorPanel.setAccessoryView(accessoryView.autorelease);
  colorPanel.setShowsAlpha(true);
  colorPanel.setDefaultButtonCell(okButton.cell);

  MenuForcefullyDisabled := true;
  try
    colorPanel.makeKeyAndOrderFront(colorDelegate);
    NSApp.runModalForWindow(colorPanel);

    Result := colorDelegate.didPickColor;
    if Result then
      Color := NSColorToCastle(colorPanel.color);
  finally
    MenuForcefullyDisabled := false;
  end;

  ref.makeKeyAndOrderFront(nil);
end;
