{
  Copyright 2023-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Utilities for Delphi FMX (FireMonkey). }
unit CastleFmxUtils;

{$I castleconf.inc}

interface

uses Types, FMX.Dialogs,
  CastleFileFilters;

{ Convert file filters into FMX Dialog.Filter, Dialog.FilterIndex.
  Suitable for both open and save dialogs (in FMX, TSaveDialog
  descends from TOpenDialog).

  Input filters are either given as a string FileFilters
  (encoded just like for TFileFilterList.AddFiltersFromString),
  or as TFileFilterList instance.

  Output filters are set as appropriate properties of given Dialog instance.

  When AllFields is false, then filters starting with "All " in the name,
  like "All files", "All images", are not included in the output.

  @groupBegin }
procedure FileFiltersToDialog(const FileFilters: string;
  const Dialog: TOpenDialog; const AllFields: boolean = true); overload;
procedure FileFiltersToDialog(FFList: TFileFilterList;
  const Dialog: TOpenDialog; const AllFields: boolean = true); overload;
{ @groupEnd }

{$ifdef LINUX}
{ Set mouse position, in screen coordinates.
  WidgetNativeHandle is a GTK widget pointer, used to determine
  display and screen where to set the pointer. }
procedure FmxSetMousePos(const WidgetNativeHandle: Pointer;
  const Point: TPointF);
{$endif}

implementation

uses CTypes, CastleLog;

procedure FileFiltersToDialog(const FileFilters: string;
  const Dialog: TOpenDialog; const AllFields: boolean = true);
var
  OutFilter: String;
  OutFilterIndex: Integer;
begin
  TFileFilterList.LclFmxFiltersFromString(FileFilters,
    OutFilter, OutFilterIndex, AllFields);
  Dialog.Filter := OutFilter;
  Dialog.FilterIndex := OutFilterIndex;
end;

procedure FileFiltersToDialog(FFList: TFileFilterList;
  const Dialog: TOpenDialog; const AllFields: boolean = true);
var
  OutFilter: String;
  OutFilterIndex: Integer;
begin
  FFList.LclFmxFilters(OutFilter, OutFilterIndex, AllFields);
  Dialog.Filter := OutFilter;
  Dialog.FilterIndex := OutFilterIndex;
end;

{$ifdef LINUX}
type
  PGdkDevice = Pointer;
  PGdkScreen = Pointer;
  PGdkDisplay = Pointer;
  PGdkDeviceManager = Pointer;
  PGtkWidget = Pointer;

procedure gdk_device_warp(Device: PGdkDevice; Screen: PGdkScreen; X, Y: CInt); cdecl; external 'libgtk-3.so.0';

//function gdk_display_get_default: PGdkDisplay; cdecl; external 'libgtk-3.so.0';
function gtk_widget_get_display(widget: PGtkWidget): PGdkDisplay; cdecl; external 'libgtk-3.so.0';
function gdk_display_get_device_manager(display: PGdkDisplay): PGdkDeviceManager; cdecl; external 'libgtk-3.so.0';
function gdk_device_manager_get_client_pointer(manager: PGdkDeviceManager): PGdkDevice; cdecl; external 'libgtk-3.so.0';

//function gdk_screen_get_default: PGdkScreen; cdecl; external 'libgtk-3.so.0';
function gtk_widget_get_screen(widget: PGtkWidget): PGdkScreen; cdecl; external 'libgtk-3.so.0';

procedure FmxSetMousePos(const WidgetNativeHandle: Pointer;
  const Point: TPointF);
var
  Display: PGdkDisplay;
  DeviceManager: PGdkDeviceManager;
  Device: PGdkDevice;
  Screen: PGdkScreen;
begin
  { Get main device (mouse) following
    https://stackoverflow.com/questions/24844489/how-to-use-gdk-device-get-position .
    We use Display and Screen that correspond to the WidgetNativeHandle .
    Then we "warp" (set mouse position) using
    https://docs.gtk.org/gdk3/method.Device.warp.html . }

  Display := gtk_widget_get_display(WidgetNativeHandle);
  DeviceManager := gdk_display_get_device_manager(Display);
  Device := gdk_device_manager_get_client_pointer(DeviceManager);

  Screen := gtk_widget_get_screen(WidgetNativeHandle);

  //WritelnLog('Mouse', 'Warping mouse to %f %f', [Point.X, Point.Y]);
  gdk_device_warp(Device, Screen, Round(Point.X), Round(Point.Y));
end;
{$endif}

end.