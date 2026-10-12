{
  Copyright 2023-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
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

uses Types,
  FMX.Dialogs, FMX.Types, FMX.Graphics,
  CastleFileFilters, CastleImages;

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

{ Convert FMX bitmap to a new Castle Game Engine image.
  The caller is responsible for freeing the returned image
  (or passing it to something that takes the ownership,
  like @link(TImageTextureNode.LoadFromImage) with TakeImageOwnership = @true).

  This is useful e.g. to use FMX camera frames
  (from FMX TCameraComponent.SampleBufferToBitmap) as a texture in a viewport.
  See examples/delphi/fmx_camera and examples/delphi/window_camera.

  Any pixel format of the FMX bitmap is handled.
  Note that we don't do anything special about the alpha premultiplication,
  so this is most suitable for opaque bitmaps. }
function BitmapToCastleImage(const Bitmap: TBitmap): TRGBAlphaImage;

{$ifdef LINUX}
{ Set mouse position, in screen coordinates.
  WidgetNativeHandle is a GTK widget pointer, used to determine
  display and screen where to set the pointer. }
procedure FmxSetMousePos(const WidgetNativeHandle: Pointer;
  const Point: TPointF);
{$endif}

implementation

uses
  SysUtils, CTypes, CastleLog;

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

{ Convert a row of pixels from any FMX pixel format to RGBA (8 bits per channel).

  This is like FMX.Types.ChangePixelFormat, but ChangePixelFormat is not
  available in older Delphi versions (like 10.2), while
  PixelToAlphaColor and AlphaColorToPixel used here are. }
procedure ConvertScanlineToRgba(const Source, Dest: Pointer;
  const PixelCount: Integer; const SourceFormat: TPixelFormat);
var
  SourcePixel, DestPixel: PByte;
  SourcePixelSize, I: Integer;
begin
  SourcePixelSize := PixelFormatBytes[SourceFormat];
  if SourcePixelSize < 1 then
    raise Exception.Create('Cannot convert pixels of FMX bitmap, unsupported pixel format');

  SourcePixel := Source;
  DestPixel := Dest;
  for I := 0 to PixelCount - 1 do
  begin
    AlphaColorToPixel(PixelToAlphaColor(SourcePixel, SourceFormat),
      DestPixel, TPixelFormat.RGBA);
    Inc(SourcePixel, SourcePixelSize);
    Inc(DestPixel, 4);
  end;
end;

function BitmapToCastleImage(const Bitmap: TBitmap): TRGBAlphaImage;
var
  Data: TBitmapData;
  Y: Integer;
  Source, Dest: Pointer;
begin
  if (Bitmap.Width = 0) or (Bitmap.Height = 0) then
    Exit(TRGBAlphaImage.Create(0, 0));

  if not Bitmap.Map(TMapAccess.Read, Data) then
    raise Exception.Create('Cannot access the pixels of FMX bitmap');
  try
    Result := TRGBAlphaImage.Create(Data.Width, Data.Height);
    try
      for Y := 0 to Data.Height - 1 do
      begin
        Source := Data.GetScanline(Y);
        // FMX bitmap rows go from the top, our image rows go from the bottom
        Dest := Result.RowPtr(Data.Height - 1 - Y);
        if Data.PixelFormat = TPixelFormat.RGBA then
          // formats of TBitmapData and TRGBAlphaImage are equal, copy fast
          Move(Source^, Dest^, Data.Width * 4)
        else
          ConvertScanlineToRgba(Source, Dest, Data.Width, Data.PixelFormat);
      end;
    except
      FreeAndNil(Result);
      raise;
    end;
  finally
    Bitmap.Unmap(Data);
  end;
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