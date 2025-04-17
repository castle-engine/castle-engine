# TODO: Unsuccessfully coping with FMX scaling on Windows for CASTLE_WINDOWS_FORM

## Overview

FMX on Windows does scaling, just like FMX on Linux. However, it seems there are significant differences in where is it applied between Windows and Linux, and these affect our code.

This file documents how we tried, and failed, to cope with it in `CASTLE_WINDOWS_FORM` backend, for Windows. And what it means.

Important note:

**The problem outlined in this document doesn't affect any real application using CGE**. That's because we don't use `CASTLE_WINDOWS_FORM` on Windows by default. The only reason to test `CASTLE_WINDOWS_FORM` on Windows is for experimenting (and comparing with Linux, that relies on `CASTLE_WINDOWS_FORM`). For real applications:

- TCastleWindow applications on Windows use `CASTLE_WINDOW_WINAPI` by default, not `CASTLE_WINDOWS_FORM`. And `CASTLE_WINDOW_WINAPI` works perfectly, and doesn't use FMX at all.

- TCastleControl (with both FMX and VCL) has no problem. FMX scaling in case of Windows with TCastleControl is handled perfectly (and we actually see non-1.0 scaling values, despite being unable to see them from `CASTLE_WINDOWS_FORM`).

## What does FMX scaling on Windows do

The scaling happens when you activate scaling in Windows 10 settings.

1. Requesting Form size like 200x200 doesn't actually give you OpenGLControl with pixel size exact 200x200 (and it's not because of window frame -- see multi_window form size differences).

    Actually it seems PixelsWidth/PixelsHeight of TOpenGLControl with FMX on Windows are incorrect, they are scaled, but they should not be.

    TODO: In the ideal world, our `FormSizeRequestPrecisely` should do `Result := Round(Result / FmxScreenScale);` to account for it. But it fails, see below.

2. FMX `Screen.Width` and `Screen.Height` are scaled too.

    TODO: In the ideal world, our `Application.ScreenWidth` and `Application.ScreenHeight` should use `FmxScreenScale` too.

    Note: FMX on Linux doesn't scale `Screen.Width` and `Screen.Height`, it seems. So possibly
    this would have to be done only on Windows (or non-Linux).

## What happens with screen size

It seems that

- on Windows, FMX actually scales the Screen.Width by Windows scaling (same as used by FGLUtiliy.Scale).

- on Linux, FMX reports unscaled Screen.Width. It is real screen size in pixels, with cut off margins for GNOME 3 top bar / sidebar.

Current code adjusts better to FMX on Linux behavior.
So

- We assume that Screen.Width is in real device pixels.

- Our FormSizeRequestPrecisely on FMXLinux also accounts to make sure TCastleWindow.Width sets the requested size in real device pixels, so e.g. setting size like "Window.Width := Screen.Width * 0.75" makes sense.

TODO: On FMX on Windows, "Window.Width := Screen.Width * 0.75" also works, but because 2 bugs "cancel each other out": `Screen.Width` is scaled, but also our `FormSizeRequestPrecisely` lets to set scaled value. So "Window.Width := Screen.Width * 0.75" works as you expect, but e.g. "Window.Width := 200" (testcase: multi_window) is actually wrong, the resulting window will not have requested size.

## Failed attempts

The failure comes to the fact we cannot ever get the internal scaling used by FMX on Windows. We tried:

```delphi
{$ifdef MSWINDOWS} Windows, FMX.Platform.Win, FMX.Helpers.Win, {$endif}
```

1. `IFMXScreenService.GetScreenScale`.

    ```delphi
    function FmxScreenScale: Single;
    var
        Service: IFMXScreenService;
    begin
        if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService) then
        begin
        Service := IFMXScreenService(TPlatformServices.Current.GetPlatformService(IFMXScreenService));
        Result := Service.GetScreenScale;
        WritelnLog('FMX IFMXScreenService.GetScreenScale %f', [Result]);
        end else
        Result := 1;
    end;
    ```

    And in FormSizeRequestPrecisely:

    ```delphi
    Result := Round(Result / FmxScreenScale);
    ```

    But: IFMXScreenService is avaiable OK, but GetScreenScale is always 1.0.

2. Get scale using `GetWndScale(TWinWindowHandle(Form.Handle).Wnd)`.

    But: this is always 1.0.

3. `GetScaleFromDC`, like FMX code does:

    ```delphi
    function GetScaleFromDC: Single;
    var
        DC: HDC;
    begin
        DC := GetDC(0);
        try
        Result := GetDCScale(DC);
        finally
        ReleaseDC(0, DC);
        end;
    end;
    ```
    But: this is always 1.0 again...

4. Crazy experiments: `TWinWindowHandle(Form.Handle).SetForcedScaleForForm(1.0);`

    has no effect

Remaining ideas:

- Test `TDisplay.Scale`.

    Note that some `TDisplay` docs seem outdated, and not mention this field (maybe was added around Delphi 11?) http://docwiki.embarcadero.com/RADStudio/Sydney/en/Multiple_Display_Support .

    Test TWinMultiDisplay code, PrimaryDisplay.Scale.

## What would be solved?

- Maybe we could unify Windows and Linux versions handling of FMX scaling, in

    - `FormSizeRequestPrecisely`
    - `Application.ScreenWidth` and `Application.ScreenHeight`

    Right now, instead, `FormSizeRequestPrecisely` has Linux-specific code, and `Application.ScreenWidth` and `Application.ScreenHeight` do nothing (as nothing needs to be done on Linux).

- Using `CASTLE_WINDOWS_FORM` on Windows would work better:

    - `multi_window` requests windows with size 200x200, but they are not actually 200x200.

    - Maximizing any window shows that FMX TOpenGLControl.PixelsWidth/PixelsHeight are scaled, but they should not be. We do not know the actual window size in pixels.

    - `TTestCastleWindow.TestViewportPositionTo` would not fail. It assumes we really can create window with requested size.
