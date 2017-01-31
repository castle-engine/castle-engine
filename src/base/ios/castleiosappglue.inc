{
  Copyright 2013-2017 Jan Adamec, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{uses} CTypes, Math, SysUtils, CastleWindow, CastleWindowTouch, CastleUtils,
  CastleKeysMouse, CastleVectors, CastleLog, CastleConfig;

procedure CGEA_Open; cdecl;
begin
  try
    InitializeLog;

    //UserConfig.Load;	// TODO: need to set correct writable location on iOS
    Window.OpenAndRun;
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

procedure CGEA_Close; cdecl;
begin
  try
    Window.Close;
    FreeAndNil(Window);
    //UserConfig.Save;
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

procedure CGEA_Resize(uiViewWidth, uiViewHeight: cUInt32); cdecl;
begin
  try
    Window.LibraryResize(uiViewWidth, uiViewHeight);
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

procedure CGEA_Render; cdecl;
begin
  try
    Window.LibraryRender;
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

procedure CGEA_SetLibraryCallbackProc(aProc: TLibraryCallbackProc); cdecl;
begin
  try
    Window.LibraryCallbackProc := aProc;
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

procedure CGEA_Update; cdecl;
begin
  try
    Application.LibraryUpdate;
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

procedure CGEA_TouchDown(X, Y: CInt32; FingerIndex: CInt32); cdecl;
begin
  try
    Window.LibraryMouseDown(Vector2Single(X, Y), mbLeft, FingerIndex);
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

procedure CGEA_Motion(X, Y: CInt32; FingerIndex: CInt32); cdecl;
begin
  try
    Window.LibraryMotion(Vector2Single(X, Y), FingerIndex);
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

procedure CGEA_TouchUp(X, Y: cInt32; FingerIndex: CInt32); cdecl;
begin
  try
    Window.LibraryMouseUp(Vector2Single(X, Y), mbLeft, FingerIndex);
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

procedure CGEA_KeyDown(eKey: CInt32); cdecl;
begin
  try
    if TKey(eKey)<>K_None then
      Window.LibraryKeyDown(TKey(eKey), #0);
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

procedure CGEA_KeyUp(eKey: CInt32); cdecl;
begin
  try
    if TKey(eKey)<>K_None then
      Window.LibraryKeyUp(TKey(eKey));
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

procedure CGEA_SetUserInterface(bAutomaticTouchInterface: cBool; nDpi: cInt32); cdecl;
begin
  try
    Window.AutomaticTouchInterface := bAutomaticTouchInterface;
    Window.Dpi := nDpi;
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

exports
  CGEA_Open, CGEA_Close,
  CGEA_Render, CGEA_Resize, CGEA_SetLibraryCallbackProc, CGEA_Update,
  CGEA_TouchDown, CGEA_Motion, CGEA_TouchUp, CGEA_KeyDown, CGEA_KeyUp,
  CGEA_SetUserInterface;

begin
  {Do not remove the exception masking lines}
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
end.

