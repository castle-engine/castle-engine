{ -*- compile-command: "./castleengine_compile.sh" -*- }
{
  Copyright 2013-2014 Jan Adamec, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ The castleengine library main source code.
  This compiles to castleengine.dll / libcastleengine.so / libcastleengine.dylib.

  Development notes:

  Adapted from Jonas Maebe's example project :
  http://users.elis.ugent.be/~jmaebe/fpc/FPC_Objective-C_Cocoa.tbz
  http://julien.marcel.free.fr/ObjP_Part7.html

  To make c-compatible (and XCode-compatible) libraries, you must :
  1- use c-types arguments
  2- add a "cdecl" declaration after your functions declarations
  3- export your functions

  See FPC CTypes unit (source rtl/unix/ctypes.inc) for a full list of c-types.
}

{$mode objfpc}{$H+}
library castleengine;

uses CTypes, Math, SysUtils, CastleWindow, CastleWindowTouch, CastleUtils,
  Classes, CastleKeysMouse, CastleCameras, CastleVectors, CastleGLUtils,
  CastleImages, CastleSceneCore, CastleUIControls, X3DNodes, CastleLog,
  CastleBoxes;

var
  Window: TCastleWindowTouch;

procedure CGE_Open(flags: cUInt32); cdecl;
begin
  try
    if (flags and 1 {ecgeofSaveMemory}) > 0 then
    begin
      Detail_QuadricSlices := 16;
      Detail_QuadricStacks := 16;
      Detail_RectDivisions := 0;
    end;
    if (flags and 2 {ecgeofLog}) > 0 then
      InitializeLog;

    Window := TCastleWindowTouch.Create(nil);
    Window.Open;
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

procedure CGE_Close; cdecl;
begin
  try
    Window.Close;
    FreeAndNil(Window);
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

procedure CGE_GetOpenGLInformation(szBuffer: pchar; nBufSize: cInt32); cdecl;
var
  sText: string;
begin
  try
    sText := GLInformationString;
    StrPLCopy(szBuffer, sText, nBufSize-1);
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

procedure CGE_Resize(uiViewWidth, uiViewHeight: cUInt32); cdecl;
begin
  try
    Window.LibraryResize(uiViewWidth, uiViewHeight);
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

procedure CGE_Render; cdecl;
begin
  try
    Window.LibraryRender;
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

procedure CGE_SaveScreenshotToFile(szFile: pcchar); cdecl;
var
  Image: TRGBImage;
  Restore2D: TUIControlList;
  I: Integer;
  C: TUIControl;
begin
  try
    Restore2D := TUIControlList.Create(false);
    try
      // hide touch controls
      for I := 0 to Window.Controls.Count - 1 do
      begin
        C := Window.Controls[I];
        if C.Exists and (C.RenderStyle = rs2D) then
        begin
          C.Exists := false;
          Restore2D.Add(C);
        end;
      end;
      // make screenshot
      Image := Window.SaveScreen;
      try
        SaveImage(Image, StrPas(PChar(szFile)));
      finally FreeAndNil(Image) end;
      // restore hidden controls
      for I := 0 to Restore2D.Count - 1 do
      begin
        C := Restore2D[I];
        C.Exists := true;
      end;
    finally FreeAndNil(Restore2D) end;
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

procedure CGE_SetLibraryCallbackProc(aProc: TLibraryCallbackProc); cdecl;
begin
  try
    Window.LibraryCallbackProc := aProc;
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

procedure CGE_Update; cdecl;
begin
  try
    Application.LibraryUpdate;
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

procedure CGE_MouseDown(X, Y: CInt32; bLeftBtn: cBool; FingerIndex: CInt32); cdecl;
var
  MyButton: TMouseButton;
begin
  try
    if (bLeftBtn) then MyButton := mbLeft else MyButton := mbRight;
    Window.LibraryMouseDown(Vector2Single(X, Y), MyButton, FingerIndex);
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

procedure CGE_Motion(X, Y: CInt32; FingerIndex: CInt32); cdecl;
begin
  try
    Window.LibraryMotion(Vector2Single(X, Y), FingerIndex);
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

procedure CGE_MouseUp(X, Y: cInt32; bLeftBtn: cBool; FingerIndex: CInt32); cdecl;
var
  MyButton: TMouseButton;
begin
  try
    if (bLeftBtn) then MyButton := mbLeft else MyButton := mbRight;
    Window.LibraryMouseUp(Vector2Single(X, Y), MyButton, FingerIndex);
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

procedure CGE_MouseWheel(zDelta: cFloat; bVertical: cBool); cdecl;
begin
  try
    Window.LibraryMouseWheel(zDelta/120, bVertical);
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

procedure CGE_KeyDown(eKey: CInt32); cdecl;
begin
  try
    if TKey(eKey)<>K_None then
      Window.LibraryKeyDown(TKey(eKey), #0);
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

procedure CGE_KeyUp(eKey: CInt32); cdecl;
begin
  try
    if TKey(eKey)<>K_None then
      Window.LibraryKeyUp(TKey(eKey));
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

procedure CGE_LoadSceneFromFile(szFile: pcchar); cdecl;
begin
  try
    Window.Load(StrPas(PChar(szFile)));
    Window.MainScene.Spatial := [ssRendering, ssDynamicCollisions];
    Window.MainScene.ProcessEvents := true;
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

function CGE_GetViewpointsCount(): cInt32; cdecl;
begin
  try
    Result := Window.MainScene.ViewpointsCount;
  except
    on E: TObject do
    begin
      WritelnLog('Window', ExceptMessage(E));
      Result := 0;
    end;
  end;
end;

procedure CGE_GetViewpointName(iViewpointIdx: cInt32; szName: pchar; nBufSize: cInt32); cdecl;
var
  sName: string;
begin
  try
    sName := Window.MainScene.GetViewpointName(iViewpointIdx);
    StrPLCopy(szName, sName, nBufSize-1);
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

procedure CGE_MoveToViewpoint(iViewpointIdx: cInt32; bAnimated: cBool); cdecl;
begin
  try
    Window.MainScene.MoveToViewpoint(iViewpointIdx, bAnimated);
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

procedure CGE_AddViewpointFromCurrentView(szName: pcchar); cdecl;
begin
  try
    Window.MainScene.AddViewpointFromCamera(
      Window.SceneManager.Camera, StrPas(PChar(szName)));
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

procedure CGE_GetBoundingBox(pfXMin, pfXMax, pfYMin, pfYMax, pfZMin, pfZMax: pcfloat); cdecl;
var
  BBox: TBox3D;
begin
  try
    BBox := Window.MainScene.BoundingBox;
    pfXMin^ := BBox.Data[0, 0]; pfXMax^ := BBox.Data[1, 0];
    pfYMin^ := BBox.Data[0, 1]; pfYMax^ := BBox.Data[1, 1];
    pfZMin^ := BBox.Data[0, 2]; pfZMax^ := BBox.Data[1, 2];
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

procedure CGE_GetViewCoords(pfPosX, pfPosY, pfPosZ, pfDirX, pfDirY, pfDirZ,
                            pfUpX, pfUpY, pfUpZ, pfGravX, pfGravY, pfGravZ: pcfloat); cdecl;
var
  Pos, Dir, Up, GravityUp: TVector3Single;
begin
  try
    Window.SceneManager.Camera.GetView(Pos, Dir, Up, GravityUp);
    pfPosX^ := Pos[0]; pfPosY^ := Pos[1]; pfPosZ^ := Pos[2];
    pfDirX^ := Dir[0]; pfDirY^ := Dir[1]; pfDirZ^ := Dir[2];
    pfUpX^ := Up[0]; pfUpY^ := Up[1]; pfUpZ^ := Up[2];
    pfGravX^ := GravityUp[0]; pfGravY^ := GravityUp[1]; pfGravZ^ := GravityUp[2];
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

procedure CGE_MoveViewToCoords(fPosX, fPosY, fPosZ, fDirX, fDirY, fDirZ,
                               fUpX, fUpY, fUpZ, fGravX, fGravY, fGravZ: cFloat;
                               bAnimated: cBool); cdecl;
var
  Pos, Dir, Up, GravityUp: TVector3Single;
begin
  try
    Pos[0] := fPosX; Pos[1] := fPosY; Pos[2] := fPosZ;
    Dir[0] := fDirX; Dir[1] := fDirY; Dir[2] := fDirZ;
    Up[0] := fUpX; Up[1] := fUpY; Up[2] := fUpZ;
    GravityUp[0] := fGravX; GravityUp[1] := fGravY; GravityUp[2] := fGravZ;
    if bAnimated then
      Window.SceneManager.Camera.AnimateTo(Pos, Dir, Up, 0.5)
    else
      Window.SceneManager.Camera.SetView(Pos, Dir, Up, GravityUp);
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

function CGE_GetNavigationType(): cInt32; cdecl;
begin
  try
    case Window.NavigationType of
      ntWalk     : Result := 0;
      ntFly      : Result := 1;
      ntExamine  : Result := 2;
      ntTurntable: Result := 3;
      ntNone     : Result := 4;
      else raise EInternalError.Create('CGE_GetNavigationType: Unrecognized Window.NavigationType');
    end;
  except
    on E: TObject do
    begin
      WritelnLog('Window', ExceptMessage(E));
      Result := -1;
    end;
  end;
end;

procedure CGE_SetNavigationType(NewType: cInt32); cdecl;
var
  aNavType: TNavigationType;
begin
  try
    case NewType of
      0: aNavType := ntWalk;
      1: aNavType := ntFly;
      2: aNavType := ntExamine;
      3: aNavType := ntTurntable;
      4: aNavType := ntNone;
      else raise EInternalError.CreateFmt('CGE_SetNavigationType: Invalid navigation type %d', [NewType]);
    end;
    Window.NavigationType := aNavType;
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

procedure CGE_SetTouchInterface(eMode: cInt32); cdecl;
var
  aNewMode: TTouchCtlInterface;
begin
  try
    case eMode of
      0: aNewMode := etciNone;
      1: aNewMode := etciCtlWalkCtlRotate;
      2: aNewMode := etciCtlWalkDragRotate;
      3: aNewMode := etciCtlFlyCtlWalkDragRotate;
      4: aNewMode := etciCtlPanXYDragRotate;
      else raise EInternalError.CreateFmt('CGE_SetTouchInterface: Invalid touch interface mode %d', [eMode]);
    end;
    Window.TouchInterface := aNewMode;
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

procedure CGE_SetUserInterface(bAutomaticTouchInterface: cBool; nDpi: cInt32); cdecl;
begin
  try
    Window.AutomaticTouchInterface := bAutomaticTouchInterface;
    Window.Dpi := nDpi;
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

function cgehelper_getWalkCamera: TWalkCamera;
begin
  Result := nil;
  if Window.SceneManager.Camera <> nil then
  begin
    if Window.SceneManager.Camera is TUniversalCamera then
      Result := (Window.SceneManager.Camera as TUniversalCamera).Walk else
    if Window.SceneManager.Camera is TWalkCamera then
      Result := Window.SceneManager.Camera as TWalkCamera;
  end;
end;

procedure CGE_SetVariableInt(eVar: cInt32; nValue: cInt32); cdecl;
var
  WalkCamera: TWalkCamera;
begin
  try
    case eVar of
      0: begin    // ecgevarWalkHeadBobbing
        WalkCamera := cgehelper_getWalkCamera;
        if WalkCamera <> nil then begin
          if nValue>0 then
            WalkCamera.HeadBobbing := TWalkCamera.DefaultHeadBobbing else
            WalkCamera.HeadBobbing := 0.0;
        end;
      end;

      1: begin    // ecgevarEffectSSAO
        if Window.SceneManager.ScreenSpaceAmbientOcclusionAvailable then
          Window.SceneManager.ScreenSpaceAmbientOcclusion := (nValue > 0);
      end;

      2: begin    // ecgevarMouseLook
        WalkCamera := cgehelper_getWalkCamera;
        if WalkCamera <> nil then
            WalkCamera.MouseLook := (nValue > 0);
      end;

      3: begin    // ecgevarCrossHair
        { TODO }
      end;

    end;
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

function CGE_GetVariableInt(eVar: cInt32): cInt32; cdecl;
var
  WalkCamera: TWalkCamera;
begin
  try
    case eVar of
      0: begin    // ecgevarWalkHeadBobbing
        WalkCamera := cgehelper_getWalkCamera;
        if (WalkCamera <> nil) and (WalkCamera.HeadBobbing > 0) then
          Result := 1 else
          Result := 0;
      end;

      1: begin    // ecgevarEffectSSAO
        if Window.SceneManager.ScreenSpaceAmbientOcclusionAvailable and
            Window.SceneManager.ScreenSpaceAmbientOcclusion then
          Result := 1 else
          Result := 0;
      end;

      2: begin    // ecgevarMouseLook
        WalkCamera := cgehelper_getWalkCamera;
        if (WalkCamera <> nil) and WalkCamera.MouseLook then
          Result := 1 else
          Result := 0;
      end;

      3: begin    // ecgevarCrossHair
        { TODO }
      end;

      4: begin    // ecgevarAnimationRunning
        if Window.SceneManager.Camera.Animation then
          Result := 1 else
          Result := 0;
      end;

      else Result := -1; // unsupported variable
    end;
  except
    on E: TObject do WritelnLog('Window', ExceptMessage(E));
  end;
end;

exports
  CGE_Open, CGE_Close, CGE_GetOpenGLInformation,
  CGE_Render, CGE_Resize, CGE_SetLibraryCallbackProc, CGE_Update,
  CGE_MouseDown, CGE_Motion, CGE_MouseUp, CGE_MouseWheel, CGE_KeyDown, CGE_KeyUp,
  CGE_LoadSceneFromFile, CGE_GetNavigationType, CGE_SetNavigationType,
  CGE_GetViewpointsCount, CGE_GetViewpointName, CGE_MoveToViewpoint, CGE_AddViewpointFromCurrentView,
  CGE_GetBoundingBox, CGE_GetViewCoords, CGE_MoveViewToCoords, CGE_SaveScreenshotToFile,
  CGE_SetTouchInterface, CGE_SetUserInterface,
  CGE_SetVariableInt, CGE_GetVariableInt;

begin
  {Do not remove the exception masking lines}
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
end.

