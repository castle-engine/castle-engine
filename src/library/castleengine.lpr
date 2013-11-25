{$MODE ObjFPC}{$H+}
Library castleengine;

{ Adapted from Jonas Maebe's example project :
  http://users.elis.ugent.be/~jmaebe/fpc/FPC_Objective-C_Cocoa.tbz
  http://julien.marcel.free.fr/ObjP_Part7.html

  To make c-compatible (and XCode-compatible) libraries, you must :
  1- use c-types arguments
  2- add a "cdecl" declaration after your functions declarations
  3- export your functions
}

uses
  ctypes, math, CastleFrame, Classes, CastleKeysMouse, CastleCameras, sysutils,
  CastleGLUtils, CastleVectors, CastleImages, CastleGLImages;

{ See http://fpc.freedoors.org/dos204full/source/rtl/unix/ctypes.inc
  For a full list of c-types
}

var
  aCastleFrame: TCastleFrame;

procedure CGE_Init(); cdecl;
begin
  try
    aCastleFrame := TCastleFrame.Create(nil);
    aCastleFrame.GLContextOpen;
  except
  end;
end;

procedure CGE_Close; cdecl;
begin
  try
    aCastleFrame.GLContextClose();
    aCastleFrame.Destroy();
    aCastleFrame := nil;
  except
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
  end;
end;

procedure CGE_SetRenderParams(uiViewWidth, uiViewHeight: cUInt32); cdecl;
begin
  try
    aCastleFrame.SetRenderSize(uiViewWidth, uiViewHeight);
  except
  end;
end;

procedure CGE_Render; cdecl;
begin
  try
    aCastleFrame.Paint();
  except
    //on E: Exception do OutputDebugString(@E.Message[1]);
  end;
end;

procedure CGE_SaveScreenshotToFile(szFile: pcchar); cdecl;
var
  Image: TRGBImage;
begin
  try
    // TODO: remove touch controls
    Image := SaveScreen_NoFlush(aCastleFrame.Rect, cbBack);
    SaveImage(Image, StrPas(PChar(szFile)));
  except
  end;
end;

procedure CGE_SetLibraryCallbackProc(aProc: TCgeLibraryCallbackProc); cdecl;
begin
  try
    aCastleFrame.SetLibraryCallbackProc(aProc);
  except
    //on E: Exception do OutputDebugString(@E.Message[1]);
  end;
end;

procedure CGE_OnIdle; cdecl;
begin
  try
    aCastleFrame.Update();
  except
    //on E: Exception do OutputDebugString(@E.Message[1]);
  end;
end;

function CGE_ShiftToFPCShift(uiShift: cUInt32): TShiftState;
var
  Shift: TShiftState;
begin
  Shift := [];
  if (uiShift and 1)=1 then Include(Shift, ssShift);
  if (uiShift and 2)=2 then Include(Shift, ssAlt);
  if (uiShift and 4)=4 then Include(Shift, ssCtrl);
  Result := Shift;
end;

procedure CGE_OnMouseDown(x, y: cInt32; bLeftBtn: cBool; uiShift: cUInt32); cdecl;
var
  MyButton: TMouseButton;
  Shift: TShiftState;
begin
  try
    if (bLeftBtn) then MyButton := mbLeft else MyButton := mbRight;
    Shift := CGE_ShiftToFPCShift(uiShift);
    aCastleFrame.OnMouseDown(MyButton, Shift, x, y);
  except
  end;
end;

procedure CGE_OnMouseMove(x, y: cInt32; uiShift: cUInt32); cdecl;
var
  Shift: TShiftState;
begin
  try
    Shift := CGE_ShiftToFPCShift(uiShift);
    aCastleFrame.OnMouseMove(Shift, x, y);
  except
  end;
end;

procedure CGE_OnMouseUp(x, y: cInt32; bLeftBtn: cBool; uiShift: cUInt32); cdecl;
var
  MyButton: TMouseButton;
  Shift: TShiftState;
begin
  try
    if (bLeftBtn) then MyButton := mbLeft else MyButton := mbRight;
    Shift := CGE_ShiftToFPCShift(uiShift);
    aCastleFrame.OnMouseUp(MyButton, Shift, x, y);
  except
  end;
end;

procedure CGE_OnMouseWheel(zDelta: cFloat; bVertical: cBool; uiShift: cUint32); cdecl;
begin
  try
    aCastleFrame.OnMouseWheel(zDelta/120, bVertical);
  except
  end;
end;

procedure CGE_LoadSceneFromFile(szFile: pcchar); cdecl;
begin
  try
    aCastleFrame.Load(StrPas(PChar(szFile)));
  except
    //on E: Exception do OutputDebugString(@E.Message[1]);
  end;
end;

function CGE_GetViewpointsCount(): cInt32; cdecl;
begin
  try
    Result := aCastleFrame.MainScene.ViewpointsCount;
  except
    Result := 0;
  end;
end;

procedure CGE_GetViewpointName(iViewpointIdx: cInt32; szName: pchar; nBufSize: cInt32); cdecl;
var
  sName: string;
begin
  try
    sName := aCastleFrame.MainScene.GetViewpointName(iViewpointIdx);
    StrPLCopy(szName, sName, nBufSize-1);
  except
  end;
end;

procedure CGE_MoveToViewpoint(iViewpointIdx: cInt32; bAnimated: cBool); cdecl;
begin
  try
    aCastleFrame.MainScene.MoveToViewpoint(iViewpointIdx, bAnimated);
  except
  end;
end;

procedure CGE_AddViewpointFromCurrentView(szName: pcchar); cdecl;
begin
  try
    aCastleFrame.MainScene.AddViewpointFromCamera(aCastleFrame.Camera, StrPas(PChar(szName)));
  except
  end;
end;

procedure CGE_GetViewCoords(pfPosX, pfPosY, pfPosZ, pfDirX, pfDirY, pfDirZ,
                            pfUpX, pfUpY, pfUpZ, pfGravX, pfGravY, pfGravZ: pcfloat); cdecl;
var
  Pos, Dir, Up, GravityUp: TVector3Single;
begin
  try
    aCastleFrame.Camera.GetView(Pos, Dir, Up, GravityUp);
    pfPosX^ := Pos[0]; pfPosY^ := Pos[1]; pfPosZ^ := Pos[2];
    pfDirX^ := Dir[0]; pfDirY^ := Dir[1]; pfDirZ^ := Dir[2];
    pfUpX^ := Up[0]; pfUpY^ := Up[1]; pfUpZ^ := Up[2];
    pfGravX^ := GravityUp[0]; pfGravY^ := GravityUp[1]; pfGravZ^ := GravityUp[2];
  except
  end;
end;

procedure CGE_MoveViewToCoords(fPosX, fPosY, fPosZ, fDirX, fDirY, fDirZ,
                               fUpX, fUpY, fUpZ, fGravX, fGravY, fGravZ: cFloat); cdecl;
var
  Pos, Dir, Up, GravityUp: TVector3Single;
begin
  try
    Pos[0] := fPosX; Pos[1] := fPosY; Pos[2] := fPosZ;
    Dir[0] := fDirX; Dir[1] := fDirY; Dir[2] := fDirZ;
    Up[0] := fUpX; Up[1] := fUpY; Up[2] := fUpZ;
    GravityUp[0] := fGravX; GravityUp[1] := fGravY; GravityUp[2] := fGravZ;
    aCastleFrame.Camera.SetView(Pos, Dir, Up, GravityUp);
  except
  end;
end;

function CGE_GetCurrentNavigationType(): cInt32; cdecl;
var
  aNavType: TCameraNavigationType;
begin
  Result := 0;
  try
    aNavType := aCastleFrame.GetCurrentNavigationType();
    if (aNavType = ntWalk) then Result := 0
    else if (aNavType = ntFly) then Result := 1
    else if (aNavType = ntExamine) then Result := 2
    else if (aNavType = ntArchitecture) then Result := 3;
  except
  end;
end;

procedure CGE_SetNavigationType(NewType: cInt32); cdecl;
var
  aNavType: TCameraNavigationType;
begin
  try
    case NewType of
    0: aNavType := ntWalk;
    1: aNavType := ntFly;
    2: aNavType := ntExamine;
    3: aNavType := ntArchitecture;
    else aNavType := ntExamine;
    end;
    aCastleFrame.SetNavigationType(aNavType);
  except
  end;
end;

procedure CGE_UpdateTouchInterface(eMode: cInt32); cdecl;
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
    else aNewMode := etciNone;
    end;
    aCastleFrame.UpdateTouchInterface(aNewMode);
  except
  end;
end;

procedure CGE_SetUserInterfaceInfo(eMode, nDpi: cInt32); cdecl;
var
  aNewMode: TUserInterface;
begin
  try
    case eMode of
    0: aNewMode := euiDesktop;
    1: aNewMode := euiTouch;
    else aNewMode := euiDesktop;
    end;
    aCastleFrame.UserInterface := aNewMode;
    aCastleFrame.Dpi := nDpi;
  except
  end;
end;

exports
  CGE_Init, CGE_Close, CGE_GetOpenGLInformation,
  CGE_Render, CGE_SetRenderParams, CGE_SetLibraryCallbackProc, CGE_OnIdle,
  CGE_OnMouseDown, CGE_OnMouseMove, CGE_OnMouseUp, CGE_OnMouseWheel,
  CGE_LoadSceneFromFile, CGE_GetCurrentNavigationType, CGE_SetNavigationType,
  CGE_GetViewpointsCount, CGE_GetViewpointName, CGE_MoveToViewpoint, CGE_AddViewpointFromCamera,
  CGE_GetViewCoords, CGE_MoveViewToCoords, CGE_SaveScreenshotToFile,
  CGE_UpdateTouchInterface, CGE_SetUserInterfaceInfo;

begin
  {Do not remove the exception masking lines}
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  //Set8087CW(Get8087CW or $3f);
end.

