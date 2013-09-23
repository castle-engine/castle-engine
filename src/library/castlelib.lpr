{$MODE ObjFPC}
Library castlelib;

{ Adapted from Jonas Maebe's example project :
  http://users.elis.ugent.be/~jmaebe/fpc/FPC_Objective-C_Cocoa.tbz
  http://julien.marcel.free.fr/ObjP_Part7.html

  To make c-compatible (and XCode-compatible) libraries, you must :
  1- use c-types arguments
  2- add a "cdecl" declaration after your functions declarations
  3- export your functions
}

uses
  ctypes, math, CastleFrame, Classes, CastleKeysMouse, sysutils;

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
    aCastleFrame.Destroy();
    aCastleFrame := nil;
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

procedure CGE_OnMouseWheel(zDelta: cFloat); cdecl;
begin
  try
    aCastleFrame.OnMouseWheel(zDelta/120, true);
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

procedure CGE_MoveToViewpoint(iViewpointIdx: cInt32; bAnimated: cBool); cdecl;
begin
  //rEngineCore.MoveToViewpoint(iViewpointIdx, bAnimated);
end;

exports
  CGE_Init, CGE_Close,
  CGE_Render, CGE_SetRenderParams, CGE_OnIdle,
  CGE_OnMouseDown, CGE_OnMouseMove, CGE_OnMouseUp, CGE_OnMouseWheel,
  CGE_LoadSceneFromFile, CGE_MoveToViewpoint;

begin
  {Do not remove the exception masking lines}
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  Set8087CW(Get8087CW or $3f);
end.

