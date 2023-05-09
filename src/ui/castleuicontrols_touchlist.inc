{%MainUnit castleuicontrols.pas}
{
  Copyright 2010-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

//type

  { Tracking of a touch by a single finger, used by TTouchList. }
  TTouch = record
  public
    { Index of the finger/mouse. Always simply zero on traditional desktops with
      just a single mouse device. For devices with multi-touch
      (and/or possible multiple mouse pointers) this is actually useful
      to connect touches from different frames into a single move action.
      In other words: the FinderIndex stays constant while user moves
      a finger over a touch device.

      All the touches on a TCastleContainer.Touches array always have different
      FingerIndex value.

      Note that the index of TTouch structure in TCastleContainer.Touches array is
      @italic(not) necessarily equal to the FingerIndex. It cannot be ---
      imagine you press 1st finger, then press 2nd finger, then let go of
      the 1st finger. The FingerIndex of the 2nd finger cannot change
      (to keep events sensibly reporting the same touch),
      so there has to be a temporary "hole" in FinderIndex numeration. }
    FingerIndex: TFingerIndex;

    { Position of the touch over a device.

      Position (0, 0) is the window's bottom-left corner.
      This is consistent with how our 2D controls (TCastleUserInterface)
      treat all positions.

      The position is expressed as a float value, to support backends
      that can report positions with sub-pixel accuracy.
      For example GTK and Android can do it, although it depends on
      underlying hardware capabilities as well.
      The top-right corner or the top-right pixel has the coordinates
      (Width, Height).
      Note that if you want to actually draw something at the window's
      edge (for example, paint the top-right pixel of the window with some
      color), then the pixel coordinates are (Width - 1, Height - 1).
      The idea is that the whole top-right pixel is an area starting
      in (Width - 1, Height - 1) and ending in (Width, Height).

      Note that we have mouse capturing (when user presses and holds
      the mouse button, all the following mouse events are reported to this
      window, even when user moves the mouse outside of the window).
      This is typical of all window libraries (GTK, LCL etc.).
      This implicates that mouse positions are sometimes tracked also
      when mouse is outside the window, which means that mouse position
      may be outside the rectangle (0, 0) - (Width, Height),
      so it may even be negative. }
    Position: TVector2;
  end;
  PTouch = ^TTouch;

  { Tracking of multi-touch, a position of each finger on the screen. }
  TTouchList = class({$ifdef FPC}specialize{$endif} TStructList<TTouch>)
  private
    { Find an item with given FingerIndex, or -1 if not found. }
    function FindFingerIndex(const FingerIndex: TFingerIndex): Integer;
    function GetFingerIndexPosition(const FingerIndex: TFingerIndex): TVector2;
    procedure SetFingerIndexPosition(const FingerIndex: TFingerIndex;
      const Value: TVector2);
  public
    { Gets or sets a position corresponding to given FingerIndex.
      If there is no information for given FingerIndex on the list,
      the getter will return zero, and the setter will automatically create
      and add appropriate information. }
    property FingerIndexPosition[const FingerIndex: TFingerIndex]: TVector2
      read GetFingerIndexPosition write SetFingerIndexPosition;
    { Remove a touch item for given FingerIndex. }
    procedure RemoveFingerIndex(const FingerIndex: TFingerIndex);
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TTouchList ----------------------------------------------------------------- }

function TTouchList.FindFingerIndex(const FingerIndex: TFingerIndex): Integer;
begin
  for Result := 0 to Count - 1 do
    if L[Result].FingerIndex = FingerIndex then
      Exit;
  Result := -1;
end;

function TTouchList.GetFingerIndexPosition(const FingerIndex: TFingerIndex): TVector2;
var
  Index: Integer;
begin
  Index := FindFingerIndex(FingerIndex);
  if Index <> -1 then
    Result := L[Index].Position
  else
    Result := TVector2.Zero;
end;

procedure TTouchList.SetFingerIndexPosition(const FingerIndex: TFingerIndex;
  const Value: TVector2);
var
  Index: Integer;
  NewTouch: PTouch;
begin
  Index := FindFingerIndex(FingerIndex);
  if Index <> -1 then
    L[Index].Position := Value else
  begin
    NewTouch := PTouch(Add);
    NewTouch^.FingerIndex := FingerIndex;
    NewTouch^.Position := Value;
  end;
end;

procedure TTouchList.RemoveFingerIndex(const FingerIndex: TFingerIndex);
var
  Index: Integer;
begin
  Index := FindFingerIndex(FingerIndex);
  if Index <> -1 then
    Delete(Index);
end;

{$endif read_implementation}
