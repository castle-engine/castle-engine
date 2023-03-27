{%MainUnit fpcanvas.pp}
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Implementation of TFPCanvasHelper

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ TFPCanvasHelper }

constructor TFPCanvasHelper.Create;
begin
  inherited create;
  FCanvas := nil;
  FFixedCanvas := false;
  FAllocated := false;
end;

destructor TFPCanvasHelper.destroy;
begin
  if Allocated then
    DeAllocateResources;
  inherited;
end;

procedure TFPCanvasHelper.SetFixedCanvas (AValue : boolean);
begin
  FFixedCanvas := AValue;
end;

procedure TFPCanvasHelper.NotifyCanvas;
// called to unbind from canvas
begin
  if FCanvas<>nil then
    FCanvas.CheckHelper (self);
end;

procedure TFPCanvasHelper.CheckAllocated (ValueNeeded:boolean);

  procedure RaiseErrAllocation;
  begin
    Raise TFPFontException.CreateFmt (ErrAllocation,
                                      [EFont, ErrAlloc[ValueNeeded]]);
  end;

begin
  if (Allocated <> ValueNeeded) then
    RaiseErrAllocation;
end;

procedure TFPCanvasHelper.SetFPColor(const AValue:TFPColor);
begin
  FFPColor := AValue;
end;

procedure TFPCanvasHelper.Changing;
begin
  if Assigned(FOnChanging) then FOnChanging(Self);
end;

procedure TFPCanvasHelper.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TFPCanvasHelper.Lock;
begin

end;

procedure TFPCanvasHelper.UnLock;
begin

end;

procedure TFPCanvasHelper.SetFlags (index:integer; AValue:boolean);
begin
  if AValue then
    FFlags := FFlags or (1 shl index)
  else
    FFlags := FFlags and not (1 shl index);
end;

function TFPCanvasHelper.GetFlags (index:integer) : boolean;
begin
  result := (FFlags and (1 shl index)) <> 0;
end;

function TFPCanvasHelper.GetAllocated : boolean;
begin
  if FFixedCanvas then
    result := assigned(FCanvas)
  else
    result := FAllocated;
end;

procedure TFPCanvasHelper.AllocateResources (ACanvas : TFPCustomCanvas;
  CanDelay: boolean);
begin
  if FFixedCanvas and FAllocated then
    DeallocateResources;
  FCanvas := ACanvas;
  if DelayAllocate and CanDelay then exit;
  try
    DoAllocateResources;
    FAllocated := True;
  except
    FCanvas := nil;
    FAllocated := False;

    raise;
  end;
end;

procedure TFPCanvasHelper.DeallocateResources;
begin
  if FAllocated then
    try
      DoDeallocateResources;
    finally
      FAllocated := false;
      NotifyCanvas;
      FCanvas := nil;
    end;
end;

procedure TFPCanvasHelper.DoCopyProps (From:TFPCanvasHelper);
begin
  FPColor := from.FPColor;
end;

procedure TFPCanvasHelper.DoAllocateResources;
begin
end;

procedure TFPCanvasHelper.DoDeallocateResources;
begin
end;


