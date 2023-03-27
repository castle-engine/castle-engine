{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    TFPCustomPen implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ TFPCustomPen }

procedure TFPCustomPen.SetMode (AValue : TFPPenMode);
begin
  FMode := AValue;
end;

procedure TFPCustomPen.SetWidth (AValue : Integer);
begin
  if AValue < 0 then
    FWidth := 0
  else
    FWidth := AValue;
end;

procedure TFPCustomPen.SetStyle (AValue : TFPPenStyle);
begin
  FStyle := AValue;
end;

procedure TFPCustomPen.SetPattern (AValue : longword);
begin
  FPattern := AValue;
end;

procedure TFPCustomPen.SetEndCap(AValue: TFPPenEndCap);
begin
  if FEndCap=AValue then Exit;
  FEndCap:=AValue;
end;

procedure TFPCustomPen.SetJoinStyle(AValue: TFPPenJoinStyle);
begin
  if FJoinStyle=AValue then Exit;
  FJoinStyle:=AValue;
end;

procedure TFPCustomPen.DoCopyProps (From:TFPCanvasHelper);
begin
  with From as TFPCustomPen do
    begin
    self.Style := Style;
    self.Width := Width;
    self.Mode := Mode;
    self.pattern := pattern;
    end;
  inherited;
end;

function TFPCustomPen.CopyPen : TFPCustomPen;
begin
  result := TFPCustomPen(self.ClassType.Create);
  result.DoCopyProps (self);
end;


