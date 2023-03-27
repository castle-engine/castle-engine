{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    TFPCustomBrush implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ TFPCustomBrush }

procedure TFPCustomBrush.SetStyle (AValue : TFPBrushStyle);
begin
  FStyle := AValue
end;

procedure TFPCustomBrush.SetImage (AValue : TFPCustomImage);
begin
  FImage := AValue;
end;

procedure TFPCustomBrush.DoCopyProps (From:TFPCanvasHelper);
begin
  with From as TFPCustomBrush do
    begin
    self.Style := Style;
    self.Image := Image;
    end;
  inherited DoCopyProps(From);
end;

function TFPCustomBrush.CopyBrush : TFPCustomBrush;
begin
  result := TFPCustomBrush(self.ClassType.Create);
  result.DoCopyProps (self);
end;



