{
  Sample program by Ondrey Pokorny to demonstrate drawing modes of the TFPCustomCanvas:
    - opaque 
    - alphablend 
    - custom blending, using a callback (not-used in this case)
}
program FPCanvasAlphaDraw;

uses FPImage, FPImgCanv, FPCanvas, FPReadPNG, FPWritePNG, Classes, SysUtils, freetype, ftFont;

const
  cImageName: array[TFPDrawingMode] of string = ('opaque', 'alphablend', 'not-used');

var
  xNew, xImage: TFPMemoryImage;
  xCanvas: TFPImageCanvas;
  xDrawingMode: TFPDrawingMode;
  xRect: TRect;
begin
  ftFont.InitEngine;
  xNew := nil;
  xCanvas := nil;
  xImage := nil;
  try
    xImage := TFPMemoryImage.Create(0, 0);
    xImage.LoadFromFile('edit-clear.png');

    for xDrawingMode := dmOpaque to dmAlphaBlend do
    begin
      xNew := TFPMemoryImage.Create(200, 200);
      xCanvas := TFPImageCanvas.Create(xNew);

      xCanvas.DrawingMode := xDrawingMode;

      xCanvas.Pen.Style := psClear;
      xCanvas.Brush.FPColor := colRed;

      xCanvas.FillRect(0, 0, xNew.Width, xNew.Height);
      // draw semi-transparent objects
      xCanvas.Brush.FPColor := FPColor($FFFF, $FFFF, $FFFF, $8000);
      xRect := Rect(0, 0, 50, 50);
      xCanvas.Ellipse(xRect);
      xRect.Offset(50, 0);
      xCanvas.Rectangle(xRect);

      xRect := Rect(0, 50, 50, 100);

      xCanvas.Pen.Style := psSolid;
      xCanvas.Pen.FPColor := FPColor($FFFF, $FFFF, $FFFF, $8000);
      xCanvas.Pen.Width := 4;
      xCanvas.Brush.Style := bsClear;

      xCanvas.Ellipse(xRect);
      xRect.Offset(50, 0);
      xCanvas.Rectangle(xRect);
      xRect.Offset(50, 0);
      xCanvas.Polyline([
        Point(xRect.CenterPoint.X, xRect.Top),
        Point(xRect.Right, xRect.CenterPoint.Y),
        Point(xRect.CenterPoint.X, xRect.Bottom),
        Point(xRect.Left, xRect.CenterPoint.Y),
        Point(xRect.CenterPoint.X, xRect.Top)]);
      xRect.Offset(50, 0);
      xCanvas.MoveTo(xRect.TopLeft);
      xCanvas.LineTo(xRect.Right, xRect.Top);

      xRect := Rect(0, 100, 50, 150);
      xCanvas.Draw(xRect.Left, xRect.Top, xImage);
      xRect.Offset(50, 0);
      xCanvas.StretchDraw(xRect.Left, xRect.Top, xRect.Width, xRect.Height, xImage);

      xRect := Rect(0, 150, 50, 200);
      xCanvas.Font:=TFreeTypeFont.Create;
      xCanvas.Font.FPColor := FPColor($FFFF, $FFFF, $FFFF, $8000);
      xCanvas.Font.Name := 'DejaVuLGCSans.ttf';
      xCanvas.Font.Size := 15;
      (xCanvas.Font as TFreeTypeFont).AntiAliased := True;
      xCanvas.TextOut(xRect.Left, xRect.CenterPoint.Y, 'Hello');

      xRect.Offset(100, 0);
      (xCanvas.Font as TFreeTypeFont).AntiAliased := False;
      xCanvas.TextOut(xRect.Left, xRect.CenterPoint.Y, 'Hello');

      xNew.SaveToFile(cImageName[xDrawingMode]+'.png');

      xCanvas.Font.Free;
      xCanvas.Font := nil;
      FreeAndNil(xNew);
      FreeAndNil(xCanvas);
    end;
  finally
    xCanvas.Free;
    xNew.Free;
    xImage.Free;
  end;
end.

