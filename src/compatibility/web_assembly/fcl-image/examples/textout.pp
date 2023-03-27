{$mode objfpc}{$h+}
{$CODEPAGE UTF8}
program textout;

uses
  {$IFDEF UNIX}cwstring, {$ENDIF} classes, sysutils, Types, FPImage, FPCanvas, FPImgCanv, ftFont, FPWritePNG, freetype;

procedure DoDraw(FN, fnChinese : String);

var
  canvas : TFPcustomCAnvas;
  image : TFPCustomImage;
  writer : TFPCustomImageWriter;
  f : TFreeTypeFont;
  S : String;
  U : UnicodeString;
  p : TSize;

begin
  f:=Nil;
  image := TFPMemoryImage.Create (256,256);
  Canvas := TFPImageCanvas.Create (image);
  Writer := TFPWriterPNG.Create;
  InitEngine;
  with TFPWriterPNG(Writer) do
    begin
    indexed := false;
    wordsized := false;
    UseAlpha := false;
    GrayScale := false;
    end;
  try
    with Canvas as TFPImageCanvas do
      begin
      // Clear background
      brush.FPcolor:=colwhite;
      brush.style:=bsSolid;
      pen.mode := pmCopy;
      pen.style := psSolid;
      pen.width := 1;
      pen.FPColor := colWhite;
      FillRect(0,0,255,255);
      // Set font
      F:=TFreeTypeFont.Create;
      Font:=F;
      Font.Name:=FN;
      Font.Size:=14;
      Font.FPColor:=colBlack;
      brush.style:=bsClear;
      pen.FPColor:=colRed;
      S:='Hello, world!';
      Canvas.TextOut(20,20,S);
      F.Size := 14.5;
      Canvas.TextOut(20,30,S);
      F.Angle := -45*2*3.14/360;
      Canvas.TextOut(160,30,S);
      p := Canvas.TextExtent(S);
      Canvas.Rectangle(160,30,160+p.Width-1,30+p.Height-1); // the rectangle is misplaced in the y-direction but that is by design
      F.Angle := 0;
      U:=UTF8Decode('привет, Мир!a');
      Font.FPColor:=colBlue;
      Canvas.TextOut(30,50,U);
      p := Canvas.TextExtent(U);
      Canvas.Rectangle(30,50,30+p.Width-1,50-p.Height+1); // the rectangle is misplaced in the y-direction but that is by design
      if (FNChinese<>'') then
        begin
        Font.Name:=FNChinese;
        U:=UTF8Decode('你好，世界!');
        Font.FPColor:=colRed;
        Canvas.TextOut(20,100,U);
        end
      else
        begin
        Font.Size:=10;
        Canvas.TextOut(20,100,'No chinese font available.');
        end;
      U:=UTF8Decode('non-ASCII chars: ßéùµàçè§âêû');
      Font.Size:=10;
      Canvas.TextOut(20,180,U);
      end;
    writeln ('Saving to "TextTest.png" for inspection !');
    Image.SaveToFile ('TextTest.png', writer);
  finally
    F.Free;
    Canvas.Free;
    image.Free;
    writer.Free;
  end;
end;

Var
  FontFile, FontFileChinese : String;
  {$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
  D : String;
  Info : TSearchRec;
  {$ENDIF}
begin
  // Initialize font search path;
{$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
  D := '/usr/share/fonts/truetype/';
  DefaultSearchPath:=D;
  if FindFirst(DefaultSearchPath+AllFilesMask,faDirectory,Info)=0 then
    try
      repeat
        if (Info.Attr and faDirectory)<>0 then
          if (Info.Name<>'.') and (info.name<>'..') then
            DefaultSearchPath:=DefaultSearchPath+';'+D+Info.Name;
      Until FindNext(Info)<>0;
    finally
      FindClose(Info);
    end;
{$ENDIF}
  FontFile:=ParamStr(1);
  if FontFile='' then
    FontFile:='LiberationSans-Regular.ttf';
  FontFileChinese:=ParamStr(2);
  if FontFileChinese='' then
    With TFontManager.Create do
      try
          FontFileChinese:=SearchFont('wqy-microhei.ttc',False);
      finally
        Free;
      end;
  DoDraw(FontFile,FontFileChinese);
end.
