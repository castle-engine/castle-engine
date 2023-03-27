{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2022 by the Free Pascal development team

    QOI writer class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
unit fpwriteqoi;
interface

uses FPImage, classes, sysutils, QoiComn;

type

  TFPWriterQoi = class (TFPCustomImageWriter)
  private
    QoiHeader : TQoiHeader;
    procedure setUseAlpha(useAlpha:boolean);
    function getUseAlpha:boolean;
  protected
    function  SaveHeader(Stream:TStream; Img: TFPCustomImage):boolean; virtual;
    procedure InternalWrite (Stream:TStream; Img: TFPCustomImage); override;
  public
    constructor Create; override;
    property useAlpha:boolean read getUseAlpha write setUseAlpha;
  end;


implementation

constructor TFPWriterQoi.create;
begin
  inherited create;
  with QoiHeader do
    begin
      magic:='qoif';
      channels:=qoChannelRGB;
      colorspace:=0;
    end;
end;


procedure TFPWriterQoi.setUseAlpha(useAlpha:boolean);
begin
     if useAlpha then QoiHeader.channels := qoChannelRGBA else QoiHeader.channels:=qoChannelRGB;
end;

function TFPWriterQoi.getUseAlpha:boolean;
begin
     result:= (QoiHeader.channels=qoChannelRGBA);
end;

function TFPWriterQoi.SaveHeader(Stream:TStream; Img : TFPCustomImage):boolean;
begin
  Result:=False;
  with QoiHeader do
    begin
      Width:=Img.Width;
      Height:=Img.Height;
      //writeln('Save width ',width, '   height  ', height);
    end;

  {$IFDEF ENDIAN_LITTLE}
  QoiHeader.width:=Swap32(QoiHeader.width);
  QoiHeader.height:=Swap32(QoiHeader.height);
  {$ENDIF}

  //writeln('Save width 2 ',QoiHeader.width, '   height  ', QoiHeader.height);
  Stream.Write(QoiHeader,sizeof(TQoiHeader));

  {$IFDEF ENDIAN_LITTLE}
  QoiHeader.width:=Swap32(QoiHeader.width);
  QoiHeader.height:=Swap32(QoiHeader.height);
  {$ENDIF}
  Result:=true;
end;



procedure TFPWriterQoi.InternalWrite (Stream:TStream; Img:TFPCustomImage);
var
  Row,Col,RowSize:sizeuint;
  h, w, orgSize, mSize : sizeuint;
  aLine, p: PByte;

  color : TFPColor;


var iP,  iq, imgSize : qword;
    //q: PQoiPixel;
    b, run : byte;
    g, dr, dg, db, dr_dg, db_dg : byte;// shortint;
    px, cx : TQoiPixel;
    arr : array [0..63] of TQoiPixel;
    iA : dword; {index in pixel array}
    //endOf : qword;

begin
    mSize:= img.Width * sizeof(TQoiPixel)+ img.Width;
    RowSize:= img.Width * sizeof(TQoiPixel)+ img.Width + 8+sizeof(TQoiPixel)*64+64;

    SaveHeader(Stream,Img); { write the headers }

    GetMem(aLine,RowSize);

    p:=aLine;

    dword(px):=0;
    px.a:=255;

    {initalize previosly seen pixel array}
    fillchar(arr,sizeof(arr),0);
    iA:=QoiPixelIndex(px);
     //for iA:=0 to 63 do
     //arr[iA]:=px;

    Row:=0;
    Col:=0;
    h:=Img.Height;
    w:=Img.Width;
    iq:=0;
    ip:=0;
    imgSize:= h*w;
    if imgSize > 0 then
    while (imgSize)> iq do
    begin
         color:=img.colors[Col,Row];
         cx.r:=color.Red shr 8;
         cx.g:=color.Green shr (8);
         cx.b:=color.Blue shr (8);
         cx.a:=color.Alpha shr (8);

          iA:=QoiPixelIndex (cx);

          if dword(cx)=dword(px) then { run }
          begin
               run:=0;
               //inc (q);
               inc (iq);

               inc(col);
               if col = w then begin inc(row); col:=0; end;

               if (col < w) and  (row<h) then
               begin

                 color:=img.colors[Col,Row];
                 cx.r:=color.Red shr 8;
                 cx.g:=color.Green shr (8);
                 cx.b:=color.Blue shr (8);
                 cx.a:=color.Alpha shr (8);


               while (imgSize >= (iq+1))
                    and (dword(cx)=dword(px)) do
               begin
                    inc (run);
                    inc (iq);

                   inc(col);
               if col = w then begin
                    inc(row); col:=0;
                    if (col >= w) or  (row>=h) then break;
               end;



                 color:=img.colors[Col,Row];
                 cx.r:=color.Red shr 8;
                 cx.g:=color.Green shr (8);
                 cx.b:=color.Blue shr (8);
                 cx.a:=color.Alpha shr (8);



                    if run = 61 then break;
               end;
               end;

               b:=($ff xor 63) or run;
               p^:=b;
               inc(p);
               inc(ip);

          end else
          if dword(arr[iA]) = dword(cx) then { index }
          begin

               px:=cx;
               p^:=byte(iA);
               inc(p);
               inc(ip);
               //inc(q);
               inc(iq);

               inc(col);
               if col = w then begin inc(row); col:=0; end;


          end else
          if px.a <> cx.a then { rgba }
          begin
               b:=$ff;
               p^:=b;
               inc(p);
               px:=cx;
               //PQoiPixel(p)^:=cx;
               //inc(p,4);

               p^:=cx.r;inc(p);
               p^:=cx.g;inc(p);
               p^:=cx.b;inc(p);
               p^:=cx.a;inc(p);

               inc(ip,5);
               //inc(q);
               inc (iq);

               inc(col);
               if col = w then begin inc(row); col:=0; end;

               arr[iA]:=cx;

          end else
          begin
               dr := (cx.r - px.r);
               dg := (cx.g - px.g);
               db := (cx.b - px.b);

               px:=cx;

               dr_dg := dr-dg+8;
               db_dg := db-dg+8;

               dr:=dr+2;
               dg:=dg+2;
               db:=db+2;
               g:=dg+30;

               //inc(q);
               inc (iq);
               inc(col);
               if col = w then begin inc(row); col:=0; end;

               arr[iA]:=cx;

               if (dr and ($ff xor 3))+(dg and ($ff xor 3))+(db and ($ff xor 3)) = 0 then  { diff }
               begin
                    b:=64 or (dr shl 4) or (dg shl 2)or (db ) ;
                    p^:=b;
                    inc(p);
                    inc(ip);

               end else
               if ((g) and ($ff xor 63)) + (dr_dg and ($ff xor 15))+ (db_dg and ($ff xor 15))=0 then { luma }
               begin
                    b:=128 or g;
                    p^:=b;
                    inc(p);
                    b:=(dr_dg shl 4) or db_dg;
                    p^:=b;
                    inc(p);
                    inc(ip,2);

               end else {rgb}
               begin
                    b:=$fe;
                    p^:=b;
                    inc(p);
                    //PQoiPixel(p)^:=cx;
                    //inc(p,3);


                    p^:=cx.r;inc(p);
                    p^:=cx.g;inc(p);
                    p^:=cx.b;inc(p);

                    inc(ip,4);
               end;

          end;
          if ip >= mSize then
          begin
               {save data }
               orgSize:=ip;
               Stream.Write(aLine[0],orgSize);
               ip:=0;
               p:=aLine;
          end;
     end;


     {mark end of encoding}
     {
     endof:=qword(1) shl 56;
     pqword(p)^:=endof;
     inc(p,8);
     }
     p^:=0; inc(p);
     p^:=0; inc(p);
     p^:=0; inc(p);
     p^:=0; inc(p);
     p^:=0; inc(p);
     p^:=0; inc(p);
     p^:=0; inc(p);
     p^:=1; inc(p);

     inc(ip,8);

     orgSize:=ip;
     Stream.Write(aLine[0],orgSize);



     FreeMem(aLine);


end;

initialization
  ImageHandlers.RegisterImageWriter ('QOI Format', 'qoi', TFPWriterQoi);
end.
