{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2022 by the Free Pascal development team

    QOI reader implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
unit FPReadQoi;

interface

uses FPImage, classes, sysutils, QoiComn;

type
  TFPReaderQoi = class (TFPCustomImageReader)
    Private
      QoiHeader : TQoiHeader;  // The header as read from the stream.
      function getUseAlpha:boolean;
    protected
      // required by TFPCustomImageReader
      procedure InternalRead  (Stream:TStream; Img:TFPCustomImage); override;
      function  InternalCheck (Stream:TStream) : boolean; override;
    public
      constructor Create; override;
      property UseAlpha : boolean read getUseAlpha;
  end;

implementation


function RGBAToFPColor(Const RGBA: TQoiPixel) : TFPcolor;

begin
  with Result, RGBA do
    begin
    Red   :=(R shl 8) or R;
    Green :=(G shl 8) or G;
    Blue  :=(B shl 8) or B;
    Alpha :=(A shl 8) or A;
    end;
end;


Constructor TFPReaderQoi.create;

begin
  inherited create;
end;

function TFPReaderQoi.getUseAlpha:boolean;
begin
     result := (QoiHeader.channels=qoChannelRGBA);
end;

function  TFPReaderQoi.InternalCheck (Stream:TStream) : boolean;
// NOTE: Does not rewind the stream!
var

  n: Int64;
begin
  Result:=False;
  if Stream=nil then
    exit;
  n:=SizeOf(TQoiHeader);
  Result:=Stream.Read(QoiHeader,n)=n;
  if Result then
    begin
   {$IFDEF ENDIAN_LITTLE}
    QoiHeader.width:=Swap32(QoiHeader.width);
    QoiHeader.height:=Swap32(QoiHeader.height);
   {$ENDIF}
    Result := (QoiHeader.magic = 'qoif'); // Just check magic number
    end;
end;

// NOTE: It is assumed that signature and IDHDR chunk already have been read.
procedure TFPReaderQoi.InternalRead (Stream:TStream; Img:TFPCustomImage);
var iP, q : qword;
    b, run : byte;
    g : shortint;
    px : TQoiPixel;
    arr : array [0..63] of TQoiPixel;
    iA : dword; {index in pixel array}

    p, aQ : pbyte; orgSize, imgSize : qword;
    Row, Col, w, h : dword;
    aLine : pbyte;


begin
     with QoiHeader do
     begin
       Img.SetSize (Width, Height);
       imgSize := Width * Height;
       w:=Width;
       h:=Height;
     end;


     orgSize:=Stream.size;
     orgSize:=orgSize-sizeof(TQoiHeader);
     getmem(aLine,orgSize);

     q:=Stream.Read(aLine^,orgSize);
     if orgSize>q then orgSize:=q;


     ip:=0;
     q:=0;
     p:=aLine;

     dword(px):=0;
     px.a:=255;

     {initalize previosly seen pixel array}
     //fillchar(arr,sizeof(arr),0);
     iA:=QoiPixelIndex(px);
     //for iA:=0 to 63 do
     arr[iA]:=px;

     Row:=0;
     Col:=0;


     {actual decoding loop}
     while (orgSize> ip) and (imgSize>q) do
     begin
          b:=p^;
          inc(p);
          inc(ip);

          case (b shr 6) of
             0: begin  { pixel from previos pixel array}

                     if b = p^ then {deal with end of encoding}
                     begin
                          if b = 0 then
                          begin
                               dec(p);
                               for iA:=0 to 6 do
                               begin
                                    b:=p^;
                                    inc(p);
                                    inc(ip);
                                    if b<>0 then break;
                               end;
                               if b<>0 then
                               begin
                                    {invalid encoding}
                                    break;
                               end;
                               b:=p^;
                               inc(p);
                               inc(ip);
                               if b = 1 then
                               begin
                                    //writeln('end of encoding ');
                                    {success - no more encoded pixels}
                                    break;
                               end else
                               begin
                                    {invalid encoding}
                                    break;
                               end;

                          end else
                          begin
                               {invalid encoding}
                               break;
                          end;
                     end;

                     {pixel from array}
                     iA:= b and 63;
                     px:=arr[iA];
                     img.Colors[Col,Row] := RGBAToFPColor( px );
                     inc(q);
                     inc(Col);
                     if Col = w then begin inc(Row); Col:=0; if Row>=h then break; end;

                end;

             1: begin { diff }
                     b:=b and 63;
                     px.r:=px.r+ byte(b shr 4) and 3+shortint(-2);
                     px.g:=px.g+ byte(b shr 2) and 3+shortint(-2);
                     px.b:=px.b+ byte(b shr 0) and 3+shortint(-2);

                     img.Colors[Col,Row] := RGBAToFPColor( px );
                     inc(q);
                     inc(Col);
                     if Col = w then begin inc(Row); Col:=0; if Row>=h then break; end;
                     iA:=QoiPixelIndex(px);
                     arr[iA]:=px;

                end;

             2: begin { luma }
                     g:=b and 63 - 32;
                     b:=p^;
                     inc(p);
                     inc(ip);
                     px.g:=px.g + g;
                     px.r:=px.r+g+shortint((b shr 4)-8);
                     px.b:=px.b+g+shortint((b and 15)-8);
                     img.Colors[Col,Row] := RGBAToFPColor( px );
                     inc(q);
                     inc(Col);
                     if Col = w then begin inc(Row); Col:=0; if Row>=h then break; end;
                     iA:=QoiPixelIndex(px);
                     arr[iA]:=px;


                end;

             3: begin
                     run:=b and 63+1;
                     case run of
                       64: begin  { rgba }
                                px.r:=p^;
                                inc(p);
                                px.g:=p^;
                                inc(p);
                                px.b:=p^;
                                inc(p);
                                px.a:=p^;
                                inc(p);
                                inc(ip,4);
                                img.Colors[Col,Row] := RGBAToFPColor( px );
                                inc(q);
                                inc(Col);
                                if Col = w then begin inc(Row); Col:=0; if Row>=h then break; end;
                                iA:=QoiPixelIndex(px);
                                arr[iA]:=px;

                          end;
                       63: begin  { rgb  }
                                px.r:=p^;
                                inc(p);
                                px.g:=p^;
                                inc(p);
                                px.b:=p^;
                                inc(p);
                                inc(ip,3);
                                img.Colors[Col,Row] := RGBAToFPColor( px );
                                inc(q);
                                inc(Col);
                                if Col = w then begin inc(Row); Col:=0; if Row>=h then break; end;
                                iA:=QoiPixelIndex(px);
                                arr[iA]:=px;

                          end;
                       otherwise { run - repeat previos pixel}
                            repeat
                                img.Colors[Col,Row] := RGBAToFPColor( px );
                                inc(q);
                                inc(Col);
                                if Col = w then begin inc(Row); Col:=0; if Row>=h then break; end;
                                dec(run);

                            until run =0;
                       end;
                end;

          end;   {case of }
     end; { while do}
     freeMem(aLine);
end;

initialization
  ImageHandlers.RegisterImageReader ('QOI Format', 'qoi', TFPReaderQoi);
end.

