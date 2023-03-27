{ Copyright (C) 2003 Mattias Gaertner

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation, Inc.,
  51 Franklin Street, Fifth Floor, Boston, MA 02111-1301, USA.
}
unit FPWriteJPEG;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPImage, JPEGLib, FPReadJPEG, JcAPIstd, JcAPImin, JDataDst,
  JcParam, JError;

type
  { TFPWriterJPEG }

  TFPJPEGCompressionQuality = 1..100;   // 100 = best quality, 25 = pretty awful

  TFPWriterJPEG = class(TFPCustomImageWriter)
  private
    FGrayscale: boolean;
    FInfo: jpeg_compress_struct;
    FError: jpeg_error_mgr;
    FProgressiveEncoding: boolean;
    FQuality: TFPJPEGCompressionQuality;
    FProgressMgr: TFPJPEGProgressManager;
  protected
    procedure InternalWrite(Str: TStream; Img: TFPCustomImage); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property CompressionQuality: TFPJPEGCompressionQuality read FQuality write FQuality;
    property ProgressiveEncoding: boolean read FProgressiveEncoding write FProgressiveEncoding;
    property GrayScale: boolean read FGrayscale write FGrayScale;
  end;

implementation

procedure JPEGError(CurInfo: j_common_ptr);
begin
  if CurInfo=nil then exit;
  {$ifdef FPC_Debug_Image}
  writeln('JPEGError ',CurInfo^.err^.msg_code,' ');
  {$endif}
  raise Exception.CreateFmt('JPEG error',[CurInfo^.err^.msg_code]);
end;

procedure EmitMessage(CurInfo: j_common_ptr; msg_level: Integer);
begin
  if CurInfo=nil then exit;
  if msg_level=0 then ;
end;

procedure OutputMessage(CurInfo: j_common_ptr);
begin
  if CurInfo=nil then exit;
end;

procedure FormatMessage(CurInfo: j_common_ptr; var buffer: string);
begin
  if CurInfo=nil then exit;
  {$ifdef FPC_Debug_Image}
  writeln('FormatMessage ',buffer);
  {$endif}
end;

procedure ResetErrorMgr(CurInfo: j_common_ptr);
begin
  if CurInfo=nil then exit;
  CurInfo^.err^.num_warnings := 0;
  CurInfo^.err^.msg_code := 0;
end;

var
  jpeg_std_error: jpeg_error_mgr;

procedure ProgressCallback(CurInfo: j_common_ptr);
begin
  if CurInfo=nil then exit;
  // ToDo
end;

{ TFPWriterJPEG }

procedure TFPWriterJPEG.InternalWrite(Str: TStream; Img: TFPCustomImage);
var
  MemStream: TMemoryStream;
  Continue: Boolean;

  procedure InitWriting;
  begin
    FillChar(FInfo, sizeof(FInfo), 0);
    FError := jpeg_std_error;
    FInfo.err := jerror.jpeg_std_error(FError);

    jpeg_create_compress(@FInfo);
    FProgressMgr.pub.progress_monitor := @ProgressCallback;
    FProgressMgr.instance := Self;
    FInfo.progress := @FProgressMgr.pub;
  end;

  procedure SetDestination;
  begin
    if Str is TMemoryStream then
      MemStream:=TMemoryStream(Str)
    else
      MemStream := TMemoryStream.Create;
    jpeg_stdio_dest(@FInfo, @MemStream);
  end;

  procedure WriteHeader;
  begin
    FInfo.image_width := Img.Width;
    FInfo.image_height := Img.Height;
    if FGrayscale then
    begin
      FInfo.input_components := 1;
      FInfo.in_color_space := JCS_GRAYSCALE;
    end
    else
    begin
      FInfo.input_components := 3; // RGB has 3 components
      FInfo.in_color_space := JCS_RGB;
    end;

    jpeg_set_defaults(@FInfo);
    jpeg_set_quality(@FInfo, FQuality, True);

    if ProgressiveEncoding then
      jpeg_simple_progression(@FInfo);
  end;

  procedure WritePixels;
  var
    LinesWritten: Cardinal;
    SampArray: JSAMPARRAY;
    SampRow: JSAMPROW;
    Color: TFPColor;
    x: Integer;
    y: Integer;
  begin
    Progress(psStarting, 0, False, Rect(0,0,0,0), '', Continue);
    if not Continue then exit;
    jpeg_start_compress(@FInfo, True);

    // write one line per call
    GetMem(SampArray,SizeOf(JSAMPROW));
    GetMem(SampRow,FInfo.image_width*FInfo.input_components);
    SampArray^[0]:=SampRow;
    try
      y:=0;
      while (FInfo.next_scanline < FInfo.image_height) do begin
        if FGrayscale then
        for x:=0 to FInfo.image_width-1 do
          SampRow^[x]:=CalculateGray(Img.Colors[x,y]) shr 8
        else
        for x:=0 to FInfo.image_width-1 do begin
          Color:=Img.Colors[x,y];
          SampRow^[x*3+0]:=Color.Red shr 8;
          SampRow^[x*3+1]:=Color.Green shr 8;
          SampRow^[x*3+2]:=Color.Blue shr 8;
        end;
        LinesWritten := jpeg_write_scanlines(@FInfo, SampArray, 1);
        if LinesWritten<1 then break;
        inc(y);
      end;
    finally
      FreeMem(SampRow);
      FreeMem(SampArray);
    end;

    jpeg_finish_compress(@FInfo);
    Progress(psEnding, 100, False, Rect(0,0,0,0), '', Continue);
  end;

  procedure EndWriting;
  begin
    jpeg_destroy_compress(@FInfo);
  end;

begin
  Continue := true;
  MemStream:=nil;
  try
    InitWriting;
    SetDestination;
    WriteHeader;
    WritePixels;
    if MemStream<>Str then begin
      MemStream.Position:=0;
      Str.CopyFrom(MemStream,MemStream.Size);
    end;
  finally
    EndWriting;
    if MemStream<>Str then
      MemStream.Free;
  end;
end;

constructor TFPWriterJPEG.Create;
begin
  inherited Create;
  FQuality:=75;
end;

destructor TFPWriterJPEG.Destroy;
begin
  inherited Destroy;
end;

initialization
  with jpeg_std_error do begin
    error_exit:=@JPEGError;
    emit_message:=@EmitMessage;
    output_message:=@OutputMessage;
    format_message:=@FormatMessage;
    reset_error_mgr:=@ResetErrorMgr;
  end;
  ImageHandlers.RegisterImageWriter ('JPEG graphics', 'jpg;jpeg', TFPWriterJPEG);
end.
