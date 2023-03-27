{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Image Canvas - canvas which draws on an image.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
unit FPImgCanv;

interface

uses FPPixlCanv, FPImage, classes;

type
  TFPImageCanvas = class (TFPPixelCanvas)
  protected
    FImage : TFPCustomImage;
    procedure SetColor (x,y:integer; const AValue:TFPColor); override;
    function  GetColor (x,y:integer) : TFPColor; override;
    procedure SetHeight (AValue : integer); override;
    function  GetHeight : integer; override;
    procedure SetWidth (AValue : integer); override;
    function  GetWidth : integer; override;
  public
    constructor create (AnImage : TFPCustomImage);
    destructor destroy; override;
    property Image : TFPCustomImage read FImage write FImage;
  end;

implementation

uses clipping;

constructor TFPImageCanvas.create (AnImage : TFPCustomImage);
begin
  inherited Create;
  FImage := AnImage;
end;

destructor TFPImageCanvas.destroy;
begin
  inherited destroy;
end;

procedure TFPImageCanvas.SetColor (x,y:integer; const AValue:TFPColor);
begin
  if (x >= 0) and (x < width) and (y >= 0) and (y < height) then
    if not clipping or PointInside (x,y, ClipRect) then
      FImage.Colors[x,y] := AValue;
end;

function  TFPImageCanvas.GetColor (x,y:integer) : TFPColor;
begin
  if (x >= 0) and (x < width) and (y >= 0) and (y < height) then
    result := FImage.Colors[x,y]
  else
    result := colTransparent;
end;

procedure TFPImageCanvas.SetHeight (AValue : integer);
begin
  FImage.Height := AValue;
end;

function  TFPImageCanvas.GetHeight : integer;
begin
  result := FImage.Height;
end;

procedure TFPImageCanvas.SetWidth (AValue : integer);
begin
  FImage.Width := AValue;
end;

function  TFPImageCanvas.GetWidth : integer;
begin
  result := FImage.Width;
end;

end.
