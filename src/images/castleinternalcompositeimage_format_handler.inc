{%MainUnit castleinternalcompositeimage.pas}
{
  Copyright 2017-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TCompositeFormatHandler ---------------------------------------------------- }

type
  { Helper class to load / save particular TCompositeImage file format. }
  TCompositeFormatHandler = class
  strict private
    FComposite: TCompositeImage;

    function GetWidth: Cardinal;
    function GetHeight: Cardinal;
    function GetDepth: Cardinal;
    procedure SetWidth(const Value: Cardinal);
    procedure SetHeight(const Value: Cardinal);
    procedure SetDepth(const Value: Cardinal);
  public
    { Read / write the appropriate property of
      the connected TCompositeImage instance. }
    property Width: Cardinal read GetWidth write SetWidth;
    property Height: Cardinal read GetHeight write SetHeight;
    property Depth: Cardinal read GetDepth write SetDepth;

    property Composite: TCompositeImage read FComposite;

    function Images: TEncodedImageList;

    constructor Create(const AComposite: TCompositeImage);
    procedure LoadFromStream(const Stream: TStream; const Url: String); virtual;
    procedure SaveToStream(const Stream: TStream); virtual;
  end;

constructor TCompositeFormatHandler.Create(const AComposite: TCompositeImage);
begin
  inherited Create;
  FComposite := AComposite;
end;

function TCompositeFormatHandler.GetWidth: Cardinal;
begin
  Result := FComposite.Width;
end;

function TCompositeFormatHandler.GetHeight: Cardinal;
begin
  Result := FComposite.Height;
end;

function TCompositeFormatHandler.GetDepth: Cardinal;
begin
  Result := FComposite.Depth;
end;

procedure TCompositeFormatHandler.SetWidth(const Value: Cardinal);
begin
  FComposite.Width := Value;
end;

procedure TCompositeFormatHandler.SetHeight(const Value: Cardinal);
begin
  FComposite.Height := Value;
end;

function TCompositeFormatHandler.Images: TEncodedImageList;
begin
  Result := FComposite.Images;
end;

procedure TCompositeFormatHandler.SetDepth(const Value: Cardinal);
begin
  FComposite.Depth := Value;
end;

procedure TCompositeFormatHandler.LoadFromStream(const Stream: TStream; const Url: String);
begin
end;

procedure TCompositeFormatHandler.SaveToStream(const Stream: TStream);
begin
end;
