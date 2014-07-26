{
  Copyright 2014-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Reading and rendering Spine [http://esotericsoftware.com/] animations. }
unit CastleSpine;

interface

uses FpJson,
  CastleUIControls, CastleRectangles;

type
  TSpineAnimation = class(TUIRectangularControl)
  private
    Json: TJSONData;
  public
    destructor Destroy; override;
    procedure Load(const URL: string);
    procedure Unload;
    function Rect: TRectangle; override;
  end;

implementation

uses SysUtils, Classes, JSONParser,
  CastleURIUtils, CastleDownload;

destructor TSpineAnimation.Destroy;
begin
  Unload;
  inherited;
end;

procedure TSpineAnimation.Load(const URL: string);
var
  S: TStream;
  P: TJSONParser;
  NewJson: TJSONData;
begin
  S := Download(URL);
  try
    P := TJSONParser.Create(S);
    try
      NewJson := P.Parse;
    finally FreeAndNil(P) end;
  finally FreeAndNil(S) end;

  Unload;
  Json := NewJson;

  Writeln('Parse succesful. Dumping JSON data : ');
  If Assigned(Json) then
  begin
    Writeln('Returned JSON structure has class : ', Json.ClassName);
    Writeln(Json.AsJSON);
  end else
    Writeln('No JSON data available');
end;

procedure TSpineAnimation.Unload;
begin
  FreeAndNil(Json);
end;

function TSpineAnimation.Rect: TRectangle;
begin
  Result := TRectangle.Empty;
end;

end.