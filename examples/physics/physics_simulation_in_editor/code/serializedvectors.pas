{
  Copyright 2022-2022 Michalis Kamburelis, Andrzej Kilijański.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TSerializedVector3 }
unit SerializedVectors;

interface

uses
  Classes, SysUtils, CastleVectors;

type
  TSerializedVector3 = class(TPersistent)
  private
    FVector3: TVector3;
    function GetX: Single;
    function GetY: Single;
    function GetZ: Single;
    procedure SetX(const AValue: Single);
    procedure SetY(const AValue: Single);
    procedure SetZ(const AValue: Single);

  public
    constructor Create;
    destructor Destroy; override;

    { Use pointers to avoid copying. }
    function GetPVector3: PVector3;
    procedure SetVector3(const Vector: TVector3);
  published
    property X: Single read GetX write SetX;
    property Y: Single read GetY write SetY;
    property Z: Single read GetZ write SetZ;
  end;

implementation

{ TSerializedVector3 --------------------------------------------------------- }

function TSerializedVector3.GetX: Single;
begin
  Result := FVector3.X;
end;

function TSerializedVector3.GetY: Single;
begin
  Result := FVector3.Y;
end;

function TSerializedVector3.GetZ: Single;
begin
  Result := FVector3.Z;
end;

procedure TSerializedVector3.SetX(const AValue: Single);
begin
  FVector3.X := AValue;
end;

procedure TSerializedVector3.SetY(const AValue: Single);
begin
  FVector3.Y := AValue;
end;

procedure TSerializedVector3.SetZ(const AValue: Single);
begin
  FVector3.Z := AValue;
end;

constructor TSerializedVector3.Create;
begin
  FVector3 := Vector3(0,0,0);
end;

destructor TSerializedVector3.Destroy;
begin
  inherited Destroy;
end;

function TSerializedVector3.GetPVector3: PVector3;
begin
  Result := @FVector3;
end;

procedure TSerializedVector3.SetVector3(const Vector: TVector3);
begin
  FVector3.X := Vector.X;
  FVector3.Y := Vector.Y;
  FVector3.Z := Vector.Z;
end;

end.

