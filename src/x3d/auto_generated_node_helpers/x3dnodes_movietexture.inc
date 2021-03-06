{ -*- buffer-read-only: t -*-

  Copyright 2015-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Automatically generated node properties.

  Do not edit this file manually!
  To add new properties:
  - add them to the text files in tools/internal/x3d-nodes-to-pascal/nodes-specification/ ,
  - and regenerate include files by running x3d-nodes-to-pascal .

  The documentation for properties should go to x3dnodes_documentation.txt . }

{$ifdef read_interface}

  strict private
    function GetDescription: String;
    procedure SetDescription(const Value: String);
    function GetLoop: Boolean;
    procedure SetLoop(const Value: Boolean);
    function GetPauseTime: TFloatTime;
    procedure SetPauseTime(const Value: TFloatTime);
    function GetResumeTime: TFloatTime;
    procedure SetResumeTime(const Value: TFloatTime);
    function GetSpeed: Single;
    procedure SetSpeed(const Value: Single);
    function GetStartTime: TFloatTime;
    procedure SetStartTime(const Value: TFloatTime);
    function GetStopTime: TFloatTime;
    procedure SetStopTime(const Value: TFloatTime);
    function GetFlipVertically: Boolean;
    procedure SetFlipVertically(const Value: Boolean);
  public
    { X3D field "description". } { }
    property Description: String read GetDescription write SetDescription;
    { X3D field "loop". } { }
    property Loop: Boolean read GetLoop write SetLoop;
    { X3D field "pauseTime". } { }
    property PauseTime: TFloatTime read GetPauseTime write SetPauseTime;
    { X3D field "resumeTime". } { }
    property ResumeTime: TFloatTime read GetResumeTime write SetResumeTime;
    { X3D field "speed". } { }
    property Speed: Single read GetSpeed write SetSpeed;
    { X3D field "startTime". } { }
    property StartTime: TFloatTime read GetStartTime write SetStartTime;
    { X3D field "stopTime". } { }
    property StopTime: TFloatTime read GetStopTime write SetStopTime;
    { Set X3D field "url". } { }
    procedure SetUrl(const Value: array of string);
    { Set X3D field "url". } { }
    procedure SetUrl(const Value: TCastleStringList);
    { X3D field "flipVertically". } { }
    property FlipVertically: Boolean read GetFlipVertically write SetFlipVertically;

{$endif read_interface}

{$ifdef read_implementation}

{ TMovieTextureNode ----------------------------------------------- }

function TMovieTextureNode.GetDescription: String;
begin
  Result := FdDescription.Value;
end;

procedure TMovieTextureNode.SetDescription(const Value: String);
begin
  FdDescription.Send(Value);
end;

function TMovieTextureNode.GetLoop: Boolean;
begin
  Result := FdLoop.Value;
end;

procedure TMovieTextureNode.SetLoop(const Value: Boolean);
begin
  FdLoop.Send(Value);
end;

function TMovieTextureNode.GetPauseTime: TFloatTime;
begin
  Result := FdPauseTime.Value;
end;

procedure TMovieTextureNode.SetPauseTime(const Value: TFloatTime);
begin
  FdPauseTime.Send(Value);
end;

function TMovieTextureNode.GetResumeTime: TFloatTime;
begin
  Result := FdResumeTime.Value;
end;

procedure TMovieTextureNode.SetResumeTime(const Value: TFloatTime);
begin
  FdResumeTime.Send(Value);
end;

function TMovieTextureNode.GetSpeed: Single;
begin
  Result := FdSpeed.Value;
end;

procedure TMovieTextureNode.SetSpeed(const Value: Single);
begin
  FdSpeed.Send(Value);
end;

function TMovieTextureNode.GetStartTime: TFloatTime;
begin
  Result := FdStartTime.Value;
end;

procedure TMovieTextureNode.SetStartTime(const Value: TFloatTime);
begin
  FdStartTime.Send(Value);
end;

function TMovieTextureNode.GetStopTime: TFloatTime;
begin
  Result := FdStopTime.Value;
end;

procedure TMovieTextureNode.SetStopTime(const Value: TFloatTime);
begin
  FdStopTime.Send(Value);
end;

procedure TMovieTextureNode.SetUrl(const Value: array of string);
begin
  FdUrl.Send(Value);
end;

procedure TMovieTextureNode.SetUrl(const Value: TCastleStringList);
begin
  FdUrl.Send(Value);
end;

function TMovieTextureNode.GetFlipVertically: Boolean;
begin
  Result := FdFlipVertically.Value;
end;

procedure TMovieTextureNode.SetFlipVertically(const Value: Boolean);
begin
  FdFlipVertically.Send(Value);
end;

{$endif read_implementation}
