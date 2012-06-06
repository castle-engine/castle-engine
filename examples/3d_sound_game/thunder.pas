{
  Copyright 2003-2012 Michalis Kamburelis.

  This file is part of "lets_take_a_walk".

  "lets_take_a_walk" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "lets_take_a_walk" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "lets_take_a_walk"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ Thunder effect (lighting and sound). }
unit Thunder;

interface

uses ALSoundAllocator, ALSoundEngine, X3DNodes;

procedure ThunderALOpen;

procedure ThunderAddLight(const BaseLights: TLightInstancesList);
{ Call roughly once per second to update thunder effect. }
procedure ThunderIdleSec;

procedure ThunderNow;

function CreateWAVBuffer(const wavname: string): TALBuffer;

implementation

uses SysUtils, CastleUtils, CastleGLUtils, CastleFilesUtils,
  VectorMath, CastleTimeUtils, Math;

var
  ThunderLightStartTick: TMilisecTime = 0;
  ThunderBuffer: TALBuffer;
  ThunderLightNode: TDirectionalLightNode;
  ThunderLight: TLightInstance;

const
  GainThunder = 0.8;

function CreateWAVBuffer(const wavname: string): TALBuffer;
begin
 result := SoundEngine.LoadBuffer(
   ProgramDataPath + 'data' + PathDelim + 'sound' + PathDelim + wavname + '.wav');
end;

procedure ThunderALOpen;
begin
  { setup thunder sound source }
  ThunderBuffer := CreateWAVBuffer('thunderrumble3');
end;

procedure ThunderAddLight(const BaseLights: TLightInstancesList);
var
  ThunderTime: TMilisecTime;
begin
  ThunderTime := MilisecTimesSubtract(GetTickCount, ThunderLightStartTick);
  if (ThunderLightStartTick <> 0) and
     ( (ThunderTime < 1000) or
       ((1500 < ThunderTime) and (ThunderTime < 2500))
     ) then
    BaseLights.Add(ThunderLight);
end;

procedure ThunderNow;
begin
  SoundEngine.PlaySound(ThunderBuffer, false, false, 1, GainThunder, 0, 1,
    ZeroVector3Single);
  ThunderLightStartTick := GetTickCount;
end;

procedure ThunderIdleSec;
begin
  if Random(10) = 0 then
    ThunderNow;
end;

initialization
  ThunderLightNode := TDirectionalLightNode.Create('', '');
  ThunderLightNode.FdDirection.Value := Vector3Single(0, 0, -1);
  ThunderLightNode.FdAmbientIntensity.Value := 1;
  ThunderLightNode.FdColor.Value := Vector3Single(1, 1, 1);

  ThunderLight.Node := ThunderLightNode;
  ThunderLight.Transform := IdentityMatrix4Single;
  ThunderLight.TransformScale := 1;
  ThunderLight.Location := ZeroVector3Single;
  ThunderLight.Direction := ThunderLightNode.FdDirection.Value;
  ThunderLight.Radius := MaxSingle;
  ThunderLight.WorldCoordinates := true;
finalization
  FreeAndNil(ThunderLightNode);
end.
