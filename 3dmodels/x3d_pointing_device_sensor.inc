{
  Copyright 2007-2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{$ifdef read_interface}

type
  TNodeX3DPointingDeviceSensorNode = class(TNodeX3DSensorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    property Fddescription: TSFString index 1 read GetFieldAsSFString;
    { SFBool   [out]    isOver } { }
  end;

  TNodeX3DDragSensorNode = class(TNodeX3DPointingDeviceSensorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    property FdautoOffset: TSFBool index 2 read GetFieldAsSFBool;
    { SFVec3f  [out]    trackPoint_changed }
  end;

  TNodeX3DTouchSensorNode = class(TNodeX3DPointingDeviceSensorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    { SFTime   [out]    touchTime } { }
  end;

{$endif read_interface}

{$ifdef read_implementation}

constructor TNodeX3DPointingDeviceSensorNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TSFString.Create('description', ''));

  Events.Add(TVRMLEvent.Create('isOver', TSFBool, false));
end;

constructor TNodeX3DDragSensorNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TSFBool.Create('autoOffset', true));

  Events.Add(TVRMLEvent.Create('trackPoint_changed', TSFVec3f, false));
end;

constructor TNodeX3DTouchSensorNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  Events.Add(TVRMLEvent.Create('touchTime', TSFTime, false));
end;

{$endif read_implementation}
