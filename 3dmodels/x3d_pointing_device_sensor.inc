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

    private FFddescription: TSFString;
    public property Fddescription: TSFString read FFddescription;

    { Event: SFBool, out } { }
    private FEventisOver: TVRMLEvent;
    public property EventisOver: TVRMLEvent read FEventisOver;
  end;

  TNodeX3DDragSensorNode = class(TNodeX3DPointingDeviceSensorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFdautoOffset: TSFBool;
    public property FdautoOffset: TSFBool read FFdautoOffset;

    { Event: SFVec3f, out } { }
    private FEventtrackPoint_changed: TVRMLEvent;
    public property EventtrackPoint_changed: TVRMLEvent read FEventtrackPoint_changed;
  end;

  TNodeX3DTouchSensorNode = class(TNodeX3DPointingDeviceSensorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    { Event: SFTime, out } { }
    private FEventtouchTime: TVRMLEvent;
    public property EventtouchTime: TVRMLEvent read FEventtouchTime;
  end;

{$endif read_interface}

{$ifdef read_implementation}

constructor TNodeX3DPointingDeviceSensorNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFddescription := TSFString.Create('description', '');
  Fields.Add(FFddescription);

  FEventisOver := TVRMLEvent.Create('isOver', TSFBool, false);
  Events.Add(FEventisOver);
end;

constructor TNodeX3DDragSensorNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdautoOffset := TSFBool.Create('autoOffset', true);
  Fields.Add(FFdautoOffset);

  FEventtrackPoint_changed := TVRMLEvent.Create('trackPoint_changed', TSFVec3f, false);
  Events.Add(FEventtrackPoint_changed);
end;

constructor TNodeX3DTouchSensorNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventtouchTime := TVRMLEvent.Create('touchTime', TSFTime, false);
  Events.Add(FEventtouchTime);
end;

{$endif read_implementation}
