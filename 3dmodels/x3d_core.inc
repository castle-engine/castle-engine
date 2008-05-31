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
  TNodeX3DMetadataObject = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFdname: TSFString;
    public property Fdname: TSFString read FFdname;

    private FFdreference: TSFString;
    public property Fdreference: TSFString read FFdreference;
  end;

  INodeX3DNode = interface(IVRMLNode)
    property Fdmetadata: TSFNode;
  end;

  { Base X3D node.

    Almost all X3D nodes inherit from this. (Exception:
    X3DMetadataObject. So you cannot have metadata of metadata of...)

    (TODO: or, at least, they should.
    Many VRML 1.0 and 2.0 nodes should be upgraded to X3D nodes by making
    them inherit from this node. For now, this is not done for all nodes,
    because it would force me to recalculate all FdXxx indexes + 1.
    I have to figure out better method than these indexes.) }
  TNodeX3DNode = class(TVRMLNode, INodeX3DNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFdmetadata: TSFNode;
    public property Fdmetadata: TSFNode read FFdmetadata;
  end;

  INodeX3DChildNode = interface(INodeX3DNode)
  end;

  TNodeX3DChildNode = class(TNodeX3DNode, INodeX3DChildNode)
  end;

  TNodeX3DBindableNode = class(TNodeX3DChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    { Event: SFBool, in } { }
    private FEventset_bind: TVRMLEvent;
    public property Eventset_bind: TVRMLEvent read FEventset_bind;

    { Event: SFTime, out } { }
    private FEventbindTime: TVRMLEvent;
    public property EventbindTime: TVRMLEvent read FEventbindTime;

    { Event: SFBool, out } { }
    private FEventisBound: TVRMLEvent;
    public property EventisBound: TVRMLEvent read FEventisBound;
  end;

  TNodeX3DSensorNode = class(TNodeX3DChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFdenabled: TSFBool;
    public property Fdenabled: TSFBool read FFdenabled;

    { Event: SFBool, out } { }
    private FEventisActive: TVRMLEvent;
    public property EventisActive: TVRMLEvent read FEventisActive;
  end;

{$endif read_interface}

{$ifdef read_implementation}

constructor TNodeX3DMetadataObject.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdname := TSFString.Create('name', '');
  Fields.Add(FFdname);

  FFdreference := TSFString.Create('reference', '');
  Fields.Add(FFdreference);
end;

constructor TNodeX3DNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdmetadata := TSFNode.Create(Self, 'metadata', [TNodeX3DMetadataObject]);
  Fields.Add(FFdmetadata);
end;

constructor TNodeX3DBindableNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventset_bind := TVRMLEvent.Create('set_bind', TSFBool, true);
  Events.Add(FEventset_bind);

  FEventbindTime := TVRMLEvent.Create('bindTime', TSFTime, false);
  Events.Add(FEventbindTime);

  FEventisBound := TVRMLEvent.Create('isBound', TSFBool, false);
  Events.Add(FEventisBound);
end;

constructor TNodeX3DSensorNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdenabled := TSFBool.Create('enabled', true);
  Fields.Add(FFdenabled);

  FEventisActive := TVRMLEvent.Create('isActive', TSFBool, false);
  Events.Add(FEventisActive);
end;

{$endif read_implementation}
