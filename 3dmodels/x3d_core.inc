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

  { Base X3D node.

    Almost all X3D nodes inherit from this. (Exception:
    X3DMetadataObject. So you cannot have metadata of metadata of...)

    (TODO: or, at least, they should.
    Many VRML 1.0 and 2.0 nodes should be upgraded to X3D nodes by making
    them inherit from this node. For now, this is not done for all nodes,
    because it would force me to recalculate all FdXxx indexes + 1.
    I have to figure out better method than these indexes.) }
  TNodeX3DNode = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFdmetadata: TSFNode;
    public property Fdmetadata: TSFNode read FFdmetadata;
  end;

  TNodeX3DChildNode = class(TNodeX3DNode)
  end;

  TNodeX3DBindableNode = class(TNodeX3DChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    { eventIn      SFBool     set_bind } { }

    { eventOut     SFTime     bindTime } { }

    { eventOut     SFBool     isBound } { }
  end;

  TNodeX3DSensorNode = class(TNodeX3DChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFdenabled: TSFBool;
    public property Fdenabled: TSFBool read FFdenabled;

    { property FdisActive: TSFBool ([out]) } { }
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

  Events.Add(TVRMLEvent.Create('set_bind', TSFBool, true));

  Events.Add(TVRMLEvent.Create('bindTime', TSFTime, false));

  Events.Add(TVRMLEvent.Create('isBound', TSFBool, false));
end;

constructor TNodeX3DSensorNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdenabled := TSFBool.Create('enabled', true);
  Fields.Add(FFdenabled);

  Events.Add(TVRMLEvent.Create('isActive', TSFBool, false));
end;

{$endif read_implementation}
