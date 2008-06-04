{
  Copyright 2002-2008 Michalis Kamburelis.

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
  INodeX3DNode = interface(IVRMLNode)
  ['{399AC6B2-C2B3-4EEA-AB74-D52C4836B3B9}']
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
  ['{ECE6F71D-CA90-4B6C-8835-EAA68EEDAEED}']
  end;

  TNodeX3DChildNode = class(TNodeX3DNode, INodeX3DChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
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

  TNodeX3DInfoNode = class(TNodeX3DChildNode)
  end;

  INodeX3DMetadataObject = interface(IVRMLNode)
  ['{2D631FBF-683B-4259-A98D-74589893447C}']
    property Fdname: TSFString { read GetFdname }; { }
    property Fdreference: TSFString { read GetFdreference }; { }
  end;

  TNodeX3DPrototypeInstance = class(TNodeX3DNode)
  end;

  { X3DSensorNode when it's a secondary ancestor.
    (When it's a primary ancestor, I use class TNodeX3DSensorNode.) }
  INodeX3DSensorNode = interface(INodeX3DChildNode)
  ['{7434C62F-8084-40C3-AA57-08F9B574655A}']
    property Fdenabled: TSFBool;

    { Event: SFBool, out } { }
    property EventisActive: TVRMLEvent;
  end;

  TNodeX3DSensorNode = class(TNodeX3DChildNode, INodeX3DSensorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFdenabled: TSFBool;
    public property Fdenabled: TSFBool read FFdenabled;

    { Event: SFBool, out } { }
    private FEventisActive: TVRMLEvent;
    public property EventisActive: TVRMLEvent read FEventisActive;
  end;

  TNodeMetadataDouble = class(TNodeX3DNode, INodeX3DMetadataObject)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdname: TSFString;
    public property Fdname: TSFString read FFdname;

    private FFdreference: TSFString;
    public property Fdreference: TSFString read FFdreference;

    private FFdvalue: TMFDouble;
    public property Fdvalue: TMFDouble read FFdvalue;
  end;

  TNodeMetadataFloat = class(TNodeX3DNode, INodeX3DMetadataObject)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdname: TSFString;
    public property Fdname: TSFString read FFdname;

    private FFdreference: TSFString;
    public property Fdreference: TSFString read FFdreference;

    private FFdvalue: TMFFloat;
    public property Fdvalue: TMFFloat read FFdvalue;
  end;

  TNodeMetadataInteger = class(TNodeX3DNode, INodeX3DMetadataObject)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdname: TSFString;
    public property Fdname: TSFString read FFdname;

    private FFdreference: TSFString;
    public property Fdreference: TSFString read FFdreference;

    private FFdvalue: TMFInt32;
    public property Fdvalue: TMFInt32 read FFdvalue;
  end;

  TNodeMetadataSet = class(TNodeX3DNode, INodeX3DMetadataObject)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdname: TSFString;
    public property Fdname: TSFString read FFdname;

    private FFdreference: TSFString;
    public property Fdreference: TSFString read FFdreference;

    private FFdvalue: TMFNode;
    public property Fdvalue: TMFNode read FFdvalue;
  end;

  TNodeMetadataString = class(TNodeX3DNode, INodeX3DMetadataObject)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdname: TSFString;
    public property Fdname: TSFString read FFdname;

    private FFdreference: TSFString;
    public property Fdreference: TSFString read FFdreference;

    private FFdvalue: TMFString;
    public property Fdvalue: TMFString read FFdvalue;
  end;

  TNodeWorldInfo = class(TNodeX3DInfoNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdinfo: TMFString;
    public property Fdinfo: TMFString read FFdinfo;

    private FFdtitle: TSFString;
    public property Fdtitle: TSFString read FFdtitle;
  end;

{$endif read_interface}

{$ifdef read_implementation}

constructor TNodeX3DNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdmetadata := TSFNode.Create(Self, 'metadata', INodeX3DMetadataObject);
  Fields.Add(FFdmetadata);
end;

constructor TNodeX3DChildNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  { It's natural that X3DChildNode goes into "children" node of parent.
    Actually, this makes many other
      DefaultContainerField := 'children'
    lines in our x3d_*.inc include files useless. }
  DefaultContainerField := 'children';
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

constructor TNodeMetadataDouble.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdname := TSFString.Create('name', '');
  Fields.Add(FFdname);

  FFdreference := TSFString.Create('reference', '');
  Fields.Add(FFdreference);

  FFdvalue := TMFDouble.Create('value', []);
  Fields.Add(FFdvalue);

  DefaultContainerField := 'metadata';
end;

class function TNodeMetadataDouble.ClassNodeTypeName: string;
begin
  Result := 'MetadataDouble';
end;

function TNodeMetadataDouble.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeMetadataDouble.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeMetadataFloat.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdname := TSFString.Create('name', '');
  Fields.Add(FFdname);

  FFdreference := TSFString.Create('reference', '');
  Fields.Add(FFdreference);

  FFdvalue := TMFFloat.Create('value', []);
  Fields.Add(FFdvalue);

  DefaultContainerField := 'metadata';
end;

class function TNodeMetadataFloat.ClassNodeTypeName: string;
begin
  Result := 'MetadataFloat';
end;

function TNodeMetadataFloat.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeMetadataFloat.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeMetadataInteger.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdname := TSFString.Create('name', '');
  Fields.Add(FFdname);

  FFdreference := TSFString.Create('reference', '');
  Fields.Add(FFdreference);

  FFdvalue := TMFInt32.Create('value', []);
  Fields.Add(FFdvalue);

  DefaultContainerField := 'metadata';
end;

class function TNodeMetadataInteger.ClassNodeTypeName: string;
begin
  Result := 'MetadataInteger';
end;

function TNodeMetadataInteger.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeMetadataInteger.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeMetadataSet.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdname := TSFString.Create('name', '');
  Fields.Add(FFdname);

  FFdreference := TSFString.Create('reference', '');
  Fields.Add(FFdreference);

  FFdvalue := TMFNode.Create(Self, 'value', INodeX3DMetadataObject);
  Fields.Add(FFdvalue);

  DefaultContainerField := 'metadata';
end;

class function TNodeMetadataSet.ClassNodeTypeName: string;
begin
  Result := 'MetadataSet';
end;

function TNodeMetadataSet.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeMetadataSet.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeMetadataString.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdname := TSFString.Create('name', '');
  Fields.Add(FFdname);

  FFdreference := TSFString.Create('reference', '');
  Fields.Add(FFdreference);

  FFdvalue := TMFString.Create('value', []);
  Fields.Add(FFdvalue);

  DefaultContainerField := 'metadata';
end;

class function TNodeMetadataString.ClassNodeTypeName: string;
begin
  Result := 'MetadataString';
end;

function TNodeMetadataString.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeMetadataString.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeWorldInfo.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdinfo := TMFString.Create('info', []);
  FFdinfo.Exposed := false;
  Fields.Add(FFdinfo);

  FFdtitle := TSFString.Create('title', '');
  FFdtitle.Exposed := false;
  Fields.Add(FFdtitle);
end;

class function TNodeWorldInfo.ClassNodeTypeName: string;
begin
  Result := 'WorldInfo';
end;

function TNodeWorldInfo.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 100);
end;

class function TNodeWorldInfo.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterCoreNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeMetadataDouble,
    TNodeMetadataFloat,
    TNodeMetadataInteger,
    TNodeMetadataSet,
    TNodeMetadataString,
    TNodeWorldInfo
  ]);
end;

{$endif read_implementation}
