{
  Copyright 2002-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

type
  { }
  INodeX3DNode = interface(IVRMLNode)
  ['{399AC6B2-C2B3-4EEA-AB74-D52C4836B3B9}']
    property Fdmetadata: TSFNode;
  end;

  { Base X3D node.

    Almost all X3D nodes inherit from this. (Exception:
    X3DMetadataObject. So you cannot have metadata of metadata of...) }
  TNodeX3DNode = class(TVRMLNode, INodeX3DNode)
  public
    procedure CreateNode; override;

    private FFdmetadata: TSFNode;
    public property Fdmetadata: TSFNode read FFdmetadata;
  end;

  INodeX3DChildNode = interface(INodeX3DNode)
  ['{ECE6F71D-CA90-4B6C-8835-EAA68EEDAEED}']
  end;

  TNodeX3DChildNode = class(TNodeX3DNode, INodeX3DChildNode)
  public
    procedure CreateNode; override;
  end;

  TNodeX3DBindableNode = class(TNodeX3DChildNode)
  private
    FTransform: TMatrix4Single;
    FTransformScale: Single;
  protected
    procedure BeforeTraverse(StateStack: TVRMLGraphTraverseStateStack); override;
  public
    procedure CreateNode; override;

    { Event: SFBool, in } { }
    private FEventset_bind: TVRMLEvent;
    public property Eventset_bind: TVRMLEvent read FEventset_bind;

    { Event: SFTime, out } { }
    private FEventbindTime: TVRMLEvent;
    public property EventbindTime: TVRMLEvent read FEventbindTime;

    { Event: SFBool, out } { }
    private FEventisBound: TVRMLEvent;
    public property EventisBound: TVRMLEvent read FEventisBound;

    { Transformation of this bindable node.

      Bound nodes cannot be instantiated multiple number of times
      (this would make simple event like "set_bind" not possible,
      as it would not be known in what coordinate space the node is bound),
      so it's perfectly safe and comfortable to just keep their transformation
      here, a their property.

      It is gathered during traversing. Last BeforeTraverse call for this
      node sets Transform properties. By default, these represent identity
      transformation.

      Note that using TransformScale for bindable nodes like fog
      is a little simplification. Theoretically, the scale can be non-uniform,
      and around an arbitrary axis. So to apply e.g. fog, we should
      transform the 3D world back into local fog coordinate system,
      and calculate the distances there. Instead right now we do
      the opposite: transform stuff like fog VisibilityRange by
      TransformScale, and calculate distances in world coordinate
      system. This is a simplification, but in practice it's perfect
      (who uses non-uniform fog scale?) and it can be expressed for renderers
      (OpenGL) without any problems.

      @groupBegin }
    property Transform: TMatrix4Single read FTransform;
    property TransformScale: Single read FTransformScale;
    { @groupEnd }
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
    procedure CreateNode; override;

    private FFdenabled: TSFBool;
    public property Fdenabled: TSFBool read FFdenabled;

    { Event: SFBool, out } { }
    private FEventisActive: TVRMLEvent;
    public property EventisActive: TVRMLEvent read FEventisActive;
  end;

  TNodeMetadataDouble = class(TNodeX3DNode, INodeX3DMetadataObject)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
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
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
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
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
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
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
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
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
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
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdinfo: TMFString;
    public property Fdinfo: TMFString read FFdinfo;

    private FFdtitle: TSFString;
    public property Fdtitle: TSFString read FFdtitle;
  end;

{$endif read_interface}

{$ifdef read_implementation}

procedure TNodeX3DNode.CreateNode;
begin
  inherited;

  FFdmetadata := TSFNode.Create(Self, 'metadata', INodeX3DMetadataObject);
  Fields.Add(FFdmetadata);
end;

procedure TNodeX3DChildNode.CreateNode;
begin
  inherited;

  { It's natural that X3DChildNode goes into "children" node of parent.
    Actually, this makes many other
      DefaultContainerField := 'children'
    lines in our x3d_*.inc include files useless. }
  DefaultContainerField := 'children';
end;

procedure TNodeX3DBindableNode.CreateNode;
begin
  inherited;

  FEventset_bind := TVRMLEvent.Create(Self, 'set_bind', TSFBool, true);
  Events.Add(FEventset_bind);

  FEventbindTime := TVRMLEvent.Create(Self, 'bindTime', TSFTime, false);
  Events.Add(FEventbindTime);

  FEventisBound := TVRMLEvent.Create(Self, 'isBound', TSFBool, false);
  Events.Add(FEventisBound);

  FTransform := IdentityMatrix4Single;
  FTransformScale := 1;
end;

procedure TNodeX3DBindableNode.BeforeTraverse(
  StateStack: TVRMLGraphTraverseStateStack);
begin
  inherited;

  FTransform := StateStack.Top.Transform;
  FTransformScale := StateStack.Top.TransformScale;
end;

procedure TNodeX3DSensorNode.CreateNode;
begin
  inherited;

  FFdenabled := TSFBool.Create(Self, 'enabled', true);
  Fields.Add(FFdenabled);

  FEventisActive := TVRMLEvent.Create(Self, 'isActive', TSFBool, false);
  Events.Add(FEventisActive);
end;

procedure TNodeMetadataDouble.CreateNode;
begin
  inherited;

  FFdname := TSFString.Create(Self, 'name', '');
  Fields.Add(FFdname);

  FFdreference := TSFString.Create(Self, 'reference', '');
  Fields.Add(FFdreference);

  FFdvalue := TMFDouble.Create(Self, 'value', []);
  Fields.Add(FFdvalue);

  DefaultContainerField := 'metadata';
end;

class function TNodeMetadataDouble.ClassNodeTypeName: string;
begin
  Result := 'MetadataDouble';
end;

function TNodeMetadataDouble.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
end;

class function TNodeMetadataDouble.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeMetadataFloat.CreateNode;
begin
  inherited;

  FFdname := TSFString.Create(Self, 'name', '');
  Fields.Add(FFdname);

  FFdreference := TSFString.Create(Self, 'reference', '');
  Fields.Add(FFdreference);

  FFdvalue := TMFFloat.Create(Self, 'value', []);
  Fields.Add(FFdvalue);

  DefaultContainerField := 'metadata';
end;

class function TNodeMetadataFloat.ClassNodeTypeName: string;
begin
  Result := 'MetadataFloat';
end;

function TNodeMetadataFloat.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
end;

class function TNodeMetadataFloat.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeMetadataInteger.CreateNode;
begin
  inherited;

  FFdname := TSFString.Create(Self, 'name', '');
  Fields.Add(FFdname);

  FFdreference := TSFString.Create(Self, 'reference', '');
  Fields.Add(FFdreference);

  FFdvalue := TMFInt32.Create(Self, 'value', []);
  Fields.Add(FFdvalue);

  DefaultContainerField := 'metadata';
end;

class function TNodeMetadataInteger.ClassNodeTypeName: string;
begin
  Result := 'MetadataInteger';
end;

function TNodeMetadataInteger.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
end;

class function TNodeMetadataInteger.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeMetadataSet.CreateNode;
begin
  inherited;

  FFdname := TSFString.Create(Self, 'name', '');
  Fields.Add(FFdname);

  FFdreference := TSFString.Create(Self, 'reference', '');
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
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
end;

class function TNodeMetadataSet.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeMetadataString.CreateNode;
begin
  inherited;

  FFdname := TSFString.Create(Self, 'name', '');
  Fields.Add(FFdname);

  FFdreference := TSFString.Create(Self, 'reference', '');
  Fields.Add(FFdreference);

  FFdvalue := TMFString.Create(Self, 'value', []);
  Fields.Add(FFdvalue);

  DefaultContainerField := 'metadata';
end;

class function TNodeMetadataString.ClassNodeTypeName: string;
begin
  Result := 'MetadataString';
end;

function TNodeMetadataString.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
end;

class function TNodeMetadataString.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeWorldInfo.CreateNode;
begin
  inherited;

  FFdinfo := TMFString.Create(Self, 'info', []);
  FFdinfo.Exposed := false;
  Fields.Add(FFdinfo);

  FFdtitle := TSFString.Create(Self, 'title', '');
  FFdtitle.Exposed := false;
  Fields.Add(FFdtitle);
end;

class function TNodeWorldInfo.ClassNodeTypeName: string;
begin
  Result := 'WorldInfo';
end;

function TNodeWorldInfo.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, VRML2Version, 100);
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
