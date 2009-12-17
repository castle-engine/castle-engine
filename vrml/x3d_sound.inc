{
  Copyright 2002-2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{$ifdef read_interface}
  { }
  TNodeX3DSoundNode = class(TNodeX3DChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
  end;

  INodeX3DSoundSourceNode = interface(INodeX3DTimeDependentNode)
  ['{5E3D46A0-71B5-4561-B6B4-43A5BE5896EF}']
    property Fddescription: TSFString { read FFddescription } { };
    property Fdpitch: TSFFloat { read FFdpitch } { };
    property Eventduration_changed: TVRMLEvent { read FEventduration_changed } { };
  end;

  TNodeX3DSoundSourceNode = class(TNodeX3DTimeDependentNode, INodeX3DSoundSourceNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFddescription: TSFString;
    public property Fddescription: TSFString read FFddescription;

    private FFdpitch: TSFFloat;
    public property Fdpitch: TSFFloat read FFdpitch;

    { Event: SFTime, out } { }
    private FEventduration_changed: TVRMLEvent;
    public property Eventduration_changed: TVRMLEvent read FEventduration_changed;
  end;

  TNodeAudioClip = class(TNodeX3DSoundSourceNode, INodeX3DUrlObject)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;
  end;

  TNodeSound = class(TNodeX3DSoundNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFddirection: TSFVec3f;
    public property Fddirection: TSFVec3f read FFddirection;

    private FFdintensity: TSFFloat;
    public property Fdintensity: TSFFloat read FFdintensity;

    private FFdlocation: TSFVec3f;
    public property Fdlocation: TSFVec3f read FFdlocation;

    private FFdmaxBack: TSFFloat;
    public property FdmaxBack: TSFFloat read FFdmaxBack;

    private FFdmaxFront: TSFFloat;
    public property FdmaxFront: TSFFloat read FFdmaxFront;

    private FFdminBack: TSFFloat;
    public property FdminBack: TSFFloat read FFdminBack;

    private FFdminFront: TSFFloat;
    public property FdminFront: TSFFloat read FFdminFront;

    private FFdpriority: TSFFloat;
    public property Fdpriority: TSFFloat read FFdpriority;

    private FFdsource: TSFNode;
    public property Fdsource: TSFNode read FFdsource;

    private FFdspatialize: TSFBool;
    public property Fdspatialize: TSFBool read FFdspatialize;
  end;

{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeX3DSoundNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'children';
end;

constructor TNodeX3DSoundSourceNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFddescription := TSFString.Create(Self, 'description', '');
  Fields.Add(FFddescription);

  FFdpitch := TSFFloat.Create(Self, 'pitch', 1.0);
  Fields.Add(FFdpitch);
  { X3D specification comment: (0,Inf) }

  FEventduration_changed := TVRMLEvent.Create(Self, 'duration_changed', TSFTime, false);
  Events.Add(FEventduration_changed);

  DefaultContainerField := 'children';
end;

constructor TNodeAudioClip.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdurl := TMFString.Create(Self, 'url', []);
  Fields.Add(FFdurl);
  { X3D specification comment: [urn] }

  DefaultContainerField := 'children';
end;

class function TNodeAudioClip.ClassNodeTypeName: string;
begin
  Result := 'AudioClip';
end;

function TNodeAudioClip.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeAudioClip.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeSound.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFddirection := TSFVec3f.Create(Self, 'direction', Vector3Single(0, 0, 1));
  Fields.Add(FFddirection);
  { X3D specification comment: (-Inf,Inf) }

  FFdintensity := TSFFloat.Create(Self, 'intensity', 1);
  Fields.Add(FFdintensity);
  { X3D specification comment: [0,1] }

  FFdlocation := TSFVec3f.Create(Self, 'location', Vector3Single(0, 0, 0));
  Fields.Add(FFdlocation);
  { X3D specification comment: (-Inf,Inf) }

  FFdmaxBack := TSFFloat.Create(Self, 'maxBack', 10);
  Fields.Add(FFdmaxBack);
  { X3D specification comment: [0,Inf) }

  FFdmaxFront := TSFFloat.Create(Self, 'maxFront', 10);
  Fields.Add(FFdmaxFront);
  { X3D specification comment: [0,Inf) }

  FFdminBack := TSFFloat.Create(Self, 'minBack', 1);
  Fields.Add(FFdminBack);
  { X3D specification comment: [0,Inf) }

  FFdminFront := TSFFloat.Create(Self, 'minFront', 1);
  Fields.Add(FFdminFront);
  { X3D specification comment: [0,Inf) }

  FFdpriority := TSFFloat.Create(Self, 'priority', 0);
  Fields.Add(FFdpriority);
  { X3D specification comment: [0,1] }

  FFdsource := TSFNode.Create(Self, 'source', INodeX3DSoundSourceNode);
  Fields.Add(FFdsource);

  FFdspatialize := TSFBool.Create(Self, 'spatialize', true);
  FFdspatialize.Exposed := false;
  Fields.Add(FFdspatialize);

  DefaultContainerField := 'children';
end;

class function TNodeSound.ClassNodeTypeName: string;
begin
  Result := 'Sound';
end;

function TNodeSound.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeSound.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterSoundNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeAudioClip,
    TNodeSound
  ]);
end;

{$endif read_implementation}
