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
  TNodeX3DNetworkSensorNode = class(TNodeX3DSensorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
  end;

  INodeX3DUrlObject = interface(IVRMLNode)
  ['{668420DB-20F7-4A5E-BB89-52F29CA39057}']
    property Fdurl: TMFString { read GetFdurl }; { }
  end;

  TNodeAnchor = class(TNodeX3DGroupingNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFddescription: TSFString;
    public property Fddescription: TSFString read FFddescription;

    private FFdparameter: TMFString;
    public property Fdparameter: TMFString read FFdparameter;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;
  end;

  TNodeInline = class(TVRMLGroupingNode, INodeX3DBoundedObject, INodeX3DUrlObject,
    IVRMLInlineNode)
  protected
    procedure BeforeTraverse(var State: TVRMLGraphTraverseState); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdload: TSFBool;
    public property Fdload: TSFBool read FFdload;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;

    private FFdbboxCenter: TSFVec3f;
    public property FdbboxCenter: TSFVec3f read FFdbboxCenter;

    private FFdbboxSize: TSFVec3f;
    public property FdbboxSize: TSFVec3f read FFdbboxSize;

    class function ChildrenSaveToStream: boolean; override;

    procedure LoadInlined(CanReload: boolean);
  end;

  TNodeLoadSensor = class(TNodeX3DNetworkSensorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdtimeOut: TSFTime;
    public property FdtimeOut: TSFTime read FFdtimeOut;

    private FFdwatchList: TMFNode;
    public property FdwatchList: TMFNode read FFdwatchList;

    { Event: SFBool, out } { }
    private FEventisLoaded: TVRMLEvent;
    public property EventisLoaded: TVRMLEvent read FEventisLoaded;

    { Event: SFTime, out } { }
    private FEventloadTime: TVRMLEvent;
    public property EventloadTime: TVRMLEvent read FEventloadTime;

    { Event: SFFloat, out } { }
    private FEventprogress: TVRMLEvent;
    public property Eventprogress: TVRMLEvent read FEventprogress;
  end;

{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeX3DNetworkSensorNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'children';
end;

constructor TNodeAnchor.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFddescription := TSFString.Create('description', '');
  Fields.Add(FFddescription);

  FFdparameter := TMFString.Create('parameter', []);
  Fields.Add(FFdparameter);

  FFdurl := TMFString.Create('url', []);
  Fields.Add(FFdurl);
  { X3D specification comment: [url or urn] }

  DefaultContainerField := 'children';
end;

class function TNodeAnchor.ClassNodeTypeName: string;
begin
  Result := 'Anchor';
end;

function TNodeAnchor.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeAnchor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeAnchor.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
var
  I: Integer;
begin
  for I := 0 to FdChildren.Count - 1 do
    Func(Self, FdChildren.Items[I]);
end;

constructor TNodeInline.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdload := TSFBool.Create('load', true);
  Fields.Add(FFdload);

  FFdurl := TMFString.Create('url', []);
  Fields.Add(FFdurl);
  { X3D specification comment: [url or urn] }

  FFdbboxCenter := TSFVec3f.Create('bboxCenter', Vector3Single(0, 0, 0));
  FFdbboxCenter.Exposed := false;
  Fields.Add(FFdbboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdbboxSize := TSFVec3f.Create('bboxSize', Vector3Single(-1, -1, -1));
  FFdbboxSize.Exposed := false;
  Fields.Add(FFdbboxSize);
  { X3D specification comment: [0,Inf) or -1 -1 -1 }

  DefaultContainerField := 'children';

  { To allow inlined children (they are loaded as VRML 1.0 style children) }
  FParsingAllowedChildren := false;
  FAllowedChildren := true;
end;

class function TNodeInline.ClassNodeTypeName: string;
begin
  Result := 'Inline';
end;

function TNodeInline.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeInline.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeInline.LoadInlined(CanReload: boolean);
var
  I: Integer;
  FullUrl: string;
  NewNode: TVRMLNode;
begin
  if ChildrenCount > 0 then
  begin
    if CanReload then RemoveAllChildren else Exit;
  end;

  NewNode := nil;

  for I := 0 to FdUrl.Items.Count - 1 do
  begin
    FullUrl := PathFromWWWBasePath(FdUrl.Items[I]);
    try
      NewNode := LoadAsVRML(PathFromWWWBasePath(FullUrl), false);
      Break;
    except
      on E: Exception do
        { pamietajmy ze VRMLNonFatalError moze spowodowac rzucenie wyjatku
          (chociaz nie musi) }
        VRMLNonFatalError('Exception ' + E.ClassName +
          ' occured when trying to load '+
          'inline file from URL "' + FullUrl + ' : ' + E.Message);
    end;
  end;

  if NewNode <> nil then
    AddChild(NewNode);
end;

procedure TNodeInline.BeforeTraverse(var State: TVRMLGraphTraverseState);
begin
  inherited;
  LoadInlined(false);
end;

class function TNodeInline.ChildrenSaveToStream: boolean;
begin
  Result := false;
end;

constructor TNodeLoadSensor.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdtimeOut := TSFTime.Create('timeOut', 0);
  Fields.Add(FFdtimeOut);

  FFdwatchList := TMFNode.Create(Self, 'watchList', INodeX3DUrlObject);
  Fields.Add(FFdwatchList);

  FEventisLoaded := TVRMLEvent.Create('isLoaded', TSFBool, false);
  Events.Add(FEventisLoaded);

  FEventloadTime := TVRMLEvent.Create('loadTime', TSFTime, false);
  Events.Add(FEventloadTime);

  FEventprogress := TVRMLEvent.Create('progress', TSFFloat, false);
  Events.Add(FEventprogress);

  DefaultContainerField := 'children';
end;

class function TNodeLoadSensor.ClassNodeTypeName: string;
begin
  Result := 'LoadSensor';
end;

function TNodeLoadSensor.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeLoadSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterNetworkingNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeAnchor,
    TNodeInline,
    TNodeLoadSensor
  ]);
end;

{$endif read_implementation}
