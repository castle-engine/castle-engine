{
  Copyright 2007-2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{$ifdef read_interface}
  { }
  TNodeX3DShaderNode = class(TNodeX3DAppearanceChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    { Event: SFBool, in } { }
    private FEventactivate: TVRMLEvent;
    public property Eventactivate: TVRMLEvent read FEventactivate;

    { Event: SFBool, out } { }
    private FEventisSelected: TVRMLEvent;
    public property EventisSelected: TVRMLEvent read FEventisSelected;

    { Event: SFBool, out } { }
    private FEventisValid: TVRMLEvent;
    public property EventisValid: TVRMLEvent read FEventisValid;

    { Just a shortcut for EventIsSelected.Send,
      using current ParentEventsProcessor. }
    procedure EventIsSelectedSend(const Value: boolean);

    { Just a shortcut for EventIsValid.Send,
      using current ParentEventsProcessor. }
    procedure EventIsValidSend(const Value: boolean);

    private FFdlanguage: TSFString;
    public property Fdlanguage: TSFString read FFdlanguage;
  end;

  TNodeX3DVertexAttributeNode = class(TNodeX3DGeometricPropertyNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFdname: TSFString;
    public property Fdname: TSFString read FFdname;
  end;

  INodeX3DProgrammableShaderObject = interface
  ['{B027FD9B-2A68-4653-8A26-91DB51BE47ED}']
  end;

  TNodeComposedShader = class(TNodeX3DShaderNode, INodeX3DProgrammableShaderObject)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdparts: TMFNode;
    public property Fdparts: TMFNode read FFdparts;

    { Returns string describing all used URLs in our ShaderPart nodes.
      More precisely, all UsedFullUrl in ShaderPart nodes inside out parts field.
      This provides some description of this node's contents after ShaderPart nodes
      are loaded, so is useful for debug purposes. }
    function DescribeUsedUrls: string;
  end;

  TNodeFloatVertexAttribute = class(TNodeX3DVertexAttributeNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdvalue: TMFFloat;
    public property Fdvalue: TMFFloat read FFdvalue;

    private FFdnumComponents: TSFInt32;
    public property FdnumComponents: TSFInt32 read FFdnumComponents;
  end;

  TNodeMatrix3VertexAttribute = class(TNodeX3DVertexAttributeNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdvalue: TMFMatrix3f;
    public property Fdvalue: TMFMatrix3f read FFdvalue;
  end;

  TNodeMatrix4VertexAttribute = class(TNodeX3DVertexAttributeNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdvalue: TMFMatrix4f;
    public property Fdvalue: TMFMatrix4f read FFdvalue;
  end;

  TNodePackagedShader = class(TNodeX3DShaderNode, INodeX3DUrlObject, INodeX3DProgrammableShaderObject)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;
  end;

  TNodeProgramShader = class(TNodeX3DShaderNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdprograms: TMFNode;
    public property Fdprograms: TMFNode read FFdprograms;
  end;

  TNodeShaderPart = class(TNodeX3DNode, INodeX3DUrlObject)
  private
    FUsedFullUrl: string;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;

    private FFdtype: TSFString;
    public property Fdtype: TSFString read FFdtype;

    { Load shader code from node's URLs (or CDATA).

      Will return '' if no valid URLs and no CDATA present.
      (assuming that VRMLWarning will not raise
      an exception; if VRMLWarning will raise exception,
      then this will propagate this exception).

      Sets UsedFullUrl. }
    function LoadContents: string;

    { Once the LoadContents is called, this is set to the URL that was used to load
      the shader contents, or '' if no URL was used, or something
      special if shader contents came from CDATA (X3D XML encoding)
      or were inlined inside URL.

      This is always a full, expanded (i.e. not relative) URL. }
    property UsedFullUrl: string read FUsedFullUrl;
  end;

  TNodeShaderProgram = class(TNodeX3DNode, INodeX3DUrlObject, INodeX3DProgrammableShaderObject)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;

    private FFdtype: TSFString;
    public property Fdtype: TSFString read FFdtype;
  end;
{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeX3DShaderNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventactivate := TVRMLEvent.Create(Self, 'activate', TSFBool, true);
  Events.Add(FEventactivate);

  FEventisSelected := TVRMLEvent.Create(Self, 'isSelected', TSFBool, false);
  Events.Add(FEventisSelected);

  FEventisValid := TVRMLEvent.Create(Self, 'isValid', TSFBool, false);
  Events.Add(FEventisValid);

  FFdlanguage := TSFString.Create(Self, 'language', '');
  FFdlanguage.Exposed := false;
  Fields.Add(FFdlanguage);
  { X3D specification comment: ["CG"|"GLSL"|"HLSL"|...] }

  DefaultContainerField := 'shaders';
end;

procedure TNodeX3DShaderNode.EventIsSelectedSend(const Value: boolean);
begin
  if ParentEventsProcessor <> nil then
    EventIsSelected.Send(Value,
      (ParentEventsProcessor as TVRMLScene).WorldTime);
end;

procedure TNodeX3DShaderNode.EventIsValidSend(const Value: boolean);
begin
  if ParentEventsProcessor <> nil then
    EventIsValid.Send(Value,
      (ParentEventsProcessor as TVRMLScene).WorldTime);
end;

constructor TNodeX3DVertexAttributeNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdname := TSFString.Create(Self, 'name', '');
  FFdname.Exposed := false;
  Fields.Add(FFdname);

  DefaultContainerField := 'attrib';
end;

constructor TNodeComposedShader.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdparts := TMFNode.Create(Self, 'parts', [TNodeShaderPart]);
  Fields.Add(FFdparts);

  HasInterfaceDeclarations := AllAccessTypes;
end;

class function TNodeComposedShader.ClassNodeTypeName: string;
begin
  Result := 'ComposedShader';
end;

function TNodeComposedShader.DescribeUsedUrls: string;
var
  I: Integer;
begin
  Result := '(urls:';
  for I := 0 to FdParts.Count - 1 do
    if FdParts.Items[I] is TNodeShaderPart then
      Result += ' ' + TNodeShaderPart(FdParts.Items[I]).UsedFullUrl;
  Result += ')';
end;

function TNodeComposedShader.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeComposedShader.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeFloatVertexAttribute.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdvalue := TMFFloat.Create(Self, 'value', []);
  Fields.Add(FFdvalue);
  { X3D specification comment: (-Inf,Inf) }

  FFdnumComponents := TSFInt32.Create(Self, 'numComponents', 4);
  FFdnumComponents.Exposed := false;
  Fields.Add(FFdnumComponents);
  { X3D specification comment: [1..4] }
end;

class function TNodeFloatVertexAttribute.ClassNodeTypeName: string;
begin
  Result := 'FloatVertexAttribute';
end;

function TNodeFloatVertexAttribute.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeFloatVertexAttribute.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeMatrix3VertexAttribute.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdvalue := TMFMatrix3f.Create(Self, 'value', []);
  Fields.Add(FFdvalue);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TNodeMatrix3VertexAttribute.ClassNodeTypeName: string;
begin
  Result := 'Matrix3VertexAttribute';
end;

function TNodeMatrix3VertexAttribute.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeMatrix3VertexAttribute.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeMatrix4VertexAttribute.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdvalue := TMFMatrix4f.Create(Self, 'value', []);
  Fields.Add(FFdvalue);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TNodeMatrix4VertexAttribute.ClassNodeTypeName: string;
begin
  Result := 'Matrix4VertexAttribute';
end;

function TNodeMatrix4VertexAttribute.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeMatrix4VertexAttribute.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodePackagedShader.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdurl := TMFString.Create(Self, 'url', []);
  Fields.Add(FFdurl);
  { X3D specification comment: [URI] }

  HasInterfaceDeclarations := AllAccessTypes;
  CDataAllowed := true;
end;

class function TNodePackagedShader.ClassNodeTypeName: string;
begin
  Result := 'PackagedShader';
end;

function TNodePackagedShader.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodePackagedShader.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeProgramShader.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdprograms := TMFNode.Create(Self, 'programs', [TNodeShaderProgram]);
  Fields.Add(FFdprograms);
end;

class function TNodeProgramShader.ClassNodeTypeName: string;
begin
  Result := 'ProgramShader';
end;

function TNodeProgramShader.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeProgramShader.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeShaderPart.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdurl := TMFString.Create(Self, 'url', []);
  Fields.Add(FFdurl);
  { X3D specification comment: [URI] }

  FFdtype := TSFString.Create(Self, 'type', 'VERTEX');
  FFdtype.Exposed := false;
  Fields.Add(FFdtype);
  { X3D specification comment: ["VERTEX"|"FRAGMENT"] }

  DefaultContainerField := 'parts';

  CDataAllowed := true;
end;

class function TNodeShaderPart.ClassNodeTypeName: string;
begin
  Result := 'ShaderPart';
end;

function TNodeShaderPart.LoadContents: string;

  function IsDirectShaderSource(const S: string): boolean;
  { URL is recognized as direct shader source if it
    doesn't start with any protocol *and*
    has some newlines (needed to recognize relative URLs without
    protocol correctly). }
  var
    Protocol: string;
  begin
    Result := CharsPos([#10, #13], S) > 0;
    if Result then
    begin
      Protocol := UrlProtocol(S);
      Result := (Protocol = '') or (CharsPos(WhiteSpaces, S) > 0);
    end;
  end;

var
  I: Integer;
  FullUrl: string;
begin
  FUsedFullUrl := '';
  Result := '';

  for I := 0 to FdUrl.Items.Count - 1 do
  begin
    if IsDirectShaderSource(FdUrl.Items[I]) then
    begin
      Result := FdUrl.Items[I];
      FUsedFullUrl := 'SHADER-SOURCE-INLINED';
      Exit;
    end else
    begin
      FullUrl := PathFromWWWBasePath(FdUrl.Items[I]);
      try
        Result := FileToString(FullUrl);
        FUsedFullUrl := FullUrl;
        Exit;
      except
        on E: Exception do
          { pamietajmy ze VRMLWarning moze spowodowac rzucenie wyjatku
            (chociaz nie musi) }
          VRMLWarning(vwIgnorable, 'Exception ' + E.ClassName +
            ' occured when trying to load '+
            'shader file from URL "' + FullUrl + ' : ' + E.Message);
      end;
    end;
  end;

  if CDataExists then
  begin
    Result := CData;
    FUsedFullUrl := '#CDATA';
  end;
end;

function TNodeShaderPart.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeShaderPart.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeShaderProgram.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdurl := TMFString.Create(Self, 'url', []);
  Fields.Add(FFdurl);
  { X3D specification comment: [URI] }

  FFdtype := TSFString.Create(Self, 'type', 'VERTEX');
  FFdtype.Exposed := false;
  Fields.Add(FFdtype);
  { X3D specification comment: ["VERTEX"|"FRAGMENT"] }

  DefaultContainerField := 'programs';

  HasInterfaceDeclarations := AllAccessTypes;
  CDataAllowed := true;
end;

class function TNodeShaderProgram.ClassNodeTypeName: string;
begin
  Result := 'ShaderProgram';
end;

function TNodeShaderProgram.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeShaderProgram.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterShadersNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeComposedShader,
    TNodeFloatVertexAttribute,
    TNodeMatrix3VertexAttribute,
    TNodeMatrix4VertexAttribute,
    TNodePackagedShader,
    TNodeProgramShader,
    TNodeShaderPart,
    TNodeShaderProgram
  ]);
end;
{$endif read_implementation}
