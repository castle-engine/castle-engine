{
  Copyright 2007-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { }
  TNodeX3DShaderNode = class(TNodeX3DAppearanceChildNode)
  public
    procedure CreateNode; override;

    { Event: SFBool, in } { }
    private FEventactivate: TVRMLEvent;
    public property Eventactivate: TVRMLEvent read FEventactivate;

    { Event: SFBool, out } { }
    private FEventisSelected: TVRMLEvent;
    public property EventisSelected: TVRMLEvent read FEventisSelected;

    { Event: SFBool, out } { }
    private FEventisValid: TVRMLEvent;
    public property EventisValid: TVRMLEvent read FEventisValid;

    private FFdlanguage: TSFString;
    public property Fdlanguage: TSFString read FFdlanguage;
  end;

  TNodeX3DVertexAttributeNode = class(TNodeX3DGeometricPropertyNode)
  public
    procedure CreateNode; override;

    private FFdname: TSFString;
    public property Fdname: TSFString read FFdname;
  end;

  INodeX3DProgrammableShaderObject = interface
  ['{B027FD9B-2A68-4653-8A26-91DB51BE47ED}']
  end;

  TNodeComposedShader = class(TNodeX3DShaderNode, INodeX3DProgrammableShaderObject)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
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
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdvalue: TMFFloat;
    public property Fdvalue: TMFFloat read FFdvalue;

    private FFdnumComponents: TSFInt32;
    public property FdnumComponents: TSFInt32 read FFdnumComponents;
  end;

  TNodeMatrix3VertexAttribute = class(TNodeX3DVertexAttributeNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdvalue: TMFMatrix3f;
    public property Fdvalue: TMFMatrix3f read FFdvalue;
  end;

  TNodeMatrix4VertexAttribute = class(TNodeX3DVertexAttributeNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdvalue: TMFMatrix4f;
    public property Fdvalue: TMFMatrix4f read FFdvalue;
  end;

  TNodePackagedShader = class(TNodeX3DShaderNode, INodeX3DUrlObject, INodeX3DProgrammableShaderObject)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;
  end;

  TNodeProgramShader = class(TNodeX3DShaderNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdprograms: TMFNode;
    public property Fdprograms: TMFNode read FFdprograms;
  end;

  TShaderType = (stVertex, stFragment);

  TSFStringShaderType = class(TSFString)
  private
    UpperCaseChecked, WarningInvalidValueDone: boolean;
  protected
    function ExposedEventsFieldClass: TVRMLFieldClass; override;
  public
    function GetValue(out AValue: TShaderType): boolean;
  end;

  TNodeShaderPart = class(TNodeX3DNode, INodeX3DUrlObject)
  private
    FUsedFullUrl: string;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;

    private FFdtype: TSFStringShaderType;
    public property Fdtype: TSFStringShaderType read FFdtype;

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
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;

    private FFdtype: TSFStringShaderType;
    public property Fdtype: TSFStringShaderType read FFdtype;
  end;
{$endif read_interface}

{$ifdef read_implementation}
procedure TNodeX3DShaderNode.CreateNode;
begin
  inherited;

  FEventactivate := TVRMLEvent.Create(Self, 'activate', TSFBool, true);
  Events.Add(FEventactivate);

  FEventisSelected := TVRMLEvent.Create(Self, 'isSelected', TSFBool, false);
  Events.Add(FEventisSelected);

  FEventisValid := TVRMLEvent.Create(Self, 'isValid', TSFBool, false);
  Events.Add(FEventisValid);

  FFdlanguage := TSFString.Create(Self, 'language', '');
   Fdlanguage.Exposed := false;
   Fdlanguage.ChangesAlways := [chEverything];
  Fields.Add(FFdlanguage);
  { X3D specification comment: ["CG"|"GLSL"|"HLSL"|...] }

  DefaultContainerField := 'shaders';
end;

procedure TNodeX3DVertexAttributeNode.CreateNode;
begin
  inherited;

  FFdname := TSFString.Create(Self, 'name', '');
  FFdname.Exposed := false;
  Fields.Add(FFdname);

  DefaultContainerField := 'attrib';
end;

procedure TNodeComposedShader.CreateNode;
begin
  inherited;

  FFdparts := TMFNode.Create(Self, 'parts', [TNodeShaderPart]);
   Fdparts.ChangesAlways := [chEverything];
  Fields.Add(FFdparts);

  { Note that additional ComposedShader fields should have
      ChangesAlways := [chVisibleGeometry, chVisibleNonGeometry]
    because you want to redisplay when they change.

    But TVRMLGLRenderer takes care of it already. When shaders'
    uniform variables change, we call Scene.VisibleChangeHere.
    Implemented inside TVRMLShaderProgram.EventReceive.

    TODO: but when you don't use events, but directly change them
    and call ChangedField, we should do something here? }

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

class function TNodeComposedShader.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeFloatVertexAttribute.CreateNode;
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

class function TNodeFloatVertexAttribute.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeMatrix3VertexAttribute.CreateNode;
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

class function TNodeMatrix3VertexAttribute.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeMatrix4VertexAttribute.CreateNode;
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

class function TNodeMatrix4VertexAttribute.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodePackagedShader.CreateNode;
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

class function TNodePackagedShader.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeProgramShader.CreateNode;
begin
  inherited;

  FFdprograms := TMFNode.Create(Self, 'programs', [TNodeShaderProgram]);
  Fields.Add(FFdprograms);
end;

class function TNodeProgramShader.ClassNodeTypeName: string;
begin
  Result := 'ProgramShader';
end;

class function TNodeProgramShader.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TSFStringShaderType.ExposedEventsFieldClass: TVRMLFieldClass;
begin
  Result := TSFString;
end;

function TSFStringShaderType.GetValue(out AValue: TShaderType): boolean;
var
  UpperValue: string;
begin
  UpperValue := UpperCase(Value);

  if (not UpperCaseChecked) and (UpperValue <> Value) then
  begin
    VRMLWarning(vwSerious, Format('Shader "type" should be uppercase, but is not: "%s"',
      [Value]));
    UpperCaseChecked := true;
  end;

  if UpperValue = 'VERTEX' then
  begin
    AValue := stVertex;
    Result := true;
  end else
  if UpperValue = 'FRAGMENT' then
  begin
    AValue := stFragment;
    Result := true;
  end else
  begin
    Result := false;
    if not WarningInvalidValueDone then
    begin
      VRMLWarning(vwSerious, Format('Unknown "type" for shader: "%s"', [Value]));
      WarningInvalidValueDone := true;
    end;
  end;
end;

procedure TNodeShaderPart.CreateNode;
begin
  inherited;

  FFdurl := TMFString.Create(Self, 'url', []);
   Fdurl.ChangesAlways := [chEverything]; { TODO - something more optimized }
  Fields.Add(FFdurl);
  { X3D specification comment: [URI] }

  FFdtype := TSFStringShaderType.Create(Self, 'type', 'VERTEX');
   Fdtype.Exposed := false;
   Fdtype.ChangesAlways := [chEverything];
  Fields.Add(FFdtype);
  { X3D specification comment: ["VERTEX"|"FRAGMENT"] }

  DefaultContainerField := 'parts';

  CDataAllowed := true;
end;

class function TNodeShaderPart.ClassNodeTypeName: string;
begin
  Result := 'ShaderPart';
end;

function ShaderLoadContents(var UsedFullUrl: string;
  const Node: TVRMLNode; const FdUrl: TMFString): string;

  function IsDirectShaderSource(const S: string): boolean;
  { URL is recognized as direct shader source if it
    doesn't start with any protocol *and*
    has some newlines (needed to recognize relative URLs without
    protocol correctly). }
  begin
    Result := (CharsPos([#10, #13], S) > 0) and (UrlProtocol(S) = '');
  end;

  { Load shader source from data URI.If success,
    - sets FUsedFullUrl, ShaderSource
    - returns true
    otherwise
    - makes VRMLWarning
    - returns false }
  function LoadFromDataProtocol(const URL: string; out ShaderSource: string): boolean;
  var
    Data: TDataURI;
  begin
    Data := TDataURI.Create;
    try
      Data.URI := URL;

      if not Data.Valid then Exit(false);

      if Data.Mime <> 'text/plain' then
      begin
        VRMLWarning(vwSerious, Format('Data mime-type "%s" not recognized as shader source, not loading. If this is a valid shader source mime type that should be supported, please report.', [Data.Mime]));
        Exit(false);
      end;

      try
        ShaderSource := ReadGrowingStreamToString(Data.Stream);
        UsedFullUrl := Data.URIPrefix;
        Result := true;
      except
        on E: Exception do
        begin
          { Remember that VRMLWarning *may* raise an exception. }
          VRMLWarning(vwIgnorable, Format(SDataURILoadError,
            [E.ClassName, 'shader source', Data.URIPrefix, E.Message]));
          Result := false;
        end;
      end;
    finally FreeAndNil(Data) end;
  end;

var
  I: Integer;
  FullUrl: string;
begin
  UsedFullUrl := '';
  Result := '';

  for I := 0 to FdUrl.Items.Count - 1 do
  begin
    if IsDirectShaderSource(FdUrl.Items[I]) then
    begin
      Result := FdUrl.Items[I];
      UsedFullUrl := 'SHADER-SOURCE-INLINED';
      Exit;
    end else
    if TDataURI.IsDataURI(FdUrl.Items[I]) then
    begin
      if LoadFromDataProtocol(FdUrl.Items[I], Result) then Break;
    end else
    begin
      FullUrl := Node.PathFromWWWBasePath(FdUrl.Items[I]);
      try
        Result := FileToString(FullUrl);
        UsedFullUrl := FullUrl;
        Exit;
      except
        on E: Exception do
          { Remember that VRMLWarning *may* raise an exception. }
          VRMLWarning(vwIgnorable, Format(SLoadError,
            [E.ClassName, 'shader file', FullUrl, E.Message]));
      end;
    end;
  end;

  if Node.CDataExists then
  begin
    Result := Node.CData;
    UsedFullUrl := '#CDATA';
  end;
end;

function TNodeShaderPart.LoadContents: string;
begin
  Result := ShaderLoadContents(FUsedFullUrl, Self, FdUrl);
end;

class function TNodeShaderPart.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeShaderProgram.CreateNode;
begin
  inherited;

  FFdurl := TMFString.Create(Self, 'url', []);
  Fields.Add(FFdurl);
  { X3D specification comment: [URI] }

  FFdtype := TSFStringShaderType.Create(Self, 'type', 'VERTEX');
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
