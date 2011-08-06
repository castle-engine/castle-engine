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
    private FEventActivate: TVRMLEvent;
    public property EventActivate: TVRMLEvent read FEventActivate;

    { Event: SFBool, out } { }
    private FEventIsSelected: TVRMLEvent;
    public property EventIsSelected: TVRMLEvent read FEventIsSelected;

    { Event: SFBool, out } { }
    private FEventIsValid: TVRMLEvent;
    public property EventIsValid: TVRMLEvent read FEventIsValid;

    private FFdLanguage: TSFString;
    public property FdLanguage: TSFString read FFdLanguage;
  end;

  TNodeX3DVertexAttributeNode = class(TNodeX3DGeometricPropertyNode)
  public
    procedure CreateNode; override;

    private FFdName: TSFString;
    public property FdName: TSFString read FFdName;
  end;

  INodeX3DProgrammableShaderObject = interface
  ['{B027FD9B-2A68-4653-8A26-91DB51BE47ED}']
  end;

  TNodeComposedShader = class(TNodeX3DShaderNode, INodeX3DProgrammableShaderObject)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdParts: TMFNode;
    public property FdParts: TMFNode read FFdParts;

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

    private FFdValue: TMFFloat;
    public property FdValue: TMFFloat read FFdValue;

    private FFdNumComponents: TSFInt32;
    public property FdNumComponents: TSFInt32 read FFdNumComponents;
  end;

  TNodeMatrix3VertexAttribute = class(TNodeX3DVertexAttributeNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdValue: TMFMatrix3f;
    public property FdValue: TMFMatrix3f read FFdValue;
  end;

  TNodeMatrix4VertexAttribute = class(TNodeX3DVertexAttributeNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdValue: TMFMatrix4f;
    public property FdValue: TMFMatrix4f read FFdValue;
  end;

  TNodePackagedShader = class(TNodeX3DShaderNode, INodeX3DUrlObject, INodeX3DProgrammableShaderObject)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdUrl: TMFString;
    public property FdUrl: TMFString read FFdUrl;
  end;

  TNodeProgramShader = class(TNodeX3DShaderNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdPrograms: TMFNode;
    public property FdPrograms: TMFNode read FFdPrograms;
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

    private FFdUrl: TMFString;
    public property FdUrl: TMFString read FFdUrl;

    private FFdType: TSFStringShaderType;
    public property FdType: TSFStringShaderType read FFdType;

    { Load shader code from node's URLs.
      Will return '' if no valid URLs present.
      (Assuming that OnWarning will not raise
      an exception. If OnWarning will raise exception,
      then this will propagate this exception).

      Sets UsedFullUrl. }
    function LoadContents: string;

    { Once the LoadContents is called, this is set to the URL that was used to load
      the shader contents, or '' if no URL was used, or something
      special if shader contents came inlined inside URL.

      This is always a full, expanded (i.e. not relative) URL. }
    property UsedFullUrl: string read FUsedFullUrl;
  end;

  TNodeShaderProgram = class(TNodeX3DNode, INodeX3DUrlObject, INodeX3DProgrammableShaderObject)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdUrl: TMFString;
    public property FdUrl: TMFString read FFdUrl;

    private FFdType: TSFStringShaderType;
    public property FdType: TSFStringShaderType read FFdType;
  end;
{$endif read_interface}

{$ifdef read_implementation}
procedure TNodeX3DShaderNode.CreateNode;
begin
  inherited;

  FEventActivate := TVRMLEvent.Create(Self, 'activate', TSFBool, true);
  Events.Add(FEventActivate);

  FEventIsSelected := TVRMLEvent.Create(Self, 'isSelected', TSFBool, false);
  Events.Add(FEventIsSelected);

  FEventIsValid := TVRMLEvent.Create(Self, 'isValid', TSFBool, false);
  Events.Add(FEventIsValid);

  FFdLanguage := TSFString.Create(Self, 'language', '');
   FdLanguage.Exposed := false;
   FdLanguage.ChangesAlways := [chEverything];
  Fields.Add(FFdLanguage);
  { X3D specification comment: ["CG"|"GLSL"|"HLSL"|...] }

  DefaultContainerField := 'shaders';
end;

procedure TNodeX3DVertexAttributeNode.CreateNode;
begin
  inherited;

  FFdName := TSFString.Create(Self, 'name', '');
  FFdName.Exposed := false;
  Fields.Add(FFdName);

  DefaultContainerField := 'attrib';
end;

procedure TNodeComposedShader.CreateNode;
begin
  inherited;

  FFdParts := TMFNode.Create(Self, 'parts', [TNodeShaderPart]);
   FdParts.ChangesAlways := [chEverything];
  Fields.Add(FFdParts);

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
    if FdParts[I] is TNodeShaderPart then
      Result += ' ' + TNodeShaderPart(FdParts[I]).UsedFullUrl;
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

  FFdValue := TMFFloat.Create(Self, 'value', []);
  Fields.Add(FFdValue);
  { X3D specification comment: (-Inf,Inf) }

  FFdNumComponents := TSFInt32.Create(Self, 'numComponents', 4);
  FFdNumComponents.Exposed := false;
  Fields.Add(FFdNumComponents);
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

  FFdValue := TMFMatrix3f.Create(Self, 'value', []);
  Fields.Add(FFdValue);
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

  FFdValue := TMFMatrix4f.Create(Self, 'value', []);
  Fields.Add(FFdValue);
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

  FFdUrl := TMFString.Create(Self, 'url', []);
  Fields.Add(FFdUrl);
  { X3D specification comment: [URI] }

  HasInterfaceDeclarations := AllAccessTypes;
  CDataField := FdUrl;
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

  FFdPrograms := TMFNode.Create(Self, 'programs', [TNodeShaderProgram]);
  Fields.Add(FFdPrograms);
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
    OnWarning(wtMajor, 'VRML/X3D', Format('Shader "type" should be uppercase, but is not: "%s"',
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
      OnWarning(wtMajor, 'VRML/X3D', Format('Unknown "type" for shader: "%s"', [Value]));
      WarningInvalidValueDone := true;
    end;
  end;
end;

procedure TNodeShaderPart.CreateNode;
begin
  inherited;

  FFdUrl := TMFString.Create(Self, 'url', []);
   FdUrl.ChangesAlways := [chEverything]; { TODO - something more optimized }
  Fields.Add(FFdUrl);
  { X3D specification comment: [URI] }

  FFdType := TSFStringShaderType.Create(Self, 'type', 'VERTEX');
   FdType.Exposed := false;
   FdType.ChangesAlways := [chEverything];
  Fields.Add(FFdType);
  { X3D specification comment: ["VERTEX"|"FRAGMENT"] }

  DefaultContainerField := 'parts';
  CDataField := FdUrl;
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
    - makes OnWarning
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
        OnWarning(wtMajor, 'VRML/X3D', Format('Data mime-type "%s" not recognized as shader source, not loading. If this is a valid shader source mime type that should be supported, please report.', [Data.Mime]));
        Exit(false);
      end;

      try
        ShaderSource := ReadGrowingStreamToString(Data.Stream);
        UsedFullUrl := Data.URIPrefix;
        Result := true;
      except
        on E: Exception do
        begin
          { Remember that OnWarning *may* raise an exception. }
          OnWarning(wtMinor, 'VRML/X3D', Format(SDataURILoadError,
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
          { Remember that OnWarning *may* raise an exception. }
          OnWarning(wtMinor, 'VRML/X3D', Format(SLoadError,
            [E.ClassName, 'shader file', FullUrl, E.Message]));
      end;
    end;
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

  FFdUrl := TMFString.Create(Self, 'url', []);
  Fields.Add(FFdUrl);
  { X3D specification comment: [URI] }

  FFdType := TSFStringShaderType.Create(Self, 'type', 'VERTEX');
  FFdType.Exposed := false;
  Fields.Add(FFdType);
  { X3D specification comment: ["VERTEX"|"FRAGMENT"] }

  DefaultContainerField := 'programs';

  HasInterfaceDeclarations := AllAccessTypes;
  CDataField := FdUrl;
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
