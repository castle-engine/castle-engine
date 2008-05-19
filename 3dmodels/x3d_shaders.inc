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
  TNodeX3DAppearanceChildNode = class(TNodeX3DNode)
  public
  end;

  INodeX3DUrlObject = interface
  end;

  TNodeX3DShaderNode = class(TNodeX3DAppearanceChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    { property Eventactivate: TSFBool index ? read GetFieldAsSFBool; } { }

    { property EventisSelected: TSFBool index ? read GetFieldAsSFBool; } { }

    { property EventisValid: TSFBool index ? read GetFieldAsSFBool; } { }

    private FFdlanguage: TSFString;
    public property Fdlanguage: TSFString read FFdlanguage;

  end;

  INodeX3DProgrammableShaderObject = interface
  end;

  TNodeComposedShader = class(TNodeX3DShaderNode, INodeX3DProgrammableShaderObject)
  private
    FInterfaceDeclarations: TVRMLInterfaceDeclarationsList;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    destructor Destroy; override;

    class function ClassNodeTypeName: string; override;

    private FFdparts: TMFNode;
    public property Fdparts: TMFNode read FFdparts;

    { TODO:
    # And any number of:
    fieldType []       fieldName
    fieldType [in]     fieldName
    fieldType [out]    fieldName
    fieldType [in,out] fieldName

    These are sort of handled... They are parsed pretty much just like
    similar construct in Script node. I openly admit that I don't precisely
    grok the X3D specification at this point. I understood VRML 97 specification
    about Script node, and as far as I understood it seems that
    X3D Script and ComposedShader just expect the same syntax.

    But it needs testing. Does anyone have some reliable examples of
    X3D files with shaders, that follow X3D specification?
    I didn't found much by google (although from
    what I've seen it seems that my implementation is correct, will
    get more testing when X3D XML flavor is done).
    }

    { Returns string describing all used URLs in our ShaderPart nodes.
      More precisely, all UsedFullUrl in ShaderPart nodes inside out parts field.
      This provides some description of this node's contents after ShaderPart nodes
      are loaded, so is useful for debug purposes. }
    function DescribeUsedUrls: string;

    property InterfaceDeclarations: TVRMLInterfaceDeclarationsList
      read FInterfaceDeclarations;

    function ParseNodeBodyElement(Lexer: TVRMLLexer): boolean; override;
    procedure SaveContentsToStream(SaveProperties: TVRMLSaveToStreamProperties);
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  TNodePackagedShader = class(TNodeX3DShaderNode,
    INodeX3DUrlObject, INodeX3DProgrammableShaderObject)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;

    { TODO:
    # And any number of:
    fieldType [in]     fieldName
    fieldType [in,out] fieldName initialValue
    fieldType [out]    fieldName
    fieldType []       fieldName initialValue
    }

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  TNodeProgramShader = class(TNodeX3DShaderNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdprograms: TMFNode;
    public property Fdprograms: TMFNode read FFdprograms;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  TNodeShaderPart = class(TNodeX3DNode, INodeX3DUrlObject)
  private
    FUsedFullUrl: string;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;

    private FFdtype: TSFString;
    public property Fdtype: TSFString read FFdtype;

    { Load shader code from node's URL field.

      Will return '' if no valid URLs (assuming that VRMLNonFatalError will not raise
      an exception; if VRMLNonFatalError will raise exception, then this will propagate
      this exception).

      Sets UsedFullUrl. }
    function LoadContents: string;

    { Once the LoadContents is called, this is set to the URL that was used to load
      the shader contents, or '' if no URL was used.

      This is always a full, expanded (i.e. not relative) URL. }
    property UsedFullUrl: string read FUsedFullUrl;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  TNodeShaderProgram = class(TNodeX3DNode, INodeX3DUrlObject,
    INodeX3DProgrammableShaderObject)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;

    private FFdtype: TSFString;
    public property Fdtype: TSFString read FFdtype;

    { TODO:
    # And any number of:
    fieldType [in]     fieldName
    fieldType [in,out] fieldName initialValue
    fieldType [out]    fieldName
    fieldType []       fieldName initialValue
    }

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

constructor TNodeX3DShaderNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  Events.Add(TVRMLEvent.Create('activate', TSFBool, true));

  Events.Add(TVRMLEvent.Create('isSelected', TSFBool, false));

  Events.Add(TVRMLEvent.Create('isValid', TSFBool, false));

  FFdlanguage := TSFString.Create('language', '');
  Fields.Add(FFdlanguage);
end;

constructor TNodeComposedShader.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdparts := TMFNode.Create(Self, 'parts', [TNodeShaderPart]);
  Fields.Add(FFdparts);

  DefaultContainerField := 'shaders';

  FInterfaceDeclarations := TVRMLInterfaceDeclarationsList.Create;
end;

destructor TNodeComposedShader.Destroy;
begin
  FreeWithContentsAndNil(FInterfaceDeclarations);
  inherited;
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

function TNodeComposedShader.ParseNodeBodyElement(Lexer: TVRMLLexer): boolean;
var
  I: TVRMLInterfaceDeclaration;
begin
  Result := inherited;

  if not Result then
  begin
    Result := Lexer.TokenIsKeyword(InterfaceDeclarationKeywords);
    if Result then
    begin
      I := TVRMLInterfaceDeclaration.Create;
      InterfaceDeclarations.Add(I);
      I.Parse(Lexer, true, true);
    end;
  end;
end;

procedure TNodeComposedShader.SaveContentsToStream(
  SaveProperties: TVRMLSaveToStreamProperties);
var
  I: Integer;
begin
  for I := 0 to InterfaceDeclarations.Count - 1 do
    InterfaceDeclarations[I].SaveToStream(SaveProperties, true);

  inherited;
end;

constructor TNodePackagedShader.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdurl := TMFString.Create('url', []);
  Fields.Add(FFdurl);

  DefaultContainerField := 'shaders';
end;

class function TNodePackagedShader.ClassNodeTypeName: string;
begin
  Result := 'PackagedShader';
end;

constructor TNodeProgramShader.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdprograms := TMFNode.Create(Self, 'programs', [TNodeShaderProgram]);
  Fields.Add(FFdprograms);

  DefaultContainerField := 'shaders';
end;

class function TNodeProgramShader.ClassNodeTypeName: string;
begin
  Result := 'ProgramShader';
end;

constructor TNodeShaderPart.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdurl := TMFString.Create('url', []);
  Fields.Add(FFdurl);

  FFdtype := TSFString.Create('type', 'VERTEX');
  FFdtype.Exposed := false;
  Fields.Add(FFdtype);

  DefaultContainerField := 'parts';
end;

class function TNodeShaderPart.ClassNodeTypeName: string;
begin
  Result := 'ShaderPart';
end;

function TNodeShaderPart.LoadContents: string;
var
  I: Integer;
  FullUrl: string;
begin
  FUsedFullUrl := '';
  Result := '';

  for I := 0 to FdUrl.Items.Count - 1 do
  begin
    FullUrl := PathFromWWWBasePath(FdUrl.Items[I]);
    try
      Result := FileToString(PathFromWWWBasePath(FullUrl));
      FUsedFullUrl := FullUrl;
      Break;
    except
      on E: Exception do
        { pamietajmy ze VRMLNonFatalError moze spowodowac rzucenie wyjatku
          (chociaz nie musi) }
        VRMLNonFatalError('Exception ' + E.ClassName +
          ' occured when trying to load '+
          'shader file from URL "' + FullUrl + ' : ' + E.Message);
    end;
  end;
end;

constructor TNodeShaderProgram.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdurl := TMFString.Create('url', []);
  Fields.Add(FFdurl);

  FFdtype := TSFString.Create('type', 'VERTEX');
  FFdtype.Exposed := false;
  Fields.Add(FFdtype);

  DefaultContainerField := 'programs';
end;

class function TNodeShaderProgram.ClassNodeTypeName: string;
begin
  Result := 'ShaderProgram';
end;

{$endif read_implementation}
