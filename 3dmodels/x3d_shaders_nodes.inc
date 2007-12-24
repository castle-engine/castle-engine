{
  Copyright 2007 Michalis Kamburelis.

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
  TNodeX3DAppearanceChildNode = class(TVRMLNode)
  public
  end;

  TNodeX3DMetadataObject = class(TVRMLNode)
  public
  end;

  INodeX3DUrlObject = interface
  end;

  TNodeX3DShaderNode = class(TNodeX3DAppearanceChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    { property Eventactivate: TSFBool index 0 read GetFieldAsSFBool; } { }
    property Fdmetadata: TSFNode index 0 read GetFieldAsSFNode;
    { property EventisSelected: TSFBool index 2 read GetFieldAsSFBool; } { }
    { property EventisValid: TSFBool index 3 read GetFieldAsSFBool; } { }
    property Fdlanguage: TSFString index 1 read GetFieldAsSFString;
  end;

  INodeX3DProgrammableShaderObject = interface
  end;

  TNodeComposedShader = class(TNodeX3DShaderNode, INodeX3DProgrammableShaderObject)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    property Fdparts: TMFNode index 2 read GetFieldAsMFNode;

    { TODO:
    # And any number of:
    fieldType []       fieldName
    fieldType [in]     fieldName
    fieldType [out]    fieldName
    fieldType [in,out] fieldName
    }

    { Returns string describing all used URLs in our ShaderPart nodes.
      More precisely, all UsedFullUrl in ShaderPart nodes inside out parts field.
      This provides some description of this node's contents after ShaderPart nodes
      are loaded, so is useful for debug purposes. }
    function DescribeUsedUrls: string;
  end;

  TNodePackagedShader = class(TNodeX3DShaderNode,
    INodeX3DUrlObject, INodeX3DProgrammableShaderObject)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    property Fdurl: TMFString index 2 read GetFieldAsMFString;

    { TODO:
    # And any number of:
    fieldType [in]     fieldName
    fieldType [in,out] fieldName initialValue
    fieldType [out]    fieldName
    fieldType []       fieldName initialValue
    }
  end;

  TNodeProgramShader = class(TNodeX3DShaderNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    property Fdprograms: TMFNode index 2 read GetFieldAsMFNode;
  end;

  TNodeShaderPart = class(TVRMLNode, INodeX3DUrlObject)
  private
    FUsedFullUrl: string;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    property Fdmetadata: TSFNode index 0 read GetFieldAsSFNode;
    property Fdurl: TMFString index 1 read GetFieldAsMFString;
    property Fdtype: TSFString index 2 read GetFieldAsSFString;

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
  end;

  TNodeShaderProgram = class(TVRMLNode, INodeX3DUrlObject,
    INodeX3DProgrammableShaderObject)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    property Fdmetadata: TSFNode index 0 read GetFieldAsSFNode;
    property Fdurl: TMFString index 1 read GetFieldAsMFString;
    property Fdtype: TSFString index 2 read GetFieldAsSFString;

    { TODO:
    # And any number of:
    fieldType [in]     fieldName
    fieldType [in,out] fieldName initialValue
    fieldType [out]    fieldName
    fieldType []       fieldName initialValue
    }
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

  Fields.Add(TSFNode.Create(Self, 'metadata', [TNodeX3DMetadataObject]));
  Fields.Last.Exposed := true;

  Fields.Add(TSFString.Create('language', ''));
end;

constructor TNodeComposedShader.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TMFNode.Create(Self, 'parts', [TNodeShaderPart]));
  Fields.Last.Exposed := true;
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

constructor TNodePackagedShader.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TMFString.Create('url', []));
  Fields.Last.Exposed := true;
end;

class function TNodePackagedShader.ClassNodeTypeName: string;
begin
  Result := 'PackagedShader';
end;

constructor TNodeProgramShader.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TMFNode.Create(Self, 'programs', [TNodeShaderProgram]));
  Fields.Last.Exposed := true;
end;

class function TNodeProgramShader.ClassNodeTypeName: string;
begin
  Result := 'ProgramShader';
end;

constructor TNodeShaderPart.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TSFNode.Create(Self, 'metadata', [TNodeX3DMetadataObject]));
  Fields.Last.Exposed := true;

  Fields.Add(TMFString.Create('url', []));
  Fields.Last.Exposed := true;

  Fields.Add(TSFString.Create('type', 'VERTEX'));
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

  Fields.Add(TSFNode.Create(Self, 'metadata', [TNodeX3DMetadataObject]));
  Fields.Last.Exposed := true;

  Fields.Add(TMFString.Create('url', []));
  Fields.Last.Exposed := true;

  Fields.Add(TSFString.Create('type', 'VERTEX'));
end;

class function TNodeShaderProgram.ClassNodeTypeName: string;
begin
  Result := 'ShaderProgram';
end;

{$endif read_implementation}
