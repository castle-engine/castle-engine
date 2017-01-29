{
  Copyright 2014-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

type
  { Texture nodes, indexed by attachment name. }
  TImageTextureNodeMap = specialize TFPGMap<string, TImageTextureNode>;

  { Spine simple texture loader (without atlas).
    Reads each region attachment as a single texture file,
    from a filename 'images/' + AttachmentName + '.png'. }
  TSimpleTextureLoader = class(TTextureLoader)
  strict private
    Nodes: TImageTextureNodeMap;
    BaseUrl: string;
  public
    constructor Create(const ABaseUrl: string);
    destructor Destroy; override;
    function UseNode(const AttachmentName: string;
      out TexCoord: TQuadTexCoord; out TexRect: TQuadTexRect;
      out TexRotated: boolean): TImageTextureNode; override;
  end;

constructor TSimpleTextureLoader.Create(const ABaseUrl: string);
begin
  inherited Create;
  BaseUrl := ABaseUrl;
  Nodes := TImageTextureNodeMap.Create;
end;

destructor TSimpleTextureLoader.Destroy;
begin
  FreeAndNil(Nodes);
  inherited;
end;

function TSimpleTextureLoader.UseNode(const AttachmentName: string;
  out TexCoord: TQuadTexCoord; out TexRect: TQuadTexRect;
  out TexRotated: boolean): TImageTextureNode;
var
  Index: Integer;
begin
  Index := Nodes.IndexOf(AttachmentName);
  if Index <> -1 then
    Result := Nodes.Data[Index] else
  begin
    Result := TImageTextureNode.Create('SimpleTexture_' + ToX3DName(AttachmentName), BaseUrl);
    Result.FdUrl.Items.Add('images/' + AttachmentName + '.png');
    Result.RepeatS := false;
    Result.RepeatT := false;

    Nodes[AttachmentName] := Result;
  end;

  TexCoord[0] := Vector2Single(0, 0);
  TexCoord[1] := Vector2Single(1, 0);
  TexCoord[2] := Vector2Single(1, 1);
  TexCoord[3] := Vector2Single(0, 1);

  TexRect[0] := Vector2Single(0, 0);
  TexRect[1] := Vector2Single(1, 1);

  TexRotated := false;
end;
