{
  Copyright 2014-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

type
  TAppearanceNodeList = specialize TFPGObjectList<TAppearanceNode>;

  { Spine simple texture loader (without atlas).
    Reads each region attachment as a single texture file,
    from a filename 'images/' + AttachmentName + '.png'. }
  TSimpleTextureLoader = class(TTextureLoader)
  strict private
    { TODO: use TFPGMap.}
    NodeAttachmentNames: TStringList;
    Nodes: TAppearanceNodeList;
    BaseUrl: string;
  public
    constructor Create(const ABaseUrl: string);
    destructor Destroy; override;
    function UseNode(const AttachmentName: string;
      out TexCoord: TQuadTexCoord; out TexRect: TQuadTexRect): TAppearanceNode; override;
  end;

constructor TSimpleTextureLoader.Create(const ABaseUrl: string);
begin
  inherited Create;
  BaseUrl := ABaseUrl;
  NodeAttachmentNames := TStringList.Create;
  Nodes := TAppearanceNodeList.Create(false);
end;

destructor TSimpleTextureLoader.Destroy;
begin
  FreeAndNil(NodeAttachmentNames);
  FreeAndNil(Nodes);
  inherited;
end;

function TSimpleTextureLoader.UseNode(const AttachmentName: string;
  out TexCoord: TQuadTexCoord; out TexRect: TQuadTexRect): TAppearanceNode;
var
  Index: Integer;
  Texture: TImageTextureNode;
begin
  Index := NodeAttachmentNames.IndexOf(AttachmentName);
  if Index <> -1 then
    Result := Nodes[Index] else
  begin
    Result := TAppearanceNode.Create('SimpleTexture_' + ToX3DName(AttachmentName), BaseUrl);

    Texture := TImageTextureNode.Create('', BaseUrl);
    Texture.FdUrl.Items.Add('images/' + AttachmentName + '.png');
    Texture.RepeatS := false;
    Texture.RepeatT := false;
    Result.FdTexture.Value := Texture;

    NodeAttachmentNames.Add(AttachmentName);
    Nodes.Add(Result);
  end;

  TexCoord[0] := Vector2Single(0, 0);
  TexCoord[1] := Vector2Single(1, 0);
  TexCoord[2] := Vector2Single(1, 1);
  TexCoord[3] := Vector2Single(0, 1);

  TexRect[0] := Vector2Single(0, 0);
  TexRect[1] := Vector2Single(1, 1);
end;
