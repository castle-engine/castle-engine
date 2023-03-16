{%MainUnit x3dloadinternalspine.pas}
{
  Copyright 2014-2023 Michalis Kamburelis.

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
  TImageTextureNodeMap = {$ifdef FPC}specialize{$endif} TDictionary<string, TImageTextureNode>;

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

procedure AddSpineTextureProperties(const Node: TImageTextureNode; const IsRepeat: Boolean);
var
  TexProperties: TTexturePropertiesNode;
begin
  { No point in adjusting RepeatS/T: TextureProperties overrides it with BoundaryModeS/T.
  Node.RepeatS := IsRepeat;
  Node.RepeatT := IsRepeat;
  }

  TexProperties := TTexturePropertiesNode.Create;
  // TexProperties.MagnificationFilter := magDefault; // this is default now
  // TexProperties.MinificationFilter := minDefault; // this is default now
  TexProperties.BoundaryModeS := BoolRepeatToBoundaryMode[IsRepeat];
  TexProperties.BoundaryModeT := BoolRepeatToBoundaryMode[IsRepeat];
  { Do not force "power of 2" size, which may prevent mipmaps.
    Not resizing is consistent with X3DLoadInternalImage, LoadCastleSpriteSheet,
    Tiled maps. Users may use Spine with pixel-art and don't expect images to be resized.
    See https://github.com/castle-engine/castle-engine/issues/464 . }
  TexProperties.GuiTexture := true;
  Node.TextureProperties := TexProperties;
end;

function TSimpleTextureLoader.UseNode(const AttachmentName: string;
  out TexCoord: TQuadTexCoord; out TexRect: TQuadTexRect;
  out TexRotated: boolean): TImageTextureNode;
begin
  if not Nodes.TryGetValue(AttachmentName, Result) then
  begin
    Result := TImageTextureNode.Create('SimpleTexture_' + AttachmentName, BaseUrl);
    if not SpineIgnoreTextures then
      Result.SetUrl(['images/' + AttachmentName + '.png']);
    AddSpineTextureProperties(Result, false);

    Nodes.AddOrSetValue(AttachmentName, Result);
  end;

  TexCoord[0] := Vector2(0, 0);
  TexCoord[1] := Vector2(1, 0);
  TexCoord[2] := Vector2(1, 1);
  TexCoord[3] := Vector2(0, 1);

  TexRect[0] := Vector2(0, 0);
  TexRect[1] := Vector2(1, 1);

  TexRotated := false;
end;
