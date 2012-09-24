{
  Copyright 2007-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Texture properties, like footsteps sound (TTextureProperties,
  global TexturesProperties collection). }
unit CastleTextureProperties;

interface

uses CastleUtils, CastleClassUtils, Classes, X3DTriangles, DOM, CastleSoundEngine, FGL;

type
  { Texture properties store any information that is naturally
    associated with a given texture image. This can be any information
    related to game, for example the sound
    of footsteps when walking on this texture, or whether walking on this texture
    hurts the player (like a lava pit, or toxic waste in a game).

    We encode this information in a separate XML file,
    which must be loaded by setting @link(TTexturePropertiesList.XmlFileName
    TexturesProperties.XmlFileName) property.

    Possibly, this information will be moved to VRML/X3D extension of ImageTexture,
    settable from Blender, in the future. It's not that easy, as we want it
    associated with a texture, regardless on how many shapes it's applied.
    Ultimately, it depends on the advancement of our Blender -> X3D exporter. }
  TTextureProperties = class
  private
    FBaseName: string;
    { Footsteps sound to make when player is walking on this texture.
      stNone is no information is available. }
    FFootstepsSound: TSoundType;
    FLava: boolean;
    FLavaDamageConst, FLavaDamageRandom, FLavaDamageTime: Single;
    procedure LoadFromDOMElement(Element: TDOMElement);
  public
    { Footsteps sound to make when player is walking on this texture.
      stNone is no information is available. }
    property FootstepsSound: TSoundType read FFootstepsSound;

    property Lava: boolean read FLava;
    property LavaDamageConst: Single read FLavaDamageConst;
    property LavaDamageRandom: Single read FLavaDamageRandom;
    property LavaDamageTime: Single read FLavaDamageTime;
  end;

  { Texture properties collection, see TTextureProperties. }
  TTexturePropertiesList = class(specialize TFPGObjectList<TTextureProperties>)
  private
    Triangle_Cache: boolean;
    Triangle_Last: PTriangle;
    Triangle_LastResult: TTextureProperties;
    FXmlFileName: string;
    procedure SetXmlFileName(const Value: string);
  public
    { Load texture properties from given XML file.
      See castle1 data for example how this looks like,
      in @code(castle/data/textures/index.xml). }
    property XmlFileName: string read FXmlFileName write SetXmlFileName;

    { Find texture properties for given Triangle of 3D world.
      Returns @nil if no texture properties are specified for texture
      on this triangle, which in particular will happen if you didn't
      set XmlFileName yet.

      As a convenience, Triangle may be @nil, and in this case result
      will always be @nil too. This is comfortable, as your Triangle
      will usually come from collision routines or TWalkCamera.AboveGround,
      which may return @nil. }
    function Find(Triangle: PTriangle): TTextureProperties;
  end;

{ Known texture properties.
  Set the @link(TTexturePropertiesList.XmlFileName XmlFileName) property
  to load texture properties from XML file. }
function TexturesProperties: TTexturePropertiesList;

implementation

uses SysUtils, XMLRead, CastleXMLUtils, CastleFilesUtils, X3DNodes;

{ TTextureProperties --------------------------------------------------------- }

procedure TTextureProperties.LoadFromDOMElement(Element: TDOMElement);
var
  FootstepsSoundName: string;
  LavaDamage: TDOMElement;
  I: TXMLElementIterator;
begin
  if not DOMGetAttribute(Element, 'base_name', FBaseName) then
    raise Exception.Create('<texture> element must have "base_name" attribute');

  FootstepsSoundName := '';
  if DOMGetAttribute(Element, 'footsteps_sound', FootstepsSoundName) and
     (FootstepsSoundName <> '') then
    FFootstepsSound := SoundEngine.SoundFromName(FootstepsSoundName) else
    FFootstepsSound := stNone;

  I := TXMLElementIterator.Create(Element);
  try
    while I.GetNext do
      if I.Current.TagName = 'lava' then
      begin
        FLava := true;
        LavaDamage := DOMGetOneChildElement(I.Current);
        if (LavaDamage = nil) or (LavaDamage.TagName <> 'damage') then
          raise Exception.Create('Missing <damage> inside <lava> element');
        if not DOMGetSingleAttribute(LavaDamage, 'const', FLavaDamageConst) then
          FLavaDamageConst := 0;
        if not DOMGetSingleAttribute(LavaDamage, 'random', FLavaDamageRandom) then
          FLavaDamageRandom := 0;
        if not DOMGetSingleAttribute(LavaDamage, 'time', FLavaDamageTime) then
          FLavaDamageTime := 0;
      end else
        raise Exception.CreateFmt('Unknown element inside <texture>: "%s"',
          [I.Current.TagName]);
  finally FreeAndNil(I) end;
end;

{ TTexturePropertiesList ---------------------------------------------------------- }

procedure TTexturePropertiesList.SetXmlFileName(const Value: string);
var
  TextureConfig: TXMLDocument;
  TextureElement: TDOMElement;
  TextureElements: TDOMNodeList;
  TextureProperties: TTextureProperties;
  I: Integer;
begin
  FXmlFileName := Value;

  Clear;

  try
    { ReadXMLFile always sets TXMLDocument param (possibly to nil),
      even in case of exception. So place it inside try..finally. }
    ReadXMLFile(TextureConfig, XmlFileName);

    Check(TextureConfig.DocumentElement.TagName = 'textures',
      'Root node of textures/index.xml must be <textures>');

    TextureElements := TextureConfig.DocumentElement.ChildNodes;
    try
      for I := 0 to TextureElements.Count - 1 do
        if TextureElements.Item[I].NodeType = ELEMENT_NODE then
        begin
          TextureElement := TextureElements.Item[I] as TDOMElement;
          Check(TextureElement.TagName = 'texture',
            'Each child of texture/index.xml root node must be the <texture> element');

          TextureProperties := TTextureProperties.Create;
          Add(TextureProperties);

          TextureProperties.LoadFromDOMElement(TextureElement);
        end;
    finally FreeChildNodes(TextureElements); end;
  finally
    SysUtils.FreeAndNil(TextureConfig);
  end;
end;

function TTexturePropertiesList.Find(Triangle: PTriangle): TTextureProperties;
var
  HasTextureUrl: boolean;
  TextureUrl: string;
  I: Integer;
begin
  { Results of this are cached, since this is very often
    asked with the same Triangle pointer when player walks. }
  if Triangle_Cache and (Triangle_Last = Triangle) then
  begin
    Result := Triangle_LastResult;
    Exit;
  end;

  Result := nil;

  HasTextureUrl := false;

  if Triangle <> nil then
  begin
    if Triangle^.State.ShapeNode <> nil then
    begin
      { VRML 2.0 path }
      if (Triangle^.State.ShapeNode.Texture <> nil) and
         (Triangle^.State.ShapeNode.Texture is TImageTextureNode) then
      begin
        TextureUrl := TImageTextureNode(
          Triangle^.State.ShapeNode.Texture).FdUrl.Items[0];
        HasTextureUrl := true;
      end;
    end else
    begin
      { VRML 1.0 path }
      TextureUrl := Triangle^.State.LastNodes.Texture2.FdFileName.Value;
      HasTextureUrl := true;
    end;
  end;

  if HasTextureUrl then
  begin
    TextureUrl := DeleteFileExt(ExtractFileName(TextureUrl));

    for I := 0 to Count - 1 do
    begin
      if SameText(Items[I].FBaseName, TextureUrl) then
      begin
        Result := Items[I];
        break;
      end;
    end;
  end;

  Triangle_Cache := true;
  Triangle_Last := Triangle;
  Triangle_LastResult := Result;
end;

var
  FTexturesProperties: TTexturePropertiesList;

function TexturesProperties: TTexturePropertiesList;
begin
  if FTexturesProperties = nil then
    FTexturesProperties := TTexturePropertiesList.Create(true);
  Result := FTexturesProperties;
end;

finalization
  FreeAndNil(FTexturesProperties);
end.
