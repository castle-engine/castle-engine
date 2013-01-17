{
  Copyright 2007-2013 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Material and texture properties from external files (TMaterialProperty,
  global MaterialProperties collection). }
unit CastleMaterialProperties;

interface

uses CastleUtils, CastleClassUtils, Classes, X3DTriangles, DOM, CastleSoundEngine, FGL;

type
  { Store information that is naturally associated with a given material
    or texture in an external file. Right now this allows to define things
    like footsteps, toxic ground (hurts player), and bump mapping.

    In the future, it should be possible to express all these properties
    in pure VRML/X3D (inside Appearance / Material / ImageTexture nodes).
    Right now, you can do this with bump mapping, see
    http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_bump_mapping ,
    but not footsteps or toxic ground.
    In the future it should also be possible to express these properties
    in 3D authoring software (like Blender), and easily export them
    to appropriate VRML/X3D nodes.
    For now, this TMaterialProperty allows us to easily customize materials
    in a way that is not possible in Blender.

    Using an external file for material properties has also long-term
    advantages: it can be shared across many 3D models, for example
    you can define footsteps sound for all grounds using the @code(grass.png)
    textures, in all levels, at once.

    You have to load an XML file by setting
    @link(TMaterialProperties.FileName MaterialProperties.FileName) property. }
  TMaterialProperty = class
  private
    FBaseName: string;
    FFootstepsSound: TSoundType;
    FToxic: boolean;
    FToxicDamageConst, FToxicDamageRandom, FToxicDamageTime: Single;
    procedure LoadFromDOMElement(Element: TDOMElement);
  public
    { Footsteps sound to make when player is walking on this material.
      stNone is no information is available. }
    property FootstepsSound: TSoundType read FFootstepsSound;

    property Toxic: boolean read FToxic;
    property ToxicDamageConst: Single read FToxicDamageConst;
    property ToxicDamageRandom: Single read FToxicDamageRandom;
    property ToxicDamageTime: Single read FToxicDamageTime;
  end;

  { Material properties collection, see TMaterialProperty. }
  TMaterialProperties = class(specialize TFPGObjectList<TMaterialProperty>)
  private
    Triangle_Cache: boolean;
    Triangle_Last: PTriangle;
    Triangle_LastResult: TMaterialProperty;
    FFileName: string;
    procedure SetFileName(const Value: string);
  public
    { Load material properties from given XML file.
      See Castle1 and fps_game data for examples how this looks like,
      in @code(material_properties.xml). }
    property FileName: string read FFileName write SetFileName;

    { Find material properties for given Triangle of 3D world.
      Returns @nil if no material properties are specified for
      this triangle, which in particular will happen if you didn't
      set FileName yet.

      As a convenience, Triangle may be @nil, and in this case result
      will always be @nil too. This is comfortable, as your Triangle
      will usually come from collision routines or TWalkCamera.AboveGround,
      which may return @nil. }
    function Find(Triangle: PTriangle): TMaterialProperty;
  end;

{ Known material properties.
  Set the @link(TMaterialProperties.FileName FileName) property
  to load material properties from XML file. }
function MaterialProperties: TMaterialProperties;

implementation

uses SysUtils, XMLRead, CastleXMLUtils, CastleFilesUtils, X3DNodes;

{ TMaterialProperty --------------------------------------------------------- }

procedure TMaterialProperty.LoadFromDOMElement(Element: TDOMElement);
var
  FootstepsSoundName: string;
  ToxicDamage: TDOMElement;
  I: TXMLElementIterator;
begin
  if not DOMGetAttribute(Element, 'texture_base_name', FBaseName) then
    raise Exception.Create('<properties> element must have "texture_base_name" attribute');

  FootstepsSoundName := '';
  if DOMGetAttribute(Element, 'footsteps_sound', FootstepsSoundName) and
     (FootstepsSoundName <> '') then
    FFootstepsSound := SoundEngine.SoundFromName(FootstepsSoundName) else
    FFootstepsSound := stNone;

  I := TXMLElementIterator.Create(Element);
  try
    while I.GetNext do
      if I.Current.TagName = 'toxic' then
      begin
        FToxic := true;
        ToxicDamage := DOMGetOneChildElement(I.Current);
        if (ToxicDamage = nil) or (ToxicDamage.TagName <> 'damage') then
          raise Exception.Create('Missing <damage> inside <toxic> element');
        if not DOMGetSingleAttribute(ToxicDamage, 'const', FToxicDamageConst) then
          FToxicDamageConst := 0;
        if not DOMGetSingleAttribute(ToxicDamage, 'random', FToxicDamageRandom) then
          FToxicDamageRandom := 0;
        if not DOMGetSingleAttribute(ToxicDamage, 'time', FToxicDamageTime) then
          FToxicDamageTime := 0;
      end else
        raise Exception.CreateFmt('Unknown element inside <property>: "%s"',
          [I.Current.TagName]);
  finally FreeAndNil(I) end;
end;

{ TMaterialProperties ---------------------------------------------------------- }

procedure TMaterialProperties.SetFileName(const Value: string);
var
  Config: TXMLDocument;
  Element: TDOMElement;
  Elements: TDOMNodeList;
  MaterialProperty: TMaterialProperty;
  I: Integer;
begin
  FFileName := Value;

  Clear;

  try
    { ReadXMLFile always sets TXMLDocument param (possibly to nil),
      even in case of exception. So place it inside try..finally. }
    ReadXMLFile(Config, FileName);

    Check(Config.DocumentElement.TagName = 'properties',
      'Root node of material properties file must be <properties>');

    Elements := Config.DocumentElement.ChildNodes;
    try
      for I := 0 to Elements.Count - 1 do
        if Elements.Item[I].NodeType = ELEMENT_NODE then
        begin
          Element := Elements.Item[I] as TDOMElement;
          Check(Element.TagName = 'property',
            'Material properties file must be a sequence of <property> elements');

          MaterialProperty := TMaterialProperty.Create;
          Add(MaterialProperty);

          MaterialProperty.LoadFromDOMElement(Element);
        end;
    finally FreeChildNodes(Elements); end;
  finally
    SysUtils.FreeAndNil(Config);
  end;
end;

function TMaterialProperties.Find(Triangle: PTriangle): TMaterialProperty;
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
  FMaterialProperties: TMaterialProperties;

function MaterialProperties: TMaterialProperties;
begin
  if FMaterialProperties = nil then
    FMaterialProperties := TMaterialProperties.Create(true);
  Result := FMaterialProperties;
end;

finalization
  FreeAndNil(FMaterialProperties);
end.
