{
  Copyright 2008-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Load Collada. }
unit X3DLoadInternalCollada;

{$I castleconf.inc}

interface

uses X3DNodes;

{ Load Collada file as X3D.

  Written based on Collada 1.3.1 and 1.4.1 specifications.
  Should handle any Collada 1.3.x or 1.4.x or 1.5.x version.
  http://www.gamasutra.com/view/feature/1580/introduction_to_collada.php?page=6
  suggests that "specification stayed quite stable between 1.1 and 1.3.1",
  which means that older versions (< 1.3.1) may be handled too.

  @param(AllowKambiExtensions If @true we may use some of our engine specific
    extensions. For example, Material.mirror may be <> 0,
    see [http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_material_mirror].) }
function LoadCollada(const URL: string;
  const AllowKambiExtensions: boolean = false): TX3DRootNode;

implementation

uses SysUtils, Classes, DOM, XMLRead, FGL, Math, URIParser,
  CastleUtils, CastleStringUtils, CastleVectors, CastleColors,
  CastleXMLUtils, CastleLog, CastleClassUtils, X3DLoadInternalUtils,
  CastleDownload, CastleURIUtils, CastleGenericLists;

{ Large missing stuff:

  - GPU shaders. We fully support the equivalent X3D nodes (for GLSL),
    so importing this from Collada (converting to X3D) is possible.

  - Animations. As an argument for supporting it, Blender *can* export
    some animations to Collada:

    - Transformation animation: Blender exports it,
      in a format that is close to Blender animation curves.
      There's a separate info about location.x, location.y etc. animation.
      Animation of rotation is animating angle of rotation around the X axis, etc.
      Animations are connected to Collada nodes by

        <channel source="#Cube_rotation_euler_Z-sampler" target="Cube/rotationZ.ANGLE"/>

      "target" attribute uses Collada id path.
      We'd have to group such animations of a vector components into a single
      animation (in X3D, you cannot animate translation.x separately,
      you only animate whole 3D translation vector).
      This is doable for our importer.

    - Shape keys animation from Blender is not exported to Collada, it seems.

    - Armature animation: Blender exports armature bones animation.
      Also, armature hierarchy is exported as Collada nodes.
      Armature bone names and weights are exported too.

      So armature is exported --- in a format reflecting armature setup in Blender,
      not as a processed effect it has on a model skin.
      Rendering it means performing bones+skin animation at runtime.
      X3D equivalent of this is H-Anim with skin, which we support fully.
      So importing such Collada is possible.

      It depends on importing first normal transformation animation
      (as the bones are animated just like normal transformations;
      there's no point in importing skin stuff if you can't import bones animation).

  These are all doable ideas, but also not trivial.
  Since Blender can export animations to Collada (and not yet to X3D),
  it would be quite useful.
  Contributions are welcome!
}

{$define read_interface}
{$I x3dloadinternalcollada_controllers.inc}
{$I x3dloadinternalcollada_effects.inc}
{$I x3dloadinternalcollada_primitives.inc}
{$I x3dloadinternalcollada_geometries.inc}
{$I x3dloadinternalcollada_integerparser.inc}
{$I x3dloadinternalcollada_indexes.inc}
{$I x3dloadinternalcollada_materialsmap.inc}
{$I x3dloadinternalcollada_source.inc}
{$I x3dloadinternalcollada_sources.inc}
{$I x3dloadinternalcollada_stringtexturenodemap.inc}
{$I x3dloadinternalcollada_librarynodes.inc}
{$undef read_interface}

{$define read_implementation}
{$I x3dloadinternalcollada_controllers.inc}
{$I x3dloadinternalcollada_effects.inc}
{$I x3dloadinternalcollada_primitives.inc}
{$I x3dloadinternalcollada_geometries.inc}
{$I x3dloadinternalcollada_integerparser.inc}
{$I x3dloadinternalcollada_indexes.inc}
{$I x3dloadinternalcollada_materialsmap.inc}
{$I x3dloadinternalcollada_source.inc}
{$I x3dloadinternalcollada_sources.inc}
{$I x3dloadinternalcollada_stringtexturenodemap.inc}
{$I x3dloadinternalcollada_librarynodes.inc}
{$undef read_implementation}

{ LoadCollada ---------------------------------------------------------------- }

function LoadCollada(const URL: string;
  const AllowKambiExtensions: boolean): TX3DRootNode;
var
  BaseUrl: string;
  ResultModel: TGroupNode absolute Result;
  Version14: boolean; //< Collada version >= 1.4.x

  {$define read_interface_nested}
  {$I x3dloadinternalcollada_effects.inc}
  {$I x3dloadinternalcollada_primitives.inc}
  {$I x3dloadinternalcollada_geometries.inc}
  {$I x3dloadinternalcollada_matrix.inc}
  {$I x3dloadinternalcollada_controllers.inc}
  {$I x3dloadinternalcollada_integerparser.inc}
  {$I x3dloadinternalcollada_indexes.inc}
  {$I x3dloadinternalcollada_materialsmap.inc}
  {$I x3dloadinternalcollada_source.inc}
  {$I x3dloadinternalcollada_sources.inc}
  {$I x3dloadinternalcollada_stringtexturenodemap.inc}
  {$I x3dloadinternalcollada_read_helpers.inc}
  {$I x3dloadinternalcollada_librarynodes.inc}
  {$I x3dloadinternalcollada_materials.inc}
  {$I x3dloadinternalcollada_cameras.inc}
  {$I x3dloadinternalcollada_lights.inc}
  {$I x3dloadinternalcollada_images.inc}
  {$I x3dloadinternalcollada_node.inc}
  {$I x3dloadinternalcollada_scenes.inc}
  {$undef read_interface_nested}

  {$define read_implementation_nested}
  {$I x3dloadinternalcollada_effects.inc}
  {$I x3dloadinternalcollada_primitives.inc}
  {$I x3dloadinternalcollada_geometries.inc}
  {$I x3dloadinternalcollada_matrix.inc}
  {$I x3dloadinternalcollada_controllers.inc}
  {$I x3dloadinternalcollada_integerparser.inc}
  {$I x3dloadinternalcollada_indexes.inc}
  {$I x3dloadinternalcollada_materialsmap.inc}
  {$I x3dloadinternalcollada_source.inc}
  {$I x3dloadinternalcollada_sources.inc}
  {$I x3dloadinternalcollada_stringtexturenodemap.inc}
  {$I x3dloadinternalcollada_read_helpers.inc}
  {$I x3dloadinternalcollada_librarynodes.inc}
  {$I x3dloadinternalcollada_materials.inc}
  {$I x3dloadinternalcollada_cameras.inc}
  {$I x3dloadinternalcollada_lights.inc}
  {$I x3dloadinternalcollada_images.inc}
  {$I x3dloadinternalcollada_node.inc}
  {$I x3dloadinternalcollada_scenes.inc}
  {$undef read_implementation_nested}

var
  Doc: TXMLDocument;
  Version: string;
  I: TXMLElementIterator;
  LibraryE: TDOMElement;
  Stream: TStream;
begin
  Effects := nil;
  Materials := nil;
  Geometries := nil;
  VisualScenes := nil;
  Controllers := nil;
  Images := nil;
  Cameras := nil;
  Lights := nil;
  Result := nil;

  try
    Stream := Download(URL);
    try
      ReadXMLFile(Doc, Stream);
    finally FreeAndNil(Stream) end;

    try
      Check(Doc.DocumentElement.TagName = 'COLLADA',
        'Root node of Collada file must be <COLLADA>');

      if not Doc.DocumentElement.AttributeString('version', Version) then
      begin
        Version := '';
        Version14 := false;
        WritelnWarning('Collada', '<COLLADA> element misses "version" attribute');
      end else
      begin
        { TODO: uhm, terrible hack... I should move my lazy ass and tokenize
          Version properly. }
        Version14 := IsPrefix('1.4.', Version) or IsPrefix('1.5.', Version);
      end;

      { honour COLLADA.base (exactly for the same purpose as our BaseUrl),
        but only if it's absolute }
      if not (Doc.DocumentElement.AttributeString('base', BaseUrl) and
              IsAbsoluteURI(BaseUrl)) then
        BaseUrl := AbsoluteURI(URL);

      Effects := TColladaEffectList.Create;
      Materials := TColladaMaterialsMap.Create;
      Geometries := TColladaGeometryList.Create;
      VisualScenes := TX3DNodeList.Create(false);
      Controllers := TColladaControllerList.Create;
      Images := TX3DNodeList.Create(false);
      Cameras := TX3DNodeList.Create(false);
      Lights := TX3DNodeList.Create(false);
      LibraryNodesSwitch := nil; // will be created on-demand if needed

      Result := TX3DRootNode.Create('', BaseUrl);
      Result.HasForceVersion := true;
      Result.ForceVersion := X3DVersion;

      { Read library_images. These may be referred to inside effects. }
      LibraryE := Doc.DocumentElement.ChildElement('library_images', false);
      if LibraryE <> nil then
        ReadLibraryImages(LibraryE);

      { Read library_effects.
        Effects may be referenced by materials,
        and there's no guarantee that library_effects will occur before
        library_materials. Testcase: "COLLLADA 1.4.1 Basic Samples/Cube/cube.dae". }
      LibraryE := Doc.DocumentElement.ChildElement('library_effects', false);
      if LibraryE <> nil then
        ReadLibraryEffects(LibraryE);

      { Read libraries of things that may be referenced within library_visual_scenes.
        For some models, <library_visual_scenes> may be before them (test from
        collada.org/owl/ : "New Uploads/COLLADA 1.5.0 Kinematics/COLLADA 1.5.0 Kinematics/COLLADA/KR150/kr150.dae") }
      I := Doc.DocumentElement.ChildrenIterator;
      try
        while I.GetNext do
          if I.Current.TagName = 'library' then { only Collada < 1.4.x }
            ReadLibrary(I.Current) else
          if I.Current.TagName = 'library_materials' then { only Collada >= 1.4.x }
            ReadLibraryMaterials(I.Current) else
          if I.Current.TagName = 'library_geometries' then { only Collada >= 1.4.x }
            ReadLibraryGeometries(I.Current) else
          if I.Current.TagName = 'library_cameras' then { only Collada >= 1.4.x }
            ReadLibraryCameras(I.Current) else
          if I.Current.TagName = 'library_lights' then { only Collada >= 1.4.x }
            ReadLibraryLights(I.Current) else
          if I.Current.TagName = 'library_controllers' then { only Collada >= 1.4.x }
            ReadLibraryControllers(I.Current);
      finally FreeAndNil(I); end;

      I := Doc.DocumentElement.ChildrenIterator;
      try
        while I.GetNext do
          if I.Current.TagName = 'library_nodes' then
            ReadLibraryNodes(I.Current);
      finally FreeAndNil(I); end;

      I := Doc.DocumentElement.ChildrenIterator;
      try
        while I.GetNext do
          if I.Current.TagName = 'library_visual_scenes' then { only Collada >= 1.4.x }
            ReadLibraryVisualScenes(I.Current) else
          if I.Current.TagName = 'scene' then
            ReadSceneElement(I.Current);
      finally FreeAndNil(I); end;

      Result.Meta.PutPreserve('source', ExtractURIName(URL));
      Result.Meta['source-collada-version'] := Version;
    finally
      FreeAndNil(Doc);

      { Free unused Images before freeing Effects.
        That's because image may be used inside an effect,
        and would be invalid reference after freeing effect. }
      X3DNodeList_FreeUnusedAndNil(Images);

      FreeAndNil(Materials);

      { Note: if some effect will be used by some geometry, but the
        geometry will not be used, everything will be still Ok
        (no memory leak). First freeing over Effects will not free this
        effect (since it's used), but then freeing over Geometries will
        free the geometry together with effect (since effect usage will
        drop to zero).

        This means that also other complicated case, when one effect is
        used twice, once by unused geometry node, second time by used geometry
        node, is also Ok. }
      FreeAndNil(Effects);
      FreeAndNil(Geometries);

      X3DNodeList_FreeUnusedAndNil(Lights);
      X3DNodeList_FreeUnusedAndNil(Cameras);
      X3DNodeList_FreeUnusedAndNil(VisualScenes);
      FreeAndNil(Controllers);
    end;
    { eventually free Result *after* freeing other lists, to make sure references
      on Images, Materials etc. are valid when their unused items are freed. }
  except FreeAndNil(Result); raise; end;
end;

end.
