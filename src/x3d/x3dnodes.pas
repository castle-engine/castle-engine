{
  Copyright 2002-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

(*
  @abstract(Nodes and other important bulding blocks
  of VRML/X3D (prototypes, routes and so on).)

  This is the central unit for VRML/X3D processing, as VRML/X3D file
  is basically just a graph of nodes. We represent whole VRML/X3D file
  by it's root node. This is what we load, save and process in this unit.

  The chapter "Reading, writing, processing VRML scene graph"
  in the documentation on
  [https://castle-engine.io/vrml_engine_doc/output/xsl/html/chapter.scene_graph.html]
  is almost completely devoted to documenting the design of this single unit.

  @bold(Various uses of this unit:)

  @unorderedList(
    @item(Nodes can be loaded or saved from the stream in a classic or
      XML encoding.
      For classic encoding we use a lexer in CastleInternalX3DLexer unit.
      For XML encoding, we use standard FPC DOM unit.
      Loading and saving of fields (in both encodings) is inside X3DFields unit.

      When reading VRML/X3D files, we generally do not change the VRML/X3D graph.
      So we're able to save exactly the same VRML/X3D graph
      back to another file. See also
      [https://castle-engine.io/vrml_engine_doc/output/xsl/html/section.writing_vrml.html#section.vrml_preserving].
      This allows writing various VRML/X3D
      processing tools, that can simply read the file, change whatever
      they want, and write the file back --- knowing that the "untouched"
      parts of graph are preserved perfectly.)

    @item(TX3DNode class offers a lot of methods to process VRML/X3D graph.
      See TX3DNode.Traverse, TX3DNode.EnumerateNodes and
      TX3DNode.FindNode. TX3DNode.Traverse is especially important, as it
      walks through VRML/X3D graph just as the specification says
      (accumulating transformation, visiting only active children of
      nodes like Switch or LOD),
      gathering some state (useful especially for VRML 1.0, but also
      used for various things in later VRML/X3D versions).

      When you want to render VRML/X3D graph, you can just traverse
      the graph and render each geometry node (TAbstractGeometryNode instance)
      knowing it's state (that will contain transformation and such).
      Alternatively, simple renderer can also use TAbstractGeometryNode.Triangulate.)

    @item(TAbstractGeometryNode is an important descendant of TX3DNode,
      as it defines stuff actually visible in the 3D world.
      It has useful routines for calculating bounding volumes,
      triangulating and such.

      But note that usually it's more comfortable
      to load your scene to TCastleScene or TCastleSceneCore and then
      query the shapes list in TCastleSceneCore.Shapes --- this is usually
      more comfortable, also TCastleSceneCore and TShape cache some results
      for speed.)

    @item(This unit doesn't depend on OpenGL, or any other particular rendering
      method. So it's suitable also for CastleRayTracer, and every other possible
      renderer that will ever get implemented.)

    @item(Your own units can define new VRML/X3D nodes, by declaring
      new classes descending from TX3DNode (or other, more specialized,
      descendant). You should register your new classes by calling
      @link(TNodesManager.RegisterNodeClasses NodesManager.RegisterNodeClasses).

      Examples of defining your own VRML/X3D node types (without modifying
      sources of this unit, or any other unit) are for example in "malfunction" game
      on https://github.com/castle-engine/malfunction (see LevelUnit).)
  )

  @bold(Node class names, and inheritance:)

  @unorderedList(
    @item(Normal VRML/X3D nodes are defined by classses
      named like @code(TXxxNode). These nodes can be specified inside the VRML/X3D
      files. See VRML/X3D specifications, and also our extensions specification,
      on [https://castle-engine.io/vrml_x3d.php].

      There are also abstract node classes. Their definitions are helpful
      for handling some functionality common to many descendants,
      and to declare allowed children in SFNode/MFNode fields.
      Abstract node classes are named like @code(TAbstractXxxNode).
      Some of the abstract nodes are also defined by X3D specification,
      and some of them are just our own inventions.

      Finally, there are some special-purpose node classes that play
      important role in our VRML/X3D organization.
      They are not abstract, but also their exact instances
      are not created under normal circumstances.
      These are named like @code(TX3DXxxNode), currently
      these are only: TX3DNode, TX3DRootNode, TX3DUnknownNode, TX3DPrototypeNode.

      All node classes descend from the base TX3DNode class.

      Some abstract nodes have also Pascal interfaces, like IAbstractXxxNode.
      Some ideas of X3D specification (although not many)
      need multiple inheritance, so interfaces have to be used.
      They all descend from IX3DNode.)

    @item(
      Optional suffix _1 or _2 at the node class name indicates that
      this is only for a specific VRML/X3D standard version.
      Suffix _1 indicates nodes specific to VRML 1.0.
      Suffix _2 indicates nodes specific to VRML 2.0 (aka 97),
      that are not available in X3D.
      Latest X3D nodes do not have any suffix
      (to not clutter the source code that simply wants to use the latest
      and best version of the standard).

      For example, we have TIndexedFaceSetNode_1 for VRML 1.0 and
      TIndexedFaceSetNode for VRML 2.0 and X3D.)
  )

  @bold(VRML/X3D versions handling:)

  @unorderedList(
    @item(
      We handle VRML 1.0, VRML 2.0 (aka VRML 97) and X3D (aka VRML 3.x).

      Every correct VRML / X3D file in classic and XML encoding should be parsed
      by this unit.
      See [https://castle-engine.io/x3d_implementation_status.php]
      for much more detailed information about supported features.)

    @item(
      Also many Inventor 1.0 files are correctly parsed.
      We handle Inventor 1.0 mostly like VRML 1.0, also some small
      things and nodes specific for Inventor 1.0 are implemented here, see
      [https://castle-engine.io/x3d_extensions.php#ext_iv_in_vrml].)

    @item(
      Note that structures in this unit are @italic(not) focused
      on either VRML 1.0 or VRML >= 2.0. On the contrary: we try to handle
      the @italic(sum of all VRML and X3D). When reading VRML 1.0,
      many VRML 2.0 constructs (that do not conflict with anything in VRML 1.0)
      are allowed, and the other way around too.

      Internally, we do not convert VRML 1.0-specific constructs
      to VRML 2.0/X3D constructs (or the other way around).
      See [https://castle-engine.io/vrml_engine_doc/output/xsl/html/section.vrml_1_2_sum.html]
      for more in-depth explanation of how, and why, we handle both
      old-style (Inventor, VRML 1.0) and new-style (VRML 2.0, X3D)
      syntax.)
  )
*)

unit X3DNodes;

{$I castleconf.inc}

interface

uses SysUtils, Generics.Collections, Classes, XMLRead, DOM,
  CastleVectors, CastleInternalDoubleLists, CastleRectangles,
  CastleInternalX3DLexer, CastleUtils, CastleClassUtils,
  X3DFields, CastleBoxes, CastleImages, CastleColors,
  CastleVideos, X3DTime, CastleTransform, CastleMaterialProperties,
  CastleScript, X3DCastleScript, CastleInternalOctree, CastleCompositeImage,
  CastleTextureImages, CastleKeysMouse, CastleSoundEngine, CastleStringUtils,
  CastleTextureFontData, CastleRendererBaseTypes, CastleProjection;

{$define read_interface}

type
  {$I x3dnodes_initial_types.inc}
  {$I x3dnodes_vrml1state.inc}
  {$I x3dnodes_lightinstance.inc}
  {$I x3dnodes_clipplane.inc}
  {$I x3dnodes_x3dgraphtraversestate.inc}
  {$I x3dnodes_destructionnotification.inc}
  {$I x3dnodes_x3dnodescache.inc}
  {$I x3dnodes_x3dnode.inc}
  {$I x3dnodes_generatedtextures.inc}
  {$I x3dnodes_x3dnodeclasseslist.inc}
  {$I x3dnodes_sfnode.inc}
  {$I x3dnodes_mfnode.inc}

  { Nodes from standard X3D components }
  {$I x3dnodes_standard_core.inc}
  {$I x3dnodes_standard_time.inc}
  {$I x3dnodes_standard_grouping.inc}
  {$I x3dnodes_standard_networking.inc}
  {$I x3dnodes_standard_rendering.inc}
  {$I x3dnodes_standard_shape.inc}
  {$I x3dnodes_standard_geometry3d.inc}
  {$I x3dnodes_standard_geometry2d.inc}
  {$I x3dnodes_standard_text.inc}
  {$I x3dnodes_standard_sound.inc}
  {$I x3dnodes_standard_lighting.inc}
  {$I x3dnodes_standard_texturing.inc}
  {$I x3dnodes_standard_interpolation.inc}
  {$I x3dnodes_standard_interpolation_cubic_bezier.inc}
  {$I x3dnodes_standard_pointingdevicesensor.inc}
  {$I x3dnodes_standard_keydevicesensor.inc}
  {$I x3dnodes_standard_environmentalsensor.inc}
  {$I x3dnodes_standard_navigation.inc}
  {$I x3dnodes_standard_environmentaleffects.inc}
  {$I x3dnodes_standard_geospatial.inc}
  {$I x3dnodes_standard_h-anim.inc}
  {$I x3dnodes_standard_nurbs.inc}
  {$I x3dnodes_standard_dis.inc}
  {$I x3dnodes_standard_scripting.inc}
  {$I x3dnodes_standard_eventutilities.inc}
  {$I x3dnodes_standard_shaders.inc}
  {$I x3dnodes_standard_cadgeometry.inc}
  {$I x3dnodes_standard_texturing3d.inc}
  {$I x3dnodes_standard_cubemaptexturing.inc}
  {$I x3dnodes_standard_layering.inc}
  {$I x3dnodes_standard_layout.inc}
  {$I x3dnodes_standard_rigidbodyphysics.inc}
  {$I x3dnodes_standard_picking.inc}
  {$I x3dnodes_standard_followers.inc}
  {$I x3dnodes_standard_particlesystems.inc}

  { More X3D nodes, not from X3D standard }
  {$I x3dnodes_1.inc}
  {$I x3dnodes_inventor.inc}
  {$I x3dnodes_97_hanim.inc}
  {$I x3dnodes_97_nurbs.inc}
  {$I x3dnodes_castle.inc}
  {$I x3dnodes_instantreality.inc}
  {$I x3dnodes_bitmanagement.inc}

  {$I x3dnodes_x3dunknownnode.inc}
  {$I x3dnodes_x3dinterfacedeclaration.inc}
  {$I x3dnodes_prototypes.inc}
  {$I x3dnodes_x3droute.inc}
  {$I x3dnodes_importexport.inc}
  {$I x3dnodes_eventsengine.inc}
  {$I x3dnodes_names.inc}
  {$I x3dnodes_load.inc}

{$I x3dnodes_nodesmanager.inc}
{$I x3dnodes_encoding_classic.inc}
{$I x3dnodes_encoding_xml.inc}
{$I x3dnodes_save.inc}
{$I x3dnodes_miscellaneous_globals.inc}

{$undef read_interface}

implementation

// Silence warnings about using CastleNURBS (that will soon be renamed CastleInternalNurbs)
{$warnings off}

uses
  { Fonts for Text, FontStyle, AsciiText nodes }
  CastleTextureFont_DjvSans_20,
  {$ifdef CASTLE_EMBED_ALL_3D_FONT_VARIATIONS}
  CastleTextureFont_DjvSansB_20,
  CastleTextureFont_DjvSansO_20,
  CastleTextureFont_DjvSansBO_20,

  CastleTextureFont_DjvMono_20,
  CastleTextureFont_DjvMonoB_20,
  CastleTextureFont_DjvMonoO_20,
  CastleTextureFont_DjvMonoBO_20,

  CastleTextureFont_DjvSerif_20,
  CastleTextureFont_DjvSerifB_20,
  CastleTextureFont_DjvSerifI_20,
  CastleTextureFont_DjvSerifBI_20,
  {$endif CASTLE_EMBED_ALL_3D_FONT_VARIATIONS}

  Math, X3DLoad, CastleInternalZStream, X3DCameraUtils,
  CastleFilesUtils, StrUtils, CastleURIUtils, CastleUnicode, CastleCurves,
  CastleLog, CastleScriptParser, CastleDataURI, URIParser, CastleDownload,
  CastleNURBS, CastleQuaternions, CastleCameras, CastleXMLUtils, CastleOpenDocument;

{$warnings on}

{$define read_implementation}

{$I x3dnodes_miscellaneous_internals.inc}

{$I x3dnodes_initial_types.inc}
{$I x3dnodes_vrml1state.inc}
{$I x3dnodes_lightinstance.inc}
{$I x3dnodes_clipplane.inc}
{$I x3dnodes_x3dgraphtraversestate.inc}
{$I x3dnodes_destructionnotification.inc}
{$I x3dnodes_x3dnodescache.inc}
{$I x3dnodes_x3dnodeclasseslist.inc}

{$I x3dnodes_utils_extrusion.inc}
{$I x3dnodes_utils_elevationgrid.inc}
{$I x3dnodes_utils_cone_cylinder.inc}
{$I x3dnodes_utils_sphere.inc}
{$I x3dnodes_utils_box.inc}
{$I x3dnodes_boundingboxes.inc}
{$I x3dnodes_verticesandtrianglescounting.inc}
{$I x3dnodes_coordpolygons.inc}
{$I x3dnodes_eventsengine.inc}
{$I x3dnodes_save.inc}
{$I x3dnodes_load.inc}
{$I x3dnodes_encoding_classic.inc}
{$I x3dnodes_encoding_xml.inc}
{$I x3dnodes_generatedtextures.inc}
{$I x3dnodes_x3dunknownnode.inc}
{$I x3dnodes_x3dinterfacedeclaration.inc}
{$I x3dnodes_prototypes.inc}
{$I x3dnodes_x3droute.inc}
{$I x3dnodes_importexport.inc}
{$I x3dnodes_names.inc}
{$I x3dnodes_nodesmanager.inc}
{$I x3dnodes_miscellaneous_globals.inc}

// These must be includes after x3dnodes_encoding_{classic,xml}.inc
{$I x3dnodes_x3dnode.inc}
{$I x3dnodes_sfnode.inc}
{$I x3dnodes_mfnode.inc}

{ Nodes from standard X3D components }
{$I x3dnodes_standard_core.inc}
{$I x3dnodes_standard_time.inc}
{$I x3dnodes_standard_grouping.inc}
{$I x3dnodes_standard_networking.inc}
{$I x3dnodes_standard_rendering.inc}
{$I x3dnodes_standard_shape.inc}
{$I x3dnodes_standard_geometry3d.inc}
{$I x3dnodes_standard_geometry2d.inc}
{$I x3dnodes_standard_text.inc}
{$I x3dnodes_standard_sound.inc}
{$I x3dnodes_standard_lighting.inc}
{$I x3dnodes_standard_texturing.inc}
{$I x3dnodes_standard_interpolation.inc}
{$I x3dnodes_standard_interpolation_cubic_bezier.inc}
{$I x3dnodes_standard_pointingdevicesensor.inc}
{$I x3dnodes_standard_keydevicesensor.inc}
{$I x3dnodes_standard_environmentalsensor.inc}
{$I x3dnodes_standard_navigation.inc}
{$I x3dnodes_standard_environmentaleffects.inc}
{$I x3dnodes_standard_geospatial.inc}
{$I x3dnodes_standard_h-anim.inc}
{$I x3dnodes_standard_nurbs.inc}
{$I x3dnodes_standard_dis.inc}
{$I x3dnodes_standard_scripting.inc}
{$I x3dnodes_standard_eventutilities.inc}
{$I x3dnodes_standard_shaders.inc}
{$I x3dnodes_standard_cadgeometry.inc}
{$I x3dnodes_standard_texturing3d.inc}
{$I x3dnodes_standard_cubemaptexturing.inc}
{$I x3dnodes_standard_layering.inc}
{$I x3dnodes_standard_layout.inc}
{$I x3dnodes_standard_rigidbodyphysics.inc}
{$I x3dnodes_standard_picking.inc}
{$I x3dnodes_standard_followers.inc}
{$I x3dnodes_standard_particlesystems.inc}

{ More X3D nodes, not from X3D standard }
{$I x3dnodes_1.inc}
{$I x3dnodes_inventor.inc}
{$I x3dnodes_97_hanim.inc}
{$I x3dnodes_97_nurbs.inc}
{$I x3dnodes_castle.inc}
{$I x3dnodes_instantreality.inc}
{$I x3dnodes_bitmanagement.inc}

{ Auto-generated nodes code }
{$I auto_generated_node_helpers/x3dnodes_anchor.inc}
{$I auto_generated_node_helpers/x3dnodes_appearance.inc}
{$I auto_generated_node_helpers/x3dnodes_arc2d.inc}
{$I auto_generated_node_helpers/x3dnodes_arcclose2d.inc}
{$I auto_generated_node_helpers/x3dnodes_audioclip.inc}
{$I auto_generated_node_helpers/x3dnodes_background.inc}
{$I auto_generated_node_helpers/x3dnodes_balljoint.inc}
{$I auto_generated_node_helpers/x3dnodes_billboard.inc}
{$I auto_generated_node_helpers/x3dnodes_blendmode.inc}
{$I auto_generated_node_helpers/x3dnodes_booleanfilter.inc}
{$I auto_generated_node_helpers/x3dnodes_booleansequencer.inc}
{$I auto_generated_node_helpers/x3dnodes_booleantoggle.inc}
{$I auto_generated_node_helpers/x3dnodes_booleantrigger.inc}
{$I auto_generated_node_helpers/x3dnodes_boundedphysicsmodel.inc}
{$I auto_generated_node_helpers/x3dnodes_box.inc}
{$I auto_generated_node_helpers/x3dnodes_cadassembly.inc}
{$I auto_generated_node_helpers/x3dnodes_cadface.inc}
{$I auto_generated_node_helpers/x3dnodes_cadlayer.inc}
{$I auto_generated_node_helpers/x3dnodes_cadpart.inc}
{$I auto_generated_node_helpers/x3dnodes_circle2d.inc}
{$I auto_generated_node_helpers/x3dnodes_clipplane.inc}
{$I auto_generated_node_helpers/x3dnodes_collidableoffset.inc}
{$I auto_generated_node_helpers/x3dnodes_collidableshape.inc}
{$I auto_generated_node_helpers/x3dnodes_collision.inc}
{$I auto_generated_node_helpers/x3dnodes_collisioncollection.inc}
{$I auto_generated_node_helpers/x3dnodes_collisionsensor.inc}
{$I auto_generated_node_helpers/x3dnodes_collisionspace.inc}
{$I auto_generated_node_helpers/x3dnodes_color.inc}
{$I auto_generated_node_helpers/x3dnodes_colordamper.inc}
{$I auto_generated_node_helpers/x3dnodes_colorinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_colorrgba.inc}
{$I auto_generated_node_helpers/x3dnodes_colorsetinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_commonsurfaceshader.inc}
{$I auto_generated_node_helpers/x3dnodes_composedcubemaptexture.inc}
{$I auto_generated_node_helpers/x3dnodes_composedshader.inc}
{$I auto_generated_node_helpers/x3dnodes_composedtexture3d.inc}
{$I auto_generated_node_helpers/x3dnodes_cone.inc}
{$I auto_generated_node_helpers/x3dnodes_coneemitter.inc}
{$I auto_generated_node_helpers/x3dnodes_contact.inc}
{$I auto_generated_node_helpers/x3dnodes_contour2d.inc}
{$I auto_generated_node_helpers/x3dnodes_contourpolyline2d.inc}
{$I auto_generated_node_helpers/x3dnodes_coordinate.inc}
{$I auto_generated_node_helpers/x3dnodes_coordinatedamper.inc}
{$I auto_generated_node_helpers/x3dnodes_coordinatedouble.inc}
{$I auto_generated_node_helpers/x3dnodes_coordinateinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_coordinateinterpolator2d.inc}
{$I auto_generated_node_helpers/x3dnodes_cylinder.inc}
{$I auto_generated_node_helpers/x3dnodes_cylindersensor.inc}
{$I auto_generated_node_helpers/x3dnodes_directionallight.inc}
{$I auto_generated_node_helpers/x3dnodes_disentitymanager.inc}
{$I auto_generated_node_helpers/x3dnodes_disentitytypemapping.inc}
{$I auto_generated_node_helpers/x3dnodes_disk2d.inc}
{$I auto_generated_node_helpers/x3dnodes_doubleaxishingejoint.inc}
{$I auto_generated_node_helpers/x3dnodes_easeineaseout.inc}
{$I auto_generated_node_helpers/x3dnodes_effect.inc}
{$I auto_generated_node_helpers/x3dnodes_effectpart.inc}
{$I auto_generated_node_helpers/x3dnodes_elevationgrid.inc}
{$I auto_generated_node_helpers/x3dnodes_espdutransform.inc}
{$I auto_generated_node_helpers/x3dnodes_explosionemitter.inc}
{$I auto_generated_node_helpers/x3dnodes_extrusion.inc}
{$I auto_generated_node_helpers/x3dnodes_fillproperties.inc}
{$I auto_generated_node_helpers/x3dnodes_floatvertexattribute.inc}
{$I auto_generated_node_helpers/x3dnodes_fog.inc}
{$I auto_generated_node_helpers/x3dnodes_fogcoordinate.inc}
{$I auto_generated_node_helpers/x3dnodes_fontstyle.inc}
{$I auto_generated_node_helpers/x3dnodes_forcephysicsmodel.inc}
{$I auto_generated_node_helpers/x3dnodes_generatedcubemaptexture.inc}
{$I auto_generated_node_helpers/x3dnodes_generatedshadowmap.inc}
{$I auto_generated_node_helpers/x3dnodes_geocoordinate.inc}
{$I auto_generated_node_helpers/x3dnodes_geoelevationgrid.inc}
{$I auto_generated_node_helpers/x3dnodes_geolocation.inc}
{$I auto_generated_node_helpers/x3dnodes_geolod.inc}
{$I auto_generated_node_helpers/x3dnodes_geometadata.inc}
{$I auto_generated_node_helpers/x3dnodes_geoorigin.inc}
{$I auto_generated_node_helpers/x3dnodes_geopositioninterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_geoproximitysensor.inc}
{$I auto_generated_node_helpers/x3dnodes_geotouchsensor.inc}
{$I auto_generated_node_helpers/x3dnodes_geotransform.inc}
{$I auto_generated_node_helpers/x3dnodes_geoviewpoint.inc}
{$I auto_generated_node_helpers/x3dnodes_group.inc}
{$I auto_generated_node_helpers/x3dnodes_hanimdisplacer.inc}
{$I auto_generated_node_helpers/x3dnodes_hanimhumanoid.inc}
{$I auto_generated_node_helpers/x3dnodes_hanimjoint.inc}
{$I auto_generated_node_helpers/x3dnodes_hanimsegment.inc}
{$I auto_generated_node_helpers/x3dnodes_hanimsite.inc}
{$I auto_generated_node_helpers/x3dnodes_imagecubemaptexture.inc}
{$I auto_generated_node_helpers/x3dnodes_imagetexture.inc}
{$I auto_generated_node_helpers/x3dnodes_imagetexture3d.inc}
{$I auto_generated_node_helpers/x3dnodes_indexedfaceset.inc}
{$I auto_generated_node_helpers/x3dnodes_indexedlineset.inc}
{$I auto_generated_node_helpers/x3dnodes_indexedquadset.inc}
{$I auto_generated_node_helpers/x3dnodes_indexedtrianglefanset.inc}
{$I auto_generated_node_helpers/x3dnodes_indexedtriangleset.inc}
{$I auto_generated_node_helpers/x3dnodes_indexedtrianglestripset.inc}
{$I auto_generated_node_helpers/x3dnodes_inline.inc}
{$I auto_generated_node_helpers/x3dnodes_integersequencer.inc}
{$I auto_generated_node_helpers/x3dnodes_integertrigger.inc}
{$I auto_generated_node_helpers/x3dnodes_kambiappearance.inc}
{$I auto_generated_node_helpers/x3dnodes_kambiheadlight.inc}
{$I auto_generated_node_helpers/x3dnodes_kambiinline.inc}
{$I auto_generated_node_helpers/x3dnodes_kambinavigationinfo.inc}
{$I auto_generated_node_helpers/x3dnodes_kambioctreeproperties.inc}
{$I auto_generated_node_helpers/x3dnodes_keysensor.inc}
{$I auto_generated_node_helpers/x3dnodes_layer.inc}
{$I auto_generated_node_helpers/x3dnodes_layerset.inc}
{$I auto_generated_node_helpers/x3dnodes_layout.inc}
{$I auto_generated_node_helpers/x3dnodes_layoutgroup.inc}
{$I auto_generated_node_helpers/x3dnodes_layoutlayer.inc}
{$I auto_generated_node_helpers/x3dnodes_linepicksensor.inc}
{$I auto_generated_node_helpers/x3dnodes_lineproperties.inc}
{$I auto_generated_node_helpers/x3dnodes_lineset.inc}
{$I auto_generated_node_helpers/x3dnodes_loadsensor.inc}
{$I auto_generated_node_helpers/x3dnodes_localfog.inc}
{$I auto_generated_node_helpers/x3dnodes_lod.inc}
{$I auto_generated_node_helpers/x3dnodes_logger.inc}
{$I auto_generated_node_helpers/x3dnodes_material.inc}
{$I auto_generated_node_helpers/x3dnodes_matrix3vertexattribute.inc}
{$I auto_generated_node_helpers/x3dnodes_matrix4vertexattribute.inc}
{$I auto_generated_node_helpers/x3dnodes_matrixtransform.inc}
{$I auto_generated_node_helpers/x3dnodes_metadataboolean.inc}
{$I auto_generated_node_helpers/x3dnodes_metadatadouble.inc}
{$I auto_generated_node_helpers/x3dnodes_metadatafloat.inc}
{$I auto_generated_node_helpers/x3dnodes_metadatainteger.inc}
{$I auto_generated_node_helpers/x3dnodes_metadataset.inc}
{$I auto_generated_node_helpers/x3dnodes_metadatastring.inc}
{$I auto_generated_node_helpers/x3dnodes_motorjoint.inc}
{$I auto_generated_node_helpers/x3dnodes_movietexture.inc}
{$I auto_generated_node_helpers/x3dnodes_multigeneratedtexturecoordinate.inc}
{$I auto_generated_node_helpers/x3dnodes_multitexture.inc}
{$I auto_generated_node_helpers/x3dnodes_multitexturecoordinate.inc}
{$I auto_generated_node_helpers/x3dnodes_multitexturetransform.inc}
{$I auto_generated_node_helpers/x3dnodes_navigationinfo.inc}
{$I auto_generated_node_helpers/x3dnodes_normal.inc}
{$I auto_generated_node_helpers/x3dnodes_normalinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_nurbscurve.inc}
{$I auto_generated_node_helpers/x3dnodes_nurbscurve2d.inc}
{$I auto_generated_node_helpers/x3dnodes_nurbsorientationinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_nurbspatchsurface.inc}
{$I auto_generated_node_helpers/x3dnodes_nurbspositioninterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_nurbsset.inc}
{$I auto_generated_node_helpers/x3dnodes_nurbssurfaceinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_nurbssweptsurface.inc}
{$I auto_generated_node_helpers/x3dnodes_nurbsswungsurface.inc}
{$I auto_generated_node_helpers/x3dnodes_nurbstexturecoordinate.inc}
{$I auto_generated_node_helpers/x3dnodes_nurbstrimmedsurface.inc}
{$I auto_generated_node_helpers/x3dnodes_orientationchaser.inc}
{$I auto_generated_node_helpers/x3dnodes_orientationdamper.inc}
{$I auto_generated_node_helpers/x3dnodes_orientationinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_orthoviewpoint.inc}
{$I auto_generated_node_helpers/x3dnodes_packagedshader.inc}
{$I auto_generated_node_helpers/x3dnodes_particlesystem.inc}
{$I auto_generated_node_helpers/x3dnodes_pickablegroup.inc}
{$I auto_generated_node_helpers/x3dnodes_pixeltexture.inc}
{$I auto_generated_node_helpers/x3dnodes_pixeltexture3d.inc}
{$I auto_generated_node_helpers/x3dnodes_planesensor.inc}
{$I auto_generated_node_helpers/x3dnodes_pointemitter.inc}
{$I auto_generated_node_helpers/x3dnodes_pointlight.inc}
{$I auto_generated_node_helpers/x3dnodes_pointpicksensor.inc}
{$I auto_generated_node_helpers/x3dnodes_pointset.inc}
{$I auto_generated_node_helpers/x3dnodes_polyline2d.inc}
{$I auto_generated_node_helpers/x3dnodes_polylineemitter.inc}
{$I auto_generated_node_helpers/x3dnodes_polypoint2d.inc}
{$I auto_generated_node_helpers/x3dnodes_positionchaser.inc}
{$I auto_generated_node_helpers/x3dnodes_positionchaser2d.inc}
{$I auto_generated_node_helpers/x3dnodes_positiondamper.inc}
{$I auto_generated_node_helpers/x3dnodes_positiondamper2d.inc}
{$I auto_generated_node_helpers/x3dnodes_positioninterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_positioninterpolator2d.inc}
{$I auto_generated_node_helpers/x3dnodes_primitivepicksensor.inc}
{$I auto_generated_node_helpers/x3dnodes_programshader.inc}
{$I auto_generated_node_helpers/x3dnodes_projectedtexturecoordinate.inc}
{$I auto_generated_node_helpers/x3dnodes_proximitysensor.inc}
{$I auto_generated_node_helpers/x3dnodes_quadset.inc}
{$I auto_generated_node_helpers/x3dnodes_receiverpdu.inc}
{$I auto_generated_node_helpers/x3dnodes_rectangle2d.inc}
{$I auto_generated_node_helpers/x3dnodes_renderedtexture.inc}
{$I auto_generated_node_helpers/x3dnodes_rigidbody.inc}
{$I auto_generated_node_helpers/x3dnodes_rigidbodycollection.inc}
{$I auto_generated_node_helpers/x3dnodes_scalarchaser.inc}
{$I auto_generated_node_helpers/x3dnodes_scalarinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_screeneffect.inc}
{$I auto_generated_node_helpers/x3dnodes_screenfontstyle.inc}
{$I auto_generated_node_helpers/x3dnodes_screengroup.inc}
{$I auto_generated_node_helpers/x3dnodes_script.inc}
{$I auto_generated_node_helpers/x3dnodes_shaderpart.inc}
{$I auto_generated_node_helpers/x3dnodes_shaderprogram.inc}
{$I auto_generated_node_helpers/x3dnodes_shadertexture.inc}
{$I auto_generated_node_helpers/x3dnodes_shape.inc}
{$I auto_generated_node_helpers/x3dnodes_signalpdu.inc}
{$I auto_generated_node_helpers/x3dnodes_singleaxishingejoint.inc}
{$I auto_generated_node_helpers/x3dnodes_sliderjoint.inc}
{$I auto_generated_node_helpers/x3dnodes_sound.inc}
{$I auto_generated_node_helpers/x3dnodes_sphere.inc}
{$I auto_generated_node_helpers/x3dnodes_spheresensor.inc}
{$I auto_generated_node_helpers/x3dnodes_splinepositioninterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_splinepositioninterpolator2d.inc}
{$I auto_generated_node_helpers/x3dnodes_splinescalarinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_spotlight.inc}
{$I auto_generated_node_helpers/x3dnodes_squadorientationinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_staticgroup.inc}
{$I auto_generated_node_helpers/x3dnodes_stringsensor.inc}
{$I auto_generated_node_helpers/x3dnodes_surfaceemitter.inc}
{$I auto_generated_node_helpers/x3dnodes_switch.inc}
{$I auto_generated_node_helpers/x3dnodes_teapot.inc}
{$I auto_generated_node_helpers/x3dnodes_texcoorddamper2d.inc}
{$I auto_generated_node_helpers/x3dnodes_text.inc}
{$I auto_generated_node_helpers/x3dnodes_text3d.inc}
{$I auto_generated_node_helpers/x3dnodes_texturebackground.inc}
{$I auto_generated_node_helpers/x3dnodes_texturecoordinate.inc}
{$I auto_generated_node_helpers/x3dnodes_texturecoordinate3d.inc}
{$I auto_generated_node_helpers/x3dnodes_texturecoordinate4d.inc}
{$I auto_generated_node_helpers/x3dnodes_texturecoordinategenerator.inc}
{$I auto_generated_node_helpers/x3dnodes_textureproperties.inc}
{$I auto_generated_node_helpers/x3dnodes_texturetransform.inc}
{$I auto_generated_node_helpers/x3dnodes_texturetransform3d.inc}
{$I auto_generated_node_helpers/x3dnodes_texturetransformmatrix3d.inc}
{$I auto_generated_node_helpers/x3dnodes_timesensor.inc}
{$I auto_generated_node_helpers/x3dnodes_timetrigger.inc}
{$I auto_generated_node_helpers/x3dnodes_toggler.inc}
{$I auto_generated_node_helpers/x3dnodes_touchsensor.inc}
{$I auto_generated_node_helpers/x3dnodes_transform.inc}
{$I auto_generated_node_helpers/x3dnodes_transformsensor.inc}
{$I auto_generated_node_helpers/x3dnodes_transmitterpdu.inc}
{$I auto_generated_node_helpers/x3dnodes_trianglefanset.inc}
{$I auto_generated_node_helpers/x3dnodes_triangleset.inc}
{$I auto_generated_node_helpers/x3dnodes_triangleset2d.inc}
{$I auto_generated_node_helpers/x3dnodes_trianglestripset.inc}
{$I auto_generated_node_helpers/x3dnodes_twosidedmaterial.inc}
{$I auto_generated_node_helpers/x3dnodes_universaljoint.inc}
{$I auto_generated_node_helpers/x3dnodes_vectorinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_viewpoint.inc}
{$I auto_generated_node_helpers/x3dnodes_viewpointgroup.inc}
{$I auto_generated_node_helpers/x3dnodes_viewpointmirror.inc}
{$I auto_generated_node_helpers/x3dnodes_viewport.inc}
{$I auto_generated_node_helpers/x3dnodes_visibilitysensor.inc}
{$I auto_generated_node_helpers/x3dnodes_volumeemitter.inc}
{$I auto_generated_node_helpers/x3dnodes_volumepicksensor.inc}
{$I auto_generated_node_helpers/x3dnodes_windphysicsmodel.inc}
{$I auto_generated_node_helpers/x3dnodes_worldinfo.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dappearancechildnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dappearancenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dbackgroundnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dbindablenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dchasernode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dchildnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dcolornode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dcomposedgeometrynode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dcoordinatenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3ddampernode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3ddragsensornode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3denvironmentalsensornode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3denvironmenttexturenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dfollowernode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dfontstylenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dgeometricpropertynode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dgeometrynode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dgroupingnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dinfonode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dinterpolatornode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dkeydevicesensornode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dlayernode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dlayoutnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dlightnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dmaterialnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dnbodycollidablenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dnbodycollisionspacenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dnetworksensornode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dnormalnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dnurbscontrolcurvenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dnurbssurfacegeometrynode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dparametricgeometrynode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dparticleemitternode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dparticlephysicsmodelnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dpicksensornode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dpointingdevicesensornode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dproductstructurechildnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3drigidjointnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dscriptnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dsensornode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dsequencernode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dshadernode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dshapenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dsoundnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dsoundsourcenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dtexture2dnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dtexture3dnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dtexturecoordinatenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dtexturenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dtexturetransformnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dtimedependentnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dtouchsensornode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dtriggernode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dvertexattributenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dviewpointnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dviewportnode.inc}

{ unit init/fini ------------------------------------------------------------ }

procedure X3DNodesFinalization;
begin
  FreeAndNil(VRML1DefaultState);
  FreeAndNil(TraverseSingleStack);
  TextureCache := nil;
  FreeAndNil(X3DCache);

  FreeAndNil(NodesManager);
  FreeAndNil(AnyNodeDestructionNotifications);

  FreeAndNil(CurrentlyLoading);
end;

initialization
  AnyNodeDestructionNotifications := TNodeDestructionNotificationList.Create;

  X3DFieldsManager.RegisterClasses([TSFNode, TMFNode]);

  NodesManager := TNodesManager.Create;

  RegistedInventorNodes;
  RegisterVRML1Nodes;
  RegisterVRML97HAnimNodes;
  RegisterVRML97NodesNurbs;
  RegisterKambiNodes;
  RegisterInstantRealityNodes;
  RegisterBitManagementNodes;

  { X3D components registration : }

  RegisterCoreNodes;
  RegisterTimeNodes;
  RegisterNetworkingNodes;
  RegisterGroupingNodes;
  RegisterRenderingNodes;
  RegisterShapeNodes;
  RegisterGeometry3DNodes;
  RegisterGeometry2DNodes;
  RegisterTextNodes;
  RegisterSoundNodes;
  RegisterLightingNodes;
  RegisterTexturingNodes;
  RegisterInterpolationNodes;
  RegisterInterpolationCubicBezierNodes;
  RegisterPointingDeviceSensorNodes;
  RegisterKeyDeviceSensorNodes;
  RegisterEnvironmentalSensorNodes;
  RegisterNavigationNodes;
  RegisterEnvironmentalEffectsNodes;
  RegisterGeospatialNodes;
  RegisterHAnimNodes;
  RegisterNURBSNodes;
  RegisterDISNodes;
  RegisterScriptingNodes;
  RegisterEventUtilitiesNodes;
  RegisterShadersNodes;
  RegisterCADGeometryNodes;
  RegisterTexturing3DNodes;
  RegisterCubeMapTexturingNodes;
  RegisterLayeringNodes;
  RegisterLayoutNodes;
  RegisterRigidBodyPhysicsNodes;
  RegisterPickingNodes;
  RegisterFollowersNodes;
  RegisterParticleSystemsNodes;

  X3DCache := TX3DNodesCache.Create;
  TextureCache := X3DCache;
  VRML1DefaultState := TVRML1State.Create;
  VRML1DefaultState.CreateNodes;
  TraverseSingleStack := TX3DGraphTraverseStateStack.Create;

  CurrentlyLoading := TCastleStringList.Create;
finalization
  { Because of various finalization order (some stuff may be owned
    e.g. by CastleWindow.Application, and freed at CastleWindow finalization,
    which may be done after X3DNodes finalization) we may defer
    finalization for later. }
  if (X3DCache = nil) or X3DCache.Empty then
    X3DNodesFinalization else
    X3DCache.OnEmpty := @X3DNodesFinalization;
end.
