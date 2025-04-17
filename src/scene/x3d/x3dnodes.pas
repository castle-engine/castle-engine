{
  Copyright 2002-2023 Michalis Kamburelis.

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
  of X3D (prototypes, routes and so on).)

  This is the central unit for X3D processing, as X3D file
  is basically just a graph of nodes. We represent whole X3D file
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

      When reading X3D files, we generally do not change the X3D graph.
      So we're able to save exactly the same X3D graph
      back to another file. See also
      [https://castle-engine.io/vrml_engine_doc/output/xsl/html/section.writing_vrml.html#section.vrml_preserving].
      This allows writing various X3D
      processing tools, that can simply read the file, change whatever
      they want, and write the file back --- knowing that the "untouched"
      parts of graph are preserved perfectly.)

    @item(TX3DNode class offers a lot of methods to process X3D graph.
      See TX3DNode.Traverse, TX3DNode.EnumerateNodes and
      TX3DNode.FindNode. TX3DNode.Traverse is especially important, as it
      walks through X3D graph just as the specification says
      (accumulating transformation, visiting only active children of
      nodes like Switch or LOD),
      gathering some state (useful especially for VRML 1.0, but also
      used for various things in new X3D versions).

      When you want to render X3D graph, you can just traverse
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

    @item(Your own units can define new X3D nodes, by declaring
      new classes descending from TX3DNode (or other, more specialized,
      descendant). You should register your new classes by calling
      @link(TNodesManager.RegisterNodeClasses NodesManager.RegisterNodeClasses).)
  )

  @bold(Node class names, and inheritance:)

  @unorderedList(
    @item(Normal X3D nodes are defined by classses
      named like @code(TXxxNode). These nodes can be specified inside the X3D
      files. See X3D specifications, and also our extensions specification,
      on [https://castle-engine.io/vrml_x3d.php].

      There are also abstract node classes. Their definitions are helpful
      for handling some functionality common to many descendants,
      and to declare allowed children in SFNode/MFNode fields.
      Abstract node classes are named like @code(TAbstractXxxNode).
      Some of the abstract nodes are also defined by X3D specification,
      and some of them are just our own inventions.

      Finally, there are some special-purpose node classes that play
      important role in our X3D organization.
      They are not abstract, but also their exact instances
      are not created under normal circumstances.
      These are named like @code(TX3DXxxNode), currently
      these are only: TX3DNode, TX3DRootNode, TX3DUnknownNode, TX3DPrototypeNode.

      All node classes descend from the base TX3DNode class.

      When X3D design needs multiple inheritance, we use TNodeFunctionality,
      to expose interface and have implementation that does "something",
      but is not tied to the inheritance. E.g. varios unrelated nodes may support
      TTransformFunctionality. We don't use interfaces for this -- this avoids
      COM interface design weirdness (and in Delphi we're limited to COM interfaces),
      and allows to share implemetation too.)

    @item(
      Optional suffix _1 or _2 at the node class name indicates that
      this is only for a specific X3D or VRML standard version.
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

{$ifdef CASTLE_STRICT_CLI}
  {$error When CASTLE_STRICT_CLI is defined, you cannot link to this unit.}
{$endif}

interface

uses SysUtils, Generics.Collections, Classes, XMLRead, DOM,
  CastleVectors, CastleRectangles, CastleTimeUtils, CastleFonts,
  CastleInternalX3DLexer, CastleUtils, CastleClassUtils,
  X3DFields, CastleBoxes, CastleImages, CastleColors, CastleCameras,
  CastleVideos, X3DTime, CastleTransform, CastleInternalMaterialProperties,
  CastleScript, CastleInternalX3DScript, CastleInternalOctree,
  CastleInternalCompositeImage,
  CastleTextureImages, CastleKeysMouse, CastleSoundEngine, CastleStringUtils,
  CastleTextureFontData, CastleRenderOptions, CastleProjection, CastleBehaviors;

{$define read_interface}

type
  {$I x3dnodes_initial_types.inc}
  {$I x3dnodes_vrml1state.inc}
  {$I x3dnodes_lightinstance.inc}
  {$I x3dnodes_clipplane.inc}
  {$I x3dnodes_x3dgraphtraversestate.inc}
  {$I x3dnodes_destructionnotification.inc}
  {$I x3dnodes_x3dnodescache.inc}
  {$I x3dnodes_x3dfonttexturescache.inc}
  {$I x3dnodes_x3dnode.inc}
  {$I x3dnodes_generatedtextures.inc}
  {$I x3dnodes_x3dnodeclasseslist.inc}
  {$I x3dnodes_sfnode.inc}
  {$I x3dnodes_mfnode.inc}
  {$I x3dnodes_utils_materials.inc}

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
  {$I x3dnodes_standard_h-anim.inc}
  {$I x3dnodes_standard_nurbs.inc}
  {$I x3dnodes_standard_scripting.inc}
  {$I x3dnodes_standard_eventutilities.inc}
  {$I x3dnodes_standard_shaders.inc}
  {$I x3dnodes_standard_cadgeometry.inc}
  {$I x3dnodes_standard_texturing3d.inc}
  {$I x3dnodes_standard_cubemaptexturing.inc}

  { More X3D nodes, not from X3D standard }
  {$I x3dnodes_1.inc}
  {$I x3dnodes_inventor.inc}
  {$I x3dnodes_97_hanim.inc}
  {$I x3dnodes_castle.inc}
  {$I x3dnodes_instantreality.inc}

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

uses Math, StrUtils, URIParser,
  CastleTextureFont_Default3d_Sans,
  X3DLoad, CastleInternalZStream, X3DCameraUtils,
  CastleFilesUtils, CastleUriUtils, CastleUnicode, CastleCurves,
  CastleLog, CastleScriptParser, CastleInternalDataUri, CastleDownload,
  CastleInternalNurbs, CastleQuaternions, CastleXmlUtils, CastleOpenDocument,
  CastleSoundBase, CastleTriangles, X3DLoadInternalUtils, CastleFileFilters,
  CastleApplicationProperties, CastleInternalNodesUnsupported;

{$define read_implementation}

{$I x3dnodes_miscellaneous_internals.inc}

{$I x3dnodes_initial_types.inc}
{$I x3dnodes_vrml1state.inc}
{$I x3dnodes_lightinstance.inc}
{$I x3dnodes_clipplane.inc}
{$I x3dnodes_x3dgraphtraversestate.inc}
{$I x3dnodes_destructionnotification.inc}
{$I x3dnodes_x3dnodescache.inc}
{$I x3dnodes_x3dfonttexturescache.inc}
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
{$I x3dnodes_utils_materials.inc}

// These must be included after x3dnodes_encoding_{classic,xml}.inc
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
{$I x3dnodes_standard_h-anim.inc}
{$I x3dnodes_standard_nurbs.inc}
{$I x3dnodes_standard_scripting.inc}
{$I x3dnodes_standard_eventutilities.inc}
{$I x3dnodes_standard_shaders.inc}
{$I x3dnodes_standard_cadgeometry.inc}
{$I x3dnodes_standard_texturing3d.inc}
{$I x3dnodes_standard_cubemaptexturing.inc}

{ More X3D nodes, not from X3D standard }
{$I x3dnodes_1.inc}
{$I x3dnodes_inventor.inc}
{$I x3dnodes_97_hanim.inc}
{$I x3dnodes_castle.inc}
{$I x3dnodes_instantreality.inc}

{ Auto-generated nodes code }
{$I auto_generated_node_helpers/x3dnodes_abstractvrml1camera_1.inc}
{$I auto_generated_node_helpers/x3dnodes_abstractvrml1geometry_1.inc}
{$I auto_generated_node_helpers/x3dnodes_abstractvrml1indexed_1.inc}
{$I auto_generated_node_helpers/x3dnodes_abstractvrml1separator_1.inc}
{$I auto_generated_node_helpers/x3dnodes_abstractvrml1transformation_1.inc}
{$I auto_generated_node_helpers/x3dnodes_anchor.inc}
{$I auto_generated_node_helpers/x3dnodes_appearance.inc}
{$I auto_generated_node_helpers/x3dnodes_arc2d.inc}
{$I auto_generated_node_helpers/x3dnodes_arcclose2d.inc}
{$I auto_generated_node_helpers/x3dnodes_asciitext_1.inc}
{$I auto_generated_node_helpers/x3dnodes_audioclip.inc}
{$I auto_generated_node_helpers/x3dnodes_background.inc}
{$I auto_generated_node_helpers/x3dnodes_blendmode.inc}
{$I auto_generated_node_helpers/x3dnodes_booleanfilter.inc}
{$I auto_generated_node_helpers/x3dnodes_booleansequencer.inc}
{$I auto_generated_node_helpers/x3dnodes_booleantoggle.inc}
{$I auto_generated_node_helpers/x3dnodes_booleantrigger.inc}
{$I auto_generated_node_helpers/x3dnodes_box.inc}
{$I auto_generated_node_helpers/x3dnodes_cadassembly.inc}
{$I auto_generated_node_helpers/x3dnodes_cadface.inc}
{$I auto_generated_node_helpers/x3dnodes_cadlayer.inc}
{$I auto_generated_node_helpers/x3dnodes_cadpart.inc}
{$I auto_generated_node_helpers/x3dnodes_circle2d.inc}
{$I auto_generated_node_helpers/x3dnodes_clipplane.inc}
{$I auto_generated_node_helpers/x3dnodes_color.inc}
{$I auto_generated_node_helpers/x3dnodes_colorinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_colorrgba.inc}
{$I auto_generated_node_helpers/x3dnodes_colorsetinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_commonsurfaceshader.inc}
{$I auto_generated_node_helpers/x3dnodes_composedcubemaptexture.inc}
{$I auto_generated_node_helpers/x3dnodes_composedshader.inc}
{$I auto_generated_node_helpers/x3dnodes_composedtexture3d.inc}
{$I auto_generated_node_helpers/x3dnodes_cone.inc}
{$I auto_generated_node_helpers/x3dnodes_cone_1.inc}
{$I auto_generated_node_helpers/x3dnodes_contour2d.inc}
{$I auto_generated_node_helpers/x3dnodes_contourpolyline2d.inc}
{$I auto_generated_node_helpers/x3dnodes_converter.inc}
{$I auto_generated_node_helpers/x3dnodes_coordinate.inc}
{$I auto_generated_node_helpers/x3dnodes_coordinate3_1.inc}
{$I auto_generated_node_helpers/x3dnodes_coordinatedouble.inc}
{$I auto_generated_node_helpers/x3dnodes_coordinateinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_coordinateinterpolator2d.inc}
{$I auto_generated_node_helpers/x3dnodes_cube_1.inc}
{$I auto_generated_node_helpers/x3dnodes_cubicbezier2dorientationinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_cubicbeziercoordinateinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_cubicbezierpositioninterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_cylinder.inc}
{$I auto_generated_node_helpers/x3dnodes_cylinder_1.inc}
{$I auto_generated_node_helpers/x3dnodes_cylindersensor.inc}
{$I auto_generated_node_helpers/x3dnodes_directionallight.inc}
{$I auto_generated_node_helpers/x3dnodes_directionallight_1.inc}
{$I auto_generated_node_helpers/x3dnodes_disk2d.inc}
{$I auto_generated_node_helpers/x3dnodes_easeineaseout.inc}
{$I auto_generated_node_helpers/x3dnodes_effect.inc}
{$I auto_generated_node_helpers/x3dnodes_effectpart.inc}
{$I auto_generated_node_helpers/x3dnodes_elevationgrid.inc}
{$I auto_generated_node_helpers/x3dnodes_environment.inc}
{$I auto_generated_node_helpers/x3dnodes_environmentlight.inc}
{$I auto_generated_node_helpers/x3dnodes_extrusion.inc}
{$I auto_generated_node_helpers/x3dnodes_fillproperties.inc}
{$I auto_generated_node_helpers/x3dnodes_floatvertexattribute.inc}
{$I auto_generated_node_helpers/x3dnodes_fog.inc}
{$I auto_generated_node_helpers/x3dnodes_fogcoordinate.inc}
{$I auto_generated_node_helpers/x3dnodes_fontstyle.inc}
{$I auto_generated_node_helpers/x3dnodes_generatedcubemaptexture.inc}
{$I auto_generated_node_helpers/x3dnodes_generatedshadowmap.inc}
{$I auto_generated_node_helpers/x3dnodes_group.inc}
{$I auto_generated_node_helpers/x3dnodes_group_1.inc}
{$I auto_generated_node_helpers/x3dnodes_hanimdisplacer.inc}
{$I auto_generated_node_helpers/x3dnodes_hanimhumanoid.inc}
{$I auto_generated_node_helpers/x3dnodes_hanimjoint.inc}
{$I auto_generated_node_helpers/x3dnodes_hanimmotion.inc}
{$I auto_generated_node_helpers/x3dnodes_hanimsegment.inc}
{$I auto_generated_node_helpers/x3dnodes_hanimsite.inc}
{$I auto_generated_node_helpers/x3dnodes_imagebackground.inc}
{$I auto_generated_node_helpers/x3dnodes_imagecubemaptexture.inc}
{$I auto_generated_node_helpers/x3dnodes_imagetexture.inc}
{$I auto_generated_node_helpers/x3dnodes_imagetexture3d.inc}
{$I auto_generated_node_helpers/x3dnodes_indexedfaceset.inc}
{$I auto_generated_node_helpers/x3dnodes_indexedfaceset_1.inc}
{$I auto_generated_node_helpers/x3dnodes_indexedlineset.inc}
{$I auto_generated_node_helpers/x3dnodes_indexedlineset_1.inc}
{$I auto_generated_node_helpers/x3dnodes_indexedquadset.inc}
{$I auto_generated_node_helpers/x3dnodes_indexedtrianglefanset.inc}
{$I auto_generated_node_helpers/x3dnodes_indexedtrianglemesh_1.inc}
{$I auto_generated_node_helpers/x3dnodes_indexedtriangleset.inc}
{$I auto_generated_node_helpers/x3dnodes_indexedtrianglestripset.inc}
{$I auto_generated_node_helpers/x3dnodes_info_1.inc}
{$I auto_generated_node_helpers/x3dnodes_inline.inc}
{$I auto_generated_node_helpers/x3dnodes_inlineloadcontrol.inc}
{$I auto_generated_node_helpers/x3dnodes_integersequencer.inc}
{$I auto_generated_node_helpers/x3dnodes_integertrigger.inc}
{$I auto_generated_node_helpers/x3dnodes_kambiappearance.inc}
{$I auto_generated_node_helpers/x3dnodes_kambiinline.inc}
{$I auto_generated_node_helpers/x3dnodes_kambinavigationinfo.inc}
{$I auto_generated_node_helpers/x3dnodes_keysensor.inc}
{$I auto_generated_node_helpers/x3dnodes_lineproperties.inc}
{$I auto_generated_node_helpers/x3dnodes_lineset.inc}
{$I auto_generated_node_helpers/x3dnodes_loadsensor.inc}
{$I auto_generated_node_helpers/x3dnodes_localfog.inc}
{$I auto_generated_node_helpers/x3dnodes_lod_1.inc}
{$I auto_generated_node_helpers/x3dnodes_logger.inc}
{$I auto_generated_node_helpers/x3dnodes_material.inc}
{$I auto_generated_node_helpers/x3dnodes_material_1.inc}
{$I auto_generated_node_helpers/x3dnodes_matrix3vertexattribute.inc}
{$I auto_generated_node_helpers/x3dnodes_matrix4vertexattribute.inc}
{$I auto_generated_node_helpers/x3dnodes_matrixtransform.inc}
{$I auto_generated_node_helpers/x3dnodes_matrixtransform_1.inc}
{$I auto_generated_node_helpers/x3dnodes_metadataboolean.inc}
{$I auto_generated_node_helpers/x3dnodes_metadatadouble.inc}
{$I auto_generated_node_helpers/x3dnodes_metadatafloat.inc}
{$I auto_generated_node_helpers/x3dnodes_metadatainteger.inc}
{$I auto_generated_node_helpers/x3dnodes_metadataset.inc}
{$I auto_generated_node_helpers/x3dnodes_metadatastring.inc}
{$I auto_generated_node_helpers/x3dnodes_movietexture.inc}
{$I auto_generated_node_helpers/x3dnodes_multigeneratedtexturecoordinate.inc}
{$I auto_generated_node_helpers/x3dnodes_multitexture.inc}
{$I auto_generated_node_helpers/x3dnodes_multitexturecoordinate.inc}
{$I auto_generated_node_helpers/x3dnodes_multitexturetransform.inc}
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
{$I auto_generated_node_helpers/x3dnodes_orientationinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_orientationinterpolator2d.inc}
{$I auto_generated_node_helpers/x3dnodes_orthographiccamera_1.inc}
{$I auto_generated_node_helpers/x3dnodes_packagedshader.inc}
{$I auto_generated_node_helpers/x3dnodes_perspectivecamera_1.inc}
{$I auto_generated_node_helpers/x3dnodes_physicalmaterial.inc}
{$I auto_generated_node_helpers/x3dnodes_pixeltexture.inc}
{$I auto_generated_node_helpers/x3dnodes_pixeltexture3d.inc}
{$I auto_generated_node_helpers/x3dnodes_plane.inc}
{$I auto_generated_node_helpers/x3dnodes_planesensor.inc}
{$I auto_generated_node_helpers/x3dnodes_pointlight.inc}
{$I auto_generated_node_helpers/x3dnodes_pointlight_1.inc}
{$I auto_generated_node_helpers/x3dnodes_pointset.inc}
{$I auto_generated_node_helpers/x3dnodes_pointset_1.inc}
{$I auto_generated_node_helpers/x3dnodes_polyline2d.inc}
{$I auto_generated_node_helpers/x3dnodes_polypoint2d.inc}
{$I auto_generated_node_helpers/x3dnodes_positioninterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_positioninterpolator2d.inc}
{$I auto_generated_node_helpers/x3dnodes_programshader.inc}
{$I auto_generated_node_helpers/x3dnodes_projectedtexturecoordinate.inc}
{$I auto_generated_node_helpers/x3dnodes_proximitysensor.inc}
{$I auto_generated_node_helpers/x3dnodes_quadset.inc}
{$I auto_generated_node_helpers/x3dnodes_rectangle2d.inc}
{$I auto_generated_node_helpers/x3dnodes_renderedtexture.inc}
{$I auto_generated_node_helpers/x3dnodes_rotation_1.inc}
{$I auto_generated_node_helpers/x3dnodes_rotationxyz_1.inc}
{$I auto_generated_node_helpers/x3dnodes_scalarinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_scale_1.inc}
{$I auto_generated_node_helpers/x3dnodes_screeneffect.inc}
{$I auto_generated_node_helpers/x3dnodes_script.inc}
{$I auto_generated_node_helpers/x3dnodes_separator_1.inc}
{$I auto_generated_node_helpers/x3dnodes_shaderpart.inc}
{$I auto_generated_node_helpers/x3dnodes_shaderprogram.inc}
{$I auto_generated_node_helpers/x3dnodes_shadertexture.inc}
{$I auto_generated_node_helpers/x3dnodes_shape.inc}
{$I auto_generated_node_helpers/x3dnodes_sound.inc}
{$I auto_generated_node_helpers/x3dnodes_sphere.inc}
{$I auto_generated_node_helpers/x3dnodes_sphere_1.inc}
{$I auto_generated_node_helpers/x3dnodes_spheresensor.inc}
{$I auto_generated_node_helpers/x3dnodes_splinepositioninterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_splinepositioninterpolator2d.inc}
{$I auto_generated_node_helpers/x3dnodes_splinescalarinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_spotlight.inc}
{$I auto_generated_node_helpers/x3dnodes_spotlight_1.inc}
{$I auto_generated_node_helpers/x3dnodes_squadorientationinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_staticgroup.inc}
{$I auto_generated_node_helpers/x3dnodes_stringsensor.inc}
{$I auto_generated_node_helpers/x3dnodes_switch.inc}
{$I auto_generated_node_helpers/x3dnodes_switch_1.inc}
{$I auto_generated_node_helpers/x3dnodes_tangent.inc}
{$I auto_generated_node_helpers/x3dnodes_teapot.inc}
{$I auto_generated_node_helpers/x3dnodes_text.inc}
{$I auto_generated_node_helpers/x3dnodes_text3d.inc}
{$I auto_generated_node_helpers/x3dnodes_texture2_1.inc}
{$I auto_generated_node_helpers/x3dnodes_texture2transform_1.inc}
{$I auto_generated_node_helpers/x3dnodes_texturebackground.inc}
{$I auto_generated_node_helpers/x3dnodes_texturecoordinate.inc}
{$I auto_generated_node_helpers/x3dnodes_texturecoordinate2_1.inc}
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
{$I auto_generated_node_helpers/x3dnodes_transform_1.inc}
{$I auto_generated_node_helpers/x3dnodes_transformsensor.inc}
{$I auto_generated_node_helpers/x3dnodes_transformseparator_1.inc}
{$I auto_generated_node_helpers/x3dnodes_translation_1.inc}
{$I auto_generated_node_helpers/x3dnodes_trianglefanset.inc}
{$I auto_generated_node_helpers/x3dnodes_triangleset.inc}
{$I auto_generated_node_helpers/x3dnodes_triangleset2d.inc}
{$I auto_generated_node_helpers/x3dnodes_trianglestripset.inc}
{$I auto_generated_node_helpers/x3dnodes_twosidedmaterial.inc}
{$I auto_generated_node_helpers/x3dnodes_unlitmaterial.inc}
{$I auto_generated_node_helpers/x3dnodes_valuetrigger.inc}
{$I auto_generated_node_helpers/x3dnodes_vectorinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_viewpointmirror.inc}
{$I auto_generated_node_helpers/x3dnodes_visibilitysensor.inc}
{$I auto_generated_node_helpers/x3dnodes_worldinfo.inc}
{$I auto_generated_node_helpers/x3dnodes_wwwanchor_1.inc}
{$I auto_generated_node_helpers/x3dnodes_wwwinline_1.inc}
{$I auto_generated_node_helpers/x3dnodes_x3d3dbackgroundnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dappearancechildnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dappearancenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dbackgroundnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dbindablenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dchildnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dcolornode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dcomposedgeometrynode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dcoordinatenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dcubicbezierinterpolator.inc}
{$I auto_generated_node_helpers/x3dnodes_x3ddirectionallightnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3ddragsensornode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3denvironmentalsensornode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3denvironmenttexturenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dfontstylenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dgeometricpropertynode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dgeometrynode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dgroupingnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dinfonode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dinterpolatornode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dkeydevicesensornode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dlightnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dmaterialnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dmetadatanode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dnetworksensornode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dnormalnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dnurbscontrolcurvenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dnurbssurfacegeometrynode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3donesidedmaterialnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dparametricgeometrynode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dpointingdevicesensornode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dpointlightnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dpositionallightnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dproductstructurechildnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dpunctuallightnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dscriptnode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dsensornode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dsequencernode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dshadernode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dshapenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dsingletexturecoordinatenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dsingletexturenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dsingletexturetransformnode.inc}
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

{$warnings off} // uses deprecated constants like FSFAMILY_SERIF, we don't really plan to update it -- whole VRML 1 is deprecated
{$I auto_generated_node_helpers/x3dnodes_fontstyle_1.inc}
{$I auto_generated_node_helpers/x3dnodes_materialbinding_1.inc}
{$I auto_generated_node_helpers/x3dnodes_normalbinding_1.inc}
{$I auto_generated_node_helpers/x3dnodes_shapehints_1.inc}
{$warnings on}

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

  FontsFinalization;
end;

procedure RegisterVrmlX3dModelFormat;
var
  ModelFormat: TModelFormat;
begin
  ModelFormat := TModelFormat.Create;
  ModelFormat.OnLoad := {$ifdef FPC}@{$endif} LoadX3DClassicInternal;
  { We don't advertise to users we can save Inventor, although we're close
    to being able to do it, since we save VRML 1.0.
    But it's not something we want to maintain, Inventor is really ancient. }
  //ModelFormat.OnSave := {$ifdef FPC}@{$endif} SaveX3DClassic;
  ModelFormat.MimeTypes.Add('application/x-inventor');
  ModelFormat.FileFilterName := 'Inventor (*.iv)';
  ModelFormat.Extensions.Add('.iv');
  RegisterModelFormat(ModelFormat);

  ModelFormat := TModelFormat.Create;
  ModelFormat.OnLoad := {$ifdef FPC}@{$endif} LoadX3DClassicInternal;
  ModelFormat.OnSave := {$ifdef FPC}@{$endif} SaveX3DClassic;
  ModelFormat.MimeTypes.Add('model/vrml');
  ModelFormat.FileFilterName := 'VRML (*.wrl, *.wrl.gz, *.wrz)';
  ModelFormat.Extensions.Add('.wrl');
  ModelFormat.Extensions.Add('.wrl.gz');
  ModelFormat.Extensions.Add('.wrz');
  RegisterModelFormat(ModelFormat);

  ModelFormat := TModelFormat.Create;
  ModelFormat.OnLoad := {$ifdef FPC}@{$endif} LoadX3DClassicInternal;
  ModelFormat.OnSave := {$ifdef FPC}@{$endif} SaveX3DClassicForceX3D;
  ModelFormat.MimeTypes.Add('model/x3d+vrml');
  ModelFormat.FileFilterName := 'X3D classic (*.x3dv, *.x3dvz, *.x3dv.gz)';
  ModelFormat.Extensions.Add('.x3dv');
  ModelFormat.Extensions.Add('.x3dvz');
  ModelFormat.Extensions.Add('.x3dv.gz');
  RegisterModelFormat(ModelFormat);

  ModelFormat := TModelFormat.Create;
  ModelFormat.OnLoad := {$ifdef FPC}@{$endif} LoadX3DXmlInternal;
  ModelFormat.OnSave := {$ifdef FPC}@{$endif} SaveX3DXml;
  ModelFormat.MimeTypes.Add('model/x3d+xml');
  ModelFormat.FileFilterName := 'X3D XML (*.x3d, *.x3dz, *.x3d.gz)';
  ModelFormat.Extensions.Add('.x3d');
  ModelFormat.Extensions.Add('.x3dz');
  ModelFormat.Extensions.Add('.x3d.gz');
  { Testcases: Run on Android and play game:
      https://github.com/castle-engine/little-things
      https://github.com/castle-engine/darkest-before-dawn

    Opening a xxx.x3d.gz file on Android with OnLoadForceMemoryStream=false
    would fail, with "Illegal at document level" error (from FPC XML parsing units),
    testing with FPC 3.2.2 on Android/Arm .
    On Android, in this case we read file through Android assets and then through
    TGZFileStream (even though .gz file is no longer compressed,
    packing into APK actually decompressed it; still reading through TGZFileStream
    is OK, and tests confirm that TGZFileStream returns correct, valid XML contents).

    We assume it works on desktop with OnLoadForceMemoryStream=false
    only because on desktop, TFileStream (returned for files by default)
    is already seekable. }
  ModelFormat.OnLoadForceMemoryStream := true;
  RegisterModelFormat(ModelFormat);
end;

initialization
  AnyNodeDestructionNotifications := TNodeDestructionNotificationList.Create;

  X3DFieldsManager.RegisterClasses([TSFNode, TMFNode]);

  NodesManager := TNodesManager.Create;

  RegistedInventorNodes;
  RegisterVRML1Nodes;
  RegisterVRML97HAnimNodes;
  RegisterKambiNodes;
  RegisterInstantRealityNodes;

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
  RegisterHAnimNodes;
  RegisterNURBSNodes;
  RegisterScriptingNodes;
  RegisterEventUtilitiesNodes;
  RegisterShadersNodes;
  RegisterCADGeometryNodes;
  RegisterTexturing3DNodes;
  RegisterCubeMapTexturingNodes;

  RegisterUnsupportedNodes;

  X3DCache := TX3DFontTexturesCache.Create;
  TextureCache := X3DCache;
  VRML1DefaultState := TVRML1State.Create;
  VRML1DefaultState.CreateNodes;
  TraverseSingleStack := TX3DGraphTraverseStateStack.Create;

  CurrentlyLoading := TCastleStringList.Create;

  FontsInitialization;

  RegisterVrmlX3dModelFormat;
finalization
  { Because of various finalization order (some stuff may be owned
    e.g. by CastleWindow.Application, and freed at CastleWindow finalization,
    which may be done after X3DNodes finalization) we may defer
    finalization for later. }
  if (X3DCache = nil) or X3DCache.Empty then
    X3DNodesFinalization
  else
    X3DCache.OnEmpty := @X3DNodesFinalization;
end.

