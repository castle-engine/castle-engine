{%MainUnit x3dnodes.pas}
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

{$ifdef read_interface}

const
  DefaultMaterial_1AmbientColor: TVector3 = (X: 0.2; Y: 0.2; Z: 0.2) deprecated 'use TPhongMaterialInfo.DefaultAmbientColor';
  DefaultMaterialAmbientIntensity = 0.2 deprecated 'use TPhongMaterialInfo.DefaultAmbientIntensity';
  DefaultMaterialDiffuseColor: TVector3 = (X: 0.8; Y: 0.8; Z: 0.8) deprecated 'use TPhongMaterialInfo.DefaultDiffuseColor';
  DefaultMaterialSpecularColor: TVector3 = (X: 0; Y: 0; Z: 0) deprecated 'use TPhongMaterialInfo.DefaultSpecularColor';
  DefaultMaterialEmissiveColor: TVector3 = (X: 0; Y: 0; Z: 0) deprecated 'use TPhongMaterialInfo.DefaultEmissiveColor';
  DefaultMaterialShininess = 0.2 deprecated 'use TPhongMaterialInfo.DefaultShininess';
  DefaultMaterialTransparency = 0.0 deprecated 'use TPhongMaterialInfo.DefaultTransparency';
  DefaultMaterialMirror = 0.0 deprecated 'use TPhongMaterialInfo.DefaultMirror';
  DefaultMaterialReflSpecularExp = 1000000 deprecated 'use TPhongMaterialInfo.DefaultReflSpecularExp';
  DefaultMaterialTransSpecularExp = 1000000 deprecated 'use TPhongMaterialInfo.DefaultTransSpecularExp';

  { Default value of TAppearanceNode.AlphaCutoff. }
  DefaultAlphaCutoff = 0.5;

  { Constants for
    @link(TMaterialBindingNode_1.FdValue).Value and
    @link(TNormalBindingNode_1.FdValue).Value.
    @groupBegin }
  BIND_DEFAULT = 0 deprecated 'use X3D instead of VRML 1 nodes';
  BIND_OVERALL = 1 deprecated 'use X3D instead of VRML 1 nodes';
  BIND_PER_PART = 2 deprecated 'use X3D instead of VRML 1 nodes';
  BIND_PER_PART_INDEXED = 3 deprecated 'use X3D instead of VRML 1 nodes';
  BIND_PER_FACE = 4 deprecated 'use X3D instead of VRML 1 nodes';
  BIND_PER_FACE_INDEXED = 5 deprecated 'use X3D instead of VRML 1 nodes';
  BIND_PER_VERTEX = 6 deprecated 'use X3D instead of VRML 1 nodes';
  BIND_PER_VERTEX_INDEXED = 7 deprecated 'use X3D instead of VRML 1 nodes';
  { @groupEnd }

  { Constants for @link(TShapeHintsNode_1.FdVertexOrdering).Value.
    @groupBegin }
  VERTORDER_UNKNOWN = 0 deprecated 'use X3D instead of VRML 1 nodes';
  VERTORDER_CLOCKWISE = 1 deprecated 'use X3D instead of VRML 1 nodes';
  VERTORDER_COUNTERCLOCKWISE = 2 deprecated 'use X3D instead of VRML 1 nodes';
  { @groupEnd }

  { Constants for @link(TShapeHintsNode_1.FdShapeType).Value.
    @groupBegin }
  SHTYPE_UNKNOWN = 0 deprecated 'use X3D instead of VRML 1 nodes';
  SHTYPE_SOLID = 1 deprecated 'use X3D instead of VRML 1 nodes';
  { @groupEnd }

  { Constants for @link(TShapeHintsNode_1.FdFaceType).Value.
    @groupBegin }
  FACETYPE_UNKNOWN = 0 deprecated 'use X3D instead of VRML 1 nodes';
  FACETYPE_CONVEX = 1 deprecated 'use X3D instead of VRML 1 nodes';
  { @groupEnd }

  { Constants for @link(TFontStyleNode.FdFamily).Value.
    @groupBegin }
  FSFAMILY_SERIF = 0 deprecated 'use ffSerif (TX3DFontFamily an enumerated type) with the properties like TFontStyleNode.Family';
  FSFAMILY_SANS = 1 deprecated 'use ffSans (TX3DFontFamily an enumerated type) with the properties like TFontStyleNode.Family';
  FSFAMILY_TYPEWRITER = 2 deprecated 'use ffTypeWriter (TX3DFontFamily an enumerated type) with the properties like TFontStyleNode.Family';
  { @groupEnd }

  { Constants for VRML 1.0 @link(TFontStyleNode_1.FdStyle) flags.
    @groupBegin }
  FSSTYLE_BOLD = 0 deprecated 'use TFontStyleNode.Bold as a simple boolean';
  FSSTYLE_ITALIC = 1 deprecated 'use TFontStyleNode.Italic as a simple boolean';
  { @groupEnd }

  { Constants for TConeNode.FdParts.Flags.
    @groupBegin }
  CONE_PARTS_SIDES = 0 deprecated 'use X3D instead of VRML 1 nodes';
  CONE_PARTS_BOTTOM = 1 deprecated 'use X3D instead of VRML 1 nodes';
  { @groupEnd }

  { Constants for TCylinderNode.FdParts.Flags.
    @groupBegin }
  CYLINDER_PARTS_SIDES = 0 deprecated 'use X3D instead of VRML 1 nodes';
  CYLINDER_PARTS_TOP = 1 deprecated 'use X3D instead of VRML 1 nodes';
  CYLINDER_PARTS_BOTTOM = 2 deprecated 'use X3D instead of VRML 1 nodes';
  { @groupEnd }

  DefaultHeightMapScale = 0.01;
  DefaultVRML1CreaseAngle = 0.5;

  DefaultViewpointFieldOfView = TCastlePerspective.DefaultFieldOfView;
  DefaultNavigationInfoHeadlight = true;

  DefaultRenderedTextureWidth  = 128;
  DefaultRenderedTextureHeight = 128;

  VRML1Version: TX3DVersion = (Major: 1; Minor: 0);
  VRML2Version: TX3DVersion = (Major: 2; Minor: 0);
  { Latest X3D version supported. }
  X3DVersion: TX3DVersion = (Major: 4; Minor: 0);

  xeClassic = CastleInternalX3DLexer.xeClassic;
  xeXML = CastleInternalX3DLexer.xeXML;

  { Minimal values for
    @link(DefaultTriangulationSlices),
    @link(DefaultTriangulationStacks).

    Note that MinTriangulationSlices can be lower (2), it works,
    but the result isn't really sensible.
    @groupBegin }
  MinTriangulationSlices: Cardinal = 3;
  MinTriangulationStacks: Cardinal = 1;
  { @groupEnd }

  { Classes corresponding to nodes on TVRML1State. }
  VRML1StateClasses: array [TVRML1StateNode] of TX3DNodeClass = (
    TCoordinate3Node_1,
    TShapeHintsNode_1,
    TFontStyleNode_1,
    TMaterialNode_1,
    TMaterialBindingNode_1,
    TNormalNode,
    TNormalBindingNode_1,
    TTexture2Node_1,
    TTextureCoordinate2Node_1
    { additions here must be synchronized with additions to TVRML1State }
  ) deprecated 'use X3D instead of VRML 1 nodes';

  { Convert Boolean meaning "repeat" into TBoundaryMode.
    Useful to convert e.g. TImageTextureNode.RepeatS value
    into TTexturePropertiesNode.BoundaryModeS value. }
  BoolRepeatToBoundaryMode: array [Boolean] of TBoundaryMode = (
    bmClampToEdge,
    bmRepeat
  );

  ftExp = ftExponential deprecated 'use ftExponential';

var
  { How much to subdivide various objects (like sphere, cylinder) into triangles.

    "Slices" divide the circumference of the circle, like the slices of a pizza.
    Must be always >= @link(MinTriangulationSlices).

    See @url(https://castle-engine.io/x3d_implementation_geometry3d_extensions.php#section_triangulation
    more about triangulation). }
  DefaultTriangulationSlices: Cardinal = 30;

  { How much to subdivide various objects (like sphere, cylinder) into triangles.

    "Stacks" divide the height of the object, like the stacks of a cake or tower.
    Must be always >= @link(MinTriangulationStacks).

    See @url(https://castle-engine.io/x3d_implementation_geometry3d_extensions.php#section_triangulation
    more about triangulation). }
  DefaultTriangulationStacks: Cardinal = 20;

const
  { Standard prefix name for a time sensor to be recognized as a "named animation"
    for TCastleSceneCore.PlayAnimation and friends. }
  DefaultAnimationPrefix = '';

  AllAccessTypes = [atInputOnly, atOutputOnly, atInitializeOnly, atInputOutput];
  RestrictedAccessTypes = [atInputOnly, atOutputOnly, atInitializeOnly];

var
  { Functions registered here will be called when any TX3DNode descendant
    will be destroyed. }
  AnyNodeDestructionNotifications: TNodeDestructionNotificationList;

  { Cache, for all the resources not tied with renderer context. }
  X3DCache: TX3DFontTexturesCache;

  { Should we emit a warning when loading data from
    an URI with an absolute filename, like @code(file:///c:/blah/myimage.png).
    The warning is emitted using WritelnWarning.
    This is quite useful, as usually you want to avoid using such URIs,
    as they will probably not work on any other system than your own
    (and they prevent you from easily moving the files to some other system/location).

    In your data (X3D, VRML, Collada, OBJ materials...),
    you should always use relative paths. }
  WarnAboutAbsoluteFilenames: boolean = true;

  (*Whether to resolve relative URLs within X3D prototypes with respect
    to the file that instantiated the prototype (false, default, compatible with Instant Player)
    or to the file that defined the prototype (true, compatible with BS Contact).

    This is unfortunately ambiguous in the X3D specification, and various
    other X3D browsers behave differently.

    Consider you have an X3D file like this:

    @preformatted(
    EXTENRPROTO MyObject [ field MFString urlTexture ] "textures/library.wrl#MyObject"
    MyObject { urlTexture "textures/myfile.png" }
    MyObject { urlTexture "myfile.png" }
    )

    And next to this file, in a subfolder, you have these files:

    @preformatted(
    textures/library.wrl
    textures/myfile.png
    )

    Which declaration of @code(MyObject) is correct?

    @unorderedList(
      @item(When this is false, you should write @code(MyObject { urlTexture "myfile.png" }).
        The URL is resolved with respect to the file that contains
        the ImageTexture node, which is "textures/library.wrl".)

      @item(When this is true, you should write @code(MyObject { urlTexture "textures/myfile.png" }).
        The URL is resolved with respect to the file that instantiates
        the prototype.

        Note that our implementation of this case is a bit incomplete.
        It works for most nodes (ImageTexture / AudioClip / ShaderPart / ...),
        but it does not work for the Inline node,
        and for URLs in recursive EXTERNPROTO.
        So URLs in Inline nodes, or in EXTERNPROTO, are always resolved
        like RebaseRelativeUrlsInPrototypes = false.
      )
    )

    See the X3D specification
    http://www.web3d.org/documents/specifications/19775-1/V3.3/Part01/components/networking.html#RelativeURLs
    (unfortunately, it does not clarify this case definitively).
    See our testcase in
    https://github.com/castle-engine/demo-models/tree/master/prototypes/relative_urls_test
  *)
  RebaseRelativeUrlsInPrototypes: boolean = false;

{ Free all unused X3D nodes on the list, then free and @nil the list itself.
  Ignores @nil items within the list. }
procedure X3DNodeList_FreeUnusedAndNil(var List: TX3DNodeList);

{ Find a range within "key" field corresponding to given Fraction.
  Returns the index of @bold(right) range delimiter.
  So for normal ranges (between two values of "key" field) it's
  always between 1 and Key.Count - 1. Result 0 indicates we're
  before the left limit, and result equal Key.Count indicates
  we're after right limit.

  Result is always between 0 and Key.Count.

  Output T is the value between 0..1 indicating where within
  the range we are. It's undefined when Result is 0 or Key.Count
  (indicating we're outside limits).

  Call this only when Key.Count > 0.

  This is useful to interpreting TAbstractInterpolatorNode.KeyRange
  and such fields. }
function KeyRange(Key: TSingleList;
  const Fraction: Single; out T: Single): Integer;

{ Free TX3DNode if it is unused (see TX3DNode.FreeIfUnused),
  setting reference to @nil. Analogous to standard FreeAndNil,
  but checks if node is used first. }
procedure FreeIfUnusedAndNil(var Obj);

{$endif read_interface}

{$ifdef read_implementation}

procedure X3DNodeList_FreeUnusedAndNil(var List: TX3DNodeList);
var
  I: Integer;
begin
  if List <> nil then
  begin
    for I := 0 to List.Count - 1 do
      if (List.Items[I] <> nil) and
         (List.Items[I].VRML1ParentsCount + List.Items[I].ParentFieldsCount = 0) then
        List.Items[I].Free;
    FreeAndNil(List);
  end;
end;

function KeyRange(Key: TSingleList;
  const Fraction: Single; out T: Single): Integer;
var
  A, B: Integer;
begin
  Assert(Key.Count > 0);
  Assert(not IsNan(Fraction));

  if Fraction <= Key.First then
    Result := 0 else
  if Fraction >= Key.Last then
    Result := Key.Count else
  begin
    { Then for sure we're between two Key values.
      Note that we know that Key.Count > 1 (otherwise, Key.First = Key.Last
      so one of <= or >= comparisons above would occur; we check
      IsNan(Fraction) at the beginning to eliminate Fraction=NaN case). }
    Assert(Key.Count > 1);

    { Always A < B.
      We're considering range from Key[A] to Key[B].

      Remember that we cannot actually depend on the fact that
      Key values are non-decreasing. They should be non-decreasing,
      and we have to output correct result only when they are non-decreasing,
      but we also have to terminate (with any result) in any situation.
      Reason: Key values are supplied in X3D file, so they may be broken
      in every possible way. }

    A := 0;
    B := Key.Count - 1;
    while B - A > 1 do
    begin
      Result := (A + B) div 2;
      { A < B => (A + B) < 2B => (A + B) div 2 < B => Result < B.
        Also, Result > A (the only way how Result could be = A
        would be when B = A + 1, but we eliminated this case by "while"
        condition".

        This is good, it means A < Result < B, so Result is good candidate
        for next A or B, it will for sure shorten the distance
        between A and B. }
      Assert(A < Result);
      Assert(Result < B);
      { Using "<" instead of "<=" below: when the Fraction = Key[Result],
        we want to go to right part, not left. See
        - X3D specification
          http://www.web3d.org/documents/specifications/19775-1/V3.3/Part01/components/utils.html#SequencingEvents
          It says that f(t) = vn, if tn <= t < tn-1
        - Testcase on https://github.com/castle-engine/view3dscene/issues/2
      }
      if Fraction < Key[Result] then
        B := Result else
        A := Result;
    end;
    Result := B;

    if Key[B] > Key[A] then
      T := (Fraction - Key[A]) / (Key[B] - Key[A])
    else
      T := 0;
  end;
end;

procedure FreeIfUnusedAndNil(var Obj);
var
  Temp: TX3DNode;
begin
  Temp := TX3DNode(Obj);
  Pointer(Obj) := nil;
  Temp.FreeIfUnused;
end;

{$endif read_implementation}
