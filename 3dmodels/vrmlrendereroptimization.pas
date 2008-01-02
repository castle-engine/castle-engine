{ TGLRendererOptimization type.

  While this is needed by TVRMLFlatSceneGL, it has to be defined in this unit,
  in non-opengl units category, because it's also used by Object3dAsVRML
  and VRML animations reader. }
unit VRMLRendererOptimization;

interface

type
  { This is used by @link(TVRMLFlatSceneGL.Optimization) to describe
    what kind of optimization should be done. }
  TGLRendererOptimization = (
    { No optimization. No OpenGL display lists are constructed.
      So calling PrepareRender and ChangedAll is very fast.
      On the other hand, rendering is significantly slower,
      as display lists often help a lot.

      Use this if you plan to change the scene at runtime a lot
      (or when you're going to render TVRMLFlatSceneGL object
      very few times, say, only 1-2 frames, in some special situations)
      Then building display lists would be only a waste of time,
      since they would have to be rebuild very often. }
    roNone,

    { Treat the scene as a one big static object.
      One OpenGL display list is used, that renders the whole object.
      This is great optimization if the scene is static (otherwise
      rebuilding display lists too often would cost too much time)
      and you're sure that user will usually see the whole scene
      (or just a large part of it).

      Note that this nullifies the purpose of
      @link(TVRMLFlatSceneGL.RenderFrustum) and
      @link(TVRMLFlatSceneGL.RenderFrustumOctree) methods. And the purpose
      of the @link(TVRMLFlatSceneGL.Render) method with a parameter <> @nil.
      That's because the scene will always be rendered fully to OpenGL.

      If the scene is static but user usually only looks at some small
      part of it, then building octree for the scene and using
      roSeparateShapeStates and @link(TVRMLFlatSceneGL.RenderFrustumOctree)
      may be better. }
    roSceneAsAWhole,

    { Build separate OpenGL display list for each @link(TVRMLShapeState)
      on list @link(TVRMLFlatScene.ShapeStates). Use this if

      @orderedList(
        @item(you will change from time to time only some small parts of
          the scene (since this will allow to rebuild, on changing, only
          some small display lists, as opposed to roSceneAsAWhole,
          that has to rebuild large display list even if the change
          is very local).)

        @item(and/or you know that usually user will not see the whole scene,
          only a small part of it.
          See TestShapeStateVisibility parameter of @link(TVRMLFlatSceneGL.Render)
          and @link(TVRMLFlatSceneGL.RenderFrustum) and
          @link(TVRMLFlatSceneGL.RenderFrustumOctree).)

        @item(
          Another advantage of roSeparateShapeStates is when you use
          TVRMLGLAnimation. If part of your animation is actually still
          (i.e. the same ShapeState is used, the same nodes inside),
          and only the part changes --- then the still ShapeStates will
          use and share only one display list (only one ---
          throughout all the time when they are still!).
          This can be a huge memory saving, which is important because
          generating many display lists for TVRMLGLAnimation is generally
          very memory-hungry operation.

          You can achieve even much better memory saving by using
          roSeparateShapeStatesNoTransformation.

          This is achieved by TVRMLOpenGLRendererContextCache
          methods ShapeState_Xxx.)
      )
    }
    roSeparateShapeStates,

    { This is like roSeparateShapeStates but it allows for much more
      display lists sharing because it stores untransformed
      ShapeState in a display list.

      Where this is better over roSeparateShapeStates:
      If you use TVRMLGLAnimation when the same ShapeState occurs
      in each frame but is transformed differently.
      Or when you have a scene that uses the same ShapeState many times
      but with different transformation.
      Actually, "transformation" means here everything rendered by
      TVRMLOpenGLRenderer.RenderShapeStateBegin, which includes
      modelview transformation, texture transformation and all lights
      settings.
      In such cases, roSeparateShapeStatesNoTranform will use
      one display list, where roSeparateShapeStates would use a lot.
      What exactly "a lot" means depends on how much frames your
      animation has, how much ShapeState is duplicated etc.
      This can be a @italic(huge memory saving). Also preparing
      scene/animations (in PrepareRender) should be much faster.

      This saved me 13 MB memory in "The Castle" (much less than
      I hoped, honestly, but still something...).

      Where this is worse over roSeparateShapeStates:
      @unorderedList(
        @item(
          roSeparateShapeStatesNoTransform can be used only if you don't use
          Attributes.OnBeforeVertex feature and your model doesn't
          use volumetric fog. If you do use these features,
          you have no choice: you must use roSeparateShapeStates,
          otherwise rendering results may be wrong.

          See [http://vrmlengine.sourceforge.net/kambi_vrml_test_suite.php]
          file @code(kambi_extensions/fog_volumetric/break_no_transform_final.wrl)
          (versions for VRML 1.0 and 2.0 are available) for a demo.

          Reasons: because TVRMLOpenGLRenderer.DoBeforeGLVertex
          uses Render_State.CurrMatrix.)

        @item(In some cases roSeparateShapeStatesNoTransform
          may be a little slower at rendering than roSeparateShapeStates,
          as this doesn't wrap in display list things done
          by TVRMLOpenGLRenderer.RenderShapeStateBegin.
          So modelview matrix and texture matrix and whole
          lights state will be done each time by OpenGL commands,
          without display lists.

          Will this affect rendering speed much ?
          If your scene doesn't use lights
          then the speed difference between roSeparateShapeStates
          and roSeparateShapeStatesNoTransform should not be noticeable.
          Otherwise... well, you have to check what matters more
          in this case: memory saving by roSeparateShapeStatesNoTransform
          or additional speed saving by roSeparateShapeStates.)
      )

      This is achieved by TVRMLOpenGLRendererContextCache
      methods ShapeStateNoTransform_Xxx. }
    roSeparateShapeStatesNoTransform
  );
  PGLRendererOptimization = ^TGLRendererOptimization;

{ Parses and removes from Parameters[1]..Parameters.High
  parameter @--renderer-optimization, and sets RendererOptimization
  to the value specified by user.
  See view3dscene documentation
  [http://vrmlengine.sourceforge.net/view3dscene.php] for description. }
procedure RendererOptimizationOptionsParse(
  var RendererOptimization: TGLRendererOptimization);

{ Describe what parameters are parsed by RendererOptimizationOptionsParse.
  This is nice to use e.g. in help text (e.g. the one printed in response
  to "@--help" command-line parameter). }
function RendererOptimizationOptionsHelp: string;

{ Convert string S to a TGLRendererOptimization value.
  Raises exception if illegal value of S.

  For a list of allowed values run
  [http://vrmlengine.sourceforge.net/view3dscene.php] with @--help
  (or just look at the implementation of this unit :) }
function RendererOptimizationFromName(const S: string; IgnoreCase: boolean):
  TGLRendererOptimization;

const
  RendererOptimizationNames: array[TGLRendererOptimization] of string =
  ( 'none',
    'scene-as-a-whole',
    'separate-shape-states',
    'separate-shape-states-no-transform' );

implementation

uses SysUtils, ParseParametersUnit, KambiUtils;

  {$define ARRAY_POS_FUNCTION_NAME := RendererOptimizationFromName}
  {$define ARRAY_POS_ARRAY_NAME := RendererOptimizationNames}
  {$define ARRAY_POS_INDEX_TYPE := TGLRendererOptimization}
  {$I macarraypos.inc}
  IMPLEMENT_ARRAY_POS_CASE_CHECKED

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
var RendererOptimizationPtr: PGLRendererOptimization absolute Data;
begin
 case OptionNum of
  0: RendererOptimizationPtr^ := RendererOptimizationFromName(Argument, true);
  else raise EInternalError.Create('VRMLFlatSceneGL.OptionProc: OptionNum');
 end;
end;

procedure RendererOptimizationOptionsParse(
  var RendererOptimization: TGLRendererOptimization);
const
  Options: array[0..0] of TOption =
  ( (Short: #0; Long: 'renderer-optimization'; Argument: oaRequired)
  );
begin
 ParseParameters(Options, {$ifdef FPC_OBJFPC} @ {$endif} OptionProc,
   @RendererOptimization, true);
end;

function RendererOptimizationOptionsHelp: string;
var ro: TGLRendererOptimization;
begin
 Result :=
   '  --renderer-optimization ' +
   RendererOptimizationNames[Low(TGLRendererOptimization)];
 for ro := Succ(Low(ro)) to High(ro) do
  Result += '|' + RendererOptimizationNames[ro];
 Result += nl+
   '                        Set rendering optimization kind.';
end;

end.
