{ Rendering optimizations (TGLRendererOptimization type and helpers).

  Used primarily by TVRMLGLScene class, but declared in a separate unit,
  as some non-OpenGL stuff (like Object3DAsVRML and VRML animations reader)
  also process this. }
unit VRMLRendererOptimization;

interface

type
  { Optimization method, used by @link(TVRMLGLScene.Optimization).
    See [http://vrmlengine.sourceforge.net/vrml_engine_doc/output/xsl/html/section.scene_gl.html#section.renderer_optimization]. }
  TGLRendererOptimization = (
    roNone,
    roSceneDisplayList,
    roShapeDisplayList,
    roVertexBufferObject
  );
  PGLRendererOptimization = ^TGLRendererOptimization;

{ Parses and removes from Parameters[1]..Parameters.High
  parameter @--renderer-optimization, and sets RendererOptimization
  to the value specified by user.

  Doesn't change RendererOptimization if command-line option not
  specified.

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
    'scene-display-list',
    'shape-display-list',
    'vertex-buffer-object' );

  RendererOptimizationNiceNames: array[TGLRendererOptimization] of string =
  ( 'None',
    'Whole Scene by Display List',
    'Shape by Display List',
    'Vertex Buffer Object' );

implementation

uses SysUtils, ParseParametersUnit, KambiUtils;

function RendererOptimizationFromName(const s: string; IgnoreCase: boolean): TGLRendererOptimization;
begin
  if IgnoreCase then
  begin
    for Result := Low(RendererOptimizationNames) to High(RendererOptimizationNames) do
      if AnsiSameText(RendererOptimizationNames[Result], s) then
        Exit;

    { obsolete names, supported for compatibility }
    if AnsiSameText('scene-as-a-whole', S) then
      Exit(roSceneDisplayList);

    if AnsiSameText('separate-shapes', S) or
       AnsiSameText('separate-shapes-no-transform', S) then
      Exit(roShapeDisplayList);
  end else
  begin
    for Result := Low(RendererOptimizationNames) to High(RendererOptimizationNames) do
      if RendererOptimizationNames[Result] = s then
        Exit;

    { obsolete names, supported for compatibility }
    if 'scene-as-a-whole' = S then
      Exit(roSceneDisplayList);

    if ('separate-shapes' = S) or
       ('separate-shapes-no-transform' = S) then
      Exit(roShapeDisplayList);
  end;
  raise Exception.Create('"'+s+'" does not match any of the allowed values');
end;

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
var RendererOptimizationPtr: PGLRendererOptimization absolute Data;
begin
 case OptionNum of
  0: RendererOptimizationPtr^ := RendererOptimizationFromName(Argument, true);
  else raise EInternalError.Create('VRMLGLScene.OptionProc: OptionNum');
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
