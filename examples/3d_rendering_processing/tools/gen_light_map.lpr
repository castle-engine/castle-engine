{
  Copyright 2003-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Generates light map.

  See lets_take_a_walk game
  ([http://castle-engine.sourceforge.net/lets_take_a_walk.php])
  for example how to use this program.
  lets_take_a_walk sources contain an example model and script
  used with this program.
}

program gen_light_map;

uses SysUtils, CastleUtils, CastleVectors, X3DNodes, CastleSceneCore,
  LightMap, CastleImages, CastleProgress, CastleProgressConsole, CastleTimeUtils,
  CastleParameters;

function ReadParametersVectorVar(i: Integer): TVector3Single;
begin
 result[0] := StrToFloat(Parameters[i]);
 result[1] := StrToFloat(Parameters[i+1]);
 result[2] := StrToFloat(Parameters[i+2]);
end;

var
  Scene: TCastleSceneCore;
  Image: TCastleImage;

  SceneURL, OutImageURL: string;
  ImageSizeX, ImageSizeY: Integer;

  Quad: TQuad3Single;
  RenderDir: TVector3Single;

  i: Integer;

begin
 { parse params }
 Parameters.CheckHigh(4 + 3*5);
 SceneURL := Parameters[1];
 OutImageURL := Parameters[2];
 ImageSizeX := StrToInt(Parameters[3]);
 ImageSizeY := StrToInt(Parameters[4]);
 for i := 0 to 3 do Quad[i] := ReadParametersVectorVar(5 + i*3);
 RenderDir := ReadParametersVectorVar(5 + 4*3);

 Image := nil;

 try

  { prepare Image (Image contents are not initialized - they may contain
    trash, we will render every pixel of this image so there is no point
    in clearing image at the beginning) }
  Image := ImageClassBestForSavingToFormat(OutImageURL).
    Create(ImageSizeX, ImageSizeY);

  { calculate Scene (from the same RootNode) }
  Write('Loading scene... ');
  Scene := TCastleSceneCore.Create(nil);
  Scene.Load(SceneURL, true);
  Writeln('done.');
  if Scene.GlobalLights.Count = 0 then
   Writeln('WARNING: scene has no global lights defined (everything will be black)');

  { calculate SceneOctree }
  Progress.UserInterface := ProgressConsoleInterface;
  Scene.TriangleOctreeProgressTitle := 'Building octree';
  Scene.Spatial := [ssVisibleTriangles];

  { render to Image }
  ProcessTimerBegin;
  QuadLightMapVar(Image, Scene.GlobalLights, Scene.InternalOctreeVisibleTriangles, Quad,
    RenderDir, 'Rendering');
  Writeln(Format('Rendering done in %f seconds.', [ProcessTimerEnd]));

  SaveImage(Image, OutImageURL);
 finally
  Scene.Free;
  Image.Free;
 end;
end.