{
  Copyright 2003-2005 Michalis Kamburelis.

  This file is part of "Kambi's 3dmodels Pascal units".

  "Kambi's 3dmodels Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's 3dmodels Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's 3dmodels Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ Generates light map.

  See lets_take_a_walk game
  ([http://www.camelot.homedns.org/~michalis/lets_take_a_walk.php])
  for example how to use this program.
  lets_take_a_walk sources contain an example model and script
  used with this program.
}

program gen_light_map;

uses SysUtils, KambiUtils, VectorMath, VRMLNodes, VRMLLightSet, VRMLFlatScene,
  VRMLLightMap, Images, VRMLTriangleOctree, ProgressUnit, ProgressConsole;

function ReadParsVectorTo1st(i: Integer): TVector3Single;
begin
 result[0] := StrToFloat(ParStr(i));
 result[1] := StrToFloat(ParStr(i+1));
 result[2] := StrToFloat(ParStr(i+2));
end;

var
  LightSet: TVRMLLightSet;
  Scene: TVRMLFlatScene;
  SceneOctree: TVRMLTriangleOctree;
  Image: TImage;

  SceneFileName, OutImageFileName: string;
  ImageSizeX, ImageSizeY: Integer;

  Quad: TQuad3Single;
  RenderDir: TVector3Single;

  i: Integer;

begin
 { parse params }
 ParCountEqual(4 + 3*5);
 SceneFileName := ParStr(1);
 OutImageFileName := ParStr(2);
 ImageSizeX := StrToInt(ParStr(3));
 ImageSizeY := StrToInt(ParStr(4));
 for i := 0 to 3 do Quad[i] := ReadParsVectorTo1st(5 + i*3);
 RenderDir := ReadParsVectorTo1st(5 + 4*3);

 Image := nil;

 try

  { prepare Image (Image contents are not initialized - they may contain
    trash, we will render every pixel of this image so there is no point
    in clearing image at the beginning) }
  Image := ImageClassBestForSavingToFormat(OutImageFilename).
    Create(ImageSizeX, ImageSizeY);

  { evaluate Scene and LightSet (from the same RootNode) }
  Write('Loading scene... ');
  Scene := TVRMLFlatScene.Create(ParseVRMLFile(SceneFileName, true), true);
  LightSet := TVRMLLightSet.Create(Scene.RootNode, false);
  Writeln('done.');
  if LightSet.Lights.Count = 0 then
   Writeln('WARNING: scene has no lights defined (everything will be black)');

  { evaluate SceneOctree }
  Progress.UserInterface := ProgressConsoleInterface;
  SceneOctree := Scene.CreateTriangleOctree('Building octree');

  { render to Image }
  ProcTimerBegin;
  QuadLightMapTo1st(Image, LightSet.Lights, SceneOctree, Quad,
    RenderDir, 'Rendering');
  Writeln(Format('Rendering done in %f seconds.', [ProcTimerEnd]));

  SaveImage(Image, OutImageFilename);
 finally
  SceneOctree.Free;
  LightSet.Free;
  Scene.Free;
  Image.Free;
 end;
end.