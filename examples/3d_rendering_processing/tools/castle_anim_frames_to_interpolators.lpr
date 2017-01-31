{
  Copyright 2009-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Convert castle-anim-frames animation (containing a sequence of 3D files)
  to a single X3D file using an X3D interpolator node.

  Limitations:
  - This works only for animations where only *one* mesh (X3D Coordinate
    node) changes during an animation.
  - Also, it only uses the 1st castle-anim-frames animation for now.
    (castle-anim-frames may contain many animations, e.g. exported
    from Blender actions.)
  - So this is not a general castle-anim-frames -> interpolators converter,
    at least *not yet*.
    BTW, when we make a general converter, it will be probably automatically
    used when loading castle-anim-frames, so the usefulness of this tool
    will disappear.

  Call with three parameters on the command-line:

  $1 - Coordinate node name (in all castle-anim-frames).
  $2 - Animation input URL (usually a castle-anim-frames filename,
       MD3 would also be sensible).
  $3 - VRML/X3D output URL (filename).

  For example:
    castle_anim_frames_to_interpolators \
      coord_node_name my_animation.castle-anim-frames my_output_animation.x3dv

  Another example (for testing):
    castle_anim_frames_to_interpolators \
      coords_ME_highpoly \
      ../../../../demo-models/bump_mapping/lizardman/lizardman.castle-anim-frames \
      /tmp/output_lizardman_first_animation.x3d
}
program castle_anim_frames_to_interpolators;

uses SysUtils, Classes, CastleUtils, CastleClassUtils, X3DNodes,
  CastlePrecalculatedAnimation, CastleStringUtils, CastleProgress,
  CastleProgressConsole, CastleParameters, CastleDownload, CastleLog,
  CastleApplicationProperties;

var
  CoordinateNodeName: string;
  InputURL, OutputURL: string;
  OutputInterp: TCoordinateInterpolatorNode;

procedure SaveFile(Node: TX3DRootNode; const URL: string;
  const CycleInterval: Single; const Loop: boolean);
var
  TimeSensor: TTimeSensorNode;
  Route: TX3DRoute;
  OutputCoord: TCoordinateNode;
begin
  TimeSensor := TTimeSensorNode.Create('AnimTimeSensor');
  TimeSensor.Loop := Loop;
  TimeSensor.CycleInterval := CycleInterval;
  Node.FdChildren.Add(TimeSensor);

  Route := TX3DRoute.Create;
  Route.SetSourceDirectly(TimeSensor.EventFraction_changed);
  Route.SetDestinationDirectly(OutputInterp.EventSet_fraction);
  Node.AddRoute(Route);

  OutputCoord := Node.FindNodeByName(
    TCoordinateNode, CoordinateNodeName, false) as TCoordinateNode;

  Route := TX3DRoute.Create;
  Route.SetSourceDirectly(OutputInterp.EventValue_changed);
  Route.SetDestinationDirectly(OutputCoord.FdPoint.EventIn);
  Node.AddRoute(Route);

  Save3D(Node, URL, ApplicationName, InputURL);
end;

var
  Anim: TCastlePrecalculatedAnimation;
  OutputX3d: TX3DRootNode;
  I: Integer;
  CoordCount: Cardinal;
  FrameCoord: TCoordinateNode;
begin
  ApplicationProperties.OnWarning.Add(@ApplicationProperties.WriteWarningOnConsole);

  Parameters.CheckHigh(3);

  CoordinateNodeName := Parameters[1];
  InputURL := Parameters[2];
  OutputURL := Parameters[3];

  Anim := TCastlePrecalculatedAnimation.Create(nil);
  try
    Writeln('Reading ', InputURL, ' ...');
    Anim.LoadFromFile(InputURL, false, true);

    { check Anim.TimeBegin }
    if Anim.TimeBegin <> 0 then
      WritelnWarning('Animation', Format('Animation time starts from %f, not from zero. This time is determined by the initial <frame> in castle-anim-frames attribute time="xxx". Non-zero start time will make VRML/X3D output somewhat incorrect (animation will be stretched), as when converting we have to assume that 1st frame starts at 0.',
        [Anim.TimeBegin]));

    OutputX3d := Anim.Scenes[0].RootNode.DeepCopy as TX3DRootNode;
    try
      { find Coordinate node for the 1st time, to calculate CoordCount }
      FrameCoord := Anim.Scenes[0].RootNode.FindNodeByName(
        TCoordinateNode, CoordinateNodeName, false) as TCoordinateNode;
      CoordCount := FrameCoord.FdPoint.Count;
      Writeln('Coordinate node ', CoordinateNodeName, ' found OK (', CoordCount, ' vertexes).');

      OutputInterp := TCoordinateInterpolatorNode.Create('AnimInterpolator', '');
      OutputX3d.FdChildren.Add(OutputInterp);

      OutputInterp.FdKeyValue.Items.Count := 0;
      OutputInterp.FdKeyValue.Items.Capacity := CoordCount * Anim.ScenesCount;

      Progress.UserInterface := ProgressConsoleInterface;
      Progress.Init(Anim.ScenesCount, 'Converting to interpolators');
      try

        for I := 0 to Anim.ScenesCount - 1 do
        begin
          FrameCoord := Anim.Scenes[I].RootNode.FindNodeByName(
            TCoordinateNode, CoordinateNodeName, false) as TCoordinateNode;
          OutputInterp.FdKeyValue.Items.AddList(FrameCoord.FdPoint.Items);
          OutputInterp.FdKey.Items.Add(I / (Anim.ScenesCount - 1));
          Progress.Step;

          { check, in case castle-anim-frames is not "structurally equal" on every frame }
          if FrameCoord.FdPoint.Count <> CoordCount then
            raise Exception.CreateFmt('Coordinate node in frame %d has %d vertexes, while in frame 0 it had %d vertexes. Such animation cannot be converted to CoordinateInterpolator',
              [I, FrameCoord.FdPoint.Count, CoordCount]);
        end;

        Assert(OutputInterp.FdKeyValue.Items.Count = CoordCount * Anim.ScenesCount);

      finally Progress.Fini end;

      Writeln('Writing ', OutputURL, '...');
      SaveFile(OutputX3d, OutputURL, Anim.TimeEnd, Anim.TimeLoop);
    finally FreeAndNil(OutputX3d) end;
  finally FreeAndNil(Anim) end;
end.
