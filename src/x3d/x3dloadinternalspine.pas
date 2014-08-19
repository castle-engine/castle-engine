{
  Copyright 2014-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Spine 2D animations loader. }
unit X3DLoadInternalSpine;

interface

uses X3DNodes;

function LoadSpine(const URL: string): TX3DRootNode;

implementation

uses SysUtils, Classes, FGL, FpJson, JSONParser, Math,
  CastleVectors, CastleUtils, CastleLog, CastleURIUtils, CastleDownload,
  CastleStringUtils, CastleClassUtils, CastleColors,
  X3DLoadInternalUtils, CastleWarnings;

{ TODO:
  Some more exotic Spine animation features are not implemented (yet!).
  If you would like to see support for them, please submit
  a feature request, or ask on Castle Game Engine forum
  (or submit a patch yourself :). Many of the below features are really
  simple to implement (it is only a matter
  of adding simple reading and convertion to this file).

  - Attachment types:

    - regionsequence (easy, but unsure how exactly the images are described
      in json; in principle, this is exactly how you can use things
      like "xxx@counter(4).png" in X3D MovieTexture with our engine)

    - boundingbox (easy, convert to X3D TransformSensor;
      what to do with it depends on user code, boundingbox doesn't do anything
      by itself)

    - mesh and skinnedmesh (middle; we already render attachment as quads,
      so this just needs to read from spine the mesh coords;
      skinnedmesh requires animating mesh coordinates, also perfectly
      possible in X3D;
      note: what is "hull" parameter?)

  - Slot Timelines (easy;
    slot attachment should be animated using X3D Switch.choice;
    slot color should be animated using X3D Material color interpolator).

  - Events and Event Timeline (easy; note that events don't do anything
    by themself, they just allow animator to "name" some events on the timeline;
    we would convert them to some sensor that can be watched
    (using X3D ROUTE) by user code).

  - Draw Order Timeline (easy; you need to animate Z coordinate of
    TSlot node transform).
}

type
  ESpineReadError = class(Exception);

{$I x3dloadinternalspine_atlas.inc}

{ JSON skeleton -------------------------------------------------------------- }

type
  { forward declarations }
  TBoneList = class;
  TAttachmentList = class;

  {$define read_interface}
  {$I x3dloadinternalspine_bones.inc}
  {$I x3dloadinternalspine_slots.inc}
  {$I x3dloadinternalspine_attachments.inc}
  {$I x3dloadinternalspine_skins.inc}
  {$I x3dloadinternalspine_bonetimelines.inc}
  {$I x3dloadinternalspine_animations.inc}
  {$I x3dloadinternalspine_skeleton.inc}
  {$undef read_interface}

  {$define read_implementation}
  {$I x3dloadinternalspine_bones.inc}
  {$I x3dloadinternalspine_slots.inc}
  {$I x3dloadinternalspine_attachments.inc}
  {$I x3dloadinternalspine_skins.inc}
  {$I x3dloadinternalspine_bonetimelines.inc}
  {$I x3dloadinternalspine_animations.inc}
  {$I x3dloadinternalspine_skeleton.inc}
  {$undef read_implementation}

{ Main loading function ------------------------------------------------------ }

function LoadSpine(const URL: string): TX3DRootNode;
var
  Json: TJSONData;
  P: TJSONParser;
  S: TStream;
  Atlas: TAtlas;
  Skeleton: TSkeleton;
begin
  Atlas := TAtlas.Create;
  try
    Atlas.Parse(ChangeURIExt(URL, '.atlas'));
    Atlas.BuildNodes(URL);

    S := Download(URL);
    try
      P := TJSONParser.Create(S);
      try
        Json := P.Parse;
        try
          Result := TX3DRootNode.Create('', URL);
          try
            if Assigned(Json) then
            begin
              Skeleton := TSkeleton.Create;
              try
                Skeleton.Parse(Json);
                Skeleton.BuildNodes(URL, Atlas, Result);
              finally FreeAndNil(Skeleton) end;
            end;
          except FreeAndNil(Result); raise end;
        finally FreeAndNil(Json) end;
      finally FreeAndNil(P) end;
    finally FreeAndNil(S) end;
  finally FreeAndNil(Atlas) end;
end;

end.
