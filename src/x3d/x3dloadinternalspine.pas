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

function LoadSpine(URL: string): TX3DRootNode;

implementation

uses SysUtils, Classes, FGL, FpJson, JSONParser, Math,
  CastleVectors, CastleUtils, CastleLog, CastleURIUtils, CastleDownload,
  CastleStringUtils, CastleClassUtils, CastleColors,
  X3DLoadInternalUtils, CastleWarnings, X3DFields;

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

    - skinnedmesh (requires animating mesh coordinates;
      note: what is "hull" parameter?)

  - Events and Event Timeline (easy; note that events don't do anything
    by themself, they just allow animator to "name" some events on the timeline;
    we would convert them to some sensor that can be watched
    (using X3D ROUTE) by user code).

  - Draw Order Timeline (easy; you need to animate Z coordinate of
    TSlot node transform).
}

type
  ESpineReadError = class(Exception);

{$I x3dloadinternalspine_textureloader.inc}
{$I x3dloadinternalspine_simpletextureloader.inc}
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
  {$I x3dloadinternalspine_slottimelines.inc}
  {$I x3dloadinternalspine_drawordertimelines.inc}
  {$I x3dloadinternalspine_animations.inc}
  {$I x3dloadinternalspine_skeleton.inc}
  {$undef read_interface}

  {$define read_implementation}
  {$I x3dloadinternalspine_bones.inc}
  {$I x3dloadinternalspine_slots.inc}
  {$I x3dloadinternalspine_attachments.inc}
  {$I x3dloadinternalspine_skins.inc}
  {$I x3dloadinternalspine_bonetimelines.inc}
  {$I x3dloadinternalspine_slottimelines.inc}
  {$I x3dloadinternalspine_drawordertimelines.inc}
  {$I x3dloadinternalspine_animations.inc}
  {$I x3dloadinternalspine_skeleton.inc}
  {$undef read_implementation}

{ Main loading function ------------------------------------------------------ }

function LoadSpine(URL: string): TX3DRootNode;

  function CreateTextureLoader: TTextureLoader;
  var
    AtlasURL: string;
    Atlas: TAtlas;
  begin
    AtlasURL := ChangeURIExt(URL, '.atlas');
    if URIFileExists(AtlasURL) then
    begin
      Atlas := TAtlas.Create;
      try
        Atlas.Parse(AtlasURL);
        Atlas.BuildNodes(URL);
        Result := Atlas;
      except FreeAndNil(Atlas); raise end;
    end else
    begin
      WritelnLog('Spine', 'Atlas not found under URL "' + AtlasURL + '", will directly load images using "images/xxx.png" filenames');
      Result := TSimpleTextureLoader.Create(URL);
    end;
  end;

var
  Json: TJSONData;
  P: TJSONParser;
  S: TStream;
  Skeleton: TSkeleton;
  SkinName: string;
  TextureLoader: TTextureLoader;
begin
  { Strip SkinName from URL anchor. }
  URIExtractAnchor(URL, SkinName, true);

  TextureLoader := CreateTextureLoader;
  try
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
                Skeleton.BuildNodes(URL, TextureLoader, Result, SkinName);
              finally FreeAndNil(Skeleton) end;
            end;
          except FreeAndNil(Result); raise end;
        finally FreeAndNil(Json) end;
      finally FreeAndNil(P) end;
    finally FreeAndNil(S) end;
  finally FreeAndNil(TextureLoader) end;
end;

end.
