{
  Copyright 2014-2018 Michalis Kamburelis.

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

{$I castleconf.inc}

interface

uses X3DNodes;

function LoadSpine(URL: String): TX3DRootNode;

var
  { Turn this on to see some additional warnings when loading Spine models.
    These warnings are sometimes too verbose (often the models will work fine,
    and these warning can be ignored), so they are disabled by default. }
  SpineVerboseWarnings: Boolean = false;

  { Do not use textures and atlases referenced in the Spine model,
    do not set their URLs and do not try to load them in any way. }
  SpineIgnoreTextures: Boolean = false;

implementation

uses SysUtils, Classes, Generics.Collections, FpJson, JSONParser, JSONScanner, Math,
  CastleVectors, CastleCurves, CastleUtils, CastleLog, CastleURIUtils, CastleDownload,
  CastleStringUtils, CastleClassUtils, CastleColors, X3DLoadInternalUtils,
  CastleTriangles,
  X3DFields;

type
  ESpineReadError = class(Exception);

{$I x3dloadinternalspine_url.inc}
{$I x3dloadinternalspine_textureloader.inc}
{$I x3dloadinternalspine_simpletextureloader.inc}
{$I x3dloadinternalspine_atlas.inc}

{ JSON skeleton -------------------------------------------------------------- }

type
  { forward declarations }
  TBoneList = class;
  TAttachmentList = class;

  {$define read_interface}
  {$I x3dloadinternalspine_json.inc}
  {$I x3dloadinternalspine_bones.inc}
  {$I x3dloadinternalspine_slots.inc}
  {$I x3dloadinternalspine_attachments.inc}
  {$I x3dloadinternalspine_skins.inc}
  {$I x3dloadinternalspine_bonetimelines.inc}
  {$I x3dloadinternalspine_slottimelines.inc}
  {$I x3dloadinternalspine_drawordertimelines.inc}
  {$I x3dloadinternalspine_deformtimelines.inc}
  {$I x3dloadinternalspine_weightedmeshtimelines.inc}
  {$I x3dloadinternalspine_animations.inc}
  {$I x3dloadinternalspine_skeleton.inc}
  {$undef read_interface}

  {$define read_implementation}
  {$I x3dloadinternalspine_json.inc}
  {$I x3dloadinternalspine_bones.inc}
  {$I x3dloadinternalspine_slots.inc}
  {$I x3dloadinternalspine_attachments.inc}
  {$I x3dloadinternalspine_skins.inc}
  {$I x3dloadinternalspine_bonetimelines.inc}
  {$I x3dloadinternalspine_slottimelines.inc}
  {$I x3dloadinternalspine_drawordertimelines.inc}
  {$I x3dloadinternalspine_deformtimelines.inc}
  {$I x3dloadinternalspine_weightedmeshtimelines.inc}
  {$I x3dloadinternalspine_animations.inc}
  {$I x3dloadinternalspine_skeleton.inc}
  {$undef read_implementation}

{ Main loading function ------------------------------------------------------ }

function LoadSpine(URL: String): TX3DRootNode;

  function CreateTextureLoader(const CustomAtlasName: String): TTextureLoader;

    function FindAtlas(const InitialAtlasURL: String; out AtlasURL: String): Boolean;
    var
      NoExtension: String;
    begin
      AtlasURL := InitialAtlasURL;
      if URIFileExists(AtlasURL) then Exit(true);

      NoExtension := ChangeURIExt(URL, '');

      // try with "_tex", used by Dragon Bones
      AtlasURL := NoExtension + '_tex.atlas';
      if URIFileExists(AtlasURL) then Exit(true);

      // try to remove -pro and -ess suffixes, Spine runtimes on
      // https://github.com/EsotericSoftware/spine-runtimes
      // (sometimes) do it too
      if IsSuffix('-pro', NoExtension, true) then
      begin
        AtlasURL := SuffixRemove('-pro', NoExtension, true) + '.atlas';
        if URIFileExists(AtlasURL) then Exit(true);
      end;

      if IsSuffix('-ess', NoExtension, true) then
      begin
        AtlasURL := SuffixRemove('-ess', NoExtension, true) + '.atlas';
        if URIFileExists(AtlasURL) then Exit(true);
      end;

      Exit(false);
    end;

  var
    StandardAtlasURL, AtlasURL: String;
    Atlas: TAtlas;
  begin
    if SpineIgnoreTextures then
      Exit(TSimpleTextureLoader.Create(URL));

    // calculate StandardAtlasURL
    if CustomAtlasName <> '' then
      StandardAtlasURL := ExtractURIPath(URL) + CustomAtlasName + '.atlas'
    else
      StandardAtlasURL := ChangeURIExt(URL, '.atlas');

    if FindAtlas(StandardAtlasURL, AtlasURL) then
    begin
      Atlas := TAtlas.Create;
      try
        Atlas.Parse(AtlasURL);
        Atlas.BuildNodes(URL);
        Result := Atlas;
      except FreeAndNil(Atlas); raise end;
    end else
    begin
      WritelnLog('Spine', 'Atlas not found under URL "' + StandardAtlasURL + '" (and some alternative URLs we tried), will load images without atlas, using "images/xxx.png" filenames');
      Result := TSimpleTextureLoader.Create(URL);
    end;
  end;

var
  Json: TJSONData;
  P: TJSONParser;
  S: TStream;
  Skeleton: TSkeleton;
  SkinName, CustomAtlasName: String;
  TextureLoader: TTextureLoader;
begin
  { strip additional info (in the URL anchor, i.e. '#xxx' suffix) }
  URIExtractInfo(URL, SkinName, CustomAtlasName);

  TextureLoader := CreateTextureLoader(CustomAtlasName);
  try
    S := Download(URL);
    try
      P :=
        {$ifdef VER2} TJSONParser.Create(S);
        {$else}
          {$ifdef VER3_0_0} TJSONParser.Create(S);
          {$else} { For FPC > 3.0.0 }
            { Do not add joUTF8, it fails to work on
                tests/data/escape_from_the_universe_boss/boss.json
              with FPC 3.1.1-r36683 [2017/07/08] for Linux x86_64
              (and it may be our fault? not really sure.)
              Works with FPC 3.0.2. }
            TJSONParser.Create(S, [joComments]);
          {$endif}
        {$endif}
      try
        Json := P.Parse;
        try
          Result := TX3DRootNode.Create('', URL);
          try
            try
              if Assigned(Json) then
              begin
                Skeleton := TSkeleton.Create;
                try
                  Skeleton.Parse(URL, Json, SkinName);
                  Skeleton.BuildNodes(URL, TextureLoader, Result);
                  if Skeleton.DefaultSkin = nil then Exit;
                  Skeleton.Animations.Exported(Result);
                finally FreeAndNil(Skeleton) end;
              end;
            except
              on E: ESpineReadError do
              begin
                E.Message := E.Message + ' (inside ' + URIDisplay(URL) + ')';
                raise;
              end;
            end;
          except FreeAndNil(Result); raise end;
        finally FreeAndNil(Json) end;
      finally FreeAndNil(P) end;
    finally FreeAndNil(S) end;
  finally FreeAndNil(TextureLoader) end;
end;

end.
