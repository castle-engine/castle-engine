{
  Copyright 2024-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

type
  { How to process the URLs using @link(ProcessUrls).

    Note that we define what it does for X3D nodes.
    What happens if you save the resulting X3D nodes graph to a different format,
    like STL or glTF? This depends on capabilities of this format.
    For example,

    @unorderedList(
      @item(
        STL format doesn't allow to specify URLs to anything.

        So it doesn't matter how we process URLs at TInlineNode --- the
        resulting STL will contain the inlined contents anyway.

        And it doesn't matter how we process URLs at TImageTextureNode --
        the STL format doesn't support textures at all.)

      @item(
        The glTF format doesn't support something like TInlineNode
        but it does support textures defined by URLs.

        So it doesn't matter how we process URLs at TInlineNode --- the
        resulting STL will contain the inlined contents anyway.

        But it matters for textures. The resulting glTF will contain the
        texture URL transformed by @link(ProcessUrls).
      )
    )
  }
  TUrlProcessing = (
    { No additional processing of URLs.
      This still does something when used with @link(ProcessUrls):
      all the nodes's TX3DNode.BaseUrl will be set correctly. }
    suNone,

    { Minimal processing of URLs to avoid saving castle-data: URLs,
      instead convert them to relative URLs.
      This makes sense as default for CGE editor "Data -> Export to X3D, STL...",
      as we want to generate there a file that is independent of the project,
      so it should not use "castle-data:/". }
    suChangeCastleDataToRelative,

    { Make all resources embedded, by using data URI.
      See https://castle-engine.io/manual_network.php#section_data about data URI.
      In effect, the resulting X3D file is self-contained,
      for example you can open it on a mobile device in
      https://castle-engine.io/castle-model-viewer-mobile . }
    suEmbedResources,

    { Copy all resources to the "export-output" subdirectory alongside
      the exported file, and change all URLs to be relative and point to that
      directory.

      This allows to e.g. ZIP the resulting file + the "export-output" subdirectory
      and reliably know that it contains everything needed to display the model. }
    suCopyResourcesToSubdirectory
  );

{ Process all URLs in the graph according to the UrlProcessing.
  Useful before saving the model. }
procedure ProcessUrls(const RootNode: TX3DRootNode; const SaveBaseUrl: String;
  const UrlProcessing: TUrlProcessing);

{$endif read_interface}

{$ifdef read_implementation}

type
  { Helper for ProcessUrls. }
  TProcessUrlsHelper = class
  strict private
    function UrlChangeCastleDataToRelative(const Url, CurrentBaseUrl: String): String;
    function EmbedResource(const Url, CurrentBaseUrl: String): String;
    function CopyResourceToSubdirectory(const Url, CurrentBaseUrl: String): String;
  public
    BaseUrl, TargetBaseUrl: String;
    UrlProcessing: TUrlProcessing;
    procedure ProcessNode(Node: TX3DNode);
  end;

function TProcessUrlsHelper.UrlChangeCastleDataToRelative(const Url, CurrentBaseUrl: String): String;
var
  ResourceUrl, BaseFileName, ResourceFileName, ResultFileName: String;
begin
  ResourceUrl := CombineUri(CurrentBaseUrl, Url);
  ResourceFileName := UriToFilenameSafe(ResourceUrl);
  BaseFileName := UriToFilenameSafe(TargetBaseUrl);
  if (BaseFileName <> '') and (ResourceFileName <> '') then
  begin
    ResultFileName := ExtractRelativePath(BaseFileName, ResourceFileName);
    Result := RelativeFilenameToUriSafe(ResultFileName);
  end else
    // use original URL then, maybe with castle-data:/, maybe other protocol
    Result := Url;
end;

function TProcessUrlsHelper.EmbedResource(const Url, CurrentBaseUrl: String): String;
begin
  // TODO (use CastleInternalDataUri, see what tools/to-data-uri/to_data_uri.lpr does)
  Result := Url;
end;

function TProcessUrlsHelper.CopyResourceToSubdirectory(const Url, CurrentBaseUrl: String): String;
var
  ResourceUrl, BaseFileName, ResourceFileName, ResourceFileTitle, TargetFileName: String;
begin
  ResourceUrl := CombineUri(CurrentBaseUrl, Url);
  ResourceFileName := UriToFilenameSafe(ResourceUrl);
  BaseFileName := UriToFilenameSafe(TargetBaseUrl);
  if (BaseFileName <> '') and (ResourceFileName <> '') then
  begin
    ResourceFileTitle := ExtractFileName(ResourceFileName);
    TargetFileName := ExtractFilePath(BaseFileName) + ResourceFileTitle;
    if not FileExists(TargetFileName) then  // lets suppose same TargetFileName means same original source file
       CheckCopyFile(ResourceFileName, TargetFileName);
    Result := ResourceFileTitle;
  end else
    // use original URL then, maybe with castle-data:/, maybe other protocol
    Result := Url;
end;

procedure TProcessUrlsHelper.ProcessNode(Node: TX3DNode);

  procedure ProcessUrlField(const Field: TMFString);
  var
    I: Integer;
  begin
    for I := 0 to Field.Items.Count - 1 do
    begin
      case UrlProcessing of
        suChangeCastleDataToRelative:
          Field.Items[I] := UrlChangeCastleDataToRelative(Field.Items[I], Node.BaseUrl);
        suEmbedResources:
          Field.Items[I] := EmbedResource(Field.Items[I], Node.BaseUrl);
        suCopyResourcesToSubdirectory:
          Field.Items[I] := CopyResourceToSubdirectory(Field.Items[I], Node.BaseUrl);
      end;
    end;
  end;

var
  I: Integer;
begin
  if Node is TInlineNode then
    ProcessUrlField(TInlineNode(Node).FdUrl)
  else
  if Node is TImageTextureNode then
    ProcessUrlField(TImageTextureNode(Node).FdUrl)
  else
  if Node is TImageTexture3DNode then
    ProcessUrlField(TImageTexture3DNode(Node).FdUrl)
  else
  if Node is TMovieTextureNode then
    ProcessUrlField(TMovieTextureNode(Node).FdUrl)
  else
  if Node is TImageCubeMapTextureNode then
    ProcessUrlField(TImageCubeMapTextureNode(Node).FdUrl)
  else
  if Node is TDISEntityTypeMappingNode then
    ProcessUrlField(TDISEntityTypeMappingNode(Node).FdUrl)
  else
  if Node is TAudioClipNode then
    ProcessUrlField(TAudioClipNode(Node).FdUrl)
  else
  if Node is TScriptNode then
    ProcessUrlField(TScriptNode(Node).FdUrl)
  else
  if Node is TShaderProgramNode then
    ProcessUrlField(TShaderProgramNode(Node).FdUrl)
  else
  if Node is TShaderPartNode then
    ProcessUrlField(TShaderPartNode(Node).FdUrl)
  else
  if Node is TPackagedShaderNode then
    ProcessUrlField(TPackagedShaderNode(Node).FdUrl)
  else
  if Node is TGeoMetadataNode then
    ProcessUrlField(TGeoMetadataNode(Node).FdUrl)
  else
  if Node is TAnchorNode then
    ProcessUrlField(TAnchorNode(Node).FdUrl)
  else
  if Node is TBackgroundNode then
  begin
    ProcessUrlField(TBackgroundNode(Node).FdBackUrl);
    ProcessUrlField(TBackgroundNode(Node).FdBottomUrl);
    ProcessUrlField(TBackgroundNode(Node).FdFrontUrl);
    ProcessUrlField(TBackgroundNode(Node).FdLeftUrl);
    ProcessUrlField(TBackgroundNode(Node).FdRightUrl);
    ProcessUrlField(TBackgroundNode(Node).FdTopUrl);
  end;

  for I := 0 to Node.PrototypesCount - 1 do
    if Node.Prototypes[I] is TX3DExternalPrototype then
      ProcessUrlField(TX3DExternalPrototype(Node.Prototypes[I]).UrlList);

  { Set Node.BaseUrl, to allow opening the URLs relative to BaseUrl
    (which will also be present because of AdjustUrl).

    This is important if some code will require to load URLs from the exported
    graph, which happens e.g. for TInlineNode URLs when saving as STL:
    we need to load inline contents then.
    So, without this, saving to STL from "Export Viewport to X3D, STL..."
    in editor would fail for TCastleScene in viewport. }
  Node.BaseUrl := TargetBaseUrl;
end;

procedure ProcessUrls(const RootNode: TX3DRootNode;
  const SaveBaseUrl: String; const UrlProcessing: TUrlProcessing);
var
  Helper: TProcessUrlsHelper;
begin
  Helper := TProcessUrlsHelper.Create;
  try
    Helper.TargetBaseUrl := SaveBaseUrl;
    Helper.UrlProcessing := UrlProcessing;
    RootNode.EnumerateNodes(TX3DNode, {$ifdef FPC}@{$endif} Helper.ProcessNode, false);
  finally FreeAndNil(Helper) end;
end;

{$endif read_implementation}
