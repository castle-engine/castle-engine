{
  Copyright 2018-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Load 3D models in the glTF 2.0 format (@link(LoadGLTF)). }
unit X3DLoadInternalGLTF;

interface

uses X3DNodes;

{ Load 3D model in the GLTF format, converting it to an X3D nodes graph.
  This routine is internally used by the @link(Load3D) to load an GLTF file. }
function LoadGLTF(const URL: string): TX3DRootNode;

implementation

uses SysUtils, Classes, PasGLTF,
  CastleClassUtils, CastleDownload, CastleUtils, CastleURIUtils, CastleLog;

// TODO: PasGLTF compilation fails when debug info is included in compilation
// options (tested on FPC 3.0.4 Linux x86_64).
//
// Temporarily, we have <GenerateDebugInfo Value="False"/> in lpk,
// and it turn must be also done in all applications you're doing.

function LoadGLTF(const URL: string): TX3DRootNode;
var
  Document: TPasGLTF.TDocument;
  Stream: TStream;
  Mesh: TPasGLTF.TMesh;
  Primitive: TPasGLTF.TMesh.TPrimitive;
begin
  Stream := Download(URL, []);
  try
    Result := TX3DRootNode.Create('', URL);
    try
      Document := TPasGLTF.TDocument.Create;
      try
        Document.RootPath := InclPathDelim(ExtractFilePath(URIToFilenameSafe(URL)));
        Document.LoadFromStream(Stream);

        WritelnLogMultiline('glTF', Format(
          'Asset.Copyright: %s' + NL +
          'Asset.Generator: %s' + NL +
          'Asset.MinVersion: %s' + NL +
          'Asset.Version: %s' + NL +
          'Asset.Empty: %s' + NL +
          'Accessors: %d' + NL +
          'Animations: %d' + NL +
          'Buffers: %d' + NL +
          'BufferViews: %d' + NL +
          'Cameras: %d' + NL +
          'Images: %d' + NL +
          'Materials: %d' + NL +
          'Meshes: %d' + NL +
          'Nodes: %d' + NL +
          'Samplers: %d' + NL +
          'Scenes: %d' + NL +
          'Skins: %d' + NL +
          'Textures: %d' + NL +
          'ExtensionsUsed: %s' + NL +
          'ExtensionsRequired: %s' + NL +
          '',
          [Document.Asset.Copyright,
           Document.Asset.Generator,
           Document.Asset.MinVersion,
           Document.Asset.Version,
           BoolToStr(Document.Asset.Empty, true),

           Document.Accessors.Count,
           Document.Animations.Count,
           Document.Buffers.Count,
           Document.BufferViews.Count,
           Document.Cameras.Count,
           Document.Images.Count,
           Document.Materials.Count,
           Document.Meshes.Count,
           Document.Nodes.Count,
           Document.Samplers.Count,
           Document.Scenes.Count,
           Document.Skins.Count,
           Document.Textures.Count,
           Document.ExtensionsUsed.Text,
           Document.ExtensionsRequired.Text
          ])
        );
        if Document.Scene <> -1 then
          WritelnLog('glTF', 'Current scene %d name: "%s"',
            [Document.Scene, Document.Scenes[Document.Scene].Name]);
        for Mesh in Document.Meshes do
        begin
          WritelnLog('glTF', 'Mesh %s', [Mesh.Name]);
          for Primitive in Mesh.Primitives do
          begin
            case Primitive.Mode of
              TPasGLTF.TMesh.TPrimitive.TMode.Points       : WritelnLog('glTF', 'Got points, indices %d', [Primitive.Indices]);
              TPasGLTF.TMesh.TPrimitive.TMode.Lines        : WritelnLog('glTF', 'Got lines, indices %d', [Primitive.Indices]);
              TPasGLTF.TMesh.TPrimitive.TMode.LineLoop     : WritelnLog('glTF', 'Got line loop, indices %d', [Primitive.Indices]);
              TPasGLTF.TMesh.TPrimitive.TMode.LineStrip    : WritelnLog('glTF', 'Got line strip, indices %d', [Primitive.Indices]);
              TPasGLTF.TMesh.TPrimitive.TMode.Triangles    : WritelnLog('glTF', 'Got triangles, indices %d', [Primitive.Indices]);
              TPasGLTF.TMesh.TPrimitive.TMode.TriangleStrip: WritelnLog('glTF', 'Got triangle strip, indices %d', [Primitive.Indices]);
              TPasGLTF.TMesh.TPrimitive.TMode.TriangleFan  : WritelnLog('glTF', 'Got triangle fan, indices %d', [Primitive.Indices]);
              else WritelnWarning('Unknown glTF primitive mode %d', [Primitive.Mode]);
            end;
          end;
        end;
      finally FreeAndNil(Document) end;
    except FreeAndNil(Result); raise end;
  finally FreeAndNil(Stream) end;
end;

end.
