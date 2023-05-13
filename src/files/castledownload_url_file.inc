{%MainUnit castledownload.pas}
{
  Copyright 2013-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

{$endif}

{$ifdef read_implementation}

{ Handle file URLs. }

{ TUrlFile ------------------------------------------------------------------- }

type
  TUrlFile = class
    class function Read(const Url: string; out MimeType: string): TStream;
    class function Write(const Url: string): TStream;
    class function MimeType(const URI: string; out Gzipped: Boolean): String;
  end;

class function TUrlFile.Read(const Url: string; out MimeType: string): TStream;
var
  FileName: String;
begin
  CheckFileAccessSafe(URL);

  FileName := URIToFilenameSafe(URL);
  if FileName = '' then
    raise EDownloadError.CreateFmt('Cannot convert URL "%s" to filename', [URL]);

  Result := TFileStream.Create(FileName, fmOpenRead);
  MimeType := URIMimeType(URL);
end;

class function TUrlFile.Write(const Url: string): TStream;
var
  FileName: String;
begin
  FileName := URIToFilenameSafe(URL);
  if FileName = '' then
    raise ESaveError.CreateFmt('Cannot convert URL to a filename: "%s"', [URL]);

  Result := TFileStream.Create(FileName, fmCreate);
end;

class function TUrlFile.MimeType(const URI: string; out Gzipped: Boolean): String;
var
  Ext, ExtExt, ExtA{, ExtExtA}, URIWithoutAnchor, URIName: String;
begin
  Result := '';
  Gzipped := false;

  { We're consciously using here ExtractFileExt and ExtractFileDoubleExt on URIs,
    although they should be used for filenames.
    Note that this unit does not define public functions like ExtractURIExt
    or ExtractURIDoubleExt: *everything* should operate on MIME types instead. }
  Ext := LowerCase(ExtractFileExt(URI));
  ExtExt := LowerCase(ExtractFileDoubleExt(URI));

  { Some of our reading functions, like Spine, sprite sheets and images,
    recognize and strip anchors from URL.
    So recognize MIME type based on URL without anchor too.
    Otherwise 'xxx.json#skinname' would not be detected as Spine JSON.
    The comparison below can use ExtA / ExtExtA instead of Ext / ExtExt then.

    Note that doing it here means that *all* implementations of relevant formats
    should strip anchors, e.g. both LoadImageAsNode and LoadImage must strip anchors. }
  URIWithoutAnchor := URIDeleteAnchor(URI, true);
  ExtA := LowerCase(ExtractFileExt(URIWithoutAnchor));
  // unused: ExtExtA := LowerCase(ExtractFileDoubleExt(URIWithoutAnchor));

  URIName := LowerCase(ExtractURIName(URI));

  { This list is based on
    http://svn.freepascal.org/cgi-bin/viewvc.cgi/trunk/lcl/interfaces/customdrawn/customdrawnobject_android.inc?root=lazarus&view=co&content-type=text%2Fplain
    (license is LGPL with static linking exception, just like our engine).
    See also various resources linked from
    "Function to get the mimetype from a file extension" thread on Lazarus
    mailing list:
    http://comments.gmane.org/gmane.comp.ide.lazarus.general/62738

    We somewhat cleaned it up (e.g. "postscript" and "mpeg" lowercase),
    fixed categorization, and fixed/added many types looking at
    /etc/mime.types and
    /usr/share/mime/packages/freedesktop.org.xml on Debian.

    For description of MIME content types see also
    https://en.wikipedia.org/wiki/Internet_media_type
    http://en.wikipedia.org/wiki/MIME
    http://tools.ietf.org/html/rfc4288 }

  // 3D models (see also view3dscene MIME specification in view3dscene/desktop/view3dscene.xml)
  if Ext    = '.wrl'    then Result := 'model/vrml' else
  if Ext    = '.wrz'    then begin Result := 'model/vrml'; Gzipped := true; end else
  if ExtExt = '.wrl.gz' then begin Result := 'model/vrml'; Gzipped := true; end else
  if Ext    = '.x3dv'    then Result := 'model/x3d+vrml' else
  if Ext    = '.x3dvz'   then begin Result := 'model/x3d+vrml'; Gzipped := true; end else
  if ExtExt = '.x3dv.gz' then begin Result := 'model/x3d+vrml'; Gzipped := true; end else
  if Ext    = '.x3d'    then Result := 'model/x3d+xml' else
  if Ext    = '.x3dz'   then begin Result := 'model/x3d+xml'; Gzipped := true; end else
  if ExtExt = '.x3d.gz' then begin Result := 'model/x3d+xml'; Gzipped := true; end else
  if Ext    = '.x3db'    then Result := 'model/x3d+binary' else
  if ExtExt = '.x3db.gz' then begin Result := 'model/x3d+binary'; Gzipped := true; end else
  if Ext = '.dae' then Result := 'model/vnd.collada+xml' else
  { See http://en.wikipedia.org/wiki/.3ds about 3ds mime type.
    application/x-3ds is better (3DS is hardly an "image"),
    but Debian /usr/share/mime/packages/freedesktop.org.xml also uses
    image/x-3ds, so I guess image/x-3ds is more popular. }
  if Ext = '.3ds' then Result := 'image/x-3ds' else
  if Ext = '.max' then Result := 'image/x-3ds' else
  if Ext = '.iv' then Result := 'application/x-inventor' else
  if ExtA = '.md3' then Result := 'application/x-md3' else
  if Ext = '.obj' then Result := 'application/x-wavefront-obj' else
  if Ext = '.geo' then Result := 'application/x-geo' else
  if Ext = '.kanim' then Result := 'application/x-castle-anim-frames' else
  if Ext = '.castle-anim-frames' then Result := 'application/x-castle-anim-frames' else
  if ExtA = '.json' then Result := 'application/json' else
  { Various sites propose various MIME types for STL:
    https://gist.github.com/allysonsouza/1bf9d4a0295a14373979cd23d15df0a9
    application/wavefront-stl
    application/vnd.ms-pki.stl }
  if Ext = '.stl' then Result := 'application/x-stl' else
  if Ext = '.glb' then Result := 'model/gltf-binary' else
  if Ext = '.gltf' then Result := 'model/gltf+json' else
  // Images.
  { Only images that we cannot handle in CastleImages unit are listed below.
    For handled images, their extensions and mime types are recorded
    by CastleImages inside the URIMimeExtensions. }
  if Ext = '.svg' then Result := 'image/svg+xml' else
  if Ext = '.ico' then Result := 'image/x-icon' else
  if Ext = '.icns' then Result := 'image/icns' else
  if ExtA = '.castle-sprite-sheet' then Result := 'application/x-castle-sprite-sheet' else
  { I didn't found real MIME type for Starling Texture Atlas.
    Created as image type based on
    https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types
  }
  if ExtA = '.starling-xml' then Result := 'application/x-starling-sprite-sheet' else
  if ExtA = '.cocos2d-plist' then Result := 'application/x-cocos2d-sprite-sheet' else
  if ExtA = '.plist' then Result := 'application/x-plist' else

  // HTML
  if Ext = '.htm' then Result := 'text/html' else
  if Ext = '.html' then Result := 'text/html' else
  if Ext = '.shtml' then Result := 'text/html' else
  if Ext = '.css' then Result := 'text/css' else
  if Ext = '.php' then Result := 'text/php' else
  // Plain text
  if Ext = '.txt' then Result := 'text/plain' else
  if Ext = '.pas' then Result := 'text/plain' else
  if Ext = '.pp' then Result := 'text/plain' else
  if Ext = '.inc' then Result := 'text/plain' else
  if Ext = '.c' then Result := 'text/plain' else
  if Ext = '.cpp' then Result := 'text/plain' else
  if Ext = '.java' then Result := 'text/plain' else
  if Ext = '.log' then Result := 'text/plain'  else
  if Ext = '.md' then Result := 'text/plain' else
  if URIName = '.gitignore' then Result := 'text/plain' else
  // Videos
  if Ext = '.mp4' then Result := 'video/mp4' else
  if Ext = '.avi' then Result := 'video/x-msvideo' else
  if Ext = '.mpeg' then Result := 'video/mpeg' else
  if Ext = '.mpg'  then Result := 'video/mpeg' else
  if Ext = '.mpe'  then Result := 'video/mpeg' else
  if Ext = '.ogv'  then Result := 'video/ogg' else
  if Ext = '.mov' then Result := 'video/quicktime' else
  if Ext = '.flv' then Result := 'video/x-flv' else
  if Ext = '.swf'  then Result := 'application/x-shockwave-flash' else
  if Ext = '.swfl' then Result := 'application/x-shockwave-flash' else
  // Sounds
  if Ext = '.mp3' then Result := 'audio/mpeg' else
  if Ext = '.ogg' then Result := 'audio/ogg' else
  if Ext = '.oga' then Result := 'audio/ogg' else
  if Ext = '.wav' then Result := 'audio/x-wav' else
  if Ext = '.mid' then Result := 'audio/midi' else
  if Ext = '.midi' then Result := 'audio/midi' else
  if Ext = '.au' then Result := 'audio/basic' else
  if Ext = '.snd' then Result := 'audio/basic' else
  if Ext = '.mp2' then Result := 'audio/mpeg' else
  // Documents
  if Ext = '.rtf' then Result := 'text/rtf' else
  if Ext = '.eps' then Result := 'application/postscript' else
  if Ext = '.ps' then Result := 'application/postscript' else
  if Ext = '.pdf' then Result := 'application/pdf' else
  if Ext = '.csv' then Result := 'application/csv' else
  // Documents - old MS Office
  if Ext = '.xls' then Result := 'application/vnd.ms-excel' else
  if Ext = '.doc' then Result := 'application/msword' else
  if Ext = '.ppt' then Result := 'application/vnd.ms-powerpoint' else
  // Documents - open standards
  if Ext = '.odt' then Result := 'application/vnd.oasis.opendocument.text' else
  if Ext = '.ods' then Result := 'application/vnd.oasis.opendocument.spreadsheet' else
  if Ext = '.odp' then Result := 'application/vnd.oasis.opendocument.presentation' else
  if Ext = '.odg' then Result := 'application/vnd.oasis.opendocument.graphics' else
  if Ext = '.odc' then Result := 'application/vnd.oasis.opendocument.chart' else
  if Ext = '.odf' then Result := 'application/vnd.oasis.opendocument.formula' else
  if Ext = '.odi' then Result := 'application/vnd.oasis.opendocument.image' else
  // Documents - new MS Office
  if Ext = '.xlsx' then Result := 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet' else
  if Ext = '.pptx' then Result := 'application/vnd.openxmlformats-officedocument.presentationml.presentation' else
  if Ext = '.docx' then Result := 'application/vnd.openxmlformats-officedocument.wordprocessingml.document' else
  // Compressed archives
  if Ext = '.zip' then Result := 'application/zip' else
  if Ext = '.tar' then Result := 'application/x-tar' else
  if Ext = '.rar' then Result := 'application/x-rar-compressed' else
  if Ext = '.gz' then begin Result := 'application/gzip'; Gzipped := true; end else
  // Various
  if ExtA = '.xml' then Result := 'application/xml' else
  if Ext = '.castlescript' then Result := 'text/x-castlescript' else
  if Ext = '.kscript'      then Result := 'text/x-castlescript' else
  if Ext = '.js' then Result := 'application/javascript' else
  if Ext = '.castle-user-interface' then Result := 'text/x-castle-user-interface' else
  if Ext = '.castle-transform' then Result := 'text/x-castle-transform' else
  if Ext = '.castle-component' then Result := 'text/x-castle-component' else
  if ExtA = '.tmx' then Result := 'application/x-tiled-map' else
    { as a last resort, check URIMimeExtensions }
    URIMimeExtensions.TryGetValue(ExtA, Result);
end;

{$endif}
