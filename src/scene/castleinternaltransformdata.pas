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

{ Internal data for TCastleTransformManipulate in CastleTransformManipulate unit. }
unit CastleInternalTransformData;

interface

procedure RegisterTransformManipulateData;

implementation

uses SysUtils, Classes,
  CastleUriUtils, CastleDownload, CastleUtils;

const
  TranslateCollider: {$I transform_manipulate_data/generated-pascal/translate_collider.glb.inc};
  TranslateFinal: {$I transform_manipulate_data/generated-pascal/translate_final.x3dv.inc};
  Translate: {$I transform_manipulate_data/generated-pascal/translate.glb.inc};

  RotateCollider: {$I transform_manipulate_data/generated-pascal/rotate_collider.glb.inc};
  RotateFinal: {$I transform_manipulate_data/generated-pascal/rotate_final.x3dv.inc};
  Rotate: {$I transform_manipulate_data/generated-pascal/rotate.glb.inc};

  ScaleFinal: {$I transform_manipulate_data/generated-pascal/scale_final.x3dv.inc};
  Scale: {$I transform_manipulate_data/generated-pascal/scale.glb.inc};

{ TEmbeddedDataReader -------------------------------------------------------- }

type
  { Allow to access X3D and glTF files inside transform_manipulate_data/exported,
    like "transform_manipulate_data/exported/translate_final.x3dv", using special
    URL like "castle-internal-embedded-data:/translate_final.x3dv".
    Registering this way allows the files inside to refer to each other,
    e.g. translate_final.x3dv can refer to translate.glb using X3D Inline. }
  TEmbeddedDataReader = class
  public
    function ReadUrl(const Url: String; out MimeType: string): TStream;
  end;

function TEmbeddedDataReader.ReadUrl(const Url: String; out MimeType: string): TStream;

  procedure ReturnData(const Data: array of Byte);
  begin
    Result := TMemoryStream.Create;
    Result.Size := High(Data) + 1;
    Result.WriteBuffer(Data, Result.Size);
    Result.Seek(0, soBeginning);

    MimeType := UriMimeType(Url);
  end;

begin
  if Url = 'castle-internal-embedded-data:/translate_collider.glb' then
    ReturnData(TranslateCollider)
  else
  if Url = 'castle-internal-embedded-data:/translate_final.x3dv' then
    ReturnData(TranslateFinal)
  else
  if Url = 'castle-internal-embedded-data:/translate.glb' then
    ReturnData(Translate)
  else
  if Url = 'castle-internal-embedded-data:/rotate_collider.glb' then
    ReturnData(RotateCollider)
  else
  if Url = 'castle-internal-embedded-data:/rotate_final.x3dv' then
    ReturnData(RotateFinal)
  else
  if Url = 'castle-internal-embedded-data:/rotate.glb' then
    ReturnData(Rotate)
  else
  if Url = 'castle-internal-embedded-data:/scale_final.x3dv' then
    ReturnData(ScaleFinal)
  else
  if Url = 'castle-internal-embedded-data:/scale.glb' then
    ReturnData(Scale)
  else
    raise EInternalError.CreateFmt('Unknown URL "%s"', [Url]);
end;

var
  EmbeddedDataReader: TEmbeddedDataReader;
  EmbeddedDataReaderRegistered: Boolean;

procedure RegisterTransformManipulateData;
begin
  if not EmbeddedDataReaderRegistered then
  begin
    EmbeddedDataReader := TEmbeddedDataReader.Create;
    RegisterUrlProtocol('castle-internal-embedded-data',
      {$ifdef FPC}@{$endif} EmbeddedDataReader.ReadUrl, nil);
    EmbeddedDataReaderRegistered := true;
  end;
end;

initialization
finalization
  if EmbeddedDataReaderRegistered then
  begin
    UnregisterUrlProtocol('castle-internal-embedded-data');
    EmbeddedDataReaderRegistered := false;
  end;
  FreeAndNil(EmbeddedDataReader);
end.
