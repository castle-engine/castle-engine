{
  Copyright 2016-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Base types and concepts related to rendering.
  Independent from OpenGL, X3D and other higher-level types. }
unit CastleRendererBaseTypes deprecated 'use CastleRenderOptions';

{$I castleconf.inc}

interface

uses Classes, CastleRenderOptions;

type
  TShaderType = CastleRenderOptions.TShaderType;
  TColorMode = CastleRenderOptions.TColorMode;
  TToneMapping = CastleRenderOptions.TToneMapping;
  TGammaCorrection = CastleRenderOptions.TGammaCorrection;

const
  stVertex = CastleRenderOptions.stVertex;
  stGeometry = CastleRenderOptions.stGeometry;
  stFragment = CastleRenderOptions.stFragment;
  cmReplace = CastleRenderOptions.cmReplace;
  cmModulate = CastleRenderOptions.cmModulate;
  tmNone = CastleRenderOptions.tmNone;
  tmUncharted = CastleRenderOptions.tmUncharted;
  tmHejlRichard = CastleRenderOptions.tmHejlRichard;
  tmACES = CastleRenderOptions.tmACES;
  gcNone = CastleRenderOptions.gcNone;
  gcPhysicalMaterial = CastleRenderOptions.gcPhysicalMaterial;
  gcAlways = CastleRenderOptions.gcAlways;
  // ShaderTypeName = CastleRenderOptions.ShaderTypeName; // not allowed by FPC

function GetLogShadowVolumes: Boolean;
procedure SetLogShadowVolumes(const Value: Boolean);
function GetGammaCorrection: TGammaCorrection;
procedure SetGammaCorrection(const Value: TGammaCorrection);
function GetToneMapping: TToneMapping;
procedure SetToneMapping(const Value: TToneMapping);

property LogShadowVolumes: Boolean read GetLogShadowVolumes write SetLogShadowVolumes;
property GammaCorrection: TGammaCorrection read GetGammaCorrection write SetGammaCorrection;
property ToneMapping: TToneMapping read GetToneMapping write SetToneMapping;

implementation

function GetLogShadowVolumes: Boolean;
begin
  Result := CastleRenderOptions.LogShadowVolumes;
end;

procedure SetLogShadowVolumes(const Value: Boolean);
begin
  CastleRenderOptions.LogShadowVolumes := Value;
end;

function GetGammaCorrection: TGammaCorrection;
begin
  Result := CastleRenderOptions.GammaCorrection;
end;

procedure SetGammaCorrection(const Value: TGammaCorrection);
begin
  CastleRenderOptions.GammaCorrection := Value;
end;

function GetToneMapping: TToneMapping;
begin
  Result := CastleRenderOptions.ToneMapping;
end;

procedure SetToneMapping(const Value: TToneMapping);
begin
  CastleRenderOptions.ToneMapping := Value;
end;

end.
