{%MainUnit x3dnodes.pas}
{
  Copyright 2008-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Common stuff for generated textures. }

{$ifdef read_interface}

  { Possible update modes for generated textures (like @link(TGeneratedCubeMapTextureNode.Update)). }
  TTextureUpdate = (upNone, upNextFrameOnly, upAlways);

  { Functionality for all X3D nodes that represent generated textures. }
  TGeneratedTextureFunctionality = class(TNodeFunctionality)
  strict private
    FTextureNode: TAbstractTextureNode;
  protected
    function GetUpdate: TTextureUpdate; virtual; abstract;
    procedure SetUpdate(const Value: TTextureUpdate); virtual; abstract;
  public
    { Used by TCastleScene to track when to set InternalUpdateNeeded. }
    InternalLastStateId: TFrameId;

    { When @link(Update) is upAlways, you can check this to know if really
      something visible changed since last update.
      If not, then you do not have to update the texture --- no point, since
      it would look exactly like the current one.

      TCastleScene takes care to set this field.
      After each actual update of the texture,
      it's set to @false. Each time something visible affecting the look
      of this texture possibly changed, it's set to @true. }
    InternalUpdateNeeded: boolean;

    constructor Create(const TextureNode: TAbstractTextureNode);

    { Depending on @link(Update), decide whether we need updating. }
    function NeedsUpdate: Boolean;

    { Call after NeedsUpdate returned @true and you updated the texture.
      Will take care of sending "NONE" after "NEXT_FRAME_ONLY". }
    procedure PostUpdate;

    property Update: TTextureUpdate read GetUpdate write SetUpdate;
  end;
{$endif read_interface}

{$ifdef read_implementation}
const
  TextureUpdateNames: array [TTextureUpdate] of string = (
    'NONE',
    'NEXT_FRAME_ONLY',
    'ALWAYS'
  );

{ TGeneratedTextureFunctionality --------------------------------------------- }

constructor TGeneratedTextureFunctionality.Create(const TextureNode: TAbstractTextureNode);
begin
  FTextureNode := TextureNode;
  inherited Create(TextureNode);
end;

function TGeneratedTextureFunctionality.NeedsUpdate: Boolean;
begin
  Result :=
      (Update = upNextFrameOnly) or
    ( (Update = upAlways) and InternalUpdateNeeded );
end;

procedure TGeneratedTextureFunctionality.PostUpdate;
begin
  if Update = upNextFrameOnly then
    Update := upNone;
  InternalUpdateNeeded := false;
end;

{$endif read_implementation}
