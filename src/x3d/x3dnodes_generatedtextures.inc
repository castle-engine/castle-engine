{
  Copyright 2008-2017 Michalis Kamburelis.

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

  { Possible update modes for generated textures (like TGeneratedCubeMapTextureNode). }
  TTextureUpdate = (upNone, upNextFrameOnly, upAlways);

  { Update field for rendered textures (like TGeneratedCubeMapTextureNode). }
  TSFTextureUpdate = class(TSFStringEnum)
  private
    function GetValue: TTextureUpdate;
    procedure SetValue(const AValue: TTextureUpdate); reintroduce;
  protected
    class function ExposedEventsFieldClass: TX3DFieldClass; override;
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string;
      const AValue: TTextureUpdate);
    function ExecuteChanges: TX3DChanges; override;

    { Access the string value as TTextureUpdate enum.
      Takes care of doing warning if the underlying string value (in VRML/X3D)
      is invalid. }
    property Value: TTextureUpdate read GetValue write SetValue;
    procedure Send(const AValue: TTextureUpdate);
  end;

  { Common interface for all VRML/X3D generated texture nodes. }
  TGeneratedTextureHandler = class
  private
    FUpdate: TSFTextureUpdate;
  strict private
    FUpdateNeeded: boolean;
  public
    constructor Create;

    { When @link(Update) is upAlways, you can check this to know if really
      something visible changed since last update.
      If not, then you do not have to update the texture --- no point, since
      it would look exactly like the current one.

      Scene classes (TCastleSceneCore, TCastleScene, TGLRenderer)
      take care to set this field. After each actual update of the texture,
      it's set to @false. Each time something visible affecting the look
      of this texture possibly changed, it's set to @true. }
    property UpdateNeeded: boolean read FUpdateNeeded write FUpdateNeeded
      default true;

    property Update: TSFTextureUpdate read FUpdate;
  end;
{$endif read_interface}

{$ifdef read_implementation}
constructor TSFTextureUpdate.Create(AParentNode: TX3DFileItem;
  const AName: string; const AValue: TTextureUpdate);
const
  UpdateNames: array [TTextureUpdate] of string =
  ('NONE', 'NEXT_FRAME_ONLY', 'ALWAYS');
begin
  inherited Create(AParentNode, AName, UpdateNames, Ord(AValue));
end;

function TSFTextureUpdate.GetValue: TTextureUpdate;
begin
  Result := TTextureUpdate(EnumValue);
end;

procedure TSFTextureUpdate.SetValue(const AValue: TTextureUpdate);
begin
  EnumValue := Ord(AValue);
end;

procedure TSFTextureUpdate.Send(const AValue: TTextureUpdate);
begin
  inherited SendEnumValue(Ord(AValue));
end;

class function TSFTextureUpdate.ExposedEventsFieldClass: TX3DFieldClass;
begin
  Result := TSFString;
end;

function TSFTextureUpdate.ExecuteChanges: TX3DChanges;
begin
  { This causes appropriate @link(Changes): [chRedisplay] if value <> upNone.
    Then necessary things will be done automatically
    at next UpdateGeneratedTextures call, so nothing besides chRedisplay
    is required.

    Note we do not pass chVisibleGeometry, chVisibleNonGeometry, or such.
    So VisibleChangeHere will be called with [].
    That's logical --- only the change of "update" field doesn't visibly
    change anything on the scene. This means that if you change update
    to upAlways, but no visible change was registered since last update
    of the texture, the texture will not be actually immediately
    regenerated --- correct optimization!

    If value is upNone, nothing needs to be done. }

  if Value <> upNone then
    Result := [chRedisplay] else
    Result := [];
end;

constructor TGeneratedTextureHandler.Create;
begin
  inherited;
  FUpdateNeeded := true;
end;
{$endif read_implementation}
