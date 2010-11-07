{
  Copyright 2006-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ VRML headlight --- TVRMLHeadLight class. }
unit VRMLHeadLight;

interface

uses VectorMath, VRMLNodes;

type
  { This is a class that helps you render a headlight.

    The most common use is to render a headlight conforming to VRML
    specification, and configurable by KambiHeadLight VRML node
    (see [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_headlight]).

    @unorderedList(
      @item(
        If you create this passing HeadLightNode = @nil to constructor then
        the default VRML headlight (as required by NavigationInfo.headlight
        specification in VRML 2.0) properties will be set up.

        Equivalently, you can say that the default KambiHeadLight node
        properties will be assumed --- because default KambiHeadLight node
        are intentionally set such that they are identical with default
        headlight required by VRML 2.0 spec.)

      @item(Pass HeadLightNode <> @nil to constructor to use
        appropriate KambiHeadLight node properties.)
    ) }
  TVRMLHeadLight = class
  private
    FAmbientIntensity: Single;
    FAttenuation: TVector3Single;
    FColor: TVector3Single;
    FIntensity: Single;
    FSpot: boolean;
    FSpotCutOffAngle: Single;
    FSpotDropOffRate: Single;
    ActiveLightNode: TVRMLLightNode;
  public
    constructor Create(HeadLightNode: TNodeKambiHeadLight);
    destructor Destroy; override;

    property AmbientIntensity: Single read FAmbientIntensity write FAmbientIntensity;
    property Attenuation: TVector3Single read FAttenuation write FAttenuation;
    property Color: TVector3Single read FColor write FColor;
    property Intensity: Single read FIntensity write FIntensity;
    property Spot: boolean read FSpot write FSpot;
    property SpotCutOffAngle: Single read FSpotCutOffAngle write FSpotCutOffAngle;
    property SpotDropOffRate: Single read FSpotDropOffRate write FSpotDropOffRate;

    { TActiveLight record describing this headlight assuming
      given camera Position, Direction.

      Note: for now, it contains a link to temporary light node
      inside TActiveLight.LightNode. This is owned by this class,
      so if you call TActiveLight more than once for the same TVRMLHeadLight,
      and TVRMLHeadLight properties will change in between, than the older
      TActiveLight record may become invalid.

      This is not a problem for now (we need headlight as TActiveLight
      for ray-tracer only). When needed, maybe TActiveLight will be able
      to own LightNode. }
    function ActiveLight(const Position, Direction: TVector3Single): TActiveLight;
  end;

implementation

uses SysUtils, Math;

constructor TVRMLHeadLight.Create(HeadLightNode: TNodeKambiHeadLight);

  procedure CreateFromNode(Node: TNodeKambiHeadLight);
  begin
    FAmbientIntensity := Node.FdAmbientIntensity.Value;
    FAttenuation := Node.FdAttenuation.Value;
    FColor := Node.FdColor.Value;
    FIntensity := Node.FdIntensity.Value;
    FSpot := Node.FdSpot.Value;
    FSpotCutOffAngle := Node.FdSpotCutOffAngle.Value;
    FSpotDropOffRate := Node.FdSpotDropOffRate.Value;
  end;

begin
  inherited Create;

  if HeadLightNode <> nil then
    CreateFromNode(HeadLightNode) else
  begin
    { Create temporary HeadLightNode with default values. }
    HeadLightNode := TNodeKambiHeadLight.Create('', '');
    try
      CreateFromNode(HeadLightNode);
    finally FreeAndNil(HeadLightNode) end;
  end;
end;

destructor TVRMLHeadLight.Destroy;
begin
  FreeAndNil(ActiveLightNode);
  inherited;
end;

function TVRMLHeadLight.ActiveLight(
  const Position, Direction: TVector3Single): TActiveLight;
begin
  FreeAndNil(ActiveLightNode);

  if Spot then
  begin
    ActiveLightNode := TNodeSpotLight_2.Create('', '');
    TNodeSpotLight_2(ActiveLightNode).FdLocation.Value := Position;
    TNodeSpotLight_2(ActiveLightNode).FdDirection.Value := Direction;

    { For SpotDropOffRate = 0, spot is sharp, no drop-off.
      This can be expressed precisely by BeamWidth = SpotCutOffAngle.
      For other SpotDropOffRate, there's no way to properly translate
      to beamWidth. }
    if SpotDropOffRate = 0 then
      TNodeSpotLight_2(ActiveLightNode).FdBeamWidth.Value := SpotCutOffAngle else
      TNodeSpotLight_2(ActiveLightNode).FdBeamWidth.Value := SpotCutOffAngle / 2;
    TNodeSpotLight_2(ActiveLightNode).FdCutOffAngle.Value := SpotCutOffAngle;

    { infinite radius }
    TNodeSpotLight_2(ActiveLightNode).FdRadius.Value := MaxSingle;
    TNodeSpotLight_2(ActiveLightNode).FdAttenuation.Value := Attenuation;
  end else
  begin
    ActiveLightNode := TNodeDirectionalLight_2.Create('', '');
    TNodeDirectionalLight_2(ActiveLightNode).FdDirection.Value := Direction;
  end;

  ActiveLightNode.FdAmbientIntensity.Value := AmbientIntensity;
  ActiveLightNode.FdColor.Value := Color;
  ActiveLightNode.FdIntensity.Value := Intensity;

  Result.LightNode := ActiveLightNode;

  Result.Transform := IdentityMatrix4Single;
  Result.TransformScale := 1;

  Result.TransfLocation := Position;
  Result.TransfNormDirection := Normalized(Direction);
  Result.TransfRadius := MaxSingle;
end;

end.
