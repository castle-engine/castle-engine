{
  Copyright 2006-2009 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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
  inherited;
end;

end.
