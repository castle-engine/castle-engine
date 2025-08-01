{%MainUnit x3dnodes.pas}
{
  Copyright 2002-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Extra types related to materials. }

{$ifdef read_interface}

  { Abstract class for material information,
    that define material properties for various lighting equations.
    Particular public descendants define different lighting equations.
    Particular private descendants map X3D nodes' fields to these parameters. }
  TMaterialInfo = class abstract
  strict private
    FNode: TX3DNode;
    function GetOpacity: Single;
    procedure SetOpacity(const Value: Single);
  protected
    function GetMainColor: TVector3; virtual; abstract;
    procedure SetMainColor(const Value: TVector3); virtual; abstract;
    function GetEmissiveColor: TVector3; virtual; abstract;
    procedure SetEmissiveColor(const Value: TVector3); virtual; abstract;
    function GetTransparency: Single; virtual; abstract;
    procedure SetTransparency(const Value: Single); virtual; abstract;
  public
    const
      DefaultTransparency = 0.0;

    constructor Create(ANode: TX3DNode);

    { Associated material node. }
    property Node: TX3DNode read FNode;

    { Transparency of the material determines how much is it "see-through".
      Value 0 (DefaultTransparency) means fully opaque, 1 means fully transparent. }
    property Transparency: Single read GetTransparency write SetTransparency;

    { Opacity of the material, just @code(1 - @link(Transparency)).
      Sometimes it is more intuitive to use this than Transparency. }
    property Opacity: Single read GetOpacity write SetOpacity;

    { Single color that is the best approximation of this material look.

      For Phong material, it's DiffuseColor. For unlit material, it's EmissiveColor.
      For physical material, it's BaseColor.

      This is useful for debug purposes,
      for example when we force everything to be unlit by Attributes.Lighting=false.
      It doesn't follow any X3D official equations, it's only for debugging. }
    property MainColor: TVector3 read GetMainColor write SetMainColor;

    { Emissive color represents the color of the material
      that is not affected by any light sources.
      All material types (unlit, Phong, physical) have this color. }
    property EmissiveColor: TVector3 read GetEmissiveColor write SetEmissiveColor;

    { Texture that determines the primary RGB color and transparency (opacity).
      See TX3DGraphTraverseState.MainTexture, which is really a shortcut
      to call this.

      @groupBegin }
    function MainTexture(
      const Appearance: TAppearanceNode;
      const VRML1State: TVRML1State): TAbstractTextureNode; overload; virtual; abstract;
    function MainTexture(
      const Appearance: TAppearanceNode;
      const VRML1State: TVRML1State;
      const GeometryForTexCoord: TAbstractGeometryNode;
      out TexCoordIndex: Integer): TAbstractTextureNode; overload; virtual; abstract;
    { @groupEnd }
  end;

  { Material information that defines parameters for Phong lighting equations.
    This hides differences between various material nodes,
    like TMaterialNode and TCommonSurfaceShaderNode. }
  TPhongMaterialInfo = class(TMaterialInfo)
  protected
    function GetDiffuseColor: TVector3; virtual; abstract;
    procedure SetDiffuseColor(const Value: TVector3); virtual; abstract;
    function GetSpecularColor: TVector3; virtual; abstract;
    procedure SetSpecularColor(const Value: TVector3); virtual; abstract;
    { Main color of Phong material is @link(DiffuseColor). }
    function GetMainColor: TVector3; override;
    procedure SetMainColor(const Value: TVector3); override;
  public
    const
      { Default material parameters.

        They luckily match between all the material-like nodes:
        @unorderedList(
          @itemSpacing Compact
          @item X3D and VRML 2.0: TMaterialNode, TTwoSidedMaterialNode
          @item VRML 1.0: TMaterialNode_1
          @item Extension: TCommonSurfaceShaderNode
        )

        The AmbientColor is an exception.
        In X3D the effective AmbientColor is an AmbientIntensity * DiffuseColor,
        thus it has a default value of
        @code(DefaultAmbientIntensity * DefaultiffuseColor = (0.2 * 0.8, 0.2 * 0.8, 0.2 * 0.8)).
      }
      DefaultAmbientColor: TVector3 = (X: 0.2; Y: 0.2; Z: 0.2);
      DefaultAmbientIntensity = 0.2;
      DefaultDiffuseColor: TVector3 = (X: 0.8; Y: 0.8; Z: 0.8);
      DefaultSpecularColor: TVector3 = (X: 0; Y: 0; Z: 0);
      DefaultEmissiveColor: TVector3 = (X: 0; Y: 0; Z: 0);
      DefaultShininess = 0.2;
      DefaultShininessExp = DefaultShininess * 128;
      DefaultReflectionColor: TVector3 = (X: 0; Y: 0; Z: 0);
      DefaultTransmissionColor: TVector3 = (X: 0; Y: 0; Z: 0);
      DefaultReflSpecularExp = 1000000;
      DefaultTransSpecularExp = 1000000;

    function AmbientColor: TVector3; virtual; abstract;
    property DiffuseColor: TVector3 read GetDiffuseColor write SetDiffuseColor;
    property SpecularColor: TVector3 read GetSpecularColor write SetSpecularColor;

    { Only the emissiveColor is not black (zero),
      which means that the material behaves like unlit.

      This checks that ambient and diffuse and specular colors are all zero.
      It's an important information about the material sometimes.

      This way of achieving "something like unlit material" has issues, see
      https://github.com/michaliskambi/x3d-tests/wiki/Why-is-UnlitMaterial-useful .
      For this reason, we recommend to use (and detect, for optimization)
      the TUnlitMaterialNode instead of this "special case of Phong material". }
    function PureEmissive: boolean; deprecated 'use TUnlitMaterialNode for unlit materials';

    function Shininess: Single; virtual; abstract;

    { Shininess exponent for Phong lighting equations.

      Remember that the X3D @link(Shininess) field is "normalized",
      which means that it has to be multiplied by 128.0 to get
      actual exponent for lighting equations.
      This function returns the real exponent
      (already multiplied by 128.0, if necessary). }
    function ShininessExp: Single;

    function ReflectionColor: TVector3; virtual; abstract;
    function TransmissionColor: TVector3; virtual;

    { Used by path-tracer.
      See https://castle-engine.io/x3d_extensions.php#section_ext_material_phong_brdf_fields .
      Not useful for normal CGE rendering.
      @deprecated }
    function ReflSpecular: TVector3; virtual;
    function ReflDiffuse: TVector3; virtual;
    function TransSpecular: TVector3; virtual;
    function TransDiffuse: TVector3; virtual;
    function ReflSpecularExp: Single; virtual;
    function TransSpecularExp: Single; virtual;
  end;

  TX3DMaterialInfoAbstract = TMaterialInfo deprecated 'use TMaterialInfo';

  { Material information that defines parameters for unlit lighting equations. }
  TUnlitMaterialInfo = class(TMaterialInfo)
  protected
    { Main color of unlit material is @link(EmissiveColor). }
    function GetMainColor: TVector3; override;
    procedure SetMainColor(const Value: TVector3); override;
  end;

  { Material information that defines parameters for physical lighting equations. }
  TPhysicalMaterialInfo = class(TMaterialInfo)
  protected
    { Main color of PBR material is @link(BaseColor). }
    function GetMainColor: TVector3; override;
    procedure SetMainColor(const Value: TVector3); override;
    function GetBaseColor: TVector3; virtual; abstract;
    procedure SetBaseColor(const Value: TVector3); virtual; abstract;
  public
    { The most important color of the physical material. }
    property BaseColor: TVector3 read GetBaseColor write SetBaseColor;
    function Metallic: Single; virtual; abstract;
    function Roughness: Single; virtual; abstract;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TMaterialInfo ---------------------------------------------------------- }

constructor TMaterialInfo.Create(ANode: TX3DNode);
begin
  inherited Create;
  FNode := ANode;
end;

function TMaterialInfo.GetOpacity: Single;
begin
  Result := 1 - Transparency;
end;

procedure TMaterialInfo.SetOpacity(const Value: Single);
begin
  Transparency := 1 - Value;
end;

{ TPhongMaterialInfo --------------------------------------------------------- }

function TPhongMaterialInfo.ShininessExp: Single;
begin
  { According to VRML / X3D specification, shininess must be within 0..1 range,
    so it maps nicely to 0..128 range for OpenGL's exponent.
    We do clamp to 0..128 since using values outside this range makes
    OpenGL errors in fixed-function pipeline. }

  Result := Clamped(Shininess * 128.0, 0.0, 128.0);
end;

function TPhongMaterialInfo.TransmissionColor: TVector3;
begin
  { default TransmisionColor implementation }
  Result := Vector3(Transparency, Transparency, Transparency);
end;

function TPhongMaterialInfo.PureEmissive: boolean;
begin
  Result :=
    AmbientColor.IsPerfectlyZero and
    DiffuseColor.IsPerfectlyZero and
    SpecularColor.IsPerfectlyZero;
end;

function TPhongMaterialInfo.ReflSpecular: TVector3;
begin
  { Default implementation calculates something sensible looking at other fields. }
  Result := ReflectionColor;
end;

function TPhongMaterialInfo.ReflDiffuse: TVector3;
begin
  { Default implementation calculates something sensible looking at other fields. }
  Result := DiffuseColor;
end;

function TPhongMaterialInfo.TransSpecular: TVector3;
begin
  { Default implementation calculates something sensible looking at other fields. }
  Result := TransmissionColor;
end;

function TPhongMaterialInfo.TransDiffuse: TVector3;
begin
  { Default implementation calculates something sensible looking at other fields. }
  Result := DiffuseColor * TransmissionColor;
end;

function TPhongMaterialInfo.ReflSpecularExp: Single;
begin
  Result := DefaultReflSpecularExp;
end;

function TPhongMaterialInfo.TransSpecularExp: Single;
begin
  Result := DefaultTransSpecularExp;
end;

function TPhongMaterialInfo.GetMainColor: TVector3;
begin
  Result := DiffuseColor;
end;

procedure TPhongMaterialInfo.SetMainColor(const Value: TVector3);
begin
  DiffuseColor := Value;
end;

{ TUnlitMaterialInfo --------------------------------------------------------- }

function TUnlitMaterialInfo.GetMainColor: TVector3;
begin
  Result := EmissiveColor;
end;

procedure TUnlitMaterialInfo.SetMainColor(const Value: TVector3);
begin
  EmissiveColor := Value;
end;

{ TPhysicalMaterialInfo --------------------------------------------------------- }

function TPhysicalMaterialInfo.GetMainColor: TVector3;
begin
  Result := BaseColor;
end;

procedure TPhysicalMaterialInfo.SetMainColor(const Value: TVector3);
begin
  BaseColor := Value;
end;

{$endif read_implementation}
