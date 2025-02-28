{ -*- buffer-read-only: t -*-

  Copyright 2015-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Automatically generated node properties.

  Do not edit this file manually!
  To add new properties:
  - add them to the text files in tools/internal/x3d-nodes-to-pascal/nodes-specification/ ,
  - and regenerate include files by running x3d-nodes-to-pascal }

{$ifdef read_interface}

  strict private
    function GetAnisotropicDegree: Single;
    procedure SetAnisotropicDegree(const Value: Single);
    function GetBorderColor: TCastleColor;
    procedure SetBorderColor(const Value: TCastleColor);
    function GetBorderWidth: Integer;
    procedure SetBorderWidth(const Value: Integer);
    function GetBoundaryModeS: TBoundaryMode;
    procedure SetBoundaryModeS(const Value: TBoundaryMode);
    function GetBoundaryModeT: TBoundaryMode;
    procedure SetBoundaryModeT(const Value: TBoundaryMode);
    function GetBoundaryModeR: TBoundaryMode;
    procedure SetBoundaryModeR(const Value: TBoundaryMode);
    function GetTextureCompression: String;
    procedure SetTextureCompression(const Value: String);
    function GetTexturePriority: Single;
    procedure SetTexturePriority(const Value: Single);
    function GetGenerateMipMaps: Boolean;
    procedure SetGenerateMipMaps(const Value: Boolean);
    function GetGuiTexture: Boolean;
    procedure SetGuiTexture(const Value: Boolean);
  public
    { Create node fields and events. }
    procedure CreateNode; override;
    class function ClassX3DType: String; override;

    strict private FFdAnisotropicDegree: TSFFloat;
    { Internal wrapper for property @code(AnisotropicDegree). This wrapper API may change, we advise to access simpler @code(AnisotropicDegree) instead, if it is defined (TODO: for now, some field types do not have a simpler counterpart). }
    public property FdAnisotropicDegree: TSFFloat read FFdAnisotropicDegree;
    {  }
    property AnisotropicDegree: Single read GetAnisotropicDegree write SetAnisotropicDegree;

    strict private FFdBorderColor: TSFColorRGBA;
    { Internal wrapper for property @code(BorderColor). This wrapper API may change, we advise to access simpler @code(BorderColor) instead, if it is defined (TODO: for now, some field types do not have a simpler counterpart). }
    public property FdBorderColor: TSFColorRGBA read FFdBorderColor;
    {  }
    property BorderColor: TCastleColor read GetBorderColor write SetBorderColor;

    strict private FFdBorderWidth: TSFInt32;
    { Internal wrapper for property @code(BorderWidth). This wrapper API may change, we advise to access simpler @code(BorderWidth) instead, if it is defined (TODO: for now, some field types do not have a simpler counterpart). }
    public property FdBorderWidth: TSFInt32 read FFdBorderWidth;
    {  }
    property BorderWidth: Integer read GetBorderWidth write SetBorderWidth;

    strict private FFdBoundaryModeS: TSFStringEnum;
    { Internal wrapper for property @code(BoundaryModeS). This wrapper API may change, we advise to access simpler @code(BoundaryModeS) instead, if it is defined (TODO: for now, some field types do not have a simpler counterpart). }
    public property FdBoundaryModeS: TSFStringEnum read FFdBoundaryModeS;
    {  }
    property BoundaryModeS: TBoundaryMode read GetBoundaryModeS write SetBoundaryModeS;

    strict private FFdBoundaryModeT: TSFStringEnum;
    { Internal wrapper for property @code(BoundaryModeT). This wrapper API may change, we advise to access simpler @code(BoundaryModeT) instead, if it is defined (TODO: for now, some field types do not have a simpler counterpart). }
    public property FdBoundaryModeT: TSFStringEnum read FFdBoundaryModeT;
    {  }
    property BoundaryModeT: TBoundaryMode read GetBoundaryModeT write SetBoundaryModeT;

    strict private FFdBoundaryModeR: TSFStringEnum;
    { Internal wrapper for property @code(BoundaryModeR). This wrapper API may change, we advise to access simpler @code(BoundaryModeR) instead, if it is defined (TODO: for now, some field types do not have a simpler counterpart). }
    public property FdBoundaryModeR: TSFStringEnum read FFdBoundaryModeR;
    {  }
    property BoundaryModeR: TBoundaryMode read GetBoundaryModeR write SetBoundaryModeR;

    strict private FFdMagnificationFilter: TSFString;
    { Internal wrapper for property @code(MagnificationFilter). This wrapper API may change, we advise to access simpler @code(MagnificationFilter) instead, if it is defined (TODO: for now, some field types do not have a simpler counterpart). }
    public property FdMagnificationFilter: TSFString read FFdMagnificationFilter;

    strict private FFdMinificationFilter: TSFString;
    { Internal wrapper for property @code(MinificationFilter). This wrapper API may change, we advise to access simpler @code(MinificationFilter) instead, if it is defined (TODO: for now, some field types do not have a simpler counterpart). }
    public property FdMinificationFilter: TSFString read FFdMinificationFilter;

    strict private FFdTextureCompression: TSFString;
    { Internal wrapper for property @code(TextureCompression). This wrapper API may change, we advise to access simpler @code(TextureCompression) instead, if it is defined (TODO: for now, some field types do not have a simpler counterpart). }
    public property FdTextureCompression: TSFString read FFdTextureCompression;
    {  }
    property TextureCompression: String read GetTextureCompression write SetTextureCompression;

    strict private FFdTexturePriority: TSFFloat;
    { Internal wrapper for property @code(TexturePriority). This wrapper API may change, we advise to access simpler @code(TexturePriority) instead, if it is defined (TODO: for now, some field types do not have a simpler counterpart). }
    public property FdTexturePriority: TSFFloat read FFdTexturePriority;
    {  }
    property TexturePriority: Single read GetTexturePriority write SetTexturePriority;

    strict private FFdGenerateMipMaps: TSFBool;
    { Internal wrapper for property @code(GenerateMipMaps). This wrapper API may change, we advise to access simpler @code(GenerateMipMaps) instead, if it is defined (TODO: for now, some field types do not have a simpler counterpart). }
    public property FdGenerateMipMaps: TSFBool read FFdGenerateMipMaps;
    {  }
    property GenerateMipMaps: Boolean read GetGenerateMipMaps write SetGenerateMipMaps;

    strict private FFdGuiTexture: TSFBool;
    { Internal wrapper for property @code(GuiTexture). This wrapper API may change, we advise to access simpler @code(GuiTexture) instead, if it is defined (TODO: for now, some field types do not have a simpler counterpart). }
    public property FdGuiTexture: TSFBool read FFdGuiTexture;
    {  }
    property GuiTexture: Boolean read GetGuiTexture write SetGuiTexture;

{$endif read_interface}

{$ifdef read_implementation}

{ TTexturePropertiesNode ----------------------------------------------- }

function TTexturePropertiesNode.GetAnisotropicDegree: Single;
begin
  Result := FdAnisotropicDegree.Value;
end;

procedure TTexturePropertiesNode.SetAnisotropicDegree(const Value: Single);
begin
  FdAnisotropicDegree.Send(Value);
end;

function TTexturePropertiesNode.GetBorderColor: TCastleColor;
begin
  Result := FdBorderColor.Value;
end;

procedure TTexturePropertiesNode.SetBorderColor(const Value: TCastleColor);
begin
  FdBorderColor.Send(Value);
end;

function TTexturePropertiesNode.GetBorderWidth: Integer;
begin
  Result := FdBorderWidth.Value;
end;

procedure TTexturePropertiesNode.SetBorderWidth(const Value: Integer);
begin
  FdBorderWidth.Send(Value);
end;

function TTexturePropertiesNode.GetBoundaryModeS: TBoundaryMode;
begin
  Result := TBoundaryMode(FdBoundaryModeS.EnumValue);
end;

procedure TTexturePropertiesNode.SetBoundaryModeS(const Value: TBoundaryMode);
begin
  FdBoundaryModeS.SendEnumValue(Ord(Value));
end;

function TTexturePropertiesNode.GetBoundaryModeT: TBoundaryMode;
begin
  Result := TBoundaryMode(FdBoundaryModeT.EnumValue);
end;

procedure TTexturePropertiesNode.SetBoundaryModeT(const Value: TBoundaryMode);
begin
  FdBoundaryModeT.SendEnumValue(Ord(Value));
end;

function TTexturePropertiesNode.GetBoundaryModeR: TBoundaryMode;
begin
  Result := TBoundaryMode(FdBoundaryModeR.EnumValue);
end;

procedure TTexturePropertiesNode.SetBoundaryModeR(const Value: TBoundaryMode);
begin
  FdBoundaryModeR.SendEnumValue(Ord(Value));
end;

function TTexturePropertiesNode.GetTextureCompression: String;
begin
  Result := FdTextureCompression.Value;
end;

procedure TTexturePropertiesNode.SetTextureCompression(const Value: String);
begin
  FdTextureCompression.Send(Value);
end;

function TTexturePropertiesNode.GetTexturePriority: Single;
begin
  Result := FdTexturePriority.Value;
end;

procedure TTexturePropertiesNode.SetTexturePriority(const Value: Single);
begin
  FdTexturePriority.Send(Value);
end;

function TTexturePropertiesNode.GetGenerateMipMaps: Boolean;
begin
  Result := FdGenerateMipMaps.Value;
end;

procedure TTexturePropertiesNode.SetGenerateMipMaps(const Value: Boolean);
begin
  FdGenerateMipMaps.Send(Value);
end;

function TTexturePropertiesNode.GetGuiTexture: Boolean;
begin
  Result := FdGuiTexture.Value;
end;

procedure TTexturePropertiesNode.SetGuiTexture(const Value: Boolean);
begin
  FdGuiTexture.Send(Value);
end;

class function TTexturePropertiesNode.ClassX3DType: String;
begin
  Result := 'TextureProperties';
end;

procedure TTexturePropertiesNode.CreateNode;
begin
  inherited;

  FFdAnisotropicDegree := TSFFloat.Create(Self, True, 'anisotropicDegree', 1.0);
   FdAnisotropicDegree.ChangeAlways := chTexturePropertiesNode;
  AddField(FFdAnisotropicDegree);

  FFdBorderColor := TSFColorRGBA.Create(Self, True, 'borderColor', Vector4(0, 0, 0, 0));
   FdBorderColor.ChangeAlways := chTexturePropertiesNode;
  AddField(FFdBorderColor);

  FFdBorderWidth := TSFInt32.Create(Self, True, 'borderWidth', 0);
   FdBorderWidth.ChangeAlways := chTexturePropertiesNode;
   FdBorderWidth.MustBeNonnegative := true;
  AddField(FFdBorderWidth);

  FFdBoundaryModeS := TSFStringEnum.Create(Self, True, 'boundaryModeS', @BoundaryModeNames, Ord(High(BoundaryModeNames)) + 1, Ord(bmRepeat));
   FdBoundaryModeS.ChangeAlways := chTexturePropertiesNode;
  AddField(FFdBoundaryModeS);

  FFdBoundaryModeT := TSFStringEnum.Create(Self, True, 'boundaryModeT', @BoundaryModeNames, Ord(High(BoundaryModeNames)) + 1, Ord(bmRepeat));
   FdBoundaryModeT.ChangeAlways := chTexturePropertiesNode;
  AddField(FFdBoundaryModeT);

  FFdBoundaryModeR := TSFStringEnum.Create(Self, True, 'boundaryModeR', @BoundaryModeNames, Ord(High(BoundaryModeNames)) + 1, Ord(bmRepeat));
   FdBoundaryModeR.ChangeAlways := chTexturePropertiesNode;
  AddField(FFdBoundaryModeR);

  FFdMagnificationFilter := TSFString.Create(Self, True, 'magnificationFilter', 'DEFAULT');
   FdMagnificationFilter.ChangeAlways := chTexturePropertiesNode;
  AddField(FFdMagnificationFilter);

  FFdMinificationFilter := TSFString.Create(Self, True, 'minificationFilter', 'DEFAULT');
   FdMinificationFilter.ChangeAlways := chTexturePropertiesNode;
  AddField(FFdMinificationFilter);

  FFdTextureCompression := TSFString.Create(Self, True, 'textureCompression', 'FASTEST');
   FdTextureCompression.ChangeAlways := chTexturePropertiesNode;
  AddField(FFdTextureCompression);

  FFdTexturePriority := TSFFloat.Create(Self, True, 'texturePriority', 0);
   FdTexturePriority.ChangeAlways := chTexturePropertiesNode;
   FdTexturePriority.MustBeNonnegative := true;
  AddField(FFdTexturePriority);

  FFdGenerateMipMaps := TSFBool.Create(Self, False, 'generateMipMaps', false);
   FdGenerateMipMaps.ChangeAlways := chTexturePropertiesNode;
  AddField(FFdGenerateMipMaps);

  FFdGuiTexture := TSFBool.Create(Self, False, 'guiTexture', false);
   FdGuiTexture.ChangeAlways := chTexturePropertiesNode;
  AddField(FFdGuiTexture);

  DefaultContainerField := 'textureProperties';
end;

{$endif read_implementation}
