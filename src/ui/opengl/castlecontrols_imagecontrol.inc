{
  Copyright 2010-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Image control.
    Size is automatically adjusted to the image size, if Stretch is @false (default).
    You should set TCastleImageControl.Left, TCastleImageControl.Bottom properties,
    and load your image by setting TCastleImageControl.URL property
    or straight TCastleImageControl.Image.

    We automatically use alpha test or alpha blending based
    on loaded image alpha channel (see TGLImageCore.Alpha).
    You can influence this by @link(AlphaChannel) property. }
  TCastleImageControl = class(TUIControl)
  strict private
    FURL: string;
    FImage: TCastleImage;
    FGLImage: TGLImageCore;
    FAlphaChannel: TAutoAlphaChannel;
    FStretch: boolean;
    FProportional: boolean;
    FFullSize: boolean;
    FWidth: Cardinal;
    FHeight: Cardinal;
    FColor: TCastleColor;
    FCorners: TVector4Integer;
    FOwnsImage: boolean;
    FSmoothScaling: boolean;
    FCenterX: Single;
    FCenterY: Single;
    FRotation: Single;
    FClip: boolean;
    FClipLine: TVector3;
    procedure SetCenterX(const AValue: Single);
    procedure SetCenterY(const AValue: Single);
    procedure SetRotation(const AValue: Single);
    procedure SetURL(const Value: string);
    procedure SetImage(const Value: TCastleImage);
    procedure SetAlphaChannel(const Value: TAutoAlphaChannel);
    function GetBlending: boolean;
    procedure SetBlending(const Value: boolean);
    procedure SetStretch(const Value: boolean);
    procedure SetWidth(const Value: Cardinal);
    procedure SetHeight(const Value: Cardinal);
    procedure SetFullSize(const Value: boolean);
    procedure SetProportional(const Value: boolean);
    procedure SetColor(const Value: TCastleColor);
    procedure SetSmoothScaling(const Value: boolean);
    procedure SetClip(const Value: boolean);
    procedure SetClipLine(const Value: TVector3);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
    procedure GLContextOpen; override;
    procedure GLContextClose; override;
    function Rect: TRectangle; override;
    procedure ImageChanged;

    { Image displayed, or @nil if none. You can set it by setting @link(URL),
      or you can set this property directly if you loaded/created the image contents
      yourself.

      Note that by default the TCastleImage instance assigned here is owned
      by this component (see @link(OwnsImage)).
      So if you set this property to your custom TCastleImage instance you should
      leave memory management of this instance to this component.
      You can either create a copy by TCastleImage.MakeCopy
      if you want to give here only a copy, or you can change @link(OwnsImage)
      to @false.

      It is allowed to modify the contents or even size of this image.
      Just make sure to call ImageChanged after the modifications are done
      to update the actual rendered image.
      The control size will be updated immediately (taking into account current
      @link(Stretch) and related properties values). }
    property Image: TCastleImage read FImage write SetImage;

    { Whether the memory management of assigned @link(Image) is automatic.
      See @link(Image) documentation for details. }
    property OwnsImage: boolean read FOwnsImage write FOwnsImage default true;

    { Color tint of the image. This simply multiplies the image RGBA components,
      just like @link(TGLImageCore.Color). By default this is opaque white,
      which means that image colors are unchanged. }
    property Color: TCastleColor read FColor write SetColor;

    { Corners of the image that are not stretched even
      in case @link(Stretch) is used.
      See @link(TGLImageCore.Draw3x3) for the details how drawing image
      with borders work. }
    property Corners: TVector4Integer read FCorners write FCorners;

    { X coordinate of the center of rotation. Value from 0 to 1. Default value 0.5. }
    property CenterX: Single read FCenterX write SetCenterX default 0.5;

    { Y coordinate of the center of rotation. Value from 0 to 1. Default value 0.5. }
    property CenterY: Single read FCenterY write SetCenterY default 0.5;

    { Rotation in radians. Default value 0. }
    property Rotation: Single read FRotation write SetRotation default 0;

    { Clip the image by an arbitrary 2D line defined in @link(ClipLine). }
    property Clip: boolean read FClip write SetClip;

    { If @link(Clip), this is the line equation used to determine whether
      we clip the given pixel. Given a line (A, B, C) and pixel (x, y),
      the pixel is clipped (rejected) if @code(A * x + B * y + C < 0).

      The equation is calculated in the final scaled screen coordinates
      (not in the local, unscaled pixels). Adjust it to match the ScreenRect
      if necessary, to make it work with UI scaling. }
    property ClipLine: TVector3 read FClipLine write SetClipLine;
  published
    { URL of the image. Setting this also sets @link(Image).
      Set this to '' to clear the image. }
    property URL: string read FURL write SetURL;
    { Deprecated name for @link(URL). }
    property FileName: string read FURL write SetURL; deprecated;
    { How to treat alpha channel of the assigned image.
      By default, this is acAuto, which means that image contents
      together with current @link(Color) determine how
      the alpha of image is treated (opaque, alpha test, alpha blending).
      Set this to force specific treatment. }
    property AlphaChannel: TAutoAlphaChannel
      read FAlphaChannel write SetAlphaChannel default acAuto;
    { Deprecated, use more flexible AlphaChannel instead. }
    property Blending: boolean read GetBlending write SetBlending stored false; deprecated 'use AlphaChannel';

    { Is the image scaling mode smooth (bilinear filtering)
      or not (nearest-pixel filtering).
      See @link(TGLImageCore.SmoothScaling). }
    property SmoothScaling: boolean
      read FSmoothScaling write SetSmoothScaling default true;

    { Size of the image control.

      If Stretch = @false, then values you set for Width, Height, FullSize,
      Proportional properties do not matter (they are still remembered though,
      so you can set properties in any order).
      The displayed size (you can check it through @link(Rect) function)
      always corresponds to the underlying image size.
      The Left and Bottom properties work as usual, they allow you to move the control.

      If Stretch = @true, then the image will be stretched to fill the requested area.
      @unorderedList(
        @item(If Stretch = @true and FullSize = @true then values of Width,
          Height, Left, Bottom do not matter:
          image always fills the whole parent
          (@link(Rect) corresponds to the parent area).)

        @item(Otherwise, if Stretch = @true and Proportional = @true,
          then the image will be proportionally scaled to fit within
          the requested Width and Height. If the aspect ratio of image
          will be different than aspect ratio of Width/Height, the scaled image
          will be centered inside the Width/Height.)

        @item(Otherwise, if Stretch = @true but no other condition
          (so FullSize = @false and Proportional = @false)
          then the image will be scaled to exactly fill
          the requested Width and Height
          (without paying attention to the aspect ratio of the image).

          This is the case when you fully force the displayed size
          and position, regardless of image size. Displayed image will
          always exactly fill the requested area.
        )
      )

      Note that you can always look at @link(Rect) value to know
      the current calculated size and position of the image control on screen.

      @groupBegin }
    property Stretch: boolean read FStretch write SetStretch default false;
    property Width: Cardinal read FWidth write SetWidth default 0;
    property Height: Cardinal read FHeight write SetHeight default 0;
    property FullSize: boolean read FFullSize write SetFullSize default false;
    property Proportional: boolean read FProportional write SetProportional default false;
    { @groupEnd }
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleImageControl ---------------------------------------------------------------- }

constructor TCastleImageControl.Create(AOwner: TComponent);
begin
  inherited;
  FColor := White;
  FOwnsImage := true;
  FSmoothScaling := true;
  FCenterX := 0.5;
  FCenterY := 0.5;
  FRotation := 0;
end;

destructor TCastleImageControl.Destroy;
begin
  if OwnsImage then
    FreeAndNil(FImage) else
    FImage := nil;
  FreeAndNil(FGLImage);
  inherited;
end;

procedure TCastleImageControl.SetSmoothScaling(const Value: boolean);
begin
  if FSmoothScaling <> Value then
  begin
    FSmoothScaling := Value;
    if FGLImage <> nil then
    begin
      FGLImage.SmoothScaling := Value;
      VisibleChange;
    end;
  end;
end;

procedure TCastleImageControl.SetClip(const Value: boolean);
begin
  if FClip <> Value then
  begin
    FClip := Value;
    if FGLImage <> nil then
    begin
      FGLImage.Clip := Value;
      VisibleChange;
    end;
  end;
end;

procedure TCastleImageControl.SetClipLine(const Value: TVector3);
begin
  if not VectorsPerfectlyEqual(FClipLine, Value) then
  begin
    FClipLine := Value;
    if FGLImage <> nil then
    begin
      FGLImage.ClipLine := Value;
      VisibleChange;
    end;
  end;
end;

procedure TCastleImageControl.SetURL(const Value: string);
begin
  if Value <> '' then
    Image := LoadImage(Value) else
    Image := nil;

  { only once new Image is successfully loaded, change property value.
    If LoadImage raised exception, URL will remain unchanged. }
  FURL := Value;
end;

procedure TCastleImageControl.SetCenterX(const AValue: Single);
begin
  if FCenterX <> AValue then
  begin
    FCenterX := AValue;
    if FGLImage <> nil then
    begin
      FGLImage.CenterX := FCenterX;
      VisibleChange;
    end;
  end;
end;

procedure TCastleImageControl.SetCenterY(const AValue: Single);
begin
  if FCenterY <> AValue then
  begin
    FCenterY := AValue;
    if FGLImage <> nil then
    begin
      FGLImage.CenterY := FCenterY;
      VisibleChange;
    end;
  end;
end;

procedure TCastleImageControl.SetRotation(const AValue: Single);
begin
  if FRotation <> AValue then
  begin
    FRotation := AValue;
    if FGLImage <> nil then
    begin
      FGLImage.Rotation := FRotation;
      VisibleChange;
    end;
  end;
end;

procedure TCastleImageControl.SetImage(const Value: TCastleImage);
begin
  if FImage <> Value then
  begin
    if OwnsImage then FreeAndNil(FImage);
    FImage := Value;
    ImageChanged;
  end;
end;

procedure TCastleImageControl.Render;
var
  SR: TRectangle;
begin
  inherited;
  if FGLImage = nil then Exit;
  SR := ScreenRect;
  if ZeroVector(FCorners) then
    FGLImage.Draw(SR) else
  begin
    FGLImage.ScaleCorners := UIScale;
    FGLImage.Draw3x3(SR, FCorners);
  end;
  { Useful to debug that Proportional works.
  if Stretch and not FullSize then
    Theme.Draw(Rectangle(Left, Bottom, Width, Height), tiActiveFrame, UIScale); }
end;

function TCastleImageControl.Rect: TRectangle;
var
  NewWidth, NewHeight, NewLeft, NewBottom: Integer;
  ApplyScaling: boolean;
begin
  ApplyScaling := true;
  if not Stretch then
  begin
    if FImage <> nil then
      Result := Rectangle(Left, Bottom, FImage.Width, FImage.Height) else
      Result := Rectangle(Left, Bottom, 0, 0);
  end else
  begin
    if FullSize then
    begin
      Result := ParentRect;
      ApplyScaling := false;
    end else
    if Proportional and (FImage <> nil) then
    begin
      if Width / Height > FImage.Width / FImage.Height then
      begin
        NewWidth := FImage.Width * Height div FImage.Height;
        NewLeft := Left + (Width - NewWidth) div 2;
        Result := Rectangle(NewLeft, Bottom, NewWidth, Height);
      end else
      begin
        NewHeight := FImage.Height * Width div FImage.Width;
        NewBottom := Bottom + (Height - NewHeight) div 2;
        Result := Rectangle(Left, NewBottom, Width, NewHeight);
      end;
    end else
      Result := Rectangle(Left, Bottom, Width, Height);
  end;

  if ApplyScaling then
    // applying UIScale on this is easy...
    Result := Result.ScaleAround0(UIScale);
end;

procedure TCastleImageControl.GLContextOpen;
begin
  inherited;
  if FGLImage = nil then
    ImageChanged;
end;

procedure TCastleImageControl.GLContextClose;
begin
  FreeAndNil(FGLImage);
  inherited;
end;

procedure TCastleImageControl.ImageChanged;
begin
  if GLInitialized then
  begin
    if FImage <> nil then
    begin
      if FGLImage <> nil then
        FGLImage.Load(FImage) else
      begin
        FGLImage := TGLImageCore.Create(FImage, FSmoothScaling);
        FGLImage.Color := Color;
        FGLImage.CenterX := FCenterX;
        FGLImage.CenterY := FCenterY;
        FGLImage.Rotation := FRotation;
        FGLImage.Clip := FClip;
        FGLImage.ClipLine := FClipLine;
        FGLImage.Alpha := AlphaChannel;
      end;
    end else
      FreeAndNil(FGLImage); // make sure to free FGLImage when FImage is nil
    VisibleChange;
  end;
end;

procedure TCastleImageControl.SetAlphaChannel(const Value: TAutoAlphaChannel);
begin
  if FAlphaChannel <> Value then
  Begin
    FAlphaChannel := Value;
    if FGLImage <> nil then
    begin
      FGLImage.Alpha := Value;
      VisibleChange;
    end;
  end;
end;

function TCastleImageControl.GetBlending: boolean;
begin
  Result := AlphaChannel <> acBlending;
end;

procedure TCastleImageControl.SetBlending(const Value: boolean);
begin
  if Value then
    AlphaChannel := acBlending else
    AlphaChannel := acTest;
end;

procedure TCastleImageControl.SetStretch(const Value: boolean);
begin
  if FStretch <> Value then
  begin
    FStretch := Value;
    VisibleChange;
  end;
end;

procedure TCastleImageControl.SetProportional(const Value: boolean);
begin
  if FProportional <> Value then
  begin
    FProportional := Value;
    VisibleChange;
  end;
end;

procedure TCastleImageControl.SetColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FColor, Value) then
  begin
    FColor := Value;
    if FGLImage <> nil then
    begin
      FGLImage.Color := Value;
      VisibleChange;
    end;
  end;
end;

procedure TCastleImageControl.SetWidth(const Value: Cardinal);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    VisibleChange;
  end;
end;

procedure TCastleImageControl.SetHeight(const Value: Cardinal);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    VisibleChange;
  end;
end;

procedure TCastleImageControl.SetFullSize(const Value: boolean);
begin
  if FFullSize <> Value then
  begin
    FFullSize := Value;
    VisibleChange;
  end;
end;

{$endif read_implementation}
