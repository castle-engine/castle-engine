{
  Copyright 2008-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ CastleScript image types and built-in functions. }
unit CastleScriptImages;

{$I castleconf.inc}

interface

uses CastleVectors, CastleScript, CastleImages;

{$ifdef CASTLE_SCRIPT_FPC} // TODO: Depends on CastleVectors, which is not for Delphi now

type
  TCasScriptImage = class(TCasScriptValue)
  private
    class procedure HandleImage(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleImageLoad(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class procedure HandleImageWidth(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleImageHeight(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleImageComponents(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class procedure HandleImageGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleImageGetColor(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleImageGetAlpha(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class procedure HandleImageSet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleImageSetColor(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleImageSetAlpha(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

  private
    FValue: TCastleImage;
    procedure SetValue(const AValue: TCastleImage);
  public
    constructor Create(const AWriteable: boolean; const AValue: TCastleImage); overload;
    constructor Create(const AWriteable: boolean); overload; override;
    destructor Destroy; override;

    { Image value. Assigning here makes a @italic(copy) of the image. }
    property Value: TCastleImage read FValue write SetValue;

    procedure AssignValue(Source: TCasScriptValue); override;
  end;

  TCasScriptImageFun = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptImageLoad = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptImageWidth = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptImageHeight = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptImageComponents = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptImageGet = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptImageGetColor = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptImageGetAlpha = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptImageSet = class(TCasScriptFunction)
  protected
    procedure CheckArguments; override;
  public
    class function ShortName: string; override;
  end;

  TCasScriptImageSetColor = class(TCasScriptFunction)
  protected
    procedure CheckArguments; override;
  public
    class function ShortName: string; override;
  end;

  TCasScriptImageSetAlpha = class(TCasScriptFunction)
  protected
    procedure CheckArguments; override;
  public
    class function ShortName: string; override;
  end;

{$endif CASTLE_SCRIPT_FPC}

implementation

uses SysUtils, CastleUtils, CastleLog, CastleScriptVectors, CastleURIUtils;

{$ifdef CASTLE_SCRIPT_FPC} // TODO: Depends on CastleVectors, which is not for Delphi now

{ TCasScriptImage ------------------------------------------------------------ }

constructor TCasScriptImage.Create(const AWriteable: boolean; const AValue: TCastleImage);
begin
  Create(AWriteable);

  { First create a dummy empty TRGBImage in default
    TCasScriptImage.Create(boolean) constructor, then SetValue will
    free it and replace with AValue copy. This is Ok --- calling
    Create(boolean) first is safer (it's virtual, and in case
    TCasScriptImage will have a descendant later it will be good)
    and creating/freeing temporary TRGBImage instance with zero size
    should not make a noticeable speed overhead. }

  Value := AValue;
end;

constructor TCasScriptImage.Create(const AWriteable: boolean);
begin
  inherited;
  FValue := TRGBImage.Create(0, 0);
end;

destructor TCasScriptImage.Destroy;
begin
  FreeAndNil(FValue);
  inherited;
end;

procedure TCasScriptImage.SetValue(const AValue: TCastleImage);
begin
  FreeAndNil(FValue);
  FValue := AValue.MakeCopy;
  ValueAssigned := true;
end;

procedure TCasScriptImage.AssignValue(Source: TCasScriptValue);
begin
  if Source is TCasScriptImage then
    Value := TCasScriptImage(Source).Value else
    raise ECasScriptAssignError.CreateFmt('Assignment from %s to %s not possible', [Source.ClassName, ClassName]);
end;

{ TCasScriptImage function handlers ------------------------------------------ }

class procedure TCasScriptImage.HandleImage(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  Components: Int64;
  Width, Height: Cardinal;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptImage);

  Components := TCasScriptInteger(Arguments[2]).Value;
  if not Between(Components, 1, 4) then
  begin
    WritelnWarning('CastleScript', '"image" function 3rd parameter (components) must be between 1 and 4');
    { We have to return something... Assume any valid Components value. }
    Components := Clamped(Components, 1, 4);
  end;

  Width := TCasScriptInteger(Arguments[0]).Value;
  Height := TCasScriptInteger(Arguments[1]).Value;

  FreeAndNil(TCasScriptImage(AResult).FValue);
  case Components of
    1: TCasScriptImage(AResult).FValue := TGrayscaleImage.Create(Width, Height);
    2: TCasScriptImage(AResult).FValue := TGrayscaleAlphaImage.Create(Width, Height);
    3: TCasScriptImage(AResult).FValue := TRGBImage.Create(Width, Height);
    4: TCasScriptImage(AResult).FValue := TRGBAlphaImage.Create(Width, Height);
    else raise EInternalError.CreateFmt('TCasScriptImage.AssignNewValue: Not allowed number of components: %d',
      [Components]);
  end;
  TCasScriptImage(AResult).ValueAssigned := true;
end;

class procedure TCasScriptImage.HandleImageLoad(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  FullUrl: string;
  NewImage: TCastleImage;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptImage);

  FullUrl := TCasScriptString(Arguments[0]).Value;
  if AFunction.Environment <> nil then
    FullUrl := CombineURI(AFunction.Environment.BaseUrl, FullUrl);

  try
    NewImage := LoadImage(FullUrl, [TRGBImage, TRGBAlphaImage]);
  except
    on E: Exception do
      raise ECasScriptError.Create('Exception ' + E.ClassName +
        ' occurred when trying to load ' +
        'image from URL "' + FullUrl + '" : ' + E.Message);
  end;

  FreeAndNil(TCasScriptImage(AResult).FValue);
  TCasScriptImage(AResult).FValue := NewImage;

  TCasScriptImage(AResult).ValueAssigned := true;
end;

class procedure TCasScriptImage.HandleImageWidth(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptInteger);
  TCasScriptInteger(AResult).Value := TCasScriptImage(Arguments[0]).Value.Width;
end;

class procedure TCasScriptImage.HandleImageHeight(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptInteger);
  TCasScriptInteger(AResult).Value := TCasScriptImage(Arguments[0]).Value.Height;
end;

class procedure TCasScriptImage.HandleImageComponents(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptInteger);
  TCasScriptInteger(AResult).Value := TCasScriptImage(Arguments[0]).Value.ColorComponentsCount;
end;

procedure CheckImageCoords(Image: TCasScriptImage; const X, Y: Int64);
var
  Ok: boolean;
begin
  Ok := Between(X, 0, Image.Value.Width  - 1) and
        Between(Y, 0, Image.Value.Height - 1);
  if not Ok then
    raise ECasScriptError.CreateFmt('Invalid image coordinates: (%d, %d), while image size is (%d, %d)',
      [X, Y, Image.Value.Width, Image.Value.Height]);
end;

procedure CheckImageColorAlphaType(Image: TCasScriptImage; Color: TCasScriptValue);
var
  Ok: boolean;
begin
  case Image.Value.ColorComponentsCount of
    1: Ok := Color is TCasScriptFloat;
    2: Ok := Color is TCasScriptVec2f;
    3: Ok := Color is TCasScriptVec3f;
    4: Ok := Color is TCasScriptVec4f;
    else raise EInternalError.Create('TCasScriptImage: invalid components count');
  end;
  if not Ok then
    raise ECasScriptError.CreateFmt('Invalid image color (with alpha) type: %s, but image has %d components',
      [Color.ClassName, Image.Value.ColorComponentsCount]);
end;

procedure CheckImageColorType(Image: TCasScriptImage; Color: TCasScriptValue);
var
  Ok: boolean;
begin
  case Image.Value.ColorComponentsCount of
    1, 2: Ok := Color is TCasScriptFloat;
    3, 4: Ok := Color is TCasScriptVec3f;
    else raise EInternalError.Create('TCasScriptImage: invalid components count');
  end;
  if not Ok then
    raise ECasScriptError.CreateFmt('Invalid image color (without alpha) type: %s, but image has %d components',
      [Color.ClassName, Image.Value.ColorComponentsCount]);
end;

procedure CheckImageAlphaType(Image: TCasScriptImage; Color: TCasScriptValue);
var
  Ok: boolean;
begin
  Ok := Color is TCasScriptFloat;
  if not Ok then
    raise ECasScriptError.CreateFmt('Invalid image alpha type: %s, but should be a single float',
      [Color.ClassName]);
end;

class procedure TCasScriptImage.HandleImageGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  X, Y: Integer;
  G: PByte;
  GA: PVector2Byte;
  RGB: PVector3Byte;
  RGBAlpha: PVector4Byte;
begin
  X := TCasScriptInteger(Arguments[1]).Value;
  Y := TCasScriptInteger(Arguments[2]).Value;
  CheckImageCoords(TCasScriptImage(Arguments[0]), X, Y);

  case TCasScriptImage(Arguments[0]).Value.ColorComponentsCount of
    1: begin
         CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptFloat);
         G := TCasScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         TCasScriptFloat(AResult).Value := G^/255;
       end;
    2: begin
         CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptVec2f);
         GA := TCasScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         TCasScriptVec2f(AResult).Value := Vector2(
           GA^.X/255,
           GA^.Y/255);
       end;
    3: begin
         CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptVec3f);
         RGB := TCasScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         TCasScriptVec3f(AResult).Value := Vector3(
           RGB^.X/255,
           RGB^.Y/255,
           RGB^.Z/255);
       end;
    4: begin
         CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptVec4f);
         RGBAlpha := TCasScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         TCasScriptVec4f(AResult).Value := Vector4(
           RGBAlpha^.X/255,
           RGBAlpha^.Y/255,
           RGBAlpha^.Z/255,
           RGBAlpha^.W/255);
       end;
    else raise EInternalError.Create('TCasScriptImage: invalid components count');
  end;
end;

class procedure TCasScriptImage.HandleImageGetColor(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  X, Y: Integer;
  G: PByte;
  GA: PVector2Byte;
  RGB: PVector3Byte;
  RGBAlpha: PVector4Byte;
begin
  X := TCasScriptInteger(Arguments[1]).Value;
  Y := TCasScriptInteger(Arguments[2]).Value;
  CheckImageCoords(TCasScriptImage(Arguments[0]), X, Y);

  case TCasScriptImage(Arguments[0]).Value.ColorComponentsCount of
    1: begin
         CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptFloat);
         G := TCasScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         TCasScriptFloat(AResult).Value := G^/255;
       end;
    2: begin
         CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptFloat);
         GA := TCasScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         TCasScriptFloat(AResult).Value := GA^.X/255;
       end;
    3: begin
         CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptVec3f);
         RGB := TCasScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         TCasScriptVec3f(AResult).Value := Vector3(
           RGB^.X/255,
           RGB^.Y/255,
           RGB^.Z/255);
       end;
    4: begin
         CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptVec3f);
         RGBAlpha := TCasScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         TCasScriptVec3f(AResult).Value := Vector3(
           RGBAlpha^.X/255,
           RGBAlpha^.Y/255,
           RGBAlpha^.Z/255);
       end;
    else raise EInternalError.Create('TCasScriptImage: invalid components count');
  end;
end;

class procedure TCasScriptImage.HandleImageGetAlpha(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  X, Y: Integer;
  GA: PVector2Byte;
  RGBAlpha: PVector4Byte;
begin
  X := TCasScriptInteger(Arguments[1]).Value;
  Y := TCasScriptInteger(Arguments[2]).Value;
  CheckImageCoords(TCasScriptImage(Arguments[0]), X, Y);

  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptFloat);

  case TCasScriptImage(Arguments[0]).Value.ColorComponentsCount of
    1, 3: begin
         WritelnWarning('CastleScript', '"image_get_alpha" not allowed on image without alpha channel');
       end;
    2: begin
         GA := TCasScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         TCasScriptFloat(AResult).Value := GA^.Y/255;
       end;
    4: begin
         RGBAlpha := TCasScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         TCasScriptFloat(AResult).Value := RGBAlpha^.W/255;
       end;
    else raise EInternalError.Create('TCasScriptImage: invalid components count');
  end;
end;

class procedure TCasScriptImage.HandleImageSet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  X, Y: Integer;
  G: PByte;
  GA: PVector2Byte;
  RGB: PVector3Byte;
  RGBAlpha: PVector4Byte;
begin
  X := TCasScriptInteger(Arguments[1]).Value;
  Y := TCasScriptInteger(Arguments[2]).Value;
  CheckImageCoords(TCasScriptImage(Arguments[0]), X, Y);
  CheckImageColorAlphaType(TCasScriptImage(Arguments[0]), Arguments[3]);

  if ParentOfResult then
    AResult.FreeByParentExpression else
    AResult := nil;
  AResult := Arguments[0];
  ParentOfResult := false;

  case TCasScriptImage(Arguments[0]).Value.ColorComponentsCount of
    1: begin
         G := TCasScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         G^ := Clamped(Round(TCasScriptFloat(Arguments[3]).Value*255), 0, 255);
       end;
    2: begin
         GA := TCasScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         GA^.X := Clamped(Round(TCasScriptVec2f(Arguments[3]).Value[0]*255), 0, 255);
         GA^.Y := Clamped(Round(TCasScriptVec2f(Arguments[3]).Value[1]*255), 0, 255);
       end;
    3: begin
         RGB := TCasScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         RGB^.X := Clamped(Round(TCasScriptVec3f(Arguments[3]).Value[0]*255), 0, 255);
         RGB^.Y := Clamped(Round(TCasScriptVec3f(Arguments[3]).Value[1]*255), 0, 255);
         RGB^.Z := Clamped(Round(TCasScriptVec3f(Arguments[3]).Value[2]*255), 0, 255);
       end;
    4: begin
         RGBAlpha := TCasScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         RGBAlpha^.X := Clamped(Round(TCasScriptVec4f(Arguments[3]).Value[0]*255), 0, 255);
         RGBAlpha^.Y := Clamped(Round(TCasScriptVec4f(Arguments[3]).Value[1]*255), 0, 255);
         RGBAlpha^.Z := Clamped(Round(TCasScriptVec4f(Arguments[3]).Value[2]*255), 0, 255);
         RGBAlpha^.W := Clamped(Round(TCasScriptVec4f(Arguments[3]).Value[3]*255), 0, 255);
       end;
    else raise EInternalError.Create('TCasScriptImage: invalid components count');
  end;

  TCasScriptImage(Arguments[0]).ValueAssigned := true;
end;

class procedure TCasScriptImage.HandleImageSetColor(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  X, Y: Integer;
  G: PByte;
  GA: PVector2Byte;
  RGB: PVector3Byte;
  RGBAlpha: PVector4Byte;
begin
  X := TCasScriptInteger(Arguments[1]).Value;
  Y := TCasScriptInteger(Arguments[2]).Value;
  CheckImageCoords(TCasScriptImage(Arguments[0]), X, Y);
  CheckImageColorType(TCasScriptImage(Arguments[0]), Arguments[3]);

  if ParentOfResult then
    AResult.FreeByParentExpression else
    AResult := nil;
  AResult := Arguments[0];
  ParentOfResult := false;

  case TCasScriptImage(Arguments[0]).Value.ColorComponentsCount of
    1: begin
         G := TCasScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         G^ := Clamped(Round(TCasScriptFloat(Arguments[3]).Value*255), 0, 255);
       end;
    2: begin
         GA := TCasScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         GA^.X := Clamped(Round(TCasScriptFloat(Arguments[3]).Value*255), 0, 255);
       end;
    3: begin
         RGB := TCasScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         RGB^.X := Clamped(Round(TCasScriptVec3f(Arguments[3]).Value[0]*255), 0, 255);
         RGB^.Y := Clamped(Round(TCasScriptVec3f(Arguments[3]).Value[1]*255), 0, 255);
         RGB^.Z := Clamped(Round(TCasScriptVec3f(Arguments[3]).Value[2]*255), 0, 255);
       end;
    4: begin
         RGBAlpha := TCasScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         RGBAlpha^.X := Clamped(Round(TCasScriptVec3f(Arguments[3]).Value[0]*255), 0, 255);
         RGBAlpha^.Y := Clamped(Round(TCasScriptVec3f(Arguments[3]).Value[1]*255), 0, 255);
         RGBAlpha^.Z := Clamped(Round(TCasScriptVec3f(Arguments[3]).Value[2]*255), 0, 255);
       end;
    else raise EInternalError.Create('TCasScriptImage: invalid components count');
  end;

  TCasScriptImage(Arguments[0]).ValueAssigned := true;
end;

class procedure TCasScriptImage.HandleImageSetAlpha(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  X, Y: Integer;
  GA: PVector2Byte;
  RGBAlpha: PVector4Byte;
begin
  X := TCasScriptInteger(Arguments[1]).Value;
  Y := TCasScriptInteger(Arguments[2]).Value;
  CheckImageCoords(TCasScriptImage(Arguments[0]), X, Y);
  CheckImageAlphaType(TCasScriptImage(Arguments[0]), Arguments[3]);

  if ParentOfResult then
    AResult.FreeByParentExpression else
    AResult := nil;
  AResult := Arguments[0];
  ParentOfResult := false;

  case TCasScriptImage(Arguments[0]).Value.ColorComponentsCount of
    1, 3: begin
         WritelnWarning('CastleScript', '"image_set_alpha" not allowed on image without alpha channel');
       end;
    2: begin
         GA := TCasScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         GA^.Y := Clamped(Round(TCasScriptFloat(Arguments[3]).Value*255), 0, 255);
       end;
    4: begin
         RGBAlpha := TCasScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         RGBAlpha^.W := Clamped(Round(TCasScriptFloat(Arguments[3]).Value*255), 0, 255);
       end;
    else raise EInternalError.Create('TCasScriptImage: invalid components count');
  end;

  TCasScriptImage(Arguments[0]).ValueAssigned := true;
end;

{ Functions ------------------------------------------------------------------ }

class function TCasScriptImageFun.ShortName: string;
begin
  Result := 'image';
end;

class function TCasScriptImageLoad.ShortName: string;
begin
  Result := 'image_load';
end;

class function TCasScriptImageGet.ShortName: string;
begin
  Result := 'image_get';
end;

class function TCasScriptImageWidth.ShortName: string;
begin
  Result := 'image_width';
end;

class function TCasScriptImageHeight.ShortName: string;
begin
  Result := 'image_height';
end;

class function TCasScriptImageComponents.ShortName: string;
begin
  Result := 'image_components';
end;

class function TCasScriptImageGetColor.ShortName: string;
begin
  Result := 'image_get_color';
end;

class function TCasScriptImageGetAlpha.ShortName: string;
begin
  Result := 'image_get_alpha';
end;

class function TCasScriptImageSet.ShortName: string;
begin
  Result := 'image_set';
end;

procedure TCasScriptImageSet.CheckArguments;
begin
  inherited;
  if not ( (Args[0] is TCasScriptValue) and
           TCasScriptValue(Args[0]).Writeable ) then
    raise ECasScriptFunctionArgumentsError.Create('First argument of "image_set" function is not a writeable operand');
end;

class function TCasScriptImageSetColor.ShortName: string;
begin
  Result := 'image_set_color';
end;

procedure TCasScriptImageSetColor.CheckArguments;
begin
  inherited;
  if not ( (Args[0] is TCasScriptValue) and
           TCasScriptValue(Args[0]).Writeable ) then
    raise ECasScriptFunctionArgumentsError.Create('First argument of "image_set_color" function is not a writeable operand');
end;

class function TCasScriptImageSetAlpha.ShortName: string;
begin
  Result := 'image_set_alpha';
end;

procedure TCasScriptImageSetAlpha.CheckArguments;
begin
  inherited;
  if not ( (Args[0] is TCasScriptValue) and
           TCasScriptValue(Args[0]).Writeable ) then
    raise ECasScriptFunctionArgumentsError.Create('First argument of "image_set_alpha" function is not a writeable operand');
end;

{ unit init/fini ------------------------------------------------------------- }

initialization
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} TCasScriptImage {$ifdef FPC}(nil){$endif} .HandleImage, TCasScriptImageFun, [TCasScriptInteger, TCasScriptInteger, TCasScriptInteger], false);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} TCasScriptImage {$ifdef FPC}(nil){$endif} .HandleImageLoad, TCasScriptImageLoad, [TCasScriptString], false);

  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} TCasScriptImage {$ifdef FPC}(nil){$endif} .HandleImageWidth, TCasScriptImageWidth, [TCasScriptImage], false);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} TCasScriptImage {$ifdef FPC}(nil){$endif} .HandleImageHeight, TCasScriptImageHeight, [TCasScriptImage], false);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} TCasScriptImage {$ifdef FPC}(nil){$endif} .HandleImageComponents, TCasScriptImageComponents, [TCasScriptImage], false);

  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} TCasScriptImage {$ifdef FPC}(nil){$endif} .HandleImageGet     , TCasScriptImageGet     , [TCasScriptImage, TCasScriptInteger, TCasScriptInteger], false);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} TCasScriptImage {$ifdef FPC}(nil){$endif} .HandleImageGetColor, TCasScriptImageGetColor, [TCasScriptImage, TCasScriptInteger, TCasScriptInteger], false);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} TCasScriptImage {$ifdef FPC}(nil){$endif} .HandleImageGetAlpha, TCasScriptImageGetAlpha, [TCasScriptImage, TCasScriptInteger, TCasScriptInteger], false);

  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} TCasScriptImage {$ifdef FPC}(nil){$endif} .HandleImageSet     , TCasScriptImageSet     , [TCasScriptImage, TCasScriptInteger, TCasScriptInteger, TCasScriptValue], false);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} TCasScriptImage {$ifdef FPC}(nil){$endif} .HandleImageSetColor, TCasScriptImageSetColor, [TCasScriptImage, TCasScriptInteger, TCasScriptInteger, TCasScriptValue], false);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} TCasScriptImage {$ifdef FPC}(nil){$endif} .HandleImageSetAlpha, TCasScriptImageSetAlpha, [TCasScriptImage, TCasScriptInteger, TCasScriptInteger, TCasScriptValue], false);

{$endif CASTLE_SCRIPT_FPC}
end.
