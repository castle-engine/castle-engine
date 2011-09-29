{
  Copyright 2008-2011 Michalis Kamburelis.

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

interface

uses VectorMath, CastleScript, Images;

type
  TKamScriptImage = class(TKamScriptValue)
  private
    class procedure HandleImage(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleImageLoad(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleImageWidth(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleImageHeight(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleImageComponents(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleImageGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleImageGetColor(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleImageGetAlpha(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleImageSet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleImageSetColor(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleImageSetAlpha(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

  private
    FValue: TImage;
    procedure SetValue(const AValue: TImage);
  public
    constructor Create(const AWriteable: boolean; const AValue: TImage);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Image value. Assigning here makes a @italic(copy) of the image. }
    property Value: TImage read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptImageFun = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptImageLoad = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptImageWidth = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptImageHeight = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptImageComponents = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptImageGet = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptImageGetColor = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptImageGetAlpha = class(TKamScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TKamScriptImageSet = class(TKamScriptFunction)
  protected
    procedure CheckArguments; override;
  public
    class function ShortName: string; override;
  end;

  TKamScriptImageSetColor = class(TKamScriptFunction)
  protected
    procedure CheckArguments; override;
  public
    class function ShortName: string; override;
  end;

  TKamScriptImageSetAlpha = class(TKamScriptFunction)
  protected
    procedure CheckArguments; override;
  public
    class function ShortName: string; override;
  end;

implementation

uses SysUtils, CastleUtils, CastleWarnings, CastleScriptVectors, CastleFilesUtils;

{ TKamScriptImage ------------------------------------------------------------ }

constructor TKamScriptImage.Create(const AWriteable: boolean; const AValue: TImage);
begin
  Create(AWriteable);

  { First create a dummy empty TRGBImage in default
    TKamScriptImage.Create(boolean) constructor, then SetValue will
    free it and replace with AValue copy. This is Ok --- calling
    Create(boolean) first is safer (it's virtual, and in case
    TKamScriptImage will have a descendant later it will be good)
    and creating/freeing temporary TRGBImage instance with zero size
    should not make a noticeable speed overhead. }

  Value := AValue;
end;

constructor TKamScriptImage.Create(const AWriteable: boolean);
begin
  inherited;
  FValue := TRGBImage.Create(0, 0);
end;

destructor TKamScriptImage.Destroy;
begin
  FreeAndNil(FValue);
  inherited;
end;

procedure TKamScriptImage.SetValue(const AValue: TImage);
begin
  FreeAndNil(FValue);
  FValue := AValue.MakeCopy;
  ValueAssigned := true;
end;

procedure TKamScriptImage.AssignValue(Source: TKamScriptValue);
begin
  if Source is TKamScriptImage then
    Value := TKamScriptImage(Source).Value else
    raise EKamAssignValueError.CreateFmt('Assignment from %s to %s not possible', [Source.ClassName, ClassName]);
end;

{ TKamScriptImage function handlers ------------------------------------------ }

class procedure TKamScriptImage.HandleImage(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  Components: Int64;
  Width, Height: Cardinal;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptImage);

  Components := TKamScriptInteger(Arguments[2]).Value;
  if not Between(Components, 1, 4) then
  begin
    OnWarning(wtMajor, 'CastleScript', '"image" function 3rd parameter (components) must be between 1 and 4');
    { We have to return something... Assume any valid Components value. }
    Components := Clamped(Components, 1, 4);
  end;

  Width := TKamScriptInteger(Arguments[0]).Value;
  Height := TKamScriptInteger(Arguments[1]).Value;

  FreeAndNil(TKamScriptImage(AResult).FValue);
  case Components of
    1: TKamScriptImage(AResult).FValue := TGrayscaleImage.Create(Width, Height);
    2: TKamScriptImage(AResult).FValue := TGrayscaleAlphaImage.Create(Width, Height);
    3: TKamScriptImage(AResult).FValue := TRGBImage.Create(Width, Height);
    4: TKamScriptImage(AResult).FValue := TRGBAlphaImage.Create(Width, Height);
    else raise EInternalError.CreateFmt('TKamScriptImage.AssignNewValue: Not allowed number of components: %d',
      [Components]);
  end;
  TKamScriptImage(AResult).ValueAssigned := true;
end;

class procedure TKamScriptImage.HandleImageLoad(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  FullUrl: string;
  NewImage: TImage;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptImage);

  FullUrl := TKamScriptString(Arguments[0]).Value;
  if AFunction.Environment <> nil then
    FullUrl := CombinePaths(AFunction.Environment.WWWBasePath, FullUrl);

  try
    NewImage := LoadImage(FullUrl, [TRGBImage, TRGBAlphaImage], []);
  except
    on E: Exception do
      raise EKamScriptError.Create('Exception ' + E.ClassName +
        ' occurred when trying to load ' +
        'image from url "' + FullUrl + '" : ' + E.Message);
  end;

  FreeAndNil(TKamScriptImage(AResult).FValue);
  TKamScriptImage(AResult).FValue := NewImage;

  TKamScriptImage(AResult).ValueAssigned := true;
end;

class procedure TKamScriptImage.HandleImageWidth(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptInteger);
  TKamScriptInteger(AResult).Value := TKamScriptImage(Arguments[0]).Value.Width;
end;

class procedure TKamScriptImage.HandleImageHeight(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptInteger);
  TKamScriptInteger(AResult).Value := TKamScriptImage(Arguments[0]).Value.Height;
end;

class procedure TKamScriptImage.HandleImageComponents(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptInteger);
  TKamScriptInteger(AResult).Value := TKamScriptImage(Arguments[0]).Value.ColorComponentsCount;
end;

procedure CheckImageCoords(Image: TKamScriptImage; const X, Y: Int64);
var
  Ok: boolean;
begin
  Ok := Between(X, 0, Image.Value.Width  - 1) and
        Between(Y, 0, Image.Value.Height - 1);
  if not Ok then
    raise EKamScriptError.CreateFmt('Invalid image coordinates: (%d, %d), while image size is (%d, %d)',
      [X, Y, Image.Value.Width, Image.Value.Height]);
end;

procedure CheckImageColorAlphaType(Image: TKamScriptImage; Color: TKamScriptValue);
var
  Ok: boolean;
begin
  case Image.Value.ColorComponentsCount of
    1: Ok := Color is TKamScriptFloat;
    2: Ok := Color is TKamScriptVec2f;
    3: Ok := Color is TKamScriptVec3f;
    4: Ok := Color is TKamScriptVec4f;
    else raise EInternalError.Create('TKamScriptImage: invalid components count');
  end;
  if not Ok then
    raise EKamScriptError.CreateFmt('Invalid image color (with alpha) type: %s, but image has %d components',
      [Color.ClassName, Image.Value.ColorComponentsCount]);
end;

procedure CheckImageColorType(Image: TKamScriptImage; Color: TKamScriptValue);
var
  Ok: boolean;
begin
  case Image.Value.ColorComponentsCount of
    1, 2: Ok := Color is TKamScriptFloat;
    3, 4: Ok := Color is TKamScriptVec3f;
    else raise EInternalError.Create('TKamScriptImage: invalid components count');
  end;
  if not Ok then
    raise EKamScriptError.CreateFmt('Invalid image color (without alpha) type: %s, but image has %d components',
      [Color.ClassName, Image.Value.ColorComponentsCount]);
end;

procedure CheckImageAlphaType(Image: TKamScriptImage; Color: TKamScriptValue);
var
  Ok: boolean;
begin
  Ok := Color is TKamScriptFloat;
  if not Ok then
    raise EKamScriptError.CreateFmt('Invalid image alpha type: %s, but should be a single float',
      [Color.ClassName]);
end;

class procedure TKamScriptImage.HandleImageGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  X, Y: Integer;
  G: PByte;
  GA: PVector2Byte;
  RGB: PVector3Byte;
  RGBAlpha: PVector4Byte;
begin
  X := TKamScriptInteger(Arguments[1]).Value;
  Y := TKamScriptInteger(Arguments[2]).Value;
  CheckImageCoords(TKamScriptImage(Arguments[0]), X, Y);

  case TKamScriptImage(Arguments[0]).Value.ColorComponentsCount of
    1: begin
         CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
         G := TKamScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         TKamScriptFloat(AResult).Value := G^/255;
       end;
    2: begin
         CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptVec2f);
         GA := TKamScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         TKamScriptVec2f(AResult).Value := Vector2Single(
           GA^[0]/255,
           GA^[1]/255);
       end;
    3: begin
         CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptVec3f);
         RGB := TKamScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         TKamScriptVec3f(AResult).Value := Vector3Single(
           RGB^[0]/255,
           RGB^[1]/255,
           RGB^[2]/255);
       end;
    4: begin
         CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptVec4f);
         RGBAlpha := TKamScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         TKamScriptVec4f(AResult).Value := Vector4Single(
           RGBAlpha^[0]/255,
           RGBAlpha^[1]/255,
           RGBAlpha^[2]/255,
           RGBAlpha^[3]/255);
       end;
    else raise EInternalError.Create('TKamScriptImage: invalid components count');
  end;
end;

class procedure TKamScriptImage.HandleImageGetColor(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  X, Y: Integer;
  G: PByte;
  GA: PVector2Byte;
  RGB: PVector3Byte;
  RGBAlpha: PVector4Byte;
begin
  X := TKamScriptInteger(Arguments[1]).Value;
  Y := TKamScriptInteger(Arguments[2]).Value;
  CheckImageCoords(TKamScriptImage(Arguments[0]), X, Y);

  case TKamScriptImage(Arguments[0]).Value.ColorComponentsCount of
    1: begin
         CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
         G := TKamScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         TKamScriptFloat(AResult).Value := G^/255;
       end;
    2: begin
         CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
         GA := TKamScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         TKamScriptFloat(AResult).Value := GA^[0]/255;
       end;
    3: begin
         CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptVec3f);
         RGB := TKamScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         TKamScriptVec3f(AResult).Value := Vector3Single(
           RGB^[0]/255,
           RGB^[1]/255,
           RGB^[2]/255);
       end;
    4: begin
         CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptVec3f);
         RGBAlpha := TKamScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         TKamScriptVec3f(AResult).Value := Vector3Single(
           RGBAlpha^[0]/255,
           RGBAlpha^[1]/255,
           RGBAlpha^[2]/255);
       end;
    else raise EInternalError.Create('TKamScriptImage: invalid components count');
  end;
end;

class procedure TKamScriptImage.HandleImageGetAlpha(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  X, Y: Integer;
  GA: PVector2Byte;
  RGBAlpha: PVector4Byte;
begin
  X := TKamScriptInteger(Arguments[1]).Value;
  Y := TKamScriptInteger(Arguments[2]).Value;
  CheckImageCoords(TKamScriptImage(Arguments[0]), X, Y);

  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);

  case TKamScriptImage(Arguments[0]).Value.ColorComponentsCount of
    1, 3: begin
         OnWarning(wtMajor, 'CastleScript', '"image_get_alpha" not allowed on image without alpha channel');
       end;
    2: begin
         GA := TKamScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         TKamScriptFloat(AResult).Value := GA^[1]/255;
       end;
    4: begin
         RGBAlpha := TKamScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         TKamScriptFloat(AResult).Value := RGBAlpha^[3]/255;
       end;
    else raise EInternalError.Create('TKamScriptImage: invalid components count');
  end;
end;

class procedure TKamScriptImage.HandleImageSet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  X, Y: Integer;
  G: PByte;
  GA: PVector2Byte;
  RGB: PVector3Byte;
  RGBAlpha: PVector4Byte;
begin
  X := TKamScriptInteger(Arguments[1]).Value;
  Y := TKamScriptInteger(Arguments[2]).Value;
  CheckImageCoords(TKamScriptImage(Arguments[0]), X, Y);
  CheckImageColorAlphaType(TKamScriptImage(Arguments[0]), Arguments[3]);

  if ParentOfResult then
    AResult.FreeByParentExpression else
    AResult := nil;
  AResult := Arguments[0];
  ParentOfResult := false;

  case TKamScriptImage(Arguments[0]).Value.ColorComponentsCount of
    1: begin
         G := TKamScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         G^ := Clamped(Round(TKamScriptFloat(Arguments[3]).Value*255), 0, 255);
       end;
    2: begin
         GA := TKamScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         GA^[0] := Clamped(Round(TKamScriptVec2f(Arguments[3]).Value[0]*255), 0, 255);
         GA^[1] := Clamped(Round(TKamScriptVec2f(Arguments[3]).Value[1]*255), 0, 255);
       end;
    3: begin
         RGB := TKamScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         RGB^[0] := Clamped(Round(TKamScriptVec3f(Arguments[3]).Value[0]*255), 0, 255);
         RGB^[1] := Clamped(Round(TKamScriptVec3f(Arguments[3]).Value[1]*255), 0, 255);
         RGB^[2] := Clamped(Round(TKamScriptVec3f(Arguments[3]).Value[2]*255), 0, 255);
       end;
    4: begin
         RGBAlpha := TKamScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         RGBAlpha^[0] := Clamped(Round(TKamScriptVec4f(Arguments[3]).Value[0]*255), 0, 255);
         RGBAlpha^[1] := Clamped(Round(TKamScriptVec4f(Arguments[3]).Value[1]*255), 0, 255);
         RGBAlpha^[2] := Clamped(Round(TKamScriptVec4f(Arguments[3]).Value[2]*255), 0, 255);
         RGBAlpha^[3] := Clamped(Round(TKamScriptVec4f(Arguments[3]).Value[3]*255), 0, 255);
       end;
    else raise EInternalError.Create('TKamScriptImage: invalid components count');
  end;

  TKamScriptImage(Arguments[0]).ValueAssigned := true;
end;

class procedure TKamScriptImage.HandleImageSetColor(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  X, Y: Integer;
  G: PByte;
  GA: PVector2Byte;
  RGB: PVector3Byte;
  RGBAlpha: PVector4Byte;
begin
  X := TKamScriptInteger(Arguments[1]).Value;
  Y := TKamScriptInteger(Arguments[2]).Value;
  CheckImageCoords(TKamScriptImage(Arguments[0]), X, Y);
  CheckImageColorType(TKamScriptImage(Arguments[0]), Arguments[3]);

  if ParentOfResult then
    AResult.FreeByParentExpression else
    AResult := nil;
  AResult := Arguments[0];
  ParentOfResult := false;

  case TKamScriptImage(Arguments[0]).Value.ColorComponentsCount of
    1: begin
         G := TKamScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         G^ := Clamped(Round(TKamScriptFloat(Arguments[3]).Value*255), 0, 255);
       end;
    2: begin
         GA := TKamScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         GA^[0] := Clamped(Round(TKamScriptFloat(Arguments[3]).Value*255), 0, 255);
       end;
    3: begin
         RGB := TKamScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         RGB^[0] := Clamped(Round(TKamScriptVec3f(Arguments[3]).Value[0]*255), 0, 255);
         RGB^[1] := Clamped(Round(TKamScriptVec3f(Arguments[3]).Value[1]*255), 0, 255);
         RGB^[2] := Clamped(Round(TKamScriptVec3f(Arguments[3]).Value[2]*255), 0, 255);
       end;
    4: begin
         RGBAlpha := TKamScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         RGBAlpha^[0] := Clamped(Round(TKamScriptVec3f(Arguments[3]).Value[0]*255), 0, 255);
         RGBAlpha^[1] := Clamped(Round(TKamScriptVec3f(Arguments[3]).Value[1]*255), 0, 255);
         RGBAlpha^[2] := Clamped(Round(TKamScriptVec3f(Arguments[3]).Value[2]*255), 0, 255);
       end;
    else raise EInternalError.Create('TKamScriptImage: invalid components count');
  end;

  TKamScriptImage(Arguments[0]).ValueAssigned := true;
end;

class procedure TKamScriptImage.HandleImageSetAlpha(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  X, Y: Integer;
  GA: PVector2Byte;
  RGBAlpha: PVector4Byte;
begin
  X := TKamScriptInteger(Arguments[1]).Value;
  Y := TKamScriptInteger(Arguments[2]).Value;
  CheckImageCoords(TKamScriptImage(Arguments[0]), X, Y);
  CheckImageAlphaType(TKamScriptImage(Arguments[0]), Arguments[3]);

  if ParentOfResult then
    AResult.FreeByParentExpression else
    AResult := nil;
  AResult := Arguments[0];
  ParentOfResult := false;

  case TKamScriptImage(Arguments[0]).Value.ColorComponentsCount of
    1, 3: begin
         OnWarning(wtMajor, 'CastleScript', '"image_set_alpha" not allowed on image without alpha channel');
       end;
    2: begin
         GA := TKamScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         GA^[1] := Clamped(Round(TKamScriptFloat(Arguments[3]).Value*255), 0, 255);
       end;
    4: begin
         RGBAlpha := TKamScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         RGBAlpha^[3] := Clamped(Round(TKamScriptFloat(Arguments[3]).Value*255), 0, 255);
       end;
    else raise EInternalError.Create('TKamScriptImage: invalid components count');
  end;

  TKamScriptImage(Arguments[0]).ValueAssigned := true;
end;

{ Functions ------------------------------------------------------------------ }

class function TKamScriptImageFun.ShortName: string;
begin
  Result := 'image';
end;

class function TKamScriptImageLoad.ShortName: string;
begin
  Result := 'image_load';
end;

class function TKamScriptImageGet.ShortName: string;
begin
  Result := 'image_get';
end;

class function TKamScriptImageWidth.ShortName: string;
begin
  Result := 'image_width';
end;

class function TKamScriptImageHeight.ShortName: string;
begin
  Result := 'image_height';
end;

class function TKamScriptImageComponents.ShortName: string;
begin
  Result := 'image_components';
end;

class function TKamScriptImageGetColor.ShortName: string;
begin
  Result := 'image_get_color';
end;

class function TKamScriptImageGetAlpha.ShortName: string;
begin
  Result := 'image_get_alpha';
end;

class function TKamScriptImageSet.ShortName: string;
begin
  Result := 'image_set';
end;

procedure TKamScriptImageSet.CheckArguments;
begin
  inherited;
  if not ( (Args[0] is TKamScriptValue) and
           TKamScriptValue(Args[0]).Writeable ) then
    raise EKamScriptFunctionArgumentsError.Create('First argument of "image_set" function is not a writeable operand');
end;

class function TKamScriptImageSetColor.ShortName: string;
begin
  Result := 'image_set_color';
end;

procedure TKamScriptImageSetColor.CheckArguments;
begin
  inherited;
  if not ( (Args[0] is TKamScriptValue) and
           TKamScriptValue(Args[0]).Writeable ) then
    raise EKamScriptFunctionArgumentsError.Create('First argument of "image_set_color" function is not a writeable operand');
end;

class function TKamScriptImageSetAlpha.ShortName: string;
begin
  Result := 'image_set_alpha';
end;

procedure TKamScriptImageSetAlpha.CheckArguments;
begin
  inherited;
  if not ( (Args[0] is TKamScriptValue) and
           TKamScriptValue(Args[0]).Writeable ) then
    raise EKamScriptFunctionArgumentsError.Create('First argument of "image_set_alpha" function is not a writeable operand');
end;

{ unit init/fini ------------------------------------------------------------- }

initialization
  FunctionHandlers.RegisterHandler(@TKamScriptImage(nil).HandleImage, TKamScriptImageFun, [TKamScriptInteger, TKamScriptInteger, TKamScriptInteger], false);
  FunctionHandlers.RegisterHandler(@TKamScriptImage(nil).HandleImageLoad, TKamScriptImageLoad, [TKamScriptString], false);

  FunctionHandlers.RegisterHandler(@TKamScriptImage(nil).HandleImageWidth, TKamScriptImageWidth, [TKamScriptImage], false);
  FunctionHandlers.RegisterHandler(@TKamScriptImage(nil).HandleImageHeight, TKamScriptImageHeight, [TKamScriptImage], false);
  FunctionHandlers.RegisterHandler(@TKamScriptImage(nil).HandleImageComponents, TKamScriptImageComponents, [TKamScriptImage], false);

  FunctionHandlers.RegisterHandler(@TKamScriptImage(nil).HandleImageGet     , TKamScriptImageGet     , [TKamScriptImage, TKamScriptInteger, TKamScriptInteger], false);
  FunctionHandlers.RegisterHandler(@TKamScriptImage(nil).HandleImageGetColor, TKamScriptImageGetColor, [TKamScriptImage, TKamScriptInteger, TKamScriptInteger], false);
  FunctionHandlers.RegisterHandler(@TKamScriptImage(nil).HandleImageGetAlpha, TKamScriptImageGetAlpha, [TKamScriptImage, TKamScriptInteger, TKamScriptInteger], false);

  FunctionHandlers.RegisterHandler(@TKamScriptImage(nil).HandleImageSet     , TKamScriptImageSet     , [TKamScriptImage, TKamScriptInteger, TKamScriptInteger, TKamScriptValue], false);
  FunctionHandlers.RegisterHandler(@TKamScriptImage(nil).HandleImageSetColor, TKamScriptImageSetColor, [TKamScriptImage, TKamScriptInteger, TKamScriptInteger, TKamScriptValue], false);
  FunctionHandlers.RegisterHandler(@TKamScriptImage(nil).HandleImageSetAlpha, TKamScriptImageSetAlpha, [TKamScriptImage, TKamScriptInteger, TKamScriptInteger, TKamScriptValue], false);
end.
