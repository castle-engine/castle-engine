{
  Copyright 2008 Michalis Kamburelis.

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

  ----------------------------------------------------------------------------
}

{ KambiScript image types and built-in functions. }
unit KambiScriptImages;

interface

uses VectorMath, KambiScript, Images;

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

    class procedure HandleImageFill(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleImageFillColor(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleImageFillAlpha(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    { Notes about number of components:

      Although you can assign any image descendant to Value, and then
      this will be used as Value...

      But image() function currently uses for 1 and 3 - TRGBImage and
      for 2 and 4 - TAlphaImage.

      Yes, this means that grayscale (with eventual alpha)
      is stored as RGB (with eventual alpha) anyway.
      This is bad, but it follows current TSFImage implementation.
      And this is the result of the fact that TGrayscaleImage is not
      handled everywhere yet (in OpenGL units), and TGrayscaleAlphaImage
      is not implemented at all... This is supposed to be fixed one day.
      For now, grayscale images are really seldom used, so there's no
      pressure.
      Besides memory (and performance) penalty, this is not noticeable
      to user. }

    FValue: TImage;
    procedure SetValue(const AValue: TImage);
  public
    constructor Create(const AWriteable: boolean; const AValue: TImage);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    { Image value. Assignning here makes a @italic(copy) of the image. }
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

  TKamScriptImageFill = class(TKamScriptFunction)
  protected
    procedure CheckArguments; override;
  public
    class function ShortName: string; override;
  end;

  TKamScriptImageFillColor = class(TKamScriptFunction)
  protected
    procedure CheckArguments; override;
  public
    class function ShortName: string; override;
  end;

  TKamScriptImageFillAlpha = class(TKamScriptFunction)
  protected
    procedure CheckArguments; override;
  public
    class function ShortName: string; override;
  end;

implementation

uses SysUtils, KambiUtils, DataErrors, KambiScriptVectors;

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
    DataNonFatalError('"image" function 3rd parameter (components) must be between 1 and 4');
    { We have to return something... Assume any valid Components value. }
    Components := Clamped(Components, 1, 4);
  end;

  Width := TKamScriptInteger(Arguments[0]).Value;
  Height := TKamScriptInteger(Arguments[1]).Value;

  FreeAndNil(TKamScriptImage(AResult).FValue);
  case Components of
    1: TKamScriptImage(AResult).FValue := {}{TODO}{TGrayscaleImage}TRGBImage.Create(Width, Height);
    2: TKamScriptImage(AResult).FValue := {}{TODO}{TGrayscaleAlphaImage}TAlphaImage.Create(Width, Height);
    3: TKamScriptImage(AResult).FValue := TRGBImage.Create(Width, Height);
    4: TKamScriptImage(AResult).FValue := TAlphaImage.Create(Width, Height);
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

  { TODO: FullUrl relative from WWWBasePath }
  FullUrl := TKamScriptString(Arguments[0]).Value;
  try
    NewImage := LoadImage(FullUrl, [TRGBImage, TAlphaImage], []);
  except
    on E: Exception do
      raise EKamScriptError.Create('Exception ' + E.ClassName +
        ' occured when trying to load ' +
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
  RGB: PVector3Byte;
  RGBAlpha: PVector4Byte;
begin
  X := TKamScriptInteger(Arguments[1]).Value;
  Y := TKamScriptInteger(Arguments[2]).Value;
  CheckImageCoords(TKamScriptImage(Arguments[0]), X, Y);

  case TKamScriptImage(Arguments[0]).Value.ColorComponentsCount of
//   1: TODO
//   2: TODO
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
  RGB: PVector3Byte;
  RGBAlpha: PVector4Byte;
begin
  X := TKamScriptInteger(Arguments[1]).Value;
  Y := TKamScriptInteger(Arguments[2]).Value;
  CheckImageCoords(TKamScriptImage(Arguments[0]), X, Y);

  case TKamScriptImage(Arguments[0]).Value.ColorComponentsCount of
//   1: TODO
//   2: TODO
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
  RGBAlpha: PVector4Byte;
begin
  X := TKamScriptInteger(Arguments[1]).Value;
  Y := TKamScriptInteger(Arguments[2]).Value;
  CheckImageCoords(TKamScriptImage(Arguments[0]), X, Y);

  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);

  case TKamScriptImage(Arguments[0]).Value.ColorComponentsCount of
//   1: TODO
//   2: TODO
    3: begin
         DataNonFatalError('"image_get_alpha" not allowed on image without alpha channel');
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
//    1: TODO
//    2: TODO
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
//    1: TODO
//    2: TODO
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
//    1: TODO
//    2: TODO
    3: begin
         DataNonFatalError('"image_set_alpha" not allowed on image without alpha channel');
       end;
    4: begin
         RGBAlpha := TKamScriptImage(Arguments[0]).Value.PixelPtr(X, Y);
         RGBAlpha^[3] := Clamped(Round(TKamScriptFloat(Arguments[3]).Value*255), 0, 255);
       end;
    else raise EInternalError.Create('TKamScriptImage: invalid components count');
  end;

  TKamScriptImage(Arguments[0]).ValueAssigned := true;
end;

class procedure TKamScriptImage.HandleImageFill(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  { TODO }
end;

class procedure TKamScriptImage.HandleImageFillColor(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  { TODO }
end;

class procedure TKamScriptImage.HandleImageFillAlpha(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  { TODO }
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

class function TKamScriptImageFill.ShortName: string;
begin
  Result := 'image_fill';
end;

procedure TKamScriptImageFill.CheckArguments;
begin
  inherited;
  if not ( (Args[0] is TKamScriptValue) and
           TKamScriptValue(Args[0]).Writeable ) then
    raise EKamScriptFunctionArgumentsError.Create('First argument of "image_fill" function is not a writeable operand');
end;

class function TKamScriptImageFillColor.ShortName: string;
begin
  Result := 'image_fill_color';
end;

procedure TKamScriptImageFillColor.CheckArguments;
begin
  inherited;
  if not ( (Args[0] is TKamScriptValue) and
           TKamScriptValue(Args[0]).Writeable ) then
    raise EKamScriptFunctionArgumentsError.Create('First argument of "image_fill_color" function is not a writeable operand');
end;

class function TKamScriptImageFillAlpha.ShortName: string;
begin
  Result := 'image_fill_alpha';
end;

procedure TKamScriptImageFillAlpha.CheckArguments;
begin
  inherited;
  if not ( (Args[0] is TKamScriptValue) and
           TKamScriptValue(Args[0]).Writeable ) then
    raise EKamScriptFunctionArgumentsError.Create('First argument of "image_fill_alpha" function is not a writeable operand');
end;

{ unit init/fini ------------------------------------------------------------- }

initialization
  FunctionHandlers.RegisterHandler(@TKamScriptImage(nil).HandleImage, TKamScriptImageFun, [TKamScriptInteger, TKamScriptInteger, TKamScriptInteger], false);
  FunctionHandlers.RegisterHandler(@TKamScriptImage(nil).HandleImageLoad, TKamScriptImageLoad, [TKamScriptString], false);

  FunctionHandlers.RegisterHandler(@TKamScriptImage(nil).HandleImageWidth, TKamScriptImageWidth, [TKamScriptImage], false);
  FunctionHandlers.RegisterHandler(@TKamScriptImage(nil).HandleImageHeight, TKamScriptImageHeight, [TKamScriptImage], false);
  FunctionHandlers.RegisterHandler(@TKamScriptImage(nil).HandleImageComponents, TKamScriptImageComponents, [TKamScriptImage], false);

  FunctionHandlers.RegisterHandler(@TKamScriptImage(nil).HandleImageGet     , TKamScriptImageSet     , [TKamScriptImage, TKamScriptInteger, TKamScriptInteger], false);
  FunctionHandlers.RegisterHandler(@TKamScriptImage(nil).HandleImageGetColor, TKamScriptImageSetColor, [TKamScriptImage, TKamScriptInteger, TKamScriptInteger], false);
  FunctionHandlers.RegisterHandler(@TKamScriptImage(nil).HandleImageGetAlpha, TKamScriptImageSetAlpha, [TKamScriptImage, TKamScriptInteger, TKamScriptInteger], false);

  FunctionHandlers.RegisterHandler(@TKamScriptImage(nil).HandleImageSet     , TKamScriptImageSet     , [TKamScriptImage, TKamScriptInteger, TKamScriptInteger, TKamScriptValue], false);
  FunctionHandlers.RegisterHandler(@TKamScriptImage(nil).HandleImageSetColor, TKamScriptImageSetColor, [TKamScriptImage, TKamScriptInteger, TKamScriptInteger, TKamScriptValue], false);
  FunctionHandlers.RegisterHandler(@TKamScriptImage(nil).HandleImageSetAlpha, TKamScriptImageSetAlpha, [TKamScriptImage, TKamScriptInteger, TKamScriptInteger, TKamScriptValue], false);

  FunctionHandlers.RegisterHandler(@TKamScriptImage(nil).HandleImageFill     , TKamScriptImageFill     , [TKamScriptImage, TKamScriptInteger, TKamScriptInteger, TKamScriptValue], false);
  FunctionHandlers.RegisterHandler(@TKamScriptImage(nil).HandleImageFillColor, TKamScriptImageFillColor, [TKamScriptImage, TKamScriptInteger, TKamScriptInteger, TKamScriptValue], false);
  FunctionHandlers.RegisterHandler(@TKamScriptImage(nil).HandleImageFillAlpha, TKamScriptImageFillAlpha, [TKamScriptImage, TKamScriptInteger, TKamScriptInteger, TKamScriptValue], false);
end.
