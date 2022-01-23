{%MainUnit castletransform.pas}
{
  Copyright 2017-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

//type
  { Contents are loaded from an indicated castle-transform file
    (by the @link(URL) property).

    The file should be in the format created by Castle Game Engine Editor
    (serialized by CastleComponentSerialize routines) with .castle-transform extension. }
  TCastleTransformDesign = class(TCastleTransform)
  strict private
    FDesign: TCastleTransform;
    FDesignObserver: TFreeNotificationObserver;
    FURL: String;
    procedure SetURL(const Value: String);
    procedure DesignFreeNotification(const Sender: TFreeNotificationObserver);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    { Load file with the transform design.
      The file should be in the format created by Castle Game Engine Editor
      (see CastleComponentSerialize unit) with .castle-transform extension.
      Setting this property automatically unloads the previous design,
      and loads a new one (if the value is <> '').

      After loading a design, you can use FindComponent or
      @link(TComponentHelper.FindRequiredComponent) to find the components inside.
      The owner of the design is this TCastleTransformDesign instance. So e.g. this works:

      @longCode(#
      var
        MyDesign: TCastleTransformDesign;
        MyScene: TCastleScene;
      begin
        MyDesign := TCastleTransformDesign.Create(...);
        MyDesign.URL := 'castle-data:/my_transform.castle-transform';
        MyScene := MyDesign.FindRequiredComponent('MyScene') as TCastleScene;
        MyScene.Translation := Vector3(1, 2, 3);
      end;
      #)
    }
    property URL: String read FURL write SetURL;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleTransformDesign ----------------------------------------------------- }

constructor TCastleTransformDesign.Create(AOwner: TComponent);
begin
  inherited;
  FDesignObserver := TFreeNotificationObserver.Create(Self);
  FDesignObserver.OnFreeNotification := {$ifdef FPC}@{$endif}DesignFreeNotification;
end;

destructor TCastleTransformDesign.Destroy;
begin
  inherited;
end;

procedure TCastleTransformDesign.DesignFreeNotification(const Sender: TFreeNotificationObserver);
begin
  FDesign := nil;
end;

procedure TCastleTransformDesign.SetURL(const Value: String);
begin
  if FURL <> Value then
  begin
    if FDesign <> nil then
    begin
      FreeAndNil(FDesign);
    end;

    FURL := Value;

    if Value <> '' then
    begin
      try
        FDesign := TransformLoad(Value, Self);
      except
        { If loading design file failed, and we're inside CGE editor,
          merely report a warning. This allows deserializing in CGE editor
          designs with broken URLs. }
        on E: Exception do
        begin
          if CastleDesignMode then
          begin
            WritelnWarning('TCastleTransformDesign', 'Failed to load design "%s": %s', [URIDisplay(URL), ExceptMessage(E)]);
            Exit;
          end else
            raise;
        end;
      end;

      { This prevents this child from
        - being serialized (in case TCastleTransformDesign is placed inside
          a hierachy saved using TransformSave. Only URL should be saved.)
        - or even be visible in the hierachy of castle-editor.
      }
      FDesign.SetTransient;
      FDesignObserver.Observed := FDesign;
      Add(FDesign);
    end;
  end;
end;

function TCastleTransformDesign.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if PropertyName = 'URL' then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$endif read_implementation}
