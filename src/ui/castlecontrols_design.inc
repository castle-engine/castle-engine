{%MainUnit castlecontrols.pas}
{
  Copyright 2018-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  EMaximumDesignDepth = class(Exception);

  { Contents of this user-interface control are loaded from an indicated file
    (by the @link(URL) property).

    The file should be in the format created by Castle Game Engine Editor
    (serialized by CastleComponentSerialize routines) with .castle-user-interface extension. }
  TCastleDesign = class(TCastleUserInterface)
  strict private
    class var
      LoadDepth: Cardinal;
    var
      FDesign: TCastleUserInterface;
      { We don't use Self as Owner of FDesign, to allow freeing everything
        loaded by UserInterfaceLoad by simple FreeAndNil(FDesignOwner). }
      FDesignOwner: TComponent;
      FDesignObserver: TFreeNotificationObserver;
      FURL: String;
      FRevertURL: String;
      FStretchWhenNotLoaded: Boolean;
    procedure SetURL(const Value: String);
    function GetStretch: Boolean;
    procedure SetStretch(const Value: Boolean);
    procedure DesignFreeNotification(const Sender: TFreeNotificationObserver);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Find by name a component in currently loaded design.
      When the name is not found, raises exception (unless Required is @false,
      then it returns @nil). }
    function DesignedComponent(const ComponentName: String;
      const Required: Boolean = true): TComponent;

    function FindRequiredComponent(const ComponentName: String): TComponent; deprecated 'use DesignedComponent(ComponentName)';
    function FindComponent(const ComponentName: String): TComponent; deprecated 'use DesignedComponent(ComponentName, false)';

    { Loaded design, only for internal usage. @exclude }
    property InternalDesign: TCastleUserInterface read FDesign;
  published
    { Load file with the user interface design.
      The file should be in the format created by Castle Game Engine Editor
      (see CastleComponentSerialize unit) with .castle-user-interface extension.
      Setting this property automatically unloads the previous design,
      and loads a new one (if the value is <> '').

      After loading a design, you can use @link(DesignedComponent)
      to find the components inside.
      So e.g. this works:

      @longCode(#
      var
        MyDesign: TCastleDesign;
        MyButton: TCastleButton;
      begin
        MyDesign := TCastleDesign.Create(...);
        MyDesign.URL := 'castle-data:/my_user_interface.castle-user-interface';
        MyButton := MyDesign.DesignedComponent('MyButton') as TCastleButton;
        MyButton.OnClick := ...;
      end;
      #)
    }
    property URL: String read FURL write SetURL;

    { Whether the loaded component (from @link(URL)) fills the rectangle
      of this TCastleDesign instance.

      By default this is @true, which makes sense if the loaded component
      can be freely stretched. In this case, the loaded component will have
      @link(FullSize) set to @true, so it fills this TCastleDesign instance.
      And you can control the size of everything by controlling the size
      of this TCastleDesign instance.

      You can set this to @false if the loaded component should not
      be arbitrarily stretched. E.g. maybe it is a TCastleImageControl
      with @link(TCastleImageControl.Stretch) = @true.
      Maybe it is TCastleButton with @link(TCastleButton.AutoSize) = @true.
      While every component can be forced to be stretched (by setting FullSize
      to @true), it may look bad in some circumstances.
      In such case, you should set this property to @false.
      You can also set @link(AutoSizeToChildren) to @true, to adjust the size of this
      TCastleDesign instance to the child.
    }
    property Stretch: Boolean read GetStretch write SetStretch default true;

    { URL used to perform editor "Revert" operation. }
    property RevertURL: String read FRevertURL write FRevertURL;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleDesign -------------------------------------------------------------- }

constructor TCastleDesign.Create(AOwner: TComponent);
begin
  inherited;
  FDesignObserver := TFreeNotificationObserver.Create(Self);
  FDesignObserver.OnFreeNotification := {$ifdef FPC}@{$endif}DesignFreeNotification;
  FStretchWhenNotLoaded := true;
end;

destructor TCastleDesign.Destroy;
begin
  inherited;
end;

procedure TCastleDesign.DesignFreeNotification(const Sender: TFreeNotificationObserver);
begin
  FDesign := nil;
end;

procedure TCastleDesign.SetURL(const Value: String);
const
  MaxLoadDepth = 32; // 64 is too much for macOS/x86_64; although for Linux/x86_64 even 1024 was OK
begin
  if FURL <> Value then
  begin
    if FDesign <> nil then
    begin
      FStretchWhenNotLoaded := FDesign.FullSize;
      FreeAndNil(FDesignOwner);
      Assert(FDesign = nil); // DesignFreeNotification should set FDesign to nil
    end;

    FURL := Value;

    if Value <> '' then
    begin
      try
        { Check LoadDepth = MaxLoadDepth to prevent infinite loop when referencing your own design. }
        if LoadDepth = MaxLoadDepth then
          raise EMaximumDesignDepth.CreateFmt('Exceeded maximum depth (%d) when loading TCastleDesign within TCastleDesign. It is possible that TCastleDesign references its own design', [
            MaxLoadDepth
          ]);
        Inc(LoadDepth);
        try
          FDesignOwner := TComponent.Create(Self);
          FDesign := UserInterfaceLoad(Value, FDesignOwner);
        finally Dec(LoadDepth) end;
      except
        { If loading design file failed, and we're inside CGE editor,
          merely report a warning. This allows deserializing in CGE editor
          designs with broken URLs. }
        on E: Exception do
        begin
          if CastleDesignMode then
          begin
            WritelnWarning('TCastleDesign', 'Failed to load design "%s": %s', [URIDisplay(URL), ExceptMessage(E)]);
            Exit;
          end else
            raise;
        end;
      end;

      { This prevents this child from
        - being serialized (in case TCastleDesign is placed inside
          a hierachy saved using UserInterfaceSave. Only URL should be saved.)
        - or even be visible in the hierachy of castle-editor.
      }
      FDesign.SetTransient;
      FDesign.FullSize := FStretchWhenNotLoaded;
      FDesignObserver.Observed := FDesign;
      InsertFront(FDesign);
    end;
  end;
end;

function TCastleDesign.GetStretch: Boolean;
begin
  { Keep this property synchronized with FDesign.FullSize, when FDesign exists.
    This way when user changes FDesign.FullSize directly,
    then the TCastleDesign.Stretch property also changes automatically. }
  if FDesign <> nil then
    Result := FDesign.FullSize
  else
    Result := FStretchWhenNotLoaded;
end;

procedure TCastleDesign.SetStretch(const Value: Boolean);
begin
  if FDesign <> nil then
    FDesign.FullSize := Value
  else
    FStretchWhenNotLoaded := Value;
end;

function TCastleDesign.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'URL') or
     (PropertyName = 'Stretch') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

function TCastleDesign.DesignedComponent(const ComponentName: String;
  const Required: Boolean): TComponent;
begin
  if FDesignOwner <> nil then
    Result := FDesignOwner.FindComponent(ComponentName)
  else
    Result := nil;

  if Required and (Result = nil) then
    raise EComponentNotFound.CreateFmt('Cannot find component named "%s" in design "%s"', [ComponentName, Name]);
end;

function TCastleDesign.FindRequiredComponent(const ComponentName: String): TComponent;
begin
  Result := DesignedComponent(ComponentName, true);
end;

function TCastleDesign.FindComponent(const ComponentName: String): TComponent;
begin
  Result := DesignedComponent(ComponentName, false);
end;

{$endif read_implementation}
