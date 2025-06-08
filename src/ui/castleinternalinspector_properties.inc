{%MainUnit castleinternalinspector.pas}
{
  Copyright 2015-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

type
  { Display properties of given component (@link(SelectedComponent)).
    This is "object inspector" using Castle Game Engine UI. }
  TCastleComponentProperties = class(TCastleUserInterface)
  strict private
    type
      { Display a single property of an object. }
      TPropertyDisplay = class(TComponent)
      strict private
        LabelName: TCastleLabel;
        EditValue: TCastleEdit;
        CheckboxValue: TCastleCheckbox;
        PropObject: TObject;
        PropInfo: PPropInfo;
        PropType: TPropertyType;
        procedure CheckboxChanged(Sender: TObject);
      public
        Ui: TCastleUserInterface;
        procedure Initialize(const APropObject: TObject;
          const APropInfo: PPropInfo; const APropName: String);
        procedure UpdateCurrentValue;
      end;
      TPropertyDisplayList = {$ifdef FPC}specialize{$endif} TObjectList<TPropertyDisplay>;

    var
      // designed components
      PropertyRowParent: TCastleUserInterface;
      LabelPropertiesHeader: TCastleLabel;
      ScrollProperties: TCastleScrollView;

      // other fields
      Ui: TCastleUserInterface;
      UiOwner: TComponent;
      RowFactory: TCastleComponentFactory;
      FSelectedComponent: TComponent;
      { Properties of the SelectedComponent. }
      Properties: TPropertyDisplayList;
      TimeToUpdatePropertiesValues: TFloatTime;

    procedure SetSelectedComponent(const Value: TComponent);
    { Update properties to reflect current FSelectedComponent.
      Only SetSelectedComponent needs to call it. }
    procedure UpdateProperties;
    { Only updates property Values (not the property names and types),
      assuming SelectedComponent didn't change. }
    procedure UpdatePropertiesValues;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    // designed components that are just public
    ButtonPropertiesHide: TCastleButton;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Resize; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;

    { Selected object in hierarchy, for which we display the properties.
      Can be changed by user while operating this UI. }
    property SelectedComponent: TComponent read FSelectedComponent write SetSelectedComponent;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TPropertyDisplay ----------------------------------------------------------- }

procedure TCastleComponentProperties.TPropertyDisplay.Initialize(
  const APropObject: TObject; const APropInfo: PPropInfo; const APropName: String);
begin
  PropObject := APropObject;
  PropInfo := APropInfo;
  PropType := PropertyType(PropInfo);

  ForceFallbackLook(Ui);

  LabelName := FindRequiredComponent('PropName') as TCastleLabel;
  LabelName.Caption := APropName;

  case PropType of
    ptBoolean:
      begin
        CheckboxValue := FindRequiredComponent('PropValueCheckbox') as TCastleCheckbox;
        CheckboxValue.OnChange := {$ifdef FPC}@{$endif} CheckboxChanged;
        // free (don't waste memory) unused UI
        FindRequiredComponent('PropValue').Free;
      end;
    else
      begin
        EditValue := FindRequiredComponent('PropValue') as TCastleEdit;
        // free (don't waste memory) unused UI
        FindRequiredComponent('PropValueCheckboxParent').Free;
      end;
  end;

  UpdateCurrentValue;
end;

procedure TCastleComponentProperties.TPropertyDisplay.CheckboxChanged(Sender: TObject);
begin
  PropertySetBoolean(PropObject, PropInfo, CheckboxValue.Checked);
  // refresh colors (based on whether new value is default)
  UpdateCurrentValue;
end;

procedure TCastleComponentProperties.TPropertyDisplay.UpdateCurrentValue;

  procedure AdjustColorsBasedOnPropertyDefault(
    const Edit: TCastleEdit; const IsDefault: Boolean); overload;
  begin
    if IsDefault then
    begin
      // TCastleEdit defaults
      Edit.FocusedColor := Black;
      Edit.UnfocusedColor := Vector4(0.25, 0.25, 0.25, 1);
    end else
    begin
      Edit.FocusedColor := Blue;
      Edit.UnfocusedColor := Vector4(0.25, 0.25, 1, 1);
    end;
  end;

  procedure AdjustColorsBasedOnPropertyDefault(
    const Checkbox: TCastleCheckbox; const IsDefault: Boolean); overload;
  begin
    if IsDefault then
    begin
      Checkbox.CheckboxColor := Black;
    end else
    begin
      Checkbox.CheckboxColor := Blue;
    end;
  end;

var
  PropName, PropValue: String;
begin
  Assert((PropType = ptBoolean) = Assigned(CheckboxValue));
  Assert((PropType = ptBoolean) = not Assigned(EditValue));
  case PropType of
    ptBoolean:
      begin
        CheckboxValue.Checked := PropertyGetBoolean(PropObject, PropInfo);
        AdjustColorsBasedOnPropertyDefault(CheckboxValue,
          PropertyHasDefaultValue(PropObject, PropInfo, true));
      end;
    else
      begin
        if PropertyGet(PropObject, PropInfo, PropName, PropValue) then
        begin
          EditValue.Text := PropValue;
          AdjustColorsBasedOnPropertyDefault(EditValue,
            PropertyHasDefaultValue(PropObject, PropInfo, true));
        end else
          WritelnWarning('Cannot read property name/value, but it was possible to read it earlier');
      end;
  end;
end;

{ TCastleComponentProperties ------------------------------------------------- }

constructor TCastleComponentProperties.Create(AOwner: TComponent);
var
  PropertyRowTemplate: TCastleUserInterface;
begin
  inherited;
  UiOwner := TComponent.Create(nil);
  Ui := StringToComponent({$I designs/component_properties.castle-user-interface.inc}, UiOwner)
    as TCastleUserInterface;
  InsertFront(Ui);

  ScrollProperties := UiOwner.FindRequiredComponent('ScrollProperties') as TCastleScrollView;
  LabelPropertiesHeader := UiOwner.FindRequiredComponent('LabelPropertiesHeader') as TCastleLabel;
  PropertyRowParent := UiOwner.FindRequiredComponent('PropertyRowParent') as TCastleUserInterface;
  ButtonPropertiesHide := UiOwner.FindRequiredComponent('ButtonPropertiesHide') as TCastleButton;
  PropertyRowTemplate := UiOwner.FindRequiredComponent('PropertyRowTemplate') as TCastleUserInterface;

  // create factory, remove template that is no longer useful
  RowFactory := TCastleComponentFactory.Create(nil);
  RowFactory.LoadFromComponent(PropertyRowTemplate);
  FreeAndNil(PropertyRowTemplate);

  Properties := TPropertyDisplayList.Create(true);
end;

destructor TCastleComponentProperties.Destroy;
begin
  { set to nil by SetSelectedComponent, to detach free notification }
  SelectedComponent := nil;

  FreeAndNil(Properties);
  FreeAndNil(UiOwner);
  FreeAndNil(RowFactory);
  inherited;
end;

procedure TCastleComponentProperties.Update(
  const SecondsPassed: Single; var HandleInput: Boolean);
const
  { Delay between updating properties. }
  UpdatePropertiesValuesInterval = 0.5;
begin
  if Ui.Exists then // do not do UpdatePropertiesValues when not visible, to save speed
  begin
    TimeToUpdatePropertiesValues := TimeToUpdatePropertiesValues - SecondsPassed;
    if TimeToUpdatePropertiesValues < 0 then
    begin
      UpdatePropertiesValues;
      TimeToUpdatePropertiesValues := UpdatePropertiesValuesInterval;
    end;
  end;
end;

procedure TCastleComponentProperties.UpdateProperties;

  procedure AddPropertyRow(const PropObject: TObject; const PropInfo: PPropInfo;
    const PropName: String);
  var
    PropertyDisplay: TPropertyDisplay;
  begin
    PropertyDisplay := TPropertyDisplay.Create(Self);

    PropertyDisplay.Ui := RowFactory.ComponentLoad(PropertyDisplay) as TCastleUserInterface;
    PropertyDisplay.Ui.Culling := true; // many such rows are often not visible, in scroll view
    PropertyDisplay.Ui.Width := EffectiveWidthForChildren;

    PropertyDisplay.Initialize(PropObject, PropInfo, PropName);

    Properties.Add(PropertyDisplay);
    PropertyRowParent.InsertFront(PropertyDisplay.Ui);
  end;

  function PropertyShow(const PropObject: TComponent; const PropInfo: PPropInfo): Boolean;
  var
    PropName: String;
  begin
    PropName := PropInfo^.Name;
    if (PropName = 'Name') and
       (csSubComponent in PropObject.ComponentStyle) then
    begin
      { Do not show names of subcomponents, they are not useful (to view or edit).
        CastleComponentSerialize also doesn't save them (see TCastleJsonWriter.StreamProperty),
        CGE editor also doesn't show them (see TDesignFrame.InspectorFilter). }
      Exit(false);
    end;

    Result := true;
  end;

var
  PropInfos: TPropInfoList;
  PropInfo: PPropInfo;
  I: Integer;
  PropName, PropValue: String;
begin
  // frees TPropertyOwner instances along with their UIs
  Properties.Clear;

  if FSelectedComponent <> nil then
  begin
    PropInfos := TPropInfoList.Create(FSelectedComponent, tkProperties);
    try
      for I := 0 to PropInfos.Count - 1 do
      begin
        PropInfo := PropInfos.Items[I];
        if PropertyShow(FSelectedComponent, PropInfo) and
           { We get but don't actually pass PropValue to AddPropertyRow,
             UpdateCurrentValue (called by AddPropertyRow) will read it again. }
           PropertyGet(FSelectedComponent, PropInfo, PropName, PropValue) then
          AddPropertyRow(FSelectedComponent, PropInfo, PropName);
      end;
    finally FreeAndNil(PropInfos) end;
  end;
end;

procedure TCastleComponentProperties.UpdatePropertiesValues;
var
  Po: TPropertyDisplay;
begin
  for Po in Properties do
    Po.UpdateCurrentValue;
end;

procedure TCastleComponentProperties.SetSelectedComponent(const Value: TComponent);

  { Used to display C as String. }
  function ComponentCaption(const C: TComponent): String;
  begin
    Result := C.Name + ' (' + C.ClassName + ')';
  end;

begin
  if FSelectedComponent <> Value then
  begin
    if FSelectedComponent <> nil then
      FSelectedComponent.RemoveFreeNotification(Self);
    FSelectedComponent := Value;
    if FSelectedComponent <> nil then
      FSelectedComponent.FreeNotification(Self);

    if FSelectedComponent <> nil then
      LabelPropertiesHeader.Caption := 'Properties - ' + ComponentCaption(FSelectedComponent)
    else
      LabelPropertiesHeader.Caption := 'Properties';

    ScrollProperties.Scroll := ScrollProperties.ScrollMin;

    UpdateProperties;
  end;
end;

procedure TCastleComponentProperties.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FSelectedComponent) then
    { set to nil by SetSelectedComponent to clean nicely }
    SelectedComponent := nil;
end;

procedure TCastleComponentProperties.Resize;
var
  C: TCastleUserInterface;
begin
  inherited;
  { We need to manually adjust rows widths to follow available space,
    righ now this is not doable by simple properties,
    because ScrollArea looks at children for size (and we cannot look
    at children for height, but at the same time determine children width). }
  for C in PropertyRowParent do
    C.Width := EffectiveWidthForChildren;
end;

{$endif read_implementation}
