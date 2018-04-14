{
  Copyright 2018 Benedikt Magnus.

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
    TLocalisationHelperFields = class
      FOnUpdateLocalised: TOnUpdateLocalisationEvent;
      FLastLocalisedText: String;
    end;

  { Base class for all user interface controls using the localisation.
    Allows to automatically localise the text of the control and adjust to language changes. }
  type
    TUIControlFontHelperLocalised = class helper for TUIControlFont
    strict private
      procedure SetOnUpdateLocalised(const AOnUpdateLocalised: TOnUpdateLocalisationEvent);
    protected
      function Fields: TLocalisationHelperFields; inline;
    strict protected
      function GetLocalisationID: String;
      procedure SetLocalisationID(const ALocalisationID: String);
    public
      constructor Create(AOwner: TComponent);
      procedure BeforeDestruction;
      { Localisation ID to load a localised caption. }
      property LocalisationID: string read GetLocalisationID write SetLocalisationID;
      property OnUpdateLocalised: TOnUpdateLocalisationEvent write SetOnUpdateLocalised;
    end;

    TCastleLabelHelperLocalised = class helper for TCastleLabel
    strict private
      procedure UpdateLocalisedCaption;
    public
      constructor Create(AOwner: TComponent);
    end;

    TCastleButtonHelperLocalised = class helper for TCastleButton
    strict private
      procedure UpdateLocalisedCaption;
    public
      constructor Create(AOwner: TComponent);
    end;

    TCastleEditHelperLocalised = class helper for TCastleEdit
    strict private
      procedure UpdateLocalisedText;
    public
      constructor Create(AOwner: TComponent);
    end;

{$endif read_interface}

{$ifdef read_implementation}

{ TUIControlFontHelperLocalised ----------------------------------------------------- }

constructor TUIControlFontHelperLocalised.Create(AOwner: TComponent);
begin
  inherited;

  Tag := PtrUInt(TLocalisationHelperFields.Create);
end;

procedure TUIControlFontHelperLocalised.BeforeDestruction;
begin
  if Assigned(Fields.FOnUpdateLocalised) then
    Localisation.OnUpdateLocalisation.Remove(Fields.FOnUpdateLocalised);

  inherited;
end;

procedure TUIControlFontHelperLocalised.SetOnUpdateLocalised(const AOnUpdateLocalised: TOnUpdateLocalisationEvent);
begin
  Fields.FOnUpdateLocalised := AOnUpdateLocalised;
  if Name <> '' then
    AOnUpdateLocalised();
end;

function TUIControlFontHelperLocalised.Fields: TLocalisationHelperFields;
begin
  Result := TLocalisationHelperFields(Tag);
end;

function TUIControlFontHelperLocalised.GetLocalisationID: String;
begin
  Result := Name;
end;

procedure TUIControlFontHelperLocalised.SetLocalisationID(const ALocalisationID: String);
begin
  if ALocalisationID <> Name then
  begin
    if Assigned(Fields.FOnUpdateLocalised) then
    begin
      if Name = '' then
        Localisation.OnUpdateLocalisation.Add(Fields.FOnUpdateLocalised)
      else if ALocalisationID = '' then
        Localisation.OnUpdateLocalisation.Remove(Fields.FOnUpdateLocalised);

      Name := ALocalisationID;
      Fields.FOnUpdateLocalised;
    end
    else
      Name := ALocalisationID;
  end;
end;

{ TCastleLabelHelperLocalised ----------------------------------------------------- }

constructor TCastleLabelHelperLocalised.Create(AOwner: TComponent);
begin
  inherited;

  OnUpdateLocalised := @UpdateLocalisedCaption;
end;

procedure TCastleLabelHelperLocalised.UpdateLocalisedCaption;
begin
  Caption := Localisation[LocalisationID];
end;

{ TCastleButtonHelperLocalised ----------------------------------------------------- }

constructor TCastleButtonHelperLocalised.Create(AOwner: TComponent);
begin
  inherited;

  OnUpdateLocalised := @UpdateLocalisedCaption;
end;

procedure TCastleButtonHelperLocalised.UpdateLocalisedCaption;
begin
  Caption := Localisation[LocalisationID];
end;

{ TCastleEditHelperLocalised ----------------------------------------------------- }

constructor TCastleEditHelperLocalised.Create(AOwner: TComponent);
begin
  inherited;

  OnUpdateLocalised := @UpdateLocalisedText;
end;

procedure TCastleEditHelperLocalised.UpdateLocalisedText;
begin
  if Text = Fields.FLastLocalisedText then //Only localise if the text hasn't been changed manually.
  begin
    Fields.FLastLocalisedText := Localisation[LocalisationID];
    Text := Fields.FLastLocalisedText;
  end;
end;

{$endif read_implementation}
