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

  { Base class for all user interface controls using the localisation.
    Allows to automatically localise the text of the control and adjust to language changes. }
  TUIControlLocalised = class(TUIControlFont)
  strict private
    FOnUpdateLocalised: TOnUpdateLocalisationEvent;
    procedure SetOnUpdateLocalised(const AOnUpdateLocalised: TOnUpdateLocalisationEvent);
  strict protected
    FLocalisationID: String;
    procedure SetLocalisationID(const ALocalisationID: String);
  public
    destructor Destroy; override;
    { Localisation ID to load a localised caption. }
    property LocalisationID: string read FLocalisationID write SetLocalisationID;
    property OnUpdateLocalised: TOnUpdateLocalisationEvent write SetOnUpdateLocalised;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TUIControlLocalised -------------------------------------------------------- }

destructor TUIControlLocalised.Destroy;
begin
  if Assigned(FOnUpdateLocalised) then
    Localisation.OnUpdateLocalisation.Remove(FOnUpdateLocalised);

  inherited;
end;

procedure TUIControlLocalised.SetOnUpdateLocalised(const AOnUpdateLocalised: TOnUpdateLocalisationEvent);
begin
  FOnUpdateLocalised := AOnUpdateLocalised;
  if FLocalisationID <> '' then
    AOnUpdateLocalised();
end;

procedure TUIControlLocalised.SetLocalisationID(const ALocalisationID: String);
begin
  if ALocalisationID <> FLocalisationID then
  begin
    if Assigned(FOnUpdateLocalised) then
    begin
      if FLocalisationID = '' then
        Localisation.OnUpdateLocalisation.Add(FOnUpdateLocalised)
      else if ALocalisationID = '' then
        Localisation.OnUpdateLocalisation.Remove(FOnUpdateLocalised);

      FLocalisationID := ALocalisationID;
      FOnUpdateLocalised;
    end
    else
      FLocalisationID := ALocalisationID;
  end;
end;

{$endif read_implementation}
