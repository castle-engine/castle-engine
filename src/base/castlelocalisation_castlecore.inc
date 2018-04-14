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
    TCastleLabelLocalisable = class (TCastleLabel, ICastleLocalisation)
    public
      procedure OnUpdateLocalisation(ALocalisedText: String);
    end;

    TCastleButtonLocalisable = class (TCastleButton, ICastleLocalisation)
    public
      procedure OnUpdateLocalisation(ALocalisedText: String);
    end;

    TCastleMenuButtonLocalisable = class (TCastleMenuButton, ICastleLocalisation)
    public
      procedure OnUpdateLocalisation(ALocalisedText: String);
    end;

    TCastleEditLocalisable = class (TCastleEdit, ICastleLocalisation)
    private
      FLastLocalisedText: String;
    public
      procedure OnUpdateLocalisation(ALocalisedText: String);
    end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleLabelLocalisable ----------------------------------------------------- }

procedure TCastleLabelLocalisable.OnUpdateLocalisation(ALocalisedText: String);
begin
  Caption := ALocalisedText;
end;

{ TCastleButtonLocalisable ----------------------------------------------------- }

procedure TCastleButtonLocalisable.OnUpdateLocalisation(ALocalisedText: String);
begin
  Caption := ALocalisedText;
end;

{ TCastleMenuButtonLocalisable ----------------------------------------------------- }

procedure TCastleMenuButtonLocalisable.OnUpdateLocalisation(ALocalisedText: String);
begin
  Caption := ALocalisedText;
end;

{ TCastleEditLocalisable ----------------------------------------------------- }

procedure TCastleEditLocalisable.OnUpdateLocalisation(ALocalisedText: String);
begin
  if Text = FLastLocalisedText then //Only localise if the text hasn't been changed manually.
  begin
    FLastLocalisedText := ALocalisedText;
    Text := FLastLocalisedText;
  end;
end;

{$endif read_implementation}
