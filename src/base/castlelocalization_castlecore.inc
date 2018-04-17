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
    { A descendent of TCastleLabel that can be localised with CastleLocalization. }
    TCastleLabelLocalizable = class (TCastleLabel, ICastleLocalization)
    public
      procedure OnUpdateLocalization(const ALocalizedText: String);
    end;

    { A descendent of TCastleButton that can be localised with CastleLocalization. }
    TCastleButtonLocalizable = class (TCastleButton, ICastleLocalization)
    public
      procedure OnUpdateLocalization(const ALocalizedText: String);
    end;

    { A descendent of TCastleMenuButton that can be localised with CastleLocalization. }
    TCastleMenuButtonLocalizable = class (TCastleMenuButton, ICastleLocalization)
    public
      procedure OnUpdateLocalization(const ALocalizedText: String);
    end;

    { A descendent of TCastleEdit that can be localised with CastleLocalization. }
    TCastleEditLocalizable = class (TCastleEdit, ICastleLocalization)
    private
      FLastLocalizedText: String;
    public
      procedure OnUpdateLocalization(const ALocalizedText: String);
    end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleLabelLocalizable ----------------------------------------------------- }

procedure TCastleLabelLocalizable.OnUpdateLocalization(const ALocalizedText: String);
begin
  Caption := ALocalizedText;
end;

{ TCastleButtonLocalizable ----------------------------------------------------- }

procedure TCastleButtonLocalizable.OnUpdateLocalization(const ALocalizedText: String);
begin
  Caption := ALocalizedText;
end;

{ TCastleMenuButtonLocalizable ----------------------------------------------------- }

procedure TCastleMenuButtonLocalizable.OnUpdateLocalization(const ALocalizedText: String);
begin
  Caption := ALocalizedText;
end;

{ TCastleEditLocalizable ----------------------------------------------------- }

procedure TCastleEditLocalizable.OnUpdateLocalization(const ALocalizedText: String);
begin
  if Text = FLastLocalizedText then //Only localise if the text hasn't been changed manually.
  begin
    FLastLocalizedText := ALocalizedText;
    Text := FLastLocalizedText;
  end;
end;

{$endif read_implementation}