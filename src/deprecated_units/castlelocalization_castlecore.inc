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

{ TODO:

  This file should be removed, use TranslateProperties routine
  to uniformly translate UI stuff in CGE now.
  However, it's hard to fix it without breaking compatibility
  (of translation files) because now, CastleLocalization only allows
  a single string per component (while TranslateProperties allows
  to expose e.g. 2 properties on a component, like TCastleEdit.Text
  and TCastleEdit.Placeholder).
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

    { A descendent of TCastleOnScreenMenuItem that can be localised with CastleLocalization. }
    TCastleOnScreenMenuItemLocalizable = class (TCastleOnScreenMenuItem, ICastleLocalization)
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

{ TCastleOnScreenMenuItemLocalizable ----------------------------------------------------- }

procedure TCastleOnScreenMenuItemLocalizable.OnUpdateLocalization(const ALocalizedText: String);
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
