{%MainUnit x3dloadinternalspine.pas}
{
  Copyright 2019-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Extract extra information from Spine URL.
  See https://castle-engine.io/spine .
  For example for URL 'my_animation.json#skin:my_skin_name,atlas:my_atlas_name',
  it returns 'my_skin_name' as SkinName,
  and returns 'my_atlas_name' as CustomAtlasName. }
procedure URIExtractInfo(const BaseUrl: String; out SkinName, CustomAtlasName: String);

  procedure ProcessAnchorPart(const Part: String);
  var
    Semicolon: Integer;
    PartName, PartValue: String;
  begin
    Semicolon := Pos(':', Part);

    if Semicolon = 0 then
    begin
      WritelnWarning('Spine URL anchor is not empty, and it does not contain a semicolon: "%s". To specify Spine skin, use the new syntax "xxx.json#skin:my_skin_name" instead of "xxx.json#my_skin_name", IOW add the "skin:" specifier.', [
        URIDisplay(BaseUrl)
      ]);
      if SkinName <> '' then
        WritelnWarning('Multiple skin names given in Spine URL "%s"', [URIDisplay(BaseUrl)]);
      SkinName := Part;
    end else
    begin
      PartName := Copy(Part, 1, Semicolon - 1);
      PartValue := SEnding(Part, Semicolon + 1);

      if PartName = 'skin' then
      begin
        if SkinName <> '' then
          WritelnWarning('Multiple skin names given in Spine URL "%s"', [URIDisplay(BaseUrl)]);
        SkinName := PartValue;
      end else

      if PartName = 'atlas' then
      begin
        if CustomAtlasName <> '' then
          WritelnWarning('Multiple atlas names given in Spine URL "%s"', [URIDisplay(BaseUrl)]);
        CustomAtlasName := PartValue;
      end else

        WritelnWarning('Not recognized part name "%s" in Spine URL "%s"', [PartName, URIDisplay(BaseUrl)]);
    end;
  end;

var
  Anchor, AnchorPart: String;
  SeekPos: Integer;
begin
  URIGetAnchor(BaseUrl, Anchor, true);

  SkinName := '';
  CustomAtlasName := '';

  SeekPos := 1;
  repeat
    AnchorPart := NextToken(Anchor, SeekPos, [',']);
    if AnchorPart = '' then Break;
    ProcessAnchorPart(AnchorPart);
  until false;
end;
