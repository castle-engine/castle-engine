procedure StarlingParser;
var
  Doc: TXMLDocument;
  Atlas, N: TDOMNode;
  AniName,
  Key: string;
  Frame: TFrame;
  OW, OH, FX, FY: integer;
  HasOriginFrame: boolean;
begin
  ReadXMLFile(Doc, SSFullPath);
  try
    Atlas := Doc.FindNode('TextureAtlas');
    Meta.Name := Atlas.Attributes.GetNamedItem('imagePath').NodeValue;
    ReadMeta(Meta);
    N := Atlas.FirstChild;
    while N <> nil do
    begin
      if N.NodeName <> 'SubTexture' then
      begin
        N := N.NextSibling;
        continue;
      end;
      AniName := N.Attributes.GetNamedItem('name').NodeValue;
      Frame.X1 := StrToInt(N.Attributes.GetNamedItem('x').NodeValue);
      Frame.Y1 := StrToInt(N.Attributes.GetNamedItem('y').NodeValue);
      Frame.W := StrToInt(N.Attributes.GetNamedItem('width').NodeValue);
      Frame.H := StrToInt(N.Attributes.GetNamedItem('height').NodeValue);
      HasOriginFrame := false;
      if N.Attributes.GetNamedItem('frameX') <> nil then
      begin
        FX := -ReadInteger(N.Attributes.GetNamedItem('frameX'), 0);
        FY := -ReadInteger(N.Attributes.GetNamedItem('frameY'), 0);
        OW := ReadInteger(N.Attributes.GetNamedItem('frameWidth'), Frame.W);
        OH := ReadInteger(N.Attributes.GetNamedItem('frameHeight'), Frame.H); 
        HasOriginFrame := true;
      end;
      Frame.X2 := Frame.X1 + Frame.W;
      Frame.Y2 := Frame.Y1 + Frame.H;
      if HasOriginFrame then
        CalculateAnchor(FX, FY, OW, OH,
            Round(Frame.X1), Round(Frame.Y1), Frame.W, Frame.H,
            Frame.AX, Frame.AY)
      else
      begin
        Frame.AX := 0.5;
        Frame.AY := 0.5;
      end;
      KeyParser(AniName, Key);
      AddFrame(Key, Frame);
      N := N.NextSibling;
    end;
  finally
    FreeAndNil(Doc);
  end;
end; 
