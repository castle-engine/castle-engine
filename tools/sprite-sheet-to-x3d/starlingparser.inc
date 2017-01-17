procedure StarlingParser;
var
  Doc: TXMLDocument;
  Atlas, N: TDOMNode;
  AniName,
  Key: string;
  Frame: TFrame;
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
      Frame.X1 := StrToFloat(N.Attributes.GetNamedItem('x').NodeValue);
      Frame.Y1 := StrToFloat(N.Attributes.GetNamedItem('y').NodeValue);
      Frame.W := StrToFloat(N.Attributes.GetNamedItem('width').NodeValue);
      Frame.H := StrToFloat(N.Attributes.GetNamedItem('height').NodeValue);
      Frame.X2 := Frame.X1 + Frame.W;
      Frame.Y2 := Frame.Y1 + Frame.H;
      Frame.AX := 0.5;
      Frame.AY := 0.5;
      KeyParser(AniName, Key);
      AddFrame(Key, Frame);
      N := N.NextSibling;
    end;
  finally
    FreeAndNil(Doc);
  end;
end; 
