procedure StarlingParser;
var
  Doc: TXMLDocument;
  Atlas, N: TDOMElement;
  I: TXMLElementIterator;
  AniName, Key: string;
  Frame: TFrame;
  OW, OH, FX, FY: integer;
  HasOriginFrame: boolean;
begin
  Doc := URLReadXML(SSFullPath);
  try
    Atlas := Doc.FindNode('TextureAtlas') as TDOMElement;
    Meta.Name := Atlas.AttributeString('imagePath');
    ReadMeta(Meta);

    I := Atlas.ChildrenIterator('SubTexture');
    try
      while I.GetNext do
      begin
        N := I.Current;
        AniName := N.AttributeString('name');
        Frame.X1 := N.AttributeInteger('x');
        Frame.Y1 := N.AttributeInteger('y');
        Frame.W := N.AttributeInteger('width');
        Frame.H := N.AttributeInteger('height');
        HasOriginFrame := false;
        if N.Attributes.GetNamedItem('frameX') <> nil then
        begin
          FX := -N.AttributeIntegerDef('frameX', 0);
          FY := -N.AttributeIntegerDef('frameY', 0);
          OW := N.AttributeIntegerDef('frameWidth', Frame.W);
          OH := N.AttributeIntegerDef('frameHeight', Frame.H);
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
      end;
    finally FreeAndNil(I) end;
  finally
    FreeAndNil(Doc);
  end;
end;
