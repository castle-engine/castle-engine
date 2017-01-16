procedure Cocos2DParser;
  procedure ReadQuad(const ASrc: string; out V1, V2, V3, V4: integer);
  var
    R: TRegExpr;
  begin
    R := TRegExpr.Create;
    try
      R.Expression := '{{(\d+),(\d+)},{(\d+),(\d+)}}';
      if R.Exec(ASrc) and (R.SubExprMatchCount = 4) then
      begin
        V1 := StrToInt(R.Match[1]); 
        V2 := StrToInt(R.Match[2]);
        V3 := StrToInt(R.Match[3]);
        V4 := StrToInt(R.Match[4]);
      end;
    finally
      FreeAndNil(R);
    end;
  end;

  procedure ReadDual(const ASrc: string; out V1, V2: single);
  var
    R: TRegExpr;
  begin
    R := TRegExpr.Create;
    try
      R.Expression := '{(\d+.*\d*),(\d+.*\d*)}';
      if R.Exec(ASrc) and (R.SubExprMatchCount = 2) then
      begin
        V1 := StrToFloat(R.Match[1]);
        V2 := StrToFloat(R.Match[2]);
      end;
    finally
      FreeAndNil(R);
    end;
  end;

var
  Doc: TXMLDocument;
  Dict, N, N2, N3: TDOMNode;
  AniName,
  Key: string;
  Frame: TFrame;
  X, Y, W, H: integer;
  AX, AY: single;
begin
  ReadXMLFile(Doc, SSFullPath);
  try
    Dict := Doc.FindNode('plist').FirstChild;
    N := Dict.FirstChild;
    while N <> nil do
    begin
      if N.TextContent = 'frames' then
      begin
        N := N.NextSibling;   
        N2 := N.FirstChild;
        while N2 <> nil do
        begin
          if N2.NodeName = 'key' then
          begin
            AniName := N2.TextContent;
          end
          else
          if N2.NodeName = 'dict' then
          begin
            N3 := N2.FirstChild;
            AX := 0.5;
            AY := 0.5;
            while N3 <> nil do
            begin
              if (N3.TextContent = 'frame') or (N3.TextContent = 'textureRect') then
              begin
                N3 := N3.NextSibling;
                ReadQuad(N3.TextContent, X, Y, W, H);
              end
              else
              if N3.TextContent = 'anchor' then
              begin   
                N3 := N3.NextSibling;
                ReadDual(N3.TextContent, AX, AY);
              end;
              N3 := N3.NextSibling;
            end;   
            Frame.X1 := X;
            Frame.Y1 := Y;
            Frame.W := W;
            Frame.H := H;
            Frame.X2 := X + W;
            Frame.Y2 := Y + H;
            Frame.AX := AX;
            Frame.AY := AY;
            KeyParser(AniName, Key);
            AddFrame(Key, Frame);
          end;
          N2 := N2.NextSibling;
        end;
      end
      else
      if N.TextContent = 'metadata' then
      begin
        N := N.NextSibling;
        N2 := N.FirstChild;
        while N2 <> nil do
        begin
          if (N2.TextContent = 'textureFileName') then
          begin
            N2 := N2.NextSibling;
            Meta.Name := N2.TextContent;   
            ReadMeta(Meta);
          end;     
          N2 := N2.NextSibling;
        end;
      end;
      N := N.NextSibling;
    end;
  finally
    FreeAndNil(Doc);
  end;
end; 
