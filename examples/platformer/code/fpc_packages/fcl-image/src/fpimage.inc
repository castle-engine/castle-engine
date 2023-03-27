{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    TFPCustomImage implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ TFPCustomImage }

constructor TFPCustomImage.create (AWidth,AHeight:integer);
begin
  inherited create;
  FExtra := TStringList.Create;
  FWidth := 0;
  FHeight := 0;
  FPalette := nil;
  SetSize (AWidth,AHeight);
end;

destructor TFPCustomImage.destroy;
begin
  FExtra.Free;
  if assigned (FPalette) then
    FPalette.Free;
  inherited;
end;

procedure TFPCustomImage.LoadFromStream (Str:TStream; Handler:TFPCustomImagereader);
begin
  Handler.ImageRead (Str, self);
end;

procedure TFPCustomImage.LoadFromFile (const filename:String; Handler:TFPCustomImageReader);
var
  fs : TStream;
begin
  if FileExists (filename) then
    begin
    fs := TFileStream.Create (filename, fmOpenRead);
    try
      LoadFromStream (fs, handler);
    finally
      fs.Free;
    end;
    end
  else
    FPImgError (StrNoFile, [filename]);
end;

procedure TFPCustomImage.SaveToStream (Str:TStream; Handler:TFPCustomImageWriter);
begin
  Handler.ImageWrite (Str, Self);
end;

procedure TFPCustomImage.SaveToFile (const filename:String; Handler:TFPCustomImageWriter);
var
  fs : TStream;
begin
  fs := TFileStream.Create (filename, fmCreate);
  try
    SaveToStream (fs, handler);
  finally
    fs.Free;
  end
end;

function TFPCustomImage.SaveToFile (const filename:String):boolean;

var h : TFPCustomImageWriterClass;
    Writer : TFPCustomImageWriter;
    Msg : string;

begin
  Msg := '';
  try
    h := FindWriterFromFileName(filename);
    Result := assigned (h);
    if Result then
      begin
      Writer := h.Create;
      try
        SaveTofile (filename, Writer);
      finally
        Writer.Free;
      end;
      end;
  except
    on e : exception do
      Msg := e.message;
  end;
  if (Msg<>'') then
    FPImgError (StrWriteWithError, [Msg]);
end;


procedure TFPCustomImage.LoadFromStream (Str:TStream);
var r : integer;
    h : TFPCustomImageReaderClass;
    reader : TFPCustomImageReader;
    msg : string;
    d : TIHData;
    startPos: Int64;
begin
  msg := '';
  startPos := str.Position;
  with ImageHandlers do
    try
      r := count-1;
      while (r >= 0) do
        begin
        d := GetData(r);
        if assigned (d) then
          h := d.FReader
        else
          h := nil;
        if assigned (h) then
          begin
          reader := h.Create;
          with reader do
            try
              if CheckContents (str) then
                try
                  FStream := str;
                  FImage := self;
                  InternalRead (str, self);
                  msg := '';
                  break;
                except
                  on e : exception do
                    msg := e.message;
                end;
            finally
              Free;
              str.Position := startPos;
            end;
          end;
        dec (r);
        end;
    except
      on e : exception do
        FPImgError (StrCantDetermineType, [e.message]);
    end;
  if r < 0 then
    if msg = '' then
      FPImgError (StrNoCorrectReaderFound)
    else
      FPImgError (StrReadWithError, [Msg]);
end;

function TFPCustomImage.LoadFromFile (const filename:String):boolean;
var f : TFileStream;
    h : TFPCustomImageReaderClass;
    reader : TFPCustomImageReader;
    Msg : string;
begin
  Msg := '';
  try
    h := FindReaderFromFileName(filename);
    Result := assigned (h);
    if Result then
      begin
      reader := h.Create;
      try
        loadfromfile (filename, reader);
      finally
        Reader.Free;
      end;
      end;
  except
    on e : exception do
      Msg := e.message;
  end;
  if Msg = '' then
    begin
    if h = nil then
      begin
      f := TFileStream.Create (filename, fmOpenRead);
      try
        LoadFromStream (f);
      finally
        f.Free;
      end;
      end;
    end
  else
    FPImgError (StrReadWithError, [Msg]);
end;

procedure TFPCustomImage.SetHeight (Value : integer);
begin
  if Value <> FHeight then
    SetSize (FWidth, Value);
end;

procedure TFPCustomImage.SetWidth (Value : integer);
begin
  if Value <> FWidth then
    SetSize (Value, FHeight);
end;

procedure TFPCustomImage.SetSize (AWidth, AHeight : integer);
begin
  FWidth := AWidth;
  FHeight := AHeight;
end;

procedure TFPCustomImage.SetExtraValue (index:integer; const AValue:string);
var s : string;
    p : integer;
begin
  s := FExtra[index];
  p := pos ('=', s);
  if p > 0 then
    FExtra[index] := copy(s, 1, p) + AValue
  else
    FPImgError (StrInvalidIndex,[ErrorText[StrImageExtra],index]);
end;

function TFPCustomImage.GetExtraValue (index:integer) : string;
var s : string;
    p : integer;
begin
  s := FExtra[index];
  p := pos ('=', s);
  if p > 0 then
    result := copy(s, p+1, maxint)
  else
    result := '';
end;

procedure TFPCustomImage.SetExtraKey (index:integer; const AValue:string);
var s : string;
    p : integer;
begin
  s := FExtra[index];
  p := pos('=',s);
  if p > 0 then
    s := AValue + copy(s,p,maxint)
  else
    s := AValue;
  FExtra[index] := s;
end;

function TFPCustomImage.GetExtraKey (index:integer) : string;
begin
  result := FExtra.Names[index];
end;

procedure TFPCustomImage.SetExtra (const key:String; const AValue:string);
begin
  FExtra.values[key] := AValue;
end;

function TFPCustomImage.GetExtra (const key:String) : string;
begin
  result := FExtra.values[key];
end;

function  TFPCustomImage.ExtraCount : integer;
begin
  result := FExtra.count;
end;

const dumchar = ';';
class function TFPCustomImage.FindHandlerFromExtension(extension: String
  ): TIHData;
var s : string;
    r : integer;
begin
  if extension='' then
    Exit(nil);
  extension := lowercase (extension);
  if (extension <> '') and (extension[1] = '.') then
    delete (extension,1,1);
  with ImageHandlers do
    begin
      r := count-1;
      s := dumchar  + extension + dumchar;
      while (r >= 0) do
        begin
        Result := GetData(r);
        if (pos(s, dumchar+Result.Fextension+dumchar) <> 0) then
          Exit;
        dec (r);
        end;
    end;
  Result := nil;
end;

class function TFPCustomImage.FindHandlerFromStream(Str: TStream): TIHData;
var r : integer;
    p: Int64;
    reader: TFPCustomImageReader;
begin
  r := ImageHandlers.Count-1;
  p := Str.Position;
  while (r >= 0) do
    begin
    Result := ImageHandlers.GetData(r);
    if Result.Reader<>nil then
      begin
      reader := Result.Reader.Create;
      try
        if reader.CheckContents(Str) then
          Exit;
      finally
        reader.free;
        Str.Position := p;
      end;
      end;
    dec (r);
    end;
  Result := nil;
end;

class function TFPCustomImage.FindReaderFromExtension(const extension: String
  ): TFPCustomImageReaderClass;
var d : TIHData;
begin
  d := FindHandlerFromExtension(extension);
  if d<>nil then
    Result := d.FReader
  else
    Result := nil;
end;

class function TFPCustomImage.FindReaderFromFileName(const filename: String
  ): TFPCustomImageReaderClass;
begin
  Result := FindReaderFromExtension(ExtractFileExt(filename));
end;

class function TFPCustomImage.FindReaderFromStream(
  Str: TStream): TFPCustomImageReaderClass;
var d : TIHData;
begin
  d := FindHandlerFromStream(Str);
  if d<>nil then
    Result := d.FReader
  else
    Result := nil;
end;

class function TFPCustomImage.FindWriterFromExtension(const extension: String
  ): TFPCustomImageWriterClass;
var d : TIHData;
begin
  d := FindHandlerFromExtension(extension);
  if d<>nil then
    Result := d.FWriter
  else
    Result := nil;
end;

class function TFPCustomImage.FindWriterFromFileName(const filename: String
  ): TFPCustomImageWriterClass;
begin
  Result := FindWriterFromExtension(ExtractFileExt(filename));
end;

procedure TFPCustomImage.RemoveExtra (const key:string);
var p : integer;
begin
  p := FExtra.IndexOfName(key);
  if p >= 0 then
    FExtra.Delete (p);
end;

procedure TFPCustomImage.SetPixel (x,y:integer; Value:integer);
begin
  CheckPaletteIndex (Value);
  CheckIndex (x,y);
  SetInternalPixel (x,y,Value);
end;

function TFPCustomImage.GetPixel (x,y:integer) : integer;
begin
  CheckIndex (x,y);
  result := GetInternalPixel(x,y);
end;

procedure TFPCustomImage.SetColor (x,y:integer; const Value:TFPColor);
begin
  CheckIndex (x,y);
  SetInternalColor (x,y,Value);
end;

function TFPCustomImage.GetColor (x,y:integer) : TFPColor;
begin
  CheckIndex (x,y);
  result := GetInternalColor(x,y);
end;

procedure TFPCustomImage.SetInternalColor (x,y:integer; const Value:TFPColor);
var i : integer;
begin
  i := FPalette.IndexOf (Value);
  SetInternalPixel (x,y,i);
end;

function TFPCustomImage.GetInternalColor (x,y:integer) : TFPColor;
begin
  result := FPalette.Color[GetInternalPixel(x,y)];
end;

function TFPCustomImage.GetUsePalette : boolean;
begin
  result := assigned(FPalette);
end;

procedure TFPCustomImage.SetUsePalette(Value:boolean);
begin
  if Value <> assigned(FPalette)
  then
    if Value
    then
      begin
        FPalette := TFPPalette.Create (0);
        // FPalette.Add (colTransparent);
      end
    else
      begin
        FPalette.Free;
        FPalette := nil;
      end;
end;

procedure TFPCustomImage.CheckPaletteIndex (PalIndex:integer);
begin
  if UsePalette then
    begin
    if (PalIndex < -1) or (PalIndex >= FPalette.Count) then
      FPImgError (StrInvalidIndex,[ErrorText[StrPalette],PalIndex]);
    end
  else
    FPImgError (StrNoPaletteAvailable);
end;

procedure TFPCustomImage.CheckIndex (x,y:integer);
begin
  if (x < 0) or (x >= FWidth) then
    FPImgError (StrInvalidIndex,[ErrorText[StrImageX],x]);
  if (y < 0) or (y >= FHeight) then
    FPImgError (StrInvalidIndex,[ErrorText[StrImageY],y]);
end;

Procedure TFPCustomImage.Progress(Sender: TObject; Stage: TProgressStage;
                         PercentDone: Byte;  RedrawNow: Boolean; const R: TRect;
                         const Msg: AnsiString; var Continue: Boolean);
begin
  If Assigned(FOnProgress) then
    FonProgress(Sender,Stage,PercentDone,RedrawNow,R,Msg,Continue);
end;

Procedure TFPCustomImage.Assign(Source: TPersistent);

Var
  Src : TFPCustomImage;
  X,Y : Integer;

begin
  If Source is TFPCustomImage then
    begin
    Src:=TFPCustomImage(Source);
    // Copy extra info
    FExtra.Assign(Src.Fextra);
    // Copy palette if needed.
    SetSize(0,0); { avoid side-effects in descendant classes }
    UsePalette:=Src.UsePalette;
    If UsePalette then
      begin
      Palette.Count:=0;
      Palette.Merge(Src.Palette);
      end;
    // Copy image.
    SetSize(Src.Width,Src.height);
    If UsePalette then
      For x:=0 to Src.Width-1 do
        For y:=0 to src.Height-1 do
          pixels[X,Y]:=src.pixels[X,Y]
    else
      For x:=0 to Src.Width-1 do
        For y:=0 to src.Height-1 do
          self[X,Y]:=src[X,Y];
    end
  else
    Inherited Assign(Source);
end;

{ TFPMemoryImage }

constructor TFPMemoryImage.Create (AWidth,AHeight:integer);
begin
  Fdata := nil;
  inherited create (AWidth,AHeight);
  SetUsePalette(False);
end;

destructor TFPMemoryImage.Destroy;
begin
  // MG: missing if
  if FData<>nil then
    FreeMem (FData);
  inherited Destroy;
end;

function TFPMemoryImage.GetInternalColor(x,y:integer):TFPColor;
  begin
    if Assigned(FPalette)
    then
      Result:=inherited GetInternalColor(x,y)
    else
      Result:=PFPColorArray(FData)^[y*FWidth+x];
  end;

function TFPMemoryImage.GetInternalPixel (x,y:integer) : integer;
begin
  result := FData^[y*FWidth+x];
end;

procedure TFPMemoryImage.SetInternalColor (x,y:integer; const Value:TFPColor);
  begin
    if Assigned(FPalette)
    then
      inherited SetInternalColor(x,y,Value)
    else
      PFPColorArray(FData)^[y*FWidth+x]:=Value;
  end;

procedure TFPMemoryImage.SetInternalPixel (x,y:integer; Value:integer);
begin
  FData^[y*FWidth+x] := Value;
end;

function Lowest (a,b : integer) : integer;
begin
  if a <= b then
    result := a
  else
    result := b;
end;

procedure TFPMemoryImage.SetSize (AWidth, AHeight : integer);
var w, h, r, old : integer;
    NewData : PFPIntegerArray;
begin
  if (AWidth <> Width) or (AHeight <> Height) then
    begin
    old := Height * Width;
    r:=AWidth*AHeight;
    if Assigned(FPalette)
    then
      r:=SizeOf(integer)*r
    else
      r:=SizeOf(TFPColor)*r;
    if r = 0 then
      NewData := nil
    else
      begin
      GetMem (NewData, r);
      FillWord (Newdata^[0], r div sizeof(word), 0);
      end;
    // MG: missing "and (NewData<>nil)"
    if (old <> 0) and assigned(FData) and (NewData<>nil) then
      begin
      if r <> 0 then
        begin
        w := Lowest(Width, AWidth);
        h := Lowest(Height, AHeight);
        for r := 0 to h-1 do
          move (FData^[r*Width], NewData^[r*AWidth], w);
        end;
      end;
    if Assigned(FData) then FreeMem(FData);
    FData := NewData;
    inherited;
    end;
end;

procedure TFPMemoryImage.SetUsePalette(Value:boolean);
var
  OldColors:PFPColorArray;
  OldPixels:PFPIntegerArray;
  r,c:Integer;
begin
  if Value<>assigned(FPalette)
  then
    if Value
    then
      begin
        FPalette:=TFPPalette.Create(0);
        //FPalette.Add(colTransparent);
        if assigned(FData) then
          begin
          OldColors:=PFPColorArray(FData);
          GetMem(FData,FWidth*FHeight*SizeOf(Integer));
          for r:=0 to FHeight-1 do
            for c:=0 to FWidth-1 do
              Colors[c,r]:=OldColors^[r*FWidth+c];
          FreeMem(OldColors);
          end;
      end
    else
      begin
        if Assigned(FData) then
          begin
          OldPixels:=PFPIntegerArray(FData);
          GetMem(FData,FWidth*FHeight*SizeOf(TFPColor));
          for r:=0 to FHeight-1 do
            for c:=0 to FWidth-1 do
              Colors[c,r]:=FPalette.Color[OldPixels^[r*FWidth+c]];
          FreeMem(OldPixels);
          end;
        FPalette.Free;
        FPalette:=nil;
      end;
end;
