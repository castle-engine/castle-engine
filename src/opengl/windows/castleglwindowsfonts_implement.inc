constructor TWindowsOutlineFont.Create(const AFaceName: string;
  AHeight: Integer; AWeight: DWord; AItalic: boolean; ACharSet: DWord;
  Depth: TGLfloat; OnlyLines: boolean; dc: HDc);
var WinFont: TWindowsFont;
    WinFontHandle: HFont;
    PreviousObject: HGdiObj;
begin
 WinFont := TWindowsFont.Create(AHeight);
 try
  WinFont.OutputPrecision := OUT_TT_ONLY_PRECIS {only TrueType fonts};
  WinFont.FaceName := AFaceName;
  WinFont.Height := AHeight;
  WinFont.Weight := AWeight;
  WinFont.Italic := AItalic;
  WinFont.CharSet := ACharSet;

  WinFontHandle := WinFont.GetHandle;
  try
   PreviousObject := SelectObject(dc, WinFontHandle);
   try
    Font2OutlineFont(dc, CreatedOutline);
    inherited Create(@CreatedOutline, Depth, OnlyLines);
   finally SelectObject(dc, PreviousObject) end;
  finally DeleteObject(WinFontHandle) end;
 finally WinFont.Free end;
end;

destructor TWindowsOutlineFont.Destroy;
begin
 inherited;
 { yes, FreeMem AFTER inherited because TGLOutlineFont
   requires that CreatedOutline is valid for it's lifetime }
 FreeMemNilingAllChars(CreatedOutline);
end;