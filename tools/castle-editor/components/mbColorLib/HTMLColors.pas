unit HTMLColors;

interface

uses
  SysUtils, LCLIntf, Graphics, Variants;

const
  SPECIAL_COUNT = 140;
  WEBSAFE_COUNT = 216;
  SYSTEM_COUNT = 28;
  BASIC_COUNT = 16;
  SPECIAL_HEX: array [0..139] of string = (
    '000000', 'FAEBD7', '00FFFF', '7FFFD4', 'F0FFFF', 'F5F5DC', 'FFE4C4',
    'F0F8FF', 'FFEBCD', '0000FF', '8A2BE2', 'A52A2A', 'DEB887', '5F9EA0',
    '7FFF00', 'D2691E', 'FF7F50', '6495ED', 'FFF8DC', 'DC143C', '00FFFF',
    '00008B', '008B8B', 'B8860B', 'A9A9A9', '006400', 'BDB76B', '8B008B',
    '556B2F', 'FF8C00', '9932CC', '8B0000', 'E9967A', '8FBC8B', '483D8B',
    '2F4F4F', '00CED1', '9400D3', 'FF1493', '00BFFF', '696969', '1E90FF',
    'B22222', 'FFFAF0', '228B22', 'FF00FF', 'DCDCDC', 'F8F8FF', 'FFD700',
    'DAA520', '808080', '008000', 'ADFF2F', 'F0FFF0', 'FF69B4', 'CD5C5C',
    '4B0082', 'FFFFF0', 'F0E68C', 'E6E6FA', 'FFF0F5', '7CFC00', 'FFFACD',
    'ADD8E6', 'F08080', 'E0FFFF', 'FAFAD2', '90EE90', 'D3D3D3', 'FFB6C1',
    'FFA07A', '20B2AA', '87CEFA', '778899', 'B0C4DE', 'FFFFE0', '00FF00',
    '32CD32', 'FAF0E6', 'FF00FF', '800000', '66CDAA', '0000CD', 'BA55D3',
    '9370DB', '3CB371', '7B68EE', '00FA9A', '48D1CC', 'C71585', '191970',
    'F5FFFA', 'FFE4E1', 'FFE4B5', 'FFDEAD', '000080', 'FDF5E6', '808000',
    '6B8E23', 'FFA500', 'FF4500', 'DA70D6', 'EEE8AA', '98FB98', 'AFEEEE',
    'DB7093', 'FFEFD5', 'FFDAB9', 'CD853F', 'FFC0CB', 'DDA0DD', 'B0E0E6',
    '800080', 'FF0000', 'BC8F8F', '4169E1', '8B4513', 'FA8072', 'F4A460',
    '2E8B57', 'FFF5EE', 'A0522D', 'C0C0C0', '87CEEB', '6A5ACD', '708090',
    'FFFAFA', '00FF7F', '4682B4', 'D2B48C', '008080', 'D8BFD8', 'FF6347',
    '40E0D0', 'EE82EE', 'F5DEB3', 'FFFFFF', 'F5F5F5', 'FFFF00', '9ACD32'
  );
  SPECIAL_NAMES: array [0..139] of string = (
    'black', 'antiquewhite', 'aqua', 'aquamarine', 'azure', 'beige',
    'bisque', 'aliceblue', 'blanchedalmond', 'blue', 'blueviolet', 'brown',
    'burlywood', 'cadetblue', 'chartreuse', 'chocolate', 'coral',
    'cornflower', 'cornsilk', 'crimson', 'cyan', 'darkblue', 'darkcyan',
    'darkgoldenrod', 'darkgray', 'darkgreen', 'darkkhaki', 'darkmagenta',
    'darkolivegreen', 'darkorange', 'darkorchid', 'darkred', 'darksalmon',
    'darkseagreen', 'darkslateblue', 'darkslategray', 'darkturquoise',
    'darkviolet', 'deeppink', 'deepskyblue', 'dimgray', 'dodgerblue',
    'firebrick', 'floralwhite', 'forestgreen', 'fuchsia', 'gainsboro',
    'ghostwhite', 'gold', 'goldenrod', 'gray', 'green', 'greenyellow',
    'honeydew', 'hotpink', 'indianred', 'indigo', 'ivory', 'khaki', 'lavender',
    'lavenderblush', 'lawngreen', 'lemonchiffon', 'lightblue', 'lightcoral',
    'lightcyan', 'lightgoldenrodyellow', 'lightgreen', 'lightgray', 'lightpink',
    'lightsalmon', 'lightseagreen', 'lightskyblue', 'lightslategray',
    'lightsteelblue', 'lightyellow', 'lime', 'limegreen', 'linen', 'magenta',
    'maroon', 'mediumaquamarine', 'mediumblue', 'mediumorchid', 'mediumpurple',
    'mediumseagreen', 'mediumslateblue', 'mediumspringgreen', 'mediumturquoise',
    'mediumvioletred', 'midnightblue', 'mintcream', 'mistyrose', 'moccasin',
    'navajowhite', 'navy', 'oldlace', 'olive', 'olivedrab', 'orange', 'orangered',
    'orchid', 'palegoldenrod', 'palegreen', 'paleturquoise', 'palevioletred',
    'papayawhip', 'peachpuff', 'peru', 'pink', 'plum', 'powderblue', 'purple',
    'red', 'rosybrown', 'royalblue', 'saddlebrown', 'salmon', 'sandybrown',
    'seagreen', 'seashell', 'sienna', 'silver', 'skyblue', 'slateblue',
    'slategray', 'snow', 'springgreen', 'steelblue', 'tan', 'teal', 'thistle',
    'tomato', 'turquoise', 'violet', 'wheat', 'white', 'whitesmoke', 'yellow',
    'yellowgreen'
  );
  WEBSAFE_HEX: array [0..215] of string = (
    '000000' ,'000033' ,'000066' ,'000099' ,'0000cc' ,'0000ff',
    '003300' ,'003333' ,'003366' ,'003399' ,'0033cc' ,'0033ff',
    '006600' ,'006633' ,'006666' ,'006699' ,'0066cc' ,'0066ff',
    '009900' ,'009933' ,'009966' ,'009999' ,'0099cc' ,'0099ff',
    '00cc00' ,'00cc33' ,'00cc66' ,'00cc99' ,'00cccc' ,'00ccff',
    '00ff00' ,'00ff33' ,'00ff66' ,'00ff99' ,'00ffcc' ,'00ffff',
    '330000' ,'330033' ,'330066' ,'330099' ,'3300cc' ,'3300ff',
    '333300' ,'333333' ,'333366' ,'333399' ,'3333cc' ,'3333ff',
    '336600' ,'336633' ,'336666' ,'336699' ,'3366cc' ,'3366ff',
    '339900' ,'339933' ,'339966' ,'339999' ,'3399cc' ,'3399ff',
    '33cc00' ,'33cc33' ,'33cc66' ,'33cc99' ,'33cccc' ,'33ccff',
    '33ff00' ,'33ff33' ,'33ff66' ,'33ff99' ,'33ffcc' ,'33ffff',
    '660000' ,'660033' ,'660066' ,'660099' ,'6600cc' ,'6600ff',
    '663300' ,'663333' ,'663366' ,'663399' ,'6633cc' ,'6633ff',
    '666600' ,'666633' ,'666666' ,'666699' ,'6666cc' ,'6666ff',
    '669900' ,'669933' ,'669966' ,'669999' ,'6699cc' ,'6699ff',
    '66cc00' ,'66cc33' ,'66cc66' ,'66cc99' ,'66cccc' ,'66ccff',
    '66ff00' ,'66ff33' ,'66ff66' ,'66ff99' ,'66ffcc' ,'66ffff',
    '990000' ,'990033' ,'990066' ,'990099' ,'9900cc' ,'9900ff',
    '993300' ,'993333' ,'993366' ,'993399' ,'9933cc' ,'9933ff',
    '996600' ,'996633' ,'996666' ,'996699' ,'9966cc' ,'9966ff',
    '999900' ,'999933' ,'999966' ,'999999' ,'9999cc' ,'9999ff',
    '99cc00' ,'99cc33' ,'99cc66' ,'99cc99' ,'99cccc' ,'99ccff',
    '99ff00' ,'99ff33' ,'99ff66' ,'99ff99' ,'99ffcc' ,'99ffff',
    'cc0000' ,'cc0033' ,'cc0066' ,'cc0099' ,'cc00cc' ,'cc00ff',
    'cc3300' ,'cc3333' ,'cc3366' ,'cc3399' ,'cc33cc' ,'cc33ff',
    'cc6600' ,'cc6633' ,'cc6666' ,'cc6699' ,'cc66cc' ,'cc66ff',
    'cc9900' ,'cc9933' ,'cc9966' ,'cc9999' ,'cc99cc' ,'cc99ff',
    'cccc00' ,'cccc33' ,'cccc66' ,'cccc99' ,'cccccc' ,'ccccff',
    'ccff00' ,'ccff33' ,'CCFF66' ,'ccff99' ,'ccffcc' ,'ccffff',
    'ff0000' ,'ff0033' ,'ff0066' ,'ff0099' ,'ff00cc' ,'ff00ff',
    'ff3300' ,'ff3333' ,'ff3366' ,'ff3399' ,'ff33cc' ,'ff33ff',
    'ff6600' ,'ff6633' ,'ff6666' ,'ff6699' ,'ff66cc' ,'ff66ff',
    'ff9900' ,'ff9933' ,'ff9966' ,'ff9999' ,'ff99cc' ,'ff99ff',
    'ffcc00' ,'ffcc33' ,'ffcc66' ,'ffcc99' ,'ffcccc' ,'ffccff',
    'ffff00' ,'ffff33' ,'ffff66' ,'ffff99' ,'ffffcc' ,'ffffff'
  );
 SYSTEM_VALUES: array [0..27] of TColor = (
   clActiveBorder, clActiveCaption, clAppWorkspace, clBackground,
   clBtnFace, clBtnHighlight, clBtnShadow, clBtnText, clCaptionText,
   clGrayText, clHighlight, clHighlightText, clInactiveBorder,
   clInactiveCaption, clInactiveCaptionText, clInfoBk, clInfoText,
   clMenu, clMenuText, clScrollbar, cl3dDkShadow, cl3dLight,
   clBtnHighlight, clActiveBorder, clBtnShadow, clWindow,
   clWindowFrame, clWindowText
 );
 SYSTEM_NAMES: array [0..27] of string = (
   'activeborder', 'activecaption', 'appworkspace', 'background',
   'buttonface', 'buttonhighlight', 'buttonshadow', 'buttontext',
   'captiontext', 'graytext', 'highlight', 'highlighttext',
   'inactiveborder', 'inactivecaption', 'inactivecaptiontext',
   'infobackground', 'infotext', 'menu', 'menutext', 'scrollbar',
   'threeddarkshadow', 'threedface', 'threedhighlight',
   'threedlightshadow', 'threedshadow', 'window', 'windowframe',
   'windowtext'
 );
 BASIC_VALUES: array [0..15] of TColor = (
   clBlack, clAqua, clBlue, clFuchsia, clGray, clGreen, clLime,
   clMaroon, clNavy, clOlive, clPurple, clRed, clSilver, clTeal,
   clWhite, clYellow
 );
 BASIC_NAMES: array [0..15] of string = (
   'black', 'aqua', 'blue', 'fuchsia', 'gray', 'green', 'lime',
   'maroon', 'navy', 'olive', 'purple', 'red', 'silver', 'teal',
   'white', 'yellow'
 );

procedure MakeIntoHex(var s: string);
function IsMember(a: array of string; n: integer; s: string): boolean;
function IsSpecialColor(s: string): boolean;
function FormatHexColor(S: string): string;
function ColorToHex(Color: TColor): string;
function HexToTColor(s: OleVariant): TColor;
function GetHexFromName(s: string): string;
function GetValueFromName(s: string): TColor;
function IsWebSafe(s: string): boolean; overload;
function IsWebSafe(c: TColor): boolean; overload;
function GetWebSafe(C: TColor): TColor;

implementation

var
  WS: array [0..255] of byte;

//checks membership of a string array
function IsMember(a: array of string; n: integer; s: string): boolean;
var
  i: integer;
begin
  Result := false;
  for i := 0 to n - 1 do
    if SameText(s, a[i]) then
      Result := true;
end;

//checks if the color's name was used instead of hex
function IsSpecialColor(s: string): boolean;
begin
  Result := IsMember(BASIC_NAMES, BASIC_COUNT, s) or IsMember(SPECIAL_NAMES, SPECIAL_COUNT, s) or IsMember(SYSTEM_NAMES, SYSTEM_COUNT, s);
end;

//is hex was used then remove the wrong characters
procedure MakeIntoHex(var s: string);
var
  i: integer;
begin
  if s <> '' then
    for i := 1 to Length(s) do
      if not (s[i] in ['0'..'9', 'A'..'F', 'a'..'f']) then
        s[i] := '0';
end;

//formats entered text into a true hex value
function FormatHexColor(S: string): string;
var
  c: string;
  i: integer;
begin
  c := '';
  if not IsSpecialColor(s) then
  begin
    if (s <> '') and (s[1] = '#') then
      Delete(s, 1, 1);

    if s <> '' then
    begin
      MakeIntoHex(c);
      if Length(c) = 6 then
        Result := c
      else
      begin
        if Length(c) > 6 then
          c := Copy(c, 1, 6);
        if Length(c) < 6 then
          for i := 0 to 6 - Length(c) - 1 do
            c := '0' + c;
        Result := c;
      end;
    end
    else
      Result := '000000';
  end
  else
    Result := s;
end;

//gets a hex value from a color name from special colors
function GetHexFromName(s: string): string;
var
  i, k: integer;
begin
  k := 0;
  for i := 0 to SPECIAL_COUNT - 1 do
    if SameText(s, SPECIAL_NAMES[i]) then
    begin
      k := i;
      Break;
    end;
  Result := SPECIAL_HEX[k];
end;

// gets a TColor value from a color name from basic or system colors
function GetValueFromName(s: string): TColor;
var
  i, k: integer;
begin
  k := 0;
  s := LowerCase(s);
  if IsMember(BASIC_NAMES, BASIC_COUNT, s) then
  begin
    for i := 0 to BASIC_COUNT - 1 do
      if SameText(s, BASIC_NAMES[i]) then
      begin
        k := i;
        Break;
      end;
    Result := BASIC_VALUES[k];
  end
  else if IsMember(SYSTEM_NAMES, SYSTEM_COUNT, s) then
  begin
    for i := 0 to SYSTEM_COUNT - 1 do
      if SameText(s, SYSTEM_NAMES[i]) then
      begin
        k := i;
        Break;
      end;
    Result := SYSTEM_VALUES[k];
  end
  else
    Result := clNone;
end;

//converts a TColor value to a hex value
function ColorToHex(Color: TColor): string;
begin
  Result := IntToHex(GetRValue(Color), 2) + IntToHex(GetGValue(Color), 2) + IntToHex(GetBValue(Color), 2)
end;

//converts a hex value to a TColor
function HexToTColor(s: OleVariant): TColor;
begin
  if s <> null then
  begin
    if not IsSpecialColor(s) then
    begin
      s := FormatHexColor(s);
      if s <> '' then
        Result := RGB(StrToInt('$'+Copy(S, 1, 2)), StrToInt('$'+Copy(S, 3, 2)), StrToInt('$'+Copy(S, 5, 2)))
      else
        Result := clNone;
    end
    else if IsMember(SPECIAL_NAMES, SPECIAL_COUNT, s) then
    begin
      s := GetHexFromName(s);
      Result := RGB(StrToInt('$'+Copy(S, 1, 2)), StrToInt('$'+Copy(S, 3, 2)), StrToInt('$'+Copy(S, 5, 2)));
    end
    else
      Result := GetValueFromName(s);
  end
  else
    Result := clNone;
end;

//checks if a hex value belongs to the websafe palette
function IsWebSafe(s: string): boolean;
begin
  s := FormatHexColor(s);
  Result := IsMember(WEBSAFE_HEX, WEBSAFE_COUNT, s);
end;

//checks if a color belongs to the websafe palette
function IsWebSafe(c: TColor): boolean;
var
  s: string;
begin
  s  := ColorToHex(c);
  Result := IsMember(WEBSAFE_HEX, WEBSAFE_COUNT, s);
end;

//initializes the websafe comparison array
procedure InitializeWS;
var
  i: integer;
begin
  for i := 0 to 255 do
    WS[I] := ((i + $19) div $33) * $33;
end;

//returns the closest web safe color to the one given
function GetWebSafe(C: TColor): TColor;
begin
  Result := RGB(WS[GetRValue(C)], WS[GetGValue(C)], WS[GetBValue(C)]);
end;


initialization
  InitializeWS;

end.
