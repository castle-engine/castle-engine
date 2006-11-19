{
  Copyright 2006 Michalis Kamburelis.

  This file is part of "castle".

  "castle" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "castle" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "castle"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ }
unit KambiXMLCfg;

interface

uses KambiUtils, XMLCfg;

type
  { This is descendant of TXMLConfig that adds
    GetFloat, SetFloat, SetDeleteFloat for the Float type.

    Note: at the beginning I named them GetValue, SetValue etc.
    and made them overloaded. But this is *very* bad idea.
    Why ? Because integers are casted to floats without any problems,
    and this may cause choosing wrong overloaded version.
    Consider that default value for some float parameter is integer
    (e.g. because it was declared as an integer, I forgot to
    write "0.0" instead of "0" etc.). Then
      MyValue := GetValue(Name, IntegerValue);
    will choose GetValue that interprets given value as an integer.
    Although MyValue is variable of type float, you can assign integer
    to a float without any problem, so again no compile-time error.
    But this is obviously wrong --- if float value was recorded
    in the file, it will be read incorrectly. }
  TKamXMLConfig = class(TXMLConfig)
  public
    function GetFloat(const APath: string;
      const ADefaultValue: Float): Float;

    procedure SetFloat(const APath: string;
      const AValue: Float);

    procedure SetDeleteFloat(const APath: string;
      const AValue, ADefaultValue: Float);
  end;

implementation

uses SysUtils;

{ TKamXMLConfig -------------------------------------------------------------- }

function TKamXMLConfig.GetFloat(const APath: string;
  const ADefaultValue: Float): Float;
var
  ResultString: string;
begin
  ResultString := GetValue(APath, FloatToStr(ADefaultValue));
  Result := StrToFloatDef(ResultString, ADefaultValue);
end;

procedure TKamXMLConfig.SetFloat(const APath: string;
  const AValue: Float);
begin
  SetValue(APath, FloatToStr(AValue));
end;

procedure TKamXMLConfig.SetDeleteFloat(const APath: string;
  const AValue, ADefaultValue: Float);
begin
  SetDeleteValue(APath, FloatToStr(AValue), FloatToStr(ADefaultValue));
end;

end.