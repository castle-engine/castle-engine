unit TestFpJson;

interface

uses
  Classes, SysUtils, fpjson, fpjsonrtti, variants,
  CastleLog, CastleUtils;


procedure TestFpJson1;
procedure TestFpJson2;

implementation

procedure TestFpJson1;
var
  JSONTestString: String;
  Json, Json2, Json3: TJSONData;
  I, K: Integer;

  procedure LogJSONType(JsonData: TJSONData);
  begin

    case JsonData.JSONType of
      jtUnknown:
        WritelnLog('jtUnknown');
      jtNumber:
        WritelnLog(JsonData.AsUnicodeString + ' - ' + 'jtNumber');
      jtString:
        Writeln(JsonData.AsUnicodeString + ' - ' + 'jtString');
      jtBoolean:
        Writeln(JsonData.AsUnicodeString + ' - ' + 'jtBoolean');
      jtNull:
        WritelnLog('jtNull');
      jtArray:
        WritelnLog('jtArray');
      jtObject:
        WritelnLog('jtObject');
    end;
  end;

begin

  JSONTestString := '{"player":' + NL +
  '  {  "playerid": 1000,' + NL +
  '     "playername": "johny",' + NL +
  '     "password": "secret",' + NL +
  '     "classes": [ "warrior", "mage" ]' + NL +
  '  }' + NL +
  '}';

  Json := GetJSON(JSONTestString);
  LogJSONType(Json);

  for I := 0 to Json.Count -1 do
  begin
    Json2 := Json.Items[I];
    LogJSONType(Json2);

    for K := 0 to Json2.Count - 1 do
    begin
      Json3 := Json2.Items[K];
      LogJSONType(Json3);
    end;
  end;

  Writeln('Player name:' + Json.FindPath('player.playername').AsString);
end;

Procedure DumpJSONData(J : TJSonData; DOEOLN : Boolean = True);

Var
  I : Integer;

begin
  // JSONType property determines kind of value.
  Case J.jsontype of
    jtNull   : Write('Null');
    jtBoolean : If J.AsBoolean then
                  Write('True')
                else
                  Write('False');
    jtNumber : {JSONNumber has extra NumberType property
                which determines kind of value (int/float).}
               Case TJSONNumber(J).NumberType of
                 ntInteger : Write(J.AsInteger);
                 ntFloat   : Write(J.AsFloat:10:2);
               end;
    jtString : Write('"',J.AsString,'"');
    jtArray  : begin
               Write('[ ');
               For I:=0 to J.Count-1 do
                 begin
                 DumpJSONData(J.Items[I],False);
                 If I<J.Count-1 then
                   Write(', ');
                 end;
               Write(' ]');
               end;
    jtObject : begin
               Write('{ ');
               For I:=0 to J.Count-1 do
                 begin
                 Writeln('"',TJSONObject(J).Names[i],'" : ');
                 DumpJSONData(J.Items[I],False);
                 If I<J.Count-1 then
                   Write(',')
                 end;
               Write('}');
               end;
   end;
   If DOEOLN then
     Writeln;
end;


Procedure EndTest(Msg : String;J : TJSOnData);

begin
  Write(Msg, ' : ');
  DumpJSONData(J);
  FreeAndNil(J);
end;

Procedure DoTestCreate;


begin
  Writeln('Constructor tests');
  EndTest('Null value',TJSOnNull.Create);
  EndTest('Boolean true',TJSONBoolean.Create(True));
  EndTest('Boolean false',TJSONBoolean.Create(False));
  EndTest('Integer value',TJSONIntegerNumber.Create(100));
  EndTest('Float value',TJSONFloatNumber.Create(1.2e3));
  EndTest('String value',TJSONString.Create('Some weird JSON string'));
  EndTest('Empty Array value',TJSONArray.Create);
  EndTest('Array value from array of const',TJSONArray.Create([1,'a',2,'b']));
  EndTest('Empty Object value',TJSONObject.Create);
  // Name, Value, name, value
  EndTest('Object from array of const',TJSONObject.Create(['a',1,'b',True,'C',Nil]));

end;

Procedure DoTestAs;

Var
  J : TJsonData;

begin
  Writeln('AsNNN value accessing tests, number with value 123:');
  J:=TJSonIntegerNumber.Create(123);
  Writeln('IsNull    : ',J.IsNull);
  Writeln('AsInteger : ',J.AsInteger);
  Writeln('AsBoolean : ',J.AsBoolean);
  Writeln('AsString  : ',J.AsString);
  Writeln('AsFloat   : ',J.AsFloat:5:3);
  FreeAndNil(J);
  Writeln('Test IsNull');
  J:=TJSonNull.Create;
  Writeln('Test for null with IsNull');
  Writeln('IsNull : ',J.ISNull);
  Writeln('Test number of children :');
  Writeln('Count (0) : ',J.Count);
  FreeAndNil(J);
  J:=TJSONArray.Create(['a','b','c']);
  Writeln('Count (3): ',J.Count);
  FreeAndNil(J);
  J:=TJSONObject.Create(['a',1,'b',2]);
  Writeln('Count (2): ',J.Count);
  FreeAndNil(J);
end;

Procedure DoTestArray;

Var
  J : TJSOnArray;
  I : Integer;

begin
  Writeln('JSON array with elements 0,1,2,3');
  J:=TJSONArray.Create([0,1,2,3]);
  Write('Access through Elements[] (default) array property : ');
  For I:=0 to J.Count-1 do
    begin
    Write(J[I].AsString);
    If I<J.Count-1 then
      Write(', ');
    end;
  Writeln;
  Write('Access through Nulls[] array property : ');
  For I:=0 to J.Count-1 do
    begin
    Write(J.Nulls[I]);
    If I<J.Count-1 then
      Write(', ');
    end;
  Writeln;
  Write('Access through Booleans[] array property : ');
  For I:=0 to J.Count-1 do
    begin
    Write(J.Booleans[I]);
    If I<J.Count-1 then
      Write(', ');
    end;
  Writeln;
  Write('Access through Integers[] array property : ');
  For I:=0 to J.Count-1 do
    begin
    Write(J.Integers[I]);
    If I<J.Count-1 then
      Write(', ');
    end;
  Writeln;
  Write('Access through Floats[] array property : ');
  For I:=0 to J.Count-1 do
    begin
    Write(J.Floats[I]:5:2);
    If I<J.Count-1 then
      Write(', ');
    end;
  Writeln;
  Write('Access through Strings[] array property : ');
  For I:=0 to J.Count-1 do
    begin
    Write(J.Strings[I]);
    If I<J.Count-1 then
      Write(', ');
    end;
  Writeln;
  FreeAndNil(J);
  Writeln('Create with 3 empty TJSONObjects in array constructor');
  Write('Access through Objects[] array property : ');
  J:=TJSONArray.Create([TJSOnObject.Create,TJSOnObject.Create,TJSOnObject.Create]);
  For I:=0 to J.Count-1 do
    begin
    DumpJSONData(J.Objects[I],False);
    If I<J.Count-1 then
      Write(', ');
    end;
  Writeln;
  FreeAndNil(J);
  Writeln('Create with 3 empty TJSONArrays in array constructor');
  Write('Access through Arrays[] array property : ');
  J:=TJSONArray.Create([TJSOnArray.Create,TJSOnArray.Create,TJSOnArray.Create]);
  For I:=0 to J.Count-1 do
    begin
    DumpJSONData(J.Arrays[I],False);
    If I<J.Count-1 then
      Write(', ');
    end;
  Writeln;
  FreeAndNil(J);
  Writeln('Create empty array. Add elements with overloaded Add() method');
  J:=TJSONArray.Create;
  J.Add; // Null
  J.Add(True);
  J.Add(False);
  J.Add(123);
  J.Add(2.34);
  J.Add('A string');
  J.Add(TJSOnArray.Create);
  J.Add(TJSOnObject.Create);
  DumpJSONData(J);
  FreeAndNil(J);
end;

Procedure DoTestObject;

Var
  J : TJSONObject;
  I : Char;
  k : Integer;

begin
  Writeln('JSON object with elements a=0,b=1,c=2,d=3');
  J:=TJSONObject.Create(['a',0,'b',1,'c',2,'d',3]);
  Write('Get element names with Names[] array property : ');
  For K:=0 to J.Count-1 do
    begin
    Write(J.Names[k]);
    If K<J.Count-1 then
      Write(', ');
    end;
  Writeln;
  Write('Access through Elements[] (default) array property : ');
  For I:='a' to 'd' do
    begin
    Write(i,' : ',J[I].AsString);
    If I<'d' then
      Write(', ');
    end;
  Writeln;
  Write('Access through Nulls[] array property : ');
  For I:='a' to 'd' do
    begin
    Write(i,' : ',J.Nulls[I]);
    If I<'d' then
      Write(', ');
    end;
  Writeln;
  Write('Access through Booleans[] array property : ');
  For I:='a' to 'd' do
    begin
    Write(i,' : ',J.Booleans[I]);
    If I<'d' then
      Write(', ');
    end;
  Writeln;
  Write('Access through Integers[] array property : ');
  For I:='a' to 'd' do
    begin
    Write(i,' : ',J.Integers[I]);
    If I<'d' then
      Write(', ');
    end;
  Writeln;
  Write('Access through Floats[] array property : ');
  For I:='a' to 'd' do
    begin
    Write(i,' : ',J.Floats[I]:5:2);
    If I<'d' then
      Write(', ');
    end;
  Writeln;
  Write('Access through Strings[] array property : ');
  For I:='a' to 'd' do
    begin
    Write(i,' : ',J.Strings[I]);
    If I<'d' then
      Write(', ');
    end;
  Writeln;
  FreeAndNil(J);
  Writeln('Create with 3 empty TJSONObjects in array constructor');
  Write('Access through Objects[] array property : ');
  J:=TJSONObject.Create(['a',TJSOnObject.Create,'b',TJSOnObject.Create,'c',TJSOnObject.Create]);
  For I:='a' to 'c' do
    begin
    Write(i,' : ');
    DumpJSONData(J.Objects[i],False);
    If I<'c' then
      Write(', ');
    end;
  Writeln;
  FreeAndNil(J);
  Writeln('Create with 3 empty TJSONArrays in array constructor');
  Write('Access through Arrays[] array property : ');
  J:=TJSONObject.Create(['a',TJSONArray.Create,'b',TJSONArray.Create,'c',TJSONArray.Create]);
  For I:='a' to 'c' do
    begin
    Write(i,' : ');
    DumpJSONData(J.Arrays[I],False);
    If I<'c' then
      Write(', ');
    end;
  Writeln;
  FreeAndNil(J);
  Writeln('Create empty object. Add elements with overloaded Add() method');
  J:=TJSONObject.Create;
  J.Add('a'); // Null
  J.Add('b',True);
  J.Add('c',False);
  J.Add('d',123);
  J.Add('e',2.34);
  J.Add('f','A string');
  J.Add('g',TJSONArray.Create);
  J.Add('h',TJSOnObject.Create);
  DumpJSONData(J);
  FreeAndNil(J);
end;

{ simpledemo form fcl-json examples }
procedure TestFpJson2;
begin
  DoTestCreate;
  DoTestAs;
  DoTestArray;
  DoTestObject;
end;

end.
