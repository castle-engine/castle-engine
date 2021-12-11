unit TestFpJsonRTTI;

// tests from demortti.pp

interface

uses
  Classes, SysUtils, fpjson, fpjsonrtti, variants;

procedure TestRTTI;

Var
  JS : TJSONStreamer;


implementation

Type

  { TTestItem }

  TTestItem = Class(TCollectionItem)
  private
    FStrProp: String;
  Published
    Property StrProp : String Read FStrProp Write FStrProp;
  end;

  { TCollComponent }

  TCollComponent = Class(TComponent)
  private
    FCollProp: TCollection;
  Published
   Property CollProp : TCollection Read FCollProp Write FCollProp;
  end;

  { TCompComponent }

  TCompComponent = Class(TComponent)
  private
    FCompProp: TComponent;
  Published
   Property CompProp : TComponent Read FCompProp Write FCompProp;
  end;

  TDice = (one,two,three,four,five,six);
  TThrow = Set of TDice;

  { TEnumComponent }

  TEnumComponent = Class(TComponent)
  private
    FDice: TDice;
  Published
    Property Dice : TDice Read FDice Write FDice;
  end;

  { TSetComponent }

  TSetComponent = Class(TComponent)
  private
    FThrow: TThrow;
  Published
    Property Throw : TThrow Read FThrow Write FThrow;
  end;

  { TChildComponent }

  TChildComponent = Class(TComponent)
  Protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  end;

{ TChildComponent }

procedure TChildComponent.GetChildren(Proc: TGetChildProc; Root: TComponent);
  Var
    I : Integer;
  begin
    Writeln('Children',ComponentCount);
    For I:=0 to ComponentCount-1 do
      Proc(Components[i]);
  end;

Procedure DumpObject(const Header : String; var O : TJSONData);

begin
  Writeln(Header,' : ');
  Writeln(O.FormatJSON());
  writeln;
  FreeAndNil(O);
  JS.Options:=[];
end;

Procedure DemoObject;

Var
  C : TComponent;
  O : TJSONData;

begin
  C:=TComponent.Create(Nil);
  try
    C.Name:='DemoComponent';
    C.Tag:=23;
    O:=JS.ObjectToJSON(C);
    DumpObject('Complete component',O);
  finally
    FreeAndNil(C);
  end;
end;

Procedure DemoComponentObject;

Var
  C : TCompComponent;
  O : TJSONData;

begin
  C:=TCompComponent.Create(Nil);
  try
    C.Name:='DemoComponent';
    C.Tag:=23;
    C.CompProp:=TCompComponent.Create(C);
    C.CompProp.Name:='SubComponent';
    C.CompProp.Tag:=45;
    O:=JS.ObjectToJSON(C);
    DumpObject('Component-valued property',O);
    TCompComponent(C.CompProp).FComponentStyle:=[csSubComponent];
    O:=JS.ObjectToJSON(C);
    DumpObject('Component-valued property, csSubComponent in Componentstyle',O);
    TCompComponent(C.CompProp).FComponentStyle:=[];
    JS.options:=[jsoComponentsInline];
    O:=JS.ObjectToJSON(C);
    DumpObject('Component-valued property, options:=[jsoComponentsInline] ',O);
  finally
    FreeAndNil(C);
  end;
end;

Procedure DemoChildObject;

Var
  C : TChildComponent;
  O : TJSONData;

begin
  C:=TChildComponent.Create(Nil);
  try
    C.Name:='ParentComponent';
    C.Tag:=23;
    With TComponent.Create(C) do
      begin
      Name:='Child1';
      Tag:=1;
      end;
    With TComponent.Create(C) do
      begin
      Name:='Child2';
      Tag:=2;
      end;
    O:=JS.ObjectToJSON(C);
    DumpObject('Component with children, default options',O);
    JS.Options:=[jsoStreamChildren];
    O:=JS.ObjectToJSON(C);
    DumpObject('Component with children, Options:=[jsoStreamChildren]',O);
  finally
    FreeAndNil(C);
  end;
end;

Procedure DemoEnumObject;

Var
  C : TEnumComponent;
  O : TJSONData;

begin
  C:=TEnumComponent.Create(Nil);
  try
    C.Dice:=Three;
    O:=JS.ObjectToJSON(C);
    DumpObject('Enumerated-typed property, default settings',O);
    JS.Options:=[jsoEnumeratedAsInteger];
    O:=JS.ObjectToJSON(C);
    DumpObject('Enumerated-typed property, Options:=[jsoEnumeratedAsInteger];',O);
  finally
    FreeAndNil(C);
  end;
end;

Procedure DemoSetObject;

Var
  C : TSetComponent;
  O : TJSONData;

begin
  C:=TSetComponent.Create(Nil);
  try
    C.Throw:=[two,five];
    O:=JS.ObjectToJSON(C);
    DumpObject('set-typed property, default settings',O);
    JS.Options:=[jsoSetAsString];
    O:=JS.ObjectToJSON(C);
    DumpObject('Set-typed property, Options:=[jsoSetAsString];',O);
    JS.Options:=[jsoSetAsString,jsoSetBrackets];
    O:=JS.ObjectToJSON(C);
    DumpObject('Set-typed property, Options:=[jsoSetAsString,jsoSetBrackets];',O);
    JS.Options:=[jsoSetEnumeratedAsInteger];
    O:=JS.ObjectToJSON(C);
    DumpObject('Set-typed property, Options:=[jsoSetEnumeratedAsInteger];',O);
  finally
    FreeAndNil(C);
  end;
end;

Procedure DemoObjectAsString;

Var
  C : TComponent;

begin
  C:=TComponent.Create(Nil);
  try
    C.Name:='DemoComponent';
    C.Tag:=23;
    Writeln('Complete component, directly as string :');
    Writeln(JS.ObjectToJSONString(C));
    JS.Options:=[jsoUseFormatString];
    Writeln('Complete component, directly as string (Options:=[jsoUseFormatString]):');
    Writeln(JS.ObjectToJSONString(C));
    JS.Options:=[];
    Writeln('');
  finally
    FreeAndNil(C);
  end;
end;

Procedure DemoStrings;

Var
  S : TStrings;
  O : TJSONData;
  C : TComponent;

begin
  S:=TStringList.Create;
  try
    S.Add('One');
    S.Add('two');
    S.Add('Three');
    O:=JS.StreamTStrings(S);
    DumpObject('Default TStrings',O);
    O:=JS.StreamTStringsArray(S);
    DumpObject('TStrings as array',O);
    C:=TComponent.Create(Nil);
    try
      C.Name:='SubComponent';
      C.Tag:=12;
      S.Objects[0]:=C;
      O:=JS.StreamTStringsObject(S);
      DumpObject('TStrings as object',O);
      Writeln('TStrings Directly as JSON string, array');
      Writeln(JS.StringsToJSON(S,False));
      Writeln;
      Writeln('TStrings Directly as JSON string, object');
      Writeln(JS.StringsToJSON(S,True));
      Writeln;
      O:=JS.ObjectToJSON(S);
      DumpObject('Passing TStrings to ObjctToJSON',O);
      JS.Options:=[jsoTstringsAsArray];
      O:=JS.ObjectToJSON(S);
      DumpObject('Passing TStrings to ObjctToJSON (Options:=[jsoTstringsAsArray])',O);
      JS.Options:=[jsoTstringsAsObject];
      O:=JS.ObjectToJSON(S);
      DumpObject('Passing TStrings to ObjctToJSON (Options:=[jsoTstringsAsObject])',O);
    finally
      FreeAndNil(C);
    end;
  finally
    FreeAndNil(S);
  end;
end;

Procedure DemoCollection;

Var
  C : TCollection;
  CC : TCollComponent;
  O : TJSONData;

begin
  C:=TCollection.Create(TTestItem);
  try
    (C.Add as TTestItem).StrProp:='One';
    (C.Add as TTestItem).StrProp:='Two';
    (C.Add as TTestItem).StrProp:='Three';
    CC:=TCollComponent.Create(Nil);
    try
      CC.CollProp:=C;
      O:=JS.ObjectToJSON(CC);
      DumpObject('Collection property',O);
      O:=JS.StreamCollection(C);
      DumpObject('StreamCollection result',O);
      O:=JS.ObjectToJSON(C);
      DumpObject('Passing collection to ObjectToJSON (returns an object)',O);
      Writeln('Direct Collection to JSON String :');
      Writeln(JS.CollectionToJSON(C));
      Writeln;
    finally
      FreeAndNil(CC);
    end;
  finally
    FreeAndNil(C);
  end;
end;


Procedure DemoVariant;

Var
  V : Variant;
  O : TJSONData;
  I : integer;

begin
  V:=3;
  O:=JS.StreamVariant(V);
  DumpObject('Simple integer variant streaming',O);
  V:=EncodeDate(2010,12,24);
  O:=JS.StreamVariant(V);
  DumpObject('Date variant streaming',O);
  JS.Options:=[jsoDateTimeAsString];
  O:=JS.StreamVariant(V);
  DumpObject('Date variant streaming (Options:=[jsoDateTimeAsString];)',O);
  V:=VarArrayCreate([1,10],varInteger);
  For I:=1 to 10 do
    V[i]:=11-I;
  O:=JS.StreamVariant(V);
  DumpObject('Variant arrays also work',O);
  Writeln('Variant to JSON string :');
  Writeln(JS.VariantToJSON(V));
  Writeln('Variant to JSON string, with formatting :');
  JS.Options:=[jsoUseFormatString];
  Writeln(JS.VariantToJSON(V));
  JS.Options:=[];
end;

procedure TestRTTI;
begin
  JS:=TJSONStreamer.Create(Nil);
  try
    DemoObject;
    DemoObjectAsString;
    DemoComponentObject;
    DemoEnumObject;
    DemoSetObject;
    DemoStrings;
    DemoCollection;
    DemoVariant;
    DemoChildObject;
  Finally
    FreeAndNil(JS);
  end;
end;

end.
