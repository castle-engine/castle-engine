{
  Copyright 2001-2006 Michalis Kamburelis.

  This file is part of "Kambi's base Pascal units".

  "Kambi's base Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's base Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's base Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ @abstract(Typy i niezbedne funkcje do operowania na
  wyrazeniach matematycznych - obiektach klasy TMathExpr.)

  Prosty przyklad : deklaracja
  @longcode(#
   Expr := TMathFunction.Create(fkAdd, [
       TMathFunction.Create(fkSin, [TMathVar.Create('x')]),
       TMathConst.Create(10),
       TMathConst.Create(1)
     ]);
  #)
  sprawia ze expr reprezentuje wyrazenie sin(x) + 10 + 1.

  Mozna nastepnie wywolac expr.value(varfunc) jako parametr
  podajac funkcje ktora zamienia nazwy zmiennych na ich wartosci
  (jest oczywiscie gwarantowane ze ta funkcja zostanie zapytana
  tylko o zmienne obecne w danym wyrazeniu, w tym przypadku -
  - wiemy ze na pewno tylko o 'x'). Jako wynik otrzymamy
  wartosc tego wyrazenia.

  Wazna uwaga - obiekt TMathFunction zapamietuje sobie wyrazenia
  argumentow ktore same sa obiektami TMathExpr i zwalnia
  je gdy sam jest zwalniany. Tym samym mozemy na koncu zrobic
  expr.Free i wszystkie obiekty ktore sa jego czescia zostana
  zwolnione automatyczne.

  Jasne zastosowanie klasy TMathExpr jest do budowy parserow
  ktore musza zbudowac sobie wyrazenie matematyczne w czasie
  runtime. Patrz unit @link(MathExprParser).
}

unit MathExpr;

interface

uses SysUtils, Math, KambiUtils, KambiClassUtils;

{$define read_interface}

type
  TVariableValueFunc = function(const variable_name: string): Float;

  TMathExpr = class
  public
    { Zwroci wartosc wyrazenia.

      Funkcja varfunc zostanie zapytana o wartosci zmiennych w tym
      wyrazeniu (jesli jestes absolutnie pewien ze w tym wyrazeniu
      nie ma zadnej zmiennej, mozesz przekazac nil; ale UWAZAJ -
      - jesli sie pomylisz i w wyrazeniu bedzie jakas zmienna,
      spowodujesz w ten sposob AccessViolation (ktore nie jest
      wyjatkiem w 100% wylapywalnym i zawsze moze zrobic cos
      nieoczekiwanego).

      Jesli chcesz, mozesz w varfunc rzucic sobie wyjatkiem
      (najczesciej aby zglosic ze dana nazwa zmiennej nie jest
      zdefiniowana), jest gwarantowane ze ten wyjatek zostanie
      zupelnie poprawnie przepchniety w gore na zewntarz wywolania
      value().

      Note: Value is guaranteed to raise an exception if some
      calculation fails, e.g. if expression will be 'ln(-3)'.
      Stating it directly, Value may even call Math.ClearExceptions(true)
      if it is needed to force generating proper exceptions.

      @noAutoLinkHere }
    function Value(varfunc: TVariableValueFunc): Float; virtual; abstract;

    { TryValue podstawia pod return_value wartosc value(varfunc)
      CHYBA ze wystapil wyjatek w czasie obliczania value (bo np.
      wyrazenie jest nieprawidlowe dla takiego podstawienia zmiennej).
      Wtedy nie zmienia return_value i zwraca false. }
    function TryValue(varfunc: TVariableValueFunc; var return_value: Float): boolean;

    { output expression }
    function ToString: string; virtual; abstract;
  end;

  TEvalMathExprFunc = function(a, b: Float): Float;

  TObjectsListItem_1 = TMathExpr;
  {$I ObjectsList_1.inc}
  TMathExprList = class(TObjectsList_1)
    { Evaluate evaluates expressions [a1, a2 ... an] as
      EvalTwoExpr(an, ... evalTwoExpr(a3, evalTwoExpr(a1, a2)) ..,)
      It evaluetes from left to right using operation EvalTwoFunc.
      List MUST have at least one item.  }
    function Evaluate(EvalTwoExpr: TEvalMathExprFunc;
      VarFunc: TVariableValueFunc): Float;
  end;

  TMathConst = class(TMathExpr)
  private
    FValue: Float;
  public
    constructor Create(avalue: Float);
    property ConstValue: Float read fvalue;
    function Value(varfunc: TVariableValueFunc): Float; override;
    function ToString: string; override;
  end;

  TMathVar = class(TMathExpr)
  private
    fname: string;
  public
    constructor Create(const aname: string);
    property Name: string read fname;
    function Value(varfunc: TVariableValueFunc): Float; override;
    function ToString: string; override;
  end;

  { When you are extending TFunctionKind type, remember that you
    will also have to extend FunctionKinds array and implement this
    function type in TMathFunction.Value (this is important since failure
    to do the second thing will NOT be reported as compile-time error). }
  TFunctionKind =
  ( fkAdd, fkSubtract, fkMultiply, fkDivide, fkNegate, fkModulo,
    fkSin, fkCos, fkTan, fkCotan,
    fkArcSin, fkArcCos, fkArcTan, fkArcCotan,
    fkSinH, fkCosH, fkTanH, fkCotanH,
    fkLog2, fkLn, fkLog, fkPower2, fkExp, fkPower,
    fkSqr, fkSqrt, fkSgn, fkAbs, fkCeil, fkFloor,
    fkGreater, fkLesser, fkGreaterEq, fkLesserEq, fkEqual, fkNotEqual,
    fkOr, fkAnd, fkNot
    {rfRad});
const
  { This is an array that specifies some globally-useful properties of every
    function (defined as fkXxx constant inside TFunctionKind type).

    It also specifies some things about how such function can be specified
    as a text -- this is useful for both converting TMathExpr to a string
    (using TMathExpr.ToString method) and converting a string
    to TMathExpr (using ParseMathExpr function from MathExprParser).

    Note that not every property needed by MathExprParser is encoded
    in this array. Stating it shortly, you cannot fully control behaviour
    of MathExprParser by only modifying FunctionKinds array. For some
    more advanced control over MathExprParser (e.g. adding new operators,
    changing priority of operators) you will just have to modify
    MathExprParser unit. I may improve this with time.

    For now the only functions that can be fully controlled from
    FunctionKinds are functions with InfixOperatorName = '' and
    only FunctionName <> ''. When you add or modify such function,
    you don't have to do anything inside MathExprParser, everything
    will just work. }
  FunctionKinds : array[TFunctionKind]of
  record
    { 0 oznacza ze liczba argumentow moze byc dowolna,
      -n oznacza ze musi byc >= n.
      So e.g. ArgsCount = -1 means "any non-zero number of arguments".  }
    ArgsCount: integer;

    { Long name for user, possibly with spaces, parenthesis and other
      funny chars. }
    Name: string;

    { FunctionName: name of this function for use in expressions
      like "function_name(arg_1, arg_2 ... , arg_n)".
      Empty string ('') if no such name for this function. }
    FunctionName: string;

    { InfixOperatorName: name of this function for use in expressions
      like "arg_1 InfixOperatorName arg_2 ... arg_n".
      Empty string ('') if no such name for this function.
      But note that at least one of FunctionName and InfixOperatorName
      must not be empty !

      The only exception is the fkNegate function, that is neither
      infix operator nor a usual function that must be specified
      as "function_name(arguments)". fkNegate is an exception,
      and I there will be a need, I shall fix this (probably
      by introducing some third field, like PrefixOperatorName ?)

      Note 2 things:

      @orderedList(
        @item(
          Function that can be used as infix operator (i.e. has
          InfixOperatorName <> '') is not necessary binary operator,
          i.e. InfixOperatorName <> ''  does not determine the value of
          ArgsCount. This way I was able to define infix operators
          +, -, * etc. that take any number of arguments and operators
          like ^ and > that always take 2 arguments.)

        @item(
          Function may have both FunctionName <> '' and InfixOperatorName <> ''.
          E.g. fkPower can be used as "Power(3, 1.5)" or "3 ^ 1.5".)
      ) }
    InfixOperatorName: string;
  end =
  ( (ArgsCount:-1; Name:'add (+)';      FunctionName:''; InfixOperatorName:'+'),
    (ArgsCount:-1; Name:'subtract (-)'; FunctionName:''; InfixOperatorName:'-'),
    (ArgsCount:-1; Name:'multiply (*)'; FunctionName:''; InfixOperatorName:'*'),
    (ArgsCount:-1; Name:'division (/)'; FunctionName:''; InfixOperatorName:'/'),

    (ArgsCount:1; Name:'negation (unary -)'; FunctionName:''; InfixOperatorName:''),

    (ArgsCount:2; Name:'modulo (%)'; FunctionName:''; InfixOperatorName:'%'),

    (ArgsCount:1; Name:'sinus';     FunctionName:'sin';   InfixOperatorName:''),
    (ArgsCount:1; Name:'cosinus';   FunctionName:'cos';   InfixOperatorName:''),
    (ArgsCount:1; Name:'tangens';   FunctionName:'tan';   InfixOperatorName:''),
    (ArgsCount:1; Name:'cotangens'; FunctionName:'cotan'; InfixOperatorName:''),

    (ArgsCount:1; Name:'arcSinus';     FunctionName:'arcSin';   InfixOperatorName:''),
    (ArgsCount:1; Name:'arcCosinus';   FunctionName:'arcCos';   InfixOperatorName:''),
    (ArgsCount:1; Name:'arcTangens';   FunctionName:'arcTan';   InfixOperatorName:''),
    (ArgsCount:1; Name:'arcCotangens'; FunctionName:'arcCotan'; InfixOperatorName:''),

    (ArgsCount:1; Name:'sinh';   FunctionName:'sinh';   InfixOperatorName:''),
    (ArgsCount:1; Name:'cosh';   FunctionName:'cosh';   InfixOperatorName:''),
    (ArgsCount:1; Name:'tanh';   FunctionName:'tanh';   InfixOperatorName:''),
    (ArgsCount:1; Name:'cotanh'; FunctionName:'cotanh'; InfixOperatorName:''),

    (ArgsCount:1; Name:'logarithm (base 2)'; FunctionName:'log2';   InfixOperatorName:''),
    (ArgsCount:1; Name:'logarithm (base e)'; FunctionName:'ln';     InfixOperatorName:''),
    (ArgsCount:2; Name:'logarithm';          FunctionName:'log';    InfixOperatorName:''),
    (ArgsCount:1; Name:'power (base 2)';     FunctionName:'power2'; InfixOperatorName:''),
    (ArgsCount:1; Name:'power (base enat)';  FunctionName:'exp';    InfixOperatorName:''),
    (ArgsCount:2; Name:'power';              FunctionName:'power';  InfixOperatorName:'^'),

    (ArgsCount:1; Name:'sqr (square)';         FunctionName:'sqr';   InfixOperatorName:''),
    (ArgsCount:1; Name:'sqrt (square root)';   FunctionName:'sqrt';  InfixOperatorName:''),
    (ArgsCount:1; Name:'signum';               FunctionName:'sgn';   InfixOperatorName:''),
    (ArgsCount:1; Name:'abs (absolute value)'; FunctionName:'abs';   InfixOperatorName:''),
    (ArgsCount:1; Name:'ceil';                 FunctionName:'ceil';  InfixOperatorName:''),
    (ArgsCount:1; Name:'floor';                FunctionName:'floor'; InfixOperatorName:''),

    (ArgsCount:2; Name:'greater (>)';    FunctionName:'greater'  ; InfixOperatorName:'>'),
    (ArgsCount:2; Name:'lesser (<)';     FunctionName:'lesser'   ; InfixOperatorName:'<'),
    (ArgsCount:2; Name:'greaterEq (>=)'; FunctionName:'greaterEq'; InfixOperatorName:'>='),
    (ArgsCount:2; Name:'lesserEq (<=)';  FunctionName:'lesserEq' ; InfixOperatorName:'<='),
    (ArgsCount:2; Name:'equal (=)';      FunctionName:'equal'    ; InfixOperatorName:'='),
    (ArgsCount:2; Name:'not equal (<>)'; FunctionName:'notEqual' ; InfixOperatorName:'<>'),

    (ArgsCount:2; Name:'or (alternative)';       FunctionName:'or'; InfixOperatorName:''),
    (ArgsCount:2; Name:'and (conjunction)';      FunctionName:'and'; InfixOperatorName:''),
    (ArgsCount:1; Name:'not (logical negation)'; FunctionName:'not'; InfixOperatorName:'')
  );

type
  TMathFunction = class(TMathExpr)
  { TMathFunction frees his arguments = objects }
  private
    ffunckind: TFunctionKind;
    FArgs: TMathExprList;
    procedure CreateFinish(afuncKind: TFunctionKind);
  public
    { AArgs contents are COPIED from AArgs, i.e. AArgs object is not
      referenced by this object. But items on AArags are not copied
      recursively, we copy references from AArags items. }
    constructor Create(AFuncKind: TFunctionKind; AArgs: TMathExprList); overload;
    constructor Create(AFuncKind: TFunctionKind; const AArgs: array of TMathExpr); overload;
    destructor Destroy; override;

    property FuncKind: TFunctionKind read FFuncKind;
    { contents of Args are read-only ! }
    property Args: TMathExprList read FArgs;
    function ArgsCount: integer;

    function Value(varfunc: TVariableValueFunc): Float; override;
    function ToString: string; override;
  end;

{ exceptions ------------------------------------------------------------ }
type
  EWrongMathExpr = class(Exception);
  EWrongMathFunction = class(EWrongMathExpr);
  EUndefinedVariable = class(EWrongMathExpr)
    constructor Create(const VariableName: string);
  end;

{ global funcs ---------------------------------------------------------- }

function AddFloat(a, b: Float): Float;
function SubtractFloat(a, b: Float): Float;
function MultiplyFloat(a, b: Float): Float;
function DivideFloat(a, b: Float): Float;

{ funkcja ReturnVariableX zwraca SingleVariable gdy VariableName = SingleVariableName,
  wpp. rzuca wyjatek EUndefinedVariable. Przydatne i wygodne gdy chcesz napisac
  prosty program ktory uzywa Expression z tylko jedna zmienna, i nie przeszkadza
  ci ze SingleVariable[Name] to zmienne globalne (wiec mozesz naraz uzywac
  tylko jednego wyrazenia z ReturnSingleVariable). }
var SingleVariable: Float;
    SingleVariableName: string = 'x';
function ReturnSingleVariable(const VariableName: string): Float;
{ ReturnNoVariable zawsze rzuca wyjatek EUndefinedVariable, bez wzgledu na
  VariableName. Przydatne gdy chcesz obliczyc wartosc wyrazenia bez zmiennych. }
function ReturnNoVariable(const VariableName: string): Float;

{$undef read_interface}

implementation

{$define read_implementation}
{$I ObjectsList_1.inc}

{ strings ----------------------------------------------------------------------------- }

const
  SMathFuncArgsCountMustEqual = 'The %s function must have exactly %d parameters.';
  SMathFuncArgsCountMustGreaterEq = 'The %s function must have %d or more parameters.';

{ TMathExpr -------------------------------------------------------------------------- }

function TMathExpr.TryValue(varfunc: TVariableValueFunc; var return_value: Float): boolean;
begin
 try
  return_value := value(varfunc);
  result := true;
 except result := false end;
end;

{ TMathExprList ------------------------------------------------------------ }

function TMathExprList.Evaluate(EvalTwoExpr: TEvalMathExprFunc;
  VarFunc: TVariableValueFunc): Float;
var i: integer;
begin
 Result := Items[0].value(varfunc);
 for i := 1 to Count-1 do
  Result := EvalTwoExpr(Result, Items[i].Value(VarFunc));
end;

{ TMathConst ----------------------------------------------------------------- }

constructor TMathConst.Create(avalue: Float);
begin
 inherited Create;
 fvalue := avalue;
end;

function TMathConst.value(varfunc: TVariableValueFunc): Float;
begin result := fvalue end;

function TMathConst.ToString: string;
begin
 result := Format('%g', [ConstValue]);
end;

{ TMathVar --------------------------------------------------------------------------- }

constructor TMathVar.Create(const aName: string);
begin
 inherited Create;
 fName := aName;
end;

function TMathVar.value(varfunc: TVariableValueFunc): Float;
begin
 if Assigned(varfunc) then
  result := varfunc(Name) else
  raise Exception.Create('Variable '''+Name+''' undefined.');
end;

function TMathVar.ToString: string;
begin
 result := Name;
end;

{ TMathFunction ------------------------------------------------------------------------ }

procedure TMathFunction.CreateFinish(afuncKind: TFunctionKind);
var reqArgsCount: integer; { requested (by FunctionKinds table) ArgsCount }
begin
 ffuncKind := afuncKind;
 reqArgsCount := FunctionKinds[ffuncKind].ArgsCount;
 if reqArgsCount > 0 then
 begin
  if reqArgsCount <> ArgsCount then
   raise EWrongMathFunction.CreateFmt(SMathFuncArgsCountMustEqual, [FunctionKinds[ffuncKind].Name, reqArgsCount]);
 end else
 if reqArgsCount < 0 then
 begin
  if Abs(reqArgsCount) > ArgsCount then
   raise EWrongMathFunction.CreateFmt(SMathFuncArgsCountMustGreaterEq, [FunctionKinds[ffuncKind].Name, Abs(reqArgsCount)]);
 end;
end;

constructor TMathFunction.Create(afuncKind: TFunctionKind; AArgs: TMathExprList);
begin
 inherited Create;
 FArgs := TMathExprList.CreateFromList(AArgs);
 CreateFinish(afuncKind);
end;

constructor TMathFunction.Create(afuncKind: TFunctionKind; 
  const AArgs: array of TMathExpr);
begin
 inherited Create;
 FArgs := TMathExprList.CreateFromArray(AArgs);
 CreateFinish(afuncKind);
end;

destructor TMathFunction.Destroy;
var i: integer;
begin
 for i := 0 to ArgsCount-1 do FArgs[i].Free;
 FArgs.free;
 inherited;
end;

function TMathFunction.ArgsCount: integer;
begin result := FArgs.Count end;

function TMathFunction.Value(varfunc: TVariableValueFunc): Float;

  function ArgValue(argNum: cardinal): Float;
  begin result := FArgs[argnum].value(varfunc) end;

  function boolValue(argNum: cardinal): boolean;
  begin result := ArgValue(argNum) <> 0 end;

const boolTo01: array[boolean]of Float = (0, 1);

begin
 case ffuncKind of
  fkAdd:      Result := FArgs.Evaluate(addFloat, varfunc);
  fkSubtract: Result := FArgs.Evaluate(subtractFloat, varfunc);
  fkMultiply: Result := FArgs.Evaluate(multiplyFloat, varfunc);
  fkDivide:   Result := FArgs.Evaluate(divideFloat, varfunc);
  fkNegate:   Result := -ArgValue(0);
  fkModulo:   Result := ArgValue(0) - Floor(ArgValue(0)/ArgValue(1)) * ArgValue(1);

  fkSin:   Result := Sin(ArgValue(0));
  fkCos:   Result := Cos(ArgValue(0));
  fkTan:   Result := Tan(ArgValue(0));
  fkCotan: Result := Cotan(ArgValue(0));

  fkSinH:   Result:=  SinH(ArgValue(0));
  fkCosH:   Result:=  CosH(ArgValue(0));
  fkTanH:   Result:=  TanH(ArgValue(0));
  fkCotanH: Result := 1/TanH(ArgValue(0));

  fkArcSin:   Result := ArcSin(ArgValue(0));
  fkArcCos:   Result := ArcCos(ArgValue(0));
  fkArcTan:   Result := ArcTan(ArgValue(0));
  fkArcCotan: Result := ArcCot(ArgValue(0));

  fkLog2:   Result := Log2(ArgValue(0));
  fkLn:
  begin
   Writeln('now we will call Ln with ', ArgValue(0));
   Result := Ln(ArgValue(0));
  end;
  fkLog:    Result := Logn(ArgValue(0), ArgValue(1));
  fkPower2: Result := Power(2, ArgValue(0));
  fkExp:    Result := Exp(ArgValue(0));
  fkPower:  Result := GeneralPower(ArgValue(0), ArgValue(1));

  fkSqr:   Result := Sqr(ArgValue(0));
  fkSqrt:  Result := Sqrt(ArgValue(0));
  fkSgn:   Result := Sign(ArgValue(0));
  fkAbs:   Result := Abs(ArgValue(0));
  fkCeil:  Result := Ceil(ArgValue(0));
  fkFloor: Result := Floor(ArgValue(0));

  fkGreater:   Result := boolTo01[ArgValue(0) > ArgValue(1)];
  fkLesser:    Result := boolTo01[ArgValue(0) < ArgValue(1)];
  fkGreaterEq: Result := boolTo01[ArgValue(0) >= ArgValue(1)];
  fkLesserEq:  Result := boolTo01[ArgValue(0) <= ArgValue(1)];
  fkEqual:     Result := boolTo01[ArgValue(0) = ArgValue(1)];
  fkNotEqual:  Result := boolTo01[ArgValue(0) <> ArgValue(1)];

  fkOr:  Result := boolTo01[boolValue(0) or boolValue(1)];
  fkAnd: Result := boolTo01[boolValue(0) and boolValue(1)];
  fkNot: Result := boolTo01[not boolValue(0)];

  else raise Exception.Create('Ou ! Function '+FunctionKinds[ffuncKind].Name+' not implemented !');
 end;

 { Force raising pending exceptions by above FP calculations. }
 ClearExceptions(true);
end;

function TMathFunction.ToString: string;

  { FArgs[i].ToString, wrapped in () if it's TMathFunction and InfixOper. }
  function ArgToString(i: Integer): string;
  begin
   Result := FArgs[i].ToString;
   if (FArgs[i] is TMathFunction) and
      (FunctionKinds[TMathFunction(FArgs[i]).FuncKind].
        InfixOperatorName<> '') then
    Result := '('+Result+')';
  end;

var i: Integer;
    NegatedConst: TMathConst;
begin
 if FuncKind = fkNegate then
 begin
  if FArgs[0] is TMathConst then
  begin
   { It's simplest and safest here to just use TMathConst.ToString
     with TMathConst with negated Value.
     If not, I would be undertain here whether to wrap
     FArgs[0].ToString inside () when FArgs[0] is TMathConst. }
   NegatedConst := TMathConst.Create(-TMathConst(FArgs[0]).ConstValue);
   try
    Result := NegatedConst.ToString;
   finally NegatedConst.Free end;
  end else
   Result := '-' + ArgToString(0);
 end else
 if FunctionKinds[FuncKind].InfixOperatorName <> '' then
 begin
  Result := '';
  for i := 0 to FArgs.Count-1 do
  begin
   Result += ArgToString(i);
   if i < FArgs.Count-1 then
    Result += ' ' +FunctionKinds[FuncKind].InfixOperatorName +' ';
  end;
 end else
 begin
  Result := FunctionKinds[FuncKind].FunctionName +'(';
  for i := 0 to FArgs.Count-1 do
  begin
   Result += FArgs[i].ToString; { no need to use here ArgToString(i) }
   if i < FArgs.Count-1 then Result += ',';
  end;
  Result += ')';
 end;
end;

{ EUndefinedVariable ----------------------------------------------- }

constructor EUndefinedVariable.Create(const VariableName: string);
begin
 inherited Create('Undefined variable : '''+VariableName+'''');
end;

{ inne funkcje ------------------------------------------------------------------- }

function AddFloat(a, b: Float): Float; begin result := a+b end;
function SubtractFloat(a, b: Float): Float; begin result := a-b end;
function MultiplyFloat(a, b: Float): Float; begin result := a*b end;
function DivideFloat(a, b: Float): Float; begin result := a/b end;

function ReturnSingleVariable(const VariableName: string): Float;
begin
 if SameText(VariableName, SingleVariableName) then
  result := SingleVariable else
  raise EUndefinedVariable.Create(VariableName);
end;

function ReturnNoVariable(const VariableName: string): Float;
begin
 raise EUndefinedVariable.Create(VariableName);
end;

end.
