{
  Copyright 2001-2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ Base KambiScript structures: values, functions, expressions.

  It is designed to be extendable, so you can add new TKamScriptValue
  descendants and new TKamScriptFunction descendants, and register
  their handlers in FunctionHandlers instance (TKamScriptFunctionHandlers).

  Using structures here you can also build KambiScript expressions
  by Pascal code (that is, you don't have to parse them). For example
  this is an expression that calculates @code(sin(3) + 10 + 1):

@longcode(#
  Expr := TKamScriptAdd.Create([
      TKamScriptSin.Create([TKamScriptFloat.Create(3)]),
      TKamScriptFloat.Create(10),
      TKamScriptFloat.Create(1)
    ]);
#)

  You can then call @code(Expr.Execute) to calculate such expression.

  To make a variable in the expression, just create and remember a
  TKamScriptFloat instance first, and then change it's value freely between
  @code(Expr.Execute) calls. For example

@longcode(#
  MyVariable := TKamScriptFloat.Create(3);
  Expr := TKamScriptAdd.Create([
      TKamScriptSin.Create([MyVariable]),
      TKamScriptFloat.Create(10),
      TKamScriptFloat.Create(1)
    ]);

  Writeln((Expr.Execute as TKamStringFloat).Value); // calculate "sin(3) + 10 + 1"

  MyVariable.Value := 4;
  Writeln((Expr.Execute as TKamStringFloat).Value); // calculate "sin(4) + 10 + 1"

  MyVariable.Value := 5;
  Writeln((Expr.Execute as TKamStringFloat).Value); // calculate "sin(5) + 10 + 1"
#)

  Note that generally each TKamScriptExpression owns it's children
  expressions, so they will be automatically freed when parent is freed.
  Also, the values returned by Execute are owned by expression.
  So you can simply free whole thing by @code(Expr.Free).

  If you're want to parse KambiScript expression from a text
  file, see KambiScriptParser.
}
unit KambiScript;

interface

uses SysUtils, Math, Contnrs, KambiUtils, KambiClassUtils;

{$define read_interface}

type
  TKamScriptValue = class;

  TKamScriptExpression = class
  public
    { Execute and calculate this expression.

      Returned value is owned by this object. Which should be comfortable
      for you usually, as you do not have to worry about freeing it.
      Also, it allows us to make various optimizations to avoid
      creating/destroying lots of temporary TKamScriptExpression
      instances during calculation of complex expression.

      Execute is guaranteed to raise an exception if some
      calculation fails, e.g. if expression will be 'ln(-3)'.
      Stating it directly, Execute may even call Math.ClearExceptions(true)
      if it is needed to force generating proper exceptions.

      This ensures that we can safely execute even invalid expressions
      (like 'ln(-3)') and get reliable exceptions. }
    function Execute: TKamScriptValue; virtual; abstract;

    { Try to execute expression, or return @nil if an error within
      expression. "Error within expression" means that
      any exception occured while calculating expression. }
    function TryExecute: TKamScriptValue;

    { Call Free, but only if this is not TKamScriptValue with
      OwnedByParentExpression = false. (This cannot be implemented
      cleanly, as virtual procedure, since it must work when Self is @nil,
      and then virtual method table is not available of course.) }
    procedure FreeByParentExpression;
  end;

  TObjectsListItem_1 = TKamScriptExpression;
  {$I objectslist_1.inc}
  TKamScriptExpressionsList = TObjectsList_1;

  TKamScriptValue = class(TKamScriptExpression)
  private
    FOwnedByParentExpression: boolean;
    FName: string;
  public
    constructor Create; virtual;
    function Execute: TKamScriptValue; override;

    property OwnedByParentExpression: boolean
      read FOwnedByParentExpression write FOwnedByParentExpression
      default true;

    { Name of this value, or '' if not named.
      Named value can be recognized in expressions by KambiScriptParser. }
    property Name: string read FName write FName;
  end;

  TKamScriptValueClass = class of TKamScriptValue;

  TKamScriptFloat = class(TKamScriptValue)
  private
    class procedure HandleAdd(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleSubtract(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleMultiply(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleDivide(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleNegate(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleModulo(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleSin(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleCos(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleTan(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleCotan(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleArcSin(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleArcCos(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleArcTan(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleArcCotan(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleSinh(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleCosh(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleTanh(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleCotanh(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleLog2(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleLn(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleLog(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandlePower2(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleExp(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandlePower(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleSqr(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleSqrt(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleSgn(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleAbs(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleCeil(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleFloor(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleGreater(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleLesser(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleGreaterEq(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleLesserEq(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleEqual(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleNotEqual(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleOr(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleAnd(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
    class procedure HandleNot(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
  public
    Value: Float;

    { Comfortable constructor to set initial Value.
      Note that the inherited constructor without parameters is
      also fine to use, it will set value to zero. }
    constructor Create(AValue: Float);

    constructor Create; override;
  end;

  TKamScriptFunction = class(TKamScriptExpression)
  private
    FArgs: TKamScriptExpressionsList;
    LastExecuteResult: TKamScriptValue;
    procedure CreateFinish;
  public
    { Constructor initializing Args from given TKamScriptExpressionsList.
      AArgs list contents is copied, i.e. AArgs refence is not
      stored or freed by TKamScriptFunction. But items on AArags are not copied
      recursively, we copy references from AArags items, and so we become
      their owners. }
    constructor Create(AArgs: TKamScriptExpressionsList); overload;
    constructor Create(const AArgs: array of TKamScriptExpression); overload;
    destructor Destroy; override;

    { Number of allowed function arguments.
      This is checked when parsing expression.
      In function handlers (see TKamScriptFunctionHandlers) you may assume
      that you have specified number of arguments.

      Value 0 means that any number of arguments is Ok (including
      no arguments !).

      Value < 0 means that at least -Value arguments are required,
      but any more are allowed too. For example, ArgsCount = -1
      means that any non-zero number of argumenrs is allowed.
      ArgsCount = -2 mean that we require >= 2 arguments. }
    class function AllowedArgumentsCount: Integer; virtual; abstract;

    { Long function name for user. This is possibly with spaces,
      parenthesis and other funny characters. It will be used in
      error messages and such to describe this function. }
    class function Name: string; virtual; abstract;

    { Short function name, for the parser.
      This is the name of the function for use in expressions
      like "function_name(arg_1, arg_2 ... , arg_n)".

      This can be empty string ('') if no such name for this function exists,
      then the logic to parse this function expressions must be somehow
      built in the parser (for example, operators use this: they are
      just normal functions, TKamScriptFunction, with ShortName = ''
      and special support in the parser). }
    class function ShortName: string; virtual; abstract;

    { Function name when used as an infix operator.

      Empty string ('') if no such name for this function.
      This is returned by default implementation of this in this class.

      This does require cooperation from the parser to actually work,
      that is you cannot simply define new operators by
      registering new TKamScriptFunction with InfixOperatorName <> ''.
      For now.

      Note that at least one of ShortName and InfixOperatorName
      must not be empty.

      The only exception is the TKamScriptNegate function, that is neither
      infix operator nor a usual function that must be specified
      as "function_name(arguments)". So this is an exception,
      and if there will be a need, I shall fix this (probably
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
          Function may have both ShortName <> '' and InfixOperatorName <> ''.
          E.g. TKamScriptPower can be used as "Power(3, 1.5)" or "3 ^ 1.5".)
      ) }
    class function InfixOperatorName: string; virtual;

    { Function arguments. Don't modify this list after function is created
      (although you can modify values inside arguments). }
    property Args: TKamScriptExpressionsList read FArgs;

    function Execute: TKamScriptValue; override;
  end;

  TKamScriptFunctionClass = class of TKamScriptFunction;

  { Calculate result on given function arguments Arguments.
    Place result in AResult.

    The current function is not passed here --- you don't need it
    (you already get a list of calculated Arguments, and you should
    register different procedures for different TKamScriptFunction classes,
    so you know what operation on arguments should be done).

    If needed, previous value of AResult should be freed and new created.
    If current AResult is <> nil and it's of appropriate class,
    you may also reuse it and only change it's fields
    (this is helpful, to avoid many creations/destroying
    of class instances while calculating an expression many times).
    CreateValueIfNeeded may be helpful for implementing this. }
  TKamScriptFunctionHandler = procedure (
    const Arguments: array of TKamScriptValue;
    var AResult: TKamScriptValue) of object;

  TKamScriptRegisteredHandler = class
  private
    FHandler: TKamScriptFunctionHandler;
    FFunctionClass: TKamScriptFunctionClass;
    FArgumentClass: TKamScriptValueClass;
  public
    constructor Create(
      AHandler: TKamScriptFunctionHandler;
      AFunctionClass: TKamScriptFunctionClass;
      AArgumentClass: TKamScriptValueClass);
    property Handler: TKamScriptFunctionHandler read FHandler;
    property FunctionClass: TKamScriptFunctionClass read FFunctionClass;
    property ArgumentClass: TKamScriptValueClass read FArgumentClass;
  end;

  { This specifies for each type (TKamScriptValue class)
    and for each function (TKamScriptFunction class) how they should
    be handled. You can think of this as a 2D table that has a handler
    for each TKamScriptValue and TKamScriptFunction combination.

    The idea is to allow programmer to extend KambiScipt by

    @orderedList(
      @item(Defining new types of values for KambiScript:
        add new TKamScriptValue class, and create handlers for known
        functions to handle this type.

        It may be comfortable to place these handlers as private methods
        within your new TKamScriptValue descendant, although this is your
        private decision.)

      @item(Defining new functions for KambiScript:
        add new TKamScriptFunction class, and create handlers for known
        values to be handled by this function.

        It may be comfortable to place these handlers as private methods
        within your new TKamScriptFunction descendant, although this is your
        private decision.)
    )

    The limitation of this approach is that a function call when
    some arguments have different type than the others is immediately
    invalid (since TKamScriptRegisteredHandler has only one
    ArgumentClass field, and it must be satisfied by all arguments).

    You have a guarantee that every registered here Handler will be called
    only with AFunction of registstered type and all Arguments
    of registered type.

    As a bonus, this also provides a list of all usable function classes.
    That's because you have to register at least one handler for each
    TKamScriptFunction descendant to make this function actually usable,
    so we know about it here. }
  TKamScriptFunctionHandlers = class
  private
    { This is a list of another TObjectList lists.

      Each nested list has only TKamScriptRegisteredHandler items.
      It always has at least one item.
      Each nested list has only equal FunctionClass values. }
    FHandlersByFunction: TObjectList;

    function SearchFunctionClass(
      FunctionClass: TKamScriptFunctionClass;
      out FunctionIndex: Integer;
      out HandlersByArgument: TObjectList): boolean; overload;
    function SearchFunctionClass(
      FunctionClass: TKamScriptFunctionClass;
      out HandlersByArgument: TObjectList): boolean; overload;

    function SearchArgumentClass(
      HandlersByArgument: TObjectList;
      ArgumentClass: TKamScriptValueClass;
      out ArgumentIndex: Integer;
      out Handler: TKamScriptRegisteredHandler): boolean; overload;
    function SearchArgumentClass(
      HandlersByArgument: TObjectList;
      ArgumentClass: TKamScriptValueClass;
      out Handler: TKamScriptRegisteredHandler): boolean; overload;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterHandler(
      AHandler: TKamScriptFunctionHandler;
      AFunctionClass: TKamScriptFunctionClass;
      AArgumentClass: TKamScriptValueClass);

    { Search for function class with matching ShortName.
      Returns @nil if not found. }
    function SearchFunctionShortName(const AShortName: string): TKamScriptFunctionClass;
  end;

  EKamScriptError = class(Exception);
  EKamScriptFunctionArgumentsError = class(EKamScriptError);
  EKamScriptFunctionNoHandler = class(EKamScriptError);

var
  FunctionHandlers: TKamScriptFunctionHandlers;

{ Make sure Value is assigned and of NeededClass.
  If Value is not assigned, or is not exactly of NeededClass,
  it will be freed and new will be created. }
procedure CreateValueIfNeeded(var Value: TKamScriptValue;
  NeededClass: TKamScriptValueClass);

{$undef read_interface}

implementation

uses KambiScriptMathFunctions;

{$define read_implementation}
{$I objectslist_1.inc}

{ TKamScriptExpression ------------------------------------------------------- }

procedure TKamScriptExpression.FreeByParentExpression;
begin
  if (Self <> nil) and
      ( (not (Self is TKamScriptValue)) or
        TKamScriptValue(Self).OwnedByParentExpression ) then
    Free;
end;

function TKamScriptExpression.TryExecute: TKamScriptValue;
begin
  try
    Result := Execute;
  except
    Result := nil;
  end;
end;

{ TKamScriptValue ------------------------------------------------------------ }

constructor TKamScriptValue.Create;
begin
  inherited;
  FOwnedByParentExpression := true;
end;

function TKamScriptValue.Execute: TKamScriptValue;
begin
  { Since we own Execute result, we can simply return self here. }
  Result := Self;
end;

{ TKamScriptFloat ------------------------------------------------------- }

constructor TKamScriptFloat.Create(AValue: Float);
begin
  Create;
  Value := AValue;
end;

constructor TKamScriptFloat.Create;
begin
  inherited Create;
end;

class procedure TKamScriptFloat.HandleAdd(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  { The function allows only >= 1 arguments, and this handler is
    registered only for TKamScriptFloat values, so we can safely take
    the first arg as TKamScriptFloat. }
  TKamScriptFloat(AResult).Value := TKamScriptFloat(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TKamScriptFloat(AResult).Value += TKamScriptFloat(Arguments[I]).Value;
end;

class procedure TKamScriptFloat.HandleSubtract(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := TKamScriptFloat(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TKamScriptFloat(AResult).Value -= TKamScriptFloat(Arguments[I]).Value;
end;

class procedure TKamScriptFloat.HandleMultiply(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := TKamScriptFloat(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TKamScriptFloat(AResult).Value *= TKamScriptFloat(Arguments[I]).Value;
end;

class procedure TKamScriptFloat.HandleDivide(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := TKamScriptFloat(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TKamScriptFloat(AResult).Value /= TKamScriptFloat(Arguments[I]).Value;
end;

class procedure TKamScriptFloat.HandleNegate(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := - TKamScriptFloat(Arguments[0]).Value;
end;

class procedure TKamScriptFloat.HandleModulo(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value :=
    TKamScriptFloat(Arguments[0]).Value -
    Floor( TKamScriptFloat(Arguments[0]).Value /
           TKamScriptFloat(Arguments[1]).Value )
    * TKamScriptFloat(Arguments[1]).Value;
end;

class procedure TKamScriptFloat.HandleSin(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Sin( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleCos(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Cos( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleTan(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Tan( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleCotan(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := KamCoTan( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleArcSin(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := ArcSin( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleArcCos(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := ArcCos( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleArcTan(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := ArcTan( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleArcCotan(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := ArcCot( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleSinh(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := SinH( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleCosh(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := CosH( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleTanh(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := TanH( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleCotanh(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := 1 / TanH( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleLog2(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Log2( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleLn(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Ln( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleLog(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Logn( TKamScriptFloat(Arguments[0]).Value,
                                          TKamScriptFloat(Arguments[1]).Value );
end;

class procedure TKamScriptFloat.HandlePower2(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Power(2, TKamScriptFloat(Arguments[0]).Value);
end;

class procedure TKamScriptFloat.HandleExp(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Exp( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandlePower(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := GeneralPower(
    TKamScriptFloat(Arguments[0]).Value,
    TKamScriptFloat(Arguments[1]).Value );
end;

class procedure TKamScriptFloat.HandleSqr(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Sqr( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleSqrt(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Sqrt( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleSgn(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Sign( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleAbs(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Abs( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleCeil(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Ceil( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleFloor(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Floor( TKamScriptFloat(Arguments[0]).Value );
end;

const
  BoolToFloat: array [boolean] of Float = (0, 1);

function FloatToBool(const A: Float): boolean;
begin
  Result := A <> 0;
end;

class procedure TKamScriptFloat.HandleGreater(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := BoolToFloat[
    TKamScriptFloat(Arguments[0]).Value >
    TKamScriptFloat(Arguments[1]).Value];
end;

class procedure TKamScriptFloat.HandleLesser(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := BoolToFloat[
    TKamScriptFloat(Arguments[0]).Value <
    TKamScriptFloat(Arguments[1]).Value];
end;

class procedure TKamScriptFloat.HandleGreaterEq(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := BoolToFloat[
    TKamScriptFloat(Arguments[0]).Value >=
    TKamScriptFloat(Arguments[1]).Value];
end;

class procedure TKamScriptFloat.HandleLesserEq(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := BoolToFloat[
    TKamScriptFloat(Arguments[0]).Value <=
    TKamScriptFloat(Arguments[1]).Value];
end;

class procedure TKamScriptFloat.HandleEqual(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := BoolToFloat[
    TKamScriptFloat(Arguments[0]).Value =
    TKamScriptFloat(Arguments[1]).Value];
end;

class procedure TKamScriptFloat.HandleNotEqual(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := BoolToFloat[
    TKamScriptFloat(Arguments[0]).Value <>
    TKamScriptFloat(Arguments[1]).Value];
end;

class procedure TKamScriptFloat.HandleOr(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := BoolToFloat[
    FloatToBool(TKamScriptFloat(Arguments[0]).Value) or
    FloatToBool(TKamScriptFloat(Arguments[1]).Value) ];
end;

class procedure TKamScriptFloat.HandleAnd(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := BoolToFloat[
    FloatToBool(TKamScriptFloat(Arguments[0]).Value) and
    FloatToBool(TKamScriptFloat(Arguments[1]).Value) ];
end;

class procedure TKamScriptFloat.HandleNot(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue);
begin
  CreateValueIfNeeded(AResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := BoolToFloat[
    not FloatToBool(TKamScriptFloat(Arguments[0]).Value) ];
end;

{ TKamScriptFunction --------------------------------------------------------- }

constructor TKamScriptFunction.Create(AArgs: TKamScriptExpressionsList);
begin
  inherited Create;
  FArgs := TKamScriptExpressionsList.CreateFromList(AArgs);
  CreateFinish;
end;

constructor TKamScriptFunction.Create(const AArgs: array of TKamScriptExpression);
begin
  inherited Create;
  FArgs := TKamScriptExpressionsList.CreateFromArray(AArgs);
  CreateFinish;
end;

procedure TKamScriptFunction.CreateFinish;
const
  SFuncArgsCountMustEqual = 'The %s function must have exactly %d parameters';
  SFuncArgsCountMustGreaterEq = 'The %s function must have %d or more parameters';
begin
  if AllowedArgumentsCount > 0 then
  begin
    if AllowedArgumentsCount <> Args.Count then
      raise EKamScriptFunctionArgumentsError.CreateFmt(
        SFuncArgsCountMustEqual, [Name, AllowedArgumentsCount]);
  end else
  if AllowedArgumentsCount < 0 then
  begin
    if -AllowedArgumentsCount > Args.Count then
      raise EKamScriptFunctionArgumentsError.CreateFmt(
        SFuncArgsCountMustGreaterEq, [Name, -AllowedArgumentsCount]);
  end;
end;

destructor TKamScriptFunction.Destroy;
var
  I: Integer;
begin
  if FArgs <> nil then
  begin
    for I := 0 to FArgs.Count - 1 do
      FArgs[I].FreeByParentExpression;
    FreeAndNil(FArgs);
  end;
  LastExecuteResult.FreeByParentExpression;
  inherited;
end;

class function TKamScriptFunction.InfixOperatorName: string;
begin
  Result := '';
end;

function TKamScriptFunction.Execute: TKamScriptValue;
var
  HandlersByArgument: TObjectList;
  Handler: TKamScriptRegisteredHandler;
  Arguments: array of TKamScriptValue;
  I: Integer;
begin
  if FunctionHandlers.SearchFunctionClass(
    TKamScriptFunctionClass(Self.ClassType), HandlersByArgument) then
  begin
    { We have to calculate arguments first, to know their type,
      to decide which handler is suitable. }
    SetLength(Arguments, Args.Count);
    for I := 0 to Args.Count - 1 do
      Arguments[I] := Args[I].Execute;

    { calculate Handler }
    if Args.Count = 0 then
    begin
      { function without arguments? use any handler. }
      Handler := HandlersByArgument[0] as TKamScriptRegisteredHandler;
    end else
    begin
      if not FunctionHandlers.SearchArgumentClass(
        HandlersByArgument,
        TKamScriptValueClass(Arguments[0].ClassType), Handler) then
        raise EKamScriptFunctionNoHandler.CreateFmt('Function "%s" is not defined for arguments of type "%s"',
          [Name, Arguments[0].ClassName]);
    end;

    Handler.Handler(Arguments, LastExecuteResult);

    { Force raising pending exceptions by FP calculations in Handler.Handler. }
    ClearExceptions(true);

    Result := LastExecuteResult;
  end else
    raise EKamScriptFunctionNoHandler.CreateFmt('No handler defined for function "%s"', [Name]);
end;

{ TKamScriptRegisteredHandler ------------------------------------------------ }

constructor TKamScriptRegisteredHandler.Create(
  AHandler: TKamScriptFunctionHandler;
  AFunctionClass: TKamScriptFunctionClass;
  AArgumentClass: TKamScriptValueClass);
begin
  FHandler := AHandler;
  FFunctionClass := AFunctionClass;
  FArgumentClass := AArgumentClass;
end;

{ TKamScriptFunctionHandlers ------------------------------------------------- }

constructor TKamScriptFunctionHandlers.Create;
begin
  inherited;
  FHandlersByFunction := TObjectList.Create(true);
end;

destructor TKamScriptFunctionHandlers.Destroy;
begin
  FreeAndNil(FHandlersByFunction);
  inherited;
end;

function TKamScriptFunctionHandlers.SearchFunctionClass(
  FunctionClass: TKamScriptFunctionClass;
  out FunctionIndex: Integer;
  out HandlersByArgument: TObjectList): boolean;
var
  I: Integer;
begin
  for I := 0 to FHandlersByFunction.Count - 1 do
  begin
    HandlersByArgument := FHandlersByFunction[I] as TObjectList;
    if FunctionClass = (HandlersByArgument[0] as
      TKamScriptRegisteredHandler).FunctionClass then
    begin
      FunctionIndex := I;
      Result := true;
      Exit;
    end;
  end;
  Result := false;
end;

function TKamScriptFunctionHandlers.SearchFunctionClass(
  FunctionClass: TKamScriptFunctionClass;
  out HandlersByArgument: TObjectList): boolean;
var
  FunctionIndex: Integer;
begin
  Result := SearchFunctionClass(
    FunctionClass, FunctionIndex, HandlersByArgument);
end;

function TKamScriptFunctionHandlers.SearchArgumentClass(
  HandlersByArgument: TObjectList;
  ArgumentClass: TKamScriptValueClass;
  out ArgumentIndex: Integer;
  out Handler: TKamScriptRegisteredHandler): boolean;
var
  I: Integer;
begin
  for I := 0 to HandlersByArgument.Count - 1 do
  begin
    Handler := HandlersByArgument[I] as TKamScriptRegisteredHandler;
    if Handler.ArgumentClass = ArgumentClass then
    begin
      ArgumentIndex := I;
      Result := true;
      Exit;
    end
  end;
  Result := false;
end;

function TKamScriptFunctionHandlers.SearchArgumentClass(
  HandlersByArgument: TObjectList;
  ArgumentClass: TKamScriptValueClass;
  out Handler: TKamScriptRegisteredHandler): boolean;
var
  ArgumentIndex: Integer;
begin
  Result := SearchArgumentClass(
    HandlersByArgument, ArgumentClass, ArgumentIndex, Handler);
end;

procedure TKamScriptFunctionHandlers.RegisterHandler(
  AHandler: TKamScriptFunctionHandler;
  AFunctionClass: TKamScriptFunctionClass;
  AArgumentClass: TKamScriptValueClass);
var
  HandlersByArgument: TObjectList;
  Handler: TKamScriptRegisteredHandler;
begin
  if SearchFunctionClass(AFunctionClass, HandlersByArgument) then
  begin
    if not SearchArgumentClass(HandlersByArgument, AArgumentClass, Handler) then
    begin
      Handler := TKamScriptRegisteredHandler.Create(
        AHandler, AFunctionClass, AArgumentClass);
      HandlersByArgument.Add(Handler);
    end;
  end else
  begin
    HandlersByArgument := TObjectList.Create(true);
    FHandlersByFunction.Add(HandlersByArgument);

    Handler := TKamScriptRegisteredHandler.Create(
      AHandler, AFunctionClass, AArgumentClass);
    HandlersByArgument.Add(Handler);
  end;
end;

function TKamScriptFunctionHandlers.SearchFunctionShortName(
  const AShortName: string): TKamScriptFunctionClass;
var
  I: Integer;
  HandlersByArgument: TObjectList;
begin
  for I := 0 to FHandlersByFunction.Count - 1 do
  begin
    HandlersByArgument := FHandlersByFunction[I] as TObjectList;
    Result := (HandlersByArgument[0] as
      TKamScriptRegisteredHandler).FunctionClass;
    if SameText(AShortName, Result.ShortName) then
      Exit;
  end;
  Result := nil;
end;

{ procedural utils ----------------------------------------------------------- }

procedure CreateValueIfNeeded(var Value: TKamScriptValue;
  NeededClass: TKamScriptValueClass);
begin
  if Value = nil then
    Value := NeededClass.Create else
  if Value.ClassType <> NeededClass then
  begin
    Value.FreeByParentExpression;
    Value := NeededClass.Create;
  end;
end;

{ unit init/fini ------------------------------------------------------------- }

initialization
  FunctionHandlers := TKamScriptFunctionHandlers.Create;

  { Register handlers for TKamScriptFloat for functions in
    KambiScriptMathFunctions. }
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleAdd, TKamScriptAdd, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleSubtract, TKamScriptSubtract, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleMultiply, TKamScriptMultiply, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleDivide, TKamScriptDivide, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleNegate, TKamScriptNegate, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleModulo, TKamScriptModulo, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleSin, TKamScriptSin, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleCos, TKamScriptCos, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleTan, TKamScriptTan, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleCotan, TKamScriptCotan, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleArcSin, TKamScriptArcSin, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleArcCos, TKamScriptArcCos, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleArcTan, TKamScriptArcTan, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleArcCotan, TKamScriptArcCotan, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleSinh, TKamScriptSinh, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleCosh, TKamScriptCosh, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleTanh, TKamScriptTanh, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleCotanh, TKamScriptCotanh, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleLog2, TKamScriptLog2, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleLn, TKamScriptLn, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleLog, TKamScriptLog, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandlePower2, TKamScriptPower2, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleExp, TKamScriptExp, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandlePower, TKamScriptPower, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleSqr, TKamScriptSqr, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleSqrt, TKamScriptSqrt, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleSgn, TKamScriptSgn, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleAbs, TKamScriptAbs, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleCeil, TKamScriptCeil, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleFloor, TKamScriptFloor, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleGreater, TKamScriptGreater, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleLesser, TKamScriptLesser, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleGreaterEq, TKamScriptGreaterEq, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleLesserEq, TKamScriptLesserEq, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleEqual, TKamScriptEqual, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleNotEqual, TKamScriptNotEqual, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleOr, TKamScriptOr, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleAnd, TKamScriptAnd, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(Nil).HandleNot, TKamScriptNot, TKamScriptFloat);
finalization
  FreeAndNil(FunctionHandlers);
end.
