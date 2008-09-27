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
  { }
  TKamScriptValue = class;

  EKamScriptError = class(Exception);
  EKamAssignValueError = class(EKamScriptError);

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
  TKamScriptExpressionsList = class(TObjectsList_1)
    procedure FreeContentsByParentExpression;
  end;

  TKamScriptValue = class(TKamScriptExpression)
  private
    FOwnedByParentExpression: boolean;
    FName: string;
    FValueAssigned: boolean;
  public
    constructor Create; virtual;
    function Execute: TKamScriptValue; override;

    property OwnedByParentExpression: boolean
      read FOwnedByParentExpression write FOwnedByParentExpression
      default true;

    { Name of this value, or '' if not named.
      Named value can be recognized in expressions by KambiScriptParser. }
    property Name: string read FName write FName;

    { Assign value from Source to Self.
      @raises(EKamAssignValueError if assignment is not possible
      because types don't match.) }
    procedure AssignValue(Source: TKamScriptValue); virtual; abstract;

    { Set to @true on each assign to Value. You can reset it at any time
      to @false.

      This allows the caller to know which variables were
      assigned during script execution, which is useful if changes to
      KambiScript variables should be propagated to some other things
      after the script finished execution. This is essential for behavior
      in VRML Script node.

      Descendants note: you have to set this to @true in SetValue. }
    property ValueAssigned: boolean read FValueAssigned write FValueAssigned
      default false;
  end;

  TKamScriptValueClass = class of TKamScriptValue;

  TObjectsListItem_2 = TKamScriptValue;
  {$I objectslist_2.inc}
  TKamScriptValuesList = TObjectsList_2;

  TKamScriptFloat = class(TKamScriptValue)
  private
    class procedure HandleAdd(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleSubtract(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleMultiply(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleDivide(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleNegate(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleModulo(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleSin(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleCos(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleTan(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleCotan(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArcSin(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArcCos(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArcTan(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArcCotan(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleSinh(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleCosh(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleTanh(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleCotanh(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleLog2(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleLn(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleLog(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandlePower2(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleExp(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandlePower(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleSqr(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleSqrt(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleSgn(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleAbs(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleCeil(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleFloor(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleGreater(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleLesser(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleGreaterEq(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleLesserEq(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleEqual(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleNotEqual(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleOr(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleAnd(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleNot(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    function GetAsBoolean: boolean;
    procedure SetAsBoolean(const AValue: boolean);

    FValue: Float;
    procedure SetValue(const AValue: Float);
  public
    { Comfortable constructor to set initial Value.
      Note that the inherited constructor without parameters is
      also fine to use, it will set value to zero. }
    constructor Create(AValue: Float);

    constructor Create; override;

    property Value: Float read FValue write SetValue;

    { Read/write @link(Value) as boolean. This automatically does simple
      convertion between Float and boolean.

      When setting, false results in 0 and true in 1.
      When reading, anything different than 0 results in true. }
    property AsBoolean: boolean read GetAsBoolean write SetAsBoolean;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptFunction = class(TKamScriptExpression)
  private
    FArgs: TKamScriptExpressionsList;
    LastExecuteResult: TKamScriptValue;
    ParentOfLastExecuteResult: boolean;
  protected
    { Used by constructor to check are args valid.
      @raises(EKamScriptFunctionArgumentsError on invalid Args passed to
      function.) }
    procedure CheckArguments; virtual;
  public
    { Constructor initializing Args from given TKamScriptExpressionsList.
      AArgs list contents is copied, i.e. AArgs refence is not
      stored or freed by TKamScriptFunction. But items on AArags are not copied
      recursively, we copy references from AArags items, and so we become
      their owners.

      @raises(EKamScriptFunctionArgumentsError if you specified invalid
        number of arguments for this function.)
    }
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
    var AResult: TKamScriptValue;
    var ParentOfResult: boolean) of object;

  TKamScriptSequence = class(TKamScriptFunction)
  private
    class procedure HandleSequence(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  { KambiScript assignment operator. This is a special function,
    that must have settable TKamScriptValue as it's 1st argument.

    For now, we check TKamScriptValue.Name <> '', this determines if
    this is settable (in the future, more explicit check may be done). }
  TKamScriptAssignment = class(TKamScriptFunction)
  private
    class procedure HandleAssignment(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
  protected
    procedure CheckArguments; override;
  public
    class function AllowedArgumentsCount: Integer; override;
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

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

  EKamScriptFunctionArgumentsError = class(EKamScriptError);
  EKamScriptFunctionNoHandler = class(EKamScriptError);

  { KambiScript function definition.

    Not to be confused with TKamScriptFunction: TKamScriptFunction is
    an internal, built-in function or operator. This class represents
    functions defined by user. }
  TKamScriptFunctionDefinition = class
  private
    FName: string;
    FParameters: TKamScriptValuesList;
    FBody: TKamScriptExpression;
  public
    constructor Create;
    destructor Destroy; override;

    property Name: string read FName write FName;

    { List of function parameters.

      Note that they are also referenced inside function Expression,
      so you simply change them to set value of this parameter within
      whole function body.

      These are always fresh variables, not referenced anywhere outside
      of Body. This means that they are owned (always, regardless of
      OwnedByParentExpression) by this class. }
    property Parameters: TKamScriptValuesList read FParameters;

    { Function body. }
    property Body: TKamScriptExpression read FBody write FBody;
  end;

  TObjectsListItem_3 = TKamScriptFunctionDefinition;
  {$I objectslist_3.inc}
  TKamScriptFunctionDefinitionsList = class(TObjectsList_3)
    function IndexOf(const FunctionName: string): Integer;
  end;

  EKamScriptMissingFunction = class(EKamScriptError);

  TKamScriptProgram = class
  private
    FFunctions: TKamScriptFunctionDefinitionsList;
  public
    constructor Create;
    destructor Destroy; override;

    property Functions: TKamScriptFunctionDefinitionsList read FFunctions;

    { Execute a user-defined function (from Functions list of this program).

      @unorderedList(
        @item(Looks for given FunctionName.

          IgnoreMissingFunction says what to do in case of missing function:
          if true, it will be simply ignored (ExecuteFunction will
          silently do nothng). If false (default)
          then we will raise exception EKamScriptMissingFunction.)
        @item(Sets function parameters to given values
         (number of parameters must match, otherwise EKamScriptError).)
        @item(Finally executes function body.)
      )
    }
    procedure ExecuteFunction(const FunctionName: string;
      const Parameters: array of Float;
      const IgnoreMissingFunction: boolean = false);
  end;

var
  FunctionHandlers: TKamScriptFunctionHandlers;

{ Make sure Value is assigned and of NeededClass.
  If Value is not assigned, or is not exactly of NeededClass,
  it will be freed and new will be created. }
procedure CreateValueIfNeeded(var Value: TKamScriptValue;
  var ParentOfValue: boolean;
  NeededClass: TKamScriptValueClass);

{$undef read_interface}

implementation

uses KambiScriptMathFunctions;

{$define read_implementation}
{$I objectslist_1.inc}
{$I objectslist_2.inc}
{$I objectslist_3.inc}

{ FPC 2.2.2 has bug http://bugs.freepascal.org/view.php?id=12214
  that strongly hits calculating invalid expressions.
  This results in calls like
    gen_function "ln(x)" -10 10 0.1
    gen_function "sqrt(x)" -10 10 0.1
  to fail after a couple of "break" lines with

    gen_function: Exception EInvalidOp (at address 0x080488B5) :
    Invalid floating point operation
    An unhandled exception occurred at $080488B5 :
    EInvalidOp : Invalid floating point operation
      $080488B5  main,  line 127 of gen_function.pasprogram

  I tried to make more elegant workarounds by doing dummy fp
  operations at the end of function calculation or TryExecute, to cause
  the exception, but it just looks like EInvalidOp is never cleared by
  try..except block.

  The only workaround seems to be to use Set8087CW to mask exceptions,
  and then compare with NaN to make proper TryExecute implementation. }
{$ifdef VER2_2_2}
  {$define WORKAROUND_EXCEPTIONS_FOR_SCRIPT_EXPRESSIONS}
{$endif}

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
    {$ifdef WORKAROUND_EXCEPTIONS_FOR_SCRIPT_EXPRESSIONS}
    {$I norqcheckbegin.inc}
    if (Result is TKamScriptFloat) and
       IsNan(TKamScriptFloat(Result).Value) then
      Result := nil;
    {$I norqcheckend.inc}
    {$endif}
  except
    Result := nil;
  end;
end;

{ TKamScriptExpressionsList -------------------------------------------------- }

procedure TKamScriptExpressionsList.FreeContentsByParentExpression;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Items[I].FreeByParentExpression;
    Items[I] := nil;
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

class procedure TKamScriptFloat.HandleAdd(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  { The function allows only >= 1 arguments, and this handler is
    registered only for TKamScriptFloat values, so we can safely take
    the first arg as TKamScriptFloat. }
  TKamScriptFloat(AResult).Value := TKamScriptFloat(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TKamScriptFloat(AResult).Value :=
      TKamScriptFloat(AResult).Value + TKamScriptFloat(Arguments[I]).Value;
end;

class procedure TKamScriptFloat.HandleSubtract(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := TKamScriptFloat(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TKamScriptFloat(AResult).Value :=
      TKamScriptFloat(AResult).Value - TKamScriptFloat(Arguments[I]).Value;
end;

class procedure TKamScriptFloat.HandleMultiply(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := TKamScriptFloat(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TKamScriptFloat(AResult).Value :=
      TKamScriptFloat(AResult).Value * TKamScriptFloat(Arguments[I]).Value;
end;

class procedure TKamScriptFloat.HandleDivide(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := TKamScriptFloat(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TKamScriptFloat(AResult).Value :=
      TKamScriptFloat(AResult).Value / TKamScriptFloat(Arguments[I]).Value;
end;

class procedure TKamScriptFloat.HandleNegate(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := - TKamScriptFloat(Arguments[0]).Value;
end;

class procedure TKamScriptFloat.HandleModulo(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value :=
    TKamScriptFloat(Arguments[0]).Value -
    Floor( TKamScriptFloat(Arguments[0]).Value /
           TKamScriptFloat(Arguments[1]).Value )
    * TKamScriptFloat(Arguments[1]).Value;
end;

class procedure TKamScriptFloat.HandleSin(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Sin( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleCos(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Cos( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleTan(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Tan( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleCotan(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := KamCoTan( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleArcSin(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := ArcSin( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleArcCos(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := ArcCos( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleArcTan(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := ArcTan( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleArcCotan(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := ArcCot( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleSinh(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := SinH( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleCosh(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := CosH( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleTanh(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := TanH( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleCotanh(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := 1 / TanH( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleLog2(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Log2( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleLn(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Ln( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleLog(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Logn( TKamScriptFloat(Arguments[0]).Value,
                                          TKamScriptFloat(Arguments[1]).Value );
end;

class procedure TKamScriptFloat.HandlePower2(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Power(2, TKamScriptFloat(Arguments[0]).Value);
end;

class procedure TKamScriptFloat.HandleExp(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Exp( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandlePower(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := GeneralPower(
    TKamScriptFloat(Arguments[0]).Value,
    TKamScriptFloat(Arguments[1]).Value );
end;

class procedure TKamScriptFloat.HandleSqr(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Sqr( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleSqrt(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Sqrt( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleSgn(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Sign( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleAbs(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Abs( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleCeil(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Ceil( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleFloor(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Floor( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleGreater(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).AsBoolean :=
    TKamScriptFloat(Arguments[0]).Value >
    TKamScriptFloat(Arguments[1]).Value;
end;

class procedure TKamScriptFloat.HandleLesser(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).AsBoolean :=
    TKamScriptFloat(Arguments[0]).Value <
    TKamScriptFloat(Arguments[1]).Value;
end;

class procedure TKamScriptFloat.HandleGreaterEq(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).AsBoolean :=
    TKamScriptFloat(Arguments[0]).Value >=
    TKamScriptFloat(Arguments[1]).Value;
end;

class procedure TKamScriptFloat.HandleLesserEq(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).AsBoolean :=
    TKamScriptFloat(Arguments[0]).Value <=
    TKamScriptFloat(Arguments[1]).Value;
end;

class procedure TKamScriptFloat.HandleEqual(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).AsBoolean :=
    TKamScriptFloat(Arguments[0]).Value =
    TKamScriptFloat(Arguments[1]).Value;
end;

class procedure TKamScriptFloat.HandleNotEqual(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).AsBoolean :=
    TKamScriptFloat(Arguments[0]).Value <>
    TKamScriptFloat(Arguments[1]).Value;
end;

class procedure TKamScriptFloat.HandleOr(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).AsBoolean :=
    TKamScriptFloat(Arguments[0]).AsBoolean or
    TKamScriptFloat(Arguments[1]).AsBoolean;
end;

class procedure TKamScriptFloat.HandleAnd(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).AsBoolean :=
    TKamScriptFloat(Arguments[0]).AsBoolean and
    TKamScriptFloat(Arguments[1]).AsBoolean;
end;

class procedure TKamScriptFloat.HandleNot(const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).AsBoolean :=
    not TKamScriptFloat(Arguments[0]).AsBoolean;
end;

procedure TKamScriptFloat.AssignValue(Source: TKamScriptValue);
begin
  if Source is TKamScriptFloat then
    Value := TKamScriptFloat(Source).Value else
    raise EKamAssignValueError.CreateFmt('Assignment from %s to %s not possible', [Source.ClassName, ClassName]);
end;

function TKamScriptFloat.GetAsBoolean: boolean;
begin
  Result := Value <> 0;
end;

procedure TKamScriptFloat.SetAsBoolean(const AValue: boolean);
const
  BoolToFloat: array [boolean] of Float = (0, 1);
begin
  Value := BoolToFloat[AValue];
end;

procedure TKamScriptFloat.SetValue(const AValue: Float);
begin
  FValue := AValue;
  ValueAssigned := true;
end;

{ TKamScriptFunction --------------------------------------------------------- }

constructor TKamScriptFunction.Create(AArgs: TKamScriptExpressionsList);
begin
  inherited Create;
  FArgs := TKamScriptExpressionsList.CreateFromList(AArgs);
  CheckArguments;
end;

constructor TKamScriptFunction.Create(const AArgs: array of TKamScriptExpression);
begin
  inherited Create;
  FArgs := TKamScriptExpressionsList.CreateFromArray(AArgs);
  CheckArguments;
end;

procedure TKamScriptFunction.CheckArguments;
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
begin
  if FArgs <> nil then
  begin
    FArgs.FreeContentsByParentExpression;
    FreeAndNil(FArgs);
  end;

  if ParentOfLastExecuteResult then
    LastExecuteResult.FreeByParentExpression;
  LastExecuteResult := nil;

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

    Handler.Handler(Arguments, LastExecuteResult, ParentOfLastExecuteResult);

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

{ TKamScriptSequence --------------------------------------------------------- }

class function TKamScriptSequence.AllowedArgumentsCount: Integer;
begin
  Result := -1;
end;

class function TKamScriptSequence.Name: string;
begin
  Result := 'sequence (;)';
end;

class function TKamScriptSequence.ShortName: string;
begin
  Result := '';
end;

class function TKamScriptSequence.InfixOperatorName: string;
begin
  Result := ';';
end;

class procedure TKamScriptSequence.HandleSequence(
  const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  if ParentOfResult then
    AResult.FreeByParentExpression else
    AResult := nil;

  AResult := Arguments[High(Arguments)];
  ParentOfResult := false;
end;

{ TKamScriptAssignment --------------------------------------------------------- }

class function TKamScriptAssignment.AllowedArgumentsCount: Integer;
begin
  Result := 2;
end;

class function TKamScriptAssignment.Name: string;
begin
  Result := 'assignment (:=)';
end;

class function TKamScriptAssignment.ShortName: string;
begin
  Result := '';
end;

class function TKamScriptAssignment.InfixOperatorName: string;
begin
  Result := ':=';
end;

class procedure TKamScriptAssignment.HandleAssignment(
  const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  if ParentOfResult then
    AResult.FreeByParentExpression else
    AResult := nil;

  (Arguments[0] as TKamScriptValue).AssignValue(Arguments[1]);

  AResult := Arguments[0] as TKamScriptValue;
  ParentOfResult := false;
end;

procedure TKamScriptAssignment.CheckArguments;
begin
  inherited;
  if not ( (Args[0] is TKamScriptValue) and
           (TKamScriptValue(Args[0]).Name <> '') ) then
    raise EKamScriptFunctionArgumentsError.Create('Left side of assignment expression is not a writeable operand');
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
    if ArgumentClass.InheritsFrom(Handler.ArgumentClass)  then
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

{ TKamScriptFunctionDefinition ----------------------------------------------- }

constructor TKamScriptFunctionDefinition.Create;
begin
  inherited;
  FParameters := TKamScriptValuesList.Create;
end;

destructor TKamScriptFunctionDefinition.Destroy;
begin
  if Body <> nil then
    Body.FreeByParentExpression;
  FreeWithContentsAndNil(FParameters);
  inherited;
end;

{ TKamScriptFunctionDefinitionsList ------------------------------------------ }

function TKamScriptFunctionDefinitionsList.IndexOf(
  const FunctionName: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if SameText(FunctionName, Items[Result].Name) then
      Exit;
  Result := -1;
end;

{ TKamScriptProgram ---------------------------------------------------------- }

constructor TKamScriptProgram.Create;
begin
  inherited;
  FFunctions := TKamScriptFunctionDefinitionsList.Create;
end;

destructor TKamScriptProgram.Destroy;
begin
  FreeWithContentsAndNil(FFunctions);
  inherited;
end;

    { Execute a user-defined function (from Functions list of this program).

      @unorderedList(
        @item(Looks for given FunctionName (exception EKamScriptMissingFunction
          if not found).)
        @item(Sets function parameters to given values
         (number of parameters must match, otherwise EKamScriptError).)
        @item(Finally executes function body (various EKamScriptError
          may be raised there).)
      )

      @raises EKamScriptError
    }
procedure TKamScriptProgram.ExecuteFunction(const FunctionName: string;
  const Parameters: array of Float;
  const IgnoreMissingFunction: boolean);
var
  Func: TKamScriptFunctionDefinition;
  FuncIndex, I: Integer;
begin
  FuncIndex := Functions.IndexOf(FunctionName);
  if FuncIndex = -1 then
  begin
    if IgnoreMissingFunction then
      Exit else
      raise EKamScriptMissingFunction.CreateFmt('KambiScript function "%s" is not defined', [FunctionName]);
  end;
  Func := Functions[FuncIndex];

  if High(Parameters) <> Func.Parameters.High then
    raise EKamScriptError.CreateFmt('KambiScript function "%s" requires %d parameters, but passed %d parameters',
      [FunctionName, Func.Parameters.Count, High(Parameters) + 1]);

  { TODO: this is directed at only TKamScriptFloat now, so
    Parameters are Float and below we just cast to TKamScriptFloat. }

  for I := 0 to High(Parameters) do
    (Func.Parameters[I] as TKamScriptFloat).Value := Parameters[I];

  Func.Body.Execute;
end;

{ procedural utils ----------------------------------------------------------- }

procedure CreateValueIfNeeded(var Value: TKamScriptValue;
  var ParentOfValue: boolean;
  NeededClass: TKamScriptValueClass);
begin
  if Value = nil then
  begin
    Value := NeededClass.Create;
    ParentOfValue := true;
  end else
  if Value.ClassType <> NeededClass then
  begin
    if ParentOfValue then
      Value.FreeByParentExpression else
      Value := nil;

    Value := NeededClass.Create;
    ParentOfValue := true;
  end;
end;

{ unit init/fini ------------------------------------------------------------- }

initialization
  {$ifdef WORKAROUND_EXCEPTIONS_FOR_SCRIPT_EXPRESSIONS}
  Set8087CW($133F);
  {$endif}

  FunctionHandlers := TKamScriptFunctionHandlers.Create;

  FunctionHandlers.RegisterHandler(@TKamScriptSequence(nil).HandleSequence, TKamScriptSequence, TKamScriptValue);
  FunctionHandlers.RegisterHandler(@TKamScriptAssignment(nil).HandleAssignment, TKamScriptAssignment, TKamScriptValue);

  { Register handlers for TKamScriptFloat for functions in
    KambiScriptMathFunctions. }
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleAdd, TKamScriptAdd, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleSubtract, TKamScriptSubtract, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleMultiply, TKamScriptMultiply, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleDivide, TKamScriptDivide, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleNegate, TKamScriptNegate, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleModulo, TKamScriptModulo, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleSin, TKamScriptSin, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleCos, TKamScriptCos, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleTan, TKamScriptTan, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleCotan, TKamScriptCotan, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleArcSin, TKamScriptArcSin, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleArcCos, TKamScriptArcCos, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleArcTan, TKamScriptArcTan, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleArcCotan, TKamScriptArcCotan, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleSinh, TKamScriptSinh, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleCosh, TKamScriptCosh, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleTanh, TKamScriptTanh, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleCotanh, TKamScriptCotanh, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleLog2, TKamScriptLog2, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleLn, TKamScriptLn, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleLog, TKamScriptLog, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandlePower2, TKamScriptPower2, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleExp, TKamScriptExp, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandlePower, TKamScriptPower, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleSqr, TKamScriptSqr, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleSqrt, TKamScriptSqrt, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleSgn, TKamScriptSgn, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleAbs, TKamScriptAbs, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleCeil, TKamScriptCeil, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleFloor, TKamScriptFloor, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleGreater, TKamScriptGreater, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleLesser, TKamScriptLesser, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleGreaterEq, TKamScriptGreaterEq, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleLesserEq, TKamScriptLesserEq, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleEqual, TKamScriptEqual, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleNotEqual, TKamScriptNotEqual, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleOr, TKamScriptOr, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleAnd, TKamScriptAnd, TKamScriptFloat);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleNot, TKamScriptNot, TKamScriptFloat);
finalization
  FreeAndNil(FunctionHandlers);
end.
