{
  Copyright 2001-2011 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
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
      TKamScriptSin.Create([TKamScriptFloat.Create(false, 3)]),
      TKamScriptFloat.Create(false, 10),
      TKamScriptFloat.Create(false, 1)
    ]);
#)

  You can then call @code(Expr.Execute) to calculate such expression.

  To make a variable in the expression, just create and remember a
  TKamScriptFloat instance first, and then change it's value freely between
  @code(Expr.Execute) calls. For example

@longcode(#
  MyVariable := TKamScriptFloat.Create(false, 3);
  Expr := TKamScriptAdd.Create([
      TKamScriptSin.Create([MyVariable]),
      TKamScriptFloat.Create(false, 10),
      TKamScriptFloat.Create(false, 1)
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

uses SysUtils, Math, Contnrs, KambiUtils, KambiClassUtils, Classes,
  FGL {$ifdef VER2_2}, FGLObjectList22 {$endif};

type
  { }
  TKamScriptValue = class;

  EKamScriptError = class(Exception);
  EKamAssignValueError = class(EKamScriptError);
  EKamScriptAnyMathError = class(EKamScriptError);

  TKamScriptOutputProc = procedure (const S: string) of object;

  { Various information that may be useful for implementing some
    function handlers, but that should be supplied from outside of
    KambiScript. }
  TKamScriptEnvironment = class
  private
    FWWWBasePath: string;
    FOutputProc: TKamScriptOutputProc;
  public
    { Base URL to use for relative filenames.
      Similar to TX3DNode.WWWBasePath. }
    property WWWBasePath: string read FWWWBasePath write FWWWBasePath;
    { If assigned, it will be used to realize writeln()
      function. If not assigned, we will use OnWarning. }
    property OutputProc: TKamScriptOutputProc read FOutputProc write FOutputProc;
  end;

  TKamScriptExpression = class
  private
    FEnvironment: TKamScriptEnvironment;
  protected
    { More internal version of Execute.

      This doesn't necessarily check floating-point exceptions.
      Execute actually calls CoreExecute and then ClearExceptions.

      Also this doesn't try to convert EIntError and EMathError
      to EKamScriptAnyMathError. This is done by Execute.

      When one KambiScript CoreExecute calls another function,
      it can use CoreExecute instead of Execute. This way only one
      ClearExceptions will be needed for whole expression execution,
      instead of doing ClearExceptions after each function handler. }
    function CoreExecute: TKamScriptValue; virtual; abstract;
  public
    (*Execute and calculate this expression.

      Returned value is owned by this object. Which should be comfortable
      for you usually, as you do not have to worry about freeing it.
      Also, it allows us to make various optimizations to avoid
      creating/destroying lots of temporary TKamScriptExpression
      instances during calculation of complex expression.

      The disadvantage of this is that returned object value is valid
      only until you executed this same expression again,
      or until you freed this expression. If you need to remember the
      execute result for longer, you have to copy it somewhere.
      For example you can do

@longCode(#
  { This will always work, thanks to virtual TKamScriptValue.Create
    and AssignValue methods. }
  Copy := TKamScriptValue(ReturnedValue.ClassType).Create;
  Copy.AssignValue(ReturnedValue);
#)

      @raises(EKamScriptError

        Execute is guaranteed to raise an EKamScriptError exception if some
        calculation fails because of invalid arguments.

        This means that when you run KambiScript expression provided
        by the user, you only have to catch EKamScriptError
        to be safe from errors produced by user.
        No need to catch something more general like Exception class.

        Also it's guaranteed that no hanging floating-point errors
        are left. Normally, there is no guarantee that
        floating-point errors are raised immediately, they may
        be raised at the next fp operation (this is needed for fp operations
        to proceed in parallel, and be much faster).
        For executing KambiScript, Execute calls Math.ClearExceptions(true)
        to make sure that all floating point errors are caught.
        This ensures that we can safely execute even invalid expressions
        (like 'ln(-3)') and get reliable exceptions.

        Floating-point errors of course also result in EKamScriptError descendants.
        More specifically, EIntError and EMathError result
        in EKamScriptAnyMathError.)
    *)
    function Execute: TKamScriptValue;

    { Try to execute expression, or return @nil if a mathematical error occurred
      within expression. "Math error within expression" means that
      a EKamScriptAnyMathError exception occurred while calculating expression.

      This is useful to secure you against math arguments errors ('ln(-3)',
      'sqrt(-3)') but still raises normal exception on other EKamScriptError
      errors (like invalid argument type for function). }
    function TryExecuteMath: TKamScriptValue;

    { Call Free, but only if this is not TKamScriptValue with
      OwnedByParentExpression = false. (This cannot be implemented
      cleanly, as virtual procedure, since it must work when Self is @nil,
      and then virtual method table is not available of course.) }
    procedure FreeByParentExpression;

    { Environment (outside information) for this expression.
      May be @nil. This object is not owned by TKamScriptExpression,
      will not be freed by TKamScriptExpression and such. }
    property Environment: TKamScriptEnvironment read FEnvironment write FEnvironment;
  end;

  TKamScriptExpressionList = class(specialize TFPGObjectList<TKamScriptExpression>)
  public
    procedure AddArray(const A: array of TKamScriptExpression);
    procedure AddList(const Source: TKamScriptExpressionList);
    procedure FreeContentsByParentExpression;
  end;

  TKamScriptValue = class(TKamScriptExpression)
  private
    FOwnedByParentExpression: boolean;
    FName: string;
    FValueAssigned: boolean;
    FWriteable: boolean;
  protected
    function CoreExecute: TKamScriptValue; override;
  public
    constructor Create(const AWriteable: boolean); virtual;

    { Is this value writeable.
      If not, this will not be allowed to change by KambiScript assignment
      and such functions. Note that Writeable = @false will not prevent
      you from changing value internally, by AssignValue or changin
      Value property directly (that would be too uncomfortable). }
    property Writeable: boolean read FWriteable write FWriteable;

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
  TKamScriptValueClassArray = array of TKamScriptValueClass;

  TKamScriptValueList = class(specialize TFPGObjectList<TKamScriptValue>)
  public
    procedure AddArray(const A: array of TKamScriptValue);
  end;

  { This is a very special KambiScript value, used to represent user-defined
    function parameter. This poses itself as a TKamScriptValue descendant,
    and it has working AssignValue and everything else. This way it can
    be used in "Variables" list for various KambiScriptParser functions.

    Except it's cheating: it doesn't actually store the value.
    Instead, it has SourceValue property that is used when doing
    AssignValue. So AssignValue is handled by SourceValue.AssignValue,
    and Execute is handled by SourceValue.Execute, and so reading/writing
    this works.

    The advantage: the exact type of function parameter is not known,
    and still we can parse the function expression. This is crucial
    for parser implementation: when parsing you need to create
    TKamScriptParameterValue instance, but you don't know actual
    type of parameter that will be passed here. }
  TKamScriptParameterValue = class(TKamScriptValue)
  private
    FSourceValue: TKamScriptValue;
  protected
    function CoreExecute: TKamScriptValue; override;
  public
    property SourceValue: TKamScriptValue read FSourceValue write FSourceValue;
    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptFloat = class;
  TKamScriptFunction = class;

  TKamScriptInteger = class(TKamScriptValue)
  private
    class procedure HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleSubtract(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleNegate(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleMultiply(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleDivide(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleModulo(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandlePower(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleMax(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleMin(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleSqr(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleSgn(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleAbs(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleRandom(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleGreater(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleLesser(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleGreaterEq(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleLesserEq(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleNotEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure ConvertFromInt(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure ConvertFromFloat(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure ConvertFromBool(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure ConvertFromString(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

  private
    FPromoteToFloat: TKamScriptFloat;

    FValue: Int64;
    procedure SetValue(const AValue: Int64);
  public
    { Comfortable constructor to set initial Value.
      Note that the inherited constructor (without AValue parameter)
      is also fine to use, it will set value to zero. }
    constructor Create(const AWriteable: boolean; const AValue: Int64);
    constructor Create(const AWriteable: boolean); override;
    destructor Destroy; override;

    property Value: Int64 read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;

    { Returns this integer promoted to float.

      This object is kept and owned by this TKamScriptInteger instance,
      so it's valid as long as this TKamScriptInteger instance is valid.
      This allows you to safely use this (since you may have to return
      PromoteToFloat as return value of some Execute expressions,
      so it desirable that it's valid object reference). }
    function PromoteToFloat: TKamScriptFloat;
  end;

  TKamScriptFloat = class(TKamScriptValue)
  private
    class procedure HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleSubtract(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleMultiply(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleDivide(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleNegate(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleModulo(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleSin(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleCos(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleTan(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleCotan(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArcSin(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArcCos(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArcTan(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleArcCotan(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleSinh(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleCosh(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleTanh(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleCotanh(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleLog2(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleLn(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleLog(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandlePower2(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleExp(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandlePower(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleSqr(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleSqrt(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleMax(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleMin(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleSgn(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleAbs(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleRandom(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleCeil(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleFloor(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleRound(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleGreater(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleLesser(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleGreaterEq(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleLesserEq(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleNotEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure ConvertFromInt(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure ConvertFromFloat(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure ConvertFromBool(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure ConvertFromString(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

  private
    FValue: Float;
    procedure SetValue(const AValue: Float);
  public
    { Comfortable constructor to set initial Value.
      Note that the inherited constructor (without AValue parameter)
      is also fine to use, it will set value to zero. }
    constructor Create(const AWriteable: boolean; const AValue: Float);
    constructor Create(const AWriteable: boolean); override;

    property Value: Float read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptBoolean = class(TKamScriptValue)
  private
    class procedure HandleOr(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleAnd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleNot(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleGreater(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleLesser(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleGreaterEq(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleLesserEq(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleNotEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure ConvertFromInt(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure ConvertFromFloat(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure ConvertFromBool(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure ConvertFromString(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

  private
    FValue: boolean;
    procedure SetValue(const AValue: boolean);
  public
    { Comfortable constructor to set initial Value.
      Note that the inherited constructor (without AValue parameter)
      is also fine to use, it will set value to false. }
    constructor Create(const AWriteable: boolean; const AValue: boolean);
    constructor Create(const AWriteable: boolean); override;

    property Value: boolean read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptString = class(TKamScriptValue)
  private
    class procedure HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleGreater(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleLesser(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleGreaterEq(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleLesserEq(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleNotEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure ConvertFromInt(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure ConvertFromFloat(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure ConvertFromBool(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure ConvertFromString(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleWriteln(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleCharacterFromCode(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
  private
    FValue: string;
    procedure SetValue(const AValue: string);
  public
    { Comfortable constructor to set initial Value.
      Note that the inherited constructor (without AValue parameter)
      is also fine to use, it will set value to ''. }
    constructor Create(const AWriteable: boolean; const AValue: string);
    constructor Create(const AWriteable: boolean); override;

    property Value: string read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptRegisteredHandler = class;

  TKamScriptSearchArgumentClassesCache = record
    IsCache: boolean;
    QueryHandlersByArgument: TObjectList;
    QueryArgumentClasses: TKamScriptValueClassArray;
    Answer: boolean;
    AnswerArgumentIndex: Integer;
    AnswerHandler: TKamScriptRegisteredHandler;
  end;

  TKamScriptFunction = class(TKamScriptExpression)
  private
    FArgs: TKamScriptExpressionList;
    LastExecuteResult: TKamScriptValue;
    ParentOfLastExecuteResult: boolean;

    { This is as returned by SearchFunctionClass }
    HandlersByArgument: TObjectList;

    { Helper variables for Execute implementation.
      Initialized in CheckArguments, to optimize: profiling shows that when
      they are intialized in Execute, this takes quite a lot of Execute time. }
    ExecuteArguments: array of TKamScriptValue;
    ExecuteArgumentClasses: TKamScriptValueClassArray;

    { Caches for SearchArgumentClasses, used to speed up Execute }
    Cache1: TKamScriptSearchArgumentClassesCache;
    Cache2: TKamScriptSearchArgumentClassesCache;
  protected
    { Used by constructor to check are args valid.
      Also, right now this gets FunctionHandlersByArgument (this way we don't
      have to search it at each TKamScriptFunction.Execute call,
      so TKamScriptFunction.Execute may work much faster).
      @raises(EKamScriptFunctionArgumentsError on invalid Args passed to
      function.) }
    procedure CheckArguments; virtual;

    function CoreExecute: TKamScriptValue; override;
  public
    { Constructor initializing Args from given TKamScriptExpressionList.
      AArgs list contents is copied, i.e. AArgs refence is not
      stored or freed by TKamScriptFunction. But items on AArags are not copied
      recursively, we copy references from AArags items, and so we become
      their owners.

      @raises(EKamScriptFunctionArgumentsError if you specified invalid
        number of arguments for this function.)
    }
    constructor Create(AArgs: TKamScriptExpressionList); overload;
    constructor Create(const AArgs: array of TKamScriptExpression); overload;
    destructor Destroy; override;

    { Long function name for user. This is possibly with spaces,
      parenthesis and other funny characters. It will be used in
      error messages and such to describe this function.

      Default implementation in this class simply returns ShortName.
      This should be suitable for most "norma" functions. }
    class function Name: string; virtual;

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

    { Specify which arguments are calculated before function handler
      is called.

      If = -1 (default value returned by implementation
      in this class) then all arguments are greedily calculated,
      which simply means that all arguments are calculated before
      executing function handler. This is the usual and expected
      behavior of normal functions. It's also a prerequisite
      for most of overloaded things to work, since we need to know
      types of @italic(calculated) arguments (TKamScriptValue classes)
      before we choose overloaded handler for function.

      If this is >= 0, then arguments with index >= of this will not
      be calculated before handler execution.
      Since their type is unknown, they will match any type in
      handler's ArgumentClasses.
      Your handler will receive @nil in their places, and is responsible
      for calling their Execute on it's own if needed.

      This is particularly suited for implementing control-flow
      instructions, like "if" and "while", as normal functions
      inside KambiScript. For example, "if" will have
      GreedyArgumentsCalculation = 1, so the first argument (condition)
      will be calculated, but the execution of 2nd or 3rd argument
      ("then" code or "else" code) will be left to the handler. }
    class function GreedyArgumentsCalculation: Integer; virtual;

    { Which arguments should be assignable by this function.

      Default implementation in TKamScriptFunction just returns @false
      always. If you're making a function that changes it's argument
      (like assignment operator, or vector_set, array_set and such)
      you want to override this.

      This is actually checked by CheckArguments, called from
      constructors. }
    class function ArgumentMustBeAssignable(const Index: Integer): boolean; virtual;

    { Function arguments. Don't modify this list after function is created
      (although you can modify values inside arguments). }
    property Args: TKamScriptExpressionList read FArgs;
  end;

  TKamScriptFunctionClass = class of TKamScriptFunction;

  { Calculate result on given function arguments Arguments.
    Place result in AResult.

    The current function is also passed here, although usually you don't need it
    (you already get a list of calculated Arguments, and you should
    register different procedures for different TKamScriptFunction classes,
    so you know what operation on arguments should be done).
    For functions when GreedyArgumentsCalculation >= 0, it may be useful
    to directly access AFunction.Args.

    If needed, previous value of AResult should be freed and new created.
    If current AResult is <> nil and it's of appropriate class,
    you may also reuse it and only change it's fields
    (this is helpful, to avoid many creations/destroying
    of class instances while calculating an expression many times).
    CreateValueIfNeeded may be helpful for implementing this. }
  TKamScriptFunctionHandler = procedure (
    AFunction: TKamScriptFunction;
    const Arguments: array of TKamScriptValue;
    var AResult: TKamScriptValue;
    var ParentOfResult: boolean) of object;

  TKamScriptSequence = class(TKamScriptFunction)
  private
    class procedure HandleSequence(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
  public
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
  end;

  { KambiScript assignment operator. This is a special function,
    that must have TKamScriptValue (with Writeable = true) as it's 1st argument. }
  TKamScriptAssignment = class(TKamScriptFunction)
  private
    class procedure HandleAssignment(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
  public
    class function Name: string; override;
    class function ShortName: string; override;
    class function InfixOperatorName: string; override;
    class function ArgumentMustBeAssignable(const Index: Integer): boolean; override;
  end;

  TKamScriptIf = class(TKamScriptFunction)
  private
    class procedure HandleIf(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
  public
    class function ShortName: string; override;
    class function GreedyArgumentsCalculation: Integer; override;
  end;

  TKamScriptWhen = class(TKamScriptFunction)
  private
    class procedure HandleWhen(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
  public
    class function ShortName: string; override;
    class function GreedyArgumentsCalculation: Integer; override;
  end;

  TKamScriptWhile = class(TKamScriptFunction)
  private
    class procedure HandleWhile(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
  public
    class function ShortName: string; override;
    class function GreedyArgumentsCalculation: Integer; override;
  end;

  TKamScriptFor = class(TKamScriptFunction)
  private
    class procedure HandleFor(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
  public
    class function ShortName: string; override;
    class function GreedyArgumentsCalculation: Integer; override;
    class function ArgumentMustBeAssignable(const Index: Integer): boolean; override;
  end;

  TKamScriptRegisteredHandler = class
  private
    FHandler: TKamScriptFunctionHandler;
    FFunctionClass: TKamScriptFunctionClass;
    FArgumentClasses: TKamScriptValueClassArray;
    FVariableArgumentsCount: boolean;
  public
    constructor Create(
      AHandler: TKamScriptFunctionHandler;
      AFunctionClass: TKamScriptFunctionClass;
      const AArgumentClasses: TKamScriptValueClassArray;
      const AVariableArgumentsCount: boolean);
    property Handler: TKamScriptFunctionHandler read FHandler;
    property FunctionClass: TKamScriptFunctionClass read FFunctionClass;
    property ArgumentClasses: TKamScriptValueClassArray read FArgumentClasses;

    { Is the handler able to receive any number of arguments.

      If yes, then the last argument class
      may be repeated any number of times (but must occur
      at least once). That is, the ArgumentClasses array
      dictates the required arguments, and more arguments are allowed.
      Note that this means that at least one argument
      must be allowed (we have to know the argument class that can
      be repeated at the end), otherwise the handler will not be able to receive
      variable number of arguments anyway. }
    property VariableArgumentsCount: boolean read FVariableArgumentsCount;
  end;

  { This specifies for each type combination (array of TKamScriptValue classes)
    and for each function (TKamScriptFunction class) how they should
    be handled. You can think of this as a table that has a handler
    for each possible TKamScriptValue sequence and TKamScriptFunction
    combination.

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

    You have a guarantee that every registered here Handler will be called
    only with AFunction of registstered type and all Arguments
    matching the array of registered types and satisfying
    VariableArgumentsCount setting.

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

    function SearchArgumentClasses(
      HandlersByArgument: TObjectList;
      const ArgumentClasses: TKamScriptValueClassArray;
      out ArgumentIndex: Integer;
      out Handler: TKamScriptRegisteredHandler): boolean; overload;
    function SearchArgumentClasses(
      HandlersByArgument: TObjectList;
      const ArgumentClasses: TKamScriptValueClassArray;
      out Handler: TKamScriptRegisteredHandler): boolean; overload;

    { This uses Cache to speed up SearchArgumentClasses.
      The cache remembers last HandlersByArgument, ArgumentClasses,
      and answer for them (if @true). Use this if you suspect that
      SearchArgumentClasses will be called many times with the same
      HandlersByArgument, ArgumentClasses --- then this will use cache
      to give answer much faster. }
    function SearchArgumentClasses(
      HandlersByArgument: TObjectList;
      const ArgumentClasses: TKamScriptValueClassArray;
      out Handler: TKamScriptRegisteredHandler;
      var Cache: TKamScriptSearchArgumentClassesCache): boolean; overload;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterHandler(
      AHandler: TKamScriptFunctionHandler;
      AFunctionClass: TKamScriptFunctionClass;
      const AArgumentClasses: array of TKamScriptValueClass;
      const AVariableArgumentsCount: boolean);

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
    FParameters: TKamScriptValueList;
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
      OwnedByParentExpression) by this class.

      They must always be of TKamScriptParameterValue class. }
    property Parameters: TKamScriptValueList read FParameters;

    { Function body. }
    property Body: TKamScriptExpression read FBody write FBody;
  end;

  TKamScriptFunctionDefinitionList = class(specialize TFPGObjectList<TKamScriptFunctionDefinition>)
    function IndexOf(const FunctionName: string): Integer;
  end;

  EKamScriptMissingFunction = class(EKamScriptError);

  TKamScriptProgram = class
  private
    FFunctions: TKamScriptFunctionDefinitionList;
    FEnvironment: TKamScriptEnvironment;
  public
    constructor Create;
    destructor Destroy; override;

    property Functions: TKamScriptFunctionDefinitionList read FFunctions;

    { Execute a user-defined function (from Functions list of this program).

      @unorderedList(
        @item(Looks for given FunctionName.

          IgnoreMissingFunction says what to do in case of missing function:
          if true, it will be simply ignored (ExecuteFunction will
          silently do nothng). If false (default)
          then we will raise exception EKamScriptMissingFunction.)

        @item(Sets function parameters to given values
         (number of parameters must match, otherwise EKamScriptError).
         Values from your parameters are set as our parameters
         TKamScriptParameterValue.SourceValue, so script can read and write
         your values.)

        @item(Finally executes function body.)
      )
    }
    procedure ExecuteFunction(const FunctionName: string;
      const Parameters: array of TKamScriptValue;
      const IgnoreMissingFunction: boolean = false);

    { Environment (outside information) for this program.

      This will be created and is owned by this TKamScriptProgram instance.
      You should take care to copy this reference to all expressions
      within this program (for example ParseProgram does this),
      this way all expressions share the same Environment instance. }
    property Environment: TKamScriptEnvironment read FEnvironment write FEnvironment;
  end;

var
  FunctionHandlers: TKamScriptFunctionHandlers;

{ Make sure Value is assigned and of NeededClass.
  If Value is not assigned, or is not exactly of NeededClass,
  it will be freed and new will be created. }
procedure CreateValueIfNeeded(var Value: TKamScriptValue;
  var ParentOfValue: boolean;
  NeededClass: TKamScriptValueClass);

implementation

uses KambiScriptCoreFunctions, KambiWarnings;

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
      $080488B5  main,  line 127 of gen_function.lpr

  I tried to make more elegant workarounds by doing dummy fp
  operations at the end of function calculation or Execute, to cause
  the exception, but it just looks like EInvalidOp is never cleared by
  try..except block.

  The only workaround seems to be to use Set8087CW to mask exceptions,
  and then compare with NaN to make proper Execute implementation.

  Tests show that FPC 2.2.3 (fixes_2_2) also has this bug, and so 2.2.4
  will probably have this too...

  Also 2.3.1 (current trunk on 2009-03-03) still has it...
  It's easier to just define it always, for now. }
{$define WORKAROUND_EXCEPTIONS_FOR_SCRIPT_EXPRESSIONS}

{ TKamScriptExpression ------------------------------------------------------- }

procedure TKamScriptExpression.FreeByParentExpression;
begin
  if (Self <> nil) and
      ( (not (Self is TKamScriptValue)) or
        TKamScriptValue(Self).OwnedByParentExpression ) then
    Free;
end;

function TKamScriptExpression.Execute: TKamScriptValue;
begin
  try
    Result := CoreExecute;

    { Force raising pending exceptions by FP calculations }
    ClearExceptions(true);
  except
    { Convert EIntError and EMathError to EKamScriptAnyMathError }
    on E: EIntError do
      raise EKamScriptAnyMathError.CreateFmt('Integer error %s: %s',
        [E.ClassName, E.Message]);
    on E: EMathError do
      raise EKamScriptAnyMathError.CreateFmt('Math error %s: %s',
        [E.ClassName, E.Message]);
  end;

  {$ifdef WORKAROUND_EXCEPTIONS_FOR_SCRIPT_EXPRESSIONS}
  {$I norqcheckbegin.inc}
  if (Result is TKamScriptFloat) and
     ( IsNan(TKamScriptFloat(Result).Value) or
       IsInfinite(TKamScriptFloat(Result).Value) ) then
    raise EKamScriptAnyMathError.Create('Floating point error');
  {$I norqcheckend.inc}
  {$endif}
end;

function TKamScriptExpression.TryExecuteMath: TKamScriptValue;
begin
  try
    Result := Execute;
  except
    on EKamScriptAnyMathError do
      Result := nil;
  end;
end;

{ TKamScriptExpressionList -------------------------------------------------- }

procedure TKamScriptExpressionList.AddArray(const A: array of TKamScriptExpression);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], List^[OldCount], SizeOf(Pointer) * (High(A) + 1));
end;

procedure TKamScriptExpressionList.AddList(const Source: TKamScriptExpressionList);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  if Source.Count <> 0 then
    System.Move(Source.List^[0], List^[OldCount], SizeOf(Pointer) * Source.Count);
end;

procedure TKamScriptExpressionList.FreeContentsByParentExpression;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Items[I].FreeByParentExpression;
    FPGObjectList_NilItem(Self, I);
  end;
end;

{ TKamScriptValue ------------------------------------------------------------ }

constructor TKamScriptValue.Create(const AWriteable: boolean);
begin
  inherited Create;
  FOwnedByParentExpression := true;
  FWriteable := AWriteable;
end;

function TKamScriptValue.CoreExecute: TKamScriptValue;
begin
  { Since we own Execute result, we can simply return self here. }
  Result := Self;
end;

{ TKamScriptValueList ------------------------------------------------------- }

procedure TKamScriptValueList.AddArray(const A: array of TKamScriptValue);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], List^[OldCount], SizeOf(Pointer) * (High(A) + 1));
end;

{ TKamScriptParameterValue --------------------------------------------------- }

function TKamScriptParameterValue.CoreExecute: TKamScriptValue;
begin
  Result := SourceValue.CoreExecute;
end;

procedure TKamScriptParameterValue.AssignValue(Source: TKamScriptValue);
begin
  SourceValue.AssignValue(Source);
end;

{ TKamScriptInteger ---------------------------------------------------------- }

constructor TKamScriptInteger.Create(const AWriteable: boolean; const AValue: Int64);
begin
  Create(AWriteable);
  Value := AValue;
end;

constructor TKamScriptInteger.Create(const AWriteable: boolean);
begin
  inherited Create(AWriteable);
end;

destructor TKamScriptInteger.Destroy;
begin
  FPromoteToFloat.FreeByParentExpression;
  inherited;
end;

function TKamScriptInteger.PromoteToFloat: TKamScriptFloat;
begin
  if FPromoteToFloat = nil then
    FPromoteToFloat := TKamScriptFloat.Create(false, Value) else
    FPromoteToFloat.Value := Value;
  Result := FPromoteToFloat;
end;

class procedure TKamScriptInteger.HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptInteger);
  { The function allows only >= 1 arguments, and this handler is
    registered only for TKamScriptInteger values, so we can safely take
    the first arg as TKamScriptInteger. }
  TKamScriptInteger(AResult).Value := TKamScriptInteger(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TKamScriptInteger(AResult).Value :=
      TKamScriptInteger(AResult).Value + TKamScriptInteger(Arguments[I]).Value;
end;

class procedure TKamScriptInteger.HandleSubtract(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptInteger);
  TKamScriptInteger(AResult).Value := TKamScriptInteger(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TKamScriptInteger(AResult).Value :=
      TKamScriptInteger(AResult).Value - TKamScriptInteger(Arguments[I]).Value;
end;

class procedure TKamScriptInteger.HandleMultiply(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptInteger);
  TKamScriptInteger(AResult).Value := TKamScriptInteger(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TKamScriptInteger(AResult).Value :=
      TKamScriptInteger(AResult).Value * TKamScriptInteger(Arguments[I]).Value;
end;

class procedure TKamScriptInteger.HandleDivide(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptInteger);
  TKamScriptInteger(AResult).Value := TKamScriptInteger(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TKamScriptInteger(AResult).Value :=
      TKamScriptInteger(AResult).Value div TKamScriptInteger(Arguments[I]).Value;
end;

class procedure TKamScriptInteger.HandleNegate(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptInteger);
  TKamScriptInteger(AResult).Value := - TKamScriptInteger(Arguments[0]).Value;
end;

class procedure TKamScriptInteger.HandleModulo(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptInteger);
  TKamScriptInteger(AResult).Value :=
    TKamScriptInteger(Arguments[0]).Value mod
    TKamScriptInteger(Arguments[1]).Value;
end;

class procedure TKamScriptInteger.HandlePower(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptInteger);
  if (TKamScriptInteger(Arguments[0]).Value < 0) or
     (TKamScriptInteger(Arguments[1]).Value < 0) then
    raise EKamScriptError.Create('Power function on integer operands expects both arguments to be >= 0');

  TKamScriptInteger(AResult).Value := NatNatPower(
    TKamScriptInteger(Arguments[0]).Value,
    TKamScriptInteger(Arguments[1]).Value );
end;

class procedure TKamScriptInteger.HandleSqr(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptInteger);
  TKamScriptInteger(AResult).Value := Sqr( TKamScriptInteger(Arguments[0]).Value );
end;

class procedure TKamScriptInteger.HandleMax(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptInteger);
  TKamScriptInteger(AResult).Value := TKamScriptInteger(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TKamScriptInteger(AResult).Value := Max(
      TKamScriptInteger(AResult).Value, TKamScriptInteger(Arguments[I]).Value);
end;

class procedure TKamScriptInteger.HandleMin(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptInteger);
  TKamScriptInteger(AResult).Value := TKamScriptInteger(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TKamScriptInteger(AResult).Value := Min(
      TKamScriptInteger(AResult).Value, TKamScriptInteger(Arguments[I]).Value);
end;

class procedure TKamScriptInteger.HandleSgn(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptInteger);
  TKamScriptInteger(AResult).Value := Sign( TKamScriptInteger(Arguments[0]).Value );
end;

class procedure TKamScriptInteger.HandleAbs(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptInteger);
  TKamScriptInteger(AResult).Value := Abs( TKamScriptInteger(Arguments[0]).Value );
end;

class procedure TKamScriptInteger.HandleRandom(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptInteger);
  TKamScriptInteger(AResult).Value := Random( TKamScriptInteger(Arguments[0]).Value );
end;

class procedure TKamScriptInteger.HandleGreater(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value :=
    TKamScriptInteger(Arguments[0]).Value >
    TKamScriptInteger(Arguments[1]).Value;
end;

class procedure TKamScriptInteger.HandleLesser(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value :=
    TKamScriptInteger(Arguments[0]).Value <
    TKamScriptInteger(Arguments[1]).Value;
end;

class procedure TKamScriptInteger.HandleGreaterEq(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value :=
    TKamScriptInteger(Arguments[0]).Value >=
    TKamScriptInteger(Arguments[1]).Value;
end;

class procedure TKamScriptInteger.HandleLesserEq(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value :=
    TKamScriptInteger(Arguments[0]).Value <=
    TKamScriptInteger(Arguments[1]).Value;
end;

class procedure TKamScriptInteger.HandleEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value :=
    TKamScriptInteger(Arguments[0]).Value =
    TKamScriptInteger(Arguments[1]).Value;
end;

class procedure TKamScriptInteger.HandleNotEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value :=
    TKamScriptInteger(Arguments[0]).Value <>
    TKamScriptInteger(Arguments[1]).Value;
end;

class procedure TKamScriptInteger.ConvertFromInt(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  if ParentOfResult then
    AResult.FreeByParentExpression else
    AResult := nil;

  AResult := Arguments[0];
  Assert(AResult is TKamScriptInteger);
  ParentOfResult := false;
end;

class procedure TKamScriptInteger.ConvertFromFloat(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  F: Float;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptInteger);
  { Can't use Int function, as it returns float value }

  F := TKamScriptFloat(Arguments[0]).Value;
  if F >= 0 then
    TKamScriptInteger(AResult).Value := Floor(F) else
    TKamScriptInteger(AResult).Value := Ceil(F);
end;

class procedure TKamScriptInteger.ConvertFromBool(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  BoolTo01: array [boolean] of Int64 = (0, 1);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptInteger);
  TKamScriptInteger(AResult).Value := BoolTo01[TKamScriptBoolean(Arguments[0]).Value];
end;

class procedure TKamScriptInteger.ConvertFromString(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptInteger);
  try
    TKamScriptInteger(AResult).Value := StrToInt64(TKamScriptString(Arguments[0]).Value);
  except
    on E: EConvertError do
      { Change EConvertError to EKamScriptError }
      raise EKamScriptError.CreateFmt('Error when converting string "%s" to integer: %s',
        [TKamScriptString(Arguments[0]).Value, E.Message]);
  end;
end;

procedure TKamScriptInteger.AssignValue(Source: TKamScriptValue);
begin
  if Source is TKamScriptInteger then
    Value := TKamScriptInteger(Source).Value else
    raise EKamAssignValueError.CreateFmt('Assignment from %s to %s not possible', [Source.ClassName, ClassName]);
end;

procedure TKamScriptInteger.SetValue(const AValue: Int64);
begin
  FValue := AValue;
  ValueAssigned := true;
end;

{ TKamScriptFloat ------------------------------------------------------- }

constructor TKamScriptFloat.Create(const AWriteable: boolean; const AValue: Float);
begin
  Create(AWriteable);
  Value := AValue;
end;

constructor TKamScriptFloat.Create(const AWriteable: boolean);
begin
  inherited Create(AWriteable);
end;

class procedure TKamScriptFloat.HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
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

class procedure TKamScriptFloat.HandleSubtract(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := TKamScriptFloat(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TKamScriptFloat(AResult).Value :=
      TKamScriptFloat(AResult).Value - TKamScriptFloat(Arguments[I]).Value;
end;

class procedure TKamScriptFloat.HandleMultiply(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := TKamScriptFloat(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TKamScriptFloat(AResult).Value :=
      TKamScriptFloat(AResult).Value * TKamScriptFloat(Arguments[I]).Value;
end;

class procedure TKamScriptFloat.HandleDivide(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := TKamScriptFloat(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TKamScriptFloat(AResult).Value :=
      TKamScriptFloat(AResult).Value / TKamScriptFloat(Arguments[I]).Value;
end;

class procedure TKamScriptFloat.HandleNegate(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := - TKamScriptFloat(Arguments[0]).Value;
end;

class procedure TKamScriptFloat.HandleModulo(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value :=
    TKamScriptFloat(Arguments[0]).Value -
    Floor( TKamScriptFloat(Arguments[0]).Value /
           TKamScriptFloat(Arguments[1]).Value )
    * TKamScriptFloat(Arguments[1]).Value;
end;

class procedure TKamScriptFloat.HandleSin(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Sin( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleCos(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Cos( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleTan(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Tan( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleCotan(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := KamCoTan( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleArcSin(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := ArcSin( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleArcCos(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := ArcCos( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleArcTan(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := ArcTan( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleArcCotan(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := ArcCot( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleSinh(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := SinH( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleCosh(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := CosH( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleTanh(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := TanH( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleCotanh(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := 1 / TanH( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleLog2(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Log2( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleLn(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Ln( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleLog(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Logn( TKamScriptFloat(Arguments[0]).Value,
                                          TKamScriptFloat(Arguments[1]).Value );
end;

class procedure TKamScriptFloat.HandlePower2(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Power(2, TKamScriptFloat(Arguments[0]).Value);
end;

class procedure TKamScriptFloat.HandleExp(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Exp( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandlePower(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Power(
    TKamScriptFloat(Arguments[0]).Value,
    TKamScriptFloat(Arguments[1]).Value );
end;

class procedure TKamScriptFloat.HandleSqr(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Sqr( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleSqrt(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Sqrt( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleMax(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := TKamScriptFloat(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TKamScriptFloat(AResult).Value := Max(
      TKamScriptFloat(AResult).Value, TKamScriptFloat(Arguments[I]).Value);
end;

class procedure TKamScriptFloat.HandleMin(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := TKamScriptFloat(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TKamScriptFloat(AResult).Value := Min(
      TKamScriptFloat(AResult).Value, TKamScriptFloat(Arguments[I]).Value);
end;

class procedure TKamScriptFloat.HandleSgn(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptInteger);
  TKamScriptInteger(AResult).Value := Sign( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleAbs(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Abs( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleRandom(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := Random();
end;

class procedure TKamScriptFloat.HandleCeil(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptInteger);
  TKamScriptInteger(AResult).Value := Ceil( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleFloor(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptInteger);
  TKamScriptInteger(AResult).Value := Floor( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleRound(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptInteger);
  TKamScriptInteger(AResult).Value := Round( TKamScriptFloat(Arguments[0]).Value );
end;

class procedure TKamScriptFloat.HandleGreater(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value :=
    TKamScriptFloat(Arguments[0]).Value >
    TKamScriptFloat(Arguments[1]).Value;
end;

class procedure TKamScriptFloat.HandleLesser(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value :=
    TKamScriptFloat(Arguments[0]).Value <
    TKamScriptFloat(Arguments[1]).Value;
end;

class procedure TKamScriptFloat.HandleGreaterEq(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value :=
    TKamScriptFloat(Arguments[0]).Value >=
    TKamScriptFloat(Arguments[1]).Value;
end;

class procedure TKamScriptFloat.HandleLesserEq(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value :=
    TKamScriptFloat(Arguments[0]).Value <=
    TKamScriptFloat(Arguments[1]).Value;
end;

class procedure TKamScriptFloat.HandleEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value :=
    TKamScriptFloat(Arguments[0]).Value =
    TKamScriptFloat(Arguments[1]).Value;
end;

class procedure TKamScriptFloat.HandleNotEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value :=
    TKamScriptFloat(Arguments[0]).Value <>
    TKamScriptFloat(Arguments[1]).Value;
end;

class procedure TKamScriptFloat.ConvertFromInt(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := TKamScriptInteger(Arguments[0]).Value;
end;

class procedure TKamScriptFloat.ConvertFromFloat(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  if ParentOfResult then
    AResult.FreeByParentExpression else
    AResult := nil;

  AResult := Arguments[0];
  Assert(AResult is TKamScriptFloat);
  ParentOfResult := false;
end;

class procedure TKamScriptFloat.ConvertFromBool(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  BoolTo01: array [boolean] of Float = (0, 1);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value := BoolTo01[TKamScriptBoolean(Arguments[0]).Value];
end;

class procedure TKamScriptFloat.ConvertFromString(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  try
    TKamScriptFloat(AResult).Value := StrToFloat(TKamScriptString(Arguments[0]).Value);
  except
    on E: EConvertError do
      { Change EConvertError to EKamScriptError }
      raise EKamScriptError.CreateFmt('Error when converting string "%s" to float: %s',
        [TKamScriptString(Arguments[0]).Value, E.Message]);
  end;
end;

procedure TKamScriptFloat.AssignValue(Source: TKamScriptValue);
begin
  if Source is TKamScriptFloat then
    Value := TKamScriptFloat(Source).Value else
  { This allows for type promotion integer->float at assignment. }
  if Source is TKamScriptInteger then
    Value := TKamScriptInteger(Source).Value else
    raise EKamAssignValueError.CreateFmt('Assignment from %s to %s not possible', [Source.ClassName, ClassName]);
end;

procedure TKamScriptFloat.SetValue(const AValue: Float);
begin
  FValue := AValue;
  ValueAssigned := true;
end;

{ TKamScriptBoolean ---------------------------------------------------------- }

constructor TKamScriptBoolean.Create(const AWriteable: boolean; const AValue: boolean);
begin
  Create(AWriteable);
  Value := AValue;
end;

constructor TKamScriptBoolean.Create(const AWriteable: boolean);
begin
  inherited Create(AWriteable);
end;

class procedure TKamScriptBoolean.HandleOr(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value := TKamScriptBoolean(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TKamScriptBoolean(AResult).Value :=
      TKamScriptBoolean(AResult).Value or TKamScriptBoolean(Arguments[I]).Value;
end;

class procedure TKamScriptBoolean.HandleAnd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value := TKamScriptBoolean(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TKamScriptBoolean(AResult).Value :=
      TKamScriptBoolean(AResult).Value and TKamScriptBoolean(Arguments[I]).Value;
end;

class procedure TKamScriptBoolean.HandleNot(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value := not TKamScriptBoolean(Arguments[0]).Value;
end;

class procedure TKamScriptBoolean.HandleGreater(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value :=
    TKamScriptBoolean(Arguments[0]).Value >
    TKamScriptBoolean(Arguments[1]).Value;
end;

class procedure TKamScriptBoolean.HandleLesser(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value :=
    TKamScriptBoolean(Arguments[0]).Value <
    TKamScriptBoolean(Arguments[1]).Value;
end;

class procedure TKamScriptBoolean.HandleGreaterEq(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value :=
    TKamScriptBoolean(Arguments[0]).Value >=
    TKamScriptBoolean(Arguments[1]).Value;
end;

class procedure TKamScriptBoolean.HandleLesserEq(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value :=
    TKamScriptBoolean(Arguments[0]).Value <=
    TKamScriptBoolean(Arguments[1]).Value;
end;

class procedure TKamScriptBoolean.HandleEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value :=
    TKamScriptBoolean(Arguments[0]).Value =
    TKamScriptBoolean(Arguments[1]).Value;
end;

class procedure TKamScriptBoolean.HandleNotEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value :=
    TKamScriptBoolean(Arguments[0]).Value <>
    TKamScriptBoolean(Arguments[1]).Value;
end;

class procedure TKamScriptBoolean.ConvertFromInt(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value := TKamScriptInteger(Arguments[0]).Value <> 0;
end;

class procedure TKamScriptBoolean.ConvertFromFloat(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value := TKamScriptFloat(Arguments[0]).Value <> 0;
end;

class procedure TKamScriptBoolean.ConvertFromBool(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  if ParentOfResult then
    AResult.FreeByParentExpression else
    AResult := nil;

  AResult := Arguments[0];
  Assert(AResult is TKamScriptBoolean);
  ParentOfResult := false;
end;

class procedure TKamScriptBoolean.ConvertFromString(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  S: string;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  S := LowerCase(TKamScriptString(Arguments[0]).Value);
  if S = 'false' then
    TKamScriptBoolean(AResult).Value := false else
  if S = 'true' then
    TKamScriptBoolean(AResult).Value := true else
    raise EKamScriptError.CreateFmt('Error when converting string "%s" to boolean: invalid value, must be "false" or "true"',
      [TKamScriptString(Arguments[0]).Value]);
end;

procedure TKamScriptBoolean.AssignValue(Source: TKamScriptValue);
begin
  if Source is TKamScriptBoolean then
    Value := TKamScriptBoolean(Source).Value else
    raise EKamAssignValueError.CreateFmt('Assignment from %s to %s not possible', [Source.ClassName, ClassName]);
end;

procedure TKamScriptBoolean.SetValue(const AValue: Boolean);
begin
  FValue := AValue;
  ValueAssigned := true;
end;

{ TKamScriptString ---------------------------------------------------------- }

constructor TKamScriptString.Create(const AWriteable: boolean; const AValue: string);
begin
  Create(AWriteable);
  Value := AValue;
end;

constructor TKamScriptString.Create(const AWriteable: boolean);
begin
  inherited Create(AWriteable);
end;

class procedure TKamScriptString.HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptString);
  { The function allows only >= 1 arguments, and this handler is
    registered only for TKamScriptString values, so we can safely take
    the first arg as TKamScriptString. }
  TKamScriptString(AResult).Value := TKamScriptString(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TKamScriptString(AResult).Value :=
      TKamScriptString(AResult).Value + TKamScriptString(Arguments[I]).Value;
end;

class procedure TKamScriptString.HandleGreater(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value :=
    TKamScriptString(Arguments[0]).Value >
    TKamScriptString(Arguments[1]).Value;
end;

class procedure TKamScriptString.HandleLesser(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value :=
    TKamScriptString(Arguments[0]).Value <
    TKamScriptString(Arguments[1]).Value;
end;

class procedure TKamScriptString.HandleGreaterEq(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value :=
    TKamScriptString(Arguments[0]).Value >=
    TKamScriptString(Arguments[1]).Value;
end;

class procedure TKamScriptString.HandleLesserEq(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value :=
    TKamScriptString(Arguments[0]).Value <=
    TKamScriptString(Arguments[1]).Value;
end;

class procedure TKamScriptString.HandleEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value :=
    TKamScriptString(Arguments[0]).Value =
    TKamScriptString(Arguments[1]).Value;
end;

class procedure TKamScriptString.HandleNotEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value :=
    TKamScriptString(Arguments[0]).Value <>
    TKamScriptString(Arguments[1]).Value;
end;

class procedure TKamScriptString.ConvertFromInt(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptString);
  TKamScriptString(AResult).Value := IntToStr(TKamScriptInteger(Arguments[0]).Value);
end;

class procedure TKamScriptString.ConvertFromFloat(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptString);
  TKamScriptString(AResult).Value := FloatToStr(TKamScriptFloat(Arguments[0]).Value);
end;

class procedure TKamScriptString.ConvertFromBool(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  BoolTo01: array [boolean] of string = ('false', 'true');
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptString);
  TKamScriptString(AResult).Value := BoolTo01[TKamScriptBoolean(Arguments[0]).Value];
end;

class procedure TKamScriptString.ConvertFromString(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  if ParentOfResult then
    AResult.FreeByParentExpression else
    AResult := nil;

  AResult := Arguments[0];
  Assert(AResult is TKamScriptString);
  ParentOfResult := false;
end;

class procedure TKamScriptString.HandleWriteln(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  S: string;
begin
  if ParentOfResult then
    AResult.FreeByParentExpression else
    AResult := nil;

  AResult := Arguments[0];
  ParentOfResult := false;

  S := TKamScriptString(Arguments[0]).Value;

  if (AFunction.Environment <> nil) and
     Assigned(AFunction.Environment.OutputProc) then
    AFunction.Environment.OutputProc(S) else
    OnWarning(wtMinor, 'KambiScript', 'Writeln: '+ S);
end;

class procedure TKamScriptString.HandleCharacterFromCode(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  CharCode: Int64;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptString);

  CharCode := TKamScriptInteger(Arguments[0]).Value;

  if Between(CharCode, Low(Byte), High(Byte)) then
    TKamScriptString(AResult).Value := Chr(CharCode) else
    TKamScriptString(AResult).Value := '';
end;

procedure TKamScriptString.AssignValue(Source: TKamScriptValue);
begin
  if Source is TKamScriptString then
    Value := TKamScriptString(Source).Value else
    raise EKamAssignValueError.CreateFmt('Assignment from %s to %s not possible', [Source.ClassName, ClassName]);
end;

procedure TKamScriptString.SetValue(const AValue: String);
begin
  FValue := AValue;
  ValueAssigned := true;
end;

{ TKamScriptFunction --------------------------------------------------------- }

constructor TKamScriptFunction.Create(AArgs: TKamScriptExpressionList);
begin
  inherited Create;
  FArgs := TKamScriptExpressionList.Create(false);
  FArgs.AddList(AArgs);
  CheckArguments;
end;

constructor TKamScriptFunction.Create(const AArgs: array of TKamScriptExpression);
begin
  inherited Create;
  FArgs := TKamScriptExpressionList.Create(false);
  FArgs.AddArray(AArgs);
  CheckArguments;
end;

procedure TKamScriptFunction.CheckArguments;
var
  I: Integer;
begin
  for I := 0 to Args.Count - 1 do
    if ArgumentMustBeAssignable(I) and
       not ( (Args[I] is TKamScriptValue) and
             TKamScriptValue(Args[I]).Writeable ) then
      raise EKamScriptFunctionArgumentsError.CreateFmt('Argument %d of function %s must be a writeable operand (but is not)',
        [I, Name]);

  if not FunctionHandlers.SearchFunctionClass(
    TKamScriptFunctionClass(Self.ClassType), HandlersByArgument) then
    raise EKamScriptFunctionNoHandler.CreateFmt('No handler defined for function "%s"', [Name]);

  SetLength(ExecuteArguments, Args.Count);
  SetLength(ExecuteArgumentClasses, Args.Count);
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

class function TKamScriptFunction.Name: string;
begin
  Result := ShortName;
end;

class function TKamScriptFunction.InfixOperatorName: string;
begin
  Result := '';
end;

class function TKamScriptFunction.GreedyArgumentsCalculation: Integer;
begin
  Result := -1;
end;

class function TKamScriptFunction.ArgumentMustBeAssignable(const Index: Integer): boolean;
begin
  Result := false;
end;

function TKamScriptFunction.CoreExecute: TKamScriptValue;

  function ArgumentClassesToStr(const A: TKamScriptValueClassArray): string;
  var
    I: Integer;
  begin
    Result := '';
    for I := 0 to Length(A) - 1 do
    begin
      if I > 0 then Result += ', ';
      { A[I] = nil may happen in case of GreedyArgumentsCalculation >= 0,
        and it means then that any type of arg will be accepted. }
      if A[I] = nil then
        Result += 'anything' else
        Result += A[I].ClassName;
    end;
    Result := '(' + Result + ')';
  end;

var
  Handler: TKamScriptRegisteredHandler;
  I, GreedyArguments: Integer;
begin
  GreedyArguments := Args.Count;
  if GreedyArgumentsCalculation <> -1 then
    MinTo1st(GreedyArguments, GreedyArgumentsCalculation);

  { We have to calculate arguments first, to know their type,
    to decide which handler is suitable.
    Actually, we calculate only first GreedyArguments, rest is left as nil. }
  for I := 0 to GreedyArguments - 1 do
  begin
    ExecuteArguments[I] := Args[I].CoreExecute;
    ExecuteArgumentClasses[I] := TKamScriptValueClass(ExecuteArguments[I].ClassType);
  end;
  for I := GreedyArguments to Args.Count - 1 do
  begin
    ExecuteArguments[I] := nil;
    ExecuteArgumentClasses[I] := nil;
  end;

  { calculate Handler }
  if not FunctionHandlers.SearchArgumentClasses(
    HandlersByArgument, ExecuteArgumentClasses, Handler, Cache1) then
  begin
    { try promoting integer arguments to float, see if it will work then }
    for I := 0 to Length(ExecuteArgumentClasses) - 1 do
      if (ExecuteArgumentClasses[I] <> nil) and
         (ExecuteArgumentClasses[I].InheritsFrom(TKamScriptInteger)) then
        ExecuteArgumentClasses[I] := TKamScriptFloat;

    if FunctionHandlers.SearchArgumentClasses(
      HandlersByArgument, ExecuteArgumentClasses, Handler, Cache2) then
    begin
      { So I found a handler, that will be valid if all integer args will
        get promoted to float. Cool, let's do it.

        I use PromoteToFloat method, that will keep it's result valid
        for some time, since (depending on function handler) we may
        return PromoteToFloat result to the user. }
      for I := 0 to Length(ExecuteArguments) - 1 do
        if (ExecuteArguments[I] <> nil) and
           (ExecuteArguments[I] is TKamScriptInteger) then
          ExecuteArguments[I] := TKamScriptInteger(ExecuteArguments[I]).PromoteToFloat;
    end else
      raise EKamScriptFunctionNoHandler.CreateFmt('Function "%s" is not defined for this combination of arguments: %s',
        [Name, ArgumentClassesToStr(ExecuteArgumentClasses)]);
  end;

  Handler.Handler(Self, ExecuteArguments, LastExecuteResult, ParentOfLastExecuteResult);

  Result := LastExecuteResult;
end;

{ TKamScriptRegisteredHandler ------------------------------------------------ }

constructor TKamScriptRegisteredHandler.Create(
  AHandler: TKamScriptFunctionHandler;
  AFunctionClass: TKamScriptFunctionClass;
  const AArgumentClasses: TKamScriptValueClassArray;
  const AVariableArgumentsCount: boolean);
begin
  FHandler := AHandler;
  FFunctionClass := AFunctionClass;
  FArgumentClasses := AArgumentClasses;
  FVariableArgumentsCount := AVariableArgumentsCount;
end;

{ TKamScriptSequence --------------------------------------------------------- }

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
  AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  if ParentOfResult then
    AResult.FreeByParentExpression else
    AResult := nil;

  AResult := Arguments[High(Arguments)];
  ParentOfResult := false;
end;

{ TKamScriptAssignment --------------------------------------------------------- }

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
  AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  if ParentOfResult then
    AResult.FreeByParentExpression else
    AResult := nil;

  (Arguments[0] as TKamScriptValue).AssignValue(Arguments[1]);

  AResult := Arguments[0] as TKamScriptValue;
  ParentOfResult := false;
end;

class function TKamScriptAssignment.ArgumentMustBeAssignable(const Index: Integer): boolean;
begin
  Result := Index = 0;
end;

{ TKamScriptIf --------------------------------------------------------- }

class function TKamScriptIf.ShortName: string;
begin
  Result := 'if';
end;

class procedure TKamScriptIf.HandleIf(
  AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  if ParentOfResult then
    AResult.FreeByParentExpression;
  AResult := nil;
  ParentOfResult := false;

  if TKamScriptBoolean(Arguments[0]).Value then
    AResult := AFunction.Args[1].CoreExecute else
    AResult := AFunction.Args[2].CoreExecute;
end;

class function TKamScriptIf.GreedyArgumentsCalculation: Integer;
begin
  Result := 1;
end;

{ TKamScriptWhen --------------------------------------------------------- }

class function TKamScriptWhen.ShortName: string;
begin
  Result := 'when';
end;

class procedure TKamScriptWhen.HandleWhen(
  AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  if ParentOfResult then
    AResult.FreeByParentExpression;
  AResult := nil;
  ParentOfResult := false;

  if TKamScriptBoolean(Arguments[0]).Value then
    AResult := AFunction.Args[1].CoreExecute else
  begin
    { "when" returns simple const false on "else" condition }
    AResult := TKamScriptBoolean.Create(false);
    ParentOfResult := true;
  end;
end;

class function TKamScriptWhen.GreedyArgumentsCalculation: Integer;
begin
  Result := 1;
end;

{ TKamScriptWhile --------------------------------------------------------- }

class function TKamScriptWhile.ShortName: string;
begin
  Result := 'while';
end;

class procedure TKamScriptWhile.HandleWhile(
  AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

  function ExecuteCondition: boolean;
  var
    Condition: TKamScriptValue;
  begin
    Condition := AFunction.Args[0].CoreExecute;
    if Condition is TKamScriptBoolean then
      Result := TKamScriptBoolean(Condition).Value else
      raise EKamScriptError.Create('"if" function "condition" must return a boolean value');
  end;

begin
  if ParentOfResult then
    AResult.FreeByParentExpression;
  AResult := nil;
  ParentOfResult := false;

  while ExecuteCondition do
    AResult := AFunction.Args[1].CoreExecute;

  if AResult = nil then
  begin
    { not executed even once? return const false }
    AResult := TKamScriptBoolean.Create(false);
    ParentOfResult := true;
  end;
end;

class function TKamScriptWhile.GreedyArgumentsCalculation: Integer;
begin
  Result := 0;
end;

{ TKamScriptFor --------------------------------------------------------- }

class function TKamScriptFor.ShortName: string;
begin
  Result := 'for';
end;

class procedure TKamScriptFor.HandleFor(
  AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  BeginVal, EndVal: Int64;
  I: Integer;
begin
  if ParentOfResult then
    AResult.FreeByParentExpression;
  AResult := nil;
  ParentOfResult := false;

  BeginVal := TKamScriptInteger(Arguments[1]).Value;
  EndVal := TKamScriptInteger(Arguments[2]).Value;

  for I := BeginVal to EndVal do
  begin
    { We use Arguments[0] here, not AFunction.Args[0], this way
      we know we really have TKamScriptInteger here. While CheckArguments
      makes sure Args[0] is TKamScriptValue, it may be TKamScriptParameterValue.
      We know that TKamScriptParameterValue.Execute returns actual value,
      so Arguments[0] is Ok here. }
    (Arguments[0] as TKamScriptInteger).Value := I;

    AResult := AFunction.Args[3].CoreExecute;
  end;

  if AResult = nil then
  begin
    { not executed even once? return const false }
    AResult := TKamScriptBoolean.Create(false);
    ParentOfResult := true;
  end;
end;

class function TKamScriptFor.GreedyArgumentsCalculation: Integer;
begin
  Result := 3;
end;

class function TKamScriptFor.ArgumentMustBeAssignable(const Index: Integer): boolean;
begin
  { This will cause checking whether Args[0] is assignable TKamScriptValue.

    Note that I cannot check in CheckArguments whether is
    Args[0] is TKamScriptInteger,
    as it may be TKamScriptParameterValue, and so the actual runtime type
    (TKamScriptParameterValue.SourceValue) may be not set yet.
    That's Ok, in HandleFor this will be automatically checked by AssignValue. }

  Result := Index = 0;
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

function TKamScriptFunctionHandlers.SearchArgumentClasses(
  HandlersByArgument: TObjectList;
  const ArgumentClasses: TKamScriptValueClassArray;
  out ArgumentIndex: Integer;
  out Handler: TKamScriptRegisteredHandler): boolean;
var
  I, J: Integer;
begin
  for I := 0 to HandlersByArgument.Count - 1 do
  begin
    Handler := HandlersByArgument[I] as TKamScriptRegisteredHandler;

    { First, check do we have enough arguments: at least
      Length(Handler.ArgumentClasses) are always required. }
    Result := Length(ArgumentClasses) >= Length(Handler.ArgumentClasses);

    if Result then
    begin
      for J := 0 to Length(ArgumentClasses) - 1 do
      begin
        Assert(Result);

        { Always accept ArgumentClasses[J] = nil, this means that they
          are "lazy" arguments (not calculated before handler actually executed),
          so they are simply always assumed Ok. }

        if ArgumentClasses[J] <> nil then
        begin
          if J < Length(Handler.ArgumentClasses) then
            Result := ArgumentClasses[J].InheritsFrom(Handler.ArgumentClasses[J]) else
            { This is more than required number of arguments.
              Still it's Ok if it matches last argument and function allows variable
              number of arguments. }
            Result := Handler.VariableArgumentsCount and
              (Length(Handler.ArgumentClasses) > 0) and
              ArgumentClasses[J].InheritsFrom(
                Handler.ArgumentClasses[High(Handler.ArgumentClasses)]);
        end;

        if not Result then Break;
      end;

      if Result then
      begin
        ArgumentIndex := I;
        Exit;
      end;
    end;
  end;
  Result := false;
end;

function TKamScriptFunctionHandlers.SearchArgumentClasses(
  HandlersByArgument: TObjectList;
  const ArgumentClasses: TKamScriptValueClassArray;
  out Handler: TKamScriptRegisteredHandler): boolean;
var
  ArgumentIndex: Integer;
begin
  Result := SearchArgumentClasses(
    HandlersByArgument, ArgumentClasses, ArgumentIndex, Handler);
end;

function TKamScriptFunctionHandlers.SearchArgumentClasses(
  HandlersByArgument: TObjectList;
  const ArgumentClasses: TKamScriptValueClassArray;
  out Handler: TKamScriptRegisteredHandler;
  var Cache: TKamScriptSearchArgumentClassesCache): boolean;

  function ArgumentClassesEqual(const A1, A2: TKamScriptValueClassArray): boolean;
  begin
    Result := (Length(A1) = Length(A2)) and
      CompareMem(Pointer(A1), Pointer(A1),
        SizeOf(TKamScriptValueClass) * Length(A1));
  end;

begin
  if Cache.IsCache and
     (Cache.QueryHandlersByArgument = HandlersByArgument) and
     ArgumentClassesEqual(Cache.QueryArgumentClasses, ArgumentClasses) then
  begin
    { Use the cached result }
    Handler := Cache.AnswerHandler;
    { ArgumentIndex := Cache.ArgumentIndex; not returned here }
    Result := Cache.Answer;
  end else
  begin
    { Result not in the cache. So calculate it, and record in the cache. }
    Cache.IsCache := true;
    Cache.QueryHandlersByArgument := HandlersByArgument;
    Cache.QueryArgumentClasses := ArgumentClasses;
    Cache.Answer := SearchArgumentClasses(
      HandlersByArgument, ArgumentClasses,
      Cache.AnswerArgumentIndex, Cache.AnswerHandler);

    { Use the cached result }
    Handler := Cache.AnswerHandler;
    { ArgumentIndex := Cache.ArgumentIndex; not returned here }
    Result := Cache.Answer;
  end;
end;

procedure TKamScriptFunctionHandlers.RegisterHandler(
  AHandler: TKamScriptFunctionHandler;
  AFunctionClass: TKamScriptFunctionClass;
  const AArgumentClasses: array of TKamScriptValueClass;
  const AVariableArgumentsCount: boolean);
var
  HandlersByArgument: TObjectList;
  Handler: TKamScriptRegisteredHandler;
  ArgumentClassesDyn: TKamScriptValueClassArray;
begin
  SetLength(ArgumentClassesDyn, High(AArgumentClasses) + 1);
  if Length(ArgumentClassesDyn) > 0 then
    Move(AArgumentClasses[0], ArgumentClassesDyn[0],
      SizeOf(TKamScriptValueClass) * Length(ArgumentClassesDyn));

  if SearchFunctionClass(AFunctionClass, HandlersByArgument) then
  begin
    if not SearchArgumentClasses(HandlersByArgument, ArgumentClassesDyn, Handler) then
    begin
      Handler := TKamScriptRegisteredHandler.Create(
        AHandler, AFunctionClass, ArgumentClassesDyn, AVariableArgumentsCount);
      HandlersByArgument.Add(Handler);
    end;
  end else
  begin
    HandlersByArgument := TObjectList.Create(true);
    FHandlersByFunction.Add(HandlersByArgument);

    Handler := TKamScriptRegisteredHandler.Create(
      AHandler, AFunctionClass, ArgumentClassesDyn, AVariableArgumentsCount);
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
  FParameters := TKamScriptValueList.Create(true);
end;

destructor TKamScriptFunctionDefinition.Destroy;
begin
  if Body <> nil then
    Body.FreeByParentExpression;
  FreeAndNil(FParameters);
  inherited;
end;

{ TKamScriptFunctionDefinitionList ------------------------------------------ }

function TKamScriptFunctionDefinitionList.IndexOf(
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
  FFunctions := TKamScriptFunctionDefinitionList.Create(true);
  FEnvironment := TKamScriptEnvironment.Create;
end;

destructor TKamScriptProgram.Destroy;
begin
  FreeAndNil(FFunctions);
  FreeAndNil(FEnvironment);
  inherited;
end;

procedure TKamScriptProgram.ExecuteFunction(const FunctionName: string;
  const Parameters: array of TKamScriptValue;
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

  if High(Parameters) <> Func.Parameters.Count - 1 then
    raise EKamScriptError.CreateFmt('KambiScript function "%s" requires %d parameters, but passed %d parameters',
      [FunctionName, Func.Parameters.Count, High(Parameters) + 1]);

  for I := 0 to High(Parameters) do
    (Func.Parameters[I] as TKamScriptParameterValue).SourceValue := Parameters[I];

  Func.Body.Execute;

  { Just for safety, clear SourceValue references.

    This is safe, since no code can access Parameters instances
    (trying to get their value directly or by AssignValue) after
    ExecuteFunction. Code from the outside can only access it's own
    global variables after execution, which have values directly stored.

    This *could* become a problem if we want to return function's value
    in the future, then this will possibly have to be removed,
    as Func.Body.Execute may directly return one of our Parameters. }

  for I := 0 to High(Parameters) do
    (Func.Parameters[I] as TKamScriptParameterValue).SourceValue := nil;
end;

{ procedural utils ----------------------------------------------------------- }

procedure CreateValueIfNeeded(var Value: TKamScriptValue;
  var ParentOfValue: boolean;
  NeededClass: TKamScriptValueClass);
begin
  if Value = nil then
  begin
    Value := NeededClass.Create(false);
    ParentOfValue := true;
  end else
  if Value.ClassType <> NeededClass then
  begin
    if ParentOfValue then
      Value.FreeByParentExpression else
      Value := nil;

    Value := NeededClass.Create(false);
    ParentOfValue := true;
  end;
end;

{ unit init/fini ------------------------------------------------------------- }

initialization
  {$ifdef WORKAROUND_EXCEPTIONS_FOR_SCRIPT_EXPRESSIONS}
  Set8087CW($133F);
  {$endif}

  FunctionHandlers := TKamScriptFunctionHandlers.Create;

  FunctionHandlers.RegisterHandler(@TKamScriptSequence(nil).HandleSequence, TKamScriptSequence, [TKamScriptValue], true);
  FunctionHandlers.RegisterHandler(@TKamScriptAssignment(nil).HandleAssignment, TKamScriptAssignment, [TKamScriptValue, TKamScriptValue], false);

  FunctionHandlers.RegisterHandler(@TKamScriptIf(nil).HandleIf, TKamScriptIf, [TKamScriptBoolean, TKamScriptValue, TKamScriptValue], false);
  FunctionHandlers.RegisterHandler(@TKamScriptWhen(nil).HandleWhen, TKamScriptWhen, [TKamScriptBoolean, TKamScriptValue], false);
  FunctionHandlers.RegisterHandler(@TKamScriptWhile(nil).HandleWhile, TKamScriptWhile, [TKamScriptBoolean, TKamScriptValue], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFor(nil).HandleFor, TKamScriptFor, [TKamScriptInteger, TKamScriptInteger, TKamScriptInteger, TKamScriptValue], false);

  { Register handlers for TKamScriptInteger for functions in
    KambiScriptCoreFunctions. }
  FunctionHandlers.RegisterHandler(@TKamScriptInteger(nil).HandleAdd, TKamScriptAdd, [TKamScriptInteger], true);
  FunctionHandlers.RegisterHandler(@TKamScriptInteger(nil).HandleSubtract, TKamScriptSubtract, [TKamScriptInteger], true);
  FunctionHandlers.RegisterHandler(@TKamScriptInteger(nil).HandleNegate, TKamScriptNegate, [TKamScriptInteger], false);

  FunctionHandlers.RegisterHandler(@TKamScriptInteger(nil).HandleMultiply, TKamScriptMultiply, [TKamScriptInteger], true);
  FunctionHandlers.RegisterHandler(@TKamScriptInteger(nil).HandleDivide, TKamScriptDivide, [TKamScriptInteger], true);
  FunctionHandlers.RegisterHandler(@TKamScriptInteger(nil).HandleModulo, TKamScriptModulo, [TKamScriptInteger, TKamScriptInteger], false);
  FunctionHandlers.RegisterHandler(@TKamScriptInteger(nil).HandlePower, TKamScriptPower, [TKamScriptInteger, TKamScriptInteger], false);

  FunctionHandlers.RegisterHandler(@TKamScriptInteger(nil).HandleMax, TKamScriptMax, [TKamScriptInteger], true);
  FunctionHandlers.RegisterHandler(@TKamScriptInteger(nil).HandleMin, TKamScriptMin, [TKamScriptInteger], true);
  FunctionHandlers.RegisterHandler(@TKamScriptInteger(nil).HandleSqr, TKamScriptSqr, [TKamScriptInteger], false);
  FunctionHandlers.RegisterHandler(@TKamScriptInteger(nil).HandleSgn, TKamScriptSgn, [TKamScriptInteger], false);
  FunctionHandlers.RegisterHandler(@TKamScriptInteger(nil).HandleAbs, TKamScriptAbs, [TKamScriptInteger], false);
  FunctionHandlers.RegisterHandler(@TKamScriptInteger(nil).HandleRandom, TKamScriptRandom, [TKamScriptInteger], false);

  FunctionHandlers.RegisterHandler(@TKamScriptInteger(nil).HandleGreater, TKamScriptGreater, [TKamScriptInteger, TKamScriptInteger], false);
  FunctionHandlers.RegisterHandler(@TKamScriptInteger(nil).HandleLesser, TKamScriptLesser, [TKamScriptInteger, TKamScriptInteger], false);
  FunctionHandlers.RegisterHandler(@TKamScriptInteger(nil).HandleGreaterEq, TKamScriptGreaterEq, [TKamScriptInteger, TKamScriptInteger], false);
  FunctionHandlers.RegisterHandler(@TKamScriptInteger(nil).HandleLesserEq, TKamScriptLesserEq, [TKamScriptInteger, TKamScriptInteger], false);
  FunctionHandlers.RegisterHandler(@TKamScriptInteger(nil).HandleEqual, TKamScriptEqual, [TKamScriptInteger, TKamScriptInteger], false);
  FunctionHandlers.RegisterHandler(@TKamScriptInteger(nil).HandleNotEqual, TKamScriptNotEqual, [TKamScriptInteger, TKamScriptInteger], false);

  FunctionHandlers.RegisterHandler(@TKamScriptInteger(nil).ConvertFromInt   , TKamScriptInt, [TKamScriptInteger], false);
  FunctionHandlers.RegisterHandler(@TKamScriptInteger(nil).ConvertFromFloat , TKamScriptInt, [TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptInteger(nil).ConvertFromBool  , TKamScriptInt, [TKamScriptBoolean], false);
  FunctionHandlers.RegisterHandler(@TKamScriptInteger(nil).ConvertFromString, TKamScriptInt, [TKamScriptString], false);

  { Register handlers for TKamScriptFloat for functions in
    KambiScriptCoreFunctions. }
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleAdd, TKamScriptAdd, [TKamScriptFloat], true);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleSubtract, TKamScriptSubtract, [TKamScriptFloat], true);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleMultiply, TKamScriptMultiply, [TKamScriptFloat], true);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleDivide, TKamScriptDivide, [TKamScriptFloat], true);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleNegate, TKamScriptNegate, [TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleModulo, TKamScriptModulo, [TKamScriptFloat, TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleSin, TKamScriptSin, [TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleCos, TKamScriptCos, [TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleTan, TKamScriptTan, [TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleCotan, TKamScriptCotan, [TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleArcSin, TKamScriptArcSin, [TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleArcCos, TKamScriptArcCos, [TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleArcTan, TKamScriptArcTan, [TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleArcCotan, TKamScriptArcCotan, [TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleSinh, TKamScriptSinh, [TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleCosh, TKamScriptCosh, [TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleTanh, TKamScriptTanh, [TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleCotanh, TKamScriptCotanh, [TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleLog2, TKamScriptLog2, [TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleLn, TKamScriptLn, [TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleLog, TKamScriptLog, [TKamScriptFloat, TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandlePower2, TKamScriptPower2, [TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleExp, TKamScriptExp, [TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandlePower, TKamScriptPower, [TKamScriptFloat, TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleSqr, TKamScriptSqr, [TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleSqrt, TKamScriptSqrt, [TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleMax, TKamScriptMax, [TKamScriptFloat], true);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleMin, TKamScriptMin, [TKamScriptFloat], true);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleSgn, TKamScriptSgn, [TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleAbs, TKamScriptAbs, [TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleRandom, TKamScriptRandom, [], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleCeil, TKamScriptCeil, [TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleFloor, TKamScriptFloor, [TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleRound, TKamScriptRound, [TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleGreater, TKamScriptGreater, [TKamScriptFloat, TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleLesser, TKamScriptLesser, [TKamScriptFloat, TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleGreaterEq, TKamScriptGreaterEq, [TKamScriptFloat, TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleLesserEq, TKamScriptLesserEq, [TKamScriptFloat, TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleEqual, TKamScriptEqual, [TKamScriptFloat, TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).HandleNotEqual, TKamScriptNotEqual, [TKamScriptFloat, TKamScriptFloat], false);

  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).ConvertFromInt   , TKamScriptFloatFun, [TKamScriptInteger], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).ConvertFromFloat , TKamScriptFloatFun, [TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).ConvertFromBool  , TKamScriptFloatFun, [TKamScriptBoolean], false);
  FunctionHandlers.RegisterHandler(@TKamScriptFloat(nil).ConvertFromString, TKamScriptFloatFun, [TKamScriptString], false);

  { Register handlers for TKamScriptBoolean for functions in
    KambiScriptCoreFunctions. }
  FunctionHandlers.RegisterHandler(@TKamScriptBoolean(nil).HandleOr, TKamScriptOr, [TKamScriptBoolean], true);
  FunctionHandlers.RegisterHandler(@TKamScriptBoolean(nil).HandleAnd, TKamScriptAnd, [TKamScriptBoolean], true);
  FunctionHandlers.RegisterHandler(@TKamScriptBoolean(nil).HandleNot, TKamScriptNot, [TKamScriptBoolean], false);

  FunctionHandlers.RegisterHandler(@TKamScriptBoolean(nil).HandleGreater, TKamScriptGreater, [TKamScriptBoolean, TKamScriptBoolean], false);
  FunctionHandlers.RegisterHandler(@TKamScriptBoolean(nil).HandleLesser, TKamScriptLesser, [TKamScriptBoolean, TKamScriptBoolean], false);
  FunctionHandlers.RegisterHandler(@TKamScriptBoolean(nil).HandleGreaterEq, TKamScriptGreaterEq, [TKamScriptBoolean, TKamScriptBoolean], false);
  FunctionHandlers.RegisterHandler(@TKamScriptBoolean(nil).HandleLesserEq, TKamScriptLesserEq, [TKamScriptBoolean, TKamScriptBoolean], false);
  FunctionHandlers.RegisterHandler(@TKamScriptBoolean(nil).HandleEqual, TKamScriptEqual, [TKamScriptBoolean, TKamScriptBoolean], false);
  FunctionHandlers.RegisterHandler(@TKamScriptBoolean(nil).HandleNotEqual, TKamScriptNotEqual, [TKamScriptBoolean, TKamScriptBoolean], false);

  FunctionHandlers.RegisterHandler(@TKamScriptBoolean(nil).ConvertFromInt   , TKamScriptBool, [TKamScriptInteger], false);
  FunctionHandlers.RegisterHandler(@TKamScriptBoolean(nil).ConvertFromFloat , TKamScriptBool, [TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptBoolean(nil).ConvertFromBool  , TKamScriptBool, [TKamScriptBoolean], false);
  FunctionHandlers.RegisterHandler(@TKamScriptBoolean(nil).ConvertFromString, TKamScriptBool, [TKamScriptString], false);

  { Register handlers for TKamScriptString for functions in
    KambiScriptCoreFunctions. }
  FunctionHandlers.RegisterHandler(@TKamScriptString(nil).HandleAdd, TKamScriptAdd, [TKamScriptString, TKamScriptString], false);

  FunctionHandlers.RegisterHandler(@TKamScriptString(nil).HandleGreater, TKamScriptGreater, [TKamScriptString, TKamScriptString], false);
  FunctionHandlers.RegisterHandler(@TKamScriptString(nil).HandleLesser, TKamScriptLesser, [TKamScriptString, TKamScriptString], false);
  FunctionHandlers.RegisterHandler(@TKamScriptString(nil).HandleGreaterEq, TKamScriptGreaterEq, [TKamScriptString, TKamScriptString], false);
  FunctionHandlers.RegisterHandler(@TKamScriptString(nil).HandleLesserEq, TKamScriptLesserEq, [TKamScriptString, TKamScriptString], false);
  FunctionHandlers.RegisterHandler(@TKamScriptString(nil).HandleEqual, TKamScriptEqual, [TKamScriptString, TKamScriptString], false);
  FunctionHandlers.RegisterHandler(@TKamScriptString(nil).HandleNotEqual, TKamScriptNotEqual, [TKamScriptString, TKamScriptString], false);

  FunctionHandlers.RegisterHandler(@TKamScriptString(nil).ConvertFromInt   , TKamScriptStringFun, [TKamScriptInteger], false);
  FunctionHandlers.RegisterHandler(@TKamScriptString(nil).ConvertFromFloat , TKamScriptStringFun, [TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptString(nil).ConvertFromBool  , TKamScriptStringFun, [TKamScriptBoolean], false);
  FunctionHandlers.RegisterHandler(@TKamScriptString(nil).ConvertFromString, TKamScriptStringFun, [TKamScriptString], false);

  FunctionHandlers.RegisterHandler(@TKamScriptString(nil).HandleWriteln, TKamScriptWriteln, [TKamScriptString], false);
  FunctionHandlers.RegisterHandler(@TKamScriptString(nil).HandleCharacterFromCode, TKamScriptCharacterFromCode, [TKamScriptInteger], false);
finalization
  FreeAndNil(FunctionHandlers);
end.
