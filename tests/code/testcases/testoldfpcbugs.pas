// -*- compile-command: "./test_single_testcase.sh TTestOldFPCBugs" -*-
{
  Copyright 2004-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ A really old test unit, testing some FPC 1.0.x bugs (or, sometimes,
  just some less-visible features that I wanted to make sure work right,
  and they did). They are all fixed, since a long long time. }
unit TestOldFPCBugs;

interface

uses
  {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry
  {$else}CastleTester{$endif};

type
  TTestOldFPCBugs = class({$ifndef CASTLE_TESTER}TTestCase{$else}TCastleTestCase{$endif})
  published
    procedure TestInherited;
    procedure TestMethodPass;
    procedure TestCompareMemBug;
    procedure TestFormatIncompatibility;
    procedure TestSizeOfObject;
    procedure TestOthers;
    procedure TestSwapEndian;
  end;

implementation

uses SysUtils;

{ TestInherited -------------------------------------------------------------- }

{ sprawdzalem tylko czy freepascal na pewno dziala tak :
  - wywolanie kontruktora/destruktora z wnetrza obiektu nie powoduje
    zadnej alokacji/dealokacji pamieci na obiekt czy na jego VMT.
    Wywoluje to tylko zawarty tam normalny kod.
  - dopiero wywolanie konstrukt/destrukt z zewnatrz powoduje ze
    najpierw przydzielana jest pamiec dla obiektu, jego VMT i czego tam
      jeszcze trzeba, a POTEM wywolywany jest kod konstruktora obiektu
      (w ktorym moze ale de facto nie musi byc wywolanie inherited -
      to malo eleganckie pisac obiekty ktore nie wywoluja inherited
      w konstrukt/destrukt, ale jesli wiesz ze kod w inherited konstrukt/destrukt
      nie jest ci potrzebny to nie musisz wywolywac inherited)
    analogicznie dla destruktora
}

type
  TObj=class
    a:integer;
    constructor Create;
    destructor Destroy; override;
  end;

constructor TObj.Create;
begin
 a:=42;
 inherited;
end;

destructor TObj.Destroy;
var s:string;
begin
 inherited;

 {do some memory allocs/frees}
 s:='blablabla';
 if s<>'' then s:='popoty ' + s ;

 {check value of a}
 if a <> 42 then
   raise Exception.Create('a <> 42');
end;

procedure TTestOldFPCBugs.TestInherited;
var obj:TObj;
begin
 obj:=TObj.Create;
 FreeAndNil(obj);
end;

{ TestAbsProc ---------------------------------------------------------------- }

{ fails in 1.0.6 (corrected in 1.0.7/10) }

{ TestAbsProc needs not to be called, it will fail at compilation. }
procedure TestAbsProc(mm:TProcedure);
var m2:array[0..1]of Pointer absolute mm;
begin
 { do something on "m2" variable }
 if m2[0]=nil then Exit;
end;

{ MethodPass ----------------------------------------------------------------- }

{ fails in 1.0.10; reported to fpc-devel lists }

type
  TProcOfObj=procedure of object;
  PMethod = ^TMethod;

{ we could write the procedure below as
    procedure Proc(mm:TProcOfObj);
    var m2:TMethod absolute mm;
    begin
     AssertTrue(m2.Code <> nil);
    end;
  but it would require TEST_ABSOLUTE_PROCEDURE to pass (and we want to
  test these two things _separately_.)
}
procedure Proc(mm:TProcOfObj);
begin
  if PMethod(@mm)^.Code = nil then
   raise Exception.Create('PMethod(@mm)^.Code = nil');
end;

type
  TObjMM=class
    procedure MyMethod;
    procedure CallProcWithMyMethod;
  end;

procedure TObjMM.MyMethod; begin end;
procedure TObjMM.CallProcWithMyMethod; begin Proc(@MyMethod) end;

procedure TTestOldFPCBugs.TestMethodPass;
var o:TObjMM;
begin
 o:=TObjMM.Create;
 try
  o.CallProcWithMyMethod;
 finally o.Free end;
end;

{ TestCallFuncOfObject ------------------------------------------------------- }

{ fails in 1.1 (snapshot at 23.07.2003); submitted to bug form }

{$warnings off}
procedure TestCallFuncOfObject;
{ this is a compile-time bug; do not call this procedure }
type
  TFuncByObject = function(i:Integer):boolean of object;
var F:TFuncByObject;
begin
 F(1);
end;
{$warnings on}

{ TestCompareMemBug ---------------------------------------------------------- }

procedure TTestOldFPCBugs.TestCompareMemBug;
var b1,b2:array[0..1000]of byte;
begin
 { if CompareMem(p1, p2, 0) would work good then values for p1 and p2
   should be ignored. But, since there is a bug, they will not be ignored
   so we have to provide valid pointers for p1 and p2 or we will get
   AccessViolation. }
 AssertTrue(CompareMem(@b1, @b2, 0));
end;

procedure TTestOldFPCBugs.TestFormatIncompatibility;
begin
 AssertTrue(Format('%d %d %0:d %d', [0, 1, 2, 3]) = '0 1 0 1');
end;

{ TestSizeOfObject ----------------------------------------------------------- }

{ fails in some 1.0.7 versions and in 1.0.10
  Reported to fpc-devel lists. }

const D = SizeOf(TObject);
procedure TTestOldFPCBugs.TestSizeOfObject;
begin
  AssertTrue(D = SizeOf(Pointer));
end;

{ TestOthers ----------------------------------------------------------------- }

{ przeciazanie procedur gdy jedna wersja nie bierze zadnych arg.
  To dziala tylko pod 1.1, 1.0.x musza byc odpowiednio zmodyfikowane
  aby to dzialalo. (wiele mojego kodu wymaga zeby to dzialalo,
  juz CastleUtils.) }
function TestProc(arg:integer):boolean; overload; begin result:=true  end;
function TestProc             :boolean; overload; begin result:=false end;

procedure TTestOldFPCBugs.TestOthers;
var b1,b2:array[0..1000]of byte;
begin
 AssertTrue(not TestProc);
 AssertTrue(TestProc(2));

 AssertTrue(SizeOf(AnsiString) = SizeOf(Pointer));
 AssertTrue(SizeOf(String) = SizeOf(Pointer));

 { some set operations }
 AssertTrue([0,1] = [0,1]);
 AssertTrue([0] <= [0]);
 AssertTrue([0] <= [0,1,2,3,4]);
 AssertTrue(not ([0] <= []));
 AssertTrue([0] >= []);

 { test is CompareMem(..., ..., 0) bug fixed }
 AssertTrue(CompareMem(@b1, @b2, 0));
 { test is "Format incompatible with Delphi" bug fixed }
 AssertTrue(Format('%d %d %0:d %d', [0, 1, 2, 3]) = '0 1 0 1');
end;

procedure TTestOldFPCBugs.TestSwapEndian;
const
  A1: QWord = $0123456789ABCDEF;
var
  A2: QWord;
begin
  A2 := QWord($EFCDAB8967452301);
  AssertTrue(SwapEndian(A1) = A2);
  AssertTrue(SwapEndian(A2) = A1);
end;

initialization
 RegisterTest(TTestOldFPCBugs);
end.
