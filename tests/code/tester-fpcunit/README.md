Useful units to integrate Castle Game Engine applications with FPCUnit.

- CastleTestCase provides a descendant of FPC TTestCase.
  It adds various assertion methods that handle CGE types.
  E.g. you can use AssertVectorEquals to compare TVector3 values.

- CastleConsoleTestRunner is a trivial improvement of FPC ConsoleTestRunner.
  It makes sure that test suite exit code is <> 0 in case of error,
  which is useful when running it from shell scripts e.g. in Jenkins,
  when you want the failure to be reported upward.

To use these units now, just add them to your project (there is no lpk
with these units, for now).
