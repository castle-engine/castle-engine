# Check that Castle Game Engine Lazarus packages are correct

Right now it checks that they contain all files they should.
It is an easy mistake that you add a file (unit or include file) and forget
to add it to lpk -- the consequences are not terrible (the lpk still compiles)
but when modifying this file, the lpk will be rebuild by Lazarus.

Run with a single command-line parameter that points to the CGE top-level directory.
