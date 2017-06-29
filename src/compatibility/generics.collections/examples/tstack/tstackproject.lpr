// Generic types for FreeSparta.com and FreePascal!
// Original version by keeper89.blogspot.com, 2011
// FPC version by Maciej Izak (hnb), 2014
program TStackProject;

{$MODE DELPHI}
{$APPTYPE CONSOLE}

uses
  SysUtils,
  Generics.Collections;

type
  // We will cook pancakes, put them on a plate and take the last
  TPancakeType = (ptMeat, ptCherry, ptCurds);

  TPancake = record
    strict private
      const
        PANCAKE_TYPE_NAMES: array [TPancakeType] of string =
          ('meat', 'cherry', 'curds');
    public
      var
        PancakeType: TPancakeType;
    class function Create(PancakeType: TPancakeType): TPancake; static;
    function ToString: string;
  end;

class function TPancake.Create(PancakeType: TPancakeType): TPancake;
begin
  Result.PancakeType := PancakeType;
end;

function TPancake.ToString: string;
begin
  Result := Format('Pancake with %s', [PANCAKE_TYPE_NAMES[PancakeType]])
end;

var
  PancakesPlate: TStack<TPancake>;
  Pancake: TPancake;

begin
  WriteLn('Working with TStack - pancakes');
  WriteLn;

  // "Create" a plate of pancakes
  PancakesPlate := TStack<TPancake>.Create;

  // Bake some pancakes
  // Push - puts items on the stack
  PancakesPlate.Push(TPancake.Create(ptMeat));
  PancakesPlate.Push(TPancake.Create(ptCherry));
  PancakesPlate.Push(TPancake.Create(ptCherry));
  PancakesPlate.Push(TPancake.Create(ptCurds));
  PancakesPlate.Push(TPancake.Create(ptMeat));

  // Eating some pancakes
  // Pop - removes an item from the stack
  Pancake := PancakesPlate.Pop;
  Writeln(Format('Ate a pancake (Pop): %s', [Pancake.ToString]));
  // Extract - similar to Pop, but causes in OnNotify
  // Action = cnExtracted instead of cnRemoved
  Pancake := PancakesPlate.Extract;
  Writeln(Format('Ate a pancake (Extract): %s', [Pancake.ToString]));

  // What is the last pancake?
  // Peek - returns the last item, but does not remove it from the stack
  Writeln(Format('Last pancake: %s', [PancakesPlate.Peek.ToString]));

  // Show the remaining pancakes
  Writeln;
  Writeln(Format('Total pancakes: %d', [PancakesPlate.Count]));
  for Pancake in PancakesPlate do
    Writeln(Pancake.ToString);

  // Eat up all
  // Clear - clears the stack
  PancakesPlate.Clear;

  FreeAndNil(PancakesPlate);

  Readln;
end.

