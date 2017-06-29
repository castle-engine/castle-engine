// Generic types for FreeSparta.com and FreePascal!
// Original version by keeper89.blogspot.com, 2011
// FPC version by Maciej Izak (hnb), 2014

program TObjectListProject;

{$MODE DELPHI}
{$APPTYPE CONSOLE}

uses
  SysUtils, Generics.Collections, Generics.Defaults, DateUtils;

type
  TPlayer = class
  public
    Name, Team: string;
    BirthDay: TDateTime;
    NTeamGoals: Byte; // Number of goals for the national team
    constructor Create(const Name: string; BirthDay: TDateTime;
      const Team: string; NTeamGoals: Byte = 0);
    function ToString: string;
  end;

  // Class containing handlers add / remove list items
  TListEventsHandler = class
  public
    class procedure OnListChanged(Sender: TObject; constref Item: TPlayer;
      Action: TCollectionNotification);
  end;


constructor TPlayer.Create(const Name: string; BirthDay: TDateTime;
  const Team: string; NTeamGoals: Byte);
begin
  Self.Name := Name;
  Self.BirthDay := BirthDay;
  Self.Team := Team;
  Self.NTeamGoals := NTeamGoals;
end;

function TPlayer.ToString: string;
begin
  Result := Format('%s - Age: %d Team: %s Goals: %d',
                   [Name,
                    DateUtils.YearsBetween(Date, BirthDay),
                    Team, NTeamGoals])
end;

// Function sort descending goals for the national team
function ComparePlayersByGoalsDecs(constref Player1, Player2: TPlayer): Integer;
begin
  Result := TCompare.UInt8(Player2.NTeamGoals, Player1.NTeamGoals);
end;

class procedure TListEventsHandler.OnListChanged(Sender: TObject; constref Item: TPlayer;
  Action: TCollectionNotification);
var
  Mes: string;
begin
  // Unlike TDictionary we added Action = cnExtracted
  case Action of
    cnAdded:
      Mes := 'added to the list!';
    cnRemoved:
      Mes := 'removed from the list!';
    cnExtracted:
      Mes := 'extracted from the list!';
  end;
  Writeln(Format('Football player %s %s ', [Item.ToString, Mes]));
end;

var
  // Declare TObjectList as storage for TPlayer
  PlayersList: TObjectList<TPlayer>;
  Player: TPlayer;
  FoundIndex: PtrInt;
begin
  WriteLn('Working with TObjectList - football manager');
  WriteLn;

  PlayersList := TObjectList<TPlayer>.Create;

  // ---------------------------------------------------
  // 1) Adding items

  PlayersList.Add(
    TPlayer.Create('Zinedine Zidane', EncodeDate(1972, 06, 23), 'France', 31));
  PlayersList.Add(
    TPlayer.Create('Raul', EncodeDate(1977, 06, 27), 'Spain', 44));
  PlayersList.Add(
    TPlayer.Create('Ronaldo', EncodeDate(1976, 09, 22), 'Brazil', 62));
   // Adding the specified position
  PlayersList.Insert(0,
    TPlayer.Create('Luis Figo', EncodeDate(1972, 11, 4), 'Portugal', 33));
  // Add a few players through InsertRange (AddRange works similarly)
  PlayersList.InsertRange(0,
    [TPlayer.Create('David Beckham', EncodeDate(1975, 05, 2), 'England', 17),
     TPlayer.Create('Alessandro Del Piero', EncodeDate(1974, 11, 9), 'Italy ', 27),
     TPlayer.Create('Raul', EncodeDate(1977, 06, 27), 'Spain', 44)]);
  Player := TPlayer.Create('Raul', EncodeDate(1977, 06, 27), 'Spain', 44);
  PlayersList.Add(Player);


  // ---------------------------------------------------
  // 2) Access and check the items

  // Is there a player in the list - Contains
  if PlayersList.Contains(Player) then
    Writeln('Raul is in the list!');
  // Player index and count of items in the list
  Writeln(Format('Raul is %d-th on the list of %d players.',
                 [PlayersList.IndexOf(Player) + 1, PlayersList.Count]));
  // Index access
  Writeln(Format('1st in the list: %s', [PlayersList[0].ToString]));
  // The first player
  Writeln(Format('1st in the list: %s', [PlayersList.First.ToString]));
  // The last player
  Writeln(Format('Last in the list: %s', [PlayersList.Last.ToString]));
  // "Reverse" elements
  PlayersList.Reverse;
  Writeln('List items have been "reversed"');
  Writeln;


  // ---------------------------------------------------
  // 3) Moving and removing items

  // Changing places players in the list
  PlayersList.Exchange(0, 1);
  // Move back 1 player
  PlayersList.Move(1, 0);

  // Removes the element at index
  PlayersList.Delete(5);
  // Or a number of elements starting at index
  PlayersList.DeleteRange(5, 2);
  // Remove the item from the list, if the item
  // exists returns its index in the list
  Writeln(Format('Removed %d-st player', [PlayersList.Remove(Player) + 1]));

  // Extract and return the item, if there is no Player in the list then
  // Extract will return = nil, (anyway Raul is already removed via Remove)
  Player := PlayersList.Extract(Player);
  if Assigned(Player) then
    Writeln(Format('Extracted: %s', [Player.ToString]));

  // Clear the list completely
  PlayersList.Clear;
  Writeln;

  // ---------------------------------------------------
  // 4) Event OnNotify, sorting and searching

  PlayersList.OnNotify := TListEventsHandler.OnListChanged;

  PlayersList.Add(
    TPlayer.Create('Zinedine Zidane', EncodeDate(1972, 06, 23), 'France', 31));
  PlayersList.Add(
    TPlayer.Create('Raul', EncodeDate(1977, 06, 27), 'Spain', 44));
  PlayersList.Add(
    TPlayer.Create('Ronaldo', EncodeDate(1976, 09, 22), 'Brazil', 62));
  PlayersList.AddRange(
    [TPlayer.Create('David Beckham', EncodeDate(1975, 05, 2), 'England', 17),
     TPlayer.Create('Alessandro Del Piero', EncodeDate(1974, 11, 9), 'Italy ', 27),
     TPlayer.Create('Raul', EncodeDate(1977, 06, 27), 'Spain', 44)]);

  PlayersList.Remove(PlayersList.Last);
  Player := PlayersList.Extract(PlayersList[0]);

  PlayersList.Sort(TComparer<TPlayer>.Construct(ComparePlayersByGoalsDecs));
  Writeln;
  Writeln('Sorted list of players:');
  for Player in PlayersList do
    Writeln(Player.ToString);
  Writeln;

  // Find Ronaldo!
  // TArray BinarySearch requires sorted list
  // IndexOf does not require sorted list
  // but BinarySearch is usually faster
  Player := PlayersList[0];
  if PlayersList.BinarySearch(Player, FoundIndex,
    TComparer<TPlayer>.Construct(ComparePlayersByGoalsDecs)) then
    Writeln(Format('Ronaldo is in the sorted list at position %d', [FoundIndex + 1]));

  Writeln;

  // With the destruction of the list remove all elements
  // OnNotify show it
  FreeAndNil(PlayersList);

  Readln;
end.

