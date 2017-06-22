// Generic types for FreeSparta.com and FreePascal!
// Original version by keeper89.blogspot.com, 2011
// FPC version by Maciej Izak (hnb), 2014

program THashMapProject;

{$MODE DELPHI}
{$APPTYPE CONSOLE}

uses
  SysUtils, Generics.Collections, Generics.Defaults;

type
  TSubscriberInfo = record
    Name, SName: string;
    class function Create(const Name, SName: string): TSubscriberInfo; static;
    function ToString: string;
  end;

  // Class containing handlers add / remove items in the dictionary
  THashMapEventsHandler = class
  public
    class procedure OnKeyNotify(Sender: TObject; constref Item: string;
      Action: TCollectionNotification);
    class procedure OnValueNotify(Sender: TObject; constref Item: TSubscriberInfo;
      Action: TCollectionNotification);
  end;

class function TSubscriberInfo.Create(const Name,
  SName: string): TSubscriberInfo;
begin
  Result.Name := Name;
  Result.SName := SName;
end;

function TSubscriberInfo.ToString: string;
begin
  Result := Format('%s %s', [Name, SName]);
end;

// Function to generate the dictionary contents into a string
function PrintTelephoneDirectory(
  TelephoneDirectory: THashMap<string, TSubscriberInfo>): string;
var
  PhoneNumber: string;
begin
  Result := Format('Content directory (%d):', [TelephoneDirectory.Count]);

  for PhoneNumber in TelephoneDirectory.Keys do
    Result := Result + Format(LineEnding + '%s: %s',
      [PhoneNumber, TelephoneDirectory[PhoneNumber].ToString]);
end;

// Handlers add / remove items dictionary
class procedure THashMapEventsHandler.OnKeyNotify(Sender: TObject;
  constref Item: string; Action: TCollectionNotification);
begin
  case Action of
    cnAdded:
      Writeln(Format('OnKeyNotify! Phone %s added!', [Item]));
    cnRemoved:
      Writeln(Format('OnKeyNotify! Number %s deleted!', [Item]));
  end;
end;

class procedure THashMapEventsHandler.OnValueNotify(Sender: TObject;
  constref Item: TSubscriberInfo; Action: TCollectionNotification);
begin
  case Action of
    cnAdded:
      Writeln(Format('OnValueNotify! Subscriber %s added!', [Item.ToString]));
    cnRemoved:
      Writeln(Format('OnValueNotify! Subscriber %s deleted!', [Item.ToString]));
  end;
end;

function CustomCompare(constref Left, Right: TPair<string, TSubscriberInfo>): Integer;
begin
  // Comparable full first names, and then phones if necessary
  Result := TCompare.&String(Left.Value.ToString, Right.Value.ToString);
  if Result = 0 then
    Result := TCompare.&String(Left.Key, Right.Key);
end;

var
  // Declare the "dictionary"
  // key is the telephone number which will be possible
  // to determine information about the owner
  TelephoneDirectory: THashMap<string, TSubscriberInfo>;
  TTelephoneArray: array of TPair<string, TSubscriberInfo>;
  TTelephoneArrayItem: TPair<string, TSubscriberInfo>;
  PhoneNumber: string;
  Subscriber: TSubscriberInfo;
begin
  WriteLn('Working with THashMap - phonebook');
  WriteLn;

  // create a directory
  // Constructor has several overloaded options which will
  // enable the capacity of the container, a comparator for values
  // or the initial data - we use the easiest option
  TelephoneDirectory := THashMap<string, TSubscriberInfo>.Create;

  // ---------------------------------------------------
  // 1) Adding items to dictionary

  // Add new users to the phonebook
  TelephoneDirectory.Add('9201111111', TSubscriberInfo.Create('Arnold', 'Schwarzenegger'));
  TelephoneDirectory.Add('9202222222', TSubscriberInfo.Create('Jessica', 'Alba'));
  TelephoneDirectory.Add('9203333333', TSubscriberInfo.Create('Brad', 'Pitt'));
  TelephoneDirectory.Add('9204444444', TSubscriberInfo.Create('Brad', 'Pitt'));
  TelephoneDirectory.Add('9205555555', TSubscriberInfo.Create('Sandra', 'Bullock'));
  // Adding a new subscriber if number already exist
  TelephoneDirectory.AddOrSetValue('9204444444',
                                   TSubscriberInfo.Create('Angelina', 'Jolie'));
  // Print list
  Writeln(PrintTelephoneDirectory(TelephoneDirectory));

  // ---------------------------------------------------
  // 2) Working with the elements

  // Set the "capacity" of the dictionary according to the current number of elements
  TelephoneDirectory.TrimExcess;
  // Is there a key? - ContainsKey
  if TelephoneDirectory.ContainsKey('9205555555') then
    Writeln('Phone 9205555555 registered!');
  // Is there a subscriber? - ContainsValue
  Subscriber := TSubscriberInfo.Create('Sandra', 'Bullock');
  if TelephoneDirectory.ContainsValue(Subscriber) then
    Writeln(Format('%s is in the directory!', [Subscriber.ToString]));
  // Try to get information via telephone. TryGetValue
  if TelephoneDirectory.TryGetValue('9204444444', Subscriber) then
    Writeln(Format('Number 9204444444 belongs to %s', [Subscriber.ToString]));
  // Directly access by phone number
  Writeln(Format('Phone 9201111111 subscribers: %s', [TelephoneDirectory['9201111111'].ToString]));
  // Number of people in the directory
  Writeln(Format('Total subscribers in the directory: %d', [TelephoneDirectory.Count]));

  // ---------------------------------------------------
  // 3) Delete items

  // Schwarzenegger now will not be listed
  TelephoneDirectory.Remove('9201111111');
  // Completely clear the list
  TelephoneDirectory.Clear;

  // ---------------------------------------------------
  //   4) Events add / remove values
  //
  // Events OnKeyNotify OnValueNotify are designed for "tracking"
  // for adding / removing keys and values ​​respectively
  TelephoneDirectory.OnKeyNotify := THashMapEventsHandler.OnKeyNotify;
  TelephoneDirectory.OnValueNotify := THashMapEventsHandler.OnValueNotify;

  Writeln;
  // Try events
  TelephoneDirectory.Add('9201111111', TSubscriberInfo.Create('Arnold', 'Schwarzenegger'));
  TelephoneDirectory.Add('9202222222', TSubscriberInfo.Create('Jessica', 'Alba'));
  TelephoneDirectory['9202222222'] := TSubscriberInfo.Create('Monica', 'Bellucci');
  TelephoneDirectory.Clear;
  WriteLn;

  TelephoneDirectory.Add('9201111111', TSubscriberInfo.Create('Monica', 'Bellucci'));
  TelephoneDirectory.Add('9202222222', TSubscriberInfo.Create('Sylvester', 'Stallone'));
  TelephoneDirectory.Add('9203333333', TSubscriberInfo.Create('Bruce', 'Willis'));
  WriteLn;

  // Show keys (phones)
  Writeln('Keys (phones):');
  for PhoneNumber in TelephoneDirectory.Keys do
    Writeln(PhoneNumber);
  Writeln;

  // Show values ​​(subscribers)
  Writeln('Values (subscribers):');
  for Subscriber in TelephoneDirectory.Values do
    Writeln(Subscriber.ToString);
  Writeln;

  // All together now
  Writeln('Subscribers list with phones:');
  for PhoneNumber in TelephoneDirectory.Keys do
    Writeln(Format('%s: %s',
      [PhoneNumber, TelephoneDirectory[PhoneNumber].ToString]));
  Writeln;

  // In addition, we can "export" from the dictionary
  // to TArray
  // Sort the resulting array and display
  TTelephoneArray := TelephoneDirectory.ToArray;

  // partial specializations not allowed
  // same for anonymous methods
  //TArray.Sort<TPair<string, TSubscriberInfo>>(
  //  TTelephoneArray, TComparer<TPair<string, TSubscriberInfo>>.Construct(
  //    function (const Left, Right: TPair<string, TSubscriberInfo>): Integer
  //    begin
  //      // Comparable full first names, and then phones if necessary
  //      Result := CompareStr(Left.Value.ToString, Right.Value.ToString);
  //      if Result = 0 then
  //        Result := CompareStr(Left.Key, Right.Key);
  //    end));

  TArrayHelper<TelephoneDirectory.TDictionaryPair>.Sort(
    TTelephoneArray, TComparer<TelephoneDirectory.TDictionaryPair>.Construct(
      CustomCompare));
  // Print
  Writeln('Sorted list of subscribers into TArray (by name, and eventually by phone):');
  for TTelephoneArrayItem in TTelephoneArray do
    Writeln(Format('%s: %s',
      [TTelephoneArrayItem.Value.ToString, TTelephoneArrayItem.Key]));

  Writeln;
  FreeAndNil(TelephoneDirectory);

  Readln;
end.

