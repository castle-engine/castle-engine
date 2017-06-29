// Generic types for FreeSparta.com and FreePascal!
// Original version by keeper89.blogspot.com, 2011
// FPC version by Maciej Izak (hnb), 2014

program TQueueProject;

{$MODE DELPHI}
{$APPTYPE CONSOLE}

uses
  SysUtils, Generics.Collections;

type
  // This is FreeSpaaarta! versions =)
  TSpartaVersion = (svFreeSparta, svBasic, svStarter, svProfessional);

  TCustomer = record
    strict private
      const
        SV_NAMES: array [TSpartaVersion] of string =
          ('FreeSparta', 'Basic', 'Starter', 'Professional');
    public
      var
        SpartaVersion: TSpartaVersion;
    class function Create(SpartaVersion: TSpartaVersion): TCustomer; static;
    function ToString: string;
  end;

class function TCustomer.Create(SpartaVersion: TSpartaVersion): TCustomer;
begin
  Result.SpartaVersion := SpartaVersion;
end;

function TCustomer.ToString: string;
begin
  Result := Format('Sparta %s', [SV_NAMES[SpartaVersion]])
end;

var
  CustomerQueue: TQueue<TCustomer>;
  Customer: TCustomer;
begin
  WriteLn('Working with TQueue - buy FreeSparta.com');
  WriteLn;

  // "Create" turn in sales
  CustomerQueue := TQueue<TCustomer>.Create;

  // Add a few people in the queue
  // Enqueue - puts the item in the queue
  CustomerQueue.Enqueue(TCustomer.Create(svFreeSparta));
  CustomerQueue.Enqueue(TCustomer.Create(svBasic));
  CustomerQueue.Enqueue(TCustomer.Create(svBasic));
  CustomerQueue.Enqueue(TCustomer.Create(svBasic));
  CustomerQueue.Enqueue(TCustomer.Create(svStarter));
  CustomerQueue.Enqueue(TCustomer.Create(svStarter));
  CustomerQueue.Enqueue(TCustomer.Create(svProfessional));
  CustomerQueue.Enqueue(TCustomer.Create(svProfessional));

  // Part of customers served
  // Dequeue - remove an element from the queue
  // btw if TQueue is TObjectQueue also call Free for object
  Customer := CustomerQueue.Dequeue;
  Writeln(Format('Sold (Dequeue): %s', [Customer.ToString]));
  // Extract - similar to Dequeue, but causes in OnNotify
  // Action = cnExtracted instead cnRemoved
  Customer := CustomerQueue.Extract;
  Writeln(Format('Sold (Extract): %s', [Customer.ToString]));

  // For what came next buyer?
  // Peek - returns the first element, but does not remove it from the queue
  Writeln(Format('Serves customers come for %s',
                 [CustomerQueue.Peek.ToString]));

  // The remaining buyers
  Writeln;
  Writeln(Format('Buyers left: %d', [CustomerQueue.Count]));
  for Customer in CustomerQueue do
    Writeln(Customer.ToString);

  // We serve all
  // Clear - clears the queue
  CustomerQueue.Clear;

  FreeAndNil(CustomerQueue);

  Readln;
end.

