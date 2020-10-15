{%MainUnit castlecontrols.pas}


{$ifdef read_interface}


TMemoUndoActionType=(atAdd, atDelete, atDeleteLine, atEnter);


TMemoUndoAction = record
 Index, IndexColumn, Index1, IndexColumn1: Integer;
 LastIndex, LastIndexColumn, LastIndex1, LastIndexColumn1: Integer;
 Text, Text1: string;
 ActionType, ActionType1: TMemoUndoActionType;
end;

TMemoUndoActPointer = ^TMemoUndoAction;
{ TMemoUndo }

TMemoUndo = class ;
 {$endif read_implementation}
