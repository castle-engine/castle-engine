{%MainUnit castlecontrols.pas}


{$ifdef read_interface}


TMemoUndoActionType=(atAdd,atDelete,atDeleteLine);


TMemoUndoAction=record
 Index,IndexColumn:Integer;
 LastIndex,LastIndexColumn:Integer;
 Text:string;
 Action:TMemoUndoActionType;
end;


{ TMemoUndo }

TMemoUndo = class ;
 {$endif read_implementation}
