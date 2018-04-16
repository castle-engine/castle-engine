{
  Copyright 2018 Benedikt Magnus.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  type
    generic TGenericCollection<T> = class(TCollection)
    private
      function GetItems(AIndex: Integer): T;
      procedure SetItems(AIndex: Integer; AValue: T);
    public
      constructor Create;
      function Add: T;
      property Items[AIndex: Integer]: T read GetItems write SetItems; default;
    end;

{$endif read_interface}

{$ifdef read_implementation}

  function TGenericCollection.GetItems(AIndex: Integer): T;
  begin
    Result := T(inherited Items[AIndex]);
  end;
  
  procedure TGenericCollection.SetItems(AIndex: Integer; AValue: T);
  begin
    Items[AIndex].Assign(AValue);
  end;
  
  constructor TGenericCollection.Create;
  begin
    inherited Create(T);
  end;
  
  function TGenericCollection.Add: T;
  begin
    Result := T(inherited Add);
  end;

{$endif read_implementation}