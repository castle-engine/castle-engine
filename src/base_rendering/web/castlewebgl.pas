{
  Copyright 2024-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
{ Access WebGL API from Castle Game Engine compiled in WebAssembly. }
unit CastleWebGL;

interface

procedure TestJobWeb;

implementation

uses JOB.Shared, JOB_Web, JOB.JS;

procedure TestJobWeb;
var
  JSDiv: IJSHTMLDivElement;
  JSButton: IJSHTMLButtonElement;
begin
  writeln('TestJobWeb getElementById "pas2js-console" ...');
  // get reference of HTML element "pas2js-console" and type cast it to Div
  // note: we over-use pas2js-console div, just for test
  JSDiv:=TJSHTMLDivElement.Cast(JSDocument.getElementById('pas2js-console'));

  // create button
  writeln('TestJobWeb create button ...');
  JSButton:=TJSHTMLButtonElement.Cast(JSDocument.createElement('button'));
  writeln('TestJobWeb set button caption ...');
  JSButton.InnerHTML:='Click me!';

  // add button to div
  writeln('TestJobWeb add button to div ...');
  JSDiv.append(JSButton);

  writeln('TestJobWeb END');
end;

end.
