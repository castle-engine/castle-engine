{
  VampyreDoc Tool
  by Marek Mauder 
  http://imaginglib.sourceforge.net

  The contents of this file are used with permission, subject to the Mozilla
  Public License Version 1.1 (the "License"); you may not use this file except
  in compliance with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1.1.html

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  Info:
    - currently only for Delphi 
}

{
  TODOS:
    - handle doc files with the same names but in different directories
      (they are overwritten now in producers with the next file)
    * solve url link targets - copy existing local refered files to output?
    - crosslink output with generated reference
    - add new mode: create toc XML file from hierarchy of crosslinked documents

    XHTML Producer:
      - remove frames from XHTML documentation and make output like texi2html

    CHM Producer:
      * relative links does not work in compiled files (see Root.xml's logo and
        stylesheets). Solve: put all existing referenced files to one dir,
        handle naming conflicts (i.e. path delims to dots) and set hhp option
        Flat=True
      - add table of contents page and set root as separate page
        outside toc tree

}

program VampDoc;

{$APPTYPE CONSOLE}

{$I ImagingOptions.inc}

uses
  SysUtils,
  Classes,
  Helpers,
  ImagingUtility,
  DemoUtils,
  DOM,
  XMLRead,
  XHTMLProducer,
  CHMProducer;

const
  sTitle = 'VampyreDoc';
  sVersion = '0.12';
  sAppTitle = sTitle + ' version ' + sVersion + sLineBreak +
    'Copyright (c) 2004-2005 Marek Mauder' + sLineBreak;

type
  TDocAction = (daGenDoc, daGenTOC, daGenTemplates, daHelp);

var
  Action: TDocAction = daGenDoc;
  Project: TDocProject = nil;
  Producer: TDocProducer = nil;
  ProducerFormat: string = 'xhtml';//}'htmlhelp';
  OutputDir: string = {'doc';//}'..\Doc\_Final';
  InputFile: string = '..\Doc\VampyreDoc\Imaging.vdocproj';

procedure ErrorProject(Exc: Exception);
begin
  WriteLn('Error - when loading VampyreDoc project "' + InputFile +
    '". ' + sLineBreak + 'Exception Message: ' + Exc.Message + sLineBreak);
  Halt(1);
end;

procedure ErrorProducer;
begin
  WriteLn('Error - unknown documentation producer selected "' + ProducerFormat +
    '".' + sLineBreak);
  Halt(1);
end;

procedure ErrorProcessing(Exc: Exception);
begin
  WriteLn('Error - when processing documentation project "' + InputFile +
    '" with producer: ' + Producer.Name + '.' + sLineBreak +
    'Exception Message: ' + Exc.Message + sLineBreak);
  Halt(1);
end;

procedure ErrorTemplates(Exc: Exception);
begin
  WriteLn('Error - when generating TOC item templates for project "'
    + InputFile + '".' + sLineBreak +
    'Exception Message: ' + Exc.Message + sLineBreak);
  Halt(1);
end;

function LoadProject(const FileName: string): TDocProject;
var
  Doc: TXMLDocument;
begin
  Result := nil;
  WriteLn('Loading project: ' + FileName);
  try
    XMLRead.ReadXMLFile(Doc, InputFile);
    Result := TDocProject.Create(Doc.DocumentElement, FileName);
    Doc.Free;
  except
    ErrorProject(GetExceptObject);
  end;
  WriteLn('Project successfuly loaded.');
end;

procedure PrintHelp;
begin
  WriteLn('Help:');
  WriteLn;
  WriteLn('VampDoc [action] [format] [input] [output]');
  WriteLn;
  WriteLn('action:');
  WriteLn('  -doc        generate documentation from .vdocproj file on input');
  WriteLn('  -toc        generate TOC file from crosslinked documents (not functional now)');
  WriteLn('  -templates  generate template documents based on TOC file');
  WriteLn('  -help|-h    show this scren');
  WriteLn('format (only for -doc action):');
  WriteLn('  -f=xhtml    set output format to XHTML');
  WriteLn('  -f=htmlhelp set output format to HTMLHelp (requires HTMLHelp compiler)');
  WriteLn('input:');
  WriteLn('  -i=filename input file must be valid .vdocproj file (for -doc and -templates)');
  WriteLn('              or VampyreDoc XML file (for -toc)');
  WriteLn('output:');
  WriteLn('  -o=dir      root output directory for all generated files');
  WriteLn;
  WriteLn('Order of parameters is actually not important.');
  WriteLn('If any of the parameters is missing default ugly value is used.');
  WriteLn('See Vampyre Imaging Library documentation for more details about VampyreDoc.');
  WriteLn;
end;

procedure ParseOption(const Option: string);
var
  S, Arg: string;
  I: LongInt;
begin
  S := Option;
  if S = '-toc' then
    Action := daGenTOC
  else
  if S = '-doc' then
    Action := daGenDOC
  else
  if S = '-templates' then
    Action := daGenTemplates
  else
  if (S = '-h') or (S = '-help') then
    Action := daHelp
  else
  begin
    I := Pos('=', S);
    Arg := Copy(S, I + 1, MaxInt);
    Delete(S, I, MaxInt);
    if S = '-i' then
      InputFile := Arg
    else
    if S = '-o' then
      OutputDir := Arg
    else
    if S = '-f' then
      ProducerFormat := Arg;  
  end;
end;

procedure ParseCmdLine;
var
  I: LongInt;
begin
  for I := 1 to ParamCount do
    ParseOption(ParamStr(I));
end;

begin
  ParseCmdLine;
  WriteLn(sAppTitle);

  case Action of
    daGenDoc:
      begin
        WriteLn('Action: Generate Documentation');
        Project := LoadProject(InputFile);

        if ProducerFormat = 'xhtml' then
          Producer := TXHTMLProducer.Create
        else
          if ProducerFormat = 'htmlhelp' then
            Producer := TCHMProducer.Create
          else
            ErrorProducer;

        WriteLn('Project processing started with producer: ' + Producer.Name);
        try
          OutputDir := ExpandFileTo(OutputDir, GetCurrentDir);
          Producer.Process(Project, OutputDir);
        except
          ErrorProcessing(GetExceptObject);
        end;
        WriteLn('Documentation produced in directory: ' + OutputDir);
        Project.Free;
      end;
    daGenTemplates:
      begin
        WriteLn('Action: Generate Templates');
        Project := LoadProject(InputFile);
        try
          GenerateTOCTemplates(Project);
        except
          ErrorTemplates(GetExceptObject);
        end;
        WriteLn('TOC item templates successfuly generated.');
        Project.Free;
      end;
    daGenTOC: PrintHelp;
    daHelp: PrintHelp;
  end;
  WriteLn;
end.


