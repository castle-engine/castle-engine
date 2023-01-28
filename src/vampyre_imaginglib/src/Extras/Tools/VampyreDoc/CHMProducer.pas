unit CHMProducer;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Classes, Helpers, DemoUtils, DOM;

const
  sCHMStyleSheet = 'doc2html.xsl';
  sCHMTOCStyleSheet = 'toc2hhc.xsl';
  sCHMProjectStyleSheet = 'proj2hhp.xsl';
  sCHMExt = 'html';
  sCHMProjectExt = 'hhp';
  sCHMTOCExt = 'hhc';
  sCHMDir = 'chm';
  sCHMDelim = '/';
  sTempFile = 'temp.xml';
  sDocTempFile = 'doctemp.xsl';
  sXFile = 'file';

type
  TCHMProducer = class(TDocProducer)
  private
    FOutDir: string;
    FRootDir: string;
    FProject: TDocProject;
    FDocTempPath: string;
    procedure CreateContents;
    procedure CreateProject(Files: TStrings);
    procedure ConvertPagesToHTML(Item: TContentItem);
    function CreateProducerData(Files: TStrings; Doc: TDOMDocument): TDOMElement;
  public
    constructor Create;
    procedure Process(Project: TDocProject; const OutDir: string); override;
  end;

function CreateCHMLinker: TLinkChecker;

implementation

function CheckContext(const S, Context: string): string;
begin
  if SameText(Context, sXTOCFile) then
    Result := ChangeFileExt(S, '.' + sCHMTOCExt)
  else
    Result := S;  
end;

function CreateCHMLinker: TLinkChecker;
begin
  Result := TLinkChecker.Create;
  Result.StripDir := True;
  Result.Extension := sCHMExt;
  Result.NewPathDelim := PathDelim;
  Result.CustomConverter := CheckContext;
end;

{ TCHMProducer }

constructor TCHMProducer.Create;
begin
  inherited Create;
  FName := 'HTMLHelp Doc Producer';
end;

function TCHMProducer.CreateProducerData(Files: TStrings; Doc: TDOMDocument): TDOMElement;
var
  Elem: TDOMElement;
  Text: TDOMText;
  I: LongInt;
begin
  Result := Doc.CreateElement(sXProducer);
  for I := 0 to Files.Count - 1 do
  begin
    Elem := Doc.CreateElement(sXFile);
    Text := Doc.CreateTextNode(Files[I]);
    Elem.AppendChild(Text);
    Result.AppendChild(Elem);
  end;
end;

procedure TCHMProducer.CreateContents;
var
  Input, Style, Output: string;
  Linker: TLinkChecker;
begin
  Msg('Creating HTMLHelp contents file .hhc ...');
  Input := FProject.ContentsFile;
  Style := FProject.XslDir + PathDelim + sCHMTOCStyleSheet;
  Output := FOutDir + PathDelim + sCHMDir + PathDelim +
    ExtractFileName(ChangeFileExt(FProject.ContentsFile, '.' + sCHMTOCExt));

  Linker := CreateCHMLinker;
  Linker.CheckDocument(Input, FProject);
  Linker.SaveResult(FOutDir + PathDelim + sTempFile);

  TransformDoc(FOutDir + PathDelim + sTempFile, Output, Style);

  Linker.DeleteResult;
  Linker.Free;
end;

procedure TCHMProducer.CreateProject(Files: TStrings);
var
  Input, Style, Output: string;
  Linker: TLinkChecker;
begin
  Msg('Creating HTMLHelp project file .hhp ...');
  Input := FProject.ProjectFile;
  Style := FProject.XslDir + PathDelim + sCHMProjectStyleSheet;
  Output := FOutDir + PathDelim + sCHMDir + PathDelim +
    FProject.OutputFile + '.' + sCHMProjectExt;

  Linker := CreateCHMLinker;
  Linker.CheckDocument(Input, FProject);
  Linker.Doc.DocumentElement.AppendChild(CreateProducerData(Files, Linker.Doc));
  Linker.SaveResult(FOutDir + PathDelim + sTempFile);

  TransformDoc(FOutDir + PathDelim + sTempFile, Output, Style);

  Linker.DeleteResult;
  Linker.Free;
end;

procedure TCHMProducer.ConvertPagesToHTML(Item: TContentItem);
var
  I: LongInt;
  Input, Output: string;
  Linker: TLinkChecker;
begin
  Input := ExpandFileTo(Item.URL, FRootDir);
  Output := FOutDir + PathDelim + sCHMDir + PathDelim +
    ChangeFileExt(ExtractFileName(Item.URL), '.' + sCHMExt);

  Linker := CreateCHMLinker;
  Linker.IntendedOutput := Output;
  Linker.CheckDocument(Input, FProject);
  Linker.SaveResult(FOutDir + PathDelim + sTempFile);

  TransformDoc(FOutDir + PathDelim + sTempFile, Output, FDocTempPath);

  Linker.DeleteResult;
  Linker.Free;

  for I := 0 to Item.ChildCount - 1 do
    ConvertPagesToHTML(Item[I]);
end;

procedure TCHMProducer.Process(Project: TDocProject; const OutDir: string);
var
  Style, CmdLine: string;
  Linker: TXSLLinker;
begin
  FOutDir := OutDir;
  FProject := Project;
  FRootDir := ExtractFileDir(FProject.ProjectFile);
  
  if (not ForceDirectories(FOutDir)) or
    (not ForceDirectories(FOutDir + PathDelim + sCHMDir)) then
    raise Exception.Create('Cannot create output directories.');

  CreateContents;

  FDocTempPath := FOutDir + PathDelim + sDocTempFile;
  Style := FProject.XslDir + PathDelim + sCHMStyleSheet;
  Linker := TXSLLinker.Create;
  Linker.IntendedOutput := FOutDir + PathDelim + sCHMDir + PathDelim + 'bla.blo';
  Linker.CheckDocument(Style, FProject);
  Linker.SaveResult(FDocTempPath);

  CreateProject(Linker.References);
  Msg('Converting pages to HTML ...');
  ConvertPagesToHTML(FProject.Contents);

  Linker.DeleteResult;
  Linker.Free;

  Msg('Compiling HTMLHelp project file .hhp ...');
  CmdLine := 'hhc ' + FOutDir + PathDelim + sCHMDir + PathDelim +
    FProject.OutputFile + '.' + sCHMProjectExt;
  if not RunCmdLine(CmdLine) then
    raise Exception.Create('HTMLHelp compiler cannot be executed.');
end;

end.
