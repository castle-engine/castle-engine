program tczstreamseek;
{$MODE OBJFPC}
{$ASSERTIONS ON}

uses
  classes,
  zstream;

const
  val: Uint32 = $123456;
  wasError: boolean = False;
var
  data: TMemoryStream;
  comprStream: TCompressionStream;
  decomprStream: TDecompressionStream;
begin
  data := TMemoryStream.Create();

  comprStream := TCompressionStream.Create(clMax, data);
  comprStream.WriteDWord(val);
  comprStream.Free;

  data.Seek(0, soFromBeginning);

  decomprStream := TDecompressionStream.Create(data);
  Assert(decomprStream.ReadDWord() = val);
  Assert(decomprStream.Position = SizeOf(val));

  decomprStream.Seek(0, soFromBeginning);
  Assert(decomprStream.Position = 0);
  Assert(decomprStream.ReadDWord() = val);

  decomprStream.Seek(-SizeOf(val), soFromCurrent);
  Assert(decomprStream.Position = 0);
  Assert(decomprStream.ReadDWord() = val);

  wasError := False;
  decomprStream.Seek(0, soFromBeginning);
  try
    decomprStream.Seek(-SizeOf(val), soFromCurrent);
  except
    on EDecompressionError do
      wasError := True;
  end;
  assert(wasError);

  decomprStream.Seek(SizeOf(val), soFromBeginning);
  Assert(decomprStream.Position = SizeOf(val));

  wasError := False;
  try
    decomprStream.Seek(40, soFromBeginning);
  except
    on EDecompressionError do
      wasError := True;
  end;
  assert(wasError);
end.
