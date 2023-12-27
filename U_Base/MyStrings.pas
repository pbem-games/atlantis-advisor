unit MyStrings;

{$MODE Delphi}

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Classes, SysUtils;

type
  TVariantArray = array of Variant;
  PVariantArray = ^TVariantArray;

  TDelims = set of char;

  ENaN = class(Exception);

  TTrace = class
  private
    FText: string;
    function GetText: string;
    procedure SetText(s: string);
  public
    StPos, EnPos: integer;
    Separator: string;
    constructor Create(s: string);
    function Before(txt: string): string;
    function Backwards(txt: string): string;
    function Block: string;
    function QBlock: string;
    function NumSpace: integer;
    function Ends: boolean;
    function Num: integer;
    procedure Reset;
    function Word: string;
    procedure SkipSpaces;
    property Text: string read GetText write SetText;
  end;

  TDataParser = procedure(Trace: TTrace);

  // Boolean convertors
  function BoolToInt(b: boolean): integer;
  function BoolToStr(B: boolean): string;
  function BoolToStr0(B: boolean): string;
  function IntToBool(i: integer): boolean;
  function StrToBool(s: string): boolean;
  function ToInt(s: string): integer;
  // String testers
  function IsNumber(s: string): boolean;
  function EmptyLine(s: string): boolean;
  function LeadingSpaces(s: string): integer;
  // Quoted string readers
  function StrQuotes(s: string; var i: integer; Fail: boolean;
    Delims: TDelims): string; overload;
  function StrQuotes(s: string; var i: integer): string; overload;
  function StrParens(s: string; var i: integer; lim1, lim2: char): string;
    overload;
  function StrParens(s: string; var i: integer): string; overload;
  function ParensContents(s: string; var i: integer; lim1, lim2: char): string;
    overload;
  function ParensContents(s: string; var i: integer): string; overload;
  function QuotesContents(s: string; var i: integer): string;
  // Other
  procedure SkipSpaces(s: string; var i: integer);
  function ScriptUncomment(s: string): string;
  function GetScriptLine(Lines: TStrings; var Line, Next: integer): string;
  function MakeList(var A: array of string): string;
  function MyWrapText(Line, BreakStr: string; nBreakChars: TSysCharSet;
    MaxCol: Integer): string;
  function Uncomment(s: string): string;
  function SeekText(Lines: TStrings; var Line: longint; txt: string): boolean;
  function FindInBlock(Lines: TStrings; Line: longint; txt: string): string;
  function ExtractQuotedStr(s: string; Quote: char): string;
  function Localized(BaseDir, Path: string): string;
  function Unlocalize(BaseDir, Path: string): string;
  procedure ReadDataList(ListName: string; Lines: TStrings; line: integer;
    Parser: TDataParser);
  function GetToken(var s: string): string;
  function ScanToken(s: string): string;
  function GetLegal(s: string): string;
  function ClearOrder(s: string): string;
  function Numeric(s: string; amt: integer): string;

implementation

  { Some string handling routines }

function BoolToInt(b: boolean): integer;
begin
  if b then Result := 1 else Result := 0;
end;

function BoolToStr(B: boolean): string;
begin
  if B then Result := '1' else Result := '';
end;

function BoolToStr0(B: boolean): string;
begin
  if B then Result := '1' else Result := '0';
end;

function IntToBool(i: integer): boolean;
begin
  Result := (i <> 0);
end;

function StrToBool(s: string): boolean;
begin
  if (s = '0') or (s = '') then Result := FALSE else Result := TRUE;
end;

// Convert valid numbers, return '' for other
function ToInt(s: string): integer;
begin
  if (s <> '') and IsNumber(s) then Result := StrToInt(s)
  else Result := 0;
end;

function IsNumber(s: string): boolean;
var i: integer;
begin
  if Pos('-', s) = 1 then i := 2
  else i := 1;
  while (i <= Length(s)) and (s[i] in ['0'..'9']) do Inc(i);
  Result := (i > Length(s));
end;

function EmptyLine(s: string): boolean;
begin
  EmptyLine := (Length(Trim(s)) = 0);
end;

function LeadingSpaces(s: string): integer;
var i: integer;
begin
  i := 1;
  while (i <= Length(s)) and (s[i] = ' ') do Inc(i);
  Result := i-1;
end;

// Returns next character or substring inside "", ''
function StrQuotes(s: string; var i: integer; Fail: boolean;
  Delims: TDelims): string;
var delim: char;
begin
  if i > Length(s) then Result := ''
  else begin
    Result := s[i];
    Inc(i);
    if s[i-1] in Delims then begin
      delim := s[i-1];
      while (i <= Length(s)) and (s[i] <> delim) do begin
        Result := Result + s[i];//StrQuotes(s, i, Fail, Delims);
        Inc(i);
      end;
      if i <= Length(s) then begin
        Result := Result + delim;
        Inc(i);
      end
      else if Fail then
        raise EConvertError.Create('Closing quote expected');
    end;
  end;
end;

function StrQuotes(s: string; var i: integer): string;
begin
  Result := StrQuotes(s, i, True, ['"', '''']);
end;

// Returns next character or substring inside (), "", ''
function StrParens(s: string; var i: integer; lim1, lim2: char): string;
var level: integer;
begin
  Result := '';
  level := 0;
  while (i <= Length(s)) and not ((level = 0) and (Result <> '')) do begin
    if s[i] = lim1 then Inc(level);
    if s[i] = lim2 then Dec(level);
    Result := Result + StrQuotes(s, i);
  end;
  if level <> 0 then
    raise EConvertError.Create('Brackets not match');
end;

function StrParens(s: string; var i: integer): string;
begin
  Result := StrParens(s, i, '(', ')');
end;

// Returns contents of delimeters (), "", '' without delimeters
function ParensContents(s: string; var i: integer; lim1, lim2: char): string;
begin
  Result := StrParens(s, i, lim1, lim2);
  if Length(Result) > 1 then Result := Copy(Result, 2, Length(Result) - 2);
end;

function ParensContents(s: string; var i: integer): string;
begin
  Result := ParensContents(s, i, '(', ')');
end;

function QuotesContents(s: string; var i: integer): string;
begin
  Result := StrQuotes(s, i);
  if Length(Result) > 1 then Result := Copy(Result, 2, Length(Result) - 2);
end;

procedure SkipSpaces(s: string; var i: integer);
begin
  while (i <= Length(s)) and (s[i] = ' ') do Inc(i);
end;

function ScriptUncomment(s: string): string;
var i: integer;
    squotes, quotes: boolean;
begin
  if Pos('//', s) > 0 then begin
    i := 1;
    quotes := False; // not using StrQuotes to avoid exceptions
    squotes := False;
    while (i <= Length(s)-1) do begin
      if s[i] = '''' then squotes := not squotes;
      if s[i] = '"' then quotes := not quotes;
      if not quotes and not squotes and (s[i] = '/') and (s[i+1] = '/') then
        s := Copy(s, 1, i-1);
      Inc(i);
    end;
  end;
  Result := s;
end;

function GetScriptLine(Lines: TStrings; var Line, Next: integer): string;
begin
  Result := Trim(Lines[Line]);
  Next := Line;
  while (Result <> '') and (Result[Length(Result)] = '_')
    and (Next < Lines.Count) do begin
    Result[Length(Result)] := ' ';
    Inc(Next);
    Result := Result + TrimRight(Lines[Next]);
  end;
  Result := ScriptUncomment(Result);
  Inc(Line);
  Inc(Next);
end;

function IsLegal(c: Char): boolean;
begin
  Result := (c in ['a'..'z', 'A'..'Z', '0'..'9', '!', '[', ']', ',',
    '.', ' ', '{', '}', '@', '#', '$', '%', '^', '&', '*', '-', '_',
    '+', '=', ';', ':', '<', '>', '?', '/', '~', '''', '\', '`']);
end;

function GetLegal(s: string): string;
var i: integer;
begin
  Result := '';
  for i := 1 to Length(s) do
    if IsLegal(s[i]) then Result := Result + s[i];
end;

function GetToken(var s: string): string;
var i, st, en: integer;
begin
  Result := '';
  st := 1;
  while (st <= Length(s)) and ((s[st] = ' ') or (s[st] = #09)) do Inc(st);
  if (st > Length(s)) or (s[st] = ';') then s := ''
  else begin
    en := st + 1;
    if s[st] = '"' then begin
      while (en <= Length(s)) and (s[en] <> '"') do Inc(en);
      Inc(st);
    end
    else while (en <= Length(s)) and (s[en] <> ' ') and (s[en] <> #09)
      and (s[en] <> ';') do Inc(en);
    Result := Copy(s, st, en - st);
    if Copy(s, en, 1) = ';' then s := '' else s := Copy(s, en+1, Length(s) - en);
  end;
  for i := 1 to Length(Result) do
    if Result[i] = '_' then Result[i] := ' ';
end;

// Same than GetToken, but don't modify string
function ScanToken(s: string): string;
begin
  Result := GetToken(s);
end;

function ClearOrder(s: string): string;
begin
  Result := LowerCase(GetToken(s));
  if Copy(Result, 1, 1) = '@' then Result := Copy(Result, 2, Length(Result));
end;

function MakeList(var A: array of string): string;
var i: integer;
begin
  Result := '';
  if High(A) >= 0 then begin
    for i := Low(A) to High(A)-1 do Result := Result + A[i] + ', ';
    Result := Result + A[High(A)] + '.';
  end
  else Result := Result + '.';
end;

function UnLocalize(BaseDir, Path: string): string;
begin
  if ExtractFileDrive(Path) = '' then
    Result := IncludeTrailingBackslash(BaseDir) + Path
  else Result := Path;
end;

procedure ReadDataList(ListName: string; Lines: TStrings; line: integer;
  Parser: TDataParser);
var Trace: TTrace;
begin
  Trace := TTrace.Create('');
  if SeekText(Lines, line, ListName) then begin
    Inc(line);
    while (line < Lines.Count) and not EmptyLine(Lines[line]) do begin
      Trace.Text := Lines[line];
      Parser(Trace);
      Inc(line);
    end;
  end;
  Trace.Free;
end;

function Localized(BaseDir, Path: string): string;
begin
  BaseDir := IncludeTrailingBackslash(BaseDir);
  if Pos(BaseDir, ExtractFilePath(Path)) = 1 then
    Result := Copy(Path, Length(BaseDir) + 1, Length(Path))
  else Result := Path;
end;

function MyWrapText(Line, BreakStr: string; nBreakChars: TSysCharSet;
  MaxCol: Integer): string;
var i: integer;
begin
  for i := 1 to Length(Line) do
    if Line[i] = '''' then Line[i] := '`';
  Result := WrapText(Line, BreakStr, nBreakChars, MaxCol);
  for i := 1 to Length(Result) do
    if Result[i] = '`' then Result[i] := '''';
end;

function ExtractQuotedStr(s: string; Quote: char): string;
var i: integer;
begin
  if (s <> '') then begin
    if s[1] = Quote then s := Copy(s, 2, Length(s) - 1);
    if s[Length(s)] = Quote then s := Copy(s, 1, Length(s) - 1);
    i := 1;
    while i < Length(s) do begin
      if (s[i] = Quote) and (s[i+1] = Quote) then
        s := Copy(s, 1, i) + Copy(s, i+2, Length(s)-i-1);
      Inc(i);
    end;
  end;
  Result := s;
end;

function SeekText(Lines: TStrings; var Line: longint; txt: string): boolean;
var CurrPos: longint;
begin
 CurrPos := Line;
 while (Line < Lines.Count) and (Pos(txt, Lines[Line]) = 0) do Inc(Line);
 if Line = Lines.Count then begin
  Line := CurrPos;
  Result := FALSE;
 end
 else Result := TRUE;
end;

function FindInBlock(Lines: TStrings; Line: longint; txt: string): string;
begin
 while (Line < Lines.Count) and (Pos(txt, Lines[Line]) = 0)
   and not EmptyLine(Lines[Line]) do Inc(Line);
 if Line = Lines.Count then Result := ''
 else Result := Lines[Line];
end;

function Uncomment(s: string): string;
var i: integer;
begin
  Result := '';
  i := 1;
  while (i <= Length(s)) and (s[i] <> ';') do
    Result := Result + StrQuotes(s, i, False, ['"']);
end;

 { TTrace methods }

constructor TTrace.Create(s: string);
begin
  SetText(s);
end;

function TTrace.GetText: string;
begin
 GetText := Copy(FText, StPos, EnPos - StPos + 1);
end;

procedure TTrace.SetText(s: string);
begin
  FText := s;
  Reset;
end;

procedure TTrace.Reset;
begin
  StPos := 1;
  EnPos := Length(FText);
end;

function TTrace.Ends;
begin
  Ends := (StPos > EnPos);
end;

 { Read number }
function TTrace.Num: integer;
var i: integer;
    snum: string;
begin
  Separator := '';
  try
    if Text = '' then
      raise ENaN.Create('Unexpected end of line in "' + FText + '"');
    i := 1;
    snum := '';
    if (Text[i] = '-') or (Text[i] = '+') then begin
      snum := snum + Text[i];
      Inc(i);
    end;
    while (i <= Length(Text)) do begin
     if ((Text[i] >= '0') and (Text[i] <= '9')) then snum := snum + Text[i]
     else break;
     Inc(i);
    end;
    if (snum = '') then
      raise ENaN.Create('Number expected, but not found in "'+FText+'"');
    Result := StrToInt(snum);
    StPos := StPos + Length(snum);
  except
    on E: ENaN do raise;
    on Exception do raise ENaN.Create('Number expected, but not found in "'+FText+'"');
  end;
end;

 { Read string until meet given string }
function TTrace.Before(txt: string): string;
var txtpos: integer;
begin
 Separator := '';
 if (txt <> '') then txtpos := Pos(txt,Text) else txtpos := Length(Text)+1;
 if (txtpos = 0) then txtpos := Length(Text)+1;
 Before := Copy(Text,1,txtpos-1);
 StPos := StPos + txtpos-1 + Length(txt);
end;

function TTrace.Backwards(txt: string): string;
var txtpos, text_len: integer;
begin
 Separator := '';
 text_len := Length(Text);
 txtpos := text_len - Length(txt) + 1;
 if (txt <> '') then begin
   while (txtpos >= 1) and (Pos(txt, Copy(Text, txtpos, text_len - txtpos + 1)) = 0)
     do Dec(txtpos);
   if Pos(txt, Copy(Text, txtpos, text_len - txtpos + 1)) = 0 then
     txtpos := Length(Text)+1;
 end
 else txtpos := Length(Text)+1;
 Result := Copy(Text, txtpos + Length(txt), text_len - txtpos + Length(txt));
 EnPos := StPos + txtpos - 2;
end;

 { Read symbols until ',' '.' ' ' chars meet. }
function TTrace.Word: string;
var i: integer;
begin
 Separator := '';
 i := 1;
 while i <= Length(Text) do begin
  if (Text[i] = ',') or (Text[i] = ' ') or (Text[i] = '.') then break
  else Inc(i);
 end;
 Word := Copy(Text, 1, i-1);
 StPos := StPos + i-1;
end;

procedure TTrace.SkipSpaces;
begin
  while Copy(Text, 1, 1) = ' ' do Inc(StPos);
end;

 { Read until ', ', ';' or '.' meets. }
function TTrace.Block: string;
var i: integer;
    s: string;
begin
 s := Text + ' ';
 i := 1;
 Separator := '';
 while i < Length(s) do begin
  if ((s[i] = ',') and (s[i+1] = ' ')) or (s[i] = '.') or (s[i] = ';')
    then begin
      Separator := s[i];
      break;
    end
  else Inc(i);
 end;
 Block := Copy(s, 1, i-1);
 StPos := StPos + i+1;
end;

 { Same as block, but strips " if found }
function TTrace.QBlock: string;
var i: integer;
    s: string;
begin
 s := Text + ' ';
 i := 1;
 if s[1] = '"' then begin
   s := Copy(s, 2, Length(s)-1);
   while (i < Length(s)) and (s[i] <> '"') do Inc(i);
   Result := Copy(s, 1, i-1);
   StPos := StPos + i+1;
   Block;
 end
 else Result := Block;
end;

function TTrace.NumSpace: integer;
begin
  Result := StrToInt(Before(' '));
end;

function Numeric(s: string; amt: integer): string;
begin
  if amt = 1 then Result := s
  else Result := s + 's';
end;



end.
