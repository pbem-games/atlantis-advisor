unit uScriptEdit;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DataStructs, uScript, StdCtrls, ComCtrls, Resources,
  MemoEx, MyStrings, Math, ExtCtrls, uInterface, ToolWin;

type
  TScriptEditForm = class(TForm)
    Edit: TMemoEx;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    btnSave: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure EditGetLineAttr(Sender: TObject; const Line: String;
      Index: Integer; const SelAttrs: TSelAttrs; var Attrs: TLineAttrs);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EditKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditPaintGutter(Sender: TObject; Canvas: TCanvas);
    procedure btnSaveClick(Sender: TObject);
  private
    { Private declarations }
  public
    function Modified: boolean;
  end;

var
  ScriptEditForm: TScriptEditForm;

implementation

{$R *.lfm}

const
  styleNormal = 0;
  styleCommand = 1;
  styleDirective = 2;
  styleComment = 3;
  styleOutput = 4;
  styleCount = 5;

  CmdCount = 9;
  Commands: array[0..CmdCount-1] of string =
    ('if', 'else', 'end', 'while', 'for', 'with', 'clear', 'exec', 'do');
  DirCount = 3;
  Directives: array[0..DirCount-1] of string =
    ('#script', '#macro', '#insert');

var
  Styles: array [0..styleCount-1] of TLineAttr;


procedure SetDefaultTextStyle(ATextStyle: integer; FC, BC: TColor; Style: TFontStyles; ExStyle: byte);
begin
  Styles[ATextStyle].FC := FC;
  Styles[ATextStyle].BC := BC;
  Styles[ATextStyle].Style := Style;
  Styles[ATextStyle].ex_style := ExStyle;
end;

procedure TScriptEditForm.FormCreate(Sender: TObject);
begin
  // Define highlight styles
  SetDefaultTextStyle(styleNormal, clWindowText, clWindow, [], 0);
  SetDefaultTextStyle(styleCommand, clWindowText, clWindow, [fsBold], 0);
  SetDefaultTextStyle(styleDirective, clBlue, clWindow, [], 0);
  SetDefaultTextStyle(styleOutput, clTeal, clWindow, [], 0);
  SetDefaultTextStyle(styleComment, clMedGray, clWindow, [], 0);

  // Open scripts
  if FileExists(BaseDir + Game.Name + '\scripts.txt') then
    Edit.Lines.LoadFromFile(BaseDir + Game.Name + '\scripts.txt');

  // Setup window
  Edit.SetLeftTop(0, Config.ReadInteger('ScriptEditor', 'TopRow', 0));
  LoadFormPosition(Self);
end;

procedure TScriptEditForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Config.WriteInteger('ScriptEditor', 'TopRow', Edit.TopRow);
  SaveFormPosition(Self);
  if Edit.Modified then
    Edit.Lines.SaveToFile(BaseDir + Game.Name + '\scripts.txt');
end;

procedure TScriptEditForm.EditGetLineAttr(Sender: TObject;
  const Line: String; Index: Integer; const SelAttrs: TSelAttrs;
  var Attrs: TLineAttrs);
var i, j, k, st: integer;
    t: string;
    Style: integer;
    s, aline: string;
begin
  st := 1;
  aLine := Line;
  // Form real line from wrapped
  j := Index;
  while (j > 0) do begin
    s := TrimRight(Edit.Lines[j-1]);
    if (Length(s) > 0) and (s[Length(s)] = '_') then begin
      s[Length(s)] := ' ';
      aLine := s + aLine;
      Inc(st, Length(s));
      Dec(j);
    end
    else Break;
  end;

  i := 1;
  while (i <= Length(aLine)) and (aLine[i] = ' ') do Inc(i);
  // First token in line
  t := '';
  j := i;
  while (i <= Length(aLine)) and (aLine[i] in ['0'..'9', 'A'..'Z',
    'a'..'z', '_', '#', '[', ']']) do begin
    t := t + aLine[i];
    Inc(i);
  end;
  // Define style
  Style := -1;
  k := 0;
  t := LowerCase(t);
  while (k < CmdCount) and (Commands[k] <> t) do Inc(k);
  if k < CmdCount then Style := styleCommand;
  if Style = -1 then begin
    k := 0;
    while (k < DirCount) and (Directives[k] <> t) do Inc(k);
    if k < DirCount then Style := styleDirective
    else begin
      while (i <= Length(aLine)) and (aLine[i] = ' ') do Inc(i);
      if (i <= Length(aLine)) and (aLine[i] = '=') then Style := styleNormal;
    end;
  end;
  // Apply style
  if i >= st then begin
    i := Max(i, st) - st;
    j := Max(j, st) - st;
    if Style >= 0 then
      for k := j to i do Attrs[k] := Styles[Style];
  end;
  // Output
  j := Length(ScriptUncomment(aLine));
  if Style = -1 then
    for i := st to j do Attrs[i-st] := Styles[styleOutput];
  // Comments
  for i := Max(st, j + 1) to Length(aLine) do
    Attrs[i-st] := Styles[styleComment];
end;

procedure TScriptEditForm.EditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 13 then
    Edit.Lines[Edit.CaretY] := TrimRight(Edit.Lines[Edit.CaretY]);
end;

procedure TScriptEditForm.EditPaintGutter(Sender: TObject;
  Canvas: TCanvas);
var ln, nr, i, y: integer;

  procedure NextLine;
  var s: string;
      i, st: integer;
  begin
    Inc(ln);
    if (ln < Edit.Lines.Count) and ((Pos('#script', TrimLeft(Edit.Lines[ln]))
      = 1) or (Pos('#macro', TrimLeft(Edit.Lines[ln])) = 1)) then nr := 1
    else if Pos('#insert', TrimLeft(Edit.Lines[ln])) = 1 then begin
      s := TrimLeft(StringReplace(Edit.Lines[ln], '#insert', '#macro', []));
      i := 0;
      while (i < ln) and (Pos(s, TrimLeft(Edit.Lines[i])) <> 1) do Inc(i);
      if i < ln then begin
        st := i;
        Inc(i);
        while (i < ln) and not ((Pos('#script', TrimLeft(Edit.Lines[i]))
          = 1) or (Pos('#macro', TrimLeft(Edit.Lines[i])) = 1)) do Inc(i);
        nr := nr + i - st - 1;
      end;
    end
    else Inc(nr);
  end;

begin
  ln := 0;
  nr := 1;
  while ln < Edit.TopRow do NextLine;
  // Draw
  Canvas.Font.Name := 'Small Fonts';
  Canvas.Font.Size := 6;
  y := 0;
  for i := 0 to Edit.VisibleRowCount-1 do begin
    Canvas.TextOut(1, y + 3, IntToStr(nr));
    Inc(y, Abs(Edit.LineHeight));
    NextLine;
  end;
end;

procedure TScriptEditForm.btnSaveClick(Sender: TObject);
begin
  Edit.Lines.SaveToFile(BaseDir + Game.Name + '\scripts.txt');
end;

function TScriptEditForm.Modified: boolean;
begin
  Result := Edit.Modified;
end;

end.
