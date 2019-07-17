unit uShortcuts;

{$MODE Delphi}

interface

uses
   SysUtils, Windows, Classes, Menus, Resources, MyStrings, Dialogs, ActnList;

const
  RepeatingShortcuts = 13;
  ShortcutOrders: array[0..32] of string = ('form new', 'continue build', 'teach',
    'claim', 'tax', 'entertain', 'work', 'leave', 'advance', 'move', 'sail',
    'advance army', 'move army', 'promote', 'evict', 'enter', 'study', 'produce', 'build', 'cast',
    'buy', 'sell', 'attack', 'assassinate',
    'Declare Needs', 'Distribute Needs',
    'Declare MOVE Route', 'Declare SAIL Route', 'Run Route',
    'Add as Attacker', 'Add Army as Attacker', 'Add as Defender', 'Clear Simulation');

  function ShortcutToStr(Sc: TShortCut): string;
  function FindShortcut(Order: string; RemoveShift: boolean): TShortCut;
  function FindOrder(Key: Word; Shift: TShiftState; var Repeating: boolean): string;
  procedure RecordShortcut(Key: Word; Shift: TShiftState; Order: string);
  procedure ClearShortcut(Order: string);
  procedure ApplyShortcuts(Items: TMenuItem);

implementation

uses
  Main;

function KeyToText(Key: Word): string;
begin
  case Key of
    $20:          Result := 'Space';
    VK_F1..VK_F12:
                  Result := 'F' + IntToStr(Key - VK_F1 + 1);
    $30..$39, $41..$5A:
                  Result := Chr(Key);
    VK_NUMPAD0..VK_NUMPAD9:
                  Result := 'Num ' + IntToStr(Key - VK_NUMPAD0);
    VK_MULTIPLY:  Result := '*';
    VK_ADD:       Result := '+';
    VK_SUBTRACT:  Result := '-';
    VK_DECIMAL:   Result := '.';
    VK_DIVIDE:    Result := '/';
    VK_INSERT:    Result := 'Ins';
    VK_DELETE:    Result := 'Del';
    VK_HOME:      Result := 'Home';
    VK_END:       Result := 'End';
    VK_PRIOR:     Result := 'PgUp';
    VK_NEXT:      Result := 'PgDn';
    else          Result := '';
  end;
end;

function ShortcutToStr(Sc: TShortCut): string;
var Key: Word;
    Shift: TShiftState;
    keytext: string;

  procedure AddPlus(var s: string; s1: string);
  begin
    if s <> '' then s := s + '+';
    s := s + s1;
  end;

begin
  ShortCutToKey(Sc, Key, Shift);
  Result := '';
  keytext := KeyToText(Key);
  if keytext = '' then Exit;
  if ssCtrl in Shift then AddPlus(Result, 'Ctrl');
  if ssShift in Shift then AddPlus(Result, 'Shift');
  if ssAlt in Shift then AddPlus(Result, 'Alt');
  AddPlus(Result, keytext);
end;

function StrToShortcut(s: string): TShortCut;
var Key: Word;
    Shift: TShiftState;
    Trace: TTrace;
begin
  Shift := [];
  Trace := TTrace.Create(Trim(s));
  while (Pos('+', Trace.Text) > 0) and (Trace.Text <> '+') do begin
    if Pos('Shift', Trace.Text) = 1 then  Shift := Shift + [ssShift];
    if Pos('Ctrl', Trace.Text) = 1 then   Shift := Shift + [ssCtrl];
    if Pos('Alt', Trace.Text) = 1 then    Shift := Shift + [ssAlt];
    Trace.Before('+');
    Trace.SkipSpaces;
  end;
  if Trace.Text = '' then Result := scNone
  else begin
    Key := $FF;
    while (Key > 0) and (KeyToText(Key) <> Trace.Text) do Dec(Key);
    if Key > 0 then Result := ShortCut(Key, Shift)
    else Result := scNone;
  end;
  Trace.Free;
end;

function FindOrder(Key: Word; Shift: TShiftState; var Repeating: boolean): string;
var Lines: TStrings;
    i: integer;
    s, s1: string;
begin
  Shift := Shift - [ssShift];
  Lines := TStringList.Create;
  Config.ReadSection('Shortcuts', Lines);
  s := ShortcutToStr(ShortCut(Key, Shift));
  s1 := ShortcutToStr(ShortCut(Key, Shift + [ssShift]));

  i := Lines.Count-1;
  while (i >= 0) and (Config.ReadString('Shortcuts', Lines[i], '') <> s)
    and (Config.ReadString('Shortcuts', Lines[i], '') <> s1) do Dec(i);
  if i >= 0 then begin
    Result := Lines[i];
    Repeating := (Config.ReadString('Shortcuts', Lines[i], '') = s1);
  end
  else Result := '';

  Lines.Free;
end;

function FindShortcut(Order: string; RemoveShift: boolean): TShortCut;
begin
  Result := StrToShortcut(Config.ReadString('Shortcuts', Order, ''));
  if RemoveShift then Result := Result and (not scShift);
end;

procedure ClearShortcut(Order: string);
begin
  Config.DeleteKey('Shortcuts', Order);
end;

procedure RecordShortCut(Key: Word; Shift: TShiftState; Order: string);
var s: string;
    rep: boolean;
    i: integer;
begin
  Key := RealKey(Key);
  if not (Key in [VK_F1..VK_F12, $30..$39, $41..$5A,
    VK_NUMPAD0..VK_NUMPAD9, VK_MULTIPLY, VK_ADD, VK_SUBTRACT, VK_DECIMAL,
    VK_DIVIDE, VK_INSERT, VK_DELETE, VK_HOME, VK_END, VK_PRIOR, VK_NEXT]) then
    Exit;

  i := Length(ShortcutOrders)-1;
  while (i > 0) and (ShortcutOrders[i] <> Order) do Dec(i);
  if not ((i >= RepeatingShortcuts) or (i = -1)) then begin
    if (ssShift in Shift) then begin
      MessageDlg('Combinations with Shift are reserved for @-orders',
        mtWarning, [mbOk], 0);
      Exit;
    end;
  end
  else if not (Key in [VK_F1..VK_F12, VK_INSERT, VK_DELETE, VK_HOME,
    VK_END, VK_PRIOR, VK_NEXT]) and not (ssCtrl in Shift)
    and not (ssShift in Shift) then begin
    MessageDlg('Single character-key shortcuts not allowed for Main Menu, ' +
      'use F-keys or combinations with Ctrl and Shift',
      mtWarning, [mbOk], 0);
    Exit;
  end;

  while True do begin
    s := FindOrder(Key, Shift, rep);
    if s = '' then Break;
    Config.DeleteKey('Shortcuts', s);
  end;

  Config.WriteString('Shortcuts', Order, ShortcutToStr(ShortCut(Key, Shift)));
end;

procedure ApplyShortcuts(Items: TMenuItem);
var i: integer;
    s: string;
    Parent: TMenuItem;

  procedure ReadShortcut(Item: TMenuItem; s: string);
  var i: integer;
      Sc: TShortCut;
  begin
    if Item.Caption = '-' then Exit;

    if s <> '' then s := s + ' | ';
    s := s + StringReplace(Item.Caption, '&', '', []);
    if Item.Count = 0 then begin
      Sc := FindShortcut(s, True);
      Item.Shortcut := Sc;
      if Item.Action <> nil then
        TAction(Item.Action).Shortcut := Sc;
    end
    else
      for i := 0 to Item.Count-1 do ReadShortcut(Item.Items[i], s);
  end;

begin
  s := '';
  Parent := Items;
  while (Parent <> nil) and (Parent.Caption <> '') do begin
    if s <> '' then s := ' | ' + s;
    s := StringReplace(Parent.Caption, '&', '', []) + s;
    Parent := Parent.Parent;
  end;

  for i := 0 to Items.Count-1 do
    ReadShortcut(Items[i], s);
end;

end.
