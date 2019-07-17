unit uSoldiers;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DataStructs, Resources, Grids, PowerGrid, uArmy, uGameSubs,
  MyStrings, IniFiles;

type
  TSoldiersForm = class(TForm)
    Grid: TPowerGrid;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; ACol, ARow: Integer;
      var TxtRect: TRect; State: TGridDrawState);
  private
    FUnit: TBaseUnit;
    Soldiers: TSoldierList;
  public
    procedure Setup(AUnit: TBaseUnit; R: TRegion);
  end;

var
  SoldiersForm: TSoldiersForm;
  BattleIni: TMemIniFile;

  function UnitImg(AUnit: TBaseUnit; R: TRegion): string;
  function UnitMultiImg(AUnit: TBaseUnit; R: TRegion): string;
  function SoldierImg(Soldier: TSoldier): string;


implementation

{$R *.lfm}

function GetBattleImage(Soldier: TSoldier; Section: string): string;
var Trace: TTrace;
    Lines: TStrings;
    i, j: integer;
    t: string;
    found: boolean;
    SData: TSkillData;

  function EqData(s1: string; IData: TItemData): boolean;
  var i: integer;
      s2: string;
  begin
    if IData = nil then Result := False
    else if Length(s1) <> Length(IData.Short) then Result := False
    else begin
      s2 := IData.Short;
      i := 1;
      while (i <= Length(s1)) and ((s1[i] = '?') or (s2[i] = '?')
        or (s1[i] = s2[i])) do
        Inc(i);
      Result := (i > Length(s1));
    end;
  end;

begin
  Result := '';
  Lines := TStringList.Create;
  // Get all names from INI file
  BattleIni.ReadSection(Section, Lines);
  i := 0;
  while (i < Lines.Count) and (Result = '') do begin
    Trace := TTrace.Create(Trim(Lines[i]));
    found := not Trace.Ends;
    // Scan all tokens in name
    while found and not Trace.Ends do begin
      t := Trace.Before(' ');
      if t <> '' then begin
        if t = 'mage' then
          found := Soldier.URef.Mage
        else if t[1] in ['A'..'Z', '?'] then begin
          // Item token
          if not EqData(t, Soldier.Man)
            and not EqData(t, Soldier.Weapon)
            and not EqData(t, Soldier.Armor)
            and not EqData(t, Soldier.Mount) then begin
            if Soldier.BItems <> nil then begin
              j := Soldier.BItems.Count-1;
              while (j >= 0) and EqData(t, Soldier.BItems[j]) do Dec(j);
              if j >= 0 then found := False;
            end
            else found := False;
          end;
        end
        else if t[1] in ['a'..'z'] then begin
          // Skill token
          SData := Game.SkillData.FindByName(t);
          if (SData = nil) or (Soldier.URef.Skills.Find(SData.Short) = nil) then
            found := False;
        end
        else found := False;
      end
    end;
    if found then Result := BattleIni.ReadString(Section, Lines[i], '');
    Trace.Free;
    Inc(i);
  end;
  if Result = '' then Result := BattleIni.ReadString(Section, 'default', '');
  Lines.Free;

  // Resolve .styles in Result
  Trace := TTrace.Create(Result);
  Result := '';
  while not Trace.Ends do begin
    t := Trace.Before(' ');
    if Pos('.', t) = 1 then t := GetBattleImage(Soldier, t);
    Result := Trim(Result + ' ' + t);
  end;
  Trace.Free;
end;

function SoldierImg(Soldier: TSoldier): string;
begin
  Result := GetBattleImage(Soldier, 'Images');
end;

function UnitImg(AUnit: TBaseUnit; R: TRegion): string;
var Sl: TSoldier;
begin
  Sl := TypicalSoldier(AUnit, R);
  if Sl <> nil then begin
    Result := SoldierImg(Sl);
    Sl.Free;
  end
  else Result := '';
end;

function UnitMultiImg(AUnit: TBaseUnit; R: TRegion): string;
var i: integer;
    Items: TItemList;
    Sl: TSoldier;
begin
  if AUnit.Items.Amount(IT_MONSTER) = 0 then
    Result := UnitImg(AUnit, R)
  else begin
    Result := '';
    Items := TItemList.Create;
    Items.AssignItems(AUnit.Items);

    i := 0;
    while i < Items.Count do begin
      if Test(Items[i].Data.Flags, IT_MONSTER) then begin
        Sl := CreateSoldier(AUnit, Items[i].Data, Items, R);
        if Result <> '' then Result := Result + ' & ';
        Result := Result + SoldierImg(Sl);
        Sl.Free;
      end;
      Inc(i);
    end;

    Items.ClearAndFree;
  end
end;

{ TSoldiersForm }

procedure TSoldiersForm.FormCreate(Sender: TObject);
begin
  Soldiers := TSoldierList.Create;
end;

procedure TSoldiersForm.FormDestroy(Sender: TObject);
begin
  Soldiers.ClearAndFree;
end;

procedure TSoldiersForm.Setup(AUnit: TBaseUnit; R: TRegion);
var i, j, row: integer;
begin
  FUnit := AUnit;
  Caption := 'Soldiers in ' + FUnit.Name;
  i := -1;
  AddSoldiers(Soldiers, FUnit, R, i);
  Grid.RowCount := 0;
  row := 0;
  i := 0;
  while (i < Soldiers.Count) do begin
    Grid.Cells[0, row] := '';
    Grid.Rows[row].Data := Soldiers[i];
    j := i;
    Inc(i);
    while (i < Soldiers.Count) and
      EqualSoldiers(Grid.Rows[row].Data, Soldiers[i]) do Inc(i);
    Grid.Cells[0, row] := IntToStr(i - j);
    Inc(row);
  end;
  Grid.Fixup;
end;

procedure TSoldiersForm.GridDrawCell(Sender: TObject; ACol, ARow: Integer;
  var TxtRect: TRect; State: TGridDrawState);
const atk_x = 60; atk_y = 4;
      def_x = 145; def_y = 4;
      num_x = 60; num_y = 19;
      arm_x = 145; arm_y = 19;
      hit_x = 370; hit_y = 4;
      sp_x = 60; sp_y = 34;
      itm_x = 60; itm_y = 34;
var Soldier: TSoldier;
    i, x: integer;
    men, s: string;

  procedure Write(X, Y: integer; Text: string; Color: TColor);
  begin
    Grid.Canvas.Font.Color := Color;
    Grid.Canvas.TextOut(TxtRect.Left + X, TxtRect.Top + Y, Text);
  end;

  procedure Draw(X, Y: integer; Index: integer);
  begin
    ResForm.Extras.Draw(Grid.Canvas, TxtRect.Left + X, TxtRect.Top + Y, Index);
  end;

begin
  with Grid do begin
    Soldier := TSoldier(Grid.Rows[ARow].Data);
    DrawBattleImage(Canvas, TxtRect.Left + 4, TxtRect.Top + 10,
      SoldierImg(Soldier), FactionColor(FUnit.Faction), False);
    // Men
    men := Cells[ACol, ARow];
    Canvas.Font.Name := 'Small Fonts';
    Canvas.Font.Size := 6;
    Write(32, 32, men, clWindowText);
    Canvas.Font.Name := 'MS Sans Serif';
    Canvas.Font.Size := 8;
  end;

  Write(atk_x, atk_y, 'Attack:', clGray);
  s := IntToStr(Soldier.Attack);
  x := atk_x + 62 + Grid.Canvas.TextWidth(s);
  Grid.Canvas.Font.Size := 6;
  Grid.Canvas.Font.Name := 'Small Fonts';
  if Soldier.Weapon = nil then begin
    Draw(atk_x + 40, atk_y + 2, bmp_extWpnClasses);
    Draw(atk_x + 50, atk_y + 2, bmp_extAttackTypes);
    Write(x, atk_y + 4, 'S', clWindowText);
  end
  else begin
    Draw(atk_x + 40, atk_y + 2, bmp_extWpnClasses + Soldier.Weapon.Weapon.WpnClass);
    Draw(atk_x + 50, atk_y + 2, bmp_extAttackTypes + Soldier.Weapon.Weapon.AttackType);
    if Test(Soldier.Weapon.Weapon.Flags, WPN_SHORT) then
      Write(x, atk_y + 4, 'S', clWindowText);
    if Test(Soldier.Weapon.Weapon.Flags, WPN_LONG) then
      Write(x, atk_y + 4, 'L', clWindowText);
  end;
  Grid.Canvas.Font.Name := 'MS Sans Serif';
  Grid.Canvas.Font.Size := 8;
  Write(atk_x + 60, atk_y, s, clWindowText);

  Write(def_x, def_y, 'Defence:', clGray);
  for i := 0 to atCount-2 do begin
    Draw(def_x + 50 + i * 27, def_y + 2, bmp_extAttackTypes + i);
    Write(def_x + 62 + i * 27, def_y, IntToStr(Soldier.Defence[i]),
      clWindowText);
  end;

  Write(num_x, num_y, 'Attacks:', clGray);
  if Soldier.NumAttacks >= 0 then s := IntToStr(Soldier.NumAttacks)
  else s := '1/' + IntToStr(-Soldier.NumAttacks);
  Write(num_x + 45, num_y, s, clWindowText);

  Write(hit_x, hit_y, 'Hits:', clGray);
  Write(hit_x + 25, hit_y, IntToStr(Soldier.Hits), clWindowText);

  Write(arm_x, arm_y, 'Armor:', clGray);
  for i := 0 to wcCount-1 do begin
    Draw(arm_x + 36 + i * 27, arm_y + 2, bmp_extWpnClasses + i);
    if Soldier.Armor = nil then s := '0'
    else s := IntToStr(Soldier.Armor.Armor.Defence[i]);
    Write(arm_x + 48 + i * 27, arm_y, s, clWindowText);
  end;

  x := itm_x;
  if Soldier.Special <> nil then begin
    Write(sp_x, sp_y, 'Spell:', clGray);
    s := Soldier.Special.Name + ', lv ' + IntToStr(Soldier.SpecLevel);
    Write(sp_x + 30, sp_y, s, clWindowText);
    Inc(x, Grid.Canvas.TextWidth(s) + 45);
  end;

  // Items
  s := Soldier.Man.Name(men <> '1');
  if Soldier.Weapon <> nil then
    s := s + ', ' + Soldier.Weapon.Name(men <> '1');
  if Soldier.Armor <> nil then
    s := s + ', ' + Soldier.Armor.Name(men <> '1');
  if Soldier.Mount <> nil then
    s := s + ', ' + Soldier.Mount.Name(men <> '1');
  if Soldier.BItems <> nil then
    for i := 0 to Soldier.BItems.Count-1 do
      s := s + ', ' + Soldier.BItems[i].Name(men <> '1');
  Write(x, itm_y, s, clWindowText);

  TxtRect.Left := TxtRect.Right;
end;


initialization
  BattleIni := TMemIniFile.Create(BaseDir + BattleFolder + 'battle.ini');

finalization
  BattleIni.Free;

end.
