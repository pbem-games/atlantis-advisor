{ Processors for each particular order }

unit uOrders;

interface

uses
  SysUtils, Classes, DataStructs, uKeys, MyStrings, Math, uGameSubs, Types,
  Resources, uUnitRecs;

type
  EParseError = class(Exception);

var
  TaxUnits, PillageUnits, EntertainUnits, WorkUnits: TUnitList;
  MaintRegions: TCoordArray;

  procedure DoAttack(AUnit: TUnit; s: string; var Line: integer);
  procedure DoBuy(AUnit: TUnit; s: string; var Line: integer);
  procedure DoBuild(AUnit: TUnit; s: string; var Line: integer);
  procedure DoCast(AUnit: TUnit; s: string; var Line: integer);
  procedure DoClaim(AUnit: TUnit; s: string; var Line: integer);
  procedure DoClaimRepeat(AUnit: TUnit; s: string; var Line: integer);
  procedure DoCombat(AUnit: TUnit; s: string; var Line: integer);
  procedure DoDeclare(AUnit: TUnit; s: string; var Line: integer);
  procedure DoDescribe(AUnit: TUnit; s: string; var Line: integer);
  procedure DoDestroy(AUnit: TUnit; s: string; var Line: integer);
  procedure DoEnter(AUnit: TUnit; s: string; var Line: integer);
  procedure DoEntertain(AUnit: TUnit; s: string; var Line: integer);
  procedure DoEvict(AUnit: TUnit; s: string; var Line: integer);
  procedure DoExtFlag(AUnit: TUnit; s: string; var Line: integer);
  procedure DoFlag(AUnit: TUnit; s: string; var Line: integer);
  procedure DoFaction(AUnit: TUnit; s: string; var Line: integer);
  procedure DoForm(AUnit: TUnit; s: string; var Line: integer);
  procedure DoForget(AUnit: TUnit; s: string; var Line: integer);
  procedure DoGive(AUnit: TUnit; s: string; var Line: integer);
  procedure DoLeave(AUnit: TUnit; s: string; var Line: integer);
  procedure DoLocal(AUnit: TUnit; s: string; var Line: integer);
  procedure DoMonth(AUnit: TUnit; s: string; var Line: integer);
  procedure DoMove(AUnit: TUnit; s: string; var Line: integer);
  procedure DoName(AUnit: TUnit; s: string; var Line: integer);
  procedure DoPillage(AUnit: TUnit; s: string; var Line: integer);
  procedure DoProduce(AUnit: TUnit; s: string; var Line: integer);
  procedure DoPromote(AUnit: TUnit; s: string; var Line: integer);
  procedure DoSell(AUnit: TUnit; s: string; var Line: integer);
  procedure DoSteal(AUnit: TUnit; s: string; var Line: integer);
  procedure DoStudy(AUnit: TUnit; s: string; var Line: integer);
  procedure DoTax(AUnit: TUnit; s: string; var Line: integer);
  procedure DoTeach(AUnit: TUnit; s: string; var Line: integer);
  procedure DoWork(AUnit: TUnit; s: string; var Line: integer);

  procedure ResolveTaxes(ARegion: TRegion; TaxUnits: TUnitList; RateMul,
    IncomeMul: integer);
  procedure ResolveEntertainment(ARegion: TRegion);
  procedure ResolveWork(ARegion: TRegion);
  procedure CheckFPoints(Errors: TStrings);
  procedure ResolveMaintenance(C: TCoords; ParseErrors: TStrings);

  procedure ClearErrorComments(Lines: TStrings);


implementation

function LevelByPoints(Points: integer): integer;
var pt: integer;
begin
  Result := 0;
  pt := 30;
  while Result <= 5 do begin
    if Points < pt then Break;
    Inc(Result);
    pt := pt + (Result+1) * 30;
  end;
end;

procedure SubtractTradeMoney(AUnit: TUnit; amt: integer);
begin
  if amt <= AUnit.TradeIncome then
    Dec(AUnit.TradeIncome, amt)
  else begin
    SetMoneyInc(AUnit, AUnit.TradeIncome - amt);
    AUnit.TradeIncome := 0;
  end;
end;

// Adjust unit's skills to accept new men
procedure AddSkillsToUnit(AUnit: TUnit; Short: string; Points, AmtMen: integer);
var i, curr_men: integer;
    NewSkill: TSkill;
begin
  curr_men := AUnit.Items.Amount(IT_MAN);
  i := 0;
  while (i < AUnit.Skills.Count) and (AUnit.Skills[i].Data.Short <> Short) do Inc(i);
  if i = AUnit.Skills.Count then begin
    // Add new skill
    NewSkill := TSkill.Create;
    NewSkill.Data := Game.SkillData.Find(Short);
    AUnit.Skills.Add(NewSkill);
  end;
  // Correct skill
  AUnit.Skills[i].Points := ((AUnit.Skills[i].Points * curr_men) + (Points *
    AmtMen)) div (curr_men + AmtMen);
  AUnit.Skills[i].Level := LevelByPoints(AUnit.Skills[i].Points);
end;

function ParseTF(s: string): integer;
begin
  if (s = 'true') or (s = 't') or (s = '1') or (s = 'on') then Result := 1
  else if (s = 'false') or (s = 'f') or (s = '0') or (s = 'off') then Result := 0
  else Result := -1;
end;

function ParseSkillData(s: string): TSkillData;
var j: integer;
begin
  for j := 0 to Game.SkillData.Count-1 do
    if (Game.SkillData[j].Name = s) or (Game.SkillData[j].Short = s) then begin
      Result := Game.SkillData[j];
      Exit;
    end;
  Result := nil;
end;

function ParseAttitude(s: string): integer;
var i: integer;
begin
  i := 1;
  while (i <= 5) and (AnsiLowerCase(GetKey(s_Hostile, i - 1)) <> s) do Inc(i);
  if i <= 5 then Result := i
  else Result := -1;
end;

function GetUnit(R: TRegion; var s: string; AllowZero: boolean): TUnit;
var t: string;
    fac, num: integer;
    Troop: TTroop;
begin
  Result := nil;
  // Get unit's number
  try
    fac := -1;
    t := GetToken(s);
    // faction 1 new 1
    if t = 'faction' then begin
      fac := StrToInt(GetToken(s));
      t := GetToken(s);
    end;
    // 10 / new 1
    if t = 'new' then num := -StrToInt(GetToken(s))
    else num := StrToInt(t);
  except
    on EConvertError do Exit;
  end;
  // Find unit in region (player's troop will be scanned first)
  if fac = -1 then
    Result := R.FindUnit(num)
  else begin
    Troop := R.Troops.Find(fac);
    if Troop <> nil then Result := Troop.Units.Find(num);
  end;
  // Raise exception if unit invalid (faction 1 new 1 may be invisible)
  if (Result = nil) and not ((fac >= 0) and (num < 0))
    and not (AllowZero and (num = 0)) then
    raise EParseError.Create('Bad target');
end;

procedure DoDescribe(AUnit: TUnit; s: string; var Line: integer);
var t2, t3: string;
begin
  // Instant
  t2 := AnsiLowerCase(GetToken(s));
  if t2 = '' then raise EParseError.Create('No argument')
  // describe unit
  else if t2 = 'unit' then begin
    t3 := GetToken(s);
    AUnit.Description := GetLegal(t3);
  end
  // describe object
  else if (t2 = 'building') or (t2 = 'ship') or (t2 = 'object')
    or (t2 = 'structure') then begin
    t3 := GetToken(s);
    if (AUnit.Struct <> nil)
      and (AUnit.Struct.Owner = AUnit) then AUnit.Struct.Description := GetLegal(t3)
    else raise EParseError.Create('Unit didn''t own object');
  end
  else raise EParseError.Create('Can''t describe that');
end;

procedure DoLocal(AUnit: TUnit; s: string; var Line: integer);
begin
  UnitRecs.AddUnitRec(AUnit, s);
end;

procedure DoName(AUnit: TUnit; s: string; var Line: integer);
var t2, t3: string;
begin
  // Instant
  t2 := AnsiLowerCase(GetToken(s));
  if t2 = '' then raise EParseError.Create('No argument')
  // name faction
  else if t2 = 'faction' then begin
    t3 := GetToken(s);
    if t3 = '' then raise EParseError.Create('No name given')
    else AUnit.Faction.Name := Getlegal(t3);
  end
  // name unit
  else if t2 = 'unit' then begin
    t3 := GetToken(s);
    if t3 = '' then raise EParseError.Create('No name given')
    else AUnit.Name := GetLegal(t3);
  end
  // name object
  else if (t2 = 'building') or (t2 = 'ship') or (t2 = 'object')
    or (t2 = 'structure') then begin
    t3 := GetToken(s);
    if t3 = '' then raise EParseError.Create('No name given')
    else begin
      if (AUnit.Struct <> nil)
        and (AUnit.Struct.Owner = AUnit) then AUnit.Struct.Name := GetLegal(t3)
      else raise EParseError.Create('Unit didn''t own object');
    end;
  end
  else raise EParseError.Create('Can''t name that');
end;

procedure DoExtFlag(AUnit: TUnit; s: string; var Line: integer);
var t1, t2: string;
    ExtFlag: integer;
begin
  // Instant
  s := AUnit.Orders[Line];
  t1 := AnsiLowerCase(GetToken(s));
  if Pos('@', t1) = 1 then t1 := Copy(t1, 2, Length(t1));

  t2 := AnsiLowerCase(GetToken(s));

  if (t1 = 'consume') or (t1 = 'reveal') then begin
    if t2 = '' then ExtFlag := 0
    else if t2 = 'unit' then ExtFlag := 1
    else if t2 = 'faction' then ExtFlag := 2
    else raise EParseError.Create('Invalid value');
  end
  else begin // spoils
    if (t2 = '') or (t2 = 'all') then ExtFlag := spoilAll
    else if t2 = 'none' then ExtFlag := spoilNone
    else if t2 = 'walk' then ExtFlag := spoilWalk
    else if t2 = 'ride' then ExtFlag := spoilRide
    else if t2 = 'fly' then ExtFlag := spoilFly
    else raise EParseError.Create('Invalid value');
  end;

  if t1 = 'consume' then AUnit.Consuming := ExtFlag
  else if t1 = 'reveal' then AUnit.Revealing := ExtFlag
  else if t1 = 'spoils' then AUnit.Spoils := ExtFlag
  else raise EParseError.Create('Unknown flag');
end;

// ** TODO: check if unit can set this flag
procedure DoFlag(AUnit: TUnit; s: string; var Line: integer);
var t1, t2: string;
    flag, val: integer;
begin
  s := AUnit.Orders[Line];
  t1 := AnsiLowerCase(GetToken(s));
  if Pos('@', t1) = 1 then t1 := Copy(t1, 2, Length(t1));

  if t1 = 'autotax' then flag := flgTax
  else if t1 = 'avoid' then flag := flgAvoid
  else if t1 = 'behind' then flag := flgBehind
  else if t1 = 'guard' then flag := flgGuard
  else if t1 = 'hold' then flag := flgHold
  else if t1 = 'noaid' then flag := flgNoaid
  else if t1 = 'nocross' then flag := flgNocross
  else if t1 = 'share' then flag := flgShare
  else raise EParseError.Create('Unknown flag');
  // Instant
  t2 := AnsiLowerCase(GetToken(s));
  if t2 = '' then raise EParseError.Create('Invalid value')
  else begin
    val := ParseTF(t2);
    if val = -1 then raise EParseError.Create('Invalid value')
    else AUnit.Flags[flag] := (val <> 0);
  end;
end;

procedure DoClaim(AUnit: TUnit; s: string; var Line: integer);
var t2: string;
    val: integer;
begin
  // Instant
  t2 := GetToken(s);
  try
    val := Min(StrToInt(t2), Game.VirtTurn.Unclaimed);
    Game.VirtTurn.Unclaimed := Game.VirtTurn.Unclaimed - val;
    SetMoneyInc(AUnit, val);
  except
    on EConvertError do raise EParseError.Create('No amount given');
  end;
end;

// Take money from unclaimed, but do not modify unit (for global orders)
procedure DoClaimRepeat(AUnit: TUnit; s: string; var Line: integer);
var t2: string;
    val: integer;
begin
  // Instant
  t2 := GetToken(s);
  try
    val := Min(StrToInt(t2), Game.VirtTurn.Unclaimed);
    Game.VirtTurn.Unclaimed := Game.VirtTurn.Unclaimed - val;
  except
    on EConvertError do raise EParseError.Create('No amount given');
  end;
end;

procedure DoCombat(AUnit: TUnit; s: string; var Line: integer);
var t2: string;
    data: TSkillData;
    i: integer;
begin
  // Instant
  t2 := AnsiLowerCase(GetToken(s));
  if t2 = '' then AUnit.CombatSpell := nil
  else begin
    data := ParseSkillData(t2);
    if data = nil then raise EParseError.Create('Invalid skill')
    else if data.Flags and SK_COMBATSPELL + SK_MAGIC <> SK_COMBATSPELL + SK_MAGIC then
      raise EParseError.Create('Not a combat spell')
    else begin
      i := 0;
      while (i < AUnit.Skills.Count) and (AUnit.Skills[i].Data.Short <>
        data.Short) do Inc(i);
      if i < AUnit.Skills.Count then AUnit.CombatSpell := data
      else raise EParseError.Create('Not known');
    end;
  end;
end;

procedure DoDeclare(AUnit: TUnit; s: string; var Line: integer);
var t2, t3: string;
    AFaction: TFaction;
    att: integer;
begin
  // Instant
  t2 := AnsiLowerCase(GetToken(s));
  if t2 = '' then raise EParseError.Create('No faction given')
  else begin
    try
      if t2 = 'default' then t2 := IntToStr(Faction.Num);
      AFaction := VTurn.Factions.Seek(StrToInt(t2));
      AFaction.Name := FindFaction(AFaction.Num).Name;
    except
      on EConvertError do raise EParseError.Create('Wrong faction');
    end;
    if AFaction <> nil then begin
      t3 := AnsiLowerCase(GetToken(s));
      if t3 = '' then begin
        if AFaction.Num <> Faction.Num then AFaction.Attitude := 0
      end
      else begin
        att := ParseAttitude(t3);
        if att = -1 then raise EParseError.Create('Wrong attitude')
        else AFaction.Attitude := att;
      end;
    end;
  end;
end;

procedure DoFaction(AUnit: TUnit; s: string; var Line: integer);
var
  t2:       string;
  modid:    integer;
  martial:  integer;
  war:      integer;
  trade:    integer;
  magic:    integer;
begin
  modid := GameConfig.ReadInteger('Settings', 'Mod', modStandard);

  martial := 0;
  war := 0;
  trade := 0;
  magic := 0;

  t2 := AnsiLowerCase(GetToken(s));

  if modid = modNewOrigins then
  begin
    if t2 = 'generic' then
    begin
      martial := 1;
      magic := 1;
    end
    else
    begin
      try
        while t2 <> '' do
        begin
          if t2 = 'martial' then martial := StrToInt(GetToken(s))
          else if t2 = 'magic' then magic := StrToInt(GetToken(s));

          t2 := AnsiLowerCase(GetToken(s));
        end;
      except
        on EConvertError do raise EParseError.Create('Invalid value');
      end;

      if (martial + magic) >= Length(Progress[0]) then
        raise EParseError.Create('Too many faction points');
    end;

    Game.VirtTurn.Martial := martial;
    Game.VirtTurn.Magic := magic;
  end
  else
  begin
    if t2 = 'generic' then
    begin
      war := 1;
      trade := 1;
      magic := 1;
    end
    else
    begin
      try
        while t2 <> '' do
        begin
          if t2 = 'war' then war := StrToInt(GetToken(s))
          else if t2 = 'trade' then trade := StrToInt(GetToken(s))
          else if t2 = 'magic' then magic := StrToInt(GetToken(s));

          t2 := AnsiLowerCase(GetToken(s));
        end;
      except
        on EConvertError do raise EParseError.Create('Invalid value');
      end;

      if (war + trade + magic) >= Length(Progress[0]) then
        raise EParseError.Create('Too many faction points');
    end;

    Game.VirtTurn.War := war;
    Game.VirtTurn.Trade := trade;
    Game.VirtTurn.Magic := magic;
  end;

  // Will not check for mages here, better do it when issuing order
end;

procedure DoForm(AUnit: TUnit; s: string; var Line: integer);
var i, j, k, a, num: integer;
    NewUnit: TUnit;
    cturn: integer;
    s1: string;
begin
  if AUnit.Num < 0 then raise EParseError.Create('New units cannot FORM');
  try
    num := StrToInt(GetToken(s))
  except
    on EConvertError do raise EParseError.Create('Invalid value');
  end;
  i := 0;
  while (i < AUnit.Region.Troops.Count) and
    (AUnit.Region.Troops[i].Faction.Num <> VFaction.Num) do Inc(i);
  if i < AUnit.Region.Troops.Count then
    with AUnit.Region.Troops[i] do begin
      j := 0;
      while (j < Units.Count) and (Units[j].Num <> -num) do Inc(j);
      if j < Units.Count then raise EParseError.Create('Duplicate number')
      else begin
        NewUnit := TUnit.Create;
        NewUnit.Name := 'Unit';
        NewUnit.Num := -num;
        NewUnit.Former := AUnit;
        NewUnit.Faction := AUnit.Faction;
        NewUnit.Region := AUnit.Region;
        NewUnit.Struct := AUnit.Struct;
        for k := 0 to UnitFlagsCount-1 do NewUnit.Flags[k] := AUnit.Flags[k];
        NewUnit.Consuming := AUnit.Consuming;
        NewUnit.Revealing := AUnit.Revealing;
        Inc(Line);
        cturn := 0;
        while (Line < AUnit.Orders.Count) and (AUnit.Order(Line) <> 'end') do begin
          s1 := TrimLeft(AUnit.Orders[Line]);
          if AUnit.Order(Line) = 'endturn' then Dec(cturn);
          for k := 1 to cturn do s1 := '  ' + s1;
          if AUnit.Order(Line) = 'turn' then Inc(cturn);
          NewUnit.Orders.Add(s1);
          AUnit.Orders.Delete(Line);
        end;
        Units.Add(NewUnit);
        Faction.Units.Add(NewUnit);
        if (NewUnit.Struct <> nil) and (NewUnit.Struct.Owner = nil) then
          NewUnit.Struct.Owner := NewUnit;
        // Army
        a := Game.UArmies.Count-1;
        while (a >= 0) and (Game.UArmies[a].UnitIds.IndexOf(NewUnit.Id) = -1) do
          Dec(a);
        if a >= 0 then NewUnit.UArmy := Game.UArmies[a];
      end;
    end;
end;

procedure DoLeave(AUnit: TUnit; s: string; var Line: integer);
var Troop: TTroop;
    i: integer;
begin
  if (AUnit.Region.Terrain.Flags and TER_WATER <> 0)
    and (not CanSwim(AUnit) or AUnit.Flags[flgNocross]) then
    raise EParseError.Create('Can''t leave in ocean');
  if (AUnit.Struct <> nil) and (AUnit.Struct.Owner = AUnit) then begin
    Troop := AUnit.Region.PlayerTroop;
    i := 0;
    while (i < Troop.Units.Count) and not ((Troop.Units[i] <> AUnit)
      and (Troop.Units[i].Struct = AUnit.Struct)) do Inc(i);
    if i < Troop.Units.Count then AUnit.Struct.Owner := Troop.Units[i]
    else AUnit.Struct.Owner := nil;
  end;
  AUnit.Struct := nil;
end;

procedure DoEnter(AUnit: TUnit; s: string; var Line: integer);
var num, i: integer;
begin
  try
    num := StrToInt(GetToken(s))
  except
    on EConvertError do raise EParseError.Create('Invalid value');
  end;
  i := 0;
  while (i < AUnit.Region.Structs.Count) and (AUnit.Region.Structs[i].Num <> num) do
    Inc(i);
  if i < AUnit.Region.Structs.Count then begin
    DoLeave(AUnit, s, Line);
    AUnit.Struct := AUnit.Region.Structs[i];
    if AUnit.Region.Structs[i].Owner = nil then
      AUnit.Region.Structs[i].Owner := AUnit;
  end
  else raise EParseError.Create('Wrong object');
end;

procedure DoProduce(AUnit: TUnit; s: string; var Line: integer);
var IData: TItemData;
    Res: TItem;
    maxout, turnout: integer;
begin
  IData := Game.ItemData.FindByName(GetToken(s));
  if IData = nil then
    raise EParseError.Create('Unknown item');

  case ProduceOut(AUnit, AUnit.Region, IData, maxout, turnout, False) of
    prdWrongAlignment:
      raise EParseError.Create('Item has opposite alignment');
    prdNoSkill:
      raise EParseError.Create('No skill to produce that');
    prdNoResource:
      raise EParseError.Create('Not enough resource in region');
    prdNoMaterial:
      raise EParseError.Create('Don''t have the required item');
    prdNoStruct:
      raise EParseError.Create('Must be inside ' + IData.Produce.RequiredStruct.Group);
    prdCantProduce:
      raise EParseError.Create('Can''t produce that');
  end;

  // Take resource
  if Test(IData.Flags, IT_RESOURCE) then begin
    Res := AUnit.Region.Products.Find(IData.Short);
    if Res <> nil then Dec(Res.Amount, turnout);
  end;

  AUnit.MonthInfo.Data := IData;
  AUnit.MonthInfo.Max := maxout;
  AUnit.MonthInfo.Amount := turnout;
  DoMonth(AUnit, s, Line);
end;

procedure DoPromote(AUnit: TUnit; s: string; var Line: integer);
var Target: TUnit;
begin
  Target := GetUnit(AUnit.Region, s, False);
  if Target = nil then Exit;
  if AUnit.Struct = nil then
    raise EParseError.Create('Must be inside structure');
  if AUnit.Struct.Owner <> AUnit then
    raise EParseError.Create('Must be owner');
  if (Target.Struct <> AUnit.Struct) then
    raise EParseError.Create('Target not in same structure');
  AUnit.Struct.Owner := Target;
  SetFlag(Target.Marks, UM_PROMOTED);
end;

procedure DoSteal(AUnit: TUnit; s: string; var Line: integer);
var num, i, amt: integer;
    t3: string;
    Target: TUnit;
begin
  i := 0;
  while (i < Line) and (AUnit.Order(i) <> 'steal') do Inc(i);
  if i < Line then raise EParseError.Create('One steal permitted');
  try
    num := StrToInt(GetToken(s))
  except
    on EConvertError do raise EParseError.Create('Invalid value');
  end;
  t3 := AnsiLowerCase(GetToken(s));
  Target := AUnit.Region.FindUnit(num);
  if Target = nil then raise EParseError.Create('Wrong target')
  else begin
    i := 0;
    while (i < AUnit.Skills.Count) and (AUnit.Skills[i].Data.Short <> Keys[s_Stealth]) do
      Inc(i);
    if i >= AUnit.Skills.Count then raise EParseError.Create('No stealth skill');
    i := 0;
    while (i < Target.Items.Count) and not TestItemName(Target.Items[i].Data, t3) do
      Inc(i);
    if i < Target.Items.Count then begin
      if Target.Items[i].Data.Flags and IT_SILVER <> 0 then begin
        amt := Target.Items.Amount(IT_SILVER);
        amt := Min(200, amt div 2);
      end
      else amt := 1;
      SetAmountInc(AUnit, Target.Items[i].Data, amt);
      SetAmountInc(Target, Target.Items[i].Data, -amt);
    end
    else raise EParseError.Create('Bad item');
  end;
end;

procedure DoDestroy(AUnit: TUnit; s: string; var Line: integer);
var i, j: integer;
    AStruct: TStruct;
    R: TRegion;
begin
  if AUnit.Region.Terrain.Flags and TER_WATER <> 0 then
    raise EParseError.Create('Can''t destroy in sea');
  AStruct := AUnit.Struct;
  R := AUnit.Region;
  if (AStruct <> nil) and (AStruct.Owner = AUnit) then begin
    for i := 0 to R.Troops.Count-1 do
      for j := 0 to R.Troops[i].Units.Count-1 do
        if R.Troops[i].Units[j].Struct = AStruct then
          R.Troops[i].Units[j].Struct := nil;
    R.Structs.Delete(R.Structs.IndexOf(AStruct));
    AStruct.Free;
  end;
end;

procedure DoGive(AUnit: TUnit; s: string; var Line: integer);
var i, k, amount: integer;
    t3, t4: string;
    Target: TUnit;
    not_enough: boolean;

  procedure GiveItem(AUnit, Target: TUnit; i, amount: integer);
  var j: integer;
  begin
    if Target <> nil then begin
      // Adjust target unit's skills if men given
      if Test(AUnit.Items[i].Data.Flags, IT_MAN) then
        for j := 0 to AUnit.Skills.Count-1 do
          AddSkillsToUnit(Target, AUnit.Skills[j].Data.Short,
            AUnit.Skills[j].Points, amount);
      // Give item
      SetAmountInc(Target, AUnit.Items[i].Data, +amount);
    end;
    SetAmountInc(AUnit, AUnit.Items[i].Data, -amount);
  end;

begin
  not_enough := False;
  Target := GetUnit(AUnit.Region, s, True);

  try
    t3 := AnsiLowerCase(GetToken(s));
    if t3 = 'all' then amount := -1
    else if t3 = 'unit' then Exit
    else amount := StrToInt(t3);
  except
    on EConvertError do raise EParseError.Create('Invalid value');
  end;

  // Give items
  t4 := AnsiLowerCase(GetToken(s));
  i := 0;
  while (i < AUnit.Items.Count) and not TestItemName(AUnit.Items[i].Data, t4) do
    Inc(i);
  if i < AUnit.Items.Count then begin
    if amount = -1 then begin
      amount := AUnit.Items[i].Amount;
      if LowerCase(GetToken(s)) = 'except' then
        try
          Dec(amount, StrToInt(GetToken(s)));
        except
          on EConvertError do raise EParseError.Create('Invalid EXCEPT');
        end;
    end;
    if amount > AUnit.Items[i].Amount then not_enough := True;
    GiveItem(AUnit, Target, i, Min(amount, AUnit.Items[i].Amount));
  end
  else begin
    k := ItemClassCount-1;
    while (k >= 0) and (ItemClasses[k] <> t4) do Dec(k);
    if k >= 0 then begin
      for i := AUnit.Items.Count-1 downto 0 do
        if Test(AUnit.Items[i].Data.Flags + IT_UNKNOWN, ItemFilters[k]) then
          GiveItem(AUnit, Target, i, AUnit.Items[i].Amount);
    end
    else if t3 <> 'all' then raise EParseError.Create('Bad item');
  end;
  if not_enough then raise EParseError.Create('Not enough');
end;

procedure DoPillage(AUnit: TUnit; s: string; var Line: integer);
var R: TRegion;
begin
  if (AUnit.Region.Guard <> nil) and (AUnit.Region.Guard.Num <> Faction.Num) then
      raise EParseError.Create('Unit on guard');
  if Taxers(AUnit) = 0 then raise EParseError.Create('Unit can''t tax');
  R := Map.Region(AUnit.Region.Coords, Turn.Num);
  if Taxers(AUnit) * GameConfig.ReadInteger('Settings', 'TaxIncome', 50) <
    R.TaxRate div 2 then
    raise EParseError.Create('Too small unit to pillage');
  PillageUnits.Add(AUnit);
end;

procedure DoTax(AUnit: TUnit; s: string; var Line: integer);
begin
  if (AUnit.Region.Guard <> nil) and (AUnit.Region.Guard.Num <> Faction.Num)
    and (AUnit.Region.Guard.Attitude < attFriendly) then
      raise EParseError.Create('Unit on guard');
  if Taxers(AUnit) = 0 then raise EParseError.Create('Unit can''t tax');
  TaxUnits.Add(AUnit);
  if GameConfig.ReadBool('Settings', 'MonthTax', False) then
    DoMonth(AUnit, s, Line);
end;

procedure DoTeach(AUnit: TUnit; s: string; var Line: integer);
var Students: TUnitList;
    i, men, st_count, help, max_students: integer;
    U: TUnit;
    order: string;
    SData: TSkillData;
    Skill, Skill1: TSkill;
begin
  Students := TUnitList.Create;
  st_count := 0;
  while Trim(s) <> '' do begin
    U := GetUnit(AUnit.Region, s, False);
    if U = nil then Continue;
    // Check study order
    order := U.MonthOrder;
    if ClearOrder(order) <> 'study' then
      raise EParseError.Create(U.FullName + ' not studying');
    GetToken(order);
    SData := Game.SkillData.FindByName(GetToken(order));
    if SData = nil then
      raise EParseError.Create(U.FullName + ' studying unknown skill');
    Skill := AUnit.Skills.Find(SData.Short);
    if Skill = nil then
      raise EParseError.Create('No skill to teach ' + U.FullName);
    Skill1 := U.Skills.Find(SData.Short);
    if (Skill1 <> nil) and (Skill.Level <= Skill1.Level) then
      raise EParseError.Create(U.FullName + ' already knows this level');
    if (Skill1 <> nil)
      and (Skill1.Points >= PointsByLevel(MaxSkillLevel(U, SData)) - 30) then
      raise EParseError.Create(U.FullName + ' can finish study without teacher');

    Students.Add(U);
    Inc(st_count, U.Items.Amount(IT_MAN));
  end;

  // Record month
  men := AUnit.Items.Amount(IT_MAN);
  max_students := GameConfig.ReadInteger('Settings', 'StudentsPerTeacher', 10);
  if st_count > 0 then help := Round(Min(1, (men * max_students) / st_count) * 30)
  else help := 0;
  AUnit.MonthInfo.Amount := st_count;
  AUnit.MonthInfo.Max := men * max_students;
  for i := 0 to Students.Count-1 do begin
    Students[i].MonthInfo.Amount := Min(60, Students[i].MonthInfo.Amount + help);
    if Students[i].MonthInfo.Details <> '' then
      Students[i].MonthInfo.Details := Students[i].MonthInfo.Details + ', ';
    Students[i].MonthInfo.Details := Students[i].MonthInfo.Details + AUnit.Name;
  end;
  Students.Free;
  DoMonth(AUnit, s, Line);
end;

procedure DoCast(AUnit: TUnit; s: string; var Line: integer);
var i: integer;
begin
  i := 0;
  while (i < Line) and (AUnit.Order(i) <> 'cast') do Inc(i);
  if i < Line then raise EParseError.Create('One cast permitted');
end;

procedure DoSell(AUnit: TUnit; s: string; var Line: integer);
var i, j, amt: integer;
    t2, t3: string;
begin
  // For simplification, do sell order as instant. If next unit attemts to
  //  sell too many, it will not sell anything (no fair sales distribution,
  //  as done in Atlantis).
  try
    t2 := LowerCase(GetToken(s));
    if t2 = 'all' then amt := -1
    else amt := StrToInt(t2);
  except
    on EConvertError do raise EParseError.Create('Invalid value');
  end;
  t3 := AnsiLowerCase(GetToken(s));
  i := 0;
  while (i < AUnit.Items.Count) and not TestItemName(AUnit.Items[i].Data, t3) do
    Inc(i);
  if i >= AUnit.Items.Count then begin
    if amt > 0 then raise EParseError.Create('Hasn''t this item')
    else Exit;
  end;
  if amt = -1 then amt := AUnit.Items[i].Amount;
  amt := Min(amt, AUnit.Items[i].Amount);
  with AUnit.Region do begin
    // Make changes to region
    j := 0;
    while (j < Wanted.Count) and (Wanted[j].Data.Short <> AUnit.Items[i].Data.Short) do
      Inc(j);
    if j >= Wanted.Count then raise EParseError.Create('Not wanted');
    amt := Min(amt, Wanted[j].Amount);
    Wanted[j].Amount := Wanted[j].Amount - amt;
    // Trade items
    if Test(Wanted[j].Data.Flags, IT_TRADE) then
      SetFlag(AUnit.Region.Marks, RM_TRADE, True);
    // Changes in unit
    SetAmountInc(AUnit, AUnit.Items[i].Data, -amt);
    Inc(AUnit.TradeIncome, Wanted[j].Cost * amt);
  end;
end;

procedure DoAttack(AUnit: TUnit; s: string; var Line: integer);
var Target: TUnit;
begin
  while (Trim(s) <> '') do begin
    Target := GetUnit(AUnit.Region, s, False);
    if Target <> nil then
      SetFlag(Target.Marks, UM_ATTACK);
  end;
end;

procedure DoBuy(AUnit: TUnit; s: string; var Line: integer);
var amt, i, j: integer;
    t2, t3: string;
    Item: TItem;
begin
  // Same simplification then in Sell
  try
    t2 := LowerCase(GetToken(s));
    if t2 = 'all' then amt := -1
    else amt := StrToInt(t2);
  except
    on EConvertError do raise EParseError.Create('Invalid value');
  end;
  t3 := AnsiLowerCase(GetToken(s));
  with AUnit.Region do begin
    i := 0;
    while (i < ForSale.Count) and not TestItemName(ForSale[i].Data, t3) do Inc(i);
    if i >= ForSale.Count then raise EParseError.Create('No items for sale');

    if amt = -1 then amt := ForSale[i].Amount;
    if ForSale[i].Amount <> -1 then
      amt := Min(amt, ForSale[i].Amount);

    amt := Min(amt, (AUnit.Items.Amount(IT_SILVER) + AUnit.TradeIncome)
      div ForSale[i].Cost);
    if ForSale[i].Amount <> -1 then
      ForSale[i].Amount := ForSale[i].Amount - amt;
    if amt > 0 then begin
      // Men
      if Test(ForSale[i].Data.Flags, IT_MAN) then begin
        // Alert if mixing leaders
        if (AUnit.Items.Amount(IT_MAN) > 0)
          and (IsLeader(AUnit.Items) xor ForSale[i].Data.Man.Leader) then
          raise EParseError.Create('Can''t mix leaders and normal men');
        // Adjust skills if men bought
        for j := 0 to AUnit.Skills.Count-1 do
          AddSkillsToUnit(AUnit, AUnit.Skills[j].Data.Short, 0, amt);
      end;
      // Trade items
      if Test(ForSale[i].Data.Flags, IT_TRADE) then
        SetFlag(AUnit.Region.Marks, RM_TRADE, True);
      // Good/Evil
      if (GameConfig.ReadInteger('Game', 'Alignment', alNeutral) = alGood)
        and Test(ForSale[i].Data.Flags, IT_EVIL) then
        raise EParseError.Create('Can''t buy evil items');
      if (GameConfig.ReadInteger('Game', 'Alignment', alNeutral) = alEvil)
        and Test(ForSale[i].Data.Flags, IT_GOOD) then
        raise EParseError.Create('Can''t buy good items');
      // Buy item
      Item := TItem.Create;
      Item.Amount := amt;
      Item.Data := ForSale[i].Data;
      Item.Bought := True;
      AUnit.Items.Add(Item);
      SubtractTradeMoney(AUnit, ForSale[i].Cost * amt);
    end;
  end;
end;

procedure DoBuild(AUnit: TUnit; s: string; var Line: integer);
var i, lv, amt, maxout: integer;
    StData: TStructData;
    order, t: string;
    U: TUnit;
begin
  if Pos('help', s) = 1 then begin
    // BUILD HELP <unit>
    GetToken(s);
    U := GetUnit(AUnit.Region, s, False);
    if U = nil then Exit;
    i := U.Orders.Count-1;
    while (i >= 0) and (U.Order(i) <> 'build') do Dec(i);
    if i < 0 then raise EParseError.Create('Target doesn''t building')
    else order := U.Orders[i];
    StData := nil;
    GetToken(order);
    if (Trim(order) = '') then begin
      if (U.Struct <> nil) then StData := U.Struct.Data;
    end
    else StData := Game.StructData.Find(GetToken(order));
  end
  else if s <> '' then begin
    // BUILD "Object"
    t := GetToken(s);
    StData := Game.StructData.Find(t);
    if StData = nil then
      raise EParseError.Create('Unknown object');
  end
  else begin
    // BUILD
    if AUnit.Struct = nil then
      raise EParseError.Create('Must be inside object');
    if AUnit.Struct.Needs = 0 then
      raise EParseError.Create('Object is finished');
    StData := AUnit.Struct.Data;
  end;
  if StData <> nil then begin
    // Check productivity
    lv := BuildSkillLv(AUnit, StData);
    if lv = 0 then
      raise EParseError.Create('Can''t build that');
    amt := BuildMaterials(AUnit, StData);
    if amt = 0 then
      raise EParseError.Create('Don''t have the required item');
    AUnit.MonthInfo.Data := StData;
    maxout := AUnit.Items.Amount(IT_MAN) * lv;
    Inc(AUnit.MonthInfo.Max, maxout);
    AUnit.MonthInfo.Amount := Min(AUnit.MonthInfo.Max, amt);
  end;
  DoMonth(AUnit, s, Line);
end;

procedure DoForget(AUnit: TUnit; s: string; var Line: integer);
var t2: string;
    i: integer;
begin
  t2 := GetToken(s);
  i := 0;
  while (i < AUnit.Skills.Count) and (AUnit.Skills[i].Data.Short <> AnsiUpperCase(t2))
    and (AUnit.Skills[i].Data.Name <> AnsiLowerCase(t2)) do Inc(i);
  if i < AUnit.Skills.Count then begin
    AUnit.Skills[i].Free;
    AUnit.Skills.Delete(i);
  end
  else raise EParseError.Create('Bad skill');
end;

procedure DoMonth(AUnit: TUnit; s: string; var Line: integer);
var ord: string;
begin
  ord := AUnit.MonthOrder;
  if Line = -1 then AUnit.MonthOrder := 'tax'
  else AUnit.MonthOrder := AUnit.Orders[Line];
  if ord <> '' then raise EParseError.Create('Overwriting previous monthlong order');
  // Trade
  if Line >= 0 then begin
    if (AUnit.Order(Line) = 'produce') or (AUnit.Order(Line) = 'build') then
      SetFlag(AUnit.Region.Marks, RM_TRADE, True);
    if (AUnit.Order(Line) = 'produce') and (AUnit.MonthInfo.Data <> nil)
      and (TItemData(AUnit.MonthInfo.Data).Short = 'FISH') then
      SetFlag(AUnit.Region.Marks, RM_FISHING, True);
    if (AUnit.Order(Line) = 'build') and (AUnit.MonthInfo.Data <> nil)
      and Test(TStructData(AUnit.MonthInfo.Data).Flags, ST_ROAD) then
      SetFlag(AUnit.Region.Marks, RM_ROADBUILD, True);
  end;
end;

procedure DoEntertain(AUnit: TUnit; s: string; var Line: integer);
var Skill: TSkill;
begin
  Skill := AUnit.Skills.Find(Keys[s_EntertainSkill]);
  if Skill = nil then raise EParseError.Create('No entertainment skill');
  EntertainUnits.Add(AUnit);
  DoMonth(AUnit, s, Line);
end;

procedure DoEvict(AUnit: TUnit; s: string; var Line: integer);
var Target: TUnit;
begin
  Target := GetUnit(AUnit.Region, s, False);
  if Target = nil then Exit;
  if AUnit.Struct = nil then
    raise EParseError.Create('Must be inside structure');
  if AUnit.Struct.Owner <> AUnit then
    raise EParseError.Create('Must be owner');
  if (Target.Struct <> AUnit.Struct) then
    raise EParseError.Create('Target not in same structure');
  Target.Struct := nil;
end;

procedure DoWork(AUnit: TUnit; s: string; var Line: integer);
begin
  WorkUnits.Add(AUnit);
  DoMonth(AUnit, s, Line);
end;

procedure DoStudy(AUnit: TUnit; s: string; var Line: integer);
var t2: string;
    SData: TSkillData;
    Skill: TSkill;
    i: integer;
begin
  t2 := LowerCase(GetToken(s));
  SData := Game.SkillData.FindByName(t2);
  if SData = nil then raise EParseError.Create('Bad skill');
  // Max level
  Skill := AUnit.Skills.Find(SData.Short);
  if (Skill <> nil) and (Skill.Level = MaxSkillLevel(AUnit, SData)) then
    raise EParseError.Create('Maximum level reached');
  // Cost
  if StudyCost(AUnit, SData) > AUnit.Items.Amount(IT_SILVER) +
    AUnit.TradeIncome + AUnit.WorkIncome then
    raise EParseError.Create('Not enough money');
  // Prereqisites
  for i := 0 to SData.BasedOn.Count-1 do begin
    Skill := AUnit.Skills.Find(SData.BasedOn[i].Data.Short);
    if (Skill = nil) or (Skill.Level < SData.BasedOn[i].Level) then
      raise EParseError.Create('No pre-requisite skill (' +
        SData.BasedOn[i].Data.Name + ' ' + IntToStr(SData.BasedOn[i].Level) + ')' );
  end;
  // Days learned
  AUnit.MonthInfo.Amount := 30;
  DoMonth(AUnit, s, Line);
end;

procedure DoMove(AUnit: TUnit; s: string; var Line: integer);
var i, mt, mp, d, struct, cap: integer;
    order, t2: string;
    C: TCoords;
    swim, sail: boolean;
    R: TRegion;

  procedure Finish;
  var i: integer;
      Troop: TTroop;
  begin
    AUnit.FinalPoint := High(AUnit.Moves);
    if (order = 'sail') and (AUnit.Struct <> nil)
      and (AUnit.Struct.Owner = AUnit) then begin
      Troop := AUnit.Region.PlayerTroop;
      for i := 0 to Troop.Units.Count-1 do
        if Troop.Units[i].Struct = AUnit.Struct then begin
          if Troop.Units[i].FinalPoint = -1 then
            Troop.Units[i].FinalPoint := 0;
        end;
    end;
  end;

  procedure MoveTo(CTo: TCoords);
  begin
    if mp >= 0 then Dec(mp, EnterCost(mt, CTo, C, sail));
    if (mp < 0) and (AUnit.FinalPoint = -1) then Finish;
    C := CTo;
    AddCoords(AUnit.Moves, C);
  end;

begin
  if Line >= AUnit.Orders.Count then Exit;
  order := AUnit.Order(Line);

  C := AUnit.Region.Coords;
  SetLength(AUnit.Moves, 0);
  AddCoords(AUnit.Moves, C);

  if AUnit.Struct = nil then struct := 0
  else struct := AUnit.Struct.Num;
  AUnit.FinalPoint := -1;
  mt := MovementType(AUnit);
  swim := CanSwim(AUnit);
  sail := (order = 'sail');
  if sail then mp := 4
  else mp := MovesLeft(AUnit);

  t2 := UpperCase(AnsiUpperCase(GetToken(s)));
  while t2 <> '' do begin
    // IN, OUT
    if t2 = 'IN' then begin
      if (struct = 0) or (order = 'sail') then Break;
      R := Map.Region(C);
      if (R <> nil) and R.FullData then begin
        i := R.Structs.Count-1;
        while (i >= 0) and (R.Structs[i].Num <> struct) do Dec(i);
        if (i >= 0) and Test(R.Structs[i].Data.Flags, ST_SHAFT)
          and R.Structs[i].HasExit then
          MoveTo(R.Structs[i].Passage)
        else Break;
      end
      else Break;
    end
    else if t2 = 'OUT' then begin
      if struct <> 0 then struct := 0
      else Break;
    end

    // Object number
    else if IsNumber(t2) then begin
      AddCoords(AUnit.Moves, C);
      struct := ToInt(t2);
    end

    // Direction
    else begin
      d := 6;
      while (d > 0) and (GetDir(d) <> t2) do Dec(d);
      if d > 0 then begin
        if not MovableDir(C, d, sail, mt, swim, AUnit.Struct) then begin
          Finish;
          raise EParseError.Create('Can''t move there');
        end;
        MoveTo(CoordsInDir(C, d));
      end
      else Break;
    end;

    t2 := AnsiUpperCase(GetToken(s));
  end;
  if t2 <> '' then // some moves was not resolved
    AddCoords(AUnit.Moves, C)
  else if AUnit.FinalPoint = -1 then Finish;

  if (Length(AUnit.Moves) > 0) and (struct = 0) then begin
    R := Map.Region(AUnit.Moves[AUnit.FinalPoint]);
    if (R <> nil) and Test(R.Terrain.Flags, TER_WATER) then begin
      cap := UnitCapacity(AUnit, mtSwim);
      if not ((cap > 0) and (UnitLoad(AUnit, mtSwim) <= cap)) then
        raise EParseError.Create('Unit may drown');
    end;
  end;

  DoMonth(AUnit, s, Line);
end;

procedure ResolveMaintenance(C: TCoords; ParseErrors: TStrings);
type TConsumeRec = record
       Men, Silver, Food: integer;
       URef: TUnit;
     end;
var i, amt, silver, food: integer;
    Recs: array of TConsumeRec;
    warn: boolean;
    R: TRegion;

  procedure AddToRecs(U: TUnit);
  var j, needs, rec: integer;
      order, t: string;
      SData: TSkillData;
      Item: TItem;
      Leaders: boolean;
  begin
    SetLength(Recs, Length(Recs) + 1);
    rec := High(Recs);
    Recs[rec].URef := U;
    Leaders := False;
    for j := 0 to U.Items.Count-1 do begin
      // Set values from inventory
      Item := U.Items[j];
      if Test(Item.Data.Flags, IT_MAN) then begin
        Inc(Recs[rec].Men, Item.Amount);
        if Item.Data.Man.Leader then Leaders := True;
      end
      else if Test(Item.Data.Flags, IT_SILVER) then
        Inc(Recs[rec].Silver, Item.Amount)
      else if Test(Item.Data.Flags, IT_FOOD) then
        Inc(Recs[rec].Food, Item.Amount);
    end;
    // Add income silver
    Inc(Recs[rec].Silver, U.TradeIncome + U.WorkIncome);
    // Remove study fees
    if ClearOrder(U.MonthOrder) = 'study' then begin
      order := U.MonthOrder;
      GetToken(order); // study
      t := GetToken(order);
      SData := Game.SkillData.FindByName(t);
      if SData <> nil then Dec(Recs[rec].Silver, StudyCost(U, SData));
    end;
    // Apply maintenance costs
    if Leaders then
      needs := GameConfig.ReadInteger('Settings', 'LeaderMaintenance', 20)
    else needs := GameConfig.ReadInteger('Settings', 'PeasantMaintenance', 10);
    if U.Consuming <> ctSilver then
      Dec(Recs[rec].Food, Min(1, (needs * Recs[rec].Men) div 10))
    else Dec(Recs[rec].Silver, needs * Recs[rec].Men);
  end;

begin
  SetLength(Recs, 0);

  // Fill Recs with units from the region
  R := Map.Region(C);
  if (R <> nil) and (R.PlayerTroop <> nil) then
    for i := 0 to R.PlayerTroop.Units.Count-1 do
      if EqualCoords(R.PlayerTroop.Units[i].FinalCoords, R.Coords) then
        AddToRecs(R.PlayerTroop.Units[i]);

  // Add arriving units
  for i := 0 to VFaction.Units.Count-1 do
    if VFaction.Units[i].ArrivingTo(C) then
      AddToRecs(VFaction.Units[i]);

  // Count spare resources
  silver := 0;
  food := 0;
  for i := 0 to High(Recs) do begin
    if Recs[i].Silver > 0 then Inc(silver, Recs[i].Silver);
    if Recs[i].Food > 0 then Inc(food, Recs[i].Food);
  end;

  // Units with CONSUME SILVER and CONSUME FACTION FOOD borrows first
  for i := 0 to Length(Recs) - 1 do begin
    if (Recs[i].Silver < 0) and (Recs[i].URef.Consuming = ctSilver) then begin
      amt := Min(silver, -Recs[i].Silver);
      Inc(Recs[i].Silver, amt);
      Dec(silver, amt);
    end
    else if (Recs[i].Food < 0) and (Recs[i].URef.Consuming = ctFaction) then begin
      amt := Min(food, -Recs[i].food);
      Inc(Recs[i].Food, amt);
      Dec(food, amt);
    end;
  end;
  // Now CONSUME UNIT FOOD
  for i := 0 to Length(Recs) - 1 do begin
    if (Recs[i].Food < 0) and (Recs[i].URef.Consuming = ctUnit) then begin
      amt := Min(food, -Recs[i].food);
      Inc(Recs[i].Food, amt);
      Dec(food, amt);
    end;
  end;
  // Now redistribute resources
  silver := silver + food * 10;
  warn := False;
  for i := 0 to Length(Recs) - 1 do begin
    if Recs[i].Food < 0 then begin
      amt := Min(silver div 10, -Recs[i].food);
      Inc(Recs[i].Food, amt);
      Dec(silver, amt * 10);
      if amt > 0 then begin
        Recs[i].URef.Orders.Insert(0, ';. Will borrow money');
        ParseErrors.AddObject('!M4 ' + Recs[i].URef.FullName +
          ': Will borrow money', Recs[i].URef);
      end;
    end
    else if Recs[i].Silver < 0 then begin
      amt := Min(silver, -Recs[i].Silver);
      Inc(Recs[i].Silver, amt);
      Dec(silver, amt);
      if amt > 0 then begin
        Recs[i].URef.Orders.Insert(0, ';. Will borrow food');
        ParseErrors.AddObject('!M4 ' + Recs[i].URef.FullName +
          ': Will borrow food', Recs[i].URef);
      end;
    end;
    if (Recs[i].Silver < 0) or (Recs[i].Food < 0) then begin
      Recs[i].URef.Orders.Insert(0, ';. No money for maintenance');
      // Show region warning
      if not warn then ParseErrors.AddObject('!M4 ' + MakeRegionName(C, True) +
        ': Units hungry', Recs[i].URef);
      warn := True;
    end
  end;
end;

// Split taxes evenly between taxers
procedure ResolveTaxes(ARegion: TRegion; TaxUnits: TUnitList; RateMul,
  IncomeMul: integer);
var i, total_men, men, tax_rate, tax_income, amt: integer;
    cash: real;
    U: TUnit;
begin
  tax_income := GameConfig.ReadInteger('Settings', 'TaxIncome', 50);
  total_men := 0;
  for i := 0 to TaxUnits.Count-1 do
    total_men := total_men + Taxers(TaxUnits[i]);
  if total_men > 0 then begin
    tax_rate := ARegion.TaxRate * RateMul;
    cash := Min(tax_income * RateMul, tax_rate / total_men);
    for i := 0 to TaxUnits.Count-1 do begin
      U := TaxUnits[i];
      men := Taxers(U);
      Inc(ARegion.Activity.Taxers, men);
      amt := Round(men * cash);
      tax_rate := tax_rate - amt;
      Inc(U.TradeIncome, amt);
      if GameConfig.ReadBool('Settings', 'MonthTax', False) then begin
        U.MonthInfo.Max := men * tax_income;
        U.MonthInfo.Amount := amt;
      end;
    end;
    ARegion.TaxRate := tax_rate div RateMul;
    SetFlag(ARegion.Marks, RM_TAX, True);
  end;
end;

procedure ResolveEntertainment(ARegion: TRegion);
var i, total, amt, ent_income: integer;
    cash: real;
    U: TUnit;
begin
  ent_income := GameConfig.ReadInteger('Settings', 'EntertainIncome', 20);
  total := 0;
  for i := 0 to EntertainUnits.Count-1 do
    Inc(total, EntertainOut(EntertainUnits[i]));
  if total > 0 then begin
    cash := Min(1, ARegion.Entertainment / total);
    for i := 0 to EntertainUnits.Count-1 do begin
      U := EntertainUnits[i];
      amt := EntertainOut(U);
      Inc(ARegion.Activity.Entertainers, amt div ent_income);
      Dec(ARegion.Entertainment, Round(amt * cash));
      Inc(U.WorkIncome, Round(amt * cash));
      U.MonthInfo.Max := amt;
      U.MonthInfo.Amount := Round(amt * cash);
    end;
  end;
end;

procedure ResolveWork(ARegion: TRegion);
var i, total_men, men, amt: integer;
    cash: real;
    U: TUnit;
begin
  total_men := 0;
  for i := 0 to WorkUnits.Count-1 do
    Inc(total_men, WorkUnits[i].Items.Amount(IT_MAN));
  if total_men > 0 then begin
    cash := Min(ARegion.Wages, ARegion.MaxWages / total_men);
    for i := 0 to WorkUnits.Count-1 do begin
      U := WorkUnits[i];
      men := U.Items.Amount(IT_MAN);
      amt := Round(men * cash);
      Inc(ARegion.Activity.Workers, men);
      Dec(ARegion.MaxWages, amt);
      Inc(U.WorkIncome, amt);
      U.MonthInfo.Max := ARegion.Wages * men;
      U.MonthInfo.Amount := amt;
    end;
  end;
end;

procedure CheckFPoints(Errors: TStrings);
var i, tax, trade, fish, road: integer;
  martial:  integer;
begin
  martial := 0;
  tax := 0;
  trade := 0;
  fish := 0;
  road := 0;

  case GameConfig.ReadInteger('Settings', 'Mod', modStandard) of
  modMagicdeep:
    for i := 0 to VTurn.Regions.Count-1 do
    begin
      if Test(VTurn.Regions[i].Marks, RM_TAX) then Inc(tax);
      if Test(VTurn.Regions[i].Marks, RM_TRADE) then
      begin
        if Test(VTurn.Regions[i].Marks, RM_FISHING) then Inc(fish)
        else if Test(VTurn.Regions[i].Marks, RM_ROADBUILD) then Inc(road)
        else Inc(trade);
      end;
    end;
  modNewOrigins:
    for i := 0 to VTurn.Regions.Count-1 do
    begin
      if Test(VTurn.Regions[i].Marks, RM_TAX) then Inc(martial)
      else if Test(VTurn.Regions[i].Marks, RM_TRADE) then Inc(martial);
    end;
  else
    for i := 0 to VTurn.Regions.Count-1 do
    begin
      if Test(VTurn.Regions[i].Marks, RM_TAX) then Inc(tax);
      if Test(VTurn.Regions[i].Marks, RM_TRADE) then Inc(trade);
    end;
  end;

  if martial > Progress[prMartial, VTurn.Martial] then
    Errors.AddObject('! 3 Too many tax and/or trade regions', nil);
  if tax > Progress[prWar, VTurn.War] then
    Errors.AddObject('! 3 Too many tax regions', nil);
  if trade > Progress[prTrade, VTurn.Trade] then
    Errors.AddObject('! 3 Too many trade regions', nil);
  if fish > Progress[prFish, VTurn.Trade] then
    Errors.AddObject('! 3 Too many fishing regions', nil);
  if road > Progress[prRoad, VTurn.Trade] then
    Errors.AddObject('! 3 Too many road build regions', nil);
  // TODO : check mages?
end;

procedure ClearErrorComments(Lines: TStrings);
var i: integer;
begin
  i := 0;
  while i < Lines.Count do begin
    if Pos(';.', Lines[i]) > 0 then
      Lines[i] := Copy(Lines[i], 1, Pos(';.', Lines[i]) - 1);
    Inc(i);
  end;
end;

end.
