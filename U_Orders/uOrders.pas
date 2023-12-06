{ Processors for each particular order }

unit uOrders;

interface

uses
  SysUtils, Classes, DataStructs, uKeys, MyStrings, Math, uGameSubs, Types,
  Resources, uUnitRecs;

type
  EParseError = class(Exception);

  TUnexplored = class
    Coords: TCoords;
    Units: TUnitList;
  end;

  TUnexploredArray = array of TUnexplored;

var
  TaxUnits, PillageUnits, EntertainUnits, WorkUnits: TUnitList;
  Unexplored: TUnexploredArray;

  procedure DoAttack(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
  procedure DoBuy(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
  procedure DoBuild(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; AOrder: string);
  procedure DoCast(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
  procedure DoInvenotry(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
  procedure DoClaim(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
  procedure DoClaimRepeat(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
  procedure DoCombat(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
  procedure DoDeclare(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
  procedure DoDescribe(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
  procedure DoDestroy(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
  procedure DoEnter(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
  procedure DoEntertain(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
  procedure DoEvict(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
  procedure DoExtFlag(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
  procedure DoFlag(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
  procedure DoFaction(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
  procedure DoForm(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
  procedure DoForget(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
  procedure DoGive(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
  procedure DoLeave(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
  procedure DoLocal(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
  procedure DoMonth(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
  procedure DoMove(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; AOrder: string);
  procedure DoName(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
  procedure DoPillage(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
  procedure DoProduce(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
  procedure DoPromote(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
  procedure DoSell(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
  procedure DoSteal(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
  procedure DoStudy(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
  procedure DoTax(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
  procedure DoTeach(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; AOrder: string);
  procedure DoWork(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
  procedure DoTransport(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
  procedure DoTag(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);

  procedure ResolveTaxes(ARegion: TRegion; TaxUnits: TUnitList; RateMul, IncomeMul: integer);
  procedure ResolveEntertainment(ARegion: TRegion);
  procedure ResolveWork(ARegion: TRegion);
  procedure CheckFPoints(Errors: TStrings);
  procedure ResolveMaintenance(Coords: TCoords; Units: TUnitList; ParseErrors: TStrings);

  procedure ClearErrorComments(Lines: TStrings);

  procedure InitUnexplored;
  procedure AddToUnexplored(Coords: TCoords; AUnit: TUnit);
  procedure ClearUnexplored;


implementation

uses
  TypInfo;

procedure InitUnexplored;
begin
  SetLength(Unexplored, 0);
end;

procedure AddToUnexplored(Coords: TCoords; AUnit: TUnit);
var
  i: integer;
  reg: TUnexplored;

begin
  // 1. find the region in the UnexploredRegions if it exists
  // 1.1 if not exists, create new record
  // 2. add unit to the record

  reg := nil;
  for i := 0 to High(Unexplored) do
    if EqualCoords(Coords, Unexplored[i].Coords) then
    begin
      reg := Unexplored[i];
      Break;
    end;

  if reg = nil then
  begin
    SetLength(Unexplored, Length(Unexplored) + 1);

    reg := TUnexplored.Create;
    reg.Coords := Coords;
    reg.Units := TUnitList.Create;
    
    Unexplored[High(Unexplored)] := reg;
  end;

  reg.Units.Add(AUnit);
end;

procedure ClearUnexplored;
var
  i: integer;
begin
  for i := 0 to High(Unexplored) do
  begin
    Unexplored[i].Units.Free;
    Unexplored[i].Free;
  end;

  SetLength(Unexplored, 0);
end;

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

function GetUnit(R: TRegion; var s: string; AllowZero: boolean; Stage: TTurnStage): TUnit;
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
    Result := R.FindUnit(num, Stage)
  else begin
    Troop := R.FindFaction(fac, Stage);
    if Troop <> nil then Result := Troop.Units.Find(num);
  end;
  
  // Raise exception if unit invalid (faction 1 new 1 may be invisible)
  if (Result = nil) and not ((fac >= 0) and (num < 0))
    and not (AllowZero and (num = 0)) then
    raise EParseError.Create('Bad target');
end;

procedure DoDescribe(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
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

procedure DoLocal(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
begin
  AUnit.LocalName := s;
  // UnitRecs.AddUnitRec(AUnit, s);
end;

procedure DoName(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
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

procedure DoExtFlag(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
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
procedure DoFlag(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
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

procedure DoClaim(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
var t2: string;
    val: integer;
begin
  // Instant
  t2 := GetToken(s);
  try
    val := Min(StrToInt(t2), Game.VirtTurn.Unclaimed);
    Game.VirtTurn.Unclaimed := Game.VirtTurn.Unclaimed - val;
    SetMoneyInc(AUnit, val);
    AUnit.Inventory.Add(NewMoneyitem(val, tsClaim));
  except
    on EConvertError do raise EParseError.Create('No amount given');
  end;
end;

// Take money from unclaimed, but do not modify unit (for global orders)
procedure DoClaimRepeat(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
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

procedure DoCombat(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
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

procedure DoDeclare(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
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

procedure DoFaction(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
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

procedure DoForm(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
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
  while (i < ARegion.Troops.Count) and
    (AUnit.Region.Troops[i].Faction.Num <> VFaction.Num) do Inc(i);
  if i < ARegion.Troops.Count then
    with ARegion.Troops[i] do begin
      j := 0;
      while (j < Units.Count) and (Units[j].Num <> -num) do Inc(j);
      if j < Units.Count then raise EParseError.Create('Duplicate number')
      else begin
        NewUnit := TUnit.Create;
        NewUnit.Name := 'Unit';
        NewUnit.Num := -num;
        NewUnit.Former := AUnit;
        NewUnit.Faction := AUnit.Faction;
        NewUnit.Region := ARegion;
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

procedure DoLeave(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
var Troop: TTroop;
    i: integer;
begin
  if (ARegion.Terrain.Flags and TER_WATER <> 0)
    and (not CanSwim(AUnit) or AUnit.Flags[flgNocross]) then
    raise EParseError.Create('Can''t leave in ocean');
  if (AUnit.Struct <> nil) and (AUnit.Struct.Owner = AUnit) then begin
    Troop := ARegion.PlayerTroop;
    i := 0;
    while (i < Troop.Units.Count) and not ((Troop.Units[i] <> AUnit)
      and (Troop.Units[i].Struct = AUnit.Struct)) do Inc(i);
    if i < Troop.Units.Count then AUnit.Struct.Owner := Troop.Units[i]
    else AUnit.Struct.Owner := nil;
  end;
  AUnit.Struct := nil;
end;

procedure DoEnter(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
var num, i: integer;
begin
  try
    num := StrToInt(GetToken(s))
  except
    on EConvertError do raise EParseError.Create('Invalid value');
  end;
  i := 0;
  while (i < ARegion.Structs.Count) and (ARegion.Structs[i].Num <> num) do
    Inc(i);
  if i < ARegion.Structs.Count then begin
    DoLeave(ARegion, AUnit, s, Line, 'leave');
    AUnit.Struct := ARegion.Structs[i];
    if ARegion.Structs[i].Owner = nil then
      ARegion.Structs[i].Owner := AUnit;
  end
  else raise EParseError.Create('Wrong object');
end;

procedure DoProduce(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
var IData: TItemData;
    Res: TItem;
    i, maxout, turnout, limitOut: integer;
    token: string;
begin
  token := GetToken(s);

  limitOut := -1;
  if (token[1] >= '0') and (token[1] <= '9') then begin
    limitOut := StrToInt(token);
    IData := Game.ItemData.FindByName(GetToken(s));
  end
  else
    IData := Game.ItemData.FindByName(token);

  if IData = nil then
    raise EParseError.Create('Unknown item');

  case ProduceOut(AUnit, ARegion, IData, maxout, turnout, limitOut, False) of
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
    Res := ARegion.Products.Find(IData.Short);
    if Res <> nil then Dec(Res.Amount, turnout);
  end;

  AUnit.MonthInfo.Data := IData;
  AUnit.MonthInfo.Max := maxout;
  AUnit.MonthInfo.Amount := turnout;

  for i := 0 to IData.Produce.Materials.Count - 1 do
  begin
    Res := IData.Produce.Materials[i];
    AUnit.Inventory.Add(NewItem(Res.Data, -Res.Amount * turnout, tsProduce, Format('for %s', [IData.Name])));
  end;

  AUnit.Inventory.Add(NewItem(IData, turnout, tsProduce));

  DoMonth(ARegion, AUnit, s, Line, Order);
end;

procedure DoPromote(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
var Target: TUnit;
begin
  Target := GetUnit(ARegion, s, False, tsPromote);
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

procedure DoSteal(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
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
  Target := ARegion.FindUnit(num, tsSteal);
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

procedure DoDestroy(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
var i, j: integer;
    AStruct: TStruct;
    R: TRegion;
begin
  if ARegion.Terrain.Flags and TER_WATER <> 0 then
    raise EParseError.Create('Can''t destroy in sea');
  AStruct := AUnit.Struct;
  R := ARegion;
  if (AStruct <> nil) and (AStruct.Owner = AUnit) then begin
    for i := 0 to R.Troops.Count-1 do
      for j := 0 to R.Troops[i].Units.Count-1 do
        if R.Troops[i].Units[j].Struct = AStruct then
          R.Troops[i].Units[j].Struct := nil;
    R.Structs.Delete(R.Structs.IndexOf(AStruct));
    AStruct.Free;
  end;
end;

procedure DoGive(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
var i, k, amount: integer;
    t3, t4: string;
    Target: TUnit;
    not_enough: boolean;

  procedure GiveItem(AUnit, Target: TUnit; i, amount: integer);
  var j: integer;
    itemData: TItemData;
    note: string;
  begin
    itemData := AUnit.Items[i].Data;

    if Target <> nil then begin
      // Adjust target unit's skills if men given
      if Test(itemData.Flags, IT_MAN) then
        for j := 0 to AUnit.Skills.Count-1 do
          AddSkillsToUnit(Target, AUnit.Skills[j].Data.Short, AUnit.Skills[j].Points, amount);

      // Give item
      SetAmountInc(Target, itemData, +amount);
      Target.Inventory.Add(NewItem(itemData, +amount, tsGive, 'from ' + AUnit.FullName));
    end;

    // Remove item
    SetAmountInc(AUnit, itemData, -amount);

    note := '';
    if Target <> nil then note := 'to ' + Target.FullName;
    AUnit.Inventory.Add(NewItem(itemData, -amount, tsGive, note));
  end;

begin
  not_enough := False;
  Target := GetUnit(ARegion, s, True, tsGive);

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

procedure DoPillage(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
var R: TRegion;
begin
  if (ARegion.Guard <> nil) and (ARegion.Guard.Num <> Faction.Num) then
      raise EParseError.Create('Unit on guard');
  if Taxers(AUnit) = 0 then raise EParseError.Create('Unit can''t tax');
  R := Map.Region(ARegion.Coords, Turn.Num);
  if Taxers(AUnit) * GameConfig.ReadInteger('Settings', 'TaxIncome', 50) <
    R.TaxRate div 2 then
    raise EParseError.Create('Too small unit to pillage');
  PillageUnits.Add(AUnit);
end;

procedure DoTax(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
begin
  if (ARegion.Guard <> nil) and (ARegion.Guard.Num <> Faction.Num)
    and (ARegion.Guard.Attitude < attFriendly) then
      raise EParseError.Create('Unit on guard');
  if Taxers(AUnit) = 0 then raise EParseError.Create('Unit can''t tax');
  TaxUnits.Add(AUnit);
  if GameConfig.ReadBool('Settings', 'MonthTax', False) then
    DoMonth(ARegion, AUnit, s, Line, Order);
end;

procedure DoTeach(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; AOrder: string);
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
    U := GetUnit(ARegion, s, False, tsTeach);
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
  DoMonth(ARegion, AUnit, s, Line, Order);
end;

procedure DoCast(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
var i: integer;
begin
  i := 0;
  while (i < Line) and (AUnit.Order(i) <> 'cast') do Inc(i);
  if i < Line then raise EParseError.Create('One cast permitted');
end;

procedure DoInvenotry(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
var
  amount, ts: integer;
  t, stage: string;
  item: TItemData;
begin
  stage := LowerCase(GetToken(s));

  try
    amount := StrToInt(GetToken(s));
  except
    exit;
  end;

  t := GetToken(s);
  item := Game.ItemData.FindByName(t);
  if item = nil then
    exit;

  ts := GetEnumValue(TypeInfo(TTurnStage), 'ts' + stage);
  if ts = -1 then
    exit;

  AUnit.Inventory.Add(NewItem(item, amount, TTurnStage(ts), 'manual entry'));
end;

procedure DoSell(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
var i, j, amt, income: integer;
    t2, t3, msg: string;
    idata: TItemData;
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

  idata := AUnit.Items[i].Data;

  with ARegion do begin
    // Make changes to region
    j := 0;
    while (j < Wanted.Count) and (Wanted[j].Data.Short <> AUnit.Items[i].Data.Short) do
      Inc(j);
    if j >= Wanted.Count then raise EParseError.Create('Not wanted');
    amt := Min(amt, Wanted[j].Amount);
    Wanted[j].Amount := Wanted[j].Amount - amt;
    // Trade items
    if Test(Wanted[j].Data.Flags, IT_TRADE) then
      SetFlag(ARegion.Marks, RM_TRADE, True);
    // Changes in unit
    SetAmountInc(AUnit, AUnit.Items[i].Data, -amt);

    income := Wanted[j].Cost * amt;
    Inc(AUnit.TradeIncome, income);

    if income > 0 then
    begin
      msg := Format('%d %s for $%d', [ amt, idata.Name(amt > 1), Wanted[j].Cost ]);
      AUnit.Inventory.Add(NewMoneyitem(income, tsSell, msg));
      AUnit.Inventory.Add(NewItem(idata, -amt, tsSell, msg));
    end;
  end;
end;

procedure DoAttack(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
var Target: TUnit;
begin
  while (Trim(s) <> '') do begin
    Target := GetUnit(ARegion, s, False, tsAttack);
    if Target <> nil then
      SetFlag(Target.Marks, UM_ATTACK);
  end;
end;

procedure DoBuy(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
var amt, i, j, expense: integer;
    t2, t3, msg: string;
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
  with ARegion do begin
    i := 0;
    while (i < ForSale.Count) and not TestItemName(ForSale[i].Data, t3) do Inc(i);
    if i >= ForSale.Count then raise EParseError.Create('No items for sale');

    if amt = -1 then amt := ForSale[i].Amount;

    if ForSale[i].Amount <> -1 then amt := Min(amt, ForSale[i].Amount);

    amt := Min(amt, (AUnit.Items.Amount(IT_SILVER) + AUnit.TradeIncome) div ForSale[i].Cost);

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
        SetFlag(ARegion.Marks, RM_TRADE, True);

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

      expense := ForSale[i].Cost * amt;
      SubtractTradeMoney(AUnit, expense);

      if expense > 0 then
      begin
        msg := Format('%d %s for $%d', [ amt, ForSale[i].Data.Name(amt > 1), ForSale[i].Cost ]);
        AUnit.Inventory.Add(NewMoneyItem(-expense, tsBuy, msg));
        AUnit.Inventory.Add(NewItem(ForSale[i].Data, amt, tsBuy, msg));
      end;
    end;
  end;
end;

procedure DoBuild(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; AOrder: string);
var i, lv, amt, maxout: integer;
    StData: TStructData;
    order, t: string;
    U: TUnit;

  function consumeResource(Resource: TItemData; Amount: integer): integer;
  var onHand, used: integer;
  begin
    Result := Amount;

    if Resource = nil then Exit;
    if Amount = 0 then Exit;

    onHand := AUnit.Inventory.AmountOn(Resource, tsBuild);
    used := Min(Amount, onHand);

    if used > 0 then
      AUnit.Inventory.Add(NewItem(StData.Material1, -used, tsBuild, StData.Group));

    Result := Amount - used;
  end;

begin
  if Pos('help', s) = 1 then begin
    // BUILD HELP <unit>
    GetToken(s);
    // TODO: Must look in final unit list
    U := GetUnit(ARegion, s, False, tsBuild);
    if U = nil then Exit;
    
    i := U.Orders.Count-1;
    while (i >= 0) and (U.Order(i) <> 'build') do Dec(i);
    
    if i < 0 then
      raise EParseError.Create('Target doesn''t building')
    else
      order := U.Orders[i];

    StData := nil;
    GetToken(order);

    if (Trim(order) = '') then begin
      if (U.Struct <> nil) then StData := U.Struct.Data;
    end
    else
      StData := Game.StructData.Find(GetToken(order));
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

    amt := consumeResource(StData.Material1, amt);
    consumeResource(StData.Material2, amt);
  end;

  DoMonth(ARegion, AUnit, s, Line, AOrder);
end;

procedure DoForget(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
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

procedure DoMonth(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
var ord: string;
begin
  ord := AUnit.MonthOrder;

  if Line = -1 then
    AUnit.MonthOrder := 'tax'
  else
    AUnit.MonthOrder := AUnit.Orders[Line];

  if ord <> '' then raise EParseError.Create('Overwriting previous monthlong order');

  // Trade
  if Line >= 0 then begin
    if (AUnit.Order(Line) = 'produce') or (AUnit.Order(Line) = 'build') then
      SetFlag(ARegion.Marks, RM_TRADE, True);

    if (AUnit.Order(Line) = 'produce') and (AUnit.MonthInfo.Data <> nil)
      and (TItemData(AUnit.MonthInfo.Data).Short = 'FISH') then
      SetFlag(ARegion.Marks, RM_FISHING, True);

    if (AUnit.Order(Line) = 'build') and (AUnit.MonthInfo.Data <> nil)
      and Test(TStructData(AUnit.MonthInfo.Data).Flags, ST_ROAD) then
      SetFlag(ARegion.Marks, RM_ROADBUILD, True);
  end;
end;

procedure DoEntertain(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
var Skill: TSkill;
begin
  Skill := AUnit.Skills.Find(Keys[s_EntertainSkill]);
  if Skill = nil then raise EParseError.Create('No entertainment skill');
  EntertainUnits.Add(AUnit);
  DoMonth(ARegion, AUnit, s, Line, Order);
end;

procedure DoEvict(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
var Target: TUnit;
begin
  Target := GetUnit(ARegion, s, False, tsEvict);
  if Target = nil then Exit;
  if AUnit.Struct = nil then
    raise EParseError.Create('Must be inside structure');
  if AUnit.Struct.Owner <> AUnit then
    raise EParseError.Create('Must be owner');
  if (Target.Struct <> AUnit.Struct) then
    raise EParseError.Create('Target not in same structure');
  Target.Struct := nil;
end;

procedure DoWork(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
begin
  WorkUnits.Add(AUnit);
  DoMonth(ARegion, AUnit, s, Line, Order);
end;

procedure DoStudy(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
var t2: string;
    SData: TSkillData;
    Skill: TSkill;
    i, cost: integer;
begin
  t2 := LowerCase(GetToken(s));
  SData := Game.SkillData.FindByName(t2);
  if SData = nil then raise EParseError.Create('Bad skill');
  
  // Max level
  Skill := AUnit.Skills.Find(SData.Short);
  if (Skill <> nil) and (Skill.Level = MaxSkillLevel(AUnit, SData)) then
    raise EParseError.Create('Maximum level reached');


  // Cost
  cost := StudyCost(AUnit, SData);
  if cost > AUnit.Items.Amount(IT_SILVER) +
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

  AUnit.Inventory.Add(NewMoneyItem(-cost, tsStudy, SData.Name));

  DoMonth(ARegion, AUnit, s, Line, Order);
end;

procedure DoMove(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; AOrder: string);
var i, mt, mp, d, struct, cap: integer;
    order, t2: string;
    C: TCoords;
    swim, sail: boolean;
    R: TRegion;

  procedure Finish;
  var i: integer;
      Troop: TTroop;
      vreg: TRegion;
      finalCoords: TCoords;
      moved: boolean;
  begin
    moved := false;
    AUnit.FinalPoint := High(AUnit.Moves);

    finalCoords := AUnit.FinalCoords;

    if not EqualCoords(ARegion.Coords, finalCoords) then begin
      moved := true;
      vreg := VTurn.Regions.Find(finalCoords);

      if vreg = nil then
        // if region was not found then unit moves into an unexplored region
        // no dependency is needed
        AddToUnexplored(finalCoords, AUnit)
      else
      begin
        vreg.ArrivingTroops.Seek(AUnit.Faction.Num).Units.Add(AUnit);
        VTurn.MakeDependecy(ARegion, vreg);
      end;
    end;

    if (order = 'sail') and (AUnit.Struct <> nil) and (AUnit.Struct.Owner = AUnit) then begin
      Troop := ARegion.PlayerTroop;

      for i := 0 to Troop.Units.Count-1 do
        if Troop.Units[i].Struct = AUnit.Struct then begin
          if Troop.Units[i].FinalPoint = -1 then begin
            Troop.Units[i].FinalPoint := 0;
          end;

          // add units to moved list
          if moved then begin
            if vreg = nil then
              AddToUnexplored(finalCoords, Troop.Units[i])
            else
            begin
              vreg.ArrivingTroops.Seek(AUnit.Faction.Num).Units.Add(Troop.Units[i]);
            end;
          end;

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

  C := ARegion.Coords;
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

  DoMonth(ARegion, AUnit, s, Line, AOrder);
end;

procedure DoTransport(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
var
  skill: TSkill;
  item: TItemData;
  target: TUnit;
  amount, toTransport, limit, dist, silver, cost, costPerUnit, maxDist, skillLevel: integer;
  t2, t3: string;
  isRemote, ownsTheTradeStructure: boolean;

begin
  // Possible formats
  // [unit] [num] [item]
  // [unit] ALL [item]
  // [unit] ALL [item] EXCEPT [amount]
  try
    // unit
    target := VTurn.FindUnit(StrToInt(GetToken(s)));
    if target = nil then
      raise EParseError.Create('Target unit not found');
    
    // if (target.Faction.Num <> AUnit.Faction.Num) and (target.Faction.Attitude < attFriendly) then
    //   raise EParseError.Create('Target unit is not friendly');

    if target = AUnit then
      raise EParseError.Create('Cannot transport to self');

    // num
    t2 := AnsiLowerCase(GetToken(s));
    if t2 = '' then
      raise EParseError.Create('Expected ALL or <number> as number of items')
    else if t2 = 'all' then
      toTransport := -1
    else
      toTransport := StrToInt(t2);

    // item
    t3 := GetToken(s);
    if t3 = '' then
      raise EParseError.Create('Expected item name');
    item := Game.ItemData.FindByName(t3);
    if item = nil then
      raise EParseError.Create('Unknown item');

    // except
    limit := 0;
    if AnsiLowerCase(GetToken(s)) = 'except' then
      limit := StrToInt(GetToken(s));
  except
    on EConvertError do Exit;
  end;

  // Make sure the target and unit are at least friendly
  if not target.Faction.Player and (target.Faction.Attitude < attFriendly) then
    raise EParseError.Create('Target unit is not friendly');

  dist := Distance(AUnit.FinalCoords, target.FinalCoords);
  if dist < 0 then
    raise EParseError.Create('Cannot ' + Order + ' to another plane');
  isRemote := dist > 2;

  // Make sure the target of a transport order is a unit
  // with the quartermaster skill who owns a transport
  // structure
  if Order = 'transport' then
  begin
    skill := target.Skills.Find(Keys[s_Quartermaster]);
    if (skill = nil) or (skill.Level < 1) then
      raise EParseError.Create('Target must be a quartermaster');

    if (target.Struct = nil)
      or (target.Struct.Data.Group <> Keys[s_Caravanserai])
      or (target.Struct.Owner <> target) 
      or (target.Struct.Needs > 0)
    then
      raise EParseError.Create('Target must own a transport structure');
  end;

  // make sure target is in range.
  maxDist := 2;

  skill := AUnit.Skills.Find(Keys[s_Quartermaster]);
  if skill <> nil then
    skillLevel := skill.Level
  else
    skillLevel := 0;

  ownsTheTradeStructure := (AUnit.Struct <> nil) and (AUnit.Struct.Needs = 0) and (AUnit.Struct.Data.Group = Keys[s_Caravanserai]) and (AUnit.Struct.Owner = AUnit);

  if (Order = 'transport') and ownsTheTradeStructure then
  begin
    if skillLevel = 1 then maxDist := 3;
    if (skillLevel >= 2) and (skill.Level <= 4) then maxDist := 4;
    if skillLevel = 5 then maxDist := 5;
  end;

  if dist > maxDist then
    raise EParseError.Create('Target is out of range');

  // On long range transport or distribute, make sure the
  // issuer is a quartermaster and is owner of a structure
  if (Order = 'distribute') or (isRemote and (Order = 'transport')) then
  begin
    if skillLevel < 1 then
      raise EParseError.Create('Unit must be a quartermaster');

    if not ownsTheTradeStructure then
      raise EParseError.Create('Unit must own a transport structure');
  end;

  amount := AUnit.Inventory.AmountOn(item, tsTransport);
  if toTransport = -1 then
    toTransport := amount;
  toTransport := Max(0, toTransport - limit);
  
  if toTransport > amount then
    raise EParseError.Create('Not enough items to ' + Order);

  if isRemote then
  begin
    costPerUnit := 4 - ((skillLevel + 1) div 2);
    cost := toTransport * item.Weight * costPerUnit;

    silver := AUnit.Inventory.AmountOn(IT_SILVER, tsTransport);
    if cost > silver then
      raise EParseError.Create('Not enough silver to pay for distribution (needs $' + IntToStr(cost) + ')');

    AUnit.Inventory.Add(NewMoneyItem(-cost, tsTransport, 'transport cost'));
  end;

  AUnit.Inventory.Add(NewItem(item, -toTransport, tsTransport, 'to ' + target.FullName));
  target.Inventory.Add(NewItem(item, toTransport, tsTransport, 'from ' + AUnit.FullName));

  if not EqualCoords(AUnit.FinalCoords, target.FinalCoords) then
    VTurn.MakeDependecy(ARegion, VTurn.Regions.Find(target.FinalCoords));
end;

procedure ResolveMaintenance(Coords: TCoords; Units: TUnitList; ParseErrors: TStrings);
  type TConsumer = record
    Upkeep: integer;
    URef: TUnit;
  end;

  type TFood = record
    Value, Order, Amount: integer;
    Data: TItemData;
    Owner: TUnit;
  end;

  type TMoney = record
    Amount: integer;
    Owner: TUnit;
  end;

  type TConsumerArray = array of TConsumer;
  type TMoneyArray = array of TMoney;
  type TFoodArray = array of TFood;

  type TConsumeItem = (ciFood, ciSilver);
  type TConsumeScope = (csOwn, csAll);

var i: integer;
    consumers: TConsumerArray;
    silverStack: TMoneyArray;
    foodStack: TFoodArray;
    warn: boolean;
    U: TUnit;

  procedure swap(var a: TFoodArray; i, j: integer);
  var
    tmp: TFood;

  begin
    tmp := a[i];
    a[i] := a[j];
    a[j] := tmp;
  end;

  procedure sortFood(var arr: TFoodArray);
  var
    i, j: integer;

  begin
    if Length(arr) <= 1 then Exit;

    for i := 0 to High(arr) do
      for j := 0 to High(arr) - 1 do
        if arr[j].Order < arr[j + 1].Order then
          swap(arr, j, j + 1);
  end;

  procedure addFood(var items: TFoodArray; amount: integer; data: TItemData; owner: TUnit);
  var
    food: TFood;
  begin
    food.Value := data.Food.Value;
    food.Order := data.Food.Order;
    food.Amount := amount;
    food.Data := data;
    food.Owner := owner;

    if food.Value = 0 then Exit;
    if food.Amount = 0 then Exit;

    SetLength(items, Length(items) + 1);
    items[High(items)] := food;
  end;

  procedure addMoney(var items: TMoneyArray; amount: integer; owner: TUnit);
  var
    money: TMoney;
  begin
    money.Amount := amount;
    money.Owner := owner;

    SetLength(items, Length(items) + 1);
    items[High(items)] := money;
  end;

  procedure addConsumer(U: TUnit);
  var
    j, silver: integer;
    Item: TItem;
    rec: TConsumer;
    balance: TItemList;

  begin
    silver := 0;
    rec.Upkeep := 0;
    rec.URef := U;

    // As we going to calculate upkeep, we need to clear the inventory changes for this stage
    // otherwise we will get incorrect values.
    // This should not be needed but Advisor does not recreate arriving units and their inventory will be preserved
    // between maintenance invocations. This is a workaround for that.
    u.Inventory.ClearStage(tsUpkeep);

    balance := U.Inventory.BalanceBefore(tsUpkeep);
    for j := 0 to balance.Count - 1 do begin
      // Set values from inventory
      Item := balance[j];

      if Test(Item.Data.Flags, IT_MAN) then
        Inc(rec.Upkeep, Item.Amount * Item.Data.Upkeep.Silver)
      else if Test(Item.Data.Flags, IT_SILVER) then
        Inc(silver, Item.Amount)
      else if Test(Item.Data.Flags, IT_FOOD) then
        addFood(foodStack, Item.Amount, Item.Data, U);
    end;
    balance.ClearAndFree();

    if silver > 0 then addMoney(silverStack, silver, U);


    SetLength(consumers, Length(consumers) + 1);
    consumers[High(consumers)] := rec;
  end;

  // consume food from the stack (stack is sorted from least to most valuable food)
  // update the food stack
  // return new upkeep value
  function consume(consumer: TUnit; upkeep: integer; targetItem: TConsumeItem; scope: TConsumeScope; var silverItems: TMoneyArray; var foodItems: TFoodArray): integer;
  var
    consume, j, hi: integer;
    owner: TUnit;
    food: TFood;
    silver: TMoney;

  begin
    j := 0;
    if targetItem = ciFood then
      hi := High(foodItems)
    else
      hi := High(silverItems);

    while (upkeep > 0) and (j <= hi) do
    begin
      if targetItem = ciFood then
      begin
        food := foodItems[j];
        owner := food.Owner;
        
        if food.Amount = 0 then
        begin
          Inc(j);
          Continue;
        end;
      end
      else
      begin
        silver := silverItems[j];
        owner := silver.Owner;
        
        if silver.Amount = 0 then
        begin
          Inc(j);
          Continue;
        end;
      end;

      if (scope = csOwn) and (owner <> consumer) then
      begin
        Inc(j);
        Continue;
      end;

      // determine how much food or silver to consume
      // food can be consumed only in whole units
      if targetItem = ciFood then
        consume := Min(food.Amount, Ceil(upkeep / food.Value))
      else
        consume := Min(silver.Amount, upkeep);

      if consumer <> owner then
      begin
        if targetItem = ciFood then
        begin
          owner.Inventory.Add(NewItem(food.Data, -consume, tsUpkeep, Format('Shares food with %s', [ U.FullName ])));
          U.Inventory.Add(NewItem(food.Data, consume, tsUpkeep, Format('Borrows food from %s', [ owner.FullName ])));
        end
        else
        begin
          owner.Inventory.Add(NewMoneyItem(-consume, tsUpkeep, Format('Shares silver with %s', [ U.FullName ])));
          U.Inventory.Add(NewMoneyItem(consume, tsUpkeep, Format('Borrows silver from %s', [ owner.FullName ])));
        end;
      end;

      if targetItem = ciFood then
      begin
        U.Inventory.Add(NewItem(food.Data, -consume, tsUpkeep));

        upkeep := Max(0, upkeep - consume * food.Value);
        Dec(foodItems[j].Amount, consume);
      end
      else
      begin
        U.Inventory.Add(NewMoneyItem(-consume, tsUpkeep));

        upkeep := Max(0, upkeep - consume);
        Dec(silverItems[j].Amount, consume);
      end;

      Inc(j);
    end;

    Result := upkeep;
  end;

begin
  SetLength(consumers, 0);
  SetLength(foodStack, 0);
  SetLength(silverStack, 0);

  for i := 0 to Units.Count - 1 do
    addConsumer(Units[i]);

  // sort food so that more valuable food is consumed first
  sortFood(foodStack);

  warn := False;

  // Order of actions:
  // 1) consume food from other units when Consuming = ctFaction
  // 2) consume own silver
  // 3) consume silver from other units
  // 4) warn that not enough money for maintenance
  for i := 0 to Length(consumers) - 1 do begin
    if consumers[i].Upkeep = 0 then Continue;
    U := consumers[i].URef;

    // consume own food first if it is set to consume food
    if U.Consuming <> ctSilver then
    begin
      consumers[i].Upkeep := consume(U, consumers[i].Upkeep, ciFood, csOwn, silverStack, foodStack);
      if consumers[i].Upkeep = 0 then Continue;
    end;

    // if consuming food from other units
    if U.Consuming = ctFaction then
    begin
      consumers[i].Upkeep := consume(U, consumers[i].Upkeep, ciFood, csAll, silverStack, foodStack);
      if consumers[i].Upkeep = 0 then Continue;
    end;

    if U.Consuming <> ctSilver then
      U.Orders.Insert(0, ';. Not enough food, will use money for maintenance');

    // consume silver own silver first
    consumers[i].Upkeep := consume(U, consumers[i].Upkeep, ciSilver, csOwn, silverStack, foodStack);
    if consumers[i].Upkeep = 0 then Continue;

    // consume silver from other units if not enough own silver
    consumers[i].Upkeep := consume(U, consumers[i].Upkeep, ciSilver, csAll, silverStack, foodStack);

    // warn if not enough money for maintenance
    if consumers[i].Upkeep > 0 then
    begin
      U.Orders.Insert(0, ';. Missing $' + IntToStr(consumers[i].Upkeep) + ' for maintenance');
      U.Inventory.Add(NewMoneyItem(-consumers[i].Upkeep, tsUpkeep, 'Missing'));

      if not warn then ParseErrors.AddObject('!M4 ' + MakeRegionName(Coords, True) + ': Units hungry', U);
      warn := True;
    end;
  end;
end;

// Split taxes evenly between taxers
procedure ResolveTaxes(ARegion: TRegion; TaxUnits: TUnitList; RateMul, IncomeMul: integer);
var i, total_men, men, tax_rate, tax_income, amt: integer;
    cash: real;
    U: TUnit;
begin
  tax_income := GameConfig.ReadInteger('Settings', 'TaxIncome', 50);
  total_men := 0;

  for i := 0 to TaxUnits.Count - 1 do
    total_men := total_men + Taxers(TaxUnits[i]);

  if total_men > 0 then begin
    tax_rate := ARegion.TaxRate * RateMul;
    cash := Min(tax_income * IncomeMul, tax_rate / total_men);
    
    for i := 0 to TaxUnits.Count - 1 do
    begin
      U := TaxUnits[i];
      men := Taxers(U);
      Inc(ARegion.Activity.Taxers, men);
      amt := Round(men * cash);
      tax_rate := tax_rate - amt;

      Inc(U.TradeIncome, amt);
      U.Inventory.Add(NewMoneyItem(amt, tsPillageOrTax));

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
var i, total, amt, ent_income, income: integer;
    cash: real;
    U: TUnit;
begin
  ent_income := GameConfig.ReadInteger('Settings', 'EntertainIncome', 20);
  total := 0;
  for i := 0 to EntertainUnits.Count-1 do
    Inc(total, EntertainOut(EntertainUnits[i]));

  if total > 0 then begin
    cash := Min(1, ARegion.Entertainment / total);

    for i := 0 to EntertainUnits.Count-1 do
    begin
      U := EntertainUnits[i];
      amt := EntertainOut(U);
      Inc(ARegion.Activity.Entertainers, amt div ent_income);
      
      income := Round(amt * cash);

      Dec(ARegion.Entertainment, income);
      Inc(U.WorkIncome, income);

      U.MonthInfo.Max := amt;
      U.MonthInfo.Amount := income;

      U.Inventory.Add(NewMoneyItem(income, tsEntertain));
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

      U.Inventory.Add(NewMoneyItem(amt, tsWork));
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

procedure DoTag(ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);
begin
  AUnit.Tag := GetToken(s);

  if Length(s) > 0 then begin
    AUnit.TagColor := StrToInt(GetToken(s));
  end;

  if Length(s) > 0 then begin
    AUnit.TagTextColor := StrToInt(GetToken(s));
  end;
end;

end.
