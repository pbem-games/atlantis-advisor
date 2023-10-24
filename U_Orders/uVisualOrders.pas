unit uVisualOrders;

interface

uses
  SysUtils, Windows, DataStructs, uSelAmount, Resources, uGameSubs,
  Graphics, Math, Controls, Dialogs, MyStrings, Menus, Classes,
  uClaim, uHexMap, uKeys, uBuy, uNewUnit, uTeach, uScript, uUnitRecs,
  uSpyReps, uNeeds, uRoute, uUnitNeeds, uShortcuts, uWantedItems,
  uPathFind;

type
  TOrderHandlers = class
    Item: TItem;
    SkillData: TSkillData;
    // Skill menu
    procedure StudySkill(Sender: TObject);
    procedure CastSkill(Sender: TObject);
    procedure CombatSkill(Sender: TObject);
    procedure ForgetSkill(Sender: TObject);
    // Item menu
    procedure ItemGive(Sender: TObject);
    procedure ItemNeeds(Sender: TObject);
    procedure AllItemNeeds(Sender: TObject);
    procedure ItemSell(Sender: TObject);
    procedure ItemWanted(Sender: TObject);
    // Unit menu
    procedure Repeating(Sender: TObject);
    procedure ShowSpyRep(Sender: TObject);
    procedure AddAttacker(Sender: TObject);
    procedure AddArmyAttacker(Sender: TObject);
    procedure AddDefender(Sender: TObject);
    procedure ClearSim(Sender: TObject);
    procedure CancelForm(Sender: TObject);
    procedure SetArmy(Sender: TObject);
    procedure ArmyMove(Sender: TObject);
    procedure ArmyAdvance(Sender: TObject);
    procedure ArmyCustom(Sender: TObject);
    procedure ArmyRemOrder(Sender: TObject);
    procedure MergeArmy(Sender: TObject);
    procedure DeclareNeeds(Sender: TObject);
    procedure DistribNeeds(Sender: TObject);
    procedure DeclareRoute(Sender: TObject);
    procedure DeclareSailRoute(Sender: TObject);
    procedure DoRunRoute(Sender: TObject);
    procedure MoveTo(Sender: TObject);

    procedure Advance(Sender: TObject);
    procedure Assassinate(Sender: TObject);
    procedure Attack(Sender: TObject);
    procedure AttackUnit(Sender: TObject);
    procedure Build(Sender: TObject);
    procedure BuildNew(Sender: TObject);
    procedure BuildHelp(Sender: TObject);
    procedure Buy(Sender: TObject);
    procedure Cast(Sender: TObject);
    procedure Claim(Sender: TObject);
    procedure DescribeUnit(Sender: TObject);
    procedure DescribeObject(Sender: TObject);
    procedure Enter(Sender: TObject);
    procedure Entertain(Sender: TObject);
    procedure Evict(Sender: TObject);
    procedure Form(Sender: TObject);
    procedure FormTemplate(Sender: TObject);
    procedure Leave(Sender: TObject);
    procedure LocalDescription(Sender: TObject);
    procedure Move(Sender: TObject);
    procedure NameFaction(Sender: TObject);
    procedure NameObject(Sender: TObject);
    procedure NameUnit(Sender: TObject);
    procedure Produce(Sender: TObject);
    procedure Promote(Sender: TObject);
    procedure Pillage(Sender: TObject);
    procedure Sail(Sender: TObject);
    procedure Sell(Sender: TObject);
    procedure Script(Sender: TObject);
    procedure Study(Sender: TObject);
    procedure Tax(Sender: TObject);
    procedure Teach(Sender: TObject);
    procedure Work(Sender: TObject);
  end;

var
  Handlers: TOrderHandlers;
  GiveToMovement: integer;

  procedure CustomizeUnitPopup(AUnit: TUnit);
  procedure CustomizeSkillPopup(SkillData: TSkillData; Level: integer);
  procedure CustomizeItemPopup(Item: TItem);
  procedure AddOrder(s: string; CanRepeat: boolean);
  procedure AddOrderTo(AUnit: TUnit; s: string; CanRepeat: boolean);
  procedure ExecOrder(s: string; CanRepeat: boolean);
  function NextAttemptedNew(R: TRegion): integer;
  procedure ClearMoves;
  procedure ClearUnitMoves(AUnit: TUnit);
  procedure GetArmyOut;
  procedure SpreadMoveOrders(Order: string);
  function InsideConstruction(AUnit: TUnit; Index: integer): boolean;
  procedure MakeEnterMenu(U: TUnit; Item: TMenuItem);
  procedure MakeStudyMenu(U: TUnit; Item: TMenuItem);
  procedure MakeProduceMenu(U: TUnit; Item: TMenuItem);
  procedure MakeBuildMenu(AUnit: TUnit; Item: TMenuItem);
  procedure MakeCastMenu(U: TUnit; Item: TMenuItem);
  procedure MakeBuyMenu(U: TUnit; Item: TMenuItem);
  procedure MakeSellMenu(U: TUnit; Item: TMenuItem);
  procedure MakeAttackMenu(AUnit: TUnit; Item: TMenuItem; Handler: TNotifyEvent);
  procedure MakeStructUnitsMenu(AUnit: TUnit; Item: TMenuItem; Handler: TNotifyEvent);

implementation

uses Main;

// Issue move orders similar to current unit's to whole army
procedure SpreadMoveOrders(Order: string);
var i: integer;
    s: string;
    Troop: TTroop;
    U: TUnit;
begin
  if not MoveArmy or (CurrUnit.UArmy = nil) then Exit;
  // Find MOVE order
  i := CurrUnit.Orders.Count-1;
  while (i >= 0) and (CurrUnit.Order(i) <> Order) do Dec(i);
  if i < 0 then Exit;
  s := CurrUnit.Orders[i];
  // Cut off 'OUT' order for current unit, we have it by GetArmyOut
  i := Pos(MoveOrder, s) + Length(MoveOrder);
  if Pos('OUT', s) = i + 1 then
    s := Copy(s, 1, i) + Copy(s, i+5, Length(s));
  // Give orders to all other units
  Troop := CurrRegion.PlayerTroop;
  if Troop = nil then Exit;
  for i := 0 to Troop.Units.Count-1 do begin
    U := Troop.Units[i];
    if U.UArmy = CurrUnit.UArmy then begin
      if U <> CurrUnit then begin
        AddOrderTo(U, s, False);
        U.Moves := CurrUnit.Moves;
        if U.Orders.Count > 0 then
          U.MonthOrder := U.Orders[U.Orders.Count-1];
      end;
    end;
  end;
end;

function RepeatSign: string;
var rep: boolean;
begin
  rep := Config.ReadBool('MainWin', 'RepeatingOrders', FALSE);
  if ShiftPressed then rep := not rep;
  if rep then Result := '@' else Result := '';
end;

// Check if order of CurrUnit inside TURN or FORM construction
function InsideConstruction(AUnit: TUnit; Index: integer): boolean;
var i, cform, cturn: integer;
begin
  cform := 0;
  cturn := 0;
  for i := 0 to Index-1 do begin
    if AUnit.Order(i) = 'turn' then Inc(cturn)
    else if AUnit.Order(i) = 'endturn' then Dec(cturn)
    else if AUnit.Order(i) = 'form' then Inc(cform)
    else if AUnit.Order(i) = 'end' then Dec(cform);
  end;
  Result := (cturn > 0) or (cform > 0);
end;

// Scan orders of units to determine next unit number to form (for scripts)
function NextAttemptedNew(R: TRegion): integer;
var i, j: integer;
    Troop: TTroop;
    Trace: TTrace;
begin
  Result := 0;
  Troop := R.PlayerTroop;
  if Troop <> nil then begin
    for i := 0 to Troop.Units.Count-1 do
      for j := 0 to Troop.Units[i].Orders.Count-1 do
        if (Troop.Units[i].Order(j) = 'form')
          and not InsideConstruction(Troop.Units[i], j) then begin
          Trace := TTrace.Create(LowerCase(Troop.Units[i].Orders[j]));
          Trace.Before('form');
          while Copy(Trace.Text, 1, 1) = ' ' do Inc(Trace.StPos);
          Result := Max(Result, Trace.Num);
          Trace.Free;
        end;
  end;
  Inc(Result);
end;

procedure ClearUnitMoves(AUnit: TUnit);
var i: integer;
begin
  SetLength(AUnit.Moves, 0);
  i := 0;
  while i < AUnit.Orders.Count do
    if ( (AUnit.Order(i) = 'move') or (AUnit.Order(i) = 'advance')
      or (AUnit.Order(i) = 'sail') ) and not InsideConstruction(AUnit, i) then
      AUnit.Orders.Delete(i)
    else Inc(i);
  AUnit.MonthOrder := '';
  AUnit.FinalPoint := -1;
end;

procedure ClearMoves;
var i: integer;
    Troop: TTroop;
begin
  if not MoveArmy then ClearUnitMoves(CurrUnit)
  else begin
    if CurrUnit.UArmy = nil then Exit;
    Troop := CurrRegion.PlayerTroop;
    if Troop = nil then Exit;
    for i := 0 to Troop.Units.Count-1 do
      if Troop.Units[i].UArmy = CurrUnit.UArmy then
        ClearUnitMoves(Troop.Units[i]);
  end;
end;

procedure GetArmyOut;
var Troop: TTroop;
    i: integer;
begin
  if not MoveArmy or (CurrUnit.UArmy = nil) then begin
    if (CurrUnit.Struct <> nil) then AddOrder(MoveOrder + ' OUT', True)
  end
  else begin
    Troop := CurrUnit.Region.PlayerTroop;
    if Troop = nil then Exit;
    for i := 0 to Troop.Units.Count-1 do
      if (Troop.Units[i].UArmy = CurrUnit.UArmy)
        and (Troop.Units[i].Struct <> nil) then
        AddOrderTo(Troop.Units[i], MoveOrder + ' OUT', True)
  end;
end;

function ParseGiveOrder(Order: string; var Target: string; var Amount: integer;
  var Token: string): boolean;
var t3: string;
begin
  Result := False;
  Target := '';
  try
    GetToken(Order); // give
    Target := LowerCase(GetToken(Order));
    if Target = 'new' then Target := Target + ' ' + GetToken(Order);
    t3 := LowerCase(GetToken(Order));
    if (t3 = 'unit') then Exit;
    if (t3 = 'all') then Amount := -1
    else Amount := StrToInt(t3);
    Token := GetToken(Order);
    Result := True;
  except
  end;
end;

function ModifyGiveOrder(Order: string; Target: string; Amount: integer): string;
var Trace: TTrace;
    amt: string;
begin
  Result := '';
  if Amount = 0 then Exit;
  Trace := TTrace.Create(Order);
  Result := Trace.Before(Target) + Target;
  Trace.Text := TrimLeft(Trace.Text);
  Trace.Before(' ');
  if Amount >= 0 then amt := IntToStr(Amount)
  else amt := 'all';
  Result := Result + ' ' + amt + ' ' + Trace.Text;
  Trace.Free;
end;

function FilterFor(s: string): DWord;
var k: integer;
begin
  k := ItemClassCount-1;
  while (k >= 0) and (ItemClasses[k] <> s) do Dec(k);
  if k >= 0 then Result := ItemFilters[k]
  else Result := 0;
end;

// Compares HORS (item) and MOUNT (item-type) objects for give operations
function SameGiveTokens(s1, s2: string; var Exact: boolean): boolean;
var Data1, Data2: TItemData;
begin
  Exact := False;
  if s1 = s2 then begin
    Result := True;
    Exact := True;
  end
  else begin
    Data1 := Game.ItemData.FindByName(s1);
    Data2 := Game.ItemData.FindByName(s2);
    if (Data1 <> nil) and (Data2 <> nil) and (Data1.Short = Data2.Short) then begin
      Result := True;
      Exact := True;
    end
    else if (Data1 = nil) and (Data2 <> nil) then
      Result := Test(Data2.Flags, FilterFor(s1))
    else if (Data1 <> nil) and (Data2 = nil) then
      Result := Test(Data1.Flags, FilterFor(s2))
    else Result := ((FilterFor(s1) and FilterFor(s2)) <> 0);
  end;
end;

function ActualAmount(AUnit: TUnit; Name: string): integer;
var Item: TItem;
    IData: TItemData;
begin
  Result := 0;
  IData := Game.ItemData.FindByName(Name);
  if IData <> nil then begin
    Item := AUnit.Items.Find(IData.Short);
    if Item <> nil then Result := Item.Amount;
  end;
end;

function RealAmount(AUnit: TUnit; Name: string): integer;
var RealU: TUnit;
begin
  RealU := Faction.Units.Find(AUnit.Num);
  if RealU = nil then Result := 0
  else Result := ActualAmount(RealU, Name);
end;

// Redirect gives from other units to this order's target; modify order
// Order may not contain "ALL <item_class>" tokens
procedure AdjustItemGive(Giver: TUnit; var Order: string);
var i, j, amt, amt1: integer;
    s, target, target1, token, token1: string;
    LFac: TTroop;
    order_giveall, was_giveall, exact: boolean;
begin
  if not ParseGiveOrder(Order, target, amt, token) then Exit;
  // If order is GIVE ALL, get actual value for calculations
  order_giveall := (amt = -1);
  if order_giveall then amt := ActualAmount(Giver, token);
  // Scan other units for GIVE orders to current unit
  LFac := Giver.Region.Troops.Find(Faction.Num);
  i := 0;
  while (i < LFac.Units.Count) and (amt > 0) do begin
    if LFac.Units[i].Num <> Giver.Num then begin
      j := 0;
      while (j < LFac.Units[i].Orders.Count) and (amt > 0) do begin
        if (LFac.Units[i].Order(j) = 'give') and
          not InsideConstruction(LFac.Units[i], j) then begin
          // Parse Give order
          if ParseGiveOrder(LFac.Units[i].Orders[j], target1, amt1, token1) then begin
            if (target1 = Giver.NumStr)
              and SameGiveTokens(token1, token, exact) then begin
              was_giveall := (amt1 = -1);
              if was_giveall then amt1 := RealAmount(LFac.Units[i], token);
              // Give items to Giver's order target
              if target <> LFac.Units[i].NumStr then begin
                s := ModifyGiveOrder(Order, target, Min(amt1, amt));
                LFac.Units[i].Orders.Insert(j, s);
                Inc(j);
              end;
              // Do not give them to Giver
              if not was_giveall then begin
                if amt >= amt1 then begin
                  LFac.Units[i].Orders.Delete(j);
                  Dec(j);
                end
                else begin
                  LFac.Units[i].Orders[j] := ModifyGiveOrder(LFac.Units[i].Orders[j],
                    target1, amt1 - amt);
                end;
              end;
              if amt1 <> -1 then amt := Max(0, amt - amt1)
              else amt := -1;
            end;
          end;
        end;
        Inc(j);
      end;
    end;
    Inc(i);
  end;
  if amt = 0 then Order := ''
  else if not order_giveall then Order := ModifyGiveOrder(Order, target, amt);
end;

procedure AdjustGiveOrders(Giver: TUnit; var Order: string);
var i, amt: integer;
    s, target, token: string;
    Filter: DWord;
begin
  if not ParseGiveOrder(Order, target, amt, token) then Exit;
  Filter := FilterFor(token);
  if Filter = 0 then
    // For GIVE <short> order, just adjust
    AdjustItemGive(Giver, Order)
  else if amt = -1 then
    // For GIVE ALL <item_class>, adjust separately for all filtered items
    for i := 0 to Giver.Items.Count-1 do
      if Test(Giver.Items[i].Data.Flags, Filter) then begin
        s := 'give ' + target + ' all ' + Giver.Items[i].Data.Short;
        AdjustItemGive(Giver, s);
      end;
end;

procedure AddOrderTo(AUnit: TUnit; s: string; CanRepeat: boolean);
const SingleCount = 27;
      SingleOrders: array[0..SingleCount-1] of string = ('autotax', 'avoid',
        'behind', 'combat', 'consume', 'faction', 'guard', 'hold', 'noaid',
        'nocross', 'reveal', 'spoils', 'leave', 'enter', 'promote', 'destroy',
        'pillage', 'tax', 'cast', 'build', 'entertain', 'produce',
        'study', 'teach', 'work', 'steal', 'share');
      TokenCount = 4;
      TokenOrders: array[0..TokenCount-1] of string = ('name', 'describe',
        'declare', 'forget');
      MonthCount = 9;
      MonthFirstNonrepeat = 3;
      CMonthOrders: array[0..MonthCount-1] of string = ('sail', 'move',
        'advance', 'build', 'entertain', 'produce', 'study', 'teach', 'work');
var s1, t1, s2, t2: string;
    i, j, k: integer;
    MonthOrders: array of string;
begin
  MainForm.OrdersChanged := True;
  if CanRepeat then s := RepeatSign + s;
  if not InsideConstruction(AUnit, AUnit.Orders.Count) then begin
    s1 := s;
    t1 := GetToken(s1);
    s2 := s1;
    t2 := GetToken(s1);

    if t1 <> '' then begin
      // Cheat for monthlong TAX
      SetLength(MonthOrders, MonthCount);
      for i := 0 to MonthCount-1 do MonthOrders[i] := CMonthOrders[i];
      if Config.ReadBool('Settings', 'MonthTax', False) then begin
        SetLength(MonthOrders, MonthCount + 1);
        MonthOrders[MonthCount + 1] := 'tax';
      end;

      if t1[1] = '@' then t1 := Copy(t1, 2, Length(t1)-1);

      if t1 = 'give' then AdjustGiveOrders(AUnit, s);

      // MOVE orders to single line
      if (t1 = 'move') or (t1 = 'advance') or (t1 = 'sail') then begin
        i := 0;
        while (i < AUnit.Orders.Count)
          and not ( (AUnit.Order(i) = t1)
            and not InsideConstruction(AUnit, i) ) do Inc(i);
        if (i < AUnit.Orders.Count) then begin
          s := '';
          AUnit.Orders[i] := AUnit.Orders[i] + ' ' + s2;
        end;
      end;

      // Remove single orders (order like AUTOTAX may appear once)
      i := 0;
      while (i < SingleCount) and (SingleOrders[i] <> t1) do Inc(i);
      if i < SingleCount then begin
        j := 0;
        while j < AUnit.Orders.Count do begin
          if (AUnit.Order(j) = t1) and not InsideConstruction(AUnit, j) then
            AUnit.Orders.Delete(j)
          else Inc(j);
        end;
      end;

      // Remove local description if new given
      if Pos('@;;', Trim(s)) = 1 then begin
        i := 0;
        while i < AUnit.Orders.Count do
          if Pos('@;;', Trim(AUnit.Orders[i])) = 1 then
            AUnit.Orders.Delete(i)
          else Inc(i);
      end;

      // Remove token orders (like DECLARE 28)
      i := 0;
      while (i < TokenCount) and (TokenOrders[i] <> t1) do Inc(i);
      if i < TokenCount then begin
        j := 0;
        while j < AUnit.Orders.Count do begin
          if (Pos(t1 + ' ' + t2, TrimLeft(LowerCase(AUnit.Orders[j])))
            in [1, 2])
            and not InsideConstruction(AUnit, j) then AUnit.Orders.Delete(j)
          else Inc(j);
        end;
      end;

      if Config.ReadBool('MainWin', 'OverwriteMonthlong', False) then begin
        // Remove month-long orders if new month-long given
        i := 0;
        while (i < MonthCount) and (MonthOrders[i] <> t1) do Inc(i);
        if i < MonthCount then begin
          j := 0;
          while j < AUnit.Orders.Count do begin
            k := 0;
            while (k < MonthCount) and
              not (AUnit.Order(j) = MonthOrders[k]) do Inc(k);
            if (k < MonthCount) and not InsideConstruction(AUnit, j)
              and not ((k < MonthFirstNonrepeat) and (AUnit.Order(j) = t1)) then
              AUnit.Orders.Delete(j)
            else Inc(j);
          end;
        end;
      end;
    end;
  end;

  // Add line
  if s <> '' then AUnit.Orders.Text := AUnit.Orders.Text + s;
end;

procedure AddOrder(s: string; CanRepeat: boolean);
begin
  AddOrderTo(CurrUnit, s, CanRepeat);
end;

procedure ExecOrder(s: string; CanRepeat: boolean);
var i: integer;
    Lines: TStrings;
begin
  Lines := TStringList.Create;
  Lines.Text := s;
  for i := 0 to Lines.Count-1 do AddOrder(Lines[i], CanRepeat);
  Lines.Free;
  MainForm.ProcessOrders(CurrUnit.Region);
end;

  { Order handlers }

procedure TOrderHandlers.StudySkill(Sender: TObject);
var cost: integer;
begin
  if Config.ReadBool('MainWin', 'MonthNeeds', False) then begin
    cost := SkillData.Cost;
    if (GameMod = modMagicDeep) and not IsLeader(CurrUnit) then
      cost := cost + 10 * (CurrUnit.Skills.Count-1);
    AddNeedItem(CurrUnit, SilverData, cost * CurrUnit.Items.Amount(IT_MAN),
      Config.ReadInteger('MainWin', 'NeedsPriority', 0));
    DistributeNeeds(CurrUnit.Region);
  end;

  ExecOrder('study "' + SkillData.Name + '"', True);
end;

procedure TOrderHandlers.CastSkill(Sender: TObject);
begin
  ExecOrder('cast "' + SkillData.Name + '"', True);
end;

procedure TOrderHandlers.CombatSkill(Sender: TObject);
begin
  ExecOrder('combat "' + SkillData.Name + '"', True);
end;

procedure TOrderHandlers.ForgetSkill(Sender: TObject);
begin
  if MessageDlg('Forget ' + SkillData.Name + '?', mtConfirmation,
    [mbOk, mbCancel], 0) = mrOk then
    ExecOrder('forget "' + SkillData.Name + '"', FALSE);
end;

procedure TOrderHandlers.ItemGive(Sender: TObject);
var mt: integer;
    s: string;
begin
  // Determine required movement type
  mt := mtNone;
  s := TMenuItem(Sender).Caption;
  if Pos('Walking', s) > 0 then mt := mtWalk
  else if Pos('Riding', s) > 0 then mt := mtRide
  else if Pos('Flying', s) > 0 then mt := mtFly
  else if Pos('Swimming', s) > 0 then mt := mtSwim;
  GiveToMovement := mt;
  // Start drag
  if MainForm.ItemGrid.Focused then
    MainForm.ItemGrid.BeginDrag(False)
  else if MainForm.gAllItems.Focused then
    MainForm.gAllItems.BeginDrag(False);
end;

procedure TOrderHandlers.ItemNeeds(Sender: TObject);
begin
  AddNeedItem(CurrUnit, Item.Data, Item.Amount, 0);
  DistributeNeeds(CurrUnit.Region);
  ExecOrder('', False);
end;

procedure TOrderHandlers.AllItemNeeds(Sender: TObject);
var i: integer;
begin
  for i := 0 to CurrUnit.Items.Count-1 do
    if not Test(CurrUnit.Items[i].Data.Flags, IT_MAN + IT_SILVER) then
      AddNeedItem(CurrUnit, CurrUnit.Items[i].Data, CurrUnit.Items[i].Amount, 0);
  DistributeNeeds(CurrUnit.Region);
  ExecOrder('', False);
end;

procedure TOrderHandlers.ItemSell(Sender: TObject);
begin
  ExecOrder('sell all "' + Item.Name + '"', True);
end;

procedure TOrderHandlers.ItemWanted(Sender: TObject);
begin
  with TWantedItemsForm.Create(MainForm) do begin
    Setup(Item.Data);
    ShowModal;
    Free;
  end;
end;

procedure TOrderHandlers.Repeating(Sender: TObject);
var rep: boolean;
begin
  rep := Config.ReadBool('MainWin', 'RepeatingOrders', False);
  Config.WriteBool('MainWin', 'RepeatingOrders', not rep);
end;

procedure TOrderHandlers.ShowSpyRep(Sender: TObject);
begin
  with TSpyRepForm.Create(MainForm) do begin
    SetUnitFilter;
    ShowModal;
    Free;
  end;
end;

procedure AddToSim(U: TUnit; side: boolean);
var BUnit: TBattleUnit;
    i, tac: integer;
    Skill: TSkill;
begin
  BUnit := TBattleUnit.Create;
  i := 0;
  while (i < Turn.Factions.Count) and (BUnit.URef = nil) do begin
    BUnit.URef := Turn.Factions[i].Units.Find(U.Num);
    Inc(i);
  end;
  TBaseUnit(BUnit).Assign(U);
  BUnit.Behind := U.Flags[flgBehind];
  BUnit.Faction := Turn.Factions.Find(U.Faction.Num);
  // Tactics for monsies
  tac := 0;
  for i := 0 to BUnit.Items.Count-1 do
    if Test(BUnit.Items[i].Data.Flags, IT_MONSTER) then
      tac := Max(tac, BUnit.Items[i].Data.Monster.Tactics);
  if tac > 0 then begin
    Skill := BUnit.Skills.Seek(Keys[s_Tactics]);
    Skill.Level := tac;
  end;
  // Add
  if side = sideAttack then begin
    BUnit.Side := sideAttack;
    SimBattle.Units[sideAttack].Add(BUnit);
  end
  else begin
    BUnit.Side := sideDefence;
    SimBattle.Units[sideDefence].Add(BUnit);
  end;
end;

procedure TOrderHandlers.AddAttacker(Sender: TObject);
begin
  AddToSim(CurrUnit, sideAttack);
end;

procedure TOrderHandlers.AddArmyAttacker(Sender: TObject);
var i: integer;
    Troop: TTroop;
begin
  if CurrUnit.UArmy = nil then AddToSim(CurrUnit, sideAttack)
  else begin
    Troop := CurrUnit.Region.PlayerTroop;
    if Troop = nil then Exit;
    for i := 0 to Troop.Units.Count-1 do
      if Troop.Units[i].UArmy = CurrUnit.UArmy then
        AddToSim(Troop.Units[i], sideAttack);
  end;
end;

procedure TOrderHandlers.AddDefender(Sender: TObject);
begin
  AddToSim(CurrUnit, sideDefence);
end;

procedure TOrderHandlers.ClearSim(Sender: TObject);
begin
  SimBattle.Units[sideAttack].ClearItems;
  SimBattle.Units[sideDefence].ClearItems;
end;

procedure ChangeArmy(U: TUnit; NewArmy: TUArmy);
var RealU: TUnit;
begin
  if (U.UArmy <> nil) and (U.UArmy.UnitIds.IndexOf(U.Id) >= 0) then
    U.UArmy.UnitIds.Delete(U.UArmy.UnitIds.IndexOf(U.Id));
  U.UArmy := NewArmy;
  if (NewArmy <> nil) and (NewArmy.UnitIds.IndexOf(U.Id) = -1) then
    NewArmy.UnitIds.Add(U.Id);
  RealU := Faction.Units.Find(U.Num);
  if RealU <> nil then RealU.UArmy := NewArmy;
end;

procedure TOrderHandlers.SetArmy(Sender: TObject);
var NewArmy: TUArmy;
begin
  NewArmy := Game.UArmies[TMenuItem(Sender).Tag];
  if NewArmy = CurrUnit.UArmy then NewArmy := nil;
  ChangeArmy(CurrUnit, NewArmy);
  MainForm.UnitGrid.Invalidate;
end;

procedure TOrderHandlers.MergeArmy(Sender: TObject);
var i: integer;
    NewArmy: TUArmy;
    Troop: TTroop;
begin
  if CurrUnit.UArmy = nil then Exit;
  NewArmy := Game.UArmies[TMenuItem(Sender).Tag];
  Troop := CurrUnit.Region.PlayerTroop;
  if Troop = nil then Exit;
  for i := 0 to Troop.Units.Count-1 do
    ChangeArmy(Troop.Units[i], NewArmy);
  MainForm.UnitGrid.Invalidate;
end;

procedure TOrderHandlers.ArmyMove(Sender: TObject);
begin
  MoveOrder := 'move';
  MainForm.StartMoveMode(True, False);
end;

procedure TOrderHandlers.ArmyAdvance(Sender: TObject);
begin
  MoveOrder := 'advance';
  MainForm.StartMoveMode(True, False);
end;

procedure TOrderHandlers.ArmyCustom(Sender: TObject);
var Troop: TTroop;
    i: integer;
    s: string;
begin
  if InputQuery('Custom Army order', 'Enter order for army units', s) then begin
    Troop := CurrUnit.Region.PlayerTroop;
    if (Troop = nil) or (CurrUnit.UArmy = nil) then Exit;
    for i := 0 to Troop.Units.Count-1 do
      if Troop.Units[i].UArmy = CurrUnit.UArmy then
        AddOrderTo(Troop.Units[i], s, False);
    ExecOrder('', False);
  end;
end;

procedure TOrderHandlers.ArmyRemOrder(Sender: TObject);
var Troop: TTroop;
    U: TUnit;
    i, j: integer;
    s: string;
begin
  if InputQuery('Remove Army order', 'Enter order to remove', s) then begin
    Troop := CurrUnit.Region.PlayerTroop;
    if (Troop = nil) or (CurrUnit.UArmy = nil) then Exit;
    for i := 0 to Troop.Units.Count-1 do
      if Troop.Units[i].UArmy = CurrUnit.UArmy then begin
        U := Troop.Units[i];
        j := 0;
        while (j < U.Orders.Count) do
          if (U.Order(j) = s) then U.Orders.Delete(j)
          else Inc(j); 
      end;
    ExecOrder('', False);
  end;
end;

procedure TOrderHandlers.DeclareNeeds(Sender: TObject);
begin
  with TUnitNeedsForm.Create(MainForm) do begin
    if ShowModal = mrOk then begin
      DistributeNeeds(CurrRegion);
      ExecOrder('', False);
    end;
    Free;
  end;
end;

procedure TOrderHandlers.DistribNeeds(Sender: TObject);
begin
  DistributeNeeds(CurrRegion);
  ExecOrder('', False);
end;

procedure TOrderHandlers.DeclareRoute(Sender: TObject);
begin
  MoveOrder := 'move';
  MainForm.StartMoveMode(False, True);
end;

procedure TOrderHandlers.DeclareSailRoute(Sender: TObject);
begin
  MoveOrder := 'sail';
  MainForm.StartMoveMode(False, True);
end;

procedure TOrderHandlers.DoRunRoute(Sender: TObject);
begin
  RunRoute(CurrUnit);
  ExecOrder('', False);
end;

procedure TOrderHandlers.MoveTo(Sender: TObject);
var s, path: string;
begin
  s := StringReplace(TMenuItem(Sender).Caption, '&', '', [rfReplaceAll]);
  s := GameConfig.ReadString('Bookmarks', s, '');
  if (s = '') or EqualCoords(BookmarkCoords(s), CurrUnit.Region.Coords) then
    Exit;
  if s <> '' then
    path := PathFind(CurrUnit, BookmarkCoords(s), False);
  if path = '' then begin
    MessageDlg('Can''t find path to move', mtWarning, [mbOk], 0);
    Exit;
  end;
  ClearUnitMoves(CurrUnit);
  ExecOrder('move' + path, True);
end;

procedure TOrderHandlers.Advance(Sender: TObject);
begin
  MoveOrder := 'advance';
  MainForm.StartMoveMode(False, False);
end;

procedure TOrderHandlers.Assassinate(Sender: TObject);
var s: string;
begin
  s := StringReplace(TMenuItem(Sender).Caption, '&', '', []);
  ExecOrder('assassinate ' + Copy(s, Pos('(', s)+1, Pos(')', s) - Pos('(', s) - 1),
    False);
end;

procedure TOrderHandlers.Attack(Sender: TObject);
var s: string;
begin
  s := StringReplace(TMenuItem(Sender).Caption, '&', '', []);
  ExecOrder('attack ' + Copy(s, Pos('(', s)+1, Pos(')', s) - Pos('(', s) - 1),
    False);
end;

procedure TOrderHandlers.AttackUnit(Sender: TObject);
var i, best, best_nr: integer;
    Troop: TTroop;
    U: TUnit;
    Skill: TSkill;
begin
  // Find non-avoiding unit, or take first one
  Troop := CurrUnit.Region.PlayerTroop;
  if Troop = nil then Exit;
  best := -1;
  best_nr := -1;
  for i := 0 to Troop.Units.Count-1 do begin
    U := Troop.Units[i];
    if U.Flags[flgAvoid] then Continue;
    if best_nr = -1 then best_nr := i;
    Skill := U.Skills.Find(Keys[s_RidingSkill]);
    if (Skill <> nil) and (Skill.Level > best) then begin
      best := Skill.Level;
      best_nr := i;
    end;
  end;
  if best_nr = -1 then best_nr := 0;
  // Issue attack order
  AddOrderTo(Troop.Units[best_nr], 'attack ' + CurrUnit.NumStr, False);
  MainForm.ProcessOrders(CurrRegion);
end;

procedure TOrderHandlers.Build(Sender: TObject);
var s: string;
begin
  s := 'build';
  ExecOrder(s, True);
end;

procedure TOrderHandlers.BuildNew(Sender: TObject);
var s: string;
begin
  s := 'build "' + TMenuItem(Sender).Caption + '"';
  ExecOrder(s, True);
end;

procedure TOrderHandlers.BuildHelp(Sender: TObject);
var s: string;
begin
  s := StringReplace(TMenuItem(Sender).Caption, '&', '', []);
  ExecOrder('build help ' + Copy(s, Pos('(', s)+1, Pos(')', s) - Pos('(', s) - 1),
    True);
end;

procedure TOrderHandlers.Buy(Sender: TObject);
var i, extra, amt: integer;
    repsign, amount_str: string;
    Giver: TUnit;
begin
  repsign := RepeatSign;
  // Create new form
  BuyForm := TBuyForm.Create(nil);
  with CurrRegion do begin
    i := 0;
    while (i < ForSale.Count) and (ForSale[i].Data.MultiName <>
      TMenuItem(Sender).Caption) do Inc(i);
    if i < ForSale.Count then
      with BuyForm do begin
        Setup(CurrUnit, ForSale[i]);
        if (ShowModal = mrOk) and (Amount > 0) then begin
          // Extra money
          extra := Max(TotalCost - CurrUnit.Items.Amount(IT_SILVER), 0);
          // Take
          if (extra > 0) and cbTake.Checked and (cmTakeUnit.ItemIndex >= 0) then begin
            Giver := TUnit(cmTakeUnit.Items.Objects[cmTakeUnit.ItemIndex]);
            amt := Min(extra, Giver.Items.Amount(IT_SILVER));
            Giver.Orders.Add('give ' + CurrUnit.NumStr + ' ' +
              IntToStr(amt) + ' ' + SilverData.Name);
            extra := extra - amt;
          end;
          // Claim
          if (extra > 0) and cbClaim.Checked then
            AddOrder(repsign + 'claim ' + IntToStr(extra), False);
          // Buy
          if cbAll.Checked then amount_str := 'all'
          else amount_str := IntToStr(Amount);
          ExecOrder(repsign + 'buy ' + amount_str + ' "' +
            ForSale[i].Data.Name(Amount > 1) + '"', FALSE);
        end;
        Free;
      end;
  end;
end;

procedure TOrderHandlers.Cast(Sender: TObject);
var s: string;
begin
  s := 'cast "' + TMenuItem(Sender).Caption + '"';
  s := s + ' ' + InputBox(s, 'Spell parameters:', '');
  ExecOrder(s, TRUE);
end;

procedure TOrderHandlers.CancelForm(Sender: TObject);
var i, j, k, ch: integer;
    Lines: TStrings;
    s: string;
    quote: boolean;
begin
  CurrUnit.Orders.Clear;
  if CurrUnit.Former <> nil then begin
    // Remove forming orders from former
    Lines := CurrUnit.Former.Orders;
    i := 0;
    while (i < Lines.Count)
      and ((Lines[i] <> ('form ' + IntToStr(-CurrUnit.Num)))
      or InsideConstruction(CurrUnit.Former, i))
      do Inc(i);
    while (i < Lines.Count) and (Lines[i] <> 'end') do
      Lines.Delete(i);
    if i < Lines.Count then Lines.Delete(i); // "end" line
    // Remove orders for this unit from other region units
    i := 0;
    while (i < CurrRegion.Troops.Count-1)
      and (CurrRegion.Troops[i].Faction.Num <> VFaction.Num) do Inc(i);
    for j := 0 to CurrRegion.Troops[i].Units.Count-1 do begin
      Lines := CurrRegion.Troops[i].Units[j].Orders;
      k := 0;
      while k < Lines.Count do begin
        if InsideConstruction(CurrRegion.Troops[i].Units[j], k) then begin
          Inc(k);
          Continue;
        end;
        s := '';
        quote := False;
        // name unit "new unit"; new unit  -->  name unit
        for ch := 1 to Length(Lines[k]) do begin
          if not quote and (Lines[k][ch] = ';') then Break;
          if Lines[k][ch] = '"' then quote := not quote;
          if not quote then s := s + Lines[k][ch];
        end;
        if Pos(' ' + CurrUnit.NumStr, s) > 0 then Lines.Delete(k)
        else Inc(k);
      end;
    end;
    // Do orders
    MainForm.ProcessOrders(CurrRegion);
  end;
end;

procedure TOrderHandlers.Claim(Sender: TObject);
begin
  with TClaimForm.Create(MainForm) do begin
    Silver := VTurn.Unclaimed;
    if ShowModal = mrOk then
      ExecOrder('claim ' + IntToStr(Silver), True);
    Free;
  end;
end;

procedure TOrderHandlers.DescribeUnit(Sender: TObject);
var s: string;
begin
  s := InputBox('Describe unit', 'Enter unit description:', CurrUnit.Description);
  if (s <> CurrUnit.Description) then
    ExecOrder('describe unit "' + s + '"', FALSE);
end;

procedure TOrderHandlers.DescribeObject(Sender: TObject);
var s: string;
begin
  s := InputBox('Describe object', 'Enter object description:',
    CurrUnit.Struct.Description);
  if (s <> CurrUnit.Struct.Description) then
    ExecOrder('describe object "' + s + '"', FALSE);
end;

procedure TOrderHandlers.Enter(Sender: TObject);
var Trace: TTrace;
    s: string;
begin
  Trace := TTrace.Create(TMenuItem(Sender).Caption);
  s := Trace.Before(' [');
  ExecOrder('enter ' + Trace.Before(']') + '; ' + s, FALSE);
  Trace.Free;
end;

procedure TOrderHandlers.Entertain(Sender: TObject);
begin
  ExecOrder('entertain', True);
end;

procedure TOrderHandlers.Evict(Sender: TObject);
var s: string;
begin
  s := StringReplace(TMenuItem(Sender).Caption, '&', '', []);
  ExecOrder('evict ' + Copy(s, Pos('(', s)+1, Pos(')', s) - Pos('(', s) - 1),
    False);
end;

procedure TOrderHandlers.Form(Sender: TObject);
var s: string;
begin
  // Show dialog
  s := '';
  with TNewUnitForm.Create(MainForm) do begin
    if ShowModal <> mrOk then Exit;
    s := GetOrders(False);
    Free;
  end;
  // Do FORM order
  ExecOrder(s, False);
end;

procedure TOrderHandlers.FormTemplate(Sender: TObject);
var i, num, fee: integer;
    s: string;
    Troop: TTroop;
    Trace: TTrace;
begin
  num := 0;
  Troop := CurrRegion.PlayerTroop;
  if Troop <> nil then
    for i := 0 to Troop.Units.Count-1 do
      num := Min(num, Troop.Units[i].Num);
  num := Abs(num - 1);
  // Get template
  s := Config.ReadString('FormTemplates', TMenuItem(Sender).Caption, '');
  s := StringReplace(s, '<br>', #13#10, [rfReplaceAll]);
  s := StringReplace(s, '%N', IntToStr(num), [rfReplaceAll]);
  if CurrRegion.Peasants <> nil then begin
    s := StringReplace(s, '%PEASANTS', CurrRegion.Peasants.Data.Name,
      [rfReplaceAll]);
    Trace := TTrace.Create(s);
    Trace.Before('%RECRUITFEE(');
    if not Trace.Ends then begin
      try
        fee := Trace.Num * CurrRegion.Peasants.Cost;
        s := Copy(s, 1, Pos('%RECRUITFEE', s)-1) + IntToStr(fee) +
          Copy(s, Trace.StPos + 1, Length(s));
      except
      end;
    end;
    Trace.Free;
  end;
  // Do FORM order
  ExecOrder(s, False);
end;

procedure TOrderHandlers.Leave(Sender: TObject);
begin
  ExecOrder('leave', FALSE);
end;

procedure TOrderHandlers.LocalDescription(Sender: TObject);
var ld, s: string;
begin
  ld := UnitRecs.Local(CurrUnit.Num, CurrUnit.Region.Coords);
  s := InputBox('Local description', 'Enter local description:', ld);
  if s <> ld then begin
    if CurrUnit.Num < 0 then AddOrder('@;;' + s, False);
    UnitRecs.AddUnitRec(CurrUnit, s);
    MainForm.FillUnitGrid;
  end;
end;

procedure TOrderHandlers.Move(Sender: TObject);
begin
  MoveOrder := 'move';
  MainForm.StartMoveMode(False, False);
end;

procedure TOrderHandlers.NameFaction(Sender: TObject);
var s: string;
begin
  s := InputBox('Name faction', 'Enter new faction name:', CurrUnit.Faction.Name);
  if (s <> CurrUnit.Faction.Name) then
    ExecOrder('name faction "' + s + '"', FALSE);
end;

procedure TOrderHandlers.NameObject(Sender: TObject);
var s: string;
begin
  with CurrUnit do begin
    s := InputBox('Name object', 'Enter new name for ' + Struct.Data.Group +
      ' [' + IntToStr(Struct.Num) + ']', Struct.Name);
    if (s <> Struct.Name) then
      ExecOrder('name object "' + s + '"', FALSE);
  end;
end;

procedure TOrderHandlers.NameUnit(Sender: TObject);
var s: string;
begin
  s := InputBox('Name unit', 'Enter new unit name:', CurrUnit.Name);
  if (s <> CurrUnit.Name) then
    ExecOrder('name unit "' + s + '"', FALSE);
end;

procedure TOrderHandlers.Produce(Sender: TObject);
var IData: TItemData;
    RealR: TRegion;
    i, maxout, turnout: integer;
    Tool: TItem;
begin
  if Config.ReadBool('MainWin', 'MonthNeeds', False) then begin
    IData := Game.ItemData.FindByName(TMenuItem(Sender).Caption);
    RealR := Map.Region(CurrUnit.Region.Coords, Turn.Num);

    // TODO: looks like needs adjustment with productio limit
    if (IData <> nil) and (ProduceOut(CurrUnit, RealR, IData,
      maxout, turnout, -1, True) = prdOk) then begin

      // Tools
      if IData.Produce.Tool <> nil then begin
        CompleteNeeds.AUnit := CurrUnit; // Set spy record to spot tool grabs
        CompleteNeeds.IData := IData.Produce.Tool;
        AddNeedItem(CurrUnit, IData.Produce.Tool, -2,
          Config.ReadInteger('MainWin', 'NeedsPriority', 0));
        DistributeNeeds(CurrUnit.Region); // orders will not be changed
        CompleteNeeds.AUnit := nil; // Clear spy record

        if CompleteNeeds.Amount > 0 then begin
          // Recalculate output with new amount of tools
          Tool := TItem.Create;
          Tool.Amount := CompleteNeeds.Amount;
          Tool.Data := CompleteNeeds.IData;
          CurrUnit.Items.Add(Tool);

          // TODO: looks like needs adjustment with productio limit
          ProduceOut(CurrUnit, RealR, IData, maxout, turnout, -1, True);
          CurrUnit.Items.Delete(CurrUnit.Items.IndexOf(Tool));
          Tool.Free;
        end;
      end;

      // Produce materials
      for i := 0 to IData.Produce.Materials.Count-1 do
        AddNeedItem(CurrUnit, IData.Produce.Materials[i].Data, maxout *
          IData.Produce.Materials[i].Amount,
          Config.ReadInteger('MainWin', 'NeedsPriority', 0));
      DistributeNeeds(CurrUnit.Region);
    end;
  end;

  ExecOrder('produce "' + TMenuItem(Sender).Caption + '"', TRUE);
end;

procedure TOrderHandlers.Promote(Sender: TObject);
var s: string;
begin
  s := StringReplace(TMenuItem(Sender).Caption, '&', '', []);
  ExecOrder('promote ' + Copy(s, Pos('(', s)+1, Pos(')', s) - Pos('(', s) - 1),
    False);
end;

procedure TOrderHandlers.Pillage(Sender: TObject);
begin
  ExecOrder('pillage', True);
end;

procedure TOrderHandlers.Sail(Sender: TObject);
begin
  if (CurrUnit.Struct <> nil) and (CurrUnit.Struct.Data.Flags
    and ST_TRANSPORT <> 0) and (SkillLevel(CurrUnit, Keys[s_Sailing]) > 0) then begin
    MoveOrder := 'sail';
    MainForm.StartMoveMode(False, False);
  end;
end;

procedure TOrderHandlers.Sell(Sender: TObject);
var i, j, amount: integer;
    s, amount_str: string;
    repsign: string;
begin
  repsign := RepeatSign;
  // Create new form
  SelAmountForm := TSelAmountForm.Create(nil);
  with CurrRegion do begin
    i := 0;
    while (i < Wanted.Count) and not TestItemName(Wanted[i].Data,
      TMenuItem(Sender).Caption) do Inc(i);
    if i < Wanted.Count then
      with SelAmountForm do begin
        Caption := 'Sell ' + TMenuItem(Sender).Caption;
        FundsWantedLabel.Caption := 'Wanted: ' + IntToStr(Wanted[i].Amount);
        j := 0;
        while (j < CurrUnit.Items.Count) and (CurrUnit.Items[j].Data.MultiName <>
          Wanted[i].Data.MultiName) do Inc(j);
        HavingCountLabel.Caption := IntToStr(CurrUnit.Items[j].Amount) + ' ' +
          Wanted[i].Data.MultiName;

        GoodsLabel.Caption := Wanted[i].Data.MultiName;
        GoodsImage.Canvas.Brush.Color := clBtnFace;
        GoodsImage.Canvas.FillRect(GoodsImage.Canvas.ClipRect);
        MakeItemBmp(GoodsImage.Picture.Bitmap, Wanted[i].Data);
        Cost := Wanted[i].Cost;
        if Wanted[i].Amount = -1 then amount := 9999
        else amount := Wanted[i].Amount;
        AmountEdit.Value := Min(amount, CurrUnit.Items[j].Amount);
        CostLabel.Caption := IntToStr(AmountEdit.Value * Cost);

        if (ShowModal = mrOk) and (AmountEdit.Value > 0) then begin
          if (AmountEdit.Value = 1) and (Wanted[i].Data.SingleName <> '') then
            s := Wanted[i].Data.SingleName
          else s := Wanted[i].Data.MultiName;
          if cbAll.Checked then amount_str := 'all'
          else amount_str := IntToStr(AmountEdit.Value);
          ExecOrder(repsign + 'sell ' + amount_str + ' "' + s + '"',
            FALSE);
        end;
        Free;
      end;
  end;
end;

procedure TOrderHandlers.Script(Sender: TObject);
var idx, i, j: integer;
    args: string;
begin
  idx := ScriptIndex(TMenuItem(Sender).Caption);
  i := Pos('(', Scripts[idx]);
  j := i;
  if i > 0 then begin
    args := ParensContents(Scripts[idx], i);
    if not InputQuery('Script', 'Enter script arguments', args) then Exit;
    args := '(' + args + ')';
  end
  else j := Length(Scripts[idx]) + 1;
  CurrUnit.Orders.Add('@;script ' + Copy(Scripts[idx], 1, j-1) + args);
  CurrUnit.Orders.Add('@;end ' + ScriptName(Scripts[idx]));
  MainForm.StartProcess(CurrRegion, -1, '', True);
end;

procedure TOrderHandlers.Study(Sender: TObject);
var s: string;
    SData: TSkillData;
begin
  if Config.ReadBool('MainWin', 'MonthNeeds', False) then begin
    SData := Game.SkillData.FindByName(TMenuItem(Sender).Caption);
    if SData <> nil then begin
      AddNeedItem(CurrUnit, SilverData, SData.Cost * CurrUnit.Items.Amount(IT_MAN),
        Config.ReadInteger('MainWin', 'NeedsPriority', 0));
      DistributeNeeds(CurrUnit.Region);
    end;
  end;

  // Add order
  s := 'study "' + TMenuItem(Sender).Caption + '"';
  ExecOrder(s, TRUE);
end;

procedure TOrderHandlers.Tax(Sender: TObject);
begin
  ExecOrder('tax', True);
end;

procedure TOrderHandlers.Teach(Sender: TObject);
var s, repsign: string;
    i: integer;
begin
  repsign := RepeatSign;
  // Show dialog
  s := '';
  with TTeachForm.Create(MainForm) do begin
    if ShowModal = mrOk then begin
      s := GetOrders;
      if s <> '' then ExecOrder(repsign + s, False)
      else begin
        i := 0;
        while (i < CurrUnit.Orders.Count) do
          if Pos('teach', CurrUnit.Orders[i]) in [1..2] then
            CurrUnit.Orders.Delete(i)
          else Inc(i);
        ExecOrder('', False);
      end;
    end;
    Free;
  end;
end;

procedure TOrderHandlers.Work(Sender: TObject);
begin
  ExecOrder('work', True);
end;

 { Popup customization }

function AddMenuItem(AParent: TMenuItem; ACaption: string; AImage: integer;
  Handler: TNotifyEvent): TMenuItem;
begin
  Result := TMenuItem.Create(MainForm);
  Result.Caption := ACaption;
  if AImage <> -1 then Result.ImageIndex := AImage;
  Result.OnClick := Handler;
  AParent.Add(Result);
end;

procedure SortItems(AMenuItem: TMenuItem);
var i, j: integer;
    T: TMenuItem;
begin
  for i := 0 to AMenuItem.Count-1 do
    for j := i+1 to AMenuItem.Count-1 do
      if AMenuItem.Items[i].Caption > AMenuItem.Items[j].Caption then begin
        T := AMenuItem.Items[j];
        AMenuItem.Delete(j);
        AMenuItem.Insert(i, T);
      end;
  for i := 0 to AMenuItem.Count-1 do
    for j := i+1 to AMenuItem.Count-1 do begin
      if (AMenuItem.Items[j].ImageIndex = bmpFoundation)
        and (AMenuItem.Items[i].ImageIndex = bmpSpell) then begin
        T := AMenuItem.Items[j];
        AMenuItem.Delete(j);
        AMenuItem.Insert(i, T);
      end;
    end;
end;

procedure MakeArmyBmp(Bitmap: TBitmap; Color: TColor);
var Bmp: TBitmap;
begin
  if Color = -1 then Color := FactionColor(Faction);
  Bmp := GetMaskedBmp(ResForm.IconList, bmpForce div 2, Color);
  Bitmap.Assign(Bmp);
  Bmp.Free;
end;

procedure AddFormTemplates(Item: TMenuItem);
var Lines: TStrings;
    i: integer;
begin
  Lines := TStringList.Create;
  Config.ReadSection('FormTemplates', Lines);
  for i := 0 to Lines.Count-1 do
    AddMenuItem(Item, Lines[i], bmpFormTemplate, Handlers.FormTemplate);
  Lines.Free;
end;

procedure MakeEnterMenu(U: TUnit; Item: TMenuItem);
var i: integer;
begin
  Item.Clear;
  for i := 0 to U.Region.Structs.Count-1 do
    if U.Region.Structs[i] <> U.Struct then
      with AddMenuItem(Item,
        U.Region.Structs[i].Name + ' [' + IntToStr(U.Region.Structs[i].Num) + '] : ' +
        U.Region.Structs[i].Data.Group, -1, Handlers.Enter) do begin
        if Test(U.Region.Structs[i].Data.Flags, ST_CLOSED) then Tag := 1;
        MakeStructBmp(Bitmap, U.Region.Structs[i].Data, U.Region.Structs[i].HasExit);
      end;
  for i := 0 to Item.Count-1 do
    if Item.Items[i].Tag = 1 then Item.Items[i].Enabled := False;
end;

procedure MakeStudyMenu(U: TUnit; Item: TMenuItem);
var i, maxlv: integer;
    itmNonSpec, itmSpells: TMenuItem;
    leader, multi, canstudy: boolean;
    Skill: TSkill;
    SData: TSkillData;
begin
  Item.Clear;
  itmNonSpec := TMenuItem.Create(MainForm);
  itmNonSpec.Caption := 'Non-specialized';
  itmSpells := TMenuItem.Create(MainForm);
  itmSpells.Caption := 'Magic';

  // Leaders can study many skills; MagicDeep peasants - 3 skills.
  leader := IsLeader(U);
  multi := leader or ((GameConfig.ReadInteger('Settings', 'Mod',
    modStandard) = modMagicDeep) and (U.Skills.Count < 3));

  for i := 0 to Game.SkillData.Count-1 do begin
    SData := Game.SkillData[i];
    maxlv := MaxSkillLevel(U, SData);
    Skill := U.Skills.Find(SData.Short);
    canstudy := (U.CanStudy.Find(SData.Short) <> nil)
      or ((Skill <> nil) and (Skill.Level < maxlv));

    if multi or (U.Skills.Count = 0) or canstudy then begin
      if (Skill <> nil) and (Skill.Level >= maxlv) then Continue;
      if (SData.BasedOn.Count > 0) and not canstudy then Continue;

      if Test(SData.Flags, SK_MAGIC + SK_FOUNDATION) then begin
        if not leader then Continue;
        AddMenuItem(itmSpells, SData.MakeName, SkillIcon(SData), Handlers.Study)
      end
      else if (maxlv <= 2) and (Skill = nil) then
        AddMenuItem(itmNonSpec, SData.MakeName, SkillIcon(SData), Handlers.Study)
      else
        AddMenuItem(Item, SData.MakeName, SkillIcon(SData), Handlers.Study);
    end;
  end;

  // Sort item
  SortItems(Item);
  // Add subitems
  SortItems(itmNonSpec);
  if itmNonSpec.Count = 0 then itmNonSpec.Free
  else Item.Insert(0, itmNonSpec);
  SortItems(itmSpells);
  if itmSpells.Count = 0 then itmSpells.Free
  else Item.Insert(0, itmSpells);
end;

procedure MakeProduceMenu(U: TUnit; Item: TMenuItem);
var i, lv, amt: integer;
    can: integer;
    RealR: TRegion;
begin
  Item.Clear;
  RealR := Map.Region(U.Region.Coords, Turn.Num);
  for i := 0 to Game.ItemData.Count-1 do begin
    can := CanProduce(U, RealR, Game.ItemData[i], lv, amt);
    if can in [prdOk, prdNoMaterial, prdNoStruct] then
      with AddMenuItem(Item, Game.ItemData[i].Name, -1,
        Handlers.Produce) do begin
        if (can <> prdOk) and not ((can = prdNoMaterial) and
          Config.ReadBool('MainWin', 'MonthNeeds', False)) then Tag := 1;
        MakeItemBmp(Bitmap, Game.ItemData[i]);
      end;
  end;
  for i := 0 to Item.Count-1 do
    if Item.Items[i].Tag = 1 then Item.Items[i].Enabled := False;
end;

procedure MakeBuildMenu(AUnit: TUnit; Item: TMenuItem);
var i, j: integer;
    Child: TMenuItem;
    U: TUnit;
begin
  Item.Clear;
  if (AUnit.Struct <> nil) and (AUnit.Struct.Needs > 0)
    and (BuildSkillLv(AUnit, AUnit.Struct.Data) > 0) then begin
    with AddMenuItem(Item, 'Continue ' + AUnit.Struct.Data.Group,
      -1, Handlers.Build) do begin
      MakeStructBmp(Bitmap, AUnit.Struct.Data, AUnit.Struct.HasExit);
      if (BuildMaterials(AUnit, AUnit.Struct.Data) = 0) then
        Item.Items[Item.Count-1].Enabled := False;
    end;
  end;

  if Item.Count > 0 then AddMenuItem(Item, '-', -1, nil);

  for j := 0 to Game.StructData.Count-1 do begin
    if (BuildSkillLv(AUnit, Game.StructData[j]) > 0) then begin
      AddMenuItem(Item, Game.StructData[j].Group, -1, Handlers.BuildNew);
      MakeStructBmp(Item.Items[Item.Count-1].Bitmap, Game.StructData[j], False);
      // If needs rawmaterial, look for it
      if (BuildMaterials(AUnit, Game.StructData[j]) = 0) then
        Item.Items[Item.Count-1].Enabled := False;
    end;
  end;

  Child := TMenuItem.Create(MainForm);
  Child.Caption := 'Help';
  for i := 0 to AUnit.Region.PlayerTroop.Units.Count-1 do begin
    U := AUnit.Region.PlayerTroop.Units[i];
    if (U <> AUnit) and (ClearOrder(U.MonthOrder) = 'build') then
      with AddMenuItem(Child, U.Name + ' (' + U.NumStr + ')', bmpMen,
        Handlers.BuildHelp) do
        Tag := U.Num;
  end;
  if Child.Count = 0 then Child.Free
  else if (Item.Count > 1) and (Item.Items[1].Caption = '-') then
    Item.Insert(1, Child)
  else begin
    Item.Insert(0, Child);
    Item.Insert(1, TMenuItem.Create(MainForm));
    Item.Items[1].Caption := '-';
  end;
end;

procedure MakeCastMenu(U: TUnit; Item: TMenuItem);
var i: integer;
begin
  Item.Clear;
  for i := 0 to U.Skills.Count-1 do
    if Test(U.Skills[i].Data.Flags, SK_CAST) then
      AddMenuItem(Item, U.Skills[i].Data.Name, SkillIcon(U.Skills[i].Data),
        Handlers.Cast);
end;

procedure MakeBuyMenu(U: TUnit; Item: TMenuItem);
var i: integer;
    RealR: TRegion;
begin
  Item.Clear;
  RealR := Map.Region(U.Region.Coords, Turn.Num);
  with RealR do begin
    for i := 0 to ForSale.Count-1 do begin
      // Good/Evil
      if (GameConfig.ReadInteger('Game', 'Alignment', alNeutral) = alGood)
        and Test(ForSale[i].Data.Flags, IT_EVIL) then Continue;
      if (GameConfig.ReadInteger('Game', 'Alignment', alNeutral) = alEvil)
        and Test(ForSale[i].Data.Flags, IT_GOOD) then Continue;

      AddMenuItem(Item, ForSale[i].Data.MultiName, -1, Handlers.Buy);
      MakeItemBmp(Item.Items[Item.Count-1].Bitmap, ForSale[i].Data);
    end;
  end;
end;

procedure MakeSellMenu(U: TUnit; Item: TMenuItem);
var i, j: integer;
begin
  Item.Clear;
  for i := 0 to U.Items.Count-1 do begin
    j := 0;
    while (j < U.Region.Wanted.Count) and (U.Region.Wanted[j].Data.Short <>
      U.Items[i].Data.Short) do Inc(j);
    if j < U.Region.Wanted.Count then
      with AddMenuItem(Item, '', -1, Handlers.Sell) do begin
        if U.Items[i].Amount = 1 then
          Caption := U.Items[i].Data.SingleName
        else Caption := U.Items[i].Data.MultiName;
        MakeItemBmp(Item.Items[Item.Count-1].Bitmap, U.Items[i].Data);
      end;
  end;
end;

procedure MakeAttackMenu(AUnit: TUnit; Item: TMenuItem; Handler: TNotifyEvent);
var s: string;
    Child: TMenuItem;
    i, j: integer;
begin
  Item.Clear;
  with AUnit.Region do begin
    for i := 0 to Troops.Count-1 do
      if not Troops[i].Faction.Player and (Troops[i].Faction.Attitude < attFriendly) then begin
        s := Troops[i].Faction.Name;
        if s = '' then s := 'Unknown';
        Child := AddMenuItem(Item, s, -1, nil);
        for j := 0 to Troops[i].Units.Count-1 do
          if Troops[i].Units[j].Faction = Troops[i].Faction then
          AddMenuItem(Child, Troops[i].Units[j].Name + ' (' +
            Troops[i].Units[j].NumStr + ')', bmpMen, Handler);
        SortItems(Child);
      end;
  end;
  SortItems(Item);
end;

procedure MakeStructUnitsMenu(AUnit: TUnit; Item: TMenuItem; Handler: TNotifyEvent);
var i: integer;
    Troop: TTroop;
begin
  Item.Clear;
  Troop := AUnit.Region.PlayerTroop;
  if Troop = nil then Exit;

  for i := 0 to Troop.Units.Count-1 do
    if (Troop.Units[i].Struct = AUnit.Struct) and (Troop.Units[i] <> AUnit) then
      AddMenuItem(Item, Troop.Units[i].FullName, bmpMen, Handler);
  SortItems(Item);
end;

procedure CustomizeUnitPopup(AUnit: TUnit);
var i: integer;
    Popup: TPopupMenu;
    RealR: TRegion;
    Item, Child, Specials, SailRoute: TMenuItem;
begin
  Popup := MainForm.PopMenu;
  Popup.Images := ResForm.IconList;
  Popup.Items.Clear;

  RealR := Map.Region(AUnit.Region.Coords, Turn.Num);

  with AUnit do begin

    // Other factions' units
    if not Faction.Player then begin
      AddMenuItem(Popup.Items, 'Spy Report', -1, Handlers.ShowSpyRep);
      AddMenuItem(Popup.Items, '-', -1, nil);
      // Simulator
      AddMenuItem(Popup.Items, 'Add as Attacker', -1, Handlers.AddAttacker);
      AddMenuItem(Popup.Items, 'Add as Defender', -1, Handlers.AddDefender);
      AddMenuItem(Popup.Items, 'Clear Simulation', -1, Handlers.ClearSim);
      AddMenuItem(Popup.Items, '-', -1, nil);
      AddMenuItem(Popup.Items, 'Local Description', bmpMen, Handlers.LocalDescription);
      if (Region.PlayerTroop <> nil) and (Faction.Attitude <= attNeutral) then
        AddMenuItem(Popup.Items, 'Attack this unit', bmpWeapon, Handlers.AttackUnit);
    end

    // Player units
    else begin
      // Specials
      Specials := AddMenuItem(Popup.Items, 'Specials', -1, nil);
      AddMenuItem(Specials, 'Declare Needs', -1, Handlers.DeclareNeeds);
      AddMenuItem(Specials, 'Distribute Needs', -1, Handlers.DistribNeeds);
      AddMenuItem(Specials, '-', -1, nil);
      AddMenuItem(Specials, 'Declare MOVE Route', bmpRoute, Handlers.DeclareRoute);
      SailRoute := AddMenuItem(Specials, 'Declare SAIL Route', bmpRoute,
        Handlers.DeclareSailRoute);
      AddMenuItem(Specials, 'Run Route', bmpRoute, Handlers.DoRunRoute);
      AddMenuItem(Specials, '-', -1, nil);

      {
      // Move to
      Item := TMenuItem.Create(MainForm);
      Item.Caption := 'Move to';
      Item.ImageIndex := bmpMounts;
      Lines := TStringList.Create;
      GameConfig.ReadSection('Bookmarks', Lines);
      for i := 0 to Lines.Count-1 do
        AddMenuItem(Item, Lines[i], -1, Handlers.MoveTo);
      Lines.Free;
      if Item.Count = 0 then Item.Free
      else Popup.Items.Add(Item);
      }

      // Armies
      Item := TMenuItem.Create(MainForm);
      Item.Caption := 'Army';
      for i := 0 to Game.UArmies.Count-1 do begin
        with AddMenuItem(Item, Game.UArmies[i].Name, -1, Handlers.SetArmy) do begin
          MakeArmyBmp(Bitmap, Game.UArmies[i].Color);
          Tag := i;
        end;
      end;
      if UArmy <> nil then begin
        Item.Items[Game.UArmies.IndexOf(UArmy)].Checked := True;
        // Army options
        AddMenuItem(Item, '-', -1, nil);
        AddMenuItem(Item, 'Move Army', bmpMounts, Handlers.ArmyMove);
        AddMenuItem(Item, 'Advance Army', bmpAdvance, Handlers.ArmyAdvance);
        AddMenuItem(Item, 'Custom Army order', -1, Handlers.ArmyCustom);
        AddMenuItem(Item, 'Remove Army order', -1, Handlers.ArmyRemOrder);
        Child := TMenuItem.Create(MainForm);
        Child.Caption := 'Merge division with';
        for i := 0 to Game.UArmies.Count-1 do
          if Game.UArmies[i] <> UArmy then begin
            with AddMenuItem(Child, Game.UArmies[i].Name, -1, Handlers.MergeArmy) do begin
              MakeArmyBmp(Bitmap, Game.UArmies[i].Color);
              Tag := i;
            end;
          end;
        if Child.Count > 0 then Item.Add(Child)
        else Child.Free;
      end;
      if Item.Count = 0 then Item.Free
      else Popup.Items.Add(Item);

      if ProgOpened then begin
        // Script
        Item := TMenuItem.Create(MainForm);
        Item.Caption := 'Script';
        Item.ImageIndex := bmpScript;
        for i := 0 to Scripts.Count-1 do begin
          if (Pos('[manual]', Scripts[i]) = 0)
            or (Pos('[manual]', Scripts[i]) < Pos('(', Scripts[i])) then
            AddMenuItem(Item, ScriptName(Scripts[i]), -1, Handlers.Script);
        end;
        if Item.Count = 0 then Item.Free
        else Popup.Items.Add(Item);
        AddMenuItem(Specials, '-', -1, nil);

        // Simulator
        AddMenuItem(Specials, 'Add as Attacker', -1, Handlers.AddAttacker);
        if UArmy <> nil then
          AddMenuItem(Specials, 'Add Army as Attacker', -1, Handlers.AddArmyAttacker);
        AddMenuItem(Specials, 'Add as Defender', -1, Handlers.AddDefender);
        AddMenuItem(Specials, 'Clear Simulation', -1, Handlers.ClearSim);
      end;

      AddMenuItem(Popup.Items, '-', -1, nil);

      // Cancel Form
      if Num < 0 then
        AddMenuItem(Popup.Items, 'Cancel Form', bmpNo, Handlers.CancelForm);

      // Name
      Item := AddMenuItem(Popup.Items, 'Name', -1, nil);
      if (Struct <> nil)
        and (Struct.Owner = AUnit) then begin
          Child := TMenuItem.Create(MainForm);
          Child.ImageIndex := -1;
          MakeStructBmp(Child.Bitmap, Struct.Data, Struct.HasExit);
          Child.Caption := Struct.Data.Group;
          Child.OnClick := Handlers.NameObject;
          Item.Add(Child);
      end;
      AddMenuItem(Item, 'Unit', bmpMen, Handlers.NameUnit);
      AddMenuItem(Item, 'Faction', bmpFaction, Handlers.NameFaction);

      // Describe
      Item := AddMenuItem(Popup.Items, 'Describe', -1, nil);
      AddMenuItem(Item, 'Local Description', bmpMen, Handlers.LocalDescription);
      if (Struct <> nil)
        and (Struct.Owner = AUnit) then begin
          Child := TMenuItem.Create(MainForm);
          Child.ImageIndex := -1;
          MakeStructBmp(Child.Bitmap, Struct.Data, Struct.HasExit);
          Child.Caption := Struct.Data.Group;
          Child.OnClick := Handlers.DescribeObject;
          Item.Add(Child);
      end;
      AddMenuItem(Item, 'Unit', bmpMen, Handlers.DescribeUnit);

      // Form
      if Num > 0 then begin
        AddMenuItem(Popup.Items, 'Form New', bmpMen, Handlers.Form);
        Item := TMenuItem.Create(MainForm);
        Item.Caption := 'Form';
        AddFormTemplates(Item);
        if Item.Count = 0 then Item.Free
        else Popup.Items.Add(Item);
      end;

      // Produce
      Item := TMenuItem.Create(MainForm);
      Item.Caption := 'Produce';
      Item.ImageIndex := bmpTools;
      MakeProduceMenu(AUnit, Item);
      if Item.Count = 0 then Item.Free
      else Popup.Items.Add(Item);

      // Build
      Item := AddMenuItem(Popup.Items, 'Build', bmpStructs, nil);
      MakeBuildMenu(AUnit, Item);
      if Item.Count = 0 then begin
        Popup.Items.Delete(Popup.Items.IndexOf(Item));
        Item.Free;
      end;

      // Study
      Item := TMenuItem.Create(MainForm);
      Item.Caption := 'Study';
      Item.ImageIndex := bmpGlasses;
      MakeStudyMenu(AUnit, Item);
      if Item.Count = 0 then Item.Free
      else Popup.Items.Add(Item);

      // Teach
      if (Skills.Count > 0) and IsLeader(AUnit.Items) and ((AUnit.Region.Troops.Count > 1)
        or (AUnit.Region.Troops[0].Units.Count > 1)) then
        AddMenuItem(Popup.Items, 'Teach', bmpGlasses, Handlers.Teach);

      // Cast
      Item := TMenuItem.Create(MainForm);
      Item.Caption := 'Cast';
      Item.ImageIndex := bmpCombatSpell;
      MakeCastMenu(AUnit, Item);
      if Item.Count = 0 then Item.Free
      else Popup.Items.Add(Item);

      // Buy
      Item := TMenuItem.Create(MainForm);
      Item.Caption := 'Buy';
      Item.ImageIndex := bmpSilver;
      MakeBuyMenu(AUnit, Item);
      if Item.Count = 0 then Item.Free
      else Popup.Items.Add(Item);

      // Sell
      Item := TMenuItem.Create(MainForm);
      Item.Caption := 'Sell';
      Item.ImageIndex := bmpSilver;
      MakeSellMenu(AUnit, Item);
      if Item.Count = 0 then Item.Free
      else Popup.Items.Add(Item);

      if Turn.Unclaimed > 0 then
        AddMenuItem(Popup.Items, 'Claim', bmpSilver, Handlers.Claim);

      // Attack
      Item := TMenuItem.Create(MainForm);
      Item.Caption := 'Attack';
      Item.ImageIndex := bmpWeapon;
      MakeAttackMenu(AUnit, Item, Handlers.Attack);
      if Item.Count = 0 then Item.Free
      else Popup.Items.Add(Item);

      // Assassinate
      if (AUnit.Items.Amount(IT_MAN) = 1) and (SkillLevel(AUnit,
        Keys[s_Stealth]) > 0) then begin
        Item := TMenuItem.Create(MainForm);
        Item.Caption := 'Assassinate';
        Item.ImageIndex := bmpWeapon;
        MakeAttackMenu(AUnit, Item, Handlers.Assassinate);
        if Item.Count = 0 then Item.Free
        else Popup.Items.Add(Item);
      end;

      if (Taxers(AUnit) > 0) and (RealR.TaxRate > 0) then begin
        // Tax
        if (AUnit.Region.Guard = nil) or (AUnit.Region.Guard.Num = VFaction.Num)
          or (AUnit.Region.Guard.Attitude >= attFriendly) then
          AddMenuItem(Popup.Items, 'Tax', bmpTax, Handlers.Tax);
        // Pillage
        if ((AUnit.Region.Guard = nil) or (AUnit.Region.Guard.Num = VFaction.Num))
          and (Taxers(AUnit) * 50 >= RealR.TaxRate div 2) then
          AddMenuItem(Popup.Items, 'Pillage', bmpTax, Handlers.Pillage);
      end;

      // Entertain
      if RealR.Entertainment > 0 then begin
        i := 0;
        while (i < Skills.Count) and (Skills[i].Data.Short <>
          Keys[s_EntertainSkill]) do Inc(i);
        if i < Skills.Count then
          AddMenuItem(Popup.Items, 'Entertain', bmpSilver, Handlers.Entertain);
      end;

      if RealR.Wages > 0 then
        AddMenuItem(Popup.Items, 'Work', -1, Handlers.Work);

      if (Struct <> nil) and (Struct.Owner = AUnit) and
        not Test(AUnit.Marks, UM_PROMOTED) then begin

        // Promote
        Item := TMenuItem.Create(MainForm);
        Item.Caption := 'Promote';
        MakeStructUnitsMenu(AUnit, Item, Handlers.Promote);
        if Item.Count = 0 then Item.Free
        else Popup.Items.Add(Item);

        // Evict
        Item := TMenuItem.Create(MainForm);
        Item.Caption := 'Evict';
        MakeStructUnitsMenu(AUnit, Item, Handlers.Evict);
        if Item.Count = 0 then Item.Free
        else Popup.Items.Add(Item);
      end;

      AddMenuItem(Popup.Items, '-', -1, nil);

      // Leave
      if (Struct <> nil) and ((Region.Terrain.Flags and TER_WATER = 0)
        or (CanSwim(AUnit) and not AUnit.Flags[flgNocross])) then
        AddMenuItem(Popup.Items, 'Leave', -1, Handlers.Leave);

      // Enter
      Item := TMenuItem.Create(MainForm);
      Item.Caption := 'Enter';
      MakeEnterMenu(AUnit, Item);
      if Item.Count = 0 then Item.Free
      else Popup.Items.Add(Item);

      // Move
      if (Struct <> nil) and (Struct.Data.Flags and ST_TRANSPORT <> 0) then begin
        if SkillLevel(AUnit, Keys[s_Sailing]) > 0 then
          AddMenuItem(Popup.Items, 'Sail', bmpStructs + 2, Handlers.Sail);
        // simple handling - only sailing skill, no levels
      end
      else if SailRoute <> nil then SailRoute.Enabled := False;
      AddMenuItem(Popup.Items, 'Move', bmpMounts, Handlers.Move);
      AddMenuItem(Popup.Items, 'Advance', bmpAdvance, Handlers.Advance);
      if (MovementType(AUnit) = mtNone) and
        not (CanSwim(AUnit) and Test(AUnit.Region.Terrain.Flags, TER_WATER)) then begin
        Popup.Items[Popup.Items.Count-2].Enabled := False;
        Popup.Items[Popup.Items.Count-1].Enabled := False;
      end;
    end;
  end;
end;

procedure CustomizeSkillPopup(SkillData: TSkillData; Level: integer);
var Popup: TPopupMenu;
begin
  Handlers.SkillData := SkillData;
  Popup := MainForm.PopMenu;
  Popup.Items.Clear;
  Popup.Images := ResForm.IconList;
  with AddMenuItem(Popup.Items, 'Open Editor', -1, MainForm.SkillGridDblClick) do
    Default := True;
  if CurrUnit.Faction.Player then begin
    if MaxSkillLevel(CurrUnit, SkillData) > Level then
      AddMenuItem(Popup.Items, 'Study', bmpGlasses, Handlers.StudySkill);
    if (CurrUnit.Skills.Find(SkillData.Short) <> nil) then begin
      if Test(SkillData.Flags, SK_CAST) then
        AddMenuItem(Popup.Items, 'Cast', bmpCombatSpell, Handlers.CastSkill);
      if Test(SkillData.Flags, SK_COMBATSPELL) then
        AddMenuItem(Popup.Items, 'Combat Spell', bmpCombatSpell, Handlers.CombatSkill);
    end;
    AddMenuItem(Popup.Items, 'Forget', -1, Handlers.ForgetSkill);
  end;
end;

procedure CustomizeItemPopup(Item: TItem);
var Popup: TPopupMenu;
    Req: TItem;
begin
  Handlers.Item := Item;
  Popup := MainForm.PopMenu;
  Popup.Items.Clear;
  Popup.Images := ResForm.IconList;
  with AddMenuItem(Popup.Items, 'Open Editor', -1, MainForm.ItemGridDblClick) do
    Default := True;
  if CurrUnit.Faction.Player then begin
    if not Item.Bought then begin
      AddMenuItem(Popup.Items, '-', -1, nil);
      AddMenuItem(Popup.Items, 'Give', -1, Handlers.ItemGive);
      AddMenuItem(Popup.Items, 'Give to Walking', -1, Handlers.ItemGive);
      AddMenuItem(Popup.Items, 'Give to Riding', -1, Handlers.ItemGive);
      AddMenuItem(Popup.Items, 'Give to Flying', -1, Handlers.ItemGive);
      AddMenuItem(Popup.Items, 'Give to Swimming', -1, Handlers.ItemGive);
    end;
    if not Test(Item.Data.Flags, IT_MAN + IT_MONSTER) then begin
      AddMenuItem(Popup.Items, '-', -1, nil);
      AddMenuItem(Popup.Items, 'Add to Needs', -1, Handlers.ItemNeeds);
      AddMenuItem(Popup.Items, 'Add all to Needs', -1, Handlers.AllItemNeeds);
      Req := CurrUnit.Region.Wanted.Find(Item.Data.Short);
      if (Req <> nil) and (Req.Amount > 0) then
        AddMenuItem(Popup.Items, 'Sell', bmpSilver, Handlers.ItemSell);
      AddmenuItem(Popup.Items, 'Wanted Items', -1, Handlers.ItemWanted);  
    end;
  end;
end;




initialization
  Handlers := TOrderHandlers.Create;

finalization
  Handlers.Free;

end.
