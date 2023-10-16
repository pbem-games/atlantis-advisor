unit uNeeds;

interface

uses
  SysUtils, Windows, Classes, Math, DataStructs, Resources, MyStrings,
  uGameSubs, uKeys;

type
  TNeedItem = class
    AUnit: TUnit;
    Mask: DWord;
    Data: TItemData;
    Amount: integer;
    EnoughAmt, Equipment, NoGive: boolean;
    MaskText: string;
    Unneeded, Usable: boolean;
    Priority: integer;
    function EqualTo(Rec: TNeedItem): boolean;
  end;

  TNeedsList = class(TList)
  protected
    function Get(Index: Integer): TNeedItem;
    procedure Put(Index: Integer; Item: TNeedItem);
  public
    property Items[Index: Integer]: TNeedItem read Get write Put; default;
    procedure ClearAndFree;
    function Seek(Data: TItemData; MaskText: string): TNeedItem;
  end;

var
  CompleteNeeds: record
    AUnit: TUnit;
    IData: TItemData;
    Amount: integer;
  end;

  function DistributeNeeds(R: TRegion): boolean;
  procedure ReadNeeds(AUnit: TUnit; Needs: TNeedsList);
  procedure SetNeeds(U: TUnit; Needs: TNeedsList);
  procedure AddNeedItem(U: TUnit; IData: TItemData; Amount, Priority: integer);

implementation

uses
  uVisualOrders;

var
  Modified: boolean;

function MaskMatch(Rec: TNeedItem; IData: TItemData): boolean;
var flg: DWord;
begin
  flg := IData.Flags;
  if flg = 0 then flg := IT_UNKNOWN;
  Result := Test(flg, Rec.Mask)
    or ((Rec.MaskText = 'spoils') and IsSpoils(IData))
    or ((Rec.MaskText = 'materials') and IsMaterial(IData));
end;

{ TNeedItem }

function TNeedItem.EqualTo(Rec: TNeedItem): boolean;
begin
  Result := False;
  if Data <> nil then begin
    if Data = Rec.Data then Result := True
    else if MaskMatch(Rec, Data) then Result := True;
  end
  else begin
    if Rec.Data <> nil then begin
      if MaskMatch(Self, Rec.Data) then Result := True
    end
    else if (Mask and Rec.Mask <> 0) then Result := True
    else if MaskText = Rec.MaskText then Result := True;
  end;
end;

{ TNeedsList }

procedure TNeedsList.ClearAndFree;
var i: integer;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Free;
end;

function TNeedsList.Seek(Data: TItemData; MaskText: string): TNeedItem;
var i: integer;
begin
  i := Count-1;
  while (i >= 0) and ((Items[i].Data <> Data) or ((Data = nil) and
    (Pos(Items[i].MaskText, MaskText) <> 1) and
    (Pos(MaskText, Items[i].MaskText) <> 1))) do Dec(i);
  if i < 0 then begin
    Add(TNeedItem.Create);
    Items[Count-1].Data := Data;
    Items[Count-1].MaskText := MaskText;
    i := Count-1;
  end;
  Result := Items[i];
end;

function TNeedsList.Get(Index: Integer): TNeedItem;
begin
  Result := TNeedItem(inherited Get(Index));
end;

procedure TNeedsList.Put(Index: Integer; Item: TNeedItem);
begin
  inherited Put(Index, Item);
end;


{ Unit procedures }

procedure ReadNeeds(AUnit: TUnit; Needs: TNeedsList);
var i, j, start, pr, men: integer;
    Trace: TTrace;
    Rec: TNeedItem;
begin
  men := AUnit.Items.Amount(IT_MAN);
  // Get needs from orders, if any
  for i := 0 to AUnit.Orders.Count-1 do begin
    if InsideConstruction(AUnit, i) then Continue;
    if AUnit.Order(i) = '@;needs' then begin
      try
        start := Needs.Count;
        Trace := TTrace.Create(AUnit.Orders[i]);
        Trace.Before('@;needs');
        Trace.SkipSpaces;
        while not Trace.Ends do begin
          // Priority
          if Pos('priority', Trace.Text) = 1 then begin
            // priority 2
            Trace.Before('priority');
            Trace.SkipSpaces;
            pr := Trace.Num;
            for j := start to Needs.Count-1 do Needs[j].Priority := pr;
            Trace.Block;
            Continue;
          end;
          // Equipment
          if Pos('equipment', Trace.Text) = 1 then begin
            // enough usable weapons
            Rec := TNeedItem.Create;
            Rec.Equipment := True;
            Rec.AUnit := AUnit;
            Rec.Amount := men;
            Rec.EnoughAmt := True;
            Rec.Mask := IT_WEAPON;
            Rec.MaskText := 'weapons';
            Rec.Usable := True;
            Needs.Add(Rec);
            // enough armor
            Rec := TNeedItem.Create;
            Rec.Equipment := True;
            Rec.AUnit := AUnit;
            Rec.Amount := men;
            Rec.EnoughAmt := True;
            Rec.Mask := IT_ARMOR;
            Rec.MaskText := 'armor';
            Needs.Add(Rec);
            // enough mounts
            Rec := TNeedItem.Create;
            Rec.Equipment := True;
            Rec.AUnit := AUnit;
            Rec.Amount := men;
            Rec.EnoughAmt := True;
            Rec.Mask := IT_MOUNT;
            Rec.MaskText := 'mounts';
            Needs.Add(Rec);
            Trace.Block;
            Continue;
          end;

          // NoGive
          if Pos('nogive', Trace.Text) = 1 then begin
            Rec := TNeedItem.Create;
            Rec.AUnit := AUnit;
            Rec.NoGive := True;
            Needs.Add(Rec);
            Trace.Block;
            Continue;
          end;

          Rec := TNeedItem.Create;
          Rec.AUnit := AUnit;
          // Item
          if Pos('all', Trace.Text) = 1 then begin
            Rec.Amount := -1;
            Trace.Before('all');
          end
          else if Pos('enough', Trace.Text) = 1 then begin
            Rec.Amount := men;
            Rec.EnoughAmt := True;
            Trace.Before('enough');
          end
          else if (Copy(Trace.Text, 1, 1) >= '0')
            and (Copy(Trace.Text, 1, 1) <= '9') then Rec.Amount := Trace.Num
          else begin
            Trace.Block;
            Continue;
          end;

          Trace.SkipSpaces;

          // Usable
          if Pos('usable', Trace.Text) = 1 then begin
            Trace.Before('usable');
            Trace.SkipSpaces;
            Rec.Usable := True;
          end;

          // Unneeded
          if Pos('unneeded', Trace.Text) = 1 then begin
            Trace.Before('unneeded');
            Trace.SkipSpaces;
            Rec.Unneeded := True;
          end;

          if Pos('spoils', Trace.Text) = 1 then begin
            Rec.MaskText := 'spoils';
          end
          else if Pos('material', Trace.Text) = 1 then begin
            Rec.MaskText := 'materials';
          end
          else begin
            j := ItemClassCount-1;

            while (j >= 0) and (Pos(ItemClasses[j], Trace.Text) <> 1) do Dec(j);

            if j >= 0 then begin
              if ItemFilters[j] = IT_ALL then begin
                Rec.Mask := IT_ALL - IT_MAN - IT_MONSTER - IT_SILVER
              end
              else begin
                Rec.Mask := ItemFilters[j];
              end;

              Rec.MaskText := ItemClasses[j];
              Trace.Block;
            end
            else begin
              Rec.Data := Game.ItemData.FindByName(Trace.QBlock);

              if Rec.Data = nil then begin
                FreeAndNil(Rec);
              end
              else if Test(Rec.Data.Flags, IT_SILVER) and Rec.EnoughAmt then begin
                if IsLeader(AUnit) then begin
                  Rec.Amount := Rec.Amount * GameConfig.ReadInteger('Settings', 'LeaderMaintenance', 20);
                end
                else begin
                  Rec.Amount := Rec.Amount * GameConfig.ReadInteger('Settings', 'PeasantMaintenance', 10);
                end;
              end;
            end;
          end;

          if Rec <> nil then Needs.Add(Rec);
        end;

        Trace.Free;
      except
        on ENaN do ;
      end;
    end;
  end;
end;

procedure SetNeeds(U: TUnit; Needs: TNeedsList);
var i, j: integer;
    s: string;
    equip_added, nogive_added: boolean;
begin
  // Remove all needs from unit
  i := 0;
  while i < U.Orders.Count do
    if U.Order(i) = '@;needs' then U.Orders.Delete(i)
    else Inc(i);

  // Sort needs by priority
  for i := 0 to Needs.Count-1 do
    for j := i to Needs.Count-1 do
       if Needs[i].Priority > Needs[j].Priority then Needs.Exchange(i, j);

  // Add needs
  equip_added := False;
  nogive_added := False;
  i := 0;
  while i < Needs.Count do begin
    s := '@;needs ';
    j := i;
    while (i < Needs.Count) and (Needs[i].Priority = Needs[j].Priority) do begin
      if Needs[i].Equipment then begin
        if not equip_added then begin
          s := s + 'equipment, ';
          equip_added := True;
        end;
      end
      else if Needs[i].NoGive then begin
        if not nogive_added then begin
          s := s + 'nogive, ';
          nogive_added := True;
        end;
      end
      else begin
        // Amount
        if Needs[i].Amount = -1 then s := s + 'all'
        else if Needs[i].EnoughAmt then s := s + 'enough'
        else s := s + IntToStr(Needs[i].Amount);
        s := s + ' ';
        // Usable, Unneeded
        if Needs[i].Usable then s := s + 'usable ';
        if Needs[i].Unneeded then s := s + 'unneeded ';
        // Item
        if Needs[i].Data = nil then s := s + Needs[i].MaskText
        else s := s + '"' + Needs[i].Data.Name(Needs[i].Amount <> 1) + '"';
        s := s + ', ';
      end;

      Inc(i);
    end;
    if Needs[j].Priority > 0 then s := s + 'priority ' + IntToStr(Needs[j].Priority)
    else s := Copy(s, 1, Length(s) - 2);
    U.Orders.Add(s);
  end;
end;

procedure AddNeedItem(U: TUnit; IData: TItemData; Amount, Priority: integer);
var Needs: TNeedsList;
    Rec: TNeedItem;
begin
  Needs := TNeedsList.Create;
  ReadNeeds(U, Needs);
  Rec := Needs.Seek(IData, '');
  if Amount = -2 then Rec.EnoughAmt := True
  else Rec.Amount := Amount;
  Rec.Priority := Priority;
  SetNeeds(U, Needs);
  Needs.Free;
end;

{ Procedures }

function UnitCanUse(U: TUnit; IData: TItemData): boolean;
var Skill: TSkill;
begin
  Result := Test(IData.Flags, IT_MOUNT + IT_WEAPON + IT_ARMOR + IT_MAGIC +
    IT_SPECIAL);
  // Weapon (if needs skill)
  if Test(IData.Flags, IT_WEAPON) then begin
    if Test(IData.Weapon.Flags, WPN_NEEDSKILL) then begin
      Result := False;
      if (IData.Weapon.Skill1 <> nil)
        and (U.Skills.Find(IData.Weapon.Skill1.Short) <> nil) then
        Result := True
      else if (IData.Weapon.Skill2 <> nil)
        and (U.Skills.Find(IData.Weapon.Skill2.Short) <> nil) then
        Result := True;
    end;
  end
  // Mount allowed if skill present
  else if Test(IData.Flags, IT_MOUNT) then begin
    if (IData.Mount.RideSkill <> nil) then begin
      Skill := U.Skills.Find(IData.Mount.RideSkill.Short);
      if (Skill = nil) or (Skill.Level < IData.Mount.MinBonus) then
        Result := False;
    end;
  end
end;

function ArmorWeight(IData: TItemData; Behind: boolean): integer;
begin
  if Behind then Result := IData.Armor.Defence[wcPiercing] +
    IData.Armor.Defence[wcArmorPiercing]
  else Result := IData.Armor.Defence[wcSlashing] + IData.Armor.Defence[wcCrushing] +
    IData.Armor.Defence[wcCleaving];
  Inc(Result, (IData.Armor.Defence[wcEnergy] + IData.Armor.Defence[wcSpirit] +
    IData.Armor.Defence[wcWeather]) div 2);
end;

function RangedWeapon(IData: TItemData): boolean;
begin
  Result := Test(IData.Flags, IT_MAGIC)
    or Test(IData.Weapon.Flags, WPN_RANGED);
end;

function Bonus(U: TUnit; IData: TItemData): integer;
var Sk: TSkill;
    plus: integer;
begin
  Result := 0;
  if not UnitCanUse(U, IData) then Exit;

  if Test(IData.Flags, IT_MOUNT) then
    Result := IData.Mount.MaxBonus

  else if Test(IData.Flags, IT_ARMOR) then
    Result := ArmorWeight(IData, U.Flags[flgBehind])

  else if Test(IData.Flags, IT_WEAPON) then begin
    Result := IData.Weapon.AttackBonus + IData.Weapon.DefenceBonus;
    if Test(IData.Weapon.Flags, WPN_NUMATTSKILL) then begin
      plus := 0;
      if IData.Weapon.Skill1 <> nil then begin
        Sk := U.Skills.Find(IData.Weapon.Skill1.Short);
        if Sk <> nil then plus := Sk.Level;
      end;
      if IData.Weapon.Skill2 <> nil then begin
        Sk := U.Skills.Find(IData.Weapon.Skill2.Short);
        if Sk <> nil then plus := Max(plus, Sk.Level);
      end;
      Inc(Result, plus);
    end;
    // For behind units, prefer ranged weapons
    if U.Flags[flgBehind] and RangedWeapon(IData) then Inc(Result, 100);
    // Prefer weapon unit skilled with
    if Test(IData.Weapon.Flags, WPN_NEEDSKILL) then Inc(Result, 10);
    // Disable this for lances, if unit has Combat
    if (IData.Weapon.Skill1 <> nil)
      and (IData.Weapon.Skill1.Short = Keys[s_RidingSkill])
      and (U.Skills.Find(Keys[s_Combat]) <> nil) then
      Dec(Result, 10);
  end;
end;

function Prefer(U: TUnit; A, B: TItemData): integer;
var i, cap1, cap2: integer;
begin
  Result := CompareValue(Bonus(U, A), Bonus(U, B));
  if Result <> 0 then Exit;

  // Prefer stronger horses
  if Test(A.Flags, IT_MOUNT) and Test(B.Flags, IT_MOUNT) then begin
    cap1 := 0;
    for i := mtWalk to mtSwim do
      Inc(cap1, Max(0, A.Moves[i] - A.Weight));
    cap2 := 0;
    for i := mtWalk to mtSwim do
      Inc(cap2, Max(0, B.Moves[i] - B.Weight));
    Result := CompareValue(cap1, cap2);
  end;
  if Result <> 0 then Exit;

  // Finally, if there is equal profit, return index
  Result := CompareValue(Game.ItemData.IndexOf(A), Game.ItemData.IndexOf(B));
end;

function UnitTraining(U: TUnit): integer;
var i: integer;
begin
  Result := 0;
  for i := 0 to U.Skills.Count-1 do
    Inc(Result, (U.Skills[i].Points div 30) * U.Skills[i].Data.Cost);
end;

function UnitPriority(U1, U2: TUnit; Rec: TNeedItem): integer;
var lv1, lv2: integer;
    Skill: TSkill;
begin
  Result := 0;

  if Rec.Data <> nil then
    Result := CompareValue(Bonus(U1, Rec.Data), Bonus(U2, Rec.Data))
  else if Test(Rec.Mask, IT_MOUNT) then begin
    Skill := U1.Skills.Find(Keys[s_Riding]);
    if Skill <> nil then lv1 := Skill.Level
    else lv1 := 0;
    Skill := U2.Skills.Find(Keys[s_Riding]);
    if Skill <> nil then lv2 := Skill.Level
    else lv2 := 0;
    Result := CompareValue(lv1, lv2);
  end;

  if Result = 0 then
    Result := CompareValue(UnitTraining(U1), UnitTraining(U2));
end;

// Add all units' items to heap
procedure AddItems(U: TUnit; Needs, Heap: TNeedsList);
var i: integer;
    Rec: TNeedItem;
begin
  // Look for NoGive record
  i := Needs.Count-1;
  while (i >= 0) and not ((Needs[i].AUnit = U) and Needs[i].NoGive) do
    Dec(i);
  if i >= 0 then Exit;

  for i := 0 to U.Items.Count-1 do begin
    if U.Items[i].Bought then Continue;

    Rec := TNeedItem.Create;
    Rec.AUnit := U;
    Rec.Data := U.Items[i].Data;
    Rec.Amount := U.Items[i].Amount;
    Heap.Add(Rec);
  end;
end;

procedure AddUndeclaredNeeds(U: TUnit; Needs: TNeedsList);
var i, j: integer;
    Rec: TNeedItem;
begin
  // Determine if unit declared a needs
  j := Needs.Count-1;
  while (j >= 0) and (Needs[j].AUnit <> U) do Dec(j);
  // If this unit has no declared needs, it needs all its items
  if j < 0 then
    for i := 0 to U.Items.Count-1 do begin
      if U.Items[i].Bought then Continue;
      Rec := TNeedItem.Create;
      Rec.AUnit := U;
      Rec.Data := U.Items[i].Data;
      Rec.Amount := U.Items[i].Amount;
      Rec.Priority := 1;
      Needs.Add(Rec);
    end;
end;

procedure GetItem(Rec: TNeedItem; Heap: TNeedsList);
var i, j, amt, cmp: integer;
    Avai: TNeedsList;
begin
  Avai := TNeedsList.Create;

  // Form list of items for this request
  for i := 0 to Heap.Count-1 do
    if ((Heap[i].Data = Rec.Data) or MaskMatch(Rec, Heap[i].Data))
      and (Heap[i].Amount > 0)
      and (not Rec.Usable or UnitCanUse(Rec.AUnit, Heap[i].Data))
      then Avai.Add(Heap[i]);

  // Sort items
  for i := 0 to Avai.Count-1 do
    for j := i + 1 to Avai.Count-1 do begin
      if (Rec.Data = nil) then begin
        cmp := Prefer(Rec.AUnit, Avai[i].Data, Avai[j].Data);
        if cmp < 0 then Avai.Exchange(i, j);
        if cmp <> 0 then Continue;
      end;

      if (Avai[i].AUnit <> Rec.AUnit) and (Avai[j].AUnit = Rec.AUnit) then
        Avai.Exchange(i, j);
    end;

  // Get items
  i := 0;
  while (i < Avai.Count) and ((Rec.Amount > 0) or (Rec.Amount = -1)) do begin
    if Rec.Amount = -1 then amt := Avai[i].Amount
    else amt := Min(Rec.Amount, Avai[i].Amount);

    if Avai[i].AUnit <> Rec.AUnit then begin
      // Add to completion record
      if (Rec.AUnit = CompleteNeeds.AUnit)
        and (Avai[i].Data = CompleteNeeds.IData) then
        Inc(CompleteNeeds.Amount, amt);

      // Ugly hack to avoid modifying orders when we just want to calc smth
      if CompleteNeeds.AUnit = nil then begin
        AddOrderTo(Avai[i].AUnit, 'give ' + Rec.AUnit.NumStr + ' ' +
          IntToStr(amt) + ' "' + Avai[i].Data.Name(amt <> 1) + '"', False);
        Modified := True;
      end;
    end;

    if Rec.Amount > 0 then Dec(Rec.Amount, amt);
    Dec(Avai[i].Amount, amt);
    Inc(i);
  end;

  Avai.Free;
end;

procedure DoDistribute(R: TRegion; Needs: TNeedsList);
var i, j: integer;
    Heap: TNeedsList;
    Units: TUnitList;
begin
  Units := R.PlayerTroop.Units;

  // Fill array of all items in region, with owner priorities
  Heap := TNeedsList.Create;
  for i := 0 to Units.Count-1 do begin
    AddItems(Units[i], Needs, Heap);
    AddUndeclaredNeeds(Units[i], Needs);
  end;

  // Sort needs
  for i := 0 to Needs.Count-1 do
    for j := i+1 to Needs.Count-1 do
      // unneeded comes last (so all needs will be already distributed)
      if Needs[i].Unneeded and not Needs[j].Unneeded then Needs.Exchange(i, j)
      else if Needs[i].Unneeded = Needs[j].Unneeded then begin
        // by priority
        if Needs[i].Priority < Needs[j].Priority then Needs.Exchange(i, j)
        else if Needs[i].Priority = Needs[j].Priority then begin
          // and then by unit precedence for same items
          if Needs[i].EqualTo(Needs[j])
            and (UnitPriority(Needs[i].AUnit, Needs[j].AUnit, Needs[i]) < 0) then
              Needs.Exchange(i, j);
        end;
      end;

  // Get needed items
  for i := 0 to Needs.Count-1 do GetItem(Needs[i], Heap);

  Heap.ClearAndFree;
end;

function DistributeNeeds(R: TRegion): boolean;
var j: integer;
    Troop: TTroop;
    NeedsList: TNeedsList;
begin
  Modified := False;
  Result := False;
  CompleteNeeds.Amount := 0;

  Troop := R.PlayerTroop;
  if Troop = nil then Exit;

  // Get needs of region units
  NeedsList := TNeedsList.Create;
  for j := 0 to Troop.Units.Count-1 do ReadNeeds(Troop.Units[j], NeedsList);

  // Distribute needed items
  if NeedsList.Count > 0 then DoDistribute(R, NeedsList);

  NeedsList.ClearAndFree;
  Result := Modified;
end;

end.
