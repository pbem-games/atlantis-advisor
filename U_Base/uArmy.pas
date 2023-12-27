unit uArmy;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Types, DataStructs, Math, uKeys, Resources;

type
  TSoldier = class
    Attack: integer;
    Defence: array[0..atCount-1] of integer;
    NumAttacks: integer;
    Hits, MaxHits: integer;
    Regen: integer;
    Heals, HealRate: integer;
    Damage: integer;
    Special: TSpecData;
    SpecLevel: integer;
    AmuletOfI: boolean;
    // References
    URef: TBaseUnit;
    Struct: TStruct;
    Man, Mount, Weapon, Armor: TItemData;
    BItems: TItemDataList;
    Effects: TEffectDataList;
    destructor Destroy; override;
    function HasEffect(Effect: TEffectData): boolean;
    procedure SetEffect(Effect: TEffectData);
    procedure ClearEffect(Idx: integer);
    procedure ClearOneTimeEffects;
  end;

  TSoldierList = class(TList)
  protected
    function Get(Index: Integer): TSoldier;
    procedure Put(Index: Integer; Item: TSoldier);
  public
    property Items[Index: Integer]: TSoldier read Get write Put; default;
    procedure ClearAndFree;
  end;

  TArmy = class
    CanFront, CanBehind, NotFront, NotBehind: integer;
    HitsTotal, HitsAlive: integer;
    Round: integer;
    Tactics: integer;
    Tactitian: TBaseUnit;
    Soldiers: TSoldierList;
    Shields: array of record
      AttackType: integer;
      Level: integer;
    end;
    constructor Create(BUnits: TBattleUnitList; R: TRegion);
    destructor Destroy; override;
    function Broken: boolean;
    function CanAttack: integer;
    function NumAlive: integer;
    function NumFront: integer;
    function GetAttacker(num: integer; var behind: boolean): TSoldier;
    procedure Regenerate;
    procedure Reset;
    procedure Restart(Sort: boolean; FrontCount: integer);
    procedure DoAnAttack(Special: TSpecData; numAttacks, attackType, attackLevel:
      integer; WFlags: DWord; weaponClass: integer; Effect: TEffectData;
      mountBonus: integer);
    procedure UpdateShields;
    function GetHighShield(attackType: integer): integer;
    function Hits(a, d: integer): boolean;
    function GetTargetNum(Special: TSpecData): integer;
    function CheckSpecialTarget(Sp: TSpecData; tar: integer): boolean;
    procedure Kill(num: integer);
    procedure DoHeal;
  end;

  TSimulation = class
    Armies: array[0..1] of TArmy;
    FBattle: TBattle;
    constructor Create(Battle: TBattle; AAddActions: boolean);
    destructor Destroy; override;
    function Run: integer;
    procedure Restart;
    procedure FreeRound(Atk, Def: TArmy);
    procedure NormalRound(A, B: TArmy);
    procedure DoAttack(Round: integer; Soldier: TSoldier; Atk, Def: TArmy;
      behind: boolean);
    procedure DoSpecialAttack(Sp: TSpecData; Level: integer; Def: TArmy);
    procedure DoAddRound(Num: integer);
    procedure DoAddAction(U: TBaseUnit; AType, APower: integer;
      ASpecial: TSpecData);
  end;

  function AddSoldiers(List: TSoldierList; AUnit: TBaseUnit; R: TRegion;
    var Index: integer): integer;
  function TypicalSoldier(AUnit: TBaseUnit; R: TRegion): TSoldier;
  function EqualSoldiers(S1, S2: TSoldier): boolean;
  function TacticsLevel(BUnits: TBattleUnitList; var Tactitian: integer): integer;
  function CreateSoldier(AUnit: TBaseUnit; Man: TItemData; Items: TItemList;
    R: TRegion): TSoldier;

implementation

var
  AddActions: boolean;
  Sim: TSimulation;

function TacticsLevel(BUnits: TBattleUnitList; var Tactitian: integer): integer;
var i, num: integer;
    Skill: TSkill;
begin
  Result := 0;
  num := 0;
  Tactitian := -1;
  // Find best tactitian
  for i := 0 to BUnits.Count-1 do begin
    Skill := BUnits[i].Skills.Find(Keys[s_Tactics]);
    if (Skill <> nil) and ( (Skill.Level > Result)
      or ((Skill.Level = Result) and (num > BUnits[i].Num)) ) then begin
      Result := Skill.Level;
      num := BUnits[i].Num;
      Tactitian := i;
    end;
  end;

  // first unit is general
  if Tactitian < 0 then Tactitian := 0;
end;

function EqualSoldiers(S1, S2: TSoldier): boolean;
var i: integer;
begin
  Result := True;
  if S1.Man <> S2.Man then Result := False
  else if S1.Weapon <> S2.Weapon then Result := False
  else if S1.Armor <> S2.Armor then Result := False
  else if S1.Mount <> S2.Mount then Result := False
  else if S1.URef <> S2.URef then Result := False
  else if (S1.BItems = nil) xor (S2.BItems = nil) then Result := False
  else if (S1.BItems <> nil) and (S2.BItems <> nil) then begin
    if S1.BItems.Count <> S2.BItems.Count then Result := False
    else begin
      i := S1.BItems.Count-1;
      while (i >= 0) and (S1.BItems[i] = S2.BItems[i]) do Dec(i);
      Result := (i < 0);
    end;
  end;
end;

function GetArmor(Items: TItemList): TItemData;
var i, d: integer;
begin
  Result := nil;
  // Return best armor from list
  i := -1;
  d := 0;
  while (d < Game.ItemData.Count) and (i = -1) do
    if Test(Game.ItemData[d].Flags, IT_ARMOR) then begin
      i := Items.Count-1;
      while (i >= 0) and ((Items[i].Data <> Game.ItemData[d])
        or (Items[i].Amount = 0)) do Dec(i);
      if i = -1 then Inc(d);
    end
    else Inc(d);
  if i >= 0 then begin
    Result := Items[i].Data;
    Dec(Items[i].Amount);
  end;
end;

function GetMount(AUnit: TBaseUnit; fly, ride: boolean; Items: TItemList;
  var bonus: integer): TItemData;
var i, d: integer;
    IData: TItemData;
    Skill: TSkill;
begin
  bonus := 0;
  Result := nil;
  if not fly and not ride then Exit;
  for d := 0 to Game.ItemData.Count-1 do begin
    IData := Game.ItemData[d];
    if Test(IData.Flags, IT_MOUNT) and ( ((IData.Moves[mtFly] > 0) and fly)
      or ((IData.Moves[mtRide] > 0) and ride) ) then begin
      // Found mount available in region, check skill
      if IData.Mount.RideSkill <> nil then begin
        Skill := AUnit.Skills.Find(IData.Mount.RideSkill.Short);
        if (Skill = nil) or (Skill.Level < IData.Mount.MinBonus) then
          Continue
        else begin
          // Skill is enough, set bonus
          if (IData.Moves[mtFly] > 0) and not fly then
            bonus := Min(Skill.Level, IData.Mount.MaxHamperedBonus)
          else bonus := Min(Skill.Level, IData.Mount.MaxBonus);
        end;
      end;
      if bonus = 0 then Continue;
      // Find in items
      i := Items.Count-1;
      while (i >= 0) and ((Items[i].Data <> Game.ItemData[d])
        or (Items[i].Amount = 0)) do Dec(i);
      if i >= 0 then begin
        Result := Items[i].Data;
        Dec(Items[i].Amount);
        Break;
      end;
    end;
  end;
end;

function GetWeapon(AUnit: TBaseUnit; ridingBonus: integer; var attackBonus,
  defenceBonus, NumAttacks: integer; Items: TItemList): TItemData;
var i, d: integer;
    IData: TItemData;
    Skill, orSkill: TSkill;
    WFlags: DWord;
begin
  Result := nil;
  attackBonus := 0;
  defenceBonus := 0;
  NumAttacks := 1;

  for d := 0 to Game.ItemData.Count-1 do begin
    IData := Game.ItemData[d];
    if Test(IData.Flags, IT_WEAPON) then begin
      WFlags := IData.Weapon.Flags;
      // Check riding
      if ( Test(WFlags, WPN_NOFOOT) and (ridingBonus = 0) )
        or ( Test(WFlags, WPN_NOMOUNT) and (ridingBonus > 0) ) then
        Continue;
      // Get skill level
      Skill := nil;
      if not Test(WFlags, WPN_NEEDSKILL) then
        Skill := AUnit.Skills.Find(Keys[s_Combat])
      else begin
        orSkill := nil;
        if IData.Weapon.Skill1 <> nil then
          Skill := AUnit.Skills.Find(IData.Weapon.Skill1.Short);
        if IData.Weapon.Skill2 <> nil then
          orSkill := AUnit.Skills.Find(IData.Weapon.Skill2.Short);
        if (Skill <> nil) and (orSkill <> nil)
          and (orSkill.Level > Skill.Level) then
          Skill.Level := orSkill.Level; // will use level only, anyway
      end;
      // If needed skill not found
      if (Skill = nil) and Test(WFlags, WPN_NEEDSKILL) then Continue;

      // Find in items
      i := Items.Count-1;
      while (i >= 0) and ((Items[i].Data <> Game.ItemData[d])
        or (Items[i].Amount = 0)) do Dec(i);
      if i >= 0 then begin

        // Attack and defence skill
        attackBonus := IData.Weapon.AttackBonus;
        if Skill <> nil then Inc(attackBonus, Skill.Level);
        defenceBonus := IData.Weapon.DefenceBonus;
        if not Test(WFlags, WPN_NOATTACKERSKILL) and (Skill <> nil) then
          Inc(defenceBonus, Skill.Level);
        // Riding bonus
        if Test(WFlags, WPN_RIDINGBONUS) then
          Inc(attackBonus, ridingBonus);
        if Test(WFlags, WPN_RIDINGBONUSDEFENSE + WPN_RIDINGBONUS) then
          Inc(defenceBonus, ridingBonus);
        // Number of attacks
        if Test(WFlags, WPN_NUMATTSKILL) and (Skill <> nil) then
          NumAttacks := Skill.Level
        else if Test(WFlags, WPN_NUMATTHALFSKILL) and (Skill <> nil) then
          NumAttacks := Ceil(Skill.Level / 2)
        else NumAttacks := IData.Weapon.NumAttacks;
        if NumAttacks = 0 then NumAttacks := 1;

        Result := Items[i].Data;
        Dec(Items[i].Amount);
        Break;
      end;
    end;
  end;
end;

function CreateSoldier(AUnit: TBaseUnit; Man: TItemData; Items: TItemList;
  R: TRegion): TSoldier;
var S: TSoldier;
    i, j, ridingBonus, attackBonus, defenceBonus: integer;
    Skill: TSkill;
    Item: TItem;
    Spec: TSpecData;
begin
  Result := TSoldier.Create;
  S := Result;
  // Setup
  S.URef := AUnit;
  S.Struct := AUnit.Struct;
  S.Man := Man;
  S.NumAttacks := 1;
  S.Attack := 0;
  S.Defence[atMelee] :=   0;
  S.Defence[atEnergy] :=  -2;
  S.Defence[atSpirit] :=  -2;
  S.Defence[atWeather] := -2;
  S.Defence[atRiding] :=  0;
  S.Defence[atRanged] :=  0;
  S.Hits := 1;
  S.MaxHits := 1;
  S.Damage := 0;
  S.Heals := 0;
  S.HealRate := 0;

  // Building bonus
  if (S.Struct <> nil) and (S.Struct.Protection > 0) then begin
    for i := 0 to atCount-1 do Inc(S.Defence[i], 2);
    if S.Struct.Runes > 0 then begin
      S.Defence[atEnergy] := S.Struct.Runes;
      S.Defence[atSpirit] := S.Struct.Runes;
    end;
    Dec(S.Struct.Protection);
  end;

  // If man is actually monster
  if Test(Man.Flags, IT_MONSTER) then begin
    S.Attack := Man.Monster.Attack;
    for i := 0 to atCount-1 do
      if S.Defence[i] < 0 then
        S.Defence[i] := Max(Man.Monster.Defence[i], S.Defence[i])
      else Inc(S.Defence[i], Man.Monster.Defence[i]);
    S.Hits := Max(1, Man.Monster.Hits);
    S.MaxHits := S.Hits;
    S.NumAttacks := Max(1, Man.Monster.NumAttacks);
    S.Special := Man.Special;
    S.SpecLevel := Man.SpecLevel;
    S.Regen := Man.Monster.Regen;
  end

  // If man
  else begin
    // Setup healing
    if AUnit.Mage then begin
      Skill := AUnit.Skills.Find(Keys[s_MagicalHealing]);
      if Skill <> nil then begin
        S.Heals := HealDefs[Min(5, Skill.Level), 0];
        S.HealRate := HealDefs[Min(5, Skill.Level), 1];
      end;
    end;
    if S.Heals = 0 then begin
      Item := Items.Find(Keys[s_HealingPotions]);
      if (Item <> nil) and (Item.Amount > 0) then begin
        S.Heals := 10;
        S.HealRate := HealDefs[1, 1];
        Dec(Items[Items.IndexOf(Item)].Amount);
      end
      else begin
        Skill := AUnit.Skills.Find(Keys[s_Healing]);
        if Skill <> nil then begin
          Item := Items.Find(Keys[s_Herbs]);
          if (Item <> nil) and (Item.Amount > 0) then begin
            S.Heals := Min(Skill.Level * GameConfig.ReadInteger('Settings',
              'HealsPerMan', 5), Item.Amount);
            S.HealRate := HealDefs[1, 1];
            Dec(Items[Items.IndexOf(Item)].Amount, S.Heals);
          end
        end;
      end;
    end;

    // Setup combat spell
    if AUnit.CombatSpell <> nil then begin
      Skill := AUnit.Skills.Find(AUnit.CombatSpell.Short);
      if Skill <> nil then begin
        S.Special := Skill.Data.Special;
        S.SpecLevel := Skill.Level;
      end;
    end;

    // Setup battle items
    for i := 0 to Items.Count-1 do
      if (Items[i].Data.Special <> nil) and
        not Test(Items[i].Data.Flags, IT_WEAPON + IT_ARMOR + IT_MOUNT)
        and (Items[i].Amount > 0) then begin
        Spec := Items[i].Data.Special;
        // Skip attack items if unit already has combat spell
        if (S.Special <> nil) and (Length(Spec.Attacks) > 0) then Continue;
        // Skip mageonly items for no-mage
        if not AUnit.Mage and Items[i].Data.Magic.MageOnly then Continue;

        // Take item
        if S.BItems = nil then S.BItems := TItemDataList.Create;
        S.BItems.Add(Items[i].Data);
        Dec(Items[i].Amount);
        // Set attack ability
        if Length(Spec.Attacks) > 0 then begin
          S.Special := Spec;
          S.SpecLevel := AUnit.Items[i].Data.SpecLevel;
        end;
        // Apply personal shields
        if Test(Spec.Flags, SP_SHIELD) then begin
          j := 0;
          while (j < atCount) and not Spec.Shields[j] do Inc(j);
          if j = atCount then // Invulnerability has all shields
            S.AmuletOfI := True
          else
            for j := 0 to atCount-1 do
              if Spec.Shields[j] and (S.Defence[j] < Items[i].Data.SpecLevel) then
                S.Defence[j] := Items[i].Data.SpecLevel;
        end;
      end;

    // Get soldier's equipment
    S.Armor := GetArmor(Items);

    if R <> nil then
      S.Mount := GetMount(AUnit, Test(R.Terrain.Flags, TER_FLYINGMOUNTS),
        Test(R.Terrain.Flags, TER_RIDINGMOUNTS), Items, ridingBonus)
    else S.Mount := GetMount(AUnit, True, True, Items, ridingBonus);
    if S.Mount <> nil then Inc(S.Defence[atRiding], ridingBonus);

    S.Weapon := GetWeapon(AUnit, ridingBonus, attackBonus, defenceBonus,
      S.NumAttacks, Items);

    // If we did not get a weapon, set attack and defense bonuses to
    // combat skill (and riding bonus if applicable).
    if (S.Weapon = nil) then begin
      Skill := AUnit.Skills.Find(Keys[s_Combat]);
      if Skill <> nil then attackBonus := Skill.Level + ridingBonus
      else attackBonus := ridingBonus;
      defenceBonus := attackBonus;
      S.NumAttacks := 1;
    end
    else if S.Weapon.Special <> nil then begin
      S.Special := S.Weapon.Special;
      S.SpecLevel := S.Weapon.SpecLevel;
    end;

    // Set the attack and defense skills
    Inc(S.Attack, attackBonus);
    Inc(S.Defence[atMelee], defenceBonus);
  end;
end;

function TypicalSoldier(AUnit: TBaseUnit; R: TRegion): TSoldier;
var Items: TItemList;
    i: integer;
begin
  Items := TItemList.Create;
  Items.AssignItems(AUnit.Items);
  Result := nil;

  i := 0;
  while (i < Items.Count) and (Result = nil) do
    if Test(Items[i].Data.Flags, IT_MAN + IT_MONSTER) then
      Result := CreateSoldier(AUnit, Items[i].Data, Items, R)
    else Inc(i);

  Items.ClearAndFree;
end;

function AddSoldiers(List: TSoldierList; AUnit: TBaseUnit; R: TRegion;
  var Index: integer): integer;
var Items: TItemList;
    i: integer;
    S: TSoldier;
begin
  Items := TItemList.Create;
  Items.AssignItems(AUnit.Items);

  // Take men from unit
  Result := 0;
  for i := 0 to Items.Count-1 do
    if Test(Items[i].Data.Flags, IT_MAN + IT_MONSTER) then
      while Items[i].Amount > 0 do begin
        S := CreateSoldier(AUnit, Items[i].Data, Items, R);
        if Index = -1 then List.Add(s)
        else begin
          List.Insert(Index, s);
          Inc(Index);
        end;
        Inc(Result, List[List.Count-1].Hits);
        Dec(Items[i].Amount);
      end;

  Items.ClearAndFree;
end;


{ TSoldierList }

procedure TSoldierList.ClearAndFree;
var i: integer;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Free;
end;

function TSoldierList.Get(Index: Integer): TSoldier;
begin
  Result := TSoldier(inherited Get(Index));
end;

procedure TSoldierList.Put(Index: Integer; Item: TSoldier);
begin
  inherited Put(Index, Item);
end;


{ TSoldier }

destructor TSoldier.Destroy;
begin
  FreeAndNil(BItems);
  FreeAndNil(Effects);
end;

function TSoldier.HasEffect(Effect: TEffectData): boolean;
begin
  Result := (Effects <> nil) and (Effects.IndexOf(Effect) >= 0);
end;

procedure TSoldier.SetEffect(Effect: TEffectData);
var i: integer;
begin
  Inc(Attack, Effect.Attack);
  for i := 0 to atCount-1 do
    Inc(Defence[i], Effect.Defence[i]);
  if Effects = nil then Effects := TEffectDataList.Create;
  Effects.Add(Effect);
end;

procedure TSoldier.ClearEffect(Idx: integer);
var i: integer;
begin
  if Effects = nil then Exit;
  Dec(Attack, Effects[Idx].Attack);
  for i := 0 to atCount-1 do
    Dec(Defence[i], Effects[Idx].Defence[i]);
  Effects.Delete(Idx);
end;

procedure TSoldier.ClearOneTimeEffects;
var i: integer;
begin
  if Effects = nil then Exit;
  i := 0;
  while i < Effects.Count do
    if Test(Effects[i].Flags, EFF_ONESHOT) then ClearEffect(i)
    else Inc(i);
end;


{ TArmy }

constructor TArmy.Create(BUnits: TBattleUnitList; R: TRegion);
var i, idx, FrontCount: integer;
begin
  Soldiers := TSoldierList.Create;

  // Find best tactitian
  Tactics := TacticsLevel(BUnits, idx);

  // Fill soldiers from battle units
  FrontCount := 0;
  HitsTotal := 0;
  for i := 0 to BUnits.Count-1 do begin
    if BUnits[i].Behind then idx := -1
    else idx := FrontCount;
    Inc(HitsTotal, AddSoldiers(Soldiers, BUnits[i], R, idx));
    if idx >= 0 then FrontCount := Idx;
  end;

  Restart(False, FrontCount);
end;

procedure TArmy.Restart(Sort: boolean; FrontCount: integer);
var i: integer;
begin
  if Sort then begin
    FrontCount := 0;
    // Sort soldiers by front/behind
    for i := 0 to Soldiers.Count-1 do
      if not TBattleUnit(Soldiers[i].URef).Behind then begin
        Soldiers.Exchange(FrontCount, i);
        Inc(FrontCount);
      end;
    // Restore soldiers
    for i := 0 to Soldiers.Count-1 do begin
      if Soldiers[i].Effects <> nil then
        while Soldiers[i].Effects.Count > 0 do
          Soldiers[i].ClearEffect(0);
      Soldiers[i].Hits := Soldiers[i].MaxHits;
      Soldiers[i].Damage := 0;
    end;
  end;
  // Reset variables
  HitsAlive := HitsTotal;
  CanFront := FrontCount;
  CanBehind := Soldiers.Count;
  NotFront := Soldiers.Count;
  NotBehind := Soldiers.Count;
  if NumFront = 0 then begin
    CanFront := CanBehind;
    NotFront := NotBehind;
  end;
end;

procedure TArmy.Reset;
begin
  CanFront := NotFront;
  CanBehind := NotBehind;
  NotFront := NotBehind;
end;

destructor TArmy.Destroy;
begin
  Soldiers.Free;
end;

function TArmy.Broken: boolean;
begin
  if GameConfig.ReadInteger('Settings', 'ArmyRout', 0) = 0 then
    Result := ((NumAlive * 2) < Soldiers.Count)
  else
    Result := ((HitsAlive * 2) < HitsTotal);
end;

function TArmy.CanAttack: integer;
begin
  Result := CanBehind;
end;

function TArmy.NumAlive: integer;
begin
  Result := NotBehind;
end;

function TArmy.NumFront: integer;
begin
  Result := (CanFront + NotFront - CanBehind);
end;

function TArmy.GetAttacker(num: integer; var behind: boolean): TSoldier;
begin
  Result := Soldiers[num];
  if num < CanFront then begin
    Soldiers[num] := Soldiers[CanFront-1];
    Soldiers[CanFront-1] := Soldiers[CanBehind-1];
    Soldiers[CanBehind-1] := Result;
    Dec(CanFront);
    Dec(CanBehind);
    behind := False;
  end
  else begin
    Soldiers[num] := Soldiers[CanBehind-1];
    Soldiers[CanBehind-1] := Soldiers[NotFront-1];
    Soldiers[NotFront-1] := Result;
    Dec(CanBehind);
    Dec(NotFront);
    behind := True;
  end;
end;

procedure TArmy.Regenerate;
var i, diff: integer;
    Sl: TSoldier;
begin
  for i := 0 to NotBehind-1 do begin
    Sl := Soldiers[i];
    diff := Sl.MaxHits - Sl.Hits;
    if (diff > 0) then begin
      Sl.Damage := 0;
      Inc(Sl.Hits, Min(diff, Sl.Regen));
    end;
  end;
end;

function TArmy.GetHighShield(attackType: integer): integer;
var i, lv: integer;
begin
  Result := -1;
  lv := 0;
  for i := 0 to Length(Shields) - 1 do
    if (Shields[i].AttackType = attackType) and ((Result = -1)
      or (Shields[i].Level > lv)) then Result := i;
end;

function TArmy.Hits(a, d: integer): boolean;
var tohit, tomiss: integer;
begin
  tohit := 1;
  tomiss := 1;
  if a > d then tohit := System.Round(Power(2, a - d))
  else if d > a then tomiss := System.Round(Power(2, d - a));
  Result := (Random(tohit+tomiss) < tohit);
end;

procedure TArmy.DoAnAttack(Special: TSpecData; numAttacks, attackType,
  attackLevel: integer; WFlags: DWord; weaponClass: integer;
  Effect: TEffectData; mountBonus: integer);
var combat, canShield: boolean;
    hi, i, tarnum, defenceLevel, attLen, defLen: integer;
    tarFlags: DWord;
    tar: TSoldier;
begin
  // 1. Check against Global effects (not sure how yet)
  // 2. Attack shield
  combat := (attackType in [atRanged, atMelee, atRiding]);
  canShield := (attackType in [atRanged, atEnergy, atSpirit, atWeather]);

  if canShield then begin
    hi := GetHighShield(attackType);
    if hi >= 0 then begin
      // Check if we get through shield
      if not Hits(attackLevel, Shields[hi].Level) then Exit;
      if (Effect = nil) and not combat then begin
        // We got through shield... if killing spell, destroy shield
        for i := hi to Length(Shields)-2 do
          Shields[i] := Shields[i+1];
        SetLength(Shields, Length(Shields) - 1);
      end;
    end;
  end;

  // Now, loop through and do attacks
  for i := 1 to numAttacks do begin
    // 3. Get the target
    tarnum := GetTargetNum(Special);
    if tarnum = -1 then Continue;
    tar := Soldiers[tarnum];
    if tar.Weapon <> nil then tarFlags := tar.Weapon.Weapon.Flags
    else tarFlags := 0;

    // 4. Add in any effects, if applicable
    defenceLevel := 0;
    if attackType <> atNonResistable then
      defenceLevel := tar.Defence[attackType];

    if (Special <> nil) and Test(Special.Flags, SP_NOBUILDING) and
      (tar.Struct <> nil) then Dec(defenceLevel, 2);

    // 4.1 Check whether defense is allowed against this weapon
//    if((flags & WeaponType::NODEFENSE) && (defenceLevel > 0)) defenceLevel = 0;

    if not Test(WFlags, WPN_RANGED) then begin
      // 4.2 Check relative weapon length
      attLen := 1;
      defLen := 1;
      if Test(WFlags, WPN_LONG) then attLen := 2
      else if Test(WFlags, WPN_SHORT) then attLen := 0;
      if Test(tarFlags, WPN_LONG) then defLen := 2
      else if Test(tarFlags, WPN_SHORT) then defLen := 0;
      if attLen > defLen then Inc(attackLevel)
      else if defLen > attLen then Inc(defenceLevel);
    end;

    // 4.3 Add bonuses versus mounted
    if tar.Mount <> nil then Inc(attackLevel, mountBonus);

    // 5. Attack soldier
    if (attackType <> atNonResistable) then begin
      if not Test(WFlags, WPN_ALWAYSREADY) and (Random(2) > 0) then
        Continue;
      if not Hits(attackLevel, defenceLevel) then Continue;
    end;

    // 6. If attack got through, apply effect, or kill
    if Effect = nil then begin
      // 7. Last chance... Check armor
      if (tar.Armor <> nil)
        and (tar.Armor.Armor.Defence[weaponClass] > Random(100)) then Continue;

      // 8. Seeya!
      Kill(tarnum);
    end
    else begin
      if (tar.HasEffect(Effect)) then Continue;
      tar.SetEffect(Effect);
    end;
  end;
end;

procedure TArmy.UpdateShields;
var i, j, bonus: integer;
    Sp: TSpecData;
begin
  for i := 0 to NotBehind-1 do begin
    if Soldiers[i].Special = nil then Continue;
    Sp := Soldiers[i].Special;

    // Shields
    if Test(Sp.Flags, SP_SHIELD) then
      for j := 0 to atCount-1 do
        if Sp.Shields[j] then begin
          SetLength(Shields, Length(Shields) + 1);
          Shields[Length(Shields) - 1].AttackType := j;
          Shields[Length(Shields) - 1].Level := Soldiers[i].SpecLevel;
          if AddActions then Sim.DoAddAction(Soldiers[i].URef, raCast,
            Soldiers[i].SpecLevel, Sp);
        end;

    // Defence bonuses
    if Round = 0 then
      for j := 0 to Length(Sp.Defences) - 1 do begin
        bonus := Sp.Defences[j].Bonus;
        if Test(Sp.Flags, SP_USE_LEV) then
          bonus := bonus * Soldiers[i].SpecLevel;
        Inc(Soldiers[i].Defence[Sp.Defences[j].AttackType], bonus);
      end;
  end;
end;

function TArmy.GetTargetNum(Special: TSpecData): integer;
var tars, validtargs, i, start, targ: integer;
begin
  Result := -1;

  tars := NumFront;
  if tars = 0 then begin
    canfront := canbehind;
    notfront := notbehind;
    tars := NumFront;
    if tars = 0 then Exit;
  end;

  if Special <> nil then begin
    validtargs := 0;
    start := -1;

    for i := 0 to canfront-1 do begin
      if CheckSpecialTarget(Special, i) then begin
        Inc(validtargs);
        // slight scan optimisation - skip empty initial sequences
        if start = -1 then start := i;
      end;
    end;
    for i := canbehind to notfront-1 do begin
      if CheckSpecialTarget(Special, i) then begin
        Inc(validtargs);
        // slight scan optimisation - skip empty initial sequences
        if start = -1 then start := i;
      end;
    end;
    if validtargs > 0 then begin
      targ := Random(validtargs);
      i := start;
      while i < notfront do begin
        if i = canfront then i := canbehind;
        if CheckSpecialTarget(Special, i) then begin
          if targ = 0 then begin
            Result := i;
            Exit;
          end
          else Dec(targ);
        end;
        Inc(i);
      end;
    end;
  end
  else begin
    Result := Random(tars);
    if Result >= canfront then Result := Result + canbehind - canfront;
  end;
end;

procedure TArmy.Kill(num: integer);
var Sl: TSoldier;
begin
  Sl := Soldiers[num];
  if AddActions then
    Sim.DoAddAction(Sl.URef, raTakeHits, 1, nil);

  if Sl.AmuletOfI then Exit;

  if GameConfig.ReadInteger('Settings', 'ArmyRout', 0) = 2 then
    Dec(HitsAlive);
  Inc(Sl.Damage);
  Dec(Sl.Hits);
  if Sl.Hits > 0 then Exit;

  Inc(TBattleUnit(Sl.URef).Loses);
  if GameConfig.ReadInteger('Settings', 'ArmyRout', 0) = 3 then begin
    if Test(Sl.Man.Flags, IT_MONSTER) then
      Dec(HitsAlive, Sl.Man.Monster.Hits)
    else
      Dec(hitsalive);
  end;

  if num < CanFront then begin
    Soldiers[num] := Soldiers[CanFront-1];
    Soldiers[CanFront-1] := Sl;
    num := CanFront - 1;
    Dec(CanFront);
  end;

  if num < CanBehind then begin
    Soldiers[num] := Soldiers[CanBehind-1];
    Soldiers[CanBehind-1] := Sl;
    num := CanBehind-1;
    Dec(CanBehind);
  end;

  if num < NotFront then begin
    Soldiers[num] := Soldiers[NotFront-1];
    Soldiers[NotFront-1] := Sl;
    num := NotFront-1;
    Dec(NotFront);
  end;

  Soldiers[num] := Soldiers[NotBehind-1];
  Soldiers[NotBehind-1] := Sl;
  Dec(NotBehind);
end;

function TArmy.CheckSpecialTarget(Sp: TSpecData; tar: integer): boolean;
var i: integer;
begin
  Result := True;

  // Buildings
  if Sp.Structs.Count > 0 then begin
    i := Sp.Structs.Count-1;
    while (i >= 0) and (Soldiers[tar].Struct.Data <> Sp.Structs[i]) do Dec(i);
    if not ((i >= 0) xor Test(Sp.Flags, HIT_BUILDINGEXCEPT)) then Result := False;
  end;

  // Items (man, mount)
  if Sp.Items.Count > 0 then begin
    i := Sp.Items.Count-1;
    while (i >= 0) and (Soldiers[tar].Man <> Sp.Items[i])
      and (Soldiers[tar].Mount <> Sp.Items[i]) do Dec(i);
    if not ((i >= 0) xor Test(Sp.Flags, HIT_ITEMEXCEPT)) then Result := False;
  end;

  // Effects
  if Sp.Effects.Count > 0 then begin
    if Soldiers[tar].Effects = nil then i := -1
    else begin
      i := Sp.Effects.Count-1;
      while (i >= 0) and (Soldiers[tar].Effects.IndexOf(Sp.Effects[i]) = -1) do
        Dec(i);
    end;
    if not ((i >= 0) xor Test(Sp.Flags, HIT_EFFECTEXCEPT)) then Result := False;
  end;

  // Illusions
  if Test(Sp.Flags, HIT_ILLUSION) and Test(Soldiers[tar].Man.Flags,
    IT_MONSTER) and Soldiers[tar].Man.Monster.Illusion then
    Result := False;

  // No monsters
  if Test(Sp.Flags, HIT_NOMONSTER) and Test(Soldiers[tar].Man.Flags,
    IT_MONSTER) then Result := False;
end;

procedure TArmy.DoHeal;
var lv, i, j, attempts: integer;
    Sl, temp: TSoldier;
begin
  attempts := Soldiers.Count - NumAlive;
  lv := 5;
  while lv >= 1 do begin
    for i := 0 to NumAlive-1 do begin
      if attempts = 0 then Exit;
      Sl := Soldiers[i];
      if Sl.HealRate <> HealDefs[lv, 1] then Continue;

      // Heal
      while Sl.Heals > 0 do begin
        if attempts = 0 then Exit;
        Dec(Sl.Heals);
        j := Random(Soldiers.Count - NumAlive) + notbehind;
        if Random(100) < HealDefs[lv, 1] then begin
          temp := Soldiers[j];
          Soldiers[j] := Soldiers[notbehind];
          Soldiers[notbehind] := temp;
          Inc(notbehind);
          Dec(TBattleUnit(temp.URef).Loses, 10);
        end;
        Dec(attempts);
      end;
    end;
    Dec(lv);
  end;
end;


{ TSimulation }

constructor TSimulation.Create(Battle: TBattle; AAddActions: boolean);
var i: integer;
    AtkList, DefList: TBattleUnitList;
begin
  Sim := Self;
  AddActions := AAddActions;
  FBattle := Battle;
  AtkList := Battle.Units[sideAttack];
  DefList := Battle.Units[sideDefence];
  // Refresh protection in defence structures
  for i := 0 to AtkList.Count-1 do
    if AtkList[i].Struct <> nil then
      AtkList[i].Struct.Protection := AtkList[i].Struct.Data.Protection;
  for i := 0 to DefList.Count-1 do
    if DefList[i].Struct <> nil then
      DefList[i].Struct.Protection := DefList[i].Struct.Data.Protection;
  // Create armies
  Armies[0] := TArmy.Create(AtkList, Battle.Region);
  Armies[1] := TArmy.Create(DefList, Battle.Region);
end;

destructor TSimulation.Destroy;
begin
  Armies[0].Free;
  Armies[1].Free;
end;

function TSimulation.Run: integer;
var round: integer;
begin
  // Free round for tactics
  if Armies[0].Tactics > Armies[1].Tactics then FreeRound(Armies[0], Armies[1]);
  if Armies[1].Tactics > Armies[0].Tactics then FreeRound(Armies[1], Armies[0]);

  // Run rounds
  round := 1;
  while not Armies[0].Broken and not Armies[1].Broken and (round <= 100) do begin
    if AddActions then DoAddRound(round);
    NormalRound(Armies[0], Armies[1]);
    Inc(round);
  end;

  // End battle
  Result := -1; // noone wins

  if (Armies[0].Broken and not Armies[1].Broken)
    or ((Armies[0].NumAlive = 0) and (Armies[1].NumAlive > 0)) then begin
    Armies[1].DoHeal;
    Result := 1; // defender wins
  end;

  if (Armies[1].Broken and not Armies[0].Broken)
    or ((Armies[1].NumAlive = 0) and (Armies[0].NumAlive > 0)) then begin
    Armies[0].DoHeal;
    Result := 0; // attacker wins
  end;
end;

procedure TSimulation.FreeRound(Atk, Def: TArmy);
var num: integer;
    Soldier: TSoldier;
    behind: boolean;
begin
  if AddActions then DoAddRound(0);
  SetLength(Atk.Shields, 0);
  Atk.UpdateShields;
  SetLength(Def.Shields, 0);
  Def.UpdateShields;

  Inc(Atk.Round);

  while (Atk.CanAttack > 0) and (Def.NumAlive > 0) do begin
    num := Random(Atk.CanAttack);
    Soldier := Atk.GetAttacker(num, behind);
    DoAttack(Atk.Round, Soldier, Atk, Def, behind);
  end;

  Def.Regenerate;
  Atk.Reset;
end;

procedure TSimulation.NormalRound(A, B: TArmy);
var aalive, balive, aatt, batt, num: integer;
    Soldier: TSoldier;
    behind: boolean;
begin
  A.UpdateShields;
  B.UpdateShields;

  Inc(A.Round);
  Inc(B.Round);

  // Run attacks until round done
  while True do begin
    aalive := A.NumAlive;
    balive := B.NumAlive;
    aatt := A.CanAttack;
    batt := B.CanAttack;

    // Exit if done :)
    if not ((aalive > 0) and (balive > 0) and ((aatt > 0) or (batt > 0))) then
      Break;

    num := Random(aatt + batt);
    if (num >= aatt) then begin
      Dec(num, aatt);
      Soldier := B.GetAttacker(num, behind);
      DoAttack(B.Round, Soldier, B, A, behind);
    end
    else begin
      Soldier := A.GetAttacker(num, behind);
      DoAttack(A.Round, Soldier, A, B, behind);
    end;
  end;

  A.Regenerate;
  B.Regenerate;
  A.Reset;
  B.Reset;
end;

procedure TSimulation.DoAttack(Round: integer; Soldier: TSoldier; Atk,
  Def: TArmy; behind: boolean);
var i, numAttacks, attackType, mountBonus, wpnClass: integer;
    WFlags: DWord;
begin
  // Special attacks
  if Soldier.Special <> nil then begin
    if AddActions then
      DoAddAction(Soldier.URef, raCast, Soldier.SpecLevel, Soldier.Special);
    DoSpecialAttack(Soldier.Special, Soldier.SpecLevel, Def);
  end;
  if Def.NumAlive = 0 then Exit;

  if (Soldier.Mount <> nil) and (Soldier.Mount.Special <> nil) then begin
    if AddActions then
      DoAddAction(Soldier.URef, raCast, Soldier.SpecLevel, Soldier.Special);
    DoSpecialAttack(Soldier.Mount.Special, Soldier.Mount.SpecLevel, Def);
  end;
  if Def.NumAlive = 0 then Exit;

  // NumAttacks
  numAttacks := Soldier.NumAttacks;
  if numAttacks < 0 then begin
    if Round mod (-1 * numAttacks) = 1 then numAttacks := 1
    else numAttacks := 0;
  end;

  for i := 1 to numAttacks do begin
    // Break if behind and can't attack
    if behind and ((Soldier.Weapon = nil)
      or not Test(Soldier.Weapon.Weapon.Flags, WPN_RANGED)) then
      Break;

    if Soldier.Weapon = nil then begin
      WFlags := WPN_SHORT;
      attackType := atMelee;
      mountBonus := 0;
      wpnClass := wcSlashing;
    end
    else begin
      WFlags := Soldier.Weapon.Weapon.Flags;
      attackType := Soldier.Weapon.Weapon.AttackType;
      mountBonus := Soldier.Weapon.Weapon.MountBonus;
      wpnClass := Soldier.Weapon.Weapon.WpnClass;
    end;
    Def.DoAnAttack(nil, 1, attackType, Soldier.Attack, WFlags, wpnClass,
      nil, mountBonus);
    if AddActions then DoAddAction(Soldier.URef, raMeleeAttack, 1, nil);
    if Def.NumAlive = 0 then Break;
  end;

  Soldier.ClearOneTimeEffects;
end;

procedure TSimulation.DoSpecialAttack(Sp: TSpecData; Level: integer;
  Def: TArmy);
var i, times: integer;
begin
  for i := 0 to Length(Sp.Attacks)-1 do begin
    // Get amount of special attacks
    times := Sp.Attacks[i].MaxAmt div 2;
    if Test(Sp.Flags, SP_USE_LEV) then
      times := times * Level;
    times := Sp.Attacks[i].MinAmt + Random(times) + Random(times);
    // Fire attacks
    Def.DoAnAttack(Sp, times, Sp.Attacks[i].AttackType, Level,
      Sp.Attacks[i].WeaponFlags, Sp.Attacks[i].WeaponClass,
      Sp.Attacks[i].Effect, 0);
  end;
end;

procedure TSimulation.Restart;
begin
  Armies[0].Restart(True, -1);
  Armies[1].Restart(True, -1);
end;

procedure TSimulation.DoAddAction(U: TBaseUnit; AType,
  APower: integer; ASpecial: TSpecData);
var i: integer;
begin
  with FBattle.Rounds[High(FBattle.Rounds)] do begin
    // Group TakeHits sequence actions: hit, kill, hit, (kill)
    if (AType = raTakeHits) and (Length(Actions) >= 3) then begin
      i := High(Actions);
      if (Actions[i-2].ActionType = raMeleeAttack)
        and (Actions[i-1].ActionType = raTakeHits)
        and (Actions[i].ActionType = raMeleeAttack)
        and (Actions[i-1].BUnit = U)
        and (Actions[i-2].BUnit = Actions[i].BUnit) then begin
        Inc(Actions[i-1].Power);
        Inc(Actions[i-2].Power);
        SetLength(Actions, Length(Actions) - 1);
        Exit;
      end;
    end;
    // Group other actions
    if Length(Actions) >= 3 then begin
      i := High(Actions);
      if (Actions[i].ActionType = AType) and (Actions[i].BUnit = U) then begin
        Inc(Actions[i].Power);
        Exit;
      end;
    end;
    // Add new action
    SetLength(Actions, Length(Actions) + 1);
    with Actions[High(Actions)] do begin
      BUnit := TBattleUnit(U);
      ActionType := AType;
      Power := APower;
      Special := ASpecial;
    end;
  end;
end;

procedure TSimulation.DoAddRound(Num: integer);
begin
  SetLength(FBattle.Rounds, Length(FBattle.Rounds) + 1);
  FBattle.Rounds[High(FBattle.Rounds)].Num := Num;
end;

end.
