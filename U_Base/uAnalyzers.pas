unit uAnalyzers;

{$MODE Delphi}

interface

uses
  SysUtils, Windows, DataStructs, uUnitRecs, uGameSubs, uKeys, MyStrings, Math;

  procedure RunTurnAnalyzers;

implementation

// Mark self mages by magic skills; detect other mages by event casts
//  and battles
procedure DetectMages;
var i, j, k: integer;
    side: boolean;
    AUnit: TUnit;
    BUnit: TBattleUnit;
    Facs: TFactionList;
    found, ismage: boolean;
    Battle: TBattle;
    SData: TSkillData;
    Skill: TSkill;
begin
  Facs := Turn.Factions;
  for i := 0 to Facs.Count-1 do
    if Facs[i].Player then begin
      // Mages of player's faction
      for j := 0 to Facs[i].Units.Count-1 do begin
        if SingleLeader(Facs[i].Units[j].Items) then begin
          k := Facs[i].Units[j].Skills.Count-1;
          while (k >= 0) and not Test(Facs[i].Units[j].Skills[k].Data.Flags,
            SK_MAGIC) do Dec(k);
          Facs[i].Units[j].Mage := (k >= 0);
        end;
      end;
    end
    else begin
      for j := 0 to Facs[i].Units.Count-1 do begin
        AUnit := Facs[i].Units[j];
        // Recorded mages
        AUnit.Mage := UnitRecs.Mage(AUnit.Num, AUnit.Region.Coords);
        // Look for casts in unit events
        if not AUnit.Mage then begin
          k := AUnit.Events.Count-1;
          while (k >= 0) and (Pos(Keys[s_Uses], AUnit.Events[k]) = 0) do
            Dec(k);
          if k >= 0 then AUnit.Mage := True;
        end;
      end;
    end;
  // Mages detected in battle
  for i := 0 to Turn.Events.Count-1 do
    if (Turn.Events.Objects[i] <> nil)
      and (Turn.Events.Objects[i].ClassType = TBattle) then begin
      Battle := TBattle(Turn.Events.Objects[i]);
      with Battle do begin
        // Scan units for spells casted
        for side := sideAttack to sideDefence do
          for j := 0 to Units[side].Count-1 do begin
            BUnit := Units[side][j];
            if BUnit.Faction.Player then begin
              if (BUnit.URef <> nil) and BUnit.URef.Mage then begin
                // Setup mage of player's faction
                BUnit.Mage := True;
                for k := 0 to BUnit.URef.Skills.Count-1 do begin
                  SData := BUnit.URef.Skills[k].Data;
                  if Test(SData.Flags, SK_COMBATSPELL)
                    or (SData.Short = Keys[s_MagicalHealing]) then begin
                    Skill := TSkill.Create;
                    Skill.Data := SData;
                    Skill.Level := BUnit.URef.Skills[k].Level;
                    BUnit.Skills.Add(Skill);
                  end;
                end;
                BUnit.CombatSpell := BUnit.URef.CombatSpell;
              end;
            end
            else begin
              if (BUnit.URef <> nil) and BUnit.URef.Mage then
                // Set mage if unit found in current turn
                BUnit.Mage := True
              else begin
                // If unit not found (killed or missed from other faction)
                ismage := False;
                // Check for existing record
                if UnitRecs.Mage(BUnit.Num, Battle.Region.Coords) then
                  ismage := True
                else begin
                  if (BUnit.SpellText <> '') and SingleLeader(BUnit.Items) then begin
                    // Check for battle items with same effect
                    found := False;
                    k := BUnit.Items.Count-1;
                    while (k >= 0) and not found do
                      with BUnit.Items[k].Data do begin
                        found := (Test(Flags, IT_MAGIC)
                          and (Special <> nil)
                          and (Pos(Special.SpellText, BUnit.SpellText) = 1));
                        Dec(k);
                      end;
                    ismage := (k < 0);
                  end;
                end;
                // Set mage
                if ismage then begin
                  BUnit.Mage := True;
                  if BUnit.URef <> nil then BUnit.URef.Mage := True;
                end;
              end;
            end;
          end;
      end;
    end;
end;

// Add 0-amount advanced resources if specialist not found it
procedure DetectAdvResources;
var i, j, k: integer;
    Res: TItemData;
    Item: TItem;
    Troop: TTroop;
    R: TRegion;
begin
  for k := 0 to Turn.Regions.Count-1 do begin
    R := Turn.Regions[k];
    // For each advanced resource available on terrain
    for i := 0 to R.Terrain.AdvResources.Count-1 do begin
      Res := R.Terrain.AdvResources[i];
      // If the resource not already recorded in region
      if (Res.Produce.Skill <> nil) and (R.Products.Find(Res.Short) = nil) then begin
        // Look for men able to produce (and see) it
        Troop := R.Troops.Find(Faction.Num);
        if Troop <> nil then begin
          j := Troop.Units.Count-1;
          while (j >= 0)
            and (Troop.Units[j].Skills.Find(Res.Produce.Skill.Data.Short) = nil) do
            Dec(j);
          if j >= 0 then begin
            // Add advanced resource in zero amount
            Item := TItem.Create;
            Item.Amount := 0;
            Item.Data := Res;
            R.Products.Add(Item);
          end;
        end;
      end;
    end;
    // Add advanced resource to terrain if found in region
    for i := 0 to R.Products.Count-1 do
      if (R.Products[i].Amount > 0)
        and Test(R.Products[i].Data.Flags, IT_ADVANCED)
        and (R.Terrain.AdvResources.IndexOf(R.Products[i].Data) = -1) then
        R.Terrain.AdvResources.Add(R.Products[i].Data);
  end;
end;

function GetMaxOut(U: TUnit; R: TRegion; Mode: integer; IData: TItemData): integer;
var maxout, turnout: integer;
begin
  Result := 0;
  case Mode of
    0:  Result := Taxers(U) * 50;
    1:  Result := R.Wages * U.Items.Amount(IT_MAN);
    2:  Result := EntertainOut(U);
    3:  if ProduceOut(U, R, IData, maxout, turnout, False) = prdOk then
          Result := turnout;
  end;
end;

function ProductionGap(Units: TUnitList; PrevR: TRegion; Value: integer; Key1, Key2: string;
  Mode: integer; IData: TItemData): real;
var i, j, total, maxout, output: integer;
    U: TUnit;
    Trace: TTrace;
    reg: string;
begin
  Result := 0;
  total := 0;
  maxout := 0;

  reg := '(' + IntToStr(PrevR.x) + ',' + IntToStr(PrevR.y);
  if Map.Levels[PrevR.z].Name <> Keys[s_Surface] then
    reg := reg +',' + Map.Levels[PrevR.z].Name;
  reg := reg + ')';

  for i := 0 to Units.Count-1 do begin
    U := Units[i];
    j := U.Events.Count-1;
    while (j >= 0) and not ((Pos(Key1, U.Events[j]) > 0)
      and (Pos(Key2, U.Events[j]) > 0)
      and (Pos(reg, U.Events[j]) > 0)) do Dec(j);
    if j >= 0 then begin
      Trace := TTrace.Create(U.Events[j]);
      // Earns 284 silver entertaining in ...
      Trace.Before(Key1);
      try
        Inc(total, Trace.Num);
      except
      end;
      Inc(maxout, GetMaxOut(U, PrevR, Mode, IData));
      Trace.Free;
    end;
  end;
  output := Min(Value, maxout);
  if (total = 0) or (output = 0) or (output = total) then Exit;

  Result := Max(0, maxout / total * (Value - total));
end;

// SpyReports Economical section
procedure DetectOtherProduction;
var PrevTurn: TTurn;
    R, PrevR: TRegion;
    i, j, x: integer;
begin
  PrevTurn := Game.Turns[Game.Turns.IndexOf(Turn) - 1];
  for i := 0 to Turn.Regions.Count-1 do begin
    R := Turn.Regions[i];
    if R.PlayerTroop = nil then Continue;
    PrevR := Map.Region(R.Coords, PrevTurn.Num);
    if PrevR = nil then Continue;

    R.OtherFactions.Taxers := Round(ProductionGap(Faction.Units, PrevR,
      PrevR.TaxRate, Keys[s_Collects], Keys[s_Taxing], 0, nil) / 50);
    if PrevR.Wages > 0 then
      R.OtherFactions.Workers := Round(ProductionGap(R.PlayerTroop.Units,
        PrevR, PrevR.MaxWages,
        Keys[s_Earns], Keys[s_Working], 1, nil) / PrevR.Wages);
    R.OtherFactions.Entertainers := Round(ProductionGap(R.PlayerTroop.Units,
      PrevR, PrevR.Entertainment,
      Keys[s_Earns], Keys[s_Entertaining], 2, nil) / 20);

    for j := 0 to PrevR.Products.Count-1 do begin
      x := Round(ProductionGap(R.PlayerTroop.Units, PrevR, PrevR.Products[j].Amount,
        Keys[s_Produces], PrevR.Products[j].Data.Short, 3, PrevR.Products[j].Data));
      if x > 0 then begin
        if R.OtherFactions.Products <> '' then
          R.OtherFactions.Products := R.OtherFactions.Products + ', ';
        R.OtherFactions.Products := R.OtherFactions.Products +
          (PrevR.Products[j].Data.Name(x <> 1) + ' at ' +
          IntToStr(x) + ' levels');
      end;
    end;
  end;
end;

procedure RunTurnAnalyzers;
begin
  DetectMages;
  DetectAdvResources;
  if Game.Turns.IndexOf(Turn) > 1 then DetectOtherProduction;
end;

end.
