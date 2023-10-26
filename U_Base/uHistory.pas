unit uHistory;

interface

uses
  Classes, SysUtils, DataStructs, MyStrings, Resources, Math, uGameSubs;

type
  EHistoryError = class(Exception);

const
  HistoryVersion = 'Prediction 3.18.4';
  BasicRuleset = False;

  procedure ReadGameHistory(Lines: TStrings);
  procedure WriteGameHistory(Lines: TStrings);
  procedure WriteRuleset(Filename: string);
  procedure MergeWithHistory(ATurn: TTurn);

implementation

type
  TStringParser = function(s: string): pointer;
  TStringWriter = function(obj: pointer): string;

function ItemById(List: TList; Index: integer): pointer;
var i: integer;
begin
  for i := List.Count to Index do List.Add(nil);
  Result := List[Index];
end;

function GetItem(List: TList; s: string): pointer;
var Index: integer;
begin
  if s = '' then Result := nil
  else begin
    Index := StrToInt(s);
    if (Index >= 0) and (Index < List.Count) then Result := List[Index]
    else Result := nil;
  end;
end;

procedure ReadList(Trace: TTrace; List: TList; Parser: TStringParser);
begin
  if Pos('.', Trace.Text) = 1 then Trace.Block
  else while Trace.Separator <> '.' do
    List.Add(Parser(Trace.Block));
  List.Pack;
end;

function WriteList(List: TList; Writer: TStringWriter): string;
var i: integer;
    s: string;
begin
  Result := '';
  for i := 0 to List.Count-1 do begin
    s := Writer(List[i]);
    if (Result <> '') and (s <> '') then Result := Result + ', ';
    Result := Result + s;
  end;
  Result := Result + '.';
end;


{ Reading transformation functions }

function StrToItemData(s: string): TItemData;
var id: integer;
begin
  Result := nil;
  if s = '' then Exit;
  id := StrToInt(s);
  Result := ItemById(Game.ItemData, id);
  if Result = nil then begin
    Result := TItemData.Create('');
    Game.ItemData[id] := Result;
  end;
end;

function ItemDataRead(s: string): pointer;
begin
  Result := StrToItemData(s);
end;

function ItemDataGet(s: string): pointer;
begin
  Result := GetItem(Game.ItemData, s);
end;

function StrToItem(s: string; Seek: boolean): TItem; overload;
var Trace: TTrace;
    IData: TItemData;
begin
  Result := nil;
  Trace := TTrace.Create(s);
  // data amount [cost]
  if Seek then IData := StrToItemData(Trace.Before(' '))
  else IData := ItemDataGet(Trace.Before(' '));
  if IData = nil then Exit;
  Result := TItem.Create;
  Result.Data := IData;
  Result.Amount := Trace.Num;
  if not Trace.Ends then begin
    Trace.Before(' ');
    Result.Cost := Trace.Num;
  end;
  Trace.Free;
end;

function StrToItem(s: string): TItem; overload;
begin
  Result := StrToItem(s, True);
end;

function ItemGet(s: string): pointer;
begin
  Result := StrToItem(s, False);
end;

function ItemRead(s: string): pointer;
begin
  Result := StrToItem(s);
end;

function StrToSkillData(s: string): TSkillData;
var id: integer;
begin
  Result := nil;
  if s = '' then Exit;
  id := StrToInt(s);
  Result := ItemById(Game.SkillData, id);
  if Result = nil then begin
    Result := TSkillData.Create('');
    Game.SkillData[id] := Result;
  end;
end;

function StrToSkill(s: string; Seek: boolean): TSkill; overload;
var SData: TSkillData;
    Trace: TTrace;
begin
  Result := nil;
  Trace := TTrace.Create(s);
  // data level
  if Seek then SData := StrToSkillData(Trace.Before(' '))
  else SData := GetItem(Game.SkillData, Trace.Before(' '));
  if SData = nil then Exit;
  Result := TSkill.Create;
  Result.Data := SData;
  Result.Level := Trace.Num;
  Trace.Free;
end;

function StrToSkill(s: string): TSkill; overload;
begin
  Result := StrToSkill(s, True);
end;

function SkillRead(s: string): pointer;
begin
  Result := StrToSkill(s);
end;

function StrToStructData(s: string): TStructData;
var id: integer;
begin
  Result := nil;
  if s = '' then Exit;
  id := StrToInt(s);
  Result := ItemById(Game.StructData, id);
  if Result = nil then begin
    Result := TStructData.Create('');
    Game.StructData[id] := Result;
  end;
end;

function StructDataGet(s: string): pointer;
begin
  Result := GetItem(Game.StructData, s);
end;

procedure ReadStructList(Trace: TTrace; List: TStructList);
var StData: TStructData;
    Struct: TStruct;
begin
  List.ClearItems;
  if Pos('.', Trace.Text) = 1 then Trace.Block
  else while Trace.Separator <> '.' do begin
    // 1 1 "Shaft"; 33 4 1
    StData := GetItem(Game.StructData, Trace.Before(' '));
    if StData <> nil then begin
      Struct := TStruct.Create;
      Struct.Data := StData;
      Struct.Num := Trace.NumSpace;
      Struct.Name := Trace.QBlock;
      if Trace.Separator = ';' then
        Struct.Passage := StrToCoords(Trace.Block);
      List.Add(Struct);
    end
    else begin
      Trace.Block;
      if Trace.Separator = ';' then Trace.Block;
    end;
  end;
end;

function StrToSpecData(s: string): TSpecData;
var id: integer;
begin
  Result := nil;
  if s = '' then Exit;
  id := StrToInt(s);
  Result := ItemById(Game.SpecData, id);
  if Result = nil then begin
    Result := TSpecData.Create('');
    Game.SpecData[id] := Result;
  end;
end;

function StrToEffectData(s: string): TEffectData;
var id: integer;
begin
  Result := nil;
  if s = '' then Exit;
  id := StrToInt(s);
  Result := ItemById(Game.EffectData, id);
  if Result = nil then begin
    Result := TEffectData.Create;
    Game.EffectData[id] := Result;
  end;
end;

function EffectDataGet(s: string): pointer;
begin
  Result := GetItem(Game.EffectData, s);
end;


function StrToTerrainData(s: string): TTerrainData;
var id: integer;
begin
  Result := nil;
  if s = '' then Exit;
  id := StrToInt(s);
  Result := ItemById(Game.TerrainData, id);
  if Result = nil then begin
    Result := TTerrainData.Create('');
    Game.TerrainData[id] := Result;
  end;
end;


{ Saving transfomation functions }

function SkillDataToStr(SData: TSkillData): string;
begin
  if (SData = nil) or (SData.Short = '') then Result := ''
  else Result := IntToStr(Game.SkillData.IndexOf(SData));
end;

function SkillToStr(Skill: TSkill): string;
begin
  if Skill <> nil then
    Result := SkillDataToStr(Skill.Data) + ' ' + IntToStr(Skill.Level)
  else Result := '';
end;

function StructDataToStr(StData: TStructData): string;
begin
  if (StData = nil) or (StData.Group = '') then Result := ''
  else Result := IntToStr(Game.StructData.IndexOf(StData))
end;

function StructToStr(Struct: TStruct): string;
begin
  FmtStr(Result, '%d %d "%s"', [Game.StructData.IndexOf(Struct.Data),
    Struct.Num, Struct.Name]);
  if Struct.HasExit then
    Result := Result + '; ' + CoordsToStr(Struct.Passage);
end;

function ItemDataToStr(IData: TItemData): string;
begin
  if (IData = nil) or (IData.Short = '') then Result := ''
  else Result := IntToStr(Game.ItemData.IndexOf(IData))
end;

function ItemDataWrite(obj: pointer): string;
begin
  Result := ItemDataToStr(obj);
end;

function ItemToStr(Item: TItem; UseCost: boolean): string;
begin
  if Item <> nil then begin
    Result := ItemDataToStr(Item.Data) + ' ' + IntToStr(Item.Amount);
    if UseCost then Result := Result + ' ' + IntToStr(Item.Cost);
  end
  else Result := '';
end;

function ItemWrite(obj: pointer): string;
begin
  Result := ItemToStr(obj, False);
end;

function ItemCostWrite(obj: pointer): string;
begin
  Result := ItemToStr(obj, True);
end;

function SpecDataToStr(Spec: TSpecData): string;
begin
  if Spec <> nil then Result := IntToStr(Game.SpecData.IndexOf(Spec))
  else Result := '';
end;

function EffectDataToStr(Eff: TEffectData): string;
begin
  if Eff <> nil then Result := IntToStr(Game.EffectData.IndexOf(Eff))
  else Result := '';
end;


 { ---------- Save datas ----------- }

procedure SaveConfig(Lines: TStrings);
var i: integer;
    Keys: TStrings;
begin
  Lines.Add('[Config]');
  Keys := TStringList.Create;
  GameConfig.ReadSection('Settings', Keys);
  for i := 0 to Keys.Count-1 do
    Lines.Add('"Settings", ' + Keys[i] + '=' +
      GameConfig.ReadString('Settings', Keys[i], ''));
  Lines.Add('');
end;

procedure SaveProgress(Lines: TStrings);
var i, j: integer;
    A: array of string;
begin
  Lines.Add('[Progress]');
  for i := 0 to prCount-1 do begin
    SetLength(A, Length(Progress[i]));
    for j := 0 to High(Progress[i]) do A[j] := IntToStr(Progress[i][j]);
    Lines.Add(ProgressNames[i] + ': ' + MakeList(A));
  end;
  Lines.Add('');
end;

procedure SaveFactions(Lines: TStrings);
var t, i, j, color: integer;
    s: string;
    FList: TFactionList;
begin
  Lines.Add('[Factions]');
  // Build list of known factions
  FList := TFactionList.Create;
  FList.Add(History.Factions[0]);
  FList.Add(History.Factions[1]);
  for t := Game.Turns.Count-1 downto 0 do
    with Game.Turns[t] do
      for i := 0 to Factions.Count-1 do begin
        j := 0;
        while (j < FList.Count) and (Factions[i].Num <> FList[j].Num) do Inc(j);
        if (j = FList.Count) and ((Factions[i].Name <> '') or (Factions[i].Num = 0))
          then FList.Add(Factions[i]);
      end;
  // Save list
  for i := 0 to FList.Count-1 do
    with FList[i] do begin
      if Data.ColorIndex > 0 then
        color := TColorExtra(ColorExtras[Data.ColorIndex]).Color
      else color := 0;
      FmtStr(s, '%d, "%s", %d, "%s"', [Num, Name, color, Data.Email]);
      Lines.Add(s);
    end;
  FList.Free;
  Lines.Add('');
end;

procedure SaveItems(Lines: TStrings; Basic: boolean);
var i, j: integer;
    s, desc: string;
    A: array of string;
begin
  Lines.Add('[Items]');
  for i := 0 to Game.ItemData.Count-1 do
    with Game.ItemData[i] do begin
      if Short = '' then Continue;

      if Basic then desc := ''
      else desc := Description;
      FmtStr(s, '%d=%s, "%s", "%s", "%s", %d, %d, %d, %d, %d, %d, %d.',
        [i, Short, SingleName, MultiName, desc, Flags, Weight,
        Moves[mtWalk], Moves[mtRide], Moves[mtFly], Moves[mtSwim],
        BoolToInt(Incomplete)]);

      if not Basic or not Test(Flags, IT_ADVANCED) then begin
        // Produce
        with Produce do begin
          FmtStr(s, '%s Prd: %s, %d, %d, %s, %d, %s.', [s, SkillToStr(Skill),
            Rate, ManMonths, ItemDataToStr(Tool), ToolBonus,
            StructDataToStr(RequiredStruct)]);
          if Produce.Materials.Count > 0 then
            s := s + ' PInp: ' + WriteList(Produce.Materials, ItemWrite);
          if Produce.Byproducts.Count > 0 then
            s := s + ' Byprod: ' + WriteList(Produce.Byproducts, ItemWrite);
        end;
        with MagProduce do begin
          FmtStr(s, '%s MagPrd: %s.', [s, SkillToStr(Skill)]);
          if MagProduce.Materials.Count > 0 then
            s := s + ' MInp: ' + WriteList(MagProduce.Materials, ItemWrite);
        end;

        // Man
        if Test(Flags, IT_MAN) then begin
          FmtStr(s, '%s Man: %d, %d, %d.', [s, BoolToInt(Man.Leader),
            Man.MagDefLevel, Man.DefLevel]);
          if Man.SpecSkills.Count > 0 then begin
            SetLength(A, Man.SpecSkills.Count);
            for j := 0 to Man.SpecSkills.Count-1 do
              A[j] := SkillToStr(Man.SpecSkills[j]);
            s := s + ' ManSpec: ' + MakeList(A);
          end;
        end;

        // Monster
        if Test(Flags, IT_MONSTER) then
          with Monster do begin
            FmtStr(s, '%s Mons: %d, %d, %d, %d, %d',
              [s, Hits, NumAttacks, Regen, Tactics, Attack]);
            for j := 0 to atCount-1 do s := s + ', ' + IntToStr(Defence[j]);
            s := s + '.';
          end;

        // Weapon
        if Test(Flags, IT_WEAPON) then
          with Weapon do begin
            FmtStr(s, '%s Wpn: %d, %d, %d, %s, %s, %d, %d, %d, %d.', [s, Flags,
              WpnClass, AttackType, SkillDataToStr(Skill1), SkillDataToStr(Skill2),
              AttackBonus, DefenceBonus, MountBonus, NumAttacks]);
          end;

        // Armor
        if Test(Flags, IT_ARMOR) then
          with Armor do begin
            FmtStr(s, '%s Arm: %d', [s, Flags]);
            for j := 0 to wcCount-1 do s := s + ', ' + IntToStr(Defence[j]);
            s := s + '.';
          end;

        // Mount
        if Test(Flags, IT_MOUNT) then
          with Mount do begin
            FmtStr(s, '%s Mount: %s, %d, %d, %d.', [s, SkillDataToStr(RideSkill),
              MinBonus, MaxBonus, MaxHamperedBonus]);
          end;

        // Wagon
        if Test(Flags, IT_WAGON) then
          with Wagon do begin
            FmtStr(s, '%s Wagon: %d, %s.', [s, HitchWalk, ItemDataToStr(Hitched)]);
          end;

        // Battle item
        if Test(Flags, IT_MAGIC) then
          FmtStr(s, '%s Magic: %d.', [s, BoolToInt(Magic.MageOnly)]);

        // Special
        if Special <> nil then
          FmtStr(s, '%s Spec: %s, %d.', [s, SpecDataToStr(Special), SpecLevel]);

        if Test(Flags, IT_MAN) then
          FmtStr(s, '%s UpkeepSilver: %d.', [s, Upkeep.Silver]);

        if Test(Flags, IT_FOOD) then
        begin
          FmtStr(s, '%s FoodValue: %d.', [s, Food.Value]);
          FmtStr(s, '%s FoodOrder: %d.', [s, Food.Order]);
        end;
      end;

      Lines.Add(s);
    end;
  Lines.Add('');
end;

procedure SaveSkills(Lines: TStrings; Basic: boolean);
var i, j: integer;
    s: string;
    A: array of string;
begin
  Lines.Add('[Skills]');
  for i := 0 to Game.SkillData.Count-1 do
    with Game.SkillData[i] do begin
      if Short = '' then Continue;

      FmtStr(s, '%d=%s, "%s", %d, %d, %d.', [i, Short, Name, Flags, Cost,
        BoolToInt(Incomplete)]);

      // Descriptions
      SetLength(A, 5);
      for j := 1 to 5 do
        if Basic then A[j-1] := '""'
        else A[j-1] := '"' + Descriptions[j] + '"';
      s := s + ' Desc: ' + MakeList(A);

      if not Basic then begin
        // BasedOn
        SetLength(A, BasedOn.Count);
        for j := 0 to BasedOn.Count-1 do A[j] := SkillToStr(BasedOn[j]);
        s := s + ' Based: ' + MakeList(A);

        // Special
        if Special <> nil then
          FmtStr(s, '%s Spec: %s.', [s, SpecDataToStr(Special)]);
      end;

      Lines.Add(s);
    end;
  Lines.Add('');
end;

procedure SaveStructs(Lines: TStrings; Basic: boolean);
var i, j: integer;
    s, desc: string;
    A: array of string;
begin
  Lines.Add('[Structs]');
  for i := 0 to Game.StructData.Count-1 do
    with Game.StructData[i] do begin
      if Group = '' then Continue;

      if Basic then desc := ''
      else desc := Description;
      FmtStr(s, '%d="%s", %d, %d, %d, %d, %d, %s, %s, %s, %s, %d, %s, %d, "%s".',
        [i, Group, Flags, Size, Protection, Capacity, Sailors,
        ItemDataToStr(Material1), ItemDataToStr(Material2),
        ItemDataToStr(Resource), SkillToStr(BuildSkill), BoolToInt(Incomplete),
        ItemDataToStr(Tool), ToolBonus, desc]);

      if not Basic then begin
        SetLength(A, atCount);
        for j := 0 to atCount-1 do A[j] := IntToStr(Defence[j]);
        s := s + ' Defence: ' + MakeList(A);
      end;

      Lines.Add(s);
    end;
  Lines.Add('');
end;

procedure SaveTerrain(Lines: TStrings);
var i, j: integer;
    s: string;
    A: array of string;
begin
  Lines.Add('[Terrain]');
  for i := 0 to Game.TerrainData.Count-1 do
    with Game.TerrainData[i] do begin
      FmtStr(s, '%d="%s", %d, %d.', [i, Name, Flags, MoveCost]);

      if AdvResources.Count > 0 then begin
        SetLength(A, AdvResources.Count);
        for j := 0 to AdvResources.Count-1 do
          A[j] := ItemDataToStr(AdvResources[j]);
        s := s + ' Res: ' + MakeList(A);
      end;
      Lines.Add(s);
    end;
  Lines.Add('');
end;

procedure SaveWeather(Lines: TStrings);
var i: integer;
    s: string;
begin
  Lines.Add('[Weather]');
  for i := 0 to Game.WeatherData.Count-1 do
    with Game.WeatherData[i] do begin
      FmtStr(s, '%d="%s", "%s", "%s", %d', [i, Text, LastText, NextText, MoveCost]);
      Lines.Add(s);
    end;
  Lines.Add('');
end;

procedure SaveWeatherMonths(Lines: TStrings);
const Keys: array[0..2] of string = ('North', 'Tropical', 'South');
var i, j: integer;
    s: string;
begin
  Lines.Add('[WeatherMonths]');
  for i := 0 to 2 do begin
    s := Keys[i] + ': ';
    for j := 1 to 12 do begin
      if j > 1 then s := s + ', ';
      s := s + IntToStr(Game.WeatherData.IndexOf(WeatherMonths[i, j]));
    end;
    Lines.Add(s);
  end;
  Lines.Add('');
end;

procedure SaveEffects(Lines: TStrings);
var i, j: integer;
    s: string;
begin
  Lines.Add('[Effects]');
  for i := 0 to Game.EffectData.Count-1 do
    with Game.EffectData[i] do begin
      FmtStr(s, '%d="%s", %d, %d', [i, Name, Flags, Attack]);
      for j := 0 to atCount-1 do
        s := s + ', ' + IntToStr(Defence[j]);
      Lines.Add(s);
    end;
  Lines.Add('');
end;

procedure SaveSpecials(Lines: TStrings);
var i, j: integer;
    s: string;
begin
  Lines.Add('[Specials]');
  for i := 0 to Game.SpecData.Count-1 do
    with Game.SpecData[i] do begin
      FmtStr(s, '%d="%s", %d, "%s", "%s", %d.',
        [i, Name, Flags, Description, SpellText, BoolToInt(Incomplete)]);
      if Test(Flags, SP_SHIELD) then begin
        s := s + ' Shields: ';
        for j := 0 to atCount-1 do begin
          if j > 0 then s := s + ', ';
          s := s + BoolToStr0(Shields[j]);
        end;
        s := s + '.';
      end;
      if Length(Attacks) > 0 then begin
        s := s + ' Atk: ';
        for j := 0 to Length(Attacks)-1 do begin
          if j > 0 then s := s + '; ';
          FmtStr(s, '%s%d, %d, %d, %d, %d, %s',
            [s, Attacks[j].WeaponFlags, Attacks[j].AttackType,
            Attacks[j].MinAmt, Attacks[j].MaxAmt, Attacks[j].WeaponClass,
            EffectDataToStr(Attacks[j].Effect)]);
        end;
        s := s + '.';
      end;
      if Length(Defences) > 0 then begin
        s := s + ' Def: ';
        for j := 0 to Length(Defences)-1 do begin
          if j > 0 then s := s + '; ';
          FmtStr(s, '%s %d, %d',
            [s, Defences[j].AttackType, Defences[j].Bonus]);
        end;
        s := s + '.';
      end;
      if Items.Count > 0 then begin
        s := s + ' Items: ';
        for j := 0 to Items.Count-1 do begin
          if j > 0 then s := s + ', ';
          s := s + ItemDataToStr(Items[j]);
        end;
        s := s + '.';
      end;
      if Structs.Count > 0 then begin
        s := s + ' Structs: ';
        for j := 0 to Structs.Count-1 do begin
          if j > 0 then s := s + ', ';
          s := s + IntToStr(Game.StructData.IndexOf(Structs[j]));
        end;
        s := s + '.';
      end;
      if Effects.Count > 0 then begin
        s := s + ' Effects: ';
        for j := 0 to Effects.Count-1 do begin
          if j > 0 then s := s + ', ';
          s := s + EffectDataToStr(Effects[j]);
        end;
        s := s + '.';
      end;
      Lines.Add(s);
    end;
  Lines.Add('');
end;

procedure SaveMap(Lines: TStrings);
var i, x, y, z: integer;
    R: TRegion;

  procedure SaveRegion(R: TRegion);
  var j, own, ter: integer;
      s, s1, note: string;
      A: array of string;
  begin
    if R = nil then Exit;
    ter := Max(0, Game.TerrainData.IndexOf(R.Terrain));
    note := StringReplace(R.Notes.Text, #13#10, '<br>', [rfReplaceAll]);
    note := StringReplace(note, '"', '&quot;', [rfReplaceAll]);
    FmtStr(s, '%s=%d, %d, "%s", "%s", "%s", %d',
      [CoordsToStr(R.Coords), R.Visited, ter, R.Land, note, R.Settlement,
        R.SettlementType]);
    if R.FullData then begin
      if R.Guard <> nil then own := R.Guard.Num
      else own := 0;
      FmtStr(s, '%s, %d, %d, %d, %d, %d, %d, %s.',
        [s, R.TaxRate, R.Wages, R.MaxWages, R.Entertainment, R.Gate, own,
        ItemToStr(R.Peasants, False)]);

      SetLength(A, 6);
      for j := 1 to 6 do
        if R.HasExit[j] then A[j-1] := '1'
        else A[j-1] := '0';
      s := s + ' Exits: ' + MakeList(A);

      s := s + ' Wanted: ' + WriteList(R.Wanted, ItemCostWrite);
      s := s + ' ForSale: ' + WriteList(R.ForSale, ItemCostWrite);
      s := s + ' Products: ' + WriteList(R.Products, ItemWrite);

      s1 := '';
      for j := 0 to R.Structs.Count-1 do
        if not Test(R.Structs[j].Data.Flags, ST_TRANSPORT) then begin
          if s1 <> '' then s1 := s1 + ', ';
          s1 := s1 + StructToStr(R.Structs[j]);
        end;
      s := s + ' Structs: ' + s1 + '.';
    end;
    Lines.Add(s);
  end;

begin
  Lines.Add('[MapLevels]');
  for i := 0 to Map.Levels.Count-1 do
    if Map.Levels[i].Name <> '' then
      Lines.Add(IntToStr(i) + '=' + Map.Levels[i].Name);
  Lines.Add('');

  Lines.Add('[Map]');
  for z := 0 to Map.Levels.Count-1 do begin
    if IsNexus(z) then Continue;
    for y := Map.Levels[z].Bounds.Top to Map.Levels[z].Bounds.Bottom do
      for x := Map.Levels[z].Bounds.Left to Map.Levels[z].Bounds.Right do begin
        // Save archived regions
        R := Map.Region(x, y, z, Game.Turns[1].Num);
        if (R <> nil) and ((R.Visited <= Game.Turns[1].Num)
          or (R.Visited > Game.Turns[Game.Turns.Count-1].Num)) then
          SaveRegion(R);
        // Save regions from loaded turns
        for i := 2 to Game.Turns.Count-1 do begin
          R := Map.Region(x, y, z, Game.Turns[i].Num);
          if (R <> nil) and (R.Visited = Game.Turns[i].Num) then
            SaveRegion(R);
        end;
      end;
  end;
  Lines.Add('');
end;

procedure SaveUArmies(Lines: TStrings);
var i, j: integer;
    s: string;
begin
  Lines.Add('[Armies]');
  for i := 0 to Game.UArmies.Count-1 do
    with Game.UArmies[i] do begin
      FmtStr(s, '"%s", %d. Units: ', [Name, Color]);
      for j := 0 to UnitIds.Count-1 do begin
        if j > 0 then s := s + ', ';
        s := s + UnitIds[j];
      end;
      Lines.Add(s);
    end;
  Lines.Add('');
end;


 { ---------- Read datas --------- }

procedure ConfigParser(Trace: TTrace);
var section, key, value: string;
begin
  section := Trace.QBlock;
  key := Trace.Before('=');
  value := Trace.Text;
  GameConfig.WriteString(section, key, value);
end;

procedure ProgressParser(Trace: TTrace);
var Key: string;
    i, j: integer;
begin
  Key := Trace.Before(': ');
  i := prCount-1;
  while (i >= 0) and (Key <> ProgressNames[i]) do Dec(i);
  if i < 0 then Exit;

  j := 0;
  while not Trace.Ends do begin
    SetProgress(i, j, StrToInt(Trace.Block));
    Inc(j);
  end;
end;

procedure FactionParser(Trace: TTrace);
var c, num: integer;
    Fac: TFaction;
begin
  num := StrToInt(Trace.Block);
  if (num > 0) and (History.Factions[1].Num = 0) then begin
    // Init active faction
    Fac := History.Factions[1];
    Fac.Num := Num;
  end
  else Fac := History.Factions.Seek(num);
  with Fac do begin
    Name := Trace.QBlock;
    Data := Game.FactionData.Seek(Num);
    c := StrToInt(Trace.QBlock);
    if c <> 0 then Data.ColorIndex := SeekColorExtra(c);
    Data.Email := Trace.QBlock;
  end;
end;

procedure ItemDataParser(Trace: TTrace);
var i, id: integer;
    IData: TItemData;
begin
  id := StrToInt(Trace.Before('='));
  IData := TItemData(ItemById(Game.ItemData, id));
  if IData = nil then begin
    IData := TItemData.Create('');
    Game.ItemData[id] := IData;
  end;
  with IData do begin
    Short := Trace.Block;
    SingleName := Trace.QBlock;
    MultiName := Trace.QBlock;
    Description := Trace.QBlock;
    Flags := StrToInt(Trace.Block);
    Weight := StrToInt(Trace.Block);
    for i := 1 to 4 do Moves[i] := StrToInt(Trace.Block);
    Incomplete := StrToBool(Trace.Block);

    // Production
    if Pos('Prd: ', Trace.Text) > 0 then
      with Produce do begin
        Trace.Before('Prd: ');
        Skill := StrToSkill(Trace.Block);
        Rate := StrToInt(Trace.Block);
        ManMonths := StrToInt(Trace.Block);
        Tool := StrToItemData(Trace.Block);
        ToolBonus := StrToInt(Trace.Block);
        RequiredStruct := StrToStructData(Trace.Block);
      end;

    if Pos('PInp: ', Trace.Text) > 0 then begin
      Trace.Before('PInp: ');
      ReadList(Trace, Produce.Materials, ItemRead);
    end;

    if Pos('Byprod: ', Trace.Text) > 0 then begin
      Trace.Before('Byprod: ');
      ReadList(Trace, Produce.Byproducts, ItemRead);
    end;

    if Pos('MagPrd: ', Trace.Text) > 0 then
      with MagProduce do begin
        Trace.Before('MagPrd: ');
        Skill := StrToSkill(Trace.Block);
      end;

    if Pos('MInp: ', Trace.Text) > 0 then begin
      Trace.Before('MInp: ');
      ReadList(Trace, MagProduce.Materials, ItemRead);
    end;

    if Pos('Man: ', Trace.Text) > 0 then
      with Man do begin
        Trace.Before('Man: ');
        Leader := StrToBool(Trace.Block);
        MagDefLevel := StrToInt(Trace.Block);
        DefLevel := StrToInt(Trace.Block);
      end;

    if Pos('ManSpec: ', Trace.Text) > 0 then begin
      Trace.Before('ManSpec: ');
      ReadList(Trace, Man.SpecSkills, SkillRead);
    end;

    if Pos('Mons: ', Trace.Text) > 0 then
      with Monster do begin
        Trace.Before('Mons: ');
        Monster.Hits := StrToInt(Trace.Block);
        Monster.NumAttacks := StrToInt(Trace.Block);
        Monster.Regen := StrToInt(Trace.Block);
        Monster.Tactics := StrToInt(Trace.Block);
        Monster.Attack := StrToInt(Trace.Block);
        for i := 0 to atCount-1 do Defence[i] := StrToInt(Trace.Block);
      end;

    if Pos('Wpn: ', Trace.Text) > 0 then
      with Weapon do begin
        Trace.Before('Wpn: ');
        Flags := StrToInt(Trace.Block);
        WpnClass := StrToInt(Trace.Block);
        AttackType := StrToInt(Trace.Block);
        Weapon.Skill1 := StrToSkillData(Trace.Block);
        Weapon.Skill2 := StrToSkillData(Trace.Block);
        Weapon.AttackBonus := StrToInt(Trace.Block);
        Weapon.DefenceBonus := StrToInt(Trace.Block);
        Weapon.MountBonus := StrToInt(Trace.Block);
        Weapon.NumAttacks := StrToInt(Trace.Block);
      end;

    if Pos('Arm: ', Trace.Text) > 0 then
      with Armor do begin
        Trace.Before('Arm: ');
        Flags := StrToInt(Trace.QBlock);
        for i := 0 to wcCount-1 do Defence[i] := StrToInt(Trace.QBlock);
      end;

    if Pos('Mount: ', Trace.Text) > 0 then
      with Mount do begin
        Trace.Before('Mount: ');
        RideSkill := StrToSkillData(Trace.Block);
        MinBonus := StrToInt(Trace.QBlock);
        MaxBonus := StrToInt(Trace.QBlock);
        MaxHamperedBonus := StrToInt(Trace.QBlock);
      end;

    if Pos('Wagon: ', Trace.Text) > 0 then
      with Wagon do begin
        Trace.Before('Wagon: ');
        HitchWalk := StrToInt(Trace.Block);
        Hitched := StrToItemData(Trace.Block);
      end;

    if Pos('Magic', Trace.Text) > 0 then begin
      Trace.Before('Magic: ');
      Magic.MageOnly := StrToBool(Trace.QBlock);
    end;

    if Pos('Spec', Trace.Text) > 0 then begin
      Trace.Before('Spec: ');
      Special := StrToSpecData(Trace.Block);
      SpecLevel := StrToInt(Trace.Block);
    end;

    if Pos('UpkeepSilver', Trace.Text) > 0 then
    begin
      Trace.Before('UpkeepSilver: ');
      Upkeep.Silver := Trace.Num;
    end;

    if Pos('FoodValue', Trace.Text) > 0 then
    begin
      Trace.Before('FoodValue: ');
      Food.Value := Trace.Num;
    end;

    if Pos('FoodOrder', Trace.Text) > 0 then
    begin
      Trace.Before('FoodOrder: ');
      Food.Order := Trace.Num;
    end;
  end;
end;

procedure SkillDataParser(Trace: TTrace);
var i, id: integer;
    SData: TSkillData;
begin
  id := StrToInt(Trace.Before('='));
  SData := TSkillData(ItemById(Game.SkillData, id));
  if SData = nil then begin
    SData := TSkillData.Create('');
    Game.SkillData[id] := SData;
  end;
  with SData do begin
    Short := Trace.Block;
    Name := Trace.QBlock;
    Flags := StrToInt(Trace.Block);
    Cost := StrToInt(Trace.Block);
    Incomplete := StrToBool(Trace.Block);
    Trace.Before('Desc: ');
    for i := 1 to 5 do Descriptions[i] := Trace.QBlock;
    Trace.Before('Based: ');
    while (Pos('.', Trace.Text) > 1) and (Trace.Separator <> '.') do
      BasedOn.Add(StrToSkill(Trace.Block));
    if Pos('Spec', Trace.Text) > 0 then begin
      Trace.Before('Spec: ');
      Special := StrToSpecData(Trace.Block);
    end;
  end;
end;

procedure StructDataParser(Trace: TTrace);
var i, id: integer;
    StData: TStructData;
begin
  id := StrToInt(Trace.Before('='));
  StData := TStructData(ItemById(Game.StructData, id));
  if StData = nil then begin
    StData := TStructData.Create('');
    Game.StructData[id] := StData;
  end;
  with StData do begin
    Group := Trace.QBlock;
    Flags := StrToInt(Trace.Block);
    Size := StrToInt(Trace.Block);
    Protection := StrToInt(Trace.Block);
    Capacity := StrToInt(Trace.Block);
    Sailors := StrToInt(Trace.Block);
    Material1 := GetItem(Game.ItemData, Trace.Block);
    Material2 := GetItem(Game.ItemData, Trace.Block);
    Resource := GetItem(Game.ItemData, Trace.Block);
    BuildSkill := StrToSkill(Trace.Block, False);
    Incomplete := StrToBool(Trace.Block);
    Tool := GetItem(Game.ItemData, Trace.Block);
    ToolBonus := StrToInt(Trace.Block);
    Description := Trace.QBlock;

    if Pos('Defence: ', Trace.Text) > 0 then begin
      Trace.Before('Defence: ');
      for i := 0 to atCount-1 do Defence[i] := StrToInt(Trace.Block);
    end;
  end;
end;

procedure TerrainDataParser(Trace: TTrace);
var id: integer;
    Data: TTerrainData;
begin
  id := StrToInt(Trace.Before('='));
  Data := TTerrainData(ItemById(Game.TerrainData, id));
  if Data = nil then begin
    Data := TTerrainData.Create('');
    Game.TerrainData[id] := Data;
  end;
  with Data do begin
    Name := Trace.QBlock;
    Flags := StrToInt(Trace.QBlock);
    MoveCost := StrToInt(Trace.QBlock);

    if Pos('Res:', Trace.Text) > 0 then begin
      Trace.Before('Res: ');
      ReadList(Trace, AdvResources, ItemDataGet);
    end;
  end;
end;

procedure WeatherDataParser(Trace: TTrace);
var id: integer;
    Data: TWeatherData;
begin
  id := StrToInt(Trace.Before('='));
  Data := TWeatherData(ItemById(Game.WeatherData, id));
  if Data = nil then begin
    Data := TWeatherData.Create('');
    Game.WeatherData[id] := Data;
  end;
  with Data do begin
    Text := Trace.QBlock;
    LastText := Trace.QBlock;
    NextText := Trace.QBlock;
    MoveCost := StrToInt(Trace.Block);
  end;
end;

procedure EffectDataParser(Trace: TTrace);
var i, id: integer;
    Data: TEffectData;
begin
  id := StrToInt(Trace.Before('='));
  Data := TEffectData(ItemById(Game.EffectData, id));
  if Data = nil then begin
    Data := TEffectData.Create;
    Game.EffectData[id] := Data;
  end;
  with Data do begin
    Name := Trace.QBlock;
    Flags := StrToInt(Trace.Block);
    Attack := StrToInt(Trace.Block);
    for i := 0 to atCount-1 do
      Defence[i] := StrToInt(Trace.Block);
  end;
end;

procedure SpecDataParser(Trace: TTrace);
var i, id: integer;
    Data: TSpecData;
begin
  id := StrToInt(Trace.Before('='));
  Data := TSpecData(ItemById(Game.SpecData, id));
  if Data = nil then begin
    Data := TSpecData.Create('');
    Game.SpecData[id] := Data;
  end;
  with Data do begin
    Name := Trace.QBlock;
    Flags := StrToInt(Trace.Block);
    Description := Trace.QBlock;
    SpellText := Trace.QBlock;
    Incomplete := StrToBool(Trace.Block);

    if Pos('Shields: ', Trace.Text) > 0 then begin
      Trace.Before('Shields: ');
      for i := 0 to atCount-1 do
        Shields[i] := StrToBool(Trace.Block);
    end;

    if Pos('Atk: ', Trace.Text) > 0 then begin
      Trace.Before('Atk: ');
      while not Trace.Ends and (Trace.Separator <> '.') do begin
        i := Length(Attacks);
        SetLength(Attacks, i + 1);
        Attacks[i].WeaponFlags := StrToInt(Trace.Block);
        Attacks[i].AttackType := StrToInt(Trace.Block);
        Attacks[i].MinAmt := StrToInt(Trace.Block);
        Attacks[i].MaxAmt := StrToInt(Trace.Block);
        Attacks[i].WeaponClass := StrToInt(Trace.Block);
        Attacks[i].Effect := GetItem(Game.EffectData, Trace.Block);
      end;
    end;

    if Pos('Def: ', Trace.Text) > 0 then begin
      Trace.Before('Def: ');
      while not Trace.Ends and (Trace.Separator <> '.') do begin
        i := Length(Defences);
        SetLength(Defences, i + 1);
        Defences[i].AttackType := StrToInt(Trace.Block);
        Defences[i].Bonus := StrToInt(Trace.Block);
      end;
    end;

    if Pos('Items: ', Trace.Text) > 0 then begin
      Trace.Before('Items: ');
      ReadList(Trace, Items, ItemDataGet);
    end;

    if Pos('Structs: ', Trace.Text) > 0 then begin
      Trace.Before('Structs: ');
      ReadList(Trace, Structs, StructDataGet);
    end;

    if Pos('Effects: ', Trace.Text) > 0 then begin
      Trace.Before('Effects: ');
      ReadList(Trace, Effects, EffectDataGet);
    end;
  end;
end;

procedure WeatherMonDataParser(Trace: TTrace);
var Key: string;
    i, area: integer;
begin
  Key := Trace.Before(': ');
  if Key = 'North' then area := areaNorth
  else if Key = 'South' then area := areaSouth
  else if Key = 'Tropical' then area := areaTropical
  else Exit;
  for i := 1 to 12 do
    WeatherMonths[area, i] := Game.WeatherData[StrToInt(Trace.Block)];
end;

procedure MapLevelParser(Trace: TTrace);
var i, idx: integer;
begin
  idx := StrToInt(Trace.Before('='));
  for i := Map.Levels.Count to idx do
    Map.Levels.Add(TMapLevel.Create(''));
  Map.Levels[idx].Name := Trace.Text;
end;

procedure RegionParser(Trace: TTrace);
var i: integer;
    R: TRegion;
    RegCoords: TCoords;
    note: string;
begin
  RegCoords := StrToCoords(Trace.Before('='));
  Map.TurnNum := StrToInt(Trace.Block); // Form region relative to visit turn
  R := Map.SeekRegion(RegCoords);
  with R do begin
    Terrain := StrToTerrainData(Trace.Block);
    Land := Trace.QBlock;
    note := StringReplace(Trace.QBlock, '<br>', #13#10, [rfReplaceAll]);
    note := StringReplace(note, '&quot;', '"', [rfReplaceAll]);
    Notes.Text := note;
    Settlement := Trace.QBlock;
    SettlementType := StrToInt(Trace.QBlock);
    if not Trace.Ends then begin
      FullData := True;
      TaxRate := StrToInt(Trace.QBlock);
      Wages := StrToInt(Trace.QBlock);
      MaxWages := StrToInt(Trace.QBlock);
      Entertainment := StrToInt(Trace.QBlock);
      Gate := StrToInt(Trace.QBlock);
      i := StrToInt(Trace.QBlock);
      if i <> 0 then Guard := History.Factions.Seek(i);
      Peasants := StrToItem(Trace.QBlock, False);

      Trace.Before('Exits: ');
      for i := 1 to 6 do
        HasExit[i] := StrToBool(Trace.Block);

      Trace.Before('Wanted: ');
      ReadList(Trace, Wanted, ItemGet);

      Trace.Before('ForSale: ');
      ReadList(Trace, ForSale, ItemGet);

      Trace.Before('Products: ');
      ReadList(Trace, Products, ItemGet);

      Trace.Before('Structs: ');
      ReadStructList(Trace, Structs);
    end;
  end;
end;

procedure UArmyParser(Trace: TTrace);
var F: TUArmy;
begin
  F := TUArmy.Create(Trace.QBlock);
  Game.UArmies.Add(F);
  F.Color := StrToInt(Trace.Block);
  Trace.Before('Units: ');
  while not Trace.Ends do F.UnitIds.Add(Trace.Block);
end;

procedure InitWeather;
var i, j: integer;
begin
  // Weather data must contain 3 predef weathers
  for i := Game.WeatherData.Count to weatherCount-1 do
    Game.WeatherData.Add(TWeatherData.Create(''));
  // Init WeatherMonths
  for i := 0 to 2 do
    for j := 1 to 12 do
      WeatherMonths[i, j] := Game.WeatherData[weatherClear];
end;


 { --------- Compilators and Readers ------------- }

procedure ReadGameHistory(Lines: TStrings);
var i, line: integer;
    Trace: TTrace;
begin
  Trace := TTrace.Create(Lines[0]);
  Trace.Before('Game history: ');
  if Trace.Text = '' then
    raise EHistoryError.Create('Wrong history file.');
  if Trace.Text <> HistoryVersion then
    raise EHistoryError.Create('Old history format.');
  Trace.Free;
  line := 0;
  ReadDataList('[Config]', Lines, line, ConfigParser);
  ReadDataList('[Progress]', Lines, line, ProgressParser);
  ReadDataList('[Factions]', Lines, line, FactionParser);
  ReadDataList('[Items]', Lines, Line, ItemDataParser);
  ReadDataList('[Skills]', Lines, Line, SkillDataParser);
  ReadDataList('[Structs]', Lines, Line, StructDataParser);
  ReadDataList('[Terrain]', Lines, Line, TerrainDataParser);
  ReadDataList('[Weather]', Lines, Line, WeatherDataParser);
  InitWeather;
  ReadDataList('[WeatherMonths]', Lines, Line, WeatherMonDataParser);
  ReadDataList('[Effects]', Lines, Line, EffectDataParser);
  ReadDataList('[Specials]', Lines, Line, SpecDataParser);
  ReadDataList('[MapLevels]', Lines, line, MapLevelParser);
  ReadDataList('[Map]', Lines, line, RegionParser);
  ReadDataList('[Armies]', Lines, line, UArmyParser);

  // Remove nils from data lists
  Game.ItemData.Pack;
  Game.SkillData.Pack;
  Game.StructData.Pack;
  Game.TerrainData.Pack;
  Game.WeatherData.Pack;
  Game.EffectData.Pack;
  Game.SpecData.Pack;

  // Init SilverData
  i := Game.ItemData.Count-1;
  while (i >= 0) and not Test(Game.ItemData[i].Flags, IT_SILVER) do Dec(i);
  if i >= 0 then SilverData := Game.ItemData[i];
end;

procedure WriteGameHistory(Lines: TStrings);
begin
  Lines.Add('Game history: ' + HistoryVersion);
  Lines.Add('');
  SaveProgress(Lines);
  SaveFactions(Lines);
  SaveItems(Lines, False);
  SaveSkills(Lines, False);
  SaveStructs(Lines, False);
  SaveTerrain(Lines);
  SaveWeather(Lines);
  SaveWeatherMonths(Lines);
  SaveEffects(Lines);
  SaveSpecials(Lines);
  SaveMap(Lines);
  SaveUArmies(Lines);
end;

procedure WriteRuleset(Filename: string);
var Lines: TStrings;
begin
  Lines := TStringList.Create;
  Lines.Add('Game history: ' + HistoryVersion);
  Lines.Add('');
  SaveConfig(Lines);
  SaveProgress(Lines);
  SaveItems(Lines, BasicRuleset);
  SaveSkills(Lines, BasicRuleset);
  SaveStructs(Lines, BasicRuleset);
  SaveTerrain(Lines);
  SaveWeather(Lines);
  SaveWeatherMonths(Lines);
  SaveEffects(Lines);
  SaveSpecials(Lines);
  Lines.SaveToFile(Filename);
  Lines.Free;
end;


  { ---------- Drop turn to history ----------- }

// Pack to history when too many turns in manager
procedure MergeWithHistory(ATurn: TTurn);
var i, k, x, y, z: integer;
    Fac: TFaction;
    R: TRegion;
begin
  // Copy factions to history
  for i := 0 to ATurn.Factions.Count-1 do begin
    Fac := History.Factions.Seek(ATurn.Factions[i].Num);
    Fac.Assign(ATurn.Factions[i], False);
  end;

  // Kill factions and units in map
  for z := 0 to Map.Levels.Count-1 do
    for y := Map.Levels[z].Bounds.Top to Map.Levels[z].Bounds.Bottom do
      for x := Map.Levels[z].Bounds.Left to Map.Levels[z].Bounds.Right do begin
        R := Map.Region(x, y, z, ATurn.Num);
        if R <> nil then begin
          // Replace guard with history faction
          if R.Guard <> nil then
            R.Guard := History.Factions.Find(R.Guard.Num);
          R.Troops.ClearAndFree;
          R.Troops := TTroopList.Create;
          // Kill struct owners
          for k := 0 to R.Structs.Count-1 do R.Structs[k].Owner := nil;
        end;
      end;

  // Delete turn
  i := 1;
  while (i < Game.Turns.Count) and (Game.Turns[i] <> ATurn) do Inc(i);
  if i < Game.Turns.Count then begin
    Game.Turns[i].Free;
    Game.Turns.Delete(i);
  end;
end;

end.
