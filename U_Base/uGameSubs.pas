unit uGameSubs;

interface

uses
  SysUtils, Classes, Windows, Forms, IniFiles, DataStructs, MyStrings,
  Math, AtlaDate, uKeys, Resources, uUnitRecs;

const
  sortAscending = TRUE;
  sortDescending = FALSE;

  prdOk = 0;
  prdNoSkill = 1;
  prdNoResource = 2;
  prdNoMaterial = 3;
  prdNoStruct = 4;
  prdCantProduce = 5;
  prdWrongAlignment = 6;

type
  TReached = record
    Coords: TCoords;
    Moves: integer;
  end;

var
  MapBounds: array of TRect;
  Reached: array of TReached;
  MoveOrder: string;
  MoveArmy: boolean;
  MovePoints: integer;

  // Coords
  function PointInDir(P: TPoint; Dir: integer): TPoint;
  function CoordsInDir(C: TCoords; Dir: integer): TCoords;
  function OppositeDir(Dir: integer): integer;
  function DirectionFromTo(CFrom, CTo: TCoords): integer;
  function Distance(a, b: TCoords): integer;
  // Strings
  function StripTags(s: string): string;
  function IncludeTags(s, tags: string): string;
  // Units
  function MonsterTroop(Troop: TTroop): boolean;
  function ScoutTroop(Troop: TTroop): boolean;
  function CountMen(ALFaction: TTroop): integer;
  function HasMonsters(ARegion: TRegion): boolean;
  function UnitLoad(U: TUnit; MT: integer): integer;
  function UnitCapacity(U: TUnit; MT: integer): integer;
  function SingleRace(Items: TItemList): TItem;
  function IsLeader(Items: TItemList): boolean; overload;
  function IsLeader(U: TUnit): boolean; overload;
  function SingleLeader(Items: TItemList): boolean;
  function Taxers(AUnit: TUnit): integer;
  function FactionLeader: TUnit;
  function SkillLevel(AUnit: TUnit; Short: string): integer;
  function RaceSkillLevel(Data: TItemData; SkillData: TSkillData): integer;
  function MaxSkillLevel(AUnit: TUnit; SData: TSkillData): integer;
  function FindUnit(Num: integer): TUnit;
  function ProduceMaterials(Items, Materials: TItemList; AnyOf: boolean): integer;
  function GetToolsBonus(Items: TItemList; IData: TItemData; men: integer): integer;
  function CanProduce(U: TUnit; R: TRegion; IData: TItemData;
    var Level, Amount: integer): integer;
  function ProduceOut(U: TUnit; R: TRegion; IData: TItemData; var MaxOut,
    TurnOut: integer; limitOut: integer; StatInfo: boolean): integer;
  function EntertainOut(U: TUnit): integer;
  function BuildMaterials(AUnit: TUnit; Data: TStructData): integer;
  function BuildSkillLv(AUnit: TUnit; Data: TStructData): integer;
  function ProduceAmount(IData: TItemData; Amount: integer): integer;
  function StudyCost(U: TUnit; SData: TSkillData): integer;
  // Unit movement
  procedure DoCalcReachedRegions(C: TCoords; AUnit: TUnit);
  function MovableDir(From: TCoords; Dir: integer; Sail: boolean;
    MT: integer; CanSwim: boolean; Ship: TStruct): boolean;
  function EnterCost(MT: integer; C, From: TCoords; Sailing: boolean): integer;
  function MovesLeft(AUnit: TUnit): integer;
  function ArmyMovesLeft(AUnit: TUnit): integer;
  function MovementType(AUnit: TUnit): integer;
  function ArmyMovementType(AUnit: TUnit): integer;
  function CanSwim(AUnit: TUnit): boolean;
  function ArmyCanSwim(AUnit: TUnit): boolean;
  function FindShaft(CFrom, CTo: TCoords): TStruct;
  // Unit operations
  procedure SetMoneyInc(AUnit: TUnit; AValue: integer);
  procedure SetAmountInc(AUnit: TUnit; AData: TItemData; AValue: integer);
  procedure RemoveOrderToken(U: TUnit; s: string);
  // Regions
  function StructCarriedWeight(Struct: TStruct): integer;
  function GetWeather(ACoords: TCoords): TWeatherData;
  function RegionInDir(Coords: TCoords; Dir: integer): TRegion;
  function IsNexus(Level: integer): boolean;
  // Other data
  function TestItemName(AItemData: TItemData; s: string): boolean;
  function MakeRegionName(C: TCoords; UseSettlement: boolean): string;
  function MakeAltRegionName(C: TCoords): string;
  function SkillIcon(AData: TSkillData): integer;
  function FindFaction(ANum: integer): TFaction;
  function StructDirection(Group: string): integer;
  function FactionName(F: TFaction): string;
  function LevelByPoints(P: integer): integer;
  function PointsByLevel(Lv: integer): integer;
  function IsSpoils(IData: TItemData): boolean;
  function IsMaterial(IData: TItemData): boolean;
  function BookmarkCoords(s: string): TCoords;

  // QM support
  procedure CalcQMReach(AQmaster: TUnit);
  function FindReach(const ACoords: TCoords): integer;

implementation

function SkillLevel(AUnit: TUnit; Short: string): integer;
var i: integer;
begin
  i := 0;
  while (i < AUnit.Skills.Count) and (AUnit.Skills[i].Data.Short <>
    Short) do Inc(i);
  if i < AUnit.Skills.Count then Result := AUnit.Skills[i].Level
  else Result := 0;
end;

function RaceSkillLevel(Data: TItemData; SkillData: TSkillData): integer;
var Skill: TSkill;
begin
  if not Data.Man.Leader and Test(SkillData.Flags, SK_MAGIC) then
    Result := 0
  else if Data.Man.Leader and (Data.Man.SpecSkills.Count = 0) then
    Result := 5
  else begin
    if Test(SkillData.Flags, SK_MAGIC) then Result := Data.Man.MagDefLevel
    else Result := Data.Man.DefLevel;
    // Look for specializations
    Skill := Data.Man.SpecSkills.Find(SkillData.Short);
    if Skill <> nil then Result := Skill.Level;
  end;
end;

function MaxSkillLevel(AUnit: TUnit; SData: TSkillData): integer;
var i: integer;
begin
  Result := 9999;
  for i := 0 to AUnit.Items.Count-1 do begin
    // All men in unit must study this skill
    if Test(AUnit.Items[i].Data.Flags, IT_MAN) then
      Result := Min(Result, RaceSkillLevel(AUnit.Items[i].Data, SData));
  end;
  if Result = 9999 then Result := 5
  else if GameConfig.ReadInteger('Settings', 'Mod', modStandard) = modMagicDeep then begin
    // MagicDeep specialization boost
    if (Result > 2) and not IsLeader(AUnit) then begin
      i := AUnit.Skills.Count-1;
      while (i >= 0) and (PointsByLevel(Result) >= AUnit.Skills[i].Points) do
        Dec(i);
      if (i < 0) or (AUnit.Skills[i].Data = SData) then Inc(Result, 2);
    end;
  end;
end;

function FindUnit(Num: integer): TUnit;
var i: integer;
begin
  Result := nil;
  i := 0;
  while (i < VTurn.Factions.Count) and (Result = nil) do begin
    Result := VTurn.Factions[i].Units.Find(Num);
    Inc(i);
  end;
end;

function ProduceMaterials(Items, Materials: TItemList; AnyOf: boolean): integer;
var i, amt: integer;
    Item: TItem;
begin
  Result := 0;
  for i := 0 to Materials.Count-1 do begin
    Item := Items.Find(Materials[i].Data.Short);
    if AnyOf then begin
      // use any item
      if Item = nil then Continue;
      Inc(Result, Item.Amount div Materials[i].Amount);
    end
    else begin
      // use all items
      if Item = nil then begin
        Result := 0;
        Break;
      end;
      amt := Item.Amount div Materials[i].Amount;
      if i = 0 then Result := amt
      else Result := Min(Result, amt);
    end;
  end;
end;

function GetToolsBonus(Items: TItemList; IData: TItemData; men: integer): integer;
var i: integer;
begin
  Result := 0;
  for i := 0 to Items.Count-1 do begin
    if Items[i].Data = IData.Produce.Tool then begin
      Inc(Result, IData.Produce.ToolBonus * Min(Items[i].Amount, men));
      Dec(men, Min(Items[i].Amount, men));
    end;
  end;
end;

function ProduceAmount(IData: TItemData; Amount: integer): integer;
begin
  if IData.Produce.ManMonths <> 0 then
    Result := Amount * IData.Produce.Rate div IData.Produce.ManMonths
  else Result := Amount;
end;

function StudyCost(U: TUnit; SData: TSkillData): integer;
begin
  Result := SData.Cost;
  if (GameMod = modMagicDeep) and not IsLeader(U) then
    Result := Result + 10 * (U.Skills.Count-1);
  Result := Result * U.Items.Amount(IT_MAN);
end;

function CanProduce(U: TUnit; R: TRegion; IData: TItemData;
  var Level, Amount: integer): integer;
var Skill: TSkill;
    Res: TItem;
begin
  Level := -1;
  Amount := 0;
  // Good/Evil
  if ((GameConfig.ReadInteger('Game', 'Alignment', alNeutral) = alGood)
     and Test(IData.Flags, IT_EVIL))
    or ((GameConfig.ReadInteger('Game', 'Alignment', alNeutral) = alEvil)
     and Test(IData.Flags, IT_GOOD)) then begin
    Result := prdWrongAlignment;
    Exit;
  end;
  // Check required structure
  if (IData.Produce.RequiredStruct <> nil)
    and ((U.Struct = nil) or (IData.Produce.RequiredStruct <> U.Struct.Data)) then begin
    Result := prdNoStruct;
    Exit;
  end;
  // Check skill
  if IData.Produce.Skill <> nil then begin
    Skill := U.Skills.Find(IData.Produce.Skill.Data.Short);
    if (Skill = nil) or (Skill.Level < IData.Produce.Skill.Level) then begin
      Result := prdNoSkill;
      Exit;
    end;
    Level := Skill.Level;
  end
  else Level := 0;
  // Resources
  if Test(IData.Flags, IT_RESOURCE) then begin
    Res := R.Products.Find(IData.Short);
    if (Res = nil) or (Res.Amount <= 0) then begin
      Result := prdNoResource;
      Amount := -1;
      Exit;
    end;
    Amount := Res.Amount;
  end
  else begin
    if IData.Produce.Materials.Count > 0 then begin
      Amount := ProduceMaterials(U.Items, IData.Produce.Materials,
        Test(IData.Flags, IT_ORMATERIALS));
      if Amount <= 0 then begin
        Result := prdNoMaterial;
        Exit;
      end;
    end
    else begin
      Amount := -1;
      Result := prdCantProduce;
      Exit;
    end;
  end;
  Result := prdOk;
end;

function ProduceOut(U: TUnit; R: TRegion; IData: TItemData; var MaxOut,
  TurnOut: integer; limitOut: integer; StatInfo: boolean): integer;
var level, amt, men, bonus: integer;
begin
  maxout := 0;
  turnout := 0;
  Result := CanProduce(U, R, IData, level, amt);
  if StatInfo and not (Result in [prdCantProduce, prdNoSkill, prdWrongAlignment]) then
    Result := prdOk;
  if Result <> prdOk then Exit;
  // Calculate output
  men := U.Items.Amount(IT_MAN);
  bonus := GetToolsBonus(U.Items, IData, men);
  MaxOut := ProduceAmount(IData, men * level + bonus);

  if limitOut <= 0 then limitOut := MaxOut;
  TurnOut := Min(Min(amt, MaxOut), limitOut);
end;

function EntertainOut(U: TUnit): integer;
var men, output, bonus: integer;
begin
  men := U.Items.Amount(IT_MAN);
  output := men * U.Skills.Find(Keys[s_EntertainSkill]).Level;
  if SilverData <> nil then
    bonus := GetToolsBonus(U.Items, SilverData, men)
  else bonus := 0;
  Result := output * GameConfig.ReadInteger('Settings', 'EntertainIncome', 20)
    + bonus;
end;

function BuildMaterials(AUnit: TUnit; Data: TStructData): integer;
var Item: TItem;
begin
  Result := 0;
  if (Data.Material1 = nil) and (Data.Material2 = nil) then
    Result := -1
  else begin
    Item := nil;
    if Data.Material1 <> nil then
      Item := AUnit.Items.Find(Data.Material1.Short);
    if (Item = nil) and (Data.Material2 <> nil) then
      Item := AUnit.Items.Find(Data.Material2.Short);
    if Item <> nil then
      Result := Item.Amount;
  end;
end;

function BuildSkillLv(AUnit: TUnit; Data: TStructData): integer;
var k: integer;
begin
  Result := 0;
  // Check if the unit has needed skill
  if Data.BuildSkill <> nil then begin
    k := AUnit.Skills.Count-1;
    while (k >= 0)
      and ((AUnit.Skills[k].Data.Short <> Data.BuildSkill.Data.Short)
      or (AUnit.Skills[k].Level < Data.BuildSkill.Level)) do Dec(k);
    if k >= 0 then Result := AUnit.Skills[k].Level;
  end;
end;

function CoordsInDir(C: TCoords; Dir: integer): TCoords;
var P: TPoint;
begin
  case Dir of
    dirN:  P := Point(C.x, C.y-2);
    dirNE: P := Point(C.x+1, C.y-1);
    dirSE: P := Point(C.x+1, C.y+1);
    dirNW: P := Point(C.x-1, C.y-1);
    dirSW: P := Point(C.x-1, C.y+1);
    else   P := Point(C.x, C.y+2);
  end;
  with MapBounds[C.z] do
    if P.X < Left then P.X := Right // X displacement is no more than 1
    else if P.X > Right then P.X := Left;
  Result.x := P.X;
  Result.y := P.Y;
  Result.z := C.z;
end;

function PointInDir(P: TPoint; Dir: integer): TPoint;
var Coords: TCoords;
begin
  Coords.x := P.X;
  Coords.y := P.Y;
  Coords.z := Map.Level;
  Coords := CoordsInDir(Coords, Dir);
  Result := Point(Coords.x, Coords.y);
end;

function GetWeather(ACoords: TCoords): TWeatherData;
var area, month: integer;
    Region: TRegion;
begin
  Region := Map.Region(ACoords);
  if (Region <> nil) and (Region.Visited = Turn.Num) and Region.FullData
    and (Region.WeatherNext <> nil) then
    Result := Region.WeatherNext
  else begin
    Result := nil;
    if GameConfig.ReadBool('Map', 'GuessWeather', False) then begin
      if (Map.Levels[ACoords.z].Name = Keys[s_Surface]) then begin
        area := Min(2, 3 * (ACoords.y + 1) div GameConfig.ReadInteger('Map', 'MaxY_' +
          Map.Levels[ACoords.z].Name, Map.Levels[ACoords.z].Bounds.Bottom));
        month := ((Turn.Num-1) mod 12)+1;
        Result := WeatherMonths[area, month];
      end
      else Result := Game.WeatherData[weatherClear];
    end;
  end;
end;

function FindFaction(ANum: integer): TFaction;
var i: integer;
begin
  Result := nil;
  if VTurn <> nil then Result := VTurn.Factions.Find(ANum);
  if Result = nil then begin
    i := Game.Turns.Count-1;
    while (i >= 0) and (Result = nil) do begin
      Result := Game.Turns[i].Factions.Find(ANum);
      Dec(i);
    end;
  end;
end;

function SkillIcon(AData: TSkillData): integer;
begin
  if AData = nil then Result := 0
  else if Test(AData.Flags, SK_FOUNDATION) then Result := bmpFoundation
  else if Test(AData.Flags, SK_MAGIC) then Result := bmpSpell
  else Result := bmpSkill;
end;

function MakeRegionName(C: TCoords; UseSettlement: boolean): string;
var Region: TRegion;
begin
  Result := IntToStr(C.X) + ',' + IntToStr(C.Y);
  if Map.Levels[C.Z].Name <> Keys[s_Surface] then
    Result := Result + ',' + Map.Levels[C.Z].Name;
  Region := Map.Region(C);
  if Region <> nil then begin
    if not UseSettlement or (Region.Settlement = '') then
      Result := Region.Terrain.Name + ' (' + Result + ') in ' + Region.Land
    else Result := Region.Settlement;
  end;
end;

function MakeAltRegionName(C: TCoords): string;
var Region: TRegion;
begin
  Result := IntToStr(C.X) + ',' + IntToStr(C.Y);
  if Map.Levels[C.Z].Name <> Keys[s_Surface] then
    Result := Result + ',' + Map.Levels[C.Z].Name;
  Result := '(' + Result + ')';
  Region := Map.Region(C);
  if Region <> nil then begin
    Result := Region.Terrain.Name + ' ' + Result + ' in ' + Region.Land;
    if Region.Settlement <> '' then
      Result := Result + ', ' + Region.Settlement;
  end;
end;

function FactionLeader: TUnit;
var num: integer;
begin
  num := GameConfig.ReadInteger('Game', 'Leader', 0);
  Result := VFaction.Units.Find(num);
  if Result = nil then begin
    Result := VFaction.Units[0];
    GameConfig.WriteInteger('Game', 'Leader', num);
  end;
end;

// Returns rate of normal movement (walk, ride, fly)
function MovementType(AUnit: TUnit): integer;
var cap: integer;
begin
  Result := mtFly;
  while (Result >= mtWalk) do begin
    cap := UnitCapacity(AUnit, Result);
    if (cap > 0) and (UnitLoad(AUnit, Result) <= cap) then Break;
    Dec(Result);
  end;
end;

function ArmyMovementType(AUnit: TUnit): integer;
var Troop: TTroop;
    i: integer;
begin
  if not MoveArmy or (AUnit.UArmy = nil) then
    Result := MovementType(AUnit)
  else begin
    Result := mtFly;
    Troop := AUnit.Region.PlayerTroop;
    if Troop = nil then Exit;
    for i := 0 to Troop.Units.Count-1 do
      if Troop.Units[i].UArmy = AUnit.UArmy then
        Result := Min(Result, MovementType(Troop.Units[i]));
  end;
end;

// Returns presense of swimming movement
function CanSwim(AUnit: TUnit): boolean;
var cap: integer;
begin
  cap := UnitCapacity(AUnit, mtSwim);
  Result := (cap > 0) and (UnitLoad(AUnit, mtSwim) <= cap);
  if not Result and GameConfig.ReadBool('Settings', 'FlyingCross', True)
    and not AUnit.Flags[flgNocross] and (MovementType(AUnit) = mtFly) then
    Result := True;
end;

function ArmyCanSwim(AUnit: TUnit): boolean;
var Troop: TTroop;
    i: integer;
begin
  if not MoveArmy or (AUnit.UArmy = nil) then
    Result := CanSwim(AUnit)
  else begin
    Result := True;
    Troop := AUnit.Region.PlayerTroop;
    if Troop = nil then Exit;
    i := Troop.Units.Count-1;
    while (i >= 0) and Result do begin
      if (Troop.Units[i].UArmy = AUnit.UArmy) and
        not CanSwim(Troop.Units[i]) then Result := False;
      Dec(i);
    end;
  end;
end;

function FindShaft(CFrom, CTo: TCoords): TStruct;
var i: integer;
    R: TRegion;
begin
  Result := nil;
  R := Map.Region(CFrom);
  if R = nil then Exit;
  i := R.Structs.Count-1;
  while (i >= 0) and not EqualCoords(R.Structs[i].Passage, CTo) do Dec(i);
  if i >= 0 then Result := R.Structs[i];
end;

function MovesLeft(AUnit: TUnit): integer;
var mt, i: integer;
begin
  mt := MovementType(AUnit);
  if (mt = 0) and CanSwim(AUnit) then mt := mtWalk;
  if MoveOrder = 'sail' then Result := 4
  else Result := mt * 2;
  for i := 1 to Length(AUnit.Moves)-1 do
    Result := Max(0, Result - EnterCost(mt, AUnit.Moves[i],
      AUnit.Moves[i-1], (MoveOrder = 'sail')));
end;

function ArmyMovesLeft(AUnit: TUnit): integer;
var Troop: TTroop;
    i: integer;
begin
  if not MoveArmy or (AUnit.UArmy = nil) then
    Result := MovesLeft(AUnit)
  else begin
    Result := 10;
    Troop := AUnit.Region.PlayerTroop;
    if Troop = nil then Exit;
    for i := 0 to Troop.Units.Count-1 do
      if Troop.Units[i].UArmy = AUnit.UArmy then
        Result := Min(Result, MovesLeft(Troop.Units[i]));
  end;
end;

function RoadExists(Region: TRegion; From: TCoords): boolean;
var i: integer;
begin
  Result := False;
  if From.z <> Region.z then Exit;
  i := Region.Structs.Count-1;
  while (i >= 0) and not ( Test(Region.Structs[i].Data.Flags, ST_ROAD)
    and (EqualCoords(From, CoordsInDir(Region.Coords,
      StructDirection(Region.Structs[i].Data.Group)))) ) do Dec(i);
  Result := (i >= 0);
end;

function EnterCost(MT: integer; C, From: TCoords; Sailing: boolean): integer;
var Region: TRegion;
    Weather: TWeatherData;
begin
  Result := 1;
  Region := Map.Region(C);
  if Region <> nil then with Region do begin
    Weather := GetWeather(Region.Coords);
    if Weather <> nil then
      Result := Result * Max(Weather.MoveCost, 1);
    if ((MT = mtWalk) or (MT = mtRide)) and not Sailing then begin
      Result := Result * Max(Terrain.MoveCost, 1);
      if RoadExists(Region, From) and RoadExists(Map.Region(From), C) then
        Result := Max(Result div 2, 1);
    end;
  end;
end;

function OppositeDir(Dir: integer): integer;
begin
  if Dir in [1..6] then begin
    Result := Dir + 3;
    if Result > 6 then Result := Result - 6;
  end
  else Result := 0;
end;

function DirectionFromTo(CFrom, CTo: TCoords): integer;
var i: integer;
begin
  i := 6;
  while (i >= 1) and not EqualCoords(CoordsInDir(CFrom, i), CTo) do Dec(i);
  Result := i;
end;

function Distance(a, b: TCoords): integer;
var
  qA, rA, sA, qB, rB, sB: integer;

begin
  Result := -1;
  if a.z <> b.z then Exit;

  qA := a.x;
  rA := Trunc((a.y - a.x) / 2);
  sA := -qA - rA;

  qB := b.x;
  rB := Trunc((b.y - b.x) / 2);
  sB := -qB - rB;

  Result := Trunc((Abs(qA - qB) + Abs(rA - rB) + Abs(sA - sB)) / 2);
end;

function RegionInDir(Coords: TCoords; Dir: integer): TRegion;
var C: TCoords;
begin
  C := CoordsInDir(Coords, Dir);
  if (C.Y < MapBounds[C.z].Top) or (C.Y > MapBounds[C.z].Bottom) then
    Result := nil
  else Result := Map.Region(C);
end;

function IsNexus(Level: integer): boolean;
begin
  Result := (Level >= 0) and (Level < Map.Levels.Count) and
    (Map.Levels[Level].Name = Keys[s_Nexus]);
end;

function MovableDir(From: TCoords; Dir: integer; Sail: boolean;
  MT: integer; CanSwim: boolean; Ship: TStruct): boolean;
var Region, RegInDir: TRegion;
    C: TCoords;
begin
  C := CoordsInDir(From, Dir);
  if (C.Y < MapBounds[C.z].Top) or (C.Y > MapBounds[C.z].Bottom) then
    Result := False
  else begin
    RegInDir := Map.Region(C);
    Region := Map.Region(From);
    if RegInDir = nil then
      // Always move into unexplored region
      Result := TRUE
    else begin
      if not Sail and not ((MT = 0) and CanSwim) then begin
        if CanSwim then Result := True
        // Move to all land hexes; do not move from ocean
        else Result := not Test(RegInDir.Terrain.Flags, TER_WATER)
          and ((Region = nil) or not Test(Region.Terrain.Flags, TER_WATER));
      end
      else begin
        if Test(RegInDir.Terrain.Flags, TER_WATER) or
          ((Ship <> nil) and
          Test(Ship.Data.Flags, ST_FLYING)) then
          // Ships move in water, balloons everywhere
          Result := TRUE
        else
          // Ships also move everywhere from water
          Result := (Region <> nil) and Test(Region.Terrain.Flags, TER_WATER);
      end;
    end;
    // Check for walls
    if (Region <> nil) and Region.FullData and not Region.HasExit[Dir] then
      Result := False
    else if (RegInDir <> nil) and RegInDir.FullData and
      not RegInDir.HasExit[OppositeDir(Dir)] then Result := False;
  end;
end;

// Make list of regions unit can reach (for current map world)
// C: coords of starting region, Moves: remaining unit's MP;
//  FromDir: direction of last scanned region; MT: unit's movement type;
//  CanSwim: you know it; Ship: Unit.Struct
procedure CalcReachedRegions(C: TCoords; Moves, FromDir, MT: integer;
  CanSwim: boolean; Ship: TStruct);
var i, last, ExDir: integer;
begin
  if Moves < 0 then Exit;
  i := 0;
  while (i < Length(Reached)) and not EqualCoords(Reached[i].Coords, C) do Inc(i);
  if i = Length(Reached) then begin
    // Add new coord
    last := Length(Reached);
    SetLength(Reached, last + 1);
    Reached[last].Coords := C;
    Reached[last].Moves := Moves;
  end
  else if Reached[i].Moves < Moves then
    // Set new path from start to this coords if old path was worse
    Reached[i].Moves := Moves
  else
    // If this coords already found in better path
    Exit;
  // Scan neighbour hexes
  ExDir := OppositeDir(FromDir);
  for i := 1 to 6 do
    if (i <> ExDir)
      and MovableDir(C, i, (MoveOrder = 'sail'), MT, CanSwim, Ship) then begin
      CalcReachedRegions(CoordsInDir(C, i), Moves - EnterCost(MT,
        CoordsInDir(C, i), C, (MoveOrder = 'sail')), i, MT, CanSwim, Ship);
    end;
end;

procedure DoCalcReachedRegions(C: TCoords; AUnit: TUnit);
begin
  CalcReachedRegions(C, MovePoints, 0, ArmyMovementType(AUnit),
    ArmyCanSwim(AUnit), AUnit.Struct)
end;

// TODO : Find QM reach
procedure CalcQMReach(AQmaster: TUnit);
var
  iMaxDist: integer;
  iIdx:     integer;

  procedure BuildReach(const ACoords: TCoords; AStep, AFromDir: integer);
  var
    iIdx: integer;
    iDir: integer;
  begin
    if AStep > iMaxDist then
      Exit;

    iIdx := 0;
    while (iIdx < Length(Reached)) and not EqualCoords(ACoords, Reached[iIdx].Coords) do
      Inc(iIdx);

    if iIdx = Length(Reached) then
    begin
      iIdx := Length(Reached);
      SetLength(Reached, iIdx + 1);

      with Reached[iIdx] do
      begin
        Coords := ACoords;
        Moves := AStep;
      end;
    end
    else if AStep < Reached[iIdx].Moves then
      Reached[iIdx].Moves := AStep
    else
      exit;

    Inc(AStep);
    for iDir := 1 to 6 do
      if (iDir <> AFromDir) and MovableDir(ACoords, iDir, false, mtWalk, true, nil) then
        BuildReach(CoordsInDir(ACoords, iDir), AStep, OppositeDir(iDir));
  end;

  function HasQmaster(const ACoords: TCoords): boolean;
  var
    rRegion:    TRegion;
    iTroopIdx:  integer;
    ulUnits:    TUnitList;
    iUnitIdx:   integer;
    uUnit:      TUnit;
    sStruct:    TStruct;
  begin
    rRegion := Map.Region(ACoords);
    Result := false;

    if (rRegion <> nil) and (rRegion.Visited = Turn.Num) then
      for iTroopIdx := 0 to rRegion.Troops.Count - 1 do
      begin
        ulUnits := rRegion.Troops[iTroopIdx].Units;

        for iUnitIdx := 0 to ulUnits.Count - 1 do
        begin
          uUnit := ulUnits[iUnitIdx];
          sStruct := uUnit.Struct;

          if (SkillLevel(uUnit, Keys[s_Quartermaster]) > 0) and (sStruct <> nil)
            and (sStruct.Owner = uUnit) and (sStruct.Data.Group = Keys[s_Caravanserai]) then
          begin
            Result := true;
            exit;
          end;
        end;
      end;
  end;

  function HasTarget(const ACoords: TCoords): boolean;
  var
    rRegion:    TRegion;
    iTroopIdx:  integer;
    ulUnits:    TUnitList;
    iUnitIdx:   integer;
    uUnit:      TUnit;
  begin
    rRegion := Map.Region(ACoords);
    Result := false;

    if (rRegion <> nil) and (rRegion.Visited = Turn.Num) then
      for iTroopIdx := 0 to rRegion.Troops.Count - 1 do
      begin
        ulUnits := rRegion.Troops[iTroopIdx].Units;

        for iUnitIdx := 0 to ulUnits.Count - 1 do
        begin
          uUnit := ulUnits[iUnitIdx];

          if uUnit.Num = AQmaster.Num then
            continue;

          Result := True;
          exit;
        end;
      end;
  end;

  procedure DeleteReach(ARemove: integer);
  var
    iIdx: integer;
  begin
    iIdx := ARemove + 1;

    while iIdx < Length(Reached) do
    begin
      Reached[iIdx - 1] := Reached[iIdx];
      Inc(iIdx);
    end;

    SetLength(Reached, Length(Reached) - 1);
  end;
begin
  SetLength(Reached, 0);

  iMaxDist := 3 + ((SkillLevel(AQmaster, Keys[s_Quartermaster]) + 1) div 3);
  if Map.Levels[AQmaster.Region.z].Name <> Keys[s_Surface] then
    iMaxDist := iMaxDist div 2;

  BuildReach(AQmaster.Region.Coords, 0, 0);

  iIdx := Length(Reached) - 1;
  while iIdx >= 0 do
  begin
    if (Reached[iIdx].Moves > 2) then
    begin
      if not HasQmaster(Reached[iIdx].Coords) then
        DeleteReach(iIdx);
    end
    else if not HasTarget(Reached[iIdx].Coords) then
      DeleteReach(iIdx);

    Dec(iIdx);
  end;
end;

function FindReach(const ACoords: TCoords): integer;
var
  iIdx: integer;
begin
  Result := -1;

  for iIdx := 0 to Length(Reached) - 1 do
    if EqualCoords(Reached[iIdx].Coords, ACoords) then
    begin
      Result := Reached[iIdx].Moves;
      break;
    end;
end;

// Count total of faction's men in region
function CountMen(ALFaction: TTroop): integer;
var i: integer;
begin
  Result := 0;
  for i := 0 to ALFaction.Units.Count-1 do
    Result := Result + ALFaction.Units[i].Items.Amount(IT_MAN);
end;

function StructCarriedWeight(Struct: TStruct): integer;
var i, j: integer;
    R: TRegion;
begin
  Result := 0;
  if Struct.Owner <> nil then begin
    R := Struct.Owner.Region;
    for i := 0 to R.Troops.Count-1 do
      for j := 0 to R.Troops[i].Units.Count-1 do
        if R.Troops[i].Units[j].Struct = Struct then
          Result := Result + UnitLoad(R.Troops[i].Units[j], mtNone);
  end;
end;

// Remove [a .. ] sequences
function StripTags(s: string): string;
var Trace: TTrace;
begin
  Result := '';
  Trace := TTrace.Create(s);
  while not Trace.Ends do begin
    Result := Trim(Result + Trace.Before('[#'));
    Trace.Before(']');
  end;
  Trace.Free;
end;

// Include tags from string tags to string s
function IncludeTags(s, tags: string): string;
var Trace: TTrace;
begin
  Result := s;
  Trace := TTrace.Create(tags);
  Trace.Before('[#');
  while not Trace.Ends do begin
    Result := Trim(Result + ' [#' + Trace.Before(']') + ']');
    Trace.Before('[#');
  end;
  Trace.Free;
end;

function TestItemName(AItemData: TItemData; s: string): boolean;
begin
  Result := (AnsiUpperCase(s) = AItemData.Short) or (s = AItemData.SingleName)
    or (s = AItemData.MultiName);
end;

// Determine is this unit a monster
function Monster(AUnit: TUnit): boolean;
begin
  Result := (AUnit.Items.Amount(IT_MONSTER) > 0) and
    (AUnit.Items.Amount(IT_MAN) = 0);
end;

function MonsterTroop(Troop: TTroop): boolean;
var i: integer;
begin
  if Troop.Faction.Num = 2 then Result := True
  else if Troop.Faction.Num = 0 then begin
    i := Troop.Units.Count - 1;
    while (i >= 0) and not Monster(Troop.Units[i]) do Dec(i);
    Result := (i >= 0);
  end
  else Result := False;
end;

// Determine amount of monsters (faction 2 or unknown faction, no men, some monsters)
function HasMonsters(ARegion: TRegion): boolean;
var i: integer;
begin
  i := ARegion.Troops.Count-1;
  while (i >= 0) and not MonsterTroop(ARegion.Troops[i]) do Dec(i);
  Result := (i >= 0);
end;

// Determine if the unit is scout (no more men than mentioned in Config)
function Scout(AUnit: TUnit): boolean;
begin
  Result := (AUnit.Items.Amount(IT_MAN) <= Config.ReadInteger('Map', 'Scouts', 2));
end;

function ScoutTroop(Troop: TTroop): boolean;
begin
  Result := (Troop.Units.Count = 1) and Scout(Troop.Units[0]);
end;

function Hitched(U: TUnit; DWagon: TItemData): integer;
var i: integer;
begin
  Result := 0;
  for i := 0 to U.Items.Count-1 do
    if U.Items[i].Data = DWagon.Wagon.Hitched then
      Inc(Result, U.Items[i].Amount);
end;

// Unit's load, not including it's weight (except for mtNone)
function UnitLoad(U: TUnit; MT: integer): integer;
var i, wt, hitch: integer;
    Wagons: TItemList;
begin
  Result := 0;
  Wagons := TItemList.Create;
  for i := 0 to U.Items.Count-1 do
    if MT <= 0 then
      Result := Result + U.Items[i].Data.Weight * U.Items[i].Amount
    else begin
      wt := Max(U.Items[i].Data.Weight - U.Items[i].Data.Moves[MT], 0);
      Inc(Result, wt * U.Items[i].Amount);
      // Add wagons to list
      if (MT = mtWalk) and Test(U.Items[i].Data.Flags, IT_WAGON) then
        with Wagons.Seek(U.Items[i].Data.Short) do
          Inc(Amount, U.Items[i].Amount);
    end;

  // Process wagon bonus
  for i := 0 to Wagons.Count-1 do begin
    hitch := Min(Hitched(U, Wagons[i].Data), Wagons[i].Amount);
    Dec(Result, hitch * (Wagons[i].Data.Weight
      - Wagons[i].Data.Moves[MT]));
  end;
  Wagons.ClearAndFree;
end;

// Amount of weight unit can carry not including its own weight
function UnitCapacity(U: TUnit; MT: integer): integer;
var i, wt, hitch: integer;
    Wagons: TItemList;
begin
  Result := 0;
  Wagons := TItemList.Create;
  if MT <= 0 then Exit;

  for i := 0 to U.Items.Count-1 do begin
    wt := U.Items[i].Data.Moves[MT];
    if wt > 0 then wt := wt - U.Items[i].Data.Weight;
    Inc(Result, wt * U.Items[i].Amount);
    // Add wagons to list
    if (MT = mtWalk) and Test(U.Items[i].Data.Flags, IT_WAGON) then
      with Wagons.Seek(U.Items[i].Data.Short) do
        Inc(Amount, U.Items[i].Amount);
  end;

  // Process wagon bonus
  for i := 0 to Wagons.Count-1 do
    if Wagons[i].Data.Wagon.HitchWalk > 0 then begin
      hitch := Min(Hitched(U, Wagons[i].Data), Wagons[i].Amount);
      Inc(Result, hitch * (Wagons[i].Data.Wagon.HitchWalk -
        Wagons[i].Data.Weight));
    end;
  Wagons.ClearAndFree;
end;

function SingleRace(Items: TItemList): TItem;
var i: integer;
begin
  Result := nil;
  for i := 0 to Items.Count-1 do
    if (Items[i].Data.Flags and IT_MAN <> 0) then begin
      if Result = nil then Result := Items[i]
      else begin
        Result := nil;
        Break;
      end;
    end;
end;

function IsLeader(Items: TItemList): boolean;
var Men: TItem;
begin
  Men := SingleRace(Items);
  Result := (Men <> nil) and (Men.Data.Man.Leader);
end;

function IsLeader(U: TUnit): boolean;
begin
  Result := IsLeader(U.Items);
end;

function SingleLeader(Items: TItemList): boolean;
var Men: TItem;
begin
  Men := SingleRace(Items);
  Result := (Men <> nil) and (Men.Data.Man.Leader) and (Men.Amount = 1);
end;

procedure SetAmountInc(AUnit: TUnit; AData: TItemData; AValue: integer);
var i: integer;
begin
  i := 0;
  while (i < AUnit.Items.Count) and (AUnit.Items[i].Data <> AData) do Inc(i);
  if i < AUnit.Items.Count then begin
    AValue := AUnit.Items[i].Amount + AValue;
    if AValue <= 0 then begin
      AUnit.Items[i].Free;
      AUnit.Items.Delete(i);
    end
    else AUnit.Items[i].Amount := AValue;
  end
  else if AValue > 0 then begin
    AUnit.Items.Add(TItem.Create);
    with AUnit.Items[AUnit.Items.Count-1] do begin
      Amount := AValue;
      Data := AData;
    end;
  end;
end;

procedure SetMoneyInc(AUnit: TUnit; AValue: integer);
begin
  if SilverData <> nil then SetAmountInc(AUnit, SilverData, AValue);
end;

procedure RemoveOrderToken(U: TUnit; s: string);
var i: integer;
begin
  i := 0;
  while i < U.Orders.Count do
    if U.Order(i) = s then U.Orders.Delete(i)
    else Inc(i);
end;

function Taxers(AUnit: TUnit): integer;
var i, j, men, weapons: integer;
    Data: TItemData;
begin
  men := AUnit.Items.Amount(IT_MAN);
  // Search for combat skill
  i := 0;
  while (i < AUnit.Skills.Count) and not (AUnit.Skills[i].Data.Short = Keys[s_Combat])
    do Inc(i);
  if i < AUnit.Skills.Count then Result := men
  else begin
    // Look on weapons
    weapons := 0;
    for i := 0 to AUnit.Items.Count-1 do begin
      Data := AUnit.Items[i].Data;
      if Test(Data.Flags, IT_WEAPON) then begin
        if not Test(Data.Weapon.Flags, WPN_NEEDSKILL) then
          weapons := weapons + AUnit.Items[i].Amount
        else begin
          // Weapon needs skill
          j := 0;
          while (j < AUnit.Skills.Count) and
            ((Data.Weapon.Skill1 = nil)
              or (AUnit.Skills[j].Data.Short <> Data.Weapon.Skill1.Short)) and
            ((Data.Weapon.Skill2 = nil)
              or (AUnit.Skills[j].Data.Short <> Data.Weapon.Skill2.Short)) do Inc(j);
          if (j < AUnit.Skills.Count) then
            weapons := weapons + AUnit.Items[i].Amount;
        end;
      end;
    end;
    Result := Min(men, weapons);
  end;
end;

function StructDirection(Group: string): integer;
var dir: string;
    i: integer;
begin
  dir := Trim(Copy(Group, Length(Group) - 1, 2));
  i := 1;
  while (i <= 6) and (dir <> GetKey(s_N, i - 1)) do Inc(i);
  if i <= 6 then Result := i
  else Result := 0;
end;

function FactionName(F: TFaction): string;
begin
  if F.Num = 0 then Result := 'Unknown'
  else Result := F.Name;
end;

function LevelByPoints(P: integer): integer;
begin
  case P of
    30..89: Result := 1;
    90..179: Result := 2;
    180..299: Result := 3;
    300..449: Result := 4;
    450..9999: Result := 5;
    else Result := 0;
  end;
end;

function PointsByLevel(Lv: integer): integer;
var i: integer;
begin
  Result := 0;
  i := 1;
  while i <= Lv do begin
    Result := Result + 30 * i;
    Inc(i);
  end;
end;

function IsSpoils(IData: TItemData): boolean;
begin
  Result := not Test(IData.Flags, IT_SILVER + IT_MAN +
    IT_WEAPON + IT_ARMOR + IT_MOUNT + IT_MAGIC);
end;

function IsMaterial(IData: TItemData): boolean;
begin
  Result := (IData.Flags = IT_RESOURCE)
    or (IData.Flags = IT_ADVANCED + IT_RESOURCE);
end;

function BookmarkCoords(s: string): TCoords;
var Trace: TTrace;
begin
  Trace := TTrace.Create(s);
  Result.x := StrToInt(Trace.Before(' '));
  Result.y := StrToInt(Trace.Before(' '));
  Result.z := Map.Levels.NumOf(Trace.Text);
  Trace.Free;
end;

end.

