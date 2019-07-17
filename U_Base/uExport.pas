unit uExport;

{$MODE Delphi}

interface

uses
  SysUtils, Windows, Classes, DataStructs, Resources, Math;

const
  U_NAME = $01;
  U_FACTION = $02;
  U_FLAGS = $04;
  U_ITEMS = $08;
  U_SKILLS = $10;
  U_COMBAT = $20;
  U_CANSTUDY = $40;
  U_DESCR = $80;
  U_VISIBLE = $100;
  U_ALL = $FFF - U_VISIBLE;

  MAP_ADVRESOURCES = $01;
  MAP_BUILDINGS = $02;
  MAP_TRANSPORTS = $04;
  MAP_PLAYERUNITS = $08;
  MAP_PLAYERDETAILS = $10;
  MAP_NONPLAYERUNITS = $20;
  MAP_GATES = $40;
  MAP_ALL = $FF;

var
  LastMapParts: DWord;
  LastMapFile: string;

  procedure CompileOrder(Lines: TStrings; Compact: boolean);
  procedure CompileCommonMap(Lines: TStrings; Parts: DWord);
  procedure CompileRegion(Lines: TStrings; ARegion: TRegion; Parts: DWord);
  procedure AddMapHeader(Lines: TStrings);
  function UnitString(AUnit: TUnit; Parts: DWord): string;
  procedure CompileExportSkills(U: TUnit; Lines: TStrings);
  function ImportSkills(Lines: TStrings): string;

implementation

uses
  uKeys, MyStrings, uGameSubs, AtlaDate;


// Form string of unit with U_ flags, as it appears in report.
function UnitString(AUnit: TUnit; Parts: DWord): string;
var i: integer;
    A: array of string;
    s: string;
begin
   Result := '';
  // Name
   if (Parts and U_NAME) <> 0 then begin
     if AUnit.Num >= 0 then
       Result := Result + AUnit.Name + ' (' + IntToStr(AUnit.Num) + ')'
     else Result := Result + AUnit.Name + ' (new ' + IntToStr(Abs(AUnit.Num)) + ')';
   end;
  // onguard flag
   if ((Parts and U_FLAGS) <> 0) and (AUnit.Flags[0]) then begin
     Result := Result + ', ' + Keys[s_flgOnGuard];
   end;
  // Faction
   if ((Parts and U_FACTION) <> 0) and (AUnit.Faction.Num <> 0) then begin
     Result := Result + ', ' + AUnit.Faction.Name + ' (' +
       IntToStr(AUnit.Faction.Num) + ')';
   end;
  // Flags
   if (Parts and U_FLAGS) <> 0 then begin
     for i := flgAvoid to flgBehind do // avoiding, behind
       if AUnit.Flags[i] then Result := Result + ', ' + GetKey(s_flgOnGuard, i);
     if not Test(Parts, U_VISIBLE) then begin
       if AUnit.Revealing > 0 then
         Result := Result + ', ' + GetKey(s_RevealUnit, AUnit.Revealing - 1);
       for i := flgHold to flgNoaid do // hold, tax, noaid
         if AUnit.Flags[i] then Result := Result + ', ' + GetKey(s_flgOnGuard, i);
       if AUnit.Consuming > 0 then
         Result := Result + ', ' + GetKey(s_ConsumeUnit, AUnit.Consuming - 1);
       if AUnit.Flags[flgNocross] then Result := Result + ', ' +
         Keys[s_flgNocross];
     end;
   end;
  // Items
   if Test(Parts, U_ITEMS) then begin
     for i := 0 to AUnit.Items.Count-1 do
       with AUnit.Items[i] do begin
         if not (Test(Parts, U_VISIBLE) and (Data.Weight = 0)) then begin
           if Amount > 1 then Result := Result + ', ' + IntToStr(Amount) + ' ' + Data.MultiName
           else Result := Result + ', ' + Data.SingleName;
           Result := Result + ' [' + Data.Short + ']';
         end;
       end;
     Result := Result + '.';
   end;
  // Skills
   if (Parts and U_SKILLS) <> 0 then begin
     if AUnit.Skills.Count > 0 then begin
       SetLength(A, AUnit.Skills.Count);
       for i := 0 to AUnit.Skills.Count-1 do
         with AUnit.Skills[i] do begin
           A[i] := Data.Name + ' [' + Data.Short + '] ' +
             IntToStr(Level) + ' (' + IntToStr(Points) + ')';
         end;
       Result := Result + ' ' + Keys[s_Skills] + MakeList(A);
     end
     else Result := Result + ' ' + Keys[s_Skills] + Keys[s_none] + '.';
   end;
  // Combat spell
   if ((Parts and U_COMBAT) <> 0) and (AUnit.CombatSpell <> nil) then begin
     Result := Result + ' ' + Keys[s_CombatSpell] + AUnit.CombatSpell.Name +
       ' [' + AUnit.CombatSpell.Short + '].';
   end;
  // CanStudy
   if (Parts and U_CANSTUDY) <> 0 then begin
     if AUnit.CanStudy.Count > 0 then begin
       SetLength(A, AUnit.CanStudy.Count);
       for i := 0 to AUnit.CanStudy.Count-1 do
         with AUnit.CanStudy[i] do begin
           A[i] := Name + ' [' + Short + ']';
         end;
       Result := Result + ' ' + Keys[s_CanStudy] + MakeList(A);
     end
   end;
 // Description
  if (Parts and U_DESCR) <> 0 then begin
    i := Length(Result);
    while (i > 0) and ((Result[i] = ' ') or (Result[i] = '.')) do Dec(i);
    s := StripTags(AUnit.Description);
    if Trim(s) <> '' then s := '; ' + s;
    Result := Copy(Result, 1, i) + s + '.';
  end;
 // Remove first comma
  if (Length(Result) >= 2) and ((Result[1] = ' ') or (Result[2] = ' ')) then
    Result := Copy(Result, Pos(' ', Result) + 1, Length(Result));
end;

// Make order file
procedure CompileOrder(Lines: TStrings; Compact: boolean);
var i, k, n: integer;
    s, s1: string;
    U: TUnit;
    R: TRegion;
    CompactRepeating: boolean;

  function CompactOrder(Order, s: string): string;
  const CnvItems: array[0..9] of string = ('give', 'produce', 'buy', 'sell',
          'steal', 'claim', 'exchange', 'armor', 'weapon', '@;needs');
        CnvSkills: array[0..3] of string = ('study', 'cast', 'combat', 'forget');
  var i, j: integer;
      t: string;
      IData: TItemData;
      SData: TSkillData;
  begin
    // Strip comments
    s := TrimLeft(s);
    if Pos('@;', s) = 1 then s := '@;' + Uncomment(Copy(s, 3, Length(s)))
    else s := Uncomment(s);

    // For non-repeating orders, convert names to short names
    if CompactRepeating or (Pos('@', s) <> 1) then begin
      // Items
      i := 0;
      while (i < Length(CnvItems)) and (CnvItems[i] <> Order) do Inc(i);
      if i < Length(CnvItems) then begin
        s1 := '';
        j := 1;
        while j <= Length(s) do begin
          t := StrQuotes(s, j, False, ['"']);
          if (Copy(t, 1, 1) = '"') and (Copy(t, Length(t), 1) = '"') then begin
            IData := Game.ItemData.FindByName(Copy(t, 2, Length(t) - 2));
            if IData <> nil then t := IData.Short;
          end;
          s1 := s1 + t;
        end;
        s := s1;
      end
      else begin
        // Skills
        i := 0;
        while (i < Length(CnvSkills)) and (CnvSkills[i] <> Order) do Inc(i);
        if i < Length(CnvSkills) then begin
          s1 := '';
          j := 1;
          while j <= Length(s) do begin
            t := StrQuotes(s, j, False, ['"']);
            if (Copy(t, 1, 1) = '"') and (Copy(t, Length(t), 1) = '"') then begin
              SData := Game.SkillData.FindByName(Copy(t, 2, Length(t) - 2));
              if SData <> nil then t := SData.Short;
            end;
            s1 := s1 + t;
          end;
          s := s1;
        end;
      end;
    end;

    Result := Trim(s);
  end;

begin
  CompactRepeating := Config.ReadBool('MainWin', 'CompactRepeating', False);
  Game.SaveVTurnOrders;
  Lines.Add('; ' + TurnToDate(Turn.Num));
  Lines.Add('#atlantis ' + IntToStr(Faction.Num) + ' "' + Game.Password + '"');
  Lines.Add('');
  for n := 0 to Turn.Regions.Count-1 do begin
    R := Turn.Regions[n];

    // Region comment
    if not Compact then begin
      Lines.Add('');
      s := '; ' + MakeRegionName(R.Coords, False);
      if R.Settlement <> '' then s := s + ', ' + R.Settlement;
      Lines.Add(s);
      Lines.Add('');
    end;

    if R.PlayerTroop = nil then Continue;
    for i := 0 to R.PlayerTroop.Units.Count-1 do begin
      U := R.PlayerTroop.Units[i];

      // Unit
      s := 'unit ' + U.NumStr;
      if not Compact then s := s + '; ' + U.Name;
      Lines.Add(s);
      for k := 0 to U.Orders.Count-1 do begin
        if Compact then s := CompactOrder(U.Order(k), U.Orders[k])
        else s := U.Orders[k];
        if s <> '' then Lines.Add(TrimLeft(s));
      end;
      Lines.Add('');
    end;
  end;
  Lines.Add('#end');
end;

procedure AddMapHeader(Lines: TStrings);
begin
  Lines.Clear;
  Lines.Add('; Map for ' + Game.Name + ', ' + TurnToDate(Turn.Num));
  Lines.Add('; Generated by Atlantis Advisor');
  if not ProgOpened then Lines[1] := Lines[1] + ' (Unregistered)';
  Lines.Add('');
end;

procedure CompileRegion(Lines: TStrings; ARegion: TRegion; Parts: DWord);
var i, j: integer;
    s: string;
    C: TCoords;
    R: TRegion;
    transport: boolean;

  procedure Wrap(s: string; Lines: TStrings);
  var i: integer;
      st1: string;
  begin
    st1 := '  ';
    i := 1;
    while s[i] = ' ' do begin
      st1 := st1 + ' ';
      Inc(i);
    end;
    while Trim(s) <> '' do begin
      i := 71;
      while (Length(s) > i) and (s[i] <> ' ') do Dec(i);
      if Length(s) > i then Lines.Add(Copy(s, 1, i-1))
      else Lines.Add(s);
      s := st1 + Copy(s, i+1, Length(s));
    end;
  end;

  function MakeItemList(Items: TItemList; Costs: boolean; HideAdv: boolean): string;
  var i: integer;
  begin
    Result := '';
    for i := 0 to Items.Count-1 do
      with Items[i] do
        if not (Test(Items[i].Data.Flags, IT_ADVANCED) and HideAdv) then begin
          if Result <> '' then Result := Result + ', ';
          if Amount = -1 then Result := Result + 'unlimited ' + Name
          else Result := Result + IntToStr(Amount) + ' ' + Name;
          Result := Result + ' [' + Data.Short + ']';
          if Costs then Result := Result + ' at $' + IntToStr(Cost);
        end;
    if Result = '' then Result := 'none';
    Result := Result + '.';
  end;

  function RegionID(R: TRegion): string;
  begin
    Result := MakeRegionName(R.Coords, False);
    if R.Settlement <> '' then
      FmtStr(Result, '%s, contains %s [%s]', [Result, R.Settlement,
        GetKey(s_Village, R.SettlementType - 1)]);
  end;

  procedure WriteUnits(ARegion: TRegion; Struct: TStruct);
  var first, player: boolean;
      i, j: integer;
      s, pref: string;
      LFac: TTroop;
  begin
    first := (Struct = nil);
    if Struct <> nil then s := '  ' else s := '';
    for i := 0 to ARegion.Troops.Count-1 do begin
      LFac := ARegion.Troops[i];
      player := (LFac.Faction.Num = VFaction.Num);
      if (Test(Parts, MAP_PLAYERUNITS) and player)
        or (Test(Parts, MAP_NONPLAYERUNITS) and not player) then
        for j := 0 to LFac.Units.Count-1 do
          if LFac.Units[j].Struct = Struct then begin
            if first then Lines.Add('');
            first := False;
            if player then pref := '* '
            else pref := '- ';
            if player and Test(Parts, MAP_PLAYERDETAILS) then
              Wrap(s + pref + UnitString(LFac.Units[j], U_ALL), Lines)
            else Wrap(s + pref + UnitString(LFac.Units[j], U_NAME +
              U_FACTION + U_FLAGS + U_ITEMS + U_VISIBLE + U_DESCR), Lines);
          end;
    end;
  end;

begin
  if ARegion = nil then Exit;
  LastMapParts := Parts;
  with ARegion do begin
    if not FullData then Exit;
    // To hell all the keys, will import in English format
    // Header
    s := RegionID(ARegion);
    if Peasants <> nil then
      FmtStr(s, '%s, %d peasants (%s), $%d', [s, Peasants.Amount,
        Peasants.Name, TaxRate]);
    Wrap(s + '.', Lines);
    s := '------------------------------------------------------------;';
    s := s + IntToStr(Turn.Num);
    if Visited <> Turn.Num then s := s + '-' + IntToStr(Visited);
    Lines.Add(s);
    // Weather
    if (WeatherLast <> nil) and (WeatherNext <> nil) then
      s := '  ' + WeatherLast.LastText + ' was ' +
        WeatherLast.Text + ' last month; ' + WeatherNext.NextText + ' will be ' +
        WeatherNext.Text + ' next month.'
    else s := '  ' + Game.WeatherData[0].LastText + ' was ' +
        Game.WeatherData[0].Text + ' last month; ' + Game.WeatherData[0].NextText + ' will be ' +
        Game.WeatherData[0].Text + ' next month.';
    Wrap(s, Lines);
    // Wages: $14 (Max: $2158).
    FmtStr(s, '  Wages: $%d (Max: $%d).', [Wages, MaxWages]);
    Lines.Add(s);
    // Wanted: none.
    Wrap('  Wanted: ' + MakeItemList(Wanted, True, False), Lines);
    // For Sale: 154 plainsmen [PLAI] at $56, 30 leaders [LEAD] at $112.
    Wrap('  For Sale: ' + MakeItemList(ForSale, True, False), Lines);
    // Entertainment available: $154.
    FmtStr(s, '  Entertainment available: $%d.', [Entertainment]);
    Lines.Add(s);
    // Products: 71 livestock [LIVE], 20 horses [HORS].
    Wrap('  Products: ' + MakeItemList(Products, False,
      not Test(Parts, MAP_ADVRESOURCES)), Lines);
    Lines.Add('');
    // Exits
    Lines.Add('Exits:');
    for i := 1 to 6 do
      if HasExit[i] then begin
        R := RegionInDir(Coords, i);
        if R <> nil then
          Wrap('  ' + GetKey(s_North, i - 1) + ' : ' +
            RegionID(R) + '.', Lines);
      end;

    // Gates
    if Test(Parts, MAP_GATES) and (Gate > 0) then begin
      Lines.Add('');
      Lines.Add('There is a Gate here (Gate ' + IntToStr(Gate) + ' of ' +
        IntToStr(Turn.GateCount) + ').');
    end;

    // Units
    WriteUnits(Map.Region(x, y, z), nil);

    // Structs
    for i := 0 to Structs.Count - 1 do begin
      transport := Test(Structs[i].Data.Flags, ST_TRANSPORT);
      if (Test(Parts, MAP_TRANSPORTS) or not transport)
        and (Test(Parts, MAP_BUILDINGS) or transport) then begin
        Lines.Add('');
        s := Structs[i].Data.Group;
        if Test(Structs[i].Data.Flags, ST_ROAD) then
          for j := 1 to 6 do
            if EqualCoords(CoordsInDir(DataStructs.Coords(x, y, z), j),
              Structs[i].Passage) then s := s + ' ' + GetKey(s_N, j - 1);
        FmtStr(s, '+ %s [%d] : %s', [Structs[i].Name, Structs[i].Num, s]);
        if Test(Structs[i].Data.Flags, ST_CLOSED) then
          s := s + ', closed to player units';
        if Test(Structs[i].Data.Flags, ST_SHAFT) then begin
          s := s + ', contains an inner location';
          if Structs[i].HasExit then begin
            C := Structs[i].Passage;
            if Map.Region(C) <> nil then
              s := s + '; ;!' + MakeRegionName(C, False);
          end;
        end;
        s := s + '.';
        Wrap(s, Lines);
        WriteUnits(Map.Region(x, y, z), Structs[i]);
      end;
    end;
    Lines.Add('');
    Lines.Add('');
  end;
end;

procedure CompileCommonMap(Lines: TStrings; Parts: DWord);
var x, y, z: integer;
    R: TRegion;
begin
  AddMapHeader(Lines);
  for z := 0 to Map.Levels.Count-1 do
    for x := Map.Levels[z].Bounds.Left to Map.Levels[z].Bounds.Right do
      for y := Map.Levels[z].Bounds.Top to Map.Levels[z].Bounds.Bottom do begin
        R := Map.Region(x, y, z, Turn.Num);
        if (R <> nil) and (R.FullData) then
          CompileRegion(Lines, R, Parts);
      end;
end;

procedure CompileExportSkills(U: TUnit; Lines: TStrings);
var i: integer;
    s: string;
begin
  // !345381:345
  // FORC 120, FIRE 30, SPIR 90, NECR 90, PATT 60
  // @study GATE
  Lines.Add('!' + U.NumStr + ':' + IntToStr(U.Faction.Num));
  s := '';
  for i := 0 to U.Skills.Count-1 do begin
    if s <> '' then s := s + ', ';
    s := s + U.Skills[i].Data.Short + ' ' + IntToStr(U.Skills[i].Points);
  end;
  if s <> '' then Lines.Add(s);
  if ClearOrder(U.MonthOrder) = 'study' then
    Lines.Add(U.MonthOrder);
  Lines.Add('');
end;

function ImportSkills(Lines: TStrings): string;
var i, num: integer;
    Trace: TTrace;
    Fac: TFaction;
    U: TUnit;
    Skill: TSkill;
    SData: TSkillData;
begin
  Result := '';
  i := -1;
  while i < Lines.Count-1 do begin
    Inc(i);
    if Copy(Lines[i], 1, 1) = '!' then begin
      Trace := TTrace.Create(Lines[i]);
      try
        // !345381:345
        Inc(Trace.StPos);
        num := StrToInt(Trace.Before(':'));
        Fac := Turn.Factions.Find(Trace.Num);
        if Fac = nil then Fac := Turn.Factions[0];
        U := Fac.Units.Find(num);
        if (U = nil) and (Fac.Num = 0) then Continue
        else begin
          U := Turn.Factions[0].Units.Find(num);
          if U = nil then Continue;
          U.ChangeFaction(Fac.Num);
        end;
        if (U.Faction.Player) or Test(U.Marks, UM_ALLY) then begin
          Result := Result + U.FullName + ' already known from report; ';
          Continue;
        end;
        Inc(i);
        // FORC 120, FIRE 30, SPIR 90, NECR 90, PATT 60
        if (i < Lines.Count) and not EmptyLine(Lines[i]) then begin
          Trace.Text := Lines[i];
          U.Skills.ClearItems;
          while not Trace.Ends do begin
            SData := Game.SkillData.Find(Trace.Before(' '));
            if SData <> nil then begin
              Skill := TSkill.Create;
              Skill.Data := SData;
              Skill.Points := Trace.Num;
              Skill.Level := LevelByPoints(Skill.Points);
              U.Skills.Add(Skill);
            end;
            Trace.Block;
          end;
          Inc(i);
        end;
        // @study GATE
        if (i < Lines.Count) and not EmptyLine(Lines[i]) then begin
          U.Orders.Add(Lines[i]);
          U.MonthOrder := Lines[i];
        end;

        SetFlag(U.Marks, UM_ALLY);
        Result := Result + U.FullName + ' imported; ';
      finally
        Trace.Free;
      end;
    end;
  end;
  Result := Copy(Result, 1, Length(Result) - 2);
end;

end.
