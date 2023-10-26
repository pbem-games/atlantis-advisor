{ Report file read procedures }

unit RepRead;

interface

uses
  Classes, SysUtils, Math, Types, DataStructs, Resources;

const
  rrsNo = 0;
  rrsOK = 1;
  rrsFailed = 2;
  rrsErrors = 3;

type
  EEndOfRep = class(Exception);
  EBlockError = class(Exception);
  EWrongRep = class(Exception);

  TRepReadStatus = integer;

var
  RepPos: integer;
  RepLines: TStrings;

  function ReadRep(Report: TStrings; Log: TStrings): TRepReadStatus;
  procedure RereadIncomplete;
  function ReadRepHeader(Report: TStrings; var FName: string; var FNum: integer;
   var TurnNum: integer): TRepReadStatus;
  procedure ReadOrders;
  function OrderTurn: integer;
  procedure ImportMap();

implementation

uses
  uKeys, MyStrings, AtlaDate;

type
  TFormFunc = function(s: string; List: TList): Pointer;
  EReportError = class(Exception);

var
  TurnFaction: TFaction;
  Log: TStrings;

function TraceKey(Trace: TTrace; Key: array of TKeyStrings): boolean;
var i: integer;
begin
  Result := False;
  i := Length(Key) - 1;
  while (i >= 0) and (Pos(Keys[Key[i]], Trace.Text) <> 1) do Dec(i);
  if i >= 0 then begin
    Trace.Before(Keys[Key[i]]);
    Trace.Separator := Keys[Key[i]];
    Result := True;
  end;
end;

// Decode comma-separated lists and get values, if they are not duplicated
// (duplicated values comes from duplicated reports for same turn)
procedure ReadBlockList(list: string; ToList: TList; Former: TFormFunc; SkipName: boolean);
var Trace:  TTrace;
    itm, delimeter: string;
    p: pointer;
    bkmark, delimeterIdx: integer;
begin
  Trace := TTrace.Create(list);
  if SkipName then Trace.Before(': ');

  repeat
    bkmark := Trace.StPos;
    itm := Trace.Block;

    delimeterIdx := Pos(Keys[s_And], itm);
    if delimeterIdx <> 0 then delimeter := Keys[s_And]
    else begin
      delimeterIdx := Pos(Keys[s_Or], itm);
      if delimeterIdx <> 0 then delimeter := Keys[s_Or];
    end;

    // we need to check that and/or is not part of item name
    if (delimeterIdx > 0) and ((delimeterIdx = 1) or (itm[delimeterIdx - 1] = ' ')) then
    begin
      Trace.StPos := bkmark;
      itm := Trace.Before(delimeter);
    end;

    if itm <> Keys[s_None] then begin
      p := Former(itm, ToList);
      if p <> nil then ToList.Add(p)
    end;

    TraceKey(Trace, [s_And, s_Or]);
  until Trace.Ends;

  Trace.Free;
end;

// Read current report line
function GetLine: string;
begin
 if (RepPos < RepLines.Count)
  then GetLine := RepLines[RepPos]
  else raise EEndOfRep.Create('Read after end of report');
end;

// Navigating over report
procedure SeekStart;
begin
 RepPos := 0;
end;

procedure NextLine;
begin
 Inc(RepPos);
end;

procedure PrevLine;
begin
 Dec(RepPos);
end;

// Multiline is starting with small indent, continued with larger
// indent and ends when encountered starting indent or empty line.
// Assiming it cannot contain comments.

function GetMultiline: string;
var ls1: integer;
begin
 Result := TrimRight(GetLine);
 ls1 := LeadingSpaces(Result);
 NextLine;
 while (LeadingSpaces(GetLine) > ls1) and not EmptyLine(GetLine) do begin
  Result := Result + ' ' + Trim(GetLine);
  NextLine;
 end;
end;

// Seek until we got line w/ given text. If not found, do not move RepPos.
function SeekLineContain(txt: string): boolean;
begin
  Result := SeekText(RepLines, RepPos, txt);
end;


 { Forming of Data Structures }

// Update existing faction (by number) or form new one.
function FormFaction(fact_string: string): TFaction;
var Trace: TTrace;
    name: string;
begin
  // Forces (564)
  Trace := TTrace.Create(fact_string);
  name := Trace.Before(' (');
  Result := Turn.Factions.Seek(Trace.Num);
  Result.Name := name;
  Trace.Free;
end;

// Form item data or create new record (by short)
function FormItemData(itm_data: string; Amount: integer): TItemData;
var Trace: TTrace;
    name: string;
begin
  // leader [LEAD]
  Trace := TTrace.Create(itm_data);
  name := Trace.Before(' [');
  Result := Game.ItemData.Seek(Trace.Before(']'));
  if Amount = 1 then Result.SingleName := name
  else Result.MultiName := name;
  Trace.Free;
end;

// Make new item
function FormItem(itm_string: string): TItem;
var
  Trace: TTrace;
  tmp: string;
  sPos, ePos: integer;

begin
  Result := TItem.Create;
  Trace := TTrace.Create(itm_string);

 // unlimited sea elves [SELF] at $68
 // 31 horses [HORS]
 // leader [LEAD]
 //
 // and now ship items from Atlantis 5
 //   unfinished Cog [COG] (needs 10)
 //   unfinished Longship [LONG] (needs 2)
  if (itm_string[1] >= '0') and (itm_string[1] <= '9')
    then Result.Amount := Trace.Num
  else begin
    if Pos(Keys[s_Unlimited], itm_string) = 1 then Result.Amount := -1
    else Result.Amount := 1;
  end;
  if (Result.Amount <> 1) then Trace.Before(' ');

  if Pos(Keys[s_Unfinished], itm_string) = 1 then begin
    Trace.Before(Keys[s_Unfinished]);
    sPos := Trace.StPos;
    ePos := Trace.EnPos;

    Trace.Before(Keys[s_Needs]);
    Result.Needs := Trace.Num;

    Trace.StPos := sPos;
    Trace.EnPos := ePos;
    tmp := Trace.Before(' (');
  end
  else if Pos('$', itm_string) > 0 then begin
    tmp := Trace.Before('$');
    Result.Cost := Trace.Num;
  end
  else begin
    tmp := Trace.Text;
    Result.Cost := 0;
  end;

  Result.Data := FormItemData(tmp, Result.Amount);

  Trace.Free;
end;


// Update existing Region (by coords) or form new one.
// Full means we reading regions info.
function FormRegion(desc_string: string; Full: boolean): TRegion;
var Trace: TTrace;
    i,x,y,z: integer;
    s, terrain: string;
    Lv: TMapLevel;
begin
  // ocean (21,43) in Atlantis Ocean
  // tunnels (49,11,underworld) in Karabyaka
  // plain (39,63) in Antor, contains Barika [city].
  Trace := TTrace.Create(desc_string);
  terrain := Trace.Before(' (');
  x := Trace.Num;
  Trace.Before(',');
  y := Trace.Num;
  if Pos(')', Trace.Text) = 1 then begin
    Lv := Map.Levels.Seek(Keys[s_Surface]);
  end
  else begin
    Trace.Before(',');
    Lv := Map.Levels.Seek(Trace.Before(')'));
  end;
  z := Map.Levels.IndexOf(Lv);
  // Always create Full (re-readed) regions, seek not Full (referenced)
  if Full then begin
    if Map.TurnNum < Turn.Num then begin
      // Overwrite not Full region for this turn if found Full for previous
      Result := Map.Region(x, y, z, Turn.Num);
      // Leave Full with no changes, create underlying
      if (Result = nil) or Result.FullData then
        Result := Map.SeekRegion(x, y, z);
    end
    else Result := Map.SeekRegion(x, y, z);
  end
  else begin
    Result := Map.Region(x, y, z);
    if Result = nil then Result := Map.SeekRegion(x, y, z);
  end;
  // Set data for Full or newly-created regions
  if Full or (Result.Terrain = nil) then begin
    Trace.Before(Keys[s_In]);
    Result.Terrain := Game.TerrainData.Seek(terrain);
    Result.Land := Trace.Block;
    Result.FullData := Full;
  end;

  // contains Kashmar [city]
  if Pos(Keys[s_Contains], Trace.Text) > 0 then begin
   Trace.Before(Keys[s_Contains]);
   Trace.Backwards(']');
   s := Trace.Backwards(' [');
   Result.Settlement := Trace.Text;
   for i := 1 to 3 do
     if GetKey(s_Village, i - 1) = s then Result.SettlementType := i;
  end;
 Trace.Free;
end;

// Form skill data or update it by name and level.
function FormSkillData(skill_string: string): TSkillData;
var Trace: TTrace;
    name, short: string;
begin
 // magical healing [MHEA]
  Trace := TTrace.Create(skill_string);
  name := Trace.Before(' [');
  short := Trace.Before(']');
  Result := Game.SkillData.Seek(short);
  Result.Name := name;
  Trace.Free;
end;

function FormSkill(skill: string): TSkill;
var Trace: TTrace;
begin
  Result := TSkill.Create;

  // pattern [PATT] 1 (30)
  Trace := TTrace.Create(skill);
  Result.Data := FormSkillData(Trace.Before('] ') + ']');
  if not Trace.Ends then Result.Level := Trace.Num;
  if Pos(' (', Trace.Text) = 1 then begin
    Trace.Before('(');
    Result.Points := Trace.Num;
  end;
  Trace.Free;
end;

// Some wrappers for list reading
function ItemFormer(s: string; List: TList): Pointer;
var Item, Ex: TItem;
begin
  Item := FormItem(s);
  Ex := TItemList(List).Find(Item.Data.Short);
  if (Ex <> nil) then begin
    Ex.Assign(Item);
    FreeAndNil(Item);
  end;
  Result := Pointer(Item);
end;

function ItemDataFormer(s: string; List: TList): Pointer;
var IData: TItemData;
begin
  IData := FormItemData(s, 2);
  if List.IndexOf(IData) = -1 then Result := IData
  else Result := nil;
end;

function ItemNameDataFormer(s: string; List: TList): Pointer;
begin
  Result := Game.ItemData.FindByName(s);
end;

function StructDataFormer(s: string; List: TList): Pointer;
var StData: TStructData;
begin
  StData := Game.StructData.Seek(s);
  if List.IndexOf(StData) = -1 then Result := StData
  else Result := nil;
end;

function EffectFormer(s: string; List: TList): Pointer;
var Eff: TEffectData;
begin
  Eff := Game.EffectData.Seek(s);
  if List.IndexOf(Eff) = -1 then Result := Eff
  else Result := nil;
end;

function SkillFormer(s: string; List: TList): Pointer;
var i: integer;
    Skill: TSkill;
begin
  Skill := FormSkill(s);
  i := 0;
  // Duplicate: Data, Level, Points.
  while (i < List.Count) and not ((TSkill(List[i]).Data = Skill.Data) and
    (TSkill(List[i]).Level = Skill.Level) and (TSkill(List[i]).Points = Skill.Points))
    do Inc(i);
  if i < List.Count then begin
    Skill.Free;
    Result := nil;
  end
  else Result := Pointer(Skill);
end;

function SkillDataFormer(s: string; List: TList): Pointer;
begin
  Result := Pointer(FormSkillData(s));
end;


// Atlantis Report For:  (player's faction, date)
procedure ReadHeader(var FName: string; var FNum: integer; var TurnNum: integer);
var Trace: TTrace;
    month: string;
    year: integer;
begin
  while Uncomment(GetLine) = '' do NextLine;
  NextLine;
  Trace := TTrace.Create(GetLine);
  // December, Year 7
  month := Trace.Before(', ' + Keys[s_Year] + ' ');
  year := Trace.Num;
  TurnNum := MonthYearToTurn(month, year);
  PrevLine;
  // Forces of Raxxla (564) (War 1, Trade 1, Magic 1)
  // Conquest: when starting, Faction 2 (exactly 'Faction', without brackets)
  Trace.Text := GetLine;
  if Pos('(', Trace.Text) > 0 then
    FName := Trace.Before(' (') // Standard
  else begin // Conquest "Faction 2"
    FName := Trace.Text;
    Trace.Before(' ');
  end;
  FNum := Trace.Num;
  Trace.Free;
end;

procedure ReadFP;   
var Trace: TTrace;
begin
  // Forces of Raxxla (564) (War 1, Trade 1, Magic 1)
  Trace := TTrace.Create(GetLine);
  if Pos(Keys[s_Martial], Trace.Text) > 0 then
  begin
    Trace.Before(Keys[s_Martial]);
    Turn.Martial := Trace.Num;
  end;
  if Pos(Keys[s_War], Trace.Text) > 0 then begin
    Trace.Before(Keys[s_War]);
    Turn.War := Trace.Num;
  end;
  if Pos(Keys[s_Trade], Trace.Text) > 0 then begin
    Trace.Before(Keys[s_Trade]);
    Turn.Trade := Trace.Num;
  end;
  if Pos(Keys[s_Magic], Trace.Text) > 0 then begin
    Trace.Before(Keys[s_Magic]);
    Turn.Magic := Trace.Num;
  end;
  Trace.Free;
  Turn.Events.Add(GetLine);
end;

// Faction status:
procedure ReadStatus;
var Trace: TTrace;
    s, report: string;
begin
  NextLine;
  Trace := TTrace.Create('');
  report := '';
  while not EmptyLine(GetLine) do begin
    Trace.Text := GetLine;
    report := report + Trace.Text + '. ';
    s := Trace.Before(': ');
    if Pos(Keys[s_TaxRegions], s) > 0 then begin
      Turn.FpUsed[prWar] := Trace.Num;
      Trace.Before('(');
      SetProgress(prWar, Turn.War, Trace.Num);
    end
    else if Pos(Keys[s_TradeRegions], s) > 0 then begin
      Turn.FpUsed[prTrade] := Trace.Num;
      Trace.Before('(');
      SetProgress(prTrade, Turn.Trade, Trace.Num);
    end
    else if Pos(Keys[s_Mages], s) > 0 then begin
      Turn.FpUsed[prMagic] := Trace.Num;
      Trace.Before('(');
      SetProgress(prMagic, Turn.Magic, Trace.Num);
    end
    else if Pos(Keys[s_Apprentices], s) > 0 then begin
      Turn.FpUsed[prAppr] := Trace.Num;
      Trace.Before('(');
      SetProgress(prAppr, Turn.Magic, Trace.Num);
    end
    else if Pos(Keys[s_FishRegions], s) > 0 then begin
      Turn.FpUsed[prFish] := Trace.Num;
      Trace.Before('(');
      SetProgress(prFish, Turn.Trade, Trace.Num);
    end
    else if Pos(Keys[s_RoadRegions], s) > 0 then begin
      Turn.FpUsed[prRoad] := Trace.Num;
      Trace.Before('(');
      SetProgress(prRoad, Turn.Trade, Trace.Num);
    end;
    NextLine;
  end;
  Trace.Free;
  Turn.Events.Add(report);
end;

function EventHeader(Lv: string; Text: string): string;
begin
  Result := '!H' + Lv + ' ' + Text;
  if not TurnFaction.Player then Result := Result + ' (from ' +
    TurnFaction.Name + ')';
end;

procedure DoReadBattle(B: TBattle);
var Trace: TTrace;
    Round: TRound;
    i, j, num, pow: integer;
    s: string;
    U, U1: TBattleUnit;
    Action: TBattleAction;
    side, indecisive: boolean;

  function FormBattleUnit(str: string; Side: boolean): TBattleUnit;
  var Trace, Trc: TTrace;
      s: string;
      i: integer;
      Item: TItem;
      Skill: TSkill;
      SData: TSkillData;
  begin
    // General Lando (24), Forest Owls (494), behind, man [MAN], tactics 5, combat 1.
    // Professionals (658), behind, 10 leaders [LEAD], 10 longbows [LBOW], 14
    //   horses [HORS], longbow 5.
    // Great Apes (32481), Creatures (2), kong [KONG] (Combat 4/4, Attacks
    //   25, Hits 25, Tactics 0).
    Trace := TTrace.Create(str);
    Result := TBattleUnit.Create;
    // Name, Num
    Result.Name := Trace.Before(' (');
    Result.Num := Trace.Num;
    Trace.Before(' ');
    while not Trace.Ends and (Trace.Separator <> ';') do begin
      s := Trace.Block;
      // (Combat 4/4, Attacks 25, Hits 25, Tactics 0)
      if (Pos('(', s) > 0) and (Pos(')', s) = 0) then begin
        s := s + ', ' + Trace.Before(')') +')';
        Trace.Block;
      end;
      // faction
      if (Pos('(', s) > 0) and (s[Pos('(', s)+1] in ['0'..'9']) then
        Result.Faction := FormFaction(s)
      // behind
      else if Pos(Keys[s_flgBehind], s) = 1 then
        Result.Behind := True
      // items
      else if Pos('[', s) > 0 then begin
        Item := FormItem(s);
        Result.Items.Add(Item);
        // (Combat 4/4, Attacks 25, Hits 25, Tactics 0)
        // (Combat 3, Hits 1)
        if Pos('(', s) > 0 then
          with Item.Data do begin
            SetFlag(Flags, IT_MONSTER, True);
            Trc := TTrace.Create(s);
            // Combat
            if Pos(Keys[s_MonCombat], Trc.Text) > 0 then begin
              Trc.Before(Keys[s_MonCombat]);
              if Monster.Attack = 0 then Monster.Attack := Trc.Num;
              Skill := Result.Skills.Seek(Keys[s_Combat]);
              Skill.Level := Monster.Attack;
              if Monster.Defence[0] = 0 then begin
                if Pos('/', Trc.Text) = 1 then begin
                  Trc.Before('/');
                  Monster.Defence[0] := Trc.Num;
                end
                else Monster.Defence[0] := Monster.Attack;
                for i := 1 to atCount-1 do
                  Monster.Defence[i] := Monster.Defence[0];
              end;
            end;
            // Attacks
            if Pos(Keys[s_MonAttacks], Trc.Text) > 0 then begin
              Trc.Before(Keys[s_MonAttacks]);
              Monster.NumAttacks := Trc.Num;
            end;
            // Hits
            if Pos(Keys[s_MonHits], Trc.Text) > 0 then begin
              Trc.Before(Keys[s_MonHits]);
              Monster.Hits := Trc.Num;
            end;
            // Tactics
            if Pos(Keys[s_MonTactics], Trc.Text) > 0 then begin
              Trc.Before(Keys[s_MonTactics]);
              Monster.Tactics := Trc.Num;
              if Monster.Tactics > 0 then begin
                Skill := Result.Skills.Seek(Keys[s_Tactics]);
                Skill.Level := Monster.Tactics;
              end;
            end;
            Trc.Free;
          end;
      end
      // skills
      else begin
        i := Length(s);
        while (i >= 1) and (s[i] <> ' ') do Dec(i);
        SData := Game.SkillData.FindByName(Copy(s, 1, i-1));
        if SData <> nil then begin
          Skill := Result.Skills.Seek(SData.Short);
          Skill.Level := StrToInt(Copy(s, i+1, Length(s)));
        end;
      end;
      // Description
      if Trace.Separator = ';' then begin
        if Trace.Text[Length(Trace.Text)] = '.' then Trace.Backwards('.');
        Result.Description := Trace.Text;
        Break;
      end;
    end;
    Trace.Free;
    // Reference to unit in game
    with Result do begin
      if Faction <> nil then URef := Faction.Units.Find(Num);
      if URef = nil then URef := Turn.FindUnit(Num);
      if URef <> nil then begin
        SetFlag(URef.Marks, UM_BATTLE, True);
        // Set BattleUnit faction if it was unknown
        if Faction = nil then Faction := URef.Faction;
        // Set game Unit faction if it was recovered during battle
        if (URef.Faction.Num = 0) and (Faction.Num <> 0) then
          URef.ChangeFaction(Faction.Num);
      end;
      if Faction = nil then Faction := Turn.Factions[0];
    end;
    Result.Side := Side;
  end;

  function GetNextLine: string;
  begin
    Result := GetMultiline;
    B.Report.Add(Result);
  end;

  procedure ReadSide(Side: boolean);
  var U, Mon: TBattleUnit;
      i: integer;
      Skill: TSkill;
  begin
    GetNextLine;
    while not EmptyLine(GetLine) do begin
      U := FormBattleUnit(GetNextLine, Side);

      // Form standalone controlled monster units
      if U.Items.Amount(IT_MAN) > 0 then begin
        i := 0;
        while (i < U.Items.Count) do
          if Test(U.Items[i].Data.Flags, IT_MONSTER) then begin
            Mon := TBattleUnit.Create;
            Mon.Behind := False;
            Mon.Num := U.Num;
            Mon.Name := UpperCase(Copy(U.Items[i].Name, 1, 1)) +
              Copy(U.Items[i].Name, 2, Length(U.Items[i].Name)) +
              ' of ' + U.Name;
            Mon.Faction := U.Faction;
            Mon.Struct := U.Struct;
            Mon.Damaged := U.Damaged;
            Mon.Side := U.Side;
            Mon.Items.Add(U.Items[i]);
            with U.Items[i].Data do begin
              if Monster.Tactics > 0 then begin
                Skill := Mon.Skills.Seek(Keys[s_Tactics]);
                Skill.Level := Monster.Tactics;
              end;
              if Monster.Attack > 0 then begin
                Skill := Mon.Skills.Seek(Keys[s_Combat]);
                Skill.Level := Monster.Attack;
              end;
            end;
            U.Items.Delete(i);
            B.Units[Side].Add(Mon);
          end
          else Inc(i);
      end;

      B.Units[Side].Add(U);
    end;
    while EmptyLine(GetLine) do GetNextLine;
  end;

  function IsMonster(U: TBattleUnit): boolean;
  begin
    Result := (U.Items.Amount(IT_MONSTER) > 0) and
      (U.Items.Amount(IT_MAN) = 0);
  end;

begin
  indecisive := False;
  B.Report.Add(B.Name);
  while EmptyLine(GetLine) do GetNextLine;
  if Pos(Keys[s_Assassinated], B.Name) > 0 then Exit;

  // Attackers, Defenders
  ReadSide(sideAttack);
  ReadSide(sideDefence);

  // Rounds
  SetLength(B.Rounds, 0);
  while RepPos < RepLines.Count do begin
    s := GetNextLine;

    // skip statistics
    if Pos('statistics', s) > 0 then begin
      repeat s := GetNextLine until
          not EmptyLine(s)
        and (
          (Pos(Keys[s_FreeRound], s) > 0)
          or (Pos(Keys[s_Round], s) = 1)
          or (Pos(Keys[s_Casualities], s) = 1)
          or (Pos(Keys[s_BattleIndecisively], s) = 1)
        );
    end;
    
    // skip: Ogres (552) is routed!
    if (Pos(Keys[s_Routed], s) > 0) or (Pos(Keys[s_Destroyed], s) > 0) then
      repeat s := GetNextLine until not EmptyLine(s);

    // Unit (749) gets a free round of attacks.
    if Pos(Keys[s_FreeRound], s) > 0 then
      Round.Num := 0
    // Round 1:
    else if Pos(Keys[s_Round], s) = 1 then
      Round.Num := StrToInt(Copy(s, Pos(' ', s)+1, Pos(':', s) - Pos(' ', s)-1))
    // Total casualities:
    else if Pos(Keys[s_Casualities], s) = 1 then
      Round.Num := -1
    // The battle ends indecisively.
    else if Pos(Keys[s_BattleIndecisively], s) = 1 then begin
      indecisive := True;
      while EmptyLine(GetLine) do GetNextLine;
      Continue;
    end
    // loses 1.
    else if Pos(Keys[s_Loses], s) > 0 then begin
      Trace := TTrace.Create(s);

      U := nil;
      if Pos('(', Trace.Text) > 0 then begin
        Trace.Before('(');
        num := StrToInt(Trace.Before(')'));
        Trace.SkipSpaces;
        U := B.FindBUnit(num);
      end;

      if TraceKey(Trace, [s_Loses]) and (U <> nil) then begin
        if Round.Num >= 0 then begin
          pow := Trace.Num;
          if pow <> 0 then begin
            Action.ActionType := raSideLoses;
            Action.Power := pow;
            Action.BUnit := U;

            SetLength(Round.Actions, Length(Round.Actions) + 1);
            Round.Actions[High(Round.Actions)] := Action;
          end;
        end
        else begin
          B.Loses[U.Side] := Trace.Num;
        end;
      end;

      Trace.Free;

      while EmptyLine(GetLine) do GetNextLine;
      Continue;
    end
    // TakeHits: takes 1(/no) hits, bringing it to 47/50
    else if Pos(Keys[s_Takes], s) > 0 then begin
      Trace := TTrace.Create(s);

      U := nil;
      if Pos('(', Trace.Text) > 0 then begin
        Trace.Before('(');
        num := StrToInt(Trace.Before(')'));
        Trace.SkipSpaces;
        U := B.FindBUnit(num);
      end;

      if U <> nil then begin
        Action.ActionType := raTakeHits;
        Action.BUnit := U;

        Trace.Before('takes');
        Trace.SkipSpaces();

        if (Copy(Trace.Text, 1, 1) >= '0') and (Copy(Trace.Text, 1, 1) <= '9') then
          Action.Power := Trace.Num
        else Action.Power := 0;

        Trace.Free;
      end;

      while EmptyLine(GetLine) do GetNextLine;
      Continue;
    end
    else raise EReportError.Create('Can''t finish parsing: ' + B.Name);

    // Round actions
    SetLength(Round.Actions, 0);
    Trace := TTrace.Create('');
    while not EmptyLine(GetLine) do begin
      Trace.Text := GetNextLine;

      // Damaged units (in total casualities only)
      if TraceKey(Trace, [s_DamagedUnits]) then begin
        while not Trace.Ends do begin
          U1 := B.FindBUnit(StrToInt(Trace.Block));
          if U1 = nil then Continue;
          U1.Damaged := True;
          if U1.URef <> nil then SetFlag(U1.URef.Marks, UM_DEADMEN, True);
        end;
        Continue;
      end;

      // Read unit; if no unit found, get next line
      U := nil;
      if Pos('(', Trace.Text) > 0 then begin
        Trace.Before('(');
        num := StrToInt(Trace.Before(')'));
        Trace.SkipSpaces;
        U := B.FindBUnit(num);
      end
      else begin
        // Cloud Giant unleashes a mighty lightning strike, killing 1.
        side := sideAttack;
        repeat
          i := B.Units[side].Count-1;
          while (i >= 0) and (U = nil) do begin
            U1 := B.Units[side][i];
            Dec(i);
            if not IsMonster(U1) then Continue;
            j := U1.Items.Count-1;
            while (j >= 0) and (Pos(U1.Items[j].Data.SingleName,
              LowerCase(Trace.Text)) <> 1) do Dec(j);
            if j >= 0 then begin
              U := U1;
              Inc(Trace.StPos, Length(U1.Items[j].Data.SingleName) + 1);
            end;
          end;
          side := not side;
        until (side = sideAttack) or (U <> nil);
      end;
      if U = nil then Continue;
      Action.BUnit := U;

      // loses 1.
      if TraceKey(Trace, [s_Loses]) then begin
        if Round.Num >= 0 then begin
          pow := Trace.Num;
          if pow = 0 then Continue;
          Action.ActionType := raSideLoses;
          Action.Power := pow;
        end
        else begin
          B.Loses[U.Side] := Trace.Num;
          Continue;
        end;
      end

      // TakeHits: takes 1(/no) hits, bringing it to 47/50
      else if TraceKey(Trace, [s_Takes]) then begin
        Action.ActionType := raTakeHits;
        if (Copy(Trace.Text, 1, 1) >= '0') and (Copy(Trace.Text, 1, 1) <= '9') then
          Action.Power := Trace.Num
        else Action.Power := 0;
      end

      // Heals
      else if TraceKey(Trace, [s_Heals]) then begin
        Action.ActionType := raHeals;
        Action.Power := Trace.Num;
      end

      // tactics bonus 1
      else if TraceKey(Trace, [s_TacticsBonus]) then begin
        Action.ActionType := raTacticsBonus;
        Action.Power := Trace.Num;
      end

      // Cast: shoots a Fireball, killing 10.
      else begin
        Action.ActionType := raCast;
        // Find spell text
        Action.Special := Game.SpecData.FindByText(Trace.Before(', '));
        // Find any number to set as power
        while not Trace.Ends and not ((Copy(Trace.Text, 1, 1) >= '0')
          and (Copy(Trace.Text, 1, 1) <= '9')) do
          Inc(Trace.StPos);
        if not Trace.Ends then Action.Power := Trace.Num;
      end;

      // Add action to list
      SetLength(Round.Actions, Length(Round.Actions) + 1);
      Round.Actions[High(Round.Actions)] := Action;
    end;
    Trace.Free;

    // Add round to array
    SetLength(B.Rounds, Length(B.Rounds) + 1);
    B.Rounds[High(B.Rounds)] := Round;
    while EmptyLine(GetLine) do GetNextLine;

    // End if Spoils found
    if (Pos(Keys[s_Spoils], GetLine) = 1) or (Pos(Keys[s_NoSpoils],
      GetLine) = 1) then begin
      s := GetNextLine;
      Break;
    end;
    // End after Total Casualities in indecisive battle
    if indecisive and (Round.Num = -1) then Break;
  end;

  // Spoils
  if Pos(Keys[s_Spoils], s) = 1 then begin
    B.Spoils.ClearItems;
    ReadBlockList(s, B.Spoils, ItemFormer, True);
  end;

  // Skip empty
  while EmptyLine(GetLine) do NextLine;

  if Turn.Events.IndexOf(EventHeader('1', 'Battles during turn')) < 0 then
    Turn.Events.Add(EventHeader('1', 'Battles during turn'));
  Turn.Events.AddObject('! 1 ' + B.Name, B);
end;


 { Battles during turn: }
procedure ReadBattle;
var Trace: TTrace;
    NewRegion, Region: TRegion;
    s, desc: string;
    Battle: TBattle;
    i: integer;
begin
  NextLine;
  Trace := TTrace.Create('');

  // Start: Tribe of Centaurs (2114) attacks Unit (3418) in plain (30,44) in Fetlar!
  // End:   Spoils: 23 silver [SILV], wine [WINE], horse [HORS].
  //  ..or  Scout (4263) is assassinated in plain (19,43) in Corwen!

  s := GetMultiline;
  while (s <> '') and (s[Length(s)] = '!') do begin
    // Seek region from first string
    Trace.Text := s;
    if Pos(Keys[s_Assassinated], s) <> 0 then
      Trace.Before(Keys[s_Assassinated])
    else begin
      Trace.Before(') '); // attacker
      Trace.Before(') '); // defender
      Trace.Before(' '); // skip 'in'
    end;
    desc := Trace.Before('!');
    Region := FormRegion(desc, False);
    if Region.Visited <> Turn.Num then begin
      // Get region data from previous turns (in battle we can't see structs etc)
      NewRegion := FormRegion(desc, True);
      with NewRegion do begin
        Assign(Region, False);
        Visited := Turn.Num;
      end;
      Region := NewRegion;
    end;
    if Region.Battles = nil then Region.Battles := TBattleList.Create;

    // Create battle
    Battle := TBattle.Create(s);
    Battle.Region := Region;

    // Find same battle from ally
    i := Region.Battles.Count-1;
    while (i >= 0) and (Region.Battles[i].Name <> Battle.Name) do Dec(i);
    if (i >= 0) and TurnFaction.Player then begin
      if Turn.Events.IndexOfObject(Region.Battles[i]) >= 0 then
        Turn.Events.Delete(Turn.Events.IndexOfObject(Region.Battles[i]));
      Region.Battles[i].Free;
      Region.Battles[i] := Battle;
      DoReadBattle(Battle);
    end
    else if (i < 0) then begin
      Region.Battles.Add(Battle);
      DoReadBattle(Battle);
    end
    else begin
      DoReadBattle(Battle); // Skip battle if we seen it in active faction
      Battle.Free;
    end;

    s := GetMultiline;
  end;
  PrevLine;
  Trace.Free;
end;

procedure AddUnitEvent(s: string; AUnit: TUnit);
begin
  if (AUnit <> nil) or TurnFaction.Player then begin
    if (AUnit = nil) and (Turn.Events.IndexOf('!H7 Events from unknown units') = -1) then
      Turn.Events.Add('!H7 Events from unknown units');
    if (AUnit <> nil) and (Turn.Events.IndexOf('!H6 Errors during turn') = -1) then
      Turn.Events.Add('!H6 Errors during turn');
    Turn.Events.AddObject(s, AUnit);
  end;
end;

// Errors during turn
procedure ReadErrors;
var Trace: TTrace;
    AUnit: TUnit;
begin
  NextLine;
  // JIoxu Heoby4eHHbIe (6707): Can't do that in this region.
  while (RepPos < RepLines.Count) and not EmptyLine(GetLine) do begin
    Trace := TTrace.Create(GetMultiline);
    AUnit := nil;
    if Pos('): ', Trace.Text) > 0 then begin
      Trace.Before('(');
      AUnit := TurnFaction.Units.Find(Trace.Num);
      Trace.Before('): ');
      if AUnit <> nil then begin
        AUnit.Events.Add('!' + Trace.Text);
        if Pos(Keys[s_Starve], Trace.Text) > 0 then
          SetFlag(AUnit.Marks, UM_DEADMEN, True);
      end;
    end;
    Trace.Reset;
    if AUnit <> nil then AddUnitEvent('!G6 ' + Trace.Text, AUnit)
    else AddUnitEvent('!U7 ' + Trace.Text, nil);
    Trace.Free;
  end;
end;

// Events during turn
//   events are attached to player's units, other ones come to Turn.Events
procedure ReadEvents;
var Trace: TTrace;
    unum, i, amt: integer;
    header: boolean;
    AUnit: TUnit;

  function Shafts(R: TRegion; Unlinked: boolean): integer;
  var i: integer;
  begin
    Result := 0;
    for i := 0 to R.Structs.Count-1 do
      if Test(R.Structs[i].Data.Flags, ST_SHAFT)
        and not (Unlinked and R.Structs[i].HasExit) then
        Inc(Result);
  end;

  // Num = -1: link unlinked; Num = 0: link any; Num >= 1: link given struct
  procedure LinkShaft(Region: TRegion; Target: TCoords; Num: integer);
  var i: integer;
  begin
    i := Region.Structs.Count-1;
    while i >= 0 do begin
      if Test(Region.Structs[i].Data.Flags, ST_SHAFT) then begin
        case Num of
          -1:  if not Region.Structs[i].HasExit then Break;
          0:   Break;
          else if Region.Structs[i].Num = Num then Break;
        end;
      end;
      Dec(i);
    end;
    if i >= 0 then Region.Structs[i].Passage := Target;
  end;

  // Pickup regions from movement chain, not appeared elsewhere in report
  // Gold Fish [127] sails from ocean (37,47) in Atlantis Ocean to ocean
  //  (36,46) in Atlantis Ocean.
  procedure MakeVisitedRegions(U: TUnit; s: string);
  var Trace, Trace1: TTrace;
      RFrom, RTo: TRegion;
      i: integer;
  begin
    Trace := TTrace.Create(s);
    Trace.Before(Keys[s_From]);
    Trace.Before(')' + Keys[s_In]);
    Trace.Before(Keys[s_To]);
    Trace.Before(')' + Keys[s_In]);

    if not Trace.Ends then begin
      Trace.Reset;
      Trace.Before(Keys[s_From]);
      RFrom := FormRegion(Trace.Before(Keys[s_To]), FALSE);
      RTo := FormRegion(Trace.Before('.'), FALSE);

      // Link shafts, if unit travelled between levels
      if RFrom.z <> RTo.z then begin
        // Look for a shafts
        amt := Shafts(RFrom, False);
        if amt = 1 then LinkShaft(RFrom, RTo.Coords, 0)
        else if amt > 1 then begin
          // Look for unlinked shafts
          amt := Shafts(RFrom, True);
          if amt = 1 then LinkShaft(RFrom, RTo.Coords, -1)
          else if amt >= 1 then begin
            // Try to determine which shaft unit entered
            i := U.Events.Count-1;
            while (i >= 0) and (Pos(Keys[s_Enters], U.Events[i]) <> 1) do Dec(i);
            if i >= 0 then begin
              Trace1 := TTrace.Create(U.Events[i]);
              Trace1.Before('[');
              if not Trace1.Ends then
                LinkShaft(RFrom, RTo.Coords, StrToInt(Trace1.Before(']')));
              Trace1.Free;
            end;
          end;
        end;
        // Do not link second region if more than one unlinked shaft there
        // (impossible to tell the shaft unit come from)
        if Shafts(RTo, False) = 1 then LinkShaft(RTo, RFrom.Coords, 0)
        else if Shafts(RTo, True) = 1 then LinkShaft(RTo, RFrom.Coords, -1);
      end;
    end;
    Trace.Free;
  end;

begin
  NextLine;
  header := False;
  // JIoxu Heoby4eHHbIe (6707): Can't do that in this region.
  while (RepPos < RepLines.Count) and not EmptyLine(GetLine) do begin
    Trace := TTrace.Create(GetMultiline);
    if Pos(Keys[s_AddressOf], Trace.Text) > 0 then begin
      // The address of Forest Owls (3) is gnawer@mail.ru.
      Trace.Before(Keys[s_AddressOf]);
      Trace.Backwards('.');
      with FormFaction(Trace.Before(Keys[s_Is])) do begin
        Data.EMail := Trace.Text;
        Data.TurnNum := Turn.Num;
      end;
    end
    // Unit event: "something (567), not something (54,54)"
    else begin
      Trace.Before('(');
      if not Trace.Ends and (Pos(',', Trace.Before(')')) = 0) then begin
        Trace.Reset;
        Trace.Before('(');
        unum := Trace.Num;
        AUnit := TurnFaction.Units.Find(unum);
        if Pos(':', Trace.Text) > 0 then Trace.Before(' ') else Trace.Reset;
        MakeVisitedRegions(AUnit, Trace.Text);
        if AUnit <> nil then
          // Silently add event to known unit
          AUnit.Events.Add(Trace.Text)
        else if TurnFaction.Player then begin
          Trace.Reset;
          // Try to find unit in other factions
          i := Turn.Factions.Count-1;
          while (i > 1) and (AUnit = nil) do begin
            AUnit := Turn.Factions[i].Units.Find(unum);
            Dec(i);
          end;
          if AUnit <> nil then begin
            if not header then
              Turn.Events.Add(EventHeader('5', 'Events from other factions'));
            header := True;
            Turn.Events.AddObject('! 5 ' + Trace.Text, AUnit);
            AUnit.Events.Add(Trace.Text);
          end
          // Add it in turn Events section (usually that's dismissed units)
          else AddUnitEvent('!U7 ' + Trace.Text, nil);
        end;
      end
    // Ship [313] sails from desert (40,38) in Vaila to ocean (40,40) in
    // Atlantis Ocean.
      else begin
        Trace.Reset;
        if Pos(' [', Trace.Text) > 0 then begin
          Trace.Backwards('[');
          Trace.StPos := Trace.EnPos + 2;
          Trace.EnPos := Trace.StPos + 10;
          unum := Trace.Num;
          Trace.Reset;
          MakeVisitedRegions(nil, Trace.Text);
          with Faction do
            for i:=0 to Units.Count-1 do
              // Add to all ship passengers
              if (Units[i].Struct <> nil) and (Units[i].Struct.Num = unum) then
                Units[i].Events.Add(Trace.Text);
        end
        else Turn.Events.Add(Trace.Text);
      end
    end;
    Trace.Free;
  end;
end;

function FormEffect(Trace: TTrace): TEffectData;
var att, penalty: integer;
begin
  // fear (-2 to attack, -2 versus melee attacks) for the rest of the battle.
  Result := Game.EffectData.Seek(Trace.Before(' ('));
  while Pos(')', Trace.Text) > 0 do begin
    penalty := Trace.Num;
    if Pos(Keys[s_EffToAttack], Trace.Text) = 1 then
      Result.Attack := penalty
    else if Pos(Keys[s_EffVersus], Trace.Text) = 1 then begin
      Trace.Before(Keys[s_EffVersus]);
      att := KeyIndex(Trace.Text, s_atMelee, atCount);
      if att >= 0 then Result.Defence[att] := penalty;
    end;
    if Pos(',', Trace.Text) > 0 then Trace.Block
    else Trace.Before(')');
  end;
  if Pos(Keys[s_ForNextAttack], Trace.Text) = 1 then
    SetFlag(Result.Flags, EFF_ONESHOT, True);
end;

function FormSpecial(s: string; var Level: integer): TSpecData;
var ATrace, Trace: TTrace;
    i, amt1, amt2, at: integer;
    rep: string;
begin
  Result := nil;
  Level := 1;
  ATrace := TTrace.Create(s);
  while not ATrace.Ends do begin
    Trace := TTrace.Create(Trim(ATrace.Before('.')));
    rep := Trace.Text + '. ';

    // Item can cast spirit shield in battle at a skill level of 3.
    if TraceKey(Trace, [s_ItemCanCast, s_MonsterCanCast, s_MageCanCast,
      s_MountCauses]) then begin
      Result := Game.SpecData.Seek(Trace.Before(Keys[s_InBattle]));
      if Pos(Keys[s_AtSkillLevel], Trace.Text) = 1 then begin
        Trace.Before(Keys[s_AtSkillLevel]);
        Level := Trace.Num;
      end;
      Result.Description := '';
      rep := '';
    end
    // This ability will not target
    else if TraceKey(Trace, [s_ANotTarget, s_AOnlyTarget]) then begin
      s := Trace.Separator;
      // units which are inside the following structures:
      if TraceKey(Trace, [s_ATargetStructs]) then begin
        if s = Keys[s_ANotTarget] then
          SetFlag(Result.Flags, HIT_BUILDINGEXCEPT, True);
        ReadBlockList(Trace.Text, Result.Structs, StructDataFormer, False);
      end
      // creatures which are currently affected by fear.
      else if Pos(Keys[s_ATargetAffected], Trace.Text) = 1 then begin
        Trace.Before(Keys[s_ATargetAffected]);
        if s = Keys[s_ANotTarget] then
          SetFlag(Result.Flags, HIT_EFFECTEXCEPT, True);
        ReadBlockList(Trace.Text, Result.Effects, EffectFormer, False);
      end
      // illusions
      else if Pos(Keys[s_ATargetIllusions], Trace.Text) = 1 then begin
        SetFlag(Result.Flags, HIT_ILLUSION, True);
      end
      // skeletons [SKEL], liches [LICH]...
      // units mounted on horses [HORS]
      else if (TraceKey(Trace, [s_ATargetMounted]))
        or (Pos('[', Trace.Text) > 0) then begin
        if s = Keys[s_ANotTarget] then
          SetFlag(Result.Flags, HIT_ITEMEXCEPT, True);
        ReadBlockList(Trace.Text, Result.Items, ItemDataFormer, False);
      end;
    end
    // This ability cannot target monsters.
    else if Pos(Keys[s_ANoMonsters], Trace.Text) = 1 then begin
      SetFlag(Result.Flags, HIT_NOMONSTER, True);
    end
    // The bonus given to units inside buildings is not effective ...
    else if TraceKey(Trace, [s_ANoBuilding]) then begin
      SetFlag(Result.Flags, SP_NOBUILDING, True);
    end
    // This ability provides a shield against all spirit attacks...
    else if Pos(Keys[s_ShieldAbility], Trace.Text) = 1 then begin
      Trace.Before(Keys[s_ShieldAbility]);
      SetFlag(Result.Flags, SP_SHIELD, True);
      i := KeyIndex(Trace.Text, s_atMelee, atCount);
      if i >= 0 then Result.Shields[i] := True;
    end
    // This ability provides a defensive bonus of 1 per skill level
    // versus melee attacks to the casting mage.
    else if TraceKey(Trace, [s_AbilityProvides]) then begin
      while not Trace.Ends do begin
        Trace.Before(Keys[s_DefBonus]);
        amt1 := Trace.Num;
        Trace.Before(Keys[s_BonusVersus]);
        at := KeyIndex(Trace.Text, s_atMelee, atCount);
        if at >= 0 then with Result do begin
          i := Length(Defences) - 1;
          while (i >= 0) and (Defences[i].AttackType <> at) do Dec(i);
          if i < 0 then begin
            i := Length(Defences);
            SetLength(Defences, i+1);
          end;
          with Defences[Length(Defences) - 1] do begin
            Bonus := amt1;
            AttackType := at;
          end;
        end;
        Trace.Block;
        TraceKey(Trace, [s_And]);
      end;
    end
    // This ability does between 2 and 30 energy attacks.
    else if TraceKey(Trace, [s_AbilityDoes]) then begin
      // Read attack
      amt1 := Trace.Num;
      Trace.Before(Keys[s_And]);
      amt2 := StrToInt(Trace.Before(' '));
      // times mage skill level
      if Pos(Keys[s_TimesMageLev], Trace.Text) = 1 then
        SetFlag(Result.Flags, SP_USE_LEV, True);
      // Apply use_lev
      if Test(Result.Flags, SP_USE_LEV) then amt2 := amt2 div Level;
      // Hack report
      rep := Keys[s_AbilityDoes] + IntToStr(amt1) + ' ' + Keys[s_And] +
        IntToStr(amt2) + ' ' + Trace.Text + '. ';
      TraceKey(Trace, [s_TimesMageLev]);
      at := KeyIndex(Trace.Text, s_atMelee, atCount);
      if at >= 0 then begin
        // Look for existing attack of same type
        i := Length(Result.Attacks) - 1;
        while (i >= 0) and (Result.Attacks[i].AttackType <> at) do Dec(i);
        if i < 0 then begin
          // Setup new attack
          SetLength(Result.Attacks, Length(Result.Attacks) + 1);
          with Result.Attacks[Length(Result.Attacks) - 1] do begin
            MinAmt := amt1;
            MaxAmt := amt2;
            AttackType := at;
            SetFlag(WeaponFlags, WPN_RANGED + WPN_ALWAYSREADY, True);
            WeaponClass := wcEnergy;
          end;
        end
        else begin
          // Try to determine use_lev fom difference
          if Result.Attacks[i].MaxAmt <> amt2 then begin
            Result.Attacks[i].MaxAmt := amt2 div Level;
            SetFlag(Result.Flags, SP_USE_LEV, True);
          end;
        end;
      end;
    end
    // Each attack causes the target to be effected by fear (...)
    // - Section banded to previous, so will read into attack
    else if Pos(Keys[s_TargetAffected], Trace.Text) = 1 then begin
      Trace.Before(Keys[s_TargetAffected]);
      Result.Attacks[Length(Result.Attacks) - 1].Effect :=
        FormEffect(Trace);
    end
    else rep := '';

    if Result <> nil then Result.Description := Result.Description + rep;
  end;
end;

function ReadSkillDescription(D: TSkillData; Level: integer): boolean;
var FullTrace, Trace: TTrace;
    Skill: TSkill;
begin
  Result := True;
  D.Incomplete := False;
  FullTrace := TTrace.Create(D.Descriptions[Level]);
  while not FullTrace.Ends do begin
    Trace := TTrace.Create(FullTrace.Before('. '));

    try
      // but is rather one of the Foundation skills...
      if Pos(Keys[s_FoundationSkill], Trace.Text) > 0 then
        SetFlag(D.Flags, SK_FOUNDATION + SK_MAGIC, True)
      // This skill costs 100 silver per month of study.
      else if Pos(Keys[s_SkillCost], Trace.Text) = 1 then begin
        Trace.Before(Keys[s_SkillCost]);
        D.Cost := Trace.Num;
      end
      // This skill requires pattern [PATT] 1, and spirit [SPIR] 1 to begin to study.
      else if Pos(Keys[s_SkillRequire], Trace.Text) = 1 then begin
        Trace.Before(Keys[s_SkillRequire]);
        D.BasedOn.ClearAndFree;
        D.BasedOn := TSkillList.Create;
        while Pos(']', Trace.Text) > 0 do begin
          Skill := FormSkill(Trace.Before(', '));
          D.BasedOn.Add(Skill);
          if Pos(Keys[s_And], Trace.Text) = 1 then Trace.Before(Keys[s_And]);
        end;
      end
      // In order to use this spell in combat
      else if Pos(Keys[s_UseInCombat], Trace.Text) = 1 then begin
        Trace.Before(Keys[s_UseInCombat]);
        SetFlag(D.Flags, SK_MAGIC + SK_COMBATSPELL, True);
      end
      // To use the spell, CAST Gate_Lore
      else if Pos(Keys[s_UseCast], Trace.Text) > 0 then begin
        SetFlag(D.Flags, SK_MAGIC + SK_CAST, True);
      end
      // Special
      else if Pos(Keys[s_MageCanCast], Trace.Text) = 1 then begin
        D.Special := FormSpecial(Trace.Text + '. ' + FullTrace.Text, Level);
      end;
    except
      on E: Exception do Result := False;
    end;

    Trace.Free;
  end;
  FullTrace.Free;
end;


// Skill Reports
procedure ReadSkillDescriptions;
var Trace: TTrace;
    level: integer;
    SData: TSkillData;
    report: string;
    temp: TSkillData;
begin
  Turn.Events.Add(EventHeader('2', 'Skill reports'));
  NextLine;
  NextLine;
  // String with square bracket starts new skill, without - ends section
  while (RepPos < RepLines.Count) and (Pos('[', GetLine) > 0) do begin
    // pattern [PATT] 1: The Pattern skill is not directly useful to a mage...
    report := GetMultiline;
    Trace := TTrace.Create(report);
    SData := FormSkillData(Trace.Before('] ') + ']');
    level := Trace.Num;
    Trace.Before(' ');
    Trace.SkipSpaces;
    SData.Descriptions[level] := Trace.Text;

    temp := TSkillData.Create(SData.Short);
    temp.Descriptions[level] := SData.Descriptions[level];

    if ReadSkillDescription(temp, Level) then
      ReadSkillDescription(SData, Level)
    else
      Log.Add(SData.Short + ': non-standard description');

    Turn.Events.AddObject('! 2 ' + report, SData);

    temp.Free;
    Trace.Free;
    NextLine;
  end;
end;

function ReadItemDescription(D: TItemData): boolean;
var i: integer;
    Trace, FullTrace: TTrace;
    cap, lv, power: integer;
    IData: TItemData;
    Item: TItem;
    StData: TStructData;
    s: string;
begin
  Result := True;
  FullTrace := TTrace.Create(D.Description);
  D.Incomplete := False;

  // weight 1, costs 100 silver to withdraw.
  while not FullTrace.Ends and (FullTrace.Separator <> '.') do begin
    Trace := TTrace.Create(FullTrace.Block);

    try
      // weight 10
      if Pos(Keys[s_Weight], Trace.Text) > 0 then begin
        Trace.Before(' ');
        D.Weight := Trace.Num;
      end
      // capacity 5 (oldstyle report, seems to be lost "game2" 4.0.4 format)
      else if Pos(Keys[s_Capacity], Trace.Text) = 1 then begin
        Trace.Before(' ');
        D.Moves[mtWalk] := Trace.Num + D.Weight;
      end
      // is a man (4.0.4)
      else if Pos(Keys[s_IsAMan], Trace.Text) > 0 then
        D.Flags := D.Flags or IT_MAN
      // is a creature (4.0.4)
      else if Pos(Keys[s_IsACreature], Trace.Text) > 0 then
        D.Flags := D.Flags or IT_MONSTER
      // can walk
      else if Pos(Keys[s_CanWalk], Trace.Text) > 0 then
        D.Moves[mtWalk] := D.Weight
      // can ride (4.0.4)
      else if Pos(Keys[s_CanRide], Trace.Text) > 0 then
        D.Moves[mtRide] := D.Moves[mtWalk]
      // can fly (4.0.4)
      else if Pos(Keys[s_CanFly], Trace.Text) > 0 then
        D.Moves[mtFly] := D.Moves[mtWalk]
      // good
      else if Pos(Keys[s_itmGood], Trace.Text) = 1 then
        SetFlag(D.Flags, IT_GOOD)
      // evil
      else if Pos(Keys[s_itmEvil], Trace.Text) = 1 then
        SetFlag(D.Flags, IT_EVIL)
      // This is a ship
      else if Pos(Keys[s_ShipItem], Trace.Text) = 1 then
        SetFlag(D.Flags, IT_SHIP)
      else begin
        // walking capacity 5, riding capacity 5
        // walking capacity 200 when hitched to a horse
        for i := mtWalk to mtSwim do
          if Pos(GetKey(s_Walking, i - 1) + ' ' + Keys[s_Capacity],
            Trace.Text) > 0 then begin
            Trace.Before(' ');
            Trace.Before(' ');
            cap := Trace.Num + D.Weight;
            if TraceKey(Trace, [s_WhenHitched]) then begin
              SetFlag(D.Flags, IT_WAGON, True);
              D.Wagon.HitchWalk := cap;
              D.Wagon.Hitched := Game.ItemData.FindByName(Trace.Block);
              if D.Wagon.Hitched = nil then D.Incomplete := True;
            end
            else D.Moves[i] := cap;
          end;
      end;

    except
      on E: Exception do Result := False;
    end;

    Trace.Free;
  end;

  // The detailed description
  while not FullTrace.Ends do begin
    Trace := TTrace.Create(FullTrace.Before('. '));

    try
      // Production

      // Units with mining [MINI] 1 may PRODUCE this item at a rate of 1 per man-month.
      // Units with weaponsmith [WEAP] 2 may PRODUCE this item from 1 iron,
      //  1 wood at a rate of 1 per 2 man-months.
      if (Pos(Keys[s_UnitsWith], Trace.Text) = 1)
        and (Pos(Keys[s_ProduceRate], Trace.Text) > 0) then begin
        Trace.Before(Keys[s_UnitsWith]);
        // Skill
        if D.Produce.Skill <> nil then FreeAndNil(D.Produce.Skill);
        D.Produce.Skill := FormSkill(Trace.Text);
        if Pos(Keys[s_From], Trace.Text) > 0 then begin
          Trace.Before(Keys[s_From]);
          // any of
          if TraceKey(Trace, [s_AnyOf]) then
            SetFlag(D.Flags, IT_ORMATERIALS, True);
          // Required items
          D.Produce.Materials.ClearItems;
          while Pos(Keys[s_ProduceRate], Trace.Text) > 1 do begin
            i := Trace.Num;
            Trace.Before(' ');
            if Pos(', ', Trace.Text) > 0 then s := Trace.Before(', ')
            else s := Trace.Before(Keys[s_At]);
            IData := Game.ItemData.FindByName(s);
            if IData <> nil then begin
              Item := TItem.Create;
              Item.Amount := i;
              Item.Data := IData;
              D.Produce.Materials.Add(Item);
              // If D.Produce. from adv resources, set Advanced
              if Test(IData.Flags, IT_ADVANCED) then
                SetFlag(D.Flags, IT_ADVANCED, True);
            end
            else D.Incomplete := True;
          end;
        end
        else begin
          // If no "Produce from" given, that's resource
          SetFlag(D.Flags, IT_RESOURCE, True);
          if D.Produce.Skill.Level > 1 then SetFlag(D.Flags, IT_ADVANCED, True);
        end;
        // Rate
        Trace.Before(Keys[s_ProduceRate]);
        D.Produce.Rate := Trace.Num;
        Trace.Before(Keys[s_ProducePer]);
        if Trace.Text[1] in ['0'..'9'] then D.Produce.ManMonths := Trace.Num
        else D.Produce.ManMonths := 1;
      end
      // Units with dragon lore [DRAG] of at least level 1 may attempt
      //  to create this item via magic.
      else if (Pos(Keys[s_UnitsWith], Trace.Text) = 1)
        and (Pos(Keys[s_CreateMagic], Trace.Text) > 0) then begin
        Trace.Before(Keys[s_UnitsWith]);
        if D.MagProduce.Skill = nil then D.MagProduce.Skill := TSkill.Create;
        D.MagProduce.Skill.Data := FormSkillData(Trace.Text);
        SetFlag(D.MagProduce.Skill.Data.Flags, SK_MAGIC, True);
        Trace.Before(Keys[s_Level]);
        D.MagProduce.Skill.Level := Trace.Num;
        if Pos(Keys[s_MagProdCost], Trace.Text) > 0 then begin
          Trace.Before(Keys[s_MagProdCost]);
          D.MagProduce.Materials.ClearItems;
          while not Trace.Ends do begin
            i := StrToInt(Trace.Before(' '));
            if Pos(',', Trace.Text) > 0 then s := Trace.Block
            else if Pos(Keys[s_And], Trace.Text) > 0 then
              s := Trace.Before(' ' + Keys[s_And])
            else s := Trace.Block;
            IData := Game.ItemData.FindByName(s);
            if IData <> nil then begin
              Item := TItem.Create;
              Item.Data := IData;
              Item.Amount := i;
              D.MagProduce.Materials.Add(Item);
            end
            else D.Incomplete := True;
          end;
        end;
      end
      // By-products produced are wool.
      else if TraceKey(Trace, [s_ByProducts]) then begin
        D.Produce.Byproducts.Clear;
        while not Trace.Ends do begin
          TraceKey(Trace, [s_and]);
          IData := Game.ItemData.FindByName(Trace.Block);
          if IData <> nil then begin
            Item := D.Produce.Byproducts.Find(IData.Short);
            if Item <> nil then Inc(Item.Amount)
            else begin
              Item := TItem.Create;
              Item.Data := IData;
              Item.Amount := 1;
              D.Produce.Byproducts.Add(Item);
            end;
          end
          else D.Incomplete := True;
        end;
      end
      // To produce this item a Farm is required
      else if TraceKey(Trace, [s_StructToProduce]) then begin
        D.Produce.RequiredStruct := Game.StructData.Seek(Trace.Before(Keys[s_StructIsRequired]));
      end
      // Production of this item is increased by 2 when using picks.
      else if TraceKey(Trace, [s_ProdIncBy]) then begin
        D.Produce.ToolBonus := Trace.Num;
        Trace.Before(Keys[s_ProdIncUsing]);
        D.Produce.Tool := Game.ItemData.FindByName(Trace.Block);
        if D.Produce.Tool = nil then D.Incomplete := True
        else SetFlag(D.Produce.Tool.Flags, IT_TOOL);
      end

      // Man

      // This race may study multiple skills.
      // This race may study shipbuilding [SHIP], sailing [SAIL] to level 3 and all others to level 2
      // This race may study sailing [SAIL] to level 3 and all others to level 2
      // This race may study shipbuilding [SHIP] and sailing [SAIL] to level 3 and all others to level 2
      // This race may study all skills to level 5
      else if Pos(Keys[s_RaceMayStudy], Trace.Text) = 1 then begin
        SetFlag(D.Flags, IT_MAN, True);
        Trace.Before(Keys[s_RaceMayStudy]);
        if (Pos(Keys[s_AllSkills], Trace.Text) = 1) or
          (Pos(Keys[s_MultipleSkills], Trace.Text) = 1) then begin
          D.Man.Leader := True;
          D.Man.DefLevel := 5;
          D.Man.MagDefLevel := 5;
        end
        else begin
          D.Man.SpecSkills.ClearItems;
          while Pos('[', Trace.Text) > 0 do begin
            ReadBlockList(Trace.Before(Keys[s_To] + Keys[s_Level]),
              D.Man.SpecSkills, SkillFormer, False);
            lv := Trace.Num;
            Trace.Before(' ');
            Trace.SkipSpaces;
            for i := 0 to D.Man.SpecSkills.Count-1 do
              if D.Man.SpecSkills[i].Level = 0 then
                D.Man.SpecSkills[i].Level := lv;
          end;
          Trace.Before(Keys[s_Level]);
          D.Man.DefLevel := Trace.Num;
          if Pos(Keys[s_OtherMagic], Trace.Text) > 0 then begin
            Trace.Before(Keys[s_OtherMagic]);
            D.Man.MagDefLevel := Trace.Num;
          end
          else D.Man.MagDefLevel := 0;
        end;
      end

      // Weapon

      // This is a [ranged] [short] armor piercing D.Weapon.
      else if (Pos(Keys[s_ThisIsA], Trace.Text) = 1)
        and (Pos(Keys[s_Weapon], Trace.Text) > 0) then begin
        SetFlag(D.Flags, IT_WEAPON, True);
        Trace.Before(Keys[s_ThisIsA]);
        if Pos(Keys[s_Ranged], Trace.Text) = 1 then
          SetFlag(D.Weapon.Flags, WPN_RANGED, True)
        else if Pos(Keys[s_Long], Trace.Text) = 1 then
          SetFlag(D.Weapon.Flags, WPN_LONG, True)
        else if Pos(Keys[s_Short], Trace.Text) = 1 then
          SetFlag(D.Weapon.Flags, WPN_SHORT, True);
        for i := 0 to wcCount-1 do
          if Pos(GetKey(s_wcSlashing, i), Trace.Text) > 0 then
            D.Weapon.WpnClass := i;
      end
      // Knowledge of crossbow [XBOW] is needed to wield this D.Weapon.
      else if Pos(Keys[s_KnowledgeOf], Trace.Text) = 1 then begin
        Trace.Before(Keys[s_KnowledgeOf]);
        SetFlag(D.Weapon.Flags, WPN_NEEDSKILL, True);
        D.Weapon.Skill1 := FormSkillData(Trace.Text);
        Trace.Before('] ');
        if Pos('[', Trace.Text) > 0 then begin
          Trace.Before(' ');
          D.Weapon.Skill2 := FormSkillData(Trace.Text);
        end;
      end
      // This weapon grants a bonus of 4 on attack and [a bonus of 1 on] defense.
      // weapon grants +1 on attack and +1 on
      else if TraceKey(Trace, [s_WpnGrants]) then begin
        if not Trace.Ends and not (Trace.Text[1] in ['+', '-']) then begin
          i := 1;
          if TraceKey(Trace, [s_wpnBonus]) then i := +1
          else if TraceKey(Trace, [s_wpnPenalty]) then i := -1;
          i := i * Trace.Num;
          if Pos(Keys[s_AgainstMounted], Trace.Text) = 1 then
            D.Weapon.MountBonus := i
          else begin
            if Pos(Keys[s_Attack], Trace.Text) in [1..5] then begin
              D.Weapon.AttackBonus := i;
              i := 0;
              if Pos(Keys[s_wpnBonus], Trace.Text) > 0 then begin
                Trace.Before(Keys[s_wpnBonus]);
                i := +1;
              end
              else if Pos(Keys[s_wpnPenalty], Trace.Text) > 0 then begin
                Trace.Before(Keys[s_wpnPenalty]);
                i := -1;
              end;
              if i <> 0 then begin
                i := i * Trace.Num;
                D.Weapon.DefenceBonus := i;
              end
              else if Pos(Keys[s_Defense], Trace.Text) > 0 then
                D.Weapon.DefenceBonus := D.Weapon.AttackBonus;
            end
            else if Pos(Keys[s_Defense], Trace.Text) in [1..5] then
              D.Weapon.DefenceBonus := i;
          end;
        end
        else begin
          i := Trace.Num;
          Trace.Before(' ');
          Trace.Before(' ');
          if Pos(Keys[s_Attack], Trace.Text) = 1 then
            D.Weapon.AttackBonus := i;
          if Pos(Keys[s_Defense], Trace.Text) = 1 then
            D.Weapon.DefenceBonus := i;
          Trace.Before(Keys[s_And]);
          if not Trace.Ends then begin
            i := Trace.Num;
            Trace.Before(' ');
            Trace.Before(' ');
            if Pos(Keys[s_Attack], Trace.Text) = 1 then
              D.Weapon.AttackBonus := i;
            if Pos(Keys[s_Defense], Trace.Text) = 1 then
              D.Weapon.DefenceBonus := i;
          end;
        end;
      end
      // Only mounted troops may use this D.Weapon.
      else if Pos(Keys[s_NoFoot], Trace.Text) = 1 then begin
        SetFlag(D.Weapon.Flags, WPN_NOFOOT, True);
      end
      else if Pos(Keys[s_NoMount], Trace.Text) = 1 then begin
        SetFlag(D.Weapon.Flags, WPN_NOMOUNT, True);
      end
      // Wielders of this weapon, if mounted, get their riding skill bonus
      else if Pos(Keys[s_RidingBonus], Trace.Text) = 1 then begin
        if Pos(Keys[s_Attack], Trace.Text) > 0 then
          SetFlag(D.Weapon.Flags, WPN_RIDINGBONUS, True)
        else SetFlag(D.Weapon.Flags, WPN_RIDINGBONUSDEFENSE, True);
      end
      // Attackers do not get skill bonus on defense.
      else if Pos(Keys[s_NoAttDefense], Trace.Text) = 1 then begin
        SetFlag(D.Weapon.Flags, WPN_NOATTACKERSKILL, True);
      end
      // This weapon attacks versus the target's defense against riding attacks.
      else if Pos(Keys[s_AttackVersus], Trace.Text) = 1 then begin
        Trace.Before(Keys[s_AttackVersus]);
        i := atCount-1;
        while (i >= 0) and (Pos(GetKey(s_atMelee, i), Trace.Text) <> 1) do
          Dec(i);
        if i >= 0 then D.Weapon.AttackType := i;
      end
      // This weapon allows 1 attack  per round.
      // This weapon allows 1 attack every 2 rounds.
      // This weapon allows a number of attacks equal to the
      //   skill level of the attacker per round.
      else if Pos(Keys[s_WpnAllows], Trace.Text) = 1 then begin
        if Pos(Keys[s_WpnNumSkill], Trace.Text) > 0 then
          SetFlag(D.Weapon.Flags, WPN_NUMATTSKILL, True)
        else if Pos(Keys[s_WpnNumHalf], Trace.Text) > 0 then
          SetFlag(D.Weapon.Flags, WPN_NUMATTHALFSKILL, True)
        else begin
          Trace.Before(Keys[s_WpnAllows]);
          if Pos(Keys[s_Every], Trace.Text) = 0 then begin
            if (Trace.Text >= '0') and (Trace.Text <= '9') then
              D.Weapon.NumAttacks := Trace.Num
          end
          else begin
            Trace.Before(Keys[s_Every]);
            D.Weapon.NumAttacks := -Trace.Num;
          end;
        end;
      end

      // Armor

      // This armor will protect its wearer 67% of the time versus
      //  slashing attacks...
      else if Pos(Keys[s_ArmorProtect], Trace.Text) = 1 then begin
        SetFlag(D.Flags, IT_ARMOR, True);
        Trace.Before(Keys[s_ArmorProtect]);
        for i := 0 to wcCount-1 do begin
          D.Armor.Defence[i] := Trace.Num;
          Trace.Before(', ');
          if Pos(Keys[s_And], Trace.Text) = 1 then Trace.Before(Keys[s_And]);
        end;
      end
      // This armor may be worn during assassination attempts
      else if Pos(Keys[s_ArmorAss], Trace.Text) = 1 then begin
        SetFlag(D.Armor.Flags, ARM_USEINASS, True);
      end

      // Mount

      // This mount requires riding [RIDI] of at least level 1 to ride in combat.
      else if Pos(Keys[s_MountRequires], Trace.Text) = 1 then begin
        Trace.Before(Keys[s_MountRequires]);
        SetFlag(D.Flags, IT_MOUNT, True);
        D.Mount.RideSkill := FormSkillData(Trace.Text);
      end
      // This mount gives a minimum bonus of +1 when ridden into combat.
      else if Test(D.Flags, IT_MOUNT) and (Pos(Keys[s_MinBonus], Trace.Text) > 0) then begin
        Trace.Before(Keys[s_MinBonus]);
        D.Mount.MinBonus := Trace.Num;
      end
      // This mount gives a maximum bonus of +3 when ridden into combat.
      else if Test(D.Flags, IT_MOUNT) and (Pos(Keys[s_MaxBonus], Trace.Text) > 0) then begin
        Trace.Before(Keys[s_MaxBonus]);
        if Pos(Keys[s_HamperedBonus], Trace.Text) > 1 then
          D.Mount.MaxHamperedBonus := Trace.Num
        else D.Mount.MaxBonus := Trace.Num;
      end
      // Mount causes terrify horses
      else if Pos(Keys[s_MountCauses], Trace.Text) = 1 then begin
        D.Special := FormSpecial(D.Description, D.SpecLevel);
      end

      // Tool

      // This item increases the production of furs by 1 and floater hides by 1.
      // This item increases the production of furs [FUR] by 1 and floater hides [FLOA] by 1.
      else if TraceKey(Trace, [s_IncProduction]) then begin
        SetFlag(D.Flags, IT_TOOL, True);
        while Pos(Keys[s_By], Trace.Text) > 0 do begin
          s := Trace.Before(Keys[s_By]);
          if s = Keys[s_ProdEntertainment] then
            IData := SilverData
          else
          begin
            i := Pos('[', s);
            if i > 0 then
            begin
              Inc(i);
              s := Copy(s, i, Pos(']', s) - i);
            end;

            IData := Game.ItemData.FindByName(s);
          end;

          if IData <> nil then begin
            IData.Produce.Tool := D;
            IData.Produce.ToolBonus := Trace.Num;
          end
          else D.Incomplete := True;
          Trace.Before(' ');
          if Pos(Keys[s_And], Trace.Text) = 1 then Trace.Before(Keys[s_And]);
        end;
      end
      // This item increases the building of Rowingboat by 2 ...
      else if TraceKey(Trace, [s_IncBuilding]) then begin
        SetFlag(D.Flags, IT_TOOL, True);
        while Pos(Keys[s_By], Trace.Text) > 0 do begin
          StData := Game.StructData.Seek(Trace.Before(Keys[s_By]));
          StData.Tool := D;
          StData.ToolBonus := Trace.Num;
          Trace.Before(' ');
          if Pos(Keys[s_And], Trace.Text) = 1 then Trace.Before(Keys[s_And]);
        end;
      end

      // Trade

      // This is a trade good.
      else if Pos(Keys[s_TradeGood], Trace.Text) = 1 then begin
        SetFlag(D.Flags, IT_TRADE, True);
      end

      // Monster

      // This monster attacks with a combat skill of
      else if Pos(Keys[s_MonsterSkill], Trace.Text) = 1 then begin
        Trace.Before(Keys[s_MonsterSkill]);
        SetFlag(D.Flags, IT_MONSTER, True);
        D.Monster.Attack := Trace.Num;
      end
      // This monster has a resistance of 1 to melee attacks
      else if Pos(Keys[s_MonResistanceOf], Trace.Text) = 1 then begin
        Trace.Before(Keys[s_MonResistanceOf]);
        power := Trace.Num;
        Trace.Before(Keys[s_To]);
        i := KeyIndex(Trace.Text, s_atMelee, atCount);
        if i >= 0 then D.Monster.Defence[i] := power;
      end
      // This monster is very resistant to melee attacks.
      else if (Pos(Keys[s_MonsterIs], Trace.Text) = 1)
        and (Pos(Keys[s_Attack], Trace.Text) > 0) then begin
        Trace.Before(Keys[s_MonsterIs]);
        i := 0;
        while (i < 5) and (Pos(GetKey(s_VerySusceptible, i), Trace.Text) <> 1) do
          Inc(i);
        if i < 5 then begin
          Trace.Before(Keys[s_To]);
          power := 0;
          case i of
            1: power := 1;
            2: power := 2;
            3: power := 4;
            4: power := 6;
          end;
          i := KeyIndex(Trace.Text, s_atMelee, atCount);
          if i >= 0 then D.Monster.Defence[i] := power;
        end;
      end
      // This monster has 20 melee attacks per round and takes 20 hits to kill
      else if Pos(Keys[s_MonTakes], Trace.Text) > 0 then begin
        Trace.Before(Keys[s_MonHas]);
        D.Monster.NumAttacks := Trace.Num;
        Trace.Before(Keys[s_MonTakes]);
        D.Monster.Hits := Trace.Num;
      end
      // This monsters regenerates 20 hits per round of battle
      else if Pos(Keys[s_MonRegen], Trace.Text) = 1 then begin
        Trace.Before(Keys[s_MonRegen]);
        D.Monster.Regen := Trace.Num;
      end
      // This monster has a tactics score of 1, a stealth score of 1,
      //  and an observation score of 1.
      else if Pos(Keys[s_MonTacticsRep], Trace.Text) = 1 then begin
        Trace.Before(Keys[s_MonTacticsRep]);
        D.Monster.Tactics := Trace.Num;
        Trace.Before(Keys[s_MonStealth]);
        D.Monster.Stealth := Trace.Num;
        Trace.Before(Keys[s_MonObservation]);
        D.Monster.Observation := Trace.Num;
      end
      // Monster can cast hellfire in battle at a skill level of 3.
      else if Pos(Keys[s_MonsterCanCast], Trace.Text) = 1 then begin
        D.Special := FormSpecial(D.Description, D.SpecLevel);
      end
      // This item cannot be given to other units
      else if Pos(Keys[s_CantGive], Trace.Text) > 0 then begin
        SetFlag(D.Flags, IT_CANTGIVE, True);
      end

      // Battle item

      // This is miscellaneous combat item
      else if Pos(Keys[s_CombatItem], Trace.Text) = 1 then begin
        SetFlag(D.Flags, IT_MAGIC, True);
      end
      // Item can cast spirit shield in battle at a skill level of 3.
      else if Pos(Keys[s_ItemCanCast], Trace.Text) = 1 then begin
        D.Special := FormSpecial(D.Description, D.SpecLevel);
      end
      // This item may only be used by a mage
      else if Pos(Keys[s_MageOnly], Trace.Text) = 1 then begin
        D.Magic.MageOnly := True;
      end;

    except
      on E: Exception do Result := False;
    end;

    Trace.Free;
  end;
  FullTrace.Free;

  // Set default values for upkeep and food
  if Result = true then
  begin
    if Test(D.Flags, IT_MAN) and (D.Upkeep.Silver = 0) then
    begin
      if D.Man.Leader then
        D.Upkeep.Silver := GameConfig.ReadInteger('Settings', 'LeaderMaintenance', 20)
      else
        D.Upkeep.Silver := GameConfig.ReadInteger('Settings', 'PeasantMaintenance', 10);
    end;

    if Test(D.Flags, IT_FOOD) and (D.Food.Value = 0) then
    begin
      D.Food.Value := 10;
    end;
  end;
end;

// Item reports:
procedure ReadItemDescriptions;
var Trace: TTrace;
    report: string;
    IData: TItemData;
    temp: TItemData;
begin
  Turn.Events.Add(EventHeader('3', 'Item reports'));
  NextLine;
  NextLine;
  while (RepPos < RepLines.Count) and (Pos('[', GetLine) > 0) do begin
    report := GetMultiline;
    // javelin [JAVE], weight 1, costs 100 silver to withdraw...
    Trace := TTrace.Create(report);
    IData := FormItemData(Trace.Block, 1);
    IData.Description := Trim(Trace.Text);

    temp := TItemData.Create(IData.Short);
    temp.Description := IData.Description;

    // try to read it into temp structure, if that succeeds, read it into IData, otherwise nothing will be updated
    if ReadItemDescription(temp) then
      ReadItemDescription(IData)
    else
      Log.Add(IData.Short + ': non-standard description');

    temp.Free;
    Trace.Free;
    Turn.Events.AddObject('! 3 ' + report, IData);
    NextLine;
  end;
end;

function ReadObjectDescription(D: TStructData): boolean;
var FullTrace, Trace: TTrace;
    i, lv: integer;
    str:  string;
begin
  Result := True;
  FullTrace := TTrace.Create(D.Description);
  D.Incomplete := False;

  while not FullTrace.Ends do begin
    Trace := TTrace.Create(FullTrace.Before('. '));

    try
      // This ship requires 10 men
      if Pos(Keys[s_ShipRequire], Trace.Text) = 1 then begin
        Trace.Before(Keys[s_ShipRequire]);
        SetFlag(D.Flags, ST_TRANSPORT, True);
        D.Sailors := Trace.Num;
      end
      // This structure is built using mining 1 and requires 10 wood or stone
      else if Pos(Keys[s_StructBuilt], Trace.Text) = 1 then begin
        Trace.Before(Keys[s_StructBuilt]);
        D.BuildSkill := FormSkill(Trace.Text);
        Trace.Before(Keys[s_Requires]);
        D.Size := Trace.Num;
        Trace.SkipSpaces;
        if Pos(Keys[s_Or], Trace.Text) > 0 then begin
          D.Material1 := Game.ItemData.FindByName(Trim(Trace.Before(Keys[s_Or])));
          D.Material2 := Game.ItemData.FindByName(Trim(Trace.Before(Keys[s_To])));
          if (D.Material1 = nil) or (D.Material2 = nil) then D.Incomplete := True;
        end
        else begin
          D.Material1 := Game.ItemData.FindByName(Trace.Before(Keys[s_To]));
          if D.Material1 = nil then D.Incomplete := True;
        end;
      end
      // This trade structure increases the amount of wood available in the region
      else if (Pos(Keys[s_AvaiInRegion], Trace.Text) > 0)
        and TraceKey(Trace, [s_IncAmount]) then begin
        str := Trace.Before(Keys[s_AvaiInRegion]);
        if str = Keys[s_ProdEntertainment] then
          D.Resource := SilverData
        else
          D.Resource := Game.ItemData.FindByName(str);
        if D.Resource = nil then D.Incomplete := True;
      end
      // It provides a bonus of 1 against melee, 0 against energy...
      else if TraceKey(Trace, [s_StructDefence]) then begin
        while not Trace.Ends do begin
          TraceKey(Trace, [s_and]);
          Trace.SkipSpaces;
          lv := Trace.Num;
          Trace.Before(Keys[s_StructDefenceAgainst]);
          i := KeyIndex(Trace.Block, s_atMelee, atCount);
          if i >= 0 then D.Defence[i] := lv;
        end;
      end
      // defense to the first 10 men
      else if Pos(Keys[s_DefenseAmt], Trace.Text) > 0 then begin
        Trace.Before(Keys[s_DefenseAmt]);
        D.Protection := Trace.Num;
        SetFlag(D.Flags, ST_DEFENCE, True);
      end;
    except
      on E: Exception do Result := False;
    end;

    Trace.Free;
  end;
  FullTrace.Free;
end;

// Object reports:
procedure ReadObjectDescriptions;
var Trace: TTrace;
    report: string;
    StData: TStructData;
    temp: TStructData;
begin
  Turn.Events.Add(EventHeader('4', 'Object reports'));
  NextLine;
  NextLine;
  while (RepPos < RepLines.Count) and (Pos(':', GetLine) < Length(GetLine)) do begin
    report := GetMultiline;
    // Mine: This is a building. Monsters can potentially lair...
    Trace := TTrace.Create(report);
    StData := Game.StructData.Seek(Trace.Before(': '));
    StData.Description := Trace.Text;

    temp := TStructData.Create(StData.Group);
    temp.Description := StData.Description;

    if ReadObjectDescription(temp) then
      ReadObjectDescription(StData)
    else
      Log.Add(StData.Group + ': non-standard description');

    Turn.Events.AddObject('! 4 ' + report, StData);

    temp.Free;
    Trace.Free;
    NextLine;
  end;
end;

// Declared Attitudes (default Neutral):
procedure ReadAttitudes;
var Trace: TTrace;
    i: integer;
    s: string;
begin
  Trace := TTrace.Create(GetLine);
   { (default Neutral): }
  Trace.Before(Keys[s_DeclaredAttitudes]);
  s := Trace.Before(')');
  for i:=1 to 5 do
    if GetKey(s_Hostile, i - 1) = s then TurnFaction.Attitude := i;
   { Friendly : Green Cormorant (131). }
   { Neitrâla: Boldriks un citi zveeri... (61), Ugunsveèi (10).
      - cannot use BlockList because of s$%t in faction names }
  NextLine;
  for i:=1 to 5 do begin
    s := GetMultiline;
    Trace.Text := Copy(s, Pos(':', s)+2, Length(s)-Pos(':', s)-1);
    repeat
      s := Trace.Before(')');
      Trace.Before(' ');
      if Pos(Keys[s_None], s) = 0 then
        FormFaction(s + ')').Attitude := i;
    until Trace.Ends;
  end;
  Trace.Free;
end;

procedure ReadUnclaimed;
var Trace: TTrace;
begin
  Turn.Events.Add(GetLine);
  Trace := TTrace.Create(GetLine);
  Trace.Before(': ');
  Turn.Unclaimed := Trace.Num;
  Trace.Free;
  NextLine;
  if Pos(Keys[s_FactionMana], GetLine) = 1 then begin
    Turn.Events.Add(GetLine);
    Trace := TTrace.Create(GetLine);
    Trace.Before(': ');
    Turn.Mana := Trace.Num;
    Trace.Free;
  end;
end;

procedure ReadUnit(Region: TRegion; Struct: TStruct);
var U, OldU: TUnit;
    Trace: TTrace;
    s: string;
    i, bk: integer;
    Troop: TTroop;
    full: boolean;
    repStart: integer;
    item: TItem;

begin
  repStart := RepPos;

  Trace := TTrace.Create(TrimLeft(GetMultiline));
  U := TUnit.Create;

  // Unit and faction names, and description, can contain anything
  //   (except round brackets!). Not self (-) units have ONLY name and
  //   faction brackets, self units MUST have faction. So we'll just
  //   scan for next bracket.
  // * ... (1), ... (2), ... Skills: ... () ... ()
  // - ... (1), [... (2),] ... <- no more brackets here
  full := (Pos('*', Trace.Text) = 1);
  Inc(Trace.StPos, 2); // Skip '- ' or '* '

  // Workers (1656), on guard, Faction (162)
  // Conquest: Unit (2), Faction 2.
  U.Name := Trace.Before(' (');
  U.Num := Trace.Num;
  Trace.Before(' ');
  // OnGuard part
  if Pos(Keys[s_flgOnGuard], Trace.Text) > 0 then begin
    U.Flags[flgGuard] := True;
    Trace.Before(', ');
  end;
  // Faction part
  if Pos('(', Trace.Text) > 0 then
    U.Faction := FormFaction(Trace.Before('), '))
  else if Pos(Keys[s_Faction], Trace.Text) = 1 then begin
    // Hack Conquest starting factions (that was error... but whatever)
    s := Trace.Before(Keys[s_Faction]);
    U.Faction := Turn.Factions.Seek(Trace.Num);
    U.Faction.Name := s;
    Trace.Before(', ');
  end
  else U.Faction := Turn.Factions[0];

  // Get description if any ( ";" may not appear in items & skills)
  if Pos(';', Trace.Text) > 0 then begin
    Trace.Backwards('.');
    U.Description := Copy(Trace.Text, Pos(';', Trace.Text)+2, Length(Trace.Text));
    Trace.EnPos := Trace.StPos + Pos(';', Trace.Text) - 2;
  end;

  // Read optional flags
  repeat
    bk := Trace.StPos;
    s := Trace.Block;
    for i := 0 to UnitFlagsCount-1 do
      if s = GetKey(s_flgOnGuard, i) then U.Flags[i] := True;
    for i := rtUnit to rtFaction do
      if s = GetKey(s_RevealUnit, i - rtUnit) then U.Revealing := i;
    for i := ctUnit to ctFaction do
      if s = GetKey(s_ConsumeUnit, i - ctUnit) then U.Consuming := i;
    for i := spoilNone to spoilFly do
      if (s = GetKey(s_SpoilWeightless, i - spoilNone)) then U.Spoils := i;
  until (Pos('[', s) > 0) or Trace.Ends;
  Trace.StPos := bk;
  Trace.Separator := '';

  // Items
  while not Trace.Ends and (Trace.Separator <> '.') do
  begin
    item := FormItem(Trace.Block);
    U.Items.Add(item);
    U.Inventory.Add(NewItem(item.Data, item.Amount, tsInitial));
  end;

  // Self units' sections
  while not Trace.Ends do begin
    // Skills: pattern [PATT] 1 (30).
    if Pos(Keys[s_Skills], Trace.Text) = 1 then
      ReadBlockList(Trace.Before('. '), U.Skills, SkillFormer, True)
    // Combat spell: summon storm [SSTO].
    else if Pos(Keys[s_CombatSpell], Trace.Text) = 1 then begin
      Trace.Before(': ');
      U.CombatSpell := FormSkillData(Trace.Before('. '));
    end
    // Can Study: magical healing [MHEA].
    else if Pos(Keys[s_CanStudy], Trace.Text) = 1 then begin
      ReadBlockList(Trace.Before('. '), U.CanStudy, SkillDataFormer, True);
      // Mark CanStudy skills as magic, if that's mage
      // assuming mage wouldn't learn Weaponcraft
      if U.CanStudy.Count > 0 then begin
        i := U.Skills.Count-1;
        while (i >= 0) and not Test(U.Skills[i].Data.Flags, SK_MAGIC) do Dec(i);
        if i >= 0 then begin
          for i := 0 to U.CanStudy.Count-1 do
            SetFlag(U.CanStudy[i].Flags, SK_MAGIC);
        end;
      end;
    end
    // Ready weapons: sword [SWOR].
    else if Pos(Keys[s_ReadyWeapon], Trace.Text) = 1 then begin
      Trace.Before(': ');
      while not Trace.Ends and (Trace.Separator <> '.') do
        U.ReadyWeapons.Add(FormItemData(Trace.Block, 1))
    end
    // Ready armor: chain armor [CARM].
    else if Pos(Keys[s_ReadyArmor], Trace.Text) = 1 then begin
      Trace.Before(': ');
      while not Trace.Ends and (Trace.Separator <> '.') do
        U.ReadyArmor.Add(FormItemData(Trace.Block, 1))
    end
    else Trace.Before('. ');
  end;
  Trace.Free;

  // Mark ally units
  if (U.Faction = TurnFaction) and not TurnFaction.Player then
    SetFlag(U.Marks, UM_ALLY, True);

  // Write new unit in Region lists (if it's not already there)
  OldU := nil;
  // Handle units of unknown factions
  if U.Faction.Num = 0 then
    OldU := Region.FindUnit(U.Num)
  else begin
    Troop := Region.Troops.Find(0);
    if Troop <> nil then OldU := Troop.Units.Find(U.Num);
  end;
  // If not found, scan unit's troop
  if OldU = nil then begin
    Troop := Region.Troops.Seek(U.Faction.Num);
    // Move player's Troop to first position
    if Troop.Faction.Player and (Troop.Units.Count = 0) then
      Region.Troops.Move(Region.Troops.IndexOf(Troop), 0);
    // Find unit in troop
    OldU := Troop.Units.Find(U.Num);
    if OldU = nil then begin
      Troop.Units.Add(U);
      U.Faction.Units.Add(U);
    end;
  end;

  if OldU <> nil then begin
    if full then begin
      OldU.Assign(U);
      if U.Faction.Num <> OldU.Faction.Num then
        OldU.ChangeFaction(U.Faction.Num);
    end;
    U.Free;
    U := OldU;
  end;

  U.Region := Region;
  U.Struct := Struct;

  // Write to other structures
  if U.Flags[0] then Region.Guard := U.Faction;
  if (U.Struct <> nil) and (U.Struct.Owner = nil) then U.Struct.Owner := U;

  // Find army
  if U.Faction.Player then begin
    i := Game.UArmies.Count-1;
    while (i >= 0) and (Game.UArmies[i].UnitIds.IndexOf(U.Id) = -1) do Dec(i);
    if i >= 0 then U.UArmy := Game.UArmies[i];
  end;

  for i := repStart to RepPos do
    U.Report.Add(RepLines[i]);
end;

 { Add Structure to Region. Assuming they cannot duplicate. }
procedure ReadStruct(Region: TRegion);
var OldR: TRegion;
    Struct, OldStruct: TStruct;
    Trace: TTrace;
    s, shaft_link: string;
    i, repStart: integer;

begin
  repStart := RepPos;
  Struct := TStruct.Create;

{+ Building [7] : Timber Yard; ladnaja lesopilka iz kamnia,
   postroennaja po prikazu Anta Himena dlia vseobscej pol'zy.
   - 2-nd Lumberjacks (181), LAD Harmoni Universe (3), behind, 4
     vikings [VIKI], 4 horses [HORS].

 - needs special multiline processing}

  s := '';
  while not (EmptyLine(GetLine) or (Pos('  - ', GetLine) = 1) or
    (Pos('  * ', GetLine) = 1)) do begin
    if s <> '' then s := s + ' ';
    s := s + Trim(GetLine);
    NextLine;
  end;

  Trace := TTrace.Create(s);
  Trace.Before(' ');   // Skip '+ '
  // + Ruin [1] : Ruin, closed to player units.
  // + The Kings Highway [1] : Road N.
  // + Trade Academy [NIMB] [Nort Triders]  [2] : Tower; comment.
  Struct.Name := '';
  repeat
    if Struct.Name <> '' then Struct.Name := Struct.Name + ' [';
    Struct.Name := Struct.Name + Trace.Before(' [');
  until (Pos('[', Trace.Text) = 0) or (Pos(':', Trace.Text) < Pos('[', Trace.Text));
  Struct.Num := Trace.Num;
  Trace.Before(': ');
  Struct.Data := Game.StructData.Seek(Trace.Block);
  // Roads
  if Pos(Keys[s_Road], Struct.Data.Group) = 1 then
    SetFlag(Struct.Data.Flags, ST_ROAD, True);
  // needs 10
  if (Pos(Keys[s_Needs], Trace.Text) = 1) then begin
    Trace.Before(Keys[s_Needs]);
    if IsNumber(Copy(Trace.Text, 1, 1)) then
      Struct.Needs := Trace.Num;
    Trace.Block;
  end;
  // contains an inner location
  if (Trace.Separator <> ';')
    and (Pos(Keys[s_InnerLocation], Trace.Text) = 1) then begin
    Trace.Block;
    SetFlag(Struct.Data.Flags, ST_SHAFT, True);
  end;
  // engraved with Runes of Warding
  if (Trace.Separator <> ';')
    and (Pos(Keys[s_RunesOfWarding], Trace.Text) = 1) then begin
    Trace.Block;
    Struct.Runes := 3;
  end;
  // closed to player units.  - comes after description (stupid?)
  if (Pos(Keys[s_ClosedToPlayer], Trace.Text) = Length(Trace.Text) -
   Length(Keys[s_ClosedToPlayer]))
   and (Pos(Keys[s_ClosedToPlayer], Trace.Text) > 0)
   and (Trace.Separator <> ';') then begin
    Trace.Backwards(Keys[s_ClosedToPlayer]);
    SetFlag(Struct.Data.Flags, ST_CLOSED, True);
  end;
  // ; Ladnaya lesopilka...
  shaft_link := '';
  if Pos(';!', Trace.Text) = 1 then
    shaft_link := Copy(Trace.Text, 3, Length(Trace.Text) - 3)
  else Struct.Description := Trace.Text;
  Trace.Free;

  // Add new Structure to Region list (for double reports)
  i := 0;
  while (i < Region.Structs.Count) and (Region.Structs[i].Num <> Struct.Num) do Inc(i);
  if (i < Region.Structs.Count) then begin
    if not Struct.HasExit then
      Struct.Passage := Region.Structs[i].Passage;
    if TurnFaction.Player then Region.Structs[i].Assign(Struct);
    Struct.Free;
    Struct := Region.Structs[i];
  end
  else Region.Structs.Add(Struct);

  if Test(Struct.Data.Flags, ST_SHAFT) and not Struct.HasExit then begin
    // Get inherited shaft linkage
    OldR := Map.Region(Region.Coords, Turn.Num-1);
    if OldR <> nil then begin
      OldStruct := OldR.Structs.Find(Struct.Num);
      if (OldStruct <> nil) then Struct.Passage := OldStruct.Passage;
    end;
    // Read shaft linkage from report
    if not Struct.HasExit and (shaft_link <> '') then
      Struct.Passage := FormRegion(shaft_link, False).Coords;
  end;

  // Now read units in this Structure
  while not EmptyLine(GetLine) do ReadUnit(Region, Struct);

  for i := repStart to RepPos do
    Struct.Report.Add(RepLines[i]);
end;

function ReadRegion: TRegion;
var Trace: TTrace;
    Region, PrevRegion, ExitR, NewR: TRegion;
    Item: TItem;
    s, peasants_name: string;
    i, j, regRepStart, regRepEnd: integer;
    ac_skip: boolean;
begin
  regRepStart := RepPos;
  s := GetMultiline;

  // Import: ---------------------------------------;130-114
  Trace := TTrace.Create(GetLine);
  Trace.Before(';');
  if not Trace.Ends then begin
    if Pos('-', Trace.Text) > 0 then Trace.Before('-');
    Map.TurnNum := Max(1, Trace.Num);
  end;

  // plain (48,12) in Elgomaar, contains Cashmar [city],
  Trace.Text := s;
  s := Trace.Block;
  if Pos(Keys[s_Contains], Trace.Text) > 0 then begin
    Trace.Backwards(']');
    s := s + ', ' + Trace.Text + ']';
    i := Trace.EnPos + 4;
    Trace.Reset;
    Trace.StPos := i;
  end;
  Region := FormRegion(s, True);
  Result := Region;

  with Region do begin
     // Demographics: 5479 peasants (sea elves), $38353
     //   conquest: 800 peasants, $3200
    if Pos(Keys[s_Peasants], Trace.Text) > 0 then begin
      if Peasants = nil then Peasants := TItem.Create;
      Peasants.Amount := Trace.Num;
      Trace.Before(Keys[s_Peasants]);
      if Pos('(', Trace.Text) > 0 then begin
        Trace.Before('(');
        peasants_name := Trace.Before(')');
      end
      else peasants_name := Keys[s_DefaultPeasants];
      Trace.Before('$');
      TaxRate := Trace.Num;
    end;

     // Weather: It was winter last month; it will be winter next month.
     // Iepriekšējā mēnesī bija labs laiks; nākamajā mēnesī būs labs laiks.
     //   again, conquest misses it
    NextLine;
    if Pos(Keys[s_Was], GetLine) > 0 then begin
      Trace.Text := TrimLeft(GetMultiline);
      s := Trace.Before(Keys[s_Was]);
      WeatherLast := Game.WeatherData.Seek(Trace.Before(Keys[s_LastMonth]));
      WeatherLast.LastText := s;
      s := Trace.Before(Keys[s_WillBe]);
      WeatherNext := Game.WeatherData.Seek(Trace.Before(Keys[s_NextMonth]));
      WeatherNext.NextText := s;
    end;

    ac_skip := False;
    if Trim(GetLine) = '' then begin
      NextLine;
      if Pos(Keys[s_Exits], GetLine) > 0 then
        // We have here Atlaclient map with no region info
        ac_skip := True
      else begin
        // Read nexus description
        Notes.Text := Trim(GetMultiline);
        NextLine;
      end;
    end;

    if not ac_skip then begin
      // Wages: $17 (Max: $93143).
      // A3.0 reports do not have 'Max' section (same when Wages: $0)
      SeekLineContain(Keys[s_Wages]);
      Trace.Text := GetLine;
      Trace.Before(Keys[s_Wages]);
      Wages := Trace.Num;
      if Pos(Keys[s_MaxWages], Trace.Text) > 0 then begin
        Trace.Before(Keys[s_MaxWages]);
        MaxWages := Trace.Num;
      end
      else MaxWages := 0;

      // Wanted: none.
      SeekLineContain(Keys[s_Wanted]);
      ReadBlockList(GetMultiline, Region.Wanted, ItemFormer, True);

      // For Sale: unlimited iron [IRON] at $75...
      SeekLineContain(Keys[s_ForSale]);
      ReadBlockList(GetMultiline, Region.ForSale, ItemFormer, True);

      // Entertainment available: $1917.
      // Nexus and oceans do not have it.
      if Pos(Keys[s_Entertainment], GetLine) > 0 then begin
        Trace.Text := GetLine;
        Trace.Before('$');
        Region.Entertainment := Trace.Num;
      end;

      // Products: 55 livestock [LIVE]...
      SeekLineContain(Keys[s_Products]);
      ReadBlockList(GetMultiline, Region.Products, ItemFormer, True);
      for i := 0 to Region.Products.Count-1 do
        SetFlag(Region.Products[i].Data.Flags, IT_RESOURCE, True);
    end;

    // Now find peasants by name (they should be already formed from ForSale)
    if Peasants <> nil then begin
      Peasants.Data := Game.ItemData.FindByName(peasants_name);
      if Peasants.Data <> nil then begin
        Item := ForSale.Find(Peasants.Data.Short);
        if Item <> nil then Peasants.Cost := Item.Cost;
        SetFlag(Peasants.Data.Flags, IT_MAN, True);
      end
      else FreeAndNil(Peasants);
    end;

    // Exits:
    SeekLineContain(Keys[s_Exits]);
    NextLine;
    while not EmptyLine(GetLine) do begin
      Trace.Text := GetMultiline;
      i := 1;
      while (i <= 6) and (Pos(GetKey(s_North, i - 1) + ' :', Trace.Text) = 0) do
        Inc(i);
      if i <= 6 then begin
        // North : plain (48,10) in Elgomaar, contains Cashmar [city].
        HasExit[i] := True;
        if Map.Levels[z].Name = '0 <nexus>' then Continue;
        Trace.Before(': ');
        ExitR := FormRegion(Trace.Text, False);
        // Form starting cities for Nexus
        if (ExitR.z <> z) then begin
          NewR := Map.SeekRegion(i, 0, z);
          NewR.Terrain := ExitR.Terrain;
          NewR.Land := ExitR.Land;
          NewR.Settlement := ExitR.Settlement;
          NewR.SettlementType := ExitR.SettlementType;
        end;
      end;
    end;

   // There is a Gate here (Gate 16 of 42).   - optional
   NextLine;
   if Pos(Keys[s_NexusGate], GetLine) > 0 then
     Gate := -1
   else if Pos(Keys[s_GateHere], GetLine) > 0 then begin
     Trace.Text := GetLine;
     Trace.Before(Keys[s_GateHere]);
     Gate := Trace.Num;

      if Pos(Keys[s_Of], Trace.Text) > 0 then begin
        Trace.Before(Keys[s_Of]);
        Turn.GateCount := Trace.Num;
      end
      else begin
        Turn.GateCount := 0;
      end;
   end;
  end; // with Region

  // Seek unit/Structures list, with lines starting from '-', '*' or '+'.
  //  'Orders' or '--' (next Region) means next section of rep.

  regRepEnd := RepPos;

  // - Workers (1565), Faction (162), 150 sea elves [SELF], 39 swords
  //    [SWOR]; My first unit.
  while (RepPos < RepLines.Count) do begin
   s := GetLine;
   if Pos(Keys[s_Orders],s) > 0 then break;
   if EmptyLine(s) then NextLine
   else
    case s[1] of
     '-' :
        if s[2] = '-' then
          Break
        else begin
          ReadUnit(Region, nil);
          regRepEnd := RepPos;
        end;
     '*' : begin
        ReadUnit(Region, nil);
        regRepEnd := RepPos;
     end;
     '+' : begin
        ReadStruct(Region);
        regRepEnd := RepPos;
     end;
     else NextLine;
    end;
  end;

  for i := regRepStart to regRepEnd do
    Result.Report.Add(RepLines[i]);

  Trace.Free;

  // Pickup inherited data
  PrevRegion := Map.Region(Region.Coords, Map.TurnNum-1);
  if PrevRegion <> nil then begin
    // Add gate
    if PrevRegion.Gate <> 0 then Region.Gate := PrevRegion.Gate;
    // Add products
    for i := 0 to PrevRegion.Products.Count-1 do begin
      j := 0;
      while (j < Region.Products.Count) and (Region.Products[j].Data.Short <>
        PrevRegion.Products[i].Data.Short) do Inc(j);
      if j = Region.Products.Count then begin
        Item := TItem.Create;
        Item.Amount := PrevRegion.Products[i].Amount;
        Item.Data := Game.ItemData.Seek(PrevRegion.Products[i].Data.Short);
        Region.Products.Add(Item);
      end;
    end;
  end;

  if (Region.Visited = Turn.Num) and (Turn.Regions.IndexOf(Region) = -1) then
    Turn.Regions.Add(Region);

  Map.TurnNum := Turn.Num;
end;

procedure ReadOrders;
var Trace: TTrace;
    U: TUnit;
begin
  Trace := TTrace.Create(GetLine);
  Trace.Before(Keys[s_OrderStart]);
  Trace.Before(' "');
  if not Trace.Ends and TurnFaction.Player then
    Game.Password := Trace.Before('"');

  { unit 6623
    ;ribak (6623), avoiding, behind, holding, 20 vikings [VIKI]
    @work }
  U := nil;
  while (RepPos < RepLines.Count) and (Pos(Keys[s_OrderEnd],
    GetLine) = 0) do begin
    if Pos(Keys[s_Unit], GetLine) = 1 then begin
      Trace.Text := GetLine;
      Trace.Before(Keys[s_Unit]);
      U := TurnFaction.Units.Find(Trace.Num);
      if U <> nil then U.Orders.Clear;
      NextLine;
    end
    else begin
      if not EmptyLine(Uncomment(GetLine)) and (U <> nil) then
        U.Orders.Add(TrimLeft(GetMultiLine))
      else NextLine;
    end;
  end;
  Trace.Free;
end;


 { Main method to parse whole rep and fill data structs (quite large, eh? :) }
function ReadRep(Report: TStrings; Log: TStrings): TRepReadStatus;
var Trace: TTrace;
    regions_found, errors: integer;
    FName: string;
    FNum, TurnNum: integer;

  procedure HandleError(msg: string);
  begin
   Inc(errors);
   Log.Add('    '+msg);
  end;

  function ReadSection(Key, Msg: string; Handler: TProcedure): boolean;
  begin
    Result := False;
    if (SeekLineContain(Key)) then
      try
        Log.Add(Msg);
        Handler;
        Result := True;
      except
        on E: ENaN do HandleError(E.Message);
        on E: EReportError do HandleError(E.Message);
      end
  end;

begin
  RepRead.Log := Log;
  Result := rrsOK;
  RepLines := Report;
  Trace := TTrace.Create('');

  try
    SeekStart;
    errors := 0;

    // Atlantis / Lorenai Report For:
    if (SeekLineContain(Keys[s_Header])) then
    try
      Log.Add('..Report header');
      NextLine;
      ReadHeader(FName, FNum, TurnNum);
      Game.Turns.Seek(TurnNum);
      // Init active faction if it's unknown
      if History.Factions[1].Num = 0 then begin
        Log.Add('  Active faction: ' + FName + ' (' + IntToStr(FNum) + ')');
        History.Factions[1].Num := FNum;
      end;
      // Get Factions[1] for active faction or Seek for allies
      if History.Factions[1].Num = FNum then TurnFaction := Faction
      else begin
        TurnFaction := Turn.Factions.Seek(FNum);
        Log.Add('  Report from ally faction');
      end;
      // Setup faction
      TurnFaction.Data := Game.FactionData.Seek(FNum);
      TurnFaction.Num := FNum;
      TurnFaction.Name := FName;
      // If that's active faction, update it in history
      if History.Factions[1].Num = FNum then
        History.Factions[1].Assign(TurnFaction, False);
      if TurnFaction.Player then ReadFP;
    except
      on E: ENaN do HandleError(E.Message);
    end
    else raise EWrongRep.Create('This maybe not an Atlantis report - wrong header.');

    if TurnFaction.Player then
      if not ReadSection(Keys[s_Status], '..Faction status', ReadStatus) then
        HandleError('Warning! Faction status not found!');

    if TurnFaction.Player then
      if not ReadSection(Keys[s_DeclaredAttitudes], '..Attitudes', ReadAttitudes) then
        HandleError('Warning! Attitudes not found!');

    if TurnFaction.Player then
      if not ReadSection(Keys[s_Unclaimed], '..Unclaimed silver',
        ReadUnclaimed) then HandleError('Warning! Unclaimed silver not found!');

    // Read Regions

    { plain (48,12) in Elgomaar, contains Kashmar [city], 5479 peasants (sea
    elves), $38353. }
    { Regions should be underlined by line with many '-' }
    regions_found := 0;
    while SeekLineContain('-------------') do
      try
        Inc(Regions_found);
        if regions_found = 1 then Log.Add('..Found 1 Region')
        else Log.Strings[Log.Count-1] := '..Found '+IntToStr(regions_found)+' Regions';
        repeat PrevLine until EmptyLine(GetLine);
        NextLine;
        ReadRegion;
        if Pos(Keys[s_Orders], GetLine) > 0 then break;
      except
        on E: ENaN do begin
          HandleError(E.Message);
          regions_found := 0;
        end;
      end;

    { Read info from Orders Template }

    if (SeekLineContain(Keys[s_Orders])) then
      try
        Log.Add('..Orders Template');
        SeekLineContain('#atlantis');
        ReadOrders;
      except
        on E: ENaN do HandleError(E.Message);
      end
    else if TurnFaction.Player then
      HandleError('Warning! Orders Template not found!');

    SeekStart;
    ReadSection(Keys[s_Errors], '..Errors', ReadErrors);
    ReadSection(Keys[s_Battles], '..Battles', ReadBattle);
    ReadSection(Keys[s_Events], '..Events', ReadEvents);
    ReadSection(Keys[s_SkillReps], '..Skill reports', ReadSkillDescriptions);
    ReadSection(Keys[s_ItemReps], '..Item reports', ReadItemDescriptions);
    ReadSection(Keys[s_ObjReps], '..Object reports', ReadObjectDescriptions);

    if (errors > 0) then begin
      Log.Add('Found '+IntToStr(errors)+' error(s)!');
      Result := rrsErrors;
    end
    else Log.Add('Report OK.');

  except
    on E: Exception do begin
      HandleError(E.Message);
      Result := rrsFailed;
    end;
  end;

  Log.Add('');
  Trace.Free;
end;

// Update incomplete item descriptions (if new FindByName items found)
procedure RereadIncomplete;
var i: integer;
begin
  for i := 0 to Game.ItemData.Count-1 do
    if Game.ItemData[i].Incomplete then
      ReadItemDescription(Game.ItemData[i]);

  {for i := 0 to Game.SkillData.Count-1 do
    if Game.SkillData[i].Incomplete then
      ReadSkillDescription(Game.SkillData[i]);
  - skills now can't be incomplete}

  for i := 0 to Game.StructData.Count-1 do
    if Game.StructData[i].Incomplete then
      ReadObjectDescription(Game.StructData[i]);
end;

procedure ImportMap;
var R, CurrR: TRegion;
    Item: TItem;
    i: integer;
begin
  Map.TurnNum := Turn.Num;
  TurnFaction := Turn.Factions[0];
  // Regions should be underlined by line with many '-'
  while SeekLineContain('-------------') do
    try
      repeat PrevLine until EmptyLine(GetLine) or (RepPos = 0);
      if RepPos > 0 then NextLine;
      R := ReadRegion;
      // If region not from current turn, update products
      if R.Visited <> Turn.Num then begin
        CurrR := Map.Region(R.Coords, Turn.Num);
        for i := 0 to R.Products.Count-1 do
          if CurrR.Products.Find(R.Products[i].Data.Short) = nil then begin
            Item := TItem.Create;
            Item.Assign(R.Products[i]);
            CurrR.Products.Add(Item);
          end;
      end;
    except
      on E: ENaN do raise;
    end;
end;

function ReadRepHeader(Report: TStrings; var FName: string; var FNum: integer;
  var TurnNum: integer): TRepReadStatus;
begin
  RepLines := Report;
  SeekStart;
  if (SeekLineContain(Keys[s_Header])) then
    try
      NextLine;
      ReadHeader(FName, FNum, TurnNum);
      Result := rrsOk;
    except
      on E: ENaN do Result := rrsErrors;
    end
  else Result := rrsErrors;
end;

function OrderTurn: integer;
var Trace: TTrace;
    month: string;
    year: integer;
begin
  Trace := TTrace.Create(GetLine);
  Trace.Before('; ');
  if not Trace.Ends then begin
    month := Trace.Before(', ' + Keys[s_Year] + ' ');
    year := Trace.Num;
    Result := MonthYearToTurn(month, year);
  end
  else Result := -1;
  Trace.Free;
end;

end.

