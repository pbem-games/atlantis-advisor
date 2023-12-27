unit uScript;

{$MODE Delphi}

interface

uses
  SysUtils, LCLIntf, LCLType, LMessages, Classes, DataStructs, uKeys, MyStrings, Resources,
  uGameSubs, Regexpr, Forms, uUnitRecs, Math;

var
  TerminatedQuery: function: boolean of object;
  Scripts: TStrings;
  Context: TObject;

  procedure RunScripts(AUnit: TUnit; Order: string; Errors: TStrings);
  procedure RunScript(AUnit: TUnit; Index: integer; Args: string; Errors: TStrings);
  function EvalExpression(s: string): string;
  procedure LoadScripts;
  function ScriptName(s: string): string;
  function ScriptIndex(name: string): integer;

implementation

uses
  uOrderProcessor, uVisualOrders;

const
  exprNot = 0;
  exprMul = 1;
  exprDiv = 2;
  exprAnd = 3;
  exprPlus = 4;
  exprMinus = 5;
  exprOr = 6;
  exprEQ = 7;
  exprNE = 8;
  exprNE1 = 9;
  exprGT = 10;
  exprLT = 11;
  exprGE = 12;
  exprLE = 13;
  exprPos = 14;
  exprCount = 15;

  Expressions: array[0..exprCount-1] of string = ('!', '*', '/', '&',
    '+', '-', '|', '=', '!=', '<>', '>', '<', '>=', '<=', '~');
  Priority: array[0..exprCount-1] of integer = (0, 1, 1, 1, 2, 2, 2, 3, 3,
    3, 3, 3, 3, 3, 2);

  MinPriority = 3;

type
  EScriptError = class(Exception);
  TIndexFinder = function(List: TList; s: string): integer;
  TListValue = function(Obj: TObject): string;

  TEvalRec = class
    Expr: integer;
    Value: string;
  end;

  TRecList = class(TList)
  protected
    function Get(Index: Integer): TEvalRec;
    procedure Put(Index: Integer; Item: TEvalRec);
  public
    property Items[Index: Integer]: TEvalRec read Get write Put; default;
  end;

var
  Regex: TRegexpr;
  Variables: TStrings;
  ScriptArgs: TStringArray;
  POutFrom, POutTo: ^integer;
  OutUnit: TUnit;
  Line: integer;


{ Identifiers }

function ItemFinder(List: TList; s: string): integer;
var i: integer;
begin
  i := 0;
  while (i < List.Count) and (TItem(List[i]).Data.Short <> UpperCase(s))
    and (TItem(List[i]).Data.SingleName <> LowerCase(s))
    and (TItem(List[i]).Data.MultiName <> LowerCase(s)) do Inc(i);
  if i < List.Count then Result := i
  else Result := -1;
end;

function ItemDataFinder(List: TList; s: string): integer;
var i: integer;
begin
  i := 0;
  while (i < List.Count) and (TItemData(List[i]).Short <> UpperCase(s))
    and (TItemData(List[i]).SingleName <> LowerCase(s))
    and (TItemData(List[i]).MultiName <> LowerCase(s)) do Inc(i);
  if i < List.Count then Result := i
  else Result := -1;
end;

function SkillFinder(List: TList; s: string): integer;
var i: integer;
begin
  i := 0;
  while (i < List.Count) and (TSkill(List[i]).Data.Short <> UpperCase(s))
    and (TSkill(List[i]).Data.Name <> LowerCase(s)) do Inc(i);
  if i < List.Count then Result := i
  else Result := -1;
end;

function SkillDataFinder(List: TList; s: string): integer;
var i: integer;
begin
  i := 0;
  while (i < List.Count) and (TSkillData(List[i]).Short <> UpperCase(s))
    and (TSkillData(List[i]).Name <> LowerCase(s)) do Inc(i);
  if i < List.Count then Result := i
  else Result := -1;
end;

function StructDataFinder(List: TList; s: string): integer;
var i: integer;
begin
  i := 0;
  while (i < List.Count) and (LowerCase(TStructData(List[i]).Group)
    <> LowerCase(s)) do Inc(i);
  if i < List.Count then Result := i
  else Result := -1;
end;

function ItemValue(Obj: TObject): string;
begin
  Result := IntToStr(TItem(Obj).Amount);
end;

function ItemDataValue(Obj: TObject): string;
begin
  Result := TItemData(Obj).Short;
end;

function SkillValue(Obj: TObject): string;
begin
  Result := IntToStr(TSkill(Obj).Level);
end;

function SkillDataValue(Obj: TObject): string;
begin
  Result := TSkillData(Obj).Short;
end;

function StructValue(Obj: TObject): string;
begin
  Result := IntToStr(TStruct(Obj).Num);
end;

function StructDataValue(Obj: TObject): string;
begin
  Result := TStructData(Obj).Group;
end;

function TroopValue(Obj: TObject): string;
begin
  Result := IntToStr(TTroop(Obj).Faction.Num);
end;

function UnitValue(Obj: TObject): string;
begin
  Result := IntToStr(TUnit(Obj).Num);
end;

procedure SetVariable(name, value: string);
begin
  Variables.Values[name] := '#' + value;
end;

function GetVariable(name: string): string;
begin
  Result := Variables.Values[name];
  if Result <> '' then Result := Copy(Result, 2, Length(Result));
end;

function ArrayIndex(s: string): integer;
var st, en: integer;
begin
  st := Pos('[', s);
  en := Pos(']', s);
  if (st = 0) or (en = 0) then Result := 0
  else Result := ToInt(Copy(s, st+1, en-st-1));
end;

function ArrayName(s: string): string;
begin
  if Pos('[', s) = 0 then Result := ''
  else Result := Copy(s, 1, Pos('[', s)-1);
end;

procedure GetArrayBounds(name: string; var lo, hi: integer);
var i, idx: integer;
begin
  if name = '' then Exit;
  lo := 0;
  hi := -1;
  for i := 0 to Variables.Count-1 do
    if ArrayName(Variables[i]) = name then begin
      idx := ArrayIndex(Variables[i]);
      if hi < lo then begin
        lo := idx;
        hi := idx;
      end
      else begin
        lo := Min(lo, idx);
        hi := Max(hi, idx);
      end;
    end;
end;

function ResolveVariable(id: string; var args: TStringArray): string;
var lo, hi, idx: integer;
begin
  Result := '';
  if Variables <> nil then begin
    Result := Variables.Values[id];
    if Result <> '' then begin
      Result := Copy(Result, 2, Length(Result));
      Exit;
    end;
    // Arrays (unresolved values)
    if Pos('[', id) > 0 then begin
      GetArrayBounds(ArrayName(id), lo, hi);
      idx := ArrayIndex(id);
      if (idx < lo) or (idx > hi) then
        raise EScriptError.Create('Range check error on index ' + IntToStr(idx));
      Exit;
    end;
  end;
end;

function ResolveIdentifier(id: string; var args: TStringArray;
  var Context: TObject; UseVariables: boolean): string;
var i, idx, hi, lo: integer;
    C: TCoords;
    found: boolean;
    Weather: TWeatherData;
    U: TUnit;

  function NumArg(index, min, max: integer): boolean;
  begin
    Result := True;
    if index >= Length(args) then
      raise EScriptError.Create('Not enough arguments');
    if not IsNumber(args[index]) then
      raise EScriptError.Create('Numeric argument expected');
    if (ToInt(args[index]) < min) or (ToInt(args[index]) > max) then
      raise EScriptError.Create('Argument out of range');
  end;

  function ArgToken(id, name: string): boolean;
  begin
    Result := (id = name);
    if Result and (Length(args) = 0) then
      raise EScriptError.Create('Argument expected');
  end;

  function ArgsToken(id, name: string; count: integer): boolean;
  begin
    Result := (id = name);
    if Result and (Length(args) < count) then
      raise EScriptError.Create(IntToStr(count)+' arguments expected');
  end;

  function NumArgToken(t, name: string; min, max: integer): boolean;
  begin
    Result := (t = name) and NumArg(0, min, max);
  end;

  function Direction(s: string): integer;
  begin
    if IsNumber(s) then Result := ToInt(s)
    else begin
      Result := 1;
      while (Result <= 6) and (s <> GetDir(Result)) do Inc(Result);
    end;
    if not (Result in [1..6]) then raise EScriptError.Create('Wrong direction')
  end;

  function Movement(s: string): integer;
  begin
    if IsNumber(s) and (ToInt(s) in [mtWalk..mtSwim]) then Result := ToInt(s)
    else if s = 'walk' then Result := mtWalk
    else if s = 'ride' then Result := mtRide
    else if s = 'fly' then Result := mtFly
    else if s = 'swim' then Result := mtSwim
    else raise EScriptError.Create('Wrong movement type');
  end;

  function ItemFlag(id: string): DWord;
  begin
    if      id = 'wagon'    then Result := IT_WAGON
    else if id = 'silver'   then Result := IT_SILVER
    else if id = 'man'      then Result := IT_MAN
    else if id = 'monster'  then Result := IT_MONSTER
    else if id = 'magic'    then Result := IT_MAGIC
    else if id = 'weapon'   then Result := IT_WEAPON
    else if id = 'armor'    then Result := IT_ARMOR
    else if id = 'mount'    then Result := IT_MOUNT
    else if id = 'tool'     then Result := IT_TOOL
    else if id = 'cantgive' then Result := IT_CANTGIVE
    else if id = 'resource' then Result := IT_RESOURCE
    else if id = 'advanced' then Result := IT_ADVANCED
    else if id = 'food'     then Result := IT_FOOD
    else if id = 'trade'    then Result := IT_TRADE
    else Result := 0;
  end;

  function WeaponClass(s: string): integer;
  begin
    Result := wcCount-1;
    while (Result >= 0) and (GetKey(s_wcSlashing, Result) <> s) do Dec(i);
    if Result < 0 then raise EScriptError.Create('Wrong weapon class');
  end;

  function AttackType(s: string): integer;
  begin
    Result := atCount-1;
    while (Result >= 0) and (GetKey(s_atMelee, Result) <> s) do Dec(i);
    if Result < 0 then raise EScriptError.Create('Wrong attack type');
  end;

  function GetListValue(List: TList; IndexFinder: TIndexFinder;
    ListValue: TListValue): string;
  var idx: integer;
  begin
    Result := '';
    if Length(args) = 0 then
      Result := IntToStr(List.Count)
    else begin
      // Find item index
      if IsNumber(args[0]) then idx := ToInt(args[0])
      else if Assigned(IndexFinder) then begin
        idx := IndexFinder(List, args[0]);
        if idx = -1 then Exit;
      end
      else raise EScriptError.Create('List index or name expected');
      // Return item value
      if (idx >= 0) and (idx < List.Count) then begin
        Result := ListValue(List[idx]);
        Context := List[idx];
      end
      else raise EScriptError.Create('List index out of range');
    end;
  end;

  function CanLearn(U: TUnit; Skill: string): string;
  var
    sd: TSkillData;
    sk: TSkill;
  begin
    Result := BoolToStr(false);

    sd := Game.SkillData.FindByName(Skill);
    if sd = nil then
      Exit;

    sk := U.Skills.Find(sd.Short);
    Result := BoolToStr((sk = nil) or (sk.Level < MaxSkillLevel(U, sd)));
  end;

  function StructDataResult(Data: TStructData; var found: boolean): string;
  begin
    Result := '';
    if id = 'direction' then begin
      idx := StructDirection(Data.Group);
      if idx in [1..6] then Result := GetDir(idx);
    end
    else if id = 'type' then Result := Data.Group
    else if id = 'defence' then Result := BoolToStr(Test(Data.Flags, ST_DEFENCE))
    // TODO: Add Fleet processing here
    else if id = 'transport' then Result := BoolToStr(Test(Data.Flags, ST_TRANSPORT))
    else if id = 'flying' then Result := BoolToStr(Test(Data.Flags, ST_FLYING))
    else if id = 'closed' then Result := BoolToStr(Test(Data.Flags, ST_CLOSED))
    else if id = 'innerlocation' then Result := BoolToStr(Test(Data.Flags, ST_SHAFT))
    else if id = 'road' then Result := BoolToStr(Test(Data.Flags, ST_ROAD))
    else if id = 'size' then Result := IntToStr(Data.Size)
    else if id = 'protection' then Result := IntToStr(Data.Protection)
    else if id = 'capacity' then Result := IntToStr(Data.Capacity)
    else if id = 'sailors' then Result := IntToStr(Data.Sailors)
    else if id = 'material1' then begin
      if Data.Material1 <> nil then Result := Data.Material1.Short
    end
    else if id = 'material2' then begin
      if Data.Material2 <> nil then Result := Data.Material2.Short
    end
    else if id = 'buildskill' then begin
      if Data.BuildSkill <> nil then Result := Data.BuildSkill.Data.Short;
      Context := Data.BuildSkill;
    end
    else if id = 'resource' then begin
      if Data.Resource <> nil then Result := Data.Resource.Short
    end
    // gamesubs
    else if id = 'load' then
      Result := IntToStr(StructCarriedWeight(TStruct(Context)))
    else found := False;
  end;

  function ItemDataResult(Data: TItemData; var found: boolean): string;
  begin
    Result := '';
    if id = 'name'    then Result := Data.Name
    else if id = 'short'   then Result := Data.Short
    else if id = 'weight'  then Result := IntToStr(Data.Weight)
    else if ArgToken(id, 'capacity') then
      Result := IntToStr(Data.Moves[Movement(args[0])])
    else if id = 'description' then Result := Data.Description
    // flags
    else if ItemFlag(id) > 0 then
      Result := BoolToStr(Test(Data.Flags, ItemFlag(id)))
    // production
    else if id = 'produceskill' then begin
      if Data.Produce.Skill <> nil then Result := Data.Produce.Skill.Data.Short;
      Context := Data.Produce.Skill;
    end
    else if id = 'producerate' then Result := IntToStr(Data.Produce.Rate)
    else if id = 'producemanmonths' then Result := IntToStr(Data.Produce.ManMonths)
    else if id = 'producematerials' then
      Result := GetListValue(Data.Produce.Materials, ItemFinder, ItemValue)
    // mag production
    else if id = 'magproduceskill' then begin
      if Data.MagProduce.Skill <> nil then
        Result := Data.MagProduce.Skill.Data.Short;
      Context := Data.MagProduce.Skill;
    end
    else if id = 'magproducematerials' then
      Result := GetListValue(Data.MagProduce.Materials, ItemFinder, ItemValue)
    // man
    else if id = 'manleader' then Result := BoolToStr(Data.Man.Leader)
    else if id = 'mandeflevel' then Result := IntToStr(Data.Man.DefLevel)
    else if id = 'manspeclevel' then begin
      if Data.Man.Leader then Result := '5'
      else if Data.Man.SpecSkills.Count > 0 then
        Result := IntToStr(Data.Man.SpecSkills[0].Level)
      else Result := '3';
    end
    else if id = 'manspecskills' then
      Result := GetListValue(Data.Man.SpecSkills, SkillFinder, SkillValue)
    // weapon
    else if id = 'wpnneedskill' then
      Result := BoolToStr(Test(Data.Weapon.Flags, WPN_NEEDSKILL))
    else if id = 'wpnnofoot' then
      Result := BoolToStr(Test(Data.Weapon.Flags, WPN_NOFOOT))
    else if id = 'wpnnomount' then
      Result := BoolToStr(Test(Data.Weapon.Flags, WPN_NOMOUNT))
    else if id = 'wpnshort' then
      Result := BoolToStr(Test(Data.Weapon.Flags, WPN_SHORT))
    else if id = 'wpnlong' then
      Result := BoolToStr(Test(Data.Weapon.Flags, WPN_LONG))
    else if id = 'wpnranged' then
      Result := BoolToStr(Test(Data.Weapon.Flags, WPN_RANGED))
    else if id = 'wpnnoattackerskill' then
      Result := BoolToStr(Test(Data.Weapon.Flags, WPN_NOATTACKERSKILL))
    else if id = 'wpnridingbonus' then
      Result := BoolToStr(Test(Data.Weapon.Flags, WPN_RIDINGBONUS))
    else if id = 'wpnridingbonusdefence' then
      Result := BoolToStr(Test(Data.Weapon.Flags, WPN_RIDINGBONUSDEFENSE))
    else if id = 'wpnnumattskill' then
      Result := BoolToStr(Test(Data.Weapon.Flags, WPN_NUMATTSKILL))
    else if id = 'wpnnumatthalfskill' then
      Result := BoolToStr(Test(Data.Weapon.Flags, WPN_NUMATTHALFSKILL))
    else if id = 'wpnclass' then Result := GetKey(s_wcSlashing, Data.Weapon.WpnClass)
    else if id = 'wpnattacktype' then Result := GetKey(s_atMelee, Data.Weapon.AttackType)
    else if id = 'wpnskill1' then begin
      if Data.Weapon.Skill1 <> nil then Result := Data.Weapon.Skill1.Short
    end
    else if id = 'wpnskill2' then begin
      if Data.Weapon.Skill2 <> nil then Result := Data.Weapon.Skill2.Short
    end
    else if id = 'wpnattackbonus' then Result := IntToStr(Data.Weapon.AttackBonus)
    else if id = 'wpndefencebonus' then Result := IntToStr(Data.Weapon.DefenceBonus)
    else if id = 'wpnmountbonus' then Result := IntToStr(Data.Weapon.MountBonus)
    else if id = 'wpnnumattacks' then Result := IntToStr(Data.Weapon.NumAttacks)
    // armor
    else if id = 'armoruseinass' then
      Result := BoolToStr(Test(Data.Armor.Flags, ARM_USEINASS))
    else if id = 'armordefence' then begin
      if Length(args) > 0 then
        Result := IntToStr(Data.Armor.Defence[WeaponClass(args[0])])
      else Result := IntToStr(Data.Armor.Defence[0]);
    end
    // mount
    else if id = 'mountskill' then begin
      if Data.Mount.RideSkill <> nil then Result := Data.Mount.RideSkill.Short
    end
    else if id = 'mountminbonus' then Result := IntToStr(Data.Mount.MinBonus)
    else if id = 'mountmaxbonus' then Result := IntToStr(Data.Mount.MaxBonus)
    else if id = 'mountmaxhamperedbonus' then Result := IntToStr(Data.Mount.MaxHamperedBonus)
    // monster
    else if id = 'monsterattack' then Result := IntToStr(Data.Monster.Attack)
    else if id = 'monsternumattacks' then Result := IntToStr(Data.Monster.NumAttacks)
    else if id = 'monsterhits' then Result := IntToStr(Data.Monster.Hits)
    else if id = 'monsterregen' then Result := IntToStr(Data.Monster.Regen)
    else if id = 'monstertactics' then Result := IntToStr(Data.Monster.Tactics)
    else if NumArgToken(id, 'monsterdefence', 0, atCount-1) then
      Result := IntToStr(Data.Monster.Defence[ToInt(args[0])])
    else found := False;
  end;

  function SkillDataResult(Data: TSkillData; var found: boolean): string;
  begin
    Result := '';
    if id = 'name'    then Result := Data.MakeName
    else if id = 'short'   then Result := Data.Short
    else if NumArgToken(id, 'description', 1, 5) then
      Result := Data.Descriptions[ToInt(args[0])]
    else if id = 'cost'    then Result := IntToStr(Data.Cost)
    else if id = 'common'  then Result := BoolToStr(not Test(Data.Flags,
      SK_MAGIC) and not Test(Data.Flags, SK_FOUNDATION))
    else if id = 'magic'   then Result := BoolToStr(Test(Data.Flags, SK_MAGIC))
    else if id = 'foundation' then Result := BoolToStr(Test(Data.Flags, SK_FOUNDATION))
    else if id = 'combatspell' then Result := BoolToStr(Test(Data.Flags, SK_COMBATSPELL))
    else if id = 'cast' then Result := BoolToStr(Test(Data.Flags, SK_CAST))
    else if id = 'basedon' then
      Result := GetListValue(Data.BasedOn, SkillFinder, SkillValue)
    else found := False;
  end;

begin
  Result := '';

  if UseVariables and (Variables <> nil) then begin
    Result := Variables.Values[id];
    if Result <> '' then begin
      Result := Copy(Result, 2, Length(Result));
      Exit;
    end;
    // Arrays (unresolved values)
    if Pos('[', id) > 0 then begin
      GetArrayBounds(ArrayName(id), lo, hi);
      idx := ArrayIndex(id);
      if (idx < lo) or (idx > hi) then
        raise EScriptError.Create('Range check error on index ' + IntToStr(idx));
      Exit;
    end;
  end;

  found := True;

  // Context symbols

  // Regex context
  if Context.ClassType = TRegExpr then begin

     if ArgsToken(id, 'exec', 2) then begin
       Regex.Expression := args[0];
       if Regex.Exec(args[1]) then
         Result := IntToStr(Regex.SubExprMatchCount+1)
       else Result := '';
     end
     else if NumArgToken(id, 'match', 0, Regex.SubExprMatchCount) then
       Result := RegEx.Match[ToInt(args[0])]
     else if NumArgToken(id, 'matchpos', 0, Regex.SubExprMatchCount) then
       Result := IntToStr(RegEx.MatchPos[ToInt(args[0])])
     else if id = 'execnext' then begin
       if Regex.ExecNext then Result := IntToStr(Regex.SubExprMatchCount+1)
       else Result := '';
     end
     else if ArgsToken(id, 'replace', 3) then begin
       Regex.Expression := args[1];
       Result := Regex.Replace(args[0], args[2], True);
     end
     else found := False;

  end

  // Region context
  else if Context.ClassType = TRegion then begin
    with TRegion(Context) do begin

      if      id = 'x'       then Result := IntToStr(X)
      else if id = 'y'       then Result := IntToStr(Y)
      else if id = 'z'       then Result := Map.Levels[Z].Name
      else if id = 'coords'  then Result := CoordsToStr(Coords)
      else if id = 'terrain' then Result := Terrain.Name
      else if id = 'land'    then Result := Land
      else if id = 'visible' then Result := BoolToStr(Visited = Turn.Num)
      else if id = 'explored' then Result := BoolToStr(FullData)
      else if ArgToken(id, 'exit') then begin
        idx := Direction(args[0]);
        if HasExit[idx] then begin
          C := CoordsInDir(Coords, idx);
          Result := CoordsToStr(C);
          Context := Map.Region(C);
        end
        else Context := nil;
      end

      else if FullData then begin
        if      id = 'settlement' then Result := Settlement
        else if id = 'settlementtype' then begin
          if SettlementType > 0 then
            Result := GetKey(s_Village, SettlementType - 1);
        end
        else if id = 'peasants' then begin
          if Peasants <> nil then Result := Peasants.Data.Short;
          Context := Peasants;
        end
        else if id = 'taxrate'   then Result := IntToStr(TaxRate)
        else if id = 'wages'     then Result := IntToStr(Wages)
        else if id = 'maxwages'  then Result := IntToStr(MaxWages)
        else if id = 'entertainment' then Result := IntToStr(Entertainment)
        else if id = 'weather'   then begin
          Weather := GetWeather(Coords);
          if Weather <> nil then Result := Weather.Text;
        end
        else if id = 'gate'      then Result := IntToStr(Gate)
        else if id = 'guard' then begin
          if Guard <> nil then Result := IntToStr(Guard.Num)
        end
        else if id = 'battles' then begin
          if Battles <> nil then Result := IntToStr(Battles.Count);
        end
        else if id = 'notes'     then Result := Notes.Text
        // gamesubs
        else if ArgsToken(id, 'movecost', 2) then begin
          C := CoordsInDir(Coords, Direction(args[0]));
          if MovableDir(Coords, Direction(args[0]), False, Movement(args[1]),
            (Movement(args[1]) = mtSwim), nil) then
            Result := IntToStr(EnterCost(Movement(args[1]), C, Coords, False));
        end
        else if id = 'nextnew' then
          Result := IntToStr(NextAttemptedNew(TRegion(Context)))
        // lists
        else if id = 'wanted' then
          Result := GetListValue(Wanted, ItemFinder, ItemValue)
        else if id = 'forsale' then
          Result := GetListValue(ForSale, ItemFinder, ItemValue)
        else if id = 'products' then
          Result := GetListValue(Products, ItemFinder, ItemValue)
        else if id = 'troops' then
          Result := GetListValue(Troops, nil, TroopValue)
        else if id = 'objects' then
          Result := GetListValue(Structs, nil, StructValue)
        else found := False;

      end
      else found := False;

    end
  end

  // Unit context
  else if Context.ClassType = TUnit then begin
    U := TUnit(Context);

    if id = 'name' then Result := U.Name
    else if id = 'num' then Result := U.NumStr
    else if id = 'description' then Result := U.Description
    else if id = 'onguard' then Result := BoolToStr(U.Flags[flgGuard])
    else if id = 'autotax' then Result := BoolToStr(U.Flags[flgTax])
    else if id = 'avoiding' then Result := BoolToStr(U.Flags[flgAvoid])
    else if id = 'behind'  then Result := BoolToStr(U.Flags[flgBehind])
    else if id = 'holding' then Result := BoolToStr(U.Flags[flgHold])
    else if id = 'noaid'   then Result := BoolToStr(U.Flags[flgNoaid])
    else if id = 'nocross' then Result := BoolToStr(U.Flags[flgNocross])
    else if id = 'consume' then Result := ExtFlags[0, U.Consuming]
    else if id = 'reveal' then Result := ExtFlags[1, U.Revealing]
    else if id = 'spoils' then Result := ExtFlags[2, U.Spoils]
    else if id = 'tradeincome' then Result := IntToStr(U.TradeIncome)
    else if id = 'workincome' then Result := IntToStr(U.WorkIncome)
    else if id = 'region' then begin
      if Length(args) = 0 then begin
        Result := CoordsToStr(U.Region.Coords);
        Context := U.Region;
      end
      else found := False; // region(...) - context function
    end
    else if id = 'faction' then begin
      if Length(args) = 0 then begin
        Result := IntToStr(U.Faction.Num);
        Context := U.Faction;
      end
      else found := False; // faction(...) - context function
    end
    else if id = 'object' then begin
      if U.Struct <> nil then Result := IntToStr(U.Struct.Num);
      Context := U.Struct;
    end
    else if id = 'combatspell' then begin
      if U.CombatSpell <> nil then Result := U.CombatSpell.Short
    end
    else if id = 'former' then begin
      if U.Former <> nil then Result := U.Former.NumStr;
      Context := U.Former;
    end
    else if id = 'ismage' then Result := BoolToStr(U.Mage)
    else if id = 'localdescription' then
      Result := UnitRecs.Local(U.Num, U.Region.Coords)
    else if ArgToken(id, 'setlocaldescription') then
      UnitRecs.AddUnitRec(U, args[0])
    // lists
    else if id = 'items' then
      Result := GetListValue(U.Items, ItemFinder, ItemValue)
    else if id = 'skills' then
      Result := GetListValue(U.Skills, SkillFinder, SkillValue)
    else if id = 'canstudy' then
      Result := GetListValue(U.CanStudy, SkillDataFinder, SkillDataValue)
    else if id = 'canlearn' then
      Result := CanLearn(U, args[0])
    else if ArgToken(id, 'amountof') then begin
      idx := ItemFlag(args[0]);
      if idx > 0 then Result := IntToStr(U.Items.Amount(idx))
      else raise EScriptError.Create('Wrong item flag');
    end
    // gamesubs
    else if NumArgToken(id, 'load', mtWalk, mtSwim) then
      Result := IntToStr(UnitLoad(U, ToInt(args[0])))
    else if NumArgToken(id, 'capacity', mtWalk, mtSwim) then
      Result := IntToStr(UnitCapacity(U, ToInt(args[0])))
    else if id = 'movementtype' then
      Result := IntToStr(MovementType(U))
    else if id = 'taxers' then
      Result := IntToStr(Taxers(U))
    // orders
    else if id = 'allorders' then Result := U.Orders.Text
    else if id = 'monthorder' then Result := U.MonthOrder
    else if NumArgToken(id, 'delorder', 0, U.Orders.Count-1) then begin
      idx := ToInt(args[0]);
      U.Orders.Delete(idx);
      if U = OutUnit then begin
        if idx < POutFrom^ then Dec(POutFrom^);
        if idx < POutTo^ then Dec(POutTo^);
      end;
    end
    else if ArgsToken(id, 'insorder', 2) and NumArg(0, 0, MaxInt) then begin
      idx := Min(ToInt(args[0]), U.Orders.Count);
      U.Orders.Insert(idx, args[1]);
      if U = OutUnit then begin
        if idx < POutFrom^ then Inc(POutFrom^);
        if idx < POutTo^ then Inc(POutTo^);
      end;
    end
    else if id = 'orders' then begin
      if Length(args) = 0 then Result := IntToStr(U.Orders.Count)
      else if NumArg(0, 0, U.Orders.Count-1) then
        Result := U.Orders[ToInt(args[0])];
    end
    else if id = 'removescript' then begin
      idx := POutFrom^ - 1;
      if (idx >= 0) and (idx < U.Orders.Count) then begin
        U.Orders.Delete(idx);
        Dec(POutFrom^);
        Dec(POutTo^);
      end;
      idx := POutTo^;
      if (idx >= 0) and (idx < U.Orders.Count) then
        U.Orders.Delete(idx);
    end
    else if id = 'events' then begin
      if Length(args) = 0 then Result := IntToStr(U.Events.Count)
      else if NumArg(0, 0, U.Events.Count-1) then
        Result := U.Events[ToInt(args[0])];
    end
    // Contexts
    else if id = 'troop' then begin
      Context := U.Region.Troops.Find(U.Faction.Num);
      Result := IntToStr(U.Faction.Num);
    end
    else found := False;
  end

  // Faction context
  else if Context.ClassType = TFaction then
    with TFaction(Context) do begin

      if id = 'name' then Result := Name
      else if id = 'num' then Result := IntToStr(Num)
      else if id = 'attitude' then begin
        if Attitude > 0 then Result := GetKey(s_Hostile, Attitude - 1)
        else Result := GetKey(s_Hostile, VFaction.Attitude - 1);
      end
      else if id = 'units' then
        Result := GetListValue(Units, nil, UnitValue)
      else found := False;

    end

  // Troop context
  else if Context.ClassType = TTroop then
    with TTroop(Context) do begin

      if id = 'name' then Result := Faction.Name
      else if id = 'num' then Result := IntToStr(Faction.Num)
      else if id = 'attitude' then begin
        if Faction.Attitude > 0 then Result := GetKey(s_Hostile, Faction.Attitude - 1)
        else Result := GetKey(s_Hostile, VFaction.Attitude - 1);
      end
      else if id = 'units' then
        Result := GetListValue(Units, nil, UnitValue)
      else found := False;

    end

  // Struct context
  else if Context.ClassType = TStruct then
    with TStruct(Context) do begin

      if id = 'name' then Result := Name
      else if id = 'num' then Result := IntToStr(Num)
      else if id = 'description' then Result := Description
      else if id = 'needs' then Result := IntToStr(Needs)
      else if id = 'link' then begin
        if HasExit then Result := CoordsToStr(Passage);
        Context := Map.Region(Passage);
      end
      else if id = 'owner' then begin
        if Owner <> nil then Result := Owner.NumStr;
        Context := Owner;
      end
      else Result := StructDataResult(Data, found);

    end

  // StructData context
  else if Context.ClassType = TStructData then
    Result := StructDataResult(TStructData(Context), found)

  // Item context
  else if Context.ClassType = TItem then
    with TItem(Context) do begin

      if      id = 'amount'  then Result := IntToStr(Amount)
      else if id = 'cost'    then Result := IntToStr(Cost)
      else Result := ItemDataResult(Data, found);

    end

  // ItemData context
  else if Context.ClassType = TItemData then
    Result := ItemDataResult(TItemData(Context), found)

  // Skill context
  else if Context.ClassType = TSkill then
    with TSkill(Context) do begin

      if      id = 'level'   then Result := IntToStr(Level)
      else if id = 'days'    then Result := IntToStr(Points)
      else Result := SkillDataResult(Data, found);

    end

  else if Context.ClassType = TSkillData then
    Result := SkillDataResult(TSkillData(Context), found);

  // Non-context symbols

  if not found then begin
    found := True;

    // ScriptArgs
    if (id = 'args') and NumArg(0, 0, Length(ScriptArgs)-1) then
      Result := ScriptArgs[ToInt(args[0])]
    else if id = 'argscount' then Result := IntToStr(Length(ScriptArgs))

    // Global Variables
    else if ArgToken(id, 'global') then
      Result := GameConfig.ReadString('ScriptVariables', args[0], '')
    else if (id = 'setglobal') and (Length(args) >= 2) then
      GameConfig.WriteString('ScriptVariables', args[0], args[1])

    // Constants
    else if id = 'true' then Result := '1'
    else if id = 'false' then Result := ''

    // Functions
    else if ArgToken(id, 'length') then Result := IntToStr(Length(args[0]))
    else if ArgToken(id, 'high') then begin
      GetArrayBounds(args[0], lo, hi);
      Result := IntToStr(hi);
    end
    else if ArgToken(id, 'low') then begin
      GetArrayBounds(args[0], lo, hi);
      Result := IntToStr(lo);
    end
    else if (id = 'substr') and NumArg(1, 0, MaxInt)
      and NumArg(2, 0, MaxInt) then
      Result := Copy(args[0], ToInt(args[1]), ToInt(args[2]))
    else if id = 'dir' then
      Result := GetDir(Direction(args[0]))
    else if ArgToken(id, 'levelindex') then begin
      idx := Map.Levels.NumOf(args[0]);
      if idx >= 0 then Result := IntToStr(idx);
    end
    else if ArgToken(id, 'bool') then Result := BoolToStr(StrToBool(args[0]))
    else if NumArgToken(id, 'random', 0, MaxInt) then
      Result := IntToStr(Random(ToInt(args[0])))
    else if id = 'turn' then Result := IntToStr(Turn.Num)
    else if ArgToken(id, 'eval') then Result := EvalExpression(args[0])

    // Context functions
    else if id = 'regex' then Context := Regex
    else if NumArgToken(id, 'unit', 0, MaxInt) then begin
      Context := FindUnit(ToInt(args[0]));
      if Context <> nil then Result := TUnit(Context).NumStr;
    end
    else if (id = 'faction') and NumArg(0, 0, MaxInt) then begin
      Context := FindFaction(ToInt(args[0]));
      if Context <> nil then Result := IntToStr(TFaction(Context).Num);
    end
    else if ArgToken(id, 'region') then begin
      Context := Map.Region(StrToCoords(args[0]));
      if Context <> nil then Result := CoordsToStr(TRegion(Context).Coords);
    end
    else if id = 'allitems' then
      Result := GetListValue(Game.ItemData, ItemDataFinder, ItemDataValue)
    else if id = 'allskills' then
      Result := GetListValue(Game.SkillData, SkillDataFinder, SkillDataValue)
    else if id = 'allobjects' then
      Result := GetListValue(Game.StructData, StructDataFinder, StructDataValue)
    else if id = 'unclaimed' then
      Result := IntToStr(VTurn.Unclaimed)
    // Settings
    else if id = 'leadermaintenance' then
      Result := IntToStr(Config.ReadInteger('Settings', 'LeaderMaintenance', 20))
    else if id = 'peasantmaintenance' then
      Result := IntToStr(Config.ReadInteger('Settings', 'PeasantMaintenance', 10))
    else if id = 'entertainincome' then
      Result := IntToStr(Config.ReadInteger('Settings', 'EntertainIncome', 20))
    else if id = 'taxincome' then
      Result := IntToStr(Config.ReadInteger('Settings', 'TaxIncome', 50))
    else if id = 'studentsperteacher' then
      Result := IntToStr(Config.ReadInteger('Settings', 'StudentsPerTeacher', 10))
    else if id = 'healsperman' then
      Result := IntToStr(Config.ReadInteger('Settings', 'HealsPerMan', 5))
    else if id = 'flyingcrosswater' then
      Result := BoolToStr(Config.ReadBool('Settings', 'FlyingCross', True))

    else found := False;
  end;

  // All symbols checked
  if not found then raise EScriptError.Create('Undefined symbol: ' + id);
end;

procedure SplitParams(s: string; var A: TStringArray; Eval: boolean);
var i: integer;
    t: string;
begin
  SetLength(A, 0);
  i := 1;
  while i <= Length(s) do begin
    SetLength(A, Length(A) + 1);
    t := '';
    while (i <= Length(s)) and (s[i] <> ',') do
      t := t + StrQuotes(s, i);
    Inc(i);
    if Eval then A[Length(A)-1] := EvalExpression(Trim(t))
    else A[Length(A)-1] := Trim(t);
  end;
end;

function GetExpr(s: string; var i: integer): integer;
var t: string;
    j: integer;
begin
  Result := -1;
  t := '';
  while (i <= Length(s)) and (s[i] = ' ') do Inc(i);
  if i > Length(s) then Exit;
  while (i <= Length(s)) and (s[i] in ['+', '-', '*', '/', '!', '=', '<', '>',
    '|', '&', '~']) do begin
    t := t + s[i];
    Inc(i);
  end;
  // Return expression
  j := 0;
  while (j < exprCount) and (t <> Expressions[j]) do Inc(j);
  if j < exprCount then Result := j;
end;

function GetNumber(s: string; var i: integer): string;
begin
  Result := '';
  if i > Length(s) then Exit;
  if s[i] = '-' then begin
    Result := Result + '-';
    Inc(i);
  end;
  while (i <= Length(s)) and (s[i] in ['0'..'9']) do begin
    Result := Result + s[i];
    Inc(i);
  end;
end;

function GetIdentifier(s: string; var i: integer): string;
begin
  Result := '';
  if i > Length(s) then Exit;
  SkipSpaces(s, i);
  while (i <= Length(s)) and (s[i] in ['0'..'9', 'A'..'Z',
    'a'..'z', '_']) do begin
    Result := Result + s[i];
    Inc(i);
  end;
  if Copy(s, i, 1) = '[' then
    Result := Result + '[' + EvalExpression(ParensContents(s, i, '[', ']')) + ']';
  Result := LowerCase(Result);
end;

function CheckIdentifier(s, ident: string): boolean;
var i: integer;
begin
  i := 1;
  Result := (ident = GetIdentifier(s, i));
end;

function GetToken(s: string; var i: integer; var LContext: TObject): string;
var name: string;
    invert, first: boolean;
    A: TStringArray;
begin
  Result := '';
  SkipSpaces(s, i);
  // NOT operator
  if Copy(s, i, 1) = '!' then begin
    invert := True;
    Inc(i);
    SkipSpaces(s, i);
  end
  else invert := False;
  if i > Length(s) then Exit;
  // Evaluate token by first char
  case s[i] of
    '(':
      Result := EvalExpression(ParensContents(s, i));
    '"', '''':
      Result := QuotesContents(s, i);
    '-', '0'..'9': // number
      Result := GetNumber(s, i);
    else begin // identifier
      LContext := Context;
      first := True;
      repeat
        name := GetIdentifier(s, i);
        if Copy(s, i, 1) = '(' then
          SplitParams(ParensContents(s, i), A, ((name <> 'high')
            and (name <> 'low')))
        else SetLength(A, 0);
        Result := ResolveIdentifier(name, A, LContext, first);
        first := False;
        if Copy(s, i, 1) = '.' then Inc(i);
      until Copy(s, i-1, 1) <> '.';
    end;
  end;
  if invert then Result := BoolToStr(not StrToBool(Result));
end;

procedure UnparsedToken(s: string; var i: integer);
begin
  SkipSpaces(s, i);
  // NOT operator
  if Copy(s, i, 1) = '!' then begin
    Inc(i);
    SkipSpaces(s, i);
  end;
  if i > Length(s) then Exit;
  // Get token by first char
  case s[i] of
    '(':
      StrParens(s, i);
    '"', '''':
      StrQuotes(s, i);
    '0'..'9': // number
      GetNumber(s, i);
    else begin // identifier
      repeat
        GetIdentifier(s, i);
        if Copy(s, i, 1) = '(' then StrParens(s, i);
        if Copy(s, i, 1) = '.' then Inc(i);
      until Copy(s, i-1, 1) <> '.';
    end;
  end;
end;

function NumericArray(L: TRecList): boolean;
var i: integer;
begin
  i := 0;
  while (i < L.Count) and IsNumber(L[i].Value) do Inc(i);
  Result := (i = L.Count);
end;

function NumEval(L: TRecList; i: integer): string;
var A, B, C: integer;
begin
  if L[i].Value = '' then A := 0
  else A := ToInt(L[i].Value);
  if L[i+1].Value = '' then B := 0
  else B := ToInt(L[i+1].Value);
  case L[i+1].Expr of
    exprPlus:   C := A + B;
    exprMinus:  C := A - B;
    exprMul:    C := A * B;
    exprDiv:    C := A div B;
    exprEQ:     if A = B then C := 1 else C := 0;
    exprNE, exprNE1: if A <> B then C := 1 else C := 0;
    exprGT:     if A > B then C := 1 else C := 0;
    exprLT:     if A < B then C := 1 else C := 0;
    exprGE:     if A >= B then C := 1 else C := 0;
    exprLE:     if A <= B then C := 1 else C := 0;
    exprAnd:    if IntToBool(A) and IntToBool(B) then C := 1 else C := 0;
    exprOr:     if IntToBool(A) or IntToBool(B) then C := 1 else C := 0;
    exprPos:    C := Pos(IntToStr(B), IntToStr(A));
    else raise EScriptError.Create('Operand "' + Expressions[L[i+1].Expr] +
      '" not applicable for operators "' + L[i].Value + '" and "' +
      L[i+1].Value + '"');
  end;
  L[i].Value := IntToStr(C);
  L.Delete(i+1);
end;

function StrEval(L: TRecList; i: integer): string;
var A, B, C: string;
begin
  A := L[i].Value;
  B := L[i+1].Value;
  case L[i+1].Expr of
    exprPlus:   C := A + B;
    exprEQ:     if A = B then C := '1' else C := '0';
    exprNE, exprNE1: if A <> B then C := '1' else C := '0';
    exprAnd:    C := BoolToStr(StrToBool(A) and StrToBool(B));
    exprOr:     C := BoolToStr(StrToBool(A) or StrToBool(B));
    exprPos:    C := IntToStr(Pos(B, A));
    else raise EScriptError.Create('Operand "' + Expressions[L[i+1].Expr] +
      '" not applicable for operators "' + A + '" and "' + B +
      '" in string context');
  end;
  L[i].Value := C;
  L.Delete(i+1);
end;

// Evaluate expression string
function EvalExpression(s: string): string;
var L: TRecList;
    Rec: TEvalRec;
    i, j, p, chain: integer;
    numeric: boolean;
    dummy: TObject;
begin
  Result := '';
  s := Trim(s);
  if s = '' then Exit;
  L := TRecList.Create;
  p := 1;
  chain := -1;
  L.Add(TEvalRec.Create);
  L[0].Value := GetToken(s, p, dummy);
  // Parse expression
  while p <= Length(s) do begin
    Rec := TEvalRec.Create;
    // First token: operand (may be empty for first operator)
    Rec.Expr := GetExpr(s, p);
    if (Rec.Expr = -1) then
      if L.Count = 1 then Rec.Expr := exprPlus
      else raise EScriptError.Create('Incorrect operand in string "' + s + '"');
    // Check for logical chain (0 and ...; 1 or ...)
    if chain = -1 then begin
      if ( (Rec.Expr = exprAnd) and not StrToBool(L[L.Count-1].Value) )
        or ( (Rec.Expr = exprOr) and StrToBool(L[L.Count-1].Value) ) then
        chain := Rec.Expr;
    end
    else if Rec.Expr <> chain then chain := -1;
    // Second token: operator
    if chain = -1 then begin
      Rec.Value := GetToken(s, p, dummy);
      L.Add(Rec);
    end
    else begin
      UnparsedToken(s, p); // skip token
      Rec.Free;
    end;
  end;
  // Calculate result
  numeric := NumericArray(L);
  for j := 0 to MinPriority do begin
    i := 1;
    while i < L.Count do begin
      if Priority[L[i].Expr] = j then
        if numeric then NumEval(L, i-1)
        else StrEval(L, i-1)
      else Inc(i);
    end;
  end;
  Result := L[0].Value;
  L.Free;
end;

function FormOutput(s: string): string;
var i: integer;
begin
  Result := '';
  i := 1;
  while i <= Length(s) do begin
    while (i <= Length(s)) and (s[i] <> '(') do begin
      Result := Result + s[i];
      Inc(i);
    end;
    Result := Result + EvalExpression(ParensContents(s, i));
  end;
end;

procedure RunUnitScript(AUnit: TUnit; Index: integer; var st, en: integer;
  args: string; Errors: TStrings); forward;

procedure RunCommands(Lines, Output: TStrings; AUnit: TUnit; Errors: TStrings);
var s, t, fv, t1, t2: string;
    next_line, i, j, val, rel: integer;
    Cmd: array of integer;
    Ctx: array of TObject;
    ForVars: array of record
      Name: string;
      LastValue: integer;
    end;
    skip: integer;
    StoreVars: TStrings;
begin
  SetLength(Cmd, 0);
  SetLength(Ctx, 0);
  SetLength(ForVars, 0);
  Line := 0;
  skip := -1;

  while Line < Lines.Count do begin
    s := GetScriptLine(Lines, Line, next_line);
    i := 1;
    t := GetIdentifier(s, i);
    // END
    if t = 'end' then begin
      if Length(Cmd) < 0 then
        raise EScriptError.Create('Unmatched END');
      if skip = Cmd[Length(Cmd)-1] then
        // Remove skip
        skip := -1
      else if skip = -1 then begin
        // If WHILE statement was True, repeat it
        if CheckIdentifier(Lines[Cmd[Length(Cmd)-1]], 'while') then
          next_line := Cmd[Length(Cmd)-1]
        // Handle FOR statement
        else if CheckIdentifier(Lines[Cmd[Length(Cmd)-1]], 'for') then begin
          val := ToInt(GetVariable(ForVars[Length(ForVars)-1].Name));
          rel := CompareValue(ForVars[Length(ForVars)-1].LastValue, val);
          if rel = 0 then SetLength(ForVars, Length(ForVars) - 1)
          else begin
            SetVariable(ForVars[Length(ForVars)-1].Name, IntToStr(val + rel));
            next_line := Cmd[Length(Cmd)-1] + 1;
            SetLength(Cmd, Length(Cmd)+1); // this dummy will be removed below
          end;
        end
        // Restore context for WITH statement
        else if CheckIdentifier(Lines[Cmd[Length(Cmd)-1]], 'with') then begin
          Context := Ctx[Length(Ctx)-1];
          SetLength(Ctx, Length(Ctx)-1);
        end;
      end;
      SetLength(Cmd, Length(Cmd)-1);
    end
    // IF
    else if t = 'if' then begin
      SetLength(Cmd, Length(Cmd) + 1);
      Cmd[Length(Cmd)-1] := Line - 1;
      if (skip = -1) and not StrToBool(EvalExpression(Copy(s, i, Length(s)))) then
        skip := Line - 1;
    end
    // ELSE
    else if t = 'else' then begin
      if (Length(Cmd) < 0) or
        not CheckIdentifier(Lines[Cmd[Length(Cmd)-1]], 'if') then
        raise EScriptError.Create('ELSE without IF');
      if skip = Cmd[Length(Cmd)-1] then begin // if skipping for last IF
        skip := -1;
        Cmd[Length(Cmd)-1] := Line - 1;
      end
      else if skip = -1 then begin // if not skipping
        skip := Line - 1;
        Cmd[Length(Cmd)-1] := Line - 1;
      end;
    end
    // FOR
    else if t = 'for' then begin
      SetLength(Cmd, Length(Cmd) + 1);
      Cmd[Length(Cmd)-1] := Line - 1;
      if skip = -1 then begin
        // variable =
        fv := GetIdentifier(s, i);
        if (fv = '') or (GetExpr(s, i) <> exprEq) then
          raise EScriptError.Create('Wrong FOR syntax');
        // start expression
        t1 := '';
        while (i <= Length(s)) and (Copy(s, i, 4) <> ' to ') do
          case s[i] of
            '(': t1 := t1 + StrParens(s, i);
            '''', '"': t1 := t1 + StrQuotes(s, i);
            else begin t1 := t1 + s[i]; Inc(i); end;
          end;
        // to
        if Copy(s, i, 4) = ' to ' then Inc(i, 4)
        else raise EScriptError.Create('Wrong FOR syntax');
        // Set script variable
        SetLength(ForVars, Length(ForVars) + 1);
        ForVars[Length(ForVars) - 1].Name := fv;
        SetVariable(fv, EvalExpression(t1));
        // end expression
        ForVars[Length(ForVars) - 1].LastValue :=
          ToInt(EvalExpression(Copy(s, i, Length(s))));
      end;
    end
    // WHILE
    else if t = 'while' then begin
      SetLength(Cmd, Length(Cmd) + 1);
      Cmd[Length(Cmd)-1] := Line - 1;
      if (skip = -1) and not StrToBool(EvalExpression(Copy(s, i, Length(s)))) then
        skip := Line - 1;
    end
    // WITH
    else if t = 'with' then begin
      SetLength(Cmd, Length(Cmd) + 1);
      Cmd[Length(Cmd)-1] := Line - 1;
      if skip = -1 then begin
        SetLength(Ctx, Length(Ctx) + 1);
        Ctx[Length(Ctx)-1] := Context;
        GetToken(s, i, Context);
      end;
    end
    else if skip = -1 then begin
      // CLEAR
      if t = 'clear' then begin
        while POutTo^ > POutFrom^ do begin
          Output.Delete(POutFrom^);
          Dec(POutTo^);
        end;
      end
      // DO
      else if t = 'do' then begin
        EvalExpression(Copy(s, i, Length(s)));
      end
      // EXEC
      else if t = 'exec' then begin
        if Pos('(', s) = 0 then
          raise EScriptError.Create('Syntax error');
        // Script name
        t1 := '';
        while (i <= Length(s)) and (s[i] <> '(') do begin
          t1 := t1 + s[i];
          Inc(i);
        end;
        t1 := Trim(t1);
        // Arguments
        t2 := ParensContents(s, i);
        // Find script
        j := Scripts.Count-1;
        while (j >= 0) and (ScriptName(Scripts[j]) <> t1) do Dec(j);
        if j >= 0 then begin
          StoreVars := TStringList.Create;
          StoreVars.Assign(Variables);
          RunUnitScript(AUnit, j, POutFrom^, POutTo^, t2, Errors);
          Variables := StoreVars;
        end;
      end
      // Variable assignment
      else if GetExpr(s, i) = exprEq then begin
        SetVariable(t, EvalExpression(Copy(s, i, Length(s))));
      end
      // Write string to output
      else begin
        Output.Insert(POutTo^, FormOutput(TrimLeft(s)));
        Inc(POutTo^);
      end;
    end;

    Line := next_line;

    // Check if processor terminated
    if Assigned(TerminatedQuery) and TerminatedQuery then
      raise EScriptError.Create('Terminated');
  end;
  if Length(Cmd) > 0 then raise EScriptError.Create('END expected for ' +
    'statement in line ' + IntToStr(Cmd[Length(Cmd)-1] + 1));
end;


{ TRecList }

function TRecList.Get(Index: integer): TEvalRec;
begin
  Result := TEvalRec(inherited Get(Index));
end;

procedure TRecList.Put(Index: integer; Item: TEvalRec);
begin
  inherited Put(Index, Item);
end;


function Directive(dir, s: string): boolean;
begin
  Result := Pos(dir, TrimLeft(s)) = 1;
end;

function DirName(s: string): string;
begin
  s := TrimLeft(ScriptUncomment(s));
  Result := Trim(Copy(s, Pos(' ', s) + 1, Length(s)));
end;

procedure LoadScripts;
var Lines, Script, Macros: TStrings;
    i: integer;
    s: string;

  procedure ReadScript(var i: integer);
  var j: integer;
      line: string;
  begin
    while (i < Lines.Count) and not Directive('#script', Lines[i])
      and not Directive('#macro', Lines[i]) do begin
      line := TrimRight(Lines[i]);
      if Directive('#insert', line) then begin
        s := Macros.Values[DirName(line)];
        if s <> '' then begin
          j := StrToInt(s);
          ReadScript(j);
        end;
      end
      else Script.Add(line);
      Inc(i);
    end;
  end;

begin
  Macros := TStringList.Create;
  // Clear scripts
  for i := 0 to Scripts.Count-1 do TStrings(Scripts.Objects[i]).Free;
  Scripts.Clear;

  // Load file
  Lines := TStringList.Create;
  if FileExists(BaseDir + Game.Name + '\scripts.txt') then
    Lines.LoadFromFile(BaseDir + Game.Name + '\scripts.txt');

  // Fill scripts
  i := 0;
  while i < Lines.Count do begin
    if Directive('#macro', Lines[i]) then begin
      Macros.Add(DirName(Lines[i]) + '=' + IntToStr(i+1));
      Inc(i);
      while (i < Lines.Count) and not Directive('#script', Lines[i])
        and not Directive('#macro', Lines[i]) do Inc(i);
    end
    else if Directive('#script', Lines[i]) then begin
      Script := TStringList.Create;
      Scripts.AddObject(DirName(Lines[i]), Script);
      Inc(i);
      ReadScript(i);
      if i < Lines.Count then Dec(i);
    end
    else Inc(i);
  end;
  Lines.Free;
  Macros.Free;
end;

function ScriptCommand(s: string; var Order, Args: string): string;
var Trace: TTrace;
begin
  // @;script [final] Test Script (1, 2)
  Trace := TTrace.Create(TrimLeft(s));
  Result := '';
  if (Trace.Before(' ') = '@;script') and not Trace.Ends then begin
    Order := '';
    if Pos('[', Trace.Text) = 1 then begin
      Inc(Trace.StPos);
      Order := Trace.Before(']');
    end;
    Result := Trim(Trace.Before('('));
    Args := Trace.Before(')');
  end;
  Trace.Free;
end;

function ScriptName(s: string): string;
var Trace: TTrace;
    sq, rnd: integer;
begin
  // [final] Test (1, 2)
  sq := Pos(']', s);
  rnd := Pos('(', s);
  Trace := TTrace.Create(s);
  if (sq > 0) and ((rnd = 0) or (rnd > sq)) then Trace.Before(']');
  Result := Trim(Trace.Before('('));
  Trace.Free;
end;

function ScriptIndex(name: string): integer;
var nr: integer;
begin
  nr := 0;
  while (nr < Scripts.Count) and (ScriptName(Scripts[nr]) <> name) do Inc(nr);
  if nr < Scripts.Count then Result := nr
  else Result := -1;
end;

procedure RunUnitScript(AUnit: TUnit; Index: integer; var st, en: integer;
  args: string; Errors: TStrings);
var msg: string;
begin
  try
    POutFrom := @st;
    POutTo := @en;
    OutUnit := AUnit;
    SplitParams(args, ScriptArgs, True);
    if Variables = nil then Variables := TStringList.Create
    else Variables.Clear;
    try
      RunCommands(TStrings(Scripts.Objects[Index]), AUnit.Orders, AUnit, Errors);
    finally
      FreeAndNil(Variables);
    end;
  except
    on E: Exception do begin
      msg := 'Script error in line ' + IntToStr(Line+1) + ': ' + E.Message;
      AUnit.Orders.Insert(st, ';. ' + msg);
      Errors.AddObject('!S1 ' + AUnit.Name + ' (' + AUnit.NumStr +
        '): ' + msg, AUnit);
    end;
  end;
end;

// Process specified script on unit
procedure RunScript(AUnit: TUnit; Index: integer; Args: string; Errors: TStrings);
var st, en: integer;
begin
  Context := AUnit;
  st := 0;
  en := AUnit.Orders.Count;
  RunUnitScript(AUnit, Index, st, en, Args, Errors);
end;

// Process all scripts from unit orders with run time matching Order
procedure RunScripts(AUnit: TUnit; Order: string; Errors: TStrings);
var i, en, nr: integer;
    name, run_time, args: string;
    Orders: TStrings;
begin
  if not ProgOpened then Exit;
  Context := AUnit;
  Orders := AUnit.Orders;
  i := 0;
  en := AUnit.Orders.Count;
  while i < Orders.Count do begin
    Inc(i);
    if InsideConstruction(AUnit, i-1) then Continue;
    name := ScriptCommand(Orders[i-1], run_time, args);
    if (name <> '') and (run_time = Order) then begin
      nr := ScriptIndex(name);
      if nr >= 0 then begin
        // Find end of script output
        en := i;
        while (en < Orders.Count) and (Pos('@;end ' + name,
          TrimLeft(Orders[en])) <> 1) do Inc(en);
        // Run script
        RunUnitScript(AUnit, nr, i, en, args, Errors);
      end;
    end;
  end;
end;


initialization
  Scripts := TStringList.Create;
  Regex := TRegExpr.Create;

finalization
  Scripts.Free;
  Regex.Free;

end.
