// Bridges Advisor objects into the LUA
unit LuaScript;

interface

uses
    DataStructs, lua, LuaObject;

type
    TLuaUnit = class(TLuaObject)
    private
        AUnit : TUnit;
    public
        constructor Create(AUnit: TUnit; LuaState : PLua_State);
        function  GetPropValue(propName : String): Variant; override;
        function  SetPropValue(PropName : String; const AValue: Variant) : Boolean; override;
    end;

implementation

uses
    SysUtils;

constructor TLuaUnit.Create(AUnit: TUnit; LuaState: PLua_State);
begin
    inherited Create(LuaState);
    Self.AUnit := AUnit;

    Self.ClassInfo
end;

function TLuaUnit.GetPropValue(propName: String): Variant;
begin
    if      CompareText(propName, 'Name')        = 0 then Result := AUnit.Name
    else if CompareText(propName, 'Num')         = 0 then Result := AUnit.NumStr
    else if CompareText(propName, 'Description') = 0 then Result := AUnit.Description
    else if CompareText(propName, 'OnGuard')     = 0 then Result := AUnit.Flags[flgGuard]
    else if CompareText(propName, 'AutoTax')     = 0 then Result := AUnit.Flags[flgTax]
    else if CompareText(propName, 'Avoiding')    = 0 then Result := AUnit.Flags[flgAvoid]
    else if CompareText(propName, 'Behind')      = 0 then Result := AUnit.Flags[flgBehind]
    else if CompareText(propName, 'Holding')     = 0 then Result := AUnit.Flags[flgHold]
    else if CompareText(propName, 'Noaid')       = 0 then Result := AUnit.Flags[flgNoaid]
    else if CompareText(propName, 'Nocross')     = 0 then Result := AUnit.Flags[flgNocross]
    else if CompareText(propName, 'Consume')     = 0 then Result := ExtFlags[0, AUnit.Consuming]
    else if CompareText(propName, 'Reveal')      = 0 then Result := ExtFlags[1, AUnit.Revealing]
    else if CompareText(propName, 'Spoils')      = 0 then Result := ExtFlags[2, AUnit.Spoils]
    else if CompareText(propName, 'TradeIncome') = 0 then Result := AUnit.TradeIncome
    else if CompareText(propName, 'WorkIncome')  = 0 then Result := AUnit.WorkIncome
    else if CompareText(propName, 'Region')      = 0 then begin
      if Length(args) = 0 then begin
        Result := CoordsToStr(AUnit.Region.Coords);
        Context := AUnit.Region;
      end
      else found := False; // region(...) - context function
    end
    else if id = 'faction' then begin
      if Length(args) = 0 then begin
        Result := IntToStr(AUnit.Faction.Num);
        Context := AUnit.Faction;
      end
      else found := False; // faction(...) - context function
    end
    else if id = 'object' then begin
      if AUnit.Struct <> nil then Result := IntToStr(AUnit.Struct.Num);
      Context := AUnit.Struct;
    end
    else if id = 'combatspell' then begin
      if AUnit.CombatSpell <> nil then Result := AUnit.CombatSpell.Short
    end
    else if id = 'former' then begin
      if AUnit.Former <> nil then Result := AUnit.Former.NumStr;
      Context := AUnit.Former;
    end
    else if id = 'ismage' then Result := BoolToStr(AUnit.Mage)
    else if id = 'localdescription' then
      Result := UnitRecs.Local(AUnit.Num, AUnit.Region.Coords)
    else if ArgToken(id, 'setlocaldescription') then
      UnitRecs.AddUnitRec(U, args[0])
    // lists
    else if id = 'items' then
      Result := GetListValue(AUnit.Items, ItemFinder, ItemValue)
    else if id = 'skills' then
      Result := GetListValue(AUnit.Skills, SkillFinder, SkillValue)
    else if id = 'canstudy' then
      Result := GetListValue(AUnit.CanStudy, SkillDataFinder, SkillDataValue)
    else if id = 'canlearn' then
      Result := CanLearn(U, args[0])
    else if ArgToken(id, 'amountof') then begin
      idx := ItemFlag(args[0]);
      if idx > 0 then Result := IntToStr(AUnit.Items.Amount(idx))
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
    else if id = 'allorders' then Result := AUnit.Orders.Text
    else if id = 'monthorder' then Result := AUnit.MonthOrder
    else if NumArgToken(id, 'delorder', 0, AUnit.Orders.Count-1) then begin
      idx := ToInt(args[0]);
      AUnit.Orders.Delete(idx);
      if U = OutUnit then begin
        if idx < POutFrom^ then Dec(POutFrom^);
        if idx < POutTo^ then Dec(POutTo^);
      end;
    end
    else if ArgsToken(id, 'insorder', 2) and NumArg(0, 0, MaxInt) then begin
      idx := Min(ToInt(args[0]), AUnit.Orders.Count);
      AUnit.Orders.Insert(idx, args[1]);
      if U = OutUnit then begin
        if idx < POutFrom^ then Inc(POutFrom^);
        if idx < POutTo^ then Inc(POutTo^);
      end;
    end
    else if id = 'orders' then begin
      if Length(args) = 0 then Result := IntToStr(AUnit.Orders.Count)
      else if NumArg(0, 0, AUnit.Orders.Count-1) then
        Result := AUnit.Orders[ToInt(args[0])];
    end
    else if id = 'removescript' then begin
      idx := POutFrom^ - 1;
      if (idx >= 0) and (idx < AUnit.Orders.Count) then begin
        AUnit.Orders.Delete(idx);
        Dec(POutFrom^);
        Dec(POutTo^);
      end;
      idx := POutTo^;
      if (idx >= 0) and (idx < AUnit.Orders.Count) then
        AUnit.Orders.Delete(idx);
    end
    else if id = 'events' then begin
      if Length(args) = 0 then Result := IntToStr(AUnit.Events.Count)
      else if NumArg(0, 0, AUnit.Events.Count-1) then
        Result := AUnit.Events[ToInt(args[0])];
    end
    // Contexts
    else if id = 'troop' then begin
      Context := AUnit.Region.Troops.Find(AUnit.Faction.Num);
      Result := IntToStr(AUnit.Faction.Num);
    end
    else found := False;
  end
end;

function TLuaUnit.SetPropValue(PropName: String; const AValue: Variant): Boolean;
begin
  
end;

initialization

finalization

end.