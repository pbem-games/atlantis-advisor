unit uHexMap;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, LCLIntf, LCLType, LMessages, Graphics, DataStructs, Resources, Math, ImgList,
  uKeys, uGameSubs, CylinderMap, uScript, MyStrings;

const
  clMapText = clYellow;
  StructsOnHex = 3;
  ListItemsOnHex = 4;
  DiagramColsOnHex = 5;
  DiagramLength = 12;
  hs50 = 24;
  hs43 = 21;

  fogNone = 0;
  fogNonVisited = 1;
  fogNonVisible = 2;
  fogCustom = 3;

  sicNone = 0;
  sicSettlement = 1;
  sicWeather = 2;
  sicPeasants = 3;
  sicGuard = 4;
  sicTaxTrade = 5;

  stxNone = 0;
  stxSettlName = 1;
  stxPeasants = 2;
  stxPeasantsWCount = 3;
  stxTaxRate = 4;
  stxWages = 5;
  stxEntertain = 6;

  mliNone = 0;
  mliWanted = 1;
  mliForSale = 2;
  mliProducts = 3;

  MIC_MONSTER = $02;
  MIC_GATE = $04;
  MIC_BATTLE = $08;
  MIC_WARNING = $10;
  MIC_NOTES = $20;

  FLG_SELF = $02;
  FLG_OTHER = $04;
  FLG_NOSCOUTS = $08;
  FLG_OLD = $10;
  FLG_UNIT_DIAGRAM = $20;
  FLG_MEN_DIAGRAM = $40;
  FLG_NOSELFSCOUTS = $80;

  procedure CalcHexMapCoords(mapX, mapY: integer; var HX, HY: integer);
  procedure CalcMapCoords(HX, HY: integer; var mapX, mapY: integer);
  procedure DrawHex(ACanvas: TCanvas; CX, CY: integer; Region: TARegion);
  procedure DrawExtra(ACanvas: TCanvas; CX, CY: integer; Region: TARegion;
    Simple: boolean);
  procedure HexMapSetup(HexMap: TCylinderMap);
  procedure DrawArrow(ACanvas: TCanvas; cx, cy, Dir, bmpArrows: integer);
  procedure DrawMoveState(Region: TARegion; ACanvas: TCanvas; mapX, mapY,
    cx, cy: integer);
  procedure DrawPathArrows(ACanvas: TCanvas; mapX, mapY, cx, cy: integer);
  function SelectMoveModeHex(C: TCoords): boolean;


implementation

uses Main, uVisualOrders;


function SelectMoveModeHex(C: TCoords): boolean;
var i, dir, mt: integer;
    LastPos: TCoords;
    Moves: TCoordArray;
    swim: boolean;
begin
  Result := False;

  if MainForm.RouteMode.Enabled then begin
    Moves := MainForm.RouteMode.Route.Moves;
    mt := mtFly;
    swim := True;
  end
  else begin
    Moves := CurrUnit.Moves;
    mt := ArmyMovementType(CurrUnit);
    swim := ArmyCanSwim(CurrUnit);

    // If clicked on visited region, end move mode
    i := 0;
    while (i < Length(Moves)) and not EqualCoords(Moves[i], C) do Inc(i);
    if (i < Length(Moves)) and not MainForm.RouteMode.Enabled then begin
      MainForm.EndMoveMode;
      Exit;
    end;
  end;

  // Find direction of move from last reg
  LastPos := Moves[High(Moves)];
  dir := DirectionFromTo(LastPos, C);

  // Exit if we are on same level and can't move there
  if (LastPos.z = C.Z) and ((Dir = 0)
    or not MovableDir(LastPos, Dir, (MoveOrder = 'sail'),
    mt, swim, CurrUnit.Struct)) then Exit;

  if MainForm.RouteMode.Enabled then begin
    AddCoords(MainForm.RouteMode.Route.Moves, C);
    if CtrlPressed then AddInt(MainForm.RouteMode.Route.Stops, High(Moves) + 1);
    CurrUnit.Route := MainForm.RouteMode.Route.Moves;
  end
  else begin
    // Get out of structure
    if (Length(Moves) = 1) and (LastPos.z = C.Z) and (MoveOrder <> 'sail') then
      GetArmyOut;
    // Decrement movement points
    MovePoints := MovePoints - EnterCost(ArmyMovementType(CurrUnit),
      C, LastPos, (MoveOrder = 'sail'));
    if (MovePoints < 0) and (CurrUnit.FinalPoint = -1) then
      CurrUnit.FinalPoint := High(CurrUnit.Moves);
    MovePoints := Max(0, MovePoints);
    // Add visited region
    AddCoords(CurrUnit.Moves, C);
    if Dir <> 0 then begin
      AddOrder(MoveOrder + ' ' + GetDir(dir), True);
      CurrUnit.MonthOrder := CurrUnit.Orders[CurrUnit.Orders.Count-1];
    end;
    // Recalc available regions
    SetLength(Reached, 0);
    DoCalcReachedRegions(C, CurrUnit);
  end;

  MainForm.HexMap.Redraw;
  Result := True;
end;

procedure DrawPathArrows(ACanvas: TCanvas; mapX, mapY, cx, cy: integer);

  function CoordsInPath(C: TCoords; Path: TCoordArray): boolean;
  var i: integer;
  begin
    i := High(Path);
    while (i >= 0) and not EqualCoords(Path[i], C) do Dec(i);
    Result := (i >= 0);
  end;

  procedure DrawPath(Moves: TCoordArray; Arrows: integer; Check: boolean;
    Final: integer);
  var i, j, dir: integer;
  begin
    for i := 0 to Length(Moves)-2 do
      if EqualCoords(Moves[i], Coords(mapX, mapY, Map.Level)) then begin
        if Check then begin
          j := High(CurrUnit.Moves);
          while (j >= 0) and not EqualCoords(CurrUnit.Moves[j], Moves[i]) do
            Dec(j);
          if j >= 0 then begin
            if (j > 0) and EqualCoords(CurrUnit.Moves[j-1], Moves[i+1]) then
              Continue;
            if (j < High(CurrUnit.Moves)) and EqualCoords(CurrUnit.Moves[j+1], Moves[i+1]) then
              Continue;
          end;
        end;
        dir := DirectionFromTo(Moves[i], Moves[i+1]);
        if (Final = -1) or (i < Final) then
          DrawArrow(ACanvas, cx, cy, dir, Arrows)
        else DrawArrow(ACanvas, cx, cy, dir, bmp_arrTranspArrows);
      end;
  end;

begin
  if CurrUnit = nil then Exit;

  DrawPath(CurrUnit.Route, bmp_arrTranspArrows, True, 9999);

  if (MoveOrder = 'sail') or (ClearOrder(CurrUnit.MonthOrder) = 'sail') then
    DrawPath(CurrUnit.Moves, bmp_arrCyanArrows, False, CurrUnit.FinalPoint)
  else DrawPath(CurrUnit.Moves, bmp_arrGreenArrows, False, CurrUnit.FinalPoint);
end;

procedure DrawMoveState(Region: TARegion; ACanvas: TCanvas; mapX, mapY,
  cx, cy: integer);
var i, arr: integer;
    Bmp: TBitmap;
    HexParts: THexParts;
    mv: string;
    Moves: TCoordArray;

  function PrevDir(Dir: integer): boolean;
  var i, last: integer;
      Coords: TPoint;
  begin
    last := Length(Moves) - 1;
    Coords := PointInDir(Point(Moves[last].X, Moves[last].Y), Dir);
    i := 0;
    while (i < Length(Moves)) and not ((Coords.x = Moves[i].x) and
      (Coords.Y = Moves[i].Y) and (Map.Level = Moves[i].Z)) do Inc(i);
    Result := (i < Length(Moves));
  end;

  function Reachable(C: TCoords): boolean;
  var i: integer;
  begin
    i := 0;
    while (i < Length(Reached)) and not EqualCoords(Reached[i].Coords, C) do Inc(i);
    Result := (i < Length(Reached));
  end;

begin
  if MainForm.RouteMode.Enabled then Moves := MainForm.RouteMode.Route.Moves
  else Moves := CurrUnit.Moves;

  // Draw selection (movement range)
  ACanvas.Pen.Color := clRed;
  if Reachable(Coords(mapX, mapY, Map.Level)) then begin
    for i := 1 to 6 do
      HexParts[i] := not Reachable(CoordsInDir(Coords(mapX, mapY, Map.Level), i));
    MainForm.HexMap.PartialHex(ACanvas, cx, cy, HexParts);
  end;

  // Last hex in path (available exits)
  if (mapX = Moves[High(Moves)].X) and (mapY = Moves[High(Moves)].Y) then begin
   // Draw exit arrows
    if (Region = nil) or not Region.FullData then arr := bmp_arrTranspArrows
    else arr := bmp_arrWhiteArrows;

    if MainForm.RouteMode.Enabled then begin
      for i := 1 to 6 do
        if not PrevDir(i) and MovableDir(Coords(mapX, mapY, Map.Level), i,
          (MoveOrder = 'sail'), mtFly, True, CurrUnit.Struct) then
          DrawArrow(ACanvas, cx, cy, i, arr);
    end
    else begin
      for i := 1 to 6 do
        if not PrevDir(i) and MovableDir(Coords(mapX, mapY, Map.Level), i,
          (MoveOrder = 'sail'), ArmyMovementType(CurrUnit),
          ArmyCanSwim(CurrUnit), CurrUnit.Struct) then
          DrawArrow(ACanvas, cx, cy, i, arr);
    end;

    // Moves remaining
    mv := IntToStr(MovePoints);
    Bmp := TBitmap.Create;
    with Bmp.Canvas do begin
      Bmp.Width := TextWidth(mv);
      Bmp.Height := TextHeight(mv);
      Bmp.Transparent := TRUE;
      Font.Color := clMapText;
      TextOut(0, 0, mv);
    end;
    ACanvas.Draw(cx - Bmp.Width div 2, cy - Bmp.Height div 2, Bmp);
    Bmp.Free;
  end;
end;


procedure DrawArrow(ACanvas: TCanvas; cx, cy, Dir, bmpArrows: integer);
var x, y: integer;
begin
  case Dir of
    dirN:
      begin x := cx - 8; y := cy - 29; end;
    dirNE:
      begin x := cx + 12; y := cy - 18; end;
    dirSE:
      begin x := cx + 12; y := cy + 3; end;
    dirS:
      begin x := cx - 8; y := cy + 14; end;
    dirSW:
      begin x := cx - 27; y := cy + 3; end;
    dirNW:
      begin x := cx - 27; y := cy - 18; end;
    else
      begin x := cx - 8; y := cy - 8; end;
  end;
  ResForm.Arrows.Draw(ACanvas, x, y, bmpArrows + Dir);
end;


procedure CalcHexMapCoords(mapX, mapY: integer; var HX, HY: integer);
begin
  HX := Max(mapX - MapBounds[Map.Level].Left, 0);
  HY := Max(mapY - MapBounds[Map.Level].Top, 0);
end;

procedure CalcMapCoords(HX, HY: integer; var mapX, mapY: integer);
begin
  mapX := HX + MapBounds[Map.Level].Left;
  mapY := HY + MapBounds[Map.Level].Top;
end;

function StructFaction(R: TARegion; AStruct: TStruct): TFaction;
begin
  if (not Test(AStruct.Data.Flags, ST_SHAFT) or (R.Visited = Turn.Num)) and (AStruct.Owner <> nil) then
    Result := AStruct.Owner.Faction
  else Result := VTurn.Factions[0];
end;

procedure DrawSettlement(ARegion: TARegion; ACanvas: TCanvas; cx, cy: integer);
begin
  if ARegion.SettlementType > 0 then
    ResForm.Extras.Draw(ACanvas, cx-hs50 + 3, cy-5,
    bmp_extVillage + ARegion.SettlementType-1);
end;

procedure DrawSimpleExtras(Region: TARegion; ACanvas: TCanvas; cx, cy: integer);
var i, x, xtra: integer;
 astr: TStruct;
begin
  x := cx + hs50 div 2 - 7;
  for i := 0 to Region.Structs.Count-1 do
    if Test(Region.Structs[i].Data.Flags, ST_SHAFT) then begin
      astr := Region.Structs[i];
      xtra := StructExtra(astr);
      DrawCExtra(xtra, StructFaction(Region, astr), ACanvas, x, cy + hs43 - 12);
      x := x - 10;
    end;
  DrawSettlement(Region, ACanvas, cx, cy);
end;

procedure DrawHexExtra(Region, RealR: TARegion; ACanvas: TCanvas; cx, cy: integer);
var fright: integer;
    Flag, Mic, SExtras: DWord;

  function ShowTroop(Troop: TTroop): boolean;
  begin
    Result := True;
    if Troop.Faction.Player and (not Test(Flag, FLG_SELF)
      or (ScoutTroop(Troop) and Test(Flag, FLG_NOSELFSCOUTS))) then
      Result := False
    else if not Troop.Faction.Player and (not Test(Flag, FLG_OTHER)
      or (ScoutTroop(Troop) and Test(Flag, FLG_NOSCOUTS))) then
      Result := False
    else if Test(Mic, MIC_MONSTER) and MonsterTroop(Troop) then
      Result := False;
  end;

  procedure DrawSettlementIcon;
  var Weather: TWeatherData;
      idx: string;
  begin
    if Config.ReadBool('MainWin', 'HideInvisRegionSIC', False)
      and (not Region.FullData or (Region.Visited <> VTurn.Num))
      and (Config.ReadInteger('Map', 'SettlIcon', sicWeather) <> sicSettlement) then
      Exit;
    case Config.ReadInteger('Map', 'SettlIcon', sicWeather) of
      sicSettlement:
        DrawSettlement(Region, ACanvas, cx, cy);
      sicWeather:
        if (Region.Visited = Turn.Num) and (Region.WeatherLast <> nil)
          and (Region.WeatherNext <> nil) then begin
          ResForm.Extras.Draw(ACanvas, cx-hs50 + 4, cy-7,
            bmp_extWeather + 1 + Game.WeatherData.IndexOf(Region.WeatherLast));
          ResForm.Extras.Draw(ACanvas, cx-hs50 + 2, cy-3,
            bmp_extWeather + 1 + Game.WeatherData.IndexOf(Region.WeatherNext));
        end
        else begin
          Weather := GetWeather(Region.Coords);
          if Weather <> nil then
            ResForm.Extras.Draw(ACanvas, cx-hs50 + 3, cy-5,
              bmp_extWeather + 1 + Game.WeatherData.IndexOf(Weather));
        end;
      sicPeasants:
        if Region.Peasants <> nil then begin
          idx := PeasantExtras.Values[Region.Peasants.Data.Short];
          if idx <> '' then
            ResForm.Extras.Draw(ACanvas, cx-hs50 + 3, cy-5, StrToInt(idx));
        end;
      sicGuard:
        if Region.Guard <> nil then
          DrawCExtra(extShield, Region.Guard, ACanvas, cx-hs50 + 3, cy-5);
      sicTaxTrade:
        if Test(Region.Marks, RM_TAX) and Test(Region.Marks, RM_TRADE) then begin
          ResForm.Extras.Draw(ACanvas, cx-hs50 + 5, cy-7, bmp_extTax);
          ResForm.Extras.Draw(ACanvas, cx-hs50 + 1, cy-2, bmp_extTrade);
        end
        else if Test(Region.Marks, RM_TAX) then
          ResForm.Extras.Draw(ACanvas, cx-hs50 + 3, cy-5, bmp_extTax)
        else if Test(Region.Marks, RM_TRADE) then
          ResForm.Extras.Draw(ACanvas, cx-hs50 + 3, cy-5, bmp_extTrade);
    end;
  end;

  procedure DrawSettlementText;
  var s: string;
  begin
    case Config.ReadInteger('Map', 'SettlText', stxSettlName) of
      stxSettlName:
        s := Region.Settlement;
      stxPeasants:
        if Region.Peasants <> nil then s := Region.Peasants.Data.MultiName;
      stxPeasantsWCount:
        if Region.Peasants <> nil then
          s := IntToStr(Region.Peasants.Amount)+ ' ' + Region.Peasants.Data.MultiName;
      stxTaxRate:
        if RealR.TaxRate > 0 then
          s := IntToStr(RealR.TaxRate);
      stxWages:
        if RealR.Wages > 0 then
          s := IntToStr(RealR.Wages);
      stxEntertain:
        if RealR.Entertainment > 0 then s := IntToStr(RealR.Entertainment);
    end;
    ACanvas.Font.Color := clMapText;
    if (s <> '') then
      DrawTranspText(ACanvas, cx-hs50 + 15, cy-7, s, 200, False);
  end;

  procedure DrawMonsterIcons;
  begin
    if ((Mic and MIC_MONSTER) <> 0) and HasMonsters(Region) and
      (((Flag and FLG_OLD) <> 0) or (Region.Visited = Turn.Num)) then begin
      fright := fright - 7;
      ResForm.Extras.Draw(ACanvas, fright, cy - hs43 + 2, bmp_extMonster);
    end;
    if ((Mic and MIC_GATE) <> 0) and (Region.Gate <> 0) then begin
      fright := fright - 7;
      ResForm.Extras.Draw(ACanvas, fright, cy - hs43 + 2, bmp_extGate);
    end;
    if ((Mic and MIC_BATTLE) <> 0) and (Region.Visited = Turn.Num)
      and (Region.Battles <> nil) then begin
      fright := fright - 7;
      ResForm.Extras.Draw(ACanvas, fright, cy - hs43 + 2, bmp_extBattle);
    end;
    if ((Mic and MIC_WARNING) <> 0) and (Trim(GameConfig.ReadString('Map',
          'Warning', '')) <> '') then begin
      Context := Region;
      try
        if EvalExpression('bool(' + GameConfig.ReadString('Map',
          'Warning', '') + ')') = '1' then begin
          fright := fright - 7;
          ResForm.Extras.Draw(ACanvas, fright, cy - hs43 + 2, bmp_extWarning);
        end;
      except
      end;
    end;
    if Test(Mic, MIC_NOTES) and (Region.Notes.Text <> '') then begin
      fright := fright - 7;
      ResForm.Extras.Draw(ACanvas, fright, cy - hs43 + 2, bmp_extNotes);
    end;
  end;

  procedure DrawFactionDiagram;
  var y, i, j, len, c: integer;
      DCols: array of integer;
      DFactions: array of TFaction;
  begin
    // Initialize col lengths
    SetLength(DCols, Region.Troops.Count);
    SetLength(DFactions, Region.Troops.Count);
    for i := 0 to High(DCols) do DCols[i] := 0;

    // Make array of col lengths
    i := 0;
    c := 0;
    while i < Region.Troops.Count do begin
      if ShowTroop(Region.Troops[i]) then begin
        // Calc line length
        if Flag and FLG_UNIT_DIAGRAM <> 0 then
          DCols[c] := Min(Region.Troops[i].Units.Count, DiagramLength) * 2
        else DCols[c] := Min(CountMen(Region.Troops[i]), DiagramLength);
        DCols[c] := Min(DCols[c], fright - (cx - hs50 div 2));
        DFactions[c] := Region.Troops[i].Faction;
        Inc(c);
      end;
      Inc(i);
    end;

    // Draw diagram
    y := 0;
    for i := 0 to Min(High(DCols), DiagramColsOnHex) do begin
      len := 0;
      c := 0;
      // Find max length and drop it from array
      for j := 0 to Min(High(DCols), DiagramColsOnHex) do
        if DCols[j] > len then begin
          len := DCols[j];
          c := j;
        end;
      DCols[c] := 0;
      // Draw line
      if len > 0 then begin
        ACanvas.Pen.Color := TColorExtra(ColorExtras[FactionCIndex(DFactions[c])]).Color;
        ACanvas.MoveTo(cx - hs50 div 2, cy - hs43 + 3 + y*2);
        ACanvas.LineTo(cx - hs50 div 2 + len, cy - hs43 + 3 + y*2);
        // Draw shadow
        ACanvas.Pen.Color := clBlack;
        ACanvas.MoveTo(cx - hs50 div 2, cy - hs43 + 3 + y*2+1);
        ACanvas.LineTo(cx - hs50 div 2 + len, cy - hs43 + 3 + y*2+1);
        Inc(y);
      end;
    end;
  end;

  procedure DrawFlags;
  var i, x, y, f: integer;
  begin
    x := cx - hs50 div 2 - 4;
    f := 0;
    i := 0;
    // Self troop comes first
    if (Region.PlayerTroop <> nil) and ShowTroop(Region.PlayerTroop) then begin
      DrawCExtra(extFlag, VFaction, ACanvas, x, cy - hs43 + 5);
      x := x + 4;
      Inc(f);
      Inc(i);
    end;
    // Other flags
    if Test(Flag, FLG_OTHER) then begin
      while (i < Region.Troops.Count) and (x < fright - 6) do begin
        if ShowTroop(Region.Troops[i]) then begin
          y := cy - hs43 + 2;
          if not Odd(f) then y := y + 3;
          DrawCExtra(extFlag, Region.Troops[i].Faction, ACanvas, x, y);
          x := x + 4;
          Inc(f);
        end;
        Inc(i);
      end;
    end;
  end;

  procedure DrawStructs;
  var i, j, x: integer;
      StrList: TStructList;
  begin
    StrList := TStructList.Create;
   // Form list of structs
    with Region do begin
      for i := 0 to Structs.Count-1 do begin
        if (((Visited = Turn.Num) or ((Structs[i].Data.Flags and ST_TRANSPORT) = 0)) and (Structs[i].Data.Flags and ST_ROAD = 0)) then begin
          if ((Structs[i].Data.Flags = 0) and Test(SExtras, ST_UNKNOWN)) or Test(Structs[i].Data.Flags, SExtras) then begin
            StrList.Add(Structs[i]);
          end;
        end;

      end;
    end;
   // Throw out repeating objects of same faction
    i := 0;
    while (i < StrList.Count) and (StrList.Count > StructsOnHex) do begin
      j := i + 1;
      while j < StrList.Count do
        if (StructFaction(Region, StrList[i]) <> StructFaction(Region, StrList[j])) or
          (StructExtra(StrList[i]) <> StructExtra(StrList[j])) then Inc(j)
        else StrList.Delete(j);
      Inc(i);
    end;
   // Draw icons
    x := cx + hs50 div 2 - 7;
    i := 0;
    while (i < StrList.Count) and (i < StructsOnHex) do begin
      DrawCExtra(StructExtra(StrList[i]), StructFaction(Region, StrList[i]), ACanvas,
        x, cy + hs43 - 12);
      x := x - 10;
      Inc(i);
    end;
    StrList.Free;
  end;

begin
  if Config.ReadBool('Map', 'FlagsEnabled', True) then
    Flag := Config.ReadInteger('Map', 'Flags', FLG_SELF + FLG_OTHER)
  else
    Flag := 0;

  if Config.ReadBool('Map', 'MIcEnabled', True) then
    Mic := Config.ReadInteger('Map', 'MIc', MIC_MONSTER + MIC_GATE + MIC_BATTLE + MIC_WARNING + MIC_NOTES)
  else
    Mic := 0;

  if Config.ReadBool('Map', 'StructsEnabled', True) then
    SExtras := Config.ReadInteger('Map', 'Structs', ST_DEFENCE + ST_TRANSPORT + ST_CLOSED + ST_SHAFT + ST_ROAD + ST_UNKNOWN)
  else
    SExtras := 0;

  if Config.ReadBool('Map', 'SettlIconEnabled', True) then
    DrawSettlementIcon;

  if Config.ReadBool('Map', 'SettlTextEnabled', True) then
    DrawSettlementText;

  fright := cx + hs50 div 2 - 1;
  if Mic <> 0 then DrawMonsterIcons;
  if (Flag <> 0) and ((Region.Visited = Turn.Num) or Test(Flag, FLG_OLD)) then begin
    if (Flag and FLG_UNIT_DIAGRAM <> 0) or (Flag and FLG_MEN_DIAGRAM <> 0) then
      DrawFactionDiagram
    else
      DrawFlags;
  end;

  if SExtras <> 0 then
    DrawStructs;
end;

procedure DrawHexList(Region: TARegion; List: TItemList;
  ACanvas: TCanvas; cx, cy: integer);
var i, itms, y: integer;
    Bmp: TBitmap;
    Filter: DWord;
begin
  DrawSettlement(Region, ACanvas, cx, cy);
  Filter := Config.ReadInteger('Map', 'ListFilter', IT_ALL);
  Bmp := TBitmap.Create;
  Bmp.Width := hs50 + 4;
  Bmp.Height := hs43*2;
  with Bmp.Canvas do begin
    Font.Color := clMapText;
    Font.Name := 'Small Fonts';
    Font.Size := 6;
    y := 0;
    itms := 0;
    for i:=0 to List.Count-1 do
      if Test(List[i].Data.Flags + IT_UNKNOWN, Filter)
        and (itms < ListItemsOnHex) then begin
        if List[i].Amount = 0 then Font.Color := clMoneyGreen
        else Font.Color := clYellow;
        TextOut(0, y*10+1, List[i].Data.Short);
        Inc(y);
        Inc(itms);
      end;
  end;
  Bmp.Transparent := TRUE;
  ACanvas.Draw(cx - hs50 div 2, cy - hs43, Bmp);
  Bmp.Free;
end;

procedure DrawExtra(ACanvas: TCanvas; CX, CY: integer; Region: TARegion;
  Simple: boolean);
var RealR: TARegion;
begin
  RealR := Map.Region(Region.Coords, Turn.Num);
  if Simple then DrawSimpleExtras(Region, ACanvas, cx, cy)
  else begin
    if Config.ReadBool('Map', 'ListEnabled', False) then
      case Config.ReadInteger('Map', 'List', mliNone) of
        mliWanted:
          DrawHexList(RealR, RealR.Wanted, ACanvas, CX, CY);
        mliForSale:
          DrawHexList(RealR, RealR.ForSale, ACanvas, CX, CY);
        mliProducts:
          DrawHexList(RealR, RealR.Products, ACanvas, CX, CY);
      end
    else DrawHexExtra(Region, RealR, ACanvas, cx, cy);
  end;
end;

procedure DrawHex(ACanvas: TCanvas; CX, CY: integer; Region: TARegion);
var idx, i, terr: integer;
    Flags: DWord;
begin
  // Fog region
  ResForm.Terrains.DrawingStyle := ImgList.dsNormal;
  if Config.ReadBool('Map', 'FogEnabled', False) then begin
    case Config.ReadInteger('Map', 'FogType', fogNone) of
      fogNonVisited:
        if not Region.FullData then
          ResForm.Terrains.DrawingStyle := ImgList.dsSelected;
      fogNonVisible:
        if not Region.FullData or (Region.Visited < Turn.Num) then
          ResForm.Terrains.DrawingStyle := ImgList.dsSelected;
      fogCustom:
        if not Region.FullData or (Region.Visited < Turn.Num -
          Config.ReadInteger('MainWin', 'CustomFogMonths', 12) + 1) then
          ResForm.Terrains.DrawingStyle := ImgList.dsSelected;
    end;
  end;

  // Draw terrain
  idx := Game.WeatherData.IndexOf(GetWeather(Region.Coords));
  if Region.Terrain <> nil then begin
    if GameConfig.ReadBool('Map', 'WinterRegions', False)
      and (idx >= 0) and (idx < weatherCount) then
      terr := Region.Terrain.BmpIndexes[idx]
    else terr := Region.Terrain.BmpIndexes[weatherClear];
  end
  else terr := 0;
  ResForm.Terrains.Draw(ACanvas, CX - hs50, CY - hs43, terr);

 // Draw rock walls
  if Region.FullData and (Map.Levels[Region.z].Name <> Keys[s_Surface]) then begin
    for i := 1 to 6 do
      if not Region.HasExit[i] then
         ResForm.Terrains.Draw(ACanvas, CX - hs50, CY - hs43, bmp_terrRock + i - 1);
  end;

 // Draw roads
  Flags := Config.ReadInteger('Map', 'Structs', ST_ROAD);
  if Config.ReadBool('Map', 'StructsEnabled', True) then begin
    if Test(Flags, ST_ROAD) then
      for i := 0 to Region.Structs.Count-1 do
        if Test(Region.Structs[i].Data.Flags, ST_ROAD)
          and (Region.Structs[i].Needs = 0) then begin
          idx := StructDirection(Region.Structs[i].Data.Group);
          if idx in [1..6] then ResForm.Terrains.Draw(ACanvas, CX - hs50,
            CY - hs43, bmp_terrRoad + idx - 1);
        end;
  end;
end;

procedure HexMapSetup;
var hx, hy: integer;
begin
  HexMap.MapState := HexMap.MapState + [msNoPaint];
  Map.Level := GameConfig.ReadInteger('Map', 'Level', 0);
  while (Map.Level < 0) or (Map.Level >= Map.Levels.Count)
    or (Map.Levels[Map.Level].Empty) do
      if Map.Level < Map.Levels.Count then Inc(Map.Level)
      else Map.Level := 0;
  HexMap.FirstOdd := Odd(MapBounds[Map.Level].Left) xor Odd(MapBounds[Map.Level].Top);
  HexMap.ColCount := MapBounds[Map.Level].Right - MapBounds[Map.Level].Left + 1;
  HexMap.RowCount := MapBounds[Map.Level].Bottom - MapBounds[Map.Level].Top + 1;
  CalcHexMapCoords(GameConfig.ReadInteger('Map', 'SelX_' + Map.Levels[Map.Level].Name, 0),
    GameConfig.ReadInteger('Map', 'SelY_' + Map.Levels[Map.Level].Name, 0), hx, hy);
  HexMap.Selected := Point(hx, hy);
  HexMap.Center(HexMap.Selected.X, HexMap.Selected.Y);
  HexMap.MapState := HexMap.MapState - [msNoPaint];
  HexMap.Redraw;
end;

end.
