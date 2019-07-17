unit uGameStart;

{$MODE Delphi}

// Performs actions on first game startup (when no orders loaded)

interface

uses
  SysUtils, Windows, DataStructs, MyStrings, uNeeds, uRoute, Resources;

  procedure PreProcess;
  procedure PostProcess(RerunRegions: TRegionList);

implementation

var Modified: boolean;

function DoRedirOrder(s: string; Units: TUnitList): string;
var t, s1: string;
    i, j, num: integer;
begin
  Result := s;
  s1 := '';
  while s <> '' do begin
    if Copy(TrimLeft(s), 1, 1) = '"' then begin
      s1 := s1 + '"' + GetToken(s) + '" ';
      Continue;
    end;
    t := LowerCase(GetToken(s));
    s1 := s1 + t + ' ';
    if t <> 'new' then Continue;
    num := ToInt(GetToken(s));
    if num = 0 then Continue;

    // Look for existing new unit with the number
    if Units.Find(-num) <> nil then Continue;

    // Find "@;new 1" token in unit list
    i := Units.Count-1;
    while i >= 0 do begin
      j := Units[i].Orders.Count-1;
      while (j >= 0) and (Trim(Units[i].Orders[j]) <> '@;new ' + IntToStr(num)) do
        Dec(j);
      if j >= 0 then Break;
      Dec(i);
    end;
    if i < 0 then Continue;

    // Replace number entry with found number
    s1 := Copy(s1, 1, Length(s1) - 4) + Units[i].NumStr + ' ';
    Result := s1 + s;
    Modified := True;
  end;
end;

// Redirect orders to "new 1" units to new unit numbers, if "@;new 1" found
function RedirNewOrders(R: TRegion): boolean;
var j, k: integer;
    Troop: TTroop;
    U: TUnit;
begin
  Result := False;
  Modified := False;
  Troop := R.PlayerTroop;
  if Troop = nil then Exit;
  for j := 0 to Troop.Units.Count-1 do begin
    U := Troop.Units[j];
    if U.Num < 0 then Continue;
    // Replace calls to new units in orders
    for k := 0 to U.Orders.Count-1 do
      U.Orders[k] := DoRedirOrder(U.Orders[k], Troop.Units);
    // Remove "@;new 1" tokens
    k := 0;
    while k < U.Orders.Count do
      if U.Order(k) = '@;new' then U.Orders.Delete(k)
      else Inc(k);
  end;
  Result := Modified;
end;

procedure PreProcess;
begin
  RunAllRoutes(Turn);
end;

procedure PostProcess(RerunRegions: TRegionList);
var i: integer;
    mdf: boolean;
begin
  for i := 0 to VTurn.Regions.Count-1 do begin
    mdf := False;
    if Config.ReadBool('MainWin', 'RedirNew', False)
      and RedirNewOrders(VTurn.Regions[i]) then mdf := True;
    if DistributeNeeds(VTurn.Regions[i]) then mdf := True;
    if mdf then RerunRegions.Add(VTurn.Regions[i]);
  end;
end;

end.
