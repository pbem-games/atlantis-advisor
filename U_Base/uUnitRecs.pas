unit uUnitRecs;

{$MODE Delphi}

interface

uses
  SysUtils, Windows, Classes, DataStructs, MyStrings;

const
  KeepLocal = '$!keep!$';

type
  TUnitRec = record
    Num: integer;
    Coords: TCoords; // for "new 1" units
    Faction: integer;
    Mage: boolean;
    Local: string;
  end;

  TUnitRecs = class
  private
    Recs: array of TUnitRec;
    procedure AddRec(Num: integer; ACoords: TCoords; Faction: integer;
      Mage: boolean; Local: string);
    function RecIndex(Num: integer; ACoords: TCoords): integer;
  public
    procedure Read(Lines: TStrings);
    procedure Write(Lines: TStrings);
    procedure GatherInfo;
    procedure AddUnitRec(AUnit: TUnit; Local: string);
    function Mage(Num: integer; ACoords: TCoords): boolean;
    function Faction(Num: integer; ACoords: TCoords): integer;
    function Local(Num: integer; ACoords: TCoords): string;
  end;

var
  UnitRecs: TUnitRecs;

implementation


{ TUnitRecs }

function TUnitRecs.RecIndex(Num: integer; ACoords: TCoords): integer;
begin
  Result := Length(Recs) - 1;
  while (Result >= 0) and ((Recs[Result].Num <> Num)
    or ((Recs[Result].Num < 0) and not EqualCoords(Recs[Result].Coords,
    ACoords))) do
    Dec(Result);
end;

procedure TUnitRecs.AddRec(Num: integer; ACoords: TCoords; Faction: integer;
  Mage: boolean; Local: string);
var i: integer;
begin
  i := RecIndex(Num, ACoords);
  if i < 0 then begin
    SetLength(Recs, Length(Recs)+1);
    i := Length(Recs)-1;
    Recs[i].Local := '';
  end;
  Recs[i].Num := Num;
  Recs[i].Coords := ACoords;
  Recs[i].Faction := Faction;
  Recs[i].Mage := Mage;
  if Local <> KeepLocal then Recs[i].Local := Local;
end;

function TUnitRecs.Faction(Num: integer; ACoords: TCoords): integer;
var i: integer;
begin
  i := RecIndex(Num, ACoords);
  if i >= 0 then Result := Recs[i].Faction
  else Result := -1
end;

function TUnitRecs.Local(Num: integer; ACoords: TCoords): string;
var i: integer;
begin
  i := RecIndex(Num, ACoords);
  if i >= 0 then Result := Recs[i].Local
  else Result := '';
end;

function TUnitRecs.Mage(Num: integer; ACoords: TCoords): boolean;
var i: integer;
begin
  i := RecIndex(Num, ACoords);
  if i >= 0 then Result := Recs[i].Mage
  else Result := False;
end;

procedure DatParser(Trace: TTrace);
var num, fnum: integer;
    mage: boolean;
    local: string;
begin
  num := StrToInt(Trace.Before('='));
  fnum := StrToInt(Trace.Block);
  mage := StrToBool(Trace.Block);
  local := Trace.QBlock;
  UnitRecs.AddRec(num, Coords(0, 0, 0), fnum, mage, local);
end;

procedure TUnitRecs.Read(Lines: TStrings);
var line: integer;
begin
  line := 0;
  ReadDataList('[UnitRecs]', Lines, line, DatParser);
end;

procedure TUnitRecs.Write(Lines: TStrings);
var i: integer;
    s: string;
begin
  Lines.Add('[UnitRecs]');
  for i := 0 to Length(Recs)-1 do
    // Do not save records for "new 1" units
    if Recs[i].Num > 0 then begin
      FmtStr(s, '%d=%d, %d, "%s"', [Recs[i].Num, Recs[i].Faction,
        BoolToInt(Recs[i].Mage), Recs[i].Local]);
      Lines.Add(s);
    end;
  Lines.Add('');
end;

procedure TUnitRecs.GatherInfo;
var t, f, i: integer;
    Fac: TFaction;
begin
  for t := 1 to Game.Turns.Count-1 do
    // Skip unknown faction
    for f := 1 to Game.Turns[t].Factions.Count-1 do begin
      Fac := Game.Turns[t].Factions[f];
      for i := 0 to Fac.Units.Count-1 do
        AddUnitRec(Fac.Units[i], KeepLocal);
    end;
end;

procedure TUnitRecs.AddUnitRec(AUnit: TUnit; Local: string);
begin
  AddRec(AUnit.Num, AUnit.Region.Coords, AUnit.Faction.Num,
    AUnit.Mage, Local);
end;


initialization
  UnitRecs := TUnitRecs.Create;

finalization
  UnitRecs.Free;

end.
