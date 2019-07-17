unit uSpyReps;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DataStructs, Resources, StdCtrls, Grids, PowerGrid, uGameSubs,
  ComCtrls, ToolWin, AtlaDate, uInterface, uKeys, MyStrings;

type
  TSpyRepForm = class(TForm)
    gChanges: TPowerGrid;
    ToolBar1: TToolBar;
    btnAppeared: TToolButton;
    btnDisappeared: TToolButton;
    btnChanges: TToolButton;
    btnGuard: TToolButton;
    btnItems: TToolButton;
    ToolButton6: TToolButton;
    btnRegion: TToolButton;
    btnUnit: TToolButton;
    btnAllTurns: TToolButton;
    cmFactions: TComboBox;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    btnEconomy: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure gChangesDrawCell(Sender: TObject; ACol, ARow: Integer;
      var TxtRect: TRect; State: TGridDrawState);
    procedure FilterChange(Sender: TObject);
    procedure gChangesDblClick(Sender: TObject);
  private
    procedure AddMsg(U: TUnit; MsgType, Text: string); overload;
    procedure AddMsg(R: TRegion; MsgType, Text: string); overload;
    procedure Scan(U: TUnit; PrevR: TRegion);
    procedure ReadGuardEvents(U: TUnit);
    procedure CollectInTurn(ATurn: TTurn);
    procedure CollectChanges;
    procedure AddEconomies(R: TRegion);
  public
    procedure SetUnitFilter;
  end;

var
  SpyRepForm: TSpyRepForm;

implementation

{$R *.dfm}

uses
  Main;

procedure TSpyRepForm.FormCreate(Sender: TObject);
var i: integer;
begin
  gChanges.LoadColumns(Config);
  LoadFormPosition(Self);

  btnAppeared.Down := Config.ReadBool('SpyReports', 'Appeared', True);
  btnDisappeared.Down := Config.ReadBool('SpyReports', 'Disappeared', True);
  btnChanges.Down := Config.ReadBool('SpyReports', 'Changes', True);
  btnGuard.Down := Config.ReadBool('SpyReports', 'Flags', True);
  btnItems.Down := Config.ReadBool('SpyReports', 'Items', True);
  btnEconomy.Down := Config.ReadBool('SpyReports', 'Economy', True);
  btnRegion.Down := Config.ReadBool('SpyReports', 'Region', False);
  btnUnit.Down := Config.ReadBool('SpyReports', 'Unit', False);
  btnAllTurns.Down := Config.ReadBool('SpyReports', 'AllTurns', False);

  FillFactionsCombo(cmFactions, True, True);

  if Config.ReadInteger('SpyReports', 'Faction', -1) >= 0 then begin
    i := cmFactions.Items.Count-1;
    while (i >= 0) and (TFaction(cmFactions.Items.Objects[i]).Num <>
      Config.ReadInteger('SpyReports', 'Faction', -1)) do Dec(i);
    if i >= 0 then cmFactions.ItemIndex := i;
  end;
  if btnUnit.Down = True then cmFactions.ItemIndex := 0;
  cmFactions.Enabled := not btnUnit.Down;

  CollectChanges;
end;

procedure TSpyRepForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  gChanges.SaveColumns(Config);
  SaveFormPosition(Self);
end;

procedure TSpyRepForm.CollectChanges;
var i: integer;
begin
  gChanges.RowCount := 1;
  if not btnAllTurns.Down then
    CollectInTurn(Turn)
  else
    for i := 2 to Game.Turns.Count-1 do CollectInTurn(Game.Turns[i]);
  gChanges.Fixup;
end;

procedure TSpyRepForm.CollectInTurn(ATurn: TTurn);
var i, j, k: integer;
    R, PrevR: TRegion;
    U: TUnit;
begin
  for i := 0 to ATurn.Regions.Count-1 do begin
    R := ATurn.Regions[i];

    // Selected region
    if btnRegion.Down and not EqualCoords(R.Coords, MainForm.SelectedCoords) then
      Continue;

    if Game.Turns.IndexOf(ATurn) >= 2 then
      PrevR := Map.Region(R.Coords, ATurn.Num-1)
    else PrevR := nil;

    // Scan units visible in current turn
    for j := 0 to R.Troops.Count-1 do begin
      if R.Troops[j].Faction.Player then begin
        if not btnGuard.Down or btnUnit.Down then Continue;
        for k := 0 to R.Troops[j].Units.Count-1 do
          ReadGuardEvents(R.Troops[j].Units[k]);
      end
      else begin
        if (cmFactions.ItemIndex > 0) and (R.Troops[j].Faction.Data <>
            cmFactions.Items.Objects[cmFactions.ItemIndex]) then Continue;
        for k := 0 to R.Troops[j].Units.Count-1 do begin
          U := R.Troops[j].Units[k];
          if btnUnit.Down and not ((CurrUnit <> nil) and (CurrUnit.Num = U.Num)) then
            Continue;
          Scan(U, PrevR);
        end;
      end;
    end;

    if btnEconomy.Down and not btnUnit.Down then AddEconomies(R);

    if btnDisappeared.Down and (PrevR <> nil) then begin
      // Add messages for units not found in current turn
      for j := 0 to PrevR.Troops.Count-1 do begin
        if PrevR.Troops[j].Faction.Player then Continue;
        if (cmFactions.ItemIndex > 0) and (PrevR.Troops[j].Faction.Data <>
            cmFactions.Items.Objects[cmFactions.ItemIndex]) then Continue;
        for k := 0 to PrevR.Troops[j].Units.Count-1 do begin
          if btnUnit.Down and not ((CurrUnit <> nil)
            and (CurrUnit.Num = PrevR.Troops[j].Units[k].Num)) then
            Continue;
          if R.FindUnit(PrevR.Troops[j].Units[k].Num) = nil then
            AddMsg(PrevR.Troops[j].Units[k], 'disappear', 'Disappeared');
        end;
      end;
    end;

  end;
end;

procedure TSpyRepForm.Scan(U: TUnit; PrevR: TRegion);
var i, amt: integer;
    PrevTroop: TTroop;
    PrevU: TUnit;
    Item: TItem;
begin
  if PrevR = nil then begin
    if btnAppeared.Down then AddMsg(U, 'appear', 'Found in newly explored region');
    Exit;
  end;
  i := PrevR.Troops.Count-1;
  while (i >= 0) and (PrevR.Troops[i].Units.Find(U.Num) = nil) do Dec(i);
  if i >= 0 then PrevTroop := PrevR.Troops[i]
  else PrevTroop := nil;
  if PrevTroop = nil then begin
    if btnAppeared.Down then AddMsg(U, 'appear', 'Appeared');
    Exit;
  end;
  PrevU := PrevTroop.Units.Find(U.Num);
  if PrevU = nil then begin
    if btnAppeared.Down then AddMsg(U, 'appear', 'Appeared');
    Exit;
  end;

  // Found unit in prev region, scan parameters
  if btnChanges.Down then begin
    if PrevU.Faction.Num <> U.Faction.Num then
      AddMsg(U, 'unit', 'Faction was ' + FactionName(PrevU.Faction) +
        ', now ' + FactionName(U.Faction));
    if PrevU.Name <> U.Name then
      AddMsg(U, 'unit', 'Name was ' + PrevU.Name + ', now ' + U.Name);
    if PrevU.Description <> U.Description then
      AddMsg(U, 'unit', 'Description was "' + PrevU.Description +
        '", now "' + U.Description + '"');
  end;

  if btnGuard.Down then begin
    if U.Flags[flgGuard] and not PrevU.Flags[flgGuard] then
      AddMsg(U, 'flags', 'Stands On Guard');
    if not U.Flags[flgGuard] and PrevU.Flags[flgGuard] then
      AddMsg(U, 'flags', 'No more On Guard');
  end;

  if btnItems.Down then begin
    for i := 0 to U.Items.Count-1 do begin
      Item := PrevU.Items.Find(U.Items[i].Data.Short);
      if Item = nil then AddMsg(U, 'items', 'Acquired ' +
        IntToStr(U.Items[i].Amount) + ' ' +
        U.Items[i].Name)
      else begin
        amt := Item.Amount - U.Items[i].Amount;
        if amt > 0 then AddMsg(U, 'items', 'Lose ' + IntToStr(amt) + ' '
          + Item.Data.Name(amt <> 1) + ', was ' + IntToStr(Item.Amount));
        if amt < 0 then AddMsg(U, 'items', 'Got ' + IntToStr(-amt) + ' ' +
          Item.Data.Name(amt <> -1) + ', now ' + IntToStr(U.Items[i].Amount));
      end;
    end;
    for i := 0 to PrevU.Items.Count-1 do begin
      Item := U.Items.Find(PrevU.Items[i].Data.Short);
      if Item = nil then AddMsg(U, 'items', 'Lose all of ' +
        IntToStr(PrevU.Items[i].Amount) + ' ' +
        PrevU.Items[i].Name);
    end;
  end;
end;

procedure TSpyRepForm.AddMsg(U: TUnit; MsgType, Text: string);
var row: integer;
begin
  row := gChanges.RowCount;
  gChanges.Cells[0, row] := MsgType;
  gChanges.Cells[1, row] := U.Name;
  gChanges.Cells[2, row] := U.NumStr;
  gChanges.Cells[3, row] := FactionName(U.Faction);
  gChanges.Cells[4, row] := IntToStr(U.Faction.Num);
  gChanges.Cells[5, row] := MakeRegionName(U.Region.Coords, True);
  gChanges.Cells[6, row] := Text;
  if MsgType = 'disappear' then
    gChanges.Cells[7, row] := IntToStr(U.Region.Visited + 1)
  else gChanges.Cells[7, row] := IntToStr(U.Region.Visited);
  gChanges.Cells[8, row] := IntToStr(U.Items.Amount(IT_MAN) +
    U.Items.Amount(IT_MONSTER));
  gChanges.Rows[row].Data := U.Region;
  gChanges.Rows[row].Color := FactionColor(U.Faction);
end;

procedure TSpyRepForm.AddMsg(R: TRegion; MsgType, Text: string);
var row: integer;
begin
  row := gChanges.RowCount;
  gChanges.Cells[0, row] := MsgType;
  gChanges.Cells[1, row] := 'Unknown units';
  gChanges.Cells[5, row] := MakeRegionName(R.Coords, True);
  gChanges.Cells[6, row] := Text;
  gChanges.Cells[7, row] := IntToStr(R.Visited);
  gChanges.Rows[row].Data := R;
  gChanges.Rows[row].Color := clWhite;
end;

procedure TSpyRepForm.gChangesDrawCell(Sender: TObject; ACol,
  ARow: Integer; var TxtRect: TRect; State: TGridDrawState);
var icon: integer;
begin
  with TPowerGrid(Sender) do begin
    if (ACol = 0) and (ARow > 0) then begin
      icon := bmpUnknownItem;
      if ImgCells[ACol, ARow] = 'unit' then icon := bmpMen
      else if ImgCells[ACol, ARow] = 'appear' then icon := bmpNewUnit
      else if ImgCells[ACol, ARow] = 'disappear' then icon := bmpDelUnit
      else if ImgCells[ACol, ARow] = 'flags' then icon := bmpAdvance
      else if ImgCells[ACol, ARow] = 'items' then icon := bmpTradeGoods
      else if ImgCells[ACol, ARow] = 'economy' then icon := bmpSilver;
      ResForm.IconList.Draw(Canvas, TxtRect.Left, TxtRect.Top, icon);
      TxtRect.Left := TxtRect.Right;
    end
    else if (ImgCells[ACol, 0] = 'Date') and (ARow > 0) then begin
      Canvas.TextRect(TxtRect, TxtRect.Left + 2, TxtRect.Top + 2,
        TurnToShortDate(StrToInt(ImgCells[ACol, ARow])));
      TxtRect.Left := TxtRect.Right;
    end;
  end;
end;

procedure TSpyRepForm.FilterChange(Sender: TObject);
begin
  if btnUnit.Down = True then cmFactions.ItemIndex := 0;
  cmFactions.Enabled := not btnUnit.Down;
  Config.WriteBool('SpyReports', 'Appeared', btnAppeared.Down);
  Config.WriteBool('SpyReports', 'Disappeared', btnDisappeared.Down);
  Config.WriteBool('SpyReports', 'Changes', btnChanges.Down);
  Config.WriteBool('SpyReports', 'Flags', btnGuard.Down);
  Config.WriteBool('SpyReports', 'Items', btnItems.Down);
  Config.WriteBool('SpyReports', 'Economy', btnEconomy.Down);
  Config.WriteBool('SpyReports', 'Region', btnRegion.Down);
  Config.WriteBool('SpyReports', 'Unit', btnUnit.Down);
  Config.WriteBool('SpyReports', 'AllTurns', btnAllTurns.Down);
  if cmFactions.ItemIndex <= 0 then
    Config.WriteInteger('SpyReports', 'Faction', -1)
  else Config.WriteInteger('SpyReports', 'Faction',
    TFaction(cmFactions.Items.Objects[cmFactions.ItemIndex]).Num);

  CollectChanges;
end;

procedure TSpyRepForm.SetUnitFilter;
begin
  btnAppeared.Down := True;
  btnDisappeared.Down := True;
  btnChanges.Down := True;
  btnGuard.Down := True;
  btnItems.Down := True;
  btnUnit.Down := True;
  FilterChange(btnUnit);
end;

procedure TSpyRepForm.gChangesDblClick(Sender: TObject);
begin
  with gChanges do
    if RowCount > 1 then begin
      MainForm.HexMapGoto(TRegion(ImgRows[Row].Data).Coords);
      Close;
    end;
end;

procedure TSpyRepForm.ReadGuardEvents(U: TUnit);
var i, row, unum, num: integer;
    Trace: TTrace;
    Fac: TFaction;
    uname: string;
begin
  if not U.Flags[flgGuard] then Exit;
  for i := 0 to U.Events.Count-1 do
    if Pos(Keys[s_Forbids], U.Events[i]) = 1 then begin
      Trace := TTrace.Create(U.Events[i]);
      Trace.Before(Keys[s_Forbids]);
      row := gChanges.RowCount;
      uname := Trace.Before(' (');
      unum := StrToInt(Trace.Before(')'));
      if not btnUnit.Down or ((CurrUnit <> nil) and (CurrUnit.Num = unum)) then begin
        gChanges.Cells[0, row] := 'flags';
        gChanges.Cells[1, row] := uname;
        gChanges.Cells[2, row] := IntToStr(unum);
        Fac := nil;
        if Pos(',', Trace.Text) = 1 then begin
          Trace.Before(' ');
          gChanges.Cells[3, row] := Trace.Before(' (');
          num := Trace.Num;
          Fac := FindFaction(num);
          gChanges.Cells[4, row] := IntToStr(num);
        end;
        if Fac = nil then Fac := Turn.Factions[0];
        gChanges.Cells[5, row] := MakeRegionName(U.Region.Coords, True);
        gChanges.Cells[6, row] := 'Forbidden entry';
        gChanges.Cells[7, row] := IntToStr(U.Region.Visited);
        gChanges.Cells[8, row] := IntToStr(U.Items.Amount(IT_MAN) +
          U.Items.Amount(IT_MONSTER));
        gChanges.Rows[row].Data := U.Region;
        gChanges.Rows[row].Color := FactionColor(Fac);
      end;
      Trace.Free;
    end;
end;

procedure TSpyRepForm.AddEconomies(R: TRegion);
begin
  if R.OtherFactions.Taxers > 0 then
    AddMsg(R, 'economy', 'Taxed: ' + IntToStr(R.OtherFactions.Taxers));

  if R.OtherFactions.Entertainers > 0 then
    AddMsg(R, 'economy', 'Entertained: ' +
      IntToStr(R.OtherFactions.Entertainers) + ' ' +
      Numeric('level', R.OtherFactions.Entertainers));

  if R.OtherFactions.Workers > 0 then
    AddMsg(R, 'economy', 'Worked: ' + IntToStr(R.OtherFactions.Workers));

  if R.OtherFactions.Products <> '' then
    AddMsg(R, 'economy', 'Produced: ' + R.OtherFactions.Products);
end;

end.
