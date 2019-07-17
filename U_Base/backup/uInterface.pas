unit uInterface;

interface

uses
  SysUtils, Windows, Classes, Controls, StdCtrls, DataStructs, uGameSubs,
  Resources, PowerGrid, Graphics, MyStrings, Forms;

  procedure ListDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
    BmpIndex: integer);
  procedure ComboDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
    BmpIndex: integer);
  procedure StructComboDrawItem(Control: TWinControl; Index: Integer; Rect: TRect);
  procedure StructListDrawItem(Control: TWinControl; Index: Integer; Rect: TRect);
  procedure FillSpecCombo(Combo: TComboBox; FirstEmpty: boolean);
  procedure FillSkillCombo(Combo: TComboBox; Flags: DWord;
    NotFlags, FirstEmpty, ExcludeBased: boolean);
  procedure SkillComboDrawItem(Control: TWinControl; Index: Integer; Rect: TRect);
  procedure FillItemDataCombo(Combo: TComboBox; Flags: DWord; NotFlags,
    FirstEmpty: boolean); overload;
  procedure FillItemDataCombo(Combo: TComboBox; Flags: DWord; NotFlags,
    FirstEmpty, Many: boolean); overload;
  procedure FillStructDataCombo(Combo: TComboBox; Flags: DWord; NotFlags,
    FirstEmpty: boolean);
  procedure FillResourceCombo(Combo: TComboBox; FirstEmpty: boolean);
  procedure ItemDataComboDrawItem(Control: TWinControl; Index: Integer; Rect: TRect);
  procedure ItemDataListDrawItem(Control: TWinControl; Index: Integer; Rect: TRect);
  procedure ItemListToCombo(Items: TItemList; Combo: TComboBox; Flags: DWord; NotFlags, FirstEmpty: boolean);
  procedure ItemComboDrawItem(Control: TWinControl; Index: Integer; Rect: TRect);
  procedure FillRegionUnits(Combo: TComboBox; ARegion: TRegion; AUnit: TUnit;
    ExcludeUnit, SelectFormer: boolean);
  procedure AddItemGridItem(Grid: TPowerGrid; Item: TItem; Color: TColor);
  procedure FillItemGrid(Grid: TPowerGrid; ItemList: TItemList);
  procedure ItemGridDrawCell(Sender: TObject; ACol, ARow: Integer;
    var TxtRect: TRect; NameCol: integer);
  procedure InsertItemGridRow(Grid: TPowerGrid; Index: integer;
    Field1, Field2, SortKey: string; Data: pointer; Color: TColor);
  procedure SaveFormPosition(Form: TForm);
  procedure LoadFormPosition(Form: TForm);
  procedure FillFactionsCombo(Combo: TComboBox; NotPlayer, All: boolean);

  // common DataGrids
  procedure DelDataFromGrid(List: TList; Grid: TPowerGrid);
  procedure AddDataFromCombo(List: TList; Combo: TComboBox);

  // ItemGrids with editing
  procedure FillIDataGrid(Grid: TPowerGrid; List: TItemList; Sort: boolean); overload;
  procedure FillIDataGrid(Grid: TPowerGrid; List: TItemList); overload;
  procedure FillIDataGrid(Grid: TPowerGrid; List: TItemDataList); overload;
  procedure AddItemFromCombo(List: TItemList; Combo: TComboBox);
  procedure DelItemFromGrid(List: TItemList; Grid: TPowerGrid);
  procedure IDataGridSetEditText(Sender: TObject; const Value: String;
    List: TItemList);

  // SkillGrids with editing
  procedure FillSDataGrid(Grid: TPowerGrid; List: TSkillList);
  procedure AddSkillFromCombo(List: TSkillList; Combo: TComboBox);
  procedure DelSkillFromGrid(List: TSkillList; Grid: TPowerGrid);
  procedure SDataGridSetEditText(Sender: TObject; const Value: String;
    List: TSkillList);

implementation

procedure FillFactionsCombo(Combo: TComboBox; NotPlayer, All: boolean);
var i, j: integer;
    F: TFaction;
begin
  Combo.Sorted := True;
  for i := 1 to Game.Turns.Count-1 do
    for j := 0 to Game.Turns[i].Factions.Count-1 do begin
      F := Game.Turns[i].Factions[j];
      if F.Player and NotPlayer then Continue;
      if Combo.Items.IndexOf(FactionName(F) + ' (' + IntToStr(F.Num) +
        ')') = -1 then
        if F.Num > 0 then Combo.AddItem(FactionName(F) + ' (' +
          IntToStr(F.Num) + ')', F.Data);
    end;
  Combo.Sorted := False;
  Combo.Items.InsertObject(0, 'Unknown', Turn.Factions[0].Data);
  if All then Combo.Items.InsertObject(0, '*', nil);
end;

procedure SaveFormPosition(Form: TForm);
begin
  Config.WriteInteger(Form.Name, 'Top', Form.Top);
  Config.WriteInteger(Form.Name, 'Left', Form.Left);
  Config.WriteInteger(Form.Name, 'Width', Form.Width);
  Config.WriteInteger(Form.Name, 'Height', Form.Height);
end;

procedure LoadFormPosition(Form: TForm);
begin
  Form.Top := Config.ReadInteger(Form.Name, 'Top', Form.Top);
  Form.Left := Config.ReadInteger(Form.Name, 'Left', Form.Left);
  Form.Width := Config.ReadInteger(Form.Name, 'Width', Form.Width);
  Form.Height := Config.ReadInteger(Form.Name, 'Height', Form.Height);
end;


procedure BoxDrawItem(Canvas: TCanvas; Items: TStrings; Index: integer;
  Rect: TRect; BmpIndex: integer);
begin
  Canvas.FillRect(Rect);
  if Items.Objects[Index] <> nil then begin
    ResForm.IconList.Draw(Canvas, Rect.Left+1, Rect.Top, BmpIndex);
    Rect.Left := Rect.Left + 18;
    Canvas.TextRect(Rect, Rect.Left+1, Rect.Top+1, Items[Index]);
  end;
end;

procedure ListDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
  BmpIndex: integer);
begin
  BoxDrawItem(TListBox(Control).Canvas, TListBox(Control).Items,
    Index, Rect, BmpIndex);
end;

procedure ComboDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
  BmpIndex: integer);
begin
  BoxDrawItem(TComboBox(Control).Canvas, TComboBox(Control).Items,
    Index, Rect, BmpIndex);
end;

procedure FillSpecCombo(Combo: TComboBox; FirstEmpty: boolean);
var i: integer;
begin
  Combo.Items.Clear;
  if FirstEmpty then Combo.Items.Add('');
  for i := 0 to Game.SpecData.Count-1 do
    Combo.AddItem(Game.SpecData[i].Name, Game.SpecData[i]);
end;

function CompareSDatas(S1, S2: TSkillData): boolean;
var m1, m2: boolean;
begin
  Result := False;
  if (S1 = nil) or (S2 = nil) then Exit;
  m1 := Test(S1.Flags, SK_MAGIC) and not Test(S1.Flags, SK_FOUNDATION);
  m2 := Test(S2.Flags, SK_MAGIC) and not Test(S2.Flags, SK_FOUNDATION);
  if m1 and not m2 then Result := True
  else if not m1 and m2 then Exit
  else begin
    if Test(S1.Flags, SK_FOUNDATION) and not Test(S2.Flags, SK_FOUNDATION) then
      Result := True
    else if not Test(S1.Flags, SK_FOUNDATION) and Test(S2.Flags, SK_FOUNDATION) then
      Exit
    else Result := (S1.Name > S2.Name);
  end;
end;

procedure FillSkillCombo(Combo: TComboBox; Flags: DWord; NotFlags,
  FirstEmpty, ExcludeBased: boolean);
var i, j: integer;
    s: string;
    flg: DWord;
begin
  Combo.Items.Clear;
  if FirstEmpty then Combo.Items.Add('');
  for i := 0 to Game.SkillData.Count-1 do begin
    if ExcludeBased and (Game.SkillData[i].BasedOn.Count > 0) then
      Continue;
    flg := Game.SkillData[i].Flags;
    if flg = 0 then flg := SK_UNKNOWN;
    if Test(flg, Flags) xor NotFlags then begin
      s := Game.SkillData[i].MakeName;
      if Combo.Canvas.TextWidth(s) > Combo.Width - 16 then
        s := Game.SkillData[i].Short + ', ' + s;
      Combo.AddItem(s, Game.SkillData[i]);
    end;
  end;
  // Sort
  for i := 0 to Combo.Items.Count-2 do
    for j := i + 1 to Combo.Items.Count-1 do
      if CompareSDatas(TSkillData(Combo.Items.Objects[i]),
        TSkillData(Combo.Items.Objects[j])) then
        Combo.Items.Exchange(i, j);
end;

procedure SkillComboDrawItem(Control: TWinControl; Index: Integer; Rect: TRect);
begin
  ComboDrawItem(Control, Index, Rect,
    SkillIcon(TSkillData(TComboBox(Control).Items.Objects[Index])));
end;

procedure StructComboDrawItem(Control: TWinControl; Index: Integer; Rect: TRect);
begin
  with TComboBox(Control) do begin
    Canvas.FillRect(Rect);
    if Items.Objects[Index] <> nil then begin
      DrawStructIcon(Canvas, Rect.Left+1, Rect.Top, TStructData(Items.Objects[Index]),
        False);
      Rect.Left := Rect.Left + 18;
      Canvas.TextRect(Rect, Rect.Left+1, Rect.Top+1, Items[Index]);
    end;
  end;
end;

procedure StructListDrawItem(Control: TWinControl; Index: Integer; Rect: TRect);
begin
  with TListBox(Control) do begin
    Canvas.FillRect(Rect);
    if Items.Objects[Index] <> nil then begin
      DrawStructIcon(Canvas, Rect.Left+1, Rect.Top, TStructData(Items.Objects[Index]),
        False);
      Rect.Left := Rect.Left + 18;
      Canvas.TextRect(Rect, Rect.Left+1, Rect.Top+1, Items[Index]);
    end;
  end;
end;

procedure FillResourceCombo(Combo: TComboBox; FirstEmpty: boolean);
var flg, i: integer;
begin
  Combo.Items.Clear;
  if FirstEmpty then Combo.Items.Add('');
  for i := 0 to Game.ItemData.Count-1 do begin
    flg := Game.ItemData[i].Flags;
    if (flg = IT_RESOURCE) or (flg = IT_RESOURCE + IT_ADVANCED) then
      Combo.AddItem(Game.ItemData[i].Name(True), Game.ItemData[i]);
  end;
end;

procedure FillStructDataCombo(Combo: TComboBox; Flags: DWord; NotFlags,
  FirstEmpty: boolean);
var flg, i: integer;
begin
  Combo.Items.Clear;
  if FirstEmpty then Combo.Items.Add('');
  for i := 0 to Game.StructData.Count-1 do begin
    flg := Game.StructData[i].Flags;
    if flg = 0 then flg := ST_UNKNOWN;
    if Test(flg, Flags) xor NotFlags then
      Combo.AddItem(Game.StructData[i].Group, Game.StructData[i]);
  end;
end;

procedure FillItemDataCombo(Combo: TComboBox; Flags: DWord; NotFlags,
  FirstEmpty, Many: boolean);
var flg, i: integer;
begin
  Combo.Items.Clear;
  if FirstEmpty then Combo.Items.Add('');
  for i := 0 to Game.ItemData.Count-1 do begin
    flg := Game.ItemData[i].Flags;
    if flg = 0 then flg := IT_UNKNOWN;
    if Test(flg, Flags) xor NotFlags then
      Combo.AddItem(Game.ItemData[i].Name(Many), Game.ItemData[i]);
  end;
end;

procedure FillItemDataCombo(Combo: TComboBox; Flags: DWord; NotFlags,
  FirstEmpty: boolean);
begin
  FillItemDataCombo(Combo, Flags, NotFlags, FirstEmpty, True);
end;

procedure ItemListToCombo(Items: TItemList; Combo: TComboBox; Flags: DWord; NotFlags, FirstEmpty: boolean);
var i: integer;
begin
  Combo.Items.Clear;
  if FirstEmpty then Combo.Items.Add('');
  for i := 0 to Items.Count-1 do
    if Test(Items[i].Data.Flags, Flags) xor NotFlags then begin
      Combo.AddItem(Items[i].Data.Name(True), Items[i]);
    end;
  Combo.Enabled := (Combo.Items.Count > 0);
end;

procedure ItemDataComboDrawItem(Control: TWinControl; Index: Integer; Rect: TRect);
begin
  with TComboBox(Control) do begin
    Canvas.FillRect(Rect);
    if Items.Objects[Index] <> nil then
      DrawItemIcon(Canvas, Rect.Left+1, Rect.Top, TItemData(Items.Objects[Index]));
    Rect.Left := Rect.Left + 18;
    Canvas.TextRect(Rect, Rect.Left+1, Rect.Top+1, Items[Index]);
  end;
end;

procedure ItemDataListDrawItem(Control: TWinControl; Index: Integer; Rect: TRect);
begin
  with TListBox(Control) do begin
    Canvas.FillRect(Rect);
    if Items.Objects[Index] <> nil then begin
      DrawItemIcon(Canvas, Rect.Left+1, Rect.Top, TItemData(Items.Objects[Index]));
      Rect.Left := Rect.Left + 18;
      Canvas.TextRect(Rect, Rect.Left+1, Rect.Top+1, Items[Index]);
    end;
  end;
end;

procedure ItemComboDrawItem(Control: TWinControl; Index: Integer; Rect: TRect);
begin
  with TComboBox(Control) do begin
    Canvas.FillRect(Rect);
    if Items.Objects[Index] <> nil then begin
      DrawItemIcon(Canvas, Rect.Left+1, Rect.Top, TItem(Items.Objects[Index]).Data);
      Rect.Left := Rect.Left + 18;
      Canvas.TextRect(Rect, Rect.Left+1, Rect.Top+1, Items[Index]);
    end;
  end;
end;

procedure FillRegionUnits(Combo: TComboBox; ARegion: TRegion; AUnit: TUnit;
  ExcludeUnit, SelectFormer: boolean);
var i: integer;
begin
  for i := 0 to VFaction.Units.Count-1 do
    if (VFaction.Units[i].Region = ARegion)
      and (not ExcludeUnit or (VFaction.Units[i] <> AUnit)) then
      Combo.AddItem(VFaction.Units[i].Name + ' (' +
        VFaction.Units[i].NumStr + ')', VFaction.Units[i]);
  if AUnit <> nil then
    for i := 0 to Combo.Items.Count-1 do
      if (SelectFormer and (AUnit.Former = TUnit(Combo.Items.Objects[i])))
        or (not SelectFormer and (AUnit = TUnit(Combo.Items.Objects[i]))) then
        Combo.ItemIndex := i;
end;

procedure AddItemGridItem(Grid: TPowerGrid; Item: TItem; Color: TColor);
var i: integer;
begin
  i := Grid.RowCount;
  if Item.Amount >= 0 then
    Grid.Cells[0, i] := IntToStr(Item.Amount);
  Grid.Cells[1, i] := Item.Name;
  Grid.SortKeys[1, i] := IntToStr(Game.ItemData.IndexOf(Item.Data));
  if Grid.ColCount > 2 then Grid.Cells[2, i] := '$' + IntToStr(Item.Cost);
  Grid.Rows[i].Data := Item;
  Grid.Rows[i].Color := Color;
end;

procedure FillItemGrid(Grid: TPowerGrid; ItemList: TItemList);
var i: integer;
begin
  Grid.RowCount := 0;
  for i := 0 to ItemList.Count-1 do
    if ItemList[i].Bought then
      AddItemGridItem(Grid, ItemList[i], clGrayText)
    else AddItemGridItem(Grid, ItemList[i], clWindowText);
  Grid.Fixup;
end;

procedure ItemGridDrawCell(Sender: TObject; ACol, ARow: Integer;
  var TxtRect: TRect; NameCol: integer);
var Item: TItem;
begin
  with Sender as TPowerGrid do begin
    Item := TItem(ImgRows[ARow].Data);
    if (ACol = NameCol) and (ARow >= FixedRows) then begin
      if Item <> nil then
        DrawItemIcon(Canvas, TxtRect.Left+1, TxtRect.Top, Item.Data)
      else ResForm.IconList.Draw(Canvas, TxtRect.Left+1, TxtRect.Top, bmpSilver);
      TxtRect.Left := TxtRect.Left + 18;
    end;
  end;
end;

procedure InsertItemGridRow(Grid: TPowerGrid; Index: integer;
  Field1, Field2, SortKey: string; Data: pointer; Color: TColor);
var i, j: integer;
begin
  for i := Grid.RowCount downto Index+1 do begin
    for j := 0 to Grid.ColCount-1 do begin
      Grid.Cells[j, i] := Grid.Cells[j, i-1];
      Grid.SortKeys[j, i] := Grid.SortKeys[j, i-1];
    end;
    Grid.Rows[i].Data := Grid.Rows[i-1].Data;
    Grid.Rows[i].Color := Grid.Rows[i-1].Color;
    Grid.Rows[i].FontStyle := Grid.Rows[i-1].FontStyle;
  end;
  Grid.Cells[0, Index] := Field1;
  Grid.Cells[1, Index] := Field2;
  Grid.SortKeys[1, Index] := SortKey;
  Grid.Rows[Index].Data := Data;
  Grid.Rows[Index].Color := Color;
  Grid.Rows[Index].FontStyle := [];
end;

{ common DataGrid }

procedure DelDataFromGrid(List: TList; Grid: TPowerGrid);
var i: integer;
begin
  if (Grid.Row < Grid.FixedRows) or (Grid.Row >= Grid.RowCount) then Exit;
  i := List.IndexOf(Grid.Rows[Grid.Row].Data);
  if i >= 0 then List.Delete(i);
end;

procedure AddDataFromCombo(List: TList; Combo: TComboBox);
var Data: pointer;
begin
  if Combo.ItemIndex < 0 then Exit;
  Data := Combo.Items.Objects[Combo.ItemIndex];
  if List.IndexOf(Data) < 0 then List.Add(Data);
end;

{ ItemDataGrid }

procedure FillIDataGrid(Grid: TPowerGrid; List: TItemList; Sort: boolean);
var i: integer;
begin
  Grid.RowCount := 0;
  for i := 0 to List.Count-1 do begin
    Grid.Cells[0, i] := IntToStr(List[i].Amount);
    Grid.Cells[1, i] := List[i].Data.Name(True);
    if Sort then Grid.SortKeys[1, i] := IntToStr(Game.ItemData.IndexOf(List[i].Data));
    Grid.Rows[i].Data := List[i].Data;
  end;
  Grid.Fixup;
end;

procedure FillIDataGrid(Grid: TPowerGrid; List: TItemList);
begin
  FillIDataGrid(Grid, List, False);
end;

procedure FillIDataGrid(Grid: TPowerGrid; List: TItemDataList);
var i: integer;
begin
  Grid.RowCount := 0;
  for i := 0 to List.Count-1 do begin
    Grid.Cells[0, i] := List[i].Name(True);
    Grid.Rows[i].Data := List[i];
  end;
  Grid.Fixup;
end;

// Add item to ItemList from combo of ItemData
procedure AddItemFromCombo(List: TItemList; Combo: TComboBox);
var IData: TItemData;
begin
  if Combo.ItemIndex < 0 then Exit;
  IData := TItemData(Combo.Items.Objects[Combo.ItemIndex]);
  List.Seek(IData.Short);
end;

// Delete item selected in ItemDataGrid from ItemList
procedure DelItemFromGrid(List: TItemList; Grid: TPowerGrid);
var i: integer;
begin
  if (Grid.Row < Grid.FixedRows) or (Grid.Row >= Grid.RowCount) then Exit;
  i := List.Count-1;
  while (i >= 0) and (List[i].Data <> Grid.Rows[Grid.Row].Data) do Dec(i);
  if i < 0 then Exit;
  List[i].Free;
  List.Delete(i);
end;

procedure IDataGridSetEditText(Sender: TObject; const Value: String;
  List: TItemList);
var IData: TItemData;
    Item: TItem;
begin
  with Sender as TPowerGrid do
    IData := TItemData(ImgRows[Row].Data);
  Item := List.Find(IData.Short);
  if Item <> nil then Item.Amount := ToInt(Value);
end;

{ SkillDataGrid }

procedure FillSDataGrid(Grid: TPowerGrid; List: TSkillList);
var i: integer;
begin
  Grid.RowCount := 0;
  for i := 0 to List.Count-1 do begin
    Grid.Cells[0, i] := List[i].Data.Name;
    Grid.Cells[1, i] := IntToStr(List[i].Level);
    Grid.Rows[i].Data := List[i].Data;
    Grid.Rows[i].ImageIndex := SkillIcon(List[i].Data);
  end;
  Grid.Fixup;
end;


// Add skill to SkillList from combo of SkillData
procedure AddSkillFromCombo(List: TSkillList; Combo: TComboBox);
var SData: TSkillData;
begin
  if Combo.ItemIndex < 0 then Exit;
  SData := TSkillData(Combo.Items.Objects[Combo.ItemIndex]);
  List.Seek(SData.Short);
end;

// Delete skill selected in SkillDataGrid from SkillList
procedure DelSkillFromGrid(List: TSkillList; Grid: TPowerGrid);
var i: integer;
begin
  if (Grid.Row < Grid.FixedRows) or (Grid.Row >= Grid.RowCount) then Exit;
  i := List.Count-1;
  while (i >= 0) and (List[i].Data <> Grid.Rows[Grid.Row].Data) do Dec(i);
  if i < 0 then Exit;
  List[i].Free;
  List.Delete(i);
end;

procedure SDataGridSetEditText(Sender: TObject; const Value: String;
  List: TSkillList);
var SData: TSkillData;
    Skill: TSkill;
begin
  with Sender as TPowerGrid do
    SData := TSkillData(ImgRows[Row].Data);
  Skill := List.Find(SData.Short);
  if Skill <> nil then Skill.Level := ToInt(Value);
end;


end.
