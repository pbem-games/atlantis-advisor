unit uWantedItems;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, PowerGrid, DataStructs, Resources, uInterface;

type
  TWantedItemsForm = class(TForm)
    cmItem: TComboBox;
    Grid: TPowerGrid;
    cmTowns: TComboBox;
    btnAddTown: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cmItemDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure btnAddTownClick(Sender: TObject);
    procedure cmItemChange(Sender: TObject);
    procedure GridDblClick(Sender: TObject);
  private
    Regions: TRegionList;
    procedure FillGrid;
  public
    procedure Setup(IData: TItemData);
  end;

var
  WantedItemsForm: TWantedItemsForm;

implementation

{$R *.dfm}

uses Main;

procedure TWantedItemsForm.FormCreate(Sender: TObject);
var x, y: integer;
    R: TRegion;
begin
  Grid.Cells[0, 0] := 'Settlement';
  Grid.Cells[1, 0] := 'Cost';
  Grid.Cols[1].Format := cfNumber;

  Regions := TRegionList.Create;
  // Towns
  for x := Map.Levels[Map.Level].Bounds.Left to Map.Levels[Map.Level].Bounds.Right do
    for y := Map.Levels[Map.Level].Bounds.Top to Map.Levels[Map.Level].Bounds.Bottom do begin
      R := Map.Region(x, y, Map.Level, Turn.Num);
      if (R <> nil) and (R.Settlement <> '') then begin
        if (CurrRegion <> nil) and (X >= CurrRegion.x - 10)
          and (X <= CurrRegion.x + 10) and (Y >= CurrRegion.y - 10)
          and (Y <= CurrRegion.y + 10) then Regions.Add(R)
        else cmTowns.AddItem(R.Settlement, R);
      end;
    end;
  FillItemDataCombo(cmItem, IT_SILVER + IT_MAN + IT_MONSTER, True, False);
  cmItem.ItemIndex := 0;
  FillGrid;
end;

procedure TWantedItemsForm.FormDestroy(Sender: TObject);
begin
  Regions.Free;
end;

procedure TWantedItemsForm.FillGrid;
var i, row: integer;
    Item: TItem;
begin
  Grid.RowCount := 1;
  for i := 0 to Regions.Count-1 do begin
    Item := Regions[i].Wanted.Find(TItemData(cmItem.Items.Objects[cmItem.ItemIndex]).Short);
    if Item = nil then Continue;
    row := Grid.RowCount;
    Grid.Cells[0, row] := Regions[i].Settlement;
    Grid.Cells[1, row] := '$' + IntToStr(Item.Cost);
    Grid.SortKeys[1, row] := IntToStr(Item.Cost);
    Grid.Rows[row].Data := Regions[i];
  end;
  Grid.Fixup;
end;

procedure TWantedItemsForm.cmItemDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  ItemDataComboDrawItem(Control, Index, Rect);
end;

procedure TWantedItemsForm.btnAddTownClick(Sender: TObject);
begin
  if cmTowns.ItemIndex >= 0 then begin
    Regions.Add(cmTowns.Items.Objects[cmTowns.ItemIndex]);
    cmTowns.Items.Delete(cmTowns.ItemIndex);
    cmTowns.Text := '';
    FillGrid;
  end;
end;

procedure TWantedItemsForm.cmItemChange(Sender: TObject);
begin
  FillGrid;
end;

procedure TWantedItemsForm.Setup(IData: TItemData);
var idx: integer;
begin
  idx := cmItem.Items.IndexOfObject(IData);
  if idx >= 0 then cmItem.ItemIndex := idx;
  FillGrid;
end;

procedure TWantedItemsForm.GridDblClick(Sender: TObject);
begin
  if Grid.RowCount > 1 then
    MainForm.HexMapGoto(TRegion(Grid.Rows[Grid.Row].Data).Coords);
end;

end.
