unit uItemStats;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, uInterface, DataStructs, Resources;

type
  TItemStatsForm = class(TForm)
    Grid: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GridDrawIcon(Sender: TObject; ACanvas: TCanvas; X,
      Y: Integer; Data: Pointer);
  private
    List: TItemList;
    procedure CountItems(Fac: TFaction; List: TItemList);
  public
    { Public declarations }
  end;

var
  ItemStatsForm: TItemStatsForm;

implementation

{$R *.lfm}

procedure TItemStatsForm.FormCreate(Sender: TObject);
var OldList: TItemList;
    OldI: TItem;
    i, row, idx, amt: integer;
begin
  // FIXME: broken
  //Grid.Cells[0, 0] := 'Item';
  //Grid.Cols[0].Format := cfNumber;
  //Grid.Cells[1, 0] := 'Amount';
  //Grid.Cols[1].Format := cfNumber;
  //Grid.Cells[2, 0] := 'Change';
  //Grid.Cols[2].Format := cfNumber;
  //
  //List := TItemList.Create;
  //CountItems(Turn.Factions[1], List);
  //idx := Game.Turns.IndexOf(Turn);
  //OldList := TItemList.Create;
  //if idx > 1 then CountItems(Game.Turns[idx-1].Factions[1], OldList);
  //
  //Grid.RowCount := 1;
  //for i := 0 to List.Count-1 do begin
  //  row := Grid.RowCount;
  //  Grid.Cells[0, row] := List[i].Name;
  //  Grid.SortKeys[0, row] := IntToStr(Game.ItemData.IndexOf(List[i].Data));
  //  Grid.Cells[1, row] := IntToStr(List[i].Amount);
  //  OldI := OldList.Find(List[i].Data.Short);
  //  if OldI <> nil then begin
  //    amt := List[i].Amount - OldI.Amount;
  //    Grid.Cells[2, row] := IntToStr(amt);
  //    if amt > 0 then Grid.Cells[2, row] := '+' + Grid.Cells[2, row];
  //  end;
  //  Grid.Rows[row].Data := List[i].Data;
  //end;
  //Grid.Fixup;
  //
  //OldList.Free;
end;

procedure TItemStatsForm.FormDestroy(Sender: TObject);
begin
  List.Free;
end;

procedure TItemStatsForm.CountItems(Fac: TFaction; List: TItemList);
var Item1, Item2: TItem;
    i, j: integer;
begin
  for i := 0 to Fac.Units.Count-1 do
    for j := 0 to Fac.Units[i].Items.Count-1 do begin
      Item1 := Fac.Units[i].Items[j];
      Item2 := List.Find(Item1.Data.Short);
      if Item2 = nil then begin
        Item2 := TItem.Create;
        Item2.Data := Item1.Data;
        List.Add(Item2);
      end;
      Inc(Item2.Amount, Item1.Amount);
    end;
end;

procedure TItemStatsForm.GridDrawIcon(Sender: TObject; ACanvas: TCanvas; X,
  Y: Integer; Data: Pointer);
begin
  DrawItemIcon(ACanvas, X, Y, Data);
end;

end.
