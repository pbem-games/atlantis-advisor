unit uTownTrade;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, uInterface, DataStructs, uGameSubs,
  Buttons;

type
  TTownTradeForm = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    gWanted: TPowerGrid;
    gForSale: TPowerGrid;
    cmTowns: TComboBox;
    bAdd: TButton;
    procedure DrawCell(Sender: TObject; ACol, ARow: Integer;
      var TxtRect: TRect; State: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bAddClick(Sender: TObject);
  private
    Regions: TList;
    procedure FillGrid(Grid: TPowerGrid; List1: TItemList; Wanted: boolean);
  public
    { Public declarations }
  end;

var
  TownTradeForm: TTownTradeForm;

implementation

uses Types;

{$R *.dfm}

procedure TTownTradeForm.FormCreate(Sender: TObject);
var x, y: integer;
    R, RealR: TARegion;
begin
  if (CurrRegion <> nil) and (CurrRegion.Settlement <> '') then begin
    Caption := Caption + ' - ' + CurrRegion.Settlement;
    Regions := TList.Create;
    RealR := Map.Region(CurrRegion.Coords, Turn.Num);
    // Towns
    for x := Map.Levels[Map.Level].Bounds.Left to Map.Levels[Map.Level].Bounds.Right do
      for y := Map.Levels[Map.Level].Bounds.Top to Map.Levels[Map.Level].Bounds.Bottom do begin
        R := Map.Region(x, y, Map.Level, Turn.Num);
        if (R <> nil) and (R.Settlement <> '') then
          if (X >= RealR.x - 6) and (X <= RealR.x + 6)
            and (Y >= RealR.y - 6) and (Y <= RealR.y + 6) then
            Regions.Add(R)
          else cmTowns.AddItem(R.Settlement, R);
      end;
    // Grid
    FillGrid(gWanted, RealR.Wanted, False);
    FillGrid(gForSale, RealR.ForSale, True);
  end
  else Caption := Caption + ' - No town';
end;

procedure TTownTradeForm.FillGrid(Grid: TPowerGrid; List1: TItemList;
  Wanted: boolean);
var i, j, k, row, profit: integer;
    List2: TItemList;
begin
  Grid.RowCount := 0;
  for i := 0 to List1.Count-1 do begin
    j := 0;
    row := -1;
    while (j < Regions.Count) do begin
      if Wanted then List2 := TARegion(Regions[j]).Wanted
      else List2 := TARegion(Regions[j]).ForSale;
      for k := 0 to List2.Count-1 do
        if List2[k].Data.Short = List1[i].Data.Short then begin
          if row = -1 then begin
            row := Grid.RowCount;
            AddItemGridItem(Grid, List1[i], clWindowText);
          end;
          if Grid.Cells[3, row] <> '' then
            Grid.Cells[3, row] := Grid.Cells[3, row] + ', ';
          Grid.Cells[3, row] := Grid.Cells[3, row] +
            TARegion(Regions[j]).Settlement + ' $' +
            IntToStr(List2[k].Cost);
          profit := List2[k].Cost - List1[i].Cost;
          if (Wanted and (profit > 0))
            or (not Wanted and (profit < 0)) then
            Grid.Cells[3, row] := Grid.Cells[3, row] + ' (' +
              IntToStr(Abs(profit)) + ')';
        end;
      Inc(j);
    end;
  end;
  Grid.Fixup;
end;

procedure TTownTradeForm.FormDestroy(Sender: TObject);
begin
  Regions.Free;
end;

procedure TTownTradeForm.DrawCell(Sender: TObject; ACol,
  ARow: Integer; var TxtRect: TRect; State: TGridDrawState);
begin
  ItemGridDrawCell(Sender, ACol, ARow, TxtRect, 1);
end;

procedure TTownTradeForm.bAddClick(Sender: TObject);
begin
  if cmTowns.ItemIndex >= 0 then begin
    Regions.Add(cmTowns.Items.Objects[cmTowns.ItemIndex]);
    cmTowns.Items.Delete(cmTowns.ItemIndex);
    cmTowns.Text := '';
    FillGrid(gWanted, CurrRegion.Wanted, False);
    FillGrid(gForSale, CurrRegion.ForSale, True);
  end;
end;

end.
