unit uTradeStructs;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DataStructs, Resources, uGameSubs, StdCtrls, Grids, PowerGrid,
  uInterface, Math, IntEdit;

type
  TTradeStructForm = class(TForm)
    gProducts: TPowerGrid;
    Label1: TLabel;
    gStructs: TPowerGrid;
    Label2: TLabel;
    Button1: TButton;
    Label3: TLabel;
    eLevel: TIntEdit;
    procedure FormCreate(Sender: TObject);
    procedure gProductsDrawCell(Sender: TObject; ACol, ARow: Integer;
      var TxtRect: TRect; State: TGridDrawState);
    procedure gProductsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure gStructsDrawCell(Sender: TObject; ACol, ARow: Integer;
      var TxtRect: TRect; State: TGridDrawState);
    procedure FormDestroy(Sender: TObject);
    procedure FillChart(ARow: integer);
    procedure eLevelChange(Sender: TObject);
  private
    Entertainment: TItem;
  public
    { Public declarations }
  end;

var
  TradeStructForm: TTradeStructForm;

implementation

{$R *.dfm}

procedure TTradeStructForm.FormCreate(Sender: TObject);
var i, j, row: integer;
    R: TRegion;
begin
  gStructs.Cells[0, 0] := 'Amt';
  gStructs.Cells[1, 0] := 'Buildings';
  gStructs.Cells[2, 0] := 'Output';
  gStructs.Cells[3, 0] := 'Men';

  Entertainment := TItem.Create;
  if CurrRegion <> nil then begin
    Caption := Caption + ' - ' + MakeRegionName(CurrRegion.Coords, True);
    row := 0;
    gProducts.RowCount := 0;
    R := Map.Region(CurrRegion.Coords, Turn.Num);
    for i := 0 to R.Products.Count-1 do
      if R.Products[i].Amount > 0 then begin
        gProducts.Cells[0, row] := IntToStr(R.Products[i].Amount);
        gProducts.Cells[1, row] := R.Products[i].Name;
        gProducts.Rows[row].Data := R.Products[i];
        j := R.Structs.Count-1;
        while (j >= 0) and ((R.Structs[j].Data.Resource = nil)
          or (R.Structs[j].Data.Resource.Short <>
          R.Products[i].Data.Short)) do Dec(j);
        if j >= 0 then gProducts.Rows[row].FontStyle := [fsBold];
        Inc(row);
      end;

    if (R <> nil) and (R.Entertainment > 0) then begin
      if SilverData <> nil then begin
        Entertainment.Data := SilverData;
        Entertainment.Amount := R.Entertainment;
        gProducts.Cells[0, row] := IntToStr(R.Entertainment);
        gProducts.Cells[1, row] := 'entertainment';
        gProducts.Rows[row].Data := Entertainment;
        j := R.Structs.Count-1;
        while (j >= 0) and ((R.Structs[j].Data.Resource = nil)
          or (R.Structs[j].Data.Resource.Short <> SilverData.Short)) do
           Dec(j);
        if j >= 0 then gProducts.Rows[row].FontStyle := [fsBold];
      end;
    end;
    gProducts.Fixup;
  end
  else Caption := Caption + ' - Unexplored region';
end;

procedure TTradeStructForm.gProductsDrawCell(Sender: TObject; ACol,
  ARow: Integer; var TxtRect: TRect; State: TGridDrawState);
begin
  with Sender as TPowerGrid do
    if ACol = 1 then begin
      DrawItemIcon(Canvas, TxtRect.Left+1, TxtRect.Top,
        TItem(ImgRows[ARow].Data).Data);
      TxtRect.Left := TxtRect.Left + 18;
    end;
end;

procedure TTradeStructForm.gProductsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  FillChart(ARow);
end;

procedure TTradeStructForm.FillChart(ARow: Integer);
var Res: TItem;
    St: TStructData;
    i, j, amt, d, amt1, k1, base, ent_income: integer;
begin
  ent_income := GameConfig.ReadInteger('Settings', 'EntertainIncome', 20);

  if ARow >= gProducts.RowCount then Exit;
  Res := TItem(gProducts.Rows[ARow].Data);
  gStructs.RowCount := 1;
  amt := 0;
  St := nil;
  // Find trade structure that aids this resource
  i := CurrRegion.Structs.Count-1;
  while (i >= 0) and ((CurrRegion.Structs[i].Data.Resource = nil)
    or (CurrRegion.Structs[i].Data.Resource.Short <> Res.Data.Short)) do Dec(i);
  if i >= 0 then begin
    St := CurrRegion.Structs[i].Data;
    for j := 0 to CurrRegion.Structs.Count-1 do
      if CurrRegion.Structs[j].Data = St then Inc(amt);
  end
  else begin
    i := Game.StructData.Count-1;
    while (i >= 0) and ((Game.StructData[i].Resource = nil)
      or (Game.StructData[i].Resource.Short <> Res.Data.Short)) do Dec(i);
    if i >= 0 then St := Game.StructData[i];
  end;
  if St <> nil then begin
    // Count base amount of resource in region
    base := Res.Amount;
    for i := amt downto 1 do begin
      k1 := 2;
      for j := 1 to i do k1 := k1 * 2 + 1;
      base := base - Floor(base / k1);
    end;
    // Fill grid
    i := 0;
    amt1 := base;
    repeat
      if i = 0 then
        gStructs.Cells[1, i + 1] := 'no buildings'
      else begin
        gStructs.Cells[0, i + 1] := IntToStr(i);
        gStructs.Cells[1, i + 1] := St.Group;
        gStructs.Rows[i + 1].Data := St;
      end;
      gStructs.Cells[2, i + 1] := IntToStr(amt1);
      if Test(Res.Data.Flags, IT_SILVER) then
        gStructs.Cells[3, i + 1] := IntToStr(Ceil(amt1 / eLevel.Value / ent_income))
      else gStructs.Cells[3, i + 1] := IntToStr(Ceil(amt1 / eLevel.Value));
      if i = amt then gStructs.Rows[i + 1].FontStyle := [fsBold];
      Inc(i);
      d := Floor(base / Power(2, i+1));
      amt1 := amt1 + d;
    until d < 1;
  end
  else begin
    gStructs.Cells[1, 1] := 'no buildings';
    gStructs.Cells[2, 1] := IntToStr(Res.Amount);
    gStructs.Cells[3, 1] := IntToStr(Ceil(Res.Amount / eLevel.Value));
    gStructs.Rows[1].FontStyle := [fsBold];
  end;
  gStructs.Fixup;
end;

procedure TTradeStructForm.gStructsDrawCell(Sender: TObject; ACol,
  ARow: Integer; var TxtRect: TRect; State: TGridDrawState);
begin
  with gStructs do begin
    if (ACol = 1) and (ImgRows[ARow].Data <> nil) then begin
      DrawStructIcon(Canvas, TxtRect.Left, TxtRect.Top,
        TStructData(ImgRows[ARow].Data), False);
      TxtRect.Left := TxtRect.Left + 18;
    end;
  end;
end;

procedure TTradeStructForm.FormDestroy(Sender: TObject);
begin
  Entertainment.Free;
end;

procedure TTradeStructForm.eLevelChange(Sender: TObject);
begin
  FillChart(gProducts.Row);
end;

end.
