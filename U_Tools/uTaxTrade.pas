unit uTaxTrade;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, PowerGrid, ComCtrls, DataStructs, uGameSubs,
  Math, Resources;

type
  TTaxTradeForm = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Button1: TButton;
    gTax: TPowerGrid;
    lTax: TLabel;
    gTrade: TPowerGrid;
    lTrade: TLabel;
    tsFishing: TTabSheet;
    tsRoadBuild: TTabSheet;
    gFishing: TPowerGrid;
    lFishing: TLabel;
    gRoadBuild: TPowerGrid;
    lRoadBuild: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure GridDblClick(Sender: TObject);
    procedure gTaxDrawCell(Sender: TObject; ACol, ARow: Integer;
      var TxtRect: TRect; State: TGridDrawState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TaxTradeForm: TTaxTradeForm;

implementation

{$R *.lfm}

uses Main;

procedure TTaxTradeForm.FormCreate(Sender: TObject);
var i, tax, trade, fish, road, tax_income: integer;
    R, RealR: TRegion;
    md: boolean;
begin
  tax_income := GameConfig.ReadInteger('Settings', 'TaxIncome', 50);

  // MagicDeep additions
  md := (GameConfig.ReadInteger('Settings', 'Mod', modStandard) = modMagicDeep);
  tsFishing.TabVisible := md;
  tsRoadBuild.TabVisible := md;

  // Setup grids
  gTax.Cells[0, 0] := 'Region';
  gTax.Cells[1, 0] := 'Taxers';
  gTax.Cols[1].Format := cfNumber;
  gTax.Cells[2, 0] := 'Max';
  gTax.Cols[2].Format := cfNumber;
  gTax.Cells[3, 0] := 'Cover';
  gTax.Cols[3].Format := cfNumber;
  gTrade.Cells[0, 0] := 'Region';
  gFishing.Cells[0, 0] := 'Region';
  gRoadBuild.Cells[0, 0] := 'Region';
  // Fill grids
  tax := 0;
  trade := 0;
  fish := 0;
  road := 0;
  for i := 0 to VTurn.Regions.Count-1 do begin
    R := VTurn.Regions[i];
    RealR := Map.Region(R.Coords, Turn.Num);
    if Test(R.Marks, RM_TAX) then begin
      gTax.Cells[0, tax + 1] := MakeRegionName(R.Coords, True);
      gTax.Cells[1, tax + 1] := IntToStr(R.Activity.Taxers);
      gTax.Cells[2, tax + 1] := IntToStr(Ceil(RealR.TaxRate / tax_income));
      if RealR.TaxRate > 0 then
        gTax.Cells[3, tax + 1] := IntToStr(Round(R.Activity.Taxers /
          Ceil(RealR.TaxRate / tax_income) * 100))
      else gTax.Cells[3, tax + 1] := '0';
      gTax.Rows[tax + 1].Data := R;
      Inc(tax);
    end;
    if Test(R.Marks, RM_TRADE) then begin
      if md then begin
        if Test(R.Marks, RM_FISHING) then begin
          gFishing.Cells[0, fish + 1] := MakeRegionName(R.Coords, True);
          gFishing.Rows[fish + 1].Data := R;
          Inc(fish);
          Continue;
        end;
        if Test(R.Marks, RM_ROADBUILD) then begin
          gRoadBuild.Cells[0, road + 1] := MakeRegionName(R.Coords, True);
          gRoadBuild.Rows[road + 1].Data := R;
          Inc(road);
          Continue;
        end;
      end;
      gTrade.Cells[0, trade + 1] := MakeRegionName(R.Coords, True);
      gTrade.Rows[trade + 1].Data := R;
      Inc(trade);
    end;
  end;
  gTax.Fixup;
  gTrade.Fixup;
  gFishing.Fixup;
  gRoadBuild.Fixup;
  // Labels
  lTax.Caption := 'Used: ' + IntToStr(tax) + ' of ' +
    IntToStr(Progress[prWar, VTurn.War]) + ' (War ' + IntToStr(VTurn.War) + ')';
  lTrade.Caption := 'Used: ' + IntToStr(trade) + ' of ' +
    IntToStr(Progress[prTrade, VTurn.Trade]) + ' (Trade ' + IntToStr(VTurn.Trade) + ')';
  lFishing.Caption := 'Used: ' + IntToStr(fish) + ' of ' +
    IntToStr(Progress[prTrade, VTurn.Trade]) + ' (Trade ' + IntToStr(VTurn.Trade) + ')';
  lRoadBuild.Caption := 'Used: ' + IntToStr(road) + ' of ' +
    IntToStr(Progress[prTrade, VTurn.Trade]) + ' (Trade ' + IntToStr(VTurn.Trade) + ')';
end;

procedure TTaxTradeForm.GridDblClick(Sender: TObject);
begin
  with TPowerGrid(Sender) do
    if Row >= FixedRows then
      MainForm.HexMapGoto(TRegion(ImgRows[Row].Data).Coords);
end;

procedure TTaxTradeForm.gTaxDrawCell(Sender: TObject; ACol, ARow: Integer;
  var TxtRect: TRect; State: TGridDrawState);
begin
  with TPowerGrid(Sender) do
    if (ACol = 3) and (ARow > 0) then begin
      Canvas.TextRect(TxtRect, TxtRect.Left + 2, TxtRect.Top + 2,
        ImgCells[ACol, ARow] + '%');
      TxtRect.Left := TxtRect.Right;
    end;
end;

end.
