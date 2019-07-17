unit uBuy;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, IntEdit, uGameSubs, DataStructs, Resources, Math,
  uInterface;

type
  TBuyForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    TradeBox: TGroupBox;
    GoodsLabel: TLabel;
    GoodsImage: TImage;
    CostLabel: TLabel;
    Image1: TImage;
    HavingLabel: TLabel;
    HavingCountLabel: TLabel;
    FundsImage: TImage;
    FundsLabel: TLabel;
    FundsWantedLabel: TLabel;
    AmountEdit: TIntEdit;
    GroupBox1: TGroupBox;
    cbClaim: TCheckBox;
    cbTake: TCheckBox;
    cmTakeUnit: TComboBox;
    cbAll: TCheckBox;
    procedure AmountEditChange(Sender: TObject);
    procedure ExtraChange(Sender: TObject);
    procedure cbAllClick(Sender: TObject);
  private
    AUnit: TUnit;
    function GetAmount: integer;
    procedure SetAmount(const Value: integer);
  public
    Cost: integer;
    function TotalCost: integer;
    property Amount: integer read GetAmount write SetAmount;
    procedure Setup(AUnit: TUnit; SaleItem: TItem);
  end;

var
  BuyForm: TBuyForm;

implementation

{$R *.DFM}

procedure TBuyForm.Setup(AUnit: TUnit; SaleItem: TItem);
begin
  Self.AUnit := AUnit;
  Caption := 'Buy ' + SaleItem.Data.Name(True);
  // Top line
  if SaleItem.Amount = -1 then
    HavingCountLabel.Caption := 'unlimited ' + SaleItem.Data.Name(True)
  else
    HavingCountLabel.Caption := IntToStr(SaleItem.Amount) + ' ' +
      SaleItem.Data.Name(True);
  FundsLabel.Caption := IntToStr(AUnit.Items.Amount(IT_SILVER) +
    AUnit.TradeIncome);
  // Goods
  GoodsLabel.Caption := SaleItem.Data.MultiName;
  GoodsImage.Canvas.Brush.Color := clBtnFace;
  GoodsImage.Canvas.FillRect(GoodsImage.Canvas.ClipRect);
  MakeItemBmp(GoodsImage.Picture.Bitmap, SaleItem.Data);
  Cost := SaleItem.Cost;
  Amount := Min(SaleItem.Amount, (AUnit.Items.Amount(IT_SILVER) +
    AUnit.TradeIncome) div Cost);
  FillRegionUnits(cmTakeUnit, AUnit.Region, AUnit, True, True);
end;

function TBuyForm.TotalCost: integer;
begin
  Result := AmountEdit.Value * Cost;
end;

procedure TBuyForm.AmountEditChange(Sender: TObject);
begin
  CostLabel.Caption := IntToStr(TotalCost);
end;

function TBuyForm.GetAmount: integer;
begin
  Result := AmountEdit.Value;
end;

procedure TBuyForm.SetAmount(const Value: integer);
begin
  AmountEdit.Value := Value;
  CostLabel.Caption := IntToStr(TotalCost);
end;

procedure TBuyForm.ExtraChange(Sender: TObject);
var funds: integer;
    Src: TUnit;
begin
  funds := AUnit.Items.Amount(IT_SILVER) + AUnit.TradeIncome;
  if cbClaim.Checked then Inc(funds, Game.VirtTurn.Unclaimed);
  if cbTake.Checked and (cmTakeUnit.ItemIndex > -1) then begin
    Src := TUnit(cmTakeUnit.Items.Objects[cmTakeUnit.ItemIndex]);
    Inc(funds, Src.Items.Amount(IT_SILVER));
  end;
  FundsLabel.Caption := IntToStr(Funds);
end;

procedure TBuyForm.cbAllClick(Sender: TObject);
begin
  AmountEdit.Enabled := not cbAll.Checked;
end;

end.
