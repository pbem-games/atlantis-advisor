unit uSelAmount;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Spin;

type
  TSelAmountForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    TradeBox: TGroupBox;
    GoodsLabel: TLabel;
    GoodsImage: TImage;
    CostLabel: TLabel;
    Image1: TImage;
    HavingLabel: TLabel;
    HavingCountLabel: TLabel;
    FundsWantedLabel: TLabel;
    AmountEdit: TSpinEdit;
    cbAll: TCheckBox;
    procedure AmountEditChange(Sender: TObject);
    procedure cbAllClick(Sender: TObject);
  private
    { Private declarations }
  public
    Cost: integer;
  end;

var
  SelAmountForm: TSelAmountForm;

implementation

{$R *.DFM}

procedure TSelAmountForm.AmountEditChange(Sender: TObject);
begin
  if AmountEdit.Value < 0 then
    AmountEdit.Value := 0;
  CostLabel.Caption := IntToStr(AmountEdit.Value * Cost);
end;

procedure TSelAmountForm.cbAllClick(Sender: TObject);
begin
  AmountEdit.Enabled := not cbAll.Checked;
end;

end.
