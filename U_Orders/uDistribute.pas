unit uDistribute;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, PowerGrid,
  DataStructs, Buttons;

type
  TDistributeForm = class(TForm)
    pgItemGrid: TPowerGrid;
    lblTargetLabel: TLabel;
    cbTarget: TComboBox;
    edtTarget: TEdit;
    btnOkay: TButton;
    btnCancel: TButton;
    btnSelectTarget: TBitBtn;
    btnCheckUnit: TBitBtn;
    btnClearUnit: TBitBtn;
    lblDistState: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure edtTargetKeyPress(Sender: TObject; var Key: Char);
    procedure pgItemGridDrawCell(Sender: TObject; ACol, ARow: Integer; var TxtRect: TRect; State: TGridDrawState);
    procedure pgItemGridKeyPress(Sender: TObject; var Key: Char);
    procedure btnOkayClick(Sender: TObject);
    procedure btnClearUnitClick(Sender: TObject);
    procedure btnCheckUnitClick(Sender: TObject);
    procedure edtTargetChange(Sender: TObject);
  private
    FUpdating:  integer;
  public
    Qmaster:  TUnit;

    procedure FillForm(AQmaster: TUnit); overload;
    procedure FillForm(ARegion: TRegion; ADist: integer); overload;

    function GetOrders: string;
  end;

var
  DistributeForm: TDistributeForm;

implementation

uses
  uGameSubs, uKeys, uInterface;

const
  DS_UNKNOWNLOC:  string  = 'The target''s location is unknown.';
  DS_LOCAL:       string  = 'The target is local.';
  DS_DISTANT:     string  = 'The target is distant. A fee will be charged.';

{$R *.dfm}

procedure TDistributeForm.FillForm(AQmaster: TUnit);
var
  iIdx:     integer;
  iItem:    TItem;
  iRow:     integer;
begin
  Caption := 'Distribute - ' + AQmaster.Name + ' [' + AQmaster.NumStr + ']';

  cbTarget.Visible := false;
  btnClearUnit.Visible := false;
  edtTarget.Visible := true;
  edtTarget.Text := EmptyStr;
  btnCheckUnit.Visible := true;

  lblDistState.Caption := DS_UNKNOWNLOC;

  pgItemGrid.RowCount := 1;
  for iIdx := 0 to AQmaster.Items.Count - 1 do
  begin
    iItem := AQmaster.Items[iIdx];

    iRow := pgItemGrid.RowCount;
    pgItemGrid.Cells[0, iRow] := iItem.Name;
    pgItemGrid.Cells[1, iRow] := IntToStr(iItem.Amount);
    pgItemGrid.Rows[iRow].Data := iItem;
  end;
  pgItemGrid.Fixup;

  Qmaster := AQmaster;
end;

procedure TDistributeForm.FillForm(ARegion: TRegion; ADist: integer);
var
  iTroop:   integer;
  trTroop:  TTroop;
  iUnit:    integer;
  uUnit:    TUnit;
begin
  cbTarget.Items.Clear;

  if ADist > 2 then
  begin
    for iTroop := 0 to ARegion.Troops.Count - 1 do
    begin
      trTroop := ARegion.Troops[iTroop];

      for iUnit := 0 to trTroop.Units.Count - 1 do
      begin
        uUnit := trTroop.Units[iUnit];

        if SkillLevel(uUnit, Keys[s_Quartermaster]) > 0 then
          cbTarget.Items.AddObject(uUnit.Name + ' [' + uUnit.NumStr + ']', uUnit);
      end;
    end;
  end
  else
  begin
    for iTroop := 0 to ARegion.Troops.Count - 1 do
    begin
      trTroop := ARegion.Troops[iTroop];

      for iUnit := 0 to trTroop.Units.Count - 1 do
      begin
        uUnit := trTroop.Units[iUnit];
        cbTarget.Items.AddObject(uUnit.Name + ' [' + uUnit.NumStr + ']', uUnit);
      end;
    end;
  end;

  if cbTarget.Items.Count > 0 then
  begin
    cbTarget.Visible := true;
    cbTarget.ItemIndex := 0;
    btnClearUnit.Visible := true;
    edtTarget.Visible := false;
    btnCheckUnit.Visible := false;

    if ADist <= 2 then
      lblDistState.Caption := DS_LOCAL
    else
      lblDistState.Caption := DS_DISTANT;
  end;
end;

function TDistributeForm.GetOrders: string;
begin
  Result := '';
end;

procedure TDistributeForm.FormCreate(Sender: TObject);
begin
  pgItemGrid.Cols[0].Width := 170;
  pgItemGrid.Cells[0, 0] := 'Item';
  pgItemGrid.Cols[1].Width := 60;
  pgItemGrid.Cols[1].Format := cfNumber;
  pgItemGrid.Cells[1, 0] := 'Owned';
  pgItemGrid.Cols[2].Width := 60;
  pgItemGrid.Cols[2].Format := cfNumber;
  pgItemGrid.Cols[2].AutoEdit := true;
  pgItemGrid.Cells[2, 0] := 'Send';

  FUpdating := 0;
end;

procedure TDistributeForm.FormActivate(Sender: TObject);
begin
  if edtTarget.Visible then
  begin
    edtTarget.SetFocus;
    edtTarget.SelectAll;
  end
  else
    cbTarget.SetFocus;
end;

procedure TDistributeForm.edtTargetKeyPress(Sender: TObject; var Key: Char);
const
  Allowed: set of Char = [#8, '0'..'9'];
begin
  if not (Key in Allowed) then
    Key := #0;
end;

procedure TDistributeForm.pgItemGridDrawCell(Sender: TObject; ACol, ARow: Integer; var TxtRect: TRect; State: TGridDrawState);
begin
  if ARow > 0 then
  begin
    case ACol of
    0:  ItemGridDrawCell(Sender, ACol, ARow, TxtRect, 0);
    end;
  end;
end;

procedure TDistributeForm.pgItemGridKeyPress(Sender: TObject; var Key: Char);
const
  Allowed: set of Char = [#8, '0'..'9'];
begin
  if not (Key in Allowed) then
    Key := #0;
end;

procedure TDistributeForm.btnOkayClick(Sender: TObject);
var
  iRow:     integer;
  sSendTxt: string;
  iSendVal: integer;
  iItem:    TItem;
begin
  btnOkay.SetFocus;

  if edtTarget.Visible and (edtTarget.Text = EmptyStr) then
  begin
    MessageDlg('Distribution target not set!', mtError, [mbOK], 0);

    edtTarget.SetFocus;
    edtTarget.SelectAll;
    exit;
  end;

  for iRow := 1 to pgItemGrid.RowCount - 1 do
  begin
    sSendTxt := pgItemGrid.Cells[2, iRow];
    if sSendTxt <> EmptyStr then
    begin
      iSendVal := StrToIntDef(sSendTxt, 0);
      iItem := TItem(pgItemGrid.Rows[iRow].Data);

      if iSendVal > iItem.Amount then
      begin
        MessageDlg('Atempt to send more ' + iItem.Data.MultiName + ' than owned!', mtError, [mbOK], 0);

        pgItemGrid.SetFocus;
        pgItemGrid.Row := iRow;
        pgItemGrid.Col := 2;
        exit;
      end;
    end;
  end;

  ModalResult := mrOk;
end;

procedure TDistributeForm.btnClearUnitClick(Sender: TObject);
var
  uSelUnit: TUnit;
begin
  Inc(FUpdating);

  uSelUnit := TUnit(cbTarget.Items.Objects[cbTarget.ItemIndex]);

  cbTarget.Visible := false;
  btnClearUnit.Visible := false;
  edtTarget.Text := uSelUnit.NumStr;
  edtTarget.Visible := true;
  edtTarget.SetFocus;
  edtTarget.SelectAll;
  btnCheckUnit.Visible := true;

  Dec(FUpdating);
end;

procedure TDistributeForm.btnCheckUnitClick(Sender: TObject);
var
  sUnitID:  string;
  uUnit:    TUnit;
begin
  sUnitID := edtTarget.Text;
  if sUnitID = EmptyStr then
  begin
    MessageDlg('Distribution target not set!', mtError, [mbOK], 0);

    edtTarget.SetFocus;
    edtTarget.SelectAll;
    exit;
  end;

  uUnit := Turn.FindUnit(StrToInt(sUnitID));
  if uUnit = nil then
  begin
    MessageDlg('Distribution target not found!', mtWarning, [mbOK], 0);

    edtTarget.SetFocus;
    edtTarget.SelectAll;
    exit;
  end;
end;

procedure TDistributeForm.edtTargetChange(Sender: TObject);
begin
  if FUpdating > 0 then
    exit;
    
  lblDistState.Caption := DS_UNKNOWNLOC;
end;

end.
