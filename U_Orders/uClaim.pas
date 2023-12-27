unit uClaim;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Resources, StdCtrls, ExtCtrls, Spin;

type
  TClaimForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    UnclaimedLabel: TLabel;
    Image1: TImage;
    Button1: TButton;
    Button2: TButton;
    SpinEdit: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure SpinEditKeyPress(Sender: TObject; var Key: Char);
  private
    function GetSilver: integer;
    procedure SetSilver(const Value: integer);
  public
    property Silver: integer read GetSilver write SetSilver;
  end;

var
  ClaimForm: TClaimForm;

implementation

{$R *.lfm}

procedure TClaimForm.FormCreate(Sender: TObject);
begin
  ResForm.IconList.Draw(Image1.Canvas, 0, 0, bmpSilver);
end;

function TClaimForm.GetSilver: integer;
begin
  Result := SpinEdit.Value;
end;

procedure TClaimForm.SetSilver(const Value: integer);
begin
  UnclaimedLabel.Caption := IntToStr(Value);
  SpinEdit.MaxValue := Value;
end;

procedure TClaimForm.SpinEditKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then ModalResult := mrOk;
end;

end.
