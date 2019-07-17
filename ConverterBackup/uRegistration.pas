unit uRegistration;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RegCode, StdCtrls, Resources;

type
  TRegForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    NameEdit: TEdit;
    CodeEdit: TEdit;
    Button1: TButton;
    Button2: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  RegForm: TRegForm;

implementation

{$R *.DFM}

procedure TRegForm.Button2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TRegForm.Button1Click(Sender: TObject);
begin
  if CheckCode(NameEdit.Text, CodeEdit.Text) then begin
    Config.WriteString('Prog', 'RegName', NameEdit.Text);
    Config.WriteString('Prog', 'RegCode', CodeString(NameEdit.Text, 11));
    ProgOpened := TRUE;
    ModalResult := mrOk;
  end
  else ModalResult := mrCancel;
end;

end.
