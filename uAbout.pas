unit uAbout;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Resources, ShellAPI;

type
  TAboutForm = class(TForm)
    Image1: TImage;
    VerLabel: TLabel;
    RegLabel: TLabel;
    Label2: TLabel;
    Image2: TImage;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure Image1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

procedure TAboutForm.Image1Click(Sender: TObject);
begin
  Close;
end;

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  if ProgOpened then RegLabel.Caption := 'Registered to: ' +
    Config.ReadString('Prog', 'RegName', '')
  else RegLabel.Caption := 'Unregistered';
end;

procedure TAboutForm.Label1Click(Sender: TObject);
begin
  ShellExecute(Application.Handle, PChar('open'),
    PChar(TLabel(Sender).Caption), PChar(0), nil, SW_NORMAL);
end;

end.
