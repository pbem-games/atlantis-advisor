unit uAbout;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Resources;

type
  TAboutForm = class(TForm)
    Image1: TImage;
    VerLabel: TLabel;
    Label2: TLabel;
    Image2: TImage;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure Image1Click(Sender: TObject);
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

procedure TAboutForm.Label1Click(Sender: TObject);
begin
   OpenDocument(PChar(TLabel(Sender).Caption)); { *Converted from ShellExecute* }
end;

end.
