unit uMemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, uKeys;

type
  TMemoForm = class(TForm)
    Memo: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MemoForm: TMemoForm;

implementation

{$R *.DFM}

end.
