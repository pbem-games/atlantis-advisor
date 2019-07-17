unit uMail;

interface

uses
  SysUtils, Windows;

  function SendEMail(Sender, Recipient, Subject, Text: string; Dialog: boolean): string;

implementation

uses Mapi;

function SendEMail(Sender, Recipient, Subject, Text: string; Dialog: boolean): string;
var Msg: TMapiMessage;
    pSender, pRecip: PMapiRecipDesc;
    HResult: longword;
begin
  GetMem(pSender , SizeOf(TMapiRecipDesc));
  with pSender^ do begin
    lpszName := '';
    lpszAddress := PChar(Sender);
    ulEIDSize := 0;
    lpEntryID := nil;
    ulReserved := 0;
    ulRecipClass := MAPI_ORIG;
  end;

  GetMem(pRecip , SizeOf(TMapiRecipDesc));
  with pRecip^ do begin
    lpszName := '';
    lpszAddress := PChar(Recipient);
    ulEIDSize := 0;
    lpEntryID := nil;
    ulReserved := 0;
    ulRecipClass := MAPI_TO;
  end;

  with Msg do begin
    ulReserved := 0;
    lpszSubject := PChar(Subject);
    lpszNoteText := PChar(Text);
    lpOriginator := pSender;
    nRecipCount := 1;
    lpRecips := pRecip;
    lpszMessageType := nil;
    lpszDateReceived := nil;
    lpszConversationID := '';
    flFlags := 0;
    nFileCount := 0;
    lpFiles := nil;
  end;

  if Dialog then HResult := MapiSendMail(0, 0, Msg, MAPI_DIALOG, 0)
  else HResult := MapiSendMail(0, 0, Msg, 0, 0);

  case HResult of
    SUCCESS_SUCCESS: Result := '';
    MAPI_E_LOGIN_FAILURE: Result := 'There is no default logon in your ' +
      'mail client. No message was sent.';
    MAPI_E_USER_ABORT: Result := 'The user canceled one of the dialog boxes. ' +
      'No message was sent.';
    else Result := 'Unhandled MAPI error.';
  end;

  FreeMem(pSender);
  FreeMem(pRecip);
end;

end.
