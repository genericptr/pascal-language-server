unit PasLS.ApplyEdit;

{$mode objfpc}{$H+}

interface

uses
  { RTL }
  Classes, SysUtils,
  { LSP }
  LSP.Messages, LSP.Basic, LSP.Base;

procedure DoApplyEdit(aTransport: TMessageTransport; DocumentURI, Text: String; Range: TRange);

implementation

Uses
  { LSP }
  PasLS.Settings,
  LSP.BaseTypes,
  LSP.WorkSpace, PasLS.WorkSpace;

procedure DoApplyEdit(aTransport: TMessageTransport; DocumentURI, Text: String; Range: TRange);
var
  Params: TApplyWorkspaceEditParams;
  Edit: TWorkspaceEdit;
  TextEdit: TTextEdit;
  Msg: TWorkspaceApplyEditRequest;
  TextDocumentEdit: TTextDocumentEdit;
begin
  Msg := nil;
  Params := TApplyWorkspaceEditParams.Create;
  try
    Edit := Params.edit;

    TextDocumentEdit := Edit.documentChanges.Add;
    TextDocumentEdit.textDocument.uri := DocumentURI;

    // TODO: we're hacking around clients by using the versioning system they allow
    // but ideally you're supposed to provided correct versions.
    // See `OptionalVersionedTextDocumentIdentifier` from
    // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#versionedTextDocumentIdentifier
    if ClientInfo.name = TClients.SublimeTextLSP then
      TextDocumentEdit.textDocument.version := nil
    else
      TextDocumentEdit.textDocument.version := 0;

    TextEdit := TextDocumentEdit.edits.Add;
    TextEdit.range := range;
    TextEdit.newText := Text;

    Msg := TWorkspaceApplyEditRequest.Create(aTransport);
    Msg.Execute(params, 'workspace/applyEdit');  // TODO: the class should know it's method name
  finally
    Params.Free;
    Msg.Free;
  end;
end;


end.

