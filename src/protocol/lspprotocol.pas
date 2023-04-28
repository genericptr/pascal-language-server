{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lspprotocol;

{$warn 5023 off : no warning about unused units}
interface

uses
  codeAction, codeUtils, commands, diagnostics, documentHighlight, 
  documentSymbol, executeCommand, gotoDeclaration, gotoDefinition, 
  gotoImplementation, hover, inlayHint, memUtils, basic, capabilities, 
  completion, general, lsp, options, references, settings, signatureHelp, 
  symbols, synchronization, window, workDoneProgress, workspace, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('lspprotocol', @Register);
end.
