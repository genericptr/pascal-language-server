{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lspprotocol;

{$warn 5023 off : no warning about unused units}
interface

uses
  LSP.CodeAction, LSP.Diagnostics, LSP.DocumentHighlight, LSP.DocumentSymbol, 
  LSP.ExecuteCommand, LSP.GotoDeclaration, LSP.GotoDefinition, 
  LSP.GotoImplementation, LSP.Hover, LSP.InlayHint, MemUtils, LSP.Basic, 
  LSP.Capabilities, LSP.Completion, LSP.General, LSP.Base, LSP.Options, 
  LSP.References, PasLS.Settings, LSP.SignatureHelp, PasLS.Symbols, 
  LSP.Synchronization, LSP.Window, LSP.WorkDoneProgress, LSP.Workspace, 
  PasLS.CodeUtils, PasLS.Commands, PasLS.LazConfig, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('lspprotocol', @Register);
end.
