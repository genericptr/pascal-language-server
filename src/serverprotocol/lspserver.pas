{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lspserver;

{$warn 5023 off : no warning about unused units}
interface

uses
  PasLS.General, PasLS.References, PasLS.Diagnostics, PasLS.GotoDeclaration, 
  PasLS.GotoDefinition, PasLS.GotoImplementation, PasLS.Completion, 
  PasLS.SignatureHelp, PasLS.Synchronization, PasLS.AllCommands, 
  PasLS.CodeAction, PasLS.DocumentHighlight, PasLS.Hover, PasLS.InlayHint, 
  PasLS.Workspace, PasLS.ApplyEdit, PasLS.RemoveEmptyMethods, 
  PasLS.Command.CompleteCode, PasLS.Command.FormatCode, 
  PasLS.Command.InvertAssignment, PasLS.Command.RemoveEmptyMethods, 
  PasLS.DocumentSymbol, PasLS.Commands, PasLS.Formatter, PasLS.ExecuteCommand, 
  PasLS.CodeUtils, PasLS.InvertAssign, PasLS.LazConfig, PasLS.Parser, 
  PasLS.Symbols, PasLS.CheckInactiveRegions, PasLS.InactiveRegions, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('lspserver', @Register);
end.
