
unit diagnostics;

{$mode objfpc}{$H+}

interface

uses
  Classes, 
  CodeToolManager, CodeCache,
  lsp, basic;

type

  { TPublishDiagnosticsParams }

  TPublishDiagnosticsParams = class(TPersistent)
  private
    fUri: TDocumentUri;
    fVersion: integer;
    fDiagnostics: TDiagnosticItems;
  published
    // The URI for which diagnostic information is reported.
    property uri: TDocumentUri read fUri write fUri;

    // The version number of the document the diagnostics are published for.
    // todo: this must be optional
    //property version: integer read fVersion write fVersion;

    // An array of diagnostic information items.
    property diagnostics: TDiagnosticItems read fDiagnostics write fDiagnostics;
  public
    procedure AfterConstruction; override;
  end;

  { TPublishDiagnostics }

  TPublishDiagnostics = class(TNotificationMessage)
  private
    fParams: TPublishDiagnosticsParams;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(fileName, message: string; line, column, code: integer; severity: TDiagnosticSeverity);
    procedure Clear(fileName: string);
  end;

{ Code Tools Error Handling }

procedure CheckSyntax(Code: TCodeBuffer);
procedure PublishCodeToolsError;

implementation
uses
  SysUtils;

{ Publish the last code tools error as a diagnostics }
procedure PublishCodeToolsError;
var
  notification: TPublishDiagnostics;
begin
  //writeln(stderr, 'Syntax Error -> '+CodeToolBoss.ErrorCode.FileName+': "'+CodeToolBoss.ErrorMessage+'" @ '+IntToStr(CodeToolBoss.ErrorLine)+':'+IntToStr(CodeToolBoss.ErrorColumn)+' ID=',CodeToolBoss.ErrorID);
  //flush(stderr);

  notification := TPublishDiagnostics.Create;
  notification.Add(CodeToolBoss.ErrorCode.FileName, 
                   CodeToolBoss.ErrorMessage, 
                   CodeToolBoss.ErrorLine - 1, 
                   CodeToolBoss.ErrorColumn - 1, 
                   // TODO: code tools error ID is too large (int64), what should we do?
                   1{CodeToolBoss.ErrorID},
                   TDiagnosticSeverity.Error);
  notification.Send;
  notification.Free;
end;

{ Checks syntax for code buffer and publishes errors as diagnostics }

procedure CheckSyntax(Code: TCodeBuffer); 
var
  Tool: TCodeTool;
  notification: TPublishDiagnostics;
begin
  if not CodeToolBoss.Explore(Code,Tool,true) then
    PublishCodeToolsError
  else
    begin
      // todo: when we have a document store we can check to see
      // if we actually have any errors.
      notification := TPublishDiagnostics.Create;
      notification.Clear(Code.FileName);
      notification.Send;
      notification.Free;
    end;
end;

{ TPublishDiagnostics }

procedure TPublishDiagnostics.Clear(fileName: string);
begin
  fParams.uri := PathToURI(fileName);
  fParams.diagnostics.Clear;
end;

procedure TPublishDiagnostics.Add(fileName, message: string; line, column, code: integer; severity: TDiagnosticSeverity);
var
  diagnostic: TDiagnostic;
begin
  fParams.uri := PathToURI(fileName);

  diagnostic := TDiagnostic(fParams.diagnostics.Add);
  diagnostic.range := TRange.Create(line, column);
  diagnostic.severity := severity;
  diagnostic.code := code;
  diagnostic.source := 'Free Pascal Compiler';
  diagnostic.message := message;
end;

constructor TPublishDiagnostics.Create;
begin  
  fParams := TPublishDiagnosticsParams.Create;
  params := fParams;
  method := 'textDocument/publishDiagnostics';
end;

destructor TPublishDiagnostics.Destroy; 
begin
  fParams.Free;
  inherited;
end;

{ TPublishDiagnosticsParams }

procedure TPublishDiagnosticsParams.AfterConstruction;
begin
  inherited;
  diagnostics := TDiagnosticItems.Create;
end;

end.