
unit settings;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$scopedenums on}

interface

type
  TServerOption = (
      InsertCompletionsAsSnippets,        // Procedure completions with parameters are inserted as snippets
      InsertCompletionProcedureBrackets,  // Procedure completions with parameters (non-snippet) insert
                                          // empty brackets (and insert as snippet)
      IncludeWorkspaceFoldersAsUnitPaths,
      IncludeWorkspaceFoldersAsIncludePaths,
      CheckSyntax,                        // syntax will be checked when file opens or saves
      PublishDiagnostics                  // syntax errors will be published as diagnostics
    );
  TServerOptions = set of TServerOption;
  
  TServerSettings = record
    MainProgramFile: String;
    Options: TServerOptions;
  end;

var
  ServerSettings: TServerSettings;

implementation

end.