unit PasLS.Parser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PParser, PScanner, PasTree, CodeCache, Types;

// Detect presence of OnError etc.
{$IF Declared(TPasParserErrorHandler)}
{$DEFINE MULTIERROR}
{$ENDIF}

Type

 { TCodeToolsFileResolver }

  TCodeToolsFileResolver = Class(TFileResolver)
  Private
    FBuffer: TCodeBuffer;
  Protected
    function CreateLineReaderFromBuffer(aBuffer: TCodeBuffer): TLineReader;
  Public
    Constructor Create(aBuffer : TCodeBuffer); reintroduce;
    function FindSourceFile(const AName: string): TLineReader; override;
    Property Buffer : TCodeBuffer read FBuffer;
  end;

  { TSimpleEngine }

  TSimpleEngine = class(TPasTreeContainer)
  public
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
      override;
    function FindElement(const AName: String): TPasElement; override;
  end;

  { TSourceParser }
  TReportParserErrorEvent = Procedure (Sender : TObject; Const aError, aFileName : string; aCode, aLine, aCol : Integer) Of Object;
  TSourceParser = Class(TObject)
  private
    FOnError: TReportParserErrorEvent;
    FParser: TPasParser;
    FCode: TCodeBuffer;
    FCommandLine: TStringDynArray;
    FCPUTarget: String;
    FOptions: TParseSourceOptions;
    FOSTarget: String;
{$IFDEF MULTIERROR}
    procedure DoError(Sender: TObject; const aContext: TRecoveryContext;
      var aAllowRecovery: Boolean);
{$ENDIF}
  Public
    Function ParseSource: TPasModule;
    Property CommandLine : TStringDynArray Read FCommandLine Write FCommandLine;
    Property Code : TCodeBuffer Read FCode Write FCode;
    Property OSTarget : String Read FOSTarget Write FOSTarget;
    Property CPUTarget : String Read FCPUTarget Write FCPUTarget;
    Property Options : TParseSourceOptions Read FOptions Write FOptions;
    Property OnError : TReportParserErrorEvent Read FOnError Write FOnError;
  end;

implementation

uses CodeToolManager;

{ Checks syntax for code buffer and publishes errors as diagnostics }

{ TSimpleEngine }

function TSimpleEngine.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
begin
  Result := AClass.Create(AName, AParent);
  Result.Visibility := AVisibility;
  Result.SourceFilename := ASourceFilename;
  Result.SourceLinenumber := ASourceLinenumber;
end;

function TSimpleEngine.FindElement(const AName: String): TPasElement;

begin
  // We could try to use codetools for this.
  Result := nil;
end;

{ TSourceParser }

{$IFDEF MULTIERROR}
procedure TSourceParser.DoError(Sender: TObject;
  const aContext: TRecoveryContext; var aAllowRecovery: Boolean);

var
  aCode : Integer;

begin
  aAllowRecovery:=True;
  If Assigned(FOnError) then
    begin
    if aContext.Error is EParserError then
      aCode:=EParserError(aContext.Error).ErrNo
    else
      aCode:=-1;
    With FParser.CurSourcePos do
      FOnError(Self,aContext.Error.Message,FileName,aCode, Row,Column);
    end;
end;
{$ENDIF}
function TSourceParser.ParseSource: TPasModule;

var
  FileResolver: TBaseFileResolver;
  Filename: String;
  Scanner: TPascalScanner;

  procedure ProcessCmdLinePart(S : String);
  var
    l,Len: Integer;

  begin
    if (S='') then
      exit;
    Len:=Length(S);
    if (s[1] = '-') and (len>1) then
    begin
      case s[2] of
        'd': // -d define
          Scanner.AddDefine(UpperCase(Copy(s, 3, Len)));
        'u': // -u undefine
          Scanner.RemoveDefine(UpperCase(Copy(s, 3, Len)));
        'F': // -F
          if (len>2) and (s[3] = 'i') then // -Fi include path
            FileResolver.AddIncludePath(Copy(s, 4, Len));
        'I': // -I include path
          FileResolver.AddIncludePath(Copy(s, 3, Len));
        'S': // -S mode
          if  (len>2) then
            begin
            l:=3;
            While L<=Len do
              begin
              case S[l] of
                'c' : Scanner.Options:=Scanner.Options+[po_cassignments];
                'd' : Scanner.SetCompilerMode('DELPHI');
                '2' : Scanner.SetCompilerMode('OBJFPC');
                'h' : ; // do nothing
              end;
              inc(l);
              end;
            end;
        'M' :
           begin
           delete(S,1,2);
           Scanner.SetCompilerMode(S);
           end;
      end;
    end else
      if Filename <> '' then
        raise ENotSupportedException.Create(SErrMultipleSourceFiles)
      else
        Filename := s;
  end;

var
  S: String;
  Engine : TSimpleEngine;
  aCode : integer;

begin
  if DefaultFileResolverClass=Nil then
    raise ENotImplemented.Create(SErrFileSystemNotSupported);
  Result := nil;
  FileResolver := nil;
  Scanner := nil;
  FParser := nil;
  Engine:=Nil;
  try
    Engine:=TSimpleEngine.Create;
    FileResolver := TCodeToolsFileResolver.Create(Code);
    {$ifdef HasStreams}
    if FileResolver is TFileResolver then
      TFileResolver(FileResolver).UseStreams:=poUseStreams in Options;
    {$endif}
    Scanner := TPascalScanner.Create(FileResolver);
    Scanner.LogEvents:=Engine.ScannerLogEvents;
    Scanner.OnLog:=Engine.Onlog;
    if not (poSkipDefaultDefs in Options) then
      begin
      Scanner.AddDefine('FPK');
      Scanner.AddDefine('FPC');
      // TargetOS
      s := UpperCase(OSTarget);
      Scanner.AddDefine(s);
      Case s of
        'LINUX' : Scanner.AddDefine('UNIX');
        'DARWIN' :
          begin
          Scanner.AddDefine('DARWIN');
          Scanner.AddDefine('UNIX');
          end;
        'FREEBSD' :
          begin
          Scanner.AddDefine('BSD');
          Scanner.AddDefine('UNIX');
          end;
        'NETBSD' :
          begin
          Scanner.AddDefine('BSD');
          Scanner.AddDefine('UNIX');
          end;
        'SUNOS' :
          begin
          Scanner.AddDefine('SOLARIS');
          Scanner.AddDefine('UNIX');
          end;
        'GO32V2' : Scanner.AddDefine('DPMI');
        'BEOS' : Scanner.AddDefine('UNIX');
        'QNX' : Scanner.AddDefine('UNIX');
        'AROS' : Scanner.AddDefine('HASAMIGA');
        'MORPHOS' : Scanner.AddDefine('HASAMIGA');
        'AMIGA' : Scanner.AddDefine('HASAMIGA');
      end;
      // TargetCPU
      s := UpperCase(CPUTarget);
      Scanner.AddDefine('CPU'+s);
      if (s='X86_64') then
        Scanner.AddDefine('CPU64')
      else
        Scanner.AddDefine('CPU32');
      end;
    FParser := TPasParser.Create(Scanner, FileResolver, Engine);
    if (poSkipDefaultDefs in Options) then
      FParser.ImplicitUses.Clear;
    Filename := '';
    FParser.LogEvents:=Engine.ParserLogEvents;
    FParser.OnLog:=Engine.Onlog;
{$IFDEF MULTIERROR}
    FParser.MaxErrorCount:=50;
    FParser.OnError:=@DoError;
{$ENDIF}
    For S in CommandLine do
      ProcessCmdLinePart(S);
    if Filename = '' then
      raise Exception.Create(SErrNoSourceGiven);
    FileResolver.AddIncludePath(ExtractFilePath(FileName));
    Scanner.OpenFile(Code.FileName);
    try
      FParser.ParseMain(Result);
    except
      on E : Exception do
        begin
        FreeAndNil(Result);
        if Assigned(FOnError) then
          With FParser.CurSourcePos do
             begin
             aCode:=-1;
             {$IFDEF MULTIERROR}
             if E is EParserError then
               aCode:=EParserError(E).ErrNo;
             {$ENDIF}
             FOnError(Self,E.Message,FileName,aCode,Row,Column);
             end;
        end;
    end;
  finally
    FreeAndNil(FParser);
    Scanner.Free;
    FileResolver.Free;
  end;
end;

{ TCodeToolsFileResolver }

constructor TCodeToolsFileResolver.Create(aBuffer: TCodeBuffer);
begin
  Inherited Create;
  FBuffer:=aBuffer;
end;

function TCodeToolsFileResolver.CreateLineReaderFromBuffer(aBuffer : TCodeBuffer) : TLineReader;

Var
  SLR : TStreamLineReader;

begin
  SLR:=TStreamLineReader.Create(aBuffer.FileName);
  SLR.InitFromString(aBuffer.Source);
  Result:=SLR;
end;

function TCodeToolsFileResolver.FindSourceFile(const AName: string
  ): TLineReader;
begin
  if SameFileName(aName,FBuffer.Filename) then
    Result:=CreateLineReaderFromBuffer(FBuffer)
  else
    Result:=inherited FindSourceFile(AName);
end;


end.

