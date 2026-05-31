unit Tests.LazConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  CodeToolManager, CodeToolsConfig, FileUtil, fpjson, LSP.Messages;

type
  TNullTransport = class(TMessageTransport)
  protected
    procedure DoSendMessage(aMessage: TJSONData); override;
    procedure DoSendDiagnostic(const aMessage: UTF8String); override;
  end;

  { TTestLazConfig }

  TTestLazConfig = class(TTestCase)
  private
    FRoot: string;
    FTransport: TNullTransport;
    function MakeDir(const Parts: array of string): string;
    function MakeFile(const Parts: array of string): string;
    procedure WriteText(const FileName, Text: string);
    procedure WritePackage(const FileName, PackageName, UnitPath: string;
      const ExtraSearchPath: string = '');
    procedure WritePackageFiles(const ConfigDir, PackageName, PackageFile: string);
    procedure WriteProject(const FileName, PackageName: string);
    function NewOptions(const LazarusDir: string): TCodeToolsOptions;
    function ConfigureProjectUnitPath(const ConfigPath, ProjectFile,
      LazarusDir: string): string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestExplicitPackageLinkOverridesGlobalLink;
    procedure TestConfigFilePathLoadsPackageFilesFromContainingDirectory;
    procedure TestPathMacroExpansionKeepsUnknownMacrosRelative;
  end;

implementation

uses
  PasLS.LazConfig;

procedure TNullTransport.DoSendMessage(aMessage: TJSONData);
begin
end;

procedure TNullTransport.DoSendDiagnostic(const aMessage: UTF8String);
begin
end;

function TTestLazConfig.MakeDir(const Parts: array of string): string;
var
  Part: string;
begin
  Result := FRoot;
  for Part in Parts do
    Result := IncludeTrailingPathDelimiter(Result) + Part;
  Result := IncludeTrailingPathDelimiter(Result);
  ForceDirectories(Result);
end;

function TTestLazConfig.MakeFile(const Parts: array of string): string;
var
  I: Integer;
begin
  Result := FRoot;
  for I := Low(Parts) to High(Parts) do
  begin
    if I = High(Parts) then
      Result := IncludeTrailingPathDelimiter(Result) + Parts[I]
    else
      Result := IncludeTrailingPathDelimiter(Result) + Parts[I];
  end;
end;

procedure TTestLazConfig.WriteText(const FileName, Text: string);
var
  Lines: TStringList;
begin
  ForceDirectories(ExtractFilePath(FileName));
  Lines := TStringList.Create;
  try
    Lines.Text := Text;
    Lines.SaveToFile(FileName);
  finally
    Lines.Free;
  end;
end;

procedure TTestLazConfig.WritePackage(const FileName, PackageName,
  UnitPath: string; const ExtraSearchPath: string);
var
  SearchPath: string;
begin
  ForceDirectories(UnitPath);
  SearchPath := UnitPath;
  if ExtraSearchPath <> '' then
    SearchPath := SearchPath + ';' + ExtraSearchPath;
  WriteText(FileName,
    '<?xml version="1.0" encoding="UTF-8"?>' + LineEnding +
    '<CONFIG>' + LineEnding +
    '  <Package Version="5">' + LineEnding +
    '    <Name Value="' + PackageName + '"/>' + LineEnding +
    '    <CompilerOptions>' + LineEnding +
    '      <SearchPaths>' + LineEnding +
    '        <UnitOutputDirectory Value="lib/$(TargetCPU)-$(TargetOS)"/>' + LineEnding +
    '        <OtherUnitFiles Value="' + SearchPath + '"/>' + LineEnding +
    '      </SearchPaths>' + LineEnding +
    '    </CompilerOptions>' + LineEnding +
    '  </Package>' + LineEnding +
    '</CONFIG>' + LineEnding);
end;

procedure TTestLazConfig.WritePackageFiles(const ConfigDir, PackageName,
  PackageFile: string);
begin
  ForceDirectories(ConfigDir);
  WriteText(IncludeTrailingPathDelimiter(ConfigDir) + 'packagefiles.xml',
    '<?xml version="1.0" encoding="UTF-8"?>' + LineEnding +
    '<CONFIG>' + LineEnding +
    '  <UserPkgLinks>' + LineEnding +
    '    <Item1>' + LineEnding +
    '      <Name Value="' + PackageName + '"/>' + LineEnding +
    '      <Filename Value="' + PackageFile + '"/>' + LineEnding +
    '    </Item1>' + LineEnding +
    '  </UserPkgLinks>' + LineEnding +
    '</CONFIG>' + LineEnding);
end;

procedure TTestLazConfig.WriteProject(const FileName, PackageName: string);
begin
  WriteText(FileName,
    '<?xml version="1.0" encoding="UTF-8"?>' + LineEnding +
    '<CONFIG>' + LineEnding +
    '  <ProjectOptions>' + LineEnding +
    '    <RequiredPackages>' + LineEnding +
    '      <Item>' + LineEnding +
    '        <PackageName Value="' + PackageName + '"/>' + LineEnding +
    '      </Item>' + LineEnding +
    '    </RequiredPackages>' + LineEnding +
    '  </ProjectOptions>' + LineEnding +
    '  <CompilerOptions>' + LineEnding +
    '    <SearchPaths>' + LineEnding +
    '      <UnitOutputDirectory Value="lib/$(TargetCPU)-$(TargetOS)"/>' + LineEnding +
    '      <IncludeFiles Value="$(ProjOutDir)"/>' + LineEnding +
    '    </SearchPaths>' + LineEnding +
    '  </CompilerOptions>' + LineEnding +
    '</CONFIG>' + LineEnding);
end;

function TTestLazConfig.NewOptions(const LazarusDir: string): TCodeToolsOptions;
begin
  Result := TCodeToolsOptions.Create;
  Result.LazarusSrcDir := LazarusDir;
  Result.FPCSrcDir := MakeDir(['fpcsrc']);
  Result.TargetOS := 'linux';
  Result.TargetProcessor := 'x86_64';
  Result.LCLWidgetType := 'gtk2';
end;

function TTestLazConfig.ConfigureProjectUnitPath(const ConfigPath,
  ProjectFile, LazarusDir: string): string;
var
  Options: TCodeToolsOptions;
begin
  Options := NewOptions(LazarusDir);
  try
    GuessCodeToolConfig(FTransport, Options, ConfigPath);
    ConfigureSingleProject(FTransport, ProjectFile, Options);
    Result := CodeToolBoss.GetUnitPathForDirectory(ExtractFilePath(ProjectFile), False);
  finally
    Options.Free;
  end;
end;

procedure TTestLazConfig.SetUp;
begin
  FRoot := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'pasls-lazconfig-' + IntToStr(GetTickCount64);
  ForceDirectories(FRoot);
  FTransport := TNullTransport.Create;
  CodeToolBoss.DefineTree.Clear;
end;

procedure TTestLazConfig.TearDown;
begin
  CodeToolBoss.DefineTree.Clear;
  FreeAndNil(FTransport);
  if FRoot <> '' then
    DeleteDirectory(FRoot, False);
end;

procedure TTestLazConfig.TestExplicitPackageLinkOverridesGlobalLink;
var
  PackageName, LazarusDir, ConfigDir, ProjectFile, GlobalPkg, ExplicitPkg,
  GlobalUnitDir, ExplicitUnitDir, UnitPath: string;
begin
  PackageName := 'PkgOverride' + IntToStr(GetTickCount64);
  LazarusDir := MakeDir(['lazarus']);
  ConfigDir := MakeDir(['config']);
  GlobalUnitDir := MakeDir(['global', 'units']);
  ExplicitUnitDir := MakeDir(['explicit', 'units']);
  GlobalPkg := MakeFile(['global', 'pkg.lpk']);
  ExplicitPkg := MakeFile(['explicit', 'pkg.lpk']);
  ProjectFile := MakeFile(['project', 'project.lpi']);

  WritePackage(GlobalPkg, PackageName, GlobalUnitDir);
  WritePackage(ExplicitPkg, PackageName, ExplicitUnitDir);
  WriteText(MakeFile(['lazarus', 'packager', 'globallinks',
    LowerCase(PackageName) + '-1.0.lpl']), '$(LazarusDir)/../global/pkg.lpk');
  WritePackageFiles(ConfigDir, PackageName, ExplicitPkg);
  WriteProject(ProjectFile, PackageName);

  UnitPath := ConfigureProjectUnitPath(ConfigDir, ProjectFile, LazarusDir);

  AssertTrue('explicit package unit path should win',
    Pos(ExplicitUnitDir, UnitPath) > 0);
  AssertFalse('global package unit path should not override explicit config',
    Pos(GlobalUnitDir, UnitPath) > 0);
end;

procedure TTestLazConfig.TestConfigFilePathLoadsPackageFilesFromContainingDirectory;
var
  PackageName, LazarusDir, ConfigDir, ConfigFile, ProjectFile, PackageFile,
  PackageUnitDir, UnitPath: string;
begin
  PackageName := 'PkgConfigFile' + IntToStr(GetTickCount64);
  LazarusDir := MakeDir(['lazarus']);
  ConfigDir := MakeDir(['config']);
  ConfigFile := IncludeTrailingPathDelimiter(ConfigDir) + 'environmentoptions.xml';
  PackageUnitDir := MakeDir(['package', 'units']);
  PackageFile := MakeFile(['package', 'pkg.lpk']);
  ProjectFile := MakeFile(['project', 'project.lpi']);

  WriteText(ConfigFile, '<CONFIG/>' + LineEnding);
  WritePackage(PackageFile, PackageName, PackageUnitDir);
  WritePackageFiles(ConfigDir, PackageName, PackageFile);
  WriteProject(ProjectFile, PackageName);

  UnitPath := ConfigureProjectUnitPath(ConfigFile, ProjectFile, LazarusDir);

  AssertTrue('file-valued config should load sibling packagefiles.xml',
    Pos(PackageUnitDir, UnitPath) > 0);
end;

procedure TTestLazConfig.TestPathMacroExpansionKeepsUnknownMacrosRelative;
var
  PackageName, LazarusDir, ConfigDir, ProjectFile, PackageFile, PackageDir,
  PackageUnitDir, UnitPath, ExpectedProjOutDir, ExpectedLazarusPath: string;
begin
  PackageName := 'PkgMacros' + IntToStr(GetTickCount64);
  LazarusDir := MakeDir(['lazarus']);
  ConfigDir := MakeDir(['config']);
  PackageDir := MakeDir(['package']);
  PackageUnitDir := MakeDir(['package', 'units']);
  PackageFile := IncludeTrailingPathDelimiter(PackageDir) + 'pkg.lpk';
  ProjectFile := MakeFile(['project', 'project.lpi']);
  ExpectedProjOutDir := IncludeTrailingPathDelimiter(PackageDir) + 'lib' +
    DirectorySeparator + 'x86_64-linux';
  ExpectedLazarusPath := IncludeTrailingPathDelimiter(LazarusDir) + 'components' +
    DirectorySeparator + 'x86_64-linux-gtk2';

  WritePackage(PackageFile, PackageName, PackageUnitDir,
    '$(ProjOutDir);$(LazarusDir)/components/$(TargetCPU)-$(TargetOS)-$(LCLWidgetType);$(UnknownMacro)/keep');
  WritePackageFiles(ConfigDir, PackageName, PackageFile);
  WriteProject(ProjectFile, PackageName);

  UnitPath := ConfigureProjectUnitPath(ConfigDir, ProjectFile, LazarusDir);

  AssertTrue('ProjOutDir should expand using package UnitOutputDirectory',
    Pos(ExpectedProjOutDir, UnitPath) > 0);
  AssertTrue('Lazarus/target/widget macros should expand',
    Pos(ExpectedLazarusPath, UnitPath) > 0);
  AssertFalse('unknown macro should not be absolutized under package dir',
    Pos(IncludeTrailingPathDelimiter(PackageDir) + '$(UnknownMacro)', UnitPath) > 0);
end;

initialization
  RegisterTest(TTestLazConfig);

end.
