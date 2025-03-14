; Licensed to the Apache Software Foundation (ASF) under one or more
; contributor license agreements.  See the NOTICE file distributed with
; this work for additional information regarding copyright ownership.
; The ASF licenses this file to You under the Apache License, Version 2.0
; (the "License"); you may not use this file except in compliance with
; the License.  You may obtain a copy of the License at
;
;     http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
; See the License for the specific language governing permissions and
; limitations under the License.

[Setup]
; Do not change the AppID, this GUID uniquely identifies Daffodil
AppId={{4C966AFF-585E-4E17-8CC2-059FD70FEC77}
AppName=Apache Daffodil
AppPublisher=Apache Daffodil <dev@apache.daffodil.org>
AppPublisherURL=https://daffodil.apache.org
AppVerName=Apache Daffodil v{#VERSION}
AppVersion={#VERSION}
ChangesEnvironment=yes
Compression=none
DefaultDirName={autopf}\Apache\Daffodil
DisableProgramGroupPage=yes
DisableWelcomePage=no
LicenseFile={#BASEDIR}\bin.LICENSE
PrivilegesRequired=admin
SetupIconFile={#BASEDIR}\src\windows\apache-daffodil.ico
UninstallDisplayIcon={app}\apache-daffodil.ico
TimeStampsInUTC=yes
TouchDate={#TOUCHDATE}
TouchTime={#TOUCHTIME}
WizardImageFile={#BASEDIR}\src\windows\dialog.bmp
WizardStyle=modern

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Files]
Source: "{#BASEDIR}\target\universal\stage\*"; DestDir: "{app}"; \
  Flags: ignoreversion recursesubdirs touch
Source: "{#BASEDIR}\src\windows\apache-daffodil.ico"; DestDir: "{app}"; \
  Flags: ignoreversion touch

[InstallDelete]
; Inno Setup does not delete files from previous installations when upgrading
; to a new version. For most of our files this is fine since we'll just overwrite
; the old files with  the new files. But the Daffodil CLI adds everything in the
; lib directory to the classpath, so we need to make sure to remove old jars from
; previous installs. Rather than trying to list all possible old jars, just
; delete the entire lib directory. Note that if users put plugin jars in this
; directory, they will also be deleted, but they shouldn't do that--plugin jars
; should live in a separate directory and added to DAFFODIL_CLASSPATH.
Type: files; Name: "{app}\lib\*"

[Code]
// Inno Setup does not have a built-in way to add the {app}\bin directory to
// PATH. This is important since this is a CLI tool so it won't be started by
// things like shortscuts or the start menu so really does want to be on the
// PATH. The below code modifies PATH to add or remove the bin directory
// depending on if we are installing or uninstalling, making sure not to re-add
// it if a previous install already added it.
procedure ModifyPath(Path: string; Installing: boolean);
var
  CurrentPaths: string;
  PathList: TStringList;
  PathIndex: Integer;
begin
  RegQueryStringValue(HKEY_LOCAL_MACHINE, 'System\CurrentControlSet\Control\Session Manager\Environment', 'Path', CurrentPaths);

  PathList := TStringList.Create;
  PathList.Delimiter := ';';
  PathList.StrictDelimiter := True;
  PathList.CaseSensitive := False;
  PathList.DelimitedText := CurrentPaths;

  PathIndex := PathList.IndexOf(Path);

  if Installing and (PathIndex = -1) then
    PathList.Add(Path)
  else if not Installing and (PathIndex <> -1) then
    PathList.Delete(PathIndex);

  RegWriteStringValue(HKEY_LOCAL_MACHINE, 'System\CurrentControlSet\Control\Session Manager\Environment', 'Path', PathList.DelimitedText);

  PathList.Free;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssPostInstall then
    ModifyPath(ExpandConstant('{app}\bin'), true);
end;

procedure CurUninstallStepChanged(CurStep: TUninstallStep);
begin
  if CurStep = usPostUninstall then
    ModifyPath(ExpandConstant('{app}\bin'), false);
end;
