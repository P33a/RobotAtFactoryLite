unit VerInfo;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Forms
  // FPC 3.0 fileinfo reads exe resources as long as you register the appropriate units
 , fileinfo
 , winpeimagereader {need this for reading exe info}
 //, elfreader {needed for reading ELF executables}
 //, machoreader {needed for reading MACH-O executables}
 ;


const
  InfoNum = 10;
  InfoKey: array[1..InfoNum] of string = ('CompanyName', 'FileDescription', 'FileVersion', 'InternalName', 'LegalCopyright', 'LegalTradeMarks', 'OriginalFileName', 'ProductName', 'ProductVersion', 'Comments');

var
  InfoData: array[1..InfoNum] of string;

procedure GetVersionInfo;

implementation


procedure GetVersionInfo;
var
  FileVerInfo: TFileVersionInfo;
  i: integer;
begin
  FileVerInfo:=TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
    for i := low(InfoKey) to high(InfoKey) do begin
      InfoData[i] := FileVerInfo.VersionStrings.Values[InfoKey[i]];
    end;

  finally
    FileVerInfo.Free;
  end;
end;


end.
