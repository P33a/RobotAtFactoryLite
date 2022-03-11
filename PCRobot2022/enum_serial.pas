unit enum_serial;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$IFDEF Windows}
  ,windows
  {$ENDIF}
  {$ifdef Linux}
  ,BaseUnix
  {$ENDIF};

procedure ListComPorts(SL: TStrings);
function IsComPortAvailable(DeviceName: string): boolean;


implementation

{$IFDEF Windows}

function IsComPortAvailable(DeviceName: string): boolean;
var securityAttributes: TSecurityAttributes;
    ret: THandle;
    dwError: DWORD;
begin
  securityAttributes.nLength := SizeOf(TSecurityAttributes);
  securityAttributes.lpSecurityDescriptor := nil;
  securityAttributes.bInheritHandle := true;

  result := FALSE;
  ret := CreateFile(
      PChar(DeviceName),
      GENERIC_READ or GENERIC_WRITE,
      0,
      @securityAttributes,
      OPEN_EXISTING,
      0,
      0);
  if (ret = INVALID_HANDLE_VALUE) then begin
    dwError := GetLastError();

    //Check to see if the error was because some other app had the port open or a general failure
    if (dwError = ERROR_ACCESS_DENIED) or (dwError = ERROR_GEN_FAILURE) or (dwError = ERROR_SHARING_VIOLATION) or (dwError = ERROR_SEM_TIMEOUT) then begin
      result := TRUE;
    end;
  end else begin
    //The port was opened successfully
    result := TRUE;
    CloseHandle(ret);
  end;
end;


procedure ListComPorts(SL: TStrings);
var i: integer;
    securityAttributes: TSecurityAttributes;
    ret: THandle;
    GoodCom: boolean;
    DeviceName: string;
    dwError: DWORD;
begin
  securityAttributes.nLength := SizeOf(TSecurityAttributes);
  securityAttributes.lpSecurityDescriptor := nil;
  securityAttributes.bInheritHandle := true;

  SL.Clear;
  for i := 1 to 64 do begin
    GoodCom := FALSE;
    //ATL::CHandle port(CreateFile(szPort, GENERIC_READ | GENERIC_WRITE, 0, 0, OPEN_EXISTING, 0, 0));
    DeviceName := '\\.\COM' + IntToStr(i);
    //ret := fileCreate(DeviceName, GENERIC_READ or GENERIC_WRITE);
    ret := CreateFile(
        PChar(DeviceName),
        GENERIC_READ or GENERIC_WRITE,
        0,
        @securityAttributes,
        OPEN_EXISTING,
        0,
        0);
    if (ret = INVALID_HANDLE_VALUE) then begin
      dwError := GetLastError();

      //Check to see if the error was because some other app had the port open or a general failure
      if (dwError = ERROR_ACCESS_DENIED) or (dwError = ERROR_GEN_FAILURE) or (dwError = ERROR_SHARING_VIOLATION) or (dwError = ERROR_SEM_TIMEOUT) then begin
        GoodCom := TRUE;
      end;
    end else begin
      //The port was opened successfully
      GoodCom := TRUE;
      CloseHandle(ret);
    end;
    if GoodCom then begin
      SL.Add(DeviceName);
    end;
  end;
end;
{$ENDIF}

{$ifdef Linux}
procedure ListComPorts(SL: TStrings);
var i: integer;
    ret: THandle;
    GoodCom: boolean;
    DeviceName: string;
    dwError: DWORD;
begin

  SL.Clear;
  for i := 0 to 16 do begin
    GoodCom := FALSE;
    DeviceName := '/dev/ttyACM' + IntToStr(i);
    //ret := fileCreate(DeviceName, fmOpenRead or fmOpenWrite);
    ret := fpopen(DeviceName, O_RDWR or O_NOCTTY);

    if (ret = feInvalidHandle) then begin
      dwError := GetLastOSError();

      //Check to see if the error was because some other app had the port open or a general failure
      //if (dwError = EACCES) then begin
      //  GoodCom := TRUE;
      //end;
    end else begin
      //The port was opened successfully
      GoodCom := TRUE;
    end;

    if GoodCom then begin
      SL.Add(DeviceName);
    end;
  end;

  for i := 0 to 16 do begin
    DeviceName := '/dev/ttyUSB' + IntToStr(i);
    ret := fpopen(DeviceName, O_RDWR or O_NOCTTY);

    if (ret <> feInvalidHandle) then begin
      SL.Add(DeviceName);
      fpclose(ret);
    end;
  end;


end;
{$ENDIF}


end.

