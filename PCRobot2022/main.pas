unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TARadialSeries, sdpofpserial,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls, VerInfo,
  JSONPropStorage, Grids, math, lNetComponents, lNet, channels, enum_serial;

const

  ParsFileName = 'pars.txt';

type
  TRobot = record
    PWM1, PWM2: integer;
    enc1, enc2: integer;
    Senc1, Senc2: integer;
    T1, T2: integer;
    state: integer;
    IR: array [0..4] of integer;
    TouchSwitch: integer;
    solenoid: integer;

    vref, wref: single;
    v, w: single;
    x, y, theta: single;
    xv, yv, thetav: single;
  end;

  TPID = record
    Ki, Kp, Kd, Kf: single;
  end;


  { TFMain }

  TFMain = class(TForm)
    BSetRobotSolenoid: TButton;
    BSetRobotVW: TButton;
    BSetRobotVW0: TButton;
    BSetRobotXYTheta: TButton;
    BCloseComPort: TButton;
    BOpenComPort: TButton;
    BRefreshComPorts: TButton;
    BSetRobotPID: TButton;
    BSetT1T2: TButton;
    BSetState: TButton;
    BSaveLog: TButton;
    BSetStateAlt: TButton;
    BClearEncoderAcc: TButton;
    BSetPWMs: TButton;
    BRobotStop: TButton;
    BUDPConnect: TButton;
    BUDPDisconnect: TButton;
    Button_ClearChart: TButton;
    CBRobotSpeeds: TCheckBox;
    CBRawDebug: TCheckBox;
    CBRobotPos: TCheckBox;
    Chart: TChart;
    CBChartFreeze: TCheckBox;
    CBComPort: TComboBox;
    CBIRSeries: TCheckBox;
    CBEncoders: TCheckBox;
    LSEnc2: TLineSeries;
    LSEnc1: TLineSeries;
    LSIR5: TLineSeries;
    LSIR4: TLineSeries;
    LSIR3: TLineSeries;
    LSIR2: TLineSeries;
    LSIR1: TLineSeries;
    LSRobotV: TLineSeries;
    LSRobotW: TLineSeries;
    EditRobotX1: TEdit;
    EditStateReq: TEdit;
    EditLogName: TEdit;
    EditRemoteIP: TEdit;
    EditStateReqAlt: TEdit;
    JSONPropStorage: TJSONPropStorage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LSRobotTheta: TLineSeries;
    LSRobotY: TLineSeries;
    LSRobotX: TLineSeries;
    EditZpole: TEdit;
    EditPacketCount: TEdit;
    EditDebug: TEdit;
    MemoDebug: TMemo;
    Serial: TSdpofpSerial;
    StatusBar: TStatusBar;
    SGRobot: TStringGrid;
    Timer: TTimer;
    UDP: TLUDPComponent;
    procedure BClearEncoderAccClick(Sender: TObject);
    procedure BCloseComPortClick(Sender: TObject);
    procedure BGoClick(Sender: TObject);
    procedure BOpenComPortClick(Sender: TObject);
    procedure BRefreshComPortsClick(Sender: TObject);
    procedure BRobotStopClick(Sender: TObject);
    procedure BSaveLogClick(Sender: TObject);
    procedure BSetPWMsClick(Sender: TObject);
    procedure BSetRobotPIDClick(Sender: TObject);
    procedure BSetRobotSolenoidClick(Sender: TObject);
    procedure BSetRobotVW0Click(Sender: TObject);
    procedure BSetRobotVWClick(Sender: TObject);
    procedure BSetRobotXYThetaClick(Sender: TObject);
    procedure BSetStateAltClick(Sender: TObject);
    procedure BSetStateClick(Sender: TObject);
    procedure BSetT1T2Click(Sender: TObject);
    procedure BUDPConnectClick(Sender: TObject);
    procedure BUDPDisconnectClick(Sender: TObject);
    procedure Button_ClearChartClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SerialRxData(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure UDPError(const msg: string; aSocket: TLSocket);
    procedure UDPReceive(aSocket: TLSocket);
    procedure FormCreate(Sender: TObject);
  private
    procedure Debug(s: string);
    procedure Debugln(s: string);
    procedure processFrame(channel: char; value: integer; source: integer);
    procedure SaveLog;
    procedure SeriesAddPoint(Series: TLineSeries; Y: double);
    procedure SeriesClear;
    procedure SetComState(newState: boolean);
    { private declarations }
  public
    UDPChannels: TChannels;
    packetCount: integer;
    UDPMessageList: string;

    SerialChannels: TChannels;

    Robot: TRobot;
    PID: TPID;

    FrameTime, LastFrameTime: QWord;
    delta : double;

    SeriesMaxPoints : integer;
  end;

var
  FMain: TFMain;


function BuildMessage(c: char; val: integer): string;
procedure SendMessage(c: char; val: integer);
procedure SendRaw(s: string);


implementation

{$R *.lfm}


procedure SendMessage(c: char; val: integer);
var mess: string;
begin
  mess := c + IntToHex(dword(Val) and $FFFFFFFF, 8);
  with Fmain do begin
    if Serial.Active then serial.WriteData(mess);
    if UDP.Connected then UDP.SendMessage(mess, EditRemoteIP.Text + ':4224');
  end;
end;

procedure SendRaw(s: string);
begin
  with Fmain do begin
    if Serial.Active then serial.WriteData(s);
    if UDP.Connected then UDP.SendMessage(s, EditRemoteIP.Text + ':4224');
  end;
end;

function FilterZ(Y: double; U: double; z: double): double;
begin
  result := Y * (1 - z) + U * z;
end;

{ TFMain }

procedure TFMain.Debug(s: string);
begin
  MemoDebug.Lines.Add(s);
  while MemoDebug.Lines.Count > 10000 do begin
    MemoDebug.Lines.Delete(0);
  end;
end;

procedure TFMain.Debugln(s: string);
begin
  Debug(s + chr(10));
end;


function BuildMessage(c: char; val: integer): string;
begin
  result := c + IntToHex(dword(Val) and $FFFFFFFF, 8);
end;

function BuildMessage(c: char; f: single): string;
begin
  result := c + IntToHex((PDWord(@f))^ and $FFFFFFFF, 8);
end;

function BuildMessage(c: char; word_H: word; word_L: word): string;
begin
  result := c + IntToHex(dword((word_H shl 16) or word_L) and $FFFFFFFF, 8);
end;




procedure TFMain.processFrame(channel: char; value: integer; source: integer);
var i: integer;
begin
  //Temperature := PSingle(@value)^;
  if channel = 'I' then begin
    for i := 0 to 4 do begin
      Robot.IR[4 - i] := ((value shr (i * 6)) and $3F) * 16;
    end;
    Robot.TouchSwitch := (value shr 31) and 1;
    Robot.solenoid := (value shr 30) and 1;

  end else if channel = 'U' then begin  // PWM in 16 bits signed
    Robot.PWM1 := smallint((value shr 16) and $FFFF);
    Robot.PWM2 := smallint(value and $FFFF);

  end else if channel = 'R' then begin // Encoder measure in 16 bits signed
    Robot.enc1 := smallint((value shr 16) and $FFFF);
    Robot.enc2 := smallint(value and $FFFF);

  end else if channel = 'S' then begin // Accumulated Encoder measure in 16 bits signed
    Robot.Senc1 := smallint((value shr 16) and $FFFF);
    Robot.Senc2 := smallint(value and $FFFF);

  end else if channel = 'T' then begin // General parameters T1 and T2 in 16 bits unsigned
    Robot.T1 := (value shr 16) and $FFFF;
    Robot.T2 := value and $FFFF;

  end else if channel = 'X' then begin // Vision Marker x position
    Robot.xv := PSingle(@value)^;

  end else if channel = 'Y' then begin // Vision Marker y position
    Robot.yv := PSingle(@value)^;

  end else if channel = 'Z' then begin // Vision Marker Theta angle
    Robot.thetav := PSingle(@value)^;

  end else if channel = 'v' then begin // Robot linear speed
    Robot.v := PSingle(@value)^;

  end else if channel = 'w' then begin // Robot angular speed
    Robot.w := PSingle(@value)^;

  end else if channel = 'x' then begin // Robot estimated x position
    Robot.x := PSingle(@value)^;

  end else if channel = 'y' then begin // Robot estimated y position
    Robot.y := PSingle(@value)^;

  end else if channel = 't' then begin // Robotestimated  Theta angle
    Robot.theta := PSingle(@value)^;

  end else if channel = 'p' then begin // Robotestimated  Theta angle
    PID.Kp := PSingle(@value)^;

  end else if channel = 'i' then begin // Robotestimated  Theta angle
    PID.Ki := PSingle(@value)^;

  end else if channel = 'm' then begin // Robotestimated  Theta angle
    PID.Kd := PSingle(@value)^;

  end else if channel = 'n' then begin // Robotestimated  Theta angle
    PID.Kf := PSingle(@value)^;

  end else if channel = 'P' then begin  // This should be the last packet of the "Frame"
    delta := (value and $FFFF) / 10;
    Robot.state := ((value shr 16) and $FFFF);

    // If there are pending messages, send them and then clear the list
    if UDPMessageList <> '' then begin
      SendRaw(UDPMessageList);
      UDPMessageList := '';
    end;

    // Refresh the Interface
    SGRobot.BeginUpdate;
    SGRobot.Cells[1, 1] := format('%.1f',[delta]);
    SGRobot.Cells[1, 2] := IntToStr(Robot.state);

    SGRobot.Cells[1, 3] := IntToStr(Robot.PWM1);
    SGRobot.Cells[1, 4] := IntToStr(Robot.PWM2);

    SGRobot.Cells[1, 5] := IntToStr(Robot.enc1);
    SGRobot.Cells[1, 6] := IntToStr(Robot.enc2);

    for i := 0 to 4 do begin
      SGRobot.Cells[1, 7 + i] := IntToStr(Robot.IR[i]);
    end;

    SGRobot.Cells[1, 12] := IntToStr(Robot.Senc1);
    SGRobot.Cells[1, 13] := IntToStr(Robot.Senc2);

    SGRobot.Cells[1, 14] := IntToStr(Robot.T1);
    SGRobot.Cells[1, 15] := IntToStr(Robot.T2);

    SGRobot.Cells[1, 16] := format('%.4f', [Robot.v]);
    SGRobot.Cells[1, 17] := format('%.4f', [Robot.w]);

    SGRobot.Cells[1, 18] := format('%.4f', [Robot.x]);
    SGRobot.Cells[1, 19] := format('%.4f', [Robot.y]);
    SGRobot.Cells[1, 20] := format('%.4f', [RadToDeg(Robot.theta)]);

    SGRobot.Cells[1, 21] := format('%.4f', [PID.Kp]);
    SGRobot.Cells[1, 22] := format('%.4f', [PID.Ki]);
    SGRobot.Cells[1, 23] := format('%.4f', [PID.Kd]);
    SGRobot.Cells[1, 24] := format('%.4f', [PID.Kf]);

    SGRobot.Cells[1, 25] := IntToStr(Robot.TouchSwitch);
    SGRobot.Cells[1, 26] := IntToStr(Robot.solenoid);

    SGRobot.EndUpdate;


    // Update chart
    if not CBChartFreeze.Checked then begin
      if CBEncoders.Checked then begin
        SeriesAddPoint(LSEnc1, Robot.enc1);
        SeriesAddPoint(LSEnc2, Robot.enc2);
      end;
      if CBRobotSpeeds.Checked then begin
        SeriesAddPoint(LSRobotV, Robot.v);
        SeriesAddPoint(LSRobotW, Robot.w);
      end;
      if CBRobotSpeeds.Checked then begin
        SeriesAddPoint(LSRobotX, Robot.xv);
        SeriesAddPoint(LSRobotY, Robot.yv);
        SeriesAddPoint(LSRobotTheta, Robot.thetav);
      end;
      if CBIRSeries.Checked then begin
        SeriesAddPoint(LSIR1, Robot.IR[0] / 1024);
        SeriesAddPoint(LSIR2, Robot.IR[1] / 1024);
        SeriesAddPoint(LSIR3, Robot.IR[2] / 1024);
        SeriesAddPoint(LSIR4, Robot.IR[3] / 1024);
        SeriesAddPoint(LSIR5, Robot.IR[4] / 1024);
      end;
    end;


    //  LSRobotV.Add(Robot.enc1);
    //while LSRobotV.Count > 200 do begin
    //  LSRobotV.Delete(0);
    //end;
  end;
end;


procedure TFMain.FormShow(Sender: TObject);
var i: integer;
begin
  if FileExists(ParsFileName) then begin
    SGRobot.Cols[2].LoadFromFile(ParsFileName);
  end;
end;

procedure TFMain.SerialRxData(Sender: TObject);
var s: string;
begin
  s := Serial.ReadData;
  if s = '' then exit;
  SerialChannels.ReceiveData(s);
  if CBRawDebug.Checked then begin
    MemoDebug.Clear;
    Debugln(s);
  end;
end;

procedure TFMain.TimerTimer(Sender: TObject);
begin
  if UDP.Connected then EditRemoteIP.Color := clGreen
  else EditRemoteIP.Color := clRed;

  if Serial.Active then CBComPort.Color := clGreen
  else CBComPort.Color := clRed;
end;

procedure TFMain.BSaveLogClick(Sender: TObject);
begin
  SaveLog();
end;

procedure TFMain.BSetPWMsClick(Sender: TObject);
begin
  UDPMessageList += BuildMessage('O', StrToInt(SGRobot.Cells[2, 3]), StrToInt(SGRobot.Cells[2, 4]));
end;

procedure TFMain.BSetRobotPIDClick(Sender: TObject);
begin
  UDPMessageList += BuildMessage('p', StrToFloat(SGRobot.Cells[2, 21])) +
                    BuildMessage('i', StrToFloat(SGRobot.Cells[2, 22])) +
                    BuildMessage('m', StrToFloat(SGRobot.Cells[2, 23])) +
                    BuildMessage('n', StrToFloat(SGRobot.Cells[2, 24]));
end;

procedure TFMain.BSetRobotSolenoidClick(Sender: TObject);
begin
  UDPMessageList += BuildMessage('z', StrToInt(SGRobot.Cells[2, 26]));
end;

procedure TFMain.BSetRobotVW0Click(Sender: TObject);
begin
  robot.vref := 0;
  robot.wref := 0;
  UDPMessageList += BuildMessage('v', robot.vref) +
                    BuildMessage('w', robot.wref);    end;

procedure TFMain.BSetRobotVWClick(Sender: TObject);
begin
  robot.vref := StrToFloat(SGRobot.Cells[2, 16]);
  robot.wref := StrToFloat(SGRobot.Cells[2, 17]);
  UDPMessageList += BuildMessage('v', robot.vref) +
                    BuildMessage('w', robot.wref);
end;

procedure TFMain.BSetRobotXYThetaClick(Sender: TObject);
begin
  UDPMessageList += BuildMessage('x', StrToFloat(SGRobot.Cells[2, 18])) +
                    BuildMessage('y', StrToFloat(SGRobot.Cells[2, 19])) +
                    BuildMessage('t', DegToRad(StrToFloat(SGRobot.Cells[2, 20])));
end;

procedure TFMain.BSetStateAltClick(Sender: TObject);
begin
  UDPMessageList += BuildMessage('s', StrToInt(EditStateReqAlt.Text));
  SeriesClear();
end;

procedure TFMain.BSetStateClick(Sender: TObject);
begin
  UDPMessageList += BuildMessage('s', StrToInt(EditStateReq.Text));
end;

procedure TFMain.BSetT1T2Click(Sender: TObject);
begin
  UDPMessageList += BuildMessage('T', StrToInt(SGRobot.Cells[2, 14]), StrToInt(SGRobot.Cells[2, 15]));
end;

procedure TFMain.BClearEncoderAccClick(Sender: TObject);
begin
  UDPMessageList += BuildMessage('o', 0);
end;

procedure TFMain.BCloseComPortClick(Sender: TObject);
begin
  SetComState(false);
end;

procedure TFMain.BGoClick(Sender: TObject);
var i: integer;
begin
  for i := 0 to Chart.Series.Count - 1 do begin
    (Chart.Series.Items[i] as TLineSeries).Clear;
  end;

  CBChartFreeze.Checked := false;
end;

procedure TFMain.BOpenComPortClick(Sender: TObject);
begin
  SetComState(true);
end;

procedure TFMain.BRefreshComPortsClick(Sender: TObject);
begin
  ListComPorts(CBComPort.Items);
end;

procedure TFMain.BRobotStopClick(Sender: TObject);
begin
  UDPMessageList += BuildMessage('s', 200);
end;

procedure TFMain.SetComState(newState: boolean);
begin
  try
  if newState then begin
    Serial.Device := CBComPort.Text;
    Serial.Open();
    if not Serial.Active then
      raise Exception.Create('Could not open port ' + Serial.Device);
    //SerSetParams(SerialHandle, StrToInt(EditSerialBaudrate.Text), 8, NoneParity, 1, []);
  end else begin
    Serial.Close();
  end;
  finally
    if Serial.Active then CBComPort.Color := clGreen
    else CBComPort.Color := clRed;
  end;
end;


procedure TFMain.BUDPConnectClick(Sender: TObject);
begin
  if not UDP.Connected then begin
    UDP.Listen(UDP.Port);
  end;
  UDP.SendMessage('Init', EditRemoteIP.Text + ':4224');
end;

procedure TFMain.BUDPDisconnectClick(Sender: TObject);
begin
  if UDP.Connected then begin
    UDP.Disconnect();
  end;
end;

procedure TFMain.Button_ClearChartClick(Sender: TObject);
begin
  SeriesClear();
end;

procedure TFMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SGRobot.Cols[2].SaveToFile(ParsFileName);
end;

procedure TFMain.SeriesClear;
var i: integer;
begin
  for i := 0 to Chart.Series.Count - 1 do begin
    (Chart.Series[i] as TLineSeries).Clear;
  end;
end;

procedure TFMain.FormDestroy(Sender: TObject);
begin
  UDPChannels.Free;
  SerialChannels.Free;
end;

procedure TFMain.UDPError(const msg: string; aSocket: TLSocket);
begin
  UDP.Disconnect();
  StatusBar.SimpleText := msg;
end;

procedure TFMain.UDPReceive(aSocket: TLSocket);
var s: string;
begin
  UDP.GetMessage(s);
  if s <> '' then begin
    inc(packetCount);
    EditPacketCount.Text := IntToStr(packetCount);
    UDPChannels.ReceiveData(s);
    if CBRawDebug.Checked then begin
      MemoDebug.Clear;
      Debugln(s);
    end;
  end;
end;

procedure TFMain.FormCreate(Sender: TObject);
begin
  // Lazarus catches WM_SETTINGCHANGE and calls Application.IntfSettingChange
  // which calls GetFormatSettings in SysUtils
  // It can be switched off by setting Application.UpdateFormatSettings := False;
  {$IFDEF WINDOWS}
  Application.UpdateFormatSettings := false;
  {$ENDIF}
  DefaultFormatSettings.DecimalSeparator := '.';

  GetVersionInfo();
  Caption := 'PCRobot v' + InfoData[3];

  UDPChannels := TChannels.Create(@processFrame);
  packetCount := 0;

  SerialChannels := TChannels.Create(@processFrame);

  FrameTime := GetTickCount64();
  LastFrameTime := FrameTime;
  SeriesMaxPoints := 1024;
end;



procedure TFMain.SeriesAddPoint(Series: TLineSeries; Y: double);
var i: integer;
begin
  Series.BeginUpdate;
  Series.Add(Y);
  while Series.Count > SeriesMaxPoints do begin
    Series.Delete(0);
  end;
  for i := 0 to Series.Count - 1 do begin
    Series.SetXValue(i, i);
  end;
  Series.EndUpdate;
end;

procedure TFMain.SaveLog;
var i, NumPoints: integer;
    SL: TStringList;
    FormatSettings: TFormatSettings;
    s: string;
begin
  FormatSettings := DefaultFormatSettings;
  FormatSettings.TimeSeparator := '_';
  SL := TStringList.Create;
  try
    NumPoints := 0;
    if CBEncoders.Checked then NumPoints := max(NumPoints, LSEnc1.Count);
    if CBRobotSpeeds.Checked then NumPoints := max(NumPoints, LSRobotV.Count);
    if CBRobotPos.Checked then NumPoints := max(NumPoints, LSRobotX.Count);
    if CBIRSeries.Checked then NumPoints := max(NumPoints, LSIR1.Count);

    for i := 0 to NumPoints - 1 do begin
      s := format('%d', [i]);

      if CBEncoders.Checked then begin
        s := s + format(' %e %e', [LSEnc1.YValue[i], LSEnc2.YValue[i]]);
      end;

      if CBRobotSpeeds.Checked then begin
        s := s + format(' %e %e', [LSRobotV.YValue[i], LSRobotW.YValue[i]]);
      end;

      if CBRobotPos.Checked then begin
        s := s + format(' %e %e %e', [LSRobotX.YValue[i], LSRobotY.YValue[i], LSRobotTheta.YValue[i]]);
      end;

      if CBIRSeries.Checked then begin
        s := s + format(' %e %e %e %e %e',
                       [LSIR1.YValue[i],
                        LSIR2.YValue[i],
                        LSIR3.YValue[i],
                        LSIR4.YValue[i],
                        LSIR5.YValue[i]
                       ]);
      end;
      SL.Add(s);
    end;
    //SL.SaveToFile(EditLogName.text + DateTimeToStr(now(), FormatSettings) + '.txt');
    SL.SaveToFile(EditLogName.text + '.txt');
    SL.Clear;
  finally
    SL.Free;
  end;
end;

end.

