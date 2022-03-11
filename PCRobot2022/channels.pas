unit channels;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

 TProcessFrame = procedure(channel: char; value: integer; source: integer) of object;
 TProcessText = procedure(s: string) of object;

{ TChannels }

TChannels = class
private
public
  serialData: string;
  FrameDigits: integer;

  channel: char;
  frame, frameSource: integer;
  frameData: string;
  textMode: boolean;
  textString: string;

  ProcessFrame: TProcessFrame;
  ProcessText: TProcessText;
  procedure ReceiveData(s: string);


  constructor Create(newProcessFrame: TProcessFrame; newProcessText: TProcessText = nil; NewFrameDigits: integer = 8);
  destructor Destroy; override;
end;


function isHexDigit(c: char): boolean;

implementation

function isHexDigit(c: char): boolean;
begin
  result := c in ['0'..'9', 'A'..'F'];
end;


{ TChannels }

constructor TChannels.Create(newProcessFrame: TProcessFrame; newProcessText: TProcessText; NewFrameDigits: integer);
begin
  ProcessFrame:= newProcessFrame;
  ProcessText := newProcessText;
  FrameDigits := NewFrameDigits;
end;

destructor TChannels.Destroy;
begin
  inherited Destroy;
end;


procedure TChannels.ReceiveData(s: string);
var c: char;
    value: integer;
begin
  if s = '' then exit;
  serialData := serialData + s;

  while Length(serialData) > 0 do begin
    c := serialData[1];
    serialData := copy(serialData, 2, maxint);

    if assigned(ProcessText) then begin
      if ord(c) = 2 then begin
        textMode := true;
        textString := '';
        continue;
      end;

      if ord(c) = 3 then begin
        textMode := false;
        ProcessText(textString);
        textString := '';
        continue;
      end;

      if TextMode then begin
        textString := textString + c;
        if c = chr(10) then begin
          ProcessText(textString);
          textString := '';
        end;
        continue;
      end;
    end;


    if frame = -1 then begin

      if c = '*' then frameSource := 0
      else if c = '+' then frameSource := 1
      else if c = '-' then frameSource := 2;

      if (c in ['G'..'Z']) or (c in ['g'..'z']) then begin
        frame := 0;
        channel := c;
        frameData := '';
      end;
    end else begin
      if isHexDigit(c) then begin
        frameData := frameData + c;
        inc(frame);
        if frame = FrameDigits then begin
          value := StrToIntDef('$' + frameData, -1);
          processFrame(channel, value, frameSource);
          frame := -1;
        end;
      end else begin
        frame := -1;
      end;
    end;
  end;
end;

end.

