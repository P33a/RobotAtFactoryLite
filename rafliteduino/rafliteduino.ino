#include <SPI.h>
#include "src/MFRC522.h"

// RFID pins
#define SS_PIN  7
#define RST_PIN 8
 
MFRC522 rfid(SS_PIN, RST_PIN); // Instance of the class

MFRC522::MIFARE_Key key; 

// Init array that will store new NUID 
byte nuidPICC[4];



// Motor Pins
#define MOTOR_AIN1 6
#define MOTOR_AIN2 5
#define MOTOR_PWMA 9

  
#define MOTOR_BIN1 4
#define MOTOR_BIN2 3
#define MOTOR_PWMB 10

// Solenoid Pins
#define SOLENOID 2

// Touch Switch
#define TOUCHSW 11

void setSolenoidState(byte state)
{
  digitalWrite(SOLENOID, state);
}

byte readTouchSW(void)
{
  byte ret;
  pinMode(TOUCHSW, INPUT_PULLUP);
  ret = !digitalRead(TOUCHSW);
  pinMode(TOUCHSW, OUTPUT);
  return ret;
}

void setMotorsVoltage(int v1, int v2)
{
  if (v1 >= 0) {
    digitalWrite(MOTOR_AIN1, 1);
    digitalWrite(MOTOR_AIN2, 0);
    if (v1 > 255) v1 = 255;
    analogWrite(MOTOR_PWMA, v1);
  } else {
    digitalWrite(MOTOR_AIN1, 0);
    digitalWrite(MOTOR_AIN2, 1);
    v1 = - v1;
    if (v1 > 255) v1 = 255;
    analogWrite(MOTOR_PWMA, v1);
  }

  if (v2 >= 0) {
    digitalWrite(MOTOR_BIN1, 1);
    digitalWrite(MOTOR_BIN2, 0);
    if (v2 > 255) v2 = 255;
    analogWrite(MOTOR_PWMB, v2);
  } else {
    digitalWrite(MOTOR_BIN1, 0);
    digitalWrite(MOTOR_BIN2, 1);
    v2 = - v2;
    if (v2 > 255) v2 = 255;
    analogWrite(MOTOR_PWMB, v2);
  }
  
}


void setup() 
{
  // Motors I/O Setup
  digitalWrite(MOTOR_AIN1, 0);
  digitalWrite(MOTOR_AIN2, 0);
  digitalWrite(MOTOR_PWMA, 0);
  digitalWrite(MOTOR_BIN1, 0);
  digitalWrite(MOTOR_BIN2, 0);
  digitalWrite(MOTOR_PWMB, 0);
  
  pinMode(MOTOR_AIN1, OUTPUT);
  pinMode(MOTOR_AIN2, OUTPUT);
  pinMode(MOTOR_PWMA, OUTPUT);
  pinMode(MOTOR_BIN1, OUTPUT);
  pinMode(MOTOR_BIN2, OUTPUT);
  pinMode(MOTOR_PWMB, OUTPUT);

  // Solenoid I/O Setup
  digitalWrite(SOLENOID, 0);
  pinMode(SOLENOID, OUTPUT);

  // Faster ADC - http://forum.arduino.cc/index.php/topic,6549.0.html
  // set prescaler to 16
  // sbi(ADCSRA,ADPS2); // cbi(ADCSRA,ADPS1); // cbi(ADCSRA,ADPS0);
  ADCSRA = (ADCSRA | (1 << ADPS2)) & ~((1 << ADPS1) | (1 << ADPS0));          


  Serial.begin(115200);
  
  // RFID setup
  SPI.begin(); // Init SPI bus
  rfid.PCD_Init(); // Init MFRC522 

}

void printHex(byte *buffer, byte bufferSize) {
  for (byte i = 0; i < bufferSize; i++) {
    Serial.print(buffer[i] < 0x10 ? " 0" : " ");
    Serial.print(buffer[i], HEX);
  }
}

uint32_t t;

void readRFID(void) 
{
  byte bufferATQA[2];
  byte bufferSize = sizeof(bufferATQA);

  // Look for new cards

  //if (rfid.PICC_IsNewCardPresent()) {
  if (rfid.PICC_WakeupA(bufferATQA, &bufferSize) == 0) {
    // Verify if the NUID has been readed
    if (rfid.PICC_ReadCardSerial()) {
      Serial.print(F("PICC type: "));
      MFRC522::PICC_Type piccType = rfid.PICC_GetType(rfid.uid.sak);
      Serial.print(rfid.PICC_GetTypeName(piccType));

      // Store NUID into nuidPICC array
      for (byte i = 0; i < 4; i++) {
        nuidPICC[i] = rfid.uid.uidByte[i];
      }
      
      Serial.println(F("The NUID tag is:"));
      Serial.print(F("In hex: "));
      printHex(rfid.uid.uidByte, rfid.uid.size);
      
      // Halt PICC
      rfid.PICC_HaltA();
      
      // Stop encryption on PCD
      rfid.PCD_StopCrypto1();
    }
  } 
}  


float IR_pos, IR_total;
int IR_values[5];
int IR_WaterLevel = 100;

void readIRLinePos(void)
{
  byte c;
  int v;
  for (c = 0; c < 5; c++) {
    IR_values[c] = 1023 - analogRead(A0 + c);
  }      

  IR_pos = 0;
  IR_total = 0;
  for (c = 0; c < 5; c++) {
    v = IR_values[c] - IR_WaterLevel;
    if ( v < 0) v = 0;
 
    IR_total = IR_total + v;
    IR_pos = IR_pos + v * (c - 2) * 16.0;
  }
  if (IR_total > 0) IR_pos = IR_pos / IR_total;
}

uint32_t tis;
uint32_t current, previous, interval = 40000UL;

byte state;
float v, w;

void setState(byte new_state)
{
  tis = 0;
  state = new_state;
}

void loop() 
{  
  //if ((count + 100) % 200 == 0) setMotorsVoltage(200, 200);
  //if (count % 200 == 0) setMotorsVoltage(-200, -200);
    
  
  byte b;
  if (Serial.available()) {
    b = Serial.read();
    if (b == '+') setSolenoidState(1);
    if (b == '-') setSolenoidState(0);
    if (b == '8') setMotorsVoltage(200, 200) ;
    if (b == '4') setMotorsVoltage(-200, 200) ;
    if (b == '6') setMotorsVoltage(200, -200) ;
    if (b == '2') setMotorsVoltage(-200, -200) ;
    if (b == '5') setMotorsVoltage(0, 0) ;
    if (b == '\\') state = 0;
    if (b == 'g') state = 1;
  }

  current = micros();
  if (current - previous >= interval) {
    previous = current;
    tis = tis + interval / 1000;
    
    t = micros();
    readRFID();
    t = micros() - t;

    t = micros();
    readIRLinePos();
    t = micros() - t;

    if (state == 1 && readTouchSW()) {
      setState(2);
    } else if(state == 2 && tis > 400) {
      setState(3);
    } else if(state == 3 && tis > 1600) {
      setState(4);
    } else if(state == 4 && tis > 1600 && IR_total > 1500) {
      setState(5);
    } else if(state == 5 && tis > 1600) {
      setState(6);
    } else if(state == 6 && tis > 2400) {
      setState(0);
    }


    if (state == 0) {
      setSolenoidState(0);
      v = 0;
      w = 0;
    
    } else if (state == 1) {
      setSolenoidState(0);
      v = 40;
      w = -2.0 * IR_pos;

    } else if (state == 2) {
      setSolenoidState(1);
      v = 40;
      w = -2.0 * IR_pos;
    
    } else if (state == 3) {
      setSolenoidState(1);
      v = -40;
      w = 0;
      
    } else if (state == 4) {
      setSolenoidState(1);
      v = 0;
      w = 50;
      
    } else if (state == 5) {
      setSolenoidState(1);
      v = 40;
      w = -2.0 * IR_pos;
      
    } else if (state == 6) {
      setSolenoidState(0);
      v = -40;
      w = 0;
    } 
    
    setMotorsVoltage(v + w, v - w);
    
    byte c;
    for (c = 0; c < 5; c++) {
       Serial.print(IR_values[c]);
       Serial.print(" ");
    }
    Serial.print("Pos: ");
    Serial.print(IR_pos);
    
    //Serial.print(" readTouchSW: ");
    //Serial.print(readTouchSW());
    Serial.print(" state: ");
    Serial.print(state);


    Serial.print(" tis: ");
    Serial.print(tis);
    
    //Serial.print(" Time: ");
    //Serial.print(t);
    
    Serial.println();
  }
  
}
