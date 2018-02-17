#include <stdio.h>
#include <wiringPi.h>

unsigned long ReadCount(void);

int main (void)
{
  if (wiringPiSetup () == -1)
    return 1;

  pinMode(2, OUTPUT);
  pinMode(3, INPUT);
while(1) {
  printf("%lx\n", ReadCount());
}
  return 0;
}

unsigned long ReadCount(void){
  unsigned long Count;
  unsigned char i;
//  ADDO=1;
//  ADSK=0;
  Count=0;
  while(digitalRead(3));
  for (i=0;i<24;i++){
    digitalWrite(2, 1);
    Count=Count<<1;
    digitalWrite(2, 0);
    if(digitalRead(3)) Count++;
  }
//  ADSK=1;
  digitalWrite(2, 1);
  Count=Count^0x800000;
//  ADSK=0;
  digitalWrite(2, 0);
  digitalWrite(2, 1);
  digitalWrite(2, 0);
  digitalWrite(2, 1);
  digitalWrite(2, 0);
  return(Count);
}
