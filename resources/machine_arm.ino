#include <Servo.h>  

#define NSTEP 100
#define THETA_START 93

Servo myservoA;  
Servo myservoB;
Servo myservoC;
Servo myservoD;

void setup() 
{
  myservoA.attach(9);  // 控制腰部（A）的端口是~9号
  myservoB.attach(10); // 控制大臂（B）的端口是~10号
  myservoC.attach(11); // 控制小臂（C）的端口是~11号
  myservoD.attach(6); // 控制腕部（D）的端口是~6号

  Serial.begin(9600);
  while (!Serial) {}

  pinMode(8,INPUT);
  pinMode(12,OUTPUT);
  digitalWrite(13,LOW);
  digitalWrite(12,HIGH);
  pinMode(13,OUTPUT);
} 

/* basis     servoA range 1 ~ 179 */
/* large arm servoB range 35 ~ 128, 35 is forward, 128 is back*/ 
/*                  Length is 13.9 cm */
/* small arm servoC range 56 ~ 129, 56 is high, 129 is low*/
/*                  Length is 15.25 cm */
/* wrist     servoD range 0 ~ 180 */
int inited = 0;
/**
 * The angles for servoB, servoC, servoA 
 * int betas[]        = {...};
 * int alphas[]       = {...};
 * int delta_thetas[] = {...};
*/

// Insert Arrays Here

void loop()
{
  int len = sizeof(betas)/sizeof(betas[0]);
  if (!inited) {
    int init_beta, init_alpha, init_theta;
    init_beta = myservoB.read();
    init_alpha = myservoC.read();
    init_theta = myservoA.read();

    for (int i=0; i<NSTEP; i++) {
      myservoB.write(map(i, 0, NSTEP, init_beta, (int16_t)pgm_read_word_near(betas)));
      myservoC.write(map(i, 0, NSTEP, init_alpha, (int16_t)pgm_read_word_near(alphas)));
      myservoA.write(map(i, 0, NSTEP, init_theta, THETA_START + (int16_t)pgm_read_word_near(delta_thetas)));
    }

    inited = 1;
  }

  for (int j=0; j<len-1; j++) {
    for (int i=0; i<NSTEP; i++) {
      myservoB.write(map(i, 0, NSTEP, (int16_t)pgm_read_word_near(betas+j), (int16_t)pgm_read_word_near(betas+j+1)));
      myservoC.write(map(i, 0, NSTEP, (int16_t)pgm_read_word_near(alphas+j), (int16_t)pgm_read_word_near(alphas+j+1)));
      myservoA.write(map(i, 0, NSTEP, THETA_START + (int16_t)pgm_read_word_near(delta_thetas+j), THETA_START + (int16_t)pgm_read_word_near(delta_thetas+j+1)));
    }
  }

  for (int i=0; i<NSTEP; i++) {
    myservoB.write(map(i, 0, NSTEP, (int16_t)pgm_read_word_near(betas+len-1), (int16_t)pgm_read_word_near(betas)));
    myservoC.write(map(i, 0, NSTEP, (int16_t)pgm_read_word_near(alphas+len-1), (int16_t)pgm_read_word_near(alphas)));
    myservoA.write(map(i, 0, NSTEP, THETA_START + (int16_t)pgm_read_word_near(delta_thetas+len-1), THETA_START + (int16_t)pgm_read_word_near(delta_thetas)));
  }
}

