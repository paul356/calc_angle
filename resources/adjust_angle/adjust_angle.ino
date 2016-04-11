#include <EEPROM.h>

#define NSTEPPERS 4
#define STEP_DIVIDE 1
#define HALF_PULSE 100
#define MAX_TRIPLES (4000L/(sizeof(long)*NSTEPPERS))

#define PULSE_DEGREE_RATIO_BASE ((long)8000L*6.75)
#define PULSE_DEGREE_RATIO_FIRST ((long)8000L*15.375)
#define PULSE_DEGREE_RATIO_SECOND ((long)8000L*11.25)
#define PULSE_DEGREE_RATIO_LAST ((long)800L)

long pulse_degree_ratio [NSTEPPERS] = {PULSE_DEGREE_RATIO_BASE, PULSE_DEGREE_RATIO_FIRST, PULSE_DEGREE_RATIO_SECOND, PULSE_DEGREE_RATIO_LAST};
long real_angles        [NSTEPPERS] = {0*PULSE_DEGREE_RATIO_BASE/360., 0*PULSE_DEGREE_RATIO_FIRST/360., 0*PULSE_DEGREE_RATIO_SECOND/360., 0*PULSE_DEGREE_RATIO_LAST};

float degree_limits     [NSTEPPERS][2] = {{-90., 90.}, {0., 90.}, {-0., 135.}, {-90., 90.}};

const int enable_pin    [NSTEPPERS] = {55, 5, 25, 34};
const int direction_pin [NSTEPPERS] = {60, 54, 27, 36};
const int step_pin      [NSTEPPERS] = {56, 4, 26, 35};

const float hardcoded[] PROGMEM = {
//#include "dump_degrees_prefix.h"
#include "dump_degrees.h"
    1., 0., 0., 0., 0., 0};

void setup()
{
    Serial.begin(9600);
    while (!Serial) {
        ;
    }

    Serial.setTimeout(1000);

    for (int i=0; i<NSTEPPERS; i++) {
        pinMode(enable_pin[i], OUTPUT);
    }

    for (int i=0; i<NSTEPPERS; i++) {
        pinMode(direction_pin[i], OUTPUT);
    }

    for (int i=0; i<NSTEPPERS; i++) {
        pinMode(step_pin[i], OUTPUT);
    }
    
    EEPROM.put(0, 0L);
}

long angle_to_pulse(float deg, int idx)
{
    return (long)((pulse_degree_ratio[idx] / 360.0) * deg);
    //return (long)(deg * pulse_degree_ratio[idx]);
}

int serial_read_sync(char *ch, int secs)
{
    int  ret;
    int  seconds = 0;
    while (1) {
        ret = Serial.readBytes(ch, 1);
        if (ret) {
            return 0;
        } else {
            seconds += 1;
            if (secs && seconds >= secs) {
                return 2;
            }
        }
    }
    return 1;
}

int angle_to_pulse_with_check(float deg, int idx, long *pulse)
{
    int ret = 0;

    if (deg < degree_limits[idx][0]) {
        deg = degree_limits[idx][0];
        ret = idx + 1;
    }
    if (deg > degree_limits[idx][1]) {
        deg = degree_limits[idx][1];
        ret = idx + 1;
    }
    *pulse = angle_to_pulse(deg, idx);

    return ret;
}

int timeout = 120;

int read_angles()
{
    float degs[NSTEPPERS];
    long  angles[NSTEPPERS];
    char  ch = 0; // fake intial value
    char  tch;
    long  count = 0;
    int   ret = 0;
    int   address = 0;

    while (ch != 's' && ch != 'd' && ch != 'h') {
        ret = serial_read_sync(&ch, timeout);
        if (ret == 2) {
            // Timeout then write hardcoded strokes
            return 2;
        }
    }
    // timeout mode is disabled
    timeout = 0;

    if (ch == 'd') {
        echo_done(0);
        return 1;
    }

    // Perform hardcoded strokes
    if (ch == 'h') {
        echo_done(0);
        return 2;
    }

    // ch should be 's', now to read in the angles
    serial_read_sync(&tch, 0);
    count = Serial.parseInt();

    if (count > MAX_TRIPLES) {
        EEPROM.put(address, (long)MAX_TRIPLES);
    } else {
        EEPROM.put(address, count);
    }
    address += sizeof(count);
    echo_done(0);

    for (long k=0; k<count; k++) {
        ret = 0;
        for (int i=0; i<NSTEPPERS; i++) {
            serial_read_sync(&tch, 0);
            serial_read_sync(&tch, 0);
            degs[i] = Serial.parseFloat();

            ret = angle_to_pulse_with_check(degs[i], i, &angles[i]);

            if (k < MAX_TRIPLES) {
                EEPROM.put(address, angles[i]);
                address += sizeof(angles[i]);
            }
        }
        echo_done(ret);
    }

    return 0;
}

void pulse_stepper(int idx, long delta, int dir)
{
    digitalWrite(enable_pin[idx], LOW);
    digitalWrite(direction_pin[idx], dir);

    for (long i=0; i<delta; i++) {
        digitalWrite(step_pin[idx], HIGH);
        delayMicroseconds(HALF_PULSE);
        digitalWrite(step_pin[idx], LOW);
        delayMicroseconds(HALF_PULSE);
    }
}

void change_to_angle(long angles[])
{
    long pulse_done[NSTEPPERS] = {0, 0, 0, 0};

    long pulse_delta[NSTEPPERS];
    int  pulse_dir[NSTEPPERS];

    for (int j=0; j<NSTEPPERS; j++) {
        if (angles[j] >= real_angles[j]) {
            pulse_dir[j] = HIGH;
        } else {
            pulse_dir[j] = LOW;
        }
        pulse_delta[j] = abs(angles[j] - real_angles[j]);
    }

    long steps = pulse_delta[0];
    for (int j=1; j<NSTEPPERS; j++) {
        if (steps < pulse_delta[j]) {
            steps = pulse_delta[j];
        }
    }
    if (steps >= STEP_DIVIDE) {
        steps /= STEP_DIVIDE;
    }

    for (long i=0; i<steps; i++) {
        for (int j=0; j<NSTEPPERS; j++) {
            long interim = map(i, 0, steps, 0, pulse_delta[j]);
            if (interim > pulse_done[j]) {
                pulse_stepper(j, interim - pulse_done[j], pulse_dir[j]);
                pulse_done[j] = interim;
            }
        }
    }
    // Finish remainders
    for (int j=0; j<NSTEPPERS; j++) {
        if (pulse_delta[j] > pulse_done[j]) {
            pulse_stepper(j, pulse_delta[j] - pulse_done[j], pulse_dir[j]);
        }
    }

    // Save new angles
    for (int j=0; j<NSTEPPERS; j++) {
        real_angles[j] = angles[j];
    }
}

void echo_done (int ret)
{
    Serial.print((char)('0' + ret));
}

void dump_real_angles ()
{
    char prefix[] = {'a', 'b', 'c', 'd'};
    for (int j=0; j<NSTEPPERS; j++) {
        Serial.print(prefix[j]);
        Serial.print('=');
        Serial.print(real_angles[j]);
        Serial.print(',');
    }
    Serial.print('\n');
}

void run_angles()
{
    long angles[NSTEPPERS];
    long count;
    int  address = 0;

    EEPROM.get(address, count);
    address += sizeof(count);
    for (long i=0; i<count; i++) {
        for (int j=0; j<NSTEPPERS; j++) {
            EEPROM.get(address, angles[j]);
            address += sizeof(angles[j]);
        }
        change_to_angle(&angles[0]);
    }
}

void run_hardcoded()
{
    long offset = 0;
    long start = pgm_get_far_address(hardcoded);
    long count = 0;
    float degs[NSTEPPERS];
    long angles[NSTEPPERS];

    int first = 1;
    while (count = (long)pgm_read_float_far(start + offset)) {
        offset += sizeof(float);
        for (long i = 0; i < count; i ++) {
            for (long k = 0; k < NSTEPPERS; k ++) {
                degs[k] = pgm_read_float_far(start + offset);
                angle_to_pulse_with_check(degs[k], k, &angles[k]);

                offset += sizeof(float);
            }

            change_to_angle(&angles[0]);
        }

        if (first) {
            delay(5000);
            first = 0;
        }
    }
}

void loop()
{
    int cmd = read_angles();
    switch (cmd) {
        case 1:
            run_angles();
            break;
        case 2:
            run_hardcoded();
            break;
        default:
            break;
    }
}
