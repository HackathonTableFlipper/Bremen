#include "HX711.h"
#include <iostream>

int main(void)
{
	struct sched_param param;
	HX711 scale;

	param.sched_priority = sched_get_priority_max(SCHED_FIFO);
	sched_setscheduler(0, SCHED_FIFO, &param);
	scale.begin(21, 22);
	while(1)
	{
		printf("%ld\n", scale.read());
	}
	return 0;
}