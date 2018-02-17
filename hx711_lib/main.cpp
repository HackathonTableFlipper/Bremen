#include "HX711.h"
#include <iostream>

int main(void)
{
	HX711 scale;
	scale.begin(3, 2);
	while(1)
	{
		//std::cout << scale.read() << std::endl;
	}
	return 0;
}