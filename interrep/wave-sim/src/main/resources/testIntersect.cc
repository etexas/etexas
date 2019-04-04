
#include "ns3/core-module.h"
#include "ns3/mobility-module.h"

#include <iostream>
#include <string>

#include <unistd.h>

using namespace std;
using namespace ns3;

Vector MultiplyVScale(Vector value, double scaleFactor)
{
	value.x *= scaleFactor;
	value.y *= scaleFactor;
	value.z *= scaleFactor;
	return value;
}
Vector MinusV(Vector value1, Vector value2)
{
	value1.x -= value2.x;
	value1.y -= value2.y;
	value1.z -= value2.z;
	return value1;
}
Vector AddV(Vector value1, Vector value2)
{
	value1.x += value2.x;
	value1.y += value2.y;
	value1.z += value2.z;
	return value1;
}
bool GetIntersection(float f1, float f2, Vector P1, Vector P2, Vector &Hit)
{
	cout << "GetIntersection\n";
	if ((f1 * f2) >= 0.0f){
		cout << "f1*f2 >= 0.0 " << f1 << " " << f2 << "\n";
		return false;
	}
	if (f1 == f2){
		cout << "f1==f2 " << f1 << " " << f2 << "\n";
		return false;
	}
	cout << f1 << " " << f2 << "\n";
	//Base Equation: Hit = P1 + (P2 - P1) * (-fDst1 / (fDst2 - fDst1));
	//P2 - P1
	Hit = AddV(P1, MultiplyVScale(MinusV(P2, P1), -f1 / (f2 - f1)));
	return true;
}

bool InBox(Vector Hit, Box building, int Axis)
{
	cout << Hit.x << "," << Hit.y << "," << Hit.z;
	if (Axis == 1 && Hit.z > building.zMin && Hit.z < building.zMax && Hit.y > building.yMin && Hit.y < building.yMax) return true;
	if (Axis == 2 && Hit.z > building.zMin && Hit.z < building.zMax && Hit.x > building.xMin && Hit.x < building.xMax) return true;
	if (Axis == 3 && Hit.x > building.xMin && Hit.x < building.xMax && Hit.y > building.yMin && Hit.y < building.yMax) return true;
	cout << "   InBox is false and axis is: " << Axis << "\n";
	return false;
}
bool CheckLineBox(Box building, Vector node1, Vector node2)
{
	Vector Hit;
	if (node2.x < building.xMin && node1.x < building.xMin){
	 cout << "to the left\n";
	 return false;
	}
	if (node2.x > building.xMax && node1.x > building.xMax){
	 cout << "to the right\n";
	 return false;
	}
	if (node2.y < building.yMin && node1.y < building.yMin){
		cout << "in front\n";
		return false;
	}
	if (node2.y > building.yMax && node1.y > building.yMax) {
		cout << "behind\n";
		return false;
	}
	if (node2.z < building.zMin && node1.z < building.zMin) {
		cout << "below zMin:" << building.zMin << " l2.z:" << node2.z << " l1.z:" << node1.z << "\n";
		return false;
	}
	if (node2.z > building.zMax && node1.z > building.zMax) {
		cout << "above\n";
		return false;
	}
	if (node1.x > building.xMin && node1.x < building.xMax &&
		node1.y > building.yMin && node1.y < building.yMax &&
		node1.z > building.zMin && node1.z < building.zMax)
	{
		cout << "L1 is in the box";
		return true;
	}
	if ((GetIntersection(node1.x - building.xMin, node2.x - building.xMin, node1, node2, Hit) && InBox(Hit, building, 1))
		|| (GetIntersection(node1.y - building.yMin, node2.y - building.yMin, node1, node2, Hit) && InBox(Hit, building, 2))
		|| (GetIntersection(node1.z - building.zMin, node2.z - building.zMin, node1, node2, Hit) && InBox(Hit, building, 3))
		|| (GetIntersection(node1.x - building.xMax, node2.x - building.xMax, node1, node2, Hit) && InBox(Hit, building, 1))
		|| (GetIntersection(node1.y - building.yMax, node2.y - building.yMax, node1, node2, Hit) && InBox(Hit, building, 2))
		|| (GetIntersection(node1.z - building.zMax, node2.z - building.zMax, node1, node2, Hit) && InBox(Hit, building, 3)))
		return true;

	return false;
}

int main() 
{
	cout << "testing simple\n";
	if (CheckLineBox(Box(-1, 3, 2, 7, 0, 0), Vector(0, 0, 0), Vector(5, 5, 0)))
	{
		cout << "is in the box\n";
	}else cout << "is not in the box\n";
}