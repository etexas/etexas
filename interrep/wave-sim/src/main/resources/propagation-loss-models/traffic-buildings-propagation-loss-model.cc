/* -*-  Mode: C++; c-file-style: "gnu"; indent-tabs-mode:nil; -*- */
/*
* Copyright (c) 2011 Centre Tecnologic de Telecomunicacions de Catalunya (CTTC)
*
* This program is free software; you can redistribute it and/or modify
* it under the terms of the GNU General Public License version 2 as
* published by the Free Software Foundation;
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*
* Author: Marco Miozzo <marco.miozzo@cttc.es>
*         Nicola Baldo <nbaldo@cttc.es>
*
*/
	
#include <cmath>
	
#include "ns3/log.h"
#include "ns3/mobility-model.h"
#include "ns3/double.h"
#include "ns3/pointer.h"
#include "ns3/okumura-hata-propagation-loss-model.h"
#include "ns3/itu-r-1411-los-propagation-loss-model.h"
#include "ns3/itu-r-1411-nlos-over-rooftop-propagation-loss-model.h"
#include "ns3/itu-r-1238-propagation-loss-model.h"
#include "ns3/kun-2600-mhz-propagation-loss-model.h"
#include <ns3/mobility-building-info.h>
#include "ns3/building.h"
#include "ns3/enum.h"
	
#include "traffic-buildings-propagation-loss-model.h"
	
	
namespace ns3 {
	
NS_LOG_COMPONENT_DEFINE("TrafficBuildingsPropagationLossModel");
	
NS_OBJECT_ENSURE_REGISTERED(TrafficBuildingsPropagationLossModel);
	
		
/*
* Constructor for the TrafficBuildingsPropagationLossModel
*/		
TrafficBuildingsPropagationLossModel::TrafficBuildingsPropagationLossModel()
{
	m_okumuraHata = CreateObject<OkumuraHataPropagationLossModel>();
}

/*
* Gets the TypeId
* These attibutes have default values unless changed by the user
*
* @return TypeId - returns the TypeId
*/	
TypeId
TrafficBuildingsPropagationLossModel::GetTypeId(void)
{
	static TypeId tid = TypeId("ns3::TrafficBuildingsPropagationLossModel")
		
		.SetParent<TrafficPropagationLossModel>()
			
		.AddConstructor<TrafficBuildingsPropagationLossModel>()
		.SetGroupName("Buildings")
			
		.AddAttribute("Frequency",
				      "The Frequency  (default is 2.106 GHz).",
				      DoubleValue(2160e6),
				      MakeDoubleAccessor(&TrafficBuildingsPropagationLossModel::SetFrequency),
				      MakeDoubleChecker<double>())
			
		.AddAttribute("Environment",
				      "Environment Scenario",
				      EnumValue(UrbanEnvironment),
				      MakeEnumAccessor(&TrafficBuildingsPropagationLossModel::SetEnvironment),
				      MakeEnumChecker(UrbanEnvironment, "Urban",
					  SubUrbanEnvironment, "SubUrban",
					  OpenAreasEnvironment, "OpenAreas"))
			
		.AddAttribute("CitySize",
				      "Dimension of the city",
				      EnumValue(LargeCity),
				      MakeEnumAccessor(&TrafficBuildingsPropagationLossModel::SetCitySize),
				      MakeEnumChecker(SmallCity, "Small",
					  MediumCity, "Medium",
					  LargeCity, "Large"))
			
		.AddAttribute("RooftopLevel",
				      "The height of the rooftop level in meters",
				      DoubleValue(20.0),
				      MakeDoubleAccessor(&TrafficBuildingsPropagationLossModel::SetRooftopHeight),
				      MakeDoubleChecker<double>(0.0, 90.0))
			
		;
		
	return tid;
}
	
void
/*
* Sets the environment attribute for models that use the Enviroment attribute
*
* @param env the new environment type
*/
TrafficBuildingsPropagationLossModel::SetEnvironment(EnvironmentType env)
{
	m_okumuraHata->SetAttribute("Environment", EnumValue(env));
}

/*
* Sets the city size attribute for models that use the city size attribute
*
* @param size the new CitySize
*/
void
TrafficBuildingsPropagationLossModel::SetCitySize(CitySize size)
{
	m_okumuraHata->SetAttribute("CitySize", EnumValue(size));
}

/*
* Sets the frequency attribute for models that use the frequency attribute
*
* @param freq the new frequency
*/
void
TrafficBuildingsPropagationLossModel::SetFrequency(double freq)
{
	m_okumuraHata->SetAttribute("Frequency", DoubleValue(freq));
	m_frequency = freq;
}

/*
* Sets the rooftop height attribute for models that use the roof top height attribute
*
* @param rooftopHeight the new rooftop height
*/
void
TrafficBuildingsPropagationLossModel::SetRooftopHeight(double rooftopHeight)
{
	m_rooftopHeight = rooftopHeight;
}

/*
* Sets the buildings container for the simulation
*
* @param Buildings The list of buildings to be used
*/
void ns3::TrafficBuildingsPropagationLossModel::SetBuildings(BuildingContainer Buildings)
{
	m_buildings = Buildings;
}

/*
* This method multiplys the vector by the scalar and returns the new vector
*
* @param value the vector to be multiplied
* @param scaleFactor the scalar to be used to multiply the vector
* @return Vector the vector that is the result of the muliplication
*/
Vector TrafficBuildingsPropagationLossModel::MultiplyVScale(Vector value, double scaleFactor)const
{
	value.x *= scaleFactor;
	value.y *= scaleFactor;
	value.z *= scaleFactor;
	return value;
}

/*
* This method subtracts the second vector from the first vector
*
* @param value1 the vector to be subtracted from
* @param value2 the vector to be subtracted
* @return Vector the vector that is the result of the subtraction
*/
Vector TrafficBuildingsPropagationLossModel::MinusV(Vector value1, Vector value2)const
{
	value1.x -= value2.x;
	value1.y -= value2.y;
	value1.z -= value2.z;
	return value1;
}

/*
* This method adds two vectors together
*
* @param value1 the first vector to be added
* @param value2 the second vector to be added
* @return Vector the vector that is the result of the addition
*/
Vector TrafficBuildingsPropagationLossModel::AddV(Vector value1, Vector value2)const
{
	value1.x += value2.x;
	value1.y += value2.y;
	value1.z += value2.z;
	return value1;
}

/*
* This method gets the intersection point if one exists
*
* @param f1 the value of node1 minus the buildings value at a corresponding x,y,z. min or max
* @param f2 the value of node2 minus the buildings value at a corresponding x,y,z. min or max
* @param P1 the vector node1
* @param P2 the vector node2
* @param Hit a reference to the location of an intersection if there is one
* @return bool true if there is an intersection, false otherwise
*/
bool TrafficBuildingsPropagationLossModel::GetIntersection(float f1, float f2, Vector P1, Vector P2, Vector &Hit)const
{
	if ((f1 * f2) >= 0.0f) {
		return false;
	}
	if (f1 == f2) {
		return false;
	}
	//Base Equation: Hit = P1 + (P2 - P1) * (-fDst1 / (fDst2 - fDst1));
	//P2 - P1
	Hit = AddV(P1, MultiplyVScale(MinusV(P2, P1), -f1 / (f2 - f1)));
	return true;
}

/*
* Tests to see if the intersection is inside the building
* 
* @param Hit the intersection point
* @param building the building to be checked
* @param Axis the axis where the intersection was found
* @return bool true if the intersection is inside the building, otherwise false
*/
bool TrafficBuildingsPropagationLossModel::InBuilding(Vector Hit, Box building, int Axis)const
{
	//Checks the other two axises to see if the intersection is within the building
	if (Axis == 1 && Hit.z > building.zMin && Hit.z < building.zMax && Hit.y > building.yMin && Hit.y < building.yMax) return true;
	if (Axis == 2 && Hit.z > building.zMin && Hit.z < building.zMax && Hit.x > building.xMin && Hit.x < building.xMax) return true;
	if (Axis == 3 && Hit.x > building.xMin && Hit.x < building.xMax && Hit.y > building.yMin && Hit.y < building.yMax) return true;
	return false;
}

/*
* The main logic for checking if there is a building between two nodes
*
* @param building the building to check
* @param node1 the first node
* @param node2 the second node
* @return true if there is a building between the two nodes or false otherwise
*/
bool TrafficBuildingsPropagationLossModel::IsBuildingBetweenNodes(Box& building, Vector& node1, Vector& node2)const
{
	NS_LOG_INFO("BOX:\nxMin: " << building.xMin << "\nyMin: " << building.yMin << "\nzMin: " << building.zMin << "\nxMax: " << building.xMax << "\nyMax: " << building.yMax << "\nzMax: " << building.zMax);
	NS_LOG_INFO("node1:\nx:" << node1.x << "\ny:" << node1.y << "\nz:" << node1.z);
	NS_LOG_INFO("node1:\nx:" << node2.x << "\ny:" << node2.y << "\nz:" << node2.z);
	Vector Hit;

	//These six if statements check to see if the two points are outside the box
	//for example: node1.x = -1, node2.x = -1, building.xMin = 0 this building would
	//not be inbetween these two nodes since they are both to the left of the building
	if (node2.x < building.xMin && node1.x < building.xMin) {
		return false;
	}
	if (node2.x > building.xMax && node1.x > building.xMax) {
		return false;
	}
	if (node2.y < building.yMin && node1.y < building.yMin) {
		return false;
	}
	if (node2.y > building.yMax && node1.y > building.yMax) {
		return false;
	}
	if (node2.z < building.zMin && node1.z < building.zMin) {
		return false;
	}
	if (node2.z > building.zMax && node1.z > building.zMax) {
		return false;
	}
	//Checks to see if node1 is inside of the building
	if (node1.x > building.xMin && node1.x < building.xMax &&
		node1.y > building.yMin && node1.y < building.yMax &&
		node1.z > building.zMin && node1.z < building.zMax)
	{
		return true;
	}
	//Checks each plane of intersection and then checks if that intersection is in the building or not
	if ((GetIntersection(node1.x - building.xMin, node2.x - building.xMin, node1, node2, Hit) && InBuilding(Hit, building, 1))
		|| (GetIntersection(node1.y - building.yMin, node2.y - building.yMin, node1, node2, Hit) && InBuilding(Hit, building, 2))
		|| (GetIntersection(node1.z - building.zMin, node2.z - building.zMin, node1, node2, Hit) && InBuilding(Hit, building, 3))
		|| (GetIntersection(node1.x - building.xMax, node2.x - building.xMax, node1, node2, Hit) && InBuilding(Hit, building, 1))
		|| (GetIntersection(node1.y - building.yMax, node2.y - building.yMax, node1, node2, Hit) && InBuilding(Hit, building, 2))
		|| (GetIntersection(node1.z - building.zMax, node2.z - building.zMax, node1, node2, Hit) && InBuilding(Hit, building, 3)))
		return true;

	return false;
}

/*
* This gets the propagation loss for the model
*
* @param a the first node
* @param b the second node
* @return double the propagation loss that has been calculated
*/		
double
TrafficBuildingsPropagationLossModel::GetLoss(Ptr<MobilityModel> a, Ptr<MobilityModel> b) const
{

	//The logic for this method is not finished yet.
	NS_ASSERT_MSG((a->GetPosition().z >= 0) && (b->GetPosition().z >= 0), "TrafficBuildingsPropagationLossModel does not support underground nodes (placed at z < 0)");
	bool buildingBetweenNodes = false;
	double loss = 0.0;	
	
	//This loops through the Building Container and checks to see if the two nodes are between any of the buildings.
	//The loop will end when either a building is found to be between them or goes through all the buildings and none of the buildings were between the two nodes
	for (BuildingContainer::Iterator it = m_buildings.Begin(); it != m_buildings.End() && !buildingBetweenNodes; ++it)
	{
		
		NS_LOG_INFO("in the loop\nSize of list: " << m_buildings.GetN() << "\n");
		Building building = **it;
		Vector node1 = a->GetPosition();
		Vector node2 = b->GetPosition();
		Box box = building.GetBoundaries();
		buildingBetweenNodes = IsBuildingBetweenNodes(box, node1, node2);
		if (buildingBetweenNodes)
		{
			loss += TrafficPropagationLossModel::WallLoss(*it);
			NS_LOG_INFO("bbn: true\nLoss: " << loss);
		} else NS_LOG_INFO("bbn: false");
		
	}
	if (buildingBetweenNodes)
	{
		loss += 4; //need to get the building wall loss here
	}		
		
	loss = OkumuraHata(a, b);
	loss = std::max(loss, 0.0);
		
	return loss;
}

/*
* This method gets the propagation loss from the okumuraHata model
* 
* @param a the first node
* @param b the second node
* @return double the propagation loss
*/
double
TrafficBuildingsPropagationLossModel::OkumuraHata(Ptr<MobilityModel> a, Ptr<MobilityModel> b) const
{
	return m_okumuraHata->GetLoss(a, b);
}
			
} // namespace ns3
