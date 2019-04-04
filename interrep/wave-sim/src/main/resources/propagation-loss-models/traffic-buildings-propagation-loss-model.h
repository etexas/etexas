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
* Author: Marco Miozzo  <marco.miozzo@cttc.es>
*         Nicola Baldo <nbaldo@cttc.es>
*
*/
	
#ifndef TRAFFIC_BUILDINGS_PROPAGATION_LOSS_MODEL_H_
#define TRAFFIC_BUILDINGS_PROPAGATION_LOSS_MODEL_H_
	
#include <ns3/traffic-propagation-loss-model.h>
#include <ns3/propagation-environment.h>
#include <ns3/building-container.h>
#include "ns3/enum.h"
	
namespace ns3 {
	
class OkumuraHataPropagationLossModel;
class ItuR1411LosPropagationLossModel;
class ItuR1411NlosOverRooftopPropagationLossModel;
class ItuR1238PropagationLossModel;
class Kun2600MhzPropagationLossModel;
	
class TrafficBuildingsPropagationLossModel : public TrafficPropagationLossModel
{
		
public:
	static TypeId GetTypeId(void);
	TrafficBuildingsPropagationLossModel();
	~TrafficBuildingsPropagationLossModel();
				
					
	void SetEnvironment(EnvironmentType env);
			
	void SetCitySize(CitySize size);
			
	void SetFrequency(double freq);
				
	void SetRooftopHeight(double rooftopHeight);

	void SetBuildings(BuildingContainer Buildings);
				
	virtual double GetLoss(Ptr<MobilityModel> a, Ptr<MobilityModel> b) const;				
								
private:
	Vector MultiplyVScale(Vector value, double scaleFactor)const;
	Vector MinusV(Vector value1, Vector value2)const;
	Vector AddV(Vector value1, Vector value2)const;
	//This is not a traffic intersection
	bool GetIntersection(float f1, float f2, Vector P1, Vector P2, Vector &Hit)const;
	bool InBuilding(Vector Hit, Box building, int Axis)const;
	bool IsBuildingBetweenNodes(Box& building, Vector& node1, Vector& node2)const;

	double OkumuraHata(Ptr<MobilityModel> a, Ptr<MobilityModel> b) const;
						
	Ptr<OkumuraHataPropagationLossModel> m_okumuraHata;
							
	double m_itu1411NlosThreshold;
	double m_rooftopHeight;
	double m_frequency;
	BuildingContainer m_buildings;
						
};
			
}

#endif /* TRAFFIC_BUILDINGS_PROPAGATION_LOSS_MODEL_H_ */