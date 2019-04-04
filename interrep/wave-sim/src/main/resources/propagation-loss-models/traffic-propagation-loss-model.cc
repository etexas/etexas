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
* Author: Marco Miozzo  <marco.miozzo@cttc.es>,
*         Nicola Baldo <nbaldo@cttc.es>
*
*/
	
#include "ns3/propagation-loss-model.h"
#include "ns3/log.h"
#include "ns3/mobility-model.h"
#include "ns3/double.h"
#include "ns3/pointer.h"
#include <cmath>
#include "traffic-propagation-loss-model.h"
#include <ns3/mobility-building-info.h>
#include "ns3/enum.h"


namespace ns3 {

NS_LOG_COMPONENT_DEFINE("TrafficPropagationLossModel");
	
NS_OBJECT_ENSURE_REGISTERED(TrafficPropagationLossModel);

/*
* Constructor for the inner class ShadowingLoss
*/	
TrafficPropagationLossModel::ShadowingLoss::ShadowingLoss()
{
}

/*
* Sets the shadowing loss
*
* @param shadowingValue the new shadowing value
* @param receiver the receiver to recieve the shadowing value
*/
TrafficPropagationLossModel::ShadowingLoss::ShadowingLoss(double shadowingValue, Ptr<MobilityModel> receiver)
	: m_shadowingValue(shadowingValue), m_receiver(receiver)
{
	NS_LOG_INFO(this << " New Shadowing value " << m_shadowingValue);
}

/*
* Gets the shadowing loss
*
* @return double the shadowing loss
*/	
double
TrafficPropagationLossModel::ShadowingLoss::GetLoss() const
{
	return (m_shadowingValue);
}

/*
* Gets the receiver
*
* @return Ptr<MobilityModel> the receiver
*/	
Ptr<MobilityModel>
TrafficPropagationLossModel::ShadowingLoss::GetReceiver() const
{
	return m_receiver;
}

/*
* Gets the TypeId
* These attibutes have default values unless changed by the user
*
* @return TypeId - returns the TypeId
*/
TypeId
TrafficPropagationLossModel::GetTypeId(void)
{
	static TypeId tid = TypeId("ns3::TrafficPropagationLossModel")
			
	.SetParent<PropagationLossModel>()
	.SetGroupName("Traffic")
			
			
	.AddAttribute("ShadowSigmaOutdoor",
		                    "Standard deviation of the normal distribution used for calculate the shadowing for outdoor nodes",
		                    DoubleValue(7.0),
		                    MakeDoubleAccessor(&TrafficPropagationLossModel::m_shadowingSigmaOutdoor),
		                    MakeDoubleChecker<double>())
			
	.AddAttribute("ShadowSigmaWalls",
		                    "Standard deviation of the normal distribution used for calculate the shadowing due to ext walls ",
		                    DoubleValue(5.0),
		                    MakeDoubleAccessor(&TrafficPropagationLossModel::m_shadowingSigmaWalls),
		                    MakeDoubleChecker<double>());
			
		
			
	return tid;
}

/*
* The constructor for the TrafficPropagationLossModel
*/	
TrafficPropagationLossModel::TrafficPropagationLossModel()
{
	m_randVariable = CreateObject<NormalRandomVariable>();
}

/*
* returns the amount of wall loss based on the buildings wall type
*
* @param building the building to check the walls of
* @return double the amount of wall loss based on the buildings walls
*/	
double
TrafficPropagationLossModel::WallLoss(Ptr<Building> building) const
{
	double loss = 0.0;
	if (building->GetExtWallsType() == Building::Wood)
	{
		loss = 4;
	}
	else if (building->GetExtWallsType() == Building::ConcreteWithWindows)
	{
		loss = 7;
	}
	else if (building->GetExtWallsType() == Building::ConcreteWithoutWindows)
	{
		loss = 15; // 10 ~ 20 dB
	}
	else if (building->GetExtWallsType() == Building::StoneBlocks)
	{
		loss = 12;
	}
		return (loss);
}

/*
* Gets the shadowing loss
*
* @param a the first node
* @param b the second node
* @return double the shadowing loss
*/	
double
TrafficPropagationLossModel::GetShadowing(Ptr<MobilityModel> a, Ptr<MobilityModel> b) const
{
	std::map<Ptr<MobilityModel>, std::map<Ptr<MobilityModel>, ShadowingLoss> >::iterator ait = m_shadowingLossMap.find(a);
	if (ait != m_shadowingLossMap.end())
	{
		std::map<Ptr<MobilityModel>, ShadowingLoss>::iterator bit = ait->second.find(b);
		if (bit != ait->second.end())
		{
			return (bit->second.GetLoss());
		}
		else
		{
			double sigma = m_shadowingSigmaWalls;
			// side effect: will create new entry          
			// sigma is standard deviation, not variance
			double shadowingValue = m_randVariable->GetValue(0.0, (sigma*sigma));
			ait->second[b] = ShadowingLoss(shadowingValue, b);
			return (ait->second[b].GetLoss());
		}
	}
	else
	{
		double sigma = m_shadowingSigmaOutdoor;
		// side effect: will create new entries in both maps
		// sigma is standard deviation, not variance
		double shadowingValue = m_randVariable->GetValue(0.0, (sigma*sigma));
		m_shadowingLossMap[a][b] = ShadowingLoss(shadowingValue, b);
		return (m_shadowingLossMap[a][b].GetLoss());
	}
}	

/*
* Calculates the recieving power
*
* @param txPowerDbm the transmission power
* @param a the first node
* @param b the second node
* @return the recieved power
*/		
double
TrafficPropagationLossModel::DoCalcRxPower(double txPowerDbm, Ptr<MobilityModel> a, Ptr<MobilityModel> b) const
{
	return txPowerDbm - GetLoss(a, b) - GetShadowing(a, b);
}

/*
* Assigns streams
*
* @param stream the stream to be assigned
* @return int64_t returns 1
*/	
int64_t
TrafficPropagationLossModel::DoAssignStreams(int64_t stream)
{
	m_randVariable->SetStream(stream);
	return 1;
}	
		
		
} // namespace ns3