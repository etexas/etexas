/* -*-  Mode: C++; c-file-style: "gnu"; indent-tabs-mode:nil; -*- */
/*
* Copyright (c) 2011, 2012 Centre Tecnologic de Telecomunicacions de Catalunya (CTTC)
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
	
#ifndef TRAFFIC_PROPAGATION_LOSS_MODEL_H_
#define TRAFFIC_PROPAGATION_LOSS_MODEL_H_
	
#include "ns3/nstime.h"
#include "ns3/propagation-loss-model.h"
#include "ns3/random-variable-stream.h"
#include <ns3/building.h>
#include <ns3/mobility-building-info.h>
	
	
	
namespace ns3 {
	
class ShadowingLossModel;
class JakesFadingLossModel;
	
class TrafficPropagationLossModel : public PropagationLossModel
{
		
public:
	static TypeId GetTypeId(void);
		
	TrafficPropagationLossModel();
	virtual double GetLoss(Ptr<MobilityModel> a, Ptr<MobilityModel> b) const = 0;
			
	// inherited from PropagationLossModel
	virtual double DoCalcRxPower(double txPowerDbm, Ptr<MobilityModel> a, Ptr<MobilityModel> b) const;
		
protected:
	double WallLoss(Ptr<Building> a) const;
	
	double GetShadowing(Ptr<MobilityModel> a, Ptr<MobilityModel> b) const;
						
							
	class ShadowingLoss
	{
	public:
		ShadowingLoss();
		ShadowingLoss(double shadowingValue, Ptr<MobilityModel> receiver);
		double GetLoss() const;
		Ptr<MobilityModel> GetReceiver(void) const;
	protected:
		double m_shadowingValue;
		Ptr<MobilityModel> m_receiver;
	};
						
	mutable std::map<Ptr<MobilityModel>, std::map<Ptr<MobilityModel>, ShadowingLoss> > m_shadowingLossMap;
						
						
	double m_shadowingSigmaWalls;
	double m_shadowingSigmaOutdoor;
	Ptr<NormalRandomVariable> m_randVariable;
						
	virtual int64_t DoAssignStreams(int64_t stream);
};
			
}

#endif /* TRAFFIC_PROPAGATION_LOSS_MODEL_H_ */