/* -*-  Mode: C++; c-file-style: "gnu"; indent-tabs-mode:nil; -*- */
/*******************************************************************************
 * Copyright (c) 2012 Harmonia Holdings Group LLC and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Harmonia Partner License v1.0
 * which accompanies this distribution, and is available at
 * http://www.harmonia.com/legal/hpl-v10.html
 *
 * Contributors:
 *     Harmonia Holdings Group LLC - initial API and implementation
 *******************************************************************************/

//
// There are a number of command-line options available to control
// the default behavior.  The list of available command-line options
// can be listed with the following command:
// ./waf --run "wave-sim --help"
//
// To run use the following command line:
//
// Both nodes 1 and 2 broadcast messages.
// ./waf --run "wave-sim --txNum=2 --nodes=1,0,0,0;2,1,0,0 --txs=-1:1000:0;-1:1000:1"
//
// This sends a message from node 1 to nodes 2 and 3.
// ./waf --run "wave-sim --txNum=3 --nodes=1,0,0,0;2,1,0,0;3,2,0,0 --txs=1:1000:0,2:1000:1;;"
//
// You can disable logging by adding "export NS_LOG= && " before "./waf ..."
//
// Note that all ns-3 attributes (not just the ones exposed in the below
// script) can be changed at command line; see the documentation.
//
// NOTE: bbadillo - It may be possible to run this simulator in lock step with
// eTEXAS by simply scheduling events every time step and retrieving new nodes,
// logging out old nodes, and obtaining new transmissions to make via stdin.
//
#include "ns3/core-module.h"
#include "ns3/network-module.h"
#include "ns3/mobility-module.h"
#include "ns3/config-store-module.h"
#include "ns3/wifi-module.h"
#include "ns3/internet-module.h"
#include "ns3/wave-module.h"
#include "ns3/itu-r-1411-los-propagation-loss-model.h"
#include "ns3/okumura-hata-propagation-loss-model.h"
#include "ns3/propagation-loss-model.h"
#include "ns3/traffic-buildings-propagation-loss-model.h"
#include "ns3/building.h"
#include "ns3/building-container.h"

#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <ns3/propagation-loss-model.h>
#include <ns3/propagation-environment.h>

#include <unistd.h>

NS_LOG_COMPONENT_DEFINE("WifiSimpleAdhoc");

using namespace std;
using namespace ns3;

Ipv4InterfaceContainer i;
int txNum;
ostringstream output;

// tag definition borrowed from "IPv4TestTag". Couldn't figure out which import includes it, if any.
class TransmissionIdTag: public Tag {
private:
  uint64_t id;
  public:
  static TypeId GetTypeId() {
    static TypeId tid = TypeId("TransmissionIdTag").SetParent<Tag>().AddConstructor<TransmissionIdTag>();
    return tid;
  }
  virtual TypeId GetInstanceTypeId() const {
    return GetTypeId();
  }
  virtual uint32_t GetSerializedSize() const {
    return sizeof(id);
  }
  virtual void Serialize(TagBuffer buffer) const {
    buffer.WriteU64(id);
  }
  virtual void Deserialize(TagBuffer buffer) {
    id = buffer.ReadU64();
  }
  virtual void Print(std::ostream &os) const {
    os << "token=" << id;
  }
  void setId(uint64_t id) {
    this->id = id;
  }
  uint64_t getId() {
    return id;
  }

};

void SendPacket(Ptr<Socket> socket, uint32_t limit) {

  cerr << "entered send callback." << endl;

  Time t = Now();

  cerr << "got time: " << t.GetMilliSeconds() << endl;

  ostringstream oss;
  oss << "Sending from node: " << socket->GetNode()->GetId() << " after " << t.GetMilliSeconds() << " milliseconds.\n";

  cerr << "wrote to oss." << endl;
  NS_LOG_INFO(oss.str());

  cerr << "leaving send callback." << endl;
}

void ReceivePacket(Ptr<Socket> socket) {

  cerr << "entered receive callback." << endl;

  Address fromAddress;
  Ptr < Packet > packet = socket->RecvFrom(fromAddress);

  Ipv4Address isa = InetSocketAddress::ConvertFrom(fromAddress).GetIpv4();

  int fromIndex;
  for (fromIndex = 0; fromIndex < txNum; ++fromIndex) {

    if (isa == InetSocketAddress(i.GetAddress(fromIndex, 0), 80).GetIpv4()) {

      break;
    }
  }

  Time t = Now();

  TransmissionIdTag tag;
  packet->RemovePacketTag(tag);

  ostringstream oss;
  oss << "Packet received after " << t.GetMilliSeconds() << " milliseconds, by node " << socket->GetNode()->GetId()
      << ", from address " << isa << " (node " << fromIndex << ").";
  oss << " Packet size was " << packet->GetSize() << " bytes." << " Transmission ID was: " << tag.getId();

  output << t.GetMilliSeconds() << "," << tag.getId() << "," << socket->GetNode()->GetId() << ";";

  NS_LOG_INFO(oss.str());

  cerr << "leaving receive callback." << endl;
}

void parseNodes(int length, long macArray[], double xArray[], double yArray[], double zArray[], string s) {

  istringstream ss(s);
  for (int i = 0; i < length; i++) {
    string token;
    getline(ss, token, ',');
    macArray[i] = atol(token.c_str());

    getline(ss, token, ',');
    xArray[i] = atof(token.c_str()) / 100.0; // inputs are in centimeters. ns3 uses meters.
    cerr << xArray[i] << "\n";

    getline(ss, token, ',');
    yArray[i] = atof(token.c_str()) / 100.0;

    getline(ss, token, ';');
    zArray[i] = atof(token.c_str()) / 100.0;
  }
}

bool nextTransmission(istringstream &ss, string &destination, string &size, string &id) {

  destination.clear();
  size.clear();
  id.clear();

  string tx = "";
  char c;
  while (!ss.get(c).eof()) {

    if (c == ',') {

      istringstream sstemp(tx);
      getline(sstemp, destination, ':');
      getline(sstemp, size, ':');
      getline(sstemp, id);
      return false;
    }
    else if (c == ';') {

      istringstream sstemp(tx);
      getline(sstemp, destination, ':');
      getline(sstemp, size, ':');
      getline(sstemp, id);
      return true;
    }
    else {

      tx += c;
    }
  }

  if (tx.length() != 0) {

    istringstream sstemp(tx);
    getline(sstemp, destination, ':');
    getline(sstemp, size, ':');
    getline(sstemp, id);
  }
  return true;
}

void setupTx(long srcMac, double srcX, double srcY, double srcZ, long destMac, double destX, double destY,
    double destZ) {

  cerr << srcMac << " " << srcX << " " << srcY << " " << srcZ << " " << destMac << " " << destX << " " << destY << " "
      << destZ << endl;
}

int main(int argc, char *argv[]) {

  LogComponentEnable("WifiSimpleAdhoc", LOG_LEVEL_INFO);
  cerr << "Starting wave-sim-dsrc." << endl;

  string phyMode("OfdmRate6MbpsBW10MHz");
  string nodesStr("");
  string txsStr("");
  txNum = 0;
  double rss = -80; // -dBm
  uint32_t packetSize = 1000; // bytes
  bool verbose = true;

  CommandLine cmd;

  cmd.AddValue("phyMode", "Wifi Phy mode", phyMode);
  cmd.AddValue("rss", "received signal strength", rss);
  cmd.AddValue("packetSize", "size of application packet sent", packetSize);
  cmd.AddValue("verbose", "turn on all WifiNetDevice log components", verbose);
  cmd.AddValue("txNum", "An integer representing the number of transactions to expect.", txNum);
  // the z axis has to be > 0
  cmd.AddValue("nodes", "Semicolon-separated list nodes (MAC, x, y, z).", nodesStr);
  cmd.AddValue("txs",
      "Semicolon-separated list transmissions (destination:size:id) (destination values represent indices into the nodes array which starts at zero).",
      txsStr);

  cmd.Parse(argc, argv);

  long macArray[txNum];
  double xArray[txNum];
  double yArray[txNum];
  double zArray[txNum];

  cerr << "Got command line arguments." << endl << "txNum=" << txNum << endl << "nodesStr=" << nodesStr << endl
      << "txsStr=" << txsStr << endl;

  parseNodes(txNum, macArray, xArray, yArray, zArray, nodesStr);

  // disable fragmentation for frames below 2200 bytes
  Config::SetDefault("ns3::WifiRemoteStationManager::FragmentationThreshold", StringValue("2200"));
  // turn off RTS/CTS for frames below 2200 bytes
  Config::SetDefault("ns3::WifiRemoteStationManager::RtsCtsThreshold", StringValue("2200"));
  // Fix non-unicast data rate to be the same as that of unicast
  Config::SetDefault("ns3::WifiRemoteStationManager::NonUnicastMode", StringValue(phyMode));

  cerr << "about to create helpers." << endl;

  NodeContainer c;
  c.Create(txNum);

  // The below set of helpers will help us to put together the wifi NICs we want
  YansWifiPhyHelper wifiPhy = YansWifiPhyHelper::Default();

  // TODO: janway - once we use all 7 channels, the control channel should have power of 44.8 dBm instead of 23.0.
  wifiPhy.Set("TxPowerStart", DoubleValue(23.0));
  wifiPhy.Set("TxPowerEnd", DoubleValue(23.0));

  YansWifiChannelHelper wifiChannel = YansWifiChannelHelper::Default();
  Ptr < YansWifiChannel > channel = wifiChannel.Create();

  Ptr < TrafficBuildingsPropagationLossModel > trafficPropLoss = CreateObject<TrafficBuildingsPropagationLossModel>();
  Ptr < Building > b = CreateObject<Building>();
  b->SetBoundaries(Box(-1, 3, 2, 7, 0, 2));
  BuildingContainer buildingsList;
  Box box = b->GetBoundaries();
  cerr << "box.xMin!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!: " << box.xMin;
  buildingsList.Add(b);
  trafficPropLoss->SetBuildings(buildingsList);
  channel->SetPropagationLossModel(trafficPropLoss);

  wifiPhy.SetChannel(channel);

  NqosWaveMacHelper wifi80211pMac = NqosWaveMacHelper::Default();
  Wifi80211pHelper wifi80211p = Wifi80211pHelper::Default();
  if (verbose) {

    LogComponentEnable("WifiSimpleAdhoc", LOG_LEVEL_INFO);
  }
  LogComponentEnable("TrafficBuildingsPropagationLossModel", LOG_LEVEL_INFO);

  wifi80211p.SetRemoteStationManager("ns3::ConstantRateWifiManager", "DataMode", StringValue(phyMode), "ControlMode",
      StringValue(phyMode));
  NetDeviceContainer devices = wifi80211p.Install(wifiPhy, wifi80211pMac, c);

  // TODO: janway - Need to patch to enable multiple channels?

  /*    WifiHelper wifi;
   if (verbose) {
   wifi.EnableLogComponents(); // Turn on all Wifi logging
   }
   wifi.SetStandard(WIFI_PHY_STANDARD_80211_10MHZ);

   YansWifiPhyHelper wifiPhy = YansWifiPhyHelper::Default();

   YansWifiChannelHelper wifiChannel = YansWifiChannelHelper::Default();
   wifiPhy.SetChannel(wifiChannel.Create());

   // Add a non-QoS upper mac, and disable rate control
   NqosWifiMacHelper wifiMac = NqosWifiMacHelper::Default();
   wifi.SetStandard(WIFI_PHY_STANDARD_80211b);
   wifi.SetRemoteStationManager("ns3::ConstantRateWifiManager",
   "DataMode", StringValue(phyMode),
   "ControlMode", StringValue(phyMode));
   // Set it to adhoc mode
   wifiMac.SetType("ns3::AdhocWifiMac");
   NetDeviceContainer devices = wifi.Install(wifiPhy, wifiMac, c);
   */
  // Mobility Model
  MobilityHelper mobility;
  Ptr < ListPositionAllocator > positionAlloc = CreateObject<ListPositionAllocator>();
  for (int ind = 0; ind < txNum; ind++) {

    positionAlloc->Add(Vector(xArray[ind], yArray[ind], zArray[ind]));
  }
  mobility.SetPositionAllocator(positionAlloc);
  mobility.SetMobilityModel("ns3::ConstantPositionMobilityModel");
  mobility.Install(c);

  InternetStackHelper internet;
  internet.Install(c);

  Ipv4AddressHelper ipv4;
  NS_LOG_INFO("Assign IP Addresses.");
  ipv4.SetBase("10.1.1.0", "255.255.255.0");
  i = ipv4.Assign(devices);

  TypeId tid = TypeId::LookupByName("ns3::UdpSocketFactory");
  istringstream ss(txsStr);

  string destination("");
  string size("");
  string id("");

  cerr << "about to loop over nodes." << endl;

  for (int srcIndex = 0; srcIndex < txNum; srcIndex++) {

    // Setup node to receive
    Ptr < Socket > recvSink = Socket::CreateSocket(c.Get(srcIndex), tid);
    InetSocketAddress local = InetSocketAddress(i.GetAddress(srcIndex, 0), 80);
    recvSink->Bind(local);
    recvSink->SetRecvCallback(MakeCallback(&ReceivePacket));

    ostringstream oss;
    oss << "Node: " << srcIndex << " InetSocketAddress: " << local.GetIpv4() << "\n";
    NS_LOG_INFO(oss.str());

    cerr << "about to loop over txs for node." << endl;

    bool end = false;
    while (!end) {

      cerr << "about to parse next transmission." << endl;

      end = nextTransmission(ss, destination, size, id);

      cerr << "finished parsing next transmission." << endl;

      if (destination.empty() || size.empty() || id.empty()) {

        break;
      }
      else {

        int destIndex = atoi(destination.c_str());
        int packetSize = atoi(size.c_str());
        int txId = atoi(id.c_str());

        ostringstream oss2;
        oss2 << "srcIndex: " << srcIndex << " destIndex: " << destIndex << " packetSize: " << packetSize << " txId: "
            << txId << "\n";
        NS_LOG_INFO(oss2.str());

        cerr << "didn't break. about to create and send sockets and packets." << endl;

        if (destIndex < 0) {

          Ptr < Socket > source = Socket::CreateSocket(c.Get(srcIndex), tid);
          InetSocketAddress local = InetSocketAddress(Ipv4Address::GetBroadcast(), 80);
          source->SetAllowBroadcast(true);
          source->Connect(local);
          source->SetSendCallback(MakeCallback(&SendPacket));

          Ptr < Packet > pkt = Create < Packet > (packetSize);
          TransmissionIdTag tag;
          tag.setId(txId);
          pkt->AddPacketTag(tag);

          cerr << "created socket and packet for broadcast. about to send." << endl;

          source->Send(pkt);

          cerr << "finished sending." << endl;

        }
        else {

          Ptr < Socket > source = Socket::CreateSocket(c.Get(srcIndex), tid);
          InetSocketAddress remote = InetSocketAddress(i.GetAddress(destIndex, 0), 80);
          source->Connect(remote);
          source->SetSendCallback(MakeCallback(&SendPacket));

          Ptr < Packet > pkt = Create < Packet > (packetSize);
          TransmissionIdTag tag;
          tag.setId(txId);
          pkt->AddPacketTag(tag);

          cerr << "created socket and packet for unicast. about to send." << endl;

          source->Send(pkt);

          cerr << "finished sending." << endl;
        }
      }
    }
    cerr << "finished looping over txs for node." << endl;
  }
  cerr << "finished looping over nodes." << endl;

  // Output what we are doing
  NS_LOG_INFO("Starting simulation...");

  cerr << "about to run simulator." << endl;

  Simulator::Run();

  cerr << "about to destroy stuff." << endl;

  Simulator::Destroy();

  // Output what we are doing
  NS_LOG_INFO("Ended simulation.");

  cerr << "finished running simulator. DSRC" << endl;

  cout << output.str();
  NS_LOG_INFO(output.str());

  cout.flush();
  usleep(11000); //give the output reader a chance to wake up and check for the last bit of output

  return 0;
}

