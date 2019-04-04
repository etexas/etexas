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
#include "ns3/lte-module.h"

#include "ns3/point-to-point-helper.h"

#include <iostream>
#include <fstream>
#include <vector>
#include <string>

#include <unistd.h>

/**
 * NS3 simulator for use in etexas which currenlty has LTE and DSRC communications.
 *
 * @author ttevendale
 */

NS_LOG_COMPONENT_DEFINE("WifiSimpleAdhoc");

using namespace std;
using namespace ns3;

const string DSRC_TYPE = "DSRC";
const string LTE_TYPE = "LTE";

Ipv4InterfaceContainer i;

int totalLteNodes;
int totalDsrcNodes;
int txNum;
int indexOfFirstLteNode;
ostringstream output;
int handshakeWaitTimeMS = 400;
string type;

struct EtexasNode {

  long mac;
  double x; // Should be passed in as meters
  double y; // Should be passed in as meters
  double z; // Should be passed in as meters
};

/**
 * The method which ns3 uses when a packet is sent.
 *
 * @param socket the socket.
 * @param limit the limit.
 */
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

/**
 * The method which ns3 uses when a packet is received by a node.
 *
 * @param socket the socket.
 */
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

  int timeInMS;
  if (type == LTE_TYPE) {

    // Due to the UEs in the lte module needing time to attach to the cell towers
    // the timeInMS will be off by the handshakeWaitTimeMS.
    timeInMS = t.GetMilliSeconds() - handshakeWaitTimeMS;
  }
  else {

    timeInMS = t.GetMilliSeconds();
  }

  FlowIdTag tag;
  packet->FindFirstMatchingByteTag(tag);

  ostringstream oss;
  oss << "Packet received after " << t.GetMilliSeconds() << " milliseconds, by node " << socket->GetNode()->GetId()
      << ", from address " << isa << " (node " << fromIndex << ").";
  oss << " Packet size was " << packet->GetSize() << " bytes." << " Transmission ID was: " << tag.GetFlowId();

  output << timeInMS << "," << tag.GetFlowId() << "," << socket->GetNode()->GetId() << ";";

  NS_LOG_INFO(oss.str());

  cerr << "leaving receive callback." << endl;
}

/**
 * This method parses the cell towers passed in.
 *
 * @param length the number of cell towers to process.
 * @param cellTowers The structured list which will hold the nodes.
 * @param nodesStr the string which contains the nodes to be parsed.
 */
string parseNodes(int length, struct EtexasNode nodes[], string nodesStr) {

  istringstream ss(nodesStr);
  for (int i = 0; i < length; i++) {

    string token;
    getline(ss, token, ',');
    nodes[i].mac = atol(token.c_str());

    getline(ss, token, ',');
    nodes[i].x = atof(token.c_str());

    getline(ss, token, ',');
    nodes[i].y = atof(token.c_str());

    getline(ss, token, ';');
    nodes[i].z = atof(token.c_str());
  }
  getline(ss, nodesStr);
  return nodesStr;
}

/**
 * This method setups up the environmnet variables for the ns-3 LteHelper class based on the configs passed in.
 *
 * @param length the number of LTE configurations to process.
 * @param configs the string which contains the LTE configurations to be parsed.
 */
void setupLTEEnvironment(int length, string configs) {

  istringstream ss(configs);
  for (int i = 0; i < length; i++) {

    string token;
    getline(ss, token, ',');
    string configName = token.c_str();

    getline(ss, token, ';');
    string configValue = token.c_str();

    if (configName == "PathlossModel") {

      Config::SetDefault("ns3::LteHelper::PathlossModel", StringValue(configValue));
      cerr << "Path loss Model updated with " << configValue << endl;
    }
    else if (configName == "UplinkBandwidth") {

      Config::SetDefault("ns3::LteEnbNetDevice::UlBandwidth", StringValue(configValue));
      cerr << "Uplink Bandwidth updated with " << configValue << endl;
    }
    else if (configName == "DownlinkBandwidth") {

      Config::SetDefault("ns3::LteEnbNetDevice::DlBandwidth", StringValue(configValue));
      cerr << "Downlink Bandwidth updated with " << configValue << endl;
    }
    else if (configName == "UplinkFrequency") {

      Config::SetDefault("ns3::LteEnbNetDevice::UlEarfcn", StringValue(configValue));
      cerr << "Uplink Frequency updated with " << configValue << endl;
    }
    else if (configName == "DownlinkFrequency") {

      Config::SetDefault("ns3::LteEnbNetDevice::DlEarfcn", StringValue(configValue));
      cerr << "Downlink Frequency updated with " << configValue << endl;
    }
    else if (configName == "CellTxPower") {

      Config::SetDefault("ns3::LteUePhy::TxPower", StringValue(configValue));
      cerr << "Cell Tx Power updated with " << configValue << endl;
    }
    else if (configName == "CellNoiseFigure") {

      Config::SetDefault("ns3::LteUePhy::NoiseFigure", StringValue(configValue));
      cerr << "Cell Noise Figure updated with " << configValue << endl;
    }
    else if (configName == "CellTowerTxPower") {

      Config::SetDefault("ns3::LteEnbPhy::TxPower", StringValue(configValue));
      cerr << "Cell Tower Tx Power updated with " << configValue << endl;
    }
    else if (configName == "CellTowerNoiseFigure") {

      Config::SetDefault("ns3::LteEnbPhy::NoiseFigure", StringValue(configValue));
      cerr << "Cell Tower Noise Figure updated with " << configValue << endl;
    }
  }
}

/**
 * This method parses out the next transmission from the list of transmissions.
 *
 * @param ss the stream of the transmissions that are being parsed out.
 * @param destination the string which will hold the destination address that gets parsed out.
 * @param size the string which will hold the size of the message that gets parsed out.
 * @param id the string which will hold the tx id that gets parsed out.
 * @return a boolean of whether there are more transmissions or not.
 */
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

/**
 * This method processes transmissions for each node
 *
 * @param c the Node container that will be processed.
 * @param ss the input stream that will hold the transmissions to be processed.
 * @param offset the offset to be applied to the destIndex.
 */
void processTransmissions(NodeContainer &c, istringstream &ss, int offset) {

  TypeId tid = TypeId::LookupByName("ns3::UdpSocketFactory");

  string destination("");
  string size("");
  string id("");
  int total = 0;
  if (type == DSRC_TYPE) {

    total = totalDsrcNodes;
  }
  else if (type == LTE_TYPE) {

    cerr << "Starting simulator for a bit to allow UEs to attach to the tower(s)." << endl;
    Simulator::Stop(MilliSeconds(handshakeWaitTimeMS));
    Simulator::Run();
    cerr << "Now parsing and performing sends, before starting simulator again." << endl;
    total = totalLteNodes;
  }

  cerr << "about to loop over nodes." << endl;
  for (int srcIndex = 0; srcIndex < total; srcIndex++) {

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

        cerr << "didn't break. about to create and send sockets and packets." << endl;
        cerr << "dest: " << destIndex << endl;

        if (destIndex < 0) {

          Ptr < Socket > source = Socket::CreateSocket(c.Get(srcIndex), tid);
          InetSocketAddress local = InetSocketAddress(Ipv4Address::GetBroadcast(), 80);
          source->SetAllowBroadcast(true);
          source->Connect(local);
          source->SetSendCallback(MakeCallback(&SendPacket));

          Ptr < Packet > pkt = Create < Packet > (packetSize);

          //The tag which will hold the txId to the receiving device.
          FlowIdTag tag;
          tag.SetFlowId(txId);
          pkt->AddByteTag(tag);

          cerr << "created socket and packet for broadcast. about to send." << endl;

          source->Send(pkt);

          cerr << "finished sending." << endl;

        }
        else {

          cerr << "entered else state Process" << endl;
          Ptr < Socket > source = Socket::CreateSocket(c.Get(srcIndex), tid);
          InetSocketAddress remote = InetSocketAddress(i.GetAddress(destIndex - offset, 0), 80);
          source->Connect(remote);
          source->SetSendCallback(MakeCallback(&SendPacket));
          Ptr < Packet > pkt = Create < Packet > (packetSize);
          cerr << "after packet creation" << endl;

          //The tag which will hold the txId to the receiving device.
          FlowIdTag tag;
          tag.SetFlowId(txId);
          pkt->AddByteTag(tag);
          cerr << "after addByteTag" << endl;

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

}

/**
 * This method setups the DSRC nodes and runs the simulation
 *
 * @param phyMode the physical mode of the DSRC
 * @param dsrcNodes the nodes to be used in the simulation (mac, x, y, z).
 * @param ss the input stream that holds the transmissions
 */
void runDsrc(string phyMode, struct EtexasNode dsrcNodes[], istringstream &ss) {

  // disable fragmentation for frames below 2200 bytes
  Config::SetDefault("ns3::WifiRemoteStationManager::FragmentationThreshold", StringValue("2200"));
  // turn off RTS/CTS for frames below 2200 bytes
  Config::SetDefault("ns3::WifiRemoteStationManager::RtsCtsThreshold", StringValue("2200"));
  // Fix non-unicast data rate to be the same as that of unicast
  Config::SetDefault("ns3::WifiRemoteStationManager::NonUnicastMode", StringValue(phyMode));

  cerr << "about to create helpers." << endl;

  NodeContainer c;
  c.Create(totalDsrcNodes);

  // The below set of helpers will help us to put together the wifi NICs we want
  WifiHelper wifi;

  wifi.SetStandard(WIFI_PHY_STANDARD_80211_10MHZ);

  YansWifiPhyHelper wifiPhy = YansWifiPhyHelper::Default();

  YansWifiChannelHelper wifiChannel = YansWifiChannelHelper::Default();
  wifiPhy.SetChannel(wifiChannel.Create());

  // Add a non-QoS upper mac, and disable rate control
  NqosWifiMacHelper wifiMac = NqosWifiMacHelper::Default();
  wifi.SetStandard(WIFI_PHY_STANDARD_80211b);
  wifi.SetRemoteStationManager("ns3::ConstantRateWifiManager", "DataMode", StringValue(phyMode), "ControlMode",
      StringValue(phyMode));

  // Set it to adhoc mode
  wifiMac.SetType("ns3::AdhocWifiMac");
  NetDeviceContainer devices = wifi.Install(wifiPhy, wifiMac, c);

  // Mobility Model
  MobilityHelper mobility;
  Ptr < ListPositionAllocator > positionAlloc = CreateObject<ListPositionAllocator>();
  for (int ind = 0; ind < totalDsrcNodes; ind++) {

    positionAlloc->Add(Vector(dsrcNodes[ind].x, dsrcNodes[ind].y, dsrcNodes[ind].z));
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

  type = DSRC_TYPE;
  processTransmissions(c, ss, 0);

  Simulator::Run();

  cerr << "about to destroy stuff." << endl;

  Simulator::Destroy();

  // Output what we are doing
  NS_LOG_INFO("Ended simulation.");

  cerr << "finished running simulator. DSRC" << endl;
}

/**
 * This method setups the LTE nodes and runs the simulation
 *
 * @param lteNodes the nodes to be used in the simulation (mac, x, y, z).
 * @param ss the input stream that holds the transmissions.
 * @param cellTowers an array that holds the cell towers for the simulation.
 * @param cellTowerNum an integer that contains the number of cellTowers.
 */
void runLte(struct EtexasNode lteNodes[], istringstream &ss, struct EtexasNode cellTowers[], int cellTowerNum) {

  cerr << "about to create helpers." << endl;

  Ptr < LteHelper > lteHelper = CreateObject<LteHelper>();
  Ptr < PointToPointEpcHelper > epcHelper = CreateObject<PointToPointEpcHelper>();

  lteHelper->SetEpcHelper(epcHelper);

  // Set up point-to-point helper

  Ptr < Node > pgw = epcHelper->GetPgwNode();

  cerr << "Creating point to point helper." << endl;

  PointToPointHelper p2ph;
  p2ph.SetDeviceAttribute("DataRate", DataRateValue(DataRate("100Gb/s")));
  p2ph.SetDeviceAttribute("Mtu", UintegerValue(1500));
  p2ph.SetChannelAttribute("Delay", TimeValue(Seconds(0.010)));
  Ipv4AddressHelper ipv4h;
  ipv4h.SetBase("10.1.1.0", "255.255.255.0");

  // Create nodes
  NodeContainer c;
  NodeContainer enbNodes;

  c.Create(totalLteNodes);
  enbNodes.Create(cellTowerNum);

  // Mobility Model
  MobilityHelper mobility;
  Ptr < ListPositionAllocator > positionAlloc = CreateObject<ListPositionAllocator>();

  // Sets the positions of the LTE nodes
  for (int ind = txNum - totalLteNodes; ind < txNum; ind++) {

    positionAlloc->Add(Vector(lteNodes[ind].x, lteNodes[ind].y, lteNodes[ind].z));
  }

  // Sets the positions of the cell towers
  for (int ind = 0; ind < cellTowerNum; ind++) {

    positionAlloc->Add(Vector(cellTowers[ind].x, cellTowers[ind].y, cellTowers[ind].z));
  }
  mobility.SetPositionAllocator(positionAlloc);
  mobility.SetMobilityModel("ns3::ConstantPositionMobilityModel");
  mobility.Install(c);
  mobility.Install(enbNodes);

  cerr << "Created mobility model." << endl;

  // Create LTE devices
  NetDeviceContainer enbDevs = lteHelper->InstallEnbDevice(enbNodes);
  NetDeviceContainer ueDevs = lteHelper->InstallUeDevice(c);

  // Internet stuff
  InternetStackHelper internet;
  internet.Install(c);

  // assign IP addresses to UEs

  i = epcHelper->AssignUeIpv4Address(NetDeviceContainer(ueDevs)); // Ipv4InterfaceContainer

  lteHelper->Attach(ueDevs);

  cerr << "Finished managing IP addresses." << endl;
  type = LTE_TYPE;
  int offset = 0;
  if (indexOfFirstLteNode > 0 and indexOfFirstLteNode < txNum) {

    offset = indexOfFirstLteNode;
  }
  processTransmissions(c, ss, offset);

  Simulator::Stop(Seconds(0.1));
  Simulator::Run();

  cerr << "about to destroy stuff." << endl;

  Simulator::Destroy();

  // Output what we are doing
  NS_LOG_INFO("Ended simulation.");

  cerr << "finished running simulator. LTE" << endl;
}

/**
 * The main method for the wave-sime-lte class
 *
 * @param argc the number of arguments
 * @param argv the array of arguments
 */
int main(int argc, char *argv[]) {

  cerr << "Starting wave-sim." << endl;

  string phyMode("DsssRate1Mbps");
  string nodesStr("");
  string txsStr("");
  string cellTowersStr("");
  string lteConfigsStr("");
  int cellTowerNum = 0;
  int lteConfigNum = 0;
  txNum = 0;
  totalDsrcNodes = 0;
  totalLteNodes = 0;
  indexOfFirstLteNode = -1;
  double rss = -80; // -dBm
  uint32_t packetSize = 1000; // bytes
  bool verbose = false;

  CommandLine cmd;

  cmd.AddValue("indexOfFirstLteNode", "The index that the first LTE will be at", indexOfFirstLteNode);
  cmd.AddValue("phyMode", "Wifi Phy mode", phyMode);
  cmd.AddValue("rss", "received signal strength", rss);
  cmd.AddValue("packetSize", "size of application packet sent", packetSize);
  cmd.AddValue("verbose", "turn on all WifiNetDevice log components", verbose);
  cmd.AddValue("txNum", "An integer representing the number of transactions to expect.", txNum);
  cmd.AddValue("nodes", "Semicolon-separated list nodes (MAC, x, y, z).", nodesStr);
  cmd.AddValue("txs",
      "Semicolon-separated list transmissions (destination:size:id) (destination values represent indices into the nodes array which starts at zero).",
      txsStr);
  cmd.AddValue("cellTowerNum", "An integer representing the number of cell towers to expect.", cellTowerNum);
  cmd.AddValue("cellTowers", "Semicolon-seperated list of cell towers (MAC, x, y, z).", cellTowersStr);
  cmd.AddValue("lteConfigNum", "An integer representing the number of LTE Configurations to expect.", lteConfigNum);
  cmd.AddValue("lteConfigs",
      "Semicolon-separated list of configurations for the LTE environment in key, value pairs (PathlossModel, UplinkBandwidth, DownlinkBandwidth, UplinkFrequency, DownlinkFrequency, CellTxPower, CellNoiseFigure, CellTowerTxPower, CellTowerNoiseFigure)",
      lteConfigsStr);

  cmd.Parse(argc, argv);

  // Figures out how many DSRC and LTE nodes there are
  if (indexOfFirstLteNode >= 0) {

    totalLteNodes = txNum - indexOfFirstLteNode;
    totalDsrcNodes = txNum - totalLteNodes;
  }
  else {

    totalDsrcNodes = txNum;
  }

  cerr << "Got command line arguments." << endl << "txNum=" << txNum << endl << "nodesStr=" << nodesStr << endl
      << "txsStr=" << txsStr << endl << "cellTowersStr=" << cellTowersStr << endl << "lteConfigs=" << lteConfigsStr
      << endl;

  struct EtexasNode cellTowers[cellTowerNum];
  struct EtexasNode dsrcNodes[totalDsrcNodes];
  struct EtexasNode lteNodes[totalLteNodes];
  parseNodes(cellTowerNum, cellTowers, cellTowersStr);
  string remaningNodesStr = parseNodes(totalDsrcNodes, dsrcNodes, nodesStr);
  parseNodes(totalLteNodes, lteNodes, remaningNodesStr);

  if (verbose) {

    LogComponentEnable("WifiSimpleAdhoc", LOG_LEVEL_INFO);
  }

  istringstream ss(txsStr);

  if (totalDsrcNodes != 0) {

    runDsrc(phyMode, dsrcNodes, ss);
  }

  // If there are no cell towers none of the messages will be received.
  if (totalLteNodes != 0 and cellTowerNum != 0) {

    setupLTEEnvironment(lteConfigNum, lteConfigsStr);
    runLte(lteNodes, ss, cellTowers, cellTowerNum);
  }

  cout << output.str();
  NS_LOG_INFO(output.str());

  cout.flush();
  usleep(11000); //give the output reader a chance to wake up and check for the last bit of output

  return 0;
}

