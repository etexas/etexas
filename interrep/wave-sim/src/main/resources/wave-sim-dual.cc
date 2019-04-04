/* -*-  Mode: C++; c-file-style: "gnu"; indent-tabs-mode:nil; -*- */
/*******************************************************************************
 * Copyright (c) 2012-2016 Harmonia Holdings Group LLC and others.
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
// ./waf --run "wave-sim-dual --txNum=2 --nodes=1,0,0,0;2,1,0,0 --txs=-1:1000:0;-1:1000:1"
//
// This sends a message from node 1 to nodes 2 and 3.
// ./waf --run "wave-sim-dual --txNum=3 --nodes=1,0,0,0;2,1,0,0;3,2,0,0 --txs=2:1000:0,3:1000:1;"
//
// nodes 1 & 2 are DSRC and nodes 3 & 4 are LTE
// node 1 sends a message to node 2 and broadcasts a message
// node 2 sends a message to node 1
// node 3 sends a message to node 4
// ./waf --run "wave-sim-dual --txNum=4 --indexOfFirstLteNode=2 --nodes=1,0,0,0;2,1,0,0;3,2,0,0;4,3,0,0 --txs=2:1000:0,-1:1000:1;1:1000:2;4:500:3;"
//
// same as above except there is an extra DSRC node to check that broadcast works NOTE: LTE does not work with broadcast
// ./waf --run "wave-sim-dual --txNum=5 --indexOfFirstLteNode=3 --nodes=1,0,0,0;2,1,0,0;12,5,0,0;3,2,0,0;4,3,0,0 --txs=2:1000:0,-1:1000:1;1:1000:2;;4:500:3,-1:1000:4;"
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

NS_LOG_COMPONENT_DEFINE("DualNodeAdhoc");

using namespace std;
using namespace ns3;

Ipv4InterfaceContainer i;

int totalLteNodes;
int totalDsrcNodes;
int txNum;
ostringstream output;
int handshakeWaitTimeMS = 400;
string type;

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

  int timeInMS;
  if (type == "LTE") {

    timeInMS = t.GetMilliSeconds() - handshakeWaitTimeMS;
  }
  else {

    timeInMS = t.GetMilliSeconds();
  }

  TransmissionIdTag tag;
  packet->RemovePacketTag(tag);

  ostringstream oss;
  oss << "Packet received after " << t.GetMilliSeconds() << " milliseconds, by node " << socket->GetNode()->GetId()
      << ", from address " << isa << " (node " << fromIndex << ").";
  oss << " Packet size was " << packet->GetSize() << " bytes." << " Transmission ID was: " << tag.getId();
  output << timeInMS << "," << tag.getId() << "," << socket->GetNode()->GetId() << ";";

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
/**
 * This method gets the index in the macArray where the mac is located
 *
 * @param macArray The array to be searched
 * @param mac The mac to be searched for
 * @return this will return the index that was found or throw an exception
 */
int getMacIndex(long *macArray, int mac) {

  for (int ind = 0; ind < txNum; ind++) {

    ostringstream oss("");
    oss << "macArray ind: " << macArray[ind] << "\nmac: " << mac << "\n";
    NS_LOG_INFO(oss.str());
    if (mac == (int) macArray[ind]) {

      return ind;
    }
  }
  throw "Not a valid Mac Address";
}

/**
 * This method processes transmissions for each node
 *
 * @param c the Node container that will be processed
 * @param ss the input stream that will hold the transmissions to be processed
 * @param macArray the array that holds the mac addresses
 */
void processTransmissions(NodeContainer &c, istringstream &ss, long *macArray) {

  TypeId tid = TypeId::LookupByName("ns3::UdpSocketFactory");

  string destination("");
  string size("");
  string id("");
  int total = 0;
  if (type == "DSRC") {

    total = totalDsrcNodes;
  }
  else if (type == "LTE") {

    cerr << "Starting simulator for a bit to allow UEs to attach to the tower." << endl;
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

        int dest = atoi(destination.c_str());
        int packetSize = atoi(size.c_str());
        int txId = atoi(id.c_str());

        cerr << "didn't break. about to create and send sockets and packets." << endl;

        if (dest < 0) {

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
          InetSocketAddress remote = InetSocketAddress(i.GetAddress(getMacIndex(macArray, dest), 0), 80);
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
}

/**
 * This method setups the DSRC nodes and runs the simulation
 *
 * @param phyMode the physical mode of the DSRC
 * @param macArray an array that holds the mac address
 * @param xArray an array that holds the x position of the nodes
 * @param yArray an array that holds the y position of the nodes
 * @param zArray an array that holds the z position of the nodes
 * @param ss the input stream that holds the transmissions
 */
void runDsrc(string phyMode, long *macArray, double *xArray, double *yArray, double *zArray, istringstream &ss) {

	// disable fragmentation for frames below 2200 bytes
  Config::SetDefault("ns3::WifiRemoteStationManager::FragmentationThreshold", StringValue("2200"));
  // turn off RTS/CTS for frames below 2200 bytes
  Config::SetDefault("ns3::WifiRemoteStationManager::RtsCtsThreshold", StringValue("2200"));
  // Fix non-unicast data rate to be the same as that of unicast
  Config::SetDefault("ns3::WifiRemoteStationManager::NonUnicastMode", StringValue(phyMode));

  cerr << "about to create helpers." << endl;

  // The below set of helpers will help us to put together the wifi NICs we want
  YansWifiPhyHelper wifiPhy = YansWifiPhyHelper::Default();

  // TODO: janway - once we use all 7 channels, the control channel should have power of 44.8 dBm instead of 23.0.
  wifiPhy.Set("TxPowerStart", DoubleValue(23.0));
  wifiPhy.Set("TxPowerEnd", DoubleValue(23.0));
  wifiPhy.Set("TxPowerLevels", UintegerValue(1));
  wifiPhy.Set("TxGain", DoubleValue(0.0));
  wifiPhy.Set("RxGain", DoubleValue(0.0));

  YansWifiChannelHelper wifiChannel = YansWifiChannelHelper::Default();
  Ptr < YansWifiChannel > channel = wifiChannel.Create();
  wifiPhy.SetChannel(channel);

  NodeContainer c;
  c.Create(totalDsrcNodes);

  NqosWaveMacHelper wifi80211pMac = NqosWaveMacHelper::Default();
  Wifi80211pHelper wifi80211p = Wifi80211pHelper::Default();

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
  for (int ind = 0; ind < totalDsrcNodes; ind++) {

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
  type = "DSRC";
  processTransmissions(c, ss, macArray);
  Simulator::Run();

  cerr << "about to destroy stuff." << endl;

  Simulator::Destroy();

  // Output what we are doing
  NS_LOG_INFO("Ended simulation.");
  cerr << "finished running simulator. DSRC" << endl;
  NS_LOG_INFO(output.str());
}

/**
 * This method setups the LTE nodes and runs the simulation
 *
 * @param macArray an array that holds the mac address
 * @param xArray an array that holds the x position of the nodes
 * @param yArray an array that holds the y position of the nodes
 * @param zArray an array that holds the z position of the nodes
 * @param ss the input stream that holds the transmissions
 */
void runLte(long *macArray, double *xArray, double *yArray, double *zArray, istringstream &ss) {

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
  enbNodes.Create(1);


	// Mobility Model
  MobilityHelper mobility;
  Ptr < ListPositionAllocator > positionAlloc = CreateObject<ListPositionAllocator>();
  for (int ind = txNum - totalLteNodes; ind < txNum; ind++) {

    positionAlloc->Add(Vector(xArray[ind], yArray[ind], zArray[ind]));
  }
  positionAlloc->Add(Vector(0, 0, 0)); // location of enb node
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
  type = "LTE";
  processTransmissions(c, ss, macArray);

  Simulator::Stop(Seconds(0.1));
  Simulator::Run();

  cerr << "about to destroy stuff." << endl;

  Simulator::Destroy();

  // Output what we are doing
  NS_LOG_INFO("Ended simulation.");

  cerr << "finished running simulator. LTE" << endl;
}

int main(int argc, char *argv[]) {

  cerr << "Starting wave-sim-dual." << endl;

  string phyMode("OfdmRate6MbpsBW10MHz");
  string nodesStr("");
  string txsStr("");
  txNum = 0;
  totalDsrcNodes = 0;
  totalLteNodes = 0;
  int indexOfFirstLteNode = -1;
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
      "Semicolon-separated list transmissions (destination:size:id) (destination values represent the MAC address of the nodes).",
      txsStr);

  cmd.Parse(argc, argv);

  long macArray[txNum];
  double xArray[txNum];
  double yArray[txNum];
  double zArray[txNum];

  cerr << "Got command line arguments." << endl << "txNum=" << txNum << endl << "nodesStr=" << nodesStr << endl
      << "txsStr=" << txsStr << endl;

  parseNodes(txNum, macArray, xArray, yArray, zArray, nodesStr);

  // Firgures out how many total DSRC and LTE nodes there are
  if (indexOfFirstLteNode >= 0) {

    totalLteNodes = txNum - indexOfFirstLteNode;
    totalDsrcNodes = txNum - totalLteNodes;
  }
  else {

    totalDsrcNodes = txNum;
  }

  // if verbose is true this turns on logging messages at info level
  if (verbose) {

    LogComponentEnable("DualNodeAdhoc", LOG_LEVEL_INFO);
  }

  istringstream ss(txsStr);
  ostringstream oss("");

  if (totalDsrcNodes != 0) {

    // makes an array of DSRC macs
    long dsrcMacArray[totalDsrcNodes];
    for (int ind = 0; ind < totalDsrcNodes; ind++) {

      dsrcMacArray[ind] = macArray[ind];
      oss << "dsrcMacArray: " << dsrcMacArray[ind] << "\n";
      NS_LOG_INFO(oss.str());
      oss.clear();
    }
    runDsrc(phyMode, dsrcMacArray, xArray, yArray, zArray, ss);
  }
  if (totalLteNodes != 0) {

    // makes an array of LTE macs
    long lteMacArray[totalLteNodes];
    for (int ind = 0; ind < totalLteNodes; ind++) {

      lteMacArray[ind] = macArray[ind + indexOfFirstLteNode];
      oss << "lteMacArray: " << lteMacArray[ind] << "\n";
      NS_LOG_INFO(oss.str());
      oss.clear();
    }
    runLte(lteMacArray, xArray, yArray, zArray, ss);
  }

  cout << output.str();
  NS_LOG_INFO(output.str());

  cout.flush();
  usleep(11000); //give the output reader a chance to wake up and check for the last bit of output

  return 0;
}
