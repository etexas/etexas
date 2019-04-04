/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
 * -
 * SBIR DATA RIGHTS
 * Harmonia Holdings Group, LLC
 * 2020 Kraft Drive Suite 2400
 * Blacksburg, VA 24060
 * Contract No: DTRT57-16-c-10008
 * Start Date: 01/05/2016
 * End Date: 01/05/2018
 * Expiration of SBIR Data Rights Period: 01/05/2022
 * -
 * The Government's rights to use, modify, reproduce, release, perform,
 * display, or disclose technical data or computer software marked with
 * this legend are restricted during the period shown as provided in
 * paragraph (b)(4) of the Rights in Noncommercial Technical Data and
 * Computer Software-Small Business Innovation Research (SBIR) Program
 * clause contained in the above identified contract. No restrictions
 * apply after the expiration date shown above. Any reproduction of
 * technical data, computer software, or portions thereof marked with
 * this legend must also reproduce the markings.
 * -
 * Contributors:
 * Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
package org.etexascode.test.restharness;

/**
 * Tests for the REST simulation, project, apps, devices, and detectors API.
 * 
 * @author jrutherford
 */
public class RESTSimulationTest {

    // /**
    // * The REST service name.
    // */
    // private static final String REST_SERVICE = "ExecService";
    //
    // /**
    // * The Wave service name.
    // */
    // private static final String WAVE_SERVICE = "WaveService";
    //
    // /**
    // * The REST user.
    // */
    // private static String REST_OWNER = "admin";
    //
    // /**
    // * Executes testing of all REST functions.
    // *
    // * @param hostLocation The host location.
    // * @param owner The owner.
    // */
    // @SuppressWarnings("unused")
    // public static void testAllFunctions(String hostLocation, String owner) throws
    // URISyntaxException, JAXBException {
    // REST_OWNER = owner;
    // String simId = testAddSimulation(hostLocation);
    // List<String> jarIds = testAddJarApps(hostLocation);
    // List<String> remoteIds = testAddRemoteApps(hostLocation);
    // List<Long> deviceIds = testAddDevices(hostLocation, simId);
    // List<String> appIds = testAddAppsToDevice(hostLocation, simId, deviceIds.get(1));
    // testRemoteApplicationData(hostLocation, simId);
    // List<Long> detectorIds = testAddDetectors(hostLocation, simId);
    // testCreateProject(hostLocation);
    // testAddSimulationsToProject(hostLocation, simId);
    //
    // testGetSimulationList(hostLocation);
    // testGetAppsList(hostLocation);
    // testGetDevicesList(hostLocation, simId);
    // testGetListOfAppsOnDevice(hostLocation, simId, deviceIds.get(0));
    // testGetListOfAppsAvailableForDevice(hostLocation, simId, deviceIds.get(0));
    // testGetListOfDetectors(hostLocation, simId);
    // testGetSimulationsForProject(hostLocation);
    // testGetListOfProjects(hostLocation);
    //
    // testRemoveSimulationsFromProject(hostLocation, simId);
    // testDeleteDetectors(hostLocation, simId, detectorIds);
    // testRemoveAppsFromDevice(hostLocation, simId, deviceIds.get(0), appIds);
    // testDeleteRemoteApps(hostLocation);
    // testDeleteJarApps(hostLocation);
    // testDeleteDevices(hostLocation, simId, deviceIds);
    // testDeleteSimulation(hostLocation, simId);
    // }
    //
    // public static void testVehicleInjection(String hostLocation, String owner) {
    // REST_OWNER = owner;
    //
    // Map<String, String> params = new HashMap<String, String>();
    // params.put("owner", REST_OWNER);
    // params.put("name", "testVehInjProj");
    // params.put("type", "TEXAS");
    // params.put("template", "exam_06");
    //
    // String simId = RESTUtils.executeResource(hostLocation, REST_SERVICE, "addSimulation", params,
    // String.class, null);
    // System.out.println("SimId: " + simId);
    //
    // String execId = RESTHarness.startSim(hostLocation, simId, 17);
    // VehicleInjectionCommand vic = new VehicleInjectionCommand(0.0, Vehicle.VEHICLE_TYPE.CAR,
    // 13.4, 5, 1, 2, 1, 13.4, 0.0);
    // List<VehicleInjectionCommand> lvic = new LinkedList<VehicleInjectionCommand>();
    // lvic.add(vic);
    // testAddVehicleInjectionCommand(hostLocation, execId, lvic);
    // RESTHarness.execAdvance(hostLocation, execId, 1);
    // RESTHarness.execAdvance(hostLocation, execId, 1);
    //
    // // Map<String, String> parameters = new HashMap<String, String>();
    // // parameters.put("id", execId);
    // // StepData sd = RESTUtils.executeResource(hostLocation, REST_SERVICE, "getStepData",
    // parameters, StepData.class);
    // // boolean injected = false;
    // // for (Vehicle v : sd.getVehicles()) {
    // // if (v.getVehicleID() < 0) {
    // // injected = true;
    // // }
    // // }
    // // System.out.println(String.format("Injected vehicle: %b\n", injected));
    // testDeleteSimulation(hostLocation, simId);
    // }
    //
    // /**
    // * Tests overloading the linux server with too many open files.
    // *
    // * @param hostLocation The host location.
    // */
    // public static void testServerFileOverloading(String hostLocation) {
    // List<String> simIds = new ArrayList<String>();
    // long start = System.currentTimeMillis();
    // for (int i = 0; i < 100; i++) {
    // System.out.println("Creating sim #" + (i + 1) + ".");
    // String simId = testAddSimulation(hostLocation);
    // simIds.add(simId);
    // }
    //
    // for (String simId : simIds) {
    // testDeleteSimulation(hostLocation, simId);
    // System.out.println("Removing Sim ID: " + simId);
    // }
    // long stop = System.currentTimeMillis();
    // double totalTime = (stop - start) / 1000 / 60;
    // System.out.println("Total Time: " + totalTime + " minutes.");
    // }
    //
    // /**
    // * Tests adding a simulation.
    // *
    // * @param hostLocation The host location.
    // * @return the simId.
    // */
    // public static String testAddSimulation(String hostLocation) {
    // System.out.println("testAddSimulation");
    // Map<String, String> params = new HashMap<String, String>();
    // params.put("owner", REST_OWNER);
    // params.put("name", "testProject");
    // params.put("type", "TEXAS");
    // params.put("template", "exam_06");
    //
    // String resp = RESTUtils.executeResource(hostLocation, REST_SERVICE, "addSimulation", params,
    // String.class, null);
    // System.out.println("SimId: " + resp);
    // System.out.println("End of Test.\n");
    // return resp;
    // }
    //
    // /**
    // * Test deleting a simulation.
    // *
    // * @param hostLocation The host location.
    // * @param simId The simulation Id.
    // */
    // public static void testDeleteSimulation(String hostLocation, String simId) {
    // System.out.println("testDeleteSimulation");
    // Map<String, String> params = new HashMap<String, String>();
    // params.put("owner", REST_OWNER);
    // params.put("id", simId);
    //
    // Boolean resp = Boolean.parseBoolean(RESTUtils.executeResource(hostLocation, REST_SERVICE,
    // "deleteSimulation", params, String.class));
    // System.out.println("Was Deleted: " + resp);
    // System.out.println("End of Test.\n");
    // }
    //
    // /**
    // * Test getting all the simulations.
    // *
    // * @param hostLocation The host location.
    // */
    // public static void testGetSimulationList(String hostLocation) {
    // System.out.println("testGetSimulationsList");
    // Map<String, String> params = new HashMap<String, String>();
    // params.put("owner", REST_OWNER);
    //
    // @SuppressWarnings("unchecked")
    // WrappedList<String> resp = RESTUtils.executeResource(hostLocation, REST_SERVICE,
    // "getSimulationsList", params, WrappedList.class);
    // System.out.println("Response: ");
    // for (String item : resp.getList()) {
    // System.out.println(item);
    // }
    // System.out.println("End of Test.\n");
    // }
    //
    // /**
    // * Tests adding jar applications.
    // *
    // * @param hostLocation The host location.
    // * @return List of applications IDs.
    // */
    // public static List<String> testAddJarApps(String hostLocation) throws URISyntaxException,
    // JAXBException {
    // System.out.println("testAddJarApps");
    // Map<String, String> params = new HashMap<String, String>();
    // WrappedList<Byte[]> jarWrapped = new WrappedList<Byte[]>();
    // List<Byte[]> jars = new ArrayList<Byte[]>();
    // params.put("owner", REST_OWNER);
    // params.put("jarName", "j2735-apps-1.0-SNAPSHOT.jar");
    // jarWrapped.setList(jars);
    //
    // try {
    // URL jar = RESTSimulationTest.class.getResource("j2735-apps-1.0-SNAPSHOT.jar");
    // RandomAccessFile raf = new RandomAccessFile(new File(jar.toURI()), "r");
    // byte data[] = new byte[(int)raf.length()];
    // raf.read(data, 0, (int)raf.length());
    // raf.close();
    // jars.add(ArrayUtils.toObject(data));
    // }
    // catch (FileNotFoundException e) {
    // e.printStackTrace();
    // }
    // catch (IOException e) {
    // e.printStackTrace();
    // }
    //
    // // // create JAXB context and instantiate marshaller
    // // JAXBContext context = JAXBContext.newInstance(WrappedList.class);
    // // Marshaller m = context.createMarshaller();
    // // m.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
    // //
    // // // Write to System.out
    // // m.marshal(jarWrapped, System.out);
    //
    // @SuppressWarnings("unchecked")
    // WrappedList<String> resp = RESTUtils.executeResource(hostLocation, REST_SERVICE,
    // "addJarApps", params, WrappedList.class, jarWrapped);
    // System.out.println("Response: ");
    // for (String item : resp.getList()) {
    // System.out.println(item);
    // }
    // System.out.println("End of Test.\n");
    // return resp.getList();
    // }
    //
    // /**
    // * Test add remote applications.
    // *
    // * @param hostLocation The host locations.
    // * @return List of remote application IDs.
    // */
    // public static List<String> testAddRemoteApps(String hostLocation) {
    // System.out.println("testAddRemoteApps");
    // Map<String, String> params = new HashMap<String, String>();
    // params.put("owner", REST_OWNER);
    //
    // AppRemoteData d1 = new AppRemoteData();
    // AppData appData = new AppData();
    // appData.setAppId("Remote-producer");
    // appData.setAppType(AppDefData.APPTYPE_REMOTE);
    // List<AppRemoteData> adata = new ArrayList<AppRemoteData>();
    // d1.setAppData(appData);
    // adata.add(d1);
    // WrappedList<AppRemoteData> data = new WrappedList<AppRemoteData>(adata);
    //
    // @SuppressWarnings("unchecked")
    // WrappedList<String> resp = RESTUtils.executeResource(hostLocation, REST_SERVICE,
    // "addRemoteApps", params, WrappedList.class, data);
    // System.out.println("Response: ");
    // for (String item : resp.getList()) {
    // System.out.println(item);
    // }
    // System.out.println("End of Test.\n");
    // return resp.getList();
    // }
    //
    // /**
    // * Test deleting jar applications.
    // *
    // * @param hostLocation The host location.
    // */
    // public static void testDeleteJarApps(String hostLocation) {
    // System.out.println("testDeleteJarApps");
    // Map<String, String> params = new HashMap<String, String>();
    // params.put("owner", REST_OWNER);
    // params.put("jarNames", "j2735-apps-1.0-SNAPSHOT.jar");
    //
    // @SuppressWarnings("unchecked")
    // WrappedList<String> resp = RESTUtils.executeResource(hostLocation, REST_SERVICE,
    // "deleteJarApps", params, WrappedList.class);
    // System.out.println("Response: ");
    // for (String item : resp.getList()) {
    // System.out.println(item);
    // }
    // System.out.println("End of Test.\n");
    // }
    //
    // /**
    // * Test delete remote applications.
    // *
    // * @param hostLocation The host locations.
    // */
    // public static void testDeleteRemoteApps(String hostLocation) {
    // System.out.println("testDeleteRemoteApps");
    // Map<String, String> params = new HashMap<String, String>();
    // params.put("owner", REST_OWNER);
    // params.put("appIds", "BSM-producer");
    //
    // @SuppressWarnings("unchecked")
    // WrappedList<String> resp = RESTUtils.executeResource(hostLocation, REST_SERVICE,
    // "deleteRemoteApps", params, WrappedList.class);
    //
    // //params.put("appIds", "test-app-id-2");
    // //RESTUtils.executeResource(hostLocation,REST_SERVICE, "deleteRemoteApps", params,
    // WrappedList.class);
    // System.out.println("Response: ");
    // for (String item : resp.getList()) {
    // System.out.println(item);
    // }
    // System.out.println("End of Test.\n");
    // }
    //
    // /**
    // * Test getting applications list.
    // *
    // * @param hostLocation The host location.
    // */
    // public static void testGetAppsList(String hostLocation) {
    // System.out.println("testGetAppsList");
    // Map<String, String> params = new HashMap<String, String>();
    // params.put("owner", REST_OWNER);
    //
    // @SuppressWarnings("unchecked")
    // WrappedList<AppDefData> resp = RESTUtils.executeResource(hostLocation, REST_SERVICE,
    // "getAppsList", params, WrappedList.class);
    // System.out.println("Response: ");
    // for (AppDefData item : resp.getList()) {
    // System.out.println(item.getAppData().getAppId());
    // }
    // System.out.println("End of Test.\n");
    // }
    //
    // /**
    // * Test adding devices.
    // *
    // * @param hostLocation The host location.
    // * @param simId The simulation id.
    // * @return The list of device IDs.
    // */
    // public static List<Long> testAddDevices(String hostLocation, String simId) {
    // System.out.println("testAddDevices");
    // Map<String, String> params = new HashMap<String, String>();
    // WrappedList<DeviceRule> devices = new WrappedList<DeviceRule>();
    // List<DeviceRule> list = new ArrayList<DeviceRule>();
    //
    // VehicleRule deviceRule = new VehicleRule();
    // deviceRule.setName("Test Vehicle Rule");
    //
    // PercentageRule deviceRule2 = new PercentageRule();
    // deviceRule2.setName("Test Percentage Rule");
    // deviceRule2.setPercentage(100.0);
    //
    // EquipmentRule deviceRule3 = new EquipmentRule();
    // deviceRule3.setName("Test Equipment Rule");
    //
    // list.add(deviceRule);
    // list.add(deviceRule2);
    // list.add(deviceRule3);
    // devices.setList(list);
    //
    // params.put("owner", REST_OWNER);
    // params.put("simId", simId);
    //
    // @SuppressWarnings("unchecked")
    // WrappedList<Long> resp = RESTUtils.executeResource(hostLocation, REST_SERVICE, "addDevices",
    // params, WrappedList.class, devices);
    // System.out.println("Response: ");
    // for (Long item : resp.getList()) {
    // System.out.println(item);
    // }
    // System.out.println("End of Test.\n");
    // return resp.getList();
    // }
    //
    // /**
    // * Test deleting devices.
    // *
    // * @param hostLocation The host location.
    // * @param simId The simulation id.
    // * @param deviceIds The device IDs.
    // */
    // public static void testDeleteDevices(String hostLocation, String simId, List<Long> deviceIds)
    // {
    // System.out.println("testDeleteDevices");
    // Map<String, String> params = new HashMap<String, String>();
    // params.put("owner", REST_OWNER);
    // params.put("simId", simId);
    // String queryParams = "deleteDevices?";
    // for (Long dev : deviceIds) {
    // queryParams = queryParams + "deviceIds=" + dev + "&";
    // }
    // queryParams = queryParams.substring(0, queryParams.length() - 1);
    //
    // @SuppressWarnings("unchecked")
    // WrappedList<Long> resp = RESTUtils.executeResource(hostLocation, REST_SERVICE, queryParams,
    // params, WrappedList.class);
    // System.out.println("Response: ");
    // for (Long item : resp.getList()) {
    // System.out.println(item);
    // }
    // System.out.println("End of Test.\n");
    // }
    //
    // /**
    // * Test getting devices list.
    // *
    // * @param hostLocation The host location.
    // * @param simId The simulation id.
    // */
    // public static void testGetDevicesList(String hostLocation, String simId) {
    // System.out.println("testGetDeviceList");
    // Map<String, String> params = new HashMap<String, String>();
    // params.put("owner", REST_OWNER);
    // params.put("simId", simId);
    //
    // @SuppressWarnings("unchecked")
    // WrappedList<Long> resp = RESTUtils.executeResource(hostLocation, REST_SERVICE,
    // "getDeviceList", params, WrappedList.class);
    // System.out.println("Response: ");
    // for (Long item : resp.getList()) {
    // System.out.println(item);
    // }
    // System.out.println("End of Test.\n");
    // }
    //
    // /**
    // * Test adding applications to device.
    // *
    // * @param hostLocation The host location.
    // * @param simId The simulation id.
    // * @param deviceId The device id.
    // * @return The list of application IDs.
    // */
    // public static List<String> testAddAppsToDevice(String hostLocation, String simId, Long
    // deviceId) {
    // System.out.println("testAddAppsToDevice");
    // Map<String, String> params = new HashMap<String, String>();
    // WrappedList<AppConfigData> apps = new WrappedList<AppConfigData>();
    // List<AppConfigData> list = new ArrayList<AppConfigData>();
    // List<AppConfigParam> parameters = new ArrayList<AppConfigParam>();
    // AppConfigParam p1 = new AppConfigParam();
    // p1.setName("frequency");
    // p1.setParam("1.0");
    // parameters.add(p1);
    // AppConfigData acd = new AppConfigData();
    // AppData ad = new AppData();
    // ad.setAppId("BSM-producer");
    // ad.setAppType(AppDefData.APPTYPE_REMOTE);
    // acd.setAppParams(parameters);
    // acd.setAppData(ad);
    // list.add(acd);
    // apps.setList(list);
    //
    // try {
    // URL jar = RESTSimulationTest.class.getResource("j2735-apps-1.0-SNAPSHOT.jar");
    // RandomAccessFile raf = new RandomAccessFile(new File(jar.toURI()), "r");
    // byte data[] = new byte[(int)raf.length()];
    // raf.read(data, 0, (int)raf.length());
    // raf.close();
    // FileData fileData = new FileData();
    // fileData.setFiles(data);
    // ad.setBinFile(fileData);
    // }
    // catch (FileNotFoundException e) {
    // e.printStackTrace();
    // }
    // catch (IOException e) {
    // e.printStackTrace();
    // }
    // catch (URISyntaxException e) {
    // e.printStackTrace();
    // }
    //
    // params.put("owner", REST_OWNER);
    // params.put("simId", simId);
    // params.put("deviceId", "" + deviceId);
    //
    // @SuppressWarnings("unchecked")
    // WrappedList<String> resp = RESTUtils.executeResource(hostLocation, REST_SERVICE,
    // "addAppsToDevice", params, WrappedList.class, apps);
    // System.out.println("Response: ");
    // for (String item : resp.getList()) {
    // System.out.println(item);
    // }
    // System.out.println("End of Test.\n");
    // return resp.getList();
    // }
    //
    // /**
    // * Test removing applications from device.
    // *
    // * @param hostLocation The host location.
    // * @param simId The simulation id.
    // * @param deviceId The device id.
    // * @param appIds The application ids.
    // */
    // public static void testRemoveAppsFromDevice(String hostLocation, String simId, Long deviceId,
    // List<String> appIds) {
    // System.out.println("testRemoveAppsFromDevice");
    // Map<String, String> params = new HashMap<String, String>();
    // params.put("owner", REST_OWNER);
    // params.put("simId", simId);
    // params.put("deviceId", "" + deviceId);
    // params.put("appIds", appIds.get(0));
    //
    // @SuppressWarnings("unchecked")
    // WrappedList<String> resp = RESTUtils.executeResource(hostLocation, REST_SERVICE,
    // "removeAppsFromDevice", params, WrappedList.class);
    // System.out.println("Response: ");
    // for (String item : resp.getList()) {
    // System.out.println(item);
    // }
    // System.out.println("End of Test.\n");
    // }
    //
    // /**
    // * Test getting devices list.
    // *
    // * @param hostLocation The host location.
    // * @param simId The simulation id.
    // * @param deviceId The device ID.
    // */
    // public static void testGetListOfAppsOnDevice(String hostLocation, String simId, Long
    // deviceId) {
    // System.out.println("testGetListOfAppsOnDevice");
    // Map<String, String> params = new HashMap<String, String>();
    // params.put("owner", REST_OWNER);
    // params.put("simId", simId);
    // params.put("deviceId", "" + deviceId);
    //
    // @SuppressWarnings("unchecked")
    // WrappedList<AppConfigData> resp = RESTUtils.executeResource(hostLocation, REST_SERVICE,
    // "getListOfAppsOnDevice", params, WrappedList.class);
    // System.out.println("Response: ");
    // for (AppConfigData item : resp.getList()) {
    // System.out.println(item.getAppData().getAppId());
    // }
    // System.out.println("End of Test.\n");
    // }
    //
    // /**
    // * Test getting devices list.
    // *
    // * @param hostLocation The host location.
    // * @param simId The simulation id.
    // * @param deviceId The device ID.
    // */
    // public static void testGetListOfAppsAvailableForDevice(String hostLocation, String simId,
    // Long deviceId) {
    // System.out.println("testGetListOfAppsAvailableForDevice");
    // Map<String, String> params = new HashMap<String, String>();
    // params.put("owner", REST_OWNER);
    // params.put("simId", simId);
    // params.put("deviceId", "" + deviceId);
    //
    // @SuppressWarnings("unchecked")
    // WrappedList<AppDefData> resp = RESTUtils.executeResource(hostLocation, REST_SERVICE,
    // "getListOfAppsAvailableForDevice", params, WrappedList.class);
    // System.out.println("Response: ");
    // for (AppDefData item : resp.getList()) {
    // System.out.println(item.getAppData().getAppId());
    // }
    // System.out.println("End of Test.\n");
    // }
    //
    // /**
    // * Test adding devices.
    // *
    // * @param hostLocation The host location.
    // * @param simId The simulation id.
    // * @return The list of detector IDs.
    // */
    // public static List<Long> testAddDetectors(String hostLocation, String simId) {
    // System.out.println("testAddDetectors");
    // Map<String, String> params = new HashMap<String, String>();
    // WrappedList<DetectorData> detecs = new WrappedList<DetectorData>();
    // List<DetectorData> list = new ArrayList<DetectorData>();
    // DetectorData uid1 = new DetectorData();
    // uid1.setWidth("180");
    // uid1.setHeight("180");
    // uid1.setLaneNum("3");
    // uid1.setDistFromStopLine("28000");
    // DetectorData uid2 = new DetectorData();
    // uid2.setWidth("500");
    // uid2.setHeight("500");
    // uid2.setLaneNum("21");
    // uid2.setDistFromStopLine("30000");
    // list.add(uid1);
    // list.add(uid2);
    // detecs.setList(list);
    //
    // params.put("owner", REST_OWNER);
    // params.put("simId", simId);
    //
    // @SuppressWarnings("unchecked")
    // WrappedList<Long> resp = RESTUtils.executeResource(hostLocation, REST_SERVICE,
    // "addDetectors", params, WrappedList.class, detecs);
    // System.out.println("Response: ");
    // for (Long item : resp.getList()) {
    // System.out.println(item);
    // }
    // System.out.println("End of Test.\n");
    // return resp.getList();
    // }
    //
    // /**
    // * Test deleting devices.
    // *
    // * @param hostLocation The host location.
    // * @param simId The simulation id.
    // * @param detectorIds The detector IDs.
    // */
    // public static void testDeleteDetectors(String hostLocation, String simId, List<Long>
    // detectorIds) {
    // System.out.println("testDeleteDetectors");
    // Map<String, String> params = new HashMap<String, String>();
    // params.put("owner", REST_OWNER);
    // params.put("simId", simId);
    // String queryParams = "deleteDetectors?";
    // for (Long dev : detectorIds) {
    // queryParams = queryParams + "detectors=" + dev + "&";
    // }
    // queryParams = queryParams.substring(0, queryParams.length() - 1);
    //
    // @SuppressWarnings("unchecked")
    // WrappedList<Long> resp = RESTUtils.executeResource(hostLocation, REST_SERVICE, queryParams,
    // params, WrappedList.class);
    // System.out.println("Response: ");
    // for (Long item : resp.getList()) {
    // System.out.println(item);
    // }
    // System.out.println("End of Test.\n");
    // }
    //
    // /**
    // * Test getting detectors list.
    // *
    // * @param hostLocation The host location.
    // * @param simId The simulation id.
    // */
    // public static void testGetListOfDetectors(String hostLocation, String simId) {
    // System.out.println("testGetListOfDetectors");
    // Map<String, String> params = new HashMap<String, String>();
    // params.put("owner", REST_OWNER);
    // params.put("simId", simId);
    //
    // @SuppressWarnings("unchecked")
    // WrappedList<DetectorData> resp = RESTUtils.executeResource(hostLocation, REST_SERVICE,
    // "getListOfDetectors", params, WrappedList.class);
    // System.out.println("Response: ");
    // for (DetectorData item : resp.getList()) {
    // System.out.println(item.getId());
    // }
    // System.out.println("End of Test.\n");
    // }
    //
    // /**
    // * Test creating a project.
    // *
    // * @param hostLocation The host location.
    // */
    // public static void testCreateProject(String hostLocation) {
    // System.out.println("testCreateProject");
    // Map<String, String> params = new HashMap<String, String>();
    // params.put("owner", REST_OWNER);
    // params.put("projectName", "test-project");
    //
    // String resp = RESTUtils.executeResource(hostLocation, REST_SERVICE, "createProject", params,
    // String.class);
    // System.out.println("Was Created: " + Boolean.parseBoolean(resp));
    // System.out.println("End of Test.\n");
    // }
    //
    // /**
    // * Test adding simulations to a project.
    // *
    // * @param hostLocation The host location.
    // * @param simId The simulation id.
    // */
    // public static void testAddSimulationsToProject(String hostLocation, String simId) {
    // System.out.println("testAddSimulationsToProject");
    // Map<String, String> params = new HashMap<String, String>();
    // params.put("owner", REST_OWNER);
    // params.put("projectName", "test-project");
    // params.put("simIds", simId);
    //
    // @SuppressWarnings("unchecked")
    // WrappedList<String> resp = RESTUtils.executeResource(hostLocation, REST_SERVICE,
    // "addSimulationsToProject", params, WrappedList.class);
    // System.out.println("Response: ");
    // for (String item : resp.getList()) {
    // System.out.println(item);
    // }
    // System.out.println("End of Test.\n");
    // }
    //
    // /**
    // * Test removing simulations from a project.
    // *
    // * @param hostLocation The host location.
    // * @param simId The simulation id.
    // */
    // public static void testRemoveSimulationsFromProject(String hostLocation, String simId) {
    // System.out.println("testRemoveSimulationsFromProject");
    // Map<String, String> params = new HashMap<String, String>();
    // params.put("owner", REST_OWNER);
    // params.put("projectName", "test-project");
    // params.put("simIds", simId);
    //
    // @SuppressWarnings("unchecked")
    // WrappedList<String> resp = RESTUtils.executeResource(hostLocation, REST_SERVICE,
    // "removeSimulationsFromProject", params, WrappedList.class);
    // System.out.println("Response: ");
    // for (String item : resp.getList()) {
    // System.out.println(item);
    // }
    // System.out.println("End of Test.\n");
    // }
    //
    // /**
    // * Gets a list of simulations for a project.
    // *
    // * @param hostLocation The location of the server.
    // */
    // public static void testGetSimulationsForProject(String hostLocation) {
    // System.out.println("testGetSimulationsForProject");
    // Map<String, String> params = new HashMap<String, String>();
    // params.put("owner", REST_OWNER);
    // params.put("projectName", "test-project");
    //
    // @SuppressWarnings("unchecked")
    // WrappedList<String> resp = RESTUtils.executeResource(hostLocation, REST_SERVICE,
    // "getSimulationsForProject", params, WrappedList.class);
    // System.out.println("Response: ");
    // for (String item : resp.getList()) {
    // System.out.println(item);
    // }
    // System.out.println("End of Test.\n");
    // }
    //
    // /**
    // * Gets a list of projects for a user.
    // *
    // * @param hostLocation The location of the server.
    // */
    // public static void testGetListOfProjects(String hostLocation) {
    // System.out.println("testGetListOfProjects");
    // Map<String, String> params = new HashMap<String, String>();
    // params.put("owner", REST_OWNER);
    //
    // @SuppressWarnings("unchecked")
    // WrappedList<String> resp = RESTUtils.executeResource(hostLocation, REST_SERVICE,
    // "getListOfProjects", params, WrappedList.class);
    // System.out.println("Response: ");
    // for (String item : resp.getList()) {
    // System.out.println(item);
    // }
    // System.out.println("End of Test.\n");
    // }
    //
    // /**
    // * Tests adding vehicle injection commands.
    // *
    // * @param hostLocation The host location.
    // * @param execId The execution Id.
    // * @param lvic The list of vehicle injection commands.
    // */
    // public static void testAddVehicleInjectionCommand(String hostLocation, String execId,
    // List<VehicleInjectionCommand> lvic) {
    // System.out.println("testAddVehicleInjectionCommand");
    // System.out.println("execId: " + execId);
    // Map<String, String> params = new HashMap<String, String>();
    // // params.put("owner", REST_OWNER);
    // params.put("id", execId);
    // WrappedList<VehicleInjectionCommand> commands = new WrappedList<VehicleInjectionCommand>();
    // commands.setList(lvic);
    // //VehicleInjectionCommand cmd = commands.getList().get(0);
    // // System.out.println(String.format("Injecting vehicle via REST.\nId: %d, Speed: %.2f, Lane:
    // %d",
    // // cmd.getVehicleID(), cmd.getInitial_speed(), cmd.getInbound_lane_number()));
    // RESTUtils.executeResource(hostLocation, REST_SERVICE, "addVehicleInjectionCommand", params,
    // String.class, commands);
    // System.out.println("End of Test.\n");
    // }
    //
    // /**
    // * Tests get remote applications for time step.
    // *
    // * @param hostLocation The host location.
    // * @param simId The simulation ID to test on.
    // */
    // @SuppressWarnings("unchecked")
    // public static void testRemoteApplicationData(String hostLocation, String simId) {
    // System.out.println("Testing Remote Application Data");
    // // Get Execution ID.
    // String execId = RESTHarness.startSim(hostLocation, simId, 17);
    // System.out.println("Exec ID: " + execId);
    //
    // // Set parameters for getting started apps.
    // Map<String, String> params = new HashMap<String, String>();
    // params.put("id", execId);
    // params.put("started", "true");
    //
    // // Keep advancing until started apps are retrieved.
    // WrappedList<String> start = new WrappedList<String>(new ArrayList<String>());
    // int step = 0;
    // while (start.getList().size() == 0 && step < 50) {
    // RESTHarness.execAdvance(hostLocation, execId, 1);
    // start = RESTUtils.executeResource(hostLocation, WAVE_SERVICE, "getRemoteAppsForTimestep",
    // params, WrappedList.class);
    // step++;
    // }
    // System.out.println("Started Remote Applications: ");
    // for (String item : start.getList()) {
    // System.out.println(item);
    // }
    //
    // // Inject application data from a remote application.
    // RESTHarness.execAdvance(hostLocation, execId, 50);
    // Map<String, String> injectParams = new HashMap<String, String>();
    // injectParams.put("execId", execId);
    // injectParams.put("instanceId", start.getList().get(0));
    // ApplicationData appData = new ApplicationData();
    // List<VehicleCommand> listVC = new ArrayList<VehicleCommand>();
    // listVC.add(new VehicleSpeedCommand(1, VehicleSpeedCommand.MAX_DECELERATE_TO_XX, 0));
    //
    // List<MessageRequest> listMR = new ArrayList<MessageRequest>();
    // listMR.add(new MessageRequest(new MapData(), 1, 1));
    //
    // List<AppLogData> listAD = new ArrayList<AppLogData>();
    // listAD.add(new AppLogData(Long.valueOf(execId), new LogData(0, null, null, "key1",
    // "data1")));
    // listAD.add(new AppLogData(Long.valueOf(execId), new LogData(0, null, null, "key2",
    // "data2")));
    //
    // appData.setLogs(new WrappedList<AppLogData>(listAD));
    // //appData.setMessages(new WrappedList<MessageRequest>(listMR));
    // appData.setVehicleCommands(new WrappedList<VehicleCommand>(listVC));
    // appData.setSignalCommand(new SignalCommand(SignalCommand.CHANGE_SIGNAL, 0));
    // String result = RESTUtils.executeResource(hostLocation, WAVE_SERVICE,
    // "setApplicationDataForTimestep", injectParams, String.class, appData);
    // System.out.println("Application Data Set: " + result);
    //
    // // Verify messages set.
    // Map<String, String> rmoteParams = new HashMap<String, String>();
    // rmoteParams.put("id", execId);
    // String rIds = RESTUtils.executeResource(hostLocation, WAVE_SERVICE, "getRemoteIDs",
    // rmoteParams, String.class);
    // System.out.println("Remote ID's: " + rIds);
    // String remoteIds[] = rIds.split(",");
    // RESTHarness.execAdvance(hostLocation, execId, 1);
    //
    // System.out.println("Device Data: ");
    // Map<String, String> dinfoParams = new HashMap<String, String>();
    // dinfoParams.put("instanceId", remoteIds[remoteIds.length - 1].trim());
    // dinfoParams.put("wantDevice", "true");
    // dinfoParams.put("wantMessages", "true");
    // DeviceData demu = RESTUtils.executeResource(hostLocation, WAVE_SERVICE,
    // "getDeviceDataForTimestep", dinfoParams, DeviceData.class);
    // System.out.println("Device Info: " + demu.getDevice());
    // for (Message m : demu.getMessageList().getMessages()) {
    // System.out.println("Message: " + m.getClass().getSimpleName());
    // }
    //
    // // Set parameters for getting stopped apps.
    // params.put("started", "false");
    // params.put("stopped", "true");
    //
    // // Keep advancing until stopped apps are retrieved.
    // WrappedList<String> stop = new WrappedList<String>(new ArrayList<String>());
    // while (stop.getList().size() == 0 && step < 200) {
    // RESTHarness.execAdvance(hostLocation, execId, 1);
    // stop = RESTUtils.executeResource(hostLocation, WAVE_SERVICE, "getRemoteAppsForTimestep",
    // params, WrappedList.class);
    // step++;
    // }
    // System.out.println("Stopped Remote Applications: ");
    // for (String item : stop.getList()) {
    // System.out.println(item);
    // }
    // System.out.println("End of Test.\n");
    // }
}
