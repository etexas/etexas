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

public class RESTServiceTests {

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
    // REST_OWNER = "admin";
    // String simId = testAddSimulationEXTJS(hostLocation);
    // List<String> applicationIds = testAddJarAppsEXTJS(hostLocation);
    // Long devId = testAddDevicesEXTJS(hostLocation, simId);
    // String appId = testAddAppsToDeviceEXTJS(hostLocation, simId, devId);
    // Long dId = testAddDetectorsEXTJS(hostLocation, simId);
    //
    // testGetAppsListEXTJS(hostLocation);
    // testAddRemoteAppsEXTJS(hostLocation);
    // testAddRemoteAppEXTJS(hostLocation);
    // testGetDevicesListEXTJS(hostLocation, simId);
    // testGetListOfAppsOnDeviceEXTJS(hostLocation, simId, devId);
    // testGetListOfAppsAvailableForDeviceEXTJS(hostLocation, simId, devId);
    // testGetListOfDetectorsEXTJS(hostLocation, simId);
    //
    // testDeleteDetectorsEXTJS(hostLocation, simId, dId);
    // testRemoveAppsFromDeviceEXTJS(hostLocation, simId, devId, appId);
    // testDeleteDevicesEXTJS(hostLocation, simId, devId);
    // testDeleteJarAppsEXTJS(hostLocation);
    // testDeleteSimulationEXTJS(hostLocation, simId);
    // }
    //
    // /**
    // * Tests adding a simulation.
    // *
    // * @param hostLocation The host location.
    // * @return the simId.
    // */
    // public static String testAddSimulationEXTJS(String hostLocation) {
    // System.out.println("testAddSimulation EXTJS");
    // MultivaluedMap<String, String> params = new MultivaluedMapImpl();
    // List<String> name = new ArrayList<String>();
    // List<String> templ = new ArrayList<String>();
    // name.add("testProject");
    // templ.add("exam_06");
    // params.put("name", name);
    // params.put("template", templ);
    //
    // SimData resp = RESTUtils.executeResourceFormPost(hostLocation, REST_OWNER, "sims", new
    // HashMap<String, String>(), SimData.class, params);
    // System.out.println("SimId: " + resp.getId());
    // System.out.println("End of Test.\n");
    // return "" + resp.getId();
    // }
    //
    // /**
    // * Test deleting a simulation.
    // *
    // * @param hostLocation The host location.
    // * @param simId The simulation Id.
    // */
    // public static void testDeleteSimulationEXTJS(String hostLocation, String simId) {
    // System.out.println("testDeleteSimulation EXTJS");
    // String path = "sims/" + simId;
    // Map<String, String> params = new HashMap<String, String>();
    //
    // String resp = RESTUtils.executeResourceDelete(hostLocation, REST_OWNER, path, params,
    // String.class);
    // System.out.println("Was Deleted: " + resp);
    // System.out.println("End of Test.\n");
    // }
    //
    // /**
    // * Test add remote applications.
    // *
    // * @param hostLocation The host locations.
    // */
    // public static void testAddRemoteAppsEXTJS(String hostLocation) {
    // System.out.println("testAddRemoteApps EXTJS");
    // String path = "addRemoteApps";
    // Map<String, String> params = new HashMap<String, String>();
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
    // String resp = RESTUtils.executeResource(hostLocation, REST_OWNER, path, params, String.class,
    // data);
    // System.out.println("Response: " + resp);
    // System.out.println("End of Test.\n");
    // }
    //
    // /**
    // * Test add remote applications.
    // *
    // * @param hostLocation The host locations.
    // * @return List of remote application IDs.
    // */
    // public static void testAddRemoteAppEXTJS(String hostLocation) {
    // System.out.println("testAddRemoteApp EXTJS");
    // String path = "addRemoteApp/" + AppDefData.APPTYPE_REMOTE + "/BSM-producer";
    // Map<String, String> params = new HashMap<String, String>();
    //
    // String resp = RESTUtils.executeResource(hostLocation, REST_OWNER, path, params,
    // String.class);
    // System.out.println("Response: " + resp);
    // System.out.println("End of Test.\n");
    // }
    //
    // /**
    // * Test add remote applications.
    // *
    // * @param hostLocation The host locations.
    // * @return List of remote application IDs.
    // */
    // public static void testGetAppsListEXTJS(String hostLocation) {
    // System.out.println("testGetAppsList EXTJS");
    // String path = "getAppsList";
    // Map<String, String> params = new HashMap<String, String>();
    //
    // String resp = RESTUtils.executeResource(hostLocation, REST_OWNER, path, params,
    // String.class);
    // System.out.println("Response: " + resp);
    // System.out.println("End of Test.\n");
    // }
    //
    // /**
    // * Tests adding jar applications.
    // *
    // * @param hostLocation The host location.
    // * @return List of applications IDs.
    // */
    // public static List<String> testAddJarAppsEXTJS(String hostLocation) throws
    // URISyntaxException, JAXBException {
    // System.out.println("testAddJarApps EXTJS");
    // String path = "jarapps/j2735-apps-1.0-SNAPSHOT.jar";
    // Map<String, String> params = new HashMap<String, String>();
    // byte[] jar = null;
    //
    // try {
    // URL jarUrl = RESTSimulationTest.class.getResource("j2735-apps-1.0-SNAPSHOT.jar");
    // RandomAccessFile raf = new RandomAccessFile(new File(jarUrl.toURI()), "r");
    // byte data[] = new byte[(int)raf.length()];
    // raf.read(data, 0, (int)raf.length());
    // raf.close();
    // jar = data;
    // }
    // catch (FileNotFoundException e) {
    // e.printStackTrace();
    // }
    // catch (IOException e) {
    // e.printStackTrace();
    // }
    //
    // String resp = RESTUtils.executeResource(hostLocation, REST_OWNER, path, params, String.class,
    // jar);
    // System.out.println("Response: " + resp);
    // System.out.println("End of Test.\n");
    //
    // Type listType = new TypeToken<List<String>>() {
    // }.getType();
    // List<String> respList = new Gson().fromJson(resp, listType);
    // return respList;
    // }
    //
    // /**
    // * Test deleting jar applications.
    // *
    // * @param hostLocation The host location.
    // */
    // public static void testDeleteJarAppsEXTJS(String hostLocation) {
    // System.out.println("testDeleteJarApps EXTJS");
    // String path = "jarapps/j2735-apps-1.0-SNAPSHOT.jar";
    // Map<String, String> params = new HashMap<String, String>();
    //
    // String resp = RESTUtils.executeResourceDelete(hostLocation, REST_OWNER, path, params,
    // String.class);
    // System.out.println("Response: " + resp);
    // System.out.println("End of Test.\n");
    // }
    //
    // /**
    // * Test adding devices.
    // *
    // * @param hostLocation The host location.
    // * @param simId The simulation id.
    // * @return The device ID.
    // */
    // public static Long testAddDevicesEXTJS(String hostLocation, String simId) {
    // System.out.println("testAddDevices EXTJS");
    // String path = "device/" + simId;
    // Map<String, String> params = new HashMap<String, String>();
    //
    // PercentageRule dr = new PercentageRule();
    // dr.setName("Test Percentage Rule");
    // dr.setPercentage(100.0);
    //
    // String resp = RESTUtils.executeResource(hostLocation, REST_OWNER, path, params, String.class,
    // dr);
    // System.out.println("Response: " + resp);
    // System.out.println("End of Test.\n");
    // return Long.parseLong(resp);
    // }
    //
    // /**
    // * Test deleting devices.
    // *
    // * @param hostLocation The host location.
    // * @param simId The simulation id.
    // * @param devId The device ID.
    // */
    // public static void testDeleteDevicesEXTJS(String hostLocation, String simId, Long devId) {
    // System.out.println("testDeleteDevices EXTJS");
    // Map<String, String> params = new HashMap<String, String>();
    // String path = "device/" + simId + "/" + devId;
    //
    // String resp = RESTUtils.executeResourceDelete(hostLocation, REST_OWNER, path, params,
    // String.class);
    // System.out.println("Response: " + resp);
    // System.out.println("End of Test.\n");
    // }
    //
    // /**
    // * Test getting devices list.
    // *
    // * @param hostLocation The host location.
    // * @param simId The simulation id.
    // */
    // public static void testGetDevicesListEXTJS(String hostLocation, String simId) {
    // System.out.println("testGetDeviceList EXTJS");
    // Map<String, String> params = new HashMap<String, String>();
    // String path = "device/" + simId;
    //
    // String resp = RESTUtils.executeResource(hostLocation, REST_OWNER, path, params,
    // String.class);
    // System.out.println("Response: " + resp);
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
    // public static String testAddAppsToDeviceEXTJS(String hostLocation, String simId, Long
    // deviceId) {
    // System.out.println("testAddAppsToDevice EXTJS");
    // String path = "application/" + simId + "/" + deviceId;
    // Map<String, String> params = new HashMap<String, String>();
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
    // String resp = RESTUtils.executeResource(hostLocation, REST_OWNER, path, params, String.class,
    // acd);
    // System.out.println("Response: " + resp);
    // System.out.println("End of Test.\n");
    // return resp;
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
    // public static void testRemoveAppsFromDeviceEXTJS(String hostLocation, String simId, Long
    // deviceId, String appId) {
    // System.out.println("testRemoveAppsFromDevice EXTJS");
    // Map<String, String> params = new HashMap<String, String>();
    // String path = "application/" + simId + "/" + deviceId + "/" + appId;
    //
    // String resp = RESTUtils.executeResourceDelete(hostLocation, REST_OWNER, path, params,
    // String.class);
    // System.out.println("Response: " + resp);
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
    // public static void testGetListOfAppsOnDeviceEXTJS(String hostLocation, String simId, Long
    // deviceId) {
    // System.out.println("testGetListOfAppsOnDevice EXTJS");
    // String path = "application/" + simId + "/" + deviceId;
    // Map<String, String> params = new HashMap<String, String>();
    //
    // String resp = RESTUtils.executeResource(hostLocation, REST_OWNER, path, params,
    // String.class);
    // System.out.println("Response: " + resp);
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
    // public static void testGetListOfAppsAvailableForDeviceEXTJS(String hostLocation, String
    // simId, Long deviceId) {
    // System.out.println("testGetListOfAppsAvailableForDevice EXTJS");
    // String path = "application/" + simId + "/" + deviceId + "/available";
    // Map<String, String> params = new HashMap<String, String>();
    //
    // String resp = RESTUtils.executeResource(hostLocation, REST_OWNER, path, params,
    // String.class);
    // System.out.println("Response: " + resp);
    // System.out.println("End of Test.\n");
    // }
    //
    // /**
    // * Test adding detectors.
    // *
    // * @param hostLocation The host location.
    // * @param simId The simulation id.
    // * @return The detector ID.
    // */
    // public static Long testAddDetectorsEXTJS(String hostLocation, String simId) {
    // System.out.println("testAddDetectors EXTJS");
    // String path = "detector/" + simId;
    // Map<String, String> params = new HashMap<String, String>();
    // params.put("laneNum", "5");
    // params.put("width", "280");
    // params.put("height", "280");
    // params.put("dist", "2700");
    //
    // String resp = RESTUtils.executeResource(hostLocation, REST_OWNER, path, params, String.class,
    // null);
    // System.out.println("Response: " + resp);
    // System.out.println("End of Test.\n");
    // return Long.parseLong(resp);
    // }
    //
    // /**
    // * Test deleting devices.
    // *
    // * @param hostLocation The host location.
    // * @param simId The simulation id.
    // * @param detectorIds The detector ID.
    // */
    // public static void testDeleteDetectorsEXTJS(String hostLocation, String simId, Long
    // detectorId) {
    // System.out.println("testDeleteDetectors EXTJS");
    // String path = "detector/" + simId + "/" + detectorId;
    //
    // String resp = RESTUtils.executeResourceDelete(hostLocation, REST_OWNER, path, new
    // HashMap<String, String>(), String.class);
    // System.out.println("Response: " + resp);
    // System.out.println("End of Test.\n");
    // }
    //
    // /**
    // * Test getting detectors list.
    // *
    // * @param hostLocation The host location.
    // * @param simId The simulation id.
    // */
    // public static void testGetListOfDetectorsEXTJS(String hostLocation, String simId) {
    // System.out.println("testGetListOfDetectors EXTJS");
    // String path = "detector/" + simId;
    //
    // String resp = RESTUtils.executeResource(hostLocation, REST_OWNER, path, new HashMap<String,
    // String>(), String.class);
    // System.out.println("Response: " + resp);
    // System.out.println("End of Test.\n");
    // }

}