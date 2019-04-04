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
package org.etexascode.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import javax.ws.rs.ClientErrorException;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.Entity;
import javax.ws.rs.core.Form;
import javax.ws.rs.core.Response;

import org.etexascode.test.restharness.RestClient;
import org.etexascode.webapp.datamodel.DetectorData;
import org.etexascode.webapp.datamodel.ExecData;
import org.etexascode.webapp.datamodel.MessageType;
import org.etexascode.webapp.datamodel.app.AppDefinition;
import org.etexascode.webapp.datamodel.app.AppDefinitionParam;
import org.etexascode.webapp.datamodel.app.AppInstance;
import org.etexascode.webapp.datamodel.app.AppInstanceParam;
import org.etexascode.webapp.datamodel.device.DeviceRule;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.FixMethodOrder;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runners.MethodSorters;

import com.fasterxml.jackson.core.JsonGenerationException;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;

/**
 * JUnit Rest Services Test
 * 
 * @author jrutherford
 * @author bbadillo
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class RestServicesIT {

    /** The MIME Type. */
    public final static String xmlMimeType = "application/xml";

    /** The simulation functions URL. */
    public final static String simManagement = "/sims";

    /** Simulation ID. */
    private static Long simId;

    /** List of application IDs. */
    @SuppressWarnings("unused")
    private static List<String> appIds;

    /** Device ID. */
    private static Long devId;

    /** Device App ID. */
    private static String devAppId;

    /** Detector ID. */
    private static Long detId;

    static RestClient client;

    /**
     * @throws Exception
     */

    @BeforeClass
    public static void initialize() {
        Client c = ClientBuilder.newClient();
        Form form = new Form();
        form.param("username", "test");
        form.param("password", "test");
        form.param("firstName", "test");
        form.param("lastName", "test");
        form.param("email", "test@test.com");

        Response response = c.target("http://etexas.internal.harmonia.com").path("rest").path("user").path("register").request().post(Entity.form(form));

        client = new RestClient("etexas.internal.harmonia.com", "test", "test");
    }

    @Before
    public void setUp() throws Exception {}

    @After
    public void tearDown() throws Exception {}

    /** Test multiple executions. */
    @Test
    public void test00MultipleExecutionsIT() {
        // Create the simulation and check that it gets an ID.

        Long res = createTemplate("testProject1");
        ExecData exec = client.startExec(res.toString());
        try {
            ExecData exec2 = client.startExec(res.toString()); // this line should throw an
                                                               // exception... I think
            fail(); // expecting the to throw an exception, will want to do some cleanup after this
        }
        catch (ClientErrorException e) {
            // do nothing
        }

        removeTemplate(res);
    }

    /** Tests adding a simulation. */
    @Test
    public void test01AddSimulationIT() {
        Long res = createTemplate("testProject2");
        assertNotNull(res);
        removeTemplate(res);
    }

    /**
     * Tests adding jar applications.
     * 
     * @throws URISyntaxException
     * @throws IOException
     */
    @Test
    public void test03AddJarAppsIT() throws URISyntaxException, IOException {
        URL jarUrl = this.getClass().getResource("intellifusion-apps-2.0-SNAPSHOT.jar");
        client.addJar(new File(jarUrl.toURI()), "intellifusion");
    }

    /**
     * Test adding devices.
     * 
     * @throws IOException
     * @throws JsonMappingException
     * @throws JsonGenerationException
     */
    @Test
    public void test05AddDevicesIT() throws IOException {
        Long simId = createTemplate("testProject3");
        String res = client.addOBUPercentage(simId.toString(), "Test Percentage Rule", "100");
        removeTemplate(simId);
        assertNotNull(res);
        assertFalse(0 == res.length());
    }

    /**
     * Test adding applications to device.
     * 
     * @throws URISyntaxException
     * @throws IOException
     */
    @Test
    public void test07AddAppsToDeviceIT() throws URISyntaxException, IOException {
        URL jarUrl = this.getClass().getResource("intellifusion-apps-2.0-SNAPSHOT.jar");
        client.addJar(new File(jarUrl.toURI()), "intellifusion");

        List<AppDefinition> apps = client.getAppsList();
        List<AppInstance> appIn = new LinkedList<AppInstance>();
        String appNamePresent = null;
        for (AppDefinition a : apps) {
            if (a.getDevTarget().equals(AppDefinition.APPTYPE_RSE)) {
                appNamePresent = a.getAppName();
                List<AppInstanceParam> params = new LinkedList<AppInstanceParam>();
                for (AppDefinitionParam adp : a.getAppParams()) {
                    AppInstanceParam acp = new AppInstanceParam();
                    acp.setName(adp.getName());
                    acp.setParam(adp.getParam());
                }
                AppInstance instance = a.createInstance(params);
                appIn.add(instance);
                break;
            }
        }

        Long simId = createTemplate("testProject4");
        String devId2 = client.addRSE(simId.toString(), "RSE1", "0", "0", "0");
        List<AppInstance> ret = client.addAppsToDevice(simId.toString(), devId2.substring(6, devId2.length() - 1), appIn);
        AppInstance installedApp = ret.get(0);
        boolean retVal = client.removeAppFromDevice(simId.toString(), devId2.substring(6, devId2.length() - 1), installedApp.getId().toString());
        removeTemplate(simId);
        assertTrue(retVal);
        assertEquals(1, ret.size());
        assertEquals(appNamePresent, ret.get(0).getAppName());
        devAppId = ret.get(0).getAppName();
    }

    /** Test adding detectors. */
    @Test
    public void test09AddDetectorsIT() {
        Long simId = createTemplate("testProject5");
        DetectorData ret = client.addDetector(simId.toString(), "5", "280", "280", "2700");
        removeTemplate(simId);
        assertNotNull(ret);
        detId = ret.getId();
    }

    /**
     * Test add remote applications.
     * 
     * @throws IOException
     * @throws JsonMappingException
     * @throws JsonParseException
     */
    @Test
    public void test11AddAndRemoveRemoteAppsIT() {
        List<AppDefinition> apps = client.addRemoteApp("RemoteApp", AppDefinition.APPTYPE_RSE, new HashMap<String, String>());
        List<AppDefinition> appsTot = client.getAppsList();
        Long appId = null;
        for (AppDefinition add : appsTot) {
            if ("RemoteApp".equals(add.getAppName())) {
                appId = add.getId();
                break;
            }
        }
        assertNotNull(appId);

        client.deleteRemoteApp(appId.toString());
    }

    /**
     * Tests messages produced by apps during an execution
     */
    @Test
    @Ignore
    public void test39AppMessagesIT() {
        // XXX Exec is running in previous test

        // Create an execution
        Long simId = createTemplate("testProject6");
        ExecData ed = client.startExec(simId.toString());
        Long execId = ed.getId();

        // Step the execution to 1 second.
        client.advanceExec(simId.toString(), execId.toString());

        List<MessageType> messages = client.getMessages(simId.toString(), execId.toString());
        assertTrue(messages != null);
        assertTrue(messages.size() > 0);
        removeTemplate(simId);

        // get the first message and verify its content
        MessageType messageType = messages.get(0);
        assertTrue(messageType.getMessageId() == 0);
        assertTrue(messageType.getMessageType().equals("BSM"));
    }

    /**
     * Test getting devices list.
     * 
     * @throws IOException
     * @throws JsonMappingException
     * @throws JsonParseException
     */
    @Test
    public void test42GetDevicesListIT() throws IOException {
        Long simId = createTemplate("testProject7");
        String res = client.addOBUPercentage(simId.toString(), "Test Percentage Rule", "100");
        List<DeviceRule> add = client.getDevices(simId.toString());
        removeTemplate(simId);
        assertNotNull(add);
        assertEquals(2, add.size());
    }

    /**
     * Test getting devices list.
     * 
     * @throws IOException
     * @throws JsonMappingException
     * @throws JsonParseException
     * @throws URISyntaxException
     */
    @Test
    public void test44GetListOfAppsOnDeviceIT() throws IOException, URISyntaxException {
        URL jarUrl = this.getClass().getResource("intellifusion-apps-2.0-SNAPSHOT.jar");
        client.addJar(new File(jarUrl.toURI()), "intellifusion");

        List<AppDefinition> apps = client.getAppsList();
        List<AppInstance> appIn = new LinkedList<AppInstance>();
        String appNamePresent = null;
        for (AppDefinition a : apps) {
            if (a.getDevTarget().equals(AppDefinition.APPTYPE_RSE)) {
                appNamePresent = a.getAppName();
                List<AppInstanceParam> params = new LinkedList<AppInstanceParam>();
                for (AppDefinitionParam adp : a.getAppParams()) {
                    AppInstanceParam acp = new AppInstanceParam();
                    acp.setName(adp.getName());
                    acp.setParam(adp.getParam());
                }
                AppInstance instance = a.createInstance(params);
                appIn.add(instance);
                break;
            }
        }

        Long simId = createTemplate("testProject8");
        String devId2 = client.addRSE(simId.toString(), "RSE1", "0", "0", "0");
        List<AppInstance> ret = client.addAppsToDevice(simId.toString(), devId2.substring(6, devId2.length() - 1), appIn);
        List<AppInstance> add = client.getAppsOnDevice(simId.toString(), devId2.substring(6, devId2.length() - 1));
        removeTemplate(simId);

        assertEquals(1, ret.size());
        assertEquals(appNamePresent, ret.get(0).getAppName());
        devAppId = ret.get(0).getAppName();
    }

    /**
     * Test getting detectors list.
     * 
     * @throws IOException
     * @throws JsonMappingException
     * @throws JsonParseException
     */
    @Test
    public void test48GetListOfDetectorsIT() throws IOException {
        Long simId = createTemplate("testProject9");
        DetectorData ret = client.addDetector(simId.toString(), "5", "280", "280", "2700");
        List<DetectorData> add = client.getDetectors(simId.toString());

        removeTemplate(simId);
        assertTrue(add != null);
        assertTrue(add.size() == 1);
        assertEquals("2700", add.get(0).getDistFromStopLine());
        assertEquals("5", add.get(0).getLaneNum());
        assertEquals("280", add.get(0).getWidth());
        assertEquals("280", add.get(0).getHeight());
    }

    /** Test deleting devices. */
    @Test
    public void test91DeleteDetectorsIT() {
        Long simId = createTemplate("testProject10");
        DetectorData detId = client.addDetector(simId.toString(), "5", "280", "280", "2700");
        List<DetectorData> dets = client.getDetectors(simId.toString());
        assertTrue(client.deleteDetector(simId.toString(), dets.get(0).getId().toString()));
        removeTemplate(simId);
    }

    /** Test deleting devices. */
    @Test
    public void test95DeleteDevicesIT() {
        Long simId = createTemplate("testProject11");
        String devId = client.addOBUPercentage(simId.toString(), "Test Percentage Rule", "100");
        assertTrue(client.deleteDevice(simId.toString(), devId.substring(7, devId.length() - 2)));
        removeTemplate(simId);
    }

    /** Test deleting a simulation. */
    @Test
    public void test99DeleteSimulationIT() {
        Long simId = createTemplate("testProject12");
        assertTrue(removeTemplate(simId));
    }

    private Long createTemplate(String name) {
        Long simId = client.createSimFromTemplate(name, "exam_06");
        return simId;
    }

    private boolean removeTemplate(Long simId) {
        return client.deleteSim(simId.toString());
    }
}
