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

import java.io.File;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.etexascode.test.restharness.templateclasses.ClientInfo;
import org.etexascode.test.restharness.templateclasses.DetectorInfo;
import org.etexascode.test.restharness.templateclasses.DeviceTemplate;
import org.etexascode.test.restharness.templateclasses.JarTemplate;
import org.etexascode.test.restharness.templateclasses.LogDataTemplate;
import org.etexascode.test.restharness.templateclasses.RSETemplate;
import org.etexascode.test.restharness.templateclasses.SimConfTemplate;

/**
 * Performs complete integration tests.
 * 
 * @author ablatt
 */
public class Main {

    /**
     * Execute at the start.
     * 
     * @param args Needed args.
     * @throws URISyntaxException
     */
    public static void main(String[] args) throws URISyntaxException {
        System.out.println("starting");
        ExecutionTemplate template = createTemplate1();

        RestExecutor exec = new RestExecutor();
        exec.executeTemplate(template);
        System.out.println("finishing");
    }

    /**
     * Setup a template integration test.
     * 
     * @return The template.
     * @throws URISyntaxException
     */
    public static ExecutionTemplate createTemplate1() throws URISyntaxException {
        ExecutionTemplate template = new ExecutionTemplate();

        ClientInfo clientInfo = new ClientInfo();
        clientInfo.setUname("admin");
        clientInfo.setPass("admin");
        clientInfo.setHostLocation("localhost:8080");
        template.setClientInfo(clientInfo);

        SimConfTemplate sct = new SimConfTemplate();
        sct.setSimConfName("testConf1");
        sct.setTemplateId("exam_08");
        template.setSimConf(sct);

        List<JarTemplate> jars = new ArrayList<JarTemplate>(1);
        URL jarUrl = new Main().getClass().getResource("test-apps-2.0-SNAPSHOT.jar");
        JarTemplate j = new JarTemplate(new File(jarUrl.toURI()), "testJarName");
        jars.add(j);
        template.setJars(jars);

        List<DeviceTemplate> devs = new ArrayList<DeviceTemplate>(1);
        RSETemplate rse = new RSETemplate();
        rse.setDeviceName("testRse");
        List<String> jarAppIds = new ArrayList<String>(1);
        jarAppIds.add("TestRSE");
        rse.setJarAppIds(jarAppIds);
        devs.add(rse);
        template.setDevices(devs);

        List<DetectorInfo> dets = new ArrayList<DetectorInfo>(1);
        DetectorInfo detInf = new DetectorInfo();
        detInf.setLaneId(1);
        detInf.setDist(20000);
        detInf.setWidth(180);
        detInf.setLength(180);
        dets.add(detInf);
        template.setDets(dets);

        template.setLogTemplate(new LogDataTemplate(new File("/home/ablatt/LogOutput.csv")));

        template.cleanupEverything();
        return template;
    }
}
