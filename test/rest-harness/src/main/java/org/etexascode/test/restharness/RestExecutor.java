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

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.etexascode.devicedata.LogData;
import org.etexascode.test.restharness.templateclasses.DetectorInfo;
import org.etexascode.test.restharness.templateclasses.DeviceTemplate;
import org.etexascode.test.restharness.templateclasses.JarTemplate;
import org.etexascode.test.restharness.templateclasses.LogDataTemplate;
import org.etexascode.test.restharness.templateclasses.OBUTemplate;
import org.etexascode.test.restharness.templateclasses.RSETemplate;
import org.etexascode.test.restharness.templateclasses.ReportTemplate;
import org.etexascode.test.restharness.templateclasses.SimConfTemplate;
import org.etexascode.test.restharness.templateclasses.SimConfUpload;
import org.etexascode.webapp.datamodel.AppLogData;
import org.etexascode.webapp.datamodel.ExecData;
import org.etexascode.webapp.datamodel.SimData;
import org.etexascode.webapp.datamodel.app.AppDefinition;
import org.etexascode.webapp.datamodel.app.AppDefinitionParam;
import org.etexascode.webapp.datamodel.app.AppInstance;
import org.etexascode.webapp.datamodel.app.AppInstanceParam;
import org.etexascode.webapp.datamodel.app.RemoteAppDefinition;
import org.etexascode.webapp.datamodel.device.DeviceRule;

/**
 * Class for executing a complete REST configuration.
 * 
 * @author ablatt
 * @author bbadillo
 */
public class RestExecutor {

    /**
     * Execute a specific template.
     * 
     * @param template The template to follow when executing the test.
     */
    public void executeTemplate(ExecutionTemplate template) {
        // sanity checks
        System.out.println("Performing Sanity Checks");
        performSanityChecks(template);

        // setup client
        RestClient client = new RestClient(template.clientInfo.getHostLocation(), template.clientInfo.getUname(), template.clientInfo.getPass());

        // setup sim conf
        System.out.println("Setting up Sim");
        String simId = setupSimConf(template, client);

        // setup apps
        System.out.println("Uploading Jars");
        for (JarTemplate jar : template.jars) {
            client.addJar(jar.getJar(), jar.getName());
        }

        System.out.println("Parsing Apps");
        List<AppDefinition> apps = client.getAppsList();
        Map<String, AppDefinition> appIdToAppData = new HashMap<String, AppDefinition>();
        for (AppDefinition ad : apps) {
            if (ad instanceof RemoteAppDefinition) {
                appIdToAppData.put(ad.getAppName(), ad);
            }
            else {
                String s = ad.getAppName();
                s = s.substring(0, s.lastIndexOf(" ("));
                appIdToAppData.put(s, ad);
            }
        }

        // setup devices
        System.out.println("Creating and Configuring Devices");
        List<DeviceRule> devices = client.getDevices(simId);
        HashMap<String, DeviceRule> devIds = new HashMap<String, DeviceRule>();
        for (DeviceRule dr : devices) {
            devIds.put(dr.getName(), dr);
        }
        for (DeviceTemplate dt : template.devices) {
            processDevice(simId, dt, client, devIds, appIdToAppData);
        }

        // setup detectors
        System.out.println("Setting Up Detectors");
        for (DetectorInfo di : template.dets) {
            client.addDetector(simId, di.getLaneId(), di.getWidth(), di.getLength(), di.getDist());
        }

        System.out.println("Setting options");
        if (!client.setCommunicationModel(simId, template.getCommModel())) {
            throw new RuntimeException("failed to set comm model");
        }
        if (!client.setCoordinateSystem(simId, template.getCoorSys())) {
            throw new RuntimeException("failed to set coordinate system");
        }
        if (!client.setIntersectionCenter(simId, template.getLat(), template.getLon())) {
            throw new RuntimeException("failed to set the intersection center");
        }

        // execute sim conf
        System.out.println("Executing");
        ExecData ed = client.startExec(simId);
        String execId = ed.getId().toString();
        if (!client.finishExec(simId, execId)) {
            throw new RuntimeException("could not finish execution");
        }

        System.out.println("Fetching Logs");
        fetchLogs(template, client, simId, execId);

        // cleanup
        System.out.println("Cleaning up after execution");
        cleanup(template, simId, execId, client);
    }

    /**
     * Procedure for fetching logs.
     * 
     * @param template Template for which logs to fetch.
     * @param client Client to use to connect with the server.
     * @param simId Sim id the logs are connected with.
     * @param execId Execution id the logs are connected with.
     */
    public void fetchLogs(ExecutionTemplate template, RestClient client, String simId, String execId) {
        LogDataTemplate logs = template.getLogTemplate();
        if (logs.getLogOutputLoc() != null) {
            List<LogData> logout = client.getLogs(simId, execId, logs.getMinSimTime(), logs.getMaxSimTime(), logs.getKeys(), logs.getDevIds(), logs.getAppIds());
            try {
                PrintWriter pw = new PrintWriter(logs.getLogOutputLoc());
                pw.println(AppLogData.getCsvHeader());
                for (LogData ld : logout) {
                    pw.println(ld.toCsvString(execId));
                }
                pw.close();
            }
            catch (FileNotFoundException e) {
                throw new RuntimeException("Could not write out app logs", e);
            }
        }
    }

    /**
     * Procedure for performing sanity checks against the template.
     * 
     * @param template Template to check.
     */
    public void performSanityChecks(ExecutionTemplate template) {
        if (template.simConf == null) {
            throw new RuntimeException("need to have a sim configuration");
        }

        if (template.clientInfo == null) {
            throw new RuntimeException("need to have client information");
        }

        if (template.jars == null) {
            template.jars = new ArrayList<JarTemplate>(0);
        }

        if (template.dets == null) {
            template.dets = new ArrayList<DetectorInfo>(0);
        }

        if (template.devices == null) {
            template.devices = new ArrayList<DeviceTemplate>(0);
        }

        for (DeviceTemplate dt : template.devices) {
            if (dt.getJarAppIds() == null) {
                dt.setJarAppIds(new ArrayList<String>(0));
            }
            if (dt.getRemoteAppIds() == null) {
                dt.setRemoteAppIds(new ArrayList<String>(0));
            }
        }

        if (template.getLogTemplate() == null) {
            template.setLogTemplate(new LogDataTemplate(null));
        }
    }

    /**
     * Procedure for setting up the sim configuration.
     * 
     * @param template The template containing information on how to setup the sim.
     * @param client Client to use to connect with the server.
     * @return The sim id.
     */
    public String setupSimConf(ExecutionTemplate template, RestClient client) {
        List<SimData> sims = client.getSims();
        Long simIdTmp = null;
        for (SimData sim : sims) {
            if (sim.getSimName().equals(template.simConf.getSimConfName())) {
                simIdTmp = sim.getId();
            }
        }

        if (simIdTmp == null) {
            if (template.simConf instanceof SimConfTemplate) {
                SimConfTemplate sct = (SimConfTemplate)template.simConf;
                simIdTmp = client.createSimFromTemplate(sct.getSimConfName(), sct.getTemplateId());
            }
            else if (template.simConf instanceof SimConfUpload) {
                // need to fill this in
            }
            else {
                throw new RuntimeException("Simulation Configuration is neither an upload nor a template");
            }
        }

        return simIdTmp.toString();
    }

    /**
     * Procedure for configuring devices.
     * 
     * @param simId The id of the sim config the devices are on.
     * @param dt The information on how to configure devices.
     * @param client Client to use to connect with the server.
     * @param devIds The ids of the devices on the server.
     * @param appIdToAppData The apps recognized by the server.
     */
    public void processDevice(String simId, DeviceTemplate dt, RestClient client, Map<String, DeviceRule> devIds, Map<String, AppDefinition> appIdToAppData) {
        String devName = dt.getDeviceName();
        String devId = null;
        if ((devName == null) && !(dt instanceof ReportTemplate)) {
            throw new RuntimeException("one of the devices does not have a name");
        }
        if (!devIds.containsKey(dt.getDeviceName())) {
            if (dt instanceof RSETemplate) {
                RSETemplate tmp = (RSETemplate)dt;
                devId = client.addRSE(simId, devName, tmp.getX(), tmp.getY(), tmp.getZ());
            }
            else if (dt instanceof OBUTemplate) {
                OBUTemplate tmp = (OBUTemplate)dt;
                devId = client.addOBUPercentage(simId, devName, tmp.getPercentage());
            }
            else if (dt instanceof ReportTemplate) {
                // need to grab the known Report Device on the project and use it here
            }
            else {
                throw new RuntimeException("Device " + devName + " does not have a type");
            }
            List<AppInstance> appIn = new ArrayList<AppInstance>(dt.getJarAppIds().size() + dt.getRemoteAppIds().size());
            for (String appId : dt.getJarAppIds()) {
                appIn.add(convertAppDefData(appIdToAppData.get(appId)));
            }

            for (String appId : dt.getRemoteAppIds()) {
                appIn.add(convertAppDefData(appIdToAppData.get(appId)));
            }

            List<AppInstance> dats = client.addAppsToDevice(simId, devId.substring(6, devId.length() - 1), appIn);
            if (dats.size() > 0) { // do this to test the return value
                dats.get(0).getAppName();
            }
        }
    }

    /**
     * Convert an app definition to an app instance.
     * 
     * @param a The app definition to convert.
     * @return The app instance.
     */
    public static AppInstance convertAppDefData(AppDefinition a) {
        List<AppInstanceParam> params = new LinkedList<AppInstanceParam>();
        for (AppDefinitionParam adp : a.getAppParams()) {
            AppInstanceParam acp = new AppInstanceParam();
            acp.setName(adp.getName());
            acp.setParam(adp.getParam());
        }
        AppInstance instance = a.createInstance(params);
        return instance;
    }

    /**
     * Procedure for cleaning up the sim config.
     * 
     * @param template The template containing information about how to clean up the sim.
     * @param simId The id of the sim config to clean up.
     * @param execId The id of the execution to clean up.
     * @param client Client to use to connect with the server.
     */
    public void cleanup(ExecutionTemplate template, String simId, String execId, RestClient client) {
        if (template.cleanupApps) {
            for (JarTemplate jar : template.jars) {
                if (!client.removeJar(jar.getName())) {
                    throw new RuntimeException("could not remove a jar");
                }
            }
        }

        if (template.cleanupExec) {
            if (!client.removeExec(simId, execId)) {
                throw new RuntimeException("could not remove an execution");
            }
        }

        if (template.cleanupSimconf) {
            if (!client.removeSimConfig(simId)) {
                throw new RuntimeException("could not remove a simulation configuration");
            }
        }
    }
}
