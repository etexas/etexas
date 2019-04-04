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

import java.util.List;

import org.etexascode.interrep.datamodel.utils.UtilsLatLongConversion;
import org.etexascode.test.restharness.templateclasses.ClientInfo;
import org.etexascode.test.restharness.templateclasses.DetectorInfo;
import org.etexascode.test.restharness.templateclasses.DeviceTemplate;
import org.etexascode.test.restharness.templateclasses.JarTemplate;
import org.etexascode.test.restharness.templateclasses.LogDataTemplate;
import org.etexascode.test.restharness.templateclasses.SimConfBase;

/**
 * Container for information necessary to execute a simulation start to finish.
 * 
 * @author ablatt
 */
public class ExecutionTemplate {

    /**
     * Info for how to connect to the server.
     */
    ClientInfo clientInfo;

    /**
     * Info on the sim to execute.
     */
    SimConfBase simConf;

    /**
     * Info on the devices the execution should have.
     */
    List<DeviceTemplate> devices;

    /**
     * Info on jars to upload and use.
     */
    List<JarTemplate> jars;

    /**
     * Info on the detectors to use in the execution.
     */
    List<DetectorInfo> dets;

    /**
     * Should we cleanup the apps?
     */
    boolean cleanupApps = false;

    /**
     * Should we cleanup the sim config?
     */
    boolean cleanupSimconf = false;

    /**
     * Should we cleanup the execution?
     */
    boolean cleanupExec = false;

    /**
     * Latitude of the center of the intersection.
     */
    double lat = 0.0;

    /**
     * Longitude of the center of the intersection.
     */
    double lon = 0.0;

    /**
     * The communications model to use for this sim config.
     */
    String commModel = "Idealized";

    /**
     * The coordinate system to use for this sim config.
     */
    String coorSys = UtilsLatLongConversion.convertCalculatorType(UtilsLatLongConversion.GEODETIC2D);

    /**
     * Data concerning which logs to download.
     */
    LogDataTemplate logTemplate;

    /**
     * We should cleanup everything.
     */
    public void cleanupEverything() {
        cleanupApps = true;
        cleanupSimconf = true;
        cleanupExec = true;
    }

    /**
     * Getter.
     * 
     * @return Info for how to connect to the server.
     */
    public ClientInfo getClientInfo() {
        return clientInfo;
    }

    /**
     * Setter.
     * 
     * @param clientInfo Info for how to connect to the server.
     */
    public void setClientInfo(ClientInfo clientInfo) {
        this.clientInfo = clientInfo;
    }

    /**
     * Getter.
     * 
     * @return Info on the sim to execute.
     */
    public SimConfBase getSimConf() {
        return simConf;
    }

    /**
     * Setter.
     * 
     * @param simConf Info on the sim to execute.
     */
    public void setSimConf(SimConfBase simConf) {
        this.simConf = simConf;
    }

    /**
     * Getter.
     * 
     * @return Info on the devices the execution should have.
     */
    public List<DeviceTemplate> getDevices() {
        return devices;
    }

    /**
     * Setter.
     * 
     * @param devices Info on the devices the execution should have.
     */
    public void setDevices(List<DeviceTemplate> devices) {
        this.devices = devices;
    }

    /**
     * Getter.
     * 
     * @return Info on jars to upload and use.
     */
    public List<JarTemplate> getJars() {
        return jars;
    }

    /**
     * Setter.
     * 
     * @param jars Info on jars to upload and use.
     */
    public void setJars(List<JarTemplate> jars) {
        this.jars = jars;
    }

    /**
     * Getter.
     * 
     * @return Info on the detectors to use in the execution.
     */
    public List<DetectorInfo> getDets() {
        return dets;
    }

    /**
     * Setter.
     * 
     * @param dets Info on the detectors to use in the execution.
     */
    public void setDets(List<DetectorInfo> dets) {
        this.dets = dets;
    }

    /**
     * Getter.
     * 
     * @return Should we cleanup the apps?
     */
    public boolean isCleanupApps() {
        return cleanupApps;
    }

    /**
     * Setter.
     * 
     * @param cleanupApps Should we cleanup the apps?
     */
    public void setCleanupApps(boolean cleanupApps) {
        this.cleanupApps = cleanupApps;
    }

    /**
     * Getter.
     * 
     * @return Should we cleanup the sim config?
     */
    public boolean isCleanupSimconf() {
        return cleanupSimconf;
    }

    /**
     * Setter.
     * 
     * @param cleanupSimconf Should we cleanup the sim config?
     */
    public void setCleanupSimconf(boolean cleanupSimconf) {
        this.cleanupSimconf = cleanupSimconf;
    }

    /**
     * Getter.
     * 
     * @return Should we cleanup the execution?
     */
    public boolean isCleanupExec() {
        return cleanupExec;
    }

    /**
     * Setter.
     * 
     * @param cleanupExec Should we cleanup the execution?
     */
    public void setCleanupExec(boolean cleanupExec) {
        this.cleanupExec = cleanupExec;
    }

    /**
     * Getter.
     * 
     * @return Latitude of the center of the intersection.
     */
    public double getLat() {
        return lat;
    }

    /**
     * Setter.
     * 
     * @param lat Latitude of the center of the intersection.
     */
    public void setLat(double lat) {
        this.lat = lat;
    }

    /**
     * Getter.
     * 
     * @return Longitude of the center of the intersection.
     */
    public double getLon() {
        return lon;
    }

    /**
     * Setter.
     * 
     * @param lon Longitude of the center of the intersection.
     */
    public void setLon(double lon) {
        this.lon = lon;
    }

    /**
     * Getter.
     * 
     * @return The communications model to use for this sim config.
     */
    public String getCommModel() {
        return commModel;
    }

    /**
     * Setter.
     * 
     * @param commModel The communications model to use for this sim config.
     */
    public void setCommModel(String commModel) {
        this.commModel = commModel;
    }

    /**
     * Getter.
     * 
     * @return The coordinate system to use for this sim config.
     */
    public String getCoorSys() {
        return coorSys;
    }

    /**
     * Setter.
     * 
     * @param coorSys The coordinate system to use for this sim config.
     */
    public void setCoorSys(String coorSys) {
        this.coorSys = coorSys;
    }

    /**
     * Getter.
     * 
     * @return Data concerning which logs to download.
     */
    public LogDataTemplate getLogTemplate() {
        return logTemplate;
    }

    /**
     * Setter.
     * 
     * @param logTemplate Data concerning which logs to download.
     */
    public void setLogTemplate(LogDataTemplate logTemplate) {
        this.logTemplate = logTemplate;
    }
}
