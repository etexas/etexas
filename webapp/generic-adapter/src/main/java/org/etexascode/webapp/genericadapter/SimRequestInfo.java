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

package org.etexascode.webapp.genericadapter;

import java.util.HashMap;
import java.util.Map;

import javax.resource.spi.ConnectionRequestInfo;

import org.etexascode.interrep.datamodel.SimulatorInterface;

/**
 * Connection request information for eTEXAS Connections. This entire class should be
 * package-private.
 * 
 * @author ablatt
 */
class SimRequestInfo implements ConnectionRequestInfo {

    /**
     * uuid of the simulation.
     */
    private String uuid;

    /**
     * The class of the simulation to instantiate.
     */
    @SuppressWarnings("rawtypes")
    private Class simClass;

    /**
     * The configurations of the simulation.
     */
    private Map<String, Object> confs;

    /**
     * Constructor.
     * 
     * @param uuid The uuid of the simulation.
     * @param simClass The class containing the simulation.
     * @param confs The configuration parameters of the simulaiton.
     */
    @SuppressWarnings("rawtypes")
    SimRequestInfo(String uuid, Class simClass, Map<String, Object> confs) {
        this.uuid = checkUuid(uuid);
        checkSimClass(simClass);
        this.simClass = simClass;
        this.confs = checkConfs(confs);
    }

    @Override
    public boolean equals(Object obj) {

        if (obj instanceof SimRequestInfo) {
            SimRequestInfo info = (SimRequestInfo)obj;

            if (!info.uuid.equals(uuid)) {
                return false;
            }
            else if (!info.simClass.equals(simClass)) {
                return false;
            }

            return confs.equals(info.confs);
        }
        else {
            return false;
        }
    }

    @Override
    public int hashCode() {

        int hashcode = "".hashCode();

        hashcode += uuid.hashCode();
        hashcode += simClass.hashCode();
        hashcode += confs.hashCode();

        return hashcode;
    }

    /**
     * Getter.
     * 
     * @return
     */
    public String getUuid() {
        return uuid;
    }

    /**
     * Setter.
     * 
     * @param uuid
     */
    public void setUuid(String uuid) {
        this.uuid = checkUuid(uuid);
    }

    /**
     * Getter.
     * 
     * @return
     */
    @SuppressWarnings("rawtypes")
    public Class getSimClass() {
        return simClass;
    }

    /**
     * Setter.
     * 
     * @param simClass
     */
    @SuppressWarnings("rawtypes")
    public void setSimClass(Class simClass) {
        checkSimClass(simClass);
        this.simClass = simClass;
    }

    /**
     * Getter.
     * 
     * @return
     */
    public Map<String, Object> getConfs() {
        return confs;
    }

    /**
     * Setter.
     * 
     * @param confs
     */
    public void setConfs(Map<String, Object> confs) {
        this.confs = checkConfs(confs);
    }

    /**
     * Checks if the simClass is valid and throws a discriptive RuntimeException if it is not.
     * 
     * @param simClass
     */
    @SuppressWarnings("rawtypes")
    static void checkSimClass(Class simClass) {
        if (simClass == null) {
            throw new RuntimeException("SimClass cannot be null");
        }
        else if (!SimulatorInterface.class.isAssignableFrom(simClass)) {
            throw new RuntimeException("SimClass must implement the SimulatorInterface");
        }
    }

    /**
     * Ensures that the conf is not null.
     * 
     * @param confs
     * @return
     */
    static Map<String, Object> checkConfs(Map<String, Object> confs) {
        if (confs == null) {
            return new HashMap<String, Object>();
        }
        else {
            return confs;
        }
    }

    /**
     * Ensures that the uuid is not null. It is a bad idea to rely on this function for general use.
     * 
     * @param uuid
     * @return
     */
    static String checkUuid(String uuid) {
        if (uuid == null) {
            return "Generic uuid";
        }
        else {
            return uuid;
        }
    }
}
