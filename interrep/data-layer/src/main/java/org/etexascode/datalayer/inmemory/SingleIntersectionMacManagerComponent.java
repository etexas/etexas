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
package org.etexascode.datalayer.inmemory;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import org.etexascode.datalayer.interfaces.IMacManagerComponent;
import org.etexascode.devicedata.FixedCellDeviceData;
import org.etexascode.interrep.datamodel.interfaces.IDable;
import org.etexascode.wavesim.WaveMessage;

/**
 * Component for managing mac addresses.
 * 
 * @author ablatt
 */
public class SingleIntersectionMacManagerComponent implements IMacManagerComponent {

    /**
     * The MACs for the current devices Key: Device Id; Value: MAC
     */
    Map<String, Long> currMacs = new HashMap<String, Long>();

    /**
     * All the MACs which have been seen thus far
     */
    Set<Long> allMacs = new HashSet<Long>();

    /**
     * Current Random
     */
    Random r = null;

    /**
     * Constructor
     * 
     * @param seed The random seed to use
     * @param newDevices The new devices which will be initialized
     */
    public SingleIntersectionMacManagerComponent(int seed, Iterable<? extends IDable> newDevices) {
        r = new Random(seed);

        // adds the devices with pre-defined macs before the randomly generated macs are made for
        // devices without macs
        Collection<IDable> devicesWithoutMacs = new HashSet<IDable>();
        for (IDable idi : newDevices) {
            if (idi instanceof FixedCellDeviceData) {
                String s = idi.getProperId();
                Long mac = ((FixedCellDeviceData)idi).mac;
                allMacs.add(mac);
                currMacs.put(s, mac);
            }
            else {
                devicesWithoutMacs.add(idi);
            }
        }
        for (IDable idi : devicesWithoutMacs) {
            String s = idi.getProperId();
            Long mac = genMac(allMacs);
            allMacs.add(mac);
            currMacs.put(s, mac);
        }
    }

    /**
     * Adds new devices to MACs
     * 
     * @param stepNum The current step number
     * @param newDevices The list of new devices to add
     */
    @Override
    public void putNewDevices(int stepNum, Iterable<? extends IDable> newDevices) {
        if (allMacs == null) {
            return;
        }

        for (IDable idi : newDevices) {
            String s = idi.getProperId();
            Long mac = genMac(allMacs);
            allMacs.add(mac);
            currMacs.put(s, mac);
        }
    }

    /**
     * Removes vehicles from the MACs
     * 
     * @param stepNum The current step number
     * @param logoutVehicles The vehicles to be removed from the MACs
     */
    @Override
    public void removeDevices(int stepNum, Iterable<? extends IDable> logoutVehicles) {
        if (currMacs == null) {
            return;
        }

        for (IDable vi : logoutVehicles) {
            currMacs.remove(vi.getProperId());
        }
    }

    /**
     * Gets a MAC from a device
     * 
     * @param stepNum The current step number
     * @param deviceId The id of the device from which to retrieve the MAC
     * @return The MACs for the device
     */
    @Override
    public Long getMac(int stepNum, String deviceId) {
        if (allMacs == null) {
            return WaveMessage.MACBROADCAST;
        }

        Long ret = currMacs.get(deviceId);

        if (ret == null) {
            throw new RuntimeException("Requested a MAC for a device which is not in the Mac Manager");
        }

        return ret;
    }

    /**
     * Shuts down the MACs
     */
    @Override
    public void shutdown() {
        allMacs = null;
        currMacs = null;
    }

    /**
     * Generate a unique mac
     * 
     * @param macs The macs seen thus far
     * @return The new mac
     */
    Long genMac(Set<Long> macs) {
        Long ret = genRandomMac(r);
        while (macs.contains(ret)) {
            ret = genRandomMac(r);
        }
        return ret;
    }

    /**
     * Generate a new mac address
     * 
     * @param r A random number generator
     * @return A new mac
     */
    Long genRandomMac(Random r) {
        long ret = r.nextLong();
        ret &= 0xFFFFFFFFFFl; // mask out the top 3 bytes
        return ret;
    }

    @Override
    public void updateMacAddress(String oldDeviceId, String newDeviceId) {}
}
