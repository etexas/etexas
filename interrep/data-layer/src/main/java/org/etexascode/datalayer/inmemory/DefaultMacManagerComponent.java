/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2017 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
-
SBIR DATA RIGHTS
Harmonia Holdings Group, LLC
2020 Kraft Drive Suite 2400
Blacksburg, VA 24060
Contract No: DTRT57-16-c-10008
Start Date: 01/05/2016
End Date: 01/05/2018
Expiration of SBIR Data Rights Period: 01/05/2022
-
The Government's rights to use, modify, reproduce, release, perform,
display, or disclose technical data or computer software marked with
this legend are restricted during the period shown as provided in
paragraph (b)(4) of the Rights in Noncommercial Technical Data and
Computer Software-Small Business Innovation Research (SBIR) Program
clause contained in the above identified contract. No restrictions
apply after the expiration date shown above. Any reproduction of
technical data, computer software, or portions thereof marked with
this legend must also reproduce the markings.
-
Contributors:
Harmonia Holdings Group LLC: Initial API and implementation.
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
 * The default MAC manager component.
 * 
 * @author ablatt
 * @author emyers
 * @author ttevendale
 */
public class DefaultMacManagerComponent implements IMacManagerComponent {

    /** The map of current MAC addresses. */
    private Map<String, Long> currentMacAddressesMap;

    /** The random MAC address generator. */
    private Random macGenerator;

    /** The set of all MAC addresses. */
    private Set<Long> macAddresses;

    /**
     * Creates a new <code>DefaultMacManagerComponent</code> instance.
     * 
     * @param seedNumber The integer seed number for this component.
     * @param devices The iterable devices for this component.
     */
    public DefaultMacManagerComponent(int seedNumber, Iterable<? extends IDable> devices) {

        macAddresses = new HashSet<Long>();
        macGenerator = new Random(seedNumber);
        currentMacAddressesMap = new HashMap<String, Long>();

        // add the devices with predefined MAC addresses before generating new MAC addresses

        Collection<IDable> devicesWithoutMacs = new HashSet<IDable>();

        for (IDable device : devices) {

            if (device instanceof FixedCellDeviceData) {

                Long mac = ((FixedCellDeviceData)device).mac;
                currentMacAddressesMap.put(device.getProperId(), mac);
                macAddresses.add(mac);
            }
            else {

                devicesWithoutMacs.add(device);
            }
        }

        for (IDable device : devicesWithoutMacs) {

            Long mac = genMac(macAddresses);
            currentMacAddressesMap.put(device.getProperId(), mac);
            macAddresses.add(mac);
        }
    }

    /**
     * Generates a unique MAC address.
     * 
     * @param macAddresses The set of used MAC addresses.
     * @return A unique MAC address.
     */
    private Long genMac(Set<Long> macAddresses) {

        Long macAddress;

        do {

            // generate a MAC and mask out the top 3 bytes
            macAddress = macGenerator.nextLong();
            macAddress &= 0xFFFFFFFFFFl;

        }
        while (macAddresses.contains(macAddress));

        return macAddress;
    }

    @Override
    public Long getMac(int stepNum, String deviceId) {

        if (macAddresses == null) {

            return WaveMessage.MACBROADCAST;
        }

        Long macAddress = currentMacAddressesMap.get(deviceId);

        if (macAddress == null) {

            throw new RuntimeException("Requested a MAC address for a device which is not in the MAC Address Manager");
        }

        return macAddress;
    }

    @Override
    public void putNewDevices(int stepNum, Iterable<? extends IDable> newDevices) {

        if (macAddresses == null) {

            return;
        }

        for (IDable device : newDevices) {

            Long macAddress = genMac(macAddresses);
            macAddresses.add(macAddress);
            currentMacAddressesMap.put(device.getProperId(), macAddress);
        }
    }

    @Override
    public void removeDevices(int stepNum, Iterable<? extends IDable> logoutVehicles) {

        if (currentMacAddressesMap == null) {

            return;
        }

        for (IDable vehicle : logoutVehicles) {

            currentMacAddressesMap.remove(vehicle.getProperId());
        }
    }

    @Override
    public void shutdown() {

        macAddresses = null;
        currentMacAddressesMap = null;
    }

    @Override
    public void updateMacAddress(String oldDeviceId, String newDeviceId) {

        currentMacAddressesMap.put(newDeviceId, currentMacAddressesMap.remove(oldDeviceId));
    }
}
